//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::os::{dup_cloexec, write, OsResult};
use crate::parse::command::{IORedirectionKind, Redirection, RedirectionKind};
use crate::shell::{CommandExecutionError, Shell};
use crate::shstr::{ShStr, ShString};
use crate::wordexp::expand_word_to_string;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::os::fd::{AsRawFd, RawFd};
use std::os::unix::fs::OpenOptionsExt;
use std::rc::Rc;

pub const STDIN_FILENO: u32 = libc::STDIN_FILENO as u32;
pub const STDOUT_FILENO: u32 = libc::STDOUT_FILENO as u32;
pub const STDERR_FILENO: u32 = libc::STDERR_FILENO as u32;

pub type RedirectionResult = Result<(), CommandExecutionError>;

/// POSIX: a redirection that creates a file requests mode 0666; the process
/// umask, applied by the kernel, is what narrows it. Passing the shell's own
/// mask here instead would both use the wrong bits and apply the mask twice.
const FILE_CREATION_MODE: u32 = 0o666;

#[derive(Clone)]
pub enum OpenedFile {
    Stdin,
    Stdout,
    Stderr,
    ReadFile(Rc<File>),
    WriteFile(Rc<File>),
    ReadWriteFile(Rc<File>),
    /// `n>&-` / `n<&-`: the descriptor is closed for the command. Removing the
    /// entry from the table is not enough — an absent descriptor is simply
    /// inherited from the shell.
    Closed,
    /// A here-document. The remaining text is shared between clones of
    /// `OpenedFiles`, the way a real descriptor shares its file offset, so
    /// that `read` in a loop consumes successive lines.
    HereDocument(Rc<RefCell<ShString>>),
}

impl OpenedFile {
    /// Whether both entries are the same open file: the one an unchanged
    /// descriptor keeps when its table is cloned.
    fn is_same_as(&self, other: &OpenedFile) -> bool {
        match (self, other) {
            (OpenedFile::Stdin, OpenedFile::Stdin)
            | (OpenedFile::Stdout, OpenedFile::Stdout)
            | (OpenedFile::Stderr, OpenedFile::Stderr)
            | (OpenedFile::Closed, OpenedFile::Closed) => true,
            (OpenedFile::ReadFile(a), OpenedFile::ReadFile(b))
            | (OpenedFile::WriteFile(a), OpenedFile::WriteFile(b))
            | (OpenedFile::ReadWriteFile(a), OpenedFile::ReadWriteFile(b)) => Rc::ptr_eq(a, b),
            (OpenedFile::HereDocument(a), OpenedFile::HereDocument(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

/// What a command's redirections replaced in the shell's descriptor table,
/// so that they can be undone when the command completes.
#[must_use = "the redirections must be undone with OpenedFiles::restore"]
pub struct SavedDescriptors(Vec<(u32, Option<OpenedFile>)>);

/// Whether a failed redirection was opening a file to read or creating one.
#[derive(Clone, Copy)]
enum OpenPurpose {
    Read,
    Create,
}

/// The diagnostic for a redirection that could not open `target`: which
/// file, what for, and the system's reason.
fn open_failure(
    purpose: OpenPurpose,
    target: &ShStr,
    err: &std::io::Error,
) -> CommandExecutionError {
    let action = match purpose {
        OpenPurpose::Read => "cannot open",
        OpenPurpose::Create => "cannot create",
    };
    CommandExecutionError::Redirection(format!(
        "sh: {action} '{}': {}",
        target.display(),
        plib::diag::io_error_text(err)
    ))
}

/// A standard descriptor that a pipeline or command substitution replaces
/// with a pipe end in its child process.
#[derive(Clone, Copy)]
pub enum StandardFd {
    Stdin,
    Stdout,
}

impl StandardFd {
    fn fileno(self) -> u32 {
        match self {
            StandardFd::Stdin => STDIN_FILENO,
            StandardFd::Stdout => STDOUT_FILENO,
        }
    }

    /// Whether `file` names the shell's real descriptor for `self`.
    fn is(self, file: &OpenedFile) -> bool {
        matches!(
            (self, file),
            (StandardFd::Stdin, OpenedFile::Stdin) | (StandardFd::Stdout, OpenedFile::Stdout)
        )
    }
}

#[derive(Clone)]
pub struct OpenedFiles {
    pub opened_files: HashMap<u32, OpenedFile>,
    /// The errno of the first failed `write_out`/`write_err`, until taken.
    write_error: Cell<Option<i32>>,
}

impl OpenedFiles {
    fn io_redirect(
        &mut self,
        kind: &IORedirectionKind,
        target: &ShStr,
        file_descriptor: Option<u32>,
        shell: &Shell,
    ) -> RedirectionResult {
        match kind {
            IORedirectionKind::RedirectOutput
            | IORedirectionKind::RedirectOutputClobber
            | IORedirectionKind::RedirectOuputAppend => {
                let append = *kind == IORedirectionKind::RedirectOuputAppend;
                let noclobber_active = *kind != IORedirectionKind::RedirectOutputClobber
                    && !append
                    && shell.set_options.noclobber;

                let file = if noclobber_active {
                    // Atomically create the file with O_CREAT|O_EXCL so the
                    // existence check and creation cannot race.
                    match File::options()
                        .mode(FILE_CREATION_MODE)
                        .write(true)
                        .create_new(true)
                        .open(target.as_os_str())
                    {
                        Ok(file) => file,
                        Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => {
                            // noclobber only forbids overwriting an existing
                            // regular file; FIFOs/devices may still be written.
                            let is_regular = std::fs::metadata(target.as_os_str())
                                .map(|m| m.is_file())
                                .unwrap_or(true);
                            if is_regular {
                                return Err(CommandExecutionError::Redirection(format!(
                                    "sh: redirection would overwrite existing file {}",
                                    target.display()
                                )));
                            }
                            File::options()
                                .mode(FILE_CREATION_MODE)
                                .write(true)
                                .create(true)
                                .open(target.as_os_str())
                                .map_err(|err| open_failure(OpenPurpose::Create, target, &err))?
                        }
                        Err(err) => return Err(open_failure(OpenPurpose::Create, target, &err)),
                    }
                } else {
                    File::options()
                        .mode(FILE_CREATION_MODE)
                        .write(true)
                        .truncate(!append)
                        .append(append)
                        .create(true)
                        .open(target.as_os_str())
                        .map_err(|err| open_failure(OpenPurpose::Create, target, &err))?
                };

                let source_fd = file_descriptor.unwrap_or(STDOUT_FILENO);
                self.opened_files
                    .insert(source_fd, OpenedFile::WriteFile(Rc::new(file)));
            }
            IORedirectionKind::DuplicateOutput | IORedirectionKind::DuplicateInput => {
                let dest_fd = if *kind == IORedirectionKind::DuplicateOutput {
                    file_descriptor.unwrap_or(STDOUT_FILENO)
                } else {
                    file_descriptor.unwrap_or(STDIN_FILENO)
                };
                if *target == "-" {
                    self.opened_files.insert(dest_fd, OpenedFile::Closed);
                } else {
                    let duplicate_input = *kind == IORedirectionKind::DuplicateInput;
                    // A descriptor number is text by definition.
                    let source_fd = target
                        .to_str()
                        .and_then(|t| t.parse::<u32>().ok())
                        .ok_or_else(|| {
                            CommandExecutionError::Redirection(format!(
                                "sh: invalid file descriptor {}",
                                target.display()
                            ))
                        })?;
                    match self.opened_files.get(&source_fd) {
                        Some(OpenedFile::WriteFile(_))
                        | Some(OpenedFile::Stdout)
                        | Some(OpenedFile::Stderr)
                            if duplicate_input =>
                        {
                            return Err(CommandExecutionError::Redirection(format!(
                                "sh: '{source_fd}' is not opened for reading"
                            )));
                        }
                        Some(OpenedFile::ReadFile(_))
                        | Some(OpenedFile::Stdin)
                        | Some(OpenedFile::HereDocument(_))
                            if !duplicate_input =>
                        {
                            return Err(CommandExecutionError::Redirection(format!(
                                "sh: '{source_fd}' is not opened for writing"
                            )));
                        }
                        None => {
                            return Err(CommandExecutionError::Redirection(format!(
                                "sh: cannot duplicate unopened file descriptor '{source_fd}'"
                            )))
                        }
                        Some(other) => {
                            self.opened_files.insert(dest_fd, other.clone());
                        }
                    }
                }
            }
            IORedirectionKind::RedirectInput => {
                let file = File::options()
                    .mode(FILE_CREATION_MODE)
                    .read(true)
                    .open(target.as_os_str())
                    .map_err(|err| open_failure(OpenPurpose::Read, target, &err))?;
                let source_fd = file_descriptor.unwrap_or(STDIN_FILENO);
                self.opened_files
                    .insert(source_fd, OpenedFile::ReadFile(Rc::new(file)));
            }
            IORedirectionKind::OpenRW => {
                let file = File::options()
                    .mode(FILE_CREATION_MODE)
                    .read(true)
                    .write(true)
                    .create(true)
                    .open(target.as_os_str())
                    .map_err(|err| open_failure(OpenPurpose::Create, target, &err))?;
                let source_fd = file_descriptor.unwrap_or(STDIN_FILENO);
                self.opened_files
                    .insert(source_fd, OpenedFile::ReadWriteFile(Rc::from(file)));
            }
        }
        Ok(())
    }

    pub fn redirect(
        &mut self,
        redirections: &[Redirection],
        shell: &mut Shell,
    ) -> RedirectionResult {
        for redir in redirections {
            match &redir.kind {
                RedirectionKind::IORedirection { kind, file } => {
                    let file = expand_word_to_string(&file.word, false, shell)?;
                    // A redirection target is a path, so it keeps its bytes.
                    self.io_redirect(kind, &file, redir.file_descriptor, shell)?;
                }
                RedirectionKind::HereDocument { contents, .. } => {
                    let contents = expand_word_to_string(&contents.word, false, shell)?;
                    self.opened_files.insert(
                        redir.file_descriptor.unwrap_or(STDIN_FILENO),
                        OpenedFile::HereDocument(Rc::new(RefCell::new(contents.clone()))),
                    );
                }
                RedirectionKind::QuotedHereDocument { contents, .. } => {
                    self.opened_files.insert(
                        redir.file_descriptor.unwrap_or(STDIN_FILENO),
                        OpenedFile::HereDocument(Rc::new(RefCell::new(contents.clone()))),
                    );
                }
            }
        }
        Ok(())
    }

    fn write_file(&self, fileno: u32, contents: &[u8]) {
        let result = match self.opened_files.get(&fileno) {
            Some(OpenedFile::Stdout) => std::io::stdout().write_all(contents),
            Some(OpenedFile::Stderr) => std::io::stderr().write_all(contents),
            Some(OpenedFile::WriteFile(file)) | Some(OpenedFile::ReadWriteFile(file)) => {
                write(file.as_raw_fd(), contents)
                    .map_err(|_| std::io::Error::last_os_error())
                    .map(|_| ())
            }
            // The descriptor was closed, or redirected to something that
            // cannot be written to (a read-only file, a here-document).
            Some(OpenedFile::ReadFile(_))
            | Some(OpenedFile::HereDocument(_))
            | Some(OpenedFile::Closed)
            | None => Err(std::io::Error::from_raw_os_error(libc::EBADF)),
            // `exec 1<&0`: writing goes to the shell's stdin descriptor, which
            // succeeds when it is open for writing too (a terminal).
            Some(OpenedFile::Stdin) => write(libc::STDIN_FILENO, contents)
                .map_err(|_| std::io::Error::last_os_error())
                .map(|_| ()),
        };
        // A failed write is remembered rather than reported here: the
        // utility doing the writing reports it once, by name, and fails.
        if let Err(err) = result {
            if self.write_error.get().is_none() {
                self.write_error
                    .set(Some(err.raw_os_error().unwrap_or(libc::EIO)));
            }
        }
    }

    /// The first write that failed since the last call, if any.
    pub fn take_write_error(&self) -> Option<std::io::Error> {
        self.write_error
            .take()
            .map(std::io::Error::from_raw_os_error)
    }

    /// Takes bytes: output such as `export -p` is meant to be read back by the
    /// shell, so a value that is not valid text has to survive it.
    pub fn write_out<S: AsRef<[u8]>>(&self, string: S) {
        self.write_file(STDOUT_FILENO, string.as_ref());
    }

    pub fn write_err<S: AsRef<[u8]>>(&self, string: S) {
        self.write_file(STDERR_FILENO, string.as_ref());
    }

    /// Points standard input at `/dev/null`, unless it has already been
    /// redirected explicitly (POSIX 2.11, asynchronous lists).
    pub fn redirect_stdin_to_dev_null(&mut self) {
        if !matches!(
            self.opened_files.get(&STDIN_FILENO),
            Some(OpenedFile::Stdin)
        ) {
            return;
        }
        if let Ok(file) = File::options().read(true).open("/dev/null") {
            self.opened_files
                .insert(STDIN_FILENO, OpenedFile::ReadFile(Rc::new(file)));
        }
    }

    /// Called just before the process puts something new (a pipe end) on the
    /// real standard descriptor `fd`, as a pipeline or command substitution
    /// does in its child. From then on the command's descriptor `fd` is that
    /// new file, whatever an earlier `exec` redirected it to (`exec </dev/null;
    /// echo a | cat` must still read the pipe). Any other descriptor that was a
    /// copy of the old one (`exec 7<&0`, `exec 3>&1`) keeps its file: it is
    /// given a descriptor of its own before the old one is replaced.
    pub fn take_over_standard_fd(&mut self, fd: StandardFd) -> OsResult<()> {
        let fileno = fd.fileno();
        let mut detached: Option<Rc<File>> = None;
        for (id, file) in self.opened_files.iter_mut() {
            if *id == fileno || !fd.is(file) {
                continue;
            }
            let copy = match &detached {
                Some(copy) => copy.clone(),
                None => detached
                    .insert(Rc::new(File::from(dup_cloexec(fileno as RawFd)?)))
                    .clone(),
            };
            *file = match fd {
                StandardFd::Stdin => OpenedFile::ReadFile(copy),
                StandardFd::Stdout => OpenedFile::WriteFile(copy),
            };
        }
        let standard = match fd {
            StandardFd::Stdin => OpenedFile::Stdin,
            StandardFd::Stdout => OpenedFile::Stdout,
        };
        self.opened_files.insert(fileno, standard);
        Ok(())
    }

    /// Makes `table` (this table with a command's redirections applied) the
    /// shell's descriptor table while the command runs in the current shell
    /// environment: a compound command, a function, `eval` or `.`. Returns
    /// the entries it replaced, for `restore`.
    ///
    /// Only the descriptors that differ are saved, so that whatever an `exec`
    /// run by the command does to any other descriptor stays in effect once
    /// the command completes (`{ exec 4>&1; } >/dev/null` leaves fd 4 open
    /// on /dev/null, as in dash and bash).
    pub fn install(&mut self, table: OpenedFiles) -> SavedDescriptors {
        let mut saved = Vec::new();
        for (fd, file) in &self.opened_files {
            let unchanged = table
                .opened_files
                .get(fd)
                .is_some_and(|new| new.is_same_as(file));
            if !unchanged {
                saved.push((*fd, Some(file.clone())));
            }
        }
        for fd in table.opened_files.keys() {
            if !self.opened_files.contains_key(fd) {
                saved.push((*fd, None));
            }
        }
        *self = table;
        SavedDescriptors(saved)
    }

    /// Undoes `install`: each saved descriptor gets back the entry it had.
    pub fn restore(&mut self, saved: SavedDescriptors) {
        for (fd, file) in saved.0 {
            match file {
                Some(file) => self.opened_files.insert(fd, file),
                None => self.opened_files.remove(&fd),
            };
        }
    }

    pub fn get_file(&self, fileno: u32) -> Option<&OpenedFile> {
        self.opened_files.get(&fileno)
    }
}

impl Default for OpenedFiles {
    fn default() -> Self {
        let opened_files = HashMap::from([
            (STDIN_FILENO, OpenedFile::Stdin),
            (STDOUT_FILENO, OpenedFile::Stdout),
            (STDERR_FILENO, OpenedFile::Stderr),
        ]);
        Self {
            opened_files,
            write_error: Cell::new(None),
        }
    }
}
