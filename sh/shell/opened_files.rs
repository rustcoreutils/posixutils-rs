//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Redirections act on the shell process's own descriptors, as in dash.
//!
//! XCU 2.7: a redirection opens, duplicates or closes a descriptor of the
//! command it is attached to, and `exec` without a command applies its
//! redirections to the current shell execution environment. So they are
//! applied with `dup2`/`close` to the real descriptors, in order. A
//! redirection that lasts only as long as its command (a built-in, a
//! function, a compound command, a utility the shell forks) first saves
//! the descriptor it replaces in a copy above 9, close-on-exec, and puts
//! it back when the command completes; `exec` keeps the new descriptors
//! instead. Nothing then needs a table of what each descriptor "really"
//! is: a child process, `/dev/fd/N`, `/proc/self/fd` and a reader waiting
//! for end-of-file all see the same descriptors the shell uses.
//!
//! The shell's private descriptors (the saved copies and the signal
//! self-pipe) live above 9 where possible and are moved out of the way of
//! any redirection that names their number. Under a descriptor limit of
//! 10 there are none above 9; they then take the lowest free number.

use crate::os::errno::Errno;
use crate::os::signals::{is_signal_pipe_fd, move_signal_pipe_fd};
use crate::os::{close, dup2, dup_private, here_document_fd, OsError, OsResult};
use crate::parse::command::{IORedirectionKind, Redirection, RedirectionKind};
use crate::shell::{CommandExecutionError, Shell};
use crate::shstr::ShStr;
use crate::wordexp::expand_word_to_string;
use std::cell::Cell;
use std::ffi::CString;
use std::os::fd::{AsRawFd, FromRawFd, IntoRawFd, OwnedFd, RawFd};

pub const STDIN_FILENO: RawFd = libc::STDIN_FILENO;
pub const STDOUT_FILENO: RawFd = libc::STDOUT_FILENO;
pub const STDERR_FILENO: RawFd = libc::STDERR_FILENO;

pub type RedirectionResult = Result<(), CommandExecutionError>;

/// POSIX: a redirection that creates a file requests mode 0666; the process
/// umask, applied by the kernel, is what narrows it. Passing the shell's own
/// mask here instead would both use the wrong bits and apply the mask twice.
const FILE_CREATION_MODE: u32 = 0o666;

/// Opens `target` for a redirection, with `flags` and O_CLOEXEC (the
/// shell places the descriptor itself). Opening a FIFO waits for the other
/// end; ^C in an interactive shell ends that wait with EINTR, as in dash,
/// and any other signal lets it go on. std's `open` always retried EINTR,
/// so `read x < fifo` at the prompt could not be interrupted.
fn open_for_redirection(
    target: &ShStr,
    flags: libc::c_int,
    shell: &Shell,
) -> std::io::Result<OwnedFd> {
    let path = CString::new(target.as_bytes()).map_err(|_| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "file name contained an unexpected NUL byte",
        )
    })?;
    loop {
        let fd = unsafe {
            libc::open(
                path.as_ptr(),
                flags | libc::O_CLOEXEC,
                FILE_CREATION_MODE as libc::c_int,
            )
        };
        if fd >= 0 {
            return Ok(unsafe { OwnedFd::from_raw_fd(fd) });
        }
        let err = std::io::Error::last_os_error();
        if err.raw_os_error() != Some(libc::EINTR) || shell.signal_manager.interrupt_pending() {
            return Err(err);
        }
    }
}

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

/// The diagnostic for a descriptor that could not be duplicated, closed
/// or put back: the descriptor and the system's reason, as in dash and
/// bash (`sh: 3: Bad file descriptor`).
fn descriptor_failure(fd: impl std::fmt::Display, errno: Errno) -> CommandExecutionError {
    CommandExecutionError::Redirection(format!(
        "sh: {fd}: {}",
        plib::diag::io_error_text(&std::io::Error::from(errno))
    ))
}

/// What a redirection puts on its descriptor.
enum Source {
    /// A file just opened (close-on-exec, owned until placed).
    Open(OwnedFd),
    /// `n>&m`, `n<&m`: a copy of descriptor `m`.
    Duplicate(RawFd),
    /// `n>&-`, `n<&-`.
    Close,
}

/// A descriptor a temporary redirection replaced, and the copy holding its
/// previous open file until the redirection is undone; `None` when it was
/// not open, so undoing closes it.
#[derive(Clone)]
struct SavedFd {
    fd: RawFd,
    copy: Option<RawFd>,
}

/// The descriptors one command's redirections replaced.
#[derive(Clone, Default)]
struct Frame {
    saved: Vec<SavedFd>,
    /// Set by `exec`: the redirections stay when the command completes.
    keep: bool,
}

/// The saved descriptors of every command whose redirections are in
/// effect, innermost last.
#[derive(Clone, Default)]
pub struct RedirectionFrames {
    frames: Vec<Frame>,
}

impl RedirectionFrames {
    fn saved_mut(&mut self) -> impl Iterator<Item = &mut SavedFd> {
        self.frames
            .iter_mut()
            .flat_map(|frame| frame.saved.iter_mut())
    }

    fn holds_copy(&self, fd: RawFd) -> bool {
        self.frames
            .iter()
            .flat_map(|frame| frame.saved.iter())
            .any(|saved| saved.copy == Some(fd))
    }

    /// Whether `fd` is one of the shell's own descriptors, which no
    /// redirection may read: a saved copy or the signal pipe.
    fn is_private(&self, fd: RawFd) -> bool {
        self.holds_copy(fd) || is_signal_pipe_fd(fd)
    }

    /// Moves the shell's own descriptor numbered `fd`, if there is one, to
    /// another number, so that a redirection can take `fd`.
    fn make_room(&mut self, fd: RawFd) -> OsResult<()> {
        if self.holds_copy(fd) {
            let copy = dup_private(fd)?;
            for saved in self.saved_mut().filter(|saved| saved.copy == Some(fd)) {
                saved.copy = Some(copy);
            }
            let _ = close(fd);
        } else if is_signal_pipe_fd(fd) {
            move_signal_pipe_fd(fd)?;
        }
        Ok(())
    }

    /// Records what `fd` is now in the innermost frame, unless it already
    /// holds `fd`: undoing the frame restores what it was before the first
    /// of the command's redirections of it. `closed` says `fd` was not
    /// open, though it may be by now: the file a redirection opened takes
    /// the lowest free descriptor, which can be `fd` itself.
    fn save(&mut self, fd: RawFd, closed: bool) -> OsResult<()> {
        let Some(frame) = self.frames.last_mut() else {
            return Ok(());
        };
        if frame.saved.iter().any(|saved| saved.fd == fd) {
            return Ok(());
        }
        let copy = if closed {
            None
        } else {
            match dup_private(fd) {
                Ok(copy) => Some(copy),
                Err(err) if err.errno == Errno::EBADF => None,
                Err(err) => return Err(err),
            }
        };
        frame.saved.push(SavedFd { fd, copy });
        Ok(())
    }

    /// Whether a redirection in effect replaced `fd`.
    fn redirects(&self, fd: RawFd) -> bool {
        self.frames
            .iter()
            .any(|frame| frame.saved.iter().any(|saved| saved.fd == fd))
    }

    /// In a child process, which never undoes the redirections of the
    /// commands it was forked from: closes their saved copies.
    fn forget(&mut self) {
        for frame in std::mem::take(&mut self.frames) {
            for copy in frame.saved.into_iter().filter_map(|saved| saved.copy) {
                let _ = close(copy);
            }
        }
    }
}

/// Puts `source` on descriptor `fd`. With `save`, what `fd` was is
/// recorded in the innermost frame first.
fn place(frames: &mut RedirectionFrames, fd: RawFd, source: Source, save: bool) -> OsResult<()> {
    let opened_on_fd = matches!(&source, Source::Open(file) if file.as_raw_fd() == fd);
    if !opened_on_fd {
        frames.make_room(fd)?;
    }
    if save {
        frames.save(fd, opened_on_fd)?;
    }
    match source {
        Source::Open(file) => {
            if file.as_raw_fd() == fd {
                // `fd` was closed and the open took it: it only has to
                // survive exec like any other redirected descriptor.
                let fd = file.into_raw_fd();
                if unsafe { libc::fcntl(fd, libc::F_SETFD, 0) } < 0 {
                    return Err(OsError::from_current_errno("fcntl"));
                }
            } else {
                dup2(file.as_raw_fd(), fd)?;
            }
        }
        Source::Duplicate(from) => {
            if from != fd {
                dup2(from, fd)?;
            }
        }
        Source::Close => {
            // Closing a descriptor that is not open is not an error.
            let _ = close(fd);
        }
    }
    Ok(())
}

impl Shell {
    /// Starts a command whose redirections are undone when it completes;
    /// every call must be paired with `pop_redirections`.
    pub fn push_redirections(&mut self) {
        self.redirection_frames.frames.push(Frame::default());
    }

    /// `exec` without a command: the redirections of the command being
    /// run (the innermost frame) stay in effect once it completes.
    pub fn keep_redirections(&mut self) {
        if let Some(frame) = self.redirection_frames.frames.last_mut() {
            frame.keep = true;
        }
    }

    /// Ends the command started by the last `push_redirections`, putting
    /// back each descriptor its redirections replaced, last first.
    pub fn pop_redirections(&mut self) {
        let frames = &mut self.redirection_frames;
        let Some(keep) = frames.frames.last().map(|frame| frame.keep) else {
            return;
        };
        while let Some(saved) = frames.frames.last_mut().and_then(|frame| frame.saved.pop()) {
            if keep {
                if let Some(copy) = saved.copy {
                    let _ = close(copy);
                }
                continue;
            }
            // The signal pipe may have been moved onto a descriptor that a
            // redirection closed meanwhile.
            let _ = frames.make_room(saved.fd);
            match saved.copy {
                Some(copy) => {
                    let _ = dup2(copy, saved.fd);
                    let _ = close(copy);
                }
                None => {
                    let _ = close(saved.fd);
                }
            }
        }
        frames.frames.pop();
    }

    /// In a newly forked child: the commands it was forked from are
    /// completed by the parent, so their saved descriptors are closed here.
    pub fn forget_redirections(&mut self) {
        self.redirection_frames.forget();
    }

    /// Whether a redirection of an enclosing command is in effect for
    /// descriptor `fd` (one made by `exec` is not).
    pub fn is_redirected(&self, fd: RawFd) -> bool {
        self.redirection_frames.redirects(fd)
    }

    /// Puts `file` on descriptor `fd` for good, as a pipeline or command
    /// substitution does with a pipe end in its child process.
    pub fn place_descriptor(&mut self, file: OwnedFd, fd: RawFd) -> OsResult<()> {
        place(&mut self.redirection_frames, fd, Source::Open(file), false)
    }

    fn redirect_fd(&mut self, fd: RawFd, source: Source, save: bool) -> RedirectionResult {
        place(&mut self.redirection_frames, fd, source, save)
            .map_err(|err| descriptor_failure(fd, err.errno))
    }

    /// Opens `target` for an output redirection (`>`, `>|`, `>>`).
    fn open_for_output(
        &self,
        kind: &IORedirectionKind,
        target: &ShStr,
    ) -> Result<OwnedFd, CommandExecutionError> {
        let append = *kind == IORedirectionKind::RedirectOuputAppend;
        let noclobber_active = *kind != IORedirectionKind::RedirectOutputClobber
            && !append
            && self.set_options.noclobber;

        if noclobber_active {
            // Atomically create the file with O_CREAT|O_EXCL so the
            // existence check and creation cannot race.
            match open_for_redirection(target, libc::O_WRONLY | libc::O_CREAT | libc::O_EXCL, self)
            {
                Ok(file) => Ok(file),
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
                    open_for_redirection(target, libc::O_WRONLY | libc::O_CREAT, self)
                        .map_err(|err| open_failure(OpenPurpose::Create, target, &err))
                }
                Err(err) => Err(open_failure(OpenPurpose::Create, target, &err)),
            }
        } else {
            let mode = if append {
                libc::O_APPEND
            } else {
                libc::O_TRUNC
            };
            open_for_redirection(target, libc::O_WRONLY | libc::O_CREAT | mode, self)
                .map_err(|err| open_failure(OpenPurpose::Create, target, &err))
        }
    }

    fn io_redirect(
        &mut self,
        kind: &IORedirectionKind,
        target: &ShStr,
        file_descriptor: Option<u32>,
        save: bool,
    ) -> RedirectionResult {
        let default_fd = match kind {
            IORedirectionKind::RedirectOutput
            | IORedirectionKind::RedirectOutputClobber
            | IORedirectionKind::RedirectOuputAppend
            | IORedirectionKind::DuplicateOutput => STDOUT_FILENO,
            IORedirectionKind::RedirectInput
            | IORedirectionKind::DuplicateInput
            | IORedirectionKind::OpenRW => STDIN_FILENO,
        };
        let fd = match file_descriptor {
            Some(fd) => RawFd::try_from(fd).map_err(|_| descriptor_failure(fd, Errno::EBADF))?,
            None => default_fd,
        };
        let source = match kind {
            IORedirectionKind::RedirectOutput
            | IORedirectionKind::RedirectOutputClobber
            | IORedirectionKind::RedirectOuputAppend => {
                Source::Open(self.open_for_output(kind, target)?)
            }
            IORedirectionKind::DuplicateOutput | IORedirectionKind::DuplicateInput => {
                if *target == "-" {
                    Source::Close
                } else {
                    // A descriptor number is text by definition.
                    let from = target
                        .to_str()
                        .and_then(|t| t.parse::<RawFd>().ok())
                        .filter(|&from| from >= 0)
                        .ok_or_else(|| {
                            CommandExecutionError::Redirection(format!(
                                "sh: invalid file descriptor {}",
                                target.display()
                            ))
                        })?;
                    // XCU 2.7.5, 2.7.6: both operators make the descriptor
                    // a copy of `from` whatever it was opened for, as
                    // dup2() does; the operator only picks the default
                    // descriptor. Using the copy the other way fails where
                    // it is used, with the system's error. The shell's own
                    // descriptors are not the user's to copy.
                    if self.redirection_frames.is_private(from)
                        || unsafe { libc::fcntl(from, libc::F_GETFD) } < 0
                    {
                        return Err(descriptor_failure(from, Errno::EBADF));
                    }
                    Source::Duplicate(from)
                }
            }
            IORedirectionKind::RedirectInput => Source::Open(
                open_for_redirection(target, libc::O_RDONLY, self)
                    .map_err(|err| open_failure(OpenPurpose::Read, target, &err))?,
            ),
            IORedirectionKind::OpenRW => Source::Open(
                open_for_redirection(target, libc::O_RDWR | libc::O_CREAT, self)
                    .map_err(|err| open_failure(OpenPurpose::Create, target, &err))?,
            ),
        };
        self.redirect_fd(fd, source, save)
    }

    /// Applies `redirections` to the shell's descriptors, in order. With
    /// `save`, each descriptor they replace is saved in the innermost frame
    /// (see `push_redirections`), to be put back by `pop_redirections`;
    /// without, the process is about to be replaced or is a child that
    /// exits once the command completes.
    ///
    /// A redirection that fails leaves the earlier ones in place (and
    /// saved); undoing the frame undoes them too.
    pub fn apply_redirections(
        &mut self,
        redirections: &[Redirection],
        save: bool,
    ) -> RedirectionResult {
        for redir in redirections {
            match &redir.kind {
                RedirectionKind::IORedirection { kind, file } => {
                    let file = expand_word_to_string(&file.word, false, self)?;
                    // A redirection target is a path, so it keeps its bytes.
                    self.io_redirect(kind, &file, redir.file_descriptor, save)?;
                }
                RedirectionKind::HereDocument { contents, .. } => {
                    let contents = expand_word_to_string(&contents.word, false, self)?;
                    self.here_document(redir.file_descriptor, contents.as_bytes(), save)?;
                }
                RedirectionKind::QuotedHereDocument { contents, .. } => {
                    self.here_document(redir.file_descriptor, contents.as_bytes(), save)?;
                }
            }
        }
        Ok(())
    }

    fn here_document(
        &mut self,
        file_descriptor: Option<u32>,
        contents: &[u8],
        save: bool,
    ) -> RedirectionResult {
        let fd = match file_descriptor {
            Some(fd) => RawFd::try_from(fd).map_err(|_| descriptor_failure(fd, Errno::EBADF))?,
            None => STDIN_FILENO,
        };
        let file = here_document_fd(contents)
            .map_err(|err| CommandExecutionError::Redirection(format!("{err}")))?;
        self.redirect_fd(fd, Source::Open(file), save)
    }

    /// Runs `run` with `redirections` in effect, and undoes them afterwards
    /// unless an `exec` it ran kept them (see `keep_redirections`).
    pub fn with_redirections<T>(
        &mut self,
        redirections: &[Redirection],
        run: impl FnOnce(&mut Self) -> Result<T, CommandExecutionError>,
    ) -> Result<T, CommandExecutionError> {
        self.push_redirections();
        let result = self
            .apply_redirections(redirections, true)
            .and_then(|()| run(self));
        self.pop_redirections();
        result
    }

    /// Points standard input at /dev/null, as an asynchronous list's is
    /// without job control (POSIX 2.11).
    pub fn redirect_stdin_to_dev_null(&mut self) {
        if let Ok(file) = open_for_redirection(ShStr::new("/dev/null"), libc::O_RDONLY, self) {
            let _ = self.place_descriptor(file, STDIN_FILENO);
        }
    }
}

/// Where a utility running in the shell process writes its output: the
/// shell's standard output and standard error, which its redirections, if
/// any, have already replaced. The first write that fails is remembered,
/// so that the utility can report it once, by name.
#[derive(Clone, Default)]
pub struct OpenedFiles {
    /// The errno of the first failed `write_out`/`write_err`, until taken.
    write_error: Cell<Option<i32>>,
}

impl OpenedFiles {
    fn write_file(&self, fd: RawFd, mut contents: &[u8]) {
        let mut result = Ok(());
        while !contents.is_empty() {
            let written = unsafe {
                libc::write(fd, contents.as_ptr() as *const libc::c_void, contents.len())
            };
            if written < 0 {
                let err = std::io::Error::last_os_error();
                if err.raw_os_error() == Some(libc::EINTR) {
                    continue;
                }
                result = Err(err.raw_os_error().unwrap_or(libc::EIO));
                break;
            }
            if written == 0 {
                result = Err(libc::EIO);
                break;
            }
            contents = &contents[written as usize..];
        }
        // A failed write is remembered rather than reported here: the
        // utility doing the writing reports it once, by name, and fails.
        if let Err(errno) = result {
            if self.write_error.get().is_none() {
                self.write_error.set(Some(errno));
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
}
