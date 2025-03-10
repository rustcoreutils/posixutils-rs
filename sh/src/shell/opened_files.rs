use crate::parse::command::{IORedirectionKind, Redirection, RedirectionKind};
use crate::shell::{CommandExecutionError, Shell};
use crate::wordexp::expand_word_to_string;
use nix::libc;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{Read, Write};
use std::os::fd::AsRawFd;
use std::path::Path;
use std::rc::Rc;

const STDIN_FILENO: u32 = libc::STDIN_FILENO as u32;
const STDOUT_FILENO: u32 = libc::STDOUT_FILENO as u32;
const STDERR_FILENO: u32 = libc::STDERR_FILENO as u32;

pub type RedirectionResult = Result<(), CommandExecutionError>;

#[derive(Clone)]
pub enum OpenedFile {
    Stdin,
    Stdout,
    Stderr,
    ReadFile(Rc<File>),
    WriteFile(Rc<File>),
    ReadWriteFile(Rc<File>),
    HereDocument(String),
}

pub enum ReadFile {
    Stdin,
    File(Rc<File>),
}

impl Read for ReadFile {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            ReadFile::Stdin => std::io::stdin().lock().read(buf),
            ReadFile::File(file) => {
                nix::unistd::read(file.as_raw_fd(), buf).map_err(|err| err.into())
            }
        }
    }
}

pub enum WriteFile {
    Stdout,
    Stderr,
    File(Rc<File>),
}

impl Write for WriteFile {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            WriteFile::Stdout => std::io::stdout().lock().write(buf),
            WriteFile::Stderr => std::io::stderr().lock().write(buf),
            WriteFile::File(file) => nix::unistd::write(file, buf).map_err(|err| err.into()),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            WriteFile::Stdout => std::io::stdout().lock().flush(),
            WriteFile::Stderr => std::io::stderr().lock().flush(),
            WriteFile::File(_) => Ok(()),
        }
    }
}

impl WriteFile {
    pub fn write_str<S: AsRef<str>>(&mut self, s: S) {
        match self.write_all(s.as_ref().as_bytes()) {
            Ok(_) => {}
            Err(err) => {
                eprintln!("sh: {}", err);
            }
        }
    }
}

fn io_err_to_redirection_err(err: std::io::Error) -> CommandExecutionError {
    CommandExecutionError::RedirectionError(format!("sh: io error ({})", err))
}

#[derive(Clone)]
pub struct OpenedFiles {
    pub opened_files: HashMap<u32, OpenedFile>,
}

impl OpenedFiles {
    fn io_redirect(
        &mut self,
        kind: &IORedirectionKind,
        target: &str,
        file_descriptor: Option<u32>,
        shell: &Shell,
    ) -> RedirectionResult {
        match kind {
            IORedirectionKind::RedirectOutput
            | IORedirectionKind::RedirectOutputClobber
            | IORedirectionKind::RedirectOuputAppend => {
                if *kind != IORedirectionKind::RedirectOutputClobber
                    && shell.set_options.noclobber
                    && Path::new(target).exists()
                {
                    return Err(CommandExecutionError::RedirectionError(format!(
                        "sh: redirection would overwrite existing file {target}",
                    )));
                }

                let append = *kind == IORedirectionKind::RedirectOuputAppend;
                let file = File::options()
                    .write(true)
                    .truncate(!append)
                    .append(append)
                    .create(true)
                    .open(target)
                    .map_err(io_err_to_redirection_err)?;

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
                if target == "-" {
                    self.opened_files.remove(&dest_fd);
                } else {
                    let duplicate_input = *kind == IORedirectionKind::DuplicateInput;
                    let source_fd = target.parse::<u32>().map_err(|_| {
                        CommandExecutionError::RedirectionError(format!(
                            "sh: invalid file descriptor {target}"
                        ))
                    })?;
                    match self.opened_files.get(&source_fd) {
                        Some(OpenedFile::WriteFile(_))
                        | Some(OpenedFile::Stdout)
                        | Some(OpenedFile::Stderr)
                            if duplicate_input =>
                        {
                            return Err(CommandExecutionError::RedirectionError(format!(
                                "sh: '{source_fd}' is not opened for reading"
                            )));
                        }
                        Some(OpenedFile::ReadFile(_))
                        | Some(OpenedFile::Stdin)
                        | Some(OpenedFile::HereDocument(_))
                            if !duplicate_input =>
                        {
                            return Err(CommandExecutionError::RedirectionError(format!(
                                "sh: '{source_fd}' is not opened for writing"
                            )));
                        }
                        None => {
                            return Err(CommandExecutionError::RedirectionError(format!(
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
                    .read(true)
                    .open(target)
                    .map_err(io_err_to_redirection_err)?;
                let source_fd = file_descriptor.unwrap_or(STDIN_FILENO);
                self.opened_files
                    .insert(source_fd, OpenedFile::ReadFile(Rc::new(file)));
            }
            IORedirectionKind::OpenRW => {
                let file = File::options()
                    .read(true)
                    .write(true)
                    .create(true)
                    .open(target)
                    .map_err(io_err_to_redirection_err)?;
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
                    let file = expand_word_to_string(file, false, shell)?;
                    self.io_redirect(kind, &file, redir.file_descriptor, shell)?;
                }
                RedirectionKind::HereDocument(contents) => {
                    let contents = expand_word_to_string(contents, false, shell)?;
                    self.opened_files.insert(
                        redir.file_descriptor.unwrap_or(STDIN_FILENO),
                        OpenedFile::HereDocument(contents),
                    );
                }
                RedirectionKind::QuotedHereDocument(contents) => {
                    self.opened_files.insert(
                        redir.file_descriptor.unwrap_or(STDIN_FILENO),
                        OpenedFile::HereDocument(contents.clone()),
                    );
                }
            }
        }
        Ok(())
    }

    fn write_file(&self, fileno: u32) -> WriteFile {
        match self.opened_files.get(&fileno) {
            Some(OpenedFile::Stdout) => WriteFile::Stdout,
            Some(OpenedFile::Stderr) => WriteFile::Stderr,
            Some(OpenedFile::WriteFile(file)) | Some(OpenedFile::ReadWriteFile(file)) => {
                WriteFile::File(file.clone())
            }
            _ => unreachable!(),
        }
    }

    pub fn stdout(&self) -> WriteFile {
        self.write_file(STDOUT_FILENO)
    }

    pub fn stderr(&self) -> WriteFile {
        self.write_file(STDERR_FILENO)
    }
}

impl Default for OpenedFiles {
    fn default() -> Self {
        let opened_files = HashMap::from([
            (STDIN_FILENO, OpenedFile::Stdin),
            (STDOUT_FILENO, OpenedFile::Stdout),
            (STDERR_FILENO, OpenedFile::Stderr),
        ]);
        Self { opened_files }
    }
}
