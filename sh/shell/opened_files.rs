//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::os::write;
use crate::parse::command::{IORedirectionKind, Redirection, RedirectionKind};
use crate::shell::{CommandExecutionError, Shell};
use crate::wordexp::expand_word_to_string;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::os::fd::AsRawFd;
use std::os::unix::fs::OpenOptionsExt;
use std::path::Path;
use std::rc::Rc;

pub const STDIN_FILENO: u32 = libc::STDIN_FILENO as u32;
pub const STDOUT_FILENO: u32 = libc::STDOUT_FILENO as u32;
pub const STDERR_FILENO: u32 = libc::STDERR_FILENO as u32;

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
                    .mode(shell.umask)
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
                    .mode(shell.umask)
                    .read(true)
                    .open(target)
                    .map_err(io_err_to_redirection_err)?;
                let source_fd = file_descriptor.unwrap_or(STDIN_FILENO);
                self.opened_files
                    .insert(source_fd, OpenedFile::ReadFile(Rc::new(file)));
            }
            IORedirectionKind::OpenRW => {
                let file = File::options()
                    .mode(shell.umask)
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
                    let file = expand_word_to_string(&file.word, false, shell)?;
                    self.io_redirect(kind, &file, redir.file_descriptor, shell)?;
                }
                RedirectionKind::HereDocument { contents, .. } => {
                    let contents = expand_word_to_string(&contents.word, false, shell)?;
                    self.opened_files.insert(
                        redir.file_descriptor.unwrap_or(STDIN_FILENO),
                        OpenedFile::HereDocument(contents),
                    );
                }
                RedirectionKind::QuotedHereDocument { contents, .. } => {
                    self.opened_files.insert(
                        redir.file_descriptor.unwrap_or(STDIN_FILENO),
                        OpenedFile::HereDocument(contents.clone()),
                    );
                }
            }
        }
        Ok(())
    }

    fn write_file(&self, fileno: u32, contents: &str) {
        let result = match self.opened_files.get(&fileno) {
            Some(OpenedFile::Stdout) => std::io::stdout().write_all(contents.as_bytes()),
            Some(OpenedFile::Stderr) => std::io::stderr().write_all(contents.as_bytes()),
            Some(OpenedFile::WriteFile(file)) | Some(OpenedFile::ReadWriteFile(file)) => {
                write(file.as_raw_fd(), contents.as_bytes())
                    .map_err(|_| std::io::Error::last_os_error())
                    .map(|_| ())
            }
            _ => unreachable!(),
        };
        match result {
            Ok(_) => {}
            Err(err) => {
                eprintln!("sh: io error ({})", err);
            }
        }
    }

    pub fn write_out<S: AsRef<str>>(&self, string: S) {
        self.write_file(STDOUT_FILENO, string.as_ref());
    }

    pub fn write_err<S: AsRef<str>>(&self, string: S) {
        self.write_file(STDERR_FILENO, string.as_ref());
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
        Self { opened_files }
    }
}
