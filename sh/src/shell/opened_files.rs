use crate::parse::command::{IORedirectionKind, Redirection, RedirectionKind};
use crate::shell::Shell;
use crate::wordexp::expand_word_to_string;
use nix::libc;
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::os::fd::AsRawFd;
use std::rc::Rc;

const STDIN_FILENO: u32 = libc::STDIN_FILENO as u32;
const STDOUT_FILENO: u32 = libc::STDOUT_FILENO as u32;
const STDERR_FILENO: u32 = libc::STDERR_FILENO as u32;

#[derive(Clone)]
pub enum OpenedFile {
    Stdin,
    Stdout,
    Stderr,
    File(Rc<File>),
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

#[derive(Clone)]
pub struct OpenedFiles {
    pub opened_files: HashMap<u32, OpenedFile>,
}

impl OpenedFiles {
    pub fn redirect(&mut self, redirections: &[Redirection], shell: &mut Shell) {
        for redir in redirections {
            match &redir.kind {
                RedirectionKind::IORedirection { kind, file } => {
                    let target = expand_word_to_string(file, false, shell);
                    // TODO: pathname expansion is not allowed if the shell is non-interactive,
                    // optional otherwise. Bash does implement this, maybe we should too.
                    match kind {
                        IORedirectionKind::RedirectOutput
                        | IORedirectionKind::RedirectOutputClobber
                        | IORedirectionKind::RedirectOuputAppend => {
                            // TODO: fail if noclobber is set and file exists and is a regular file

                            // TODO: fix unwrap
                            let file = if *kind == IORedirectionKind::RedirectOuputAppend {
                                File::options()
                                    .append(true)
                                    .create(true)
                                    .open(target)
                                    .unwrap()
                            } else {
                                File::create(target).unwrap()
                            };
                            let source_fd = redir.file_descriptor.unwrap_or(STDOUT_FILENO);
                            self.opened_files
                                .insert(source_fd, OpenedFile::File(Rc::new(file)));
                        }
                        IORedirectionKind::DuplicateOutput | IORedirectionKind::DuplicateInput => {
                            let dest_fd = if *kind == IORedirectionKind::DuplicateOutput {
                                redir.file_descriptor.unwrap_or(STDOUT_FILENO)
                            } else {
                                redir.file_descriptor.unwrap_or(STDIN_FILENO)
                            };
                            if target == "-" {
                                self.opened_files.remove(&dest_fd);
                            } else {
                                // TODO: handle errors
                                let source_fd = target.parse::<u32>().unwrap();
                                let file = self.opened_files.get(&source_fd).unwrap().clone();
                                self.opened_files.insert(dest_fd, file);
                            }
                        }
                        IORedirectionKind::RedirectInput => {
                            let file = File::options().read(true).open(target).unwrap();
                            let source_fd = redir.file_descriptor.unwrap_or(STDIN_FILENO);
                            self.opened_files
                                .insert(source_fd, OpenedFile::File(Rc::new(file)));
                        }
                        IORedirectionKind::OpenRW => {
                            let file = File::options()
                                .read(true)
                                .write(true)
                                .create(true)
                                .open(target)
                                .unwrap();
                            let source_fd = redir.file_descriptor.unwrap_or(STDIN_FILENO);
                            self.opened_files
                                .insert(source_fd, OpenedFile::File(Rc::from(file)));
                        }
                    }
                }
                RedirectionKind::HereDocument(contents) => {
                    let contents = expand_word_to_string(contents, false, shell);
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
    }

    pub fn stdin(&self) -> ReadFile {
        match self.opened_files.get(&STDIN_FILENO) {
            Some(OpenedFile::Stdin) => ReadFile::Stdin,
            Some(OpenedFile::File(file)) => ReadFile::File(file.clone()),
            _ => todo!(),
        }
    }

    fn write_file(&self, fileno: u32) -> WriteFile {
        match self.opened_files.get(&fileno) {
            Some(OpenedFile::Stdout) => WriteFile::Stdout,
            Some(OpenedFile::Stderr) => WriteFile::Stderr,
            Some(OpenedFile::File(file)) => WriteFile::File(file.clone()),
            _ => todo!(),
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
        let mut opened_files = HashMap::from([
            (STDIN_FILENO, OpenedFile::Stdin),
            (STDOUT_FILENO, OpenedFile::Stdout),
            (STDERR_FILENO, OpenedFile::Stderr),
        ]);
        Self { opened_files }
    }
}
