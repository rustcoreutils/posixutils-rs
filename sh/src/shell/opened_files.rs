use crate::parse::command::{IORedirectionKind, Redirection, RedirectionKind};
use crate::shell::Shell;
use crate::wordexp::expand_word_to_string;
use nix::libc;
use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
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
}

pub enum ReadFile {
    Stdin(std::io::StdinLock<'static>),
    File(Rc<File>),
}

impl AsRef<dyn Read> for ReadFile {
    fn as_ref(&self) -> &(dyn Read + 'static) {
        match self {
            ReadFile::Stdin(stdin) => stdin,
            ReadFile::File(file) => file.as_ref(),
        }
    }
}

pub enum WriteFile {
    Stdout(std::io::StdoutLock<'static>),
    Stderr(std::io::StderrLock<'static>),
    File(Rc<File>),
}

impl AsRef<dyn Write> for WriteFile {
    fn as_ref(&self) -> &(dyn Write + 'static) {
        match self {
            WriteFile::Stdout(stdout) => stdout,
            WriteFile::Stderr(stderr) => stderr,
            WriteFile::File(file) => file.as_ref(),
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
                    // > the word that follows the redirection operator shall be subjected to tilde
                    // > expansion, parameter expansion, command substitution, arithmetic expansion,
                    // > and quote removal.
                    let path = expand_word_to_string(file, false, shell);
                    // TODO: pathname expansion is not allowed if the shell is non-interactive,
                    // optional otherwise. Bash does implement this, maybe we should too.
                    match kind {
                        IORedirectionKind::RedirectOutput
                        | IORedirectionKind::RedirectOutputClobber
                        | IORedirectionKind::RedirectOuputAppend => {
                            // TODO: fail if noclobber is set and file exists and is a regular file

                            // TODO: fix unwrap
                            let file = if *kind == IORedirectionKind::RedirectOuputAppend {
                                std::fs::OpenOptions::new()
                                    .append(true)
                                    .create(true)
                                    .open(path)
                                    .unwrap()
                            } else {
                                File::create(path).unwrap()
                            };
                            let source_fd =
                                redir.file_descriptor.unwrap_or(libc::STDOUT_FILENO as u32);
                            self.opened_files
                                .insert(source_fd, OpenedFile::File(Rc::new(file)));
                        }
                        IORedirectionKind::DuplicateOutput => {}
                        IORedirectionKind::RedirectInput => {}
                        IORedirectionKind::DuplicateInput => {}
                        IORedirectionKind::OpenRW => {}
                    }
                }
                RedirectionKind::HereDocument { contents } => {}
            }
        }
    }

    pub fn stdin(&self) -> ReadFile {
        match self.opened_files.get(&STDIN_FILENO) {
            Some(OpenedFile::Stdin) => ReadFile::Stdin(std::io::stdin().lock()),
            Some(OpenedFile::File(file)) => ReadFile::File(file.clone()),
            _ => todo!(),
        }
    }

    fn write_file(&self, fileno: u32) -> WriteFile {
        match self.opened_files.get(&fileno) {
            Some(OpenedFile::Stdout) => WriteFile::Stdout(std::io::stdout().lock()),
            Some(OpenedFile::Stderr) => WriteFile::Stderr(std::io::stderr().lock()),
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
