//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::convert::Infallible;
use std::ffi::{CStr, CString, OsString};
use std::fmt::{Display, Formatter};
use std::io;
use std::os::fd::{AsFd, AsRawFd, OwnedFd, RawFd};
use std::os::unix::ffi::OsStringExt;
use std::path::PathBuf;

use nix::errno::Errno;
use nix::libc;
use nix::sys::signal::Signal as NixSignal;
use nix::sys::wait::{WaitPidFlag, WaitStatus};
use nix::unistd::{execve, tcgetpgrp, ForkResult, Pid};

use crate::shell::environment::Environment;
use crate::shell::opened_files::{OpenedFile, OpenedFiles};

pub const DEFAULT_PATH: &str = "/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin:.";

pub fn strcoll(lhs: &CStr, rhs: &CStr) -> std::cmp::Ordering {
    // strings are valid, this is safe
    let ordering = unsafe { libc::strcoll(lhs.as_ptr(), rhs.as_ptr()) };
    ordering.cmp(&0)
}

#[derive(Clone, Debug)]
pub struct OsError {
    pub command: &'static str,
    pub errno: Errno,
}

impl OsError {
    pub fn new(command: &'static str, errno: Errno) -> Self {
        Self { command, errno }
    }
}

impl Display for OsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "sh: internal call to {} failed ({})",
            self.command, self.errno
        )
    }
}

pub type OsResult<T> = Result<T, OsError>;

pub fn fork() -> OsResult<ForkResult> {
    // fork in general is not safe for multithreaded programs, but all code in this module is single
    // threaded, so this is safe
    unsafe { nix::unistd::fork().map_err(|err| OsError::new("fork", err)) }
}

pub fn pipe() -> OsResult<(OwnedFd, OwnedFd)> {
    nix::unistd::pipe().map_err(|err| OsError::new("pipe", err))
}

pub fn dup2(old_fd: RawFd, new_fd: RawFd) -> OsResult<RawFd> {
    nix::unistd::dup2(old_fd, new_fd).map_err(|err| OsError::new("dup2", err))
}

pub fn waitpid(pid: Pid, options: Option<WaitPidFlag>) -> OsResult<WaitStatus> {
    nix::sys::wait::waitpid(pid, options).map_err(|err| OsError::new("waitpid", err))
}

pub fn close(fd: RawFd) -> OsResult<()> {
    nix::unistd::close(fd).map_err(|err| OsError::new("close", err))
}

pub fn find_in_path(command: &str, env_path: &str) -> Option<OsString> {
    for path in env_path.split(':') {
        let mut command_path = PathBuf::from(path);
        command_path.push(command);
        if command_path.is_file() {
            return Some(command_path.into_os_string());
        }
    }
    None
}

pub fn find_command(command: &str, env_path: &str) -> Option<OsString> {
    if command.contains('/') {
        let path = PathBuf::from(command);
        if path.exists() {
            Some(path.into_os_string())
        } else {
            None
        }
    } else {
        find_in_path(command, env_path)
    }
}

pub enum ExecError {
    OsError(OsError),
    CannotExecute(Errno),
}

impl From<OsError> for ExecError {
    fn from(value: OsError) -> Self {
        Self::OsError(value)
    }
}

pub fn exec(
    command: OsString,
    args: &[String],
    opened_files: &OpenedFiles,
    env: &Environment,
) -> Result<Infallible, ExecError> {
    for (id, file) in &opened_files.opened_files {
        let dest = *id as i32;
        let src = match file {
            OpenedFile::Stdin => libc::STDIN_FILENO,
            OpenedFile::Stdout => libc::STDOUT_FILENO,
            OpenedFile::Stderr => libc::STDERR_FILENO,
            OpenedFile::ReadFile(file)
            | OpenedFile::WriteFile(file)
            | OpenedFile::ReadWriteFile(file) => file.as_raw_fd(),
            OpenedFile::HereDocument(contents) => {
                let (read_pipe, write_pipe) = pipe()?;
                nix::unistd::write(write_pipe, contents.as_bytes())
                    .map_err(|err| OsError::new("write", err))?;
                dup2(read_pipe.as_raw_fd(), dest)?;
                continue;
            }
        };
        dup2(src, dest)?;
    }
    let command = CString::new(command.into_vec()).unwrap();
    let args = args
        .iter()
        .map(|s| CString::new(s.as_str()).unwrap())
        .collect::<Vec<_>>();
    let env = env
        .exported()
        .map(|(name, value)| CString::new(format!("{name}={value}")).unwrap())
        .collect::<Vec<CString>>();
    // unwrap is safe here, because execve will only return if it fails
    let err = execve(&command, &args, &env).unwrap_err();
    Err(ExecError::CannotExecute(err))
}

pub fn is_process_in_foreground() -> bool {
    if let Ok(pgid) = tcgetpgrp(io::stdin().as_fd()) {
        pgid == nix::unistd::getpgrp()
    } else {
        false
    }
}

pub fn signal_to_exit_status(signal: NixSignal) -> i32 {
    128 + signal as i32
}
