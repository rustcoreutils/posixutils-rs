//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::os::errno::{get_current_errno_value, Errno};
use crate::os::signals::TermSignal;
use crate::shell::environment::Environment;
use crate::shell::opened_files::{OpenedFile, OpenedFiles};
use std::convert::Infallible;
use std::ffi::{CString, OsStr, OsString};
use std::fmt::{Display, Formatter};
use std::io;
use std::os::fd::{AsRawFd, FromRawFd, IntoRawFd, OwnedFd, RawFd};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::PathBuf;

pub mod errno;
pub mod signals;

pub const DEFAULT_PATH: &str = "/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin:.";

pub type Pid = libc::pid_t;

#[derive(Clone, Debug)]
pub struct OsError {
    pub command: &'static str,
    pub errno: Errno,
}

impl OsError {
    pub fn from_current_errno(command: &'static str) -> Self {
        Self {
            command,
            errno: get_current_errno_value(),
        }
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
pub type LibcResult<T> = Result<T, Errno>;

pub fn getpgrp() -> Pid {
    // always successful
    unsafe { libc::getpgrp() }
}

pub fn write(fd: RawFd, bytes: &[u8]) -> OsResult<usize> {
    let bytes_written = unsafe {
        libc::write(
            fd as libc::c_int,
            bytes.as_ptr() as *const libc::c_void,
            bytes.len(),
        )
    };
    if bytes_written < 0 {
        return Err(OsError::from_current_errno("write"));
    }
    Ok(bytes_written as usize)
}

pub fn write_all(fd: RawFd, mut bytes: &[u8]) -> OsResult<()> {
    while !bytes.is_empty() {
        let written = write(fd, bytes)?;
        if written == 0 {
            return Err(OsError {
                command: "write",
                errno: Errno::EIO,
            });
        }
        bytes = &bytes[written..];
    }
    Ok(())
}

pub fn read(fd: RawFd, buf: &mut [u8]) -> OsResult<usize> {
    let bytes_read = unsafe { libc::read(fd, buf.as_ptr() as *mut libc::c_void, buf.len()) };
    if bytes_read < 0 {
        return Err(OsError::from_current_errno("read"));
    }
    Ok(bytes_read as usize)
}

pub enum ForkResult {
    Child,
    Parent { child: Pid },
}

#[allow(clippy::comparison_chain)]
pub fn fork() -> OsResult<ForkResult> {
    // fork in general is not safe for multithreaded programs, but all code in this module is single
    // threaded, so this is safe
    let fork_result = unsafe { libc::fork() };
    if fork_result < 0 {
        Err(OsError::from_current_errno("fork"))
    } else if fork_result == 0 {
        Ok(ForkResult::Child)
    } else {
        Ok(ForkResult::Parent { child: fork_result })
    }
}

pub fn pipe() -> OsResult<(OwnedFd, OwnedFd)> {
    let mut descriptors = [libc::c_int::default(); 2];
    let pipe_result = unsafe { libc::pipe(descriptors.as_mut_ptr()) };
    if pipe_result < 0 {
        return Err(OsError::from_current_errno("pipe"));
    }
    let fd0 = unsafe { OwnedFd::from_raw_fd(descriptors[0]) };
    let fd1 = unsafe { OwnedFd::from_raw_fd(descriptors[1]) };
    Ok((fd0, fd1))
}

pub fn dup(fd: RawFd) -> OsResult<RawFd> {
    let dup_result = unsafe { libc::dup(fd) };
    if dup_result < 0 {
        return Err(OsError::from_current_errno("dup"));
    }
    Ok(dup_result)
}

pub fn dup2(old_fd: RawFd, new_fd: RawFd) -> OsResult<RawFd> {
    let dup_result = unsafe { libc::dup2(old_fd, new_fd) };
    if dup_result < 0 {
        return Err(OsError::from_current_errno("dup2"));
    }
    Ok(dup_result)
}

pub enum WaitStatus {
    Exited { exit_status: libc::c_int },
    Signaled { signal: TermSignal },
    Stopped { signal: TermSignal },
    StillAlive,
}

pub fn waitpid(pid: Pid, no_hang: bool, untraced: bool) -> OsResult<WaitStatus> {
    let mut status = 0;
    let mut options = 0;
    if no_hang {
        options |= libc::WNOHANG;
    }
    if untraced {
        options |= libc::WUNTRACED;
    }
    let wait_result = unsafe { libc::waitpid(pid, &mut status, options) };
    if wait_result < 0 {
        Err(OsError::from_current_errno("waitpid"))
    } else if wait_result == 0 && no_hang {
        Ok(WaitStatus::StillAlive)
    } else if libc::WIFEXITED(status) {
        let exit_status = libc::WEXITSTATUS(status);
        Ok(WaitStatus::Exited { exit_status })
    } else if libc::WIFSIGNALED(status) {
        Ok(WaitStatus::Signaled {
            signal: TermSignal(libc::WTERMSIG(status)),
        })
    } else if libc::WIFSTOPPED(status) {
        Ok(WaitStatus::Stopped {
            signal: TermSignal(libc::WSTOPSIG(status)),
        })
    } else {
        // WIFCONTINUED, or a status this shell does not ask for; the child is
        // still around, so report it as such rather than aborting.
        Ok(WaitStatus::StillAlive)
    }
}

pub fn close(fd: RawFd) -> OsResult<()> {
    let close_result = unsafe { libc::close(fd) };
    if close_result < 0 {
        return Err(OsError::from_current_errno("close"));
    }
    Ok(())
}

/// Materializes a here-document body as a readable descriptor.
///
/// A pipe cannot be used: nothing reads it until the command is exec'd, so a
/// body larger than the pipe buffer would block the writer forever. An
/// immediately-unlinked temporary file has no such limit and, unlike a pipe,
/// is seekable, which is what a redirection from a here-document behaves like.
pub fn here_document_fd(contents: &[u8]) -> OsResult<OwnedFd> {
    let dir = std::env::var_os("TMPDIR").unwrap_or_else(|| OsString::from("/tmp"));
    let mut template = dir.into_vec();
    if template.last() != Some(&b'/') {
        template.push(b'/');
    }
    template.extend_from_slice(b"sh-heredoc-XXXXXX\0");

    // mkstemp replaces the trailing Xs in place and creates the file with mode
    // 0600, so the body is never visible to other users.
    let fd = unsafe { libc::mkstemp(template.as_mut_ptr() as *mut libc::c_char) };
    if fd < 0 {
        return Err(OsError::from_current_errno("mkstemp"));
    }
    let fd = unsafe { OwnedFd::from_raw_fd(fd) };
    // Unlink right away: the descriptor keeps the contents alive, and nothing
    // is left behind even if the shell dies before the command finishes.
    if unsafe { libc::unlink(template.as_ptr() as *const libc::c_char) } < 0 {
        return Err(OsError::from_current_errno("unlink"));
    }
    write_all(fd.as_raw_fd(), contents)?;
    if unsafe { libc::lseek(fd.as_raw_fd(), 0, libc::SEEK_SET) } < 0 {
        return Err(OsError::from_current_errno("lseek"));
    }
    Ok(fd)
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
                let here_document = here_document_fd(contents.borrow().as_bytes())?.into_raw_fd();
                // mkstemp may well hand back the descriptor the redirection
                // targets (e.g. `exec 3<<EOF` with fd 3 free), and closing it
                // after a no-op dup2 would leave the target closed.
                if here_document != dest {
                    dup2(here_document, dest)?;
                    close(here_document)?;
                }
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
    let mut args_ptr_vec = args.iter().map(|s| s.as_ptr()).collect::<Vec<_>>();
    args_ptr_vec.push(std::ptr::null());
    let env = env
        .exported()
        .map(|(name, value)| CString::new(format!("{name}={value}")).unwrap())
        .collect::<Vec<CString>>();
    let mut env_ptr_vec = env.iter().map(|s| s.as_ptr()).collect::<Vec<_>>();
    env_ptr_vec.push(std::ptr::null());
    // execve only returns on failure
    unsafe {
        libc::execve(
            command.as_ptr(),
            args_ptr_vec.as_ptr(),
            env_ptr_vec.as_ptr(),
        )
    };
    Err(ExecError::CannotExecute(get_current_errno_value()))
}

pub fn tcgetpgrp(fd: RawFd) -> OsResult<Pid> {
    let group_id = unsafe { libc::tcgetpgrp(fd) };
    if group_id < 0 {
        return Err(OsError::from_current_errno("tcgetpgrp"));
    }
    Ok(group_id)
}

pub fn tcsetpgrp(fd: RawFd, pgid: Pid) -> OsResult<()> {
    let result = unsafe { libc::tcsetpgrp(fd, pgid) };
    if result < 0 {
        return Err(OsError::from_current_errno("tcsetpgrp"));
    }
    Ok(())
}

pub fn is_process_in_foreground() -> bool {
    if let Ok(pgid) = tcgetpgrp(io::stdin().as_raw_fd()) {
        pgid == getpgrp()
    } else {
        false
    }
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

pub fn setpgid(pid: Pid, pgid: Pid) -> OsResult<()> {
    let result = unsafe { libc::setpgid(pid, pgid) };
    if result < 0 {
        return Err(OsError::from_current_errno("setpgid"));
    }
    Ok(())
}

pub fn getpgid(pid: Pid) -> OsResult<Pid> {
    let pid = unsafe { libc::getpgid(pid) };
    if pid < 0 {
        return Err(OsError::from_current_errno("getpgid"));
    }
    Ok(pid)
}

pub fn chdir(path: &OsStr) -> LibcResult<()> {
    let path = CString::new(path.as_bytes()).expect("path contains null characters");
    let result = unsafe { libc::chdir(path.as_ptr()) };
    if result < 0 {
        return Err(get_current_errno_value());
    }
    Ok(())
}

pub fn mkstemp(template: &str) -> LibcResult<(RawFd, PathBuf)> {
    let template_cstr =
        CString::new(template).expect("template for mkstemp contained a null character");
    let mut template_cstr = template_cstr.into_bytes_with_nul();
    let fd = unsafe { libc::mkstemp(template_cstr.as_mut_ptr() as *mut libc::c_char) };
    if fd < 0 {
        return Err(get_current_errno_value());
    }
    // remove null terminator
    template_cstr.pop();
    Ok((fd, PathBuf::from(OsString::from_vec(template_cstr))))
}
