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
use crate::shstr::ShString;
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
/// Sets the process file-mode creation mask and returns the previous one.
/// `umask(2)` cannot fail.
pub fn umask(mask: u32) -> u32 {
    unsafe { libc::umask(mask as libc::mode_t) as u32 }
}

/// Reads the process file-mode creation mask without changing it. There is no
/// query form of `umask(2)`, so it has to be set and put back.
pub fn get_umask() -> u32 {
    let previous = umask(0o022);
    umask(previous);
    previous
}

/// Blocks until `fd` has input available or a signal arrives, whichever comes
/// first. Replaces polling with a sleep: an idle shell should not wake up at
/// all, and a signal must not have to wait out a tick to be seen.
pub fn wait_for_input(fd: RawFd) -> OsResult<()> {
    let signal_fd = crate::os::signals::signal_read_fd();
    let mut fds = [
        libc::pollfd {
            fd,
            events: libc::POLLIN,
            revents: 0,
        },
        libc::pollfd {
            fd: signal_fd,
            events: libc::POLLIN,
            revents: 0,
        },
    ];
    let result = unsafe { libc::poll(fds.as_mut_ptr(), 2, -1) };
    if result < 0 {
        let errno = get_current_errno_value();
        // A signal without SA_RESTART interrupts the poll; that is the wake-up
        // it was waiting for, not a failure.
        if errno != Errno::EINTR {
            return Err(OsError {
                command: "poll",
                errno,
            });
        }
    }
    Ok(())
}

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

/// `dup` that marks the copy close-on-exec, so it is not inherited by the
/// utilities the shell execs.
pub fn dup_cloexec(fd: RawFd) -> OsResult<OwnedFd> {
    let dup_result = unsafe { libc::fcntl(fd, libc::F_DUPFD_CLOEXEC, 0) };
    if dup_result < 0 {
        return Err(OsError::from_current_errno("fcntl"));
    }
    Ok(unsafe { OwnedFd::from_raw_fd(dup_result) })
}

pub fn dup2(old_fd: RawFd, new_fd: RawFd) -> OsResult<RawFd> {
    let dup_result = unsafe { libc::dup2(old_fd, new_fd) };
    if dup_result < 0 {
        return Err(OsError::from_current_errno("dup2"));
    }
    Ok(dup_result)
}

pub enum WaitStatus {
    /// `waitpid` returned EINTR: no status yet, but a signal is pending.
    Interrupted,
    Exited {
        exit_status: libc::c_int,
    },
    Signaled {
        signal: TermSignal,
    },
    Stopped {
        signal: TermSignal,
    },
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
        let errno = get_current_errno_value();
        if errno == Errno::EINTR {
            // A signal arrived while blocked. The handler has recorded it on
            // the signal pipe; report it so the caller can run any trap and
            // wait again, rather than treating it as a failure.
            return Ok(WaitStatus::Interrupted);
        }
        Err(OsError {
            command: "waitpid",
            errno,
        })
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
    /// A command name, argument or environment entry contained a NUL, which
    /// `execve` cannot carry.
    InteriorNul,
}

impl From<OsError> for ExecError {
    fn from(value: OsError) -> Self {
        Self::OsError(value)
    }
}

/// Duplicates `fd` onto a descriptor strictly greater than `floor`, so that it
/// cannot collide with any redirection destination.
fn dup_above(fd: RawFd, floor: RawFd) -> OsResult<RawFd> {
    let new_fd = unsafe { libc::fcntl(fd, libc::F_DUPFD_CLOEXEC, floor + 1) };
    if new_fd < 0 {
        return Err(OsError {
            command: "fcntl",
            errno: get_current_errno_value(),
        });
    }
    Ok(new_fd)
}

pub fn exec(
    command: OsString,
    args: &[ShString],
    opened_files: &OpenedFiles,
    env: &Environment,
) -> Result<Infallible, ExecError> {
    // A source descriptor may itself be another redirection's destination
    // (`3>&1 1>&2 2>&3`), and the map is iterated in an arbitrary order, so
    // placing them one at a time can clobber a source before it is read. Move
    // every source out of the way first, above every destination, then put them
    // where they belong.
    let highest_dest = opened_files
        .opened_files
        .keys()
        .map(|id| *id as i32)
        .max()
        .unwrap_or(libc::STDERR_FILENO);
    let mut staged: Vec<(RawFd, RawFd)> = Vec::with_capacity(opened_files.opened_files.len());
    let mut to_close: Vec<RawFd> = Vec::new();
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
                here_document_fd(contents.borrow().as_bytes())?.into_raw_fd()
            }
            OpenedFile::Closed => {
                // `2>&-`: the descriptor must actually be closed, not merely
                // dropped from the table and inherited. Deferred until every
                // other redirection is in place: closing fd 5 for `5<&-` frees
                // it, so a later `<file` in the same command may well have been
                // handed fd 5 by the kernel, and closing it here would take
                // that descriptor with it.
                to_close.push(dest);
                continue;
            }
        };
        staged.push((dup_above(src, highest_dest)?, dest));
    }
    for (src, dest) in staged {
        dup2(src, dest)?;
        close(src)?;
    }
    for fd in to_close {
        let _ = close(fd);
    }
    // An interior NUL cannot be passed through execve. A shell value may
    // contain one, so this is an error to report, not a reason to abort in the
    // child after the fork.
    let command = CString::new(command.into_vec()).map_err(|_| ExecError::InteriorNul)?;
    let args = args
        .iter()
        .map(|s| s.to_c_string())
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| ExecError::InteriorNul)?;
    let mut args_ptr_vec = args.iter().map(|s| s.as_ptr()).collect::<Vec<_>>();
    args_ptr_vec.push(std::ptr::null());
    // Built by byte concatenation, not `format!`: an environment entry is a
    // value, and a lossy conversion here would corrupt it silently.
    let env = env
        .exported()
        .map(|(name, value)| {
            let mut entry = Vec::with_capacity(name.len() + 1 + value.len());
            entry.extend_from_slice(name.as_bytes());
            entry.push(b'=');
            entry.extend_from_slice(value.as_bytes());
            CString::new(entry).map_err(|_| ExecError::InteriorNul)
        })
        .collect::<Result<Vec<CString>, _>>()?;
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

pub fn find_in_path(command: &std::ffi::OsStr, env_path: &str) -> Option<OsString> {
    for path in env_path.split(':') {
        let mut command_path = PathBuf::from(path);
        command_path.push(command);
        if command_path.is_file() {
            return Some(command_path.into_os_string());
        }
    }
    None
}

pub fn find_command(command: &crate::shstr::ShStr, env_path: &str) -> Option<OsString> {
    if command.contains(&b'/') {
        let path = PathBuf::from(command.as_os_str());
        if path.exists() {
            Some(path.into_os_string())
        } else {
            None
        }
    } else {
        find_in_path(command.as_os_str(), env_path)
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
