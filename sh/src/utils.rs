use nix::libc;
use nix::sys::wait::WaitStatus;
use nix::unistd::{ForkResult, Pid};
use std::ffi::{CStr, OsString};
use std::fmt::{Display, Formatter};
use std::os::fd::{OwnedFd, RawFd};
use std::path::PathBuf;

pub fn strcoll(lhs: &CStr, rhs: &CStr) -> std::cmp::Ordering {
    // strings are valid, this is safe
    let ordering = unsafe { libc::strcoll(lhs.as_ptr(), rhs.as_ptr()) };
    if ordering < 0 {
        std::cmp::Ordering::Less
    } else if ordering == 0 {
        std::cmp::Ordering::Equal
    } else {
        std::cmp::Ordering::Greater
    }
}

#[derive(Clone, Debug)]
pub struct OsError(String);

impl From<String> for OsError {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl Display for OsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_str())
    }
}

pub type OsResult<T> = Result<T, OsError>;

pub fn fork() -> OsResult<ForkResult> {
    // fork in general is not safe for multithreaded programs, but all code in this module is single
    // threaded, so this is safe
    unsafe {
        nix::unistd::fork()
            .map_err(|err| format!("sh: internal call to fork failed ({err})").into())
    }
}

pub fn pipe() -> OsResult<(OwnedFd, OwnedFd)> {
    nix::unistd::pipe().map_err(|err| format!("sh: internal call to pipe failed ({err})").into())
}

pub fn dup2(old_fd: RawFd, new_fd: RawFd) -> OsResult<RawFd> {
    nix::unistd::dup2(old_fd, new_fd)
        .map_err(|err| format!("sh: internal call to dup2 failed ({err})").into())
}

pub fn waitpid(pid: Pid) -> OsResult<WaitStatus> {
    nix::sys::wait::waitpid(pid, None)
        .map_err(|err| format!("sh: internal call to waitpid failed ({err})").into())
}

pub fn close(fd: RawFd) -> OsResult<()> {
    nix::unistd::close(fd)
        .map_err(|err| format!("sh: internal call to close failed ({err})").into())
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
