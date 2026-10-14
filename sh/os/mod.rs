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
use crate::shstr::ShString;
use std::convert::Infallible;
use std::ffi::{CString, OsStr, OsString};
use std::fmt::{Display, Formatter};
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd, RawFd};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::PathBuf;
use std::sync::atomic::{AtomicI32, Ordering};

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
    // The mask is the most restrictive one available, so that a file created
    // between the two calls -- from a signal handler, say -- is not left more
    // permissive than the caller asked for.
    let previous = umask(0o777);
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
    } else if fds[1].revents != 0 {
        // The signals themselves are in the handler's flags; the byte that
        // woke the poll (perhaps a subshell's) must not wake it again.
        crate::os::signals::drain_signal_pipe();
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

/// Copies `fd` to a descriptor of the shell's own: close-on-exec, and above
/// 9, where no redirection names it unless it asks for that number. Under
/// a descriptor limit that leaves none above 9 (`ulimit -n 10`), the lowest
/// free one is taken instead.
pub fn dup_private(fd: RawFd) -> OsResult<RawFd> {
    let copy = unsafe { libc::fcntl(fd, libc::F_DUPFD_CLOEXEC, 10) };
    if copy >= 0 {
        return Ok(copy);
    }
    let errno = get_current_errno_value();
    if errno != Errno::EINVAL && errno != Errno::EMFILE {
        return Err(OsError {
            command: "fcntl",
            errno,
        });
    }
    let copy = unsafe { libc::fcntl(fd, libc::F_DUPFD_CLOEXEC, 0) };
    if copy < 0 {
        return Err(OsError::from_current_errno("fcntl"));
    }
    Ok(copy)
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
    /// A stopped child was resumed by SIGCONT (only with `continued`).
    Continued,
    StillAlive,
}

/// Which changes of a child's state `waitpid` reports besides termination.
#[derive(Clone, Copy)]
pub struct WaitOptions {
    /// Return at once (`StillAlive`) when the child has not changed state.
    pub no_hang: bool,
    /// Report a child that has stopped (WUNTRACED).
    pub untraced: bool,
    /// Report a stopped child that SIGCONT has resumed (WCONTINUED).
    pub continued: bool,
}

impl WaitOptions {
    /// Blocks until the child terminates, or also until it stops when
    /// `untraced`.
    pub fn blocking(untraced: bool) -> Self {
        Self {
            no_hang: false,
            untraced,
            continued: false,
        }
    }

    /// Polls for every change of state a job's process can go through.
    pub fn poll_all() -> Self {
        Self {
            no_hang: true,
            untraced: true,
            continued: true,
        }
    }
}

pub fn waitpid(pid: Pid, options: WaitOptions) -> OsResult<WaitStatus> {
    let mut status = 0;
    let mut flags = 0;
    if options.no_hang {
        flags |= libc::WNOHANG;
    }
    if options.untraced {
        flags |= libc::WUNTRACED;
    }
    if options.continued {
        flags |= libc::WCONTINUED;
    }
    let wait_result = unsafe { libc::waitpid(pid, &mut status, flags) };
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
    } else if wait_result == 0 && options.no_hang {
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
    } else if libc::WIFCONTINUED(status) {
        Ok(WaitStatus::Continued)
    } else {
        // A status this shell does not know; the child is still around, so
        // report it as such rather than aborting.
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
///
/// The descriptor returned is open only for reading, as a here-document's is
/// in dash (a pipe) and bash: a command that writes to it (`cat 1<<EOF`, or a
/// copy made with `n>&m`) gets EBADF instead of silently writing into the
/// temporary file.
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
    let writer = unsafe { OwnedFd::from_raw_fd(fd) };
    // The reading descriptor is a second open of the same file, made before
    // the name goes away; it has its own offset, still at the start. Where
    // others can rename entries in TMPDIR, the name may no longer be ours
    // by now: never follow a symbolic link, block on a FIFO or take a
    // terminal while finding out, and the identity check below refuses
    // whatever it was.
    let fd = unsafe {
        libc::open(
            template.as_ptr() as *const libc::c_char,
            libc::O_RDONLY | libc::O_CLOEXEC | libc::O_NOFOLLOW | libc::O_NONBLOCK | libc::O_NOCTTY,
        )
    };
    let reader = if fd < 0 {
        Err(OsError::from_current_errno("open"))
    } else {
        Ok(unsafe { OwnedFd::from_raw_fd(fd) })
    };
    // Unlink right away: the descriptors keep the contents alive, and nothing
    // is left behind even if the shell dies before the command finishes.
    if unsafe { libc::unlink(template.as_ptr() as *const libc::c_char) } < 0 {
        return Err(OsError::from_current_errno("unlink"));
    }
    let reader = reader?;
    // The name was reopened, so make sure it still named the file mkstemp
    // created.
    let (writer_stat, reader_stat) = (fstat(writer.as_raw_fd())?, fstat(reader.as_raw_fd())?);
    if (writer_stat.st_dev, writer_stat.st_ino) != (reader_stat.st_dev, reader_stat.st_ino) {
        return Err(OsError {
            command: "open",
            errno: Errno::EEXIST,
        });
    }
    // It is our regular file: the command gets an ordinary blocking
    // descriptor.
    if unsafe { libc::fcntl(reader.as_raw_fd(), libc::F_SETFL, libc::O_RDONLY) } < 0 {
        return Err(OsError::from_current_errno("fcntl"));
    }
    write_all(writer.as_raw_fd(), contents)?;
    Ok(reader)
}

fn fstat(fd: RawFd) -> OsResult<libc::stat> {
    let mut stat = std::mem::MaybeUninit::<libc::stat>::uninit();
    if unsafe { libc::fstat(fd, stat.as_mut_ptr()) } < 0 {
        return Err(OsError::from_current_errno("fstat"));
    }
    Ok(unsafe { stat.assume_init() })
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

/// Replaces the process with `command`. The shell has already put the
/// command's redirections on its own descriptors, and its private ones are
/// close-on-exec, so the command inherits exactly the redirected set.
pub fn exec(
    command: OsString,
    args: &[ShString],
    env: &Environment,
) -> Result<Infallible, ExecError> {
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

/// `tcsetpgrp` with SIGTTOU blocked. A process outside the terminal's
/// foreground process group, such as a shell taking the terminal back from a
/// job, is otherwise stopped by SIGTTOU instead (XSH tcsetpgrp()); blocking
/// the signal lets the call go ahead without one being sent.
pub fn give_terminal_to(fd: RawFd, pgid: Pid) -> OsResult<()> {
    with_sigttou_blocked(|| tcsetpgrp(fd, pgid))
}

/// Runs `f` with SIGTTOU blocked, for a call that changes the terminal.
fn with_sigttou_blocked<T>(f: impl FnOnce() -> T) -> T {
    // SAFETY: both sets are initialised (sigemptyset, sigprocmask) before
    // they are read.
    unsafe {
        let mut block: libc::sigset_t = std::mem::zeroed();
        let mut previous: libc::sigset_t = std::mem::zeroed();
        libc::sigemptyset(&mut block);
        libc::sigaddset(&mut block, libc::SIGTTOU);
        libc::sigprocmask(libc::SIG_BLOCK, &block, &mut previous);
        let result = f();
        libc::sigprocmask(libc::SIG_SETMASK, &previous, std::ptr::null_mut());
        result
    }
}

/// A terminal's modes, as `tcgetattr` reports them.
#[derive(Clone, Copy)]
pub struct TtyModes(libc::termios);

/// The modes of the terminal open on `fd`, or `None` when they cannot be
/// read (not a terminal, or EIO once it has hung up).
pub fn tty_modes(fd: RawFd) -> Option<TtyModes> {
    // SAFETY: zeroed is a valid termios (some systems have members beyond
    // the POSIX ones), and tcgetattr fills it in or fails.
    let mut modes = unsafe { std::mem::zeroed::<libc::termios>() };
    if unsafe { libc::tcgetattr(fd, &mut modes) } < 0 {
        return None;
    }
    Some(TtyModes(modes))
}

/// Gives the terminal open on `fd` the modes `modes`, once the output
/// already written has gone out, as bash does. A failure (the terminal
/// may have hung up) changes nothing and is ignored. SIGTTOU is blocked,
/// as for `give_terminal_to`, in case the shell is not the foreground
/// process group after all.
pub fn set_tty_modes(fd: RawFd, modes: &TtyModes) {
    // SAFETY: `modes` came from tcgetattr.
    with_sigttou_blocked(|| unsafe { libc::tcsetattr(fd, libc::TCSADRAIN, &modes.0) });
}

/// The shell's own descriptor for the terminal job control hands to jobs,
/// -1 when it has none. A private copy, so that redirections of the
/// standard descriptors do not change which terminal that is.
static JOB_CONTROL_TTY: AtomicI32 = AtomicI32::new(-1);

/// The terminal job control hands to foreground jobs, if any.
pub fn job_control_tty() -> Option<RawFd> {
    let fd = JOB_CONTROL_TTY.load(Ordering::SeqCst);
    (fd >= 0).then_some(fd)
}

/// Makes a private copy of `fd`, a terminal, the one job control uses, or
/// with `None` closes the copy.
pub fn set_job_control_tty(fd: Option<RawFd>) -> OsResult<()> {
    let copy = match fd {
        Some(fd) => dup_private(fd)?,
        None => -1,
    };
    let previous = JOB_CONTROL_TTY.swap(copy, Ordering::SeqCst);
    if previous >= 0 {
        let _ = close(previous);
    }
    Ok(())
}

/// Whether `fd` is the job-control terminal's private descriptor.
pub fn is_job_control_tty_fd(fd: RawFd) -> bool {
    fd >= 0 && JOB_CONTROL_TTY.load(Ordering::SeqCst) == fd
}

/// Moves the job-control terminal's descriptor off `fd`, so that a
/// redirection can have that number.
pub fn move_job_control_tty_fd(fd: RawFd) -> OsResult<()> {
    let copy = dup_private(fd)?;
    let _ = JOB_CONTROL_TTY.compare_exchange(fd, copy, Ordering::SeqCst, Ordering::SeqCst);
    let _ = close(fd);
    Ok(())
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

pub fn chdir(path: &OsStr) -> LibcResult<()> {
    // A shell value can hold a NUL byte (one `read` from a file, say), which
    // no pathname can: an invalid argument, as std::fs reports it.
    let Ok(path) = CString::new(path.as_bytes()) else {
        return Err(Errno::EINVAL);
    };
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
