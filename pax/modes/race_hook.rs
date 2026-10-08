//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Test-only pause points inside the windows a concurrent writer of the
//! destination could use.
//!
//! A race between creating a node and applying its attributes cannot be held
//! open from outside the process: nothing pax opens in between can be leased
//! or blocked on. So the unit tests install a hook here, run at the very point
//! a concurrent writer would act, and stage the swap there -- the same thing
//! every time, with no timing involved.

use std::cell::RefCell;
use std::ffi::CStr;
#[cfg(target_os = "linux")]
use std::ffi::CString;
#[cfg(target_os = "linux")]
use std::os::fd::{AsRawFd, OwnedFd};

/// Where in pax the hook runs.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Point {
    /// A FIFO, device or symbolic link member has just been created at `name`.
    Made,
    /// A directory has just been made at `name` with `mkdirat`.
    MadeDir,
}

type Hook = Box<dyn FnMut(Point, libc::c_int, &CStr)>;

thread_local! {
    static HOOK: RefCell<Option<Hook>> = const { RefCell::new(None) };
}

/// pax has reached `point` for `name` below `dirfd`: run the hook, if any.
pub(crate) fn reached(point: Point, dirfd: libc::c_int, name: &CStr) {
    let hook = HOOK.with(|h| h.borrow_mut().take());
    if let Some(mut hook) = hook {
        hook(point, dirfd, name);
        HOOK.with(|h| *h.borrow_mut() = Some(hook));
    }
}

/// Run `f` with `hook` installed on this thread.
pub(crate) fn with_hook<R>(
    hook: impl FnMut(Point, libc::c_int, &CStr) + 'static,
    f: impl FnOnce() -> R,
) -> R {
    HOOK.with(|h| *h.borrow_mut() = Some(Box::new(hook)));
    let result = f();
    HOOK.with(|h| *h.borrow_mut() = None);
    result
}

/// The environment variable that marks the child `rerun_in_new_session` runs.
#[cfg(target_os = "linux")]
const SESSION_CHILD: &str = "PAX_TEST_SESSION_CHILD";

/// Whether this is the child `rerun_in_new_session` started.
#[cfg(target_os = "linux")]
pub(crate) fn in_new_session() -> bool {
    std::env::var_os(SESSION_CHILD).is_some()
}

/// Run the test `name` (its full path) again, alone, in a child process that
/// leads a session of its own and so has no controlling terminal -- the state
/// in which opening a terminal without `O_NOCTTY` makes it one. The test
/// passes when the child's run of it does.
#[cfg(target_os = "linux")]
pub(crate) fn rerun_in_new_session(name: &str) {
    use std::os::unix::process::CommandExt;
    let mut command = std::process::Command::new(std::env::current_exe().unwrap());
    command
        .args([name, "--exact", "--nocapture"])
        .env(SESSION_CHILD, "1")
        .stdin(std::process::Stdio::null());
    // SAFETY: setsid is async-signal-safe.
    unsafe {
        command.pre_exec(|| {
            if libc::setsid() < 0 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        });
    }
    let out = command.output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success() && stdout.contains("1 passed"),
        "child run failed:\n{stdout}\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// A new pseudo-terminal: its master, a descriptor for the directory its
/// slave is in, and the slave's name there.
#[cfg(target_os = "linux")]
pub(crate) fn open_pty() -> (OwnedFd, OwnedFd, CString) {
    use std::os::fd::FromRawFd;
    let master = unsafe { libc::posix_openpt(libc::O_RDWR | libc::O_NOCTTY | libc::O_CLOEXEC) };
    assert!(
        master >= 0,
        "posix_openpt: {}",
        std::io::Error::last_os_error()
    );
    let master = unsafe { OwnedFd::from_raw_fd(master) };
    assert_eq!(unsafe { libc::grantpt(master.as_raw_fd()) }, 0);
    assert_eq!(unsafe { libc::unlockpt(master.as_raw_fd()) }, 0);
    let mut buf = [0 as libc::c_char; 128];
    let r = unsafe { libc::ptsname_r(master.as_raw_fd(), buf.as_mut_ptr(), buf.len()) };
    assert_eq!(r, 0);
    let path = unsafe { CStr::from_ptr(buf.as_ptr()) }.to_bytes();
    let slash = path.iter().rposition(|&b| b == b'/').unwrap();
    let dir = CString::new(&path[..slash]).unwrap();
    let name = CString::new(&path[slash + 1..]).unwrap();
    let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC;
    let dirfd = unsafe { libc::open(dir.as_ptr(), flags) };
    assert!(dirfd >= 0);
    (master, unsafe { OwnedFd::from_raw_fd(dirfd) }, name)
}

/// Whether this process has a controlling terminal.
#[cfg(target_os = "linux")]
pub(crate) fn has_controlling_tty() -> bool {
    let fd = unsafe { libc::open(c"/dev/tty".as_ptr(), libc::O_RDWR | libc::O_NOCTTY) };
    if fd >= 0 {
        unsafe { libc::close(fd) };
    }
    fd >= 0
}

/// A hook action: rename the directory `victim` over the empty directory
/// `name` below `dirfd`, as a writer of both directories would.
pub(crate) fn swap_for_directory(dirfd: libc::c_int, name: &CStr, victim: &CStr) {
    let r = unsafe { libc::renameat(libc::AT_FDCWD, victim.as_ptr(), dirfd, name.as_ptr()) };
    assert_eq!(r, 0, "renameat: {}", std::io::Error::last_os_error());
}

/// A hook action: replace `name` below `dirfd` with a hard link to `victim`,
/// as a writer of the destination directory would.
pub(crate) fn swap_for_hard_link(dirfd: libc::c_int, name: &CStr, victim: &CStr) {
    assert_eq!(unsafe { libc::unlinkat(dirfd, name.as_ptr(), 0) }, 0);
    let r = unsafe { libc::linkat(libc::AT_FDCWD, victim.as_ptr(), dirfd, name.as_ptr(), 0) };
    assert_eq!(r, 0, "linkat: {}", std::io::Error::last_os_error());
}
