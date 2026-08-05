//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Operations on *another user's* terminal device.
//!
//! `mesg`, `write` and `talkd` all need the same three things: resolve a login
//! name to a terminal, decide whether writing to it is permitted, and then
//! write. Each had grown its own copy with a subtly different permission rule,
//! and two of them checked permission on a path and then opened a *different*
//! path — a time-of-check/time-of-use gap.
//!
//! The division of labor here is deliberate:
//!
//! * **The kernel does access control.** [`open_for_write`] performs exactly
//!   one `open(2)`; if the caller has no write access, that call fails. No
//!   amount of stat-ing a pathname beforehand can substitute for it, and doing
//!   so introduces the race this module exists to close.
//! * **This module does `mesg` policy.** [`may_write`] runs *after* the open
//!   succeeded, on the very inode the descriptor refers to, and answers a
//!   different question: has the terminal's owner asked not to be disturbed?
//!
//! Contrast with `plib::curuser`, which reports facts about the *invoking*
//! user's own terminal.

use std::fs::File;
use std::io;
use std::os::unix::io::{AsRawFd, RawFd};
use std::path::{Path, PathBuf};

/// `fstat(2)` an open descriptor.
pub fn fstat(fd: RawFd) -> io::Result<libc::stat> {
    unsafe {
        let mut st: libc::stat = std::mem::zeroed();
        if libc::fstat(fd, &mut st) < 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(st)
    }
}

/// Whether the terminal described by `st` currently accepts messages — i.e.
/// whether its owner has left `mesg y`.
///
/// This is a property of the terminal, independent of who is asking: `mesg y`
/// is precisely the group/other write bits, and `mesg n` clears them.
pub fn mesg_allowed(st: &libc::stat) -> bool {
    (st.st_mode & (libc::S_IWGRP | libc::S_IWOTH)) != 0
}

/// Set or clear the group and other write bits on the open terminal `fd`.
/// A no-op when the terminal is already in the requested state.
pub fn set_mesg(fd: RawFd, st: &libc::stat, grant: bool) -> io::Result<()> {
    let mut mode = st.st_mode;

    if grant {
        if mesg_allowed(st) {
            return Ok(());
        }
        mode |= libc::S_IWGRP | libc::S_IWOTH;
    } else {
        if !mesg_allowed(st) {
            return Ok(());
        }
        mode &= !(libc::S_IWGRP | libc::S_IWOTH);
    }

    if unsafe { libc::fchmod(fd, mode) } < 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

/// Whether policy permits writing to the terminal described by `st`.
///
/// This layers on top of the kernel's own check, which [`open_for_write`] has
/// already performed — so the question here is only whether the recipient has
/// asked not to be disturbed:
///
/// * the superuser is not constrained by `mesg`;
/// * neither is the terminal's own owner writing to it;
/// * otherwise the terminal must be in `mesg y`.
///
/// Note this does *not* re-check group membership. By the time it runs, the
/// kernel has already proved the caller has write access by owner, group,
/// supplementary group, or other — re-deriving that from `st` with `getgid()`
/// alone would be both redundant and wrong, since it ignores `getgroups(2)`.
pub fn may_write(st: &libc::stat) -> bool {
    if unsafe { libc::geteuid() } == 0 {
        return true;
    }
    if st.st_uid == unsafe { libc::getuid() } && st.st_mode & libc::S_IWUSR != 0 {
        return true;
    }
    mesg_allowed(st)
}

/// A terminal selected from the utmpx database for a login name.
pub struct UserTty {
    /// Full device path, `/dev/<line>`.
    pub path: PathBuf,
    /// The bare utmpx `ut_line`.
    pub line: String,
    /// How many login terminals that user has. Greater than one means the user
    /// is logged in more than once, which `write` reports on stdout.
    pub login_count: usize,
}

/// Resolve a login name to a terminal via the utmpx database.
///
/// When `requested_line` is `Some`, only that line matches; otherwise the
/// first login terminal wins. Returns `None` when the user has no login
/// terminal (or none matching `requested_line`).
pub fn user_tty(user: &str, requested_line: Option<&str>) -> Option<UserTty> {
    let entries: Vec<_> = crate::utmpx::load()
        .into_iter()
        .filter(|e| e.user == user && e.typ == crate::platform::USER_PROCESS)
        .collect();

    let login_count = entries.len();
    let chosen = entries
        .into_iter()
        .find(|e| requested_line.is_none_or(|want| e.line == want))?;

    Some(UserTty {
        path: PathBuf::from(format!("/dev/{}", chosen.line)),
        line: chosen.line,
        login_count,
    })
}

/// Why [`open_for_write`] declined to open a terminal.
#[derive(Debug)]
pub enum OpenError {
    /// The pathname is not a lexically safe `/dev` device name.
    BadPath,
    /// It opened, but the descriptor is not a terminal.
    NotTerminal,
    /// It opened and is a terminal, but `mesg n` forbids writing.
    PermissionDenied,
    /// The open itself failed (`ENOENT`, `EACCES`, …).
    Io(io::Error),
}

impl std::fmt::Display for OpenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpenError::BadPath => write!(f, "not a terminal device"),
            OpenError::NotTerminal => write!(f, "not a terminal device"),
            OpenError::PermissionDenied => write!(f, "permission denied"),
            OpenError::Io(e) => write!(f, "{}", e),
        }
    }
}

/// Whether `path` is lexically a safe terminal device name: under `/dev`, with
/// no `.` or `..` component. Purely textual — no syscalls, so a hostile operand
/// is rejected before anything is opened.
fn is_safe_dev_path(path: &Path) -> bool {
    use std::path::Component;

    if !path.is_absolute() {
        return false;
    }
    let mut components = path.components();
    if components.next() != Some(Component::RootDir) {
        return false;
    }
    if components.next() != Some(Component::Normal("dev".as_ref())) {
        return false;
    }
    // Everything after /dev must be an ordinary name: `/dev/../etc/passwd` and
    // `/dev/./tty` are both refused.
    let mut saw_name = false;
    for c in components {
        match c {
            Component::Normal(_) => saw_name = true,
            _ => return false,
        }
    }
    saw_name
}

/// Open a terminal for writing, without a time-of-check/time-of-use gap.
///
/// The descriptor that is validated and permission-checked is the descriptor
/// that is returned, so the inode cannot be swapped between the check and the
/// write. The sequence is:
///
/// 1. reject a pathname that is not lexically a `/dev` device name — no
///    syscalls, so `/dev/../etc/passwd` never reaches an `open(2)`;
/// 2. one `open(2)`, at which point **the kernel enforces write permission**;
/// 3. `fstat` + `isatty` on the returned descriptor;
/// 4. [`may_write`] on that same `stat`, honoring `mesg n`.
///
/// The open carries `O_NOCTTY` so writing to a terminal never makes it the
/// caller's controlling terminal (which would deliver that terminal's job
/// control signals to a daemon), `O_NOFOLLOW` so a symlink planted at the final
/// component is refused, and `O_NONBLOCK` so a FIFO or modem-control line
/// cannot block the open. `O_NONBLOCK` is cleared before returning so the
/// caller's writes behave normally.
pub fn open_for_write(path: &Path) -> Result<File, OpenError> {
    use std::os::unix::fs::OpenOptionsExt;

    if !is_safe_dev_path(path) {
        return Err(OpenError::BadPath);
    }

    let file = std::fs::OpenOptions::new()
        .write(true)
        .custom_flags(libc::O_NOCTTY | libc::O_NOFOLLOW | libc::O_NONBLOCK)
        .open(path)
        .map_err(OpenError::Io)?;

    let fd = file.as_raw_fd();

    if unsafe { libc::isatty(fd) } != 1 {
        return Err(OpenError::NotTerminal);
    }

    let st = fstat(fd).map_err(OpenError::Io)?;
    if !may_write(&st) {
        return Err(OpenError::PermissionDenied);
    }

    // Restore blocking semantics for the caller's writes.
    unsafe {
        let flags = libc::fcntl(fd, libc::F_GETFL);
        if flags >= 0 {
            libc::fcntl(fd, libc::F_SETFL, flags & !libc::O_NONBLOCK);
        }
    }

    Ok(file)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn safe_dev_path_accepts_ordinary_terminals() {
        assert!(is_safe_dev_path(Path::new("/dev/tty1")));
        assert!(is_safe_dev_path(Path::new("/dev/pts/3")));
        assert!(is_safe_dev_path(Path::new("/dev/ttys004")));
    }

    #[test]
    fn safe_dev_path_rejects_traversal_and_non_dev() {
        // The whole point: a traversal is refused textually, before any open.
        assert!(!is_safe_dev_path(Path::new("/dev/../etc/passwd")));
        assert!(!is_safe_dev_path(Path::new("/dev/tty1/../../etc/passwd")));
        assert!(!is_safe_dev_path(Path::new("/etc/passwd")));
        assert!(!is_safe_dev_path(Path::new("/dev")));
        assert!(!is_safe_dev_path(Path::new("tty1")));
        assert!(!is_safe_dev_path(Path::new("/devious/tty1")));
    }

    #[test]
    fn safe_dev_path_tolerates_a_redundant_dot() {
        // `Path::components()` folds `.` away but preserves `..`, so this is
        // literally the same path as /dev/tty1 and is accepted. Only `..`,
        // which can escape /dev, is refused.
        assert!(is_safe_dev_path(Path::new("/dev/./tty1")));
    }

    #[test]
    fn mesg_allowed_tracks_group_and_other_write_bits() {
        let mut st: libc::stat = unsafe { std::mem::zeroed() };
        st.st_mode = 0o600;
        assert!(!mesg_allowed(&st));
        st.st_mode = 0o620;
        assert!(mesg_allowed(&st));
        st.st_mode = 0o602;
        assert!(mesg_allowed(&st));
    }

    #[test]
    fn may_write_permits_the_owner_of_a_private_terminal() {
        // The case talkd's old rule got wrong: a 0600 terminal you own.
        let mut st: libc::stat = unsafe { std::mem::zeroed() };
        st.st_mode = 0o600;
        st.st_uid = unsafe { libc::getuid() };
        assert!(may_write(&st));
    }

    #[test]
    fn may_write_refuses_a_third_partys_mesg_n_terminal() {
        if unsafe { libc::geteuid() } == 0 {
            return; // root bypasses mesg by design
        }
        let mut st: libc::stat = unsafe { std::mem::zeroed() };
        st.st_mode = 0o600;
        // A uid that is certainly not ours.
        st.st_uid = unsafe { libc::getuid() }.wrapping_add(1);
        assert!(!may_write(&st));
    }
}
