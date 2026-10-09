//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::{c_char, c_int, CStr};

extern "C" {
    // POSIX, in every libc this builds against, but not in the libc crate.
    fn getlogin_r(buf: *mut c_char, bufsize: libc::size_t) -> c_int;
}

/// Call a reentrant function that writes a C string into a caller's buffer
/// and returns an error number, growing the buffer while it reports `ERANGE`.
/// `None` for any other failure, or a result that is not UTF-8.
///
/// Used in place of `getlogin` and `ttyname`, which return a pointer into one
/// static buffer that another thread's call overwrites.
fn string_from_r(mut call: impl FnMut(&mut [c_char]) -> c_int) -> Option<String> {
    const MAX_LEN: usize = 64 * 1024;
    let mut len = 256;
    loop {
        let mut buf = vec![0 as c_char; len];
        match call(&mut buf) {
            0 => {
                // SAFETY: on success the buffer holds a NUL-terminated string.
                let s = unsafe { CStr::from_ptr(buf.as_ptr()) };
                return s.to_str().ok().map(str::to_owned);
            }
            libc::ERANGE if len < MAX_LEN => len *= 2,
            _ => return None,
        }
    }
}

/// `getlogin_r(3)`: the login name of the session, or `None`.
fn getlogin() -> Option<String> {
    // SAFETY: `buf` is writable for its full length.
    string_from_r(|buf| unsafe { getlogin_r(buf.as_mut_ptr(), buf.len()) })
}

/// Whether the real and effective user *and group* IDs all match, i.e. the
/// process carries no elevated privilege from its executable's mode bits.
///
/// Use this before honouring an environment variable that names a file to read
/// or a program to execute. Checking only `getuid() == geteuid()` is the common
/// mistake: it is true for a **set-gid** binary, and set-gid is how the
/// utilities that need this are conventionally installed — `crontab` set-gid
/// `crontab` so it can write the spool, `lp` set-gid `lp`/`daemon`. A guard
/// that misses that case protects nothing in the deployment that matters.
pub fn real_and_effective_ids_match() -> bool {
    // SAFETY: getuid/geteuid/getgid/getegid never fail and take no arguments.
    unsafe { libc::getuid() == libc::geteuid() && libc::getgid() == libc::getegid() }
}

/// Return the login name strictly via `getlogin(3)`, with no environment or
/// password-database fallback.
///
/// POSIX `logname` requires the login name be the one `getlogin()` reports and
/// mandates a diagnostic + non-zero exit when `getlogin()` would fail — so it
/// must NOT fall back to `$USER`/`getpwuid` (the APPLICATION USAGE section warns
/// that environment changes could produce erroneous results). Use this instead
/// of [`login_name`] where that strict contract matters.
pub fn login_name_strict() -> Option<String> {
    getlogin()
}

pub fn login_name() -> String {
    // Try getlogin() first
    if let Some(name) = getlogin() {
        return name;
    }

    // Fall back to USER environment variable
    if let Ok(user) = std::env::var("USER") {
        return user;
    }

    // Fall back to the user database
    // SAFETY: getuid never fails.
    let uid = unsafe { libc::getuid() };
    if let Some(name) = crate::user::get_by_uid(uid).and_then(|u| u.name.into_string().ok()) {
        return name;
    }

    // Last resort
    String::from("unknown")
}

/// Return the terminal pathname of a specific file descriptor via `ttyname(3)`.
///
/// Unlike [`tty`], this consults exactly the given fd. POSIX `tty` must report
/// the name of *standard input only*, so it uses `ttyname_of(STDIN_FILENO)`
/// rather than searching stdout/stderr.
pub fn ttyname_of(fd: libc::c_int) -> Option<String> {
    // SAFETY: `buf` is writable for its full length.
    string_from_r(|buf| unsafe { libc::ttyname_r(fd, buf.as_mut_ptr(), buf.len()) })
}

pub fn tty() -> Option<String> {
    // Try to get the tty name from STDIN, STDOUT, STDERR in that order
    for fd in [libc::STDIN_FILENO, libc::STDOUT_FILENO, libc::STDERR_FILENO].iter() {
        if let Some(name) = ttyname_of(*fd) {
            return Some(name);
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::{real_and_effective_ids_match, ttyname_of};
    use std::os::fd::{FromRawFd, OwnedFd};

    /// `ttyname_of` names a terminal through `ttyname_r`, and says nothing for
    /// a descriptor that is not one.
    #[test]
    fn ttyname_of_names_a_pty_and_nothing_else() {
        let (mut master, mut slave) = (-1, -1);
        // SAFETY: the out-pointers are valid; the optional ones are null.
        let rc = unsafe {
            libc::openpty(
                &mut master,
                &mut slave,
                std::ptr::null_mut(),
                // `*mut` on macOS, `*const` on Linux; a null `*mut` suits both.
                std::ptr::null_mut(),
                std::ptr::null_mut(),
            )
        };
        assert_eq!(rc, 0, "openpty: {}", std::io::Error::last_os_error());
        // Not inherited by the children other tests spawn meanwhile.
        for fd in [master, slave] {
            // SAFETY: fd is open; F_SETFD takes an int.
            unsafe { libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC) };
        }
        // SAFETY: openpty returned two descriptors that nothing else owns.
        let (_master, _slave) =
            unsafe { (OwnedFd::from_raw_fd(master), OwnedFd::from_raw_fd(slave)) };

        let name = ttyname_of(slave).expect("a pty slave has a name");
        assert!(name.starts_with("/dev/"), "{name}");
        assert_eq!(ttyname_of(-1), None);
    }

    /// An ordinary test process inherits no set-uid or set-gid bit, so every
    /// caller that gates an environment override on this must see it as true —
    /// otherwise the overrides those utilities' tests depend on would be
    /// silently ignored and the tests would pass for the wrong reason.
    #[test]
    fn ids_match_in_an_unprivileged_process() {
        assert!(
            real_and_effective_ids_match(),
            "a plain `cargo test` process should carry no elevated privilege"
        );
    }

    /// The check must consider the group, not only the user. A uid-only guard
    /// is true for a set-gid binary, which is how `crontab` and `lp` are
    /// conventionally installed.
    #[test]
    fn ids_match_considers_the_group() {
        // SAFETY: these getters never fail.
        let (uid, euid, gid, egid) = unsafe {
            (
                libc::getuid(),
                libc::geteuid(),
                libc::getgid(),
                libc::getegid(),
            )
        };
        assert_eq!(
            real_and_effective_ids_match(),
            uid == euid && gid == egid,
            "the guard must be the conjunction of both comparisons"
        );
    }
}
