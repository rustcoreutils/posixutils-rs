//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::CStr;

/// Return the login name strictly via `getlogin(3)`, with no environment or
/// password-database fallback.
///
/// POSIX `logname` requires the login name be the one `getlogin()` reports and
/// mandates a diagnostic + non-zero exit when `getlogin()` would fail — so it
/// must NOT fall back to `$USER`/`getpwuid` (the APPLICATION USAGE section warns
/// that environment changes could produce erroneous results). Use this instead
/// of [`login_name`] where that strict contract matters.
pub fn login_name_strict() -> Option<String> {
    unsafe {
        let c_str = libc::getlogin();
        if c_str.is_null() {
            return None;
        }
        CStr::from_ptr(c_str).to_str().ok().map(|s| s.to_owned())
    }
}

pub fn login_name() -> String {
    // Try getlogin() first
    unsafe {
        let c_str = libc::getlogin();
        if !c_str.is_null() {
            if let Ok(s) = CStr::from_ptr(c_str).to_str() {
                return s.to_owned();
            }
        }
    }

    // Fall back to USER environment variable
    if let Ok(user) = std::env::var("USER") {
        return user;
    }

    // Fall back to getpwuid
    unsafe {
        let uid = libc::getuid();
        let pw = libc::getpwuid(uid);
        if !pw.is_null() && !(*pw).pw_name.is_null() {
            if let Ok(s) = CStr::from_ptr((*pw).pw_name).to_str() {
                return s.to_owned();
            }
        }
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
    unsafe {
        let c_str = libc::ttyname(fd);
        if c_str.is_null() {
            return None;
        }
        CStr::from_ptr(c_str).to_str().ok().map(|s| s.to_owned())
    }
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
