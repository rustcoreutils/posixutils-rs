//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::CStr;

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

pub fn tty() -> Option<String> {
    // Try to get the tty name from STDIN, STDOUT, STDERR in that order
    for fd in [libc::STDIN_FILENO, libc::STDOUT_FILENO, libc::STDERR_FILENO].iter() {
        unsafe {
            let c_str = libc::ttyname(*fd);

            if !c_str.is_null() {
                let c_str = CStr::from_ptr(c_str);

                return match c_str.to_str() {
                    Ok(s) => Some(s.to_owned()),
                    Err(_) => None,
                };
            }
        }
    }

    None
}
