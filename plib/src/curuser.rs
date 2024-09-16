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
    let username = unsafe {
        // Call getlogin to get the username as a *mut c_char
        let c_str = libc::getlogin();

        // Check if the pointer is not null
        if c_str.is_null() {
            panic!("Failed to get login name");
        }

        // Convert the *mut c_char to a &CStr
        let c_str = CStr::from_ptr(c_str);

        // Convert the &CStr to a Rust String
        match c_str.to_str() {
            Ok(s) => s.to_owned(), // Successfully converted CStr to Rust String
            Err(e) => panic!("Failed to convert login name to a Rust String: {}", e),
        }
    };

    username
}

pub fn tty() -> String {
    // Try to get the tty name from STDIN, STDOUT, STDERR in that order
    for fd in [libc::STDIN_FILENO, libc::STDOUT_FILENO, libc::STDERR_FILENO].iter() {
        unsafe {
            let c_str = libc::ttyname(*fd);

            if !c_str.is_null() {
                let c_str = CStr::from_ptr(c_str);

                return match c_str.to_str() {
                    Ok(s) => s.to_owned(),
                    Err(e) => panic!("Failed to convert tty name to a Rust String: {}", e),
                };
            }
        }
    }

    panic!("Failed to get tty name from any file descriptor");
}
