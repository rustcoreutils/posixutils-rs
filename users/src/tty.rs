//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate atty;
extern crate libc;
use std::ffi::CStr;

fn main() {
    let is_tty = atty::is(atty::Stream::Stdin);
    if !is_tty {
        println!("not a tty");
        std::process::exit(1);
    }

    let ttyname = unsafe {
        // Call getlogin to get the username as a *mut c_char
        let c_str = libc::ttyname(libc::STDIN_FILENO);

        // Check if the pointer is not null
        if c_str.is_null() {
            panic!("Failed to get tty name");
        }

        // Convert the *mut c_char to a &CStr
        let c_str = CStr::from_ptr(c_str);

        // Convert the &CStr to a Rust String
        match c_str.to_str() {
            Ok(s) => s.to_owned(), // Successfully converted CStr to Rust String
            Err(e) => panic!("Failed to convert tty name to a Rust String: {}", e),
        }
    };

    println!("{}", ttyname);
}
