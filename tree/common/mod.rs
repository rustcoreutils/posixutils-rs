//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

// This module is shared between `cp`, `mv` and `rm` but is considered as three
// separated modules due to the project structure. The `#![allow(unused)]` is
// to remove warnings when, say, `rm` doesn't use all the the functions in this
// module (but is used in `cp` or `mv`).
#![allow(unused)]

mod change_ownership;
mod copy;

use gettextrs::gettext;
use std::{ffi::CStr, io};

// cp and mv
pub use copy::{copy_file, copy_files, CopyConfig};

// chgrp and chown
pub use change_ownership::{chown_traverse, ChangeOwnershipArgs};

/// Return the error message.
///
/// This is for compatibility with coreutils mv. `format!("{e}")` will append
/// the error code after the error message which we do not want.
pub fn error_string(e: &io::Error) -> String {
    let s = match e.raw_os_error() {
        // Like `format!("{e}")` except without the error code.
        //
        // `std` doesn't expose `sys::os::error_string` so this was copied from:
        // https://github.com/rust-lang/rust/blob/72f616273cbbacc06918ef50470d052d39d9b514/library/std/src/sys/pal/unix/os.rs#L124-L149
        Some(errno) => {
            let mut buf = [0; 128];

            unsafe {
                if libc::strerror_r(errno as _, buf.as_mut_ptr(), buf.len()) == 0 {
                    String::from_utf8_lossy(CStr::from_ptr(buf.as_ptr()).to_bytes()).to_string()
                } else {
                    // `std` just panics here
                    String::from("Unknown error")
                }
            }
        }
        None => format!("{e}"),
    };

    // Translate the error string
    gettext(s)
}
