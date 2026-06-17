//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// This is a library module: it returns errors to the caller rather than
// printing diagnostics itself (the application owns the diagnostic surface,
// e.g. locale-aware messages via plib::diag).
//

use std::io;

cfg_if::cfg_if! {
    if #[cfg(any(target_os = "macos", target_env = "musl"))] {
        type PPriorityWhichT = libc::c_int;
    } else {
        type PPriorityWhichT = libc::__priority_which_t;
    }
}

pub fn getpriority(which: u32, id: u32) -> io::Result<i32> {
    errno::set_errno(errno::Errno(0));

    let res = unsafe { libc::getpriority(which as PPriorityWhichT, id) };

    let errno_res = errno::errno().0;
    if errno_res == 0 {
        Ok(res)
    } else {
        Err(io::Error::from_raw_os_error(errno_res))
    }
}

pub fn setpriority(which: u32, id: u32, prio: i32) -> io::Result<()> {
    let res = unsafe { libc::setpriority(which as PPriorityWhichT, id, prio) };

    if res < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}
