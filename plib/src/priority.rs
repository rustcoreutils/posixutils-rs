//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// NOTES:
// - Having console output in this module is probably wrong.  This
//   should be a library module, moving console output back into
//   the application... which implies errno handling probably
//   should be done in the application as well.
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
        let e = io::Error::from_raw_os_error(errno_res);
        eprintln!("getpriority: {e}");
        Err(e)
    }
}

pub fn setpriority(which: u32, id: u32, prio: i32) -> io::Result<()> {
    let res = unsafe { libc::setpriority(which as PPriorityWhichT, id, prio) };

    if res < 0 {
        let e = io::Error::last_os_error();
        eprintln!("setpriority: {e}");
        Err(e)
    } else {
        Ok(())
    }
}
