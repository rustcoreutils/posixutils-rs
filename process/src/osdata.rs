//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate libc;

use std::collections::HashMap;

#[cfg(target_os = "macos")]
pub fn get_sigmap() -> HashMap<&'static str, u32> {
    HashMap::from([
        ("HUP", 1),
        ("INT", 2),
        ("QUIT", 3),
        ("ILL", 4),
        ("TRAP", 5),
        ("ABRT", 6),
        ("EMT", 7),
        ("FPE", 8),
        ("KILL", 9),
        ("BUS", 10),
        ("SEGV", 11),
        ("SYS", 12),
        ("PIPE", 13),
        ("ALRM", 14),
        ("TERM", 15),
        ("URG", 16),
        ("STOP", 17),
        ("TSTP", 18),
        ("CONT", 19),
        ("CHLD", 20),
        ("TTIN", 21),
        ("TTOU", 22),
        ("IO", 23),
        ("XCPU", 24),
        ("XFSZ", 25),
        ("VTALRM", 26),
        ("PROF", 27),
        ("WINCH", 28),
        ("INFO", 29),
        ("USR1", 30),
        ("USR2", 31),
    ])
}

#[cfg(target_os = "linux")]
pub fn get_sigmap() -> HashMap<&'static str, u32> {
    HashMap::from([
        ("HUP", 1),
        ("INT", 2),
        ("QUIT", 3),
        ("ILL", 4),
        ("TRAP", 5),
        ("ABRT", 6),
        ("IOT", 6),
        ("BUS", 7),
        ("FPE", 8),
        ("KILL", 9),
        ("USR1", 10),
        ("SEGV", 11),
        ("USR2", 12),
        ("PIPE", 13),
        ("ALRM", 14),
        ("TERM", 15),
        ("STKFLT", 16),
        ("CHLD", 17),
        ("CONT", 18),
        ("STOP", 19),
        ("TSTP", 20),
        ("TTIN", 21),
        ("TTOU", 22),
        ("URG", 23),
        ("XCPU", 24),
        ("XFSZ", 25),
        ("VTALRM", 26),
        ("PROF", 27),
        ("WINCH", 28),
        ("IO", 29),
        ("PWR", 30),
        ("SYS", 31),
    ])
}
