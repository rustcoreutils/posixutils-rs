//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

// `kill` and `timeout` bins

pub fn list_signals() {
    let mut output = String::new();
    for (name, _) in SIGLIST.iter() {
        output.push_str(name);
        output.push(' ');
    }

    println!("{}", output);
}

/// Parses [str] into [Signal].
///
/// # Arguments
///
/// * `s` - [str] that represents the signal name.
///
/// # Returns
///
/// Returns the parsed [Signal] value.
///
/// # Errors
///
/// Returns an [String] error if passed invalid signal name.
pub fn parse_signal(s: &str) -> Result<i32, String> {
    let normalized = s.trim().to_uppercase();
    let normalized = normalized.strip_prefix("SIG").unwrap_or(&normalized);

    for (name, num) in SIGLIST.iter() {
        if name == &normalized {
            return Ok(*num);
        }
    }
    Err(format!("invalid signal name '{s}'"))
}

pub fn lookup_signum(signame: &str) -> Result<i32, &'static str> {
    if signame == "0" {
        Ok(0)
    } else {
        match siglist_get(signame.to_uppercase().as_str()) {
            Some(sig_no) => Ok(sig_no),
            None => Err("Unknown signal name"),
        }
    }
}

fn siglist_get(name: &str) -> Option<i32> {
    for (signame, signo) in SIGLIST.iter() {
        if *signame == name {
            return Some(*signo);
        }
    }

    None
}

#[cfg(target_os = "macos")]
const SIGLIST: [(&str, i32); 31] = [
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
];

#[cfg(target_os = "linux")]
const SIGLIST: [(&str, i32); 32] = [
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
];
