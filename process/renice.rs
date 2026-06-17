//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::CString;
use std::process;

use clap::Parser;
use gettextrs::gettext;
use libc::{getpwnam, passwd};
use plib::diag;
use plib::priority::{getpriority, setpriority};

const PRIO_MIN: i32 = -20;
const PRIO_MAX: i32 = 20;

#[derive(Parser)]
#[command(version, about = gettext("renice - set nice values of running processes"))]
struct Args {
    #[arg(
        short = 'n',
        required = true,
        allow_hyphen_values = true,
        value_parser = clap::value_parser!(i32).range(-20..=20),
        help = gettext(
            "A positive or negative decimal integer increment applied to the \
             current nice value of each process"
        )
    )]
    niceval: i32,

    #[arg(
        short = 'g',
        long,
        group = "mode",
        help = gettext(
            "Interpret the following operands as unsigned decimal integer process group IDs"
        )
    )]
    pgrp: bool,

    #[arg(
        short,
        long,
        group = "mode",
        default_value_t = true,
        help = gettext(
            "Interpret the following operands as unsigned decimal integer process IDs. \
             The -p option is the default if no options are specified"
        )
    )]
    pid: bool,

    #[arg(
        short,
        long,
        group = "mode",
        help = gettext("Interpret the following operands as users")
    )]
    user: bool,

    #[arg(required = true, help = gettext("One or more IDs whose priority to adjust"))]
    ids: Vec<String>,
}

fn lookup_uid(username: &str) -> Result<u32, ()> {
    let c_username = match CString::new(username) {
        Ok(s) => s,
        Err(_) => return Err(()),
    };
    let passwd = unsafe { getpwnam(c_username.as_ptr()) };

    if passwd.is_null() {
        return Err(());
    }

    let passwd: &passwd = unsafe { &*passwd };
    Ok(passwd.pw_uid)
}

/// Resolve an operand to a numeric id for the given priority class. For the
/// user class a non-numeric operand is looked up via `getpwnam`. Returns the
/// resolved id, or `Err` with a localized diagnostic already emitted.
#[allow(clippy::unnecessary_cast)] // PRIO_* types differ: i32 on macOS, u32 on Linux
fn parse_id(which: u32, input: &str) -> Result<u32, ()> {
    if let Ok(n) = input.parse::<u32>() {
        // 0 is a valid operand (PID/PGID 0 = caller's process/group,
        // UID 0 = root); do not reject it.
        return Ok(n);
    }

    if which == libc::PRIO_USER as u32 {
        if let Ok(uid) = lookup_uid(input) {
            return Ok(uid);
        }
        diag::error(&format!("{}: {}", gettext("no such user"), input));
    } else {
        diag::error(&format!("{}: {}", gettext("invalid ID"), input));
    }
    Err(())
}

fn main() {
    diag::init_locale("renice");

    let args = Args::parse();

    // which class of priority to modify
    // Cast to u32 for cross-platform compatibility (i32 on macOS, u32 on Linux)
    #[allow(clippy::unnecessary_cast)]
    let which: u32 = {
        if args.pgrp {
            libc::PRIO_PGRP as u32
        } else if args.user {
            libc::PRIO_USER as u32
        } else {
            libc::PRIO_PROCESS as u32
        }
    };

    // Process each ID independently: a failure on one ID must not prevent
    // processing the rest, and the exit status is non-zero if any failed.
    for id_str in &args.ids {
        let id = match parse_id(which, id_str) {
            Ok(id) => id,
            Err(()) => continue,
        };

        let prio = match getpriority(which, id) {
            Ok(p) => p,
            Err(e) => {
                diag::error(&format!("{}: {}: {}", gettext("getpriority"), id_str, e));
                continue;
            }
        };

        let newprio = (prio + args.niceval).clamp(PRIO_MIN, PRIO_MAX);

        if let Err(e) = setpriority(which, id, newprio) {
            diag::error(&format!("{}: {}: {}", gettext("setpriority"), id_str, e));
        }
    }

    process::exit(diag::exit_status());
}
