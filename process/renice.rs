//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::CString;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use libc::{getpwnam, passwd};
use plib::priority::{getpriority, setpriority};

const PRIO_MIN: i32 = -20;
const PRIO_MAX: i32 = 20;

#[derive(Parser)]
#[command(version, about = gettext("renice - set nice values of running processes"))]
struct Args {
    #[arg(
        short,
        long,
        required = true,
        value_parser = clap::value_parser!(i32).range(-20..20),
        help = gettext(
            "A positive or negative decimal integer which shall have the same effect \
             on the execution of the utility as if the utility had called the nice() \
             function with the numeric value of the increment option-argument"
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

    #[arg(help = gettext("Process id to adjust priority"))]
    id: String,
}

fn lookup_uid(username: &str) -> Result<u32, &'static str> {
    let c_username = CString::new(username).expect("CString::new failed");
    let passwd = unsafe { getpwnam(c_username.as_ptr()) };

    if passwd.is_null() {
        return Err("User not found");
    }

    let passwd: &passwd = unsafe { &*passwd };
    Ok(passwd.pw_uid)
}

fn parse_id(which: u32, input: &str) -> Result<u32, &'static str> {
    match input.parse::<u32>() {
        Ok(0) => Err("Invalid ID"),
        Ok(n) => Ok(n),
        Err(e) => {
            if which != libc::PRIO_USER as u32 {
                eprintln!("{}", e);
                Err("Invalid ID")
            } else {
                match lookup_uid(input) {
                    Ok(uid) => Ok(uid),
                    Err(e) => {
                        eprintln!("{}", e);
                        Err("Invalid ID or username")
                    }
                }
            }
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    // which class of priority to modify
    let which: u32 = {
        if args.pgrp {
            libc::PRIO_PGRP as u32
        } else if args.user {
            libc::PRIO_USER as u32
        } else {
            libc::PRIO_PROCESS as u32
        }
    };

    // who: obtain pgrp/pid/uid
    let id = parse_id(which, &args.id)?;

    // get current priority
    let prio = getpriority(which, id)?;

    // adjust priority based on user input
    let newprio = (prio + args.niceval).clamp(PRIO_MIN, PRIO_MAX);

    // attempt to set new priority
    setpriority(which, id, newprio)?;

    Ok(())
}
