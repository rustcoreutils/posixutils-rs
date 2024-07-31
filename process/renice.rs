//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate libc;
extern crate plib;

use clap::Parser;
use errno::{errno, set_errno};
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use libc::{getpwnam, passwd};
use plib::PROJECT_NAME;
use std::ffi::CString;
use std::io;

const PRIO_MIN: i32 = -20;
const PRIO_MAX: i32 = 20;

/// renice - set nice values of running processes
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// A positive or negative decimal integer which shall have the same effect on the execution of the utility as if the utility had called the nice() function with the numeric value of the increment option-argument.
    #[arg(short, long, required=true, value_parser = clap::value_parser!(i32).range(-20..20))]
    niceval: i32,

    /// Interpret the following operands as unsigned decimal integer process group IDs.
    #[arg(short = 'g', long, group = "mode")]
    pgrp: bool,

    /// Interpret the following operands as unsigned decimal integer process IDs. The -p option is the default if no options are specified.
    #[arg(short, long, group = "mode", default_value_t = true)]
    pid: bool,

    /// Interpret the following operands as users.
    #[arg(short, long, group = "mode")]
    user: bool,

    /// process id to adjust priority
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

fn xgetpriority(which: u32, id: u32) -> io::Result<i32> {
    set_errno(errno::Errno(0));

    #[cfg(not(target_os = "macos"))]
    let res = unsafe { libc::getpriority(which, id) };

    #[cfg(target_os = "macos")]
    let res = unsafe { libc::getpriority(which as i32, id) };

    let errno_res = errno().0;
    if errno_res == 0 {
        Ok(res)
    } else {
        let e = io::Error::from_raw_os_error(errno_res);
        eprintln!("getpriority: {}", e);
        Err(e)
    }
}

fn xsetpriority(which: u32, id: u32, prio: i32) -> io::Result<()> {
    #[cfg(not(target_os = "macos"))]
    let res = unsafe { libc::setpriority(which, id, prio) };

    #[cfg(target_os = "macos")]
    let res = unsafe { libc::setpriority(which as i32, id, prio) };

    if res < 0 {
        let e = io::Error::last_os_error();
        eprintln!("setpriority: {}", e);
        Err(e)
    } else {
        Ok(())
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

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
    let prio = xgetpriority(which, id)?;

    // adjust priority based on user input
    let newprio = (prio + args.niceval).clamp(PRIO_MIN, PRIO_MAX);

    // attempt to set new priority
    xsetpriority(which, id, newprio)?;

    Ok(())
}
