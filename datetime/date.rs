//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - add tests (how?)
// - double-check that Rust stftime() is POSIX compliant
//

use chrono::{DateTime, Datelike, Local, LocalResult, TimeZone, Utc};
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

const DEF_TIMESTR: &str = "%a %b %e %H:%M:%S %Z %Y";

#[derive(Parser)]
#[command(version, about = gettext("date - write the date and time"))]
struct Args {
    #[arg(
        short,
        long,
        help = gettext(
            "Perform operations as if the TZ env var was set to the string \"UTC0\""
        )
    )]
    utc: bool,

    #[arg(
        help = gettext(
            "If prefixed with '+', Display the current time in the given FORMAT, \
             as in strftime(3). Otherwise, set the current time to the given string"
        )
    )]
    timestr: Option<String>,
}

fn show_time_local(formatstr: &str) -> String {
    let now = chrono::Local::now();
    now.format(formatstr).to_string()
}

fn show_time_utc(formatstr: &str) -> String {
    let now = chrono::Utc::now();
    now.format(formatstr).to_string()
}

fn show_time(utc: bool, formatstr: &str) {
    let timestr = {
        if utc {
            show_time_utc(formatstr)
        } else {
            show_time_local(formatstr)
        }
    };

    println!("{}", timestr);
}

fn set_time(utc: bool, timestr: &str) -> Result<(), &'static str> {
    for ch in timestr.chars() {
        if !ch.is_ascii_digit() {
            return Err("date: invalid date");
        }
    }

    let cur_year = {
        if utc {
            let now = chrono::Utc::now();
            now.year()
        } else {
            let now = chrono::Local::now();
            now.year()
        }
    };

    let (year, month, day, hour, minute) = match timestr.len() {
        8 => {
            let month = timestr[0..2].parse::<u32>().unwrap();
            let day = timestr[2..4].parse::<u32>().unwrap();
            let hour = timestr[4..6].parse::<u32>().unwrap();
            let minute = timestr[6..8].parse::<u32>().unwrap();
            (cur_year, month, day, hour, minute)
        }
        10 => {
            let month = timestr[0..2].parse::<u32>().unwrap();
            let day = timestr[2..4].parse::<u32>().unwrap();
            let hour = timestr[4..6].parse::<u32>().unwrap();
            let minute = timestr[6..8].parse::<u32>().unwrap();
            let year = timestr[8..10].parse::<i32>().unwrap();
            if year < 70 {
                (year + 2000, month, day, hour, minute)
            } else {
                (year + 1900, month, day, hour, minute)
            }
        }
        12 => {
            let month = timestr[0..2].parse::<u32>().unwrap();
            let day = timestr[2..4].parse::<u32>().unwrap();
            let hour = timestr[4..6].parse::<u32>().unwrap();
            let minute = timestr[6..8].parse::<u32>().unwrap();
            let year = timestr[8..12].parse::<i32>().unwrap();
            (year, month, day, hour, minute)
        }
        _ => {
            return Err("date: invalid date");
        }
    };

    // calculate system time
    let new_time = {
        if utc {
            match chrono::Utc.with_ymd_and_hms(year, month, day, hour, minute, 0) {
                LocalResult::<DateTime<Utc>>::Single(t) => t.timestamp(),
                _ => {
                    return Err("date: invalid date");
                }
            }
        } else {
            match chrono::Local.with_ymd_and_hms(year, month, day, hour, minute, 0) {
                LocalResult::<DateTime<Local>>::Single(t) => t.timestamp(),
                _ => {
                    return Err("date: invalid date");
                }
            }
        }
    };

    let new_time = libc::timespec {
        tv_sec: new_time,
        tv_nsec: 0,
    };

    // set system time
    unsafe {
        if libc::clock_settime(libc::CLOCK_REALTIME, &new_time) != 0 {
            return Err("date: failed to set time");
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

    let args = Args::parse();

    match &args.timestr {
        None => show_time(args.utc, DEF_TIMESTR),
        Some(timestr) => {
            if let Some(st) = timestr.strip_prefix("+") {
                show_time(args.utc, st);
            } else {
                set_time(args.utc, timestr)?;
            }
        }
    }

    Ok(())
}
