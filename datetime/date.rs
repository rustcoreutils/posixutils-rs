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
use chrono_tz::Tz;
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::env;

const DEF_TIMESTR: &str = "%a %b %e %H:%M:%S %Z %Y";

/// Get the timezone abbreviation for the given datetime
fn get_timezone_abbreviation(dt: &DateTime<Local>) -> String {
    // Try to get TZ environment variable
    if let Ok(tz_str) = env::var("TZ") {
        // Handle special cases for UTC
        if tz_str == "UTC" || tz_str == "UTC0" || tz_str.is_empty() {
            return "UTC".to_string();
        }
        
        // Try to parse it as a chrono-tz timezone
        if let Ok(tz) = tz_str.parse::<Tz>() {
            // Convert the local datetime to the specified timezone
            if let Some(dt_tz) = tz.from_local_datetime(&dt.naive_local()).single() {
                return dt_tz.format("%Z").to_string();
            }
        }
    }
    
    // Fallback: if TZ is not set or invalid, try to detect system timezone
    // For now, check if offset is zero, then it's UTC
    let offset = dt.offset().local_minus_utc();
    if offset == 0 {
        return "UTC".to_string();
    }
    
    // Otherwise, use the offset format as fallback
    dt.format("%:z").to_string()
}

/// Format a datetime string, replacing %Z with proper timezone abbreviation
fn format_with_timezone(formatstr: &str, dt: &DateTime<Local>) -> String {
    if formatstr.contains("%Z") {
        let tz_abbr = get_timezone_abbreviation(dt);
        let formatted = dt.format(formatstr).to_string();
        // Replace the offset (like +01:00) with the timezone abbreviation
        // chrono uses %Z for offset, so we need to replace it
        // The offset format from chrono with %Z is like "+01:00" or "+00:00"
        let offset_pattern = dt.format("%Z").to_string();
        formatted.replace(&offset_pattern, &tz_abbr)
    } else {
        dt.format(formatstr).to_string()
    }
}

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
    format_with_timezone(formatstr, &now)
}

fn show_time_utc(formatstr: &str) -> String {
    let now = chrono::Utc::now();
    // For UTC, %Z should always be "UTC"
    if formatstr.contains("%Z") {
        let formatted = now.format(formatstr).to_string();
        let offset_pattern = now.format("%Z").to_string();
        formatted.replace(&offset_pattern, "UTC")
    } else {
        now.format(formatstr).to_string()
    }
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
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

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
