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
            // Use earliest() to handle DST transitions consistently, with latest() as fallback
            let local_result = tz.from_local_datetime(&dt.naive_local());
            let dt_tz = local_result.earliest().or_else(|| local_result.latest());
            if let Some(dt_tz) = dt_tz {
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

/// Parse a format string and replace %Z with the provided timezone abbreviation
/// This function handles escaped %% sequences properly
fn parse_format_string_with_tz(formatstr: &str, tz_abbr: &str) -> String {
    let mut result = String::new();
    let mut chars = formatstr.chars().peekable();
    
    while let Some(ch) = chars.next() {
        if ch == '%' {
            if let Some(&next_ch) = chars.peek() {
                if next_ch == '%' {
                    // Preserve %% so chrono can later convert it to a single %
                    result.push('%');
                    result.push('%');
                    chars.next(); // consume the second %
                    continue;
                } else if next_ch == 'Z' {
                    // Replace %Z with the timezone abbreviation
                    result.push_str(tz_abbr);
                    chars.next(); // consume 'Z'
                    continue;
                }
            }
        }
        result.push(ch);
    }
    
    result
}

/// Format a datetime string, replacing %Z with proper timezone abbreviation
fn format_with_timezone_local(formatstr: &str, dt: &DateTime<Local>) -> String {
    if formatstr.contains("%Z") {
        let tz_abbr = get_timezone_abbreviation(dt);
        let modified_format = parse_format_string_with_tz(formatstr, &tz_abbr);
        dt.format(&modified_format).to_string()
    } else {
        dt.format(formatstr).to_string()
    }
}

/// Format a datetime string for UTC, replacing %Z with "UTC"
fn format_with_timezone_utc(formatstr: &str, dt: &DateTime<Utc>) -> String {
    if formatstr.contains("%Z") {
        let modified_format = parse_format_string_with_tz(formatstr, "UTC");
        dt.format(&modified_format).to_string()
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
    format_with_timezone_local(formatstr, &now)
}

fn show_time_utc(formatstr: &str) -> String {
    let now = chrono::Utc::now();
    format_with_timezone_utc(formatstr, &now)
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
