//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::{DateTime, Datelike, Local, LocalResult, TimeZone, Utc};
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::ffi::CString;
use std::io::{self, Write};
use std::mem::MaybeUninit;
use std::process;

const DEF_TIMESTR: &str = "%a %b %e %H:%M:%S %Z %Y";

/// Upper bound for the `strftime` output buffer. A zero return at this size is
/// treated as a legitimately-empty conversion rather than a buffer overflow.
const STRFTIME_BUF_MAX: usize = 64 * 1024;

/// Map a 2-digit year to a full year per POSIX: values in [00,68] refer to
/// 2000–2068, and values in [69,99] refer to 1969–1999.
fn infer_century(yy: i32) -> i32 {
    if yy < 69 {
        yy + 2000
    } else {
        yy + 1900
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

fn show_time(utc: bool, formatstr: &str) {
    if formatstr.is_empty() {
        println!();
        return;
    }

    let c_format = match CString::new(formatstr) {
        Ok(s) => s,
        Err(_) => {
            eprintln!("date: format string contains NUL byte");
            process::exit(1);
        }
    };

    let now = unsafe { libc::time(std::ptr::null_mut()) };
    if now == -1 {
        eprintln!("date: failed to get current time");
        process::exit(1);
    }
    let mut tm = MaybeUninit::<libc::tm>::uninit();

    let tm_ptr = unsafe {
        if utc {
            libc::gmtime_r(&now, tm.as_mut_ptr())
        } else {
            libc::localtime_r(&now, tm.as_mut_ptr())
        }
    };

    if tm_ptr.is_null() {
        eprintln!("date: failed to get current time");
        process::exit(1);
    }

    let tm = unsafe { tm.assume_init() };

    // Grow the buffer until strftime succeeds. strftime returns 0 both when the
    // buffer is too small AND when the conversion is legitimately empty; once
    // the buffer is provably large enough (STRFTIME_BUF_MAX), a 0 return can
    // only mean an empty result, which is valid — emit just the trailing
    // <newline> rather than treating it as an error.
    let mut buf_size = 256;
    loop {
        let mut buf = vec![0u8; buf_size];
        let len = unsafe {
            libc::strftime(
                buf.as_mut_ptr() as *mut libc::c_char,
                buf.len(),
                c_format.as_ptr(),
                &tm,
            )
        };
        if len > 0 {
            // Write the raw bytes so non-UTF-8 locale output is preserved.
            let mut out = io::stdout().lock();
            let _ = out.write_all(&buf[..len]);
            let _ = out.write_all(b"\n");
            return;
        }
        if buf_size >= STRFTIME_BUF_MAX {
            // Empty-but-valid conversion: a <newline> shall always be appended.
            let _ = io::stdout().write_all(b"\n");
            return;
        }
        buf_size *= 2;
    }
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
            let year = infer_century(timestr[8..10].parse::<i32>().unwrap());
            (year, month, day, hour, minute)
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

#[cfg(test)]
mod tests {
    use super::infer_century;

    #[test]
    fn century_boundaries() {
        // POSIX: [00,68] -> 2000..2068, [69,99] -> 1969..1999.
        assert_eq!(infer_century(0), 2000);
        assert_eq!(infer_century(68), 2068);
        assert_eq!(infer_century(69), 1969);
        assert_eq!(infer_century(70), 1970);
        assert_eq!(infer_century(99), 1999);
    }
}
