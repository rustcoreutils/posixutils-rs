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
use gettextrs::gettext;
use plib::{date_arg, diag};
#[cfg(unix)]
use std::ffi::CString;
use std::io::{self, Write};
#[cfg(unix)]
use std::mem::MaybeUninit;
use std::process;

const DEF_TIMESTR: &str = "%a %b %e %H:%M:%S %Z %Y";

/// Upper bound for the `strftime` output buffer. A zero return at this size is
/// treated as a legitimately-empty conversion rather than a buffer overflow.
#[cfg(unix)]
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
        short,
        long,
        allow_hyphen_values = true,
        help = gettext(
            "Display the given time instead of the current time: an ISO 8601 date-time, \
             an RFC 5322 date as printed by 'date -R', or @SECONDS"
        )
    )]
    date: Option<String>,

    #[arg(
        help = gettext(
            "If prefixed with '+', Display the current time in the given FORMAT, \
             as in strftime(3). Otherwise, set the current time to the given string"
        )
    )]
    timestr: Option<String>,
}

/// The current time, in seconds since the Epoch.
fn current_time() -> libc::time_t {
    let now = unsafe { libc::time(std::ptr::null_mut()) };
    if now == -1 {
        diag::error(&gettext("failed to get current time"));
        process::exit(1);
    }
    now
}

/// Write `when` formatted by `formatstr`, in UTC or local time.
fn show_time(when: libc::time_t, utc: bool, formatstr: &str) {
    if formatstr.is_empty() {
        println!();
        return;
    }

    match format_time(when, utc, formatstr) {
        Ok(text) => {
            // Write the raw bytes so non-UTF-8 locale output is preserved.
            let mut out = io::stdout().lock();
            let _ = out.write_all(&text);
            let _ = out.write_all(b"\n");
        }
        Err(msg) => {
            diag::error(&gettext(msg));
            process::exit(1);
        }
    }
}

/// `now` formatted by `formatstr` with the C library's `strftime`, in UTC or
/// local time.
#[cfg(unix)]
fn format_time(now: libc::time_t, utc: bool, formatstr: &str) -> Result<Vec<u8>, &'static str> {
    let c_format = CString::new(formatstr).map_err(|_| "format string contains NUL byte")?;

    let mut tm = MaybeUninit::<libc::tm>::uninit();

    let tm_ptr = unsafe {
        if utc {
            libc::gmtime_r(&now, tm.as_mut_ptr())
        } else {
            libc::localtime_r(&now, tm.as_mut_ptr())
        }
    };

    if tm_ptr.is_null() {
        return Err("failed to get current time");
    }

    let tm = unsafe { tm.assume_init() };

    // strftime returns 0 both when the buffer is too small AND when the
    // conversion is legitimately empty (e.g. %Z with no zone abbreviation).
    // Disambiguate with a first-byte sentinel: on any success — including an
    // empty result — strftime writes a terminating NUL at offset 0, whereas a
    // too-small buffer leaves the sentinel (or partial content) in place. So a
    // 0 return with buf[0] == 0 is an empty-but-valid conversion, and a 0
    // return with buf[0] != 0 means the output did not fit and we grow.
    let mut buf_size = 256;
    loop {
        let mut buf = vec![0u8; buf_size];
        buf[0] = 1;
        let len = unsafe {
            libc::strftime(
                buf.as_mut_ptr() as *mut libc::c_char,
                buf.len(),
                c_format.as_ptr(),
                &tm,
            )
        };
        if len > 0 {
            buf.truncate(len);
            return Ok(buf);
        }
        if buf[0] == 0 {
            // Empty-but-valid conversion: a <newline> shall still be appended.
            return Ok(Vec::new());
        }
        // Output did not fit; grow and retry, capped to guard against a
        // pathologically large format silently allocating unbounded memory.
        if buf_size >= STRFTIME_BUF_MAX {
            return Err("formatted output exceeds internal buffer limit");
        }
        buf_size *= 2;
    }
}

/// `now` formatted by `formatstr` in UTC or local time; see `plib::timefmt`.
#[cfg(windows)]
fn format_time(now: libc::time_t, utc: bool, formatstr: &str) -> Result<Vec<u8>, &'static str> {
    plib::timefmt::format_time(formatstr, now, utc)
        .map(String::into_bytes)
        .map_err(|_| "failed to format the time")
}

fn set_time(utc: bool, timestr: &str) -> Result<(), &'static str> {
    for ch in timestr.chars() {
        if !ch.is_ascii_digit() {
            return Err("invalid date");
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
            return Err("invalid date");
        }
    };

    // calculate system time
    let new_time = {
        if utc {
            match chrono::Utc.with_ymd_and_hms(year, month, day, hour, minute, 0) {
                LocalResult::<DateTime<Utc>>::Single(t) => t.timestamp(),
                _ => {
                    return Err("invalid date");
                }
            }
        } else {
            match chrono::Local.with_ymd_and_hms(year, month, day, hour, minute, 0) {
                LocalResult::<DateTime<Local>>::Single(t) => t.timestamp(),
                _ => {
                    return Err("invalid date");
                }
            }
        }
    };

    set_clock(new_time)
}

/// Set the system clock to `secs` seconds since the Epoch.
#[cfg(unix)]
fn set_clock(secs: i64) -> Result<(), &'static str> {
    let new_time = libc::timespec {
        tv_sec: secs,
        tv_nsec: 0,
    };

    unsafe {
        if libc::clock_settime(libc::CLOCK_REALTIME, &new_time) != 0 {
            return Err("failed to set time");
        }
    }

    Ok(())
}

/// Set the system clock to `secs` seconds since the Epoch: `SetSystemTime`,
/// which needs the system-time privilege.
#[cfg(windows)]
fn set_clock(secs: i64) -> Result<(), &'static str> {
    use chrono::Timelike;

    #[repr(C)]
    struct SystemTime {
        year: u16,
        month: u16,
        day_of_week: u16,
        day: u16,
        hour: u16,
        minute: u16,
        second: u16,
        milliseconds: u16,
    }

    #[link(name = "kernel32")]
    extern "system" {
        fn SetSystemTime(time: *const SystemTime) -> i32;
    }

    let t = DateTime::from_timestamp(secs, 0).ok_or("invalid date")?;
    let field = |v: u32| u16::try_from(v).map_err(|_| "invalid date");
    let new_time = SystemTime {
        year: u16::try_from(t.year()).map_err(|_| "invalid date")?,
        month: field(t.month())?,
        day_of_week: field(t.weekday().num_days_from_sunday())?,
        day: field(t.day())?,
        hour: field(t.hour())?,
        minute: field(t.minute())?,
        second: field(t.second())?,
        milliseconds: 0,
    };
    // SAFETY: new_time is a valid SYSTEMTIME for the duration of the call.
    if unsafe { SetSystemTime(&new_time) } == 0 {
        return Err("failed to set time");
    }
    Ok(())
}

/// -d: write the time `date` names, in the operand's format if there is one.
/// The operand cannot set the clock then, so it must be a `+format`.
fn show_given_time(args: &Args, date: &str) {
    let formatstr = match args.timestr.as_deref() {
        None => DEF_TIMESTR,
        Some(timestr) => match timestr.strip_prefix('+') {
            Some(formatstr) => formatstr,
            None => {
                diag::error(&gettext!(
                    "the argument '{}' lacks a leading '+'; with -d, an operand must be a format",
                    timestr
                ));
                process::exit(1);
            }
        },
    };

    let zoneless = if args.utc {
        date_arg::Zoneless::Utc
    } else {
        date_arg::Zoneless::Local
    };
    let when = match date_arg::parse(date, zoneless) {
        Ok((secs, _)) => libc::time_t::try_from(secs).ok(),
        Err(msg) => {
            diag::error(&msg);
            process::exit(1);
        }
    };
    let Some(when) = when else {
        diag::error(&gettext!("invalid date format: '{}'", date));
        process::exit(1);
    };
    show_time(when, args.utc, formatstr);
}

fn main() {
    diag::init_locale("date");

    let args = Args::parse();

    if let Some(date) = &args.date {
        show_given_time(&args, date);
        return;
    }

    match &args.timestr {
        None => show_time(current_time(), args.utc, DEF_TIMESTR),
        Some(timestr) => {
            if let Some(st) = timestr.strip_prefix("+") {
                show_time(current_time(), args.utc, st);
            } else if let Err(msg) = set_time(args.utc, timestr) {
                diag::error(&gettext(msg));
                process::exit(1);
            }
        }
    }
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
