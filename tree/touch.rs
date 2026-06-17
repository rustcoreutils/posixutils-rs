//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::{Datelike, Local, NaiveDate, NaiveDateTime, TimeZone};
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::ffi::CString;
use std::io;
use std::time::{SystemTime, UNIX_EPOCH};

/// touch - change file access and modification times
#[derive(Parser)]
#[command(version, about = gettext("touch - change file access and modification times"))]
struct Args {
    #[arg(short, long, help = gettext("Change the access time of file"))]
    access: bool,

    #[arg(short = 'c', long, help = gettext("Do not create a specified file if it does not exist"))]
    no_create: bool,

    #[arg(short, long, help = gettext("Change the modification time of file"))]
    mtime: bool,

    #[arg(short, long, group = "timefmt", help = gettext("Use the specified ISO 8601:2000 date-time format, instead of the current time"))]
    datetime: Option<String>,

    #[arg(short, long, group = "timefmt", help = gettext("Use the specified POSIX [[CC]YY]MMDDhhmm[.SS] format, instead of the current time"))]
    time: Option<String>,

    #[arg(short, long, group = "timefmt", help = gettext("Use the corresponding time of the file named by the pathname ref_file instead of the current time"))]
    ref_file: Option<String>,

    #[arg(help = gettext("A pathname of a file whose times shall be modified"))]
    files: Vec<String>,
}

fn mk_ts(sec: i64, nsec: u32) -> libc::timespec {
    libc::timespec {
        tv_sec: sec as libc::time_t,
        tv_nsec: nsec as libc::c_long,
    }
}

fn omit() -> libc::timespec {
    libc::timespec {
        tv_sec: 0,
        tv_nsec: libc::UTIME_OMIT,
    }
}

fn now() -> libc::timespec {
    libc::timespec {
        tv_sec: 0,
        tv_nsec: libc::UTIME_NOW,
    }
}

fn systemtime_to_ts(t: SystemTime) -> libc::timespec {
    match t.duration_since(UNIX_EPOCH) {
        Ok(d) => mk_ts(d.as_secs() as i64, d.subsec_nanos()),
        Err(e) => {
            // Before the epoch (rare for a real reference file).
            let d = e.duration();
            mk_ts(-(d.as_secs() as i64), 0)
        }
    }
}

/// Parse the `-d` extended ISO-8601 form. Accepts `T` or a space separator, `.`/`,` fractional
/// seconds, an optional `Z`/numeric timezone (else the value is interpreted in the local zone, so
/// `TZ` is honored).
fn parse_datetime(input: &str) -> Result<libc::timespec, String> {
    let norm = input.trim().replace(',', ".");

    // If a timezone is present (offset or `Z`), parse it as a fixed-offset instant. RFC 3339 needs
    // a `T`, so restore it for parsing.
    let with_t = if norm.contains(' ') && !norm.contains('T') {
        norm.replacen(' ', "T", 1)
    } else {
        norm.clone()
    };
    if let Ok(dt) = chrono::DateTime::parse_from_rfc3339(&with_t) {
        return Ok(mk_ts(dt.timestamp(), dt.timestamp_subsec_nanos()));
    }

    // Otherwise interpret the naive date-time in the local timezone.
    let body = norm.replacen('T', " ", 1);
    let body = body.trim();
    for fmt in [
        "%Y-%m-%d %H:%M:%S%.f",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
    ] {
        if let Ok(naive) = NaiveDateTime::parse_from_str(body, fmt) {
            if let Some(dt) = Local.from_local_datetime(&naive).single() {
                return Ok(mk_ts(dt.timestamp(), dt.timestamp_subsec_nanos()));
            }
        }
    }

    Err(gettext!("invalid date format: '{}'", input))
}

/// Parse the `-t` POSIX `[[CC]YY]MMDDhhmm[.SS]` form, interpreted in the local timezone.
fn parse_posix_time(input: &str) -> Result<libc::timespec, String> {
    let (date_str, secs) = match input.split_once('.') {
        Some((d, s)) => {
            let s: u32 = s
                .parse()
                .map_err(|_| gettext!("invalid time format: '{}'", input))?;
            (d, s)
        }
        None => (input, 0),
    };

    if !date_str.bytes().all(|b| b.is_ascii_digit()) {
        return Err(gettext!("invalid time format: '{}'", input));
    }

    let g = |a: usize, b: usize| date_str[a..b].parse::<u32>().unwrap();
    let (year, month, day, hour, minute) = match date_str.len() {
        8 => (Local::now().year(), g(0, 2), g(2, 4), g(4, 6), g(6, 8)),
        10 => {
            let yy = g(0, 2);
            let year = if yy <= 68 { 2000 + yy } else { 1900 + yy } as i32;
            (year, g(2, 4), g(4, 6), g(6, 8), g(8, 10))
        }
        12 => (g(0, 4) as i32, g(4, 6), g(6, 8), g(8, 10), g(10, 12)),
        _ => return Err(gettext!("invalid time format: '{}'", input)),
    };

    // POSIX allows SS in [00,60]; clamp a leap second to 59 (chrono has no plain leap instant).
    let secs = secs.min(59);

    let naive = NaiveDate::from_ymd_opt(year, month, day)
        .and_then(|d| d.and_hms_opt(hour, minute, secs))
        .ok_or_else(|| gettext!("invalid time: '{}'", input))?;
    let dt = Local
        .from_local_datetime(&naive)
        .single()
        .ok_or_else(|| gettext!("invalid time: '{}'", input))?;
    Ok(mk_ts(dt.timestamp(), 0))
}

/// The (atime, mtime) the time options request. For `-r`, the two come from the reference file's
/// corresponding fields; for `-d`/`-t` both are the parsed instant; otherwise both are "now".
fn time_source(args: &Args) -> Result<(libc::timespec, libc::timespec), String> {
    if let Some(d) = &args.datetime {
        let ts = parse_datetime(d)?;
        Ok((ts, ts))
    } else if let Some(t) = &args.time {
        let ts = parse_posix_time(t)?;
        Ok((ts, ts))
    } else if let Some(rf) = &args.ref_file {
        let md = std::fs::metadata(rf).map_err(|e| format!("{rf}: {e}"))?;
        let atime = md
            .accessed()
            .map(systemtime_to_ts)
            .unwrap_or_else(|_| now());
        let mtime = md
            .modified()
            .map(systemtime_to_ts)
            .unwrap_or_else(|_| now());
        Ok((atime, mtime))
    } else {
        Ok((now(), now()))
    }
}

fn touch_file(
    args: &Args,
    source: &(libc::timespec, libc::timespec),
    filename: &str,
) -> io::Result<()> {
    let c_path =
        CString::new(filename).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;

    let exists = std::fs::symlink_metadata(filename).is_ok();
    if !exists {
        if args.no_create {
            // POSIX -c: do not create, and write no diagnostic; exit success.
            return Ok(());
        }
        // Create an empty file (without truncating — it does not exist yet).
        let fd = unsafe {
            libc::open(
                c_path.as_ptr(),
                libc::O_CREAT | libc::O_WRONLY,
                0o666 as libc::c_int,
            )
        };
        if fd < 0 {
            return Err(io::Error::last_os_error());
        }
        unsafe { libc::close(fd) };
    }

    // Set only the requested field(s); leave the other unchanged (UTIME_OMIT). On a just-created
    // file the omitted field keeps its creation-time value.
    let atime = if args.access { source.0 } else { omit() };
    let mtime = if args.mtime { source.1 } else { omit() };
    let times = [atime, mtime];

    let ret = unsafe { libc::utimensat(libc::AT_FDCWD, c_path.as_ptr(), times.as_ptr(), 0) };
    if ret != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    // Default to changing both access and modification times.
    if !args.access && !args.mtime {
        args.access = true;
        args.mtime = true;
    }

    let source = match time_source(&args) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("touch: {e}");
            std::process::exit(1);
        }
    };

    let mut exit_code = 0;
    for filename in &args.files {
        if let Err(e) = touch_file(&args, &source, filename) {
            exit_code = 1;
            eprintln!("touch: {filename}: {e}");
        }
    }

    std::process::exit(exit_code)
}
