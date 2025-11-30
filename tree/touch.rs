//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::{DateTime, Datelike, LocalResult, TimeZone, Utc};
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};

/// touch - change file access and modification times
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Change the access time of file.
    #[arg(short, long)]
    access: bool,

    /// Do not create a specified file if it does not exist.
    #[arg(short = 'c', long)]
    no_create: bool,

    /// Change the modification time of file.
    #[arg(short, long)]
    mtime: bool,

    /// Use the specified ISO 8601:2000 date-time format, instead of the current time.
    #[arg(short, long, group = "timefmt")]
    datetime: Option<String>,

    /// Use the specified POSIX [[CC]YY]MMDDhhmm[.SS] format, instead of the current time.
    #[arg(short, long, group = "timefmt")]
    time: Option<String>,

    /// Use the corresponding time of the file named by the pathname ref_file instead of the current time.
    #[arg(short, long, group = "timefmt")]
    ref_file: Option<String>,

    /// A pathname of a file whose times shall be modified.
    files: Vec<String>,
}

fn parse_tm_iso(time: &str) -> Result<DateTime<Utc>, Box<dyn std::error::Error>> {
    let dt = DateTime::parse_from_rfc3339(time)?;
    Ok(dt.into())
}

fn parse_tm_posix(time: &str) -> Result<DateTime<Utc>, Box<dyn std::error::Error>> {
    let mut time = String::from(time);
    let mut seconds = String::from("0");

    // split into YYYYMMDDhhmm and [.SS] components
    let mut tmp_time = String::new();
    if let Some((t, secs)) = time.split_once('.') {
        tmp_time = t.to_string();
        seconds = secs.to_string();
    }
    if !tmp_time.is_empty() {
        time = tmp_time;
    }

    // extract date and time elements, with length implying format
    let tmp_year;
    let (year_str, month_str, day_str, hour_str, minute_str) = match time.len() {
        // format: MMDDhhmm[.SS]
        8 => {
            tmp_year = Utc::now().year().to_string();
            (
                tmp_year.as_str(),
                &time[0..2],
                &time[2..4],
                &time[4..6],
                &time[6..8],
            )
        }

        // format: YYMMDDhhmm[.SS]
        10 => {
            let mut yearling = time[0..2].parse::<u32>()?;
            if yearling <= 68 {
                yearling += 2000;
            } else {
                yearling += 1900;
            }
            tmp_year = yearling.to_string();
            (
                tmp_year.as_str(),
                &time[2..4],
                &time[4..6],
                &time[6..8],
                &time[8..10],
            )
        }

        // format: YYYYMMDDhhmm[.SS]
        12 => (
            &time[0..4],
            &time[4..6],
            &time[6..8],
            &time[8..10],
            &time[10..12],
        ),
        _ => {
            return Err("Invalid time format".into());
        }
    };

    // convert strings to integers
    let year = year_str.parse::<i32>()?;
    let month = month_str.parse::<u32>()?;
    let day = day_str.parse::<u32>()?;
    let hour = hour_str.parse::<u32>()?;
    let minute = minute_str.parse::<u32>()?;
    let secs = seconds.parse::<u32>()?;

    // convert to DateTime and validate input
    let res = Utc.with_ymd_and_hms(year, month, day, hour, minute, secs);
    if res == LocalResult::None {
        return Err("Invalid time".into());
    }

    // return parsed date
    let dt = res.unwrap();
    Ok(dt)
}

fn parse_tm_ref_file(filename: &str) -> Result<DateTime<Utc>, Box<dyn std::error::Error>> {
    let metadata = std::fs::metadata(filename)?;
    let timespec = metadata.modified()?;
    Ok(DateTime::from(timespec))
}

fn touch_file_new(time: libc::time_t, filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    // open file for writing, creating if necessary
    let flags = libc::O_CREAT | libc::O_WRONLY | libc::O_TRUNC;
    let fd = unsafe { libc::open(filename.as_ptr() as *const libc::c_char, flags, 0o666) };
    if fd < 0 {
        return Err("Failed to open file".into());
    }

    // configure file times array
    let times = [
        libc::timeval {
            tv_sec: time,
            tv_usec: 0,
        },
        libc::timeval {
            tv_sec: time,
            tv_usec: 0,
        },
    ];

    // set file times
    if unsafe { libc::futimes(fd, times.as_ptr()) } < 0 {
        return Err("Failed to change file times".into());
    }

    // close file
    if unsafe { libc::close(fd) } < 0 {
        return Err("Failed to close file".into());
    }

    Ok(())
}

fn touch_file_existing(
    args: &Args,
    time: libc::time_t,
    filename: &str,
    md: std::fs::Metadata,
) -> Result<(), Box<dyn std::error::Error>> {
    // configure access and modification times
    let atime = if args.access {
        libc::timeval {
            tv_sec: time,
            tv_usec: 0,
        }
    } else {
        libc::timeval {
            tv_sec: md
                .accessed()?
                .duration_since(std::time::UNIX_EPOCH)?
                .as_secs() as libc::time_t,
            tv_usec: 0,
        }
    };

    let mtime = if args.mtime {
        libc::timeval {
            tv_sec: time,
            tv_usec: 0,
        }
    } else {
        libc::timeval {
            tv_sec: md
                .modified()?
                .duration_since(std::time::UNIX_EPOCH)?
                .as_secs() as libc::time_t,
            tv_usec: 0,
        }
    };

    // configure file times array
    let times = [
        libc::timeval {
            tv_sec: atime.tv_sec,
            tv_usec: atime.tv_usec,
        },
        libc::timeval {
            tv_sec: mtime.tv_sec,
            tv_usec: mtime.tv_usec,
        },
    ];

    // set file times
    if unsafe { libc::utimes(filename.as_ptr() as *const libc::c_char, times.as_ptr()) } < 0 {
        return Err("Failed to change file times".into());
    }

    Ok(())
}

fn touch_file(
    args: &Args,
    timespec: &DateTime<Utc>,
    filename: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    // convert timespec to time_t
    let time = timespec.timestamp() as libc::time_t;

    // check if file exists, and dispatch based on that
    match std::fs::metadata(filename) {
        Ok(md) => touch_file_existing(args, time, filename, md),
        Err(_) => {
            if args.no_create {
                return Err("File does not exist".into());
            }
            touch_file_new(time, filename)
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    // default to changing both access and modification times
    if !args.access && !args.mtime {
        args.access = true;
        args.mtime = true;
    }

    // parse time format, or default to current time
    let timespec: DateTime<Utc> = {
        if let Some(datetime) = &args.datetime {
            parse_tm_iso(datetime)?
        } else if let Some(time) = &args.time {
            parse_tm_posix(time)?
        } else if let Some(ref_file) = &args.ref_file {
            parse_tm_ref_file(ref_file)?
        } else {
            Utc::now()
        }
    };

    let mut exit_code = 0;

    // touch each file
    for filename in &args.files {
        if let Err(e) = touch_file(&args, &timespec, filename) {
            exit_code = 1;
            eprintln!("{}: {}", filename, e);
        }
    }

    std::process::exit(exit_code)
}
