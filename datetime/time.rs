//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, Write};
use std::process::{Command, Stdio};
use std::time::Instant;

use clap::Parser;
use gettextrs::{
    LocaleCategory, bind_textdomain_codeset, bindtextdomain, gettext, setlocale, textdomain,
};

#[derive(Parser)]
#[command(
    version,
    about = gettext("time - time a simple command or give resource usage"),
    help_template = gettext("{about}\n\nUsage: {usage}\n\nArguments:\n{positionals}\n\nOptions:\n{options}"),
    disable_help_flag = true,
    disable_version_flag = true,
)]
struct Args {
    #[arg(
        short,
        long,
        help = gettext("Write timing output to standard error in POSIX format")
    )]
    posix: bool,

    #[arg(help = gettext("The utility to be invoked"))]
    utility: String,

    #[arg(
        name = "ARGUMENT",
        trailing_var_arg = true,
        help = gettext("Arguments for the utility")
    )]
    arguments: Vec<String>,

    #[arg(short, long, help = gettext("Print help"), action = clap::ArgAction::HelpLong)]
    help: Option<bool>,

    #[arg(short = 'V', long, help = gettext("Print version"), action = clap::ArgAction::Version)]
    version: Option<bool>,
}

enum TimeError {
    ExecCommand(String),
    ExecTime,
    CommandNotFound(String),
}

fn time(args: Args) -> Result<(), TimeError> {
    let start_time = Instant::now();
    // SAFETY: std::mem::zeroed() is used to create an instance of libc::tms with all fields set to zero.
    // This is safe here because libc::tms is a Plain Old Data type, and zero is a valid value for all its fields.
    let mut tms_start: libc::tms = unsafe { std::mem::zeroed() };
    // SAFETY: sysconf is a POSIX function that returns the number of clock ticks per second.
    // It is safe to call because it does not modify any memory and has no side effects.
    let clock_ticks_per_second = unsafe { libc::sysconf(libc::_SC_CLK_TCK) as f64 };

    // SAFETY: times is a POSIX function that fills the provided tms structure with time-accounting information.
    // It is safe to call because we have correctly allocated and initialized tms_start, and the function
    // only writes to this structure.
    unsafe { libc::times(&mut tms_start) };

    let mut child = Command::new(&args.utility)
        .args(args.arguments)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        .map_err(|e| match e.kind() {
            io::ErrorKind::NotFound => TimeError::CommandNotFound(args.utility),
            _ => TimeError::ExecCommand(args.utility),
        })?;

    let _ = child.wait().map_err(|_| TimeError::ExecTime)?;

    let elapsed = start_time.elapsed();
    let tms_end: libc::tms = unsafe { std::mem::zeroed() };

    let user_time = (tms_start.tms_utime - tms_end.tms_utime) as f64 / clock_ticks_per_second;
    let system_time = (tms_start.tms_stime - tms_end.tms_stime) as f64 / clock_ticks_per_second;

    if args.posix {
        writeln!(
            io::stderr(),
            "real {:.6}\nuser {:.6}\nsys {:.6}",
            elapsed.as_secs_f64(),
            user_time,
            system_time
        )
        .map_err(|_| TimeError::ExecTime)?;
    } else {
        writeln!(
            io::stderr(),
            "Elapsed time: {:.6} seconds\nUser time: {:.6} seconds\nSystem time: {:.6} seconds",
            elapsed.as_secs_f64(),
            user_time,
            system_time
        )
        .map_err(|_| TimeError::ExecTime)?;
    }

    Ok(())
}

enum Status {
    Ok,
    TimeError,
    UtilError,
    UtilNotFound,
}

impl Status {
    fn exit(self) -> ! {
        let res = match self {
            Status::Ok => 0,
            Status::TimeError => 1,
            Status::UtilError => 126,
            Status::UtilNotFound => 127,
        };

        std::process::exit(res)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bindtextdomain("posixutils-rs", "locale")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    if let Err(err) = time(args) {
        match err {
            TimeError::CommandNotFound(err) => {
                eprintln!("Command not found: {}", err);
                Status::UtilNotFound.exit()
            }
            TimeError::ExecCommand(err) => {
                eprintln!("Error while executing command: {}", err);
                Status::UtilError.exit()
            }
            TimeError::ExecTime => {
                eprintln!("Error while executing time utility");
                Status::TimeError.exit()
            }
        }
    }

    Status::Ok.exit()
}
