//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, Write};
use std::os::unix::process::ExitStatusExt;
use std::process::{Command, Stdio};
use std::time::Instant;

use clap::Parser;
use gettextrs::gettext;
use plib::diag;

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

/// Run `args.utility`, write timing statistics to standard error, and return
/// the exit code that `time` itself should exit with (the utility's exit
/// status, per POSIX EXIT STATUS).
fn time(args: Args) -> Result<i32, TimeError> {
    let start_time = Instant::now();
    // SAFETY: std::mem::zeroed() is used to create an instance of libc::tms with all fields set to zero.
    // This is safe here because libc::tms is a Plain Old Data type, and zero is a valid value for all its fields.
    let mut tms_start: libc::tms = unsafe { std::mem::zeroed() };
    // SAFETY: sysconf is a POSIX function that returns the number of clock ticks per second.
    // It is safe to call because it does not modify any memory and has no side effects.
    let clock_ticks_per_second = unsafe { libc::sysconf(libc::_SC_CLK_TCK) as f64 };

    // Snapshot the process's accumulated CPU times *before* spawning the child.
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

    let status = child.wait().map_err(|_| TimeError::ExecTime)?;

    let elapsed = start_time.elapsed();

    // Snapshot again *after* the child has been waited for, so the child's CPU
    // usage has been folded into this process's tms_cutime/tms_cstime fields.
    // SAFETY: same invariant as the tms_start call above.
    let mut tms_end: libc::tms = unsafe { std::mem::zeroed() };
    unsafe { libc::times(&mut tms_end) };

    // POSIX: User CPU time is the sum of tms_utime and tms_cutime, System CPU
    // time the sum of tms_stime and tms_cstime, for the process in which the
    // utility is executed. The child's usage is in the c* fields after wait().
    let user_ticks =
        (tms_end.tms_utime + tms_end.tms_cutime) - (tms_start.tms_utime + tms_start.tms_cutime);
    let system_ticks =
        (tms_end.tms_stime + tms_end.tms_cstime) - (tms_start.tms_stime + tms_start.tms_cstime);
    let user_time = user_ticks as f64 / clock_ticks_per_second;
    let system_time = system_ticks as f64 / clock_ticks_per_second;

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

    // EXIT STATUS: the exit status of time shall be the exit status of utility.
    // A child terminated by a signal is reported as 128 + signal number.
    let code = match status.code() {
        Some(code) => code,
        None => 128 + status.signal().unwrap_or(0),
    };
    Ok(code)
}

enum Status {
    /// The utility was invoked; exit with its exit status (per POSIX).
    Utility(i32),
    TimeError,
    UtilError,
    UtilNotFound,
}

impl Status {
    fn exit(self) -> ! {
        let res = match self {
            Status::Utility(code) => code,
            Status::TimeError => 1,
            Status::UtilError => 126,
            Status::UtilNotFound => 127,
        };

        std::process::exit(res)
    }
}

fn main() {
    diag::init_locale("time");

    let args = Args::parse();

    match time(args) {
        Ok(code) => Status::Utility(code).exit(),
        Err(err) => match err {
            TimeError::CommandNotFound(util) => {
                diag::error(&format!("{}: {}", gettext("utility not found"), util));
                Status::UtilNotFound.exit()
            }
            TimeError::ExecCommand(util) => {
                diag::error(&format!("{}: {}", gettext("cannot execute utility"), util));
                Status::UtilError.exit()
            }
            TimeError::ExecTime => {
                diag::error(&gettext("error running time utility"));
                Status::TimeError.exit()
            }
        },
    }
}
