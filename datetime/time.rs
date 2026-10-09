//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, Write};
use std::process::{Child, Command, ExitStatus, Stdio};
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

    // XBD 12.2 Guideline 9: time's options all precede the utility, so the
    // utility name and every word after it are one trailing operand list.
    // With the utility as a positional of its own, clap went on parsing
    // options after it: `time echo -p x` took `-p` as time's own.
    #[arg(
        value_name = "UTILITY",
        required = true,
        trailing_var_arg = true,
        help = gettext("The utility to be invoked and its arguments")
    )]
    command: Vec<String>,

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

/// Run the utility named by `args.command`, write timing statistics to
/// standard error, and return the exit code that `time` itself should exit
/// with (the utility's exit status, per POSIX EXIT STATUS).
fn time(args: Args) -> Result<i32, TimeError> {
    let start_time = Instant::now();
    let cpu_start = CpuStart::now();

    let (utility, arguments) = args
        .command
        .split_first()
        .expect("clap requires the utility operand");

    let mut child = Command::new(utility)
        .args(arguments)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        .map_err(|e| match e.kind() {
            io::ErrorKind::NotFound => TimeError::CommandNotFound(utility.clone()),
            _ => TimeError::ExecCommand(utility.clone()),
        })?;

    let status = child.wait().map_err(|_| TimeError::ExecTime)?;

    let elapsed = start_time.elapsed();
    let (user_time, system_time) = cpu_start.used(&child);

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
    Ok(exit_code(status))
}

/// This process's CPU times before the utility ran: `times(2)`, whose
/// children's fields take in the utility's usage once it has been waited for.
#[cfg(unix)]
struct CpuStart(libc::tms);

#[cfg(unix)]
impl CpuStart {
    fn now() -> Self {
        // SAFETY: tms is plain data, and times only writes to it.
        let mut tms: libc::tms = unsafe { std::mem::zeroed() };
        unsafe { libc::times(&mut tms) };
        CpuStart(tms)
    }

    /// User and system CPU seconds used since `now`, by this process and the
    /// waited-for utility.
    fn used(&self, _child: &Child) -> (f64, f64) {
        let start = &self.0;
        let end = CpuStart::now().0;
        // SAFETY: sysconf has no side effects.
        let ticks_per_second = unsafe { libc::sysconf(libc::_SC_CLK_TCK) as f64 };

        // POSIX: User CPU time is the sum of tms_utime and tms_cutime, System
        // CPU time the sum of tms_stime and tms_cstime, for the process in
        // which the utility is executed.
        let user = (end.tms_utime + end.tms_cutime) - (start.tms_utime + start.tms_cutime);
        let system = (end.tms_stime + end.tms_cstime) - (start.tms_stime + start.tms_cstime);
        (
            user as f64 / ticks_per_second,
            system as f64 / ticks_per_second,
        )
    }
}

/// Process CPU times, user then kernel, in 100-nanosecond units.
#[cfg(windows)]
fn process_times(process: std::os::windows::io::RawHandle) -> (u64, u64) {
    #[link(name = "kernel32")]
    extern "system" {
        fn GetProcessTimes(
            process: std::os::windows::io::RawHandle,
            creation: *mut u64,
            exit: *mut u64,
            kernel: *mut u64,
            user: *mut u64,
        ) -> i32;
    }
    let (mut creation, mut exit, mut kernel, mut user) = (0, 0, 0, 0);
    // SAFETY: each pointer is a FILETIME-sized out parameter; a failed call
    // leaves them zero.
    unsafe { GetProcessTimes(process, &mut creation, &mut exit, &mut kernel, &mut user) };
    (user, kernel)
}

/// This process's CPU times before the utility ran. Windows keeps no
/// children's totals, so the utility's own times are read from its handle.
#[cfg(windows)]
struct CpuStart((u64, u64));

#[cfg(windows)]
impl CpuStart {
    fn now() -> Self {
        CpuStart(process_times(Self::current_process()))
    }

    fn current_process() -> std::os::windows::io::RawHandle {
        #[link(name = "kernel32")]
        extern "system" {
            fn GetCurrentProcess() -> std::os::windows::io::RawHandle;
        }
        // SAFETY: returns a pseudo-handle; nothing to release.
        unsafe { GetCurrentProcess() }
    }

    /// User and system CPU seconds used since `now`, by this process and the
    /// waited-for utility.
    fn used(&self, child: &Child) -> (f64, f64) {
        use std::os::windows::io::AsRawHandle;
        const TICKS_PER_SECOND: f64 = 10_000_000.0;

        let (start_user, start_kernel) = self.0;
        let (end_user, end_kernel) = process_times(Self::current_process());
        let (child_user, child_kernel) = process_times(child.as_raw_handle());
        let user = end_user - start_user + child_user;
        let system = end_kernel - start_kernel + child_kernel;
        (
            user as f64 / TICKS_PER_SECOND,
            system as f64 / TICKS_PER_SECOND,
        )
    }
}

/// The utility's exit status; one terminated by a signal is reported as
/// 128 + the signal number.
#[cfg(unix)]
fn exit_code(status: ExitStatus) -> i32 {
    use std::os::unix::process::ExitStatusExt;
    match status.code() {
        Some(code) => code,
        None => 128 + status.signal().unwrap_or(0),
    }
}

/// The utility's exit status; a Windows process always has an exit code.
#[cfg(windows)]
fn exit_code(status: ExitStatus) -> i32 {
    status.code().unwrap_or(1)
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
