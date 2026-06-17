//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};

use clap::Parser;
use gettextrs::gettext;
use plib::diag;
use posixutils_process::exec::exec_error_exit;

#[derive(Parser)]
#[command(
    version,
    about = gettext("nice - invoke a utility with an altered nice value")
)]
struct Args {
    #[arg(
        short = 'n',
        default_value_t = 10,
        allow_hyphen_values = true,
        help = gettext(
            "A positive or negative decimal integer which shall have \
             the same effect on the execution of the utility as if the \
             utility had called the nice() function with the numeric \
             value of the increment option-argument"
        )
    )]
    niceval: i32,

    #[arg(help = gettext("Utility to invoke"))]
    util: String,

    #[arg(help = gettext("Utility arguments"))]
    util_args: Vec<String>,
}

#[cfg(target_os = "linux")]
unsafe fn errno_ptr() -> *mut libc::c_int {
    libc::__errno_location()
}

#[cfg(not(target_os = "linux"))]
unsafe fn errno_ptr() -> *mut libc::c_int {
    libc::__error()
}

/// Apply the nice increment. Per POSIX, if the nice value cannot be changed
/// (e.g. lack of privilege), a warning may be written but this "shall not
/// prevent the invocation of utility or affect the exit status" — so this
/// never aborts.
fn apply_increment(increment: i32) {
    // nice(3) returns the new nice value (which can legitimately be -1), so the
    // return value cannot be used to detect failure. Clear errno, call, then
    // inspect errno.
    unsafe {
        *errno_ptr() = 0;
        let _ = libc::nice(increment);
        let errno = *errno_ptr();
        if errno != 0 {
            let e = io::Error::from_raw_os_error(errno);
            // Non-fatal: warn and continue to exec the utility anyway.
            diag::warning(&format!("{}: {}", gettext("cannot set nice value"), e));
        }
    }
}

fn exec_util(util: &str, util_args: &[String]) -> ! {
    let err = Command::new(util)
        .args(util_args)
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .exec();

    // exec() only returns on failure.
    exec_error_exit(util, err)
}

fn main() {
    diag::init_locale("nice");

    let args = Args::parse();

    apply_increment(args.niceval);

    exec_util(&args.util, &args.util_args);
}
