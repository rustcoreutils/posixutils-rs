//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::io;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};

/// nice - invoke a utility with an altered nice value
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// A positive or negative decimal integer which shall have the same effect on the execution of the utility as if the utility had called the nice() function with the numeric value of the increment option-argument.
    #[arg(short, long, default_value_t=10, value_parser = clap::value_parser!(i32).range(-30..30))]
    niceval: i32,

    /// utility to invoke
    util: String,

    /// utility arguments
    util_args: Vec<String>,
}

fn exec_util(util: &str, util_args: Vec<String>) -> io::Result<()> {
    Err(Command::new(util)
        .args(util_args)
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .exec())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let res = unsafe { libc::nice(args.niceval) };
    if res < 0 {
        let e = io::Error::last_os_error();
        eprintln!("nice: {}", e);
        return Err(Box::new(e));
    }

    exec_util(&args.util, args.util_args)?;

    Ok(())
}
