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
use modestr::ChmodMode;
use plib::modestr;
use std::io;

/// mkfifo - make FIFO special files
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Set the file permission bits of the newly-created FIFO to the specified mode value.
    #[arg(short, long)]
    mode: Option<String>,

    /// A pathname of the FIFO special file to be created.
    files: Vec<String>,
}

fn do_mkfifo(filename: &str, mode: &ChmodMode) -> io::Result<()> {
    let mode_val = match mode {
        ChmodMode::Absolute(mode, _) => *mode,
        ChmodMode::Symbolic(sym) => modestr::mutate(0o666, false, sym),
    };

    let res = unsafe { libc::mkfifo(filename.as_ptr() as *const i8, mode_val as libc::mode_t) };
    if res < 0 {
        return Err(io::Error::last_os_error());
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    // parse the mode string
    let mode = match args.mode {
        Some(mode) => modestr::parse(&mode)?,
        None => ChmodMode::Absolute(0o666, 3),
    };

    // apply the mode to each file
    for filename in &args.files {
        if let Err(e) = do_mkfifo(filename, &mode) {
            exit_code = 1;
            eprintln!("{}: {}", filename, e);
        }
    }

    std::process::exit(exit_code)
}
