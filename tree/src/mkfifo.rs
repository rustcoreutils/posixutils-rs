//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate libc;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use modestr::ChmodMode;
use plib::{modestr, PROJECT_NAME};
use std::io;

/// mkfifo - make FIFO special files
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Set the file permission bits of the newly-created FIFO to the specified mode value.
    #[arg(short, long)]
    mode: Option<String>,

    /// A pathname of the FIFO special file to be created.
    files: Vec<String>,
}

fn do_mkfifo(filename: &str, mode: &ChmodMode) -> io::Result<()> {
    let mode_val = match mode {
        ChmodMode::Absolute(mode) => *mode,
        ChmodMode::Symbolic(sym) => modestr::mutate(0o666, sym),
    };

    let res = unsafe { libc::mkfifo(filename.as_ptr() as *const i8, mode_val as u16) };
    if res < 0 {
        return Err(io::Error::last_os_error());
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    // parse the mode string
    let mode = match args.mode {
        Some(mode) => modestr::parse(&mode)?,
        None => ChmodMode::Absolute(0o666),
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
