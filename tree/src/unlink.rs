//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::{fs, io};

/// unlink - call the unlink function
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// The pathname of an existing file.
    pathname: String,
}

fn do_unlink(pathname: &str) -> io::Result<()> {
    fs::remove_file(pathname)?;

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    match do_unlink(&args.pathname) {
        Ok(()) => {}
        Err(e) => {
            exit_code = 1;
            eprintln!("{}: {}", args.pathname, e);
        }
    }

    std::process::exit(exit_code)
}
