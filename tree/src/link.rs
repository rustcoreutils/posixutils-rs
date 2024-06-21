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
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::{fs, io};

/// link - call link function
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    file1: String,

    file2: String,
}

fn do_link(file1: &str, file2: &str) -> io::Result<()> {
    fs::hard_link(file1, file2)?;

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    if let Err(e) = do_link(&args.file1, &args.file2) {
        exit_code = 1;
        eprintln!("{} -> {}: {}", args.file1, args.file2, e);
    }

    std::process::exit(exit_code)
}
