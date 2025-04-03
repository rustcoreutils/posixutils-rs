//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{fs, io};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};

/// link - call link function
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Link source
    file1: String,

    /// Link target
    file2: String,
}

fn do_link(file1: &str, file2: &str) -> io::Result<()> {
    fs::hard_link(file1, file2)?;
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    if let Err(e) = do_link(&args.file1, &args.file2) {
        exit_code = 1;
        eprintln!("link: {} -> {}: {}", args.file1, args.file2, e);
    }

    std::process::exit(exit_code)
}
