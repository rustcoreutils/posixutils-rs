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
use std::{fs, io};

/// unlink - call the unlink function
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// An existing pathname to be unlinked (removed).
    pathname: String,
}

fn do_unlink(pathname: &str) -> io::Result<()> {
    fs::remove_file(pathname)?;

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    if let Err(e) = do_unlink(&args.pathname) {
        exit_code = 1;
        eprintln!("unlink: {}: {}", args.pathname, e);
    }

    std::process::exit(exit_code)
}
