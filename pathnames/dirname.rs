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
use std::ffi::OsString;
use std::path::PathBuf;

/// dirname - return the directory portion of a pathname
#[derive(Parser)]
#[command(version, about)]
struct Args {
    pathname: OsString,
}

fn show_dirname(args: &Args) {
    if args.pathname.is_empty() {
        println!(".");
        return;
    }

    let mut pb = PathBuf::from(&args.pathname);
    pb.pop();

    let mut dn = pb.to_string_lossy();
    if dn.is_empty() {
        dn = String::from(".").into();
    }

    println!("{}", dn);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    show_dirname(&args);

    Ok(())
}
