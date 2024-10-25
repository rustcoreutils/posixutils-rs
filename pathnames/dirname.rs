//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::OsString;
use std::path::PathBuf;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

#[derive(Parser)]
#[command(
    version,
    about = gettext("dirname - return the directory portion of a pathname")
)]
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
    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

    let args = Args::parse();

    show_dirname(&args);

    Ok(())
}
