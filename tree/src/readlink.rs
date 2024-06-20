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
use std::path::PathBuf;
use std::{fs, io};

/// readlink — display the contents of a symbolic link
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Do not output a trailing <newline> character.
    #[arg(short, long)]
    no_newline: bool,

    /// The pathname of an existing symbolic link
    pathname: PathBuf,
}

fn do_readlink(args: &Args) -> io::Result<()> {
    let path = fs::read_link(args.pathname.clone())?;

    if args.no_newline {
        print!("{}", path.display());
    } else {
        println!("{}", path.display());
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    if let Err(e) = do_readlink(&args) {
        exit_code = 1;
        eprintln!("{}: {}", args.pathname.display(), e);
    }

    std::process::exit(exit_code)
}
