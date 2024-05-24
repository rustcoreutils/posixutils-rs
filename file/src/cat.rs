//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - Bug:  if stdout write_all() produces Err, the program will erroneously
//   output the filename as the culprit, rather than the string "stdout"
// - Questionable behavior:  if write_all() produces Err, the program will
//   continue to the next file, rather than stopping.

extern crate clap;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::io::{self, Read, Write};
use std::path::PathBuf;

/// cat - concatenate and print files
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Disable output buffering (a no-op, for POSIX compat.)
    #[arg(short, long, default_value_t = true)]
    unbuffered: bool,

    /// Files to read as input.  Use "-" or no-args for stdin.
    files: Vec<PathBuf>,
}

fn cat_file(pathname: &PathBuf) -> io::Result<()> {
    let mut file = plib::io::input_stream(pathname, true)?;
    let mut buffer = [0; plib::BUFSZ];

    loop {
        let n_read = file.read(&mut buffer[..])?;
        if n_read == 0 {
            break;
        }

        io::stdout().write_all(&buffer[0..n_read])?;
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let mut args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    // if no file args, read from stdin
    if args.files.is_empty() {
        args.files.push(PathBuf::from("-"));
    }

    let mut exit_code = 0;

    for filename in &args.files {
        if let Err(e) = cat_file(filename) {
            exit_code = 1;
            eprintln!("{}: {}", filename.display(), e);
        }
    }

    std::process::exit(exit_code)
}
