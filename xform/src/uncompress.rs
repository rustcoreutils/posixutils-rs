//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - FIXME: file tail truncated (data corruption)
//

extern crate clap;
extern crate plib;

mod lzw;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use lzw::UnixLZWReader;
use plib::PROJECT_NAME;
use std::fs;
use std::io::{self, Read, Write};

/// cksum - write file checksums and sizes
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Files to read as input.  Use "-" or no-args for stdin.
    files: Vec<String>,
}

fn uncompress_file(filename: &str) -> io::Result<()> {
    let file: Box<dyn Read>;
    if filename == "" {
        file = Box::new(io::stdin().lock());
    } else {
        file = Box::new(fs::File::open(filename)?);
    }

    let mut decoder = UnixLZWReader::new(file);

    loop {
        let buf = decoder.read()?;
        if buf.is_empty() {
            break;
        }

        io::stdout().write_all(&buf)?;
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
        args.files.push(String::new());
    }

    let mut exit_code = 0;

    for filename in &args.files {
        match uncompress_file(filename) {
            Ok(()) => {}
            Err(e) => {
                exit_code = 1;
                eprintln!("{}: {}", filename, e);
            }
        }
    }

    std::process::exit(exit_code)
}
