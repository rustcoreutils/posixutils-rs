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
extern crate uuencode;

use base64::prelude::*;
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::fs;
use std::io::{self, Read, Write};

/// uuencode - encode a binary file
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Encode to base64 (MIME) standard, rather than UUE format.
    #[arg(short = 'm', long)]
    base64: bool,

    /// File to read as input.
    file: Option<String>,

    /// Decode pathname
    decode_path: Option<String>,
}

fn encode_file(args: &Args) -> io::Result<()> {
    let mut file: Box<dyn Read>;
    if let Some(filename) = &args.file {
        file = Box::new(fs::File::open(filename)?);
    } else {
        file = Box::new(io::stdin().lock());
    }

    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let decode_path;
    match &args.decode_path {
        None => {
            decode_path = String::from("/dev/stdout");
        }
        Some(path) => {
            decode_path = String::from(path);
        }
    }

    let output;
    if args.base64 {
        output = BASE64_STANDARD.encode(&buffer[..]);
    } else {
        output = uuencode::uuencode(&decode_path, &buffer[..]);
    }

    io::stdout().write_all(output.as_bytes())?;
    io::stdout().write_all(b"\n")?;

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    match encode_file(&args) {
        Ok(()) => {}
        Err(e) => {
            exit_code = 1;
            eprintln!("{:?}: {}", args.file, e);
        }
    }

    std::process::exit(exit_code)
}
