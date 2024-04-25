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
use std::io::{self, Read, Write};
use std::path::PathBuf;

/// uuencode - encode a binary file
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Encode to base64 (MIME) standard, rather than UUE format.
    #[arg(short = 'm', long)]
    base64: bool,

    /// File to read as input.
    file: Option<PathBuf>,

    /// Decode pathname
    decode_path: Option<String>,
}

fn encode_file(args: &Args) -> io::Result<()> {
    let mut file = plib::io::input_stream_opt(&args.file)?;

    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let decode_path = match &args.decode_path {
        None => String::from("/dev/stdout"),
        Some(path) => String::from(path),
    };

    let output = {
        if args.base64 {
            BASE64_STANDARD.encode(&buffer[..])
        } else {
            uuencode::uuencode(&decode_path, &buffer[..])
        }
    };

    io::stdout().write_all(output.as_bytes())?;
    io::stdout().write_all(b"\n")?;

    Ok(())
}

fn pathname_display(path: &Option<PathBuf>) -> String {
    match path {
        None => String::from("stdin"),
        Some(p) => p.display().to_string(),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    if let Err(e) = encode_file(&args) {
        exit_code = 1;
        eprintln!("{:?}: {}", pathname_display(&args.file), e);
    }

    std::process::exit(exit_code)
}
