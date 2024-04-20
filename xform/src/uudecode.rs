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
use std::fs::OpenOptions;
use std::io::{self, Error, ErrorKind, Read, Write};
use std::path::PathBuf;

/// uudecode - decode a binary file
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// A pathname of a file that shall be used instead of any pathname contained in the input data.
    #[arg(short, long)]
    outfile: Option<PathBuf>,

    /// The pathname of a file containing uuencoded data.
    file: Option<PathBuf>,
}

fn write_file(pathname: &PathBuf, bindata: &[u8]) -> io::Result<()> {
    let f_res = OpenOptions::new()
        .read(false)
        .write(true)
        .create(true)
        .truncate(true)
        .open(pathname);

    match f_res {
        Err(e) => {
            eprintln!("{}: {}", pathname.display(), e);
            return Err(e);
        }
        Ok(mut file) => file.write_all(bindata),
    }
}

fn decode_file(args: &Args) -> io::Result<()> {
    let mut file = plib::io::input_stream_opt(&args.file)?;

    // read entire file into memory.
    // ugly but necessary due to uudecode crate implementation.
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;

    // attempt base64 decode
    let b64_res = BASE64_STANDARD.decode(buffer.as_bytes());
    match b64_res {
        // fall through to uudecode format
        Err(_e) => {}

        // decode succeeded. exit here.
        Ok(bindata) => match &args.outfile {
            None => return write_file(&PathBuf::from("bindata.out"), &bindata[..]),
            Some(outfn) => return write_file(outfn, &bindata[..]),
        },
    }

    // attempt to decode using uudecode format
    match uuencode::uudecode(&buffer) {
        None => return Err(Error::new(ErrorKind::Other, "invalid input data")),
        Some((bindata, filename)) => match &args.outfile {
            None => write_file(&PathBuf::from(filename), &bindata[..]),
            Some(outfn) => write_file(outfn, &bindata[..]),
        },
    }
}

fn pathname_display(path: &Option<PathBuf>) -> String {
    match path {
        None => "stdin".to_string(),
        Some(p) => p.display().to_string(),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    if let Err(e) = decode_file(&args) {
        exit_code = 1;
        eprintln!("{:?}: {}", pathname_display(&args.file), e);
    }

    std::process::exit(exit_code)
}
