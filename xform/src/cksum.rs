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

mod crc32;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::fs;
use std::io::{self, Read};

/// cksum - write file checksums and sizes
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Files to read as input.  Use "-" or no-args for stdin.
    files: Vec<String>,
}

fn cksum_file(filename: &str) -> io::Result<()> {
    let mut file: Box<dyn Read>;
    if filename == "" {
        file = Box::new(io::stdin().lock());
    } else {
        file = Box::new(fs::File::open(filename)?);
    }

    let mut buffer = [0; plib::BUFSZ];
    let mut n_bytes: u64 = 0;
    let mut crc: u32 = 0;

    loop {
        let n_read = file.read(&mut buffer[..])?;
        if n_read == 0 {
            break;
        }

        n_bytes = n_bytes + n_read as u64;
        crc = crc32::update(crc, &buffer[0..n_read]);
    }

    println!(
        "{} {} {}",
        crc32::finalize(crc, n_bytes as usize),
        n_bytes,
        filename
    );

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
        match cksum_file(filename) {
            Ok(()) => {}
            Err(e) => {
                exit_code = 1;
                eprintln!("{}: {}", filename, e);
            }
        }
    }

    std::process::exit(exit_code)
}
