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
use std::fs;
use std::io::{self, BufRead, Write};

/// head - copy the first part of files
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// The first <N> lines of each input file shall be copied to standard output.
    #[arg(short, default_value_t = 10, value_parser = clap::value_parser!(u64).range(1..))]
    n: u64,

    /// Files to read as input.
    files: Vec<String>,
}

fn head_file(args: &Args, filename: &str) -> io::Result<()> {
    let file = fs::File::open(filename)?;
    let mut reader = io::BufReader::new(file);
    let mut line_no = 0;

    loop {
        line_no = line_no + 1;

        let mut raw_line = String::new();
        let n_read = reader.read_line(&mut raw_line)?;
        if n_read == 0 {
            break;
        }

        io::stdout().write_all(raw_line.as_bytes())?;

        if line_no == args.n {
            break;
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    for filename in &args.files {
        match head_file(&args, filename) {
            Ok(()) => {}
            Err(e) => {
                exit_code = 1;
                eprintln!("{}: {}", filename, e);
            }
        }
    }

    std::process::exit(exit_code)
}
