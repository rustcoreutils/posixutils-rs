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
use gettextrs::{bind_textdomain_codeset, gettext, textdomain};
use plib::PROJECT_NAME;
use std::fs;
use std::io::{self, BufRead, Write};

/// asa - interpret carriage-control characters
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Files to read as input.
    files: Vec<String>,
}

fn asa_file(filename: &str) -> io::Result<()> {
    let file = fs::File::open(filename)?;
    let mut reader = io::BufReader::new(file);
    let mut line_no: usize = 0;

    loop {
        line_no = line_no + 1;

        let mut raw_line = String::new();
        let n_read = reader.read_line(&mut raw_line)?;
        if n_read == 0 {
            break;
        }

        if raw_line.len() < 2 {
            eprintln!("{} {}", gettext("malformed line"), line_no);
            continue;
        }

        let ch = raw_line.chars().next().unwrap();

        // exclude first char, and trailing newline
        let line = &raw_line[1..(raw_line.len() - 1)];

        let output = match ch {
            ' ' => format!("{}", line),
            '0' => format!("\n{}", line),
            '1' => format!("\x12{}", line),
            '+' => format!("\r{}", line),
            _ => {
                eprintln!("{} {}", gettext("malformed line"), line_no);
                continue;
            }
        };

        io::stdout().write_all(output.as_bytes())?;
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
        match asa_file(filename) {
            Ok(()) => {}
            Err(e) => {
                exit_code = 1;
                eprintln!("{}: {}", filename, e);
            }
        }
    }

    std::process::exit(exit_code)
}
