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
use std::io::{self, Read, Write};

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

fn head_file(args: &Args, filename: &str, first: bool, want_header: bool) -> io::Result<()> {
    // print file header
    if want_header {
        if first {
            println!("==> {} <==\n", filename);
        } else {
            println!("\n==> {} <==\n", filename);
        }
    }

    // open file, or stdin
    let mut file: Box<dyn Read>;
    if filename == "" {
        file = Box::new(io::stdin().lock());
    } else {
        file = Box::new(fs::File::open(filename)?);
    }

    let mut raw_buffer = [0; 4096];
    let mut nl = 0;

    loop {
        // read a chunk of file data
        let n_read = file.read(&mut raw_buffer[..])?;
        if n_read == 0 {
            break;
        }

        // slice of buffer containing file data
        let buf = &raw_buffer[0..n_read];
        let mut pos = 0;

        // count newlines
        for chv in buf {
            // LF character encountered
            if *chv == 10 {
                nl = nl + 1;
            }

            pos = pos + 1;

            // if user-specified limit reached, stop
            if nl >= args.n {
                break;
            }
        }

        // output full or partial buffer
        let final_buf = &raw_buffer[0..pos];
        io::stdout().write_all(final_buf)?;

        // if user-specified limit reached, stop
        if nl >= args.n {
            break;
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let mut args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    // if no files, read from stdin
    if args.files.is_empty() {
        args.files.push(String::new());
    }

    let mut exit_code = 0;
    let want_header = args.files.len() > 1;
    let mut first = true;

    for filename in &args.files {
        match head_file(&args, filename, first, want_header) {
            Ok(()) => {}
            Err(e) => {
                exit_code = 1;
                eprintln!("{}: {}", filename, e);
            }
        }

        first = false;
    }

    std::process::exit(exit_code)
}
