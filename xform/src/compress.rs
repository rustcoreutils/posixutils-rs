//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod lzw;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use lzw::{UnixLZWReader, UnixLZWWriter};
use plib::PROJECT_NAME;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;

/// compress - compress data
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Write to the standard output; the input file is not changed, and no .Z files are created.
    #[arg(short = 'c', long)]
    stdout: bool,

    /// Specify the maximum number of bits to use in a code. 9 <= bits <= 14
    #[arg(short = 'b')]
    bits: Option<u32>,

    /// Do not prompt for overwriting files
    #[arg(short, long)]
    force: bool,

    /// Write messages to standard error concerning the expansion of each file.
    #[arg(short, long)]
    verbose: bool,

    /// Files to read as input.  Use "-" or no-args for stdin.
    files: Vec<PathBuf>,
}

fn compress_file(args: &Args, pathname: &PathBuf) -> io::Result<()> {
    let mut file = plib::io::input_stream(pathname, false)?;

    let mut encoder = UnixLZWWriter::new(args.bits);
    let mut inp_buf = Vec::new();
    file.read_to_end(&mut inp_buf)?;

    let out_buf = encoder.write(&inp_buf)?;

    if args.stdout {
        io::stdout().write_all(&out_buf)?;
    } else {
        let fname = pathname.file_name().unwrap().to_str().unwrap();
        let mut f = fs::File::create(format!("{fname}.Z"))?;
        f.write_all(&out_buf)?;
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    if args.files.is_empty() {
        args.files.push(PathBuf::new());
    }

    let mut exit_code = 0;

    for filename in &args.files {
        if let Err(e) = compress_file(&args, filename) {
            exit_code = 1;
            eprintln!("{}: {}", filename.display(), e);
        }
    }

    std::process::exit(exit_code)
}
