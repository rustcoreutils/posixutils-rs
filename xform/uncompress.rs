//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - support NOT writing to stdout (but to file.Z, with .Z suffix removed)
// - support options -f, -v
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::lzw::UnixLZWReader;
use plib::PROJECT_NAME;
use std::io::{self, Write};
use std::path::PathBuf;

/// uncompress - expand compressed data
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Write to standard output; no files are changed.
    #[arg(short = 'c', long)]
    stdout: bool,

    /// Do not prompt for overwriting files
    #[arg(short, long)]
    force: bool,

    /// Write messages to standard error concerning the expansion of each file.
    #[arg(short, long)]
    verbose: bool,

    /// Files to read as input.  Use "-" or no-args for stdin.
    files: Vec<PathBuf>,
}

fn uncompress_file(pathname: &PathBuf) -> io::Result<()> {
    let file = plib::io::input_stream(pathname, false)?;
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

fn prog_is_zcat() -> bool {
    let progname = std::env::args().next().unwrap();
    progname.ends_with("zcat")
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let mut args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    // if no file args, read from stdin
    if args.files.is_empty() {
        args.files.push(PathBuf::new());
    }

    // zcat is a special case:  always write to stdout
    if prog_is_zcat() {
        args.stdout = true;
    }

    let mut exit_code = 0;

    for filename in &args.files {
        if let Err(e) = uncompress_file(filename) {
            exit_code = 1;
            eprintln!("{}: {}", filename.display(), e);
        }
    }

    std::process::exit(exit_code)
}
