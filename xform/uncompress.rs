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
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::io::input_stream;
use plib::lzw::UnixLZWReader;
use std::io::{self, Write};
use std::path::PathBuf;

/// uncompress - expand compressed data
#[derive(Parser)]
#[command(version, about = gettext("uncompress - expand compressed data"))]
struct Args {
    #[arg(short = 'c', long, help = gettext("Write to standard output; no files are changed"))]
    stdout: bool,

    #[arg(short, long, help = gettext("Do not prompt for overwriting files"))]
    force: bool,

    #[arg(short, long, help = gettext("Write messages to standard error concerning the expansion of each file"))]
    verbose: bool,

    #[arg(help = gettext("Files to read as input. Use \"-\" or no-args for stdin"))]
    files: Vec<PathBuf>,
}

fn uncompress_file(pathname: &PathBuf) -> io::Result<()> {
    let file = input_stream(pathname, false)?;
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
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

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
