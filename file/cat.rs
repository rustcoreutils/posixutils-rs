//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::io::input_stream;
use plib::BUFSZ;

#[derive(Parser)]
#[command(version, about = gettext("cat - concatenate and print files"))]
struct Args {
    #[arg(
        short,
        long,
        default_value_t = true,
        help = gettext("Disable output buffering (a no-op, for POSIX compat)")
    )]
    unbuffered: bool,

    #[arg(help = gettext("Files to read as input. Use '-' or no-args for stdin"))]
    files: Vec<PathBuf>,
}

/// Copy one input file to standard output. Diagnostics are emitted here so a
/// read/open error is attributed to the input file while a write error is
/// attributed to standard output (not the input filename). Returns true if an
/// error occurred.
fn cat_file(pathname: &Path) -> bool {
    let mut file = match input_stream(pathname, true) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("cat: {}: {}", pathname.display(), e);
            return true;
        }
    };
    let mut buffer = [0; BUFSZ];
    let stdout = io::stdout();
    let mut handle = stdout.lock();

    loop {
        let n_read = match file.read(&mut buffer[..]) {
            Ok(0) => break,
            Ok(n) => n,
            Err(e) => {
                eprintln!("cat: {}: {}", pathname.display(), e);
                return true;
            }
        };

        if let Err(e) = handle.write_all(&buffer[0..n_read]) {
            eprintln!("cat: {}: {}", gettext("standard output"), e);
            return true;
        }
    }

    false
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    // if no file args, read from stdin
    if args.files.is_empty() {
        args.files.push(PathBuf::from("-"));
    }

    let mut exit_code = 0;

    for filename in &args.files {
        if cat_file(filename) {
            exit_code = 1;
        }
    }

    std::process::exit(exit_code)
}
