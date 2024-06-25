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
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use modestr::{ChmodMode, ChmodSymbolic};
use plib::{modestr, PROJECT_NAME};
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::{fs, io};

/// chmod - change the file modes
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Recursively change file mode bits.
    #[arg(short = 'R', long)]
    recurse: bool,

    /// Represents the change to be made to the file mode bits of each file named by one of the file operands.
    mode: String,

    /// The files to change
    files: Vec<String>,
}

// apply symbolic mutations to the given file at path
fn set_permissions_symbolic(path: &Path, symbolic: &ChmodSymbolic) -> Result<(), io::Error> {
    // query the current mode bits
    let metadata = fs::metadata(path)?;
    let mut perms = metadata.permissions();

    // perform mutations on the mode bits
    let new_mode = modestr::mutate(perms.mode(), symbolic);

    // update path in filesystem
    perms.set_mode(new_mode);
    fs::set_permissions(path, perms)?;

    Ok(())
}

fn chmod_file(filename: &str, mode: &ChmodMode, recurse: bool) -> Result<(), io::Error> {
    let path = Path::new(filename);
    let metadata = fs::metadata(path)?;

    if metadata.is_dir() && recurse {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let entry_path = entry.path();
            let entry_filename = entry_path.to_str().unwrap();
            chmod_file(entry_filename, mode, recurse)?;
        }
    }

    match mode {
        // set the mode bits to the given value
        ChmodMode::Absolute(m) => {
            fs::set_permissions(path, fs::Permissions::from_mode(*m))?;
        }

        // apply symbolic mutations to the mode bits
        ChmodMode::Symbolic(s) => {
            set_permissions_symbolic(path, s)?;
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    // initialize translations
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    // parse the mode string
    let mode = modestr::parse(&args.mode)?;

    // apply the mode to each file
    for filename in &args.files {
        if let Err(e) = chmod_file(filename, &mode, args.recurse) {
            exit_code = 1;
            eprintln!("{}: {}", filename, e);
        }
    }

    std::process::exit(exit_code)
}
