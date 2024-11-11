//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use modestr::ChmodMode;
use plib::modestr;
use std::{cell::RefCell, io, os::unix::fs::MetadataExt};

/// chmod - change the file modes
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Recursively change file mode bits.
    #[arg(short = 'R', long)]
    recurse: bool,

    /// Represents the change to be made to the file mode bits of each file named by one of the file operands.
    mode: String,

    /// The files to change
    files: Vec<String>,
}

fn chmod_file(filename: &str, mode: &ChmodMode, recurse: bool) -> Result<(), io::Error> {
    let terminate = RefCell::new(false);
    let result = RefCell::new(Ok(())); // Either `Ok(())` or the last error encountered

    ftw::traverse_directory(
        filename,
        |entry| {
            if *terminate.borrow() {
                return Ok(false);
            }

            let md = entry.metadata().unwrap();
            let is_dir = md.is_dir();

            let new_mode = match mode {
                ChmodMode::Absolute(m) => *m,
                ChmodMode::Symbolic(s) => modestr::mutate(md.mode(), is_dir, s),
            };

            let ret =
                unsafe { libc::fchmodat(entry.dir_fd(), entry.file_name().as_ptr(), new_mode, 0) };
            if ret != 0 {
                let e = io::Error::last_os_error();

                *result.borrow_mut() = Err(e);
                *terminate.borrow_mut() = true;
                return Err(());
            }

            Ok(is_dir && recurse)
        },
        |_| Ok(()), // No-op
        |_entry, _error| todo!(),
        ftw::TraverseDirectoryOpts::default(),
    );

    result.into_inner()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

    let args = Args::parse();

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
