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
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::{Path, PathBuf};

/// what — identify SCCS files
#[derive(Parser)]
#[command(author, version, about, long_about)]
struct Args {
    /// Display at most one identification string per file
    #[arg(short = 's', long)]
    single: bool,

    /// Input files
    files: Vec<PathBuf>,
}

fn process_file<R: BufRead>(reader: R, single: bool) -> io::Result<()> {
    let mut found = false;
    for line in reader.lines() {
        let line = line?;
        let mut start = 0;
        while let Some(pos) = line[start..].find("@(#)") {
            if found && single {
                return Ok(());
            }
            let rest = &line[start + pos + 4..];
            if let Some(end) =
                rest.find(['"', '>', '\n', '\\', '\0'])
            {
                println!("@(#){}", &rest[..end]);
            } else {
                println!("@(#){}", rest);
            }
            found = true;
            start += pos + 4;
        }
    }
    Ok(())
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    for file in &args.files {
        let path = Path::new(file);
        if let Ok(file) = File::open(path) {
            let reader = BufReader::new(file);
            process_file(reader, args.single)?;
        } else {
            eprintln!("what: {}: {}", gettext("Cannot open file"), file.display());
        }
    }

    Ok(())
}
