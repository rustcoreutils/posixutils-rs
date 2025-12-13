//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::{Path, PathBuf};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

#[derive(Parser)]
#[command(version, about = gettext("what - identify SCCS files"))]
struct Args {
    #[arg(
        short = 's',
        long,
        help = gettext("Display at most one identification string per file")
    )]
    single: bool,

    #[arg(help = gettext("Input files"))]
    files: Vec<PathBuf>,
}

fn process_file<R: BufRead>(reader: R, single: bool) -> io::Result<bool> {
    let mut found = false;
    for line in reader.lines() {
        let line = line?;
        let mut start = 0;
        while let Some(pos) = line[start..].find("@(#)") {
            if found && single {
                return Ok(true);
            }
            let rest = &line[start + pos + 4..];
            let ident = if let Some(end) = rest.find(['"', '>', '\n', '\\', '\0']) {
                &rest[..end]
            } else {
                rest
            };
            println!("\t{}", ident);
            found = true;
            start += pos + 4;
        }
    }
    Ok(found)
}

fn main() -> io::Result<()> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();
    let mut any_found = false;

    for file in &args.files {
        let path = Path::new(file);
        if let Ok(f) = File::open(path) {
            // Print the pathname first per POSIX format
            println!("{}:", file.display());
            let reader = BufReader::new(f);
            if process_file(reader, args.single)? {
                any_found = true;
            }
        } else {
            eprintln!("what: {}: {}", gettext("Cannot open file"), file.display());
        }
    }

    // Exit 0 if any matches found, 1 otherwise
    std::process::exit(if any_found { 0 } else { 1 });
}
