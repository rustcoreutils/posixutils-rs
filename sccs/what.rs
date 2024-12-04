//
// Copyright (c) 2024 Jeff Garzik
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
            if let Some(end) = rest.find(['"', '>', '\n', '\\', '\0']) {
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
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

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
