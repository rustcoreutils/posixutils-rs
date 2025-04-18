//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::fs;
use std::io::{self, Error, ErrorKind};
use std::path::Path;

/// rmdir - remove directories
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Remove all directories in a pathname
    #[arg(short, long)]
    parents: bool,

    /// Directories to remove
    dirs: Vec<String>,
}

fn remove_dir(dirname: &str, rm_parents: bool) -> io::Result<()> {
    fs::remove_dir(dirname)?;

    if rm_parents {
        let parent = Path::new(dirname).parent();
        if parent.is_some() && parent != Some(Path::new("")) {
            match parent.unwrap().to_str() {
                Some(parent_name) => return remove_dir(parent_name, rm_parents),
                None => {
                    eprintln!("{}", gettext("Non-unicode directory name rejected"));
                    return Err(Error::new(ErrorKind::Other, "Non-unicode dir name"));
                }
            }
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    for dirname in &args.dirs {
        if let Err(e) = remove_dir(dirname, args.parents) {
            exit_code = 1;
            eprintln!("{}: {}", dirname, e);
        }
    }

    std::process::exit(exit_code)
}
