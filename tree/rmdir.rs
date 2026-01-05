//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use std::fs;
use std::io::{self, Error};
use std::path::Path;

/// rmdir - remove directories
#[derive(Parser)]
#[command(version, about = gettext("rmdir - remove directories"))]
struct Args {
    #[arg(short, long, help = gettext("Remove all directories in a pathname"))]
    parents: bool,

    #[arg(help = gettext("Directories to remove"))]
    dirs: Vec<String>,
}

fn remove_dir(dirname: &str, rm_parents: bool) -> io::Result<()> {
    fs::remove_dir(dirname)?;

    if rm_parents {
        if let Some(parent) = Path::new(dirname).parent() {
            if parent != Path::new("") {
                match parent.to_str() {
                    Some(parent_name) => return remove_dir(parent_name, rm_parents),
                    None => {
                        eprintln!("{}", gettext("Non-unicode directory name rejected"));
                        return Err(Error::other("Non-unicode dir name"));
                    }
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
