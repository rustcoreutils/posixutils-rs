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
use gettextrs::{bind_textdomain_codeset, gettext, textdomain};
use plib::PROJECT_NAME;
use std::fs;
use std::io::{self, Error, ErrorKind};
use std::path::Path;

/// rmdir - remove directories
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
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
                    return Err(Error::new(ErrorKind::Other, "Missing --dbid"));
                }
            }
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    for dirname in &args.dirs {
        match remove_dir(dirname, args.parents) {
            Ok(()) => {}
            Err(e) => {
                exit_code = 1;
                eprintln!("{}: {}", dirname, e);
            }
        }
    }

    std::process::exit(exit_code)
}
