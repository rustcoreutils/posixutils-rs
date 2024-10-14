//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - compliance:  for -L mode, Rust performs unwanted normalization for "."
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::ffi::OsStr;
use std::path::{Component, Path};

const PWD_ENV: &str = "PWD";

/// pwd - return working directory name
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Count number of bytes in each file
    #[arg(short = 'L', long)]
    env: bool,

    /// Count number of bytes in each file
    #[arg(short = 'P', long)]
    process: bool,
}

fn dirname_valid(name: &OsStr) -> bool {
    let path = Path::new(name);

    let mut first = true;
    for component in path.components() {
        if first {
            first = false;

            if component != Component::RootDir {
                return false;
            }
        } else {
            if component == Component::CurDir || component == Component::ParentDir {
                return false;
            }
        }
    }

    true
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut cwd = std::env::current_dir()?.into_os_string();
    if args.env {
        if let Some(dir) = std::env::var_os(PWD_ENV) {
            if dirname_valid(&dir) {
                cwd = dir.clone();
            }
        }
    }

    println!("{}", cwd.to_string_lossy());

    Ok(())
}
