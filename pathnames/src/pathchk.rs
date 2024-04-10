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
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::path::{Component, Path};

const _POSIX_PATH_MAX: usize = 255;
const _POSIX_NAME_MAX: usize = 14;

/// pathchk - check pathnames
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Instead of performing checks based on the underlying file system,
    /// perform portable, POSIX-compliant checks.
    #[arg(short, long, group = "mode")]
    portable: bool,

    /// Instead of performing checks based on the underlying file system,
    /// Check each component in pathname for basic validity
    #[arg(short = 'P', group = "mode")]
    basic: bool,

    /// The pathnames to be checked
    pathnames: Vec<String>,
}

fn check_path_basic(pathname: &str) -> Result<(), &'static str> {
    if pathname.is_empty() {
        return Err("empty pathname");
    }

    for component in Path::new(pathname).components() {
        match component {
            Component::Normal(filename) => {
                if filename.to_string_lossy().starts_with("-") {
                    return Err("filename begins with -");
                }
            }
            _ => {}
        }
    }

    Ok(())
}

fn check_path_posix(pathname: &str) -> Result<(), &'static str> {
    if pathname.len() > _POSIX_PATH_MAX {
        return Err("pathname too long");
    }

    for component in Path::new(pathname).components() {
        match component {
            Component::Normal(filename) => {
                if filename.len() > _POSIX_NAME_MAX {
                    return Err("filename too long");
                }
                if !filename.is_ascii() {
                    return Err("filename contains non-portable characters");
                }
            }
            _ => {}
        }
    }

    Ok(())
}

fn check_path_fs(_pathname: &str) -> Result<(), &'static str> {
    todo!()
}

fn check_path(args: &Args, pathname: &str) -> Result<(), &'static str> {
    if args.portable {
        check_path_posix(pathname)
    } else if args.basic {
        check_path_basic(pathname)
    } else {
        check_path_fs(pathname)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    for pathname in &args.pathnames {
        if let Err(e) = check_path(&args, pathname) {
            exit_code = 1;
            eprintln!("{}: {}", pathname, e);
        }
    }

    std::process::exit(exit_code)
}
