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
use std::io::Write;
use std::path::PathBuf;
use std::{fs, io};

/// readlink — display the contents of a symbolic link
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Do not output a trailing <newline> character.
    #[arg(short, long)]
    no_newline: bool,

    /// The pathname of an existing symbolic link
    pathname: PathBuf,
}

fn do_readlink(args: &Args) -> Result<String, String> {
    let path = PathBuf::from(&args.pathname);

    match fs::read_link(&path) {
        Ok(target) => {
            let output = target.display().to_string();
            if args.no_newline {
                Ok(output)
            } else {
                Ok(output + "\n")
            }
        }
        Err(e) => {
            let err_message = match e.kind() {
                io::ErrorKind::NotFound => {
                    format!("readlink: {}: No such file or directory\n", path.display())
                }
                io::ErrorKind::InvalidInput => {
                    format!("readlink: {}: Not a symbolic link\n", path.display())
                }
                _ => format!("readlink: {}: {}\n", path.display(), e),
            };
            Err(err_message)
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(env!("PROJECT_NAME"))?;
    bind_textdomain_codeset(env!("PROJECT_NAME"), "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    match do_readlink(&args) {
        Ok(output) => {
            print!("{}", output);
            io::stdout().flush().unwrap();
        }
        Err(err) => {
            eprint!("{}", err);
            io::stderr().flush().unwrap();
            exit_code = 1;
        }
    }

    std::process::exit(exit_code);
}
