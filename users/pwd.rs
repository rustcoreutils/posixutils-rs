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
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::ffi::OsStr;
use std::path::{Component, Path};

const PWD_ENV: &str = "PWD";

/// pwd - return working directory name
#[derive(Parser)]
#[command(version, about = gettext("pwd - return working directory name"))]
struct Args {
    #[arg(short = 'L', long, help = gettext("Use PWD from environment if valid"))]
    env: bool,

    #[arg(short = 'P', long, help = gettext("Use current working directory from process"))]
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
        } else if component == Component::CurDir || component == Component::ParentDir {
            return false;
        }
    }

    true
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

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
