//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::fs;
use std::path::{Path, PathBuf};

/// rmdir - remove directories
#[derive(Parser)]
#[command(version, about = gettext("rmdir - remove directories"))]
struct Args {
    #[arg(short, long, help = gettext("Remove all directories in a pathname"))]
    parents: bool,

    #[arg(help = gettext("Directories to remove"))]
    dirs: Vec<String>,
}

/// Remove `operand` and, with `-p`, its parent directories. Returns `true` on success. The
/// diagnostic names the directory that actually failed (#RD1), and the `-p` walk stops at the
/// filesystem root, `.`, or `..` (#RD2).
fn remove_dir(operand: &str, rm_parents: bool) -> bool {
    let mut path = PathBuf::from(operand);
    loop {
        if let Err(e) = fs::remove_dir(&path) {
            eprintln!("rmdir: {}: {}", path.display(), e);
            return false;
        }

        if !rm_parents {
            return true;
        }

        match path.parent() {
            Some(parent)
                if !parent.as_os_str().is_empty()
                    && parent != Path::new("/")
                    && parent != Path::new(".")
                    && parent != Path::new("..") =>
            {
                path = parent.to_path_buf();
            }
            _ => return true,
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    for dirname in &args.dirs {
        if !remove_dir(dirname, args.parents) {
            exit_code = 1;
        }
    }

    std::process::exit(exit_code)
}
