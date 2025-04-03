//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::path::{Path, PathBuf};
use std::{fs, io};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

/// ln - link files
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Force existing destination pathnames to be removed to allow the link.
    #[arg(short, long)]
    force: bool,

    /// Create symbolic links instead of hard links.
    #[arg(short, long)]
    symlink: bool,

    /// Source(s) and target of link(s).
    files: Vec<String>,
}

#[allow(deprecated)] // for soft_link()
fn do_link(args: &Args, file1: &str, file2: &str) -> io::Result<()> {
    if args.symlink {
        fs::soft_link(file1, file2)
    } else {
        fs::hard_link(file1, file2)
    }
}

fn do_link_into(args: &Args, src: &str, target_dir: &str) -> io::Result<()> {
    let mut path = PathBuf::from(target_dir);
    path.push(Path::new(src).file_name().expect("Invalid source name"));

    let target_name = path.to_str().expect("Unicode filenames required");

    do_link(args, src, target_name)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    if args.files.len() < 2 {
        eprintln!("{}", gettext("Must supply a source and target for linking"));
        std::process::exit(1);
    }

    let sources = &args.files[0..args.files.len() - 1];
    let target = &args.files[args.files.len() - 1];

    let mut exit_code = 0;

    if sources.len() == 1 {
        let src = &sources[0];
        if let Err(e) = do_link(&args, src, target) {
            exit_code = 1;
            eprintln!("{} -> {}: {}", src, target, e);
        }
    } else {
        for src in sources {
            if let Err(e) = do_link_into(&args, src, target) {
                exit_code = 1;
                eprintln!("{} -> {}: {}", src, target, e);
            }
        }
    }

    std::process::exit(exit_code)
}
