//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - implement -H, -L, -x
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use std::os::unix::fs::MetadataExt;
use std::path::Path;
use std::{fs, io};

/// du - estimate file space usage
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Write counts for all files, not just directories
    #[arg(short, long)]
    all: bool,

    /// Follow command line symlinks
    #[arg(short = 'H', long)]
    follow_cli: bool,

    /// Dereference all symlinks
    #[arg(short = 'L', long)]
    dereference: bool,

    /// Write the files sizes in units of 1024 bytes, rather than the default 512-byte units.
    #[arg(short, long)]
    kilo: bool,

    /// Write only the sum of all arguments
    #[arg(short, long)]
    sum: bool,

    /// When evaluating file sizes, evaluate only those files that have the same device as the file specified by the file operand.
    #[arg(short = 'x', long)]
    one_fs: bool,

    /// The files to receive metadata processing
    files: Vec<String>,
}

fn calc_size(kilo: bool, size: u64) -> u64 {
    if kilo {
        size / 2
    } else {
        size
    }
}

fn print_pathinfo(args: &Args, filename: &str, size: u64, toplevel: bool) {
    if args.sum && !toplevel {
        return;
    }

    // print the file size
    println!("{}\t{}", size, filename);
}

fn du_cli_arg(
    args: &Args,
    filename: &str,
    total: &mut u64,
    toplevel: bool,
) -> Result<(), io::Error> {
    let path = Path::new(filename);
    let metadata = fs::metadata(path)?;

    // recursively process directories
    if metadata.is_dir() {
        let mut sub_total = 0;
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
            let filename = path.to_str().unwrap();
            if let Err(e) = du_cli_arg(args, filename, &mut sub_total, false) {
                eprintln!("{}: {}", filename, e);
            }
        }
        print_pathinfo(args, filename, sub_total, toplevel);

        *total += sub_total;
        return Ok(());
    }

    // print the file size
    let size = calc_size(args.kilo, metadata.blocks());
    *total += size;

    if args.all {
        print_pathinfo(args, filename, size, toplevel);
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    // default to current directory
    if args.files.is_empty() {
        args.files.push(".".to_string());
    }
    let mut exit_code = 0;
    let mut total = 0;

    // apply the group to each file
    for filename in &args.files {
        if let Err(e) = du_cli_arg(&args, filename, &mut total, true) {
            exit_code = 1;
            eprintln!("{}: {}", filename, e);
        }
    }

    std::process::exit(exit_code)
}
