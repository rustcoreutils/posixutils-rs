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
use std::{cell::RefCell, collections::LinkedList, os::unix::fs::MetadataExt};

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

struct Node {
    total_blocks: u64,
}

fn du_impl(args: &Args, filename: &str) -> bool {
    let terminate = RefCell::new(false);

    let stack: RefCell<LinkedList<Node>> = RefCell::new(LinkedList::new());

    let path = std::path::Path::new(filename);

    // TODO: Comment this
    ftw::traverse_directory(
        path,
        |entry| {
            if *terminate.borrow() {
                return Ok(false);
            }

            let md = entry.metadata().unwrap();
            let size = calc_size(args.kilo, md.blocks());
            let recurse = md.is_dir();

            let mut stack = stack.borrow_mut();
            if let Some(back) = stack.back_mut() {
                back.total_blocks += size;
            }

            // -a
            if !recurse && !args.sum {
                println!("{}\t{}", size, entry.path());
            }

            if recurse {
                stack.push_back(Node { total_blocks: size });
            }

            Ok(recurse)
        },
        |entry| {
            let mut stack = stack.borrow_mut();
            if let Some(node) = stack.pop_back() {
                let size = node.total_blocks;

                // Recursively sum the block size
                if let Some(back) = stack.back_mut() {
                    back.total_blocks += size;
                }

                if args.sum {
                    // -s, report only the total sum for the args
                    let entry_path = entry.path();
                    if entry_path.as_inner() == path {
                        println!("{}\t{}", size, entry_path);
                    }
                } else {
                    println!("{}\t{}", size, entry.path());
                }
            }
            Ok(())
        },
        |_entry, error| {
            *terminate.borrow_mut() = true;
            eprintln!("du: {}", error.inner());
        },
        ftw::TraverseDirectoryOpts {
            follow_symlinks_on_args: args.follow_cli,
            follow_symlinks: args.dereference,
            ..Default::default()
        },
    );

    let failed = *terminate.borrow();
    !failed
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

    // apply the group to each file
    for filename in &args.files {
        if !du_impl(&args, filename) {
            exit_code = 1;
        }
    }

    std::process::exit(exit_code)
}
