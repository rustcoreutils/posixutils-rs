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
use std::{
    cell::RefCell,
    collections::{HashSet, LinkedList},
    os::unix::fs::MetadataExt,
};

/// du - estimate file space usage
#[derive(Parser)]
#[command(version, about = gettext("du - estimate file space usage"))]
struct Args {
    #[arg(short, long, help = gettext("Write counts for all files, not just directories"))]
    all: bool,

    #[arg(short = 'H', long, help = gettext("Follow command line symlinks"))]
    follow_cli: bool,

    #[arg(short = 'L', long, help = gettext("Dereference all symlinks"))]
    dereference: bool,

    #[arg(short, long, help = gettext("Write the files sizes in units of 1024 bytes, rather than the default 512-byte units"))]
    kilo: bool,

    #[arg(short, long, help = gettext("Write only the sum of all arguments"))]
    sum: bool,

    #[arg(short = 'x', long, help = gettext("When evaluating file sizes, evaluate only those files that have the same device as the file specified by the file operand"))]
    one_fs: bool,

    #[arg(help = gettext("The files to receive metadata processing"))]
    files: Vec<String>,
}

fn calc_size(kilo: bool, size: u64) -> u64 {
    if kilo { size / 2 } else { size }
}

struct Node {
    total_blocks: u64,
}

fn du_impl(args: &Args, filename: &str) -> bool {
    let terminate = RefCell::new(false);
    let stack: RefCell<LinkedList<Node>> = RefCell::new(LinkedList::new());
    // Track seen (dev, ino) pairs for hard link deduplication
    let seen: RefCell<HashSet<(u64, u64)>> = RefCell::new(HashSet::new());
    // Track initial device for -x option
    let initial_dev: RefCell<Option<u64>> = RefCell::new(None);

    let path = std::path::Path::new(filename);

    ftw::traverse_directory(
        path,
        |entry| {
            if *terminate.borrow() {
                return Ok(false);
            }

            let md = entry
                .metadata()
                .expect("ftw::traverse_directory yielded an entry without metadata");

            // -x: skip files on different filesystems
            if args.one_fs {
                let dev = md.dev();
                let mut init_dev = initial_dev.borrow_mut();
                if init_dev.is_none() {
                    *init_dev = Some(dev);
                } else if Some(dev) != *init_dev {
                    // Different filesystem, skip this entry
                    return Ok(false);
                }
            }

            let is_dir = md.is_dir();

            // Handle hard link deduplication: files with nlink > 1 should only be counted once
            let size = if !is_dir && md.nlink() > 1 {
                let key = (md.dev(), md.ino());
                let mut seen_set = seen.borrow_mut();
                if seen_set.contains(&key) {
                    // Already counted this file, use size 0
                    0
                } else {
                    seen_set.insert(key);
                    calc_size(args.kilo, md.blocks())
                }
            } else {
                calc_size(args.kilo, md.blocks())
            };

            let mut stack = stack.borrow_mut();

            // Check if this is the original file operand (root of traversal)
            let is_root = entry.path().as_inner() == path;

            if is_dir {
                // For directories, push onto stack. Don't add to parent here -
                // the directory's total will be added when we exit the directory.
                stack.push_back(Node { total_blocks: size });
            } else {
                // For files, add size to parent directory's total
                if let Some(back) = stack.back_mut() {
                    back.total_blocks += size;
                }
                // For non-directories:
                // - Always print if it's the original file operand (POSIX BSD behavior)
                // - Print if -a is specified
                // - Don't print with -s (handled separately)
                let display_size = size;
                if is_root {
                    // File operands are always listed
                    println!("{}\t{}", display_size, entry.path());
                } else if args.all && !args.sum {
                    // -a: report all files within directories
                    println!("{}\t{}", display_size, entry.path());
                }
            }

            Ok(is_dir)
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
                    // -s: report only the total sum for the file operand
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
