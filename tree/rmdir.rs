//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::gettext;
use std::fs;
use std::path::{Path, PathBuf};

/// rmdir - remove directories
#[derive(Parser)]
#[command(version, about = gettext("rmdir - remove directories"))]
struct Args {
    #[arg(short, long, help = gettext("Remove all directories in a pathname"))]
    parents: bool,

    #[arg(
        long,
        help = gettext("Ignore each failure that is solely because a directory is non-empty")
    )]
    ignore_fail_on_non_empty: bool,

    #[arg(help = gettext("Directories to remove"))]
    dirs: Vec<String>,
}

/// Did removing `path` fail only because the directory is not empty?  `ENOTEMPTY` and
/// `EEXIST` (which some systems return instead) say so directly.  Like GNU, a permission,
/// read-only or busy error on a directory that does hold an entry also counts: the
/// directory could not have been removed even with the permission.
fn failed_because_non_empty(path: &Path, e: &std::io::Error) -> bool {
    match e.raw_os_error() {
        Some(libc::ENOTEMPTY) | Some(libc::EEXIST) => true,
        Some(libc::EACCES) | Some(libc::EPERM) | Some(libc::EROFS) | Some(libc::EBUSY) => {
            fs::read_dir(path).is_ok_and(|mut entries| entries.next().is_some())
        }
        _ => false,
    }
}

/// Remove `operand` and, with `-p`, its parent directories. Returns `true` on success. The
/// diagnostic names the directory that actually failed (#RD1), and the `-p` walk stops at the
/// filesystem root, `.`, or `..` (#RD2).  With `ignore_non_empty`, a directory that cannot be
/// removed because it is not empty ends the walk silently and counts as success.
fn remove_dir(operand: &str, rm_parents: bool, ignore_non_empty: bool) -> bool {
    let mut path = PathBuf::from(operand);
    loop {
        if let Err(e) = fs::remove_dir(&path) {
            if ignore_non_empty && failed_because_non_empty(&path, &e) {
                return true;
            }
            eprintln!(
                "rmdir: {}: {}",
                path.display(),
                plib::diag::io_error_text(&e)
            );
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
    plib::diag::init_locale("rmdir");

    let args = Args::parse();

    let mut exit_code = 0;

    for dirname in &args.dirs {
        if !remove_dir(dirname, args.parents, args.ignore_fail_on_non_empty) {
            exit_code = 1;
        }
    }

    std::process::exit(exit_code)
}
