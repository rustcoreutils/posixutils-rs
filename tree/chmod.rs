//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod common;

use self::common::error_string;
use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use modestr::ChmodMode;
use plib::modestr;
use std::{cell::RefCell, io, os::unix::fs::MetadataExt};

/// chmod - change the file modes
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Recursively change file mode bits.
    #[arg(short = 'R', long)]
    recurse: bool,

    /// Represents the change to be made to the file mode bits of each file named by one of the file operands.
    #[arg(allow_hyphen_values = true)] // To allow passing `chmod -x f`
    mode: String,

    /// The files to change
    files: Vec<String>,
}

fn chmod_file(filename: &str, mode: &ChmodMode, recurse: bool) -> Result<(), io::Error> {
    let terminate = RefCell::new(false);
    let result = RefCell::new(Ok(())); // Either `Ok(())` or the last error encountered

    ftw::traverse_directory(
        filename,
        |entry| {
            if *terminate.borrow() {
                return Ok(false);
            }

            let md = entry.metadata().unwrap();
            let is_dir = md.is_dir();

            let new_mode = match mode {
                ChmodMode::Absolute(m, num_digits) => {
                    // Done to match the behavior of coreutils chmod:
                    // "For directories chmod preserves set-user-ID and set-group-ID bits unless
                    // you explicitly specify otherwise"
                    //
                    // Odd quirk: 0755 would not clear the set-group-ID bit but 00755 would even
                    // though the set-group-ID is at the 4th position from the right
                    if is_dir && (*num_digits < 5) {
                        *m | (md.mode() & (libc::S_ISUID | libc::S_ISGID) as u32)
                    } else {
                        *m
                    }
                }
                ChmodMode::Symbolic(s) => modestr::mutate(md.mode(), is_dir, s),
            };

            if md.is_symlink() {
                // Uses libc::fstatat to check for the validity of the symlink
                let is_dangling = {
                    let target_deref_md =
                        ftw::Metadata::new(entry.dir_fd(), entry.file_name(), true);
                    target_deref_md.is_err()
                };

                if is_dangling {
                    let err_str = gettext!("cannot operate on dangling symlink '{}'", entry.path());
                    *result.borrow_mut() = Err(io::Error::other(err_str));
                    *terminate.borrow_mut() = true;
                    return Err(());
                } else {
                    // Symlink permissions are always lrwxrwxrwx
                    // fchmodat with AT_SYMLINK_NOFOLLOW on Linux would fail
                    if cfg!(target_os = "linux") {
                        return Ok(is_dir && recurse);
                    }
                }
            }

            let ret = unsafe {
                libc::fchmodat(
                    entry.dir_fd(),
                    entry.file_name().as_ptr(),
                    new_mode as libc::mode_t, // Cast for macOS
                    libc::AT_SYMLINK_NOFOLLOW,
                )
            };

            if ret != 0 {
                let e = io::Error::last_os_error();

                *result.borrow_mut() = Err(e);
                *terminate.borrow_mut() = true;
                return Err(());
            }

            Ok(is_dir && recurse)
        },
        |_| Ok(()), // No-op
        |entry, error| {
            let e = error.inner();
            let err_str = gettext!("cannot access '{}': {}", entry.path(), error_string(&e));
            *result.borrow_mut() = Err(io::Error::other(err_str));
            *terminate.borrow_mut() = true;
        },
        ftw::TraverseDirectoryOpts {
            follow_symlinks_on_args: true, // Default behavior of coreutils chmod with or without -R
            ..Default::default()
        },
    );

    result.into_inner()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    // parse the mode string
    let Ok(mode) = modestr::parse(&args.mode) else {
        eprintln!("chmod: {}", gettext!("invalid mode: '{}'", args.mode));
        std::process::exit(1);
    };

    // apply the mode to each file
    for filename in &args.files {
        if let Err(e) = chmod_file(filename, &mode, args.recurse) {
            exit_code = 1;
            eprintln!("chmod: {}", error_string(&e));
        }
    }

    std::process::exit(exit_code)
}
