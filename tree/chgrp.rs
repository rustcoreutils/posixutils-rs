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
use std::{cell::RefCell, ffi::CString, io, os::unix::fs::MetadataExt};

/// chgrp - change file group ownership
#[derive(Parser)]
#[command(version, about, disable_help_flag = true)]
struct Args {
    #[arg(long, action = clap::ArgAction::HelpLong)] // Bec. help clashes with -h
    help: Option<bool>,

    /// Change symbolic links, rather than the files they point to
    #[arg(short = 'h', long, default_value_t = false)]
    no_derereference: bool,

    /// Follow command line symlinks during -R recursion
    #[arg(short = 'H', overrides_with_all = ["follow_cli", "follow_symlinks", "follow_none"])]
    follow_cli: bool,

    /// Follow symlinks during -R recursion
    #[arg(short = 'L', overrides_with_all = ["follow_cli", "follow_symlinks", "follow_none"])]
    follow_symlinks: bool,

    /// Never follow symlinks during -R recursion
    #[arg(short = 'P', overrides_with_all = ["follow_cli", "follow_symlinks", "follow_none"], default_value_t = true)]
    follow_none: bool,

    /// Recursively change groups of directories and their contents
    #[arg(short, short_alias = 'R', long)]
    recurse: bool,

    /// A group name from the group database or a numeric group ID
    group: String,

    /// The files to change
    files: Vec<String>,
}

fn chgrp_file(filename: &str, gid: Option<u32>, args: &Args) -> bool {
    let recurse = args.recurse;
    let no_derereference = args.no_derereference;

    let terminate = RefCell::new(false);

    ftw::traverse_directory(
        filename,
        |entry| {
            if *terminate.borrow() {
                return Ok(false);
            }

            let md = entry.metadata().unwrap();

            // According to the spec:
            // "The user ID of the file shall be used as the owner argument."
            let uid = md.uid();

            // Don't change the group ID if the group argument is empty
            let gid = gid.unwrap_or(libc::gid_t::MAX);

            let ret = unsafe {
                libc::fchownat(
                    entry.dir_fd(),
                    entry.file_name().as_ptr(),
                    uid,
                    gid,
                    // Default is to change the file that the symbolic link points to unless the
                    // -h flag is specified.
                    if no_derereference {
                        libc::AT_SYMLINK_NOFOLLOW
                    } else {
                        0
                    },
                )
            };
            if ret != 0 {
                let e = io::Error::last_os_error();
                let err_str = match e.kind() {
                    io::ErrorKind::PermissionDenied => {
                        gettext!("cannot access '{}': {}", entry.path(), error_string(&e))
                    }
                    _ => {
                        gettext!("changing group of '{}': {}", entry.path(), error_string(&e))
                    }
                };
                eprintln!("chgrp: {}", err_str);
                *terminate.borrow_mut() = true;
                return Err(());
            }

            Ok(recurse)
        },
        |_| Ok(()), // Do nothing on `postprocess_dir`
        |entry, error| {
            let e = error.inner();
            let err_str = match e.kind() {
                io::ErrorKind::PermissionDenied => {
                    gettext!(
                        "cannot read directory '{}': {}",
                        entry.path(),
                        error_string(&e)
                    )
                }
                _ => {
                    gettext!("changing group of '{}': {}", entry.path(), error_string(&e))
                }
            };
            eprintln!("chgrp: {}", err_str);
            *terminate.borrow_mut() = true;
        },
        ftw::TraverseDirectoryOpts {
            follow_symlinks_on_args: args.follow_cli,
            follow_symlinks: args.follow_symlinks,
            ..Default::default()
        },
    );

    let failed = *terminate.borrow();
    !failed
}

// lookup string group by name, or parse numeric group ID
fn parse_group(group: &str) -> Result<Option<u32>, String> {
    // empty strings are accepted without errors
    if group.is_empty() {
        return Ok(None);
    }

    match group.parse::<u32>() {
        Ok(gid) => Ok(Some(gid)),
        Err(_) => {
            // lookup group by name
            let group_cstr = CString::new(group).unwrap();
            let group_st = unsafe { libc::getgrnam(group_cstr.as_ptr()) };
            if group_st.is_null() {
                let err_str = gettext!("invalid group: '{}'", group);
                return Err(err_str);
            }

            let gid = unsafe { (*group_st).gr_gid };
            Ok(Some(gid))
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let mut args = Args::parse();

    // Enable `no_derereference` if `-R` is enabled without either `-H` or `-L`
    if args.recurse && !(args.follow_cli || args.follow_symlinks) {
        args.no_derereference = true;
    }

    // initialize translations
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut exit_code = 0;

    // lookup string group by name, or parse numeric group ID
    let gid = match parse_group(&args.group) {
        Ok(gid) => gid,
        Err(e) => {
            eprintln!("chgrp: {}", e);
            std::process::exit(1);
        }
    };

    // apply the group to each file
    for filename in &args.files {
        let success = chgrp_file(filename, gid, &args);
        if !success {
            exit_code = 1;
        }
    }

    std::process::exit(exit_code)
}
