//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::error_string;
use clap::Parser;
use gettextrs::gettext;
use std::{cell::RefCell, io, os::unix::fs::MetadataExt};

#[derive(Parser)]
#[command(version, about, disable_help_flag = true)]
pub struct ChangeOwnershipArgs {
    #[arg(long, action = clap::ArgAction::HelpLong)] // Bec. help clashes with -h
    help: Option<bool>,

    /// Change symbolic links, rather than the files they point to
    #[arg(short = 'h', long, default_value_t = false)]
    pub no_dereference: bool,

    /// Follow command line symlinks during -R recursion
    #[arg(short = 'H', overrides_with_all = ["follow_cli", "follow_symlinks", "follow_none"])]
    pub follow_cli: bool,

    /// Follow symlinks during -R recursion
    #[arg(short = 'L', overrides_with_all = ["follow_cli", "follow_symlinks", "follow_none"])]
    pub follow_symlinks: bool,

    /// Never follow symlinks during -R recursion
    #[arg(short = 'P', overrides_with_all = ["follow_cli", "follow_symlinks", "follow_none"])]
    pub follow_none: bool,

    /// Recursively change groups of directories and their contents
    #[arg(short, short_alias = 'R', long)]
    pub recurse: bool,
}

pub fn chown_traverse<F, G>(
    filename: &str,
    uid: Option<u32>,
    gid: Option<u32>,
    args: &ChangeOwnershipArgs,
    err_handler: F,
    chown_err_handler: G,
) -> bool
where
    F: Fn(io::Error, ftw::DisplayablePath), // F and G are the same but they must be declared
    G: Fn(io::Error, ftw::DisplayablePath), // separately to use two different closures
{
    let recurse = args.recurse;
    let no_dereference = args.no_dereference;
    let follow_none = args.follow_none;
    let follow_symlinks = args.follow_symlinks;

    let terminate = RefCell::new(false);

    ftw::traverse_directory(
        filename,
        |entry| {
            if *terminate.borrow() {
                return Ok(false);
            }

            let md = entry.metadata().unwrap();

            // Use the UID from the args if present. If not given, according to the chgrp spec:
            // "The user ID of the file shall be used as the owner argument."
            let uid = uid.unwrap_or(md.uid());

            // Don't change the group ID if the group argument is empty
            let gid = gid.unwrap_or(libc::gid_t::MAX);

            let ret = unsafe {
                libc::fchownat(
                    entry.dir_fd(),
                    entry.file_name().as_ptr(),
                    uid,
                    gid,
                    // Default is to change the file that the symbolic link points to unless the
                    // -h flag or -P flag is specified.
                    if no_dereference || follow_none {
                        libc::AT_SYMLINK_NOFOLLOW
                    } else {
                        0
                    },
                )
            };

            if ret != 0 {
                chown_err_handler(io::Error::last_os_error(), entry.path());
                *terminate.borrow_mut() = true;
                return Err(());
            }

            Ok(recurse)
        },
        |_| Ok(()), // Do nothing on `postprocess_dir`
        |entry, error| {
            err_handler(error.inner(), entry.path());
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
