//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::error_string;
use clap::Parser;
use gettextrs::gettext;
use std::{cell::RefCell, io};

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

    // A per-file error is reported and the walk continues; this records whether any error
    // occurred (for the exit status), instead of aborting the rest of the `-R` subtree.
    let had_error = RefCell::new(false);

    ftw::traverse_directory(
        filename,
        |entry| {
            // An owner or group not given is passed as -1, which leaves it as it is. The chgrp
            // spec says "The user ID of the file shall be used as the owner argument": the
            // file's own user ID at the moment of the change. The owner the walk saw is not
            // that: a symlink's when chown follows it to its target, or a file's that was
            // replaced since, and as root passing it would give the file to that owner.
            let uid = uid.unwrap_or(libc::uid_t::MAX);
            let gid = gid.unwrap_or(libc::gid_t::MAX);

            let ret = unsafe {
                libc::fchownat(
                    entry.dir_fd(),
                    entry.file_name().as_ptr(),
                    uid,
                    gid,
                    chown_flags(&entry, args),
                )
            };

            if ret != 0 {
                chown_err_handler(io::Error::last_os_error(), entry.path());
                *had_error.borrow_mut() = true;
                return Err(());
            }

            Ok(recurse)
        },
        |_, _| Ok(()), // Do nothing on `postprocess_dir`
        |entry, error| {
            err_handler(error.inner(), entry.path());
            *had_error.borrow_mut() = true;
        },
        ftw::TraverseDirectoryOpts {
            follow_symlinks_on_args: args.follow_cli,
            follow_symlinks: args.follow_symlinks,
            ..Default::default()
        },
    );

    let had_error = *had_error.borrow();
    !had_error
}

/// The `fchownat` flag for `entry`: whether a symlink is followed to its target or changed
/// itself.
///
/// -h and -P change every symlink itself. Under -R without -L (that is, with -H), POSIX leaves
/// symlinks met during the traversal unspecified, and following one would change a file
/// anywhere (a planted link to /etc/shadow, run as root), so only a symlink the walk followed,
/// which is an operand, is followed. -L follows everywhere, as POSIX requires, and without -R an
/// operand is followed by default.
fn chown_flags(entry: &ftw::Entry<'_>, args: &ChangeOwnershipArgs) -> libc::c_int {
    if args.no_dereference || args.follow_none {
        return libc::AT_SYMLINK_NOFOLLOW;
    }
    if args.recurse && !args.follow_symlinks {
        let walk_followed =
            entry.is_symlink() == Some(true) && entry.metadata().is_some_and(|md| !md.is_symlink());
        if !walk_followed {
            return libc::AT_SYMLINK_NOFOLLOW;
        }
    }
    0
}
