//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod common;

use self::common::{chown_traverse, error_string, ChangeOwnershipArgs};
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::{ffi::CString, io};

/// chgrp - change file group ownership
#[derive(Parser)]
#[command(version, about = gettext("chgrp - change file group ownership"), disable_help_flag = true)]
struct Args {
    #[command(flatten)]
    delegate: ChangeOwnershipArgs,

    /// A group name from the group database or a numeric group ID
    group: String,

    /// The files to change
    files: Vec<String>,
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

fn err_handler(e: io::Error, path: ftw::DisplayablePath) {
    let err_str = match e.kind() {
        io::ErrorKind::PermissionDenied => {
            gettext!("cannot read directory '{}': {}", path, error_string(&e))
        }
        _ => {
            gettext!("changing group of '{}': {}", path, error_string(&e))
        }
    };
    eprintln!("chgrp: {}", err_str);
}

fn chown_err_handler(e: io::Error, path: ftw::DisplayablePath) {
    let err_str = match e.kind() {
        io::ErrorKind::PermissionDenied => {
            gettext!("cannot access '{}': {}", path, error_string(&e))
        }
        _ => {
            gettext!("changing group of '{}': {}", path, error_string(&e))
        }
    };
    eprintln!("chgrp: {}", err_str);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let mut args = Args::parse();

    // Enable `no_derereference` if `-R` is enabled without either `-H` or `-L`
    if args.delegate.recurse && !(args.delegate.follow_cli || args.delegate.follow_symlinks) {
        args.delegate.no_dereference = true;
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
        let success = chown_traverse(
            filename,
            None,
            gid,
            &args.delegate,
            err_handler,
            chown_err_handler,
        );
        if !success {
            exit_code = 1;
        }
    }

    std::process::exit(exit_code)
}
