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

/// chown - change the file ownership
#[derive(Parser)]
#[command(version, about = gettext("chown - change the file ownership"))]
struct Args {
    #[command(flatten)]
    delegate: ChangeOwnershipArgs,

    /// Owner and group are changed to OWNER[:GROUP]
    owner_group: String,

    /// The files to change
    files: Vec<String>,
}

// lookup string group by name, or parse numeric group ID
fn parse_group(group: &str) -> Result<u32, String> {
    match group.parse::<u32>() {
        Ok(gid) => Ok(gid),
        Err(_) => {
            // lookup group by name
            let group_cstr = CString::new(group).unwrap();
            let group_name = unsafe { libc::getgrnam(group_cstr.as_ptr()) };
            if group_name.is_null() {
                return Err(gettext!("invalid group: '{}'", group));
            }

            let gid = unsafe { (*group_name).gr_gid };
            Ok(gid)
        }
    }
}

// lookup string user by name, or parse numeric user ID
fn parse_user(user: &str) -> Result<u32, String> {
    match user.parse::<u32>() {
        Ok(uid) => Ok(uid),
        Err(_) => {
            // lookup user by name
            let user_cstr = CString::new(user).unwrap();
            let user_name = unsafe { libc::getpwnam(user_cstr.as_ptr()) };
            if user_name.is_null() {
                return Err(gettext!("invalid user: '{}'", user));
            }

            let uid = unsafe { (*user_name).pw_uid };
            Ok(uid)
        }
    }
}

enum ParseOwnerGroupResult {
    EmptyOrColon,
    OwnerOnly(u32),
    OwnerGroup((Option<u32>, Option<u32>)),
}

fn parse_owner_group(owner_group: &str) -> Result<ParseOwnerGroupResult, String> {
    if owner_group.is_empty() || owner_group == ":" {
        Ok(ParseOwnerGroupResult::EmptyOrColon)
    } else {
        match owner_group.split_once(':') {
            None => {
                let uid = parse_user(owner_group)?;
                Ok(ParseOwnerGroupResult::OwnerOnly(uid))
            }
            Some((owner, group)) => {
                let uid = if owner.is_empty() {
                    None
                } else {
                    Some(parse_user(owner)?)
                };

                let gid = if group.is_empty() {
                    None
                } else {
                    Some(parse_group(group)?)
                };

                Ok(ParseOwnerGroupResult::OwnerGroup((uid, gid)))
            }
        }
    }
}

fn main() -> Result<(), io::Error> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    let (uid, gid) = match parse_owner_group(&args.owner_group) {
        Ok(owner_group) => match owner_group {
            ParseOwnerGroupResult::EmptyOrColon => {
                // GNU behavior: If `owner_group` is empty or just ":", do nothing
                return Ok(());
            }
            ParseOwnerGroupResult::OwnerOnly(uid) => (Some(uid), None),
            ParseOwnerGroupResult::OwnerGroup(pair) => match pair {
                (None, None) => {
                    // Should have been handled in the `ParseOwnerGroupResult::EmptyOrColon` branch
                    unreachable!()
                }
                (None, Some(gid)) => {
                    // `chown :group f` is equivalent to `chgrp group f`
                    (None, Some(gid))
                }
                (Some(_), None) => {
                    // `chown owner: f` is invalid. There needs to be a group after the :
                    let err_str = gettext!("invalid spec: '{}'", &args.owner_group);
                    eprintln!("chown: {}", err_str);
                    std::process::exit(1);
                }
                (Some(uid), Some(gid)) => (Some(uid), Some(gid)),
            },
        },
        Err(err_str) => {
            eprintln!("chown: {}", err_str);
            std::process::exit(1);
        }
    };

    // apply the group to each file
    for filename in &args.files {
        let success = chown_traverse(
            filename,
            uid,
            gid,
            &args.delegate,
            |e: io::Error, path: ftw::DisplayablePath| {
                let err_str = match e.kind() {
                    io::ErrorKind::PermissionDenied => {
                        gettext!("cannot read directory '{}': {}", path, error_string(&e))
                    }
                    io::ErrorKind::NotFound => {
                        gettext!("cannot access '{}': {}", path, error_string(&e))
                    }
                    _ => {
                        gettext!("changing ownership of '{}': {}", path, error_string(&e))
                    }
                };
                eprintln!("chown: {}", err_str);
            },
            |e: io::Error, path: ftw::DisplayablePath| {
                let err_str = if uid.is_none() {
                    gettext!("changing group of '{}': {}", path, error_string(&e))
                } else {
                    gettext!("changing ownership of '{}': {}", path, error_string(&e))
                };

                eprintln!("chown: {}", err_str);
            },
        );
        if !success {
            exit_code = 1;
        }
    }

    std::process::exit(exit_code)
}
