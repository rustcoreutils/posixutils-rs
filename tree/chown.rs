//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - implement -h, -H, -L, -P
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::ffi::CString;
use std::os::unix::fs::MetadataExt;
use std::path::Path;
use std::{fs, io};

/// chown - change the file ownership
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Change symbolic links, rather than the files they point to
    #[arg(short = 'h', long)]
    no_derereference: bool,

    /// Follow command line symlinks during -R recursion
    #[arg(short = 'H', long)]
    follow_cli: bool,

    /// Follow symlinks during -R recursion
    #[arg(short = 'L', group = "deref")]
    dereference: bool,

    /// Never follow symlinks during -R recursion
    #[arg(short = 'P', group = "deref")]
    no_dereference2: bool,

    /// Recursively change groups of directories and their contents
    #[arg(short, short_alias = 'R', long)]
    recurse: bool,

    /// Owner and group are changed to OWNER[:GROUP]
    owner_group: String,

    /// The files to change
    files: Vec<String>,
}

fn chown_file(filename: &str, uid: u32, gid: Option<u32>, recurse: bool) -> Result<(), io::Error> {
    let path = Path::new(filename);
    let metadata = fs::metadata(path)?;

    // recurse into directories
    if metadata.is_dir() && recurse {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let entry_path = entry.path();
            let entry_filename = entry_path.to_str().unwrap();
            chown_file(entry_filename, uid, gid, recurse)?;
        }
    }

    // change the user, and optionally, the group
    let pathstr = CString::new(filename).unwrap();
    let gid = {
        if let Some(gid) = gid {
            gid
        } else {
            metadata.gid()
        }
    };
    unsafe {
        if libc::chown(pathstr.as_ptr(), uid, gid) != 0 {
            return Err(io::Error::last_os_error());
        }
    }

    Ok(())
}

// lookup string group by name, or parse numeric group ID
fn parse_group(group: &str) -> Result<u32, &'static str> {
    match group.parse::<u32>() {
        Ok(gid) => Ok(gid),
        Err(_) => {
            // lookup group by name
            let group_cstr = CString::new(group).unwrap();
            let group = unsafe { libc::getgrnam(group_cstr.as_ptr()) };
            if group.is_null() {
                return Err("group not found");
            }

            let gid = unsafe { (*group).gr_gid };
            Ok(gid)
        }
    }
}

// lookup string user by name, or parse numeric user ID
fn parse_user(user: &str) -> Result<u32, &'static str> {
    match user.parse::<u32>() {
        Ok(uid) => Ok(uid),
        Err(_) => {
            // lookup user by name
            let user_cstr = CString::new(user).unwrap();
            let user = unsafe { libc::getpwnam(user_cstr.as_ptr()) };
            if user.is_null() {
                return Err("user not found");
            }

            let uid = unsafe { (*user).pw_uid };
            Ok(uid)
        }
    }
}

fn parse_owner_group(owner_group: &str) -> Result<(u32, Option<u32>), &'static str> {
    match owner_group.split_once(':') {
        None => {
            let uid = parse_user(owner_group)?;
            Ok((uid, None))
        }
        Some((owner, group)) => {
            let uid = parse_user(owner)?;
            let gid = parse_group(group)?;
            Ok((uid, Some(gid)))
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    // initialize translations
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    // lookup the owner and group
    let (uid, gid) = parse_owner_group(&args.owner_group)?;

    // apply the group to each file
    for filename in &args.files {
        if let Err(e) = chown_file(filename, uid, gid, args.recurse) {
            exit_code = 1;
            eprintln!("{}: {}", filename, e);
        }
    }

    std::process::exit(exit_code)
}
