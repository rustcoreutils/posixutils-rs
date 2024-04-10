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

extern crate clap;
extern crate libc;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::ffi::CString;
use std::path::Path;
use std::{fs, io};

/// chgrp - change file group ownership
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

    /// A group name from the group database or a numeric group ID
    group: String,

    /// The files to change
    files: Vec<String>,
}

fn chgrp_file(filename: &str, gid: u32, recurse: bool) -> Result<(), io::Error> {
    let path = Path::new(filename);
    let metadata = fs::metadata(path)?;

    // recurse into directories
    if metadata.is_dir() && recurse {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let entry_path = entry.path();
            let entry_filename = entry_path.to_str().unwrap();
            chgrp_file(entry_filename, gid, recurse)?;
        }
    }

    // change the group
    let pathstr = CString::new(filename).unwrap();
    unsafe {
        if libc::chown(pathstr.as_ptr(), libc::geteuid(), gid) != 0 {
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    // initialize translations
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    // lookup string group by name, or parse numeric group ID
    let gid = parse_group(&args.group)?;

    // apply the group to each file
    for filename in &args.files {
        if let Err(e) = chgrp_file(filename, gid, args.recurse) {
            exit_code = 1;
            eprintln!("{}: {}", filename, e);
        }
    }

    std::process::exit(exit_code)
}
