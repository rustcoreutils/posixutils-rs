//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate plib;

mod modestr;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use libc::{
    S_IRGRP, S_IROTH, S_IRUSR, S_IRWXG, S_IRWXO, S_IRWXU, S_ISUID, S_ISVTX, S_IWGRP, S_IWOTH,
    S_IWUSR, S_IXGRP, S_IXOTH, S_IXUSR,
};
use modestr::{ChmodActionOp, ChmodMode, ChmodSymbolic};
use plib::PROJECT_NAME;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::{fs, io};

/// chmod - change the file modes
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Recursively change file mode bits.
    #[arg(short = 'R', long)]
    recurse: bool,

    /// Represents the change to be made to the file mode bits of each file named by one of the file operands.
    mode: String,

    /// The files to change
    files: Vec<String>,
}

// apply symbolic mutations to the given file at path
fn set_permissions_symbolic(path: &Path, symbolic: &ChmodSymbolic) -> Result<(), io::Error> {
    let metadata = fs::metadata(path)?;
    let mut perms = metadata.permissions();

    let mut new_mode = perms.mode();
    let mut user = perms.mode() & S_IRWXU as u32;
    let mut group = perms.mode() & S_IRWXG as u32;
    let mut others = perms.mode() & S_IRWXO as u32;

    // apply each clause
    for clause in &symbolic.clauses {
        // apply each action
        for action in &clause.actions {
            match action.op {
                // add bits to the mode
                ChmodActionOp::Add => {
                    if action.copy_user {
                        user |= perms.mode() & S_IRWXU as u32;
                    }
                    if action.copy_group {
                        group |= perms.mode() & S_IRWXG as u32;
                    }
                    if action.copy_others {
                        others |= perms.mode() & S_IRWXO as u32;
                    }
                    if action.read {
                        user |= S_IRUSR as u32;
                        group |= S_IRGRP as u32;
                        others |= S_IROTH as u32;
                    }
                    if action.write {
                        user |= S_IWUSR as u32;
                        group |= S_IWGRP as u32;
                        others |= S_IWOTH as u32;
                    }
                    if action.execute {
                        user |= S_IXUSR as u32;
                        group |= S_IXGRP as u32;
                        others |= S_IXOTH as u32;
                    }
                    if action.execute_dir {
                        user |= S_IXUSR as u32;
                        group |= S_IXGRP as u32;
                        others |= S_IXOTH as u32;
                    }
                    if action.setuid {
                        user |= S_ISUID as u32;
                    }
                    if action.sticky {
                        others |= S_ISVTX as u32;
                    }
                }

                // remove bits from the mode
                ChmodActionOp::Remove => {
                    if action.copy_user {
                        user &= !(perms.mode() & S_IRWXU as u32);
                    }
                    if action.copy_group {
                        group &= !(perms.mode() & S_IRWXG as u32);
                    }
                    if action.copy_others {
                        others &= !(perms.mode() & S_IRWXO as u32);
                    }
                    if action.read {
                        user &= !S_IRUSR as u32;
                        group &= !S_IRGRP as u32;
                        others &= !S_IROTH as u32;
                    }
                    if action.write {
                        user &= !S_IWUSR as u32;
                        group &= !S_IWGRP as u32;
                        others &= !S_IWOTH as u32;
                    }
                    if action.execute {
                        user &= !S_IXUSR as u32;
                        group &= !S_IXGRP as u32;
                        others &= !S_IXOTH as u32;
                    }
                    if action.execute_dir {
                        user &= !S_IXUSR as u32;
                        group &= !S_IXGRP as u32;
                        others &= !S_IXOTH as u32;
                    }
                    if action.setuid {
                        user &= !S_ISUID as u32;
                    }
                    if action.sticky {
                        others &= !S_ISVTX as u32;
                    }
                }

                // set the mode bits
                ChmodActionOp::Set => {
                    if action.copy_user {
                        user = perms.mode() & S_IRWXU as u32;
                    } else {
                        user = 0;
                    }
                    if action.copy_group {
                        group = perms.mode() & S_IRWXG as u32;
                    } else {
                        group = 0;
                    }
                    if action.copy_others {
                        others = perms.mode() & S_IRWXO as u32;
                    } else {
                        others = 0;
                    }
                    if action.read {
                        user |= S_IRUSR as u32;
                        group |= S_IRGRP as u32;
                        others |= S_IROTH as u32;
                    }
                    if action.write {
                        user |= S_IWUSR as u32;
                        group |= S_IWGRP as u32;
                        others |= S_IWOTH as u32;
                    }
                    if action.execute {
                        user |= S_IXUSR as u32;
                        group |= S_IXGRP as u32;
                        others |= S_IXOTH as u32;
                    }
                    if action.execute_dir {
                        user |= S_IXUSR as u32;
                        group |= S_IXGRP as u32;
                        others |= S_IXOTH as u32;
                    }
                    if action.setuid {
                        user |= S_ISUID as u32;
                    }
                    if action.sticky {
                        others |= S_ISVTX as u32;
                    }
                }
            }
        }

        // apply the clause
        if clause.user {
            new_mode = (new_mode & !S_IRWXU as u32) | user;
        }
        if clause.group {
            new_mode = (new_mode & !S_IRWXG as u32) | group;
        }
        if clause.others {
            new_mode = (new_mode & !S_IRWXO as u32) | others;
        }
    }

    // update path in filesystem
    perms.set_mode(new_mode);
    fs::set_permissions(path, perms)?;

    Ok(())
}

fn chmod_file(filename: &str, mode: &ChmodMode, recurse: bool) -> Result<(), io::Error> {
    let path = Path::new(filename);
    let metadata = fs::metadata(path)?;

    if metadata.is_dir() && recurse {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let entry_path = entry.path();
            let entry_filename = entry_path.to_str().unwrap();
            chmod_file(entry_filename, mode, recurse)?;
        }
    }

    match mode {
        // set the mode bits to the given value
        ChmodMode::Absolute(m) => {
            fs::set_permissions(path, fs::Permissions::from_mode(*m))?;
        }

        // apply symbolic mutations to the mode bits
        ChmodMode::Symbolic(s) => {
            set_permissions_symbolic(path, s)?;
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    // initialize translations
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    // parse the mode string
    let mode = modestr::parse(&args.mode)?;

    // apply the mode to each file
    for filename in &args.files {
        if let Err(e) = chmod_file(filename, &mode, args.recurse) {
            exit_code = 1;
            eprintln!("{}: {}", filename, e);
        }
    }

    std::process::exit(exit_code)
}
