//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use modestr::ChmodMode;
use plib::modestr;
use std::ffi::CString;
use std::io;
use std::path::PathBuf;

/// mkdir - make directories
#[derive(Parser)]
#[command(version, about = gettext("mkdir - make directories"))]
struct Args {
    #[arg(short, long, help = gettext("Create any missing intermediate pathname components"))]
    parents: bool,

    #[arg(short, long, help = gettext("Set the file permission bits of the newly-created directory to the specified mode value"))]
    mode: Option<String>,

    #[arg(help = gettext("A pathname of a directory to be created"))]
    dirs: Vec<String>,
}

fn create_dir_with_mode(path: &str, mode: u32) -> io::Result<()> {
    let c_path = CString::new(path).expect("CString::new failed");
    let c_mode = mode as libc::mode_t;

    let result = unsafe { libc::mkdir(c_path.as_ptr(), c_mode) };
    if result == 0 {
        Ok(())
    } else {
        Err(io::Error::last_os_error())
    }
}

fn do_mkdir(dirname: &str, mode: &ChmodMode, parents: bool) -> io::Result<()> {
    let mode_val = match mode {
        ChmodMode::Absolute(mode, _) => *mode,
        ChmodMode::Symbolic(sym) => modestr::mutate(0o777, true, sym),
    };

    if parents {
        let mut path = PathBuf::new();
        for part in dirname.split('/') {
            path.push(part);
            if path.is_dir() {
                continue;
            }
            create_dir_with_mode(&path.to_string_lossy(), mode_val)?;
        }
    } else {
        create_dir_with_mode(dirname, mode_val)?;
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    // parse the mode string
    let mode = match args.mode {
        Some(mode) => modestr::parse(&mode)?,
        None => ChmodMode::Absolute(0o777, 3),
    };

    // apply the mode to each file
    for dirname in &args.dirs {
        if let Err(e) = do_mkdir(dirname, &mode, args.parents) {
            exit_code = 1;
            eprintln!("{}: {}", dirname, e);
        }
    }

    std::process::exit(exit_code)
}
