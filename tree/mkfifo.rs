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
use std::io;

/// mkfifo - make FIFO special files
#[derive(Parser)]
#[command(version, about = gettext("mkfifo - make FIFO special files"))]
struct Args {
    #[arg(short, long, allow_hyphen_values = true, help = gettext("Set the file permission bits of the newly-created FIFO to the specified mode value"))]
    mode: Option<String>,

    #[arg(help = gettext("A pathname of the FIFO special file to be created"))]
    files: Vec<String>,
}

fn do_mkfifo(filename: &str, mode: &ChmodMode, explicit_mode: bool) -> io::Result<()> {
    let mode_val = match mode {
        ChmodMode::Absolute(mode, _) => *mode,
        ChmodMode::Symbolic(sym) => modestr::mutate(0o666, false, sym),
    };

    // When mode is explicitly specified with -m, bypass umask per POSIX spec
    // When no mode is specified, let umask apply normally
    let old_umask = if explicit_mode {
        unsafe { libc::umask(0) }
    } else {
        0 // Dummy value, won't be used
    };

    let res = unsafe {
        libc::mkfifo(
            filename.as_ptr() as *const libc::c_char,
            mode_val as libc::mode_t,
        )
    };

    // Restore the original umask if we changed it
    if explicit_mode {
        unsafe { libc::umask(old_umask) };
    }

    if res < 0 {
        return Err(io::Error::last_os_error());
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
    let explicit_mode = args.mode.is_some();
    let mode = match args.mode {
        Some(mode) => modestr::parse(&mode)?,
        None => ChmodMode::Absolute(0o666, 3),
    };

    // apply the mode to each file
    for filename in &args.files {
        if let Err(e) = do_mkfifo(filename, &mode, explicit_mode) {
            exit_code = 1;
            eprintln!("{}: {}", filename, e);
        }
    }

    std::process::exit(exit_code)
}
