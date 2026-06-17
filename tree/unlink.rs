//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::ffi::CString;
use std::io;

/// unlink - call the unlink function
#[derive(Parser)]
#[command(version, about = gettext("unlink - call the unlink function"))]
struct Args {
    /// An existing pathname to be unlinked (removed).
    pathname: String,
}

fn do_unlink(pathname: &str) -> io::Result<()> {
    // POSIX: "perform the function call: unlink(path);" — use the syscall verbatim (#UN2).
    let c_path =
        CString::new(pathname).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;
    if unsafe { libc::unlink(c_path.as_ptr()) } != 0 {
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

    if let Err(e) = do_unlink(&args.pathname) {
        exit_code = 1;
        eprintln!("unlink: {}", gettext!("{}: {}", args.pathname, e));
    }

    std::process::exit(exit_code)
}
