//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::OsString;
use std::io::{self, Write};
use std::os::unix::ffi::OsStrExt;
use std::path::PathBuf;

use clap::Parser;
use gettextrs::gettext;
use plib::diag;

#[derive(Parser)]
#[command(
    version,
    about = gettext("dirname - return the directory portion of a pathname")
)]
struct Args {
    #[arg(allow_hyphen_values = true)]
    pathname: OsString,
}

fn show_dirname(args: &Args) {
    let result: OsString = if args.pathname.is_empty() {
        OsString::from(".")
    } else {
        let mut pb = PathBuf::from(&args.pathname);
        pb.pop();
        if pb.as_os_str().is_empty() {
            OsString::from(".")
        } else {
            pb.into_os_string()
        }
    };

    // FUTURE DIRECTIONS: treat an embedded <newline> in the output as an error.
    if result.as_bytes().contains(&b'\n') {
        diag::error(&gettext("result contains a newline character"));
        return;
    }

    let mut out = io::stdout().lock();
    let _ = out.write_all(result.as_bytes());
    let _ = out.write_all(b"\n");
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    diag::init_locale("dirname");

    let args = Args::parse();

    show_dirname(&args);

    std::process::exit(diag::exit_status())
}
