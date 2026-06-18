//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::gettext;
use plib::curuser::ttyname_of;
use std::io::{self, IsTerminal};
use std::process::ExitCode;

/// tty - return user's terminal name
#[derive(Parser)]
#[command(version, about = gettext("tty - return user's terminal name"))]
struct Args {}

fn main() -> ExitCode {
    plib::diag::init_locale("tty");

    let _args = Args::parse();

    // POSIX: report the name of the terminal open as *standard input* only.
    if !io::stdin().is_terminal() {
        // Informative output to stdout (not a diagnostic); exit 1.
        println!("{}", gettext("not a tty"));
        return ExitCode::from(1);
    }

    match ttyname_of(libc::STDIN_FILENO) {
        Some(ttyname) => {
            println!("{}", ttyname);
            ExitCode::SUCCESS
        }
        None => {
            // stdin IS a terminal but ttyname() failed: a genuine error
            // (>1), distinct from the "stdin is not a terminal" status of 1.
            plib::diag::error(&gettext("could not determine terminal name"));
            ExitCode::from(2)
        }
    }
}
