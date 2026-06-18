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
use plib::curuser::login_name_strict;
use std::process::ExitCode;

/// logname - return the user's login name
#[derive(Parser)]
#[command(version, about = gettext("logname - return the user's login name"))]
struct Args {}

fn main() -> ExitCode {
    plib::diag::init_locale("logname");

    let _args = Args::parse();

    // POSIX: write the login name as reported by getlogin(). Under the
    // conditions where getlogin() would fail, write a diagnostic to stderr and
    // exit non-zero. Deliberately no $USER / getpwuid fallback — the spec's
    // APPLICATION USAGE warns environment changes could produce wrong results.
    match login_name_strict() {
        Some(name) => {
            println!("{}", name);
            ExitCode::SUCCESS
        }
        None => {
            plib::diag::error(&gettext("no login name"));
            ExitCode::from(1)
        }
    }
}
