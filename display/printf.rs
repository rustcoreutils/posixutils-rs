//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use gettextrs::gettext;
use plib::diag;
use plib::printf::{printf, split_operands, usage, USAGE_STATUS};
use std::{io, process::ExitCode};

fn main() -> ExitCode {
    diag::init_locale("printf");

    let args = std::env::args_os().skip(1).collect::<Vec<_>>();
    let args: Vec<&[u8]> = args.iter().map(|os| os.as_encoded_bytes()).collect();

    match split_operands(&args) {
        Some((format, arguments)) => {
            let mut stdout = io::stdout().lock();
            let mut report = |msg: &str| diag::error(msg);
            match printf(&mut stdout, format, arguments, &mut report) {
                Ok(had_error) => {
                    if had_error {
                        ExitCode::FAILURE
                    } else {
                        ExitCode::SUCCESS
                    }
                }
                Err(er) => {
                    // An `io::Error` can only have come from writing the
                    // output; anything else is the format's own error.
                    match er.downcast_ref::<io::Error>() {
                        Some(e) => diag::error(&format!(
                            "{}: {}",
                            gettext("write error"),
                            diag::io_error_text(e)
                        )),
                        None => diag::error(&er.to_string()),
                    }
                    ExitCode::FAILURE
                }
            }
        }
        None => {
            diag::error(&usage());
            ExitCode::from(USAGE_STATUS)
        }
    }
}
