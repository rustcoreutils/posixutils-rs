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
use plib::printf::printf;
use std::{io, process::ExitCode};

fn main() -> ExitCode {
    diag::init_locale("printf");

    let args = std::env::args_os().collect::<Vec<_>>();

    match args.as_slice() {
        &[ref _program_file_name, ref format, ref arguments @ ..] => {
            let arguments_vec: Vec<&[u8]> =
                arguments.iter().map(|os| os.as_encoded_bytes()).collect();

            let mut stdout = io::stdout().lock();
            let mut report = |msg: &str| diag::error(msg);
            match printf(
                &mut stdout,
                format.as_encoded_bytes(),
                &arguments_vec,
                &mut report,
            ) {
                Ok(had_error) => {
                    if had_error {
                        ExitCode::FAILURE
                    } else {
                        ExitCode::SUCCESS
                    }
                }
                Err(er) => {
                    diag::error(&er.to_string());
                    ExitCode::FAILURE
                }
            }
        }
        _ => {
            diag::error(&gettext("not enough arguments"));
            ExitCode::FAILURE
        }
    }
}
