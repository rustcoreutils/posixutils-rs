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
use plib::echo::echo_output;
use std::io::{self, Write};
use std::process::ExitCode;

fn main() -> ExitCode {
    diag::init_locale("echo");

    let args: Vec<Vec<u8>> = std::env::args_os()
        .skip(1)
        .map(|arg| arg.as_encoded_bytes().to_vec())
        .collect();
    let echo_bytes = echo_output(&args);

    let mut stdout = io::stdout().lock();
    if let Err(e) = stdout.write_all(&echo_bytes).and_then(|()| stdout.flush()) {
        diag::error(&format!("{}: {e}", gettext("write error")));
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
