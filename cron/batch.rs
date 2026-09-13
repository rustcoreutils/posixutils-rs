//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::Utc;
use cron::spool::{at, print_err_and_exit, read_commands_from_stdin};

fn main() -> std::process::ExitCode {
    // Registers the utility name as well as setting the locale. batch used raw
    // gettextrs, so `print_err_and_exit` had no prefix to print and every
    // failure arrived unattributed.
    plib::diag::init_locale("batch");

    match batch_main() {
        Ok(()) => std::process::ExitCode::SUCCESS,
        Err(e) => {
            plib::diag::error(&plib::diag::error_text(e.as_ref()));
            std::process::ExitCode::FAILURE
        }
    }
}

fn batch_main() -> Result<(), Box<dyn std::error::Error>> {
    // batch is `at now`: schedule for the current absolute instant (audit #B4).
    let time = Utc::now();

    let cmd = read_commands_from_stdin("batch", &time)?;

    // batch is equivalent to `at -q b -m now`.
    let _ = at(Some('b'), &time, cmd, true).inspect_err(|err| print_err_and_exit(1, err));

    Ok(())
}
