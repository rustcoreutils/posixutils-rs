//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::process::ExitCode;

use posixutils_m4::error::GetExitCode;

fn main() -> ExitCode {
    env_logger::init();
    // Honor LC_ALL/LC_CTYPE/LANG (and NLSPATH/LC_MESSAGES for the catalog) so
    // that character-oriented built-ins interpret multibyte input correctly.
    plib::diag::init_locale("m4");
    let args = posixutils_m4::Args::parse();

    let stdout = std::io::stdout();
    let mut stderr = std::io::stderr();
    if let Err(error) = posixutils_m4::run(stdout, &mut stderr, args) {
        ExitCode::from(u8::try_from(error.get_exit_code()).unwrap_or_else(|e| {
            eprintln!("Error casting exit code {e} into platform agnostic u8");
            1
        }))
    } else {
        ExitCode::SUCCESS
    }
}
