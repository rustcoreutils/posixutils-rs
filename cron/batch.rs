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
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    // batch is `at now`: schedule for the current absolute instant (audit #B4).
    let time = Utc::now();

    let cmd = read_commands_from_stdin("batch", &time)?;

    // batch is equivalent to `at -q b -m now`.
    let _ = at(Some('b'), &time, cmd, true).inspect_err(|err| print_err_and_exit(1, err));

    Ok(())
}
