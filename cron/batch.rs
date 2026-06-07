//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::{Local, TimeZone, Utc};
use cron::spool::{at, print_err_and_exit};
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};

use std::io::{BufRead, Write};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let time = {
        let datetime_utc = Utc::now();
        let datetime_local = datetime_utc.with_timezone(&Local);
        Utc.from_local_datetime(&datetime_local.naive_local())
            .unwrap()
    };

    let cmd = {
        let stdout = std::io::stdout();
        let mut stdout_lock = stdout.lock();

        writeln!(&mut stdout_lock, "at {}", time.to_rfc2822())?;
        write!(&mut stdout_lock, "at> ")?;
        stdout_lock.flush()?;

        let stdin = std::io::stdin();
        let mut stdin_lock = stdin.lock();

        let mut result = Vec::new();
        let mut buf = String::new();

        while stdin_lock.read_line(&mut buf)? != 0 {
            write!(&mut stdout_lock, "at> ")?;
            stdout_lock.flush()?;

            result.push(buf.to_owned());
        }

        writeln!(&mut stdout_lock, "<EOT>")?;
        stdout_lock.flush()?;

        result.join("\n")
    };

    // batch is equivalent to `at -q b -m now`.
    let _ = at(Some('b'), &time, cmd, true).inspect_err(|err| print_err_and_exit(1, err));

    Ok(())
}
