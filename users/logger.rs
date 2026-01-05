//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use gettextrs::{LocaleCategory, bind_textdomain_codeset, setlocale, textdomain};
use std::process::ExitCode;
use syslog::{Facility, Formatter3164};

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let mut args: Vec<String> = std::env::args().collect();
    args.remove(0);
    let log_str = args.join(" ");

    let formatter = Formatter3164 {
        facility: Facility::LOG_USER,
        hostname: None,
        process: "logger".into(),
        pid: 0,
    };

    match syslog::unix(formatter) {
        Err(e) => {
            eprintln!("logger: unable to connect to syslog: {:?}", e);
            ExitCode::from(1)
        }
        Ok(mut writer) => match writer.err(&log_str) {
            Ok(()) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("logger: could not write message: {:?}", e);
                ExitCode::from(1)
            }
        },
    }
}
