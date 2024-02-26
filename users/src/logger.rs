//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate plib;

use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use syslog::{Facility, Formatter3164};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

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
        Err(e) => eprintln!("Unable to connect to syslog: {:?}", e),
        Ok(mut writer) => {
            writer.err(&log_str).expect("could not write error message");
        }
    }

    Ok(())
}
