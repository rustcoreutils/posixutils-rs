//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs::remove_file;
use std::process;

use gettextrs::gettext;
use libc::{signal, SIGHUP, SIGINT, SIGQUIT, SIGTERM};

use crate::rule::INTERRUPT_FLAG;

/// Handles incoming signals by setting the interrupt flag and exiting the process.
pub fn handle_signals(signal_code: libc::c_int) {
    let interrupt_flag = INTERRUPT_FLAG.lock().unwrap();
    if let Some((target, precious)) = interrupt_flag.as_ref() {
        eprintln!("{}", gettext("make: Interrupt"));
        // .PRECIOUS special target
        if !precious {
            eprintln!(
                "{}: {} '{}'",
                gettext("make"),
                gettext("Deleting file"),
                target
            );
            if let Err(err) = remove_file(target) {
                eprintln!("{}: {}", gettext("Error deleting file"), err);
            }
        }
    }

    process::exit(128 + signal_code);
}

pub fn register_signals() {
    unsafe {
        signal(SIGINT, handle_signals as usize);
        signal(SIGQUIT, handle_signals as usize);
        signal(SIGTERM, handle_signals as usize);
        signal(SIGHUP, handle_signals as usize);
    }
}
