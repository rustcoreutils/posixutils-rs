//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub mod group;
pub mod io;
pub mod modestr;
pub mod testing;

pub const PROJECT_NAME: &'static str = "posixutils-rs";

pub const BUFSZ: usize = 8 * 1024;

pub const TERM_VAR: &'static str = "TERM";
pub const DEFAULT_TERM: &'static str = "vt100";

pub use testing::*;

pub fn get_terminal() -> String {
    let term: String = match std::env::var(TERM_VAR) {
        Ok(val) => val,
        Err(_) => String::new(),
    };

    if term.is_empty() {
        String::from(DEFAULT_TERM)
    } else {
        term
    }
}
