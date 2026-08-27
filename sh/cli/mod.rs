//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub mod args;
pub mod terminal;
pub mod vi;

use std::io::Write;

/// The interactive line editor draws on standard error, never standard output.
/// POSIX requires the prompt itself there ("After expansion, the value shall be
/// written to standard error"), and the cursor control that goes with it must
/// follow, or `sh -i > log` fills the log with escape sequences.
fn write_terminal(text: &str) {
    let _ = std::io::stderr().write_all(text.as_bytes());
}

pub fn clear_line() {
    write_terminal("\r\x1b[K");
}

pub fn set_cursor_pos(pos: usize) {
    write_terminal(&format!("\r\x1b[{}G", pos + 1));
}
