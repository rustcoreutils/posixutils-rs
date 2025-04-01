//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub mod args;
pub mod terminal;
pub mod vi;

pub fn clear_line() {
    print!("\r\x1b[K");
}

pub fn set_cursor_pos(pos: usize) {
    print!("\r\x1b[{}G", pos + 1);
}
