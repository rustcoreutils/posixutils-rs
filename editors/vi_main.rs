//
// Copyright (c) 2025-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! vi/ex - POSIX visual/line editor
//!
//! This is the entry point for both vi and ex binaries.
//! The mode is determined by argv[0]: if it ends with "ex",
//! the editor starts in ex (line) mode; otherwise in visual mode.

use std::process;
use vi_rs::{run_editor, InvokedAs};

fn main() {
    let invoked_as = InvokedAs::detect();
    plib::diag::init_locale(invoked_as.name());

    // As editor text, a byte that is not UTF-8 escaped (`vi_rs::bytes`): a
    // file operand is a pathname, which may hold any byte, and so may a
    // `-c` command that names text in such a file.
    let args: Vec<String> = std::env::args_os()
        .map(|a| vi_rs::bytes::from_os(&a))
        .collect();
    let exit_code = run_editor(invoked_as, &args);
    process::exit(exit_code);
}
