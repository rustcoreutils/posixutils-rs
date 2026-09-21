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

use std::env;
use std::process;
use vi_rs::{run_editor, InvokedAs};

fn main() {
    let invoked_as = InvokedAs::detect();
    plib::diag::init_locale(invoked_as.name());
    // `:!cmd` and `:%!cmd` pipe buffer text into a command the editor spawned;
    // `vi::shell` treats that command closing its input as the command's own
    // choice. Dying by the signal instead would lose the buffer.
    plib::io::ignore_sigpipe();

    let args: Vec<String> = env::args().collect();
    let exit_code = run_editor(invoked_as, &args);
    process::exit(exit_code);
}
