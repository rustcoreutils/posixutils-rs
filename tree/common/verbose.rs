//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The lines `-v` writes to standard output, one per step done.
//!
//! A step is done whether or not its line can be written: a copy, move or
//! removal goes on to the end when standard output fails, and only then is
//! the failure reported, once, as `UTILITY: write error: REASON`, and the
//! utility exits 1 (GNU behaviour).  `println!` would instead end the
//! process at the first failed line, mid-tree.

use super::error_string;
use gettextrs::gettext;
use std::io::{self, Write};
use std::sync::Mutex;

/// The first write of a `-v` line that failed.
static WRITE_ERROR: Mutex<Option<io::Error>> = Mutex::new(None);

/// Write `line` and a <newline> to standard output, as a `-v` report of a step just done.
pub fn report_verbose(line: &str) {
    report_verbose_bytes(line.as_bytes());
}

/// [`report_verbose`] for a line given as bytes, which need not be text in the locale's
/// encoding.  After a write has failed, the rest are not tried.
pub fn report_verbose_bytes(line: &[u8]) {
    let mut failed = WRITE_ERROR.lock().unwrap_or_else(|e| e.into_inner());
    if failed.is_some() {
        return;
    }
    let mut out = io::stdout().lock();
    let written = out
        .write_all(line)
        .and_then(|()| out.write_all(b"\n"))
        .and_then(|()| out.flush());
    if let Err(err) = written {
        *failed = Some(err);
    }
}

/// End the process once its work is done: status 0 when `ok` and every `-v` line was
/// written; otherwise 1, after reporting a failed `-v` write.
pub fn exit_after_verbose(ok: bool) -> ! {
    let failed = WRITE_ERROR.lock().unwrap_or_else(|e| e.into_inner()).take();
    if let Some(err) = &failed {
        plib::diag::error(&format!(
            "{}: {}",
            gettext("write error"),
            error_string(err)
        ));
    }
    std::process::exit(if ok && failed.is_none() { 0 } else { 1 })
}
