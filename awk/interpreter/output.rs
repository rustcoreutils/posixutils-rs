//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! awk's standard output.
//!
//! On a terminal it is written a line at a time, as Rust's standard output
//! does.  Otherwise it is collected in a buffer and written in large
//! pieces, so that a program printing a record at a time does not make a
//! system call for each.  The interpreter flushes the buffer before it
//! starts or waits for a command (whose output must come after awk's
//! output so far), when `fflush` asks, and at the end of the run.

use std::cell::RefCell;
use std::io::{IsTerminal, Write};

/// How much output is collected before it is written.
const BUFFER_SIZE: usize = 64 * 1024;

thread_local! {
    /// Output collected for standard output; `None` when it is a terminal,
    /// which is written to at once.
    static BUFFER: RefCell<Option<Vec<u8>>> = RefCell::new(
        (!std::io::stdout().is_terminal()).then(|| Vec::with_capacity(BUFFER_SIZE)),
    );
}

/// The system's text for the first failed write to standard output, if one
/// failed. It ends the run like any runtime error, but is reported as the
/// write error it is (see [`stdout_write_error`]).
static STDOUT_WRITE_ERROR: std::sync::OnceLock<String> = std::sync::OnceLock::new();

/// Why writing standard output failed, if it did.
pub fn stdout_write_error() -> Option<&'static str> {
    STDOUT_WRITE_ERROR.get().map(String::as_str)
}

/// The error for the failed write `e` to standard output, remembered for
/// [`stdout_write_error`].
fn write_error(e: std::io::Error) -> String {
    let reason = STDOUT_WRITE_ERROR.get_or_init(|| plib::diag::io_error_text(&e));
    format!("{}: {reason}", gettextrs::gettext("write error"))
}

/// Writes `s` to standard output, as the bytes awk writes for it.
pub fn write_stdout(s: &str) -> Result<(), String> {
    let bytes = crate::charset::encode(s);
    let full = BUFFER.with_borrow_mut(|buffer| match buffer {
        Some(buffer) => {
            buffer.extend_from_slice(&bytes);
            Ok(buffer.len() >= BUFFER_SIZE)
        }
        None => std::io::stdout().write_all(&bytes).map(|_| false),
    });
    match full {
        Ok(true) => flush_stdout(),
        Ok(false) => Ok(()),
        Err(e) => Err(write_error(e)),
    }
}

/// Writes out what standard output has collected.
pub fn flush_stdout() -> Result<(), String> {
    let pending = BUFFER.with_borrow_mut(|buffer| match buffer {
        Some(buffer) => std::mem::take(buffer),
        None => Vec::new(),
    });
    let mut stdout = std::io::stdout().lock();
    let result = stdout.write_all(&pending).and_then(|_| stdout.flush());
    // keep the buffer's allocation for what comes next
    BUFFER.with_borrow_mut(|buffer| {
        if let Some(buffer) = buffer {
            if buffer.is_empty() {
                *buffer = pending;
                buffer.clear();
            }
        }
    });
    result.map_err(write_error)
}
