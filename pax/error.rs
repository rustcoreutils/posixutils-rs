//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fmt;
use std::io;
use std::sync::atomic::{AtomicBool, AtomicU8, Ordering};

/// Error type for pax operations
#[derive(Debug)]
pub enum PaxError {
    /// I/O error
    Io(io::Error),
    /// Invalid archive format
    InvalidFormat(String),
    /// Invalid header field
    InvalidHeader(String),
    /// Path too long for format
    PathTooLong(String),
    /// Pattern matching error
    PatternError(String),
    /// Malformed command line: the message is already a complete diagnostic
    Usage(String),
    /// Nothing left to do and nothing went wrong -- `--help` and `--version`
    /// have already written their output and want a success exit.
    EarlyExit,
}

impl fmt::Display for PaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // The fixed clause of each diagnostic is routed through gettext so it can
        // be localized; the variable detail is interpolated outside. In the C
        // locale gettext returns the message unchanged, so output is unchanged.
        use gettextrs::gettext;
        match self {
            PaxError::Io(e) => write!(f, "{}: {}", gettext("I/O error"), e),
            PaxError::InvalidFormat(msg) => {
                write!(f, "{}: {}", gettext("Invalid archive format"), msg)
            }
            PaxError::InvalidHeader(msg) => write!(f, "{}: {}", gettext("Invalid header"), msg),
            PaxError::PathTooLong(path) => write!(f, "{}: {}", gettext("Path too long"), path),
            PaxError::PatternError(msg) => write!(f, "{}: {}", gettext("Pattern error"), msg),
            PaxError::Usage(msg) => write!(f, "{}", msg),
            PaxError::EarlyExit => Ok(()),
        }
    }
}

impl std::error::Error for PaxError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            PaxError::Io(e) => Some(e),
            _ => None,
        }
    }
}

impl From<io::Error> for PaxError {
    fn from(err: io::Error) -> Self {
        PaxError::Io(err)
    }
}

/// Result type for pax operations
pub type PaxResult<T> = Result<T, PaxError>;

/// Check if a PaxError represents end-of-file
///
/// This is preferred over string matching on error messages,
/// which is fragile across different platforms and Rust versions.
pub fn is_eof_error(error: &PaxError) -> bool {
    match error {
        PaxError::Io(e) => e.kind() == io::ErrorKind::UnexpectedEof,
        _ => false,
    }
}

/// Process-wide "an error occurred" flag. Per POSIX CONSEQUENCES OF ERRORS, a
/// per-file failure (or an unmatched pattern/operand) shall be diagnosed and a
/// non-zero exit status returned, but processing continues; this flag carries
/// that deferred non-zero status to the single exit point in `main`.
static HAD_ERROR: AtomicBool = AtomicBool::new(false);

/// Mark the run as failed (a non-fatal error occurred). Processing continues,
/// but `main` will return a non-zero exit status.
pub fn note_error() {
    HAD_ERROR.store(true, Ordering::Relaxed);
}

/// Whether any non-fatal error has been reported during this run.
pub fn had_error() -> bool {
    HAD_ERROR.load(Ordering::Relaxed)
}

/// The name this run was invoked under: `pax`, or `tar`/`cpio` when the
/// compatibility front-ends are driving. Diagnostics are prefixed with it, so a
/// script that greps its own stderr sees the command it actually ran.
static PROGRAM_NAME: AtomicU8 = AtomicU8::new(0);

/// Record which command line was parsed, for diagnostic prefixes.
pub fn set_program_name(name: &'static str) {
    let code = match name {
        "tar" => 1,
        "cpio" => 2,
        _ => 0,
    };
    PROGRAM_NAME.store(code, Ordering::Relaxed);
}

/// The name to prefix diagnostics with.
pub fn program_name() -> &'static str {
    match PROGRAM_NAME.load(Ordering::Relaxed) {
        1 => "tar",
        2 => "cpio",
        _ => "pax",
    }
}

/// Write a per-item diagnostic to standard error in the `pax: <context>: <err>`
/// form and flag the run as failed, so that processing can continue (per POSIX)
/// while the final exit status is still non-zero.
pub fn report_error(context: impl fmt::Display, err: impl fmt::Display) {
    eprintln!("{}: {}: {}", program_name(), context, err);
    note_error();
}
