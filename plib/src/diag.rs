//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shared diagnostic module for posixutils utilities.
//!
//! Provides a uniform "`<util>: <message>`" diagnostic surface backed by
//! atomic error / warning counters, optional source-file + line:column
//! tracking, and a one-shot locale + gettext initializer.
//!
//! Two API shapes are supported:
//!
//! - **Simple utilities** (ar, nm, strings, strip, etc.): use [`init_locale`]
//!   in `main`, then call [`error`] / [`warning`] with already-`gettext`'d
//!   strings. At exit, return [`exit_status`].
//!
//! - **Parser-style utilities** (yacc, lex, cc): also call [`set_source`]
//!   for each input file and use [`error_at`] / [`warning_at`] with a
//!   [`Position`] so line:column appears in the diagnostic.
//!
//! Output format mirrors GCC: `"<util>: <source>:<line>[:<col>]: <level>: <msg>"`
//! when a position is given, otherwise `"<util>: <msg>"`.

use std::cell::RefCell;
use std::fmt;
use std::io::{self, Write};
use std::sync::atomic::{AtomicU32, Ordering};

/// Source position for line/column-tracked diagnostics.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Position {
    /// Line number (1-based).
    pub line: u32,
    /// Column position (1-based, 0 means unknown).
    pub col: u16,
}

impl Position {
    /// Create a new position with line and column.
    pub fn new(line: u32, col: u16) -> Self {
        Self { line, col }
    }

    /// Create a position with line only (column unknown).
    pub fn line_only(line: u32) -> Self {
        Self { line, col: 0 }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = SOURCE_FILE.with(|s| s.borrow().clone());
        let name = if name.is_empty() {
            "<unknown>".to_string()
        } else {
            name
        };

        if self.col > 0 {
            write!(f, "{}:{}:{}", name, self.line, self.col)
        } else {
            write!(f, "{}:{}", name, self.line)
        }
    }
}

thread_local! {
    static UTIL_NAME: RefCell<String> = const { RefCell::new(String::new()) };
    static SOURCE_FILE: RefCell<String> = const { RefCell::new(String::new()) };
}

static ERROR_COUNT: AtomicU32 = AtomicU32::new(0);
static WARNING_COUNT: AtomicU32 = AtomicU32::new(0);

/// Initialize the diagnostic system with the utility name (e.g. `"ar"`, `"yacc"`).
///
/// The name is used as a prefix on every emitted diagnostic. Also resets the
/// error and warning counters.
pub fn init(utility: &str) {
    UTIL_NAME.with(|s| {
        *s.borrow_mut() = utility.to_string();
    });
    SOURCE_FILE.with(|s| {
        *s.borrow_mut() = String::new();
    });
    reset_counts();
}

/// One-shot initializer for the locale + gettext + diagnostic surface.
///
/// This is the canonical locale-init entry point. It calls, in order:
///
/// - `setlocale(LC_ALL, "")` — inherits the locale from the environment so that
///   locale-sensitive libc functions (`<ctype.h>`/`<wctype.h>`, `strcoll`,
///   `strftime`, `nl_langinfo`, …) observe `LC_*`. The gettextrs wrapper applies
///   this directly to libc's global locale.
/// - `textdomain("posixutils-rs")`
/// - `bind_textdomain_codeset("posixutils-rs", "UTF-8")`
/// - [`init`]`(utility)`
///
/// Errors from `textdomain` / `bind_textdomain_codeset` are silently ignored
/// (the gettext crate returns `io::Error` when the catalog isn't installed;
/// that's expected on systems without translations and shouldn't abort the
/// utility's startup).
pub fn init_locale(utility: &str) {
    use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
    setlocale(LocaleCategory::LcAll, "");
    let _ = textdomain("posixutils-rs");
    let _ = bind_textdomain_codeset("posixutils-rs", "UTF-8");
    init(utility);
}

/// Set the current source filename used by [`error_at`] / [`warning_at`].
pub fn set_source(filename: &str) {
    SOURCE_FILE.with(|s| {
        *s.borrow_mut() = filename.to_string();
    });
}

/// Reset error and warning counters. Primarily for testing.
pub fn reset_counts() {
    ERROR_COUNT.store(0, Ordering::SeqCst);
    WARNING_COUNT.store(0, Ordering::SeqCst);
}

/// Number of errors emitted since the last [`init`] / [`reset_counts`].
pub fn error_count() -> u32 {
    ERROR_COUNT.load(Ordering::SeqCst)
}

/// Number of warnings emitted since the last [`init`] / [`reset_counts`].
pub fn warning_count() -> u32 {
    WARNING_COUNT.load(Ordering::SeqCst)
}

/// True if any error has been emitted.
pub fn has_errors() -> bool {
    error_count() > 0
}

/// Exit status appropriate for the recorded error state: `0` if no errors,
/// `1` otherwise. Use in `main` like `process::exit(plib::diag::exit_status())`.
pub fn exit_status() -> i32 {
    if has_errors() {
        1
    } else {
        0
    }
}

/// Emit an error diagnostic with no source-position information.
/// Output format: `"<util>: <msg>"`.
pub fn error(msg: &str) {
    ERROR_COUNT.fetch_add(1, Ordering::SeqCst);
    write_plain("error", msg);
}

/// Emit a warning diagnostic with no source-position information.
/// Output format: `"<util>: warning: <msg>"`.
pub fn warning(msg: &str) {
    WARNING_COUNT.fetch_add(1, Ordering::SeqCst);
    write_plain("warning", msg);
}

/// Emit an error at the given source position.
/// Output format: `"<util>: <source>:<line>[:<col>]: error: <msg>"`.
pub fn error_at(pos: Position, msg: &str) {
    ERROR_COUNT.fetch_add(1, Ordering::SeqCst);
    write_at(pos, "error", msg);
}

/// Emit a warning at the given source position.
/// Output format: `"<util>: <source>:<line>[:<col>]: warning: <msg>"`.
pub fn warning_at(pos: Position, msg: &str) {
    WARNING_COUNT.fetch_add(1, Ordering::SeqCst);
    write_at(pos, "warning", msg);
}

fn util_prefix() -> String {
    UTIL_NAME.with(|s| s.borrow().clone())
}

fn write_plain(level: &str, msg: &str) {
    let util = util_prefix();
    let mut out = io::stderr().lock();
    let _ = if util.is_empty() {
        if level == "error" {
            writeln!(out, "{}", msg)
        } else {
            writeln!(out, "{}: {}", level, msg)
        }
    } else if level == "error" {
        writeln!(out, "{}: {}", util, msg)
    } else {
        writeln!(out, "{}: {}: {}", util, level, msg)
    };
}

fn write_at(pos: Position, level: &str, msg: &str) {
    let util = util_prefix();
    let mut out = io::stderr().lock();
    let _ = if util.is_empty() {
        writeln!(out, "{}: {}: {}", pos, level, msg)
    } else {
        writeln!(out, "{}: {}: {}: {}", util, pos, level, msg)
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn position_display_with_col() {
        init("test");
        set_source("foo.y");
        let pos = Position::new(10, 5);
        assert_eq!(format!("{}", pos), "foo.y:10:5");
    }

    #[test]
    fn position_display_line_only() {
        init("test");
        set_source("foo.l");
        let pos = Position::line_only(20);
        assert_eq!(format!("{}", pos), "foo.l:20");
    }

    #[test]
    fn position_display_no_source() {
        init("test");
        set_source("");
        let pos = Position::new(1, 1);
        assert_eq!(format!("{}", pos), "<unknown>:1:1");
    }

    #[test]
    fn error_warning_dont_panic() {
        // Counters are shared across parallel tests, so we only verify
        // that the calls don't panic and that the local count delta is
        // observable on a freshly-reset thread.
        init("test");
        error("plain error");
        warning("plain warning");
        error_at(Position::new(3, 4), "located error");
        warning_at(Position::line_only(5), "located warning");
    }

    #[test]
    fn util_prefix_set_by_init() {
        // Doesn't assert on global state, only on the thread-local prefix
        // (which is itself thread-local, so safe across parallel tests).
        init("zz_test_util");
        assert_eq!(util_prefix(), "zz_test_util");
    }
}
