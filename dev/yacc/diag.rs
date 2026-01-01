//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Diagnostic module for yacc - provides source position tracking and
// GCC-compatible error/warning output.
//

use std::cell::RefCell;
use std::fmt;
use std::io::{self, Write};
use std::sync::atomic::{AtomicU32, Ordering};

// ============================================================================
// Source Position
// ============================================================================

/// Source position tracking for tokens and diagnostics.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Position {
    /// Line number (1-based)
    pub line: u32,
    /// Column position (1-based, 0 means unknown)
    pub col: u16,
}

impl Position {
    /// Create a new position
    pub fn new(line: u32, col: u16) -> Self {
        Self { line, col }
    }

    /// Create a position with line only (column unknown)
    pub fn line_only(line: u32) -> Self {
        Self { line, col: 0 }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = FILENAME.with(|s| s.borrow().clone());
        let name = if name.is_empty() { "<unknown>" } else { &name };

        if self.col > 0 {
            write!(f, "{}:{}:{}", name, self.line, self.col)
        } else {
            write!(f, "{}:{}", name, self.line)
        }
    }
}

// ============================================================================
// Global State
// ============================================================================

thread_local! {
    static FILENAME: RefCell<String> = const { RefCell::new(String::new()) };
}

static ERROR_COUNT: AtomicU32 = AtomicU32::new(0);
static WARNING_COUNT: AtomicU32 = AtomicU32::new(0);

// ============================================================================
// Public API
// ============================================================================

/// Initialize the diagnostic system with the source filename.
pub fn init(filename: &str) {
    FILENAME.with(|s| {
        *s.borrow_mut() = filename.to_string();
    });
    reset_counts();
}

/// Reset error and warning counts (for testing).
pub fn reset_counts() {
    ERROR_COUNT.store(0, Ordering::SeqCst);
    WARNING_COUNT.store(0, Ordering::SeqCst);
}

/// Check if any errors have been reported.
pub fn has_errors() -> bool {
    ERROR_COUNT.load(Ordering::SeqCst) > 0
}

/// Get the current error count.
#[allow(dead_code)]
pub fn error_count() -> u32 {
    ERROR_COUNT.load(Ordering::SeqCst)
}

/// Get the current warning count.
#[allow(dead_code)]
pub fn warning_count() -> u32 {
    WARNING_COUNT.load(Ordering::SeqCst)
}

/// Report a warning at the given position.
#[allow(dead_code)]
pub fn warning(pos: Position, msg: &str) {
    WARNING_COUNT.fetch_add(1, Ordering::SeqCst);
    do_diag("warning", pos, msg);
}

/// Report an error at the given position.
pub fn error(pos: Position, msg: &str) {
    ERROR_COUNT.fetch_add(1, Ordering::SeqCst);
    do_diag("error", pos, msg);
}

// ============================================================================
// Internal Implementation
// ============================================================================

fn do_diag(level: &str, pos: Position, msg: &str) {
    let _ = writeln!(io::stderr(), "{}: {}: {}", pos, level, msg);
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_display() {
        init("test.y");
        let pos = Position::new(10, 5);
        assert_eq!(format!("{}", pos), "test.y:10:5");

        let pos_no_col = Position::line_only(20);
        assert_eq!(format!("{}", pos_no_col), "test.y:20");
    }

    #[test]
    fn test_error_warning_functions() {
        // Note: These tests avoid checking state since tests run in parallel
        // and share global atomic counters.
        init("test.y");

        // Just verify that error/warning functions don't panic
        warning(Position::new(1, 1), "test warning");
        error(Position::new(2, 1), "test error");
        // Note: has_errors() not checked because parallel tests share global state
    }
}
