//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Diagnostic and stream management module for pcc C99 compiler
//

use std::cell::RefCell;
use std::fmt;
use std::io::{self, Write};
use std::sync::atomic::{AtomicU32, Ordering};

// ============================================================================
// Source Position
// ============================================================================

/// Source position tracking for tokens and diagnostics.
///
/// A compact structure attached to every token, tracking file, line,
/// column, and preprocessor state (whitespace, newline flags).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Position {
    /// Stream/file index (which file this is in)
    pub stream: u16,
    /// Line number (1-based)
    pub line: u32,
    /// Column position (1-based, 0 means unknown)
    pub col: u16,
    /// Token preceded by newline
    pub newline: bool,
    /// Token preceded by whitespace
    pub whitespace: bool,
    /// Don't expand macros (for preprocessor)
    pub noexpand: bool,
}

impl Position {
    /// Create a new position
    pub fn new(stream: u16, line: u32, col: u16) -> Self {
        Self {
            stream,
            line,
            col,
            newline: false,
            whitespace: false,
            noexpand: false,
        }
    }

    /// Create a "bad" position for error tokens
    #[cfg(test)]
    pub fn bad() -> Self {
        Self {
            stream: u16::MAX,
            line: 0,
            col: 0,
            newline: false,
            whitespace: false,
            noexpand: false,
        }
    }

    /// Check if this is a bad/invalid position
    pub fn is_bad(&self) -> bool {
        self.stream == u16::MAX && self.line == 0
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Get filename from global registry
        let name = STREAMS.with(|s| {
            s.borrow()
                .get_name(self.stream)
                .map(|n| n.to_string())
                .unwrap_or_else(|| "<unknown>".to_string())
        });

        if self.col > 0 {
            write!(f, "{}:{}:{}", name, self.line, self.col)
        } else {
            write!(f, "{}:{}", name, self.line)
        }
    }
}

// ============================================================================
// Stream (Input Source)
// ============================================================================

/// Input stream information for tracking source files and includes.
#[derive(Debug, Clone)]
pub struct Stream {
    /// Filename
    pub name: String,
    /// Position of #include directive that included this file
    /// None for the main file
    pub include_pos: Option<Position>,
    /// Line number offset from #line directive
    /// If Some((new_line, new_file)), positions should be adjusted
    pub line_directive: Option<(u32, Option<String>)>,
}

impl Stream {
    /// Create a new stream for a file
    pub fn new(name: String) -> Self {
        Self {
            name,
            include_pos: None,
            line_directive: None,
        }
    }

    /// Create a stream for an included file
    #[cfg(test)]
    pub fn included(name: String, include_pos: Position) -> Self {
        Self {
            name,
            include_pos: Some(include_pos),
            line_directive: None,
        }
    }
}

// ============================================================================
// Stream Registry (Global)
// ============================================================================

/// Stream registry for managing all input files
#[derive(Debug, Default)]
pub struct StreamRegistry {
    streams: Vec<Stream>,
}

impl StreamRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a new stream, returning its ID
    pub fn add(&mut self, name: String) -> u16 {
        let id = self.streams.len() as u16;
        self.streams.push(Stream::new(name));
        id
    }

    /// Add a stream for an included file
    #[cfg(test)]
    pub fn add_included(&mut self, name: String, include_pos: Position) -> u16 {
        let id = self.streams.len() as u16;
        self.streams.push(Stream::included(name, include_pos));
        id
    }

    /// Get stream by ID
    pub fn get(&self, id: u16) -> Option<&Stream> {
        self.streams.get(id as usize)
    }

    /// Get stream name by ID
    pub fn get_name(&self, id: u16) -> Option<&str> {
        self.streams.get(id as usize).map(|s| s.name.as_str())
    }

    /// Get the previous stream in the include chain
    /// Returns None for the main file
    pub fn prev_stream(&self, id: u16) -> Option<u16> {
        self.streams
            .get(id as usize)
            .and_then(|s| s.include_pos)
            .map(|pos| pos.stream)
    }

    /// Get effective filename and line for a position
    /// This handles #line directives
    pub fn effective_position(&self, pos: Position) -> (String, u32, u16) {
        if let Some(stream) = self.streams.get(pos.stream as usize) {
            if let Some((line_offset, ref opt_file)) = stream.line_directive {
                let name = opt_file
                    .as_ref()
                    .map(|s| s.as_str())
                    .unwrap_or(&stream.name);
                // Adjust line number based on #line directive
                return (name.to_string(), pos.line + line_offset, pos.col);
            }
            (stream.name.clone(), pos.line, pos.col)
        } else {
            ("<unknown>".to_string(), pos.line, pos.col)
        }
    }

    /// Clear all streams (for reuse between compilations)
    pub fn clear(&mut self) {
        self.streams.clear();
    }
}

// Thread-local stream registry
thread_local! {
    pub static STREAMS: RefCell<StreamRegistry> = RefCell::new(StreamRegistry::new());
}

/// Initialize a new stream, returning its ID
pub fn init_stream(name: &str) -> u16 {
    STREAMS.with(|s| s.borrow_mut().add(name.to_string()))
}

/// Initialize a stream for an included file
#[cfg(test)]
pub fn init_included_stream(name: &str, include_pos: Position) -> u16 {
    STREAMS.with(|s| s.borrow_mut().add_included(name.to_string(), include_pos))
}

/// Get stream name by ID (for tests)
#[cfg(test)]
pub fn stream_name(id: u16) -> String {
    STREAMS.with(|s| {
        s.borrow()
            .get_name(id)
            .map(|n| n.to_string())
            .unwrap_or_else(|| "<unknown>".to_string())
    })
}

/// Get previous stream in include chain
#[cfg(test)]
pub fn stream_prev(id: u16) -> Option<u16> {
    STREAMS.with(|s| s.borrow().prev_stream(id))
}

/// Clear all streams (call at start of new compilation)
pub fn clear_streams() {
    STREAMS.with(|s| s.borrow_mut().clear());
}

/// Get all stream names (for DWARF .file directives)
/// Returns a vector of file paths, indexed by stream ID
pub fn get_all_stream_names() -> Vec<String> {
    STREAMS.with(|s| {
        s.borrow()
            .streams
            .iter()
            .map(|stream| stream.name.clone())
            .collect()
    })
}

// ============================================================================
// Error Tracking
// ============================================================================

/// Error phase flag
pub const ERROR_CURR_PHASE: u32 = 1;

// Global error state
static HAS_ERROR: AtomicU32 = AtomicU32::new(0);
static ERROR_COUNT: AtomicU32 = AtomicU32::new(0);
static WARNING_COUNT: AtomicU32 = AtomicU32::new(0);

/// Get current error state
#[cfg(test)]
pub fn has_error() -> u32 {
    HAS_ERROR.load(Ordering::Relaxed)
}

/// Set error flag
fn set_error(flag: u32) {
    HAS_ERROR.fetch_or(flag, Ordering::Relaxed);
}

/// Get error count
#[cfg(test)]
pub fn error_count() -> u32 {
    ERROR_COUNT.load(Ordering::Relaxed)
}

/// Get warning count
#[cfg(test)]
pub fn warning_count() -> u32 {
    WARNING_COUNT.load(Ordering::Relaxed)
}

/// Reset error/warning counts
#[cfg(test)]
pub fn reset_counts() {
    ERROR_COUNT.store(0, Ordering::Relaxed);
    WARNING_COUNT.store(0, Ordering::Relaxed);
    HAS_ERROR.store(0, Ordering::Relaxed);
}

// ============================================================================
// Diagnostic Output
// ============================================================================

/// Diagnostic severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DiagLevel {
    Warning,
    Error,
}

impl DiagLevel {
    fn prefix(&self) -> &'static str {
        match self {
            DiagLevel::Warning => "warning: ",
            DiagLevel::Error => "error: ",
        }
    }
}

/// Build include chain message for nested includes
fn show_include_chain(stream_id: u16) -> Option<String> {
    STREAMS.with(|s| {
        let streams = s.borrow();
        let mut chain = Vec::new();
        let mut current = stream_id;

        // Walk up the include chain
        while let Some(prev) = streams.prev_stream(current) {
            if let Some(stream) = streams.get(prev) {
                chain.push(prettify_path(&stream.name));
            }
            current = prev;
        }

        if chain.is_empty() {
            None
        } else {
            Some(format!(" (through {})", chain.join(", ")))
        }
    })
}

/// Prettify a path by removing ./ prefix if present
fn prettify_path(path: &str) -> String {
    path.strip_prefix("./")
        .map(|s| s.to_string())
        .unwrap_or_else(|| path.to_string())
}

/// Output a diagnostic message
fn do_diag(level: DiagLevel, pos: Position, msg: &str) {
    // Don't output if position is bad
    if pos.is_bad() {
        return;
    }

    // Track errors/warnings
    match level {
        DiagLevel::Error => {
            ERROR_COUNT.fetch_add(1, Ordering::Relaxed);
            set_error(ERROR_CURR_PHASE);
        }
        DiagLevel::Warning => {
            WARNING_COUNT.fetch_add(1, Ordering::Relaxed);
        }
    }

    // Format the message
    let (filename, line, col) = STREAMS.with(|s| s.borrow().effective_position(pos));
    let filename = prettify_path(&filename);

    // Check for include chain (only on first occurrence of a file)
    let include_note = show_include_chain(pos.stream);

    // Print include context if present
    if let Some(chain) = include_note {
        // Get base filename
        let base = STREAMS.with(|s| {
            s.borrow()
                .get(0)
                .map(|st| st.name.clone())
                .unwrap_or_else(|| "<unknown>".to_string())
        });
        eprintln!("{}: note: in included file{}:", base, chain);
    }

    // Print the diagnostic
    let _ = if col > 0 {
        writeln!(
            io::stderr(),
            "{}:{}:{}: {}{}",
            filename,
            line,
            col,
            level.prefix(),
            msg
        )
    } else {
        writeln!(
            io::stderr(),
            "{}:{}: {}{}",
            filename,
            line,
            level.prefix(),
            msg
        )
    };
}

// ============================================================================
// Public Diagnostic Functions
// ============================================================================

/// Print a warning message
pub fn warning(pos: Position, msg: &str) {
    do_diag(DiagLevel::Warning, pos, msg);
}

/// Print an error message
pub fn error(pos: Position, msg: &str) {
    do_diag(DiagLevel::Error, pos, msg);
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_display() {
        clear_streams();
        let stream = init_stream("test.c");
        let pos = Position::new(stream, 10, 5);
        let s = format!("{}", pos);
        assert_eq!(s, "test.c:10:5");
    }

    #[test]
    fn test_position_no_column() {
        clear_streams();
        let stream = init_stream("test.c");
        let pos = Position::new(stream, 42, 0);
        let s = format!("{}", pos);
        assert_eq!(s, "test.c:42");
    }

    #[test]
    fn test_bad_position() {
        let pos = Position::bad();
        assert!(pos.is_bad());
    }

    #[test]
    fn test_stream_registry() {
        clear_streams();
        let s1 = init_stream("main.c");
        let s2 = init_stream("header.h");
        assert_eq!(stream_name(s1), "main.c");
        assert_eq!(stream_name(s2), "header.h");
    }

    #[test]
    fn test_include_chain() {
        clear_streams();
        let main_stream = init_stream("main.c");
        let main_pos = Position::new(main_stream, 5, 1);

        let header_stream = init_included_stream("header.h", main_pos);

        assert_eq!(stream_prev(header_stream), Some(main_stream));
        assert_eq!(stream_prev(main_stream), None);
    }

    #[test]
    fn test_prettify_path() {
        assert_eq!(prettify_path("./test.c"), "test.c");
        assert_eq!(prettify_path("test.c"), "test.c");
        assert_eq!(prettify_path("./src/main.c"), "src/main.c");
    }

    #[test]
    fn test_error_counting() {
        reset_counts();
        assert_eq!(error_count(), 0);
        assert_eq!(warning_count(), 0);

        clear_streams();
        let stream = init_stream("test.c");
        let pos = Position::new(stream, 1, 1);

        error(pos, "test error");
        assert_eq!(error_count(), 1);
        assert!(has_error() & ERROR_CURR_PHASE != 0);

        warning(pos, "test warning");
        assert_eq!(warning_count(), 1);

        reset_counts();
        assert_eq!(error_count(), 0);
    }
}
