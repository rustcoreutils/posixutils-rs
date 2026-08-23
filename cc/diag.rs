//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Diagnostic and stream management module for c17 C17 compiler
//

use gettextrs::{gettext, gettext_args, ngettext_args};
use std::cell::RefCell;
use std::fmt;
use std::io::{self, Write};
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};

// Source Position

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

// Stream (Input Source)

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
    /// Named by a `# N "file" 3` linemarker, i.e. a system header. Warnings
    /// from such a stream are suppressed, as they are in GCC: a preprocessed
    /// file carries the whole of glibc with it, and its warnings are not the
    /// user's to act on.
    pub is_system: bool,
}

impl Stream {
    pub fn new(name: String) -> Self {
        Self {
            name,
            include_pos: None,
            line_directive: None,
            is_system: false,
        }
    }

    pub fn included(name: String, include_pos: Position) -> Self {
        Self {
            name,
            include_pos: Some(include_pos),
            line_directive: None,
            is_system: false,
        }
    }
}

// Stream Registry (Global)

/// Stream registry for managing all input files
#[derive(Debug, Default)]
pub struct StreamRegistry {
    streams: Vec<Stream>,
}

impl StreamRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, name: String) -> u16 {
        let id = self.streams.len() as u16;
        self.streams.push(Stream::new(name));
        id
    }

    /// Find the stream already registered under `name`, or add one.
    ///
    /// A preprocessed file re-enters the same file many times -- every
    /// `# N "foo.h" 2` returns to one already seen -- and each name must map
    /// to one stream, or the include-chain note and the `-E` marker writer
    /// both see a file they have never met before.
    pub fn find_or_add(&mut self, name: &str) -> u16 {
        match self.streams.iter().position(|s| s.name == name) {
            Some(i) => i as u16,
            None => self.add(name.to_string()),
        }
    }

    /// Mark a stream as a system header (linemarker flag 3).
    pub fn set_system(&mut self, id: u16, is_system: bool) {
        if let Some(stream) = self.streams.get_mut(id as usize) {
            stream.is_system = is_system;
        }
    }

    /// Whether warnings from this stream are suppressed.
    pub fn is_system(&self, id: u16) -> bool {
        self.streams.get(id as usize).is_some_and(|s| s.is_system)
    }

    pub fn add_included(&mut self, name: String, include_pos: Position) -> u16 {
        let id = self.streams.len() as u16;
        self.streams.push(Stream::included(name, include_pos));
        id
    }

    pub fn get(&self, id: u16) -> Option<&Stream> {
        self.streams.get(id as usize)
    }

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

pub fn init_stream(name: &str) -> u16 {
    STREAMS.with(|s| s.borrow_mut().add(name.to_string()))
}

/// Resolve `name` to its stream, registering one if this is the first sighting.
pub fn find_or_add_stream(name: &str) -> u16 {
    STREAMS.with(|s| s.borrow_mut().find_or_add(name))
}

/// Mark a stream as a system header, silencing its warnings.
pub fn set_stream_system(id: u16, is_system: bool) {
    STREAMS.with(|s| s.borrow_mut().set_system(id, is_system));
}

pub fn init_included_stream(name: &str, include_pos: Position) -> u16 {
    STREAMS.with(|s| s.borrow_mut().add_included(name.to_string(), include_pos))
}

/// Resolve a position to `(file, line, column)`, honoring any `#line`
/// directive in effect for that stream.
///
/// The `-E` line markers need this: `#line` renames the stream for the reader,
/// and a marker that ignored it would contradict the diagnostics.
pub fn effective_position(pos: Position) -> (String, u32, u16) {
    STREAMS.with(|s| s.borrow().effective_position(pos))
}

/// The name registered for a stream.
///
/// Read only by this module's own tests; production formats a name through
/// `effective_position`, which also follows `#line`.
#[cfg(test)]
pub fn stream_name(id: u16) -> String {
    STREAMS.with(|s| {
        s.borrow()
            .get_name(id)
            .map(|n| n.to_string())
            .unwrap_or_else(|| "<unknown>".to_string())
    })
}

#[cfg(test)]
pub fn stream_prev(id: u16) -> Option<u16> {
    STREAMS.with(|s| s.borrow().prev_stream(id))
}

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

// Error Tracking

/// Error phase flag
pub const ERROR_CURR_PHASE: u32 = 1;

// Global error state
static HAS_ERROR: AtomicU32 = AtomicU32::new(0);
static ERROR_COUNT: AtomicU32 = AtomicU32::new(0);
static WARNING_COUNT: AtomicU32 = AtomicU32::new(0);

/// Set by `-w`: warnings are counted but not printed.
///
/// Counting them still is deliberate -- `-w` is about output, and a caller
/// asking how many warnings a translation unit produced should get the truth.
static SUPPRESS_WARNINGS: AtomicBool = AtomicBool::new(false);

/// Suppress warning output for the rest of the process (`-w`).
pub fn suppress_warnings() {
    SUPPRESS_WARNINGS.store(true, Ordering::Relaxed);
}

/// Warning groups turned off by `-Wno-<name>`.
///
/// `-w` is all-or-nothing and lives in `SUPPRESS_WARNINGS`; this is the named
/// half. Set once from the driver, because the code that emits a warning is
/// nowhere near the code that parsed the command line.
static SUPPRESSED_GROUPS: std::sync::OnceLock<std::collections::HashSet<String>> =
    std::sync::OnceLock::new();

/// Record the `-Wno-<name>` groups. Ignored if called twice.
pub fn suppress_warning_groups(names: std::collections::HashSet<String>) {
    let _ = SUPPRESSED_GROUPS.set(names);
}

/// Is the warning group `name` still on?
pub fn warning_group_enabled(name: &str) -> bool {
    !SUPPRESSED_GROUPS
        .get()
        .is_some_and(|groups| groups.contains(name))
}

/// Are warnings being printed?
pub fn warnings_suppressed() -> bool {
    SUPPRESS_WARNINGS.load(Ordering::Relaxed)
}

pub fn has_error() -> u32 {
    HAS_ERROR.load(Ordering::Relaxed)
}

fn set_error(flag: u32) {
    HAS_ERROR.fetch_or(flag, Ordering::Relaxed);
}

#[cfg(test)]
pub fn error_count() -> u32 {
    ERROR_COUNT.load(Ordering::Relaxed)
}

#[cfg(test)]
pub fn warning_count() -> u32 {
    WARNING_COUNT.load(Ordering::Relaxed)
}

/// Reset error/warning counts.
///
/// The driver compiles every source operand in one process (POSIX requires it
/// to continue past a failing operand), so this state has to be cleared between
/// translation units — otherwise the first file's errors make every later file
/// look like it failed too.
pub fn reset_counts() {
    ERROR_COUNT.store(0, Ordering::Relaxed);
    WARNING_COUNT.store(0, Ordering::Relaxed);
    HAS_ERROR.store(0, Ordering::Relaxed);
}

// Diagnostic Output

/// Diagnostic severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DiagLevel {
    Warning,
    Error,
}

impl DiagLevel {
    /// The `warning: ` / `error: ` label, translated.
    ///
    /// These two words appear on every diagnostic the compiler emits, so they
    /// are where translation buys the most for the least fragmentation. The
    /// message bodies are built with `format!` at their call sites, which
    /// makes them unusable as msgids without restructuring every one — see
    /// #U7 in cc/audit.md.
    fn prefix(&self) -> String {
        match self {
            DiagLevel::Warning => format!("{}: ", gettext("warning")),
            DiagLevel::Error => format!("{}: ", gettext("error")),
        }
    }
}

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
            // The one piece of user-visible text in this function, so it
            // gets the same treatment as the "note"/"in included file" labels
            // just below rather than being the lone English fragment.
            Some(format!(" ({} {})", gettext("through"), chain.join(", ")))
        }
    })
}

/// Prettify a path by removing ./ prefix if present
fn prettify_path(path: &str) -> String {
    path.strip_prefix("./")
        .map(|s| s.to_string())
        .unwrap_or_else(|| path.to_string())
}

fn do_diag(level: DiagLevel, pos: Position, msg: &str) {
    // Track errors/warnings
    match level {
        DiagLevel::Error => {
            ERROR_COUNT.fetch_add(1, Ordering::Relaxed);
            set_error(ERROR_CURR_PHASE);
        }
        DiagLevel::Warning => {
            WARNING_COUNT.fetch_add(1, Ordering::Relaxed);
            if warnings_suppressed() {
                return;
            }
            if STREAMS.with(|s| s.borrow().is_system(pos.stream)) {
                return;
            }
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
        eprintln!(
            "{}: {}: {}{}:",
            base,
            gettext("note"),
            gettext("in included file"),
            chain
        );
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

// Public Diagnostic Functions

pub fn warning(pos: Position, msg: &str) {
    do_diag(DiagLevel::Warning, pos, msg);
}

pub fn error(pos: Position, msg: &str) {
    do_diag(DiagLevel::Error, pos, msg);
}

/// Print a warning built from a translatable template.
///
/// `template` is the msgid and must be a literal, with positional `{0}`/`{1}`
/// placeholders for the substitutions. That is the difference from passing a
/// `format!` result to [`warning`]: `format!` bakes the values in at compile
/// time, leaving the catalog to be searched for a string no extractor ever saw,
/// so the message can never be translated.
pub fn warning_args(pos: Position, template: &str, args: &[&str]) {
    do_diag(DiagLevel::Warning, pos, &gettext_args(template, args));
}

/// Print an error built from a translatable template. See [`warning_args`].
pub fn error_args(pos: Position, template: &str, args: &[&str]) {
    do_diag(DiagLevel::Error, pos, &gettext_args(template, args));
}

/// Print an error whose wording depends on a count.
///
/// Both forms are msgids. English needs only two; a catalog may define its own
/// rule for others.
pub fn error_plural(pos: Position, singular: &str, plural: &str, n: usize, args: &[&str]) {
    do_diag(
        DiagLevel::Error,
        pos,
        &ngettext_args(singular, plural, n, args),
    );
}

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

    /// The chain names every file between the diagnostic and the top, in
    /// order; `init_included_stream` is what records it.
    #[test]
    fn test_include_chain_is_recorded() {
        clear_streams();
        let main = init_stream("main.c");
        let outer = init_included_stream("outer.h", Position::new(main, 1, 1));
        let inner = init_included_stream("inner.h", Position::new(outer, 1, 1));

        assert_eq!(stream_prev(inner), Some(outer));
        assert_eq!(stream_prev(outer), Some(main));
        assert_eq!(stream_prev(main), None);
        assert_eq!(
            show_include_chain(inner),
            Some(" (through outer.h, main.c)".to_string())
        );
        assert_eq!(show_include_chain(main), None);
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
