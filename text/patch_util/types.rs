//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Core data types for the patch utility.

use std::path::PathBuf;
use thiserror::Error;

/// Represents a single line operation within a hunk.
#[derive(Debug, Clone, PartialEq)]
pub enum LineOp {
    /// Unchanged context line (for matching)
    Context(String),
    /// Line to insert
    Add(String),
    /// Line to remove (for verification)
    Delete(String),
}

impl Default for LineOp {
    fn default() -> Self {
        LineOp::Context(String::new())
    }
}

/// How a hunk's location in the target file is determined.
///
/// This is a property of the diff format, not of a hunk's contents. An ed
/// script names absolute line numbers and does not record the text it removes,
/// so there is nothing to verify against the file and no cumulative offset to
/// carry between hunks. Every other format identifies its target by matching
/// recorded old-side text, and each applied hunk shifts the search origin for
/// the ones that follow.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Placement {
    /// Apply at the recorded line number, verbatim.
    Positional,
    /// Locate by matching the hunk's old-side text.
    ContentMatched,
}

impl From<DiffFormat> for Placement {
    fn from(format: DiffFormat) -> Self {
        match format {
            DiffFormat::EdScript => Placement::Positional,
            DiffFormat::Unified | DiffFormat::Context | DiffFormat::Normal => {
                Placement::ContentMatched
            }
        }
    }
}

/// A hunk represents a contiguous region of changes.
///
/// `old_start` is the 1-based line number of the first old-side line the hunk
/// replaces. When `old_count` is 0 the hunk inserts rather than replaces, and
/// `old_start` is the line number *before which* the new lines go — so the
/// insertion index is `old_start - 1` in either case, and `old_start` ranges
/// over `1 ..= len + 1`. `new_start` is the mirror of this on the new side.
/// The parsers normalize every format to this convention, which keeps the
/// invariant symmetric under [`Hunk::reverse`].
#[derive(Debug, Clone)]
pub struct Hunk {
    /// Original file starting line (1-indexed, from header)
    pub old_start: usize,
    /// Original file line count
    pub old_count: usize,
    /// New file starting line (1-indexed, from header)
    pub new_start: usize,
    /// New file line count
    pub new_count: usize,
    /// The actual line operations
    pub lines: Vec<LineOp>,
    /// The old-side last line had no trailing newline ("\ No newline...").
    pub old_no_newline: bool,
    /// The new-side last line had no trailing newline ("\ No newline...").
    pub new_no_newline: bool,
}

impl Hunk {
    /// Create a new empty hunk with pre-allocated capacity.
    pub fn new(old_start: usize, old_count: usize, new_start: usize, new_count: usize) -> Self {
        // Pre-allocate: typically need old_count + new_count lines
        // (context lines counted in both, plus adds and deletes)
        let estimated_lines = old_count.saturating_add(new_count);
        Self {
            old_start,
            old_count,
            new_start,
            new_count,
            lines: Vec::with_capacity(estimated_lines),
            old_no_newline: false,
            new_no_newline: false,
        }
    }

    /// Reverse this hunk (swap add/delete operations) in place without cloning.
    pub fn reverse(&mut self) {
        std::mem::swap(&mut self.old_start, &mut self.new_start);
        std::mem::swap(&mut self.old_count, &mut self.new_count);
        std::mem::swap(&mut self.old_no_newline, &mut self.new_no_newline);
        // Mutate in place using mem::take to avoid cloning strings
        for op in &mut self.lines {
            *op = match std::mem::take(op) {
                LineOp::Context(s) => LineOp::Context(s),
                LineOp::Add(s) => LineOp::Delete(s),
                LineOp::Delete(s) => LineOp::Add(s),
            };
        }
    }

    /// Lengths of the leading and trailing runs of [`LineOp::Context`].
    ///
    /// These are the only lines a fuzzy match may ignore. A hunk made
    /// entirely of context has no separate trailing run — the leading one
    /// already covers every op — so the two never overlap.
    fn context_runs(&self) -> (usize, usize) {
        let lead = self
            .lines
            .iter()
            .take_while(|op| matches!(op, LineOp::Context(_)))
            .count();
        if lead == self.lines.len() {
            return (lead, 0);
        }
        let trail = self
            .lines
            .iter()
            .rev()
            .take_while(|op| matches!(op, LineOp::Context(_)))
            .count();
        (lead, trail)
    }

    /// The whole hunk, with nothing ignored.
    pub fn full_window(&self) -> MatchWindow<'_> {
        MatchWindow {
            ops: &self.lines,
            lead_skip: 0,
        }
    }

    /// The hunk with up to `fuzz` context lines ignored at each end.
    ///
    /// Returns `None` when neither end can give up another line, or when
    /// trimming would leave nothing on the old side to verify — a window that
    /// matches everywhere is not a match. A format that records no context at
    /// all (a normal diff, or `diff -U0` output) therefore never yields a fuzz
    /// window, which is how POSIX's "if it is a context difference" gets
    /// enforced structurally rather than by inspecting the format.
    pub fn fuzz_window(&self, fuzz: usize) -> Option<MatchWindow<'_>> {
        let (lead, trail) = self.context_runs();
        let lead_skip = fuzz.min(lead);
        let trail_skip = fuzz.min(trail);
        if lead_skip == 0 && trail_skip == 0 {
            return None;
        }
        // A side with fewer context lines than `fuzz` is already fully
        // trimmed; if that is true of both sides, this window is the one the
        // previous fuzz level already tried.
        if fuzz > 1 && lead < fuzz && trail < fuzz {
            return None;
        }
        let ops = &self.lines[lead_skip..self.lines.len() - trail_skip];
        if !ops
            .iter()
            .any(|op| matches!(op, LineOp::Context(_) | LineOp::Delete(_)))
        {
            return None;
        }
        Some(MatchWindow { ops, lead_skip })
    }
}

/// A hunk narrowed to the operations that will actually be verified and
/// written.
///
/// The `lead_skip` leading context ops, and any trailing ones, are ignored:
/// the file lines they would have covered are left exactly as they are. This
/// is what keeps a fuzzy match from writing the patch's own unverified context
/// over real file content — the ignored ops are absent from the window rather
/// than something the writer must remember to skip.
pub struct MatchWindow<'h> {
    ops: &'h [LineOp],
    /// Context lines ignored before the window starts.
    pub lead_skip: usize,
}

impl<'h> MatchWindow<'h> {
    /// The operations inside the window, in source order.
    pub fn ops(&self) -> &'h [LineOp] {
        self.ops
    }

    /// Old-side text inside the window (Context and Delete payloads).
    pub fn old_lines(&self) -> Vec<&'h str> {
        self.ops
            .iter()
            .filter_map(|op| match op {
                LineOp::Context(s) | LineOp::Delete(s) => Some(s.as_str()),
                LineOp::Add(_) => None,
            })
            .collect()
    }

    /// New-side text inside the window (Context and Add payloads).
    pub fn new_lines(&self) -> Vec<&'h str> {
        self.ops
            .iter()
            .filter_map(|op| match op {
                LineOp::Context(s) | LineOp::Add(s) => Some(s.as_str()),
                LineOp::Delete(_) => None,
            })
            .collect()
    }
}

/// The detected diff format.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiffFormat {
    /// Unified diff (diff -u)
    Unified,
    /// Context diff (diff -c)
    Context,
    /// Normal diff (default diff output)
    Normal,
    /// Ed script (diff -e)
    EdScript,
}

impl std::fmt::Display for DiffFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DiffFormat::Unified => write!(f, "unified"),
            DiffFormat::Context => write!(f, "context"),
            DiffFormat::Normal => write!(f, "normal"),
            DiffFormat::EdScript => write!(f, "ed script"),
        }
    }
}

/// Represents a complete patch for a single file.
#[derive(Debug, Clone)]
pub struct FilePatch {
    /// Original filename from patch header (*** or --- line)
    pub old_path: Option<String>,
    /// New filename from patch header (--- or +++ line)
    pub new_path: Option<String>,
    /// Index line if present
    pub index_path: Option<String>,
    /// Detected format
    pub format: DiffFormat,
    /// Collection of hunks
    pub hunks: Vec<Hunk>,
    /// Whether this creates a new file (old_path is /dev/null)
    pub is_new_file: bool,
    /// Whether this deletes a file (new_path is /dev/null)
    pub is_delete_file: bool,
}

impl FilePatch {
    /// Create a new empty file patch with pre-allocated hunk capacity.
    pub fn new(format: DiffFormat) -> Self {
        Self {
            old_path: None,
            new_path: None,
            index_path: None,
            format,
            // Pre-allocate for typical patch size (1-20 hunks is common)
            hunks: Vec::with_capacity(8),
            is_new_file: false,
            is_delete_file: false,
        }
    }

    /// Reverse this patch (swap old/new, reverse all hunks).
    pub fn reverse(&mut self) {
        std::mem::swap(&mut self.old_path, &mut self.new_path);
        std::mem::swap(&mut self.is_new_file, &mut self.is_delete_file);
        for hunk in &mut self.hunks {
            hunk.reverse();
        }
    }
}

/// Complete patch input (may contain multiple file patches).
#[derive(Debug, Default)]
pub struct Patch {
    pub file_patches: Vec<FilePatch>,
}

impl Patch {
    /// Reverse all patches.
    pub fn reverse(&mut self) {
        for fp in &mut self.file_patches {
            fp.reverse();
        }
    }
}

/// Result of applying a single hunk.
#[derive(Debug)]
pub enum HunkResult {
    /// Hunk was applied successfully.
    Applied {
        /// 1-based line in the patched file where the hunk landed
        line: usize,
        /// Line offset from expected position
        offset: i64,
        /// Fuzz factor used (0 = exact match)
        fuzz: usize,
    },
    /// Patch was already applied (matched in reverse).
    AlreadyApplied,
    /// Hunk could not be applied.
    Rejected {
        /// Reason for rejection
        reason: String,
    },
}

/// Result of applying a patch to a file.
#[derive(Debug)]
pub struct ApplyResult {
    /// Hunks that were rejected
    pub rejected_hunks: Vec<(usize, Hunk, String)>,
    /// Final file content
    pub content: Vec<String>,
    /// Whether the resulting file's last line has no trailing newline.
    pub no_trailing_newline: bool,
    /// Whether any hunk was actually applied.
    ///
    /// When none was, the content is byte-identical to what was read and must
    /// not be written back: rewriting it would change the file's modification
    /// time, and under -b leave a backup, for a patch that did nothing.
    pub applied_any: bool,
}

/// Configuration options for patch.
#[derive(Debug, Clone, Default)]
pub struct PatchConfig {
    /// Save .orig backup (-b)
    pub backup: bool,
    /// Force application without prompting (-f)
    pub force: bool,
    /// Force context diff interpretation (-c)
    pub force_context: bool,
    /// Change directory before processing (-d)
    pub directory: Option<PathBuf>,
    /// Wrap changes in #ifdef (-D)
    pub ifdef_define: Option<String>,
    /// Force ed script interpretation (-e)
    pub force_ed: bool,
    /// Read patch from file (-i)
    pub patchfile: Option<PathBuf>,
    /// Loose whitespace matching (-l)
    pub loose_whitespace: bool,
    /// Force normal diff interpretation (-n)
    pub force_normal: bool,
    /// Ignore already-applied patches (-N)
    pub ignore_applied: bool,
    /// Write output to file (-o)
    pub output_file: Option<PathBuf>,
    /// Strip path components (-p)
    pub strip_count: Option<usize>,
    /// Override reject filename (-r)
    pub reject_file: Option<PathBuf>,
    /// Reverse patch direction (-R)
    pub reverse: bool,
    /// Force unified diff interpretation (-u)
    pub force_unified: bool,
    /// Target file from command line
    pub target_file: Option<PathBuf>,
}

/// Errors that can occur during patch operations.
#[derive(Error, Debug)]
pub enum PatchError {
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("parse error at line {line}: {message}")]
    Parse { line: usize, message: String },

    #[error("could not determine target file for patch")]
    NoTargetFile,

    #[error("{0}")]
    Other(String),
}
