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

/// A hunk represents a contiguous region of changes.
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
}

impl Hunk {
    /// Create a new empty hunk.
    pub fn new(old_start: usize, old_count: usize, new_start: usize, new_count: usize) -> Self {
        Self {
            old_start,
            old_count,
            new_start,
            new_count,
            lines: Vec::new(),
        }
    }

    /// Reverse this hunk (swap add/delete operations).
    pub fn reverse(&mut self) {
        std::mem::swap(&mut self.old_start, &mut self.new_start);
        std::mem::swap(&mut self.old_count, &mut self.new_count);
        self.lines = self
            .lines
            .iter()
            .map(|op| match op {
                LineOp::Context(s) => LineOp::Context(s.clone()),
                LineOp::Add(s) => LineOp::Delete(s.clone()),
                LineOp::Delete(s) => LineOp::Add(s.clone()),
            })
            .collect();
    }

    /// Get context lines for matching (Delete and Context lines).
    pub fn get_old_lines(&self) -> Vec<&str> {
        self.lines
            .iter()
            .filter_map(|op| match op {
                LineOp::Context(s) | LineOp::Delete(s) => Some(s.as_str()),
                LineOp::Add(_) => None,
            })
            .collect()
    }

    /// Get result lines after applying (Context and Add lines).
    pub fn get_new_lines(&self) -> Vec<&str> {
        self.lines
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
    /// Create a new empty file patch.
    pub fn new(format: DiffFormat) -> Self {
        Self {
            old_path: None,
            new_path: None,
            index_path: None,
            format,
            hunks: Vec::new(),
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
    /// Whether any hunks had offset != 0
    pub had_offset: bool,
    /// Whether any hunks used fuzz > 0
    pub had_fuzz: bool,
}

/// Configuration options for patch.
#[derive(Debug, Clone, Default)]
pub struct PatchConfig {
    /// Save .orig backup (-b)
    pub backup: bool,
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

    #[error("reversed (or previously applied) patch detected")]
    ReversedPatch,

    #[error("{0}")]
    Other(String),
}
