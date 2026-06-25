//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Hunk application logic with fuzzy matching.

use super::types::{ApplyResult, FilePatch, Hunk, HunkResult, LineOp, PatchConfig, PatchError};

/// Maximum number of lines to scan when fuzzy matching.
const MAX_FUZZ_LINES: usize = 1000;

/// Applies patches to file content.
pub struct PatchApplier<'a> {
    config: &'a PatchConfig,
    file_lines: Vec<String>,
    offset: i64,
    /// Whether the resulting file's last line currently has no trailing newline.
    eof_no_newline: bool,
}

impl<'a> PatchApplier<'a> {
    /// Create a new applier with the given configuration and file content.
    ///
    /// `orig_trailing_newline` reflects whether the file being patched ended
    /// with a newline; this is preserved unless a hunk that reaches end-of-file
    /// changes it.
    pub fn new(
        config: &'a PatchConfig,
        file_lines: Vec<String>,
        orig_trailing_newline: bool,
    ) -> Self {
        Self {
            config,
            file_lines,
            offset: 0,
            eof_no_newline: !orig_trailing_newline,
        }
    }

    /// Apply all hunks from a file patch.
    pub fn apply_patch(&mut self, patch: &mut FilePatch) -> Result<ApplyResult, PatchError> {
        // Automatic reversal detection (POSIX): if the patch does not apply
        // forward but the reversed patch does, it was probably already applied
        // or created in the opposite direction. Prompt (or honor -f / -N).
        // Skipped when -R was given (already reversed) or -N (ignore applied).
        if !self.config.reverse && !self.config.ignore_applied && self.detect_reversed(patch) {
            if self.decide_assume_reverse() {
                patch.reverse();
            } else if !self.config.force {
                return Err(PatchError::ReversedPatch);
            }
            // With -f and a "no" decision, fall through and apply forward
            // (which will reject), matching GNU's "apply anyway" path.
        }

        let mut rejected_hunks: Vec<(usize, Hunk, String)> = Vec::new();

        for (i, hunk) in patch.hunks.iter_mut().enumerate() {
            let hunk_num = i + 1;
            match self.apply_hunk(hunk, hunk_num) {
                HunkResult::Applied { offset, fuzz } => {
                    if offset != 0 {
                        let target_line = (hunk.old_start as i64 + self.offset) as usize;
                        eprintln!(
                            "Hunk #{} succeeded at {} (offset {} line{})",
                            hunk_num,
                            target_line,
                            offset,
                            if offset.abs() == 1 { "" } else { "s" }
                        );
                    }
                    if fuzz > 0 {
                        eprintln!("Hunk #{} succeeded with fuzz {}", hunk_num, fuzz);
                    }
                    // Update cumulative offset (but not for ed scripts, where line numbers
                    // are already correct for the file state at execution time)
                    let is_ed_script = hunk.get_old_lines().iter().all(|s| s.is_empty());
                    if !is_ed_script {
                        self.offset += (hunk.new_count as i64) - (hunk.old_count as i64);
                    }
                }
                HunkResult::AlreadyApplied => {
                    if !self.config.ignore_applied {
                        return Err(PatchError::ReversedPatch);
                    }
                    eprintln!("Hunk #{} already applied", hunk_num);
                }
                HunkResult::Rejected { reason } => {
                    // Adjust the reject's header line numbers by the cumulative
                    // offset of previously-applied hunks so they approximate the
                    // positions in the (partially) patched file (matching GNU).
                    let mut rej = hunk.clone();
                    if self.offset != 0 {
                        let adjust = |start: usize| -> usize {
                            ((start as i64 + self.offset).max(1)) as usize
                        };
                        rej.old_start = adjust(rej.old_start);
                        rej.new_start = adjust(rej.new_start);
                    }
                    rejected_hunks.push((hunk_num, rej, reason));
                }
            }
        }

        let no_trailing_newline = self.eof_no_newline && !self.file_lines.is_empty();

        Ok(ApplyResult {
            rejected_hunks,
            // Use mem::take to avoid cloning the entire file content
            content: std::mem::take(&mut self.file_lines),
            no_trailing_newline,
        })
    }

    /// Detect whether the patch appears reversed/already-applied: its first
    /// content hunk fails to apply forward at its expected position but the
    /// reversed hunk (the new-side lines) matches there.
    fn detect_reversed(&self, patch: &FilePatch) -> bool {
        for hunk in &patch.hunks {
            let old_lines = hunk.get_old_lines();
            // Skip pure additions and ed-style hunks (no real context to test).
            if old_lines.is_empty() || old_lines.iter().all(|s| s.is_empty()) {
                continue;
            }
            let expected_pos = if hunk.old_start == 0 {
                0
            } else {
                (hunk.old_start - 1).min(self.file_lines.len())
            };
            let forward_ok = self.lines_match_at(&old_lines, expected_pos);
            if forward_ok {
                return false;
            }
            let new_lines = hunk.get_new_lines();
            if !new_lines.is_empty() && self.lines_match_at(&new_lines, expected_pos) {
                return true;
            }
            // First testable hunk did not clearly indicate a reversal.
            return false;
        }
        false
    }

    /// Decide whether to assume -R for a detected reversed patch. With -f,
    /// assume yes. Otherwise prompt on the controlling terminal; if no
    /// terminal is available, default to "no" (preserving the error path).
    fn decide_assume_reverse(&self) -> bool {
        if self.config.force {
            return true;
        }
        // No terminal available -> default to "no" (preserves the error path).
        super::file_ops::prompt_yes_no(
            "Reversed (or previously applied) patch detected!  Assume -R? [y] ",
        )
        .unwrap_or(false)
    }

    /// Apply a single hunk.
    fn apply_hunk(&mut self, hunk: &Hunk, _hunk_num: usize) -> HunkResult {
        let old_lines = hunk.get_old_lines();

        if old_lines.is_empty() {
            // Pure addition - insert at the right position
            // For ed scripts, don't apply cumulative offset (line numbers are absolute)
            let insert_pos = if hunk.old_start == 0 {
                0
            } else {
                ((hunk.old_start as i64 - 1).max(0) as usize).min(self.file_lines.len())
            };

            let new_lines: Vec<String> =
                hunk.get_new_lines().iter().map(|s| s.to_string()).collect();
            let new_len = new_lines.len();
            // Use splice for O(n) instead of multiple insert() which is O(n²)
            self.file_lines.splice(insert_pos..insert_pos, new_lines);
            if insert_pos + new_len == self.file_lines.len() {
                self.eof_no_newline = hunk.new_no_newline;
            }
            return HunkResult::Applied { offset: 0, fuzz: 0 };
        }

        // Check if this is an ed script hunk (all old_lines are empty)
        // Ed scripts don't include the content being deleted, so we apply by position.
        // Ed scripts also have line numbers that are correct for the file state at
        // execution time, so we don't apply cumulative offset.
        let is_ed_script = old_lines.iter().all(|s| s.is_empty());
        if is_ed_script {
            let pos = if hunk.old_start == 0 {
                0
            } else {
                (hunk.old_start - 1).min(self.file_lines.len())
            };
            self.perform_changes(hunk, pos);
            // Don't update self.offset - ed script line numbers are already correct
            return HunkResult::Applied { offset: 0, fuzz: 0 };
        }

        // Calculate expected position
        let expected_pos = if hunk.old_start == 0 {
            0
        } else {
            (hunk.old_start as i64 - 1 + self.offset).max(0) as usize
        };

        // Try exact match at expected position
        if let Some(offset) = self.try_match_at(&old_lines, expected_pos, 0) {
            self.perform_changes(hunk, expected_pos);
            return HunkResult::Applied { offset, fuzz: 0 };
        }

        // Try fuzzy matching - scan forwards and backwards
        for delta in 1..=MAX_FUZZ_LINES {
            // Try forward
            if expected_pos + delta < self.file_lines.len() + old_lines.len() {
                if let Some(offset) = self.try_match_at(&old_lines, expected_pos + delta, 0) {
                    self.perform_changes(hunk, expected_pos + delta);
                    return HunkResult::Applied {
                        offset: offset + delta as i64,
                        fuzz: 0,
                    };
                }
            }

            // Try backward
            if delta <= expected_pos {
                if let Some(offset) = self.try_match_at(&old_lines, expected_pos - delta, 0) {
                    self.perform_changes(hunk, expected_pos - delta);
                    return HunkResult::Applied {
                        offset: offset - delta as i64,
                        fuzz: 0,
                    };
                }
            }
        }

        // Try with fuzz factor (ignore some context lines)
        for fuzz in 1..=2 {
            if old_lines.len() <= fuzz * 2 {
                continue;
            }

            let fuzzed_lines: Vec<&str> = old_lines[fuzz..old_lines.len() - fuzz].to_vec();

            for delta in 0..=MAX_FUZZ_LINES {
                // Try at expected position with fuzz
                if delta == 0 && self.lines_match_at(&fuzzed_lines, expected_pos + fuzz) {
                    self.perform_changes(hunk, expected_pos);
                    return HunkResult::Applied { offset: 0, fuzz };
                }

                // Try forward with fuzz
                if expected_pos + delta + fuzz < self.file_lines.len() + fuzzed_lines.len()
                    && self.lines_match_at(&fuzzed_lines, expected_pos + delta + fuzz)
                {
                    self.perform_changes(hunk, expected_pos + delta);
                    return HunkResult::Applied {
                        offset: delta as i64,
                        fuzz,
                    };
                }

                // Try backward with fuzz
                if delta <= expected_pos
                    && expected_pos - delta + fuzz < self.file_lines.len()
                    && self.lines_match_at(&fuzzed_lines, expected_pos - delta + fuzz)
                {
                    self.perform_changes(hunk, expected_pos - delta);
                    return HunkResult::Applied {
                        offset: -(delta as i64),
                        fuzz,
                    };
                }
            }
        }

        // Check if already applied (reverse match)
        let new_lines = hunk.get_new_lines();
        if !new_lines.is_empty() && self.lines_match_at(&new_lines, expected_pos) {
            return HunkResult::AlreadyApplied;
        }

        HunkResult::Rejected {
            reason: format!(
                "patch does not apply at line {}",
                hunk.old_start as i64 + self.offset
            ),
        }
    }

    /// Try to match old_lines at the given position.
    /// Returns Some(0) if match found, None otherwise.
    fn try_match_at(&self, old_lines: &[&str], pos: usize, _fuzz: usize) -> Option<i64> {
        if self.lines_match_at(old_lines, pos) {
            Some(0)
        } else {
            None
        }
    }

    /// Check if the given lines match at the specified position.
    fn lines_match_at(&self, lines: &[&str], pos: usize) -> bool {
        if pos + lines.len() > self.file_lines.len() {
            return false;
        }

        for (i, expected) in lines.iter().enumerate() {
            let actual = &self.file_lines[pos + i];
            if !self.lines_match(actual, expected) {
                return false;
            }
        }

        true
    }

    /// Compare two lines, with optional loose whitespace matching.
    fn lines_match(&self, actual: &str, expected: &str) -> bool {
        if self.config.loose_whitespace {
            normalize_whitespace(actual) == normalize_whitespace(expected)
        } else {
            actual == expected
        }
    }

    /// Perform the actual changes for a hunk at the given position.
    fn perform_changes(&mut self, hunk: &Hunk, pos: usize) {
        if let Some(ref define) = self.config.ifdef_define {
            self.perform_ifdef_changes(hunk, pos, define);
        } else {
            self.perform_normal_changes(hunk, pos);
        }
    }

    /// Perform changes normally (delete old, insert new).
    fn perform_normal_changes(&mut self, hunk: &Hunk, pos: usize) {
        // Count deletes and build new lines with pre-allocated capacity
        let mut delete_count = 0;
        let mut new_lines: Vec<String> = Vec::with_capacity(hunk.lines.len());

        for op in &hunk.lines {
            match op {
                LineOp::Context(s) => {
                    delete_count += 1;
                    new_lines.push(s.clone());
                }
                LineOp::Delete(_) => {
                    delete_count += 1;
                }
                LineOp::Add(s) => {
                    new_lines.push(s.clone());
                }
            }
        }

        // Use splice for O(n) instead of drain() + multiple insert() which is O(n²)
        let remove_end = (pos + delete_count).min(self.file_lines.len());
        let new_len = new_lines.len();
        self.file_lines.splice(pos..remove_end, new_lines);
        // If this hunk's new content now ends the file, its newline marker
        // determines whether the output gets a trailing newline.
        if pos + new_len == self.file_lines.len() {
            self.eof_no_newline = hunk.new_no_newline;
        }
    }

    /// Perform changes with #ifdef wrapping.
    ///
    /// Adjacent deletes+adds (a change) are emitted as a single
    /// `#ifndef DEFINE` / old / `#else` / new / `#endif` block, matching the
    /// conventional `diff -D` / `patch -D` output. Pure additions use
    /// `#ifdef DEFINE` / new / `#endif`; pure deletions use
    /// `#ifndef DEFINE` / old / `#endif`.
    fn perform_ifdef_changes(&mut self, hunk: &Hunk, pos: usize, define: &str) {
        // Pre-allocate: worst case each line becomes ~2 lines (ifdef wrapped)
        let mut result: Vec<String> = Vec::with_capacity(hunk.lines.len() * 2);
        let mut delete_count = 0;

        let ops = &hunk.lines;
        let mut i = 0;
        while i < ops.len() {
            match &ops[i] {
                LineOp::Context(s) => {
                    delete_count += 1;
                    result.push(s.clone());
                    i += 1;
                }
                LineOp::Delete(_) | LineOp::Add(_) => {
                    // Gather a run of consecutive deletes followed by adds.
                    let mut dels: Vec<String> = Vec::new();
                    while let Some(LineOp::Delete(s)) = ops.get(i) {
                        dels.push(s.clone());
                        delete_count += 1;
                        i += 1;
                    }
                    let mut adds: Vec<String> = Vec::new();
                    while let Some(LineOp::Add(s)) = ops.get(i) {
                        adds.push(s.clone());
                        i += 1;
                    }

                    if !dels.is_empty() && !adds.is_empty() {
                        result.push(format!("#ifndef {}", define));
                        result.extend(dels);
                        result.push("#else".to_string());
                        result.extend(adds);
                        result.push("#endif".to_string());
                    } else if !adds.is_empty() {
                        result.push(format!("#ifdef {}", define));
                        result.extend(adds);
                        result.push("#endif".to_string());
                    } else if !dels.is_empty() {
                        result.push(format!("#ifndef {}", define));
                        result.extend(dels);
                        result.push("#endif".to_string());
                    }
                }
            }
        }

        // Use splice for O(n) instead of drain() + multiple insert() which is O(n²)
        let remove_end = (pos + delete_count).min(self.file_lines.len());
        self.file_lines.splice(pos..remove_end, result);
    }
}

/// Normalize whitespace for loose matching.
fn normalize_whitespace(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}
