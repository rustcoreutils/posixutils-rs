//
// Copyright (c) 2025 Jeff Garzik
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
}

impl<'a> PatchApplier<'a> {
    /// Create a new applier with the given configuration and file content.
    pub fn new(config: &'a PatchConfig, file_lines: Vec<String>) -> Self {
        Self {
            config,
            file_lines,
            offset: 0,
        }
    }

    /// Apply all hunks from a file patch.
    pub fn apply_patch(&mut self, patch: &mut FilePatch) -> Result<ApplyResult, PatchError> {
        let mut rejected_hunks: Vec<(usize, Hunk, String)> = Vec::new();
        let mut had_offset = false;
        let mut had_fuzz = false;

        for (i, hunk) in patch.hunks.iter_mut().enumerate() {
            let hunk_num = i + 1;
            match self.apply_hunk(hunk, hunk_num) {
                HunkResult::Applied { offset, fuzz } => {
                    if offset != 0 {
                        had_offset = true;
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
                        had_fuzz = true;
                        eprintln!("Hunk #{} succeeded with fuzz {}", hunk_num, fuzz);
                    }
                    // Update cumulative offset
                    self.offset += (hunk.new_count as i64) - (hunk.old_count as i64);
                }
                HunkResult::AlreadyApplied => {
                    if !self.config.ignore_applied {
                        return Err(PatchError::ReversedPatch);
                    }
                    eprintln!("Hunk #{} already applied", hunk_num);
                }
                HunkResult::Rejected { reason } => {
                    rejected_hunks.push((hunk_num, hunk.clone(), reason));
                }
            }
        }

        Ok(ApplyResult {
            rejected_hunks,
            content: self.file_lines.clone(),
            had_offset,
            had_fuzz,
        })
    }

    /// Apply a single hunk.
    fn apply_hunk(&mut self, hunk: &Hunk, _hunk_num: usize) -> HunkResult {
        let old_lines = hunk.get_old_lines();

        if old_lines.is_empty() {
            // Pure addition - insert at the right position
            let insert_pos = if hunk.old_start == 0 {
                0
            } else {
                ((hunk.old_start as i64 - 1 + self.offset).max(0) as usize)
                    .min(self.file_lines.len())
            };

            let new_lines = hunk.get_new_lines();
            for (i, line) in new_lines.iter().enumerate() {
                self.file_lines.insert(insert_pos + i, line.to_string());
            }
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
        // Count deletes and build new lines
        let mut delete_count = 0;
        let mut new_lines: Vec<String> = Vec::new();

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

        // Remove old lines
        let remove_end = (pos + delete_count).min(self.file_lines.len());
        self.file_lines.drain(pos..remove_end);

        // Insert new lines
        for (i, line) in new_lines.into_iter().enumerate() {
            self.file_lines.insert(pos + i, line);
        }
    }

    /// Perform changes with #ifdef wrapping.
    fn perform_ifdef_changes(&mut self, hunk: &Hunk, pos: usize, define: &str) {
        let mut result: Vec<String> = Vec::new();

        for op in &hunk.lines {
            match op {
                LineOp::Context(s) => {
                    result.push(s.clone());
                }
                LineOp::Delete(s) => {
                    result.push(format!("#ifndef {}", define));
                    result.push(s.clone());
                    result.push(format!("#endif /* !{} */", define));
                }
                LineOp::Add(s) => {
                    result.push(format!("#ifdef {}", define));
                    result.push(s.clone());
                    result.push(format!("#endif /* {} */", define));
                }
            }
        }

        // Count how many lines we need to remove
        let delete_count = hunk
            .lines
            .iter()
            .filter(|op| matches!(op, LineOp::Context(_) | LineOp::Delete(_)))
            .count();

        // Remove old lines
        let remove_end = (pos + delete_count).min(self.file_lines.len());
        self.file_lines.drain(pos..remove_end);

        // Insert new lines
        for (i, line) in result.into_iter().enumerate() {
            self.file_lines.insert(pos + i, line);
        }
    }
}

/// Normalize whitespace for loose matching.
fn normalize_whitespace(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}
