//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Hunk application logic with fuzzy matching.

use super::types::{
    ApplyResult, FilePatch, Hunk, HunkResult, LineOp, MatchWindow, PatchConfig, PatchError,
    Placement,
};
use gettextrs::gettext;

/// How far to scan in each direction for a hunk's context.
///
/// POSIX requires scanning "at least 1 000 bytes"; a line is at least one byte,
/// so this many lines always satisfies it.
const MAX_SCAN_LINES: usize = 1000;

/// How many lines of context a fuzzy match may ignore at each end.
///
/// POSIX describes exactly two rescans: one ignoring the first and last line of
/// context, then one ignoring the first two and last two.
const MAX_FUZZ: usize = 2;

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
                // Skip this file rather than abandoning the run: a patch
                // covering several files may have had only this one applied
                // already, and the rest still need patching. The hunks go to
                // the reject file, which POSIX makes exit status 1.
                eprintln!("patch: {}", gettext("Skipping patch."));
                return Ok(self.skip_all(patch));
            }
            // With -f and a "no" decision, fall through and apply forward
            // (which will reject), matching GNU's "apply anyway" path.
        }

        let placement = Placement::from(patch.format);
        let mut rejected_hunks: Vec<(usize, Hunk, String)> = Vec::new();
        let mut applied_any = false;

        for (i, hunk) in patch.hunks.iter_mut().enumerate() {
            let hunk_num = i + 1;
            let result = match placement {
                Placement::Positional => self.apply_positional_hunk(hunk),
                Placement::ContentMatched => self.apply_matched_hunk(hunk),
            };
            match result {
                HunkResult::Applied { line, offset, fuzz } => {
                    if offset != 0 {
                        eprintln!(
                            "Hunk #{} succeeded at {} (offset {} line{})",
                            hunk_num,
                            line,
                            offset,
                            if offset.abs() == 1 { "" } else { "s" }
                        );
                    }
                    if fuzz > 0 {
                        eprintln!("Hunk #{} succeeded with fuzz {}", hunk_num, fuzz);
                    }
                    applied_any = true;
                }
                HunkResult::AlreadyApplied => {
                    if !self.config.ignore_applied {
                        // Not an error for the run as a whole: reject this hunk
                        // and carry on, so the remaining hunks and files are
                        // still attempted.
                        rejected_hunks.push((
                            hunk_num,
                            self.reject_at_offset(hunk),
                            String::from("reversed (or previously applied) patch"),
                        ));
                        continue;
                    }
                    eprintln!("Hunk #{} already applied", hunk_num);
                }
                HunkResult::Rejected { reason } => {
                    rejected_hunks.push((hunk_num, self.reject_at_offset(hunk), reason));
                }
            }
        }

        let no_trailing_newline = self.eof_no_newline && !self.file_lines.is_empty();

        Ok(ApplyResult {
            rejected_hunks,
            // Use mem::take to avoid cloning the entire file content
            content: std::mem::take(&mut self.file_lines),
            no_trailing_newline,
            applied_any,
        })
    }

    /// Copy a hunk for the reject file, shifting its header line numbers by the
    /// offset accumulated so far so they approximate positions in the
    /// partially patched file (matching GNU).
    fn reject_at_offset(&self, hunk: &Hunk) -> Hunk {
        let mut rej = hunk.clone();
        if self.offset != 0 {
            let adjust = |start: usize| ((start as i64 + self.offset).max(1)) as usize;
            rej.old_start = adjust(rej.old_start);
            rej.new_start = adjust(rej.new_start);
        }
        rej
    }

    /// Reject every hunk without touching the file, for a patch the user
    /// declined to apply.
    fn skip_all(&mut self, patch: &FilePatch) -> ApplyResult {
        let rejected_hunks = patch
            .hunks
            .iter()
            .enumerate()
            .map(|(i, hunk)| {
                (
                    i + 1,
                    hunk.clone(),
                    String::from("reversed (or previously applied) patch"),
                )
            })
            .collect();
        ApplyResult {
            rejected_hunks,
            content: std::mem::take(&mut self.file_lines),
            no_trailing_newline: self.eof_no_newline,
            applied_any: false,
        }
    }

    /// Detect whether the patch appears reversed/already-applied: its first
    /// content hunk fails to apply forward at its expected position but the
    /// reversed hunk (the new-side lines) matches there.
    fn detect_reversed(&self, patch: &FilePatch) -> bool {
        // A positional hunk records no old-side text, so there is nothing to
        // test either way.
        if Placement::from(patch.format) == Placement::Positional {
            return false;
        }
        for hunk in &patch.hunks {
            let window = hunk.full_window();
            let old_lines = window.old_lines();
            // A pure addition matches anywhere, so it can never disprove
            // forward applicability.
            if old_lines.is_empty() {
                continue;
            }
            // This runs before any hunk is applied, so `self.offset` is still
            // zero and the recorded line number is the position to test.
            let expected_pos = hunk.old_start.saturating_sub(1).min(self.file_lines.len());
            if self.lines_match_at(&old_lines, expected_pos) {
                return false;
            }
            let new_lines = window.new_lines();
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

    /// Apply a hunk whose position is recorded absolutely (an ed script).
    ///
    /// There is no old-side text to verify and no cumulative offset to carry:
    /// an ed script's line numbers already describe the file as it stands when
    /// the command runs. How much to remove comes from `old_count`, because
    /// the script does not record the removed text.
    fn apply_positional_hunk(&mut self, hunk: &Hunk) -> HunkResult {
        let pos = hunk.old_start.saturating_sub(1).min(self.file_lines.len());
        let remove_end = (pos + hunk.old_count).min(self.file_lines.len());
        let adds: Vec<&str> = hunk
            .lines
            .iter()
            .filter_map(|op| match op {
                LineOp::Add(s) => Some(s.as_str()),
                LineOp::Context(_) | LineOp::Delete(_) => None,
            })
            .collect();

        let (replacement, ends_with_directive) = match self.config.ifdef_define.as_deref() {
            // An ed script does not carry the text it removes, so read it back
            // from the file to build the #ifndef arm.
            Some(define) => {
                let dels: Vec<&str> = self.file_lines[pos..remove_end]
                    .iter()
                    .map(|s| s.as_str())
                    .collect();
                let block = ifdef_block(define, &dels, &adds);
                // A non-empty block always closes with #endif; an empty one
                // emitted no directive to close.
                let ends_with_directive = !block.is_empty();
                (block, ends_with_directive)
            }
            None => (adds.iter().map(|s| s.to_string()).collect(), false),
        };

        self.splice(
            pos,
            remove_end,
            replacement,
            hunk.new_no_newline && !ends_with_directive,
        );

        HunkResult::Applied {
            line: pos + 1,
            offset: 0,
            fuzz: 0,
        }
    }

    /// Apply a hunk located by matching its recorded old-side text.
    ///
    /// POSIX: begin searching at the hunk's own line number plus the offset
    /// accumulated by previously applied hunks, scanning both ways. If that
    /// fails and the hunk carries context, rescan ignoring the first and last
    /// line of context, then the first two and last two.
    fn apply_matched_hunk(&mut self, hunk: &Hunk) -> HunkResult {
        let expected = (hunk.old_start as i64 - 1 + self.offset).max(0) as usize;

        for fuzz in 0..=MAX_FUZZ {
            let window = if fuzz == 0 {
                Some(hunk.full_window())
            } else {
                hunk.fuzz_window(fuzz)
            };
            let Some(window) = window else { continue };
            if let Some(pos) = self.locate_hunk(&window, expected) {
                self.apply_window(hunk, &window, pos);
                self.offset += hunk.new_count as i64 - hunk.old_count as i64;
                return HunkResult::Applied {
                    line: pos + 1,
                    offset: pos as i64 - expected as i64,
                    fuzz,
                };
            }
        }

        let new_lines = hunk.full_window().new_lines();
        if !new_lines.is_empty() && self.lines_match_at(&new_lines, expected) {
            return HunkResult::AlreadyApplied;
        }

        HunkResult::Rejected {
            reason: format!("patch does not apply at line {}", expected + 1),
        }
    }

    /// Scan outward from `expected` for a place where the window's old-side
    /// text matches.
    ///
    /// Returns the position of the hunk's first line, which sits `lead_skip`
    /// lines before the text that was actually verified.
    fn locate_hunk(&self, window: &MatchWindow, expected: usize) -> Option<usize> {
        let old_lines = window.old_lines();
        let skip = window.lead_skip;
        // Furthest hunk start at which the window still fits inside the file.
        let last_start = self.file_lines.len().saturating_sub(old_lines.len() + skip);

        for delta in 0..=MAX_SCAN_LINES {
            if expected + delta <= last_start
                && self.lines_match_at(&old_lines, expected + delta + skip)
            {
                return Some(expected + delta);
            }
            if delta > 0
                && delta <= expected
                && self.lines_match_at(&old_lines, expected - delta + skip)
            {
                return Some(expected - delta);
            }
            // Both directions have run off the end of the file.
            if delta > expected && expected + delta > last_start {
                break;
            }
        }
        None
    }

    /// Splice the window's new-side text over the file lines it matched.
    ///
    /// Only the window is written. Context lines that a fuzzy match agreed to
    /// ignore are left exactly as the file has them — they were never verified,
    /// so the patch's copy of them is not evidence of anything.
    fn apply_window(&mut self, hunk: &Hunk, window: &MatchWindow, pos: usize) {
        let file_start = pos + window.lead_skip;
        let old_len = window.old_lines().len();
        let remove_end = (file_start + old_len).min(self.file_lines.len());

        let (replacement, ends_with_directive) = match self.config.ifdef_define.as_deref() {
            Some(define) => ifdef_lines(window.ops(), define),
            None => (
                window.new_lines().iter().map(|s| s.to_string()).collect(),
                false,
            ),
        };

        self.splice(
            file_start,
            remove_end,
            replacement,
            hunk.new_no_newline && !ends_with_directive,
        );
    }

    /// Replace `file_lines[at..remove_end]` with `replacement`, recording
    /// whether the file now ends without a trailing newline.
    ///
    /// Only a write that reaches the end of the file can change that; under
    /// fuzz the file's real last line was never replaced, so the write ends
    /// short and the marker correctly stays put.
    ///
    /// A hunk that removes nothing and adds nothing leaves the file exactly as
    /// it was, so it must not disturb the marker either. Such a hunk is
    /// degenerate rather than typical -- an ed `a` command with an empty text
    /// block, or a "@@ -2,0 +3,0 @@" unified header -- but without this guard
    /// one sitting at end of file would add or drop a trailing newline while
    /// changing no line at all.
    fn splice(&mut self, at: usize, remove_end: usize, replacement: Vec<String>, no_newline: bool) {
        if replacement.is_empty() && remove_end == at {
            return;
        }
        let write_end = at + replacement.len();
        self.file_lines.splice(at..remove_end, replacement);
        if write_end == self.file_lines.len() {
            self.eof_no_newline = no_newline;
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
}

/// Render one window's operations as `#ifdef`-guarded text (-D).
///
/// Returns the lines to write and whether the last of them is a synthesized
/// preprocessor directive. A directive always carries its own newline, so it
/// overrides the hunk's "no newline at end of file" marker.
fn ifdef_lines(ops: &[LineOp], define: &str) -> (Vec<String>, bool) {
    let mut result: Vec<String> = Vec::with_capacity(ops.len() * 2);
    let mut last_is_directive = false;
    let mut i = 0;

    while i < ops.len() {
        if let LineOp::Context(s) = &ops[i] {
            result.push(s.clone());
            last_is_directive = false;
            i += 1;
            continue;
        }
        // A change is a run of deletions followed by a run of additions;
        // either run may be empty, but not both.
        let mut dels: Vec<&str> = Vec::new();
        while let Some(LineOp::Delete(s)) = ops.get(i) {
            dels.push(s);
            i += 1;
        }
        let mut adds: Vec<&str> = Vec::new();
        while let Some(LineOp::Add(s)) = ops.get(i) {
            adds.push(s);
            i += 1;
        }
        result.extend(ifdef_block(define, &dels, &adds));
        last_is_directive = true;
    }

    (result, last_is_directive)
}

/// Build one `-D` guarded block from a run of removed and added lines.
fn ifdef_block(define: &str, dels: &[&str], adds: &[&str]) -> Vec<String> {
    let mut out: Vec<String> = Vec::with_capacity(dels.len() + adds.len() + 3);
    match (dels.is_empty(), adds.is_empty()) {
        // A replacement: the old text under #ifndef, the new under #else.
        (false, false) => {
            out.push(format!("#ifndef {}", define));
            out.extend(dels.iter().map(|s| s.to_string()));
            out.push(String::from("#else"));
            out.extend(adds.iter().map(|s| s.to_string()));
        }
        // A pure addition: new text only when the macro is defined.
        (true, false) => {
            out.push(format!("#ifdef {}", define));
            out.extend(adds.iter().map(|s| s.to_string()));
        }
        // A pure deletion: old text kept only when the macro is not defined.
        (false, true) => {
            out.push(format!("#ifndef {}", define));
            out.extend(dels.iter().map(|s| s.to_string()));
        }
        (true, true) => return out,
    }
    out.push(String::from("#endif"));
    out
}

/// Normalize whitespace for loose matching.
fn normalize_whitespace(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}
