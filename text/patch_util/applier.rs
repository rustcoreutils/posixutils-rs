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
use std::hash::{BuildHasher, RandomState};

/// How many lines of context a fuzzy match may ignore at each end, unless -F
/// says otherwise.
///
/// POSIX describes exactly two rescans: one ignoring the first and last line of
/// context, then one ignoring the first two and last two.
const DEFAULT_MAX_FUZZ: usize = 2;

/// What to do with a patch that looks reversed or already applied.
enum ReversalChoice {
    /// Reverse the patch and apply it (the user asked for -R after all).
    ApplyReversed,
    /// Apply it as written; the hunks will reject, leaving the file alone.
    ApplyForward,
    /// Leave the file untouched and send every hunk to the reject file.
    Skip,
}

/// Applies patches to file content.
pub struct PatchApplier<'a> {
    config: &'a PatchConfig,
    file_lines: Vec<String>,
    /// A hash of each line of `file_lines` (of its blank-normalized form
    /// under -l), kept in step with it, so locating a hunk costs one pass
    /// over the file rather than one comparison per file line per hunk line.
    line_hashes: Vec<u64>,
    /// Prefix hashes of `line_hashes` ([`Self::prefix_hashes`]), built when a
    /// search first needs them and dropped when the file changes, so hunks
    /// that are rejected one after another share one.
    prefix: Option<Vec<u64>>,
    hasher: RandomState,
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
        let hasher = RandomState::new();
        let line_hashes = file_lines
            .iter()
            .map(|l| line_hash(&hasher, config, l))
            .collect();
        Self {
            config,
            file_lines,
            line_hashes,
            prefix: None,
            hasher,
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
            match self.decide_reversal() {
                ReversalChoice::ApplyReversed => patch.reverse(),
                ReversalChoice::ApplyForward => {}
                ReversalChoice::Skip => {
                    // Skip this file rather than abandoning the run: a patch
                    // covering several files may have had only this one applied
                    // already, and the rest still need patching. The hunks go to
                    // the reject file, which POSIX makes exit status 1.
                    eprintln!("patch: {}", gettext("Skipping patch."));
                    return Ok(self.skip_all(patch));
                }
            }
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
            let adjust = |start: usize| {
                let moved = i64::try_from(start)
                    .unwrap_or(i64::MAX)
                    .saturating_add(self.offset)
                    .max(1);
                usize::try_from(moved).unwrap_or(usize::MAX)
            };
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

    /// Decide what to do about a patch that looks reversed or already applied.
    fn decide_reversal(&self) -> ReversalChoice {
        // -f is "do not ask any questions and assume answers". The answer to
        // assume is that the patch is *not* reversed: applying it forward
        // rejects the hunks and leaves the file alone, whereas assuming -R
        // would silently undo a change the file already has.
        if self.config.force {
            return ReversalChoice::ApplyForward;
        }
        // -t asks nothing either, but its assumed answer is GNU's: a patch
        // that looks reversed is reversed.
        if self.config.batch {
            eprintln!(
                "patch: {}",
                gettext("Reversed (or previously applied) patch detected!  Assuming -R.")
            );
            return ReversalChoice::ApplyReversed;
        }
        match super::file_ops::prompt_yes_no(
            "Reversed (or previously applied) patch detected!  Assume -R? [y] ",
        ) {
            Some(true) => ReversalChoice::ApplyReversed,
            // No controlling terminal to ask means no answer, which is a "no".
            Some(false) | None => ReversalChoice::Skip,
        }
    }

    /// Apply a hunk whose position is recorded absolutely (an ed script).
    ///
    /// There is no old-side text to verify and no cumulative offset to carry:
    /// an ed script's line numbers already describe the file as it stands when
    /// the command runs. How much to remove comes from `old_count`, because
    /// the script does not record the removed text.
    fn apply_positional_hunk(&mut self, hunk: &Hunk) -> HunkResult {
        let pos = hunk.old_start.saturating_sub(1).min(self.file_lines.len());
        let remove_end = pos
            .saturating_add(hunk.old_count)
            .min(self.file_lines.len());
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
    ///
    /// The header's line number is untrusted: the search starts from it, but
    /// never from beyond the end of the file. More fuzz than the hunk has
    /// lines would only retry windows already tried, so the fuzz levels are
    /// bounded by the hunk's length as well as by -F.
    fn apply_matched_hunk(&mut self, hunk: &Hunk) -> HunkResult {
        let named = i64::try_from(hunk.old_start)
            .unwrap_or(i64::MAX)
            .saturating_sub(1)
            .saturating_add(self.offset)
            .max(0);
        let expected = usize::try_from(named)
            .unwrap_or(usize::MAX)
            .min(self.file_lines.len());

        let max_fuzz = self
            .config
            .max_fuzz
            .unwrap_or(DEFAULT_MAX_FUZZ)
            .min(hunk.lines.len());
        for fuzz in 0..=max_fuzz {
            let window = if fuzz == 0 {
                Some(hunk.full_window())
            } else {
                hunk.fuzz_window(fuzz)
            };
            let Some(window) = window else { continue };
            if let Some(pos) = self.locate_hunk(&window, expected) {
                self.apply_window(hunk, &window, pos);
                // The lines the hunk really carries, not its header's counts,
                // which nothing has checked.
                let full = hunk.full_window();
                let grew = full.new_lines().len() as i64 - full.old_lines().len() as i64;
                self.offset = self.offset.saturating_add(grew);
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
    /// The scan covers the whole file, nearest position first. POSIX asks for
    /// "at least 1 000 bytes" either way; GNU patch looks everywhere, and
    /// series of patches rely on it (one of Debian glibc's hunks lands over a
    /// thousand lines from the line it names).
    ///
    /// Returns the position of the hunk's first line, which sits `lead_skip`
    /// lines before the text that was actually verified.
    ///
    /// `expected` is at most the file's length, so each direction runs off
    /// the file within that many steps. Past `expected` itself, a position is
    /// compared line by line only when the window's hash matches the file's
    /// there (from the cached prefix hashes), so the whole search is linear
    /// in the file and the window, whatever the file repeats.
    fn locate_hunk(&mut self, window: &MatchWindow, expected: usize) -> Option<usize> {
        let old_lines = window.old_lines();
        let skip = window.lead_skip;
        let len = old_lines.len();
        if len + skip > self.file_lines.len() {
            return None;
        }
        // Furthest hunk start at which the window still fits inside the file.
        let last_start = self.file_lines.len() - (len + skip);
        if expected <= last_start && self.lines_match_at(&old_lines, expected + skip) {
            return Some(expected);
        }

        if self.prefix.is_none() {
            self.prefix = Some(self.prefix_hashes());
        }
        let prefix = self.prefix.as_deref().unwrap_or_default();
        let want = old_lines.iter().fold(0u64, |h, l| {
            h.wrapping_mul(HASH_BASE)
                .wrapping_add(line_hash(&self.hasher, self.config, l))
        });
        let base_pow = (0..len).fold(1u64, |p, _| p.wrapping_mul(HASH_BASE));
        let matches = |start: usize| {
            let at = start + skip;
            let got = prefix[at + len].wrapping_sub(prefix[at].wrapping_mul(base_pow));
            got == want && self.lines_match_at(&old_lines, at)
        };

        let reach = expected.max(last_start.saturating_sub(expected));
        for delta in 1..=reach {
            if expected + delta <= last_start && matches(expected + delta) {
                return Some(expected + delta);
            }
            if delta > 0
                && delta <= expected
                && expected - delta <= last_start
                && matches(expected - delta)
            {
                return Some(expected - delta);
            }
        }
        None
    }

    /// Polynomial prefix hashes of the file's lines: `prefix[i]` covers lines
    /// `0..i`, so any run of lines hashes in constant time.
    fn prefix_hashes(&self) -> Vec<u64> {
        let mut prefix = Vec::with_capacity(self.line_hashes.len() + 1);
        prefix.push(0u64);
        let mut h = 0u64;
        for &lh in &self.line_hashes {
            h = h.wrapping_mul(HASH_BASE).wrapping_add(lh);
            prefix.push(h);
        }
        prefix
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
        let hashes: Vec<u64> = replacement
            .iter()
            .map(|l| line_hash(&self.hasher, self.config, l))
            .collect();
        self.line_hashes.splice(at..remove_end, hashes);
        self.prefix = None;
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

/// Multiplier for the polynomial hash of a run of lines (odd, so it is
/// invertible modulo 2^64; the line hashes it combines are keyed per run).
const HASH_BASE: u64 = 0x9e37_79b9_7f4a_7c15;

/// The hash of one line as `lines_match` compares it: blank runs normalized
/// under -l, exact otherwise.
fn line_hash(hasher: &RandomState, config: &PatchConfig, line: &str) -> u64 {
    if config.loose_whitespace {
        hasher.hash_one(normalize_whitespace(line))
    } else {
        hasher.hash_one(line)
    }
}

/// Normalize whitespace for loose matching.
fn normalize_whitespace(s: &str) -> String {
    s.split_ascii_whitespace().collect::<Vec<_>>().join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::patch_util::types::DiffFormat;

    fn hunk_with(old_start: usize, ops: Vec<LineOp>) -> Hunk {
        let mut h = Hunk::new(old_start, 1, old_start, 1);
        h.lines = ops;
        h
    }

    /// Declining a reversed patch must leave the file exactly as it was and
    /// send every hunk to the reject file. This path is otherwise only
    /// reachable through a prompt on the controlling terminal.
    #[test]
    fn skip_all_rejects_every_hunk_and_keeps_content() {
        let config = PatchConfig::default();
        let original = vec![String::from("a"), String::from("b")];
        let mut applier = PatchApplier::new(&config, original.clone(), true);

        let mut patch = FilePatch::new(DiffFormat::Unified);
        patch.hunks.push(hunk_with(
            1,
            vec![
                LineOp::Delete(String::from("a")),
                LineOp::Add(String::from("A")),
            ],
        ));
        patch.hunks.push(hunk_with(
            2,
            vec![
                LineOp::Delete(String::from("b")),
                LineOp::Add(String::from("B")),
            ],
        ));

        let result = applier.skip_all(&patch);

        assert_eq!(result.content, original, "file content must be untouched");
        assert!(!result.applied_any, "nothing was applied");
        assert_eq!(result.rejected_hunks.len(), 2, "every hunk is rejected");
        let numbers: Vec<usize> = result.rejected_hunks.iter().map(|(n, _, _)| *n).collect();
        assert_eq!(numbers, vec![1, 2], "hunks are numbered from one");
        for (_, _, reason) in &result.rejected_hunks {
            assert!(reason.contains("previously applied"), "reason: {}", reason);
        }
    }
}
