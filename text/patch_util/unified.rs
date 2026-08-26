//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Unified diff format parser.

use super::header::parse_filename;
use super::types::{DiffFormat, FilePatch, Hunk, LineOp, PatchError};
use regex::Regex;
use std::sync::LazyLock;

/// Pre-compiled regex for hunk headers to avoid recompilation on each parse.
static HUNK_HEADER_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@").expect("invalid regex")
});

/// Parse a unified diff from the given lines.
pub fn parse_unified(lines: &[&str], start: usize) -> Result<(FilePatch, usize), PatchError> {
    let mut patch = FilePatch::new(DiffFormat::Unified);
    let mut pos = start;

    // Parse headers: --- old_file and +++ new_file
    while pos < lines.len() {
        let line = lines[pos];
        if let Some(rest) = line.strip_prefix("--- ") {
            patch.old_path = Some(parse_filename(rest));
            if patch.old_path.as_deref() == Some("/dev/null") {
                patch.is_new_file = true;
            }
            pos += 1;
            break;
        } else if let Some(rest) = line.strip_prefix("Index: ") {
            patch.index_path = Some(rest.trim().to_string());
            pos += 1;
        } else {
            pos += 1;
        }
    }

    // Expect +++ line
    if pos < lines.len() && lines[pos].starts_with("+++ ") {
        patch.new_path = Some(parse_filename(&lines[pos][4..]));
        if patch.new_path.as_deref() == Some("/dev/null") {
            patch.is_delete_file = true;
        }
        pos += 1;
    }

    // Parse hunks using pre-compiled static regex
    while pos < lines.len() {
        let line = lines[pos];

        // Check for end of this patch (start of next)
        if line.starts_with("diff ")
            || line.starts_with("Index: ")
            || (line.starts_with("--- ")
                && pos + 1 < lines.len()
                && lines[pos + 1].starts_with("+++ "))
        {
            break;
        }

        // Parse hunk header
        if let Some(caps) = HUNK_HEADER_RE.captures(line) {
            let old_start: usize = caps[1].parse().unwrap_or(1);
            let old_count: usize = caps.get(2).map_or(1, |m| m.as_str().parse().unwrap_or(1));
            let new_start: usize = caps[3].parse().unwrap_or(1);
            let new_count: usize = caps.get(4).map_or(1, |m| m.as_str().parse().unwrap_or(1));

            // A unified header spells a zero-count side as the line *before*
            // which the change happens ("@@ -5,0 +6,2 @@" inserts after line
            // 5). `Hunk` wants the line before which to insert, so add one.
            let old_start = if old_count == 0 {
                old_start + 1
            } else {
                old_start
            };
            let new_start = if new_count == 0 {
                new_start + 1
            } else {
                new_start
            };

            let mut hunk = Hunk::new(old_start, old_count, new_start, new_count);
            pos += 1;

            // Track the side(s) the previous content line belongs to, so a
            // following "\ No newline at end of file" marker is attributed
            // correctly: context lines belong to both old and new, '+' to new,
            // '-' to old.
            let mut prev_old = false;
            let mut prev_new = false;

            // Consume exactly the number of old/new lines declared in the
            // header. This prevents the next file's "--- " header (which begins
            // with '-') from being swallowed as a delete line, so a patch file
            // covering several files splits into separate FilePatches.
            let mut old_remaining = old_count;
            let mut new_remaining = new_count;

            // Parse hunk lines
            while pos < lines.len() {
                let hunk_line = lines[pos];

                // "\ No newline at end of file" applies to the preceding line
                // and does not consume any old/new line count.
                if hunk_line.starts_with('\\') {
                    if prev_old {
                        hunk.old_no_newline = true;
                    }
                    if prev_new {
                        hunk.new_no_newline = true;
                    }
                    pos += 1;
                    continue;
                }

                // Stop once all declared old and new lines are consumed.
                if old_remaining == 0 && new_remaining == 0 {
                    break;
                }

                if let Some(rest) = hunk_line.strip_prefix(' ') {
                    hunk.lines.push(LineOp::Context(rest.to_string()));
                    prev_old = true;
                    prev_new = true;
                    old_remaining = old_remaining.saturating_sub(1);
                    new_remaining = new_remaining.saturating_sub(1);
                    pos += 1;
                } else if let Some(rest) = hunk_line.strip_prefix('+') {
                    hunk.lines.push(LineOp::Add(rest.to_string()));
                    prev_old = false;
                    prev_new = true;
                    new_remaining = new_remaining.saturating_sub(1);
                    pos += 1;
                } else if let Some(rest) = hunk_line.strip_prefix('-') {
                    hunk.lines.push(LineOp::Delete(rest.to_string()));
                    prev_old = true;
                    prev_new = false;
                    old_remaining = old_remaining.saturating_sub(1);
                    pos += 1;
                } else if hunk_line.starts_with('@') {
                    // Next hunk
                    break;
                } else if hunk_line.is_empty() {
                    // Empty line could be context with no prefix (some patches do this)
                    hunk.lines.push(LineOp::Context(String::new()));
                    prev_old = true;
                    prev_new = true;
                    old_remaining = old_remaining.saturating_sub(1);
                    new_remaining = new_remaining.saturating_sub(1);
                    pos += 1;
                } else {
                    // End of hunk content
                    break;
                }
            }

            patch.hunks.push(hunk);
        } else {
            pos += 1;
        }
    }

    Ok((patch, pos))
}

/// Index of the first line that identifies this as a unified diff.
///
/// Returning the position, rather than a bare yes, lets the caller pick the
/// format whose marker appears earliest. Without that, a marker buried in some
/// other format's *content* -- an ed script's text block, say -- outvotes the
/// real header at the top of the file.
pub fn unified_marker(lines: &[&str]) -> Option<usize> {
    // Scan the whole input: a mailed patch or `git format-patch` output can
    // carry an arbitrarily long commit message before the first header.
    for (i, line) in lines.iter().enumerate() {
        if HUNK_HEADER_RE.is_match(line) {
            return Some(i);
        }
        // Require the two file headers to be adjacent, as diff writes them.
        // Testing only that both appear somewhere would misread any patch
        // whose *content* happens to contain a "+++ " line.
        if line.starts_with("--- ") && lines.get(i + 1).is_some_and(|n| n.starts_with("+++ ")) {
            return Some(i);
        }
    }

    None
}
