//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Unified diff format parser.

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

            let mut hunk = Hunk::new(old_start, old_count, new_start, new_count);
            pos += 1;

            // Parse hunk lines
            while pos < lines.len() {
                let hunk_line = lines[pos];

                if let Some(rest) = hunk_line.strip_prefix(' ') {
                    hunk.lines.push(LineOp::Context(rest.to_string()));
                    pos += 1;
                } else if let Some(rest) = hunk_line.strip_prefix('+') {
                    hunk.lines.push(LineOp::Add(rest.to_string()));
                    pos += 1;
                } else if let Some(rest) = hunk_line.strip_prefix('-') {
                    hunk.lines.push(LineOp::Delete(rest.to_string()));
                    pos += 1;
                } else if hunk_line.starts_with('\\') {
                    // "\ No newline at end of file" - skip
                    pos += 1;
                } else if hunk_line.starts_with('@') {
                    // Next hunk
                    break;
                } else if hunk_line.is_empty() {
                    // Empty line could be context with no prefix (some patches do this)
                    hunk.lines.push(LineOp::Context(String::new()));
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

/// Parse filename from a header line (remove timestamp if present).
fn parse_filename(s: &str) -> String {
    // Format: "filename\ttimestamp" or just "filename"
    let s = s.trim();
    if let Some(tab_pos) = s.find('\t') {
        s[..tab_pos].to_string()
    } else if let Some(space_pos) = s.find("  ") {
        // Some diffs use double-space as separator
        s[..space_pos].to_string()
    } else {
        s.to_string()
    }
}

/// Check if a line looks like a unified diff header.
pub fn looks_like_unified(lines: &[&str]) -> bool {
    let mut has_minus = false;
    let mut has_plus = false;

    for line in lines.iter().take(20) {
        if line.starts_with("--- ") {
            has_minus = true;
        }
        if line.starts_with("+++ ") {
            has_plus = true;
        }
        if line.starts_with("@@ ") {
            return true;
        }
    }

    has_minus && has_plus
}
