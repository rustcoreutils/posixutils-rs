//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Context diff format parser.

use super::types::{DiffFormat, FilePatch, Hunk, LineOp, PatchError};
use regex::Regex;
use std::sync::LazyLock;

/// Pre-compiled regex for old range headers to avoid recompilation on each parse.
static OLD_RANGE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^\*\*\* (\d+)(?:,(\d+))? \*\*\*\*").expect("invalid regex"));

/// Pre-compiled regex for new range headers to avoid recompilation on each parse.
static NEW_RANGE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^--- (\d+)(?:,(\d+))? ----").expect("invalid regex"));

/// Parse a context diff from the given lines.
pub fn parse_context(lines: &[&str], start: usize) -> Result<(FilePatch, usize), PatchError> {
    let mut patch = FilePatch::new(DiffFormat::Context);
    let mut pos = start;

    // Parse headers: *** old_file and --- new_file
    while pos < lines.len() {
        let line = lines[pos];
        if let Some(rest) = line.strip_prefix("*** ") {
            if !rest.starts_with("**********") {
                patch.old_path = Some(parse_filename(rest));
                if patch.old_path.as_deref() == Some("/dev/null") {
                    patch.is_new_file = true;
                }
                pos += 1;
                break;
            }
        } else if let Some(rest) = line.strip_prefix("Index: ") {
            patch.index_path = Some(rest.trim().to_string());
        }
        pos += 1;
    }

    // Expect --- line (new file in context diff)
    if pos < lines.len() && lines[pos].starts_with("--- ") {
        patch.new_path = Some(parse_filename(&lines[pos][4..]));
        if patch.new_path.as_deref() == Some("/dev/null") {
            patch.is_delete_file = true;
        }
        pos += 1;
    }

    // Parse hunks using pre-compiled static regexes
    while pos < lines.len() {
        let line = lines[pos];

        // Check for end of this patch
        if line.starts_with("diff ") || line.starts_with("Index: ") {
            break;
        }
        if line.starts_with("*** ")
            && !line.starts_with("***************")
            && pos + 1 < lines.len()
            && lines[pos + 1].starts_with("--- ")
        {
            break;
        }

        // Look for hunk separator
        if line.starts_with("***************") {
            pos += 1;
            if pos >= lines.len() {
                break;
            }

            // Parse old section: *** start,end ****
            let old_line = lines[pos];
            let (old_start, old_end) = if let Some(caps) = OLD_RANGE_RE.captures(old_line) {
                let start: usize = caps[1].parse().unwrap_or(1);
                let end: usize = caps
                    .get(2)
                    .map_or(start, |m| m.as_str().parse().unwrap_or(start));
                (start, end)
            } else {
                pos += 1;
                continue;
            };
            pos += 1;

            // Collect old section lines
            let mut old_lines: Vec<(char, String)> = Vec::new();
            while pos < lines.len() {
                let l = lines[pos];
                if l.starts_with("--- ") && NEW_RANGE_RE.is_match(l) {
                    break;
                }
                if l.len() >= 2 {
                    let prefix = l.chars().next().unwrap_or(' ');
                    let content = if l.len() > 2 { &l[2..] } else { "" };
                    old_lines.push((prefix, content.to_string()));
                } else if l.is_empty() {
                    old_lines.push((' ', String::new()));
                }
                pos += 1;
            }

            // Parse new section: --- start,end ----
            if pos >= lines.len() {
                break;
            }
            let new_line = lines[pos];
            let (new_start, new_end) = if let Some(caps) = NEW_RANGE_RE.captures(new_line) {
                let start: usize = caps[1].parse().unwrap_or(1);
                let end: usize = caps
                    .get(2)
                    .map_or(start, |m| m.as_str().parse().unwrap_or(start));
                (start, end)
            } else {
                continue;
            };
            pos += 1;

            // Collect new section lines
            let mut new_lines: Vec<(char, String)> = Vec::new();
            while pos < lines.len() {
                let l = lines[pos];
                if l.starts_with("***************")
                    || l.starts_with("diff ")
                    || l.starts_with("Index: ")
                {
                    break;
                }
                if l.starts_with("*** ") && !l.starts_with("***************") {
                    break;
                }
                if l.len() >= 2 {
                    let prefix = l.chars().next().unwrap_or(' ');
                    let content = if l.len() > 2 { &l[2..] } else { "" };
                    new_lines.push((prefix, content.to_string()));
                } else if l.is_empty() {
                    new_lines.push((' ', String::new()));
                }
                pos += 1;
            }

            // Convert to unified-style hunk
            let hunk = convert_context_to_hunk(
                old_start, old_end, new_start, new_end, &old_lines, &new_lines,
            );
            patch.hunks.push(hunk);
        } else {
            pos += 1;
        }
    }

    Ok((patch, pos))
}

/// Convert context diff sections to a unified-style hunk.
fn convert_context_to_hunk(
    old_start: usize,
    old_end: usize,
    new_start: usize,
    new_end: usize,
    old_lines: &[(char, String)],
    new_lines: &[(char, String)],
) -> Hunk {
    let old_count = if old_end >= old_start {
        old_end - old_start + 1
    } else {
        0
    };
    let new_count = if new_end >= new_start {
        new_end - new_start + 1
    } else {
        0
    };

    let mut hunk = Hunk::new(old_start, old_count, new_start, new_count);

    // Merge old and new sections
    // In context diff:
    //   ' ' = context
    //   '-' = delete in old
    //   '+' = add in new
    //   '!' = change (appears in both old and new)

    let mut old_idx = 0;
    let mut new_idx = 0;

    while old_idx < old_lines.len() || new_idx < new_lines.len() {
        // Handle old section
        if old_idx < old_lines.len() {
            let (prefix, content) = &old_lines[old_idx];
            match prefix {
                ' ' => {
                    hunk.lines.push(LineOp::Context(content.clone()));
                    old_idx += 1;
                    new_idx += 1; // Context lines appear in both sections
                }
                '-' => {
                    hunk.lines.push(LineOp::Delete(content.clone()));
                    old_idx += 1;
                }
                '!' => {
                    // Change: delete from old
                    hunk.lines.push(LineOp::Delete(content.clone()));
                    old_idx += 1;
                }
                _ => {
                    old_idx += 1;
                }
            }
        }
        // Handle new section for additions
        if new_idx < new_lines.len() {
            let (prefix, content) = &new_lines[new_idx];
            match prefix {
                '+' => {
                    hunk.lines.push(LineOp::Add(content.clone()));
                    new_idx += 1;
                }
                '!' => {
                    hunk.lines.push(LineOp::Add(content.clone()));
                    new_idx += 1;
                }
                ' ' => {
                    // Already handled in old section
                    new_idx += 1;
                }
                _ => {
                    new_idx += 1;
                }
            }
        }
    }

    hunk
}

/// Parse filename from a header line (remove timestamp if present).
fn parse_filename(s: &str) -> String {
    let s = s.trim();
    if let Some(tab_pos) = s.find('\t') {
        s[..tab_pos].to_string()
    } else if let Some(space_pos) = s.find("  ") {
        s[..space_pos].to_string()
    } else {
        s.to_string()
    }
}

/// Check if a line looks like a context diff header.
pub fn looks_like_context(lines: &[&str]) -> bool {
    let mut has_stars = false;
    let mut has_dashes = false;

    for line in lines.iter().take(20) {
        if line.starts_with("*** ") && !line.starts_with("***************") {
            has_stars = true;
        }
        if line.starts_with("--- ") && has_stars {
            has_dashes = true;
        }
        if line.starts_with("***************") {
            return true;
        }
    }

    has_stars && has_dashes
}
