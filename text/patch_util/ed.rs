//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Ed script format parser.

use super::types::{DiffFormat, FilePatch, Hunk, LineOp, PatchError};
use regex::Regex;
use std::sync::LazyLock;

/// Pre-compiled regex for ed commands to avoid recompilation on each parse.
static CMD_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(\d+)(?:,(\d+))?([acd])$").expect("invalid regex"));

/// Pre-compiled regex for detecting ed commands.
static DETECT_CMD_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^\d+(?:,\d+)?[acd]$").expect("invalid regex"));

/// Parse an ed script from the given lines.
pub fn parse_ed(lines: &[&str], start: usize) -> Result<(FilePatch, usize), PatchError> {
    let mut patch = FilePatch::new(DiffFormat::EdScript);
    let mut pos = start;

    // Ed script may have Index: line
    while pos < lines.len() {
        let line = lines[pos];
        if let Some(rest) = line.strip_prefix("Index: ") {
            patch.index_path = Some(rest.trim().to_string());
            pos += 1;
        } else if line.starts_with("diff ") {
            pos += 1;
        } else {
            break;
        }
    }

    // Parse ed commands using pre-compiled static regex: Na, Nd, Nc, N,Ma, N,Md, N,Mc
    while pos < lines.len() {
        let line = lines[pos];

        // Check for end of patch
        if line.starts_with("diff ") || line.starts_with("Index: ") {
            break;
        }

        if let Some(caps) = CMD_RE.captures(line) {
            let start_line: usize = caps[1].parse().unwrap_or(1);
            let end_line: usize = caps
                .get(2)
                .map_or(start_line, |m| m.as_str().parse().unwrap_or(start_line));
            let cmd = &caps[3];

            // A range whose end precedes its start is malformed; the count
            // arithmetic below would underflow.
            if end_line < start_line {
                return Err(PatchError::Parse {
                    line: pos + 1,
                    message: format!("malformed range in command: {}", line),
                });
            }

            pos += 1;

            let hunk = match cmd {
                "a" => {
                    // Append after line start_line - collect text until "."
                    let mut text_lines: Vec<String> = Vec::new();
                    while pos < lines.len() {
                        let text_line = lines[pos];
                        if text_line == "." {
                            pos += 1;
                            break;
                        }
                        text_lines.push(text_line.to_string());
                        pos += 1;
                    }
                    let mut h = Hunk::new(start_line + 1, 0, start_line + 1, text_lines.len());
                    for line in text_lines {
                        h.lines.push(LineOp::Add(line));
                    }
                    h
                }
                "d" => {
                    // Delete lines start_line to end_line - no text block
                    // An ed script does not record the text it removes; the
                    // applier takes that from old_count and the file itself.
                    let count = end_line - start_line + 1;
                    Hunk::new(start_line, count, start_line, 0)
                }
                "c" => {
                    // Change lines start_line to end_line - collect text until "."
                    let mut text_lines: Vec<String> = Vec::new();
                    while pos < lines.len() {
                        let text_line = lines[pos];
                        if text_line == "." {
                            pos += 1;
                            break;
                        }
                        text_lines.push(text_line.to_string());
                        pos += 1;
                    }
                    let old_count = end_line - start_line + 1;
                    let mut h = Hunk::new(start_line, old_count, start_line, text_lines.len());
                    // Only the replacement text is recorded; see the 'd' arm.
                    for line in text_lines {
                        h.lines.push(LineOp::Add(line));
                    }
                    h
                }
                _ => continue,
            };

            patch.hunks.push(hunk);
        } else {
            pos += 1;
        }
    }

    // Ed scripts are written with commands from higher to lower line numbers
    // so that earlier commands don't shift line numbers for later commands.
    // We must apply them in the order they appear (no reversal needed).

    Ok((patch, pos))
}

/// Check if a line looks like an ed script command.
pub fn looks_like_ed(lines: &[&str]) -> bool {
    for line in lines.iter().take(20) {
        // Skip Index: and diff lines
        if line.starts_with("Index: ") || line.starts_with("diff ") {
            continue;
        }
        if DETECT_CMD_RE.is_match(line) {
            return true;
        }
        // If we hit content that's not an ed command, it's probably not ed
        if !line.is_empty() && !line.starts_with('.') {
            return false;
        }
    }

    false
}
