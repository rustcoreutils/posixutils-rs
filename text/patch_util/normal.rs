//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Normal diff format parser.

use super::types::{DiffFormat, FilePatch, Hunk, LineOp, PatchError};
use regex::Regex;

/// Parse a normal diff from the given lines.
pub fn parse_normal(lines: &[&str], start: usize) -> Result<(FilePatch, usize), PatchError> {
    let mut patch = FilePatch::new(DiffFormat::Normal);
    let mut pos = start;

    // Normal diff doesn't have file headers, so we rely on Index: or command line
    while pos < lines.len() {
        let line = lines[pos];
        if let Some(rest) = line.strip_prefix("Index: ") {
            patch.index_path = Some(rest.trim().to_string());
            pos += 1;
        } else if line.starts_with("diff ") {
            // Skip diff command line
            pos += 1;
        } else {
            break;
        }
    }

    // Parse commands: NaN (add), NdN (delete), NcN (change)
    let cmd_re = Regex::new(r"^(\d+)(?:,(\d+))?([acd])(\d+)(?:,(\d+))?$").expect("invalid regex");

    while pos < lines.len() {
        let line = lines[pos];

        // Check for end of patch
        if line.starts_with("diff ") || line.starts_with("Index: ") {
            break;
        }

        if let Some(caps) = cmd_re.captures(line) {
            let old_start: usize = caps[1].parse().unwrap_or(1);
            let old_end: usize = caps
                .get(2)
                .map_or(old_start, |m| m.as_str().parse().unwrap_or(old_start));
            let cmd = &caps[3];
            let new_start: usize = caps[4].parse().unwrap_or(1);
            let new_end: usize = caps
                .get(5)
                .map_or(new_start, |m| m.as_str().parse().unwrap_or(new_start));

            pos += 1;

            let mut hunk = match cmd {
                "a" => {
                    // Add: old_start is line after which to add
                    // old_count = 0, new_count = new_end - new_start + 1
                    Hunk::new(old_start + 1, 0, new_start, new_end - new_start + 1)
                }
                "d" => {
                    // Delete: new_start is line before which content was deleted
                    // old_count = old_end - old_start + 1, new_count = 0
                    Hunk::new(old_start, old_end - old_start + 1, new_start + 1, 0)
                }
                "c" => {
                    // Change: replace old lines with new lines
                    Hunk::new(
                        old_start,
                        old_end - old_start + 1,
                        new_start,
                        new_end - new_start + 1,
                    )
                }
                _ => continue,
            };

            // Parse content lines
            while pos < lines.len() {
                let content_line = lines[pos];

                if let Some(rest) = content_line.strip_prefix("< ") {
                    // Delete line (from old file)
                    hunk.lines.push(LineOp::Delete(rest.to_string()));
                    pos += 1;
                } else if let Some(rest) = content_line.strip_prefix("> ") {
                    // Add line (to new file)
                    hunk.lines.push(LineOp::Add(rest.to_string()));
                    pos += 1;
                } else if content_line == "---" {
                    // Separator between delete and add in change
                    pos += 1;
                } else if content_line.starts_with("\\") {
                    // "\ No newline at end of file"
                    pos += 1;
                } else {
                    // End of this hunk
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

/// Check if a line looks like a normal diff command.
pub fn looks_like_normal(lines: &[&str]) -> bool {
    let cmd_re = Regex::new(r"^\d+(?:,\d+)?[acd]\d+(?:,\d+)?$").expect("invalid regex");

    for line in lines.iter().take(20) {
        if cmd_re.is_match(line) {
            return true;
        }
    }

    false
}
