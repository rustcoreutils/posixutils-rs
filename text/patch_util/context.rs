//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Context diff format parser.

use super::header::parse_filename;
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
                let end: Option<usize> = caps.get(2).map(|m| m.as_str().parse().unwrap_or(start));
                (start, end)
            } else {
                pos += 1;
                continue;
            };
            pos += 1;

            // Collect old section lines, at most as many as the range
            // declares. The cap keeps text that follows the final section --
            // a mail signature, say -- from being read as part of the hunk.
            let old_declared = old_end.map_or(1, |e| declared_len(old_start, e));
            let mut old_no_newline = false;
            let mut old_lines: Vec<(char, String)> = Vec::new();
            while pos < lines.len() {
                let l = lines[pos];
                if l.starts_with("--- ") && NEW_RANGE_RE.is_match(l) {
                    break;
                }
                if l.starts_with('\\') {
                    // "\ No newline at end of file" for the old side. This is
                    // checked before the cap: the marker follows the section's
                    // last content line, so a cap applied first would leave it
                    // unconsumed and the new-section header unreachable.
                    old_no_newline = true;
                    pos += 1;
                    continue;
                }
                if old_lines.len() == old_declared {
                    break;
                }
                old_lines.push(split_section_line(l));
                pos += 1;
            }

            // Parse new section: --- start,end ----
            if pos >= lines.len() {
                break;
            }
            let new_line = lines[pos];
            let (new_start, new_end) = if let Some(caps) = NEW_RANGE_RE.captures(new_line) {
                let start: usize = caps[1].parse().unwrap_or(1);
                let end: Option<usize> = caps.get(2).map(|m| m.as_str().parse().unwrap_or(start));
                (start, end)
            } else {
                continue;
            };
            pos += 1;

            // Collect new section lines, capped the same way.
            let new_declared = new_end.map_or(1, |e| declared_len(new_start, e));
            let mut new_no_newline = false;
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
                if l.starts_with('\\') {
                    // "\ No newline at end of file" for the new side; checked
                    // before the cap, as in the old section above.
                    new_no_newline = true;
                    pos += 1;
                    continue;
                }
                if new_lines.len() == new_declared {
                    break;
                }
                new_lines.push(split_section_line(l));
                pos += 1;
            }

            // Convert to unified-style hunk
            let mut hunk = convert_context_to_hunk(
                old_start, old_end, new_start, new_end, &old_lines, &new_lines, pos,
            )?;
            hunk.old_no_newline = old_no_newline;
            hunk.new_no_newline = new_no_newline;
            patch.hunks.push(hunk);
        } else {
            pos += 1;
        }
    }

    Ok((patch, pos))
}

/// Number of lines a `start,end` section range declares.
fn declared_len(start: usize, end: usize) -> usize {
    if end >= start {
        end - start + 1
    } else {
        0
    }
}

/// Split a context-diff section line into its one-character change prefix and
/// its payload.
///
/// The prefix is separated from the payload by a single space. Mailers that
/// strip trailing whitespace turn a line holding an empty payload into a bare
/// `+`, `-`, `!` or `<space>`, so the separator is optional. Splitting by
/// `char` rather than by byte index keeps non-ASCII payloads intact.
fn split_section_line(l: &str) -> (char, String) {
    let mut chars = l.chars();
    let prefix = chars.next().unwrap_or(' ');
    let rest = chars.as_str();
    (prefix, rest.strip_prefix(' ').unwrap_or(rest).to_string())
}

/// Convert context diff sections to a unified-style hunk.
///
/// `old_end` and `new_end` are `None` when the range was written as a bare line
/// number rather than `start,end`.
fn convert_context_to_hunk(
    old_start: usize,
    old_end: Option<usize>,
    new_start: usize,
    new_end: Option<usize>,
    old_lines: &[(char, String)],
    new_lines: &[(char, String)],
    line: usize,
) -> Result<Hunk, PatchError> {
    // A side that contributes nothing is written as a bare line number
    // ("*** 5 ****" for an insertion after line 5). A bare number is also how a
    // one-line range is written, so the body settles which it is. `Hunk` wants
    // the line before which to insert, so add one for the empty case.
    let (old_start, old_count) = match old_end {
        Some(end) => (old_start, declared_len(old_start, end)),
        None if old_lines.is_empty() => (old_start + 1, 0),
        None => (old_start, old_lines.len()),
    };
    let (new_start, new_count) = match new_end {
        Some(end) => (new_start, declared_len(new_start, end)),
        None if new_lines.is_empty() => (new_start + 1, 0),
        None => (new_start, new_lines.len()),
    };

    // diff omits a section body entirely when that side holds only context, so
    // an empty body next to a non-zero count is not an empty side -- it is the
    // other side's context lines. Recover them, or every context line in the
    // hunk is lost and there is nothing left to match the file against.
    let recovered: Vec<(char, String)>;
    let old_lines = if old_lines.is_empty() && old_count > 0 {
        recovered = context_only(new_lines);
        &recovered
    } else {
        old_lines
    };
    let recovered_new: Vec<(char, String)>;
    let new_lines = if new_lines.is_empty() && new_count > 0 {
        recovered_new = context_only(old_lines);
        &recovered_new
    } else {
        new_lines
    };

    let mut hunk = Hunk::new(old_start, old_count, new_start, new_count);

    // Merge the two sections. A context diff writes them separately:
    //   ' ' = context, present in both sections
    //   '-' = removed, old section only
    //   '+' = added, new section only
    //   '!' = changed, appearing in both sections as separate runs
    // The context lines are the alignment: they correspond one-to-one, in
    // order, and every other line sits in a gap between them.
    let mut old_idx = 0;
    let mut new_idx = 0;

    while old_idx < old_lines.len() || new_idx < new_lines.len() {
        let progress = (old_idx, new_idx);

        // Context, consumed from both sections together.
        while old_idx < old_lines.len()
            && new_idx < new_lines.len()
            && old_lines[old_idx].0 == ' '
            && new_lines[new_idx].0 == ' '
        {
            hunk.lines
                .push(LineOp::Context(old_lines[old_idx].1.clone()));
            old_idx += 1;
            new_idx += 1;
        }

        // Removals, then the two halves of a change, then additions -- the
        // order a unified hunk would list them in.
        while let Some((_, content)) = take_run(old_lines, &mut old_idx, '-') {
            hunk.lines.push(LineOp::Delete(content));
        }
        while let Some((_, content)) = take_run(old_lines, &mut old_idx, '!') {
            hunk.lines.push(LineOp::Delete(content));
        }
        while let Some((_, content)) = take_run(new_lines, &mut new_idx, '!') {
            hunk.lines.push(LineOp::Add(content));
        }
        while let Some((_, content)) = take_run(new_lines, &mut new_idx, '+') {
            hunk.lines.push(LineOp::Add(content));
        }

        if (old_idx, new_idx) == progress {
            // Neither section could be advanced: the two disagree about where
            // their context lines are.
            let stuck = old_lines
                .get(old_idx)
                .or_else(|| new_lines.get(new_idx))
                .map(|(p, _)| *p)
                .unwrap_or(' ');
            return Err(PatchError::Parse {
                line,
                message: format!("cannot align context sections at '{}'", stuck),
            });
        }
    }

    Ok(hunk)
}

/// The context lines of a section body, as a body in their own right.
fn context_only(lines: &[(char, String)]) -> Vec<(char, String)> {
    lines.iter().filter(|(p, _)| *p == ' ').cloned().collect()
}

/// Consume the next line of `lines` if it carries `prefix`, advancing `idx`.
fn take_run(lines: &[(char, String)], idx: &mut usize, prefix: char) -> Option<(char, String)> {
    let (p, content) = lines.get(*idx)?;
    if *p != prefix {
        return None;
    }
    *idx += 1;
    Some((*p, content.clone()))
}

/// Index of the first line that identifies this as a context diff.
/// See [`super::unified::unified_marker`] for why this reports a position.
pub fn context_marker(lines: &[&str]) -> Option<usize> {
    for (i, line) in lines.iter().enumerate() {
        if line.starts_with("***************") {
            return Some(i);
        }
        // Require the two file headers to be adjacent, as diff writes them, so
        // a patch whose content contains a "--- " line is not misread.
        if line.starts_with("*** ") && lines.get(i + 1).is_some_and(|n| n.starts_with("--- ")) {
            return Some(i);
        }
    }

    None
}
