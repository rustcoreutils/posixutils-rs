//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Patch format detection and parsing.

use super::{
    context::{context_marker, parse_context},
    ed::{ed_marker, parse_ed},
    normal::{normal_marker, parse_normal},
    types::{DiffFormat, Patch, PatchConfig, PatchError},
    unified::{parse_unified, unified_marker},
};

/// Report git extended-header sections that carry no textual difference.
///
/// git records renames, mode changes and binary blobs with header lines and no
/// hunk. Neither POSIX nor GNU patch applies those, but a patch made only of
/// them is well-formed input, not garbage -- name what is being skipped so the
/// user can act on it. Returns whether any such section was found.
fn report_git_only_sections(lines: &[&str]) -> bool {
    let mut found = false;
    let mut i = 0;
    while i < lines.len() {
        if !lines[i].starts_with("diff --git ") {
            i += 1;
            continue;
        }
        let header = lines[i];
        let mut what: Option<String> = None;
        let mut j = i + 1;
        while j < lines.len() && !lines[j].starts_with("diff --git ") {
            let l = lines[j];
            if let Some(to) = l.strip_prefix("rename to ") {
                let from = lines[i + 1..j]
                    .iter()
                    .find_map(|p| p.strip_prefix("rename from "))
                    .unwrap_or("?");
                what = Some(format!("rename of {} to {}", from, to));
            } else if l.starts_with("GIT binary patch") {
                what = Some(String::from("binary difference"));
            } else if l.starts_with("copy to ") {
                what = Some(String::from("file copy"));
            } else if what.is_none() && l.starts_with("old mode ") {
                what = Some(String::from("file mode change"));
            }
            j += 1;
        }
        if let Some(what) = what {
            eprintln!("patch: ignoring {} ({})", what, header);
            found = true;
        }
        i = j;
    }
    found
}

/// Detect the diff format from the patch content.
///
/// Every format is identified by a marker line, and a marker can appear inside
/// another format's *content* -- an ed script's text block is unprefixed file
/// text, so it may contain something that reads as a normal-diff command. The
/// format whose marker comes *first* is the one that describes the file;
/// anything later is content belonging to it. Ties keep the historical
/// precedence: unified, context, normal, ed.
pub fn detect_format(lines: &[&str], config: &PatchConfig) -> Option<DiffFormat> {
    // Honor forced format options
    if config.force_unified {
        return Some(DiffFormat::Unified);
    }
    if config.force_context {
        return Some(DiffFormat::Context);
    }
    if config.force_normal {
        return Some(DiffFormat::Normal);
    }
    if config.force_ed {
        return Some(DiffFormat::EdScript);
    }

    [
        (unified_marker(lines), DiffFormat::Unified),
        (context_marker(lines), DiffFormat::Context),
        (normal_marker(lines), DiffFormat::Normal),
        (ed_marker(lines), DiffFormat::EdScript),
    ]
    .into_iter()
    .filter_map(|(at, format)| at.map(|at| (at, format)))
    .min_by_key(|(at, _)| *at)
    .map(|(_, format)| format)
}

/// Split text into lines on '\n' only, so a '\r' stays part of the line.
///
/// Unlike `str::lines`, this does not strip a carriage return: a '\r' belongs
/// to a line's content, both in a file being patched and in the patch's own
/// content lines. Structural lines -- hunk headers, ed commands, separators --
/// tolerate a trailing '\r' wherever they are matched.
pub fn split_lines(content: &str) -> Vec<&str> {
    if content.is_empty() {
        return Vec::new();
    }
    let mut lines: Vec<&str> = content.split('\n').collect();
    if content.ends_with('\n') {
        // The split leaves an empty piece after the final newline.
        lines.pop();
    }
    lines
}

/// Whether a line carries no text, allowing for a CRLF line ending.
pub fn is_blank(line: &str) -> bool {
    line.is_empty() || line == "\r"
}

/// Compute the byte length of the leading <blank> (space/tab) run of a line.
fn leading_blank_len(line: &str) -> usize {
    line.find(|c| c != ' ' && c != '\t').unwrap_or(line.len())
}

/// If all non-empty lines of the patch begin with the same leading sequence of
/// <blank> characters, return that common prefix length (in bytes) so it can be
/// removed before proceeding (POSIX). Truly empty lines are ignored when
/// computing the prefix (they would otherwise spuriously defeat the rule for
/// patches separated by blank lines).
fn common_blank_prefix_len(lines: &[&str]) -> usize {
    let mut prefix: Option<&str> = None;
    for &line in lines {
        if is_blank(line) {
            continue;
        }
        let blanks = &line[..leading_blank_len(line)];
        prefix = Some(match prefix {
            None => blanks,
            Some(p) => {
                let n = p
                    .bytes()
                    .zip(blanks.bytes())
                    .take_while(|(a, b)| a == b)
                    .count();
                &p[..n]
            }
        });
        if prefix == Some("") {
            return 0;
        }
    }
    prefix.map(|p| p.len()).unwrap_or(0)
}

/// Parse the patch content into a Patch structure.
pub fn parse_patch(content: &str, config: &PatchConfig) -> Result<Patch, PatchError> {
    let raw_lines = split_lines(content);

    if raw_lines.is_empty() {
        return Ok(Patch::default());
    }

    // POSIX: if all lines begin with the same leading <blank> sequence, remove
    // it before proceeding (e.g. a uniformly indented / email-quoted patch).
    let prefix_len = common_blank_prefix_len(&raw_lines);
    let lines: Vec<&str> = if prefix_len > 0 {
        raw_lines
            .iter()
            .map(|l| {
                if l.len() >= prefix_len {
                    &l[prefix_len..]
                } else {
                    l
                }
            })
            .collect()
    } else {
        raw_lines
    };

    let format = match detect_format(&lines, config) {
        Some(format) => format,
        None => {
            // A git patch section that changes no text (a rename, a mode
            // change, a binary blob) carries no hunk for any format to
            // recognize. Say what was skipped instead of failing the whole
            // patch as unreadable.
            if report_git_only_sections(&lines) {
                return Ok(Patch::default());
            }
            return Err(PatchError::Parse {
                line: 1,
                message: "could not determine diff format".to_string(),
            });
        }
    };

    let mut patch = Patch::default();
    let mut pos = 0;

    // Skip leading blank lines and comments
    while pos < lines.len() {
        let line = lines[pos];
        if line.is_empty() || line.starts_with('#') {
            pos += 1;
        } else {
            break;
        }
    }

    while pos < lines.len() {
        // Skip any blank lines or header comments between patches
        while pos < lines.len() {
            let line = lines[pos];
            if is_blank(line) {
                pos += 1;
            } else {
                break;
            }
        }

        if pos >= lines.len() {
            break;
        }

        // Detect format for this specific patch (might change within a file)
        let local_format = if config.force_unified {
            DiffFormat::Unified
        } else if config.force_context {
            DiffFormat::Context
        } else if config.force_normal {
            DiffFormat::Normal
        } else if config.force_ed {
            DiffFormat::EdScript
        } else {
            // Look at the current position; fall back to the whole-file
            // detection when nothing here identifies itself.
            detect_format(&lines[pos..], config).unwrap_or(format)
        };

        let (file_patch, new_pos) = match local_format {
            DiffFormat::Unified => parse_unified(&lines, pos)?,
            DiffFormat::Context => parse_context(&lines, pos)?,
            DiffFormat::Normal => parse_normal(&lines, pos)?,
            DiffFormat::EdScript => parse_ed(&lines, pos)?,
        };

        if new_pos == pos {
            // No progress made, skip this line
            pos += 1;
        } else {
            pos = new_pos;
            if !file_patch.hunks.is_empty()
                || file_patch.old_path.is_some()
                || file_patch.new_path.is_some()
            {
                patch.file_patches.push(file_patch);
            }
        }
    }

    Ok(patch)
}
