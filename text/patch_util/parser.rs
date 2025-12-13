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
    context::{looks_like_context, parse_context},
    ed::{looks_like_ed, parse_ed},
    normal::{looks_like_normal, parse_normal},
    types::{DiffFormat, Patch, PatchConfig, PatchError},
    unified::{looks_like_unified, parse_unified},
};

/// Detect the diff format from the patch content.
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

    // Auto-detect format
    if looks_like_unified(lines) {
        return Some(DiffFormat::Unified);
    }
    if looks_like_context(lines) {
        return Some(DiffFormat::Context);
    }
    if looks_like_normal(lines) {
        return Some(DiffFormat::Normal);
    }
    if looks_like_ed(lines) {
        return Some(DiffFormat::EdScript);
    }

    None
}

/// Parse the patch content into a Patch structure.
pub fn parse_patch(content: &str, config: &PatchConfig) -> Result<Patch, PatchError> {
    let lines: Vec<&str> = content.lines().collect();

    if lines.is_empty() {
        return Ok(Patch::default());
    }

    let format = detect_format(&lines, config).ok_or_else(|| PatchError::Parse {
        line: 1,
        message: "could not determine diff format".to_string(),
    })?;

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
            if line.is_empty() {
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
            // Look at current position to detect format
            let remaining = &lines[pos..];
            if looks_like_unified(remaining) {
                DiffFormat::Unified
            } else if looks_like_context(remaining) {
                DiffFormat::Context
            } else if looks_like_normal(remaining) {
                DiffFormat::Normal
            } else if looks_like_ed(remaining) {
                DiffFormat::EdScript
            } else {
                format // Use initial detected format
            }
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
