//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! File operations for the patch utility.

use super::types::{DiffFormat, FilePatch, Hunk, LineOp, PatchConfig, PatchError};
use std::{
    fs::{self, File},
    io::{self, BufWriter, Write},
    path::{Path, PathBuf},
};

/// Determine the target file for a patch.
pub fn determine_target_file(
    patch: &FilePatch,
    config: &PatchConfig,
) -> Result<PathBuf, PatchError> {
    // If file operand was specified, use it
    if let Some(ref target) = config.target_file {
        return Ok(target.clone());
    }

    let strip = config.strip_count;

    // Order per POSIX spec:
    // 1. Try old_path (*** or --- in context/unified)
    // 2. Try new_path (--- or +++ in context/unified)
    // 3. Try index_path (Index: line)

    // For unified diff: --- is old, +++ is new
    // For context diff: *** is old, --- is new

    let candidates = [&patch.old_path, &patch.new_path, &patch.index_path];

    for candidate in candidates.iter().filter_map(|c| c.as_ref()) {
        if candidate == "/dev/null" {
            continue;
        }

        let stripped = strip_path(candidate, strip);
        let path = PathBuf::from(&stripped);

        if path.exists() {
            return Ok(path);
        }
    }

    // For new files, try the new_path directly
    if patch.is_new_file {
        if let Some(ref new_path) = patch.new_path {
            if new_path != "/dev/null" {
                let stripped = strip_path(new_path, strip);
                return Ok(PathBuf::from(stripped));
            }
        }
    }

    // Could not determine file - in interactive mode we'd prompt
    // For now, return error
    Err(PatchError::NoTargetFile)
}

/// Strip leading path components from a path.
fn strip_path(path: &str, strip_count: Option<usize>) -> String {
    match strip_count {
        None => {
            // Default: use basename only
            Path::new(path)
                .file_name()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_else(|| path.to_string())
        }
        Some(0) => {
            // Use full path
            path.to_string()
        }
        Some(n) => {
            // Strip n components
            let components: Vec<&str> = path.split('/').collect();
            if n >= components.len() {
                components.last().unwrap_or(&path).to_string()
            } else {
                components[n..].join("/")
            }
        }
    }
}

/// Read a file into a vector of lines.
/// Optimized to read the entire file at once and split, avoiding
/// per-line allocations and system calls.
pub fn read_file_lines(path: &Path) -> io::Result<Vec<String>> {
    let content = fs::read_to_string(path)?;
    // Count lines for pre-allocation
    let line_count = content.bytes().filter(|&b| b == b'\n').count() + 1;
    let mut lines = Vec::with_capacity(line_count);
    for line in content.lines() {
        lines.push(line.to_string());
    }
    Ok(lines)
}

/// Write content to the output file, handling backup if needed.
pub fn write_output(content: &[String], target: &Path, config: &PatchConfig) -> io::Result<()> {
    // Determine output path
    let output_path = config.output_file.as_deref().unwrap_or(target);

    // Handle backup (-b option)
    if config.backup {
        if config.output_file.is_some() {
            // When -o is used, backup the output file if it exists
            if output_path.exists() {
                let backup_path = format!("{}.orig", output_path.display());
                fs::copy(output_path, &backup_path)?;
            }
        } else {
            // Backup the target file
            if target.exists() {
                let backup_path = format!("{}.orig", target.display());
                fs::copy(target, &backup_path)?;
            }
        }
    }

    // Create parent directories if needed
    if let Some(parent) = output_path.parent() {
        if !parent.as_os_str().is_empty() && !parent.exists() {
            fs::create_dir_all(parent)?;
        }
    }

    // Write content using BufWriter for better I/O performance
    let file = File::create(output_path)?;
    let mut writer = BufWriter::new(file);
    for line in content {
        writeln!(writer, "{}", line)?;
    }
    writer.flush()?;

    Ok(())
}

/// Write rejected hunks to a reject file.
pub fn write_rejects(
    rejects: &[(usize, Hunk, String)],
    target: &Path,
    format: DiffFormat,
    config: &PatchConfig,
) -> io::Result<()> {
    if rejects.is_empty() {
        return Ok(());
    }

    // Determine reject file path
    let reject_path = config
        .reject_file
        .clone()
        .unwrap_or_else(|| PathBuf::from(format!("{}.rej", target.display())));

    let file = File::create(&reject_path)?;
    let mut writer = BufWriter::new(file);

    // Write rejects in context diff format per POSIX
    // (even if input was unified, rejects should be in context format)
    for (hunk_num, hunk, _reason) in rejects {
        write_hunk_as_context(&mut writer, hunk, *hunk_num, format)?;
    }
    writer.flush()?;

    Ok(())
}

/// Write a hunk in context diff format.
fn write_hunk_as_context<W: Write>(
    writer: &mut W,
    hunk: &Hunk,
    _hunk_num: usize,
    _format: DiffFormat,
) -> io::Result<()> {
    // Write separator
    writeln!(writer, "***************")?;

    // Write old section header
    let old_end = hunk.old_start + hunk.old_count.saturating_sub(1);
    if hunk.old_count == 0 {
        writeln!(writer, "*** {} ****", hunk.old_start)?;
    } else {
        writeln!(writer, "*** {},{} ****", hunk.old_start, old_end)?;
    }

    // Write old section lines
    for op in &hunk.lines {
        match op {
            LineOp::Context(s) => writeln!(writer, "  {}", s)?,
            LineOp::Delete(s) => writeln!(writer, "- {}", s)?,
            LineOp::Add(_) => {} // Skip adds in old section
        }
    }

    // Write new section header
    let new_end = hunk.new_start + hunk.new_count.saturating_sub(1);
    if hunk.new_count == 0 {
        writeln!(writer, "--- {} ----", hunk.new_start)?;
    } else {
        writeln!(writer, "--- {},{} ----", hunk.new_start, new_end)?;
    }

    // Write new section lines
    for op in &hunk.lines {
        match op {
            LineOp::Context(s) => writeln!(writer, "  {}", s)?,
            LineOp::Delete(_) => {} // Skip deletes in new section
            LineOp::Add(s) => writeln!(writer, "+ {}", s)?,
        }
    }

    Ok(())
}
