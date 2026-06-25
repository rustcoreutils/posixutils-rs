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
    collections::HashSet,
    fs::{self, File, OpenOptions},
    io::{self, BufRead, BufWriter, Write},
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

    // Filename Determination step 5: prompt the user on the controlling
    // terminal for a filename. If no terminal is available or the response is
    // empty, give up and skip the patch (as before).
    if let Some(name) = prompt_for_filename() {
        let trimmed = name.trim();
        if !trimmed.is_empty() {
            return Ok(PathBuf::from(trimmed));
        }
    }

    Err(PatchError::NoTargetFile)
}

/// Prompt on the controlling terminal (/dev/tty) for a filename to patch.
/// Returns None if /dev/tty cannot be opened or nothing was read.
fn prompt_for_filename() -> Option<String> {
    let mut tty = OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/tty")
        .ok()?;
    write!(tty, "File to patch: ").ok()?;
    tty.flush().ok()?;
    let mut reader = io::BufReader::new(tty);
    let mut line = String::new();
    let n = reader.read_line(&mut line).ok()?;
    if n == 0 {
        return None;
    }
    Some(line)
}

/// Prompt a yes/no question on the controlling terminal (/dev/tty).
/// Returns Some(true) for an affirmative (or empty/default) answer, Some(false)
/// for a negative answer, or None if /dev/tty is unavailable.
pub fn prompt_yes_no(prompt: &str) -> Option<bool> {
    let mut tty = OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/tty")
        .ok()?;
    write!(tty, "{}", prompt).ok()?;
    tty.flush().ok()?;
    let mut reader = io::BufReader::new(tty);
    let mut line = String::new();
    let n = reader.read_line(&mut line).ok()?;
    if n == 0 {
        return None;
    }
    let answer = line.trim();
    // Default (empty) answer is affirmative, matching the "[y]" prompt.
    Some(answer.is_empty() || answer.starts_with('y') || answer.starts_with('Y'))
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
            // Strip n components. Per POSIX, a sequence of adjacent <slash>
            // characters counts as a single <slash> when counting components.
            let collapsed = collapse_slashes(path);
            let components: Vec<&str> = collapsed.split('/').collect();
            if n >= components.len() {
                components
                    .last()
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| collapsed.clone())
            } else {
                components[n..].join("/")
            }
        }
    }
}

/// Collapse runs of adjacent <slash> characters into a single <slash>.
fn collapse_slashes(path: &str) -> String {
    let mut out = String::with_capacity(path.len());
    let mut prev_slash = false;
    for c in path.chars() {
        if c == '/' {
            if !prev_slash {
                out.push(c);
            }
            prev_slash = true;
        } else {
            out.push(c);
            prev_slash = false;
        }
    }
    out
}

/// Read a file into a vector of lines.
///
/// Returns the lines and a flag indicating whether the file ended with a
/// trailing newline (false for an empty file). Optimized to read the entire
/// file at once and split, avoiding per-line allocations and system calls.
pub fn read_file_lines(path: &Path) -> io::Result<(Vec<String>, bool)> {
    let content = fs::read_to_string(path)?;
    let trailing_newline = content.ends_with('\n');
    // Count lines for pre-allocation
    let line_count = content.bytes().filter(|&b| b == b'\n').count() + 1;
    let mut lines = Vec::with_capacity(line_count);
    for line in content.lines() {
        lines.push(line.to_string());
    }
    Ok((lines, trailing_newline))
}

/// Back up a file with the .orig suffix, but only the first time it is seen in
/// this run (tracked via `backed_up`). This preserves the true original across
/// a multi-patch run rather than overwriting it with an intermediate version.
fn backup_once(path: &Path, backed_up: &mut HashSet<PathBuf>) -> io::Result<()> {
    if !path.exists() {
        return Ok(());
    }
    let key = path.to_path_buf();
    if backed_up.contains(&key) {
        return Ok(());
    }
    let backup_path = format!("{}.orig", path.display());
    fs::copy(path, &backup_path)?;
    backed_up.insert(key);
    Ok(())
}

/// Remove the target file for a deletion patch (new file is /dev/null),
/// honoring -b backup first. Used instead of writing an empty file.
pub fn delete_target(
    target: &Path,
    config: &PatchConfig,
    backed_up: &mut HashSet<PathBuf>,
) -> io::Result<()> {
    if config.backup {
        backup_once(target, backed_up)?;
    }
    if target.exists() {
        fs::remove_file(target)?;
    }
    Ok(())
}

/// Write content to the output file, handling backup if needed.
///
/// `no_trailing_newline` suppresses the final newline (the patched file's last
/// line had no newline). `backed_up` tracks which files have already been
/// backed up this run (so -b preserves the true original). `written_outputs`
/// tracks which -o output files have already been written, so successive
/// patched versions of the same -o file are concatenated rather than truncated.
#[allow(clippy::too_many_arguments)]
pub fn write_output(
    content: &[String],
    target: &Path,
    config: &PatchConfig,
    no_trailing_newline: bool,
    backed_up: &mut HashSet<PathBuf>,
    written_outputs: &mut HashSet<PathBuf>,
) -> io::Result<()> {
    // Determine output path
    let output_path = config.output_file.as_deref().unwrap_or(target);

    // Handle backup (-b option) once per file.
    if config.backup {
        if config.output_file.is_some() {
            backup_once(output_path, backed_up)?;
        } else {
            backup_once(target, backed_up)?;
        }
    }

    // Create parent directories if needed
    if let Some(parent) = output_path.parent() {
        if !parent.as_os_str().is_empty() && !parent.exists() {
            fs::create_dir_all(parent)?;
        }
    }

    // For -o output, concatenate successive patched versions of the same file:
    // truncate on first write, append thereafter.
    let key = output_path.to_path_buf();
    let append = config.output_file.is_some() && written_outputs.contains(&key);
    let file = if append {
        OpenOptions::new().append(true).open(output_path)?
    } else {
        File::create(output_path)?
    };
    written_outputs.insert(key);

    // Write content using BufWriter for better I/O performance
    let mut writer = BufWriter::new(file);
    let last = content.len().saturating_sub(1);
    for (i, line) in content.iter().enumerate() {
        if i == last && no_trailing_newline {
            write!(writer, "{}", line)?;
        } else {
            writeln!(writer, "{}", line)?;
        }
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
