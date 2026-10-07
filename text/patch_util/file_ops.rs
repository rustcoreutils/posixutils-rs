//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! File operations for the patch utility.

use super::bytes;
use super::types::{BackupName, FilePatch, Hunk, LineOp, PatchConfig, PatchError, RejectFile};
use gettextrs::gettext;
use plib::io::{open_terminal_input, open_terminal_output};
use std::{
    collections::HashSet,
    fmt::{self, Write as _},
    fs::{self, File, OpenOptions},
    io::{self, BufRead, BufWriter, Write},
    path::{Component, Path, PathBuf},
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

        let Some(path) = safe_patch_path(candidate, strip) else {
            continue;
        };
        if path.exists() {
            return Ok(path);
        }
    }

    // For new files, try the new_path directly. This is the one path that
    // returns a name without first checking that it exists, so it is also the
    // one that would happily create a file -- and, via write_output's
    // create_dir_all, a whole directory tree -- wherever the patch says.
    if patch.creates_file() {
        if let Some(ref new_path) = patch.new_path {
            if new_path != "/dev/null" {
                return safe_patch_path(new_path, strip).ok_or(PatchError::NoTargetFile);
            }
        }
    }

    // Filename Determination step 5: prompt the user on the controlling
    // terminal for a filename. -f and -t mean "do not ask any questions", and
    // if no terminal is available or the response is empty, give up and skip
    // the patch.
    if !config.force && !config.batch {
        if let Some(name) = prompt_for_filename() {
            let trimmed = name.trim();
            if !trimmed.is_empty() {
                return Ok(PathBuf::from(trimmed));
            }
        }
    }

    Err(PatchError::NoTargetFile)
}

/// Whether a file name taken from the patch may be written.
///
/// A patch is untrusted input. A name that is absolute, or that walks upward
/// through a `..` component, reaches outside the directory the user chose to
/// patch in; applying it would let a downloaded patch write anywhere the user
/// can. Refuse those. A name the user supplied -- the `file` operand or `-o` --
/// is their own instruction and is not subject to this.
///
/// "Absolute" is any name with a root or a prefix. On Unix that is exactly
/// `is_absolute`; on Windows `/etc/passwd` (rooted on the current drive) and
/// `C:file` (relative to that drive's own current directory) are not
/// `is_absolute`, yet reach outside the directory just the same.
fn is_safe_patch_path(path: &Path) -> bool {
    !path.components().any(|c| {
        matches!(
            c,
            Component::Prefix(_) | Component::RootDir | Component::ParentDir
        )
    })
}

/// The path a file name from the patch names after stripping, or None (with a
/// warning) if it may not be written.
fn safe_patch_path(name: &str, strip: Option<usize>) -> Option<PathBuf> {
    let path = bytes::to_path(&strip_path(name, strip));
    if is_safe_patch_path(&path) {
        Some(path)
    } else {
        warn_dangerous_name(&path);
        None
    }
}

/// Report a refused file name, in the same terms GNU patch uses.
fn warn_dangerous_name(name: &Path) {
    eprintln!(
        "patch: {}: {}",
        gettext("ignoring potentially dangerous file name"),
        name.display()
    );
}

/// Write `prompt` to the controlling terminal (/dev/tty; the console on
/// Windows) and read one line of answer from it. Returns None if the terminal
/// cannot be opened or nothing was read.
fn ask_terminal(prompt: &str) -> Option<String> {
    let mut tty = open_terminal_output().ok()?;
    write!(tty, "{}", prompt).ok()?;
    tty.flush().ok()?;
    let mut reader = io::BufReader::new(open_terminal_input().ok()?);
    let mut line = String::new();
    let n = reader.read_line(&mut line).ok()?;
    if n == 0 {
        return None;
    }
    Some(line)
}

/// Prompt on the controlling terminal for a filename to patch.
/// Returns None if the terminal cannot be opened or nothing was read.
fn prompt_for_filename() -> Option<String> {
    ask_terminal("File to patch: ")
}

/// Prompt a yes/no question on the controlling terminal.
/// Returns Some(true) for an affirmative (or empty/default) answer, Some(false)
/// for a negative answer, or None if the terminal is unavailable.
pub fn prompt_yes_no(prompt: &str) -> Option<bool> {
    let line = ask_terminal(prompt)?;
    let answer = line.trim();
    // Default (empty) answer is affirmative, matching the "[y]" prompt; a
    // non-empty answer is matched against the locale's YESEXPR.
    Some(answer.is_empty() || plib::locale::is_affirmative(answer))
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
    let content = bytes::decode(&fs::read(path)?);
    let trailing_newline = content.ends_with('\n');
    // Keeping any '\r' as part of the line makes the round trip through
    // write_output lossless for a CRLF file, and makes a patch written against
    // LF text simply fail to match one -- which is what should happen, rather
    // than the patch quietly rewriting every line ending in the file as a side
    // effect of changing one line.
    let lines: Vec<String> = super::parser::split_lines(&content)
        .into_iter()
        .map(str::to_string)
        .collect();
    Ok((lines, trailing_newline))
}

/// Back up a file under the name `naming` gives it, but only the first time it
/// is seen in this run (tracked via `backed_up`). This preserves the true
/// original across a multi-patch run rather than overwriting it with an
/// intermediate version.
///
/// A file the patch is about to create has no original, so its backup is an
/// empty file, as GNU patch makes one. That placeholder is what dpkg-source
/// (and quilt) read as "this file did not exist": restoring a patch deletes
/// any file whose backup is empty, and a 1.0 source package's unpack removes
/// FILE.dpkg-orig for every file its diff touches, failing if one is missing.
/// The backup name may lead into directories that do not exist yet (-B
/// .pc/NAME/); they are created.
fn backup_once(
    path: &Path,
    naming: &BackupName,
    backed_up: &mut HashSet<PathBuf>,
) -> io::Result<()> {
    let key = path.to_path_buf();
    if backed_up.contains(&key) {
        return Ok(());
    }
    let backup_path = naming.for_file(path);
    if let Some(parent) = backup_path.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent)?;
        }
    }
    if path.exists() {
        fs::copy(path, &backup_path)?;
    } else {
        File::create(&backup_path)?;
    }
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
    if let Some(naming) = &config.backup {
        backup_once(target, naming, backed_up)?;
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
    if let Some(naming) = &config.backup {
        backup_once(output_path, naming, backed_up)?;
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
        writer.write_all(&bytes::encode(line))?;
        if i != last || !no_trailing_newline {
            writer.write_all(b"\n")?;
        }
    }
    writer.flush()?;

    Ok(())
}

/// Write rejected hunks to a reject file.
pub fn write_rejects(
    rejects: &[(usize, Hunk, String)],
    target: &Path,
    config: &PatchConfig,
    written_rejects: &mut HashSet<PathBuf>,
) -> io::Result<()> {
    if rejects.is_empty() {
        return Ok(());
    }

    // Determine reject file path
    let reject_path = match &config.reject_file {
        Some(RejectFile::Discard) => return Ok(()),
        Some(RejectFile::Path(path)) => path.clone(),
        None => bytes::with_suffix(target, ".rej"),
    };

    // POSIX: rejected hunks are *appended* to the reject file. With -r, or with
    // two patch sections naming the same file, truncating per section would
    // leave only the last one's rejects.
    let file = if written_rejects.contains(&reject_path) {
        OpenOptions::new().append(true).open(&reject_path)?
    } else {
        File::create(&reject_path)?
    };
    written_rejects.insert(reject_path);

    // Name the file each group of rejects belongs to, so an aggregated reject
    // file stays attributable. The header is context-style to match the hunks
    // below it: a unified-style "--- "/"+++ " pair would make the reject file
    // read as a unified diff that then contains no hunks at all.
    let name = bytes::from_path(target);
    let mut text = format!("*** {}\n--- {}\n", name, name);

    // Write rejects in context diff format per POSIX
    // (even if input was unified, rejects should be in context format)
    for (_hunk_num, hunk, _reason) in rejects {
        write_hunk_as_context(&mut text, hunk).map_err(io::Error::other)?;
    }
    let mut writer = BufWriter::new(file);
    writer.write_all(&bytes::encode(&text))?;
    writer.flush()?;

    Ok(())
}

/// Write a hunk in context diff format, as patch text.
fn write_hunk_as_context(writer: &mut String, hunk: &Hunk) -> fmt::Result {
    // Write separator
    writeln!(writer, "***************")?;

    // Write old section header. A zero-count side is normalized to the line
    // before which the change goes; a context diff spells it as the line after
    // which, so convert back.
    let old_end = hunk.old_start + hunk.old_count.saturating_sub(1);
    if hunk.old_count == 0 {
        writeln!(writer, "*** {} ****", hunk.old_start.saturating_sub(1))?;
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
        writeln!(writer, "--- {} ----", hunk.new_start.saturating_sub(1))?;
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
