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
use super::safe_fs::{open_user_output, prune_empty_dirs, Name, Original, Place, Refusal};
use super::types::{BackupName, FilePatch, Hunk, LineOp, PatchConfig, PatchError, RejectFile};
use gettextrs::gettext;
use plib::io::{open_terminal_input, open_terminal_output};
use std::{
    collections::HashSet,
    fmt::{self, Write as _},
    io::{self, BufRead, BufWriter, Write},
    path::{Component, Path, PathBuf},
};

/// The file a patch section applies to, as found before patching: the
/// directory it is in, held open, and what it held.
pub struct Target {
    pub name: Name,
    /// None when a directory on the way does not exist yet.
    place: Option<Place>,
    /// None when there is no file.
    pub original: Option<Original>,
}

/// Find and read the file `name`. A file the patch names is reached without
/// following a link; whatever is found must be a regular file.
pub fn open_target(name: &Name) -> Result<Target, Refusal> {
    let place = Place::locate(name, false)?;
    let original = match &place {
        Some(place) => place.read_regular()?,
        None => None,
    };
    Ok(Target {
        name: name.clone(),
        place,
        original,
    })
}

/// Whether anything stands at `name`, reached without following a link. A
/// name that leads through a link does not exist, as GNU patch finds it.
fn exists(name: &Name) -> bool {
    matches!(Place::locate(name, false), Ok(Some(place)) if place.exists())
}

/// Determine the target file for a patch.
pub fn determine_target_file(patch: &FilePatch, config: &PatchConfig) -> Result<Name, PatchError> {
    // If file operand was specified, use it
    if let Some(ref target) = config.target_file {
        return Ok(Name::from_user(target.clone()));
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

        let Some(name) = safe_patch_path(candidate, strip) else {
            continue;
        };
        if exists(&name) {
            return Ok(name);
        }
    }

    // For new files, try the new_path directly. This is the one path that
    // returns a name without first checking that it exists, so it is also the
    // one that would happily create a file -- and a whole directory tree --
    // wherever the patch says.
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
                return Ok(Name::from_user(PathBuf::from(trimmed)));
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

/// The name a file name from the patch gives after stripping, or None (with a
/// warning) if it may not be written.
fn safe_patch_path(name: &str, strip: Option<usize>) -> Option<Name> {
    let path = bytes::to_path(&strip_path(name, strip));
    if is_safe_patch_path(&path) {
        Some(Name::from_patch(path))
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

/// Split a file's contents into lines.
///
/// Returns the lines and a flag indicating whether the file ended with a
/// trailing newline (false for an empty file).
pub fn file_lines(original: &Original) -> (Vec<String>, bool) {
    let content = bytes::decode(&original.bytes);
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
    (lines, trailing_newline)
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
/// .pc/NAME/); they are created. The backup is a new file renamed over its
/// name, with the original's owner and mode, so whatever stood at the name --
/// a link to anywhere -- is replaced, not written through.
fn backup_once(
    name: &Name,
    original: Option<&Original>,
    naming: &BackupName,
    backed_up: &mut HashSet<PathBuf>,
) -> io::Result<()> {
    let key = name.path().to_path_buf();
    if backed_up.contains(&key) {
        return Ok(());
    }
    let backup = name.backup(naming);
    let place = locate_creating(&backup)?;
    let bytes = original.map_or(&[][..], |o| &o.bytes);
    place.replace(original.map(|o| &o.meta), |w| w.write_all(bytes))?;
    backed_up.insert(key);
    Ok(())
}

/// Reach the directory `name` goes in, making any that are missing.
fn locate_creating(name: &Name) -> io::Result<Place> {
    Place::locate(name, true)
        .map_err(|r| r.into_io(name.path()))?
        .ok_or_else(|| io::Error::from(io::ErrorKind::NotFound))
}

/// Remove the target file for a deletion patch (new file is /dev/null),
/// honoring -b backup first. Used instead of writing an empty file. The file
/// is removed from the directory it was read in, and only if it is still the
/// file that was read; then the directories it leaves empty go too.
pub fn delete_target(
    target: &Target,
    config: &PatchConfig,
    backed_up: &mut HashSet<PathBuf>,
) -> io::Result<()> {
    if let Some(naming) = &config.backup {
        backup_once(&target.name, target.original.as_ref(), naming, backed_up)?;
    }
    if let (Some(place), Some(original)) = (&target.place, &target.original) {
        place.remove(original)?;
        prune_empty_dirs(&target.name);
    }
    Ok(())
}

/// Write the patched lines; `no_trailing_newline` drops the final newline.
fn write_lines(w: &mut dyn Write, content: &[String], no_trailing_newline: bool) -> io::Result<()> {
    let last = content.len().saturating_sub(1);
    for (i, line) in content.iter().enumerate() {
        w.write_all(&bytes::encode(line))?;
        if i != last || !no_trailing_newline {
            w.write_all(b"\n")?;
        }
    }
    Ok(())
}

/// Back up a -o output file the user named, if it exists.
fn backup_user_file(
    path: &Path,
    naming: &BackupName,
    backed_up: &mut HashSet<PathBuf>,
) -> io::Result<()> {
    let name = Name::from_user(path.to_path_buf());
    let original = match Place::locate(&name, false) {
        Ok(Some(place)) => place.read_regular().map_err(|r| r.into_io(path))?,
        Ok(None) => None,
        Err(r) => return Err(r.into_io(path)),
    };
    backup_once(&name, original.as_ref(), naming, backed_up)
}

/// Write the patched file to the -o output file, concatenating successive
/// patched versions of the same file: truncate on first write, append
/// thereafter.
fn write_user_output(
    content: &[String],
    path: &Path,
    no_trailing_newline: bool,
    written_outputs: &mut HashSet<PathBuf>,
) -> io::Result<()> {
    let append = written_outputs.contains(path);
    let file = open_user_output(path, append)?;
    written_outputs.insert(path.to_path_buf());
    let mut writer = BufWriter::new(file);
    write_lines(&mut writer, content, no_trailing_newline)?;
    writer.flush()
}

/// Write content to the output file, handling backup if needed.
///
/// `no_trailing_newline` suppresses the final newline (the patched file's last
/// line had no newline). `backed_up` tracks which files have already been
/// backed up this run (so -b preserves the true original). `written_outputs`
/// tracks which -o output files have already been written, so successive
/// patched versions of the same -o file are concatenated rather than truncated.
///
/// The patched file replaces the target: it is written to a new file in the
/// directory the target was read from, given the original's owner and mode,
/// and renamed over the target's name. Directories a new file needs are made
/// on the way, never through a link.
pub fn write_output(
    content: &[String],
    target: &Target,
    config: &PatchConfig,
    no_trailing_newline: bool,
    backed_up: &mut HashSet<PathBuf>,
    written_outputs: &mut HashSet<PathBuf>,
) -> io::Result<()> {
    if let Some(output) = &config.output_file {
        if let Some(naming) = &config.backup {
            backup_user_file(output, naming, backed_up)?;
        }
        return write_user_output(content, output, no_trailing_newline, written_outputs);
    }

    let original = target.original.as_ref();
    if let Some(naming) = &config.backup {
        backup_once(&target.name, original, naming, backed_up)?;
    }
    let made;
    let place = match &target.place {
        Some(place) => place,
        None => {
            made = locate_creating(&target.name)?;
            &made
        }
    };
    place.replace(original.map(|o| &o.meta), |w| {
        write_lines(w, content, no_trailing_newline)
    })
}

/// Write rejected hunks to a reject file.
///
/// POSIX: rejected hunks are *appended* to the reject file. With -r, or with
/// two patch sections naming the same file, truncating per section would
/// leave only the last one's rejects. A -r file is the user's own, opened as
/// named; the default FILE.rej is reached like FILE and, the first time, made
/// new over whatever stood at its name.
pub fn write_rejects(
    rejects: &[(usize, Hunk, String)],
    target: &Name,
    config: &PatchConfig,
    written_rejects: &mut HashSet<PathBuf>,
) -> io::Result<()> {
    if rejects.is_empty() {
        return Ok(());
    }
    let text = bytes::encode(&reject_text(rejects, target.path())?);
    match &config.reject_file {
        Some(RejectFile::Discard) => Ok(()),
        Some(RejectFile::Path(path)) => {
            let append = written_rejects.contains(path);
            let mut file = open_user_output(path, append)?;
            written_rejects.insert(path.clone());
            file.write_all(&text)
        }
        None => {
            let name = target.with_suffix(".rej");
            let place = locate_creating(&name)?;
            if written_rejects.contains(name.path()) {
                place.append(&text).map_err(|r| r.into_io(name.path()))?;
            } else {
                place.replace(None, |w| w.write_all(&text))?;
                written_rejects.insert(name.path().to_path_buf());
            }
            Ok(())
        }
    }
}

/// The reject file text for the hunks of one file.
fn reject_text(rejects: &[(usize, Hunk, String)], target: &Path) -> io::Result<String> {
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
    Ok(text)
}

/// Write a hunk in context diff format, as patch text.
fn write_hunk_as_context(writer: &mut String, hunk: &Hunk) -> fmt::Result {
    // Write separator
    writeln!(writer, "***************")?;

    // Write old section header. A zero-count side is normalized to the line
    // before which the change goes; a context diff spells it as the line after
    // which, so convert back.
    let old_end = hunk
        .old_start
        .saturating_add(hunk.old_count.saturating_sub(1));
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
    let new_end = hunk
        .new_start
        .saturating_add(hunk.new_count.saturating_sub(1));
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
