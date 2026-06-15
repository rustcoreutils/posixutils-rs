//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Read mode implementation - extract archive contents

use crate::archive::{ArchiveEntry, ArchiveFormat, ArchiveReader, EntryType, ExtractedLinks};
use crate::error::{PaxError, PaxResult};
use crate::formats::{CpioReader, PaxReader, UstarReader};
use crate::interactive::{InteractivePrompter, RenameResult};
use crate::pattern::{find_matching_pattern, Pattern};
use crate::subst::{apply_substitutions, SubstResult, Substitution};
use std::collections::HashSet;
use std::fs::{self, File, Permissions};
use std::io::{Read, Write};
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

/// Options for read/extract mode
pub struct ReadOptions {
    /// Patterns to match
    pub patterns: Vec<Pattern>,
    /// Match all except patterns
    pub exclude: bool,
    /// Don't overwrite existing files
    pub no_clobber: bool,
    /// Verbose output
    pub verbose: bool,
    /// Preserve permissions
    pub preserve_perms: bool,
    /// Preserve modification time
    pub preserve_mtime: bool,
    /// Preserve access time
    pub preserve_atime: bool,
    /// Preserve owner (requires privileges)
    pub preserve_owner: bool,
    /// Interactive rename mode
    pub interactive: bool,
    /// Update mode - only extract if archive member is newer
    pub update: bool,
    /// Path substitutions (-s option)
    pub substitutions: Vec<Substitution>,
    /// Select only first archive member matching each pattern (-n)
    pub first_match: bool,
    /// Process file-creation mask, applied to the mode of extracted files when
    /// the mode is not explicitly preserved (no `-p p`/`-p e`).
    pub umask: u32,
    /// `-o` extended-header options (delete=/keyword:=value) applied on extract.
    pub format_options: crate::options::FormatOptions,
}

impl Default for ReadOptions {
    fn default() -> Self {
        ReadOptions {
            patterns: Vec::new(),
            exclude: false,
            no_clobber: false,
            verbose: false,
            preserve_perms: true,
            preserve_mtime: true,
            preserve_atime: true,
            preserve_owner: false,
            interactive: false,
            update: false,
            substitutions: Vec::new(),
            first_match: false,
            umask: 0,
            format_options: crate::options::FormatOptions::default(),
        }
    }
}

/// Extract archive contents
pub fn extract_archive<R: Read>(
    reader: R,
    format: ArchiveFormat,
    options: &ReadOptions,
) -> PaxResult<()> {
    match format {
        ArchiveFormat::Ustar => {
            let mut archive = UstarReader::new(reader);
            extract_entries(&mut archive, options)
        }
        ArchiveFormat::Cpio => {
            let mut archive = CpioReader::new(reader);
            extract_entries(&mut archive, options)
        }
        ArchiveFormat::Pax => {
            let mut archive = PaxReader::new(reader).with_options(options.format_options.clone());
            extract_entries(&mut archive, options)
        }
    }
}

/// Extract archive contents from an ArchiveReader (for multi-volume support)
pub fn extract_archive_from_reader<R: ArchiveReader>(
    archive: &mut R,
    options: &ReadOptions,
) -> PaxResult<()> {
    extract_entries(archive, options)
}

/// Extract entries from any archive reader
fn extract_entries<R: ArchiveReader>(archive: &mut R, options: &ReadOptions) -> PaxResult<()> {
    let mut extracted_links = ExtractedLinks::new();

    // Track which patterns have been matched (for -n first_match option)
    let mut matched_patterns: HashSet<usize> = HashSet::new();

    // Create interactive prompter if needed
    let mut prompter = if options.interactive {
        Some(InteractivePrompter::new()?)
    } else {
        None
    };

    while let Some(mut entry) = archive.read_entry()? {
        if let Some(should_output) = should_extract(&entry, options, &mut matched_patterns) {
            if !should_output {
                // Entry matched a pattern that's already been matched (first_match mode)
                archive.skip_data()?;
                continue;
            }
            // Apply `-o keyword:=value` overrides before substitutions/rename so a
            // forced path/uid/gid/etc. takes effect on the extracted file.
            apply_keyword_overrides(&mut entry, &options.format_options);
            // Apply substitutions first (per POSIX: -s applies before -i)
            if !options.substitutions.is_empty() {
                let path_str = entry.path.to_string_lossy();
                match apply_substitutions(&options.substitutions, &path_str) {
                    SubstResult::Unchanged => {
                        // Keep original path
                    }
                    SubstResult::Changed(new_path) => {
                        entry.path = PathBuf::from(new_path);
                    }
                    SubstResult::Empty => {
                        // Skip this entry
                        archive.skip_data()?;
                        continue;
                    }
                }
            }

            // Handle interactive rename if enabled
            if let Some(ref mut p) = prompter {
                let path_str = entry.path.to_string_lossy();
                match p.prompt(&path_str)? {
                    RenameResult::Skip => {
                        archive.skip_data()?;
                        continue;
                    }
                    RenameResult::UseOriginal => {
                        // Keep the original path
                    }
                    RenameResult::Rename(new_path) => {
                        entry.path = new_path;
                    }
                }
            }
            // Per POSIX CONSEQUENCES OF ERRORS: diagnose a per-file failure and
            // set a non-zero exit, but continue with the next member. Skip any
            // unconsumed data of the failed entry to realign the reader.
            if let Err(e) = extract_entry(archive, &entry, options, &mut extracted_links) {
                crate::error::report_error(entry.path.display(), e);
                let _ = archive.skip_data();
            }
        } else {
            archive.skip_data()?;
        }
    }

    // Diagnose any pattern operand that matched no archive member (non-exclude
    // mode) and set a non-zero exit status (POSIX DESCRIPTION).
    if !options.exclude {
        for (idx, pat) in options.patterns.iter().enumerate() {
            if !matched_patterns.contains(&idx) {
                crate::error::report_error(&pat.source, "not found");
            }
        }
    }

    Ok(())
}

/// Apply `-o keyword:=value` per-file overrides to an entry on extract.
///
/// The `:=` form forces the value regardless of what the archive carried, so it
/// is applied after the reader has merged any extended-header records. Only the
/// standard keywords that map onto an entry field are handled; unknown keywords
/// have no extraction effect. `delete=` is handled in the pax reader (so the
/// ustar value remains), not here.
fn apply_keyword_overrides(entry: &mut ArchiveEntry, opts: &crate::options::FormatOptions) {
    for (keyword, value) in opts.per_file_options() {
        match keyword.as_str() {
            "uid" => {
                if let Ok(v) = value.parse() {
                    entry.uid = v;
                }
            }
            "gid" => {
                if let Ok(v) = value.parse() {
                    entry.gid = v;
                }
            }
            "uname" => entry.uname = Some(value.clone()),
            "gname" => entry.gname = Some(value.clone()),
            "path" => entry.path = PathBuf::from(value),
            "linkpath" => entry.link_target = Some(PathBuf::from(value)),
            "size" => {
                if let Ok(v) = value.parse() {
                    entry.size = v;
                }
            }
            "mtime" => {
                if let Ok(t) = value.parse::<f64>() {
                    entry.mtime = t as u64;
                    entry.mtime_nsec = (t.fract() * 1_000_000_000.0) as u32;
                }
            }
            "atime" => {
                if let Ok(t) = value.parse::<f64>() {
                    entry.atime = Some(t as u64);
                    entry.atime_nsec = (t.fract() * 1_000_000_000.0) as u32;
                }
            }
            _ => {}
        }
    }
}

/// Check if entry should be extracted
/// Returns:
/// - None: entry should not be extracted (doesn't match patterns or excluded)
/// - Some(true): entry should be extracted
/// - Some(false): entry matches but pattern already matched (first_match mode)
fn should_extract(
    entry: &ArchiveEntry,
    options: &ReadOptions,
    matched_patterns: &mut HashSet<usize>,
) -> Option<bool> {
    let path = entry.path.to_string_lossy();

    // Try matching against both the full path and the path with "./" prefix stripped
    let path_stripped = path.strip_prefix("./").unwrap_or(&path);

    if options.patterns.is_empty() {
        // No patterns means match all
        if options.exclude {
            return None; // Exclude all
        }
        return Some(true); // Match all
    }

    // Find which pattern matches (if any)
    let matching_pattern = find_matching_pattern(&options.patterns, &path)
        .or_else(|| find_matching_pattern(&options.patterns, path_stripped));

    match matching_pattern {
        Some(pattern_idx) => {
            if options.exclude {
                // Entry matched a pattern, so exclude it
                None
            } else if options.first_match && matched_patterns.contains(&pattern_idx) {
                // first_match (-n): this pattern has already selected a member
                Some(false)
            } else {
                // Record the match (used for the unmatched-pattern sweep and for
                // -n first-match tracking) and select the entry.
                matched_patterns.insert(pattern_idx);
                Some(true)
            }
        }
        None => {
            // No pattern matched
            if options.exclude {
                Some(true) // Exclude mode: extract entries that don't match
            } else {
                None // Normal mode: skip entries that don't match
            }
        }
    }
}

/// Check if archive member is newer than existing file (for -u option)
fn is_archive_newer(entry: &ArchiveEntry, path: &Path) -> bool {
    // If file doesn't exist, always extract
    if !path.exists() {
        return true;
    }

    // Get the modification time of existing file
    let existing_mtime = match fs::metadata(path) {
        Ok(meta) => {
            #[cfg(unix)]
            {
                use std::os::unix::fs::MetadataExt;
                meta.mtime() as u64
            }
            #[cfg(not(unix))]
            {
                meta.modified()
                    .ok()
                    .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
                    .map(|d| d.as_secs())
                    .unwrap_or(0)
            }
        }
        Err(_) => return true, // If we can't stat, assume we should extract
    };

    // Extract if archive entry is newer than existing file
    entry.mtime > existing_mtime
}

/// Extract a single entry
fn extract_entry<R: ArchiveReader>(
    archive: &mut R,
    entry: &ArchiveEntry,
    options: &ReadOptions,
    extracted_links: &mut ExtractedLinks,
) -> PaxResult<()> {
    let path = sanitize_path(&entry.path)?;

    // Skip current directory entries
    if path.as_os_str() == "." {
        archive.skip_data()?;
        return Ok(());
    }

    // Check no_clobber
    if options.no_clobber && path.exists() {
        archive.skip_data()?;
        return Ok(());
    }

    // Check update mode (-u): only extract if archive member is newer
    if options.update && !is_archive_newer(entry, &path) {
        archive.skip_data()?;
        return Ok(());
    }

    if options.verbose {
        eprintln!("{}", path.display());
    }

    // Create parent directories
    create_parent_dirs(&path)?;

    match entry.entry_type {
        EntryType::Directory => {
            extract_directory(&path, entry, options)?;
            archive.skip_data()?;
        }
        EntryType::Symlink => {
            extract_symlink(&path, entry)?;
            archive.skip_data()?;
        }
        EntryType::Hardlink => {
            extract_hardlink(&path, entry, extracted_links)?;
            archive.skip_data()?;
        }
        EntryType::Regular => {
            extract_file(archive, &path, entry, options)?;
            archive.skip_data()?; // Skip padding to block boundary
            extracted_links.record(entry, &path);
        }
        EntryType::BlockDevice | EntryType::CharDevice => {
            extract_device(&path, entry, options)?;
            archive.skip_data()?;
        }
        EntryType::Fifo => {
            extract_fifo(&path, entry, options)?;
            archive.skip_data()?;
        }
        EntryType::Socket => {
            // Sockets cannot be extracted from archives
            if options.verbose {
                eprintln!("pax: skipping socket: {}", path.display());
            }
            archive.skip_data()?;
        }
    }

    Ok(())
}

/// Sanitize path to prevent directory traversal
fn sanitize_path(path: &Path) -> PaxResult<PathBuf> {
    let mut result = PathBuf::new();

    for component in path.components() {
        match component {
            std::path::Component::Normal(c) => result.push(c),
            std::path::Component::CurDir => {
                // Skip . components
            }
            std::path::Component::ParentDir => {
                // Skip parent directory references
                if !result.pop() {
                    // Can't go above current directory - just ignore
                }
            }
            std::path::Component::RootDir => {
                // Strip leading slash
            }
            std::path::Component::Prefix(_) => {
                // Windows prefix - ignore
            }
        }
    }

    // If path was just "." or empty, skip it
    if result.as_os_str().is_empty() {
        // Return "." for current directory entries
        return Ok(PathBuf::from("."));
    }

    Ok(result)
}

/// Create parent directories for a path
fn create_parent_dirs(path: &Path) -> PaxResult<()> {
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() && !parent.exists() {
            fs::create_dir_all(parent)?;
        }
    }
    Ok(())
}

/// Extract a directory
fn extract_directory(path: &Path, entry: &ArchiveEntry, options: &ReadOptions) -> PaxResult<()> {
    if !path.exists() {
        fs::create_dir_all(path)?;
    }

    set_owner(path, entry, options)?;
    set_permissions(path, entry, options)?;
    set_times(path, entry, options)?;

    Ok(())
}

/// Extract a symlink
fn extract_symlink(path: &Path, entry: &ArchiveEntry) -> PaxResult<()> {
    let target = entry
        .link_target
        .as_ref()
        .ok_or_else(|| PaxError::InvalidHeader("symlink without target".to_string()))?;

    // Remove existing file if present
    if path.exists() || path.symlink_metadata().is_ok() {
        fs::remove_file(path)?;
    }

    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(target, path)?;
    }

    #[cfg(windows)]
    {
        // Windows symlinks are more complex - try file symlink
        std::os::windows::fs::symlink_file(target, path)
            .or_else(|_| std::os::windows::fs::symlink_dir(target, path))?;
    }

    Ok(())
}

/// Extract a hard link
fn extract_hardlink(
    path: &Path,
    entry: &ArchiveEntry,
    extracted_links: &ExtractedLinks,
) -> PaxResult<()> {
    // Try to find the target from link_target first
    let target = if let Some(ref link_target) = entry.link_target {
        sanitize_path(link_target)?
    } else if let Some(existing) = extracted_links.get_link_target(entry) {
        existing.clone()
    } else {
        return Err(PaxError::InvalidHeader(
            "hard link target not found".to_string(),
        ));
    };

    // Remove existing file if present
    if path.exists() {
        fs::remove_file(path)?;
    }

    fs::hard_link(&target, path)?;

    Ok(())
}

/// Extract a block or character device (requires root privileges)
#[cfg(unix)]
fn extract_device(path: &Path, entry: &ArchiveEntry, options: &ReadOptions) -> PaxResult<()> {
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt;

    // Remove existing file if present
    if path.exists() || path.symlink_metadata().is_ok() {
        fs::remove_file(path)?;
    }

    let path_cstr = CString::new(path.as_os_str().as_bytes())
        .map_err(|_| PaxError::InvalidHeader("path contains null".to_string()))?;

    // makedev has different signatures on different platforms:
    // - Linux: makedev(major: u32, minor: u32) -> u64
    // - macOS: makedev(major: i32, minor: i32) -> i32
    #[cfg(target_os = "macos")]
    let dev = libc::makedev(entry.devmajor as i32, entry.devminor as i32);
    #[cfg(not(target_os = "macos"))]
    let dev = libc::makedev(entry.devmajor, entry.devminor);
    let type_bits: libc::mode_t = match entry.entry_type {
        EntryType::BlockDevice => libc::S_IFBLK,
        EntryType::CharDevice => libc::S_IFCHR,
        _ => 0,
    };
    let mode: libc::mode_t = (entry.mode as libc::mode_t) | type_bits;

    let result = unsafe { libc::mknod(path_cstr.as_ptr(), mode, dev) };

    if result != 0 {
        let err = std::io::Error::last_os_error();
        // EPERM usually means we're not root
        if err.raw_os_error() == Some(libc::EPERM) {
            eprintln!(
                "pax: cannot create device {}: Operation not permitted (requires root)",
                path.display()
            );
            crate::error::note_error();
            return Ok(());
        }
        return Err(err.into());
    }

    set_owner(path, entry, options)?;
    set_permissions(path, entry, options)?;
    set_times(path, entry, options)?;

    Ok(())
}

#[cfg(not(unix))]
fn extract_device(path: &Path, _entry: &ArchiveEntry, _options: &ReadOptions) -> PaxResult<()> {
    eprintln!(
        "pax: cannot create device {}: not supported on this platform",
        path.display()
    );
    Ok(())
}

/// Extract a FIFO (named pipe) - requires Unix
#[cfg(unix)]
fn extract_fifo(path: &Path, entry: &ArchiveEntry, options: &ReadOptions) -> PaxResult<()> {
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt;

    // Remove existing file if present
    if path.exists() || path.symlink_metadata().is_ok() {
        fs::remove_file(path)?;
    }

    let path_cstr = CString::new(path.as_os_str().as_bytes())
        .map_err(|_| PaxError::InvalidHeader("path contains null".to_string()))?;

    let result = unsafe { libc::mkfifo(path_cstr.as_ptr(), entry.mode as libc::mode_t) };

    if result != 0 {
        let err = std::io::Error::last_os_error();
        // EPERM usually means we're not root or filesystem doesn't support FIFOs
        if err.raw_os_error() == Some(libc::EPERM) {
            eprintln!(
                "pax: cannot create FIFO {}: Operation not permitted",
                path.display()
            );
            crate::error::note_error();
            return Ok(());
        }
        return Err(err.into());
    }

    set_owner(path, entry, options)?;
    set_permissions(path, entry, options)?;
    set_times(path, entry, options)?;

    Ok(())
}

#[cfg(not(unix))]
fn extract_fifo(path: &Path, _entry: &ArchiveEntry, _options: &ReadOptions) -> PaxResult<()> {
    eprintln!(
        "pax: cannot create FIFO {}: not supported on this platform",
        path.display()
    );
    Ok(())
}

/// Extract a regular file
fn extract_file<R: ArchiveReader>(
    archive: &mut R,
    path: &Path,
    entry: &ArchiveEntry,
    options: &ReadOptions,
) -> PaxResult<()> {
    // Remove existing file if present
    if path.exists() {
        fs::remove_file(path)?;
    }

    let mut file = File::create(path)?;
    copy_file_data(archive, &mut file, entry.size)?;

    // Set permissions and times after writing
    drop(file); // Close file before setting attributes

    set_owner(path, entry, options)?;
    set_permissions(path, entry, options)?;
    set_times(path, entry, options)?;

    Ok(())
}

/// Copy file data from archive to file
fn copy_file_data<R: ArchiveReader>(archive: &mut R, file: &mut File, size: u64) -> PaxResult<()> {
    let mut remaining = size;
    let mut buf = [0u8; 8192];

    while remaining > 0 {
        let to_read = std::cmp::min(remaining, buf.len() as u64) as usize;
        let n = archive.read_data(&mut buf[..to_read])?;
        if n == 0 {
            break;
        }
        file.write_all(&buf[..n])?;
        remaining -= n as u64;
    }

    Ok(())
}

/// Set file permissions
fn set_permissions(path: &Path, entry: &ArchiveEntry, options: &ReadOptions) -> PaxResult<()> {
    #[cfg(unix)]
    {
        let mut mode = entry.mode;

        // Per POSIX: If owner is not preserved, clear SUID and SGID bits
        if !options.preserve_owner {
            // Cast to u32 for cross-platform compatibility (u16 on macOS, u32 on Linux)
            #[allow(clippy::unnecessary_cast)]
            let setid_mask = !((libc::S_ISUID | libc::S_ISGID) as u32);
            mode &= setid_mask;
        }

        // When the mode is not explicitly preserved (no `-p p`/`-p e`), the file
        // is created as part of the "normal file creation action": the archived
        // mode is modified by the process file-creation mask (umask), exactly as
        // open()/mkdir() would do. With `-p p`/`-p e` the exact mode is restored.
        if !options.preserve_perms {
            mode &= !options.umask;
        }

        let perms = Permissions::from_mode(mode);
        fs::set_permissions(path, perms)?;
    }

    #[cfg(not(unix))]
    {
        // On non-Unix, we can only set read-only
        let mut perms = fs::metadata(path)?.permissions();
        perms.set_readonly(entry.mode & 0o200 == 0);
        fs::set_permissions(path, perms)?;
    }

    Ok(())
}

/// Set file owner (uid/gid) - requires privileges
#[cfg(unix)]
fn set_owner(path: &Path, entry: &ArchiveEntry, options: &ReadOptions) -> PaxResult<()> {
    if !options.preserve_owner {
        return Ok(());
    }

    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt;

    let path_cstr = CString::new(path.as_os_str().as_bytes())
        .map_err(|_| PaxError::InvalidHeader("path contains null".to_string()))?;

    let result = unsafe { libc::chown(path_cstr.as_ptr(), entry.uid, entry.gid) };

    if result != 0 {
        let err = std::io::Error::last_os_error();
        // EPERM usually means we're not root - warn but continue
        if err.raw_os_error() == Some(libc::EPERM) {
            eprintln!(
                "pax: cannot change owner of {}: Operation not permitted",
                path.display()
            );
            crate::error::note_error();
            return Ok(());
        }
        return Err(err.into());
    }

    Ok(())
}

#[cfg(not(unix))]
fn set_owner(_path: &Path, _entry: &ArchiveEntry, _options: &ReadOptions) -> PaxResult<()> {
    // Owner preservation not supported on non-Unix platforms
    Ok(())
}

/// Set file access and modification times
fn set_times(path: &Path, entry: &ArchiveEntry, options: &ReadOptions) -> PaxResult<()> {
    // If neither atime nor mtime preservation is requested, skip
    if !options.preserve_mtime && !options.preserve_atime {
        return Ok(());
    }

    #[cfg(unix)]
    {
        use std::ffi::CString;
        use std::os::unix::ffi::OsStrExt;

        let path_cstr = CString::new(path.as_os_str().as_bytes())
            .map_err(|_| PaxError::InvalidHeader("path contains null".to_string()))?;

        // Get current times for any we're not preserving
        let current_meta = fs::metadata(path).ok();

        use std::os::unix::fs::MetadataExt;

        // Determine the atime to set, preserving nanosecond precision.
        let (atime_sec, atime_nsec) = if options.preserve_atime {
            // Use the archive's atime if present, else fall back to mtime.
            match entry.atime {
                Some(sec) => (sec as i64, entry.atime_nsec as i64),
                None => (entry.mtime as i64, entry.mtime_nsec as i64),
            }
        } else {
            // Keep the current atime.
            current_meta
                .as_ref()
                .map(|m| (m.atime(), m.atime_nsec()))
                .unwrap_or((0, 0))
        };

        // Determine the mtime to set, preserving nanosecond precision.
        let (mtime_sec, mtime_nsec) = if options.preserve_mtime {
            (entry.mtime as i64, entry.mtime_nsec as i64)
        } else {
            current_meta
                .as_ref()
                .map(|m| (m.mtime(), m.mtime_nsec()))
                .unwrap_or((0, 0))
        };

        let times = [
            libc::timespec {
                tv_sec: atime_sec as libc::time_t,
                tv_nsec: atime_nsec as _,
            },
            libc::timespec {
                tv_sec: mtime_sec as libc::time_t,
                tv_nsec: mtime_nsec as _,
            },
        ];

        let result =
            unsafe { libc::utimensat(libc::AT_FDCWD, path_cstr.as_ptr(), times.as_ptr(), 0) };

        if result != 0 {
            let err = std::io::Error::last_os_error();
            // EPERM means we don't have permission - warn but continue
            if err.raw_os_error() == Some(libc::EPERM) {
                eprintln!(
                    "pax: warning: cannot set times on {}: Operation not permitted",
                    path.display()
                );
            } else {
                eprintln!(
                    "pax: warning: cannot set times on {}: {}",
                    path.display(),
                    err
                );
            }
            crate::error::note_error();
        }
    }

    Ok(())
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::os::unix::fs::PermissionsExt;
    use tempfile::TempDir;

    /// Without explicit `-p p`/`-p e` the extracted mode is the archived mode
    /// masked by the umask (normal file-creation action); with preservation the
    /// exact archived mode is restored.
    #[test]
    fn test_set_permissions_umask_vs_preserve() {
        let tmp = TempDir::new().unwrap();
        let path = tmp.path().join("member");
        std::fs::File::create(&path).unwrap();

        let entry = ArchiveEntry {
            path: path.clone(),
            mode: 0o777,
            entry_type: EntryType::Regular,
            ..Default::default()
        };

        // Not preserved: 0o777 & ~0o022 == 0o755.
        let opts = ReadOptions {
            preserve_perms: false,
            umask: 0o022,
            ..Default::default()
        };
        set_permissions(&path, &entry, &opts).unwrap();
        assert_eq!(
            std::fs::metadata(&path).unwrap().permissions().mode() & 0o777,
            0o755
        );

        // Preserved: exact 0o777 regardless of umask.
        let opts = ReadOptions {
            preserve_perms: true,
            umask: 0o022,
            ..Default::default()
        };
        set_permissions(&path, &entry, &opts).unwrap();
        assert_eq!(
            std::fs::metadata(&path).unwrap().permissions().mode() & 0o777,
            0o777
        );
    }
}
