//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Copy mode implementation - copy files between directories
//!
//! In copy mode (-r -w), pax copies files to a destination directory
//! without creating an intermediate archive. Hard links are created
//! between source and destination when possible (with -l option).

use crate::error::{PaxError, PaxResult};
use crate::interactive::{InteractivePrompter, RenameResult};
use crate::pattern::{matches_any, Pattern};
use crate::subst::{apply_substitutions, SubstResult, Substitution};
use std::collections::HashMap;
use std::fs::{self, File, Permissions};
use std::io::{Read, Write};
#[cfg(unix)]
use std::os::unix::fs::{MetadataExt, PermissionsExt};
use std::path::{Path, PathBuf};

/// Options for copy mode
#[derive(Default)]
pub struct CopyOptions {
    /// Patterns to match (empty means match all)
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
    /// Create hard links instead of copying
    pub link: bool,
    /// Follow symlinks on command line
    pub cli_dereference: bool,
    /// Follow all symlinks
    pub dereference: bool,
    /// Don't descend into directories
    pub no_recurse: bool,
    /// Stay on one filesystem
    pub one_file_system: bool,
    /// Interactive rename mode
    pub interactive: bool,
    /// Reset access time after reading files
    #[allow(dead_code)]
    pub reset_atime: bool,
    /// Update mode - only copy if source is newer than destination
    pub update: bool,
    /// Path substitutions (-s option)
    pub substitutions: Vec<Substitution>,
}

/// Tracks hard links during copy to preserve link structure
#[derive(Debug, Default)]
struct HardLinkTracker {
    /// Maps (dev, ino) to the destination path
    seen: HashMap<(u64, u64), PathBuf>,
}

impl HardLinkTracker {
    fn new() -> Self {
        HardLinkTracker {
            seen: HashMap::new(),
        }
    }

    /// Check if we've seen this file before (by dev/ino)
    /// Returns the destination path if this is a hard link to a file we already copied
    #[cfg(unix)]
    fn check(&mut self, src_path: &Path, dest_path: &Path) -> PaxResult<Option<PathBuf>> {
        let metadata = fs::symlink_metadata(src_path)?;

        // Only track files with multiple links
        if metadata.nlink() <= 1 {
            return Ok(None);
        }

        let key = (metadata.dev(), metadata.ino());
        if let Some(original_dest) = self.seen.get(&key) {
            Ok(Some(original_dest.clone()))
        } else {
            self.seen.insert(key, dest_path.to_path_buf());
            Ok(None)
        }
    }

    #[cfg(not(unix))]
    fn check(&mut self, _src_path: &Path, _dest_path: &Path) -> PaxResult<Option<PathBuf>> {
        // Hard link tracking not supported on non-Unix
        Ok(None)
    }
}

/// Copy files to a destination directory
pub fn copy_files(files: &[PathBuf], dest_dir: &Path, options: &CopyOptions) -> PaxResult<()> {
    // Verify destination is a directory
    if !dest_dir.exists() {
        return Err(PaxError::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!(
                "destination directory does not exist: {}",
                dest_dir.display()
            ),
        )));
    }

    if !dest_dir.is_dir() {
        return Err(PaxError::Io(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("destination is not a directory: {}", dest_dir.display()),
        )));
    }

    let mut link_tracker = HardLinkTracker::new();
    #[cfg(unix)]
    let initial_dev: Option<u64> = None;
    #[cfg(not(unix))]
    let initial_dev: Option<u64> = None;

    // Create interactive prompter if needed
    let mut prompter = if options.interactive {
        Some(InteractivePrompter::new()?)
    } else {
        None
    };

    for path in files {
        copy_path(
            path,
            dest_dir,
            options,
            &mut link_tracker,
            initial_dev,
            true,
            &mut prompter,
        )?;
    }

    Ok(())
}

/// Copy a single path (file or directory) to the destination
fn copy_path(
    src: &Path,
    dest_dir: &Path,
    options: &CopyOptions,
    link_tracker: &mut HardLinkTracker,
    initial_dev: Option<u64>,
    is_cli_arg: bool,
    prompter: &mut Option<InteractivePrompter>,
) -> PaxResult<()> {
    // Handle special case of "." - copy contents directly to destination
    let src_str = src.to_string_lossy();
    if src_str == "." {
        return copy_current_dir_contents(
            src,
            dest_dir,
            options,
            link_tracker,
            initial_dev,
            prompter,
        );
    }

    // Get metadata (following symlinks if requested)
    let follow = should_follow_symlink(options, is_cli_arg);
    let metadata = if follow {
        fs::metadata(src)
    } else {
        fs::symlink_metadata(src)
    };

    let metadata = match metadata {
        Ok(m) => m,
        Err(e) => {
            eprintln!("pax: {}: {}", src.display(), e);
            return Ok(());
        }
    };

    // Check pattern matching
    let path_str = src.to_string_lossy();
    let matches = matches_any(&options.patterns, &path_str);
    let should_copy = if options.exclude { !matches } else { matches };

    if !should_copy {
        return Ok(());
    }

    // Check one_file_system
    #[cfg(unix)]
    {
        if options.one_file_system {
            let dev = metadata.dev();
            if let Some(initial) = initial_dev {
                if dev != initial {
                    return Ok(());
                }
            }
        }
    }

    // Apply substitutions first (per POSIX: -s applies before -i)
    let dest_name = if !options.substitutions.is_empty() {
        match apply_substitutions(&options.substitutions, &path_str) {
            SubstResult::Unchanged => src
                .file_name()
                .map(PathBuf::from)
                .unwrap_or_else(|| PathBuf::from(".")),
            SubstResult::Changed(new_path) => PathBuf::from(new_path),
            SubstResult::Empty => return Ok(()), // Skip this file
        }
    } else {
        src.file_name()
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("."))
    };

    // Handle interactive rename
    let dest_name = if let Some(ref mut p) = prompter {
        let name_str = dest_name.to_string_lossy();
        match p.prompt(&name_str)? {
            RenameResult::Skip => return Ok(()),
            RenameResult::UseOriginal => dest_name,
            RenameResult::Rename(new_name) => new_name,
        }
    } else {
        dest_name
    };

    // Compute destination path using the (possibly renamed) name
    let dest = dest_dir.join(&dest_name);

    // Check no_clobber
    if options.no_clobber && dest.exists() {
        return Ok(());
    }

    // Check update mode (-u): only copy if source is newer than dest
    if options.update && !is_source_newer(&metadata, &dest) {
        return Ok(());
    }

    if options.verbose {
        eprintln!("{}", src.display());
    }

    if metadata.is_dir() {
        copy_directory(src, &dest, options, link_tracker, &metadata, prompter)?;
    } else if metadata.is_symlink() {
        copy_symlink(src, &dest)?;
    } else if metadata.is_file() {
        copy_file(src, &dest, options, link_tracker, &metadata)?;
    } else {
        eprintln!("pax: {}: unsupported file type", src.display());
    }

    Ok(())
}

/// Copy contents of current directory to destination (for "." argument)
fn copy_current_dir_contents(
    src: &Path,
    dest_dir: &Path,
    options: &CopyOptions,
    link_tracker: &mut HardLinkTracker,
    initial_dev: Option<u64>,
    prompter: &mut Option<InteractivePrompter>,
) -> PaxResult<()> {
    let entries = match fs::read_dir(src) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("pax: {}: {}", src.display(), e);
            return Ok(());
        }
    };

    for entry in entries {
        let entry = match entry {
            Ok(e) => e,
            Err(e) => {
                eprintln!("pax: {}: {}", src.display(), e);
                continue;
            }
        };

        let child_src = entry.path();
        copy_path(
            &child_src,
            dest_dir,
            options,
            link_tracker,
            initial_dev,
            false,
            prompter,
        )?;
    }

    Ok(())
}

/// Check if we should follow symlinks
fn should_follow_symlink(options: &CopyOptions, is_cli_arg: bool) -> bool {
    options.dereference || (is_cli_arg && options.cli_dereference)
}

/// Check if source is newer than destination (for -u option)
fn is_source_newer(src_metadata: &fs::Metadata, dest: &Path) -> bool {
    // If destination doesn't exist, always copy
    if !dest.exists() {
        return true;
    }

    // Get the modification time of destination
    let dest_mtime = match fs::metadata(dest) {
        Ok(meta) => {
            #[cfg(unix)]
            {
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
        Err(_) => return true, // If we can't stat dest, assume we should copy
    };

    // Get source modification time
    #[cfg(unix)]
    let src_mtime = src_metadata.mtime() as u64;
    #[cfg(not(unix))]
    let src_mtime = src_metadata
        .modified()
        .ok()
        .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
        .map(|d| d.as_secs())
        .unwrap_or(0);

    // Copy if source is newer than destination
    src_mtime > dest_mtime
}

/// Copy a directory and its contents
fn copy_directory(
    src: &Path,
    dest: &Path,
    options: &CopyOptions,
    link_tracker: &mut HardLinkTracker,
    metadata: &fs::Metadata,
    prompter: &mut Option<InteractivePrompter>,
) -> PaxResult<()> {
    // Create the destination directory
    if !dest.exists() {
        fs::create_dir(dest)?;
    }

    // Set permissions
    set_permissions(dest, metadata, options)?;

    // Recurse into directory unless no_recurse
    if !options.no_recurse {
        #[cfg(unix)]
        let initial_dev = if options.one_file_system {
            Some(metadata.dev())
        } else {
            None
        };
        #[cfg(not(unix))]
        let initial_dev: Option<u64> = None;

        let entries = match fs::read_dir(src) {
            Ok(e) => e,
            Err(e) => {
                eprintln!("pax: {}: {}", src.display(), e);
                return Ok(());
            }
        };

        for entry in entries {
            let entry = match entry {
                Ok(e) => e,
                Err(e) => {
                    eprintln!("pax: {}: {}", src.display(), e);
                    continue;
                }
            };

            let child_src = entry.path();
            let child_name = entry.file_name();
            let child_dest = dest.join(&child_name);

            // Recurse with the child paths
            copy_path_to_dest(
                &child_src,
                &child_dest,
                options,
                link_tracker,
                initial_dev,
                prompter,
            )?;
        }
    }

    // Set times after contents are copied
    set_times(dest, metadata, options)?;

    Ok(())
}

/// Copy a path directly to a specific destination (used for recursion)
fn copy_path_to_dest(
    src: &Path,
    dest: &Path,
    options: &CopyOptions,
    link_tracker: &mut HardLinkTracker,
    initial_dev: Option<u64>,
    prompter: &mut Option<InteractivePrompter>,
) -> PaxResult<()> {
    // Get metadata
    let follow = options.dereference;
    let metadata = if follow {
        fs::metadata(src)
    } else {
        fs::symlink_metadata(src)
    };

    let metadata = match metadata {
        Ok(m) => m,
        Err(e) => {
            eprintln!("pax: {}: {}", src.display(), e);
            return Ok(());
        }
    };

    // Check one_file_system
    #[cfg(unix)]
    {
        if options.one_file_system {
            let dev = metadata.dev();
            if let Some(initial) = initial_dev {
                if dev != initial {
                    return Ok(());
                }
            }
        }
    }

    // Handle interactive rename if enabled
    let actual_dest = if let Some(ref mut p) = prompter {
        let path_str = src.to_string_lossy();
        match p.prompt(&path_str)? {
            RenameResult::Skip => return Ok(()),
            RenameResult::UseOriginal => dest.to_path_buf(),
            RenameResult::Rename(new_name) => {
                // Use the new name as the destination, but in the same parent directory
                dest.parent().map(|p| p.join(&new_name)).unwrap_or(new_name)
            }
        }
    } else {
        dest.to_path_buf()
    };

    // Check no_clobber
    if options.no_clobber && actual_dest.exists() {
        return Ok(());
    }

    // Check update mode (-u): only copy if source is newer than dest
    if options.update && !is_source_newer(&metadata, &actual_dest) {
        return Ok(());
    }

    if options.verbose {
        eprintln!("{}", src.display());
    }

    if metadata.is_dir() {
        copy_directory(
            src,
            &actual_dest,
            options,
            link_tracker,
            &metadata,
            prompter,
        )?;
    } else if metadata.is_symlink() {
        copy_symlink(src, &actual_dest)?;
    } else if metadata.is_file() {
        copy_file(src, &actual_dest, options, link_tracker, &metadata)?;
    } else {
        eprintln!("pax: {}: unsupported file type", src.display());
    }

    Ok(())
}

/// Copy a symlink
fn copy_symlink(src: &Path, dest: &Path) -> PaxResult<()> {
    let target = fs::read_link(src)?;

    // Remove existing file if present
    if dest.exists() || dest.symlink_metadata().is_ok() {
        fs::remove_file(dest)?;
    }

    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(&target, dest)?;
    }

    #[cfg(windows)]
    {
        std::os::windows::fs::symlink_file(&target, dest)
            .or_else(|_| std::os::windows::fs::symlink_dir(&target, dest))?;
    }

    Ok(())
}

/// Copy a regular file
fn copy_file(
    src: &Path,
    dest: &Path,
    options: &CopyOptions,
    link_tracker: &mut HardLinkTracker,
    metadata: &fs::Metadata,
) -> PaxResult<()> {
    // Check if we should create a hard link instead of copying
    if options.link {
        // Try to create hard link to source
        if dest.exists() {
            fs::remove_file(dest)?;
        }

        if let Err(e) = fs::hard_link(src, dest) {
            // Hard link failed (maybe cross-device), fall back to copy
            eprintln!("pax: hard link failed, copying: {}: {}", src.display(), e);
            do_copy_file(src, dest, metadata, options)?;
        }
        return Ok(());
    }

    // Check if this is a hard link to a file we already copied
    if let Some(link_target) = link_tracker.check(src, dest)? {
        // Create hard link to the already-copied file
        if dest.exists() {
            fs::remove_file(dest)?;
        }
        fs::hard_link(&link_target, dest)?;
        return Ok(());
    }

    // Normal copy
    do_copy_file(src, dest, metadata, options)
}

/// Actually copy file contents
fn do_copy_file(
    src: &Path,
    dest: &Path,
    metadata: &fs::Metadata,
    options: &CopyOptions,
) -> PaxResult<()> {
    // Remove existing file if present
    if dest.exists() {
        fs::remove_file(dest)?;
    }

    // Create parent directories if needed
    if let Some(parent) = dest.parent() {
        if !parent.exists() {
            fs::create_dir_all(parent)?;
        }
    }

    // Copy file contents
    let mut src_file = File::open(src)?;
    let mut dest_file = File::create(dest)?;

    let mut buf = [0u8; 8192];
    loop {
        let n = src_file.read(&mut buf)?;
        if n == 0 {
            break;
        }
        dest_file.write_all(&buf[..n])?;
    }

    drop(dest_file);

    // Set permissions and times
    set_permissions(dest, metadata, options)?;
    set_times(dest, metadata, options)?;

    Ok(())
}

/// Set file permissions
fn set_permissions(path: &Path, metadata: &fs::Metadata, options: &CopyOptions) -> PaxResult<()> {
    if !options.preserve_perms {
        return Ok(());
    }

    #[cfg(unix)]
    {
        let mode = metadata.mode() & 0o7777;
        let perms = Permissions::from_mode(mode);
        fs::set_permissions(path, perms)?;
    }

    #[cfg(not(unix))]
    {
        let perms = metadata.permissions();
        fs::set_permissions(path, perms)?;
    }

    Ok(())
}

/// Set file modification time
fn set_times(path: &Path, metadata: &fs::Metadata, options: &CopyOptions) -> PaxResult<()> {
    if !options.preserve_mtime {
        return Ok(());
    }

    #[cfg(unix)]
    {
        use std::ffi::CString;
        use std::os::unix::ffi::OsStrExt;

        let mtime = metadata.mtime();

        let path_cstr = CString::new(path.as_os_str().as_bytes())
            .map_err(|_| PaxError::InvalidHeader("path contains null".to_string()))?;

        let times = [
            libc::timeval {
                tv_sec: mtime as libc::time_t,
                tv_usec: 0,
            },
            libc::timeval {
                tv_sec: mtime as libc::time_t,
                tv_usec: 0,
            },
        ];

        unsafe {
            libc::utimes(path_cstr.as_ptr(), times.as_ptr());
        }
    }

    Ok(())
}

/// Read file list from stdin (one path per line)
pub fn read_file_list<R: std::io::Read>(reader: R) -> PaxResult<Vec<PathBuf>> {
    use std::io::BufRead;

    let reader = std::io::BufReader::new(reader);
    let mut files = Vec::new();

    for line in reader.lines() {
        let line = line?;
        let line = line.trim();
        if !line.is_empty() {
            files.push(PathBuf::from(line));
        }
    }

    Ok(files)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_copy_file() {
        let src_dir = TempDir::new().unwrap();
        let dest_dir = TempDir::new().unwrap();

        // Create source file
        let src_file = src_dir.path().join("test.txt");
        fs::write(&src_file, "hello world").unwrap();

        let options = CopyOptions {
            preserve_perms: true,
            preserve_mtime: true,
            ..Default::default()
        };

        copy_files(&[src_file], dest_dir.path(), &options).unwrap();

        let dest_file = dest_dir.path().join("test.txt");
        assert!(dest_file.exists());
        assert_eq!(fs::read_to_string(&dest_file).unwrap(), "hello world");
    }

    #[test]
    fn test_copy_directory() {
        let src_dir = TempDir::new().unwrap();
        let dest_dir = TempDir::new().unwrap();

        // Create source directory with files
        let subdir = src_dir.path().join("subdir");
        fs::create_dir(&subdir).unwrap();
        fs::write(subdir.join("file1.txt"), "content1").unwrap();
        fs::write(subdir.join("file2.txt"), "content2").unwrap();

        let options = CopyOptions::default();

        copy_files(&[subdir], dest_dir.path(), &options).unwrap();

        let copied_subdir = dest_dir.path().join("subdir");
        assert!(copied_subdir.is_dir());
        assert_eq!(
            fs::read_to_string(copied_subdir.join("file1.txt")).unwrap(),
            "content1"
        );
        assert_eq!(
            fs::read_to_string(copied_subdir.join("file2.txt")).unwrap(),
            "content2"
        );
    }

    #[test]
    fn test_no_clobber() {
        let src_dir = TempDir::new().unwrap();
        let dest_dir = TempDir::new().unwrap();

        // Create source file
        let src_file = src_dir.path().join("test.txt");
        fs::write(&src_file, "new content").unwrap();

        // Create existing dest file
        let dest_file = dest_dir.path().join("test.txt");
        fs::write(&dest_file, "existing content").unwrap();

        let options = CopyOptions {
            no_clobber: true,
            ..Default::default()
        };

        copy_files(&[src_file], dest_dir.path(), &options).unwrap();

        // Destination should still have original content
        assert_eq!(fs::read_to_string(&dest_file).unwrap(), "existing content");
    }

    #[cfg(unix)]
    #[test]
    fn test_copy_symlink() {
        let src_dir = TempDir::new().unwrap();
        let dest_dir = TempDir::new().unwrap();

        // Create source file and symlink
        let src_file = src_dir.path().join("target.txt");
        fs::write(&src_file, "target content").unwrap();

        let src_link = src_dir.path().join("link.txt");
        std::os::unix::fs::symlink("target.txt", &src_link).unwrap();

        let options = CopyOptions::default();

        copy_files(&[src_link], dest_dir.path(), &options).unwrap();

        let dest_link = dest_dir.path().join("link.txt");
        assert!(dest_link.symlink_metadata().unwrap().is_symlink());
        assert_eq!(
            fs::read_link(&dest_link).unwrap().to_str().unwrap(),
            "target.txt"
        );
    }
}
