//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Write mode implementation - create archives

use crate::archive::{ArchiveEntry, ArchiveFormat, ArchiveWriter, EntryType, HardLinkTracker};
use crate::error::PaxResult;
use crate::formats::{CpioWriter, PaxWriter, UstarWriter};
use crate::interactive::{InteractivePrompter, RenameResult};
use crate::options::{FormatOptions, InvalidAction};
use crate::subst::{apply_substitutions, SubstResult, Substitution};
use std::fs::{self, File, Metadata};
use std::io::{Read, Write};
#[cfg(unix)]
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};

/// Options for write/create mode
#[derive(Default)]
pub struct WriteOptions {
    /// Follow symlinks on command line
    pub cli_dereference: bool,
    /// Follow all symlinks
    pub dereference: bool,
    /// Don't descend into directories
    pub no_recurse: bool,
    /// Verbose output
    pub verbose: bool,
    /// Stay on one filesystem
    pub one_file_system: bool,
    /// Interactive rename mode
    pub interactive: bool,
    /// Reset access time after reading files
    pub reset_atime: bool,
    /// Path substitutions (-s option)
    pub substitutions: Vec<Substitution>,
    /// Format-specific options (-o option)
    pub format_options: FormatOptions,
}

/// Create an archive from files
pub fn create_archive<W: Write>(
    writer: W,
    files: &[PathBuf],
    format: ArchiveFormat,
    options: &WriteOptions,
) -> PaxResult<()> {
    match format {
        ArchiveFormat::Ustar => {
            let mut archive = UstarWriter::new(writer);
            write_files(&mut archive, files, options)?;
            archive.finish()
        }
        ArchiveFormat::Cpio => {
            let mut archive = CpioWriter::new(writer);
            write_files(&mut archive, files, options)?;
            archive.finish()
        }
        ArchiveFormat::Pax => {
            let mut archive = PaxWriter::with_options(writer, options.format_options.clone());
            write_files(&mut archive, files, options)?;
            archive.finish()
        }
    }
}

/// Write files to any archive writer
fn write_files<W: ArchiveWriter>(
    archive: &mut W,
    files: &[PathBuf],
    options: &WriteOptions,
) -> PaxResult<()> {
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
        write_path(
            archive,
            path,
            options,
            &mut link_tracker,
            initial_dev,
            true,
            &mut prompter,
        )?;
    }

    Ok(())
}

/// Write a path (file or directory) to the archive
fn write_path<W: ArchiveWriter>(
    archive: &mut W,
    path: &Path,
    options: &WriteOptions,
    link_tracker: &mut HardLinkTracker,
    initial_dev: Option<u64>,
    is_cli_arg: bool,
    prompter: &mut Option<InteractivePrompter>,
) -> PaxResult<()> {
    // Get metadata
    let follow = should_follow_symlink(options, is_cli_arg);
    let metadata = if follow {
        fs::metadata(path)
    } else {
        fs::symlink_metadata(path)
    };

    let metadata = match metadata {
        Ok(m) => m,
        Err(e) => {
            eprintln!("pax: {}: {}", path.display(), e);
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

    // Apply substitutions first (per POSIX: -s applies before -i)
    let archive_path = if !options.substitutions.is_empty() {
        let path_str = path.to_string_lossy();
        match apply_substitutions(&options.substitutions, &path_str) {
            SubstResult::Unchanged => path.to_path_buf(),
            SubstResult::Changed(new_path) => PathBuf::from(new_path),
            SubstResult::Empty => return Ok(()), // Skip this file
        }
    } else {
        path.to_path_buf()
    };

    // Handle interactive rename
    let archive_path = if let Some(ref mut p) = prompter {
        let path_str = archive_path.to_string_lossy();
        match p.prompt(&path_str)? {
            RenameResult::Skip => return Ok(()),
            RenameResult::UseOriginal => archive_path,
            RenameResult::Rename(new_path) => new_path,
        }
    } else {
        archive_path
    };

    // Handle invalid filename characters according to -o invalid=action
    let (archive_path, _needs_binary_charset) = match handle_invalid_filename(
        &archive_path,
        options.format_options.invalid_action,
        prompter,
    )? {
        InvalidHandleResult::Use(p) => (p, false),
        InvalidHandleResult::Skip => return Ok(()),
        InvalidHandleResult::Binary(p) => (p, true), // TODO: pass to entry for hdrcharset
    };

    if options.verbose {
        eprintln!("{}", path.display());
    }

    if metadata.is_dir() {
        write_directory(
            archive,
            path,
            &archive_path,
            &metadata,
            options,
            link_tracker,
            prompter,
        )?;
    } else if metadata.is_symlink() {
        write_symlink(archive, &archive_path, &metadata, path)?;
    } else if metadata.is_file() {
        write_file(
            archive,
            path,
            &archive_path,
            &metadata,
            link_tracker,
            options,
        )?;
    } else {
        // Handle special file types (block device, char device, fifo, socket)
        write_special(archive, &archive_path, &metadata)?;
    }

    Ok(())
}

/// Check if we should follow symlinks
fn should_follow_symlink(options: &WriteOptions, is_cli_arg: bool) -> bool {
    options.dereference || (is_cli_arg && options.cli_dereference)
}

/// Check if a path contains valid UTF-8 characters
#[cfg(unix)]
fn is_valid_utf8_path(path: &Path) -> bool {
    use std::os::unix::ffi::OsStrExt;
    // On Unix, check if the raw bytes are valid UTF-8
    std::str::from_utf8(path.as_os_str().as_bytes()).is_ok()
}

#[cfg(not(unix))]
fn is_valid_utf8_path(path: &Path) -> bool {
    // On non-Unix platforms, paths are typically already UTF-16 or UTF-8
    path.to_str().is_some()
}

/// Sanitize a path by replacing invalid UTF-8 sequences with replacement char
fn sanitize_path(path: &Path) -> PathBuf {
    PathBuf::from(path.to_string_lossy().into_owned())
}

/// Result of handling an invalid filename
enum InvalidHandleResult {
    /// Use this path (possibly sanitized)
    Use(PathBuf),
    /// Skip this file
    Skip,
    /// Mark as needing binary charset header
    Binary(PathBuf),
}

/// Handle a path with potentially invalid UTF-8 according to the invalid action
fn handle_invalid_filename(
    path: &Path,
    action: InvalidAction,
    prompter: &mut Option<InteractivePrompter>,
) -> PaxResult<InvalidHandleResult> {
    if is_valid_utf8_path(path) {
        return Ok(InvalidHandleResult::Use(path.to_path_buf()));
    }

    match action {
        InvalidAction::Bypass => {
            eprintln!(
                "pax: {}: Filename contains invalid characters, skipping",
                path.display()
            );
            Ok(InvalidHandleResult::Skip)
        }
        InvalidAction::Rename => {
            // Use interactive prompt to get new name
            if let Some(ref mut p) = prompter {
                let path_str = path.to_string_lossy();
                eprintln!(
                    "pax: {}: Filename contains invalid characters",
                    path.display()
                );
                match p.prompt(&path_str)? {
                    RenameResult::Skip => Ok(InvalidHandleResult::Skip),
                    RenameResult::UseOriginal => {
                        // User chose to use original despite warning - sanitize it
                        Ok(InvalidHandleResult::Use(sanitize_path(path)))
                    }
                    RenameResult::Rename(new_path) => Ok(InvalidHandleResult::Use(new_path)),
                }
            } else {
                // No prompter available, fall back to bypass
                eprintln!(
                    "pax: {}: Filename contains invalid characters, skipping (no terminal for rename)",
                    path.display()
                );
                Ok(InvalidHandleResult::Skip)
            }
        }
        InvalidAction::Write => {
            // Sanitize the name and write
            Ok(InvalidHandleResult::Use(sanitize_path(path)))
        }
        InvalidAction::Utf8 => {
            // Use lossy conversion (already done by to_string_lossy internally)
            Ok(InvalidHandleResult::Use(sanitize_path(path)))
        }
        InvalidAction::Binary => {
            // Mark for binary charset header, use lossy path
            Ok(InvalidHandleResult::Binary(sanitize_path(path)))
        }
    }
}

/// Write a directory and its contents
fn write_directory<W: ArchiveWriter>(
    archive: &mut W,
    src_path: &Path,
    archive_path: &Path,
    metadata: &Metadata,
    options: &WriteOptions,
    link_tracker: &mut HardLinkTracker,
    prompter: &mut Option<InteractivePrompter>,
) -> PaxResult<()> {
    // Write directory entry
    let entry = build_entry(archive_path, metadata, EntryType::Directory)?;
    archive.write_entry(&entry)?;
    archive.finish_entry()?;

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

        let entries = match fs::read_dir(src_path) {
            Ok(e) => e,
            Err(e) => {
                eprintln!("pax: {}: {}", src_path.display(), e);
                return Ok(());
            }
        };

        for entry in entries {
            let entry = match entry {
                Ok(e) => e,
                Err(e) => {
                    eprintln!("pax: {}: {}", src_path.display(), e);
                    continue;
                }
            };
            write_path(
                archive,
                &entry.path(),
                options,
                link_tracker,
                initial_dev,
                false,
                prompter,
            )?;
        }
    }

    Ok(())
}

/// Write a symlink
fn write_symlink<W: ArchiveWriter>(
    archive: &mut W,
    archive_path: &Path,
    metadata: &Metadata,
    src_path: &Path,
) -> PaxResult<()> {
    let target = fs::read_link(src_path)?;
    let target_str = target.to_string_lossy();
    let mut entry = build_entry(archive_path, metadata, EntryType::Symlink)?;
    entry.link_target = Some(target.clone());
    // For cpio format, the symlink target is written as file data
    // Set size to target length so cpio writer includes it
    entry.size = target_str.len() as u64;

    archive.write_entry(&entry)?;
    // Write the symlink target as data (needed for cpio format)
    archive.write_data(target_str.as_bytes())?;
    archive.finish_entry()?;

    Ok(())
}

/// Write a special file (block device, char device, fifo, socket)
#[cfg(unix)]
fn write_special<W: ArchiveWriter>(
    archive: &mut W,
    path: &Path,
    metadata: &Metadata,
) -> PaxResult<()> {
    use std::os::unix::fs::FileTypeExt;

    let file_type = metadata.file_type();
    let entry_type = if file_type.is_block_device() {
        EntryType::BlockDevice
    } else if file_type.is_char_device() {
        EntryType::CharDevice
    } else if file_type.is_fifo() {
        EntryType::Fifo
    } else if file_type.is_socket() {
        EntryType::Socket
    } else {
        eprintln!("pax: {}: unsupported file type", path.display());
        return Ok(());
    };

    let entry = build_entry(path, metadata, entry_type)?;
    archive.write_entry(&entry)?;
    archive.finish_entry()?;

    Ok(())
}

#[cfg(not(unix))]
fn write_special<W: ArchiveWriter>(
    _archive: &mut W,
    path: &Path,
    _metadata: &Metadata,
) -> PaxResult<()> {
    eprintln!(
        "pax: {}: special files not supported on this platform",
        path.display()
    );
    Ok(())
}

/// Write a regular file
fn write_file<W: ArchiveWriter>(
    archive: &mut W,
    src_path: &Path,
    archive_path: &Path,
    metadata: &Metadata,
    link_tracker: &mut HardLinkTracker,
    options: &WriteOptions,
) -> PaxResult<()> {
    // Save access time if we need to reset it after reading
    #[cfg(unix)]
    let original_atime = if options.reset_atime {
        Some((metadata.atime(), metadata.atime_nsec()))
    } else {
        None
    };

    let mut entry = build_entry(archive_path, metadata, EntryType::Regular)?;
    // But use src_path for hard link tracking (dev/ino)
    entry.dev = {
        #[cfg(unix)]
        {
            metadata.dev()
        }
        #[cfg(not(unix))]
        {
            0
        }
    };
    entry.ino = {
        #[cfg(unix)]
        {
            metadata.ino()
        }
        #[cfg(not(unix))]
        {
            0
        }
    };
    entry.nlink = {
        #[cfg(unix)]
        {
            metadata.nlink() as u32
        }
        #[cfg(not(unix))]
        {
            1
        }
    };

    // Check for hard link
    if let Some(original_path) = link_tracker.check(&entry) {
        entry.entry_type = EntryType::Hardlink;
        entry.link_target = Some(original_path);

        // Per POSIX: -o linkdata means write file contents for each hard link
        // By default, hard links have size=0 and no data
        if !options.format_options.link_data {
            entry.size = 0;
            archive.write_entry(&entry)?;
            archive.finish_entry()?;
            return Ok(());
        }
        // With linkdata, fall through to write the file contents
    }

    // Write regular file
    archive.write_entry(&entry)?;

    // Copy file contents
    let mut file = File::open(src_path)?;
    copy_file_data(&mut file, archive)?;

    archive.finish_entry()?;

    // Reset access time if requested
    #[cfg(unix)]
    if let Some((atime_sec, atime_nsec)) = original_atime {
        reset_atime(src_path, atime_sec, atime_nsec);
    }

    Ok(())
}

/// Copy file data to archive
fn copy_file_data<W: ArchiveWriter>(file: &mut File, archive: &mut W) -> PaxResult<()> {
    let mut buf = [0u8; 8192];

    loop {
        let n = file.read(&mut buf)?;
        if n == 0 {
            break;
        }
        archive.write_data(&buf[..n])?;
    }

    Ok(())
}

/// Build an ArchiveEntry from path and metadata
fn build_entry(path: &Path, metadata: &Metadata, entry_type: EntryType) -> PaxResult<ArchiveEntry> {
    let mut entry = ArchiveEntry::new(path.to_path_buf(), entry_type);

    #[cfg(unix)]
    {
        entry.mode = metadata.mode() & 0o7777;
        entry.uid = metadata.uid();
        entry.gid = metadata.gid();
        entry.mtime = metadata.mtime() as u64;
        entry.dev = metadata.dev();
        entry.ino = metadata.ino();
        entry.nlink = metadata.nlink() as u32;

        // Extract device major/minor for block/char devices
        if entry_type == EntryType::BlockDevice || entry_type == EntryType::CharDevice {
            let rdev = metadata.rdev() as libc::dev_t;
            entry.devmajor = libc::major(rdev) as u32;
            entry.devminor = libc::minor(rdev) as u32;
        }
    }

    #[cfg(not(unix))]
    {
        entry.mode = if metadata.permissions().readonly() {
            0o444
        } else {
            0o644
        };
        entry.mtime = metadata
            .modified()
            .ok()
            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
            .map(|d| d.as_secs())
            .unwrap_or(0);
    }

    if entry_type == EntryType::Regular || entry_type == EntryType::Symlink {
        entry.size = metadata.len();
    }

    // Try to get user/group names
    #[cfg(unix)]
    {
        entry.uname = get_username(entry.uid);
        entry.gname = get_groupname(entry.gid);
    }

    Ok(entry)
}

/// Get username from uid
#[cfg(unix)]
fn get_username(uid: u32) -> Option<String> {
    unsafe {
        let pw = libc::getpwuid(uid);
        if pw.is_null() {
            return None;
        }
        let name = std::ffi::CStr::from_ptr((*pw).pw_name);
        name.to_str().ok().map(|s| s.to_string())
    }
}

/// Get group name from gid
#[cfg(unix)]
fn get_groupname(gid: u32) -> Option<String> {
    unsafe {
        let gr = libc::getgrgid(gid);
        if gr.is_null() {
            return None;
        }
        let name = std::ffi::CStr::from_ptr((*gr).gr_name);
        name.to_str().ok().map(|s| s.to_string())
    }
}

/// Write files to a pre-existing archive writer (for multi-volume support)
pub fn write_files_to_archive<W: ArchiveWriter>(
    archive: &mut W,
    files: &[PathBuf],
    _format: ArchiveFormat,
    options: &WriteOptions,
) -> PaxResult<()> {
    write_files(archive, files, options)
}

/// Read file list from stdin (one path per line)
pub fn read_file_list<R: Read>(reader: R) -> PaxResult<Vec<PathBuf>> {
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

/// Reset access time of a file to the specified time
#[cfg(unix)]
fn reset_atime(path: &Path, atime_sec: i64, atime_nsec: i64) {
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt;

    let path_cstr = match CString::new(path.as_os_str().as_bytes()) {
        Ok(s) => s,
        Err(_) => return,
    };

    // Get current modification time to preserve it
    let metadata = match fs::symlink_metadata(path) {
        Ok(m) => m,
        Err(_) => return,
    };

    let mtime_sec = metadata.mtime();
    let mtime_nsec = metadata.mtime_nsec();

    // Use utimensat for nanosecond precision if available
    let times = [
        libc::timespec {
            tv_sec: atime_sec as libc::time_t,
            tv_nsec: atime_nsec as libc::c_long,
        },
        libc::timespec {
            tv_sec: mtime_sec as libc::time_t,
            tv_nsec: mtime_nsec as libc::c_long,
        },
    ];

    unsafe {
        libc::utimensat(libc::AT_FDCWD, path_cstr.as_ptr(), times.as_ptr(), 0);
    }
}
