//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Append mode implementation - add files to existing archives
//!
//! Append mode works by:
//! 1. Opening the existing archive for read+write
//! 2. Detecting the archive format
//! 3. Seeking to find the two zero blocks (end-of-archive marker)
//! 4. Positioning write cursor at start of first zero block
//! 5. Writing new entries using existing write infrastructure
//! 6. Writing new end-of-archive marker
//!
//! Note: Only ustar and pax formats are supported. Appending to cpio
//! is problematic due to device/inode conflicts (per POSIX).

use crate::archive::{ArchiveFormat, ArchiveWriter, HardLinkTracker};
use crate::error::{PaxError, PaxResult};
use crate::formats::{PaxWriter, UstarWriter};
use crate::modes::write::WriteOptions;
use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom};
use std::path::PathBuf;

const BLOCK_SIZE: usize = 512;

/// Append files to an existing archive
pub fn append_to_archive(
    archive_path: &PathBuf,
    files: &[PathBuf],
    options: &WriteOptions,
) -> PaxResult<()> {
    // Open archive for read+write
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(archive_path)?;

    // Detect the archive format
    let format = detect_format(&mut file)?;

    // Only support ustar and pax for append
    if format == ArchiveFormat::Cpio {
        return Err(PaxError::InvalidFormat(
            "appending to cpio archives is not supported".to_string(),
        ));
    }

    // Find the end-of-archive position (two zero blocks)
    let append_pos = find_end_of_archive(&mut file)?;

    // Seek to the append position
    file.seek(SeekFrom::Start(append_pos))?;

    // Write the new entries
    match format {
        ArchiveFormat::Ustar => {
            let mut archive = UstarWriter::new(&mut file);
            write_files(&mut archive, files, options)?;
            archive.finish()?;
        }
        ArchiveFormat::Pax => {
            let mut archive = PaxWriter::new(&mut file);
            write_files(&mut archive, files, options)?;
            archive.finish()?;
        }
        ArchiveFormat::Cpio => unreachable!(), // Already checked above
    }

    Ok(())
}

/// Detect archive format from file
fn detect_format(file: &mut File) -> PaxResult<ArchiveFormat> {
    let mut header = [0u8; BLOCK_SIZE];
    file.read_exact(&mut header)?;
    file.seek(SeekFrom::Start(0))?; // Reset to beginning

    // Check for ustar magic at offset 257
    if &header[257..262] == b"ustar" {
        // Check typeflag at offset 156 for pax extended headers
        let typeflag = header[156];
        if typeflag == b'x' || typeflag == b'g' {
            return Ok(ArchiveFormat::Pax);
        }
        return Ok(ArchiveFormat::Ustar);
    }

    // Check for cpio magic at offset 0
    // ASCII formats (6 bytes):
    //   070707 = POSIX octet-oriented (odc)
    //   070701 = SVR4 newc (no CRC)
    //   070702 = SVR4 newc with CRC
    // Binary format (2 bytes):
    //   0x71C7 = old binary cpio (little-endian)
    //   0xC771 = old binary cpio (big-endian)
    let magic = &header[0..6];
    if magic == b"070707" || magic == b"070701" || magic == b"070702" {
        return Ok(ArchiveFormat::Cpio);
    }
    // Check for binary cpio magic
    let magic16 = u16::from_le_bytes([header[0], header[1]]);
    let magic16_be = u16::from_be_bytes([header[0], header[1]]);
    if magic16 == 0o070707 || magic16_be == 0o070707 {
        return Ok(ArchiveFormat::Cpio);
    }

    // Check for old-style tar by validating checksum
    if is_valid_tar_checksum(&header) {
        let typeflag = header[156];
        if typeflag == b'x' || typeflag == b'g' {
            return Ok(ArchiveFormat::Pax);
        }
        return Ok(ArchiveFormat::Ustar);
    }

    Err(PaxError::InvalidFormat(
        "unable to detect archive format".to_string(),
    ))
}

/// Verify tar checksum
fn is_valid_tar_checksum(header: &[u8]) -> bool {
    if header.len() < 512 {
        return false;
    }

    // Parse checksum field at offset 148
    let chksum_str = std::str::from_utf8(&header[148..156]).unwrap_or("");
    let chksum_str = chksum_str.trim_matches(|c| c == ' ' || c == '\0');
    if chksum_str.is_empty() {
        return false;
    }

    // Reject if checksum contains a sign
    if chksum_str.starts_with('+') || chksum_str.starts_with('-') {
        return false;
    }

    let stored = match u32::from_str_radix(chksum_str, 8) {
        Ok(v) => v,
        Err(_) => return false,
    };

    // Calculate checksum
    let mut sum: u32 = 0;
    for (i, &byte) in header[0..512].iter().enumerate() {
        if (148..156).contains(&i) {
            sum += b' ' as u32;
        } else {
            sum += byte as u32;
        }
    }

    sum == stored
}

/// Find the position of the end-of-archive marker (two zero blocks)
fn find_end_of_archive(file: &mut File) -> PaxResult<u64> {
    let file_size = file.seek(SeekFrom::End(0))?;
    file.seek(SeekFrom::Start(0))?;

    if file_size < (BLOCK_SIZE * 2) as u64 {
        return Err(PaxError::InvalidFormat(
            "archive too small to contain end marker".to_string(),
        ));
    }

    // Read the archive block by block looking for zero blocks
    let mut header = [0u8; BLOCK_SIZE];
    let mut pos: u64 = 0;
    let mut zero_block_start: Option<u64> = None;

    while pos < file_size {
        let n = file.read(&mut header)?;
        if n < BLOCK_SIZE {
            // End of file reached
            break;
        }

        if is_zero_block(&header) {
            if let Some(start) = zero_block_start {
                // Found second zero block - this is the end marker
                // Return the position of the first zero block
                return Ok(start);
            } else {
                zero_block_start = Some(pos);
            }
        } else {
            zero_block_start = None;

            // If this is a valid header, skip the data blocks
            if is_valid_tar_checksum(&header) {
                // Parse size to skip data
                let size = parse_octal(&header[124..136]).unwrap_or(0);
                let data_blocks = size.div_ceil(BLOCK_SIZE as u64);
                let skip = data_blocks * BLOCK_SIZE as u64;
                pos += skip;
                file.seek(SeekFrom::Current(skip as i64))?;
            }
        }

        pos += BLOCK_SIZE as u64;
    }

    // If we didn't find the end marker, append at the current end
    // This handles malformed archives or single zero block
    if let Some(start) = zero_block_start {
        Ok(start)
    } else {
        // No zero blocks found, append at end (but this shouldn't happen
        // with a valid archive)
        Ok(file_size)
    }
}

/// Check if a block is all zeros
fn is_zero_block(block: &[u8]) -> bool {
    block.iter().all(|&b| b == 0)
}

/// Parse an octal number from bytes
fn parse_octal(bytes: &[u8]) -> PaxResult<u64> {
    let s = std::str::from_utf8(bytes)
        .map_err(|_| PaxError::InvalidHeader("invalid octal".to_string()))?;
    let s = s.trim_matches(|c| c == ' ' || c == '\0');
    if s.is_empty() {
        return Ok(0);
    }
    // Reject if the octal string contains a sign
    if s.starts_with('+') || s.starts_with('-') {
        return Err(PaxError::InvalidHeader(format!("invalid octal: {}", s)));
    }
    u64::from_str_radix(s, 8).map_err(|_| PaxError::InvalidHeader(format!("invalid octal: {}", s)))
}

/// Write files to archive (reuse write module infrastructure)
fn write_files<W: ArchiveWriter>(
    archive: &mut W,
    files: &[PathBuf],
    options: &WriteOptions,
) -> PaxResult<()> {
    use crate::interactive::InteractivePrompter;

    let mut link_tracker = HardLinkTracker::new();

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
            None,
            true,
            &mut prompter,
        )?;
    }

    Ok(())
}

/// Write a single path to the archive
fn write_path<W: ArchiveWriter>(
    archive: &mut W,
    path: &std::path::Path,
    options: &WriteOptions,
    link_tracker: &mut HardLinkTracker,
    initial_dev: Option<u64>,
    is_cli_arg: bool,
    prompter: &mut Option<crate::interactive::InteractivePrompter>,
) -> PaxResult<()> {
    use crate::interactive::RenameResult;
    use std::fs;
    #[cfg(unix)]
    use std::os::unix::fs::MetadataExt;

    // Get metadata
    let follow = options.dereference || (is_cli_arg && options.cli_dereference);
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

    // Handle interactive rename
    let archive_path = if let Some(ref mut p) = prompter {
        let path_str = path.to_string_lossy();
        match p.prompt(&path_str)? {
            RenameResult::Skip => return Ok(()),
            RenameResult::UseOriginal => path.to_path_buf(),
            RenameResult::Rename(new_path) => new_path,
        }
    } else {
        path.to_path_buf()
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
        // Handle special file types
        write_special(archive, &archive_path, &metadata)?;
    }

    Ok(())
}

/// Write a directory and its contents
fn write_directory<W: ArchiveWriter>(
    archive: &mut W,
    src_path: &std::path::Path,
    archive_path: &std::path::Path,
    metadata: &std::fs::Metadata,
    options: &WriteOptions,
    link_tracker: &mut HardLinkTracker,
    prompter: &mut Option<crate::interactive::InteractivePrompter>,
) -> PaxResult<()> {
    use crate::archive::EntryType;
    use std::fs;
    #[cfg(unix)]
    use std::os::unix::fs::MetadataExt;

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
    archive_path: &std::path::Path,
    metadata: &std::fs::Metadata,
    src_path: &std::path::Path,
) -> PaxResult<()> {
    use crate::archive::EntryType;
    use std::fs;

    let target = fs::read_link(src_path)?;
    let target_str = target.to_string_lossy();
    let mut entry = build_entry(archive_path, metadata, EntryType::Symlink)?;
    entry.link_target = Some(target.clone());
    entry.size = target_str.len() as u64;

    archive.write_entry(&entry)?;
    archive.write_data(target_str.as_bytes())?;
    archive.finish_entry()?;

    Ok(())
}

/// Write a regular file
fn write_file<W: ArchiveWriter>(
    archive: &mut W,
    src_path: &std::path::Path,
    archive_path: &std::path::Path,
    metadata: &std::fs::Metadata,
    link_tracker: &mut HardLinkTracker,
    options: &WriteOptions,
) -> PaxResult<()> {
    use crate::archive::EntryType;
    use std::io::Read as IoRead;
    #[cfg(unix)]
    use std::os::unix::fs::MetadataExt;

    // Save access time if we need to reset it after reading
    #[cfg(unix)]
    let original_atime = if options.reset_atime {
        Some((metadata.atime(), metadata.atime_nsec()))
    } else {
        None
    };

    let mut entry = build_entry(archive_path, metadata, EntryType::Regular)?;

    #[cfg(unix)]
    {
        entry.dev = metadata.dev();
        entry.ino = metadata.ino();
        entry.nlink = metadata.nlink() as u32;
    }

    // Check for hard link
    if let Some(original_path) = link_tracker.check(&entry) {
        entry.entry_type = EntryType::Hardlink;
        entry.link_target = Some(original_path);
        entry.size = 0;

        archive.write_entry(&entry)?;
        archive.finish_entry()?;
        return Ok(());
    }

    // Write regular file
    archive.write_entry(&entry)?;

    // Copy file contents
    let mut file = File::open(src_path)?;
    let mut buf = [0u8; 8192];
    loop {
        let n = file.read(&mut buf)?;
        if n == 0 {
            break;
        }
        archive.write_data(&buf[..n])?;
    }

    archive.finish_entry()?;

    // Reset access time if requested
    #[cfg(unix)]
    if let Some((atime_sec, atime_nsec)) = original_atime {
        reset_atime(src_path, atime_sec, atime_nsec);
    }

    Ok(())
}

/// Write a special file (block device, char device, fifo, socket)
#[cfg(unix)]
fn write_special<W: ArchiveWriter>(
    archive: &mut W,
    path: &std::path::Path,
    metadata: &std::fs::Metadata,
) -> PaxResult<()> {
    use crate::archive::EntryType;
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
    path: &std::path::Path,
    _metadata: &std::fs::Metadata,
) -> PaxResult<()> {
    eprintln!(
        "pax: {}: special files not supported on this platform",
        path.display()
    );
    Ok(())
}

/// Build an ArchiveEntry from path and metadata
fn build_entry(
    path: &std::path::Path,
    metadata: &std::fs::Metadata,
    entry_type: crate::archive::EntryType,
) -> PaxResult<crate::archive::ArchiveEntry> {
    use crate::archive::ArchiveEntry;
    #[cfg(unix)]
    use std::os::unix::fs::MetadataExt;

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
        if entry_type == crate::archive::EntryType::BlockDevice
            || entry_type == crate::archive::EntryType::CharDevice
        {
            let rdev = metadata.rdev() as libc::dev_t;
            entry.devmajor = libc::major(rdev) as u32;
            entry.devminor = libc::minor(rdev) as u32;
        }

        // Get user/group names
        entry.uname = get_username(entry.uid);
        entry.gname = get_groupname(entry.gid);
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

    if entry_type == crate::archive::EntryType::Regular {
        entry.size = metadata.len();
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

/// Reset access time of a file to the specified time
#[cfg(unix)]
fn reset_atime(path: &std::path::Path, atime_sec: i64, atime_nsec: i64) {
    use std::ffi::CString;
    use std::fs;
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::MetadataExt;

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
