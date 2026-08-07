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

use crate::archive::ArchiveFormat;
use crate::blocked_io::BlockedWriter;
use crate::error::{PaxError, PaxResult};
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
    requested_format: Option<ArchiveFormat>,
    record_size: usize,
) -> PaxResult<()> {
    // Open archive for read+write
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(archive_path)?;

    // Detect the archive format
    let format = detect_format(&mut file)?;

    // Per POSIX, an explicit `-x` that names a format different from the existing
    // archive is an error — pax must not silently coerce the new members into the
    // archive's format.
    if let Some(requested) = requested_format {
        if requested != format {
            return Err(PaxError::InvalidFormat(format!(
                "cannot append in {} format to an existing {} archive",
                requested, format
            )));
        }
    }

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

    // Append through the same traversal engine that -w uses. This file used to
    // carry its own 441-line copy of it, which had gone stale: it had never
    // gained -s substitutions, -o invalid= handling, -o linkdata, or the
    // sub-second/atime/ctime fields, and it built the pax writer with
    // PaxWriter::new so every -o option was discarded.
    {
        let blocked = BlockedWriter::new(&mut file, record_size);
        crate::modes::write::create_archive(blocked, files, format, options)?;
    }

    // Discard whatever remains of the old archive. The previous end-of-archive
    // marker and its record padding sit beyond what we just wrote whenever the
    // appended members are shorter, and would otherwise survive as trailing
    // garbage past the new marker.
    let end = file.stream_position()?;
    file.set_len(end)?;

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
