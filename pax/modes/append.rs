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

use crate::archive::{ArchiveFormat, ArchiveReader};
use crate::blocked_io::BlockedWriter;
use crate::error::{PaxError, PaxResult};
use crate::formats::{CpioReader, PaxReader};
use crate::modes::write::WriteOptions;
use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom};
use std::path::{Path, PathBuf};

const BLOCK_SIZE: usize = 512;

/// Append files to an existing archive
pub fn append_to_archive(
    archive_path: &PathBuf,
    files: &[PathBuf],
    options: &mut WriteOptions,
    requested_format: Option<ArchiveFormat>,
    record_size: usize,
    update: bool,
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

    // -u needs the times the archive already records. The decision itself is
    // made per member during the traversal below, not on the operands: an
    // operand is usually a directory, and what has to be compared is each
    // member name it expands to, after -s has had its say.
    if update {
        options.update_times = Some(archived_mtimes(archive_path, format)?);
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

/// The latest modification time recorded for each member name in the archive.
///
/// A name can appear more than once -- that is what appending does -- and the
/// most recent copy is the one an extraction would produce, so it is the one
/// `-u` has to compare against.
fn archived_mtimes(archive_path: &Path, format: ArchiveFormat) -> PaxResult<HashMap<PathBuf, u64>> {
    let file = File::open(archive_path)?;
    let mut mtimes: HashMap<PathBuf, u64> = HashMap::new();

    fn collect<R: ArchiveReader>(
        archive: &mut R,
        mtimes: &mut HashMap<PathBuf, u64>,
    ) -> PaxResult<()> {
        while let Some(entry) = archive.read_entry()? {
            // A directory is stored as "dir/" but named "dir" while being
            // written, so the trailing slash comes off here and the lookup
            // side spells it the same way.
            let name = entry.path.to_string_lossy();
            let key = PathBuf::from(name.trim_end_matches('/'));
            mtimes
                .entry(key)
                .and_modify(|t| *t = std::cmp::max(*t, entry.mtime))
                .or_insert(entry.mtime);
            archive.skip_data()?;
        }
        Ok(())
    }

    match format {
        // Read with the superset reader whatever the archive's identity is: a
        // pax archive writes an extended header only for the members that need
        // one, so reading a tar-family archive as plain ustar turns any 'x'
        // block it does contain into a member of its own and truncates the name
        // of the member it described.
        ArchiveFormat::Ustar | ArchiveFormat::Pax => {
            collect(&mut PaxReader::new(file), &mut mtimes)?
        }
        ArchiveFormat::Cpio => collect(&mut CpioReader::new(file), &mut mtimes)?,
    }

    Ok(mtimes)
}

/// What the archive already *is*, which is what new members must be written as
/// and what an explicit `-x` has to match.
///
/// Deliberately not the same question the reader asks. Any ustar-magic archive
/// is *read* as pax, because a pax archive is a ustar archive with extra headers
/// and the pax reader handles a member without them identically. Appending has
/// to preserve what is there instead: writing pax members into a plain ustar
/// archive would change its format, and refusing to append ustar members to one
/// would be wrong the other way.
///
/// So the family comes from the first block, and within the tar family the
/// archive counts as pax only if it actually carries an extended header. Judging
/// that from the first block alone was the bug: a pax archive whose first member
/// needs no extended header begins with an ordinary ustar header, so appending
/// to it refused an explicit `-x pax` and otherwise wrote ustar members, which
/// silently drops any name too long for a ustar header.
fn detect_format(file: &mut File) -> PaxResult<ArchiveFormat> {
    let mut header = [0u8; BLOCK_SIZE];
    file.read_exact(&mut header)?;
    file.seek(SeekFrom::Start(0))?;

    let family = crate::detect_format_from_bytes(&header)?;
    if family == ArchiveFormat::Cpio {
        return Ok(family);
    }

    let has_extended = archive_has_extended_header(file)?;
    file.seek(SeekFrom::Start(0))?;
    Ok(if has_extended {
        ArchiveFormat::Pax
    } else {
        ArchiveFormat::Ustar
    })
}

/// Whether any member of a tar-family archive carries an extended header.
///
/// Walks the member headers, stepping over each one's data by the size it
/// records, so that a byte sequence inside a file's contents is never mistaken
/// for a header.
fn archive_has_extended_header(file: &mut File) -> PaxResult<bool> {
    file.seek(SeekFrom::Start(0))?;
    loop {
        let mut header = [0u8; BLOCK_SIZE];
        match file.read_exact(&mut header) {
            Ok(()) => {}
            // A truncated archive is not this function's business to diagnose.
            Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => return Ok(false),
            Err(e) => return Err(e.into()),
        }

        // A zero block ends the archive.
        if header.iter().all(|&b| b == 0) {
            return Ok(false);
        }

        if header[156] == b'x' || header[156] == b'g' {
            return Ok(true);
        }

        let size = parse_octal_size(&header[124..136]);
        let blocks = size.div_ceil(BLOCK_SIZE as u64);
        if blocks > 0 {
            file.seek(SeekFrom::Current((blocks * BLOCK_SIZE as u64) as i64))?;
        }
    }
}

/// The size field of a tar header, which is octal digits padded with NUL or
/// space. An unparsable field reads as zero, which only costs an early stop.
fn parse_octal_size(field: &[u8]) -> u64 {
    let mut value: u64 = 0;
    for &b in field {
        match b {
            b'0'..=b'7' => value = value.saturating_mul(8).saturating_add((b - b'0') as u64),
            _ => break,
        }
    }
    value
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
