//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX ustar (tar) format implementation
//!
//! Header format (512 bytes):
//! - name:     100 bytes (offset 0)
//! - mode:       8 bytes (offset 100)
//! - uid:        8 bytes (offset 108)
//! - gid:        8 bytes (offset 116)
//! - size:      12 bytes (offset 124)
//! - mtime:     12 bytes (offset 136)
//! - chksum:     8 bytes (offset 148)
//! - typeflag:   1 byte  (offset 156)
//! - linkname: 100 bytes (offset 157)
//! - magic:      6 bytes (offset 257) "ustar\0"
//! - version:    2 bytes (offset 263) "00"
//! - uname:     32 bytes (offset 265)
//! - gname:     32 bytes (offset 297)
//! - devmajor:   8 bytes (offset 329)
//! - devminor:   8 bytes (offset 337)
//! - prefix:   155 bytes (offset 345)

use crate::archive::{ArchiveEntry, ArchiveReader, ArchiveWriter, EntryType};
use crate::error::{PaxError, PaxResult};
use std::io::{Read, Write};
use std::path::PathBuf;

const BLOCK_SIZE: usize = 512;
/// Static zero buffer for padding and end-of-archive markers
static ZERO_BLOCK: [u8; BLOCK_SIZE] = [0u8; BLOCK_SIZE];
const NAME_LEN: usize = 100;
const PREFIX_LEN: usize = 155;
const LINKNAME_LEN: usize = 100;
const UNAME_LEN: usize = 32;
const GNAME_LEN: usize = 32;

// Header field offsets
const NAME_OFF: usize = 0;
const MODE_OFF: usize = 100;
const UID_OFF: usize = 108;
const GID_OFF: usize = 116;
const SIZE_OFF: usize = 124;
const MTIME_OFF: usize = 136;
const CHKSUM_OFF: usize = 148;
const TYPEFLAG_OFF: usize = 156;
const LINKNAME_OFF: usize = 157;
const MAGIC_OFF: usize = 257;
const VERSION_OFF: usize = 263;
const UNAME_OFF: usize = 265;
const GNAME_OFF: usize = 297;
const PREFIX_OFF: usize = 345;

// Type flags
const REGTYPE: u8 = b'0';
const AREGTYPE: u8 = b'\0';
const LNKTYPE: u8 = b'1';
const SYMTYPE: u8 = b'2';
const CHRTYPE: u8 = b'3';
const BLKTYPE: u8 = b'4';
const DIRTYPE: u8 = b'5';
const FIFOTYPE: u8 = b'6';

// Device number field offsets and lengths
const DEVMAJOR_OFF: usize = 329;
const DEVMINOR_OFF: usize = 337;

/// ustar archive reader
pub struct UstarReader<R: Read> {
    reader: R,
    current_size: u64,
    bytes_read: u64,
}

impl<R: Read> UstarReader<R> {
    /// Create a new ustar reader
    pub fn new(reader: R) -> Self {
        UstarReader {
            reader,
            current_size: 0,
            bytes_read: 0,
        }
    }

    /// Read exactly n bytes
    fn read_exact(&mut self, buf: &mut [u8]) -> PaxResult<()> {
        self.reader.read_exact(buf)?;
        Ok(())
    }
}

impl<R: Read> ArchiveReader for UstarReader<R> {
    fn read_entry(&mut self) -> PaxResult<Option<ArchiveEntry>> {
        // Skip any remaining data from previous entry
        self.skip_data()?;

        let mut header = [0u8; BLOCK_SIZE];
        if let Err(e) = self.read_exact(&mut header) {
            if e.to_string().contains("unexpected end of file") {
                return Ok(None);
            }
            return Err(e);
        }

        // Check for end of archive (two zero blocks)
        if is_zero_block(&header) {
            return Ok(None);
        }

        // Verify checksum
        if !verify_checksum(&header) {
            return Err(PaxError::InvalidHeader("checksum mismatch".to_string()));
        }

        let entry = parse_header(&header)?;
        self.current_size = entry.size;
        self.bytes_read = 0;

        Ok(Some(entry))
    }

    fn read_data(&mut self, buf: &mut [u8]) -> PaxResult<usize> {
        let remaining = self.current_size - self.bytes_read;
        if remaining == 0 {
            return Ok(0);
        }

        let to_read = std::cmp::min(buf.len() as u64, remaining) as usize;
        let n = self.reader.read(&mut buf[..to_read])?;
        self.bytes_read += n as u64;
        Ok(n)
    }

    fn skip_data(&mut self) -> PaxResult<()> {
        // Calculate total bytes including padding to block boundary
        let total_bytes = round_up_block(self.current_size);
        let to_skip = total_bytes - self.bytes_read;

        if to_skip > 0 {
            skip_bytes(&mut self.reader, to_skip)?;
        }

        // Reset state - we've finished with this entry's data
        self.bytes_read = total_bytes;
        Ok(())
    }
}

/// ustar archive writer
pub struct UstarWriter<W: Write> {
    writer: W,
    bytes_written: u64,
    current_size: u64,
}

impl<W: Write> UstarWriter<W> {
    /// Create a new ustar writer
    pub fn new(writer: W) -> Self {
        UstarWriter {
            writer,
            bytes_written: 0,
            current_size: 0,
        }
    }
}

impl<W: Write> ArchiveWriter for UstarWriter<W> {
    fn write_entry(&mut self, entry: &ArchiveEntry) -> PaxResult<()> {
        let header = build_header(entry)?;
        self.writer.write_all(&header)?;
        self.bytes_written = 0;
        self.current_size = entry.size;
        Ok(())
    }

    fn write_data(&mut self, data: &[u8]) -> PaxResult<()> {
        self.writer.write_all(data)?;
        self.bytes_written += data.len() as u64;
        Ok(())
    }

    fn finish_entry(&mut self) -> PaxResult<()> {
        // Pad to block boundary using static zero buffer
        let padding = padding_needed(self.bytes_written);
        if padding > 0 {
            self.writer.write_all(&ZERO_BLOCK[..padding])?;
        }
        Ok(())
    }

    fn finish(&mut self) -> PaxResult<()> {
        // Write two zero blocks using static buffer
        self.writer.write_all(&ZERO_BLOCK)?;
        self.writer.write_all(&ZERO_BLOCK)?;
        self.writer.flush()?;
        Ok(())
    }
}

// ============================================================================
// Header parsing functions
// ============================================================================

/// Check if a block is all zeros
fn is_zero_block(block: &[u8]) -> bool {
    block.iter().all(|&b| b == 0)
}

/// Parse a header block into an ArchiveEntry
fn parse_header(header: &[u8; BLOCK_SIZE]) -> PaxResult<ArchiveEntry> {
    let name = parse_string(&header[NAME_OFF..NAME_OFF + NAME_LEN]);
    let prefix = parse_string(&header[PREFIX_OFF..PREFIX_OFF + PREFIX_LEN]);

    let path = build_path(&prefix, &name);

    let mode = parse_octal(&header[MODE_OFF..MODE_OFF + 8])? as u32;
    let uid = parse_octal(&header[UID_OFF..UID_OFF + 8])? as u32;
    let gid = parse_octal(&header[GID_OFF..GID_OFF + 8])? as u32;
    let size = parse_octal(&header[SIZE_OFF..SIZE_OFF + 12])?;
    let mtime = parse_octal(&header[MTIME_OFF..MTIME_OFF + 12])?;

    let typeflag = header[TYPEFLAG_OFF];
    let entry_type = parse_typeflag(typeflag)?;

    let linkname = parse_string(&header[LINKNAME_OFF..LINKNAME_OFF + LINKNAME_LEN]);
    let link_target = if !linkname.is_empty() {
        Some(PathBuf::from(linkname))
    } else {
        None
    };

    let uname = parse_string(&header[UNAME_OFF..UNAME_OFF + UNAME_LEN]);
    let gname = parse_string(&header[GNAME_OFF..GNAME_OFF + GNAME_LEN]);

    // Parse device major/minor for block/char devices
    let devmajor = parse_octal(&header[DEVMAJOR_OFF..DEVMAJOR_OFF + 8])? as u32;
    let devminor = parse_octal(&header[DEVMINOR_OFF..DEVMINOR_OFF + 8])? as u32;

    Ok(ArchiveEntry {
        path,
        mode,
        uid,
        gid,
        size,
        mtime,
        entry_type,
        link_target,
        uname: if uname.is_empty() { None } else { Some(uname) },
        gname: if gname.is_empty() { None } else { Some(gname) },
        devmajor,
        devminor,
        ..Default::default()
    })
}

/// Parse a NUL-terminated or space-padded string
fn parse_string(bytes: &[u8]) -> String {
    let end = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
    String::from_utf8_lossy(&bytes[..end])
        .trim_end()
        .to_string()
}

/// Parse an octal number from bytes
fn parse_octal(bytes: &[u8]) -> PaxResult<u64> {
    let s = parse_string(bytes);
    if s.is_empty() {
        return Ok(0);
    }
    // Reject if the octal string contains a sign
    if s.starts_with('+') || s.starts_with('-') {
        return Err(PaxError::InvalidHeader(format!("invalid octal: {}", s)));
    }
    u64::from_str_radix(&s, 8).map_err(|_| PaxError::InvalidHeader(format!("invalid octal: {}", s)))
}

/// Parse typeflag to EntryType
fn parse_typeflag(flag: u8) -> PaxResult<EntryType> {
    match flag {
        REGTYPE | AREGTYPE => Ok(EntryType::Regular),
        LNKTYPE => Ok(EntryType::Hardlink),
        SYMTYPE => Ok(EntryType::Symlink),
        CHRTYPE => Ok(EntryType::CharDevice),
        BLKTYPE => Ok(EntryType::BlockDevice),
        DIRTYPE => Ok(EntryType::Directory),
        FIFOTYPE => Ok(EntryType::Fifo),
        _ => Ok(EntryType::Regular), // Treat unknown as regular
    }
}

/// Build full path from prefix and name
fn build_path(prefix: &str, name: &str) -> PathBuf {
    if prefix.is_empty() {
        PathBuf::from(name)
    } else {
        PathBuf::from(format!("{}/{}", prefix, name))
    }
}

/// Verify header checksum
fn verify_checksum(header: &[u8; BLOCK_SIZE]) -> bool {
    let stored = match parse_octal(&header[CHKSUM_OFF..CHKSUM_OFF + 8]) {
        Ok(v) => v as u32,
        Err(_) => return false,
    };

    let calculated = calculate_checksum(header);
    stored == calculated
}

/// Calculate header checksum
fn calculate_checksum(header: &[u8; BLOCK_SIZE]) -> u32 {
    let mut sum: u32 = 0;
    for (i, &byte) in header.iter().enumerate() {
        if (CHKSUM_OFF..CHKSUM_OFF + 8).contains(&i) {
            sum += b' ' as u32;
        } else {
            sum += byte as u32;
        }
    }
    sum
}

// ============================================================================
// Header building functions
// ============================================================================

/// Build a header block from an ArchiveEntry
fn build_header(entry: &ArchiveEntry) -> PaxResult<[u8; BLOCK_SIZE]> {
    let mut header = [0u8; BLOCK_SIZE];

    // Split path into name and prefix if needed
    let (name, prefix) = split_path(entry)?;

    // Write fields
    write_string(&mut header[NAME_OFF..], &name, NAME_LEN);
    write_octal(&mut header[MODE_OFF..], entry.mode as u64, 8);
    write_octal(&mut header[UID_OFF..], entry.uid as u64, 8);
    write_octal(&mut header[GID_OFF..], entry.gid as u64, 8);
    write_octal(&mut header[SIZE_OFF..], entry.size, 12);
    write_octal(&mut header[MTIME_OFF..], entry.mtime, 12);

    // Typeflag
    header[TYPEFLAG_OFF] = entry_type_to_flag(&entry.entry_type);

    // Linkname
    if let Some(ref target) = entry.link_target {
        let link_str = target.to_string_lossy();
        if link_str.len() > LINKNAME_LEN {
            return Err(PaxError::PathTooLong(link_str.to_string()));
        }
        write_string(&mut header[LINKNAME_OFF..], &link_str, LINKNAME_LEN);
    }

    // Magic and version
    header[MAGIC_OFF..MAGIC_OFF + 6].copy_from_slice(b"ustar\0");
    header[VERSION_OFF..VERSION_OFF + 2].copy_from_slice(b"00");

    // uname and gname
    if let Some(ref uname) = entry.uname {
        write_string(&mut header[UNAME_OFF..], uname, UNAME_LEN);
    }
    if let Some(ref gname) = entry.gname {
        write_string(&mut header[GNAME_OFF..], gname, GNAME_LEN);
    }

    // Device major/minor for block/char devices
    if entry.is_device() {
        write_octal(&mut header[DEVMAJOR_OFF..], entry.devmajor as u64, 8);
        write_octal(&mut header[DEVMINOR_OFF..], entry.devminor as u64, 8);
    }

    // Prefix
    write_string(&mut header[PREFIX_OFF..], &prefix, PREFIX_LEN);

    // Calculate and write checksum
    let checksum = calculate_checksum(&header);
    write_octal(&mut header[CHKSUM_OFF..], checksum as u64, 8);

    Ok(header)
}

/// Split path into name (max 100) and prefix (max 155)
fn split_path(entry: &ArchiveEntry) -> PaxResult<(String, String)> {
    let path_str = entry.path.to_string_lossy();

    // Add trailing slash for directories
    let path_str = if entry.is_dir() && !path_str.ends_with('/') {
        format!("{}/", path_str)
    } else {
        path_str.to_string()
    };

    if path_str.len() <= NAME_LEN {
        return Ok((path_str, String::new()));
    }

    // Try to split at a '/' within bounds
    if path_str.len() <= NAME_LEN + PREFIX_LEN + 1 {
        // Find a split point
        for i in (1..=PREFIX_LEN).rev() {
            if i >= path_str.len() {
                continue;
            }
            if path_str.as_bytes()[i] == b'/' {
                let prefix = &path_str[..i];
                let name = &path_str[i + 1..];
                if name.len() <= NAME_LEN {
                    return Ok((name.to_string(), prefix.to_string()));
                }
            }
        }
    }

    Err(PaxError::PathTooLong(path_str))
}

/// Convert EntryType to typeflag
fn entry_type_to_flag(entry_type: &EntryType) -> u8 {
    match entry_type {
        EntryType::Regular => REGTYPE,
        EntryType::Directory => DIRTYPE,
        EntryType::Symlink => SYMTYPE,
        EntryType::Hardlink => LNKTYPE,
        EntryType::CharDevice => CHRTYPE,
        EntryType::BlockDevice => BLKTYPE,
        EntryType::Fifo => FIFOTYPE,
        EntryType::Socket => REGTYPE, // Sockets typically not stored; fallback to regular
    }
}

/// Write a string to a field, NUL-terminated if space permits
fn write_string(buf: &mut [u8], s: &str, max_len: usize) {
    let bytes = s.as_bytes();
    let len = std::cmp::min(bytes.len(), max_len);
    buf[..len].copy_from_slice(&bytes[..len]);
}

/// Write an octal number to a field
/// Format: leading zeros, octal digits, space or NUL terminator
fn write_octal(buf: &mut [u8], val: u64, width: usize) {
    // Format: (width-2) digits + space + NUL, or (width-1) digits + NUL
    // Standard format uses (width-1) octal digits followed by space or NUL
    let s = format!("{:0width$o} ", val, width = width - 2);
    let bytes = s.as_bytes();
    let len = std::cmp::min(bytes.len(), width);
    buf[..len].copy_from_slice(&bytes[..len]);
}

// ============================================================================
// Utility functions
// ============================================================================

/// Round up to next block boundary
fn round_up_block(size: u64) -> u64 {
    size.div_ceil(BLOCK_SIZE as u64) * BLOCK_SIZE as u64
}

/// Calculate padding needed to reach block boundary
fn padding_needed(bytes_written: u64) -> usize {
    let remainder = (bytes_written % BLOCK_SIZE as u64) as usize;
    if remainder == 0 {
        0
    } else {
        BLOCK_SIZE - remainder
    }
}

/// Skip bytes in a reader
fn skip_bytes<R: Read>(reader: &mut R, count: u64) -> PaxResult<()> {
    let mut remaining = count;
    let mut buf = [0u8; 4096];
    while remaining > 0 {
        let to_read = std::cmp::min(remaining, buf.len() as u64) as usize;
        reader.read_exact(&mut buf[..to_read])?;
        remaining -= to_read as u64;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_octal() {
        assert_eq!(parse_octal(b"000644 \0").unwrap(), 0o644);
        assert_eq!(parse_octal(b"0000755\0").unwrap(), 0o755);
        assert_eq!(parse_octal(b"       \0").unwrap(), 0);
    }

    #[test]
    fn test_parse_string() {
        assert_eq!(parse_string(b"hello\0\0\0\0\0"), "hello");
        assert_eq!(parse_string(b"test    "), "test");
        assert_eq!(parse_string(b"\0\0\0\0"), "");
    }

    #[test]
    fn test_split_path_short() {
        let entry = ArchiveEntry::new(PathBuf::from("short.txt"), EntryType::Regular);
        let (name, prefix) = split_path(&entry).unwrap();
        assert_eq!(name, "short.txt");
        assert_eq!(prefix, "");
    }

    #[test]
    fn test_checksum() {
        let mut header = [0u8; BLOCK_SIZE];
        header[NAME_OFF..NAME_OFF + 4].copy_from_slice(b"test");
        let checksum = calculate_checksum(&header);
        assert!(checksum > 0);
    }

    #[test]
    fn test_round_up_block() {
        assert_eq!(round_up_block(0), 0);
        assert_eq!(round_up_block(1), 512);
        assert_eq!(round_up_block(512), 512);
        assert_eq!(round_up_block(513), 1024);
    }

    #[test]
    fn test_padding_needed() {
        assert_eq!(padding_needed(0), 0);
        assert_eq!(padding_needed(100), 412);
        assert_eq!(padding_needed(512), 0);
        assert_eq!(padding_needed(600), 424);
    }
}
