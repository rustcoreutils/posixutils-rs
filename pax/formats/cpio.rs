//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX cpio (octet-oriented) format implementation
//!
//! Header format (76 bytes):
//! - c_magic:     6 bytes "070707"
//! - c_dev:       6 bytes (octal)
//! - c_ino:       6 bytes (octal)
//! - c_mode:      6 bytes (octal)
//! - c_uid:       6 bytes (octal)
//! - c_gid:       6 bytes (octal)
//! - c_nlink:     6 bytes (octal)
//! - c_rdev:      6 bytes (octal)
//! - c_mtime:    11 bytes (octal)
//! - c_namesize:  6 bytes (octal)
//! - c_filesize: 11 bytes (octal)
//!
//! Followed by c_namesize bytes of filename (including NUL)
//! Followed by c_filesize bytes of file data

use crate::archive::{ArchiveEntry, ArchiveReader, ArchiveWriter, EntryType};
use crate::error::{PaxError, PaxResult};
use std::io::{Read, Write};
use std::path::PathBuf;

const HEADER_SIZE: usize = 76;
const MAGIC: &[u8; 6] = b"070707";
const TRAILER: &str = "TRAILER!!!";

// c_mode file type bits
const C_ISREG: u32 = 0o100000;
const C_ISDIR: u32 = 0o040000;
const C_ISLNK: u32 = 0o120000;
const C_ISBLK: u32 = 0o060000;
const C_ISCHR: u32 = 0o020000;
const C_ISFIFO: u32 = 0o010000;
const C_ISSOCK: u32 = 0o140000;

// c_mode permission mask
const C_PERM_MASK: u32 = 0o7777;

/// cpio archive reader
pub struct CpioReader<R: Read> {
    reader: R,
    current_size: u64,
    bytes_read: u64,
    finished: bool,
}

impl<R: Read> CpioReader<R> {
    /// Create a new cpio reader
    pub fn new(reader: R) -> Self {
        CpioReader {
            reader,
            current_size: 0,
            bytes_read: 0,
            finished: false,
        }
    }

    /// Read exactly n bytes
    fn read_exact(&mut self, buf: &mut [u8]) -> PaxResult<()> {
        self.reader.read_exact(buf)?;
        Ok(())
    }
}

impl<R: Read> ArchiveReader for CpioReader<R> {
    fn read_entry(&mut self) -> PaxResult<Option<ArchiveEntry>> {
        if self.finished {
            return Ok(None);
        }

        // Skip any remaining data from previous entry
        self.skip_data()?;

        // Read header
        let mut header = [0u8; HEADER_SIZE];
        if let Err(e) = self.read_exact(&mut header) {
            if e.to_string().contains("unexpected end of file") {
                return Ok(None);
            }
            return Err(e);
        }

        // Verify magic
        if &header[0..6] != MAGIC {
            return Err(PaxError::InvalidFormat("bad cpio magic".to_string()));
        }

        // Parse header fields
        let entry = parse_header(&header, &mut self.reader)?;

        // Check for trailer
        if entry.path.to_string_lossy() == TRAILER {
            self.finished = true;
            return Ok(None);
        }

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
        let remaining = self.current_size - self.bytes_read;
        if remaining == 0 {
            return Ok(());
        }

        skip_bytes(&mut self.reader, remaining)?;
        self.bytes_read = self.current_size;
        Ok(())
    }
}

/// cpio archive writer
pub struct CpioWriter<W: Write> {
    writer: W,
    bytes_written: u64,
    current_size: u64,
    inode_counter: u64,
}

impl<W: Write> CpioWriter<W> {
    /// Create a new cpio writer
    pub fn new(writer: W) -> Self {
        CpioWriter {
            writer,
            bytes_written: 0,
            current_size: 0,
            inode_counter: 1,
        }
    }

    /// Get next inode number
    fn next_inode(&mut self) -> u64 {
        let ino = self.inode_counter;
        self.inode_counter += 1;
        ino
    }
}

impl<W: Write> ArchiveWriter for CpioWriter<W> {
    fn write_entry(&mut self, entry: &ArchiveEntry) -> PaxResult<()> {
        let ino = if entry.ino == 0 {
            self.next_inode()
        } else {
            entry.ino
        };

        let header = build_header(entry, ino)?;
        self.writer.write_all(&header)?;

        // Write filename including NUL
        let name = entry.path.to_string_lossy();
        self.writer.write_all(name.as_bytes())?;
        self.writer.write_all(&[0])?;

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
        // cpio doesn't need padding between entries
        Ok(())
    }

    fn finish(&mut self) -> PaxResult<()> {
        // Write trailer entry
        let trailer = ArchiveEntry::new(PathBuf::from(TRAILER), EntryType::Regular);
        self.write_entry(&trailer)?;
        self.writer.flush()?;
        Ok(())
    }
}

// ============================================================================
// Header parsing functions
// ============================================================================

/// Parse a cpio header
fn parse_header<R: Read>(header: &[u8; HEADER_SIZE], reader: &mut R) -> PaxResult<ArchiveEntry> {
    let dev = parse_octal_field(&header[6..12])?;
    let ino = parse_octal_field(&header[12..18])?;
    let mode = parse_octal_field(&header[18..24])? as u32;
    let uid = parse_octal_field(&header[24..30])? as u32;
    let gid = parse_octal_field(&header[30..36])? as u32;
    let nlink = parse_octal_field(&header[36..42])? as u32;
    let rdev = parse_octal_field(&header[42..48])?;
    let mtime = parse_octal_field(&header[48..59])?;
    let namesize = parse_octal_field(&header[59..65])? as usize;
    let filesize = parse_octal_field(&header[65..76])?;

    // Read filename
    let mut name_buf = vec![0u8; namesize];
    reader.read_exact(&mut name_buf)?;

    // Remove trailing NUL
    let name = parse_name(&name_buf);
    let path = PathBuf::from(name);

    let entry_type = parse_mode_type(mode);

    // For symlinks, the file data is the link target - read it now
    let link_target = if entry_type == EntryType::Symlink && filesize > 0 {
        let mut target_buf = vec![0u8; filesize as usize];
        reader.read_exact(&mut target_buf)?;
        Some(PathBuf::from(parse_name(&target_buf)))
    } else {
        None
    };

    // If we read the symlink target, size for data is now 0
    let size = if entry_type == EntryType::Symlink {
        0
    } else {
        filesize
    };

    // Extract device major/minor from rdev (for block/char devices)
    // Traditional cpio packs major in high bits, minor in low bits
    let devmajor = ((rdev >> 8) & 0xff) as u32;
    let devminor = (rdev & 0xff) as u32;

    Ok(ArchiveEntry {
        path,
        mode: mode & C_PERM_MASK,
        uid,
        gid,
        size,
        mtime,
        entry_type,
        link_target,
        dev,
        ino,
        nlink,
        devmajor,
        devminor,
        ..Default::default()
    })
}

/// Parse an octal field from bytes
fn parse_octal_field(bytes: &[u8]) -> PaxResult<u64> {
    let s = std::str::from_utf8(bytes)
        .map_err(|_| PaxError::InvalidHeader("invalid octal field".to_string()))?;
    let s = s.trim();
    // Reject if the octal string contains a sign
    if s.starts_with('+') || s.starts_with('-') {
        return Err(PaxError::InvalidHeader(format!("invalid octal: {}", s)));
    }
    u64::from_str_radix(s, 8).map_err(|_| PaxError::InvalidHeader(format!("invalid octal: {}", s)))
}

/// Parse filename, removing NUL terminator
fn parse_name(bytes: &[u8]) -> String {
    let end = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
    String::from_utf8_lossy(&bytes[..end]).to_string()
}

/// Parse file type from c_mode
fn parse_mode_type(mode: u32) -> EntryType {
    let type_bits = mode & 0o170000;
    match type_bits {
        C_ISDIR => EntryType::Directory,
        C_ISLNK => EntryType::Symlink,
        C_ISBLK => EntryType::BlockDevice,
        C_ISCHR => EntryType::CharDevice,
        C_ISFIFO => EntryType::Fifo,
        C_ISSOCK => EntryType::Socket,
        C_ISREG => EntryType::Regular,
        _ => EntryType::Regular,
    }
}

// ============================================================================
// Header building functions
// ============================================================================

/// Build a cpio header
fn build_header(entry: &ArchiveEntry, ino: u64) -> PaxResult<Vec<u8>> {
    let mut header = Vec::with_capacity(HEADER_SIZE);

    // c_magic
    header.extend_from_slice(MAGIC);

    // c_dev
    write_octal_field(&mut header, entry.dev, 6);

    // c_ino
    write_octal_field(&mut header, ino, 6);

    // c_mode (file type + permissions)
    let mode = build_mode(entry);
    write_octal_field(&mut header, mode as u64, 6);

    // c_uid
    write_octal_field(&mut header, entry.uid as u64, 6);

    // c_gid
    write_octal_field(&mut header, entry.gid as u64, 6);

    // c_nlink
    write_octal_field(&mut header, entry.nlink as u64, 6);

    // c_rdev (device major/minor for block/char devices)
    let rdev = if entry.is_device() {
        ((entry.devmajor as u64 & 0xff) << 8) | (entry.devminor as u64 & 0xff)
    } else {
        0
    };
    write_octal_field(&mut header, rdev, 6);

    // c_mtime
    write_octal_field(&mut header, entry.mtime, 11);

    // c_namesize (including NUL)
    let namesize = entry.path.to_string_lossy().len() + 1;
    write_octal_field(&mut header, namesize as u64, 6);

    // c_filesize
    write_octal_field(&mut header, entry.size, 11);

    Ok(header)
}

/// Build c_mode from entry
fn build_mode(entry: &ArchiveEntry) -> u32 {
    let type_bits = match entry.entry_type {
        EntryType::Regular => C_ISREG,
        EntryType::Directory => C_ISDIR,
        EntryType::Symlink => C_ISLNK,
        EntryType::Hardlink => C_ISREG, // Hard links are stored as regular files
        EntryType::BlockDevice => C_ISBLK,
        EntryType::CharDevice => C_ISCHR,
        EntryType::Fifo => C_ISFIFO,
        EntryType::Socket => C_ISSOCK,
    };
    type_bits | (entry.mode & C_PERM_MASK)
}

/// Write an octal field with exact width
fn write_octal_field(buf: &mut Vec<u8>, val: u64, width: usize) {
    let s = format!("{:0width$o}", val, width = width);
    // Take last 'width' characters
    let bytes = s.as_bytes();
    if bytes.len() >= width {
        buf.extend_from_slice(&bytes[bytes.len() - width..]);
    } else {
        // Pad with zeros
        for _ in 0..(width - bytes.len()) {
            buf.push(b'0');
        }
        buf.extend_from_slice(bytes);
    }
}

// ============================================================================
// Utility functions
// ============================================================================

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
    fn test_parse_octal_field() {
        assert_eq!(parse_octal_field(b"000644").unwrap(), 0o644);
        assert_eq!(parse_octal_field(b"000755").unwrap(), 0o755);
        assert_eq!(parse_octal_field(b"000000").unwrap(), 0);
    }

    #[test]
    fn test_parse_name() {
        assert_eq!(parse_name(b"hello\0"), "hello");
        assert_eq!(parse_name(b"test"), "test");
    }

    #[test]
    fn test_parse_mode_type() {
        assert_eq!(parse_mode_type(C_ISREG | 0o644), EntryType::Regular);
        assert_eq!(parse_mode_type(C_ISDIR | 0o755), EntryType::Directory);
        assert_eq!(parse_mode_type(C_ISLNK | 0o777), EntryType::Symlink);
    }

    #[test]
    fn test_write_octal_field() {
        let mut buf = Vec::new();
        write_octal_field(&mut buf, 0o644, 6);
        assert_eq!(&buf, b"000644");

        let mut buf = Vec::new();
        write_octal_field(&mut buf, 0, 6);
        assert_eq!(&buf, b"000000");
    }

    #[test]
    fn test_build_mode() {
        let entry = ArchiveEntry {
            path: PathBuf::from("test"),
            mode: 0o644,
            entry_type: EntryType::Regular,
            ..Default::default()
        };
        let mode = build_mode(&entry);
        assert_eq!(mode & 0o170000, C_ISREG);
        assert_eq!(mode & 0o7777, 0o644);
    }
}
