//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX cpio format implementation
//!
//! Supports two cpio variants:
//!
//! ## ODC format (POSIX octet-oriented, magic "070707")
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
//! Followed by c_namesize bytes of filename (including NUL),
//! then c_filesize bytes of file data.
//!
//! ## newc format (SVR4 new ASCII, magic "070701" or "070702")
//! Header format (110 bytes):
//! - c_magic:     6 bytes "070701" or "070702"
//! - c_ino:       8 bytes (hex)
//! - c_mode:      8 bytes (hex)
//! - c_uid:       8 bytes (hex)
//! - c_gid:       8 bytes (hex)
//! - c_nlink:     8 bytes (hex)
//! - c_mtime:     8 bytes (hex)
//! - c_filesize:  8 bytes (hex)
//! - c_devmajor:  8 bytes (hex)
//! - c_devminor:  8 bytes (hex)
//! - c_rdevmajor: 8 bytes (hex)
//! - c_rdevminor: 8 bytes (hex)
//! - c_namesize:  8 bytes (hex)
//! - c_check:     8 bytes (hex) - always 0 for 070701, CRC for 070702
//!
//! Followed by filename padded to 4-byte boundary,
//! then file data padded to 4-byte boundary.

use crate::archive::{ArchiveEntry, ArchiveReader, ArchiveWriter, EntryType};
use crate::error::{PaxError, PaxResult, is_eof_error};
use std::io::{Read, Write};
use std::path::PathBuf;

// ODC format constants
const ODC_HEADER_SIZE: usize = 76;
const ODC_MAGIC: &[u8; 6] = b"070707";

// newc format constants
const NEWC_HEADER_SIZE: usize = 110;
const NEWC_MAGIC: &[u8; 6] = b"070701";
const NEWC_CRC_MAGIC: &[u8; 6] = b"070702";

// Binary cpio format constants
const BIN_HEADER_SIZE: usize = 26;
const BIN_MAGIC: u16 = 0o070707; // Binary magic as 16-bit value

const TRAILER: &str = "TRAILER!!!";

/// cpio format variant
#[derive(Debug, Clone, Copy, PartialEq)]
enum CpioFormat {
    /// POSIX octet-oriented (odc) format
    Odc,
    /// SVR4 new ASCII format (newc)
    Newc,
    /// Old binary format
    Binary,
}

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
    format: Option<CpioFormat>,
    /// Padding bytes remaining after file data (newc format)
    data_padding: u64,
}

impl<R: Read> CpioReader<R> {
    /// Create a new cpio reader
    pub fn new(reader: R) -> Self {
        CpioReader {
            reader,
            current_size: 0,
            bytes_read: 0,
            finished: false,
            format: None,
            data_padding: 0,
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

        // Read first 2 bytes to check for binary format
        let mut magic2 = [0u8; 2];
        if let Err(e) = self.read_exact(&mut magic2) {
            if is_eof_error(&e) {
                return Ok(None);
            }
            return Err(e);
        }

        // Check for binary cpio format (2-byte magic)
        let magic16_le = u16::from_le_bytes(magic2);
        let magic16_be = u16::from_be_bytes(magic2);
        let is_binary = magic16_le == BIN_MAGIC || magic16_be == BIN_MAGIC;
        let is_swapped = magic16_be == BIN_MAGIC && magic16_le != BIN_MAGIC;

        let (format, entry, data_padding) = if is_binary {
            // Binary format - read remaining 24 bytes of header
            let mut header = [0u8; BIN_HEADER_SIZE - 2];
            self.read_exact(&mut header)?;
            let (entry, padding) = parse_bin_header(&header, is_swapped, &mut self.reader)?;
            (CpioFormat::Binary, entry, padding)
        } else {
            // ASCII format - read remaining 4 bytes to complete 6-byte magic
            let mut magic_rest = [0u8; 4];
            self.read_exact(&mut magic_rest)?;
            let mut magic6 = [0u8; 6];
            magic6[..2].copy_from_slice(&magic2);
            magic6[2..].copy_from_slice(&magic_rest);

            if &magic6 == ODC_MAGIC {
                // ODC format - read remaining 70 bytes of header
                let mut header = [0u8; ODC_HEADER_SIZE - 6];
                self.read_exact(&mut header)?;
                let entry = parse_odc_header(&header, &mut self.reader)?;
                (CpioFormat::Odc, entry, 0)
            } else if &magic6 == NEWC_MAGIC || &magic6 == NEWC_CRC_MAGIC {
                // newc format - read remaining 104 bytes of header
                let mut header = [0u8; NEWC_HEADER_SIZE - 6];
                self.read_exact(&mut header)?;
                let (entry, padding) = parse_newc_header(&header, &mut self.reader)?;
                (CpioFormat::Newc, entry, padding)
            } else {
                return Err(PaxError::InvalidFormat(format!(
                    "bad cpio magic: {:?}",
                    String::from_utf8_lossy(&magic6)
                )));
            }
        };
        self.format = Some(format);

        // Check for trailer
        if entry.path.to_string_lossy() == TRAILER {
            self.finished = true;
            return Ok(None);
        }

        self.current_size = entry.size;
        self.bytes_read = 0;
        self.data_padding = data_padding;

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
        if remaining > 0 {
            skip_bytes(&mut self.reader, remaining)?;
            self.bytes_read = self.current_size;
        }

        // Skip padding (newc format pads data to 4-byte boundary)
        if self.data_padding > 0 {
            skip_bytes(&mut self.reader, self.data_padding)?;
            self.data_padding = 0;
        }
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

/// Parse ODC format header (magic already read)
/// header contains the 70 bytes after the 6-byte magic
fn parse_odc_header<R: Read>(header: &[u8], reader: &mut R) -> PaxResult<ArchiveEntry> {
    // ODC header layout (after 6-byte magic):
    // c_dev:      6 bytes (octal)  [0..6]
    // c_ino:      6 bytes (octal)  [6..12]
    // c_mode:     6 bytes (octal)  [12..18]
    // c_uid:      6 bytes (octal)  [18..24]
    // c_gid:      6 bytes (octal)  [24..30]
    // c_nlink:    6 bytes (octal)  [30..36]
    // c_rdev:     6 bytes (octal)  [36..42]
    // c_mtime:   11 bytes (octal)  [42..53]
    // c_namesize: 6 bytes (octal)  [53..59]
    // c_filesize:11 bytes (octal)  [59..70]
    let dev = parse_octal_field(&header[0..6])?;
    let ino = parse_octal_field(&header[6..12])?;
    let mode = parse_octal_field(&header[12..18])? as u32;
    let uid = parse_octal_field(&header[18..24])? as u32;
    let gid = parse_octal_field(&header[24..30])? as u32;
    let nlink = parse_octal_field(&header[30..36])? as u32;
    let rdev = parse_octal_field(&header[36..42])?;
    let mtime = parse_octal_field(&header[42..53])?;
    let namesize = parse_octal_field(&header[53..59])? as usize;
    let filesize = parse_octal_field(&header[59..70])?;

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

/// Parse newc format header (magic already read)
/// header contains the 104 bytes after the 6-byte magic
/// Returns (entry, data_padding) where data_padding is bytes to skip after file data
fn parse_newc_header<R: Read>(header: &[u8], reader: &mut R) -> PaxResult<(ArchiveEntry, u64)> {
    // newc header layout (after 6-byte magic):
    // c_ino:       8 bytes (hex)  [0..8]
    // c_mode:      8 bytes (hex)  [8..16]
    // c_uid:       8 bytes (hex)  [16..24]
    // c_gid:       8 bytes (hex)  [24..32]
    // c_nlink:     8 bytes (hex)  [32..40]
    // c_mtime:     8 bytes (hex)  [40..48]
    // c_filesize:  8 bytes (hex)  [48..56]
    // c_devmajor:  8 bytes (hex)  [56..64]
    // c_devminor:  8 bytes (hex)  [64..72]
    // c_rdevmajor: 8 bytes (hex)  [72..80]
    // c_rdevminor: 8 bytes (hex)  [80..88]
    // c_namesize:  8 bytes (hex)  [88..96]
    // c_check:     8 bytes (hex)  [96..104]
    let ino = parse_hex_field(&header[0..8])?;
    let mode = parse_hex_field(&header[8..16])? as u32;
    let uid = parse_hex_field(&header[16..24])? as u32;
    let gid = parse_hex_field(&header[24..32])? as u32;
    let nlink = parse_hex_field(&header[32..40])? as u32;
    let mtime = parse_hex_field(&header[40..48])?;
    let filesize = parse_hex_field(&header[48..56])?;
    let devmajor = parse_hex_field(&header[56..64])? as u32;
    let devminor = parse_hex_field(&header[64..72])? as u32;
    let rdevmajor = parse_hex_field(&header[72..80])? as u32;
    let rdevminor = parse_hex_field(&header[80..88])? as u32;
    let namesize = parse_hex_field(&header[88..96])? as usize;
    // c_check at [96..104] is ignored for reading

    // Read filename
    let mut name_buf = vec![0u8; namesize];
    reader.read_exact(&mut name_buf)?;

    // Calculate padding after filename (header + name must be 4-byte aligned)
    // Header is 110 bytes, so (110 + namesize) must be aligned to 4
    let header_plus_name = NEWC_HEADER_SIZE + namesize;
    let name_padding = (4 - (header_plus_name % 4)) % 4;
    if name_padding > 0 {
        let mut pad = vec![0u8; name_padding];
        reader.read_exact(&mut pad)?;
    }

    // Remove trailing NUL
    let name = parse_name(&name_buf);
    let path = PathBuf::from(name);

    let entry_type = parse_mode_type(mode);

    // For symlinks, the file data is the link target - read it now
    let (link_target, size, data_padding) = if entry_type == EntryType::Symlink && filesize > 0 {
        let mut target_buf = vec![0u8; filesize as usize];
        reader.read_exact(&mut target_buf)?;
        // Calculate padding after symlink data
        let symlink_padding = (4 - (filesize as usize % 4)) % 4;
        if symlink_padding > 0 {
            let mut pad = vec![0u8; symlink_padding];
            reader.read_exact(&mut pad)?;
        }
        (Some(PathBuf::from(parse_name(&target_buf))), 0, 0)
    } else {
        // Calculate padding after file data
        let file_padding = if filesize > 0 {
            ((4 - (filesize as usize % 4)) % 4) as u64
        } else {
            0
        };
        (None, filesize, file_padding)
    };

    // Construct dev from major/minor
    let dev = ((devmajor as u64) << 8) | (devminor as u64);

    Ok((
        ArchiveEntry {
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
            devmajor: rdevmajor,
            devminor: rdevminor,
            ..Default::default()
        },
        data_padding,
    ))
}

/// Parse binary cpio format header (magic already read)
/// header contains the 24 bytes after the 2-byte magic
/// Returns (entry, data_padding) where data_padding is bytes to skip after file data
fn parse_bin_header<R: Read>(
    header: &[u8],
    swapped: bool,
    reader: &mut R,
) -> PaxResult<(ArchiveEntry, u64)> {
    // Binary cpio header layout (after 2-byte magic):
    // c_dev:      2 bytes  [0..2]
    // c_ino:      2 bytes  [2..4]
    // c_mode:     2 bytes  [4..6]
    // c_uid:      2 bytes  [6..8]
    // c_gid:      2 bytes  [8..10]
    // c_nlink:    2 bytes  [10..12]
    // c_rdev:     2 bytes  [12..14]
    // c_mtime:    4 bytes  [14..18] (2x2 bytes, high then low)
    // c_namesize: 2 bytes  [18..20]
    // c_filesize: 4 bytes  [20..24] (2x2 bytes, high then low)

    let read_u16 = |offset: usize| -> u16 {
        let bytes = [header[offset], header[offset + 1]];
        if swapped {
            u16::from_be_bytes(bytes)
        } else {
            u16::from_le_bytes(bytes)
        }
    };

    let read_u32 = |offset: usize| -> u32 {
        // mtime and filesize are stored as two 16-bit words (high, low)
        let high = read_u16(offset) as u32;
        let low = read_u16(offset + 2) as u32;
        (high << 16) | low
    };

    let dev = read_u16(0) as u64;
    let ino = read_u16(2) as u64;
    let mode = read_u16(4) as u32;
    let uid = read_u16(6) as u32;
    let gid = read_u16(8) as u32;
    let nlink = read_u16(10) as u32;
    let rdev = read_u16(12) as u64;
    let mtime = read_u32(14) as u64;
    let namesize = read_u16(18) as usize;
    let filesize = read_u32(20) as u64;

    // Read filename (padded to word boundary)
    let mut name_buf = vec![0u8; namesize];
    reader.read_exact(&mut name_buf)?;

    // Skip padding after filename (header + name must be word-aligned)
    // Header is 26 bytes, so total = 26 + namesize, must be even
    let name_padding = (26 + namesize) % 2;
    if name_padding > 0 {
        let mut pad = [0u8; 1];
        reader.read_exact(&mut pad)?;
    }

    // Remove trailing NUL
    let name = parse_name(&name_buf);
    let path = PathBuf::from(name);

    let entry_type = parse_mode_type(mode);

    // For symlinks, the file data is the link target - read it now
    let (link_target, size, data_padding) = if entry_type == EntryType::Symlink && filesize > 0 {
        let mut target_buf = vec![0u8; filesize as usize];
        reader.read_exact(&mut target_buf)?;
        // Skip padding after symlink data
        let symlink_padding = (filesize as usize) % 2;
        if symlink_padding > 0 {
            let mut pad = [0u8; 1];
            reader.read_exact(&mut pad)?;
        }
        (Some(PathBuf::from(parse_name(&target_buf))), 0, 0)
    } else {
        // Calculate padding after file data (must be word-aligned)
        let file_padding = if filesize > 0 {
            ((filesize as usize) % 2) as u64
        } else {
            0
        };
        (None, filesize, file_padding)
    };

    // Extract device major/minor from rdev
    let devmajor = ((rdev >> 8) & 0xff) as u32;
    let devminor = (rdev & 0xff) as u32;

    Ok((
        ArchiveEntry {
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
        },
        data_padding,
    ))
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

/// Parse a hexadecimal field from bytes (for newc format)
fn parse_hex_field(bytes: &[u8]) -> PaxResult<u64> {
    let s = std::str::from_utf8(bytes)
        .map_err(|_| PaxError::InvalidHeader("invalid hex field".to_string()))?;
    u64::from_str_radix(s, 16).map_err(|_| PaxError::InvalidHeader(format!("invalid hex: {}", s)))
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

/// Build a cpio header (ODC format for writing)
fn build_header(entry: &ArchiveEntry, ino: u64) -> PaxResult<Vec<u8>> {
    let mut header = Vec::with_capacity(ODC_HEADER_SIZE);

    // c_magic
    header.extend_from_slice(ODC_MAGIC);

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
    fn test_parse_hex_field() {
        assert_eq!(parse_hex_field(b"000001A4").unwrap(), 0x1A4); // 420 decimal = 0644 octal
        assert_eq!(parse_hex_field(b"000001ED").unwrap(), 0x1ED); // 493 decimal = 0755 octal
        assert_eq!(parse_hex_field(b"00000000").unwrap(), 0);
        assert_eq!(parse_hex_field(b"FFFFFFFF").unwrap(), 0xFFFFFFFF);
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
