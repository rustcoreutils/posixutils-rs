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
use crate::error::{is_eof_error, PaxError, PaxResult};
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CpioFormat {
    /// POSIX octet-oriented (odc) format
    #[default]
    Odc,
    /// SVR4 new ASCII format (newc)
    Newc,
    /// SVR4 new ASCII format with a data checksum (newc, magic 070702)
    NewcCrc,
    /// Old binary format
    Binary,
}

impl CpioFormat {
    /// Byte alignment the format requires for the header+name and the data.
    ///
    /// The ASCII odc format is unaligned; newc pads both to 4 bytes and the old
    /// binary format to the 2-byte word it stores its fields in.
    fn alignment(self) -> usize {
        match self {
            CpioFormat::Odc => 1,
            CpioFormat::Newc | CpioFormat::NewcCrc => 4,
            CpioFormat::Binary => 2,
        }
    }

    /// Fixed header size in bytes, excluding the pathname that follows it.
    fn header_size(self) -> usize {
        match self {
            CpioFormat::Odc => ODC_HEADER_SIZE,
            CpioFormat::Newc | CpioFormat::NewcCrc => NEWC_HEADER_SIZE,
            CpioFormat::Binary => BIN_HEADER_SIZE,
        }
    }
}

/// Fold `data` into a running cpio "crc" checksum.
///
/// Despite the name the 070702 format uses no CRC polynomial: c_check is simply
/// the sum of every data byte as an unsigned 32-bit quantity.
pub fn checksum_bytes(acc: u32, data: &[u8]) -> u32 {
    data.iter().fold(acc, |a, &b| a.wrapping_add(b as u32))
}

/// Bytes of padding needed to bring `len` up to a multiple of `align`.
fn pad_len(len: u64, align: u64) -> usize {
    ((align - (len % align)) % align) as usize
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
                let variant = if &magic6 == NEWC_CRC_MAGIC {
                    CpioFormat::NewcCrc
                } else {
                    CpioFormat::Newc
                };
                (variant, entry, padding)
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
    /// Header flavor emitted for every member
    format: CpioFormat,
    bytes_written: u64,
    current_size: u64,
    inode_counter: u64,
}

impl<W: Write> CpioWriter<W> {
    /// Create a new cpio writer emitting `format`
    pub fn with_format(writer: W, format: CpioFormat) -> Self {
        CpioWriter {
            writer,
            format,
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

    /// Emit `n` NUL bytes of alignment padding (`n` is under 4 by construction)
    fn pad(&mut self, n: usize) -> PaxResult<()> {
        const PADDING: [u8; 4] = [0; 4];
        if n > 0 {
            self.writer.write_all(&PADDING[..n])?;
        }
        Ok(())
    }
}

impl<W: Write> ArchiveWriter for CpioWriter<W> {
    /// cpio has no link typeflag: `build_mode` maps `EntryType::Hardlink` onto
    /// `C_ISREG`, and the reader hands it back as a plain regular file. Each
    /// link must therefore carry its own copy of the data.
    fn supports_hardlinks(&self) -> bool {
        false
    }

    fn needs_data_checksum(&self) -> bool {
        self.format == CpioFormat::NewcCrc
    }

    fn write_entry(&mut self, entry: &ArchiveEntry) -> PaxResult<()> {
        let ino = if entry.ino == 0 {
            self.next_inode()
        } else {
            entry.ino
        };

        // The name is stored NUL-terminated and c_namesize counts the NUL.
        let name = entry.path.to_string_lossy();
        let namesize = name.len() + 1;

        let header = match self.format {
            CpioFormat::Odc => build_odc_header(entry, ino, namesize)?,
            CpioFormat::Newc | CpioFormat::NewcCrc => {
                build_newc_header(entry, ino, namesize, self.format)?
            }
            CpioFormat::Binary => build_bin_header(entry, ino, namesize)?,
        };
        self.writer.write_all(&header)?;
        self.writer.write_all(name.as_bytes())?;
        self.writer.write_all(&[0])?;

        // newc aligns the header plus its name to 4 bytes, the old binary
        // format to 2; odc is unaligned and pads nothing.
        let align = self.format.alignment() as u64;
        let pad = pad_len((self.format.header_size() + namesize) as u64, align);
        self.pad(pad)?;

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
        // Pad against the declared size, not the bytes actually handed over:
        // that is what the reader derives its own padding from, so the two stay
        // in step even if a member was short.
        let align = self.format.alignment() as u64;
        let pad = pad_len(self.current_size, align);
        self.pad(pad)
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

/// Device major/minor packed into a single traditional cpio c_rdev field.
fn packed_rdev(entry: &ArchiveEntry) -> u64 {
    if entry.is_device() {
        ((entry.devmajor as u64 & 0xff) << 8) | (entry.devminor as u64 & 0xff)
    } else {
        0
    }
}

/// Link count as recorded in a header.
///
/// No real cpio ever writes zero here, and a reader that trusts the field would
/// see a file with no names at all, so synthesized entries (the trailer, and
/// anything built without stat data) are floored at one.
fn header_nlink(entry: &ArchiveEntry) -> u64 {
    std::cmp::max(entry.nlink, 1) as u64
}

/// Build a cpio ODC (POSIX octet-oriented) header
fn build_odc_header(entry: &ArchiveEntry, ino: u64, namesize: usize) -> PaxResult<Vec<u8>> {
    let mut header = Vec::with_capacity(ODC_HEADER_SIZE);

    // c_magic
    header.extend_from_slice(ODC_MAGIC);

    // c_dev (identity only; mask on overflow — large real dev numbers are normal)
    write_octal_field_masked(&mut header, entry.dev, 6);

    // c_ino (identity only; mask on overflow to preserve hard-link grouping)
    write_octal_field_masked(&mut header, ino, 6);

    // c_mode (file type + permissions)
    let mode = build_mode(entry);
    write_octal_field(&mut header, mode as u64, 6)?;

    // c_uid (mask on overflow — high uids exceed the ODC 6-digit width)
    write_octal_field_masked(&mut header, entry.uid as u64, 6);

    // c_gid (mask on overflow — high gids exceed the ODC 6-digit width)
    write_octal_field_masked(&mut header, entry.gid as u64, 6);

    // c_nlink
    write_octal_field(&mut header, entry.nlink as u64, 6)?;

    // c_rdev (device major/minor for block/char devices)
    write_octal_field(&mut header, packed_rdev(entry), 6)?;

    // c_mtime
    write_octal_field(&mut header, entry.mtime, 11)?;

    // c_namesize (including NUL)
    write_octal_field(&mut header, namesize as u64, 6)?;

    // c_filesize
    write_octal_field(&mut header, entry.size, 11)?;

    Ok(header)
}

/// Build a cpio SVR4 "newc" header, with or without the 070702 data checksum.
///
/// Every field is eight hex digits. The identity fields (c_ino, c_dev*) are
/// masked on overflow for the same reason as in ODC -- they only group hard
/// links -- while the fields the reader frames the stream with are rejected if
/// they do not fit.
fn build_newc_header(
    entry: &ArchiveEntry,
    ino: u64,
    namesize: usize,
    format: CpioFormat,
) -> PaxResult<Vec<u8>> {
    let mut header = Vec::with_capacity(NEWC_HEADER_SIZE);

    header.extend_from_slice(if format == CpioFormat::NewcCrc {
        NEWC_CRC_MAGIC
    } else {
        NEWC_MAGIC
    });

    write_hex_field_masked(&mut header, ino);
    write_hex_field(&mut header, build_mode(entry) as u64, "c_mode")?;
    write_hex_field_masked(&mut header, entry.uid as u64);
    write_hex_field_masked(&mut header, entry.gid as u64);
    write_hex_field(&mut header, header_nlink(entry), "c_nlink")?;
    write_hex_field(&mut header, entry.mtime, "c_mtime")?;
    write_hex_field(&mut header, entry.size, "c_filesize")?;

    // c_devmajor / c_devminor describe the filesystem the member came from;
    // parse_newc_header folds them back into ArchiveEntry::dev the same way.
    write_hex_field_masked(&mut header, (entry.dev >> 8) & 0xff);
    write_hex_field_masked(&mut header, entry.dev & 0xff);

    // c_rdevmajor / c_rdevminor describe the device a member *is*.
    let (rdevmajor, rdevminor) = if entry.is_device() {
        (entry.devmajor as u64, entry.devminor as u64)
    } else {
        (0, 0)
    };
    write_hex_field_masked(&mut header, rdevmajor);
    write_hex_field_masked(&mut header, rdevminor);

    write_hex_field(&mut header, namesize as u64, "c_namesize")?;

    // c_check is zero for 070701. For 070702 it is the unsigned sum of every
    // data byte, which the caller supplies because it precedes the data here.
    let check = match format {
        CpioFormat::NewcCrc => entry.data_checksum.unwrap_or(0) as u64,
        _ => 0,
    };
    write_hex_field(&mut header, check, "c_check")?;

    Ok(header)
}

/// Build an old binary cpio header.
///
/// Fields are 16-bit words in host byte order; c_mtime and c_filesize are two
/// such words, most significant first. Nothing wider than 16 bits (or 32 for
/// those two) can be expressed, which is why this format is only a sensible
/// default for the small trees historic cpio was used on.
fn build_bin_header(entry: &ArchiveEntry, ino: u64, namesize: usize) -> PaxResult<Vec<u8>> {
    let mut header = Vec::with_capacity(BIN_HEADER_SIZE);

    let mut push_u16 = |val: u64| header.extend_from_slice(&(val as u16).to_ne_bytes());

    push_u16(BIN_MAGIC as u64);
    push_u16(entry.dev & 0xffff);
    push_u16(ino & 0xffff);
    push_u16(build_mode(entry) as u64 & 0xffff);
    push_u16(entry.uid as u64 & 0xffff);
    push_u16(entry.gid as u64 & 0xffff);
    push_u16(header_nlink(entry) & 0xffff);
    push_u16(packed_rdev(entry));

    if entry.mtime > u32::MAX as u64 {
        return Err(PaxError::InvalidHeader(format!(
            "modification time {} does not fit the binary cpio format",
            entry.mtime
        )));
    }
    push_u16(entry.mtime >> 16);
    push_u16(entry.mtime & 0xffff);

    if namesize > u16::MAX as usize {
        return Err(PaxError::InvalidHeader(format!(
            "pathname of {} bytes does not fit the binary cpio format",
            namesize - 1
        )));
    }
    push_u16(namesize as u64);

    if entry.size > u32::MAX as u64 {
        return Err(PaxError::InvalidHeader(format!(
            "file size {} does not fit the binary cpio format",
            entry.size
        )));
    }
    push_u16(entry.size >> 16);
    push_u16(entry.size & 0xffff);

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

/// Write a stream-framing octal field (c_namesize, c_filesize, c_mtime) for the
/// cpio ODC header.
///
/// These fields determine how the reader frames the rest of the stream, so a
/// value too large for `width` octal digits is rejected with an error rather
/// than silently keeping only its low-order digits — truncation here would
/// mis-frame every following header (e.g. c_filesize for a file ≥8 GiB in the
/// 11-digit field).
fn write_octal_field(buf: &mut Vec<u8>, val: u64, width: usize) -> PaxResult<()> {
    let s = format!("{:0width$o}", val, width = width);
    if s.len() > width {
        return Err(PaxError::InvalidHeader(format!(
            "value {} too large for {}-digit cpio ODC field",
            val, width
        )));
    }
    buf.extend_from_slice(s.as_bytes());
    Ok(())
}

/// Write an identity octal field (c_dev, c_ino, c_uid, c_gid) for the cpio ODC
/// header, keeping only the low-order `width` octal digits on overflow.
///
/// Unlike the framing fields, these carry no stream-length information: c_dev /
/// c_ino exist only to associate hard links within the archive, and large real
/// device/inode numbers routinely exceed the ODC 6-digit width on modern
/// filesystems. Masking (the historical cpio behavior, matching GNU cpio's
/// `odc` format) preserves hard-link grouping without corrupting the stream or
/// failing on ordinary archives.
fn write_octal_field_masked(buf: &mut Vec<u8>, val: u64, width: usize) {
    let s = format!("{:0width$o}", val, width = width);
    let bytes = s.as_bytes();
    // Keep the last `width` digits (low-order), zero-filled if shorter.
    buf.extend_from_slice(&bytes[bytes.len() - width..]);
}

/// Width of every field in a cpio newc header.
const NEWC_FIELD_WIDTH: usize = 8;

/// Write an eight-digit hex newc field, rejecting a value that does not fit.
///
/// `field` names the header member for the diagnostic; it is used for the
/// fields whose value the reader needs verbatim to frame or restore the member.
fn write_hex_field(buf: &mut Vec<u8>, val: u64, field: &str) -> PaxResult<()> {
    let s = format!("{:0width$X}", val, width = NEWC_FIELD_WIDTH);
    if s.len() > NEWC_FIELD_WIDTH {
        return Err(PaxError::InvalidHeader(format!(
            "value {} too large for the {} field of the cpio newc format",
            val, field
        )));
    }
    buf.extend_from_slice(s.as_bytes());
    Ok(())
}

/// Write an eight-digit hex newc identity field, masking on overflow.
///
/// As with the ODC identity fields, c_ino / c_dev* / c_uid / c_gid only need to
/// be self-consistent within the archive, and real inode numbers routinely
/// exceed 32 bits.
fn write_hex_field_masked(buf: &mut Vec<u8>, val: u64) {
    let s = format!("{:0width$X}", val & 0xffff_ffff, width = NEWC_FIELD_WIDTH);
    buf.extend_from_slice(s.as_bytes());
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
        write_octal_field(&mut buf, 0o644, 6).unwrap();
        assert_eq!(&buf, b"000644");

        let mut buf = Vec::new();
        write_octal_field(&mut buf, 0, 6).unwrap();
        assert_eq!(&buf, b"000000");

        // The widest value that fits an 11-digit c_filesize field is 8 GiB - 1.
        let mut buf = Vec::new();
        write_octal_field(&mut buf, 0o77_777_777_777, 11).unwrap();
        assert_eq!(&buf, b"77777777777");

        // One larger overflows and must be rejected, not truncated.
        let mut buf = Vec::new();
        assert!(write_octal_field(&mut buf, 0o100_000_000_000, 11).is_err());
    }

    #[test]
    fn test_pad_len_matches_format_alignment() {
        // odc is unaligned; newc pads to 4, the old binary format to 2.
        assert_eq!(pad_len(7, CpioFormat::Odc.alignment() as u64), 0);
        assert_eq!(pad_len(110 + 11, 4), 3);
        assert_eq!(pad_len(112, 4), 0);
        assert_eq!(pad_len(26 + 11, 2), 1);
        assert_eq!(pad_len(0, 4), 0);
    }

    #[test]
    fn test_checksum_bytes_is_a_plain_sum() {
        assert_eq!(checksum_bytes(0, b""), 0);
        assert_eq!(checksum_bytes(0, b"\x01\x02\x03"), 6);
        // It accumulates across calls, so a streamed file sums the same way.
        assert_eq!(checksum_bytes(checksum_bytes(0, b"\x01"), b"\x02"), 3);
        // And it wraps rather than panicking on a large file.
        assert_eq!(checksum_bytes(u32::MAX, b"\x01"), 0);
    }

    #[test]
    fn test_build_newc_header_layout() {
        let entry = ArchiveEntry {
            path: PathBuf::from("hello"),
            mode: 0o644,
            uid: 7,
            gid: 9,
            size: 0x1234,
            mtime: 0x5678,
            nlink: 1,
            entry_type: EntryType::Regular,
            data_checksum: Some(0xABCD),
            ..Default::default()
        };

        let plain = build_newc_header(&entry, 0x11, 6, CpioFormat::Newc).unwrap();
        assert_eq!(plain.len(), NEWC_HEADER_SIZE);
        assert_eq!(&plain[0..6], NEWC_MAGIC);
        assert_eq!(&plain[6..14], b"00000011"); // c_ino
        assert_eq!(&plain[14..22], b"000081A4"); // c_mode: C_ISREG | 0644
        assert_eq!(&plain[22..30], b"00000007"); // c_uid
        assert_eq!(&plain[30..38], b"00000009"); // c_gid
        assert_eq!(&plain[38..46], b"00000001"); // c_nlink
        assert_eq!(&plain[46..54], b"00005678"); // c_mtime
        assert_eq!(&plain[54..62], b"00001234"); // c_filesize
        assert_eq!(&plain[94..102], b"00000006"); // c_namesize
        assert_eq!(&plain[102..110], b"00000000"); // c_check is zero for 070701

        // The checksummed variant differs only in its magic and c_check.
        let crc = build_newc_header(&entry, 0x11, 6, CpioFormat::NewcCrc).unwrap();
        assert_eq!(&crc[0..6], NEWC_CRC_MAGIC);
        assert_eq!(&crc[102..110], b"0000ABCD");
        assert_eq!(&crc[6..102], &plain[6..102]);
    }

    #[test]
    fn test_build_newc_header_masks_identities_but_rejects_framing() {
        let mut entry = ArchiveEntry {
            path: PathBuf::from("x"),
            entry_type: EntryType::Regular,
            // A real inode number well past what eight hex digits hold.
            ino: 0x1_2345_6789,
            ..Default::default()
        };
        // c_ino only groups hard links, so it is masked rather than refused.
        let header = build_newc_header(&entry, entry.ino, 2, CpioFormat::Newc).unwrap();
        assert_eq!(&header[6..14], b"23456789");

        // c_filesize frames the stream, so an unrepresentable value is an error
        // instead of a silently truncated one.
        entry.size = 0x1_0000_0000;
        assert!(build_newc_header(&entry, 1, 2, CpioFormat::Newc).is_err());
    }

    #[test]
    fn test_build_bin_header_layout_and_limits() {
        let entry = ArchiveEntry {
            path: PathBuf::from("hello"),
            mode: 0o644,
            size: 0x1_2345,
            mtime: 0x8_9ABC,
            entry_type: EntryType::Regular,
            ..Default::default()
        };

        let header = build_bin_header(&entry, 3, 6).unwrap();
        assert_eq!(header.len(), BIN_HEADER_SIZE);
        let word = |i: usize| u16::from_ne_bytes([header[i * 2], header[i * 2 + 1]]);
        assert_eq!(word(0), BIN_MAGIC);
        assert_eq!(word(2), 3); // c_ino
        assert_eq!(word(3), (C_ISREG | 0o644) as u16); // c_mode
                                                       // c_mtime and c_filesize are two words each, most significant first,
                                                       // and sit after c_rdev at word 7.
        assert_eq!(word(8), 0x0008);
        assert_eq!(word(9), 0x9ABC);
        assert_eq!(word(10), 6); // c_namesize
        assert_eq!(word(11), 0x0001);
        assert_eq!(word(12), 0x2345);

        // The 16-bit format cannot describe a file past 4 GiB.
        let too_big = ArchiveEntry {
            size: u32::MAX as u64 + 1,
            ..entry
        };
        assert!(build_bin_header(&too_big, 3, 6).is_err());
    }

    #[test]
    fn test_header_nlink_never_writes_zero() {
        // The trailer, and anything built without stat data, has nlink 0; no
        // real cpio writes that, and a reader would see a file with no names.
        let synthesized = ArchiveEntry {
            nlink: 0,
            ..Default::default()
        };
        assert_eq!(header_nlink(&synthesized), 1);
        assert_eq!(
            header_nlink(&ArchiveEntry {
                nlink: 3,
                ..synthesized
            }),
            3
        );
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
