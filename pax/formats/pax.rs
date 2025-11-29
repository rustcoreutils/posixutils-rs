//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX pax format implementation
//!
//! The pax format extends ustar with extended headers that can contain:
//! - Long paths (> 256 characters)
//! - Large file sizes (> 8GB)
//! - Large UID/GID values (> 2097151)
//! - Subsecond timestamps
//! - UTF-8 encoded filenames
//! - Access times (atime)
//! - Additional metadata
//!
//! Extended header format:
//! - typeflag 'x' for per-file extended headers
//! - typeflag 'g' for global extended headers
//! - Data format: "%d %s=%s\n" (length, keyword, value)

use crate::archive::{ArchiveEntry, ArchiveReader, ArchiveWriter, EntryType};
use crate::error::{PaxError, PaxResult};
use std::collections::HashMap;
use std::io::{Read, Write};
use std::path::PathBuf;

const BLOCK_SIZE: usize = 512;

// Extended header typeflags
const PAX_XHDR: u8 = b'x'; // Per-file extended header
const PAX_GHDR: u8 = b'g'; // Global extended header

// Regular ustar typeflags (for reference)
const REGTYPE: u8 = b'0';
const AREGTYPE: u8 = b'\0';
const LNKTYPE: u8 = b'1';
const SYMTYPE: u8 = b'2';
const CHRTYPE: u8 = b'3';
const BLKTYPE: u8 = b'4';
const DIRTYPE: u8 = b'5';
const FIFOTYPE: u8 = b'6';

// Header field offsets (same as ustar)
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
const DEVMAJOR_OFF: usize = 329;
const DEVMINOR_OFF: usize = 337;
const PREFIX_OFF: usize = 345;

const NAME_LEN: usize = 100;
const PREFIX_LEN: usize = 155;
const LINKNAME_LEN: usize = 100;
const UNAME_LEN: usize = 32;
const GNAME_LEN: usize = 32;

/// Extended header keywords as per POSIX
#[derive(Debug, Clone, Default)]
pub struct ExtendedHeader {
    /// atime - file access time
    pub atime: Option<f64>,
    /// mtime - file modification time
    pub mtime: Option<f64>,
    /// path - file pathname
    pub path: Option<String>,
    /// linkpath - link target pathname
    pub linkpath: Option<String>,
    /// size - file size
    pub size: Option<u64>,
    /// uid - user ID
    pub uid: Option<u32>,
    /// gid - group ID
    pub gid: Option<u32>,
    /// uname - user name
    pub uname: Option<String>,
    /// gname - group name
    pub gname: Option<String>,
    /// Additional custom keywords
    pub extra: HashMap<String, String>,
}

impl ExtendedHeader {
    /// Create a new empty extended header
    pub fn new() -> Self {
        Self::default()
    }

    /// Check if the header is empty (no overrides)
    pub fn is_empty(&self) -> bool {
        self.atime.is_none()
            && self.mtime.is_none()
            && self.path.is_none()
            && self.linkpath.is_none()
            && self.size.is_none()
            && self.uid.is_none()
            && self.gid.is_none()
            && self.uname.is_none()
            && self.gname.is_none()
            && self.extra.is_empty()
    }

    /// Parse extended header records from data
    pub fn parse(data: &[u8]) -> PaxResult<Self> {
        let mut header = ExtendedHeader::new();
        let mut pos = 0;

        while pos < data.len() {
            // Find the space after length
            let space_pos = data[pos..].iter().position(|&b| b == b' ').ok_or_else(|| {
                PaxError::InvalidHeader("invalid extended header format".to_string())
            })?;

            // Parse length
            let len_str = std::str::from_utf8(&data[pos..pos + space_pos]).map_err(|_| {
                PaxError::InvalidHeader("invalid extended header length".to_string())
            })?;
            let record_len: usize = len_str.parse().map_err(|_| {
                PaxError::InvalidHeader("invalid extended header length".to_string())
            })?;

            if pos + record_len > data.len() {
                return Err(PaxError::InvalidHeader(
                    "extended header record extends past end".to_string(),
                ));
            }

            // Extract keyword=value (after space, before newline)
            let record_start = pos + space_pos + 1;
            let record_end = pos + record_len - 1; // Exclude trailing newline

            if record_end <= record_start {
                pos += record_len;
                continue;
            }

            let record = std::str::from_utf8(&data[record_start..record_end]).map_err(|_| {
                PaxError::InvalidHeader("invalid UTF-8 in extended header".to_string())
            })?;

            // Find the '=' separator
            if let Some(eq_pos) = record.find('=') {
                let keyword = &record[..eq_pos];
                let value = &record[eq_pos + 1..];

                header.set_keyword(keyword, value)?;
            }

            pos += record_len;
        }

        Ok(header)
    }

    /// Set a keyword value
    fn set_keyword(&mut self, keyword: &str, value: &str) -> PaxResult<()> {
        match keyword {
            "atime" => {
                self.atime = Some(parse_pax_time(value)?);
            }
            "mtime" => {
                self.mtime = Some(parse_pax_time(value)?);
            }
            "path" => {
                self.path = Some(value.to_string());
            }
            "linkpath" => {
                self.linkpath = Some(value.to_string());
            }
            "size" => {
                self.size =
                    Some(value.parse().map_err(|_| {
                        PaxError::InvalidHeader(format!("invalid size: {}", value))
                    })?);
            }
            "uid" => {
                self.uid = Some(
                    value
                        .parse()
                        .map_err(|_| PaxError::InvalidHeader(format!("invalid uid: {}", value)))?,
                );
            }
            "gid" => {
                self.gid = Some(
                    value
                        .parse()
                        .map_err(|_| PaxError::InvalidHeader(format!("invalid gid: {}", value)))?,
                );
            }
            "uname" => {
                self.uname = Some(value.to_string());
            }
            "gname" => {
                self.gname = Some(value.to_string());
            }
            _ => {
                // Store unknown keywords for potential future use
                self.extra.insert(keyword.to_string(), value.to_string());
            }
        }
        Ok(())
    }

    /// Serialize extended header to bytes
    pub fn serialize(&self) -> Vec<u8> {
        let mut data = Vec::new();

        if let Some(atime) = self.atime {
            write_pax_record(&mut data, "atime", &format_pax_time(atime));
        }
        if let Some(mtime) = self.mtime {
            write_pax_record(&mut data, "mtime", &format_pax_time(mtime));
        }
        if let Some(ref path) = self.path {
            write_pax_record(&mut data, "path", path);
        }
        if let Some(ref linkpath) = self.linkpath {
            write_pax_record(&mut data, "linkpath", linkpath);
        }
        if let Some(size) = self.size {
            write_pax_record(&mut data, "size", &size.to_string());
        }
        if let Some(uid) = self.uid {
            write_pax_record(&mut data, "uid", &uid.to_string());
        }
        if let Some(gid) = self.gid {
            write_pax_record(&mut data, "gid", &gid.to_string());
        }
        if let Some(ref uname) = self.uname {
            write_pax_record(&mut data, "uname", uname);
        }
        if let Some(ref gname) = self.gname {
            write_pax_record(&mut data, "gname", gname);
        }
        for (key, value) in &self.extra {
            write_pax_record(&mut data, key, value);
        }

        data
    }

    /// Apply extended header overrides to an ArchiveEntry
    pub fn apply_to(&self, entry: &mut ArchiveEntry) {
        if let Some(ref path) = self.path {
            entry.path = PathBuf::from(path);
        }
        if let Some(ref linkpath) = self.linkpath {
            entry.link_target = Some(PathBuf::from(linkpath));
        }
        if let Some(size) = self.size {
            entry.size = size;
        }
        if let Some(uid) = self.uid {
            entry.uid = uid;
        }
        if let Some(gid) = self.gid {
            entry.gid = gid;
        }
        if let Some(ref uname) = self.uname {
            entry.uname = Some(uname.clone());
        }
        if let Some(ref gname) = self.gname {
            entry.gname = Some(gname.clone());
        }
        if let Some(mtime) = self.mtime {
            entry.mtime = mtime as u64;
            entry.mtime_nsec = ((mtime.fract()) * 1_000_000_000.0) as u32;
        }
        if let Some(atime) = self.atime {
            entry.atime = Some(atime as u64);
            entry.atime_nsec = ((atime.fract()) * 1_000_000_000.0) as u32;
        }
    }

    /// Create extended header from an ArchiveEntry (for values that need extended headers)
    pub fn from_entry(entry: &ArchiveEntry) -> Self {
        let mut header = ExtendedHeader::new();

        // Path needs extended header if too long for ustar
        let path_str = entry.path.to_string_lossy();
        if path_str.len() > NAME_LEN + PREFIX_LEN + 1 {
            header.path = Some(path_str.to_string());
        }

        // Link path needs extended header if too long
        if let Some(ref link) = entry.link_target {
            let link_str = link.to_string_lossy();
            if link_str.len() > LINKNAME_LEN {
                header.linkpath = Some(link_str.to_string());
            }
        }

        // Size > 8GB needs extended header
        if entry.size > 0o77777777777 {
            header.size = Some(entry.size);
        }

        // UID/GID > 2097151 needs extended header
        if entry.uid > 0o7777777 {
            header.uid = Some(entry.uid);
        }
        if entry.gid > 0o7777777 {
            header.gid = Some(entry.gid);
        }

        // Include subsecond mtime if present
        if entry.mtime_nsec > 0 {
            let mtime_float = entry.mtime as f64 + (entry.mtime_nsec as f64 / 1_000_000_000.0);
            header.mtime = Some(mtime_float);
        }

        // Include atime if present
        if let Some(atime) = entry.atime {
            let atime_float = atime as f64 + (entry.atime_nsec as f64 / 1_000_000_000.0);
            header.atime = Some(atime_float);
        }

        // uname/gname with non-ASCII characters
        if let Some(ref uname) = entry.uname {
            if !uname.is_ascii() || uname.len() > UNAME_LEN {
                header.uname = Some(uname.clone());
            }
        }
        if let Some(ref gname) = entry.gname {
            if !gname.is_ascii() || gname.len() > GNAME_LEN {
                header.gname = Some(gname.clone());
            }
        }

        header
    }
}

/// Parse pax time format (decimal seconds with optional fractional part)
fn parse_pax_time(s: &str) -> PaxResult<f64> {
    s.parse()
        .map_err(|_| PaxError::InvalidHeader(format!("invalid pax time: {}", s)))
}

/// Format time for pax extended header
fn format_pax_time(time: f64) -> String {
    if time.fract() == 0.0 {
        format!("{}", time as u64)
    } else {
        // Format with enough precision for nanoseconds
        format!("{:.9}", time).trim_end_matches('0').to_string()
    }
}

/// Write a pax extended header record
fn write_pax_record(data: &mut Vec<u8>, keyword: &str, value: &str) {
    // Record format: "%d %s=%s\n"
    // Length includes itself, so we need to calculate iteratively
    let content = format!(" {}={}\n", keyword, value);

    // Start with an estimate
    let mut len = content.len() + 1; // +1 for at least one digit
    loop {
        let len_str = len.to_string();
        let total = len_str.len() + content.len();
        if total == len {
            break;
        }
        len = total;
    }

    data.extend_from_slice(len.to_string().as_bytes());
    data.extend_from_slice(content.as_bytes());
}

/// pax archive reader
pub struct PaxReader<R: Read> {
    reader: R,
    current_size: u64,
    bytes_read: u64,
    global_header: ExtendedHeader,
}

impl<R: Read> PaxReader<R> {
    /// Create a new pax reader
    pub fn new(reader: R) -> Self {
        PaxReader {
            reader,
            current_size: 0,
            bytes_read: 0,
            global_header: ExtendedHeader::new(),
        }
    }

    /// Read exactly n bytes
    fn read_exact(&mut self, buf: &mut [u8]) -> PaxResult<()> {
        self.reader.read_exact(buf)?;
        Ok(())
    }

    /// Read a raw header block
    fn read_header_block(&mut self) -> PaxResult<Option<[u8; BLOCK_SIZE]>> {
        let mut header = [0u8; BLOCK_SIZE];
        if let Err(e) = self.read_exact(&mut header) {
            if e.to_string().contains("unexpected end of file") {
                return Ok(None);
            }
            return Err(e);
        }

        // Check for end of archive (zero block)
        if is_zero_block(&header) {
            return Ok(None);
        }

        // Verify checksum
        if !verify_checksum(&header) {
            return Err(PaxError::InvalidHeader("checksum mismatch".to_string()));
        }

        Ok(Some(header))
    }

    /// Read extended header data
    fn read_extended_header(&mut self, size: u64) -> PaxResult<ExtendedHeader> {
        let mut data = vec![0u8; size as usize];
        self.reader.read_exact(&mut data)?;

        // Skip padding to block boundary
        let padding = padding_needed(size);
        if padding > 0 {
            let mut pad = vec![0u8; padding];
            self.reader.read_exact(&mut pad)?;
        }

        ExtendedHeader::parse(&data)
    }
}

impl<R: Read> ArchiveReader for PaxReader<R> {
    fn read_entry(&mut self) -> PaxResult<Option<ArchiveEntry>> {
        // Skip any remaining data from previous entry
        self.skip_data()?;

        let mut extended_header: Option<ExtendedHeader> = None;

        loop {
            let header = match self.read_header_block()? {
                Some(h) => h,
                None => return Ok(None),
            };

            let typeflag = header[TYPEFLAG_OFF];

            match typeflag {
                PAX_GHDR => {
                    // Global extended header - affects all subsequent files
                    let size = parse_octal(&header[SIZE_OFF..SIZE_OFF + 12])?;
                    self.global_header = self.read_extended_header(size)?;
                }
                PAX_XHDR => {
                    // Per-file extended header
                    let size = parse_octal(&header[SIZE_OFF..SIZE_OFF + 12])?;
                    extended_header = Some(self.read_extended_header(size)?);
                }
                _ => {
                    // Regular file entry - parse and apply extended headers
                    let mut entry = parse_ustar_header(&header)?;

                    // Apply global header first
                    self.global_header.apply_to(&mut entry);

                    // Apply per-file extended header (overrides global)
                    if let Some(ref ext) = extended_header {
                        ext.apply_to(&mut entry);
                    }

                    self.current_size = entry.size;
                    self.bytes_read = 0;

                    return Ok(Some(entry));
                }
            }
        }
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
        let total_bytes = round_up_block(self.current_size);
        let to_skip = total_bytes - self.bytes_read;

        if to_skip > 0 {
            skip_bytes(&mut self.reader, to_skip)?;
        }

        self.bytes_read = total_bytes;
        Ok(())
    }
}

/// pax archive writer
pub struct PaxWriter<W: Write> {
    writer: W,
    bytes_written: u64,
    current_size: u64,
    sequence: u64, // For generating unique names for extended header files
}

impl<W: Write> PaxWriter<W> {
    /// Create a new pax writer
    pub fn new(writer: W) -> Self {
        PaxWriter {
            writer,
            bytes_written: 0,
            current_size: 0,
            sequence: 0,
        }
    }

    /// Write extended header block
    fn write_extended_header(
        &mut self,
        ext_header: &ExtendedHeader,
        entry: &ArchiveEntry,
    ) -> PaxResult<()> {
        let data = ext_header.serialize();
        if data.is_empty() {
            return Ok(());
        }

        // Create a header for the extended header block
        let mut header = [0u8; BLOCK_SIZE];

        // Generate a unique name for the extended header
        self.sequence += 1;
        let ext_name = format!(
            "PaxHeader/{}.{}",
            entry
                .path
                .file_name()
                .map(|n| n.to_string_lossy())
                .unwrap_or_default(),
            self.sequence
        );
        let ext_name = if ext_name.len() > NAME_LEN {
            format!("PaxHeader/{}", self.sequence)
        } else {
            ext_name
        };
        write_string(&mut header[NAME_OFF..], &ext_name, NAME_LEN);

        // Mode, uid, gid (use reasonable defaults)
        write_octal(&mut header[MODE_OFF..], 0o644, 8);
        write_octal(&mut header[UID_OFF..], 0, 8);
        write_octal(&mut header[GID_OFF..], 0, 8);

        // Size of extended header data
        write_octal(&mut header[SIZE_OFF..], data.len() as u64, 12);

        // Mtime (use entry's mtime)
        write_octal(&mut header[MTIME_OFF..], entry.mtime, 12);

        // Typeflag 'x' for per-file extended header
        header[TYPEFLAG_OFF] = PAX_XHDR;

        // Magic and version
        header[MAGIC_OFF..MAGIC_OFF + 6].copy_from_slice(b"ustar\0");
        header[VERSION_OFF..VERSION_OFF + 2].copy_from_slice(b"00");

        // Calculate and write checksum
        let checksum = calculate_checksum(&header);
        write_octal(&mut header[CHKSUM_OFF..], checksum as u64, 8);

        // Write header
        self.writer.write_all(&header)?;

        // Write extended header data
        self.writer.write_all(&data)?;

        // Pad to block boundary
        let padding = padding_needed(data.len() as u64);
        if padding > 0 {
            let zeros = vec![0u8; padding];
            self.writer.write_all(&zeros)?;
        }

        Ok(())
    }
}

impl<W: Write> ArchiveWriter for PaxWriter<W> {
    fn write_entry(&mut self, entry: &ArchiveEntry) -> PaxResult<()> {
        // Check if we need extended headers
        let ext_header = ExtendedHeader::from_entry(entry);

        if !ext_header.is_empty() {
            self.write_extended_header(&ext_header, entry)?;
        }

        // Write the regular ustar header
        let header = build_ustar_header(entry)?;
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
        let padding = padding_needed(self.bytes_written);
        if padding > 0 {
            let zeros = vec![0u8; padding];
            self.writer.write_all(&zeros)?;
        }
        Ok(())
    }

    fn finish(&mut self) -> PaxResult<()> {
        // Write two zero blocks
        let zeros = [0u8; BLOCK_SIZE];
        self.writer.write_all(&zeros)?;
        self.writer.write_all(&zeros)?;
        self.writer.flush()?;
        Ok(())
    }
}

// ============================================================================
// Helper functions (shared with ustar where needed)
// ============================================================================

/// Check if a block is all zeros
fn is_zero_block(block: &[u8]) -> bool {
    block.iter().all(|&b| b == 0)
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
        _ => Ok(EntryType::Regular),
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

/// Parse a ustar header into an ArchiveEntry
fn parse_ustar_header(header: &[u8; BLOCK_SIZE]) -> PaxResult<ArchiveEntry> {
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

    // Parse device major/minor numbers for device files
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

/// Build a ustar header block from an ArchiveEntry
fn build_ustar_header(entry: &ArchiveEntry) -> PaxResult<[u8; BLOCK_SIZE]> {
    let mut header = [0u8; BLOCK_SIZE];

    // Split path into name and prefix if needed
    let (name, prefix) = split_path(entry)?;

    // Write fields
    write_string(&mut header[NAME_OFF..], &name, NAME_LEN);
    write_octal(&mut header[MODE_OFF..], entry.mode as u64, 8);
    write_octal(
        &mut header[UID_OFF..],
        std::cmp::min(entry.uid as u64, 0o7777777),
        8,
    );
    write_octal(
        &mut header[GID_OFF..],
        std::cmp::min(entry.gid as u64, 0o7777777),
        8,
    );
    write_octal(
        &mut header[SIZE_OFF..],
        std::cmp::min(entry.size, 0o77777777777),
        12,
    );
    write_octal(&mut header[MTIME_OFF..], entry.mtime, 12);

    // Typeflag
    header[TYPEFLAG_OFF] = entry_type_to_flag(&entry.entry_type);

    // Linkname
    if let Some(ref target) = entry.link_target {
        let link_str = target.to_string_lossy();
        let truncated = if link_str.len() > LINKNAME_LEN {
            &link_str[..LINKNAME_LEN]
        } else {
            &link_str
        };
        write_string(&mut header[LINKNAME_OFF..], truncated, LINKNAME_LEN);
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

    // Device major/minor numbers for device files
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

    // For pax format, if path is too long, we use extended headers
    // Just truncate for the ustar header (extended header will have full path)
    let truncated = if path_str.len() > NAME_LEN {
        path_str[..NAME_LEN].to_string()
    } else {
        path_str
    };
    Ok((truncated, String::new()))
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
        EntryType::Socket => REGTYPE, // Sockets not supported in tar, fall back to regular
    }
}

/// Write a string to a field, NUL-terminated if space permits
fn write_string(buf: &mut [u8], s: &str, max_len: usize) {
    let bytes = s.as_bytes();
    let len = std::cmp::min(bytes.len(), max_len);
    buf[..len].copy_from_slice(&bytes[..len]);
}

/// Write an octal number to a field
fn write_octal(buf: &mut [u8], val: u64, width: usize) {
    let s = format!("{:0width$o} ", val, width = width - 2);
    let bytes = s.as_bytes();
    let len = std::cmp::min(bytes.len(), width);
    buf[..len].copy_from_slice(&bytes[..len]);
}

/// Round up to next block boundary
fn round_up_block(size: u64) -> u64 {
    size.div_ceil(BLOCK_SIZE as u64) * BLOCK_SIZE as u64
}

/// Calculate padding needed to reach block boundary
fn padding_needed(bytes: u64) -> usize {
    let remainder = (bytes % BLOCK_SIZE as u64) as usize;
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
    fn test_write_pax_record() {
        let mut data = Vec::new();
        write_pax_record(&mut data, "path", "/some/path");
        let s = String::from_utf8(data).unwrap();
        // Record format: "len path=/some/path\n"
        // len includes itself + " " + "path=/some/path\n" = 2 + 1 + 16 = 19 chars
        assert_eq!(s, "19 path=/some/path\n");
    }

    #[test]
    fn test_parse_pax_time() {
        assert_eq!(parse_pax_time("1234567890").unwrap(), 1234567890.0);
        assert_eq!(
            parse_pax_time("1234567890.123456789").unwrap(),
            1234567890.123456789
        );
    }

    #[test]
    fn test_format_pax_time() {
        assert_eq!(format_pax_time(1234567890.0), "1234567890");
        assert_eq!(format_pax_time(1234567890.5), "1234567890.5");
    }

    #[test]
    fn test_extended_header_roundtrip() {
        let mut ext = ExtendedHeader::new();
        ext.path = Some("/very/long/path/that/exceeds/ustar/limits".to_string());
        ext.size = Some(10000000000);
        ext.mtime = Some(1234567890.123456789);

        let data = ext.serialize();
        let parsed = ExtendedHeader::parse(&data).unwrap();

        assert_eq!(parsed.path, ext.path);
        assert_eq!(parsed.size, ext.size);
        assert!((parsed.mtime.unwrap() - ext.mtime.unwrap()).abs() < 0.000001);
    }

    #[test]
    fn test_extended_header_from_entry() {
        let mut entry = ArchiveEntry::new(PathBuf::from("test.txt"), EntryType::Regular);
        entry.uid = 3000000; // > 2097151
        entry.mtime_nsec = 500000000; // 0.5 seconds

        let ext = ExtendedHeader::from_entry(&entry);
        assert!(ext.uid.is_some());
        assert!(ext.mtime.is_some());
    }
}
