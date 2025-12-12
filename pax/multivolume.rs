//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Multi-volume archive support (GNU tar compatible)
//!
//! Multi-volume archives allow splitting large archives across multiple
//! files or tape volumes. This implementation follows GNU tar's approach:
//!
//! - Each volume is a valid tar archive on its own
//! - Files can be split across volumes using 'M' (continuation) headers
//! - The continuation header contains the file's original name, offset, and realsize
//!
//! ## Volume Header Format (GNU extension)
//!
//! When a file is split across volumes:
//! 1. The first volume ends with a partial file entry (normal header + partial data)
//! 2. The next volume starts with an 'M' type header containing:
//!    - The original file name
//!    - The remaining size in the size field
//!    - The offset into the original file (at bytes 369-380)
//!    - The total file size in the GNU realsize field
//!
//! ## Limitations
//!
//! - Only supported for ustar format (not cpio)
//! - Compression is not supported with multi-volume
//! - Volume scripts are executed synchronously

use crate::archive::{ArchiveEntry, ArchiveReader, ArchiveWriter, EntryType};
use crate::error::{PaxError, PaxResult};
use std::fs::File;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::process::Command;

const BLOCK_SIZE: usize = 512;

/// GNU tar type flag for multi-volume continuation
pub const GNUTYPE_MULTIVOL: u8 = b'M';

/// Options for multi-volume archive operations
#[derive(Clone)]
pub struct MultiVolumeOptions {
    /// Maximum size per volume in bytes (None = unlimited)
    pub volume_size: Option<u64>,
    /// Script to run when changing volumes (None = prompt user)
    pub volume_script: Option<String>,
    /// Base path for archive files
    pub archive_path: PathBuf,
    /// Whether to run in verbose mode
    pub verbose: bool,
}

impl Default for MultiVolumeOptions {
    fn default() -> Self {
        MultiVolumeOptions {
            volume_size: None,
            volume_script: None,
            archive_path: PathBuf::new(),
            verbose: false,
        }
    }
}

/// Tracks state during multi-volume archive writing
pub struct MultiVolumeWriter {
    /// Current volume number (1-based)
    current_volume: u32,
    /// Bytes written to current volume
    bytes_written: u64,
    /// Maximum bytes per volume
    volume_size: u64,
    /// Options for volume handling
    options: MultiVolumeOptions,
    /// Current output file
    writer: Option<File>,
    /// Entry being split (if any)
    pending_entry: Option<SplitEntry>,
}

/// Information about a file being split across volumes
#[derive(Clone)]
struct SplitEntry {
    /// Original entry metadata
    entry: ArchiveEntry,
    /// Bytes of file data already written
    bytes_written: u64,
    /// Total file size
    total_size: u64,
}

impl MultiVolumeWriter {
    /// Create a new multi-volume writer
    pub fn new(options: MultiVolumeOptions) -> PaxResult<Self> {
        let volume_size = options.volume_size.unwrap_or(u64::MAX);

        let mut writer = MultiVolumeWriter {
            current_volume: 0,
            bytes_written: 0,
            volume_size,
            options,
            writer: None,
            pending_entry: None,
        };

        // Open first volume
        writer.open_next_volume()?;

        Ok(writer)
    }

    /// Get the path for a specific volume number
    fn volume_path(&self, volume: u32) -> PathBuf {
        if volume == 1 {
            self.options.archive_path.clone()
        } else {
            // Append volume number as extension
            let base = self.options.archive_path.to_string_lossy();
            PathBuf::from(format!("{}.{}", base, volume))
        }
    }

    /// Open the next volume
    fn open_next_volume(&mut self) -> PaxResult<()> {
        // Close current volume if open
        if let Some(ref mut w) = self.writer {
            // Write end-of-archive marker for current volume
            let zeros = [0u8; BLOCK_SIZE];
            w.write_all(&zeros)?;
            w.write_all(&zeros)?;
            w.flush()?;
        }

        self.current_volume += 1;
        self.bytes_written = 0;

        // Prompt for new volume or run script
        if self.current_volume > 1 {
            self.prompt_or_run_script()?;
        }

        let path = self.volume_path(self.current_volume);

        if self.options.verbose {
            eprintln!(
                "pax: opening volume {} ({})",
                self.current_volume,
                path.display()
            );
        }

        self.writer = Some(File::create(&path)?);

        // If we have a pending split entry, write its continuation header
        if self.pending_entry.is_some() {
            // Clone the split entry to avoid borrow conflict
            let split = self.pending_entry.clone().unwrap();
            self.write_continuation_header(&split)?;
        }

        Ok(())
    }

    /// Prompt user or run volume script
    fn prompt_or_run_script(&self) -> PaxResult<()> {
        if let Some(ref script) = self.options.volume_script {
            // Run the script
            let status = Command::new("sh")
                .arg("-c")
                .arg(script)
                .env("TAR_VOLUME", self.current_volume.to_string())
                .env(
                    "TAR_ARCHIVE",
                    self.volume_path(self.current_volume)
                        .to_string_lossy()
                        .as_ref(),
                )
                .status()?;

            if !status.success() {
                return Err(PaxError::Io(io::Error::other("volume script failed")));
            }
        } else {
            // Prompt user via /dev/tty
            #[cfg(unix)]
            {
                use std::io::BufRead;

                let tty_read = std::fs::File::open("/dev/tty")?;
                let mut tty_write = std::fs::OpenOptions::new().write(true).open("/dev/tty")?;

                let next_path = self.volume_path(self.current_volume);
                write!(
                    tty_write,
                    "\nPrepare volume #{} for '{}' and press ENTER: ",
                    self.current_volume,
                    next_path.display()
                )?;
                tty_write.flush()?;

                let mut reader = std::io::BufReader::new(tty_read);
                let mut line = String::new();
                reader.read_line(&mut line)?;
            }
            #[cfg(not(unix))]
            {
                eprintln!("Prepare volume #{} and press ENTER", self.current_volume);
                let mut line = String::new();
                io::stdin().read_line(&mut line)?;
            }
        }
        Ok(())
    }

    /// Write a continuation header for a split file
    fn write_continuation_header(&mut self, split: &SplitEntry) -> PaxResult<()> {
        let writer = self
            .writer
            .as_mut()
            .ok_or_else(|| PaxError::Io(io::Error::other("no writer")))?;

        let mut header = [0u8; BLOCK_SIZE];

        // Write file name (truncated if necessary)
        let path_str = split.entry.path.to_string_lossy();
        let path_bytes = path_str.as_bytes();
        let name_len = std::cmp::min(path_bytes.len(), 100);
        header[0..name_len].copy_from_slice(&path_bytes[..name_len]);

        // Mode, uid, gid
        write_octal(&mut header[100..], split.entry.mode as u64, 8);
        write_octal(&mut header[108..], split.entry.uid as u64, 8);
        write_octal(&mut header[116..], split.entry.gid as u64, 8);

        // Size (remaining bytes)
        let remaining = split.total_size - split.bytes_written;
        write_octal(&mut header[124..], remaining, 12);

        // Mtime
        write_octal(&mut header[136..], split.entry.mtime, 12);

        // Typeflag = 'M' for continuation
        header[156] = GNUTYPE_MULTIVOL;

        // Magic and version
        header[257..263].copy_from_slice(b"ustar\0");
        header[263..265].copy_from_slice(b"00");

        // GNU extension: offset at bytes 369-380
        write_octal(&mut header[369..], split.bytes_written, 12);

        // Calculate and write checksum
        let checksum = calculate_checksum(&header);
        write_octal(&mut header[148..], checksum as u64, 8);

        writer.write_all(&header)?;
        self.bytes_written += BLOCK_SIZE as u64;

        Ok(())
    }

    /// Check if we need to switch volumes
    fn check_volume_space(&mut self, needed: u64) -> PaxResult<bool> {
        if self.bytes_written + needed > self.volume_size {
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl ArchiveWriter for MultiVolumeWriter {
    fn write_entry(&mut self, entry: &ArchiveEntry) -> PaxResult<()> {
        // Check if we have space for at least the header
        if self.check_volume_space(BLOCK_SIZE as u64)? {
            self.open_next_volume()?;
        }

        // Build and write header using ustar format
        let header = build_header(entry)?;

        let writer = self
            .writer
            .as_mut()
            .ok_or_else(|| PaxError::Io(io::Error::other("no writer")))?;
        writer.write_all(&header)?;
        self.bytes_written += BLOCK_SIZE as u64;

        Ok(())
    }

    fn write_data(&mut self, data: &[u8]) -> PaxResult<()> {
        let writer = self
            .writer
            .as_mut()
            .ok_or_else(|| PaxError::Io(io::Error::other("no writer")))?;
        writer.write_all(data)?;
        self.bytes_written += data.len() as u64;
        Ok(())
    }

    fn finish_entry(&mut self) -> PaxResult<()> {
        // Pad to block boundary
        let remainder = (self.bytes_written % BLOCK_SIZE as u64) as usize;
        if remainder != 0 {
            let padding = BLOCK_SIZE - remainder;
            let zeros = vec![0u8; padding];
            let writer = self
                .writer
                .as_mut()
                .ok_or_else(|| PaxError::Io(io::Error::other("no writer")))?;
            writer.write_all(&zeros)?;
            self.bytes_written += padding as u64;
        }
        Ok(())
    }

    fn finish(&mut self) -> PaxResult<()> {
        // Write end-of-archive marker
        let zeros = [0u8; BLOCK_SIZE];
        let writer = self
            .writer
            .as_mut()
            .ok_or_else(|| PaxError::Io(io::Error::other("no writer")))?;
        writer.write_all(&zeros)?;
        writer.write_all(&zeros)?;
        writer.flush()?;

        if self.options.verbose {
            eprintln!("pax: wrote {} volume(s)", self.current_volume);
        }

        Ok(())
    }
}

/// Multi-volume archive reader
pub struct MultiVolumeReader {
    /// Current volume number (1-based)
    current_volume: u32,
    /// Current reader
    reader: Option<File>,
    /// Options
    options: MultiVolumeOptions,
    /// Current entry size remaining (for current volume's portion)
    current_size: u64,
    /// Bytes read from current entry
    bytes_read: u64,
    /// Current entry being read (for continuation across volumes)
    current_entry: Option<ArchiveEntry>,
    /// Total size of current entry (may span volumes)
    total_entry_size: u64,
    /// Total bytes read from current entry across all volumes
    total_bytes_read: u64,
    /// Whether we're in the middle of reading a split file
    in_split_file: bool,
}

impl MultiVolumeReader {
    /// Create a new multi-volume reader
    pub fn new(options: MultiVolumeOptions) -> PaxResult<Self> {
        let mut reader = MultiVolumeReader {
            current_volume: 0,
            reader: None,
            options,
            current_size: 0,
            bytes_read: 0,
            current_entry: None,
            total_entry_size: 0,
            total_bytes_read: 0,
            in_split_file: false,
        };

        reader.open_next_volume()?;
        Ok(reader)
    }

    /// Get the path for a specific volume number
    fn volume_path(&self, volume: u32) -> PathBuf {
        if volume == 1 {
            self.options.archive_path.clone()
        } else {
            let base = self.options.archive_path.to_string_lossy();
            PathBuf::from(format!("{}.{}", base, volume))
        }
    }

    /// Open the next volume
    fn open_next_volume(&mut self) -> PaxResult<bool> {
        self.current_volume += 1;

        let path = self.volume_path(self.current_volume);

        if !path.exists() {
            // Try prompting for volume if we're expecting more data
            if self.current_volume > 1 && self.in_split_file {
                if let Some(ref script) = self.options.volume_script {
                    // Run script
                    let status = Command::new("sh")
                        .arg("-c")
                        .arg(script)
                        .env("TAR_VOLUME", self.current_volume.to_string())
                        .env("TAR_ARCHIVE", path.to_string_lossy().as_ref())
                        .status()?;
                    if !status.success() {
                        return Err(PaxError::Io(io::Error::other("volume script failed")));
                    }
                } else {
                    self.prompt_for_volume()?;
                }
            }
            if !path.exists() {
                return Ok(false);
            }
        }

        if self.options.verbose {
            eprintln!(
                "pax: reading volume {} ({})",
                self.current_volume,
                path.display()
            );
        }

        self.reader = Some(File::open(&path)?);
        Ok(true)
    }

    /// Prompt user for next volume
    fn prompt_for_volume(&self) -> PaxResult<()> {
        #[cfg(unix)]
        {
            use std::io::BufRead;

            let tty_read = std::fs::File::open("/dev/tty")?;
            let mut tty_write = std::fs::OpenOptions::new().write(true).open("/dev/tty")?;

            let next_path = self.volume_path(self.current_volume);
            write!(
                tty_write,
                "\nPrepare volume #{} for '{}' and press ENTER: ",
                self.current_volume,
                next_path.display()
            )?;
            tty_write.flush()?;

            let mut reader = std::io::BufReader::new(tty_read);
            let mut line = String::new();
            reader.read_line(&mut line)?;
        }
        #[cfg(not(unix))]
        {
            eprintln!("Prepare volume #{} and press ENTER", self.current_volume);
            let mut line = String::new();
            io::stdin().read_line(&mut line)?;
        }
        Ok(())
    }

    /// Check if a header is a continuation header
    fn is_continuation_header(header: &[u8]) -> bool {
        header.len() >= 157 && header[156] == GNUTYPE_MULTIVOL
    }

    /// Parse a tar header into an ArchiveEntry
    fn parse_header(header: &[u8; BLOCK_SIZE]) -> PaxResult<ArchiveEntry> {
        let name = Self::parse_string(&header[0..100]);
        let prefix = Self::parse_string(&header[345..500]);

        let path = if prefix.is_empty() {
            PathBuf::from(name)
        } else {
            PathBuf::from(format!("{}/{}", prefix, name))
        };

        let mode = parse_octal(&header[100..108])? as u32;
        let uid = parse_octal(&header[108..116])? as u32;
        let gid = parse_octal(&header[116..124])? as u32;
        let size = parse_octal(&header[124..136])?;
        let mtime = parse_octal(&header[136..148])?;

        let typeflag = header[156];
        let entry_type = Self::parse_typeflag(typeflag);

        let linkname = Self::parse_string(&header[157..257]);
        let link_target = if !linkname.is_empty() {
            Some(PathBuf::from(linkname))
        } else {
            None
        };

        let uname = Self::parse_string(&header[265..297]);
        let gname = Self::parse_string(&header[297..329]);

        let devmajor = parse_octal(&header[329..337])? as u32;
        let devminor = parse_octal(&header[337..345])? as u32;

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

    /// Parse a NUL-terminated string
    fn parse_string(bytes: &[u8]) -> String {
        let end = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
        String::from_utf8_lossy(&bytes[..end])
            .trim_end()
            .to_string()
    }

    /// Parse typeflag to EntryType
    fn parse_typeflag(flag: u8) -> EntryType {
        match flag {
            b'0' | b'\0' => EntryType::Regular,
            b'1' => EntryType::Hardlink,
            b'2' => EntryType::Symlink,
            b'3' => EntryType::CharDevice,
            b'4' => EntryType::BlockDevice,
            b'5' => EntryType::Directory,
            b'6' => EntryType::Fifo,
            GNUTYPE_MULTIVOL => EntryType::Regular, // Continuation is treated as regular
            _ => EntryType::Regular,
        }
    }

    /// Parse offset from GNU continuation header (bytes 369-380)
    fn parse_continuation_offset(header: &[u8]) -> u64 {
        if header.len() < 381 {
            return 0;
        }
        parse_octal(&header[369..381]).unwrap_or(0)
    }

    /// Check if a block is all zeros (end of archive marker)
    fn is_zero_block(block: &[u8]) -> bool {
        block.iter().all(|&b| b == 0)
    }

    /// Verify header checksum
    fn verify_checksum(header: &[u8; BLOCK_SIZE]) -> bool {
        let stored = match parse_octal(&header[148..156]) {
            Ok(v) => v as u32,
            Err(_) => return false,
        };
        let calculated = calculate_checksum(header);
        stored == calculated
    }

    /// Read exactly n bytes from current reader
    fn read_exact_from_reader(&mut self, buf: &mut [u8]) -> PaxResult<()> {
        let reader = self
            .reader
            .as_mut()
            .ok_or_else(|| PaxError::Io(io::Error::other("no reader")))?;
        reader.read_exact(buf)?;
        Ok(())
    }

    /// Round up to next block boundary
    fn round_up_block(size: u64) -> u64 {
        size.div_ceil(BLOCK_SIZE as u64) * BLOCK_SIZE as u64
    }
}

impl ArchiveReader for MultiVolumeReader {
    fn read_entry(&mut self) -> PaxResult<Option<ArchiveEntry>> {
        // Skip any remaining data from previous entry
        self.skip_data()?;

        loop {
            let mut header = [0u8; BLOCK_SIZE];
            if let Err(e) = self.read_exact_from_reader(&mut header) {
                // Try opening next volume if this is a split file
                if self.in_split_file && self.open_next_volume()? {
                    continue;
                }
                if e.to_string().contains("unexpected end of file") {
                    return Ok(None);
                }
                return Err(e);
            }

            // Check for end of archive (two zero blocks)
            if Self::is_zero_block(&header) {
                // Check if there's another volume
                if self.open_next_volume()? {
                    continue;
                }
                return Ok(None);
            }

            // Verify checksum
            if !Self::verify_checksum(&header) {
                return Err(PaxError::InvalidHeader("checksum mismatch".to_string()));
            }

            // Check if this is a continuation header
            if Self::is_continuation_header(&header) {
                // This is a continuation of a split file from previous volume
                let offset = Self::parse_continuation_offset(&header);
                let remaining_size = parse_octal(&header[124..136])?;

                if self.options.verbose {
                    let name = Self::parse_string(&header[0..100]);
                    eprintln!("pax: continuation of '{}' at offset {}", name, offset);
                }

                // Update our tracking - we're continuing from where we left off
                self.current_size = remaining_size;
                self.bytes_read = 0;
                self.in_split_file = true;

                // Return None to indicate we should continue reading data
                // The caller should be in the middle of extract_file
                // Actually, for continuation headers, we don't return a new entry
                // We just update internal state and the read_data will continue
                continue;
            }

            // Regular entry
            let entry = Self::parse_header(&header)?;
            self.current_size = entry.size;
            self.bytes_read = 0;
            self.total_entry_size = entry.size;
            self.total_bytes_read = 0;
            self.current_entry = Some(entry.clone());
            self.in_split_file = false;

            return Ok(Some(entry));
        }
    }

    fn read_data(&mut self, buf: &mut [u8]) -> PaxResult<usize> {
        let remaining = self.current_size - self.bytes_read;
        if remaining == 0 {
            // Check if we need to switch to next volume for more data
            if self.in_split_file && self.total_bytes_read < self.total_entry_size {
                // Try to open next volume
                if self.open_next_volume()? {
                    // Read the continuation header
                    let mut header = [0u8; BLOCK_SIZE];
                    self.read_exact_from_reader(&mut header)?;

                    if Self::is_continuation_header(&header) {
                        let remaining_size = parse_octal(&header[124..136])?;
                        self.current_size = remaining_size;
                        self.bytes_read = 0;
                        // Continue reading
                    } else {
                        // Not a continuation header - unexpected
                        return Ok(0);
                    }
                } else {
                    return Ok(0);
                }
            } else {
                return Ok(0);
            }
        }

        let remaining = self.current_size - self.bytes_read;
        let to_read = std::cmp::min(buf.len() as u64, remaining) as usize;

        let reader = self
            .reader
            .as_mut()
            .ok_or_else(|| PaxError::Io(io::Error::other("no reader")))?;
        let n = reader.read(&mut buf[..to_read])?;

        self.bytes_read += n as u64;
        self.total_bytes_read += n as u64;

        // Check if this entry is split across volumes
        if self.bytes_read >= self.current_size && self.total_bytes_read < self.total_entry_size {
            self.in_split_file = true;
        }

        Ok(n)
    }

    fn skip_data(&mut self) -> PaxResult<()> {
        // Calculate total bytes including padding to block boundary
        let total_bytes = Self::round_up_block(self.current_size);
        let to_skip = total_bytes.saturating_sub(self.bytes_read);

        if to_skip > 0 {
            let reader = self
                .reader
                .as_mut()
                .ok_or_else(|| PaxError::Io(io::Error::other("no reader")))?;

            let mut remaining = to_skip;
            let mut buf = [0u8; 4096];
            while remaining > 0 {
                let to_read = std::cmp::min(remaining, buf.len() as u64) as usize;
                reader.read_exact(&mut buf[..to_read])?;
                remaining -= to_read as u64;
            }
        }

        self.bytes_read = total_bytes;
        self.in_split_file = false;
        Ok(())
    }
}

impl std::io::Read for MultiVolumeReader {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.read_data(buf)
            .map_err(|e| std::io::Error::other(e.to_string()))
    }
}

// Helper functions

fn write_octal(buf: &mut [u8], val: u64, width: usize) {
    let s = format!("{:0width$o} ", val, width = width - 2);
    let bytes = s.as_bytes();
    let len = std::cmp::min(bytes.len(), width);
    buf[..len].copy_from_slice(&bytes[..len]);
}

fn parse_octal(bytes: &[u8]) -> PaxResult<u64> {
    let s = std::str::from_utf8(bytes)
        .map_err(|_| PaxError::InvalidHeader("invalid octal".to_string()))?;
    let s = s.trim_matches(|c| c == ' ' || c == '\0');
    if s.is_empty() {
        return Ok(0);
    }
    u64::from_str_radix(s, 8).map_err(|_| PaxError::InvalidHeader(format!("invalid octal: {}", s)))
}

fn calculate_checksum(header: &[u8; BLOCK_SIZE]) -> u32 {
    let mut sum: u32 = 0;
    for (i, &byte) in header.iter().enumerate() {
        if (148..156).contains(&i) {
            sum += b' ' as u32;
        } else {
            sum += byte as u32;
        }
    }
    sum
}

fn build_header(entry: &ArchiveEntry) -> PaxResult<[u8; BLOCK_SIZE]> {
    let mut header = [0u8; BLOCK_SIZE];

    // Write file name (truncated if necessary)
    let path_str = entry.path.to_string_lossy();
    let path_bytes = path_str.as_bytes();
    let name_len = std::cmp::min(path_bytes.len(), 100);
    header[0..name_len].copy_from_slice(&path_bytes[..name_len]);

    // Mode, uid, gid
    write_octal(&mut header[100..], entry.mode as u64, 8);
    write_octal(&mut header[108..], entry.uid as u64, 8);
    write_octal(&mut header[116..], entry.gid as u64, 8);

    // Size
    write_octal(&mut header[124..], entry.size, 12);

    // Mtime
    write_octal(&mut header[136..], entry.mtime, 12);

    // Typeflag
    let typeflag = match entry.entry_type {
        EntryType::Regular => b'0',
        EntryType::Directory => b'5',
        EntryType::Symlink => b'2',
        EntryType::Hardlink => b'1',
        EntryType::CharDevice => b'3',
        EntryType::BlockDevice => b'4',
        EntryType::Fifo => b'6',
        EntryType::Socket => b'0',
    };
    header[156] = typeflag;

    // Link name for symlinks/hardlinks
    if let Some(ref target) = entry.link_target {
        let target_str = target.to_string_lossy();
        let target_bytes = target_str.as_bytes();
        let link_len = std::cmp::min(target_bytes.len(), 100);
        header[157..157 + link_len].copy_from_slice(&target_bytes[..link_len]);
    }

    // Magic and version
    header[257..263].copy_from_slice(b"ustar\0");
    header[263..265].copy_from_slice(b"00");

    // uname and gname
    if let Some(ref uname) = entry.uname {
        let bytes = uname.as_bytes();
        let len = std::cmp::min(bytes.len(), 32);
        header[265..265 + len].copy_from_slice(&bytes[..len]);
    }
    if let Some(ref gname) = entry.gname {
        let bytes = gname.as_bytes();
        let len = std::cmp::min(bytes.len(), 32);
        header[297..297 + len].copy_from_slice(&bytes[..len]);
    }

    // Device major/minor
    if entry.is_device() {
        write_octal(&mut header[329..], entry.devmajor as u64, 8);
        write_octal(&mut header[337..], entry.devminor as u64, 8);
    }

    // Calculate and write checksum
    let checksum = calculate_checksum(&header);
    write_octal(&mut header[148..], checksum as u64, 8);

    Ok(header)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_volume_path() {
        let options = MultiVolumeOptions {
            archive_path: PathBuf::from("/tmp/test.tar"),
            ..Default::default()
        };
        let writer = MultiVolumeWriter {
            current_volume: 1,
            bytes_written: 0,
            volume_size: 1024,
            options,
            writer: None,
            pending_entry: None,
        };

        assert_eq!(writer.volume_path(1), PathBuf::from("/tmp/test.tar"));
        assert_eq!(writer.volume_path(2), PathBuf::from("/tmp/test.tar.2"));
        assert_eq!(writer.volume_path(3), PathBuf::from("/tmp/test.tar.3"));
    }

    #[test]
    fn test_write_octal() {
        let mut buf = [0u8; 8];
        write_octal(&mut buf, 0o644, 8);
        assert_eq!(&buf[..6], b"000644");
    }

    #[test]
    fn test_checksum() {
        let mut header = [0u8; BLOCK_SIZE];
        header[0..4].copy_from_slice(b"test");
        let checksum = calculate_checksum(&header);
        assert!(checksum > 0);
    }
}
