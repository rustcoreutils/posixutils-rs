//
// Copyright (c) 2025-2026 Jeff Garzik
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
//! - pax does not *write* split members: a volume holds whole members only, and
//!   a member too large for one volume is refused with a diagnostic. The 'M'
//!   continuation handling below is therefore read-only, kept because archives
//!   produced by GNU tar do contain split members.
//! - Only supported for ustar format (not cpio)
//! - Compression is not supported with multi-volume
//! - Volume scripts are executed synchronously

use crate::archive::{ArchiveEntry, ArchiveReader, ArchiveWriter};
use crate::blocked_io::{BlockedReader, BlockedWriter};
use crate::error::{PaxError, PaxResult};
use crate::formats::ustar::{
    build_header, is_zero_block, parse_numeric, stores_data, verify_checksum,
};
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
    /// Bytes per write to a volume (-b)
    record_size: usize,
    /// Current output file
    writer: Option<BlockedWriter<File>>,
    /// The member being written stores no data (a link): drop what is offered.
    skip_data: bool,
}

impl MultiVolumeWriter {
    /// Create a new multi-volume writer that writes `record_size` bytes at a time
    pub fn new(options: MultiVolumeOptions, record_size: usize) -> PaxResult<Self> {
        let volume_size = options.volume_size.unwrap_or(u64::MAX);

        let mut writer = MultiVolumeWriter {
            current_volume: 0,
            bytes_written: 0,
            volume_size,
            options,
            record_size,
            writer: None,
            skip_data: false,
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

        self.writer = Some(BlockedWriter::new(File::create(&path)?, self.record_size));

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
            prompt_volume_change(self.current_volume, &self.volume_path(self.current_volume))?;
        }
        Ok(())
    }

    /// Check if we need to switch volumes
    fn check_volume_space(&mut self, needed: u64) -> PaxResult<bool> {
        Ok(self.volume_bytes(self.bytes_written + needed) > self.volume_size)
    }

    /// The size of a volume holding `content` bytes of members: they are
    /// followed by the end-of-archive marker, and the last record is padded
    /// out whole.
    fn volume_bytes(&self, content: u64) -> u64 {
        let record = self.record_size as u64;
        (content + 2 * BLOCK_SIZE as u64).div_ceil(record) * record
    }
}

/// Ask on `/dev/tty` for the next volume to be made ready, and wait for the
/// response. End of file there ends the run, as it does for `-i`.
fn prompt_volume_change(volume: u32, path: &std::path::Path) -> PaxResult<()> {
    use std::io::BufRead;

    let tty_read = File::open("/dev/tty")?;
    let mut tty_write = std::fs::OpenOptions::new().write(true).open("/dev/tty")?;

    let mut msg = format!("\nPrepare volume #{} for '", volume).into_bytes();
    crate::escape::push_escaped(
        &mut msg,
        crate::rawpath::as_bytes(path),
        crate::escape::Style::TTY,
    );
    msg.extend_from_slice(b"' and press ENTER: ");
    tty_write.write_all(&msg)?;
    tty_write.flush()?;

    let mut line = Vec::new();
    if std::io::BufReader::new(tty_read).read_until(b'\n', &mut line)? == 0 {
        return Err(PaxError::TtyEof);
    }
    Ok(())
}

impl ArchiveWriter for MultiVolumeWriter {
    fn write_entry(&mut self, entry: &ArchiveEntry) -> PaxResult<()> {
        // A member is never divided between volumes, so it has to fit in one
        // whole. Refuse it up front: the alternative is streaming the payload
        // past the tape length and producing a volume that silently exceeds the
        // limit the user asked for.
        let data = if stores_data(entry.entry_type) {
            entry.size
        } else {
            0
        };
        let needed = BLOCK_SIZE as u64 + data.div_ceil(BLOCK_SIZE as u64) * BLOCK_SIZE as u64;
        if self.volume_bytes(needed) > self.volume_size {
            return Err(PaxError::InvalidFormat(format!(
                "{}: {} bytes does not fit in a {}-byte volume",
                entry.path.display(),
                entry.size,
                self.volume_size
            )));
        }

        // Check if we have space for at least the header
        if self.check_volume_space(needed)? {
            self.open_next_volume()?;
        }

        let header = build_header(entry)?;
        self.skip_data = !stores_data(entry.entry_type);

        let writer = self
            .writer
            .as_mut()
            .ok_or_else(|| PaxError::Io(io::Error::other("no writer")))?;
        writer.write_all(&header)?;
        self.bytes_written += BLOCK_SIZE as u64;

        Ok(())
    }

    fn write_data(&mut self, data: &[u8]) -> PaxResult<()> {
        // A link's target is in its header; nothing follows it.
        if self.skip_data {
            return Ok(());
        }
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
    reader: Option<BlockedReader<File>>,
    /// Options
    options: MultiVolumeOptions,
    /// Current entry size remaining (for current volume's portion)
    current_size: u64,
    /// Bytes read from current entry
    bytes_read: u64,
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
                    prompt_volume_change(
                        self.current_volume,
                        &self.volume_path(self.current_volume),
                    )?;
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

        self.reader = Some(BlockedReader::new(File::open(&path)?));
        Ok(true)
    }

    /// Check if a header is a continuation header
    fn is_continuation_header(header: &[u8]) -> bool {
        header.len() >= 157 && header[156] == GNUTYPE_MULTIVOL
    }

    /// Parse a tar header into an ArchiveEntry.
    ///
    /// This is the plain ustar parse. The GNU continuation typeflag is
    /// recognised by `is_continuation_header` before we get here, and the
    /// entry it describes is an ordinary member in every other respect.
    fn parse_header(header: &[u8; BLOCK_SIZE]) -> PaxResult<ArchiveEntry> {
        crate::formats::ustar::parse_header(header, crate::formats::ustar::SizeRule::Ustar)
    }

    /// Parse offset from GNU continuation header (bytes 369-380)
    fn parse_continuation_offset(header: &[u8]) -> u64 {
        if header.len() < 381 {
            return 0;
        }
        parse_numeric(&header[369..381]).unwrap_or(0)
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

    /// Read a header block, or `false` at end of file (see `formats::read_header`).
    fn read_header_from_reader(&mut self, buf: &mut [u8]) -> PaxResult<bool> {
        let reader = self
            .reader
            .as_mut()
            .ok_or_else(|| PaxError::Io(io::Error::other("no reader")))?;
        crate::formats::read_header(reader, buf)
    }

    /// Round up to next block boundary
    fn round_up_block(size: u64) -> u64 {
        // See the note in formats/ustar.rs: a declared size near u64::MAX
        // rounds up to 0 and the skip length then underflows.
        size.div_ceil(BLOCK_SIZE as u64)
            .saturating_mul(BLOCK_SIZE as u64)
    }
}

impl ArchiveReader for MultiVolumeReader {
    fn read_entry(&mut self) -> PaxResult<Option<ArchiveEntry>> {
        // Skip any remaining data from previous entry
        self.skip_data()?;

        loop {
            let mut header = [0u8; BLOCK_SIZE];
            let read = self.read_header_from_reader(&mut header);
            if !matches!(read, Ok(true)) {
                // Try opening next volume if this is a split file
                if self.in_split_file && self.open_next_volume()? {
                    continue;
                }
                // Nothing at all is the end of the archive; part of a header
                // is a truncated one.
                read?;
                return Ok(None);
            }

            // Check for end of archive (two zero blocks)
            if is_zero_block(&header) {
                // Check if there's another volume
                if self.open_next_volume()? {
                    continue;
                }
                return Ok(None);
            }

            // Verify checksum
            if !verify_checksum(&header) {
                return Err(PaxError::InvalidHeader("checksum mismatch".to_string()));
            }

            // Check if this is a continuation header
            if Self::is_continuation_header(&header) {
                // This is a continuation of a split file from previous volume
                let offset = Self::parse_continuation_offset(&header);
                let remaining_size = parse_numeric(&header[124..136])?;

                if self.options.verbose {
                    // A member name out of the archive, so it goes out as its
                    // bytes and is escaped only for a terminal -- the same
                    // treatment every other name-bearing diagnostic gets.
                    // `display()` here would both render an invalid byte as
                    // U+FFFD and let an escape sequence through.
                    let name = crate::rawpath::from_bytes(crate::formats::ustar::path_field(
                        &header[0..100],
                    ));
                    let mut line = Vec::new();
                    line.extend_from_slice(b"pax: continuation of '");
                    line.extend_from_slice(crate::rawpath::as_bytes(&name));
                    line.extend_from_slice(format!("' at offset {offset}").as_bytes());
                    crate::escape::write_stderr_line(&line);
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
            self.in_split_file = false;

            return Ok(Some(entry));
        }
    }

    fn read_data(&mut self, buf: &mut [u8]) -> PaxResult<usize> {
        let remaining = self.current_size.saturating_sub(self.bytes_read);
        if remaining == 0 {
            // Check if we need to switch to next volume for more data
            if self.in_split_file && self.total_bytes_read < self.total_entry_size {
                // Try to open next volume
                if self.open_next_volume()? {
                    // Read the continuation header
                    let mut header = [0u8; BLOCK_SIZE];
                    self.read_exact_from_reader(&mut header)?;

                    if Self::is_continuation_header(&header) {
                        let remaining_size = parse_numeric(&header[124..136])?;
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

        let remaining = self.current_size.saturating_sub(self.bytes_read);
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
            record_size: 512,
            writer: None,
            skip_data: false,
        };

        assert_eq!(writer.volume_path(1), PathBuf::from("/tmp/test.tar"));
        assert_eq!(writer.volume_path(2), PathBuf::from("/tmp/test.tar.2"));
        assert_eq!(writer.volume_path(3), PathBuf::from("/tmp/test.tar.3"));
    }

    #[test]
    fn test_volume_bytes_counts_trailer_and_record_padding() {
        let writer = MultiVolumeWriter {
            current_volume: 1,
            bytes_written: 0,
            volume_size: 20480,
            options: MultiVolumeOptions::default(),
            record_size: 10240,
            writer: None,
            skip_data: false,
        };
        assert_eq!(writer.volume_bytes(0), 10240);
        assert_eq!(writer.volume_bytes(9216), 10240);
        // No room left for the end-of-archive marker in the first record.
        assert_eq!(writer.volume_bytes(9728), 20480);
    }
}
