//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Blocked I/O for tape drives and other block devices
//!
//! Tar archives are organized as:
//! - **block**: 512 bytes (fundamental tar unit)
//! - **record**: multiple blocks written in a single I/O operation
//! - **blocking factor**: number of 512-byte blocks per record
//!
//! For tape drives and block devices, I/O must be performed at exact
//! record boundaries. This module provides readers and writers that
//! ensure all I/O operations are done at the specified block size.

use crate::error::PaxResult;
use std::io::{Read, Write};
use std::mem::ManuallyDrop;

/// Default blocking factor (number of 512-byte blocks per record)
pub const DEFAULT_BLOCKING_FACTOR: usize = 20;

/// Size of a single tar block in bytes
pub const TAR_BLOCK_SIZE: usize = 512;

/// Default record size in bytes (blocking factor * block size)
pub const DEFAULT_RECORD_SIZE: usize = DEFAULT_BLOCKING_FACTOR * TAR_BLOCK_SIZE;

/// Maximum record size per POSIX (32256 bytes = 63 blocks)
pub const MAX_RECORD_SIZE: usize = 32256;

/// A reader that reads data in fixed-size records
///
/// This is essential for reading from tape drives where each read()
/// must be exactly the right size to match what was written.
pub struct BlockedReader<R: Read> {
    reader: R,
    /// Size of each record in bytes
    record_size: usize,
    /// Buffer holding the current record
    buffer: Vec<u8>,
    /// Current position within the buffer
    pos: usize,
    /// Number of valid bytes in the buffer
    valid: usize,
    /// Whether we've reached EOF
    eof: bool,
}

impl<R: Read> BlockedReader<R> {
    /// Create a new blocked reader with the specified record size
    pub fn new(reader: R, record_size: usize) -> Self {
        BlockedReader {
            reader,
            record_size,
            buffer: vec![0u8; record_size],
            pos: 0,
            valid: 0,
            eof: false,
        }
    }

    /// Read the next record from the underlying reader
    ///
    /// For tape drives, this performs a single read() of exactly record_size bytes.
    /// Returns the number of bytes actually read (may be less at EOF).
    fn fill_buffer(&mut self) -> PaxResult<usize> {
        if self.eof {
            return Ok(0);
        }

        // Perform a single read of exactly record_size bytes
        // This is critical for tape drives
        let n = self.reader.read(&mut self.buffer)?;

        if n == 0 {
            self.eof = true;
            self.valid = 0;
            self.pos = 0;
            return Ok(0);
        }

        // For tape drives, a short read indicates end of data
        // Zero-fill the rest of the buffer for consistency
        if n < self.record_size {
            self.buffer[n..].fill(0);
        }

        self.valid = n;
        self.pos = 0;
        Ok(n)
    }
}

impl<R: Read> Read for BlockedReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        // If buffer is exhausted, read next record
        if self.pos >= self.valid {
            match self.fill_buffer() {
                Ok(0) => return Ok(0),
                Ok(_) => {}
                Err(e) => return Err(std::io::Error::other(e.to_string())),
            }
        }

        // Copy from buffer to output
        let available = self.valid - self.pos;
        let to_copy = std::cmp::min(available, buf.len());
        buf[..to_copy].copy_from_slice(&self.buffer[self.pos..self.pos + to_copy]);
        self.pos += to_copy;

        Ok(to_copy)
    }
}

/// A writer that writes data in fixed-size records
///
/// This is essential for writing to tape drives where each write()
/// must be exactly the specified size.
pub struct BlockedWriter<W: Write> {
    writer: ManuallyDrop<W>,
    /// Size of each record in bytes
    record_size: usize,
    /// Buffer holding the current record being built
    buffer: Vec<u8>,
    /// Current position within the buffer
    pos: usize,
    /// Whether finish() has been called (to avoid double-flush in Drop)
    finished: bool,
}

impl<W: Write> BlockedWriter<W> {
    /// Create a new blocked writer with the specified record size
    pub fn new(writer: W, record_size: usize) -> Self {
        BlockedWriter {
            writer: ManuallyDrop::new(writer),
            record_size,
            buffer: vec![0u8; record_size],
            pos: 0,
            finished: false,
        }
    }

    /// Flush the current record to the underlying writer
    ///
    /// This writes exactly record_size bytes, zero-padding if necessary.
    fn flush_record(&mut self) -> std::io::Result<()> {
        if self.pos == 0 {
            return Ok(());
        }

        // Zero-fill the rest of the record
        self.buffer[self.pos..].fill(0);

        // Write exactly one record
        self.writer.write_all(&self.buffer)?;

        self.pos = 0;
        Ok(())
    }

    /// Finish writing and flush any remaining data
    ///
    /// This ensures the final record is written (with zero padding).
    /// Returns the underlying writer for further use.
    #[cfg(test)]
    pub fn finish(mut self) -> std::io::Result<W> {
        self.flush_record()?;
        self.writer.flush()?;
        self.finished = true;
        // SAFETY: We've marked finished=true so Drop won't try to use the writer
        unsafe { Ok(ManuallyDrop::take(&mut self.writer)) }
    }
}

impl<W: Write> Write for BlockedWriter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let mut written = 0;

        while written < buf.len() {
            let space = self.record_size - self.pos;
            let to_copy = std::cmp::min(space, buf.len() - written);

            self.buffer[self.pos..self.pos + to_copy]
                .copy_from_slice(&buf[written..written + to_copy]);
            self.pos += to_copy;
            written += to_copy;

            // If record is full, flush it
            if self.pos >= self.record_size {
                self.flush_record()?;
            }
        }

        Ok(written)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        // For blocked I/O, flush writes a complete record if there's pending data
        self.flush_record()?;
        self.writer.flush()
    }
}

impl<W: Write> Drop for BlockedWriter<W> {
    fn drop(&mut self) {
        if !self.finished {
            // Try to flush any remaining data, ignore errors in drop
            let _ = self.flush_record();
            let _ = self.writer.flush();
        }
        // Note: We don't drop the writer here if finished=true because
        // ManuallyDrop::take already took ownership in finish()
    }
}

/// Calculate record size from a blocksize specification
///
/// The blocksize can be specified as:
/// - Bytes directly (if >= 512)
/// - Blocking factor (if < 512, multiply by 512)
///
/// Returns the record size in bytes, clamped to valid range.
pub fn parse_blocksize(blocksize: u32) -> usize {
    let size = if blocksize < TAR_BLOCK_SIZE as u32 {
        // Treat as blocking factor
        blocksize as usize * TAR_BLOCK_SIZE
    } else {
        blocksize as usize
    };

    // Clamp to valid range and round up to block boundary
    let size = std::cmp::max(size, TAR_BLOCK_SIZE);
    let size = std::cmp::min(size, MAX_RECORD_SIZE);

    // Round up to nearest block boundary
    size.div_ceil(TAR_BLOCK_SIZE) * TAR_BLOCK_SIZE
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_parse_blocksize() {
        // Blocking factor
        assert_eq!(parse_blocksize(1), 512);
        assert_eq!(parse_blocksize(20), 10240);
        assert_eq!(parse_blocksize(63), 32256);

        // Direct bytes
        assert_eq!(parse_blocksize(512), 512);
        assert_eq!(parse_blocksize(1024), 1024);
        assert_eq!(parse_blocksize(10240), 10240);

        // Clamping
        assert_eq!(parse_blocksize(0), 512);
        assert_eq!(parse_blocksize(100000), MAX_RECORD_SIZE);

        // Rounding
        assert_eq!(parse_blocksize(1000), 1024); // rounds up to 2 blocks
    }

    #[test]
    fn test_blocked_writer_basic() {
        let output = Vec::new();
        let mut writer = BlockedWriter::new(output, 1024);

        // Write some data
        writer.write_all(b"Hello, World!").unwrap();

        // Finish and get the output
        let result = writer.finish().unwrap();

        // Should have written exactly one record (1024 bytes)
        assert_eq!(result.len(), 1024);
        assert_eq!(&result[..13], b"Hello, World!");
        // Rest should be zeros
        assert!(result[13..].iter().all(|&b| b == 0));
    }

    #[test]
    fn test_blocked_writer_multiple_records() {
        let output = Vec::new();
        let mut writer = BlockedWriter::new(output, 512);

        // Write more than one record
        let data = vec![0x42u8; 1000];
        writer.write_all(&data).unwrap();

        let result = writer.finish().unwrap();

        // Should have written 2 records (1024 bytes)
        assert_eq!(result.len(), 1024);
        assert_eq!(&result[..1000], &data[..]);
        // Rest should be zeros
        assert!(result[1000..].iter().all(|&b| b == 0));
    }

    #[test]
    fn test_blocked_reader_basic() {
        // Create a buffer with exactly one record
        let mut data = vec![0u8; 1024];
        data[..5].copy_from_slice(b"Hello");

        let cursor = Cursor::new(data);
        let mut reader = BlockedReader::new(cursor, 1024);

        let mut buf = [0u8; 100];
        let n = reader.read(&mut buf).unwrap();

        assert_eq!(n, 100);
        assert_eq!(&buf[..5], b"Hello");
    }

    #[test]
    fn test_blocked_reader_multiple_records() {
        // Create two records
        let mut data = vec![0u8; 2048];
        data[..5].copy_from_slice(b"Hello");
        data[1024..1029].copy_from_slice(b"World");

        let cursor = Cursor::new(data);
        let mut reader = BlockedReader::new(cursor, 1024);

        // Read across record boundary
        let mut buf = vec![0u8; 2048];
        let n = reader.read(&mut buf).unwrap();
        assert_eq!(n, 1024); // First record

        let n = reader.read(&mut buf[1024..]).unwrap();
        assert_eq!(n, 1024); // Second record

        assert_eq!(&buf[..5], b"Hello");
        assert_eq!(&buf[1024..1029], b"World");
    }

    #[test]
    fn test_roundtrip() {
        let original = b"The quick brown fox jumps over the lazy dog.";

        // Write with blocking
        let output = Vec::new();
        let mut writer = BlockedWriter::new(output, 512);
        writer.write_all(original).unwrap();
        let written = writer.finish().unwrap();

        // Read with blocking
        let cursor = Cursor::new(written);
        let mut reader = BlockedReader::new(cursor, 512);
        let mut result = vec![0u8; original.len()];
        reader.read_exact(&mut result).unwrap();

        assert_eq!(&result[..], original);
    }
}
