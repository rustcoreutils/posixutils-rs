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

use crate::error::{PaxError, PaxResult};
use std::io::{Read, Write};
use std::mem::ManuallyDrop;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

/// A running count of archive bytes moved through a `BlockedReader` or
/// `BlockedWriter`.
///
/// cpio reports the size of the archive it just read or wrote as a count of
/// 512-byte blocks, and the blocked layer is the only place that sees the whole
/// stream. The handle is shared so a caller can keep it after the reader or
/// writer has been moved into the mode implementation.
pub type ByteCounter = Arc<AtomicU64>;

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
    /// Total bytes read from the underlying reader
    counter: ByteCounter,
}

impl<R: Read> BlockedReader<R> {
    /// Create a blocked reader that adds every byte it reads to `counter`
    pub fn with_counter(reader: R, record_size: usize, counter: ByteCounter) -> Self {
        BlockedReader {
            reader,
            record_size,
            buffer: vec![0u8; record_size],
            pos: 0,
            valid: 0,
            eof: false,
            counter,
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

        self.counter.fetch_add(n as u64, Ordering::Relaxed);
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
    /// How much of a full record the underlying writer has already accepted
    sent: usize,
    /// Whether finish() has been called (to avoid double-flush in Drop)
    finished: bool,
    /// Total bytes written to the underlying writer
    counter: ByteCounter,
}

impl<W: Write> BlockedWriter<W> {
    /// Create a new blocked writer with the specified record size
    pub fn new(writer: W, record_size: usize) -> Self {
        Self::with_counter(writer, record_size, ByteCounter::default())
    }

    /// Create a blocked writer that adds every byte it writes to `counter`
    pub fn with_counter(writer: W, record_size: usize, counter: ByteCounter) -> Self {
        BlockedWriter {
            writer: ManuallyDrop::new(writer),
            record_size,
            buffer: vec![0u8; record_size],
            pos: 0,
            sent: 0,
            finished: false,
            counter,
        }
    }

    /// Flush the current record to the underlying writer
    ///
    /// This writes exactly record_size bytes, zero-padding if necessary.
    ///
    /// Bytes the underlying writer has accepted are never offered to it again.
    /// A write can be partial -- a pipe, a file at its size limit -- and fail on
    /// the next call; starting the record over on a retry would put the
    /// accepted part into the stream twice. So progress is kept in `sent`, and
    /// once this has been called the record is committed: `pos` stays at the
    /// end of it until all of it is out.
    ///
    /// An interrupted write is retried. Any other error is returned as it is,
    /// EAGAIN on a non-blocking descriptor included: nothing here knows the
    /// descriptor to wait on, and the caller treats a failed archive write as
    /// the end of the run (`PaxError::ArchiveWrite`).
    fn flush_record(&mut self) -> std::io::Result<()> {
        if self.pos == 0 {
            return Ok(());
        }

        // Zero-fill the rest of the record
        self.buffer[self.pos..].fill(0);
        self.pos = self.record_size;

        while self.sent < self.record_size {
            match self.writer.write(&self.buffer[self.sent..]) {
                Ok(0) => return Err(std::io::ErrorKind::WriteZero.into()),
                Ok(n) => {
                    self.sent += n;
                    self.counter.fetch_add(n as u64, Ordering::Relaxed);
                }
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {}
                Err(e) => return Err(e),
            }
        }

        self.pos = 0;
        self.sent = 0;
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
    /// Take up to one record's worth of `buf`.
    ///
    /// A full record goes out when the next byte arrives (or on `flush`), not
    /// when it fills: an error is then returned only from a call that has taken
    /// none of its `buf`, as `Write` requires, and a retry resumes the record
    /// where the underlying writer left off.
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if buf.is_empty() {
            return Ok(0);
        }
        if self.pos == self.record_size {
            self.flush_record()?;
        }

        let to_copy = std::cmp::min(self.record_size - self.pos, buf.len());
        self.buffer[self.pos..self.pos + to_copy].copy_from_slice(&buf[..to_copy]);
        self.pos += to_copy;
        Ok(to_copy)
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
            // SAFETY: We must drop the inner writer to ensure it finalizes properly
            // (e.g., GzipWriter needs Drop to write the gzip trailer)
            // ManuallyDrop::take was not called, so we own the writer
            unsafe {
                ManuallyDrop::drop(&mut self.writer);
            }
        }
        // Note: We don't drop the writer here if finished=true because
        // ManuallyDrop::take already took ownership in finish()
    }
}

/// Validate a `-b` blocksize argument and return the record size in bytes.
///
/// Per POSIX, `-b` is "a positive decimal integer number of bytes per write"
/// (not a GNU blocking factor), and it must be a multiple of 512 not exceeding
/// the 32256-byte maximum. Out-of-range, zero, and non-multiple values are
/// diagnosed and rejected rather than silently clamped or rounded.
pub fn parse_blocksize(blocksize: u32) -> PaxResult<usize> {
    let size = blocksize as usize;

    if size == 0 {
        return Err(PaxError::InvalidFormat(
            "blocksize (-b) must be a positive number of bytes".to_string(),
        ));
    }
    if !size.is_multiple_of(TAR_BLOCK_SIZE) {
        return Err(PaxError::InvalidFormat(format!(
            "blocksize (-b) must be a multiple of {} bytes",
            TAR_BLOCK_SIZE
        )));
    }
    if size > MAX_RECORD_SIZE {
        return Err(PaxError::InvalidFormat(format!(
            "blocksize (-b) must not exceed {} bytes",
            MAX_RECORD_SIZE
        )));
    }

    Ok(size)
}

/// Default record size in bytes for a freshly created archive of `format`.
///
/// POSIX `-x` specifies the cpio and pax interchange formats default to 5120
/// bytes (10×512); the ustar format defaults to 10240 (20×512).
pub fn default_record_size(format: crate::archive::ArchiveFormat) -> usize {
    use crate::archive::ArchiveFormat;
    match format {
        ArchiveFormat::Ustar => DEFAULT_RECORD_SIZE,
        ArchiveFormat::Cpio | ArchiveFormat::Pax => 10 * TAR_BLOCK_SIZE,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_parse_blocksize() {
        // -b is a byte count, a multiple of 512 up to 32256.
        assert_eq!(parse_blocksize(512).unwrap(), 512);
        assert_eq!(parse_blocksize(1024).unwrap(), 1024);
        assert_eq!(parse_blocksize(10240).unwrap(), 10240);
        assert_eq!(
            parse_blocksize(MAX_RECORD_SIZE as u32).unwrap(),
            MAX_RECORD_SIZE
        );

        // Rejected: zero, non-multiple-of-512, out-of-range. No silent clamp,
        // no GNU blocking-factor interpretation of small values.
        assert!(parse_blocksize(0).is_err());
        assert!(parse_blocksize(20).is_err()); // not a multiple of 512
        assert!(parse_blocksize(1000).is_err()); // not a multiple of 512
        assert!(parse_blocksize(100000).is_err()); // exceeds the maximum
        assert!(parse_blocksize(MAX_RECORD_SIZE as u32 + 512).is_err());
    }

    #[test]
    fn test_default_record_size() {
        use crate::archive::ArchiveFormat;
        assert_eq!(default_record_size(ArchiveFormat::Ustar), 10240);
        assert_eq!(default_record_size(ArchiveFormat::Cpio), 5120);
        assert_eq!(default_record_size(ArchiveFormat::Pax), 5120);
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

    /// Is interrupted `interrupts` times, then accepts `budget` bytes in
    /// total, a few at a time, then fails every write with WouldBlock until
    /// given more budget.
    struct ChokingWriter {
        out: Vec<u8>,
        interrupts: usize,
        budget: usize,
    }

    impl ChokingWriter {
        fn new(interrupts: usize, budget: usize) -> Self {
            ChokingWriter {
                out: Vec::new(),
                interrupts,
                budget,
            }
        }
    }

    impl Write for ChokingWriter {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            if self.interrupts > 0 {
                self.interrupts -= 1;
                return Err(std::io::ErrorKind::Interrupted.into());
            }
            if self.budget == 0 {
                return Err(std::io::ErrorKind::WouldBlock.into());
            }
            let n = buf.len().min(self.budget).min(300);
            self.out.extend_from_slice(&buf[..n]);
            self.budget -= n;
            Ok(n)
        }

        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    #[test]
    fn test_blocked_writer_never_resends_accepted_bytes() {
        let data: Vec<u8> = (0..2048u32).map(|i| (i % 251) as u8).collect();
        let mut writer = BlockedWriter::new(ChokingWriter::new(0, 700), 1024);

        // The first record is accepted 300 + 300 + 100 bytes, then refused.
        let err = writer.write_all(&data).unwrap_err();
        assert_eq!(err.kind(), std::io::ErrorKind::WouldBlock);
        assert_eq!(writer.writer.out, &data[..700]);

        // Once the writer can take more, the stream picks up at byte 700 --
        // not at the start of the record, which would duplicate 700 bytes --
        // and the 1024 bytes the failed call did not take are taken now.
        writer.writer.budget = usize::MAX;
        writer.write_all(&data[1024..]).unwrap();
        let out = writer.finish().unwrap().out;
        assert_eq!(out, data);
    }

    #[test]
    fn test_blocked_writer_retries_interrupted() {
        let mut writer = BlockedWriter::new(ChokingWriter::new(3, usize::MAX), 512);
        writer.write_all(&[7u8; 1000]).unwrap();
        let out = writer.finish().unwrap().out;
        assert_eq!(&out[..1000], &[7u8; 1000]);
        assert_eq!(out.len(), 1024);
    }

    #[test]
    fn test_blocked_reader_basic() {
        // Create a buffer with exactly one record
        let mut data = vec![0u8; 1024];
        data[..5].copy_from_slice(b"Hello");

        let cursor = Cursor::new(data);
        let mut reader = BlockedReader::with_counter(cursor, 1024, ByteCounter::default());

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
        let mut reader = BlockedReader::with_counter(cursor, 1024, ByteCounter::default());

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
        let mut reader = BlockedReader::with_counter(cursor, 512, ByteCounter::default());
        let mut result = vec![0u8; original.len()];
        reader.read_exact(&mut result).unwrap();

        assert_eq!(&result[..], original);
    }
}
