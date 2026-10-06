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
//! A tape drive transfers one record per read(2) or write(2). The writer
//! here issues every write as one whole record of the requested size; the
//! reader issues reads large enough for any record, so the blocking of an
//! archive being read is whatever it was written with.

use crate::error::{PaxError, PaxResult};
use std::io::{BufRead, Read, Write};
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

/// How much every read of the archive asks for.
///
/// A tape drive -- or a datagram socket -- returns one record per read, and
/// drops whatever part of that record the read had no room for. So a read is
/// never smaller than this: larger than any record POSIX lets pax write
/// (`MAX_RECORD_SIZE`) and than BSD pax's 64512-byte maximum.
pub const READ_SIZE: usize = 64 * 1024;

/// A reader for an archive in records of unknown size
///
/// "Blocking shall be automatically determined on input": each read(2)
/// offers `READ_SIZE` bytes, so a device that returns one record per read
/// hands over a whole record, whatever its size, and the record size is
/// simply what the reads return. A pipe or file returning fewer bytes than a
/// record is no different -- what came back is served, and the next read
/// picks up where it left off.
///
/// It is also the one place the raw archive is buffered: looking ahead at
/// the archive (`peek`, `BufRead`) reads through it rather than past it.
pub struct BlockedReader<R: Read> {
    reader: R,
    /// Bytes read and not yet consumed are `buffer[pos..valid]`
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
    /// Create a blocked reader
    pub fn new(reader: R) -> Self {
        Self::with_counter(reader, ByteCounter::default())
    }

    /// Create a blocked reader that adds every byte it reads to `counter`
    pub fn with_counter(reader: R, counter: ByteCounter) -> Self {
        BlockedReader {
            reader,
            buffer: Vec::new(),
            pos: 0,
            valid: 0,
            eof: false,
            counter,
        }
    }

    /// Read until `n` bytes are buffered or the archive ends, and return
    /// the buffered bytes without consuming them.
    pub fn peek(&mut self, n: usize) -> std::io::Result<&[u8]> {
        while self.valid - self.pos < n && !self.eof {
            self.read_more()?;
        }
        Ok(&self.buffer[self.pos..self.valid])
    }

    /// One read(2) of `READ_SIZE` bytes, kept after what is already buffered.
    ///
    /// The buffer grows to make room rather than shrinking the read, which
    /// would cut a record short.
    fn read_more(&mut self) -> std::io::Result<()> {
        if self.pos == self.valid {
            self.pos = 0;
            self.valid = 0;
        }
        let end = self.valid + READ_SIZE;
        if self.buffer.len() < end {
            self.buffer.resize(end, 0);
        }
        let n = loop {
            match self.reader.read(&mut self.buffer[self.valid..end]) {
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => {}
                result => break result?,
            }
        };
        if n == 0 {
            self.eof = true;
        }
        self.counter.fetch_add(n as u64, Ordering::Relaxed);
        self.valid += n;
        Ok(())
    }
}

impl<R: Read> BufRead for BlockedReader<R> {
    fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
        if self.pos == self.valid && !self.eof {
            self.read_more()?;
        }
        Ok(&self.buffer[self.pos..self.valid])
    }

    fn consume(&mut self, amt: usize) {
        self.pos = std::cmp::min(self.pos + amt, self.valid);
    }
}

impl<R: Read> Read for BlockedReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let available = self.fill_buf()?;
        let to_copy = std::cmp::min(available.len(), buf.len());
        buf[..to_copy].copy_from_slice(&available[..to_copy]);
        self.consume(to_copy);
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
    /// Whether the last record is padded out to a whole one
    pad_last: bool,
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
            pad_last: true,
            counter,
        }
    }

    /// Leave the last record as short as its data.
    ///
    /// For compressed output to a regular file, as bsdtar does: no device
    /// needs the padding there, and gzip(1) on BSD and macOS warns about zeros
    /// after the gzip trailer. Every record but the last is still whole.
    pub fn unpadded_last_record(mut self) -> Self {
        self.pad_last = false;
        self
    }

    /// Flush the current record to the underlying writer
    ///
    /// This writes exactly record_size bytes, zero-padding if necessary --
    /// except a last record left short by `unpadded_last_record`.
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
        if self.pad_last {
            self.buffer[self.pos..].fill(0);
            self.pos = self.record_size;
        }
        let len = self.pos;

        while self.sent < len {
            match self.writer.write(&self.buffer[self.sent..len]) {
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
    fn test_blocked_writer_unpadded_last_record() {
        let mut writer = BlockedWriter::new(Vec::new(), 512).unpadded_last_record();
        writer.write_all(&[9u8; 700]).unwrap();
        let out = writer.finish().unwrap();
        assert_eq!(out, vec![9u8; 700]);
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

    /// Returns one record per read, as a tape drive does, and loses the
    /// part of the record a read had no room for.
    struct Tape {
        records: Vec<Vec<u8>>,
    }

    impl Read for Tape {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            if self.records.is_empty() {
                return Ok(0);
            }
            let record = self.records.remove(0);
            let n = record.len().min(buf.len());
            buf[..n].copy_from_slice(&record[..n]);
            Ok(n)
        }
    }

    #[test]
    fn test_blocked_reader_takes_whole_records() {
        let data: Vec<u8> = (0..3 * 10240u32).map(|i| (i % 251) as u8).collect();
        let tape = Tape {
            records: data.chunks(10240).map(<[u8]>::to_vec).collect(),
        };
        let counter = ByteCounter::default();
        let mut reader = BlockedReader::with_counter(tape, ByteCounter::clone(&counter));

        // Small reads by the caller never become small reads of the tape.
        let mut out = Vec::new();
        let mut buf = [0u8; 512];
        loop {
            let n = reader.read(&mut buf).unwrap();
            if n == 0 {
                break;
            }
            out.extend_from_slice(&buf[..n]);
        }
        assert_eq!(out, data);
        assert_eq!(counter.load(Ordering::Relaxed), data.len() as u64);
    }

    #[test]
    fn test_blocked_reader_peek_spans_reads() {
        // A pipe may return a byte at a time; peek keeps reading, and every
        // read still offers a full READ_SIZE.
        let data = b"\x1f\x8brest".to_vec();
        let tape = Tape {
            records: data.chunks(1).map(<[u8]>::to_vec).collect(),
        };
        let mut reader = BlockedReader::new(tape);
        assert_eq!(reader.peek(2).unwrap(), b"\x1f\x8b");
        let mut out = Vec::new();
        reader.read_to_end(&mut out).unwrap();
        assert_eq!(out, data);
    }

    #[test]
    fn test_blocked_reader_peek_stops_at_eof() {
        let mut reader = BlockedReader::new(Cursor::new(vec![7u8; 3]));
        assert_eq!(reader.peek(512).unwrap(), &[7u8; 3]);
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
        let mut reader = BlockedReader::new(cursor);
        let mut result = vec![0u8; original.len()];
        reader.read_exact(&mut result).unwrap();

        assert_eq!(&result[..], original);
    }
}
