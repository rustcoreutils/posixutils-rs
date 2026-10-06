//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Archive format implementations

pub mod cpio;
pub mod pax;
pub mod ustar;

use crate::error::{PaxError, PaxResult};
use std::io::{Read, Seek, SeekFrom};

/// Largest extended-header record set (pax `x`/`g`) this will accept.
pub const MAX_EXTENDED_HEADER: u64 = 64 * 1024 * 1024;

/// Largest pathname or symbolic-link target this will accept from a header.
///
/// `PATH_MAX` is 4096 on Linux and 1024 on macOS; this is far above both, so it
/// rejects only lengths no filesystem could name anyway.
pub const MAX_NAME: u64 = 64 * 1024;

/// Read exactly the `len` bytes a header claims, without trusting `len` enough to
/// allocate it up front.
///
/// Header length fields are attacker-controlled. Sizing a buffer from one lets a
/// 512-byte archive declare 4 GiB and abort the process on the allocation alone,
/// before a single byte of it has been read. `limit` is what the format could
/// legitimately need; past that the archive is rejected by name. Within it the
/// buffer grows only as bytes actually arrive, so a header that lies about a
/// length it cannot supply costs what it delivers and then hits end-of-file.
pub fn read_declared<R: Read>(
    reader: &mut R,
    len: u64,
    limit: u64,
    what: &str,
) -> PaxResult<Vec<u8>> {
    if len > limit {
        return Err(PaxError::InvalidHeader(format!(
            "{what} of {len} bytes exceeds the {limit} byte limit"
        )));
    }

    let mut buf = Vec::new();
    let mut chunk = [0u8; 8192];
    let mut remaining = len as usize;
    while remaining > 0 {
        let n = remaining.min(chunk.len());
        reader.read_exact(&mut chunk[..n])?;
        buf.extend_from_slice(&chunk[..n]);
        remaining -= n;
    }
    Ok(buf)
}

/// An archive stream that knows how far into the archive it is, and steps
/// over member data by seeking when the archive is a seekable file.
///
/// The offset is what lets `-a` find where the end-of-archive indicator
/// starts by walking the archive exactly as reading it does. The seek is what
/// keeps that walk from reading every byte of a large member just to discard
/// it.
pub struct ArchiveStream<R> {
    inner: R,
    offset: u64,
    skip: fn(&mut R, u64) -> std::io::Result<()>,
}

impl<R: Read> ArchiveStream<R> {
    /// A stream that steps over data by reading it -- a pipe, a tape.
    pub fn new(inner: R) -> Self {
        ArchiveStream {
            inner,
            offset: 0,
            skip: read_over::<R>,
        }
    }

    /// Step over `count` bytes of member data.
    pub fn skip(&mut self, count: u64) -> PaxResult<()> {
        (self.skip)(&mut self.inner, count)?;
        self.offset = self.offset.saturating_add(count);
        Ok(())
    }

    /// Bytes consumed since the stream was created.
    pub fn offset(&self) -> u64 {
        self.offset
    }
}

impl<R: Read + Seek> ArchiveStream<R> {
    /// A stream over a seekable file, positioned at the start of the archive.
    pub fn seekable(inner: R) -> Self {
        ArchiveStream {
            inner,
            offset: 0,
            skip: seek_over::<R>,
        }
    }
}

impl<R: Read> Read for ArchiveStream<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let n = self.inner.read(buf)?;
        self.offset += n as u64;
        Ok(n)
    }
}

/// Step over bytes by reading and discarding them.
fn read_over<R: Read>(reader: &mut R, count: u64) -> std::io::Result<()> {
    let copied = std::io::copy(&mut reader.by_ref().take(count), &mut std::io::sink())?;
    if copied < count {
        return Err(std::io::ErrorKind::UnexpectedEof.into());
    }
    Ok(())
}

/// Step over bytes by seeking past them.
///
/// The last byte is read rather than seeked over: a seek past the end of a
/// file succeeds, and a member whose data the file does not hold is a
/// truncated archive, which reading would have reported.
fn seek_over<R: Read + Seek>(reader: &mut R, count: u64) -> std::io::Result<()> {
    if count == 0 {
        return Ok(());
    }
    let ahead = i64::try_from(count - 1).map_err(|_| std::io::ErrorKind::UnexpectedEof)?;
    reader.seek(SeekFrom::Current(ahead))?;
    reader.read_exact(&mut [0u8; 1])
}

pub use cpio::{checksum_bytes, CpioFormat, CpioReader, CpioWriter};
pub use pax::{PaxReader, PaxWriter};
pub use ustar::{UstarReader, UstarWriter};

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    /// Seeking and reading over data have to agree: the same offset after,
    /// and the same error for data the archive does not hold.
    #[test]
    fn test_skip_counts_and_detects_truncation() {
        let data = vec![7u8; 1024];
        for seek in [false, true] {
            let make = |d: &[u8]| {
                let c = Cursor::new(d.to_vec());
                if seek {
                    ArchiveStream::seekable(c)
                } else {
                    ArchiveStream::new(c)
                }
            };

            let mut stream = make(&data);
            stream.read_exact(&mut [0u8; 10]).unwrap();
            stream.skip(1014).unwrap();
            assert_eq!(stream.offset(), 1024, "seek={seek}");
            stream.skip(0).unwrap();

            let mut stream = make(&data);
            let err = stream.skip(1025).unwrap_err();
            assert!(
                matches!(&err, PaxError::Io(e) if e.kind() == std::io::ErrorKind::UnexpectedEof),
                "seek={seek}: {err}"
            );
        }
    }
}
