//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Gzip compression support for pax archives
//!
//! This module provides transparent gzip compression and decompression
//! as a filter layer that wraps Read/Write streams.

use flate2::bufread::GzDecoder;
use flate2::write::GzEncoder;
use flate2::Compression;
use std::io::{self, BufRead, Read, Write};

/// Gzip magic bytes (first two bytes of a gzip file)
pub const GZIP_MAGIC: [u8; 2] = [0x1f, 0x8b];

/// Check if data starts with gzip magic bytes
pub fn is_gzip(data: &[u8]) -> bool {
    data.len() >= 2 && data[0] == GZIP_MAGIC[0] && data[1] == GZIP_MAGIC[1]
}

/// Gzip decompression wrapper for buffered Read streams
///
/// A gzip stream may hold more than one deflate member -- `gzip -c a >> x.gz`,
/// `cat a.gz b.gz` and bgzip all produce one -- and stopping at the end of the
/// first is silent truncation of the archive. So after each member, a next
/// one is started if there is more input.
///
/// More input that starts with a zero byte is not another member but the zero
/// padding of the last record: pax blocks the compressed stream (see
/// `GzipWriter`), as do tar implementations writing to a tape, and gzip(1)
/// itself ignores trailing zeros. `MultiGzDecoder` would reject them as a bad
/// header.
pub struct GzipReader<R: BufRead> {
    /// The member being decoded; `None` once the stream has ended
    decoder: Option<GzDecoder<R>>,
}

impl<R: BufRead> GzipReader<R> {
    /// Create a new gzip decompressor wrapping the given reader
    ///
    /// A malformed gzip header is reported by the first [`Read::read`] rather
    /// than here, because the decoder parses the header lazily.
    pub fn new(reader: R) -> io::Result<Self> {
        Ok(GzipReader {
            decoder: Some(GzDecoder::new(reader)),
        })
    }
}

impl<R: BufRead> Read for GzipReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        loop {
            let Some(decoder) = self.decoder.as_mut() else {
                return Ok(0);
            };
            let n = decoder.read(buf)?;
            if n > 0 || buf.is_empty() {
                return Ok(n);
            }
            // The member has ended; the bufread decoder has consumed exactly it.
            let Some(mut inner) = self.decoder.take().map(GzDecoder::into_inner) else {
                return Ok(0);
            };
            let next = inner.fill_buf()?;
            if next.first().is_some_and(|&b| b != 0) {
                self.decoder = Some(GzDecoder::new(inner));
            }
        }
    }
}

/// Gzip compression wrapper for Write streams
///
/// Under it is the blocked archive writer, so the compressed stream is what is
/// written in records. The archive writers flush once, when the archive is
/// complete, and that flush is what ends the gzip stream: a flush of the
/// blocked writer pads out the last record, and that padding has to come after
/// the gzip trailer, not before it.
pub struct GzipWriter<W: Write> {
    encoder: GzEncoder<W>,
    /// Whether `flush` has ended the stream
    finished: bool,
}

impl<W: Write> GzipWriter<W> {
    /// Create a new gzip compressor wrapping the given writer
    pub fn new(writer: W) -> io::Result<Self> {
        Ok(GzipWriter {
            encoder: GzEncoder::new(writer, Compression::default()),
            finished: false,
        })
    }
}

impl<W: Write> Write for GzipWriter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if self.finished {
            return Err(io::Error::other("GzipWriter already finished"));
        }
        self.encoder.write(buf)
    }

    /// End the gzip stream, then flush what is under it.
    fn flush(&mut self) -> io::Result<()> {
        self.encoder.try_finish()?;
        self.finished = true;
        self.encoder.get_mut().flush()
    }
}

// Dropping a GzipWriter that was never flushed still ends the stream:
// GzEncoder's own Drop writes the trailer.

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_is_gzip() {
        assert!(is_gzip(&[0x1f, 0x8b, 0x08]));
        assert!(is_gzip(&GZIP_MAGIC));
        assert!(!is_gzip(&[0x00, 0x00]));
        assert!(!is_gzip(&[0x1f])); // Too short
        assert!(!is_gzip(&[]));
    }

    #[test]
    fn test_gzip_roundtrip() {
        let original = b"Hello, World! This is a test of gzip compression.";

        // Compress (Drop finishes the gzip stream)
        let mut compressed = Vec::new();
        {
            let mut writer = GzipWriter::new(&mut compressed).unwrap();
            writer.write_all(original).unwrap();
            // Drop triggers finish
        }

        // Verify it's actually gzip
        assert!(is_gzip(&compressed));

        // Decompress
        let mut decompressed = Vec::new();
        {
            let mut reader = GzipReader::new(Cursor::new(&compressed)).unwrap();
            reader.read_to_end(&mut decompressed).unwrap();
        }

        assert_eq!(decompressed, original);
    }

    #[test]
    fn test_gzip_large_data() {
        // Test with larger data to ensure streaming works
        let original: Vec<u8> = (0..10000).map(|i| (i % 256) as u8).collect();

        // Compress (Drop finishes the gzip stream)
        let mut compressed = Vec::new();
        {
            let mut writer = GzipWriter::new(&mut compressed).unwrap();
            writer.write_all(&original).unwrap();
            // Drop triggers finish
        }

        // Decompress
        let mut decompressed = Vec::new();
        {
            let mut reader = GzipReader::new(Cursor::new(&compressed)).unwrap();
            reader.read_to_end(&mut decompressed).unwrap();
        }

        assert_eq!(decompressed, original);
    }

    fn gzip(data: &[u8]) -> Vec<u8> {
        let mut compressed = Vec::new();
        let mut writer = GzipWriter::new(&mut compressed).unwrap();
        writer.write_all(data).unwrap();
        writer.flush().unwrap();
        drop(writer);
        compressed
    }

    #[test]
    fn test_gzip_multi_member_and_record_padding() {
        // Two members, then the zero padding of a blocked last record.
        let mut stream = gzip(b"first ");
        stream.extend(gzip(b"second"));
        stream.extend([0u8; 700]);

        let mut decompressed = Vec::new();
        GzipReader::new(Cursor::new(stream))
            .unwrap()
            .read_to_end(&mut decompressed)
            .unwrap();
        assert_eq!(decompressed, b"first second");
    }

    #[test]
    fn test_gzip_flush_ends_the_stream() {
        let mut compressed = Vec::new();
        let mut writer = GzipWriter::new(&mut compressed).unwrap();
        writer.write_all(b"data").unwrap();
        writer.flush().unwrap();
        assert!(writer.write(b"more").is_err());
        drop(writer);
        // Dropping after the flush adds nothing past the trailer.
        assert_eq!(compressed, gzip(b"data"));
    }
}
