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

use libflate::gzip;
use std::io::{self, Read, Write};

/// Gzip magic bytes (first two bytes of a gzip file)
pub const GZIP_MAGIC: [u8; 2] = [0x1f, 0x8b];

/// Check if data starts with gzip magic bytes
pub fn is_gzip(data: &[u8]) -> bool {
    data.len() >= 2 && data[0] == GZIP_MAGIC[0] && data[1] == GZIP_MAGIC[1]
}

/// Gzip decompression wrapper for Read streams
pub struct GzipReader<R: Read> {
    decoder: gzip::Decoder<R>,
}

impl<R: Read> GzipReader<R> {
    /// Create a new gzip decompressor wrapping the given reader
    pub fn new(reader: R) -> io::Result<Self> {
        let decoder = gzip::Decoder::new(reader)?;
        Ok(GzipReader { decoder })
    }
}

impl<R: Read> Read for GzipReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.decoder.read(buf)
    }
}

/// Gzip compression wrapper for Write streams
pub struct GzipWriter<W: Write> {
    encoder: Option<gzip::Encoder<W>>,
}

impl<W: Write> GzipWriter<W> {
    /// Create a new gzip compressor wrapping the given writer
    pub fn new(writer: W) -> io::Result<Self> {
        let encoder = gzip::Encoder::new(writer)?;
        Ok(GzipWriter {
            encoder: Some(encoder),
        })
    }
}

impl<W: Write> Write for GzipWriter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if let Some(ref mut encoder) = self.encoder {
            encoder.write(buf)
        } else {
            Err(io::Error::other("GzipWriter already finished"))
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        if let Some(ref mut encoder) = self.encoder {
            encoder.flush()
        } else {
            Ok(())
        }
    }
}

impl<W: Write> Drop for GzipWriter<W> {
    fn drop(&mut self) {
        // If encoder hasn't been finished yet, finish it now
        if let Some(encoder) = self.encoder.take() {
            // Ignore errors during drop - nothing we can do about them
            let _ = encoder.finish();
        }
    }
}

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
}
