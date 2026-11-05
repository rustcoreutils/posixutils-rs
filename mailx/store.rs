//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The session's private copy of a mailbox.
//!
//! As in BSD and Heirloom mailx, a mailbox is copied into an unnamed
//! temporary file as it is read, and messages are read back from that copy
//! when needed: memory holds where each message lies, not its text. The copy
//! has no name, so the kernel reclaims it whenever mailx exits, however it
//! exits; another process rewriting the mailbox cannot disturb it.

use std::fs::File;
use std::io::{self, Write};
use std::os::unix::fs::FileExt;

/// A run of bytes in the store.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Span {
    pub offset: u64,
    pub len: u64,
}

impl Span {
    /// The span following this one, `len` bytes long.
    pub fn after(self, len: u64) -> Span {
        Span {
            offset: self.offset + self.len,
            len,
        }
    }
}

/// Bytes written to the store but not yet to its file.
const BUFFER: usize = 64 * 1024;

#[derive(Debug, Default)]
pub struct Store {
    /// Created on the first append, so a mailbox with no messages -- a
    /// start-up file's scratch mailbox, an empty spool -- needs none.
    file: Option<File>,
    /// Bytes appended so far, written or still buffered.
    len: u64,
    pending: Vec<u8>,
}

impl Store {
    /// Append `bytes`, returning where they lie.
    ///
    /// Appends are buffered; [`Store::flush`] must run before the bytes are
    /// read back.
    pub fn append(&mut self, bytes: &[u8]) -> io::Result<Span> {
        let span = Span {
            offset: self.len,
            len: bytes.len() as u64,
        };
        self.pending.extend_from_slice(bytes);
        self.len += span.len;
        if self.pending.len() >= BUFFER {
            self.flush()?;
        }
        Ok(span)
    }

    /// Write out buffered appends.
    pub fn flush(&mut self) -> io::Result<()> {
        if self.pending.is_empty() {
            return Ok(());
        }
        let file = match &mut self.file {
            Some(file) => file,
            None => self.file.insert(plib::tmp::tempfile()?),
        };
        let at = self.len - self.pending.len() as u64;
        file.write_all_at(&self.pending, at)?;
        self.pending.clear();
        Ok(())
    }

    /// The bytes of `span`.
    pub fn read(&self, span: Span) -> io::Result<Vec<u8>> {
        let mut buf = vec![0; span.len as usize];
        if span.len > 0 {
            self.file()?.read_exact_at(&mut buf, span.offset)?;
        }
        Ok(buf)
    }

    /// Copy the bytes of `span` to `w`, a buffer at a time.
    pub fn copy_to(&self, span: Span, w: &mut impl Write) -> io::Result<()> {
        let mut buf = vec![0; BUFFER.min(span.len as usize)];
        let mut done = 0;
        while done < span.len {
            let n = buf.len().min((span.len - done) as usize);
            self.file()?
                .read_exact_at(&mut buf[..n], span.offset + done)?;
            w.write_all(&buf[..n])?;
            done += n as u64;
        }
        Ok(())
    }

    /// A buffered reader over the bytes of `span`.
    pub fn reader(&self, span: Span) -> io::BufReader<SpanReader<'_>> {
        io::BufReader::with_capacity(BUFFER, SpanReader { store: self, span })
    }

    fn file(&self) -> io::Result<&File> {
        debug_assert!(self.pending.is_empty(), "store read before flush");
        self.file
            .as_ref()
            .ok_or_else(|| io::Error::new(io::ErrorKind::UnexpectedEof, "empty store"))
    }
}

/// Reads the bytes of one span of a store.
pub struct SpanReader<'a> {
    store: &'a Store,
    /// What is left to read.
    span: Span,
}

impl io::Read for SpanReader<'_> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let n = buf.len().min(self.span.len as usize);
        if n == 0 {
            return Ok(0);
        }
        let n = self
            .store
            .file()?
            .read_at(&mut buf[..n], self.span.offset)?;
        self.span.offset += n as u64;
        self.span.len -= n as u64;
        Ok(n)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn appended_bytes_read_back() {
        let mut store = Store::default();
        let a = store.append(b"From a\n").unwrap();
        let b = store.append(b"body\n").unwrap();
        store.flush().unwrap();
        assert_eq!(b, a.after(5));
        assert_eq!(store.read(a).unwrap(), b"From a\n");
        assert_eq!(store.read(b).unwrap(), b"body\n");
        assert_eq!(store.read(Span::default()).unwrap(), b"");
    }

    #[test]
    fn copies_spans_larger_than_the_buffer() {
        let mut store = Store::default();
        let big: Vec<u8> = (0..3 * BUFFER + 17).map(|i| i as u8).collect();
        store.append(b"x").unwrap();
        let span = store.append(&big).unwrap();
        store.flush().unwrap();
        let mut out = Vec::new();
        store.copy_to(span, &mut out).unwrap();
        assert_eq!(out, big);
    }
}
