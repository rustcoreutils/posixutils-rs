//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Standard input, for ex's lines (its commands, its text input and the
//! answers `:s///c` reads) and for visual mode's keys alike.
//!
//! Read with `read(2)` itself, so that a signal reaches the editor: std's
//! buffered line reads retry an interrupted read, and a hangup or interrupt
//! arriving while ex waited for a line went unnoticed until the next line
//! came -- which, after a hangup, it never does.  The one buffer serves both
//! modes, so input typed ahead is neither lost nor reordered when `Q` or
//! `:vi` switches between them.

use std::io;
use std::sync::Mutex;

/// Bytes read but not yet consumed: `buf[start..]`, with no newline in
/// `buf[start..scanned]`.
struct Pending {
    buf: Vec<u8>,
    start: usize,
    scanned: usize,
}

static PENDING: Mutex<Pending> = Mutex::new(Pending {
    buf: Vec::new(),
    start: 0,
    scanned: 0,
});

fn pending() -> std::sync::MutexGuard<'static, Pending> {
    PENDING.lock().unwrap_or_else(|e| e.into_inner())
}

impl Pending {
    /// Take `buf[start..end]`, keeping the buffer from growing without end.
    fn take(&mut self, end: usize) -> Vec<u8> {
        let out = self.buf[self.start..end].to_vec();
        self.start = end;
        self.scanned = self.scanned.max(end);
        if self.start == self.buf.len() {
            self.buf.clear();
            self.start = 0;
            self.scanned = 0;
        } else if self.start > self.buf.len() / 2 {
            self.buf.drain(..self.start);
            self.scanned -= self.start;
            self.start = 0;
        }
        out
    }
}

/// One `read(2)` of standard input into `chunk`, once there is input or
/// after a signal (see [`crate::signals::wait_for_input`]).
fn read_stdin(chunk: &mut [u8]) -> io::Result<usize> {
    crate::signals::wait_for_input()?;
    // SAFETY: `chunk` is a writable buffer of the length given.
    let n = unsafe { libc::read(libc::STDIN_FILENO, chunk.as_mut_ptr().cast(), chunk.len()) };
    usize::try_from(n).map_err(|_| io::Error::last_os_error())
}

/// Append the next line of standard input, newline included, to `out` and
/// return its length: 0 at end of file.  A read a signal interrupts is an
/// `Interrupted` error, and loses nothing: a partial line waits for the next
/// call, unless [`discard_partial`] drops it.
pub fn read_line(out: &mut Vec<u8>) -> io::Result<usize> {
    let mut p = pending();
    loop {
        let from = p.scanned.max(p.start);
        if let Some(i) = p.buf[from..].iter().position(|&b| b == b'\n') {
            let line = p.take(from + i + 1);
            out.extend_from_slice(&line);
            return Ok(line.len());
        }
        p.scanned = p.buf.len();
        let mut chunk = [0u8; 4096];
        match read_stdin(&mut chunk)? {
            0 => {
                let end = p.buf.len();
                let rest = p.take(end);
                out.extend_from_slice(&rest);
                return Ok(rest.len());
            }
            n => p.buf.extend_from_slice(&chunk[..n]),
        }
    }
}

/// Drop a partly entered line, as an interrupt does (94297-94299).
pub fn discard_partial() {
    let mut p = pending();
    let end = p.buf.len();
    p.take(end);
}

/// Some bytes of standard input, for visual mode's keys: what is waiting
/// first, else one `read(2)`.  0 at end of file.
pub fn read_some(chunk: &mut [u8]) -> io::Result<usize> {
    {
        let mut p = pending();
        if p.start < p.buf.len() {
            let end = p.buf.len().min(p.start + chunk.len());
            let bytes = p.take(end);
            chunk[..bytes.len()].copy_from_slice(&bytes);
            return Ok(bytes.len());
        }
    }
    read_stdin(chunk)
}

/// Give back `bytes`, read but not used, to be read again first.
pub fn unread(bytes: &[u8]) {
    if bytes.is_empty() {
        return;
    }
    let mut p = pending();
    let start = p.start;
    p.buf.splice(start..start, bytes.iter().copied());
    p.scanned = start;
}
