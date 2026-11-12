//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Input reading from the terminal.

use super::key::Key;
use crate::error::{Result, ViError};
use std::io;

/// Reads key events from standard input.
pub struct InputReader {
    /// Buffer for reading bytes.
    buffer: [u8; 32],
    /// Number of bytes in buffer.
    len: usize,
    /// Current position in buffer.
    pos: usize,
}

/// Keys read ahead but not yet taken go back to standard input's buffer,
/// for ex to read when the editor leaves visual mode.
impl Drop for InputReader {
    fn drop(&mut self) {
        super::lines::unread(&self.buffer[self.pos..self.len]);
    }
}

impl InputReader {
    /// Create a new input reader.
    pub fn new() -> Self {
        Self {
            buffer: [0; 32],
            len: 0,
            pos: 0,
        }
    }

    /// Read a single key from input.
    pub fn read_key(&mut self) -> Result<Key> {
        let b = self.read_byte()?;

        // An escape sequence, if what follows the ESC (already here, not
        // still to come) is one.  Otherwise the ESC is a key of its own and
        // the bytes after it are keys too: ESC then `j` typed or pasted fast
        // is ESC and `j`, not an unknown Alt-j that loses both.
        // A terminal writes a sequence whole, so only bytes already read
        // count: a sequence's parse never waits for more input.
        if b == 27 {
            let at = self.pos;
            if let Some(key) = self.parse_escape_sequence() {
                return Ok(key);
            }
            self.pos = at;
            return Ok(Key::Escape);
        }

        // Handle multi-byte UTF-8
        if b & 0x80 != 0 {
            return self.parse_utf8(b);
        }

        Ok(Key::from_byte(b))
    }

    /// Read a single byte from input.
    fn read_byte(&mut self) -> Result<u8> {
        if self.pos < self.len {
            let b = self.buffer[self.pos];
            self.pos += 1;
            return Ok(b);
        }

        // Read more bytes
        self.pos = 0;
        self.len = match super::lines::read_some(&mut self.buffer) {
            Ok(n) => n,
            // A signal (SIGWINCH/SIGCONT/SIGINT) interrupted the read; surface
            // it so the caller can service the pending signal and redraw.
            Err(e) if e.kind() == io::ErrorKind::Interrupted => {
                return Err(ViError::Interrupted);
            }
            Err(e) => return Err(ViError::Io(e)),
        };

        if self.len == 0 {
            // End of input: not an interrupt, which only asks for a redraw.
            return Err(ViError::Io(io::ErrorKind::UnexpectedEof.into()));
        }

        let b = self.buffer[self.pos];
        self.pos += 1;
        Ok(b)
    }

    /// The next byte already read, if there is one; never reads more.
    fn buffered_byte(&mut self) -> Option<u8> {
        let b = *self.buffer[..self.len].get(self.pos)?;
        self.pos += 1;
        Some(b)
    }

    /// Parse an escape sequence: `None` if the bytes after the ESC are not
    /// one (`ESC [` or `ESC O` with a valid tail).
    fn parse_escape_sequence(&mut self) -> Option<Key> {
        match self.buffered_byte()? {
            b'[' => self.parse_csi_sequence(),
            b'O' => self.parse_ss3_sequence(),
            _ => None,
        }
    }

    /// Parse CSI (Control Sequence Introducer) sequences: ESC [
    ///
    /// Consumes the whole sequence per ECMA-48: any number of parameter bytes
    /// (0x30-0x3F) and intermediate bytes (0x20-0x2F), then exactly one final
    /// byte (0x40-0x7E).
    ///
    /// Stopping at the first byte that was not a digit or `~` left the rest of
    /// the sequence in the input stream, where it was read as ordinary
    /// commands: a Ctrl-Right arrow (`ESC [ 1 ; 5 C`) delivered `5C`, which
    /// changes to the end of the line -- a destructive edit from a cursor key.
    fn parse_csi_sequence(&mut self) -> Option<Key> {
        let mut params: Vec<u8> = Vec::new();
        let mut intermediate = false;
        let final_byte = loop {
            let b = self.buffered_byte()?;
            match b {
                0x30..=0x3F => params.push(b),
                0x20..=0x2F => intermediate = true, // consumed, unused
                0x40..=0x7E => break b,
                // Not a CSI sequence at all.
                _ => return None,
            }
        };
        // A bare `ESC [` and a final byte no key ends -- `ESC [ [`, `ESC [ x`
        // -- is ESC, then `[`, then keys typed fast after it.
        if params.is_empty() && !intermediate && !b"ABCDHF".contains(&final_byte) {
            return None;
        }

        // The leading numeric parameter, if there is one.
        let num = params
            .iter()
            .take_while(|b| b.is_ascii_digit())
            .fold(0u8, |acc, b| {
                acc.saturating_mul(10).saturating_add(b - b'0')
            });

        // A whole CSI sequence: a key, or one this editor does not know.
        Some(match final_byte {
            b'A' => Key::Up,
            b'B' => Key::Down,
            b'C' => Key::Right,
            b'D' => Key::Left,
            b'H' => Key::Home,
            b'F' => Key::End,
            b'~' => self.csi_number_to_key(num),
            _ => Key::Unknown,
        })
    }

    /// Convert CSI number to key.
    fn csi_number_to_key(&self, num: u8) -> Key {
        match num {
            1 => Key::Home,
            2 => Key::Insert,
            3 => Key::Delete,
            4 => Key::End,
            5 => Key::PageUp,
            6 => Key::PageDown,
            11..=15 => Key::F(num - 10), // F1-F5
            17..=21 => Key::F(num - 11), // F6-F10
            23..=24 => Key::F(num - 12), // F11-F12
            _ => Key::Unknown,
        }
    }

    /// Parse SS3 sequences: ESC O
    ///
    /// Only the SS3 keys a terminal sends: anything else is ESC, then `O`
    /// (the open-above command), then more keys.
    fn parse_ss3_sequence(&mut self) -> Option<Key> {
        match self.buffered_byte()? {
            b'A' => Some(Key::Up),
            b'B' => Some(Key::Down),
            b'C' => Some(Key::Right),
            b'D' => Some(Key::Left),
            b'H' => Some(Key::Home),
            b'F' => Some(Key::End),
            b'P' => Some(Key::F(1)),
            b'Q' => Some(Key::F(2)),
            b'R' => Some(Key::F(3)),
            b'S' => Some(Key::F(4)),
            _ => None,
        }
    }

    /// Parse multi-byte UTF-8 sequence.
    fn parse_utf8(&mut self, first: u8) -> Result<Key> {
        let mut bytes = [first, 0, 0, 0];
        let len = match first {
            0xC0..=0xDF => 2,
            0xE0..=0xEF => 3,
            0xF0..=0xF7 => 4,
            _ => return Ok(Key::Unknown),
        };

        for byte in bytes.iter_mut().take(len).skip(1) {
            *byte = self.read_byte()?;
        }

        // An undecodable byte is bad input, not a reason to end the session:
        // propagating the error unwound out of the visual loop and exited vi
        // *without* preserving the buffer.
        let Ok(s) = std::str::from_utf8(&bytes[..len]) else {
            return Ok(Key::Unknown);
        };

        if let Some(c) = s.chars().next() {
            Ok(Key::Char(c))
        } else {
            Ok(Key::Unknown)
        }
    }
}

impl Default for InputReader {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A reader with `bytes` already read, so no test touches stdin.
    fn reading(bytes: &[u8]) -> InputReader {
        let mut r = InputReader::new();
        r.buffer[..bytes.len()].copy_from_slice(bytes);
        r.len = bytes.len();
        r
    }

    fn keys(bytes: &[u8]) -> Vec<Key> {
        let mut r = reading(bytes);
        let mut out = Vec::new();
        while r.pos < r.len {
            out.push(r.read_key().unwrap());
        }
        out
    }

    #[test]
    fn test_escape_then_keys_typed_fast() {
        assert_eq!(keys(b"\x1bj"), [Key::Escape, Key::Char('j')]);
        assert_eq!(
            keys(b"x\x1b:wq"),
            [
                Key::Char('x'),
                Key::Escape,
                Key::Char(':'),
                Key::Char('w'),
                Key::Char('q')
            ]
        );
        // ESC O then a letter that ends no SS3 sequence: ESC, `O`, the letter.
        assert_eq!(
            keys(b"\x1bOh"),
            [Key::Escape, Key::Char('O'), Key::Char('h')]
        );
        // `[` then a byte no CSI sequence holds.
        assert_eq!(
            keys(b"\x1b[\x01"),
            [Key::Escape, Key::Char('['), Key::Ctrl('a')]
        );
    }

    #[test]
    fn test_escape_then_bracket_commands() {
        // `[[` typed fast after ESC is ESC and the motion.
        assert_eq!(
            keys(b"\x1b[["),
            [Key::Escape, Key::Char('['), Key::Char('[')]
        );
        assert_eq!(
            keys(b"\x1b[x"),
            [Key::Escape, Key::Char('['), Key::Char('x')]
        );
        // ESC [ or ESC O last in what was read: ESC, then the letter, now --
        // not a wait for a sequence that may never come.
        assert_eq!(keys(b"\x1bO"), [Key::Escape, Key::Char('O')]);
        assert_eq!(keys(b"\x1b["), [Key::Escape, Key::Char('[')]);
    }

    #[test]
    fn test_escape_sequences_are_keys() {
        assert_eq!(
            keys(b"\x1b[A\x1bOB\x1b[5~"),
            [Key::Up, Key::Down, Key::PageUp]
        );
    }

    #[test]
    fn test_csi_number_to_key() {
        let reader = InputReader::new();
        assert_eq!(reader.csi_number_to_key(1), Key::Home);
        assert_eq!(reader.csi_number_to_key(3), Key::Delete);
        assert_eq!(reader.csi_number_to_key(5), Key::PageUp);
        assert_eq!(reader.csi_number_to_key(6), Key::PageDown);
    }
}
