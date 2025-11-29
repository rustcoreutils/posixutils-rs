//! Input reading from the terminal.

use super::key::Key;
use crate::error::{Result, ViError};
use std::io::{self, Read};

/// Reads key events from standard input.
pub struct InputReader {
    /// Buffer for reading bytes.
    buffer: [u8; 32],
    /// Number of bytes in buffer.
    len: usize,
    /// Current position in buffer.
    pos: usize,
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

        // Handle escape sequences
        if b == 27 {
            // Check for escape sequence
            if self.has_pending_byte()? {
                return self.parse_escape_sequence();
            }
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
        self.len = io::stdin().read(&mut self.buffer).map_err(ViError::Io)?;

        if self.len == 0 {
            return Err(ViError::Interrupted);
        }

        let b = self.buffer[self.pos];
        self.pos += 1;
        Ok(b)
    }

    /// Check if there are pending bytes available (non-blocking check).
    fn has_pending_byte(&self) -> Result<bool> {
        // In practice, if we just read escape, we need to check
        // if there are more bytes waiting. This is a simplified check.
        Ok(self.pos < self.len)
    }

    /// Parse an escape sequence.
    fn parse_escape_sequence(&mut self) -> Result<Key> {
        let b1 = self.read_byte()?;

        match b1 {
            b'[' => self.parse_csi_sequence(),
            b'O' => self.parse_ss3_sequence(),
            _ => {
                // Alt+key or unknown
                Ok(Key::Unknown)
            }
        }
    }

    /// Parse CSI (Control Sequence Introducer) sequences: ESC [
    fn parse_csi_sequence(&mut self) -> Result<Key> {
        let b = self.read_byte()?;

        match b {
            b'A' => Ok(Key::Up),
            b'B' => Ok(Key::Down),
            b'C' => Ok(Key::Right),
            b'D' => Ok(Key::Left),
            b'H' => Ok(Key::Home),
            b'F' => Ok(Key::End),
            b'1'..=b'9' => {
                // Could be function key or other
                let mut num = b - b'0';
                loop {
                    let next = self.read_byte()?;
                    match next {
                        b'0'..=b'9' => {
                            num = num.saturating_mul(10).saturating_add(next - b'0');
                        }
                        b'~' => {
                            return Ok(self.csi_number_to_key(num));
                        }
                        _ => return Ok(Key::Unknown),
                    }
                }
            }
            _ => Ok(Key::Unknown),
        }
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
    fn parse_ss3_sequence(&mut self) -> Result<Key> {
        let b = self.read_byte()?;

        match b {
            b'A' => Ok(Key::Up),
            b'B' => Ok(Key::Down),
            b'C' => Ok(Key::Right),
            b'D' => Ok(Key::Left),
            b'H' => Ok(Key::Home),
            b'F' => Ok(Key::End),
            b'P' => Ok(Key::F(1)),
            b'Q' => Ok(Key::F(2)),
            b'R' => Ok(Key::F(3)),
            b'S' => Ok(Key::F(4)),
            _ => Ok(Key::Unknown),
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

        let s = std::str::from_utf8(&bytes[..len])
            .map_err(|_| ViError::InvalidCommand("Invalid UTF-8 sequence".to_string()))?;

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

    #[test]
    fn test_csi_number_to_key() {
        let reader = InputReader::new();
        assert_eq!(reader.csi_number_to_key(1), Key::Home);
        assert_eq!(reader.csi_number_to_key(3), Key::Delete);
        assert_eq!(reader.csi_number_to_key(5), Key::PageUp);
        assert_eq!(reader.csi_number_to_key(6), Key::PageDown);
    }
}
