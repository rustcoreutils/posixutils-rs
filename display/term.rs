//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The terminal as `more` drives it: the ANSI sequences it writes, the
//! window size, and the decoding of what the user types into command text.

use std::fmt;
use std::io::{self, Read, Write};

/// Show the cursor.
pub const SHOW_CURSOR: &str = "\x1b[?25h";
/// Hide the cursor.
pub const HIDE_CURSOR: &str = "\x1b[?25l";
/// Reset every character attribute.
pub const RESET_STYLE: &str = "\x1b[m";
/// Underline what follows.
pub const UNDERLINE: &str = "\x1b[4m";
/// Show what follows in reverse video.
pub const INVERT: &str = "\x1b[7m";
/// Erase the line the cursor is on.
pub const CLEAR_LINE: &str = "\x1b[2K";

/// Switch to the alternate screen buffer.
const TO_ALTERNATE_SCREEN: &str = "\x1b[?1049h";
/// Switch back to the main screen buffer.
const TO_MAIN_SCREEN: &str = "\x1b[?1049l";

/// Move the cursor to a one-based (column, row).
pub struct Goto(pub u16, pub u16);

impl fmt::Display for Goto {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[{};{}H", self.1, self.0)
    }
}

/// A writer drawing on the alternate screen, which puts the user's own
/// screen back when it is dropped.
pub struct AlternateScreen<W: Write>(W);

impl<W: Write> AlternateScreen<W> {
    /// Switch `out`'s terminal to the alternate screen.
    pub fn new(mut out: W) -> io::Result<Self> {
        out.write_all(TO_ALTERNATE_SCREEN.as_bytes())?;
        Ok(Self(out))
    }
}

impl<W: Write> Drop for AlternateScreen<W> {
    fn drop(&mut self) {
        let _ = self.0.write_all(TO_MAIN_SCREEN.as_bytes());
        let _ = self.0.flush();
    }
}

/// The terminal's size as (columns, rows), asked of standard output.
#[cfg(unix)]
pub fn terminal_size() -> io::Result<(u16, u16)> {
    // SAFETY: TIOCGWINSZ fills the winsize it is handed and nothing else.
    let mut size: libc::winsize = unsafe { std::mem::zeroed() };
    if unsafe { libc::ioctl(libc::STDOUT_FILENO, libc::TIOCGWINSZ, &mut size) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok((size.ws_col, size.ws_row))
}

/// One complete unit of terminal input.
#[derive(Debug, PartialEq, Eq)]
enum Unit {
    /// The command text for the first `len` bytes, or `None` when they mean
    /// nothing to `more`.
    Complete { len: usize, command: Option<String> },
    /// The bytes so far are the start of a unit that has not all arrived.
    Partial,
}

const ESC: u8 = 0x1b;

/// Split the first unit of input off `bytes` (which is not empty).
///
/// Ordinary characters, control characters included, are command text as
/// typed; carriage return is newline. An escape sequence is a key the
/// terminal encoded: the up and down arrows are `k` and newline, Alt with a
/// character is that ESC-prefixed text, and every other key (function,
/// navigation, mouse report) is dropped rather than handed on as stray
/// characters. An ESC with nothing after it in the same read is the Escape
/// key itself.
fn split_unit(bytes: &[u8]) -> Unit {
    match bytes[0] {
        ESC => split_escape(bytes),
        b'\r' | b'\n' => Unit::Complete {
            len: 1,
            command: Some("\n".to_string()),
        },
        _ => match split_char(bytes) {
            Some((len, ch)) => Unit::Complete {
                len,
                command: ch.map(String::from),
            },
            None => Unit::Partial,
        },
    }
}

/// The first UTF-8 character of `bytes`: its length and the character, or
/// `None` for the character when the bytes are not UTF-8 (one byte is then
/// skipped). `None` overall when the character is cut short.
fn split_char(bytes: &[u8]) -> Option<(usize, Option<char>)> {
    let len = match bytes[0] {
        0x00..=0x7f => 1,
        0xc0..=0xdf => 2,
        0xe0..=0xef => 3,
        0xf0..=0xf7 => 4,
        _ => return Some((1, None)),
    };
    if bytes.len() < len {
        // A byte that cannot continue the character ends it now.
        if bytes[1..].iter().any(|b| b & 0xc0 != 0x80) {
            return Some((1, None));
        }
        return None;
    }
    match std::str::from_utf8(&bytes[..len]) {
        Ok(s) => Some((len, s.chars().next())),
        Err(_) => Some((1, None)),
    }
}

/// Split an escape sequence off `bytes`, which starts with ESC.
fn split_escape(bytes: &[u8]) -> Unit {
    let Some(&second) = bytes.get(1) else {
        return Unit::Complete {
            len: 1,
            command: Some("\x1b".to_string()),
        };
    };
    match second {
        b'[' => split_csi(bytes),
        // SS3: ESC O and one final byte (keypad and F1-F4 keys).
        b'O' if bytes.len() < 3 => Unit::Partial,
        b'O' => Unit::Complete {
            len: 3,
            command: None,
        },
        _ => match split_char(&bytes[1..]) {
            Some((len, Some(ch))) => Unit::Complete {
                len: 1 + len,
                command: Some(format!("\x1b{ch}")),
            },
            Some((len, None)) => Unit::Complete {
                len: 1 + len,
                command: None,
            },
            None => Unit::Partial,
        },
    }
}

/// Split a control sequence (ESC `[` ...) off `bytes`.
fn split_csi(bytes: &[u8]) -> Unit {
    // An X10 mouse report is ESC [ M and three raw bytes; the Linux console's
    // F1-F5 are ESC [ [ and a letter.
    let extra = match bytes.get(2) {
        Some(b'M') => 3,
        Some(b'[') => 1,
        _ => 0,
    };
    // Parameter and intermediate bytes, then one final byte.
    let Some(final_at) = bytes[2..]
        .iter()
        .position(|b| (0x40..=0x7e).contains(b))
        .map(|i| i + 2)
    else {
        return Unit::Partial;
    };
    let len = final_at + 1 + extra;
    if bytes.len() < len {
        return Unit::Partial;
    }
    let command = match &bytes[..len] {
        b"\x1b[A" => Some("k".to_string()),
        b"\x1b[B" => Some("\n".to_string()),
        _ => None,
    };
    Unit::Complete { len, command }
}

/// The commands typed on `source`, one key at a time.
pub struct Commands<R> {
    source: R,
    pending: Vec<u8>,
}

impl<R: Read> Commands<R> {
    pub fn new(source: R) -> Self {
        Self {
            source,
            pending: Vec::new(),
        }
    }

    /// Append one read's worth of input; `false` at end of input.
    fn fill(&mut self) -> io::Result<bool> {
        let mut buf = [0u8; 64];
        let n = loop {
            match self.source.read(&mut buf) {
                Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
                other => break other?,
            }
        };
        self.pending.extend_from_slice(&buf[..n]);
        Ok(n > 0)
    }
}

impl<R: Read> Iterator for Commands<R> {
    type Item = io::Result<String>;

    fn next(&mut self) -> Option<io::Result<String>> {
        loop {
            if self.pending.is_empty() {
                match self.fill() {
                    Ok(true) => {}
                    Ok(false) => return None,
                    Err(e) => return Some(Err(e)),
                }
            }
            match split_unit(&self.pending) {
                Unit::Complete { len, command } => {
                    self.pending.drain(..len);
                    if let Some(command) = command {
                        return Some(Ok(command));
                    }
                }
                Unit::Partial => match self.fill() {
                    Ok(true) => {}
                    // What was cut off by the end of input means nothing.
                    Ok(false) => return None,
                    Err(e) => return Some(Err(e)),
                },
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The commands decoded from `chunks`, each arriving as one read.
    fn decode(chunks: &[&[u8]]) -> Vec<String> {
        struct Reads<'a>(std::slice::Iter<'a, &'a [u8]>);
        impl Read for Reads<'_> {
            fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
                let Some(chunk) = self.0.next() else {
                    return Ok(0);
                };
                buf[..chunk.len()].copy_from_slice(chunk);
                Ok(chunk.len())
            }
        }
        Commands::new(Reads(chunks.iter()))
            .map(Result::unwrap)
            .collect()
    }

    #[test]
    fn characters_are_command_text() {
        assert_eq!(decode(&[b"10j"]), ["1", "0", "j"]);
        assert_eq!(decode(&[b"\x06\x7f\t"]), ["\x06", "\x7f", "\t"]);
        assert_eq!(decode(&["é/€".as_bytes()]), ["é", "/", "€"]);
    }

    #[test]
    fn return_is_newline() {
        assert_eq!(decode(&[b"\r\n"]), ["\n", "\n"]);
    }

    #[test]
    fn arrows_scroll() {
        assert_eq!(decode(&[b"\x1b[A\x1b[B"]), ["k", "\n"]);
    }

    #[test]
    fn other_keys_are_dropped() {
        // Left, Page Up, Shift-Up, F1 (SS3), F1 (Linux console), an SGR and
        // an X10 mouse report: none of them may leak characters such as the
        // `5` of Page Up into the command line as a count.
        let keys: &[u8] = b"\x1b[D\x1b[5~\x1b[1;2A\x1bOP\x1b[[A\x1b[<64;3;4M\x1b[M`!!q";
        assert_eq!(decode(&[keys]), ["q"]);
    }

    #[test]
    fn alt_and_escape() {
        assert_eq!(decode(&[b"\x1bf"]), ["\x1bf"]);
        assert_eq!(decode(&[b"\x1b", b"q"]), ["\x1b", "q"]);
    }

    #[test]
    fn a_sequence_split_across_reads_is_reassembled() {
        assert_eq!(decode(&[b"\x1b[", b"A"]), ["k"]);
        assert_eq!(decode(&[b"\xe2\x82", b"\xac"]), ["€"]);
    }

    #[test]
    fn bytes_that_are_not_utf8_are_skipped() {
        assert_eq!(decode(&[b"\xffa\xc3("]), ["a", "("]);
    }

    #[test]
    fn input_ending_mid_sequence_ends_the_commands() {
        assert_eq!(decode(&[b"q\x1b[1;"]), ["q"]);
    }

    #[test]
    fn goto_is_row_then_column() {
        assert_eq!(Goto(3, 7).to_string(), "\x1b[7;3H");
    }
}
