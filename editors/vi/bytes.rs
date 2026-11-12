//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Bytes in, the same bytes out.
//!
//! vi holds text as `String`, but the files it edits are bytes, and not every
//! file is UTF-8.  Reading them as UTF-8 refused any file with one byte that
//! was not.  Instead, every place bytes enter the editor (files, command
//! output, recovery files, ex commands and arguments) decodes them with
//! [`decode`], and every place they leave encodes with [`encode`]:
//!
//! - valid UTF-8 is kept as the characters it spells;
//! - a byte that is not part of valid UTF-8 becomes the private-use
//!   character `U+10FF00 + byte` (`U+10FF80`..`U+10FFFF`), an *escaped
//!   byte*, and [`encode`] turns it back into that byte;
//! - so that this is lossless, a file that genuinely holds one of those 128
//!   characters has each byte of its UTF-8 form escaped too.
//!
//! `encode(decode(b)) == b` for every `b`.  The only text that does not
//! survive the other way is one of the 128 reserved characters entered some
//! other way (typed), which is written as the single byte it stands for.
//!
//! This is the scheme of Python's `surrogateescape` (PEP 383), using
//! private-use characters because a Rust `String` cannot hold surrogates.

use std::ffi::{OsStr, OsString};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::PathBuf;

/// The first escaped byte's character, less 0x80: byte `b` is `BASE + b`.
const BASE: u32 = 0x10_FF00;

/// The byte `c` stands for, if it is an escaped byte.
pub fn escaped_byte(c: char) -> Option<u8> {
    let v = c as u32;
    (BASE + 0x80..=BASE + 0xFF)
        .contains(&v)
        .then(|| (v - BASE) as u8)
}

/// The character that stands for byte `b` (0x80 or above).
fn escape(b: u8) -> char {
    debug_assert!(b >= 0x80);
    char::from_u32(BASE + u32::from(b)).expect("U+10FF80..U+10FFFF are characters")
}

/// Bytes as editor text: valid UTF-8 as itself, anything else escaped.
pub fn decode(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len());
    let mut rest = bytes;
    while !rest.is_empty() {
        match std::str::from_utf8(rest) {
            Ok(valid) => {
                push_valid(&mut out, valid);
                break;
            }
            Err(e) => {
                let (valid, after) = rest.split_at(e.valid_up_to());
                // `valid_up_to` marks a prefix that is valid UTF-8.
                push_valid(&mut out, std::str::from_utf8(valid).unwrap_or_default());
                let bad = e.error_len().unwrap_or(after.len());
                for &b in &after[..bad] {
                    out.push(escape(b));
                }
                rest = &after[bad..];
            }
        }
    }
    out
}

/// Append valid UTF-8, escaping the bytes of any reserved character so it
/// cannot be taken for an escaped byte.
fn push_valid(out: &mut String, valid: &str) {
    for c in valid.chars() {
        if escaped_byte(c).is_some() {
            let mut buf = [0u8; 4];
            for &b in c.encode_utf8(&mut buf).as_bytes() {
                out.push(escape(b));
            }
        } else {
            out.push(c);
        }
    }
}

/// Editor text as bytes: escaped bytes as the bytes they stand for.
pub fn encode(text: &str) -> Vec<u8> {
    let mut out = Vec::with_capacity(text.len());
    let mut buf = [0u8; 4];
    for c in text.chars() {
        match escaped_byte(c) {
            Some(b) => out.push(b),
            None => out.extend_from_slice(c.encode_utf8(&mut buf).as_bytes()),
        }
    }
    out
}

/// Whether `text` holds no escaped byte, so it is its own encoding.
fn plain(text: &str) -> bool {
    !text.chars().any(|c| escaped_byte(c).is_some())
}

/// Editor text naming a file, as the path it names.
pub fn path(text: &str) -> PathBuf {
    if plain(text) {
        PathBuf::from(text)
    } else {
        PathBuf::from(OsString::from_vec(encode(text)))
    }
}

/// Write editor text and a newline to standard output as the bytes it
/// stands for: an escaped byte goes out as itself, not as the private-use
/// character that holds it.
pub fn println(text: &str) {
    use std::io::Write;
    let mut out = std::io::stdout().lock();
    let _ = out.write_all(&encode(text));
    let _ = out.write_all(b"\n");
}

/// As [`println`], to standard error.
pub fn eprintln(text: &str) {
    use std::io::Write;
    let mut err = std::io::stderr().lock();
    let _ = err.write_all(&encode(text));
    let _ = err.write_all(b"\n");
}

/// A path as editor text: [`path`] gives it back unchanged.
pub fn from_path(p: &std::path::Path) -> String {
    from_os(p.as_os_str())
}

/// An argument or other OS string as editor text.
pub fn from_os(s: &OsStr) -> String {
    decode(s.as_bytes())
}

/// Editor text as an OS string, for a command line.
pub fn to_os(text: &str) -> OsString {
    OsString::from_vec(encode(text))
}

/// A regular expression over editor text, matched against the bytes the
/// text stands for.
///
/// The C library's `regexec` sees text, not characters: handed an escaped
/// byte as the four bytes of its private-use character, `.` matched part of
/// it in a byte locale, and slicing the line at that offset panicked.  So the
/// pattern is compiled from, and every match is made against, the
/// [`encode`]d bytes, and the offsets found are mapped back onto the text.
/// An offset that falls inside a character -- possible in a byte locale,
/// where `.` matches one byte of `é` -- is widened to the whole character.
#[derive(Clone, Debug)]
pub struct TextRegex {
    regex: plib::regex::Regex,
}

/// Editor text, its encoding, and the way between their offsets.
struct Encoded<'t> {
    text: &'t str,
    bytes: std::borrow::Cow<'t, [u8]>,
    /// For each encoded byte, the text offset of the character it is part
    /// of; empty when the encoding is the text itself.
    char_of: Vec<usize>,
}

impl<'t> Encoded<'t> {
    fn new(text: &'t str) -> Self {
        if plain(text) {
            return Encoded {
                text,
                bytes: std::borrow::Cow::Borrowed(text.as_bytes()),
                char_of: Vec::new(),
            };
        }
        let mut bytes = Vec::with_capacity(text.len());
        let mut char_of = Vec::with_capacity(text.len());
        let mut buf = [0u8; 4];
        for (at, c) in text.char_indices() {
            let enc: &[u8] = match escaped_byte(c) {
                Some(b) => {
                    buf[0] = b;
                    &buf[..1]
                }
                None => c.encode_utf8(&mut buf).as_bytes(),
            };
            bytes.extend_from_slice(enc);
            char_of.extend(std::iter::repeat_n(at, enc.len()));
        }
        Encoded {
            text,
            bytes: std::borrow::Cow::Owned(bytes),
            char_of,
        }
    }

    /// The text offset of the character holding encoded byte `b`: a start
    /// offset, rounded down to a character.
    fn floor(&self, b: usize) -> usize {
        if self.char_of.is_empty() {
            let mut at = b.min(self.text.len());
            while !self.text.is_char_boundary(at) {
                at -= 1;
            }
            return at;
        }
        self.char_of.get(b).copied().unwrap_or(self.text.len())
    }

    /// The text offset an end offset `b` maps to, rounded up to a
    /// character.
    fn ceil(&self, b: usize) -> usize {
        if self.char_of.is_empty() {
            let mut at = b.min(self.text.len());
            while !self.text.is_char_boundary(at) {
                at += 1;
            }
            return at;
        }
        if b == 0 {
            return 0;
        }
        match self.char_of.get(b) {
            None => self.text.len(),
            // `b` starts a character when it begins a new one.
            Some(&here) if here != self.char_of[b - 1] => here,
            Some(_) => self.char_of[b..]
                .iter()
                .find(|&&at| at != self.char_of[b])
                .copied()
                .unwrap_or(self.text.len()),
        }
    }

    /// The encoded offset of text offset `at`.
    fn encoded(&self, at: usize) -> usize {
        if self.char_of.is_empty() {
            at
        } else {
            self.char_of.partition_point(|&c| c < at)
        }
    }

    fn to_text(&self, m: plib::regex::Match) -> plib::regex::Match {
        let start = self.floor(m.start);
        plib::regex::Match {
            start,
            end: self.ceil(m.end).max(start),
        }
    }
}

impl TextRegex {
    /// Compile `pattern`, its escaped bytes as the bytes they stand for.
    pub fn new(pattern: &str, flags: plib::regex::RegexFlags) -> std::io::Result<Self> {
        Ok(TextRegex {
            regex: plib::regex::Regex::new_bytes(&encode(pattern), flags)?,
        })
    }

    pub fn is_match(&self, text: &str) -> bool {
        self.regex.is_match_bytes(&Encoded::new(text).bytes)
    }

    pub fn find(&self, text: &str) -> Option<plib::regex::Match> {
        let enc = Encoded::new(text);
        self.regex.find_bytes(&enc.bytes).map(|m| enc.to_text(m))
    }

    /// As [`TextRegex::find`], `text` not starting a line: `^` cannot match
    /// at its start.
    pub fn find_notbol(&self, text: &str) -> Option<plib::regex::Match> {
        let enc = Encoded::new(text);
        self.regex
            .find_notbol_bytes(&enc.bytes)
            .map(|m| enc.to_text(m))
    }

    /// The whole match and every group, for a match at or after text offset
    /// `offset`.
    pub fn captures_at(&self, text: &str, offset: usize) -> Option<Vec<plib::regex::Match>> {
        let enc = Encoded::new(text);
        let caps = self
            .regex
            .captures_at_bytes(&enc.bytes, enc.encoded(offset))?;
        Some(caps.into_iter().map(|m| enc.to_text(m)).collect())
    }

    /// Every non-overlapping match, left to right.
    pub fn find_iter(&self, text: &str) -> Vec<plib::regex::Match> {
        let enc = Encoded::new(text);
        let mut out = Vec::new();
        let mut pos = 0;
        while pos <= enc.bytes.len() {
            let Some(caps) = self.regex.captures_at_bytes(&enc.bytes, pos) else {
                break;
            };
            let m = caps[0];
            out.push(enc.to_text(m));
            pos = if m.end > m.start {
                m.end
            } else {
                match plib::locale::next_char_offset(&enc.bytes, m.end) {
                    Some(next) => next,
                    None => break,
                }
            };
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_roundtrip_every_byte_pattern() {
        let samples: &[&[u8]] = &[
            b"",
            b"plain ascii\n",
            "caf\u{e9} \u{1F600}\n".as_bytes(),
            b"a\xffb\xfe\xed",
            b"\xc3",         // truncated sequence at the end
            b"\xe2\x82",     // truncated in the middle of text
            b"\xed\xa0\x80", // an encoded surrogate: not UTF-8
            "\u{10FF80}\u{10FFFF}".as_bytes(),
        ];
        for &s in samples {
            assert_eq!(encode(&decode(s)), s, "{s:?}");
        }
        let all: Vec<u8> = (0..=255).collect();
        assert_eq!(encode(&decode(&all)), all);
    }

    #[test]
    fn test_valid_utf8_is_kept_as_text() {
        assert_eq!(decode("caf\u{e9}".as_bytes()), "caf\u{e9}");
        assert_eq!(decode(b"a\xffb").chars().count(), 3);
        assert_eq!(
            escaped_byte(decode(b"\xff").chars().next().unwrap()),
            Some(0xff)
        );
    }

    #[test]
    fn test_text_regex_offsets_are_text_offsets() {
        let flags = plib::regex::RegexFlags::bre();
        let text = decode(b"a\xffb");
        let re = TextRegex::new(&decode(b"\xff"), flags).unwrap();
        let m = re.find(&text).unwrap();
        assert_eq!(&text[m.start..m.end], &decode(b"\xff"));
        let dot = TextRegex::new("b", flags).unwrap();
        let m = dot.find(&text).unwrap();
        assert_eq!(&text[m.start..m.end], "b");
        // Every match lands on character boundaries, so slicing is safe.
        let any = TextRegex::new(".", flags).unwrap();
        for m in any.find_iter(&text) {
            let _ = &text[m.start..m.end];
        }
        assert_eq!(any.find_iter(&text).len(), 3);
    }

    #[test]
    fn test_reserved_characters_are_escaped_bytewise() {
        let text = decode("\u{10FF80}".as_bytes());
        assert_eq!(text.chars().count(), 4);
        assert!(text.chars().all(|c| escaped_byte(c).is_some()));
    }
}
