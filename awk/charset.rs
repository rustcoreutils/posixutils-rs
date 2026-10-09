//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! How awk turns the bytes it reads into its `String` values, and back.
//!
//! awk keeps every string as a Rust `String`, one `char` per awk character.
//! What a character is depends on `LC_CTYPE`:
//!
//! - In a UTF-8 locale a character is a UTF-8 sequence. Bytes are decoded as
//!   UTF-8 (falling back to one `char` per byte for input that is not valid
//!   UTF-8) and written back out as UTF-8.
//! - In a single-byte locale (the C/POSIX locale among them) a character is a
//!   byte. Every byte `b` becomes the `char` with code point `b`, so a string
//!   holds only code points up to U+00FF, and each such `char` is written back
//!   out as that one byte. This keeps the bytes awk reads and writes
//!   unchanged, makes `length` count bytes, and lets `printf("%c", 200)`
//!   write the single byte 200.
//!
//! The rest of awk sees only `String`s; the conversions below are applied
//! wherever bytes cross into or out of the program: program text, input
//! records, the environment, operands, output, file and command names, and
//! the subject and pattern of a regular expression.

use std::borrow::Cow;
use std::ffi::CString;
use std::sync::atomic::{AtomicBool, Ordering};

/// Whether characters are bytes. Off until [`init`] runs, so code that runs
/// without it (unit tests) keeps the UTF-8 behaviour.
static SINGLE_BYTE: AtomicBool = AtomicBool::new(false);

/// Decide, from the current `LC_CTYPE`, whether characters are bytes. Call
/// once, after the locale is set and before anything is decoded.
pub fn init() {
    // A UTF-8 locale reads "é" (C3 A9) as one character; a single-byte locale
    // reads it as two.
    let single = plib::locale::next_char_offset("é".as_bytes(), 0) == Some(1);
    SINGLE_BYTE.store(single, Ordering::Relaxed);
}

/// Whether the locale is a single-byte one, where an awk character is a byte.
pub fn single_byte() -> bool {
    SINGLE_BYTE.load(Ordering::Relaxed)
}

/// The awk string for the bytes `bytes`.
pub fn decode(bytes: Vec<u8>) -> String {
    if single_byte() {
        return latin1(&bytes);
    }
    match String::from_utf8(bytes) {
        Ok(s) => s,
        Err(e) => latin1(e.as_bytes()),
    }
}

/// The awk string for text that arrived already decoded as UTF-8 (the
/// program text, operands and environment, as the standard library hands
/// them over).
pub fn decode_utf8(text: String) -> String {
    if single_byte() && !text.is_ascii() {
        latin1(text.as_bytes())
    } else {
        text
    }
}

/// One `char` per byte, with the byte's value as its code point.
fn latin1(bytes: &[u8]) -> String {
    bytes.iter().map(|&b| char::from(b)).collect()
}

/// The single byte for `c` in a single-byte locale: its code point, keeping
/// the low eight bits of one above U+00FF (as gawk does for `%c`).
pub fn byte_of(c: char) -> u8 {
    c as u32 as u8
}

/// The bytes awk writes for the string `s`.
pub fn encode(s: &str) -> Cow<'_, [u8]> {
    if single_byte() && !s.is_ascii() {
        Cow::Owned(s.chars().map(byte_of).collect())
    } else {
        Cow::Borrowed(s.as_bytes())
    }
}

/// [`encode`] as a C string, for a name or command handed to libc.
pub fn to_cstring(s: &str) -> Result<CString, String> {
    CString::new(encode(s)).map_err(|_| "string contains a NUL character".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn latin1_round_trips_every_byte() {
        let bytes: Vec<u8> = (0..=255).collect();
        let s = latin1(&bytes);
        assert_eq!(s.chars().count(), 256);
        let back: Vec<u8> = s.chars().map(byte_of).collect();
        assert_eq!(back, bytes);
    }

    #[test]
    fn byte_of_keeps_the_low_eight_bits() {
        assert_eq!(byte_of('\u{c8}'), 0xc8);
        assert_eq!(byte_of('\u{141}'), 0x41);
    }
}
