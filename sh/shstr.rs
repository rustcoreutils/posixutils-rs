//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The string type the shell operates on: a byte string.
//!
//! POSIX XCU 2.6.5 is explicit that "the shell processes arbitrary bytes from
//! the input fields; there is no requirement that those bytes form valid
//! characters", and the same is true of arguments, environment entries, file
//! names and script text. `String` cannot represent any of that, so it is the
//! wrong type for a shell value.
//!
//! [`ShStr`] and [`ShString`] are `Path`/`PathBuf` in shape: a borrowed and an
//! owned view of the same bytes, with `Deref` down to `[u8]` so that the whole
//! slice API is available without re-exporting it.
//!
//! There is deliberately **no** `Display`. A blanket lossy `Display` would be a
//! trap: `format!("{name}={value}")` builds an *environment entry*, not a
//! message, and a lossy conversion there would corrupt it while compiling
//! cleanly. Callers ask for [`ShStr::display`] instead, which makes
//! `grep -rn '\.display()'` the exact list of places that intentionally lose
//! bytes.

use std::borrow::Borrow;
use std::ffi::{CString, NulError, OsStr, OsString};
use std::fmt;
use std::ops::Deref;
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::Path;

/// A borrowed shell string: arbitrary bytes.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct ShStr([u8]);

/// An owned shell string: arbitrary bytes.
#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct ShString(Vec<u8>);

impl ShStr {
    pub fn new<B: AsRef<[u8]> + ?Sized>(bytes: &B) -> &ShStr {
        // Safe: ShStr is a transparent wrapper around [u8].
        unsafe { &*(bytes.as_ref() as *const [u8] as *const ShStr) }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }

    /// The bytes as text, when they happen to be valid UTF-8. Used where the
    /// value must be text to mean anything at all — a number to parse, a
    /// variable name, an option letter.
    pub fn to_str(&self) -> Option<&str> {
        std::str::from_utf8(&self.0).ok()
    }

    /// A lossy view for diagnostics. Every call is a deliberate loss of bytes;
    /// never use it to build a value.
    pub fn display(&self) -> impl fmt::Display + '_ {
        String::from_utf8_lossy(&self.0)
    }

    /// Iterates the characters without losing anything: a byte that is not
    /// part of a valid character is yielded as itself. Unlike
    /// `String::from_utf8_lossy().chars()`, nothing collapses to U+FFFD, so a
    /// value can be taken apart and put back together unchanged.
    pub fn chars_lossless(&self) -> CharsLossless<'_> {
        CharsLossless {
            bytes: &self.0,
            offset: 0,
        }
    }

    pub fn to_sh_string(&self) -> ShString {
        ShString(self.0.to_vec())
    }

    pub fn as_os_str(&self) -> &OsStr {
        OsStr::from_bytes(&self.0)
    }

    /// Fails on an interior NUL rather than panicking: a shell value may
    /// contain one, but it cannot be passed to `execve`.
    pub fn to_c_string(&self) -> Result<CString, NulError> {
        CString::new(self.0.to_vec())
    }
}

impl ShString {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn as_sh_str(&self) -> &ShStr {
        ShStr::new(&self.0)
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.0
    }

    pub fn push_bytes<B: AsRef<[u8]>>(&mut self, bytes: B) {
        self.0.extend_from_slice(bytes.as_ref());
    }

    pub fn push_char(&mut self, c: char) {
        let mut buf = [0u8; 4];
        self.push_bytes(c.encode_utf8(&mut buf).as_bytes());
    }

    pub fn into_os_string(self) -> OsString {
        OsString::from_vec(self.0)
    }
}

impl Deref for ShStr {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        &self.0
    }
}

impl Deref for ShString {
    type Target = ShStr;
    fn deref(&self) -> &ShStr {
        self.as_sh_str()
    }
}

impl Borrow<ShStr> for ShString {
    fn borrow(&self) -> &ShStr {
        self.as_sh_str()
    }
}

impl AsRef<ShStr> for ShString {
    fn as_ref(&self) -> &ShStr {
        self.as_sh_str()
    }
}

impl AsRef<ShStr> for ShStr {
    fn as_ref(&self) -> &ShStr {
        self
    }
}

impl AsRef<[u8]> for ShStr {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl AsRef<[u8]> for ShString {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl AsRef<OsStr> for ShStr {
    fn as_ref(&self) -> &OsStr {
        self.as_os_str()
    }
}

impl AsRef<OsStr> for ShString {
    fn as_ref(&self) -> &OsStr {
        self.as_os_str()
    }
}

impl AsRef<Path> for ShStr {
    fn as_ref(&self) -> &Path {
        Path::new(self.as_os_str())
    }
}

impl AsRef<Path> for ShString {
    fn as_ref(&self) -> &Path {
        Path::new(self.as_os_str())
    }
}

impl From<&String> for ShString {
    fn from(value: &String) -> Self {
        Self(value.as_bytes().to_vec())
    }
}

impl From<&str> for ShString {
    fn from(value: &str) -> Self {
        Self(value.as_bytes().to_vec())
    }
}

impl From<String> for ShString {
    fn from(value: String) -> Self {
        Self(value.into_bytes())
    }
}

impl From<&[u8]> for ShString {
    fn from(value: &[u8]) -> Self {
        Self(value.to_vec())
    }
}

impl From<Vec<u8>> for ShString {
    fn from(value: Vec<u8>) -> Self {
        Self(value)
    }
}

impl From<&ShString> for ShString {
    fn from(value: &ShString) -> Self {
        value.clone()
    }
}

impl From<&ShStr> for ShString {
    fn from(value: &ShStr) -> Self {
        value.to_sh_string()
    }
}

impl From<OsString> for ShString {
    fn from(value: OsString) -> Self {
        Self(value.into_vec())
    }
}

impl From<&OsStr> for ShString {
    fn from(value: &OsStr) -> Self {
        Self(value.as_bytes().to_vec())
    }
}

impl PartialEq<str> for ShStr {
    fn eq(&self, other: &str) -> bool {
        self.0 == *other.as_bytes()
    }
}

impl PartialEq<&str> for ShStr {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other.as_bytes()
    }
}

impl PartialEq<str> for ShString {
    fn eq(&self, other: &str) -> bool {
        self.0 == *other.as_bytes()
    }
}

impl PartialEq<&str> for ShString {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other.as_bytes()
    }
}

/// Escaped-ASCII, so a failing `assert_eq!` stays readable when the value is
/// not text.
impl fmt::Debug for ShStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b\"")?;
        for &byte in &self.0 {
            match byte {
                b'"' => write!(f, "\\\"")?,
                b'\\' => write!(f, "\\\\")?,
                0x20..=0x7e => write!(f, "{}", byte as char)?,
                b'\n' => write!(f, "\\n")?,
                b'\t' => write!(f, "\\t")?,
                _ => write!(f, "\\x{byte:02x}")?,
            }
        }
        write!(f, "\"")
    }
}

impl fmt::Debug for ShString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_sh_str(), f)
    }
}

/// One decoded character, or one byte that is not part of one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CharOrByte {
    Char(char),
    Byte(u8),
}

#[derive(Clone)]
pub struct CharsLossless<'a> {
    bytes: &'a [u8],
    offset: usize,
}

impl Iterator for CharsLossless<'_> {
    type Item = CharOrByte;

    fn next(&mut self) -> Option<CharOrByte> {
        if self.offset >= self.bytes.len() {
            return None;
        }
        let rest = &self.bytes[self.offset..];
        match std::str::from_utf8(rest) {
            Ok(valid) => {
                let c = valid.chars().next()?;
                self.offset += c.len_utf8();
                Some(CharOrByte::Char(c))
            }
            Err(err) if err.valid_up_to() > 0 => {
                // Safe: `valid_up_to` is a character boundary.
                let valid = std::str::from_utf8(&rest[..err.valid_up_to()]).unwrap();
                let c = valid.chars().next()?;
                self.offset += c.len_utf8();
                Some(CharOrByte::Char(c))
            }
            Err(_) => {
                let byte = rest[0];
                self.offset += 1;
                Some(CharOrByte::Byte(byte))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn holds_bytes_that_are_not_text() {
        let s = ShString::from(vec![b'a', 0xff, b'b']);
        assert_eq!(s.as_bytes(), &[b'a', 0xff, b'b']);
        assert_eq!(s.to_str(), None);
        assert_eq!(s.display().to_string(), "a\u{fffd}b");
    }

    #[test]
    fn compares_against_str_literals() {
        let s = ShString::from("abc");
        assert_eq!(s, "abc");
        assert_eq!(*s.as_sh_str(), *"abc");
        assert_eq!(s.to_str(), Some("abc"));
    }

    #[test]
    fn derefs_to_the_slice_api() {
        let s = ShString::from("hello");
        assert_eq!(s.len(), 5);
        assert!(s.starts_with(b"he"));
        assert!(s.ends_with(b"lo"));
        assert!(!s.is_empty());
    }

    #[test]
    fn round_trips_through_os_string() {
        let s = ShString::from(vec![b'x', 0xfe]);
        let os = s.clone().into_os_string();
        assert_eq!(ShString::from(os), s);
    }

    #[test]
    fn an_interior_nul_is_an_error_not_a_panic() {
        assert!(ShString::from(vec![b'a', 0, b'b']).to_c_string().is_err());
        assert!(ShString::from("ab").to_c_string().is_ok());
    }

    #[test]
    fn chars_lossless_keeps_every_byte() {
        let s = ShString::from(vec![b'a', 0xff, 0xc3, 0xa9, b'b']);
        let parts: Vec<_> = s.chars_lossless().collect();
        assert_eq!(
            parts,
            vec![
                CharOrByte::Char('a'),
                CharOrByte::Byte(0xff),
                CharOrByte::Char('é'),
                CharOrByte::Char('b'),
            ]
        );
        // Nothing is lost: putting it back together reproduces the value.
        let mut rebuilt = ShString::new();
        for part in s.chars_lossless() {
            match part {
                CharOrByte::Char(c) => rebuilt.push_char(c),
                CharOrByte::Byte(b) => rebuilt.push_bytes([b]),
            }
        }
        assert_eq!(rebuilt, s);
    }

    #[test]
    fn debug_is_readable_for_non_text() {
        assert_eq!(
            format!("{:?}", ShString::from(vec![b'a', 0xff])),
            r#"b"a\xff""#
        );
    }
}
