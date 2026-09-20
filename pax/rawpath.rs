//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Pathnames as the bytes they are.
//!
//! A Unix pathname is a byte string: any byte but NUL and `/` may appear in a
//! component, and nothing requires those bytes to be text in any encoding. An
//! archive records exactly those bytes, and extracting one has to put exactly
//! those bytes back.
//!
//! `String::from_utf8_lossy` does not do that. It replaces each invalid byte
//! with U+FFFD, so a member GNU tar wrote as `na\377me.txt` came back as
//! `na\357\277\275me.txt` -- a different file, silently -- and two members
//! whose names differ only in invalid bytes collapsed onto one name and
//! clobbered each other.
//!
//! So: bytes in, bytes out. `PathBuf` already holds arbitrary bytes on Unix,
//! so nothing about the in-memory model needed to change; the corruption was
//! entirely at the edges, and this module is those edges.
//!
//! ## The one place bytes are still given up
//!
//! Pattern matching (`pattern.rs`) and `-s` substitution (`subst.rs`) work on
//! `&str`, and converting for them is lossy. That is a real limitation, not an
//! oversight: a member whose name is not UTF-8 may fail to match a pattern that
//! ought to select it. It is confined to *selecting and renaming*, never to the
//! name that reaches the filesystem or a header.
//!
//! [`MatchName`] is that boundary, made explicit. It implements no `Display`,
//! no `AsRef<Path>` and no `Into<PathBuf>`, so a lossy name cannot be printed,
//! stored, or written into a header by accident -- the compiler refuses. The
//! complete list of places this crate gives up bytes is `MatchName::of`,
//! [`from_substituted`], and the `uname`/`gname` fields, which are text by
//! definition.

use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::{Path, PathBuf};

/// The bytes of a pathname.
pub fn as_bytes(path: &Path) -> &[u8] {
    path.as_os_str().as_bytes()
}

/// A pathname from the bytes a header recorded, exactly as recorded.
pub fn from_bytes(bytes: &[u8]) -> PathBuf {
    PathBuf::from(OsString::from_vec(bytes.to_vec()))
}

/// Join a ustar `prefix` field to its `name` field.
///
/// An empty prefix means the name stands alone; the separator is not part of
/// either field.
pub fn join(prefix: &[u8], name: &[u8]) -> PathBuf {
    if prefix.is_empty() {
        return from_bytes(name);
    }
    let mut joined = Vec::with_capacity(prefix.len() + 1 + name.len());
    joined.extend_from_slice(prefix);
    joined.push(b'/');
    joined.extend_from_slice(name);
    from_bytes(&joined)
}

/// A pathname rendered for pattern matching and `-s` substitution, which work
/// on `&str` and so cannot see a name that is not UTF-8 as it really is.
///
/// Deliberately not printable, not storable and not convertible back to a
/// `Path`: this is the lossy form, and the type is what keeps it from leaking
/// into an extracted filename or a header field. To get a name *out* of a
/// substitution, use [`from_substituted`], which says in its own name that the
/// bytes have been through a `&str`.
pub struct MatchName<'a>(Cow<'a, str>);

impl<'a> MatchName<'a> {
    /// The only sanctioned lossy conversion of a pathname in this crate.
    pub fn of(path: &'a Path) -> Self {
        MatchName(path.to_string_lossy())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// A member name produced by `-s` substitution.
///
/// `-s` matches against the lossy form, so a name that is not UTF-8 and that a
/// substitution *changes* comes back laundered -- there is nowhere for the
/// original bytes to survive once a regex has rewritten the text. A
/// substitution that leaves a name unchanged does not reach this function, and
/// that is the case that still round-trips exactly.
pub fn from_substituted(s: &str) -> PathBuf {
    PathBuf::from(OsStr::from_bytes(s.as_bytes()).to_owned())
}

/// The byte offsets at which each *display unit* of `bytes` begins.
///
/// A unit is one valid UTF-8 character, or one byte that cannot begin a valid
/// one. `listopt`'s `%.N` precision and `%N` width count units, so that `%.1F`
/// on `élan.txt` yields `é` rather than half of it, and a column holding a
/// non-ASCII name still lines up. Counting bytes would split a character;
/// counting `char`s is not defined for a name that is not UTF-8 at all.
pub fn unit_starts(bytes: &[u8]) -> Vec<usize> {
    let mut starts = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        starts.push(i);
        i += unit_len(&bytes[i..]);
    }
    starts
}

/// The length in bytes of the display unit beginning at `bytes[0]`.
pub fn unit_len(bytes: &[u8]) -> usize {
    let lead = bytes[0];
    let want = if lead < 0x80 {
        1
    } else if lead >> 5 == 0b110 {
        2
    } else if lead >> 4 == 0b1110 {
        3
    } else if lead >> 3 == 0b11110 {
        4
    } else {
        // A continuation byte or an invalid lead: one unit of one byte.
        return 1;
    };

    // The lead byte only proposes a length. Checking the whole sequence is
    // what rejects an overlong encoding, a surrogate, or a truncated tail --
    // each of which is then one unit per byte, like any other invalid byte.
    if bytes.len() >= want && std::str::from_utf8(&bytes[..want]).is_ok() {
        want
    } else {
        1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_round_trip_keeps_invalid_bytes() {
        let raw = b"na\xffme.txt";
        let path = from_bytes(raw);
        assert_eq!(as_bytes(&path), raw, "the bytes must survive unchanged");
    }

    /// The bug this module exists for: two names differing only in bytes that
    /// are not valid UTF-8 must stay two names.
    #[test]
    fn test_distinct_invalid_names_do_not_collide() {
        let a = from_bytes(b"a\xffb");
        let b = from_bytes(b"a\xfeb");
        assert_ne!(a, b);
        // ...whereas the lossy form of each is the same string, which is
        // exactly why matching is documented as a limitation.
        assert_eq!(MatchName::of(&a).as_str(), MatchName::of(&b).as_str());
    }

    #[test]
    fn test_unit_starts_counts_characters_and_invalid_bytes() {
        // ASCII: one unit per byte.
        assert_eq!(unit_starts(b"abc"), vec![0, 1, 2]);
        // A two-byte character is one unit.
        assert_eq!(unit_starts("\u{e9}lan".as_bytes()), vec![0, 2, 3, 4]);
        // An invalid byte is one unit, and does not swallow what follows.
        assert_eq!(unit_starts(b"a\xffb"), vec![0, 1, 2]);
        // An overlong encoding of '/' is not a character: two invalid bytes.
        assert_eq!(unit_starts(b"\xc0\xaf"), vec![0, 1]);
        // A truncated multi-byte sequence at the end is its bytes.
        assert_eq!(unit_starts(b"a\xe2\x82"), vec![0, 1, 2]);
    }

    #[test]
    fn test_join_prefix_and_name() {
        assert_eq!(as_bytes(&join(b"", b"a.txt")), b"a.txt");
        assert_eq!(as_bytes(&join(b"dir", b"a.txt")), b"dir/a.txt");
        assert_eq!(as_bytes(&join(b"d\xff", b"a\xfe")), b"d\xff/a\xfe");
    }
}
