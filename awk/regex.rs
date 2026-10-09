//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::charset;
use plib::regex::{Match, Regex as PlibRegex, RegexFlags};
use std::borrow::Cow;
use std::ffi::CString;

/// A regex wrapper that provides CString-compatible API for AWK.
/// Internally uses plib::regex for POSIX ERE support.
///
/// Patterns and subjects reach `regexec` as the bytes awk would write for
/// them (see [`crate::charset`]), so in a single-byte locale a character is
/// a byte to the regular expression as well.
pub struct Regex {
    inner: PlibRegex,
    pattern_string: String,
}

#[cfg_attr(test, derive(Debug))]
#[derive(Copy, Clone, Default, PartialEq, Eq)]
pub struct RegexMatch {
    pub start: usize,
    pub end: usize,
}

impl From<Match> for RegexMatch {
    fn from(m: Match) -> Self {
        RegexMatch {
            start: m.start,
            end: m.end,
        }
    }
}

/// Iterator over regex matches in a string, giving offsets into that string.
pub struct MatchIter<'re> {
    /// The subject as `regexec` sees it.
    bytes: Vec<u8>,
    /// For a subject whose bytes are not its own UTF-8 (a non-ASCII string in
    /// a single-byte locale, where each character is one byte): the string
    /// offset of each byte, and of the end.
    string_offsets: Option<Vec<usize>>,
    next_start: usize,
    regex: &'re Regex,
}

impl MatchIter<'_> {
    fn string_offset(&self, pos: usize) -> usize {
        match &self.string_offsets {
            Some(offsets) => offsets[pos],
            None => pos,
        }
    }

    /// The offset just past the character that starts at `pos` in `bytes`.
    fn next_char(&self, pos: usize) -> usize {
        let mut next = pos + 1;
        if !charset::single_byte() {
            // UTF-8: skip continuation bytes.
            while next < self.bytes.len() && (self.bytes[next] & 0xc0) == 0x80 {
                next += 1;
            }
        }
        next
    }
}

impl Iterator for MatchIter<'_> {
    type Item = RegexMatch;
    fn next(&mut self) -> Option<Self::Item> {
        if self.next_start > self.bytes.len() {
            return None;
        }

        // Find match starting from current offset
        let subject = &self.bytes[self.next_start..];
        let m = if self.next_start == 0 {
            self.regex.inner.find_bytes(subject)?
        } else {
            self.regex.inner.find_notbol_bytes(subject)?
        };

        let start = self.next_start + m.start;
        let end = self.next_start + m.end;
        let result = RegexMatch {
            start: self.string_offset(start),
            end: self.string_offset(end),
        };

        // Resume after the match; after an empty match, one character past
        // it, or the next search would find the same empty match again.
        self.next_start = if m.start == m.end {
            self.next_char(end)
        } else {
            end
        };

        Some(result)
    }
}

impl Regex {
    /// Compile the pattern whose bytes are `regex`.
    pub fn new(regex: CString) -> Result<Self, String> {
        let bytes = regex.into_bytes();
        let inner = PlibRegex::new_bytes(&bytes, RegexFlags::ere()).map_err(|e| e.to_string())?;
        Ok(Self {
            inner,
            pattern_string: charset::decode(bytes),
        })
    }

    /// Returns the first match location in the raw input bytes `bytes`, as
    /// byte offsets.
    pub fn find_bytes(&self, bytes: &[u8]) -> Option<RegexMatch> {
        self.inner.find_bytes(bytes).map(RegexMatch::from)
    }

    /// Returns an iterator over all match locations in `string`.
    pub fn match_locations(&self, string: &str) -> MatchIter<'_> {
        let bytes = charset::encode(string);
        let string_offsets = match bytes {
            Cow::Borrowed(_) => None,
            Cow::Owned(_) => Some(
                string
                    .char_indices()
                    .map(|(i, _)| i)
                    .chain([string.len()])
                    .collect(),
            ),
        };
        MatchIter {
            bytes: bytes.into_owned(),
            string_offsets,
            next_start: 0,
            regex: self,
        }
    }

    pub fn pattern(&self) -> &str {
        &self.pattern_string
    }

    /// Whether the regular expression matches the bytes of `string`.
    pub fn matches(&self, string: &CString) -> bool {
        self.inner.is_match_bytes(string.as_bytes())
    }
}

impl Drop for Regex {
    fn drop(&mut self) {
        // plib::regex handles cleanup internally
    }
}

#[cfg(test)]
impl core::fmt::Debug for Regex {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        writeln!(f, "/{}/", self.pattern_string)
    }
}

impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        self.pattern_string == other.pattern_string
    }
}

/// utility function for writing tests
#[cfg(test)]
pub fn regex_from_str(re: &str) -> Regex {
    Regex::new(CString::new(re).unwrap()).expect("error compiling ere")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_create_regex() {
        regex_from_str("test");
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_regex_matches() {
        let ere = regex_from_str("ab*c");
        assert!(ere.matches(&CString::new("abbbbc").unwrap()));
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_regex_match_locations() {
        let ere = regex_from_str("match");
        let mut iter = ere.match_locations("match 12345 match2 matchmatch");
        assert_eq!(iter.next(), Some(RegexMatch { start: 0, end: 5 }));
        assert_eq!(iter.next(), Some(RegexMatch { start: 12, end: 17 }));
        assert_eq!(iter.next(), Some(RegexMatch { start: 19, end: 24 }));
        assert_eq!(iter.next(), Some(RegexMatch { start: 24, end: 29 }));
        assert_eq!(iter.next(), None);
    }
}
