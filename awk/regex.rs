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
        next_char(&self.bytes, pos)
    }
}

/// The offset just past the character that starts at `pos` in `bytes`.
fn next_char(bytes: &[u8], pos: usize) -> usize {
    let mut next = pos + 1;
    if !charset::single_byte() {
        // UTF-8: skip continuation bytes.
        while next < bytes.len() && (bytes[next] & 0xc0) == 0x80 {
            next += 1;
        }
    }
    next
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

/// The bytes of the character an escape sequence of an awk ERE stands for,
/// and the length of the sequence, for the escapes regcomp does not know:
/// `\a`, `\b`, `\f`, `\n`, `\r`, `\t`, `\v` and `\ddd` (POSIX awk,
/// "Regular Expressions").  `escape` is the text after the backslash.
fn awk_escape(escape: &[u8]) -> Option<(Vec<u8>, usize)> {
    let control = match escape.first()? {
        b'a' => 0x07,
        b'b' => 0x08,
        b'f' => 0x0c,
        b'n' => b'\n',
        b'r' => b'\r',
        b't' => b'\t',
        b'v' => 0x0b,
        b'0'..=b'7' => {
            let digits = escape
                .iter()
                .take(3)
                .take_while(|c| (b'0'..=b'7').contains(c))
                .count();
            let code = escape[..digits]
                .iter()
                .fold(0u32, |code, digit| code * 8 + u32::from(digit - b'0'));
            // as in a string, the character with that code point, which is the
            // byte itself in a single-byte locale
            let c = char::from_u32(code)?;
            return Some((charset::encode(&c.to_string()).into_owned(), digits));
        }
        _ => return None,
    };
    Some((vec![control], 1))
}

/// The characters with a meaning in an ERE outside a bracket expression.
const ERE_SPECIAL: &[u8] = b".[]()*+?{}|^$\\";

/// Translates the escape sequences of an awk ERE, which may also appear in
/// bracket expressions, to what regcomp understands, where a backslash in a
/// bracket expression is an ordinary character.
///
/// Outside a bracket expression `\t`, `\ddd` and the other escapes of
/// [`awk_escape`] become their character, `\/` and `\"` a slash and a
/// quote, and `\8` and `\9` the digit (there are no back-references in an
/// ERE); any other escape is left to regcomp.  Inside one, every escaped
/// character stands for itself, as in gawk and mawk: `[\]a]` holds `]` and
/// `a`.  The characters a bracket expression would take as syntax are
/// written as collating symbols, so that `[\[-\]]` is the range from `[` to
/// `]`.
fn translate_awk_escapes(pattern: &[u8]) -> Cow<'_, [u8]> {
    // most patterns, `\.` and the like included, need no change
    let changes = |w: &[u8]| {
        w[0] == b'\\'
            && matches!(
                w[1],
                b'a' | b'b' | b'f' | b'n' | b'r' | b't' | b'v' | b'0'..=b'9' | b'/' | b'"'
            )
    };
    if !pattern.contains(&b'\\') || !(pattern.contains(&b'[') || pattern.windows(2).any(changes)) {
        return Cow::Borrowed(pattern);
    }
    let mut out = Vec::with_capacity(pattern.len() + 8);
    let mut i = 0;
    let mut in_bracket = false;
    while i < pattern.len() {
        let c = pattern[i];
        if in_bracket {
            match c {
                b'[' if matches!(pattern.get(i + 1), Some(b':' | b'.' | b'=')) => {
                    // a character class, collating symbol or equivalence
                    // class: copy it up to its closing `:]`, `.]` or `=]`
                    let delimiter = pattern[i + 1];
                    let close = pattern[i + 2..]
                        .windows(2)
                        .position(|w| w == [delimiter, b']'])
                        .map_or(pattern.len(), |p| i + 2 + p + 2);
                    out.extend_from_slice(&pattern[i..close]);
                    i = close;
                }
                b']' => {
                    in_bracket = false;
                    out.push(c);
                    i += 1;
                }
                b'\\' if i + 1 < pattern.len() => {
                    let (bytes, length) =
                        awk_escape(&pattern[i + 1..]).unwrap_or((vec![pattern[i + 1]], 1));
                    if let [b @ (b']' | b'[' | b'-' | b'^')] = bytes[..] {
                        out.extend_from_slice(&[b'[', b'.', b, b'.', b']']);
                    } else {
                        out.extend_from_slice(&bytes);
                    }
                    i += 1 + length;
                }
                _ => {
                    out.push(c);
                    i += 1;
                }
            }
        } else if c == b'[' {
            // a `]` or `^]` right after the `[` belongs to the expression
            in_bracket = true;
            out.push(c);
            i += 1;
            if pattern.get(i) == Some(&b'^') {
                out.push(b'^');
                i += 1;
            }
            if pattern.get(i) == Some(&b']') {
                out.push(b']');
                i += 1;
            }
        } else if c == b'\\' && i + 1 < pattern.len() {
            let next = pattern[i + 1];
            if let Some((bytes, length)) = awk_escape(&pattern[i + 1..]) {
                if let [b] = bytes[..] {
                    if ERE_SPECIAL.contains(&b) {
                        out.push(b'\\');
                    }
                }
                out.extend_from_slice(&bytes);
                i += 1 + length;
            } else {
                if !matches!(next, b'/' | b'"' | b'8' | b'9') {
                    out.push(b'\\');
                }
                out.push(next);
                i += 2;
            }
        } else {
            out.push(c);
            i += 1;
        }
    }
    Cow::Owned(out)
}

impl Regex {
    /// Compile the awk ERE whose bytes are `regex`.
    pub fn new(regex: CString) -> Result<Self, String> {
        let bytes = regex.into_bytes();
        let inner = PlibRegex::new_bytes(&translate_awk_escapes(&bytes), RegexFlags::ere())
            .map_err(|e| e.to_string())?;
        Ok(Self {
            inner,
            pattern_string: charset::decode(bytes),
        })
    }

    /// Returns the first match in the raw input bytes `bytes` that is not
    /// empty, as byte offsets: an empty match separates nothing.
    pub fn find_nonempty_bytes(&self, bytes: &[u8]) -> Option<RegexMatch> {
        let mut start = 0;
        while start <= bytes.len() {
            let subject = &bytes[start..];
            let m = if start == 0 {
                self.inner.find_bytes(subject)?
            } else {
                self.inner.find_notbol_bytes(subject)?
            };
            if m.start != m.end {
                return Some(RegexMatch {
                    start: start + m.start,
                    end: start + m.end,
                });
            }
            start = next_char(bytes, start + m.start);
        }
        None
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
