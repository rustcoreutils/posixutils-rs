//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX Regular Expression support using regcomp/regexec.
//!
//! This module provides a safe Rust wrapper around POSIX regex functions,
//! supporting both Basic Regular Expressions (BRE) and Extended Regular
//! Expressions (ERE).
//!
//! On Unix the functions are the C library's. Windows has none, so there
//! they are musl's, vendored in `plib/vendor/musl-regex` and compiled by
//! `plib/build.rs` under `plib_`-prefixed names. Matching is per character
//! under the C runtime's `LC_CTYPE`, which [`crate::diag::init_locale`] makes
//! UTF-8 on Windows; `wchar_t` is 16 bits there, so characters above U+FFFF
//! are not supported.
//!
//! # Example
//!
//! ```ignore
//! use plib::regex::{Regex, RegexFlags};
//!
//! // Simple BRE matching
//! let re = Regex::new("hello", RegexFlags::default())?;
//! assert!(re.is_match("hello world"));
//!
//! // ERE with case-insensitive matching
//! let re = Regex::new("hello+", RegexFlags::ere().ignore_case())?;
//! assert!(re.is_match("HELLOOO"));
//! ```

use ffi::{
    regcomp, regerror, regexec, regfree, RegMatchT, RegexT, REG_EXTENDED, REG_ICASE, REG_NOTBOL,
};
use std::ffi::{c_char, c_int, CString};
use std::io::{Error, ErrorKind};
use std::ptr;

/// The C library's regex.
#[cfg(unix)]
mod ffi {
    pub use libc::{
        regcomp, regerror, regex_t as RegexT, regexec, regfree, regmatch_t as RegMatchT,
        REG_EXTENDED, REG_ICASE, REG_NOTBOL,
    };
}

/// musl's regex, built by `build.rs` under `plib_` names. The layouts and
/// values mirror `vendor/musl-regex/include/regex.h`, which is musl's own
/// except that `regoff_t` is `ptrdiff_t`: musl's pointer-sized `_Addr`, which
/// on 64-bit Windows is not `long` (32 bits there).
#[cfg(windows)]
mod ffi {
    use std::ffi::{c_char, c_int, c_void};

    /// Only ever handled through a pointer by the C side.
    #[repr(C)]
    pub struct RegexT {
        _re_nsub: usize,
        _opaque: *mut c_void,
        _padding: [*mut c_void; 4],
        _nsub2: usize,
        _padding2: c_char,
    }

    /// `regoff_t` is `ptrdiff_t`.
    #[repr(C)]
    pub struct RegMatchT {
        pub rm_so: isize,
        pub rm_eo: isize,
    }

    pub const REG_EXTENDED: c_int = 1;
    pub const REG_ICASE: c_int = 2;
    pub const REG_NOTBOL: c_int = 1;

    extern "C" {
        #[link_name = "plib_regcomp"]
        pub fn regcomp(preg: *mut RegexT, pattern: *const c_char, cflags: c_int) -> c_int;
        #[link_name = "plib_regexec"]
        pub fn regexec(
            preg: *const RegexT,
            input: *const c_char,
            nmatch: usize,
            pmatch: *mut RegMatchT,
            eflags: c_int,
        ) -> c_int;
        #[link_name = "plib_regfree"]
        pub fn regfree(preg: *mut RegexT);
        #[link_name = "plib_regerror"]
        pub fn regerror(
            errcode: c_int,
            preg: *const RegexT,
            errbuf: *mut c_char,
            errbuf_size: usize,
        ) -> usize;
    }
}

/// Whether `regexec` returned a match. Anything but 0 is none: `REG_NOMATCH`,
/// or an error -- macOS's `REG_ILLSEQ` for text that is not valid in the
/// locale's encoding -- which leaves the match offsets unset. Taking an error
/// for a match made it an empty one at the start of the text.
fn matched(result: c_int) -> bool {
    result == 0
}

/// Maximum number of capture groups supported
pub const MAX_CAPTURES: usize = 10;

/// Flags controlling regex compilation behavior.
#[derive(Debug, Clone, Copy, Default)]
pub struct RegexFlags {
    /// Use Extended Regular Expressions (ERE) instead of Basic (BRE)
    pub extended: bool,
    /// Perform case-insensitive matching
    pub ignore_case: bool,
}

impl RegexFlags {
    /// Create flags for Basic Regular Expression (BRE) mode.
    /// This is the default.
    pub fn bre() -> Self {
        Self::default()
    }

    /// Create flags for Extended Regular Expression (ERE) mode.
    pub fn ere() -> Self {
        Self {
            extended: true,
            ignore_case: false,
        }
    }

    /// Enable case-insensitive matching.
    pub fn ignore_case(mut self) -> Self {
        self.ignore_case = true;
        self
    }

    /// Convert to regcomp cflags
    fn to_cflags(self) -> c_int {
        let mut cflags = 0;
        if self.extended {
            cflags |= REG_EXTENDED;
        }
        if self.ignore_case {
            cflags |= REG_ICASE;
        }
        cflags
    }
}

/// A match location within the input string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Match {
    /// Start byte offset of the match
    pub start: usize,
    /// End byte offset of the match (exclusive)
    pub end: usize,
}

impl Match {
    /// Returns true if this is an empty/invalid match.
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Extract the matched substring from the original input.
    ///
    /// # Panics
    ///
    /// Panics if the match does not fall on character boundaries. Offsets come
    /// from `regexec` and are byte offsets, and in a single-byte locale a
    /// pattern such as `.` matches one byte of a multi-byte character. Use
    /// [`Match::as_bytes`] where the input may not be text.
    pub fn as_str<'a>(&self, input: &'a str) -> &'a str {
        &input[self.start..self.end]
    }

    /// Extract the matched bytes from the original input.
    pub fn as_bytes<'a>(&self, input: &'a [u8]) -> &'a [u8] {
        &input[self.start..self.end]
    }
}

/// A compiled POSIX regular expression.
pub struct Regex {
    raw: RegexT,
    /// Original pattern, kept for Clone and Debug. Bytes rather than a String
    /// because POSIX patterns are byte strings.
    pattern: Vec<u8>,
    /// Flags used for compilation, kept for Clone
    flags: RegexFlags,
    /// An empty pattern is not compiled at all; see [`Regex::new_bytes`].
    empty: bool,
}

// SAFETY: regex_t is thread-safe for matching (regexec is reentrant)
unsafe impl Send for Regex {}
unsafe impl Sync for Regex {}

impl Regex {
    /// Compile a new regular expression with the given flags.
    ///
    /// # Arguments
    ///
    /// * `pattern` - The regex pattern string
    /// * `flags` - Compilation flags (BRE vs ERE, case sensitivity)
    ///
    /// # Errors
    ///
    /// Returns an error if the pattern is invalid.
    pub fn new(pattern: &str, flags: RegexFlags) -> std::io::Result<Self> {
        Self::new_bytes(pattern.as_bytes(), flags)
    }

    /// Compile a regular expression given as bytes.
    ///
    /// POSIX patterns are byte strings, and this is the implementation the
    /// `&str` entry points delegate to.
    pub fn new_bytes(pattern: &[u8], flags: RegexFlags) -> std::io::Result<Self> {
        // An empty pattern matches the empty string at the start of the
        // subject. It is not handed to regcomp: macOS rejects it outright with
        // REG_EMPTY, and rewriting it to something else (".*" was tried) gives
        // a pattern with entirely different behavior. Matching it directly
        // makes both platforms agree, and agree with what glibc's regcomp
        // already does for it.
        if pattern.is_empty() {
            return Ok(Regex {
                raw: unsafe { std::mem::zeroed::<RegexT>() },
                pattern: Vec::new(),
                flags,
                empty: true,
            });
        }

        let c_pattern =
            CString::new(pattern).map_err(|e| Error::new(ErrorKind::InvalidInput, e))?;

        let mut raw = unsafe { std::mem::zeroed::<RegexT>() };
        let cflags = flags.to_cflags();

        let result = unsafe { regcomp(&mut raw, c_pattern.as_ptr(), cflags) };

        if result != 0 {
            // Get the error message using regerror
            let err_msg = Self::get_error_message(result, &raw);
            unsafe { regfree(&mut raw) };
            return Err(Error::new(
                ErrorKind::InvalidInput,
                format!(
                    "invalid regex '{}': {}",
                    String::from_utf8_lossy(pattern),
                    err_msg
                ),
            ));
        }

        Ok(Regex {
            raw,
            pattern: pattern.to_vec(),
            flags,
            empty: false,
        })
    }

    /// Compile a Basic Regular Expression (BRE).
    ///
    /// Convenience method equivalent to `Regex::new(pattern, RegexFlags::bre())`.
    pub fn bre(pattern: &str) -> std::io::Result<Self> {
        Self::new(pattern, RegexFlags::bre())
    }

    /// Compile an Extended Regular Expression (ERE).
    ///
    /// Convenience method equivalent to `Regex::new(pattern, RegexFlags::ere())`.
    pub fn ere(pattern: &str) -> std::io::Result<Self> {
        Self::new(pattern, RegexFlags::ere())
    }

    /// Compile a Basic Regular Expression (BRE) given as bytes.
    pub fn bre_bytes(pattern: &[u8]) -> std::io::Result<Self> {
        Self::new_bytes(pattern, RegexFlags::bre())
    }

    /// Compile an Extended Regular Expression (ERE) given as bytes.
    pub fn ere_bytes(pattern: &[u8]) -> std::io::Result<Self> {
        Self::new_bytes(pattern, RegexFlags::ere())
    }

    /// All capture groups set to an empty match at `offset`, the result for an
    /// empty pattern.
    fn empty_captures(offset: usize) -> Vec<Match> {
        vec![
            Match {
                start: offset,
                end: offset
            };
            MAX_CAPTURES
        ]
    }

    /// Get error message from regcomp failure using regerror.
    fn get_error_message(errcode: c_int, regex: &RegexT) -> String {
        let mut errbuf = [0u8; 256];
        unsafe {
            regerror(
                errcode,
                regex as *const RegexT,
                errbuf.as_mut_ptr() as *mut c_char,
                errbuf.len(),
            );
        }

        // Find null terminator and convert to string
        let len = errbuf.iter().position(|&b| b == 0).unwrap_or(errbuf.len());
        String::from_utf8_lossy(&errbuf[..len]).to_string()
    }

    /// Returns true if the pattern matches anywhere in the input string.
    ///
    /// # Arguments
    ///
    /// * `text` - The string to search
    ///
    /// # Returns
    ///
    /// `true` if the pattern matches, `false` otherwise.
    pub fn is_match(&self, text: &str) -> bool {
        self.is_match_bytes(text.as_bytes())
    }

    /// Returns true if the pattern matches anywhere in the input bytes.
    pub fn is_match_bytes(&self, text: &[u8]) -> bool {
        if self.empty {
            return true;
        }
        let Ok(c_text) = CString::new(text) else {
            return false;
        };

        let result = unsafe { regexec(&self.raw, c_text.as_ptr(), 0, ptr::null_mut(), 0) };

        matched(result)
    }

    /// Find the first match in the input string.
    ///
    /// # Arguments
    ///
    /// * `text` - The string to search
    ///
    /// # Returns
    ///
    /// `Some(Match)` with the location of the first match, or `None` if no match.
    pub fn find(&self, text: &str) -> Option<Match> {
        self.find_bytes(text.as_bytes())
    }

    /// Find the first match in the input bytes.
    pub fn find_bytes(&self, text: &[u8]) -> Option<Match> {
        if self.empty {
            return Some(Match { start: 0, end: 0 });
        }
        let c_text = CString::new(text).ok()?;

        let mut pmatch = RegMatchT {
            rm_so: -1,
            rm_eo: -1,
        };

        let result = unsafe {
            regexec(
                &self.raw,
                c_text.as_ptr(),
                1,
                &mut pmatch as *mut RegMatchT,
                0,
            )
        };

        if !matched(result) || pmatch.rm_so < 0 {
            return None;
        }

        Some(Match {
            start: pmatch.rm_so as usize,
            end: pmatch.rm_eo as usize,
        })
    }

    /// Find the first match, treating the start of the string as NOT the beginning of a line.
    /// This prevents `^` from matching at the start of `text`.
    pub fn find_notbol(&self, text: &str) -> Option<Match> {
        self.find_notbol_bytes(text.as_bytes())
    }

    /// As [`Regex::find_notbol`], over bytes.
    pub fn find_notbol_bytes(&self, text: &[u8]) -> Option<Match> {
        if self.empty {
            return Some(Match { start: 0, end: 0 });
        }
        let c_text = CString::new(text).ok()?;

        let mut pmatch = RegMatchT {
            rm_so: -1,
            rm_eo: -1,
        };

        let result = unsafe {
            regexec(
                &self.raw,
                c_text.as_ptr(),
                1,
                &mut pmatch as *mut RegMatchT,
                REG_NOTBOL,
            )
        };

        if !matched(result) || pmatch.rm_so < 0 {
            return None;
        }

        Some(Match {
            start: pmatch.rm_so as usize,
            end: pmatch.rm_eo as usize,
        })
    }

    /// Find all capture groups in the input string.
    ///
    /// Group 0 is always the entire match. Groups 1-9 are the parenthesized
    /// subexpressions (in BRE: `\(...\)`, in ERE: `(...)`).
    ///
    /// # Arguments
    ///
    /// * `text` - The string to search
    ///
    /// # Returns
    ///
    /// `Some(Vec<Match>)` with all capture groups, or `None` if no match.
    /// Empty/unused groups have `start == end == 0`.
    pub fn captures(&self, text: &str) -> Option<Vec<Match>> {
        self.captures_bytes(text.as_bytes())
    }

    /// Find all capture groups in the input bytes.
    ///
    /// Offsets are byte offsets, so a caller holding text must slice with
    /// [`Match::as_bytes`] or check character boundaries itself.
    pub fn captures_bytes(&self, text: &[u8]) -> Option<Vec<Match>> {
        if self.empty {
            return Some(Self::empty_captures(0));
        }
        let c_text = CString::new(text).ok()?;

        let mut pmatch: [RegMatchT; MAX_CAPTURES] = unsafe { std::mem::zeroed() };

        let result = unsafe {
            regexec(
                &self.raw,
                c_text.as_ptr(),
                MAX_CAPTURES,
                pmatch.as_mut_ptr(),
                0,
            )
        };

        if !matched(result) {
            return None;
        }

        let matches: Vec<Match> = pmatch
            .iter()
            .map(|m| {
                if m.rm_so >= 0 && m.rm_eo >= 0 {
                    Match {
                        start: m.rm_so as usize,
                        end: m.rm_eo as usize,
                    }
                } else {
                    Match::default()
                }
            })
            .collect();

        Some(matches)
    }

    /// Execute regex and return captures, continuing from a given offset.
    ///
    /// This is useful for finding multiple matches in a string.
    ///
    /// # Arguments
    ///
    /// * `text` - The string to search (full string, not a slice)
    /// * `offset` - Byte offset to start searching from
    ///
    /// # Returns
    ///
    /// `Some(Vec<Match>)` with captures (positions relative to start of `text`),
    /// or `None` if no match found at or after offset.
    pub fn captures_at(&self, text: &str, offset: usize) -> Option<Vec<Match>> {
        self.captures_at_bytes(text.as_bytes(), offset)
    }

    /// As [`Regex::captures_at`], over bytes.
    pub fn captures_at_bytes(&self, text: &[u8], offset: usize) -> Option<Vec<Match>> {
        if offset > text.len() {
            return None;
        }
        if self.empty {
            return Some(Self::empty_captures(offset));
        }

        let substring = &text[offset..];
        let c_text = CString::new(substring).ok()?;

        let mut pmatch: [RegMatchT; MAX_CAPTURES] = unsafe { std::mem::zeroed() };

        // Past the start of `text`, the substring's first byte is not the
        // beginning of a line, so `^` must not match there.  Without
        // REG_NOTBOL a global substitute re-anchors `^` at every restart:
        // `s/^/> /g` on "abc" produced "> a> b> c> ".
        let flags = if offset == 0 { 0 } else { REG_NOTBOL };

        let result = unsafe {
            regexec(
                &self.raw,
                c_text.as_ptr(),
                MAX_CAPTURES,
                pmatch.as_mut_ptr(),
                flags,
            )
        };

        if !matched(result) {
            return None;
        }

        // Adjust positions to be relative to original string start
        let matches: Vec<Match> = pmatch
            .iter()
            .map(|m| {
                if m.rm_so >= 0 && m.rm_eo >= 0 {
                    Match {
                        start: offset + m.rm_so as usize,
                        end: offset + m.rm_eo as usize,
                    }
                } else {
                    Match::default()
                }
            })
            .collect();

        Some(matches)
    }

    /// Returns an iterator over all non-overlapping matches in the input.
    pub fn find_iter<'r, 't>(&'r self, text: &'t str) -> MatchIter<'r, 't> {
        MatchIter {
            regex: self,
            text,
            offset: 0,
        }
    }

    /// Returns the original pattern as text, or the empty string if the
    /// pattern is not valid UTF-8 (only a byte constructor can produce that).
    pub fn as_str(&self) -> &str {
        std::str::from_utf8(&self.pattern).unwrap_or_default()
    }

    /// Returns the original pattern bytes.
    pub fn as_bytes(&self) -> &[u8] {
        &self.pattern
    }
}

impl Clone for Regex {
    fn clone(&self) -> Self {
        // Re-compile the pattern (regex_t cannot be safely copied)
        Regex::new_bytes(&self.pattern, self.flags).expect("failed to clone already-valid regex")
    }
}

impl Drop for Regex {
    fn drop(&mut self) {
        // An empty pattern was never compiled, so there is nothing to free.
        if !self.empty {
            unsafe { regfree(&mut self.raw) }
        }
    }
}

impl std::fmt::Debug for Regex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Regex")
            .field("pattern", &String::from_utf8_lossy(&self.pattern))
            .field("flags", &self.flags)
            .finish()
    }
}

impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern
            && self.flags.extended == other.flags.extended
            && self.flags.ignore_case == other.flags.ignore_case
    }
}

impl Eq for Regex {}

/// Iterator over all non-overlapping matches in a string.
pub struct MatchIter<'r, 't> {
    regex: &'r Regex,
    text: &'t str,
    offset: usize,
}

impl Iterator for MatchIter<'_, '_> {
    type Item = Match;

    fn next(&mut self) -> Option<Self::Item> {
        if self.offset > self.text.len() {
            return None;
        }

        let substring = &self.text[self.offset..];
        let m = if self.offset == 0 {
            self.regex.find(substring)?
        } else {
            self.regex.find_notbol(substring)?
        };

        let result = Match {
            start: self.offset + m.start,
            end: self.offset + m.end,
        };

        // Move past this match for next iteration
        // Ensure we make progress even on zero-width matches
        self.offset = if m.end > 0 {
            self.offset + m.end
        } else {
            let mut next = self.offset + 1;
            while next < self.text.len() && !self.text.is_char_boundary(next) {
                next += 1;
            }
            next
        };

        Some(result)
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn empty_pattern_matches_empty_at_the_start() {
        // macOS regcomp rejects an empty pattern outright, so it is matched
        // directly rather than compiled; both platforms must agree with what
        // glibc does for it.
        let re = Regex::bre("").expect("an empty pattern is valid");
        assert!(re.is_match("abc"));
        assert_eq!(re.find("abc"), Some(Match { start: 0, end: 0 }));
        let caps = re.captures("abc").expect("empty matches");
        assert_eq!(caps[0], Match { start: 0, end: 0 });
        assert_eq!(
            re.captures_at("abc", 2).unwrap()[0],
            Match { start: 2, end: 2 }
        );
        assert_eq!(re.as_str(), "");
    }

    #[test]
    fn byte_patterns_and_subjects() {
        // POSIX operands are byte strings, and need not be valid text.
        //
        // `.` matches an invalid-UTF-8 byte only in the C locale, so this holds
        // the locale lock: a `locale` test running in parallel would otherwise
        // have the process in UTF-8 and `\xff` would match nothing.
        let _guard = crate::locale_test_lock();
        let re = Regex::bre_bytes(b"a.b").expect("valid");
        let subject = b"xa\xffb";
        let caps = re.captures_bytes(subject).expect("should match");
        assert_eq!(caps[0], Match { start: 1, end: 4 });
        assert_eq!(caps[0].as_bytes(subject), b"a\xffb");
        assert!(re.is_match_bytes(subject));
        assert_eq!(re.find_bytes(subject), Some(Match { start: 1, end: 4 }));
    }

    #[test]
    fn byte_and_str_entry_points_agree() {
        let re = Regex::bre("h\\(.*\\)o").expect("valid");
        assert_eq!(re.captures("hello"), re.captures_bytes(b"hello"));
        assert_eq!(re.find("hello"), re.find_bytes(b"hello"));
        assert_eq!(re.is_match("hello"), re.is_match_bytes(b"hello"));
    }

    #[test]
    fn match_offsets_are_bytes_not_characters() {
        // The offsets come from regexec and are byte offsets; a caller holding
        // text has to slice with as_bytes or check boundaries.
        //
        // Locale-dependent for the same reason: in UTF-8 the two bytes of "é"
        // are one character, so `..` would need a second one.
        let _guard = crate::locale_test_lock();
        let re = Regex::bre("..").expect("valid");
        let subject = "é".as_bytes(); // two bytes, one character
        let m = re.find_bytes(subject).expect("matches two bytes");
        assert_eq!(m, Match { start: 0, end: 2 });
        assert_eq!(m.as_bytes(subject), subject);
    }
    use super::*;

    #[test]
    fn test_bre_simple_match() {
        let re = Regex::bre("hello").unwrap();
        assert!(re.is_match("hello world"));
        assert!(re.is_match("say hello"));
        assert!(!re.is_match("HELLO"));
        assert!(!re.is_match("hi"));
    }

    #[test]
    fn test_ere_simple_match() {
        let re = Regex::ere("hello+").unwrap();
        assert!(re.is_match("hellooooo"));
        assert!(re.is_match("hello"));
        assert!(!re.is_match("hell"));
    }

    #[test]
    fn test_case_insensitive() {
        let re = Regex::new("hello", RegexFlags::bre().ignore_case()).unwrap();
        assert!(re.is_match("HELLO"));
        assert!(re.is_match("Hello"));
        assert!(re.is_match("hello"));
    }

    #[test]
    fn test_ere_case_insensitive() {
        let re = Regex::new("hello+", RegexFlags::ere().ignore_case()).unwrap();
        assert!(re.is_match("HELLOOO"));
        assert!(re.is_match("Hello"));
    }

    #[test]
    fn test_find() {
        let re = Regex::bre("world").unwrap();
        let m = re.find("hello world").unwrap();
        assert_eq!(m.start, 6);
        assert_eq!(m.end, 11);
        assert_eq!(m.as_str("hello world"), "world");
    }

    #[test]
    fn test_find_no_match() {
        let re = Regex::bre("xyz").unwrap();
        assert!(re.find("hello world").is_none());
    }

    #[test]
    fn test_find_iter() {
        let re = Regex::bre("a").unwrap();
        let matches: Vec<Match> = re.find_iter("abracadabra").collect();
        assert_eq!(matches.len(), 5);
        assert_eq!(matches[0], Match { start: 0, end: 1 });
        assert_eq!(matches[1], Match { start: 3, end: 4 });
        assert_eq!(matches[2], Match { start: 5, end: 6 });
        assert_eq!(matches[3], Match { start: 7, end: 8 });
        assert_eq!(matches[4], Match { start: 10, end: 11 });
    }

    #[test]
    fn test_captures_bre() {
        // BRE uses \( \) for groups
        let re = Regex::bre(r"^\(.*\)$").unwrap();
        let caps = re.captures("hello").unwrap();
        assert_eq!(caps[0], Match { start: 0, end: 5 }); // Full match
        assert_eq!(caps[1], Match { start: 0, end: 5 }); // Group 1
    }

    #[test]
    fn test_captures_ere() {
        // ERE uses ( ) for groups
        let re = Regex::ere(r"^(.*)$").unwrap();
        let caps = re.captures("hello").unwrap();
        assert_eq!(caps[0], Match { start: 0, end: 5 }); // Full match
        assert_eq!(caps[1], Match { start: 0, end: 5 }); // Group 1
    }

    #[test]
    fn test_invalid_pattern() {
        let result = Regex::bre("[invalid");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("invalid regex"));
    }

    #[test]
    fn test_clone() {
        let re1 = Regex::bre("hello").unwrap();
        let re2 = re1.clone();
        assert!(re2.is_match("hello"));
        assert_eq!(re1.as_str(), re2.as_str());
    }

    #[test]
    fn test_empty_pattern() {
        // An empty pattern is matched directly rather than compiled, so this
        // holds on macOS too, where regcomp rejects it.
        let re = Regex::bre("").unwrap();
        assert!(re.is_match("anything"));
    }

    #[test]
    fn test_anchors() {
        let re = Regex::bre("^hello$").unwrap();
        assert!(re.is_match("hello"));
        assert!(!re.is_match("hello world"));
        assert!(!re.is_match("say hello"));
    }

    #[test]
    fn test_special_bre_chars() {
        // In BRE, ( is literal, \( is grouping
        let re = Regex::bre("main(").unwrap();
        assert!(re.is_match("int main() {"));
    }

    #[test]
    fn test_special_ere_chars() {
        // In ERE, ( is grouping, \( is literal
        let re = Regex::ere(r"main\(").unwrap();
        assert!(re.is_match("int main() {"));
    }

    #[test]
    fn test_captures_at_empty_string() {
        // Regression test: captures_at must use `offset > text.len()` not `offset >= text.len()`
        // to allow patterns like ^$ to match empty strings
        let re = Regex::bre("^$").unwrap();

        // Pattern ^$ should match an empty string
        let caps = re.captures("");
        assert!(caps.is_some(), "^$ should match empty string");

        // captures_at with offset 0 on empty string should also match
        let caps = re.captures_at("", 0);
        assert!(
            caps.is_some(),
            "captures_at(\"\", 0) should match ^$ pattern"
        );

        // The match should be at position 0..0 (zero-length match)
        let caps = caps.unwrap();
        assert_eq!(caps[0].start, 0);
        assert_eq!(caps[0].end, 0);

        // captures_at with offset past end should return None
        let caps = re.captures_at("", 1);
        assert!(caps.is_none(), "captures_at(\"\", 1) should return None");
    }

    #[test]
    fn test_find_iter_empty_string() {
        // Regression test: find_iter must handle empty strings correctly
        let re = Regex::bre("^$").unwrap();

        // find_iter on empty string should yield exactly one match
        let matches: Vec<Match> = re.find_iter("").collect();
        assert_eq!(matches.len(), 1, "^$ should match empty string once");
        assert_eq!(matches[0], Match { start: 0, end: 0 });
    }

    #[test]
    fn test_find_iter_multibyte_zero_width() {
        // Regression test: zero-width match must not split multi-byte UTF-8 characters
        // "aéb" — é is 2 bytes (U+00E9), so "a*" zero-width matches between bytes
        // must skip to the next char boundary
        let re = Regex::ere("a*").unwrap();
        let matches: Vec<Match> = re.find_iter("aéb").collect();
        // Expected: "a" at 0..1, "" at 1..1 (before é), "" at 3..3 (before b), "" at 4..4 (end)
        assert_eq!(matches.len(), 4, "should produce 4 matches without panic");
        assert_eq!(matches[0], Match { start: 0, end: 1 });
    }

    #[test]
    fn test_captures_at_end_of_string() {
        // Test that $ anchor works at end of non-empty string
        let re = Regex::bre("$").unwrap();

        // $ should match at the end of "hello" (position 5)
        let caps = re.captures_at("hello", 5);
        assert!(
            caps.is_some(),
            "$ should match at end of string (offset == len)"
        );
        let caps = caps.unwrap();
        assert_eq!(caps[0].start, 5);
        assert_eq!(caps[0].end, 5);
    }

    /// Past offset 0 the substring's first byte is not a beginning of line.
    /// Without REG_NOTBOL a global substitute loop re-anchors `^` at each
    /// restart, so `s/^/> /g` on "abc" yielded "> a> b> c> ".
    #[test]
    fn test_captures_at_does_not_reanchor_bol() {
        let re = Regex::new("^", RegexFlags::bre()).unwrap();
        assert!(
            re.captures_at("abc", 0).is_some(),
            "^ must match at the start of the text"
        );
        for offset in 1..=3 {
            assert!(
                re.captures_at("abc", offset).is_none(),
                "^ must not match at offset {}",
                offset
            );
        }
    }

    /// `$` is unaffected: the substring really does end where the text ends.
    #[test]
    fn test_captures_at_still_matches_eol() {
        let re = Regex::new("$", RegexFlags::bre()).unwrap();
        assert!(re.captures_at("abc", 3).is_some());
    }

    #[test]
    fn bre_backreference() {
        let re = Regex::bre(r"^\(a*\)b\1$").unwrap();
        let caps = re.captures("aabaa").expect("matches");
        assert_eq!(caps[0], Match { start: 0, end: 5 });
        assert_eq!(caps[1], Match { start: 0, end: 2 });
        assert!(!re.is_match("aaba"));

        let re = Regex::bre(r"\(ab\)\1").unwrap();
        assert_eq!(re.find("xxabab"), Some(Match { start: 2, end: 6 }));
        assert!(!re.is_match("abba"));
    }

    /// POSIX picks the longest of the leftmost matches, not the first
    /// alternative that matches.
    #[test]
    fn leftmost_longest() {
        let re = Regex::ere("a|ab").unwrap();
        assert_eq!(re.find("xabc"), Some(Match { start: 1, end: 3 }));

        let re = Regex::ere("(a|ab)(c|bcd)").unwrap();
        assert_eq!(re.find("abcd"), Some(Match { start: 0, end: 4 }));

        let re = Regex::bre("x*").unwrap();
        assert_eq!(re.find("xxxy"), Some(Match { start: 0, end: 3 }));
    }

    #[test]
    fn bracket_classes() {
        let re = Regex::ere("[[:digit:]]+").unwrap();
        assert_eq!(re.find("ab123c"), Some(Match { start: 2, end: 5 }));

        let re = Regex::ere("[[:alpha:]][[:alnum:]_]*").unwrap();
        assert_eq!(re.find("12 foo_9 x"), Some(Match { start: 3, end: 8 }));

        let re = Regex::ere("[[:upper:]][[:lower:]]+").unwrap();
        assert_eq!(re.find("abc Def"), Some(Match { start: 4, end: 7 }));

        // blank is space and tab, and not newline.
        let re = Regex::ere("a[[:blank:]]b").unwrap();
        assert!(re.is_match("a b"));
        assert!(re.is_match("a\tb"));
        assert!(!re.is_match("a\nb"));

        let re = Regex::ere("[^[:space:]]+").unwrap();
        assert_eq!(re.find(" \t xy z"), Some(Match { start: 3, end: 5 }));

        let re = Regex::bre("[[:punct:]]").unwrap();
        assert_eq!(re.find("ab,c"), Some(Match { start: 2, end: 3 }));

        assert!(Regex::bre("[[:nosuchclass:]]").is_err());
    }

    #[test]
    fn ignore_case_ranges_and_backreferences() {
        let re = Regex::new("[a-c]x", RegexFlags::bre().ignore_case()).unwrap();
        assert_eq!(re.find("zBX"), Some(Match { start: 1, end: 3 }));

        // The group itself ignores case. Whether the back-reference then
        // matches its text in another case differs: glibc's does, musl's
        // (Windows) compares bytes.
        let re = Regex::new(r"\(ab\)\1", RegexFlags::bre().ignore_case()).unwrap();
        assert!(re.is_match("ABAB"));
        assert!(re.is_match("xAbAb"));
    }

    #[test]
    fn notbol_suppresses_caret_only() {
        let re = Regex::bre("^a").unwrap();
        assert_eq!(re.find("abc"), Some(Match { start: 0, end: 1 }));
        assert_eq!(re.find_notbol("abc"), None);

        let re = Regex::bre("a").unwrap();
        assert_eq!(re.find_notbol("abc"), Some(Match { start: 0, end: 1 }));

        let re = Regex::bre("c$").unwrap();
        assert_eq!(re.find_notbol("abc"), Some(Match { start: 2, end: 3 }));
    }

    /// Restores `LC_CTYPE` when dropped, so a failing assertion cannot leave
    /// the process in another locale for the tests after it.
    struct CtypeRestore(Vec<u8>);

    impl Drop for CtypeRestore {
        fn drop(&mut self) {
            gettextrs::setlocale(gettextrs::LocaleCategory::LcCType, self.0.clone());
        }
    }

    /// Switch `LC_CTYPE` to UTF-8 until the result is dropped. `None` when the
    /// C runtime has no UTF-8 locale: a Unix host may have none installed,
    /// and the msvcrt that the Windows `-gnu` targets link (as under Wine)
    /// has none at all. The UCRT of an MSVC build always has one.
    fn utf8_ctype() -> Option<CtypeRestore> {
        use gettextrs::{setlocale, LocaleCategory};
        const NAMES: &[&str] = if cfg!(windows) {
            &[".UTF-8"]
        } else {
            &["C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8"]
        };
        // SAFETY: a null locale only queries; the returned string is copied
        // before the next setlocale call can invalidate it.
        let saved = unsafe {
            let p = libc::setlocale(libc::LC_CTYPE, ptr::null());
            assert!(!p.is_null(), "LC_CTYPE has a name");
            std::ffi::CStr::from_ptr(p).to_bytes().to_vec()
        };
        let restore = CtypeRestore(saved);
        let found = NAMES
            .iter()
            .any(|name| setlocale(LocaleCategory::LcCType, *name).is_some());
        #[cfg(target_env = "msvc")]
        assert!(found, "the UCRT accepts setlocale(LC_CTYPE, \".UTF-8\")");
        found.then_some(restore)
    }

    /// Under a UTF-8 `LC_CTYPE` a multibyte character is one character to the
    /// pattern, and offsets are still bytes.
    #[test]
    fn utf8_multibyte_pattern() {
        let _guard = crate::locale_test_lock();
        let Some(_ctype) = utf8_ctype() else {
            return;
        };

        // "é" is two bytes but one character, so `.` takes all of it.
        let re = Regex::bre("^.$").unwrap();
        assert!(re.is_match("é"));
        let re = Regex::bre("^..$").unwrap();
        assert!(!re.is_match("é"));

        let re = Regex::ere("é+").unwrap();
        assert_eq!(re.find("xééy"), Some(Match { start: 1, end: 5 }));

        let re = Regex::ere("[àé]x").unwrap();
        assert_eq!(re.find("aàx"), Some(Match { start: 1, end: 4 }));

        let re = Regex::ere("[^é]").unwrap();
        assert_eq!(re.find("éa"), Some(Match { start: 2, end: 3 }));

        // Back-references after a multibyte character: musl's backtracking
        // matcher took every character to be one byte here, which the vendored
        // copy fixes (see vendor/musl-regex/README.md). Apple's libc regex
        // comes from the same TRE code and still has the flaw, and on macOS
        // plib uses the system engine, so these hold everywhere but there.
        if cfg!(not(target_vendor = "apple")) {
            let re = Regex::bre(r"\(ü\)\1").unwrap();
            assert_eq!(re.find("aüü"), Some(Match { start: 1, end: 5 }));
            let re = Regex::bre(r"\(a\)\1").unwrap();
            assert_eq!(re.find("üaa"), Some(Match { start: 2, end: 4 }));
            let re = Regex::bre(r"\(.\)\1").unwrap();
            assert_eq!(re.find("xéüü"), Some(Match { start: 3, end: 7 }));
        }
    }
}
