//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX Regular Expression support using libc regcomp/regexec.
//!
//! This module provides a safe Rust wrapper around POSIX regex functions,
//! supporting both Basic Regular Expressions (BRE) and Extended Regular
//! Expressions (ERE).
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

use libc::{regcomp, regex_t, regexec, regfree, regmatch_t, REG_EXTENDED, REG_ICASE, REG_NOMATCH};
use std::ffi::CString;
use std::io::{Error, ErrorKind};
use std::ptr;

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

    /// Convert to libc cflags
    fn to_cflags(self) -> libc::c_int {
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
    pub fn as_str<'a>(&self, input: &'a str) -> &'a str {
        &input[self.start..self.end]
    }
}

/// A compiled POSIX regular expression.
pub struct Regex {
    raw: regex_t,
    /// Original pattern string, kept for Clone and Debug
    pattern: String,
    /// Flags used for compilation, kept for Clone
    flags: RegexFlags,
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
        // Handle macOS quirk: empty pattern causes REG_EMPTY error
        #[cfg(target_os = "macos")]
        let pattern = if pattern.is_empty() { ".*" } else { pattern };

        let c_pattern =
            CString::new(pattern).map_err(|e| Error::new(ErrorKind::InvalidInput, e))?;

        let mut raw = unsafe { std::mem::zeroed::<regex_t>() };
        let cflags = flags.to_cflags();

        let result = unsafe { regcomp(&mut raw, c_pattern.as_ptr(), cflags) };

        if result != 0 {
            // Get the error message using regerror
            let err_msg = Self::get_error_message(result, &raw);
            unsafe { regfree(&mut raw) };
            return Err(Error::new(
                ErrorKind::InvalidInput,
                format!("invalid regex '{}': {}", pattern, err_msg),
            ));
        }

        Ok(Regex {
            raw,
            pattern: pattern.to_string(),
            flags,
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

    /// Get error message from regcomp failure using regerror.
    fn get_error_message(errcode: libc::c_int, regex: &regex_t) -> String {
        let mut errbuf = [0u8; 256];
        unsafe {
            libc::regerror(
                errcode,
                regex as *const regex_t,
                errbuf.as_mut_ptr() as *mut libc::c_char,
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
        let Ok(c_text) = CString::new(text) else {
            return false;
        };

        let result = unsafe { regexec(&self.raw, c_text.as_ptr(), 0, ptr::null_mut(), 0) };

        result != REG_NOMATCH
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
        let c_text = CString::new(text).ok()?;

        let mut pmatch = regmatch_t {
            rm_so: -1,
            rm_eo: -1,
        };

        let result = unsafe {
            regexec(
                &self.raw,
                c_text.as_ptr(),
                1,
                &mut pmatch as *mut regmatch_t,
                0,
            )
        };

        if result == REG_NOMATCH || pmatch.rm_so < 0 {
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
        let c_text = CString::new(text).ok()?;

        let mut pmatch: [regmatch_t; MAX_CAPTURES] = unsafe { std::mem::zeroed() };

        let result = unsafe {
            regexec(
                &self.raw,
                c_text.as_ptr(),
                MAX_CAPTURES,
                pmatch.as_mut_ptr(),
                0,
            )
        };

        if result == REG_NOMATCH {
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
        if offset > text.len() {
            return None;
        }

        let substring = &text[offset..];
        let c_text = CString::new(substring).ok()?;

        let mut pmatch: [regmatch_t; MAX_CAPTURES] = unsafe { std::mem::zeroed() };

        let result = unsafe {
            regexec(
                &self.raw,
                c_text.as_ptr(),
                MAX_CAPTURES,
                pmatch.as_mut_ptr(),
                0,
            )
        };

        if result == REG_NOMATCH {
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

    /// Returns the original pattern string.
    pub fn as_str(&self) -> &str {
        &self.pattern
    }
}

impl Clone for Regex {
    fn clone(&self) -> Self {
        // Re-compile the pattern (regex_t cannot be safely copied)
        Regex::new(&self.pattern, self.flags).expect("failed to clone already-valid regex")
    }
}

impl Drop for Regex {
    fn drop(&mut self) {
        unsafe { regfree(&mut self.raw) }
    }
}

impl std::fmt::Debug for Regex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Regex")
            .field("pattern", &self.pattern)
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
        let c_text = CString::new(substring).ok()?;

        let mut pmatch = regmatch_t {
            rm_so: -1,
            rm_eo: -1,
        };

        let result = unsafe {
            regexec(
                &self.regex.raw,
                c_text.as_ptr(),
                1,
                &mut pmatch as *mut regmatch_t,
                0,
            )
        };

        if result == REG_NOMATCH || pmatch.rm_so < 0 {
            self.offset = self.text.len(); // Stop iteration
            return None;
        }

        let m = Match {
            start: self.offset + pmatch.rm_so as usize,
            end: self.offset + pmatch.rm_eo as usize,
        };

        // Move past this match for next iteration
        // Ensure we make progress even on zero-width matches
        self.offset = if pmatch.rm_eo as usize > 0 {
            self.offset + pmatch.rm_eo as usize
        } else {
            self.offset + 1
        };

        Some(m)
    }
}

#[cfg(test)]
mod tests {
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
        // Should work on all platforms (macOS workaround)
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
}
