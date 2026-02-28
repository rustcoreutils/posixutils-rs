//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::regex::{Match, Regex as PlibRegex, RegexFlags};
use std::ffi::CString;

/// A regex wrapper that provides CString-compatible API for AWK.
/// Internally uses plib::regex for POSIX ERE support.
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

/// Iterator over regex matches in a string.
/// Owns the input CString to preserve lifetimes.
pub struct MatchIter<'re> {
    // Store the string as owned String to avoid lifetime issues
    string: String,
    next_start: usize,
    regex: &'re Regex,
}

impl Iterator for MatchIter<'_> {
    type Item = RegexMatch;
    fn next(&mut self) -> Option<Self::Item> {
        if self.next_start >= self.string.len() {
            return None;
        }

        // Find match starting from current offset
        let substring = &self.string[self.next_start..];
        let m = self.regex.inner.find(substring)?;

        let result = RegexMatch {
            start: self.next_start + m.start,
            end: self.next_start + m.end,
        };

        // Move past this match for next iteration
        // Ensure we make progress even on zero-width matches
        self.next_start = if m.end > 0 {
            self.next_start + m.end
        } else {
            self.next_start + 1
        };

        Some(result)
    }
}

impl Regex {
    pub fn new(regex: CString) -> Result<Self, String> {
        let pattern = regex.to_str().map_err(|e| e.to_string())?;
        let inner = PlibRegex::new(pattern, RegexFlags::ere()).map_err(|e| e.to_string())?;
        Ok(Self {
            inner,
            pattern_string: pattern.to_string(),
        })
    }

    /// Returns the first match location in the string, or `None`.
    /// Delegates to `PlibRegex::find` which handles CString conversion internally.
    pub fn find_first(&self, string: &str) -> Option<RegexMatch> {
        self.inner.find(string).map(RegexMatch::from)
    }

    /// Returns an iterator over all match locations in the string.
    /// Takes ownership of the CString.
    pub fn match_locations(&self, string: CString) -> MatchIter<'_> {
        let s = string.into_string().unwrap_or_default();
        MatchIter {
            next_start: 0,
            regex: self,
            string: s,
        }
    }

    pub fn pattern(&self) -> &str {
        &self.pattern_string
    }

    pub fn matches(&self, string: &CString) -> bool {
        let s = string.to_str().unwrap_or("");
        self.inner.is_match(s)
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
        let mut iter = ere.match_locations(CString::new("match 12345 match2 matchmatch").unwrap());
        assert_eq!(iter.next(), Some(RegexMatch { start: 0, end: 5 }));
        assert_eq!(iter.next(), Some(RegexMatch { start: 12, end: 17 }));
        assert_eq!(iter.next(), Some(RegexMatch { start: 19, end: 24 }));
        assert_eq!(iter.next(), Some(RegexMatch { start: 24, end: 29 }));
        assert_eq!(iter.next(), None);
    }
}
