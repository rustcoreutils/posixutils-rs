//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::pattern::parse::{parse_pattern, PatternItem};
use crate::pattern::regex::{parsed_pattern_to_regex, Regex};
use crate::wordexp::expanded_word::ExpandedWord;
use std::ffi::{CStr, CString};

mod parse;
mod regex;

pub struct Pattern {
    pattern_string: String,
    regex: Regex,
}

impl Pattern {
    pub fn new(word: &ExpandedWord) -> Result<Self, String> {
        let parsed_pattern = parse_pattern(word, false)?;
        let regex = parsed_pattern_to_regex(&parsed_pattern)?;
        Ok(Self {
            pattern_string: word.to_string(),
            regex,
        })
    }

    pub fn matches(&self, s: &CStr) -> bool {
        // A shell pattern must match the ENTIRE string (e.g. in `case`), unlike
        // the substring semantics of the underlying regex engine. Because the
        // engine is POSIX leftmost-longest, if any match begins at offset 0 then
        // the leftmost match begins at 0 and is the longest there, so checking
        // that it spans the whole string is a correct full-match test.
        match self.regex.match_locations(s).next() {
            Some(m) => m.start == 0 && m.end == s.to_bytes().len(),
            None => false,
        }
    }

    /// Byte offsets at which an affix of `s` may begin or end: every character
    /// boundary, including both ends. Removal must never split a character.
    fn boundaries(s: &str) -> impl DoubleEndedIterator<Item = usize> + '_ {
        s.char_indices()
            .map(|(i, _)| i)
            .chain(std::iter::once(s.len()))
    }

    /// Does the pattern match the whole of `candidate`, anchored at both ends?
    ///
    /// The affix-removal operators are defined in terms of affixes that the
    /// pattern matches *in their entirety* (POSIX 2.6.2), so a substring test
    /// is the wrong question to ask. The underlying engine is NUL-terminated,
    /// so a candidate containing an interior NUL cannot be expressed and never
    /// matches; the byte matcher that replaces this engine removes that
    /// limitation.
    fn matches_entire(&self, candidate: &str) -> bool {
        CString::new(candidate).is_ok_and(|c| self.matches(&c))
    }

    /// Longest prefix the pattern matches entirely, removed. On no match the
    /// value is returned unchanged.
    pub fn remove_largest_prefix(&self, s: String) -> String {
        let found = Self::boundaries(&s)
            .rev()
            .find(|&e| self.matches_entire(&s[..e]));
        match found {
            Some(end) => s[end..].to_string(),
            None => s,
        }
    }

    /// Shortest prefix the pattern matches entirely, removed.
    pub fn remove_shortest_prefix(&self, s: String) -> String {
        let found = Self::boundaries(&s).find(|&e| self.matches_entire(&s[..e]));
        match found {
            Some(end) => s[end..].to_string(),
            None => s,
        }
    }

    /// Longest suffix the pattern matches entirely, removed. The longest suffix
    /// is the one starting earliest.
    pub fn remove_largest_suffix(&self, s: String) -> String {
        let found = Self::boundaries(&s).find(|&b| self.matches_entire(&s[b..]));
        match found {
            Some(begin) => s[..begin].to_string(),
            None => s,
        }
    }

    /// Shortest suffix the pattern matches entirely, removed.
    pub fn remove_shortest_suffix(&self, s: String) -> String {
        let found = Self::boundaries(&s)
            .rev()
            .find(|&b| self.matches_entire(&s[b..]));
        match found {
            Some(begin) => s[..begin].to_string(),
            None => s,
        }
    }
}

impl From<Pattern> for String {
    fn from(value: Pattern) -> Self {
        value.pattern_string
    }
}

struct FilenamePatternPart {
    regex: Regex,
    starts_with_dot: bool,
}

pub struct FilenamePattern {
    path_parts: Vec<FilenamePatternPart>,
    pattern_string: String,
}

impl FilenamePattern {
    pub fn new(word: &ExpandedWord) -> Result<Self, String> {
        let pattern_string = word.to_string();
        let parsed_pattern = parse_pattern(word, true)?;
        let mut path_parts = Vec::new();

        parsed_pattern
            .split(|item| *item == PatternItem::Char('/'))
            .filter(|items| !items.is_empty())
            .try_for_each(|items| {
                let starts_with_dot = items.starts_with(&[PatternItem::Char('.')]);
                let regex = parsed_pattern_to_regex(items)?;
                path_parts.push(FilenamePatternPart {
                    regex,
                    starts_with_dot,
                });
                Ok::<(), String>(())
            })?;

        Ok(Self {
            path_parts,
            pattern_string,
        })
    }

    /// # Panics
    /// panics if `depth` is smaller than 1 or bigger than `component_count`
    pub fn matches_all(&self, depth: usize, s: &CStr) -> bool {
        assert!(
            depth > 0 && depth <= self.component_count(),
            "invalid depth"
        );
        let component_index = depth - 1;
        if s.to_bytes().first() == Some(&b'.') && !self.path_parts[component_index].starts_with_dot
        {
            // dot at the start is only matched explicitly
            return false;
        }
        if let Some(loc) = self.path_parts[component_index]
            .regex
            .match_locations(s)
            .next()
        {
            loc.start == 0 && loc.end == s.count_bytes()
        } else {
            false
        }
    }

    /// Returns number of components in the path
    /// If it returns 0 then the pattern is just a directory (root if it starts
    /// with '/', the current directory otherwise)
    pub fn component_count(&self) -> usize {
        self.path_parts.len()
    }

    pub fn is_absolute(&self) -> bool {
        self.pattern_string.starts_with('/')
    }
}

impl From<FilenamePattern> for String {
    fn from(value: FilenamePattern) -> Self {
        value.pattern_string
    }
}

impl TryFrom<String> for FilenamePattern {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        let value = ExpandedWord::unquoted_literal(value);
        FilenamePattern::new(&value)
    }
}

pub struct HistoryPattern {
    regex: Regex,
    match_only_at_line_start: bool,
}

impl HistoryPattern {
    pub fn new(pattern: String) -> Result<Self, String> {
        let parsed_pattern = parse_pattern(&ExpandedWord::unquoted_literal(pattern), false)?;
        if parsed_pattern
            .first()
            .is_some_and(|p| *p == PatternItem::Char('^'))
        {
            let regex = parsed_pattern_to_regex(&parsed_pattern[1..])?;
            Ok(Self {
                regex,
                match_only_at_line_start: true,
            })
        } else {
            let regex = parsed_pattern_to_regex(&parsed_pattern)?;
            Ok(Self {
                regex,
                match_only_at_line_start: false,
            })
        }
    }

    pub fn matches(&self, s: &str) -> bool {
        if let Ok(s_cstr) = CString::new(s) {
            if let Some(first_match) = self.regex.match_locations(&s_cstr).next() {
                if self.match_only_at_line_start && first_match.start != 0 {
                    return false;
                }
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    pub fn pattern_from_str(pat: &str) -> Pattern {
        Pattern::new(&ExpandedWord::unquoted_literal(pat)).expect("failed to create pattern")
    }

    pub fn filename_pattern_from_str(pat: &str) -> FilenamePattern {
        FilenamePattern::new(&ExpandedWord::unquoted_literal(pat))
            .expect("failed to create filename pattern")
    }

    fn cstring_from_str(s: &str) -> CString {
        CString::new(s).unwrap()
    }

    #[test]
    fn remove_largest_prefix_from_empty_string() {
        assert_eq!(
            pattern_from_str("abcd").remove_largest_prefix("".to_string()),
            ""
        )
    }

    #[test]
    fn remove_smallest_prefix_from_empty_string() {
        assert_eq!(
            pattern_from_str("abcd").remove_shortest_prefix("".to_string()),
            ""
        )
    }

    #[test]
    fn remove_largest_suffix_from_empty_string() {
        assert_eq!(
            pattern_from_str("abcd").remove_largest_suffix("".to_string()),
            ""
        )
    }

    #[test]
    fn remove_smallest_suffix_from_empty_string() {
        assert_eq!(
            pattern_from_str("abcd").remove_shortest_suffix("".to_string()),
            ""
        )
    }

    #[test]
    fn remove_largest_prefix() {
        assert_eq!(
            pattern_from_str("*b").remove_largest_prefix("abaaaaabtest".to_string()),
            "test"
        )
    }

    #[test]
    fn remove_smallest_prefix() {
        assert_eq!(
            pattern_from_str("*b").remove_shortest_prefix("abaaaaabtest".to_string()),
            "aaaaabtest"
        )
    }

    #[test]
    fn remove_largest_suffix() {
        assert_eq!(
            pattern_from_str("b*").remove_largest_suffix("testbaaaaaba".to_string()),
            "test"
        )
    }

    #[test]
    fn remove_smallest_suffix() {
        assert_eq!(
            pattern_from_str("b*").remove_shortest_suffix("testbaaaaaba".to_string()),
            "testbaaaaa"
        )
    }

    #[test]
    fn filename_pattern_matches_simple_components_in_path() {
        let pattern = filename_pattern_from_str("/path/to/file");
        assert!(pattern.matches_all(1, &cstring_from_str("path")));
        assert!(pattern.matches_all(2, &cstring_from_str("to")));
        assert!(pattern.matches_all(3, &cstring_from_str("file")));
    }

    #[test]
    fn period_at_the_start_is_only_matched_explicitly() {
        let pattern = filename_pattern_from_str("*test");
        assert!(!pattern.matches_all(1, &cstring_from_str(".test")));
        assert!(pattern.matches_all(1, &cstring_from_str("atest")));

        let pattern = filename_pattern_from_str("/dir/*file");
        assert!(!pattern.matches_all(2, &cstring_from_str(".file")));

        let pattern = filename_pattern_from_str(".test");
        assert!(pattern.matches_all(1, &cstring_from_str(".test")));
    }

    #[test]
    fn period_at_the_start_is_not_matched_by_bracket_expression_with_multiple_chars() {
        // the standard leaves this case to the implementation, here we follow what bash does
        let pattern = filename_pattern_from_str("[.abc]*");
        assert!(!pattern.matches_all(1, &cstring_from_str(".a")));
    }

    #[test]
    fn match_history_pattern() {
        let pattern = HistoryPattern::new("arg".to_string()).unwrap();
        assert!(pattern.matches("cmd arg"));

        let pattern = HistoryPattern::new("^cmd".to_string()).unwrap();
        assert!(pattern.matches("cmd arg"));

        let pattern = HistoryPattern::new("^arg".to_string()).unwrap();
        assert!(!pattern.matches("cmd arg"));
    }

    // POSIX 2.6.2: when the pattern does not match, the parameter's value is
    // used unchanged. Verified against dash 0.5.12 and bash 5.2.21, which
    // agree on every case below.

    #[test]
    fn remove_affix_that_does_not_match_returns_the_subject_unchanged() {
        // `remove_shortest_suffix` used to compute the no-match sentinel from
        // the length *including* a pushed NUL, then `drain` past the end and
        // abort the process; the other three each got it wrong differently.
        assert_eq!(
            pattern_from_str("z").remove_shortest_suffix("abc".into()),
            "abc"
        );
        assert_eq!(
            pattern_from_str("z").remove_largest_suffix("abc".into()),
            "abc"
        );
        assert_eq!(
            pattern_from_str("z").remove_shortest_prefix("abc".into()),
            "abc"
        );
        assert_eq!(
            pattern_from_str("z").remove_largest_prefix("abc".into()),
            "abc"
        );
    }

    #[test]
    fn remove_affix_matches_are_anchored() {
        // "b" occurs inside "abc" but is neither a prefix nor a suffix of it;
        // the removal loops used the *unanchored* `Regex::matches`.
        assert_eq!(
            pattern_from_str("b").remove_shortest_suffix("abc".into()),
            "abc"
        );
        assert_eq!(
            pattern_from_str("b").remove_largest_suffix("abc".into()),
            "abc"
        );
        assert_eq!(
            pattern_from_str("b").remove_shortest_prefix("abc".into()),
            "abc"
        );
        assert_eq!(
            pattern_from_str("b").remove_largest_prefix("abc".into()),
            "abc"
        );
        // ... while a genuine one-character affix is removed.
        assert_eq!(
            pattern_from_str("c").remove_shortest_suffix("abc".into()),
            "ab"
        );
        assert_eq!(
            pattern_from_str("a").remove_shortest_prefix("abc".into()),
            "bc"
        );
    }

    #[test]
    fn remove_affix_can_consume_the_whole_subject() {
        // The loops ran `1..len - 1`, so a pattern matching the entire value
        // was never tested and never removed.
        assert_eq!(
            pattern_from_str("abc").remove_shortest_prefix("abc".into()),
            ""
        );
        assert_eq!(
            pattern_from_str("abc").remove_largest_prefix("abc".into()),
            ""
        );
        assert_eq!(
            pattern_from_str("abc").remove_shortest_suffix("abc".into()),
            ""
        );
        assert_eq!(
            pattern_from_str("abc").remove_largest_suffix("abc".into()),
            ""
        );
    }

    #[test]
    fn remove_affix_with_asterisk_spans_empty_and_whole() {
        // `*` matches the empty affix (shortest) and the whole value (largest).
        assert_eq!(
            pattern_from_str("*").remove_shortest_prefix("abc".into()),
            "abc"
        );
        assert_eq!(
            pattern_from_str("*").remove_largest_prefix("abc".into()),
            ""
        );
        assert_eq!(
            pattern_from_str("*").remove_shortest_suffix("abc".into()),
            "abc"
        );
        assert_eq!(
            pattern_from_str("*").remove_largest_suffix("abc".into()),
            ""
        );
    }

    #[test]
    fn remove_affix_does_not_split_a_multibyte_character() {
        // The two "shortest" variants walked *byte* indices and sliced at them,
        // so a multi-byte character could be cut in half and the trailing
        // `String::from_utf8(..).expect(..)` aborted the process.
        assert_eq!(
            pattern_from_str("z").remove_shortest_suffix("héllo".into()),
            "héllo"
        );
        assert_eq!(
            pattern_from_str("z").remove_shortest_prefix("héllo".into()),
            "héllo"
        );
        assert_eq!(
            pattern_from_str("?").remove_shortest_prefix("héllo".into()),
            "éllo"
        );
        assert_eq!(
            pattern_from_str("o").remove_shortest_suffix("héllo".into()),
            "héll"
        );
    }

    #[test]
    fn remove_affix_with_interior_nul_does_not_panic() {
        // The two "largest" variants used `CString::new(s).expect(..)`.
        assert_eq!(
            pattern_from_str("z").remove_largest_prefix("a\0b".into()),
            "a\0b"
        );
        assert_eq!(
            pattern_from_str("z").remove_largest_suffix("a\0b".into()),
            "a\0b"
        );
    }

    #[test]
    fn filename_pattern_matches_all_accepts_an_empty_component() {
        // `matches_all` indexed `s.to_bytes()[0]` with no emptiness guard.
        let pattern = filename_pattern_from_str("*");
        assert!(pattern.matches_all(1, &cstring_from_str("")));
    }
}
