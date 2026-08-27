//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::pattern::parse::{parse_pattern, ParsedPattern, PatternItem};
use crate::shstr::ShString;
use crate::wordexp::expanded_word::ExpandedWord;

mod matcher;
mod parse;

pub struct Pattern {
    pattern_string: ShString,
    items: ParsedPattern,
}

impl Pattern {
    pub fn new(word: &ExpandedWord) -> Result<Self, String> {
        let items = parse_pattern(word, false)?;
        matcher::validate(&items)?;
        Ok(Self {
            pattern_string: word.to_sh_string(),
            items,
        })
    }

    /// A shell pattern matches the ENTIRE string (POSIX 2.14), which is what
    /// `case` needs and what each affix candidate is tested against.
    pub fn matches(&self, s: &[u8]) -> bool {
        matcher::matches_whole(&self.items, s)
    }

    /// Longest prefix the pattern matches entirely, removed. On no match the
    /// value is returned unchanged.
    pub fn remove_largest_prefix(&self, s: String) -> String {
        match matcher::longest_prefix_end(&self.items, s.as_bytes()) {
            Some(end) => s[end..].to_string(),
            None => s,
        }
    }

    /// Shortest prefix the pattern matches entirely, removed.
    pub fn remove_shortest_prefix(&self, s: String) -> String {
        match matcher::shortest_prefix_end(&self.items, s.as_bytes()) {
            Some(end) => s[end..].to_string(),
            None => s,
        }
    }

    /// Longest suffix the pattern matches entirely, removed.
    pub fn remove_largest_suffix(&self, s: String) -> String {
        match matcher::longest_suffix_start(&self.items, s.as_bytes()) {
            Some(begin) => s[..begin].to_string(),
            None => s,
        }
    }

    /// Shortest suffix the pattern matches entirely, removed.
    pub fn remove_shortest_suffix(&self, s: String) -> String {
        match matcher::shortest_suffix_start(&self.items, s.as_bytes()) {
            Some(begin) => s[..begin].to_string(),
            None => s,
        }
    }
}

impl From<Pattern> for ShString {
    fn from(value: Pattern) -> Self {
        value.pattern_string
    }
}

struct FilenamePatternPart {
    items: ParsedPattern,
    starts_with_dot: bool,
}

pub struct FilenamePattern {
    path_parts: Vec<FilenamePatternPart>,
    pattern_string: ShString,
    /// A pattern ending in `/` matches directories only, and the `/` is part of
    /// the result: `*/` lists subdirectories as `sub/`, not `sub`.
    has_trailing_slash: bool,
}

impl FilenamePattern {
    pub fn new(word: &ExpandedWord) -> Result<Self, String> {
        let pattern_string = word.to_sh_string();
        let parsed_pattern = parse_pattern(word, true)?;
        // Splitting on `/` drops the empty component a trailing slash leaves
        // behind, so the distinction has to be recorded before that happens.
        let has_trailing_slash = parsed_pattern.last() == Some(&PatternItem::Char('/'));
        let mut path_parts = Vec::new();

        parsed_pattern
            .split(|item| *item == PatternItem::Char('/'))
            .filter(|items| !items.is_empty())
            .try_for_each(|items| {
                let starts_with_dot = items.starts_with(&[PatternItem::Char('.')]);
                matcher::validate(items)?;
                path_parts.push(FilenamePatternPart {
                    items: items.to_vec(),
                    starts_with_dot,
                });
                Ok::<(), String>(())
            })?;

        Ok(Self {
            path_parts,
            pattern_string,
            has_trailing_slash,
        })
    }

    /// # Panics
    /// panics if `depth` is smaller than 1 or bigger than `component_count`
    pub fn matches_all(&self, depth: usize, s: &[u8]) -> bool {
        assert!(
            depth > 0 && depth <= self.component_count(),
            "invalid depth"
        );
        let component_index = depth - 1;
        if s.first() == Some(&b'.') && !self.path_parts[component_index].starts_with_dot {
            // dot at the start is only matched explicitly
            return false;
        }
        matcher::matches_whole(&self.path_parts[component_index].items, s)
    }

    /// Returns number of components in the path
    /// If it returns 0 then the pattern is just a directory (root if it starts
    /// with '/', the current directory otherwise)
    pub fn component_count(&self) -> usize {
        self.path_parts.len()
    }

    pub fn is_absolute(&self) -> bool {
        self.pattern_string.starts_with(b"/")
    }

    /// True when the pattern ends in `/`, so only directories match its last
    /// component and the slash belongs to the result.
    pub fn matches_directories_only(&self) -> bool {
        self.has_trailing_slash
    }
}

impl From<FilenamePattern> for ShString {
    fn from(value: FilenamePattern) -> Self {
        value.pattern_string
    }
}

impl TryFrom<ShString> for FilenamePattern {
    type Error = String;

    fn try_from(value: ShString) -> Result<Self, Self::Error> {
        let value = ExpandedWord::unquoted_literal(value);
        FilenamePattern::new(&value)
    }
}

pub struct HistoryPattern {
    items: ParsedPattern,
    /// Kept for `Debug`/introspection; the anchoring is baked into `items`.
    #[allow(dead_code)]
    match_only_at_line_start: bool,
}

impl HistoryPattern {
    pub fn new(pattern: String) -> Result<Self, String> {
        let parsed = parse_pattern(&ExpandedWord::unquoted_literal(pattern), false)?;
        let match_only_at_line_start = parsed.first() == Some(&PatternItem::Char('^'));
        // History search asks whether the command *contains* the pattern, which
        // the whole-match engine expresses by padding with `*`. A leading `^`
        // anchors it to the start, so only the trailing pad applies.
        let mut items = Vec::with_capacity(parsed.len() + 2);
        if !match_only_at_line_start {
            items.push(PatternItem::Asterisk);
        }
        items.extend(
            parsed
                .into_iter()
                .skip(usize::from(match_only_at_line_start)),
        );
        items.push(PatternItem::Asterisk);
        matcher::validate(&items)?;
        Ok(Self {
            items,
            match_only_at_line_start,
        })
    }

    pub fn matches(&self, s: &str) -> bool {
        matcher::matches_whole(&self.items, s.as_bytes())
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
        assert!(pattern.matches_all(1, b"path"));
        assert!(pattern.matches_all(2, b"to"));
        assert!(pattern.matches_all(3, b"file"));
    }

    #[test]
    fn period_at_the_start_is_only_matched_explicitly() {
        let pattern = filename_pattern_from_str("*test");
        assert!(!pattern.matches_all(1, b".test"));
        assert!(pattern.matches_all(1, b"atest"));

        let pattern = filename_pattern_from_str("/dir/*file");
        assert!(!pattern.matches_all(2, b".file"));

        let pattern = filename_pattern_from_str(".test");
        assert!(pattern.matches_all(1, b".test"));
    }

    #[test]
    fn period_at_the_start_is_not_matched_by_bracket_expression_with_multiple_chars() {
        // the standard leaves this case to the implementation, here we follow what bash does
        let pattern = filename_pattern_from_str("[.abc]*");
        assert!(!pattern.matches_all(1, b".a"));
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
        assert!(pattern.matches_all(1, b""));
    }
}
