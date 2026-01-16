//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::pattern::parse::{BracketExpression, BracketItem, PatternItem, RangeEndpoint};
use core::fmt;
use plib::regex::{Match, Regex as PlibRegex, RegexFlags};
use std::ffi::CStr;
use std::fmt::{Formatter, Write};

/// A regex wrapper that provides CStr-compatible API for shell pattern matching.
/// Internally uses plib::regex for POSIX BRE support.
pub struct Regex {
    inner: PlibRegex,
    pattern_string: String,
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
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

pub struct MatchIter<'a> {
    inner: plib::regex::MatchIter<'a, 'a>,
}

impl Iterator for MatchIter<'_> {
    type Item = RegexMatch;
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(RegexMatch::from)
    }
}

impl Regex {
    pub fn new(pattern: &str) -> Result<Self, String> {
        let inner = PlibRegex::new(pattern, RegexFlags::bre()).map_err(|e| e.to_string())?;
        Ok(Self {
            inner,
            pattern_string: pattern.to_string(),
        })
    }

    pub fn match_locations<'a>(&'a self, string: &'a CStr) -> MatchIter<'a> {
        // Convert CStr to &str - shell strings should always be valid UTF-8
        let s = string.to_str().unwrap_or("");
        MatchIter {
            inner: self.inner.find_iter(s),
        }
    }

    pub fn matches(&self, string: &CStr) -> bool {
        let s = string.to_str().unwrap_or("");
        self.inner.is_match(s)
    }
}

impl fmt::Display for Regex {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.pattern_string)
    }
}

impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        self.pattern_string == other.pattern_string
    }
}

fn is_special_character(c: char) -> bool {
    match c {
        // BRE special characters
        '.' | '[' | '\\' | '*' | '^' | '$' => true,
        _ => false,
    }
}

fn push_char_literal(c: char, string: &mut String) {
    match c {
        c if is_special_character(c) => {
            write!(string, "\\{c}").unwrap();
        }
        _ => string.push(c),
    }
}

fn push_collating_symbol(symbol: &str, string: &mut String) {
    string.push_str("[.");
    string.push_str(symbol);
    string.push_str(".]")
}

fn push_range_endpoint(endpoint: &RangeEndpoint, string: &mut String) {
    match endpoint {
        RangeEndpoint::Char(c) => {
            string.push(*c);
        }
        RangeEndpoint::CollatingSymbol(symbol) => {
            push_collating_symbol(symbol, string);
        }
    }
}

fn push_bracket_expression(expr: &BracketExpression, string: &mut String) {
    string.push('[');
    if !expr.matching {
        string.push('^');
    }
    for item in &expr.items {
        match item {
            BracketItem::Char(c) => push_char_literal(*c, string),
            BracketItem::CharacterClass(class) => {
                string.push_str("[:");
                string.push_str(class);
                string.push_str(":]");
            }
            BracketItem::CollatingSymbol(symbol) => push_collating_symbol(symbol, string),
            BracketItem::EquivalenceClass(class) => {
                string.push_str("[=");
                string.push_str(class);
                string.push_str("=]");
            }
            BracketItem::RangeExpression(start, end) => {
                push_range_endpoint(start, string);
                string.push('-');
                push_range_endpoint(end, string);
            }
        }
    }
    string.push(']');
}

pub fn parsed_pattern_to_regex(parsed_pattern: &[PatternItem]) -> Result<Regex, String> {
    let mut regex_str = String::new();
    for item in parsed_pattern {
        match item {
            PatternItem::Char(c) => push_char_literal(*c, &mut regex_str),
            PatternItem::QuestionMark => regex_str.push('.'),
            PatternItem::Asterisk => regex_str.push_str(".*"),
            PatternItem::BracketExpression(expr) => push_bracket_expression(expr, &mut regex_str),
        }
    }
    Regex::new(&regex_str)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pattern_to_regex_string(parsed_pattern: &[PatternItem]) -> String {
        parsed_pattern_to_regex(parsed_pattern)
            .expect("failed to convert pattern")
            .to_string()
    }

    #[test]
    fn convert_empty_pattern() {
        let regex = pattern_to_regex_string(&[]);
        assert_eq!(regex, "");
    }

    #[test]
    fn convert_single_char_pattern() {
        let regex = pattern_to_regex_string(&[PatternItem::Char('a')]);
        assert_eq!(regex, "a");
    }

    #[test]
    fn convert_multiple_char_pattern() {
        let regex = pattern_to_regex_string(&[
            PatternItem::Char('a'),
            PatternItem::Char('b'),
            PatternItem::Char('c'),
        ]);
        assert_eq!(regex, "abc");
    }

    #[test]
    fn convert_question_mark_pattern() {
        let regex = pattern_to_regex_string(&[
            PatternItem::Char('a'),
            PatternItem::QuestionMark,
            PatternItem::Char('b'),
        ]);
        assert_eq!(regex, "a.b");
    }

    #[test]
    fn convert_asterisk_pattern() {
        let regex = pattern_to_regex_string(&[
            PatternItem::Char('a'),
            PatternItem::Asterisk,
            PatternItem::Char('b'),
        ]);
        assert_eq!(regex, "a.*b");
    }

    #[test]
    fn convert_bracket_with_single_character() {
        let regex = pattern_to_regex_string(&[PatternItem::BracketExpression(BracketExpression {
            matching: true,
            items: vec![BracketItem::Char('a')],
        })]);
        assert_eq!(regex, "[a]");
    }

    #[test]
    fn convert_bracket_with_multiple_characters() {
        let regex = pattern_to_regex_string(&[PatternItem::BracketExpression(BracketExpression {
            matching: true,
            items: vec![
                BracketItem::Char('a'),
                BracketItem::Char('b'),
                BracketItem::Char('c'),
            ],
        })]);
        assert_eq!(regex, "[abc]");
    }

    #[test]
    fn convert_bracket_with_character_class() {
        let regex = pattern_to_regex_string(&[PatternItem::BracketExpression(BracketExpression {
            matching: true,
            items: vec![BracketItem::CharacterClass("digit".to_string())],
        })]);
        assert_eq!(regex, "[[:digit:]]");
    }

    #[test]
    fn convert_bracket_expression_with_collating_symbol() {
        let regex = pattern_to_regex_string(&[PatternItem::BracketExpression(BracketExpression {
            matching: true,
            items: vec![BracketItem::CollatingSymbol("a".to_string())],
        })]);
        assert_eq!(regex, "[[.a.]]");
    }

    #[test]
    fn convert_bracket_expression_with_range() {
        let regex = pattern_to_regex_string(&[PatternItem::BracketExpression(BracketExpression {
            matching: true,
            items: vec![BracketItem::RangeExpression(
                RangeEndpoint::Char('a'),
                RangeEndpoint::Char('z'),
            )],
        })]);
        assert_eq!(regex, "[a-z]");
    }

    #[test]
    fn convert_non_matching_bracket_expression_with_single_character() {
        let regex = pattern_to_regex_string(&[PatternItem::BracketExpression(BracketExpression {
            matching: false,
            items: vec![BracketItem::Char('a')],
        })]);
        assert_eq!(regex, "[^a]");
    }

    #[test]
    fn convert_non_matching_bracket_expression_with_multiple_characters() {
        let regex = pattern_to_regex_string(&[PatternItem::BracketExpression(BracketExpression {
            matching: false,
            items: vec![
                BracketItem::Char('a'),
                BracketItem::Char('b'),
                BracketItem::Char('c'),
            ],
        })]);
        assert_eq!(regex, "[^abc]");
    }

    #[test]
    fn convert_non_matching_bracket_expression_with_character_class() {
        let regex = pattern_to_regex_string(&[PatternItem::BracketExpression(BracketExpression {
            matching: false,
            items: vec![BracketItem::CharacterClass("digit".to_string())],
        })]);
        assert_eq!(regex, "[^[:digit:]]");
    }

    #[test]
    fn convert_bracket_expression_with_characters_and_character_class() {
        let regex = pattern_to_regex_string(&[PatternItem::BracketExpression(BracketExpression {
            matching: true,
            items: vec![
                BracketItem::Char('a'),
                BracketItem::CharacterClass("digit".to_string()),
                BracketItem::Char('b'),
            ],
        })]);
        assert_eq!(regex, "[a[:digit:]b]");
    }

    #[test]
    fn convert_non_matching_bracket_expression_with_characters_character_class() {
        let regex = pattern_to_regex_string(&[PatternItem::BracketExpression(BracketExpression {
            matching: false,
            items: vec![
                BracketItem::Char('a'),
                BracketItem::CharacterClass("digit".to_string()),
                BracketItem::Char('b'),
            ],
        })]);
        assert_eq!(regex, "[^a[:digit:]b]");
    }

    #[test]
    fn special_bre_characters_are_escaped() {
        let regex = pattern_to_regex_string(&[
            PatternItem::Char('.'),
            PatternItem::Char('['),
            PatternItem::Char('\\'),
            PatternItem::Char('*'),
            PatternItem::Char('^'),
            PatternItem::Char('$'),
        ]);
        assert_eq!(regex, "\\.\\[\\\\\\*\\^\\$");
    }
}
