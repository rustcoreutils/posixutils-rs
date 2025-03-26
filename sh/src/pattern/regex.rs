use crate::pattern::parse::{BracketExpression, BracketItem, PatternItem, RangeEndpoint};
use core::fmt;
use nix::libc;
use std::ffi::{CStr, CString};
use std::fmt::{Formatter, Write};
use std::ptr;

fn regex_compilation_result(
    status_integer: libc::c_int,
    regex: &libc::regex_t,
) -> Result<(), String> {
    if status_integer != 0 {
        let mut error_buffer = vec![b'\0'; 128];
        unsafe {
            libc::regerror(
                status_integer,
                ptr::from_ref(regex),
                error_buffer.as_mut_ptr() as *mut libc::c_char,
                128,
            )
        };
        let error = CStr::from_bytes_until_nul(&error_buffer)
            .expect("error message returned from `libc::regerror` is an invalid CString");
        Err(error.to_string_lossy().into_owned())
    } else {
        Ok(())
    }
}

// TODO: the implementation is copied from awk, maybe we should consider putting it in a shared crate
pub struct Regex {
    raw_regex: libc::regex_t,
    regex_string: CString,
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct RegexMatch {
    pub start: usize,
    pub end: usize,
}

pub struct MatchIter<'a> {
    string: &'a CStr,
    next_start: usize,
    regex: &'a Regex,
}

impl Iterator for MatchIter<'_> {
    type Item = RegexMatch;
    fn next(&mut self) -> Option<Self::Item> {
        if self.next_start >= self.string.to_bytes().len() {
            return None;
        }
        let mut match_range = libc::regmatch_t {
            rm_so: -1,
            rm_eo: -1,
        };
        let exec_status = unsafe {
            libc::regexec(
                ptr::from_ref(&self.regex.raw_regex),
                self.string.as_ptr().add(self.next_start),
                1,
                ptr::from_mut(&mut match_range),
                0,
            )
        };
        if exec_status == libc::REG_NOMATCH {
            return None;
        }
        let result = RegexMatch {
            start: self.next_start + match_range.rm_so as usize,
            end: self.next_start + match_range.rm_eo as usize,
        };
        self.next_start += match_range.rm_eo as usize;
        Some(result)
    }
}

impl Regex {
    pub fn new(regex: CString) -> Result<Self, String> {
        let mut raw = unsafe { std::mem::zeroed::<libc::regex_t>() };
        // difference from awk implementation: use 0 instead of REG_EXTENDED
        let compilation_status =
            unsafe { libc::regcomp(ptr::from_mut(&mut raw), regex.as_ptr(), 0) };
        regex_compilation_result(compilation_status, &raw)?;
        Ok(Self {
            raw_regex: raw,
            regex_string: regex,
        })
    }

    pub fn match_locations<'a>(&'a self, string: &'a CStr) -> MatchIter<'a> {
        MatchIter {
            next_start: 0,
            regex: self,
            string,
        }
    }

    pub fn matches(&self, string: &CStr) -> bool {
        self.match_locations(string).next().is_some()
    }
}

impl Drop for Regex {
    fn drop(&mut self) {
        unsafe {
            libc::regfree(ptr::from_mut(&mut self.raw_regex));
        }
    }
}

impl fmt::Display for Regex {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.regex_string.to_str().unwrap())
    }
}

impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        self.regex_string == other.regex_string
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
    let regex_cstr = CString::new(regex_str).expect("pattern cannot contain null characters");
    Regex::new(regex_cstr)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pattern_to_regex_string(parsed_pattern: &[PatternItem]) -> String {
        parsed_pattern_to_regex(&parsed_pattern)
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
