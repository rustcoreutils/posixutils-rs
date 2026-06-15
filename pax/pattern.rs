//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX-style pattern matching for pax
//!
//! Implements glob patterns as specified in POSIX:
//! - `*` matches any string (including empty)
//! - `?` matches any single character
//! - `[...]` matches a character class
//! - `[!...]` matches complement of character class

use crate::error::{PaxError, PaxResult};

/// A compiled pattern for matching
#[derive(Debug, Clone)]
pub struct Pattern {
    tokens: Vec<Token>,
    /// The original pattern string, retained for "not found" diagnostics when a
    /// pattern operand matches no archive member.
    pub source: String,
}

#[derive(Debug, Clone)]
enum Token {
    /// Literal character
    Char(char),
    /// Match any single character (?)
    Any,
    /// Match any sequence (*)
    Star,
    /// Character class [...]
    Class(CharClass),
}

#[derive(Debug, Clone)]
struct CharClass {
    negated: bool,
    ranges: Vec<CharRange>,
}

#[derive(Debug, Clone)]
enum CharRange {
    Single(char),
    Range(char, char),
}

impl Pattern {
    /// Compile a pattern string
    pub fn new(pattern: &str) -> PaxResult<Self> {
        let tokens = parse_pattern(pattern)?;
        Ok(Pattern {
            tokens,
            source: pattern.to_string(),
        })
    }

    /// Check if a string matches this pattern
    pub fn matches(&self, text: &str) -> bool {
        // Pre-collect chars once to avoid repeated allocations in recursive calls
        let text_chars: Vec<char> = text.chars().collect();
        match_tokens_with_chars(&self.tokens, &text_chars, 0)
    }
}

/// Parse a pattern string into tokens
fn parse_pattern(pattern: &str) -> PaxResult<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut chars = pattern.chars().peekable();

    while let Some(c) = chars.next() {
        let token = match c {
            '*' => Token::Star,
            '?' => Token::Any,
            '[' => parse_char_class(&mut chars)?,
            '\\' => {
                // Escape next character
                match chars.next() {
                    Some(escaped) => Token::Char(escaped),
                    None => Token::Char('\\'),
                }
            }
            _ => Token::Char(c),
        };
        tokens.push(token);
    }

    Ok(tokens)
}

/// Parse a character class [...]
fn parse_char_class(chars: &mut std::iter::Peekable<std::str::Chars>) -> PaxResult<Token> {
    let mut negated = false;
    let mut ranges = Vec::new();

    // Check for negation
    if chars.peek() == Some(&'!') || chars.peek() == Some(&'^') {
        negated = true;
        chars.next();
    }

    // Handle ] as first character (literal)
    if chars.peek() == Some(&']') {
        ranges.push(CharRange::Single(']'));
        chars.next();
    }

    while let Some(c) = chars.next() {
        if c == ']' {
            return Ok(Token::Class(CharClass { negated, ranges }));
        }

        // Check for range
        if chars.peek() == Some(&'-') {
            chars.next(); // consume -
            if let Some(&end) = chars.peek() {
                if end == ']' {
                    // - at end is literal
                    ranges.push(CharRange::Single(c));
                    ranges.push(CharRange::Single('-'));
                } else {
                    chars.next();
                    ranges.push(CharRange::Range(c, end));
                }
            } else {
                ranges.push(CharRange::Single(c));
                ranges.push(CharRange::Single('-'));
            }
        } else {
            ranges.push(CharRange::Single(c));
        }
    }

    // Unclosed bracket
    Err(PaxError::PatternError("unclosed bracket".to_string()))
}

/// Recursive matching with position tracking using pre-collected chars
fn match_tokens_with_chars(tokens: &[Token], text_chars: &[char], pos: usize) -> bool {
    if tokens.is_empty() {
        return pos == text_chars.len();
    }

    match &tokens[0] {
        Token::Char(c) => {
            if pos < text_chars.len() && text_chars[pos] == *c {
                match_tokens_with_chars(&tokens[1..], text_chars, pos + 1)
            } else {
                false
            }
        }
        Token::Any => {
            // ? matches any single character except /, and may not match a
            // leading '.' (start of string or after '/'), per FNM_PERIOD.
            if pos < text_chars.len()
                && text_chars[pos] != '/'
                && !is_leading_period(text_chars, pos)
            {
                match_tokens_with_chars(&tokens[1..], text_chars, pos + 1)
            } else {
                false
            }
        }
        Token::Star => {
            // * matches any sequence except /
            match_star_with_chars(&tokens[1..], text_chars, pos)
        }
        Token::Class(class) => {
            // A bracket expression also may not match a leading '.'.
            if pos < text_chars.len()
                && class_matches(class, text_chars[pos])
                && !is_leading_period(text_chars, pos)
            {
                match_tokens_with_chars(&tokens[1..], text_chars, pos + 1)
            } else {
                false
            }
        }
    }
}

/// Whether the character at `pos` is a '.' in a leading position — at the start
/// of the string or immediately after a '/'. Such a '.' must be matched by an
/// explicit literal, never by `*`, `?`, or a bracket expression (FNM_PERIOD).
fn is_leading_period(text_chars: &[char], pos: usize) -> bool {
    text_chars[pos] == '.' && (pos == 0 || text_chars[pos - 1] == '/')
}

/// Handle star matching (greedy with backtracking) using pre-collected chars
/// Star matches any sequence except /
fn match_star_with_chars(
    remaining_tokens: &[Token],
    text_chars: &[char],
    start_pos: usize,
) -> bool {
    let text_len = text_chars.len();

    // Try matching zero or more characters (but not /)
    for pos in start_pos..=text_len {
        if match_tokens_with_chars(remaining_tokens, text_chars, pos) {
            return true;
        }

        // Don't try to extend past a slash
        if pos < text_len && text_chars[pos] == '/' {
            break;
        }

        // Don't let `*` consume a leading '.' (FNM_PERIOD); it may match empty
        // before such a '.', but the '.' must be matched by an explicit literal.
        if pos < text_len && is_leading_period(text_chars, pos) {
            break;
        }
    }
    false
}

/// Check if a character matches a character class
fn class_matches(class: &CharClass, c: char) -> bool {
    let mut matched = false;

    for range in &class.ranges {
        match range {
            CharRange::Single(ch) => {
                if c == *ch {
                    matched = true;
                    break;
                }
            }
            CharRange::Range(start, end) => {
                if c >= *start && c <= *end {
                    matched = true;
                    break;
                }
            }
        }
    }

    if class.negated {
        !matched
    } else {
        matched
    }
}

/// Check if any pattern matches the given path
pub fn matches_any(patterns: &[Pattern], path: &str) -> bool {
    if patterns.is_empty() {
        return true; // No patterns means match all
    }
    patterns.iter().any(|p| p.matches(path))
}

/// Find the index of the first pattern that matches the given path
/// Returns None if no pattern matches, or if patterns is empty (which means "match all")
pub fn find_matching_pattern(patterns: &[Pattern], path: &str) -> Option<usize> {
    if patterns.is_empty() {
        return None; // No patterns means match all - return None to indicate no specific pattern
    }
    patterns.iter().position(|p| p.matches(path))
}

/// Find the first pattern that matches `path`, or — when `expand_subtree` is set
/// — that matches one of its ancestor directory components.
///
/// Per POSIX, a pattern that selects a directory member also selects the entire
/// file hierarchy rooted at that directory; `-d` (`expand_subtree == false`)
/// restricts the match to the directory itself.
pub fn find_matching_pattern_subtree(
    patterns: &[Pattern],
    path: &str,
    expand_subtree: bool,
) -> Option<usize> {
    if let Some(idx) = find_matching_pattern(patterns, path) {
        return Some(idx);
    }
    // Directory members are stored with a trailing slash (e.g. "dir/"); a pattern
    // like "dir" should still match the directory itself.
    let trimmed = path.strip_suffix('/').unwrap_or(path);
    if trimmed != path {
        if let Some(idx) = find_matching_pattern(patterns, trimmed) {
            return Some(idx);
        }
    }
    if expand_subtree {
        let mut ancestor = trimmed;
        while let Some(slash) = ancestor.rfind('/') {
            ancestor = &ancestor[..slash];
            if let Some(idx) = find_matching_pattern(patterns, ancestor) {
                return Some(idx);
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal() {
        let p = Pattern::new("hello").unwrap();
        assert!(p.matches("hello"));
        assert!(!p.matches("hello2"));
        assert!(!p.matches("hell"));
    }

    #[test]
    fn test_star() {
        let p = Pattern::new("*.txt").unwrap();
        assert!(p.matches(".txt"));
        assert!(p.matches("file.txt"));
        assert!(p.matches("long_filename.txt"));
        assert!(!p.matches("file.txt.bak"));
    }

    #[test]
    fn test_question() {
        let p = Pattern::new("file?.txt").unwrap();
        assert!(p.matches("file1.txt"));
        assert!(p.matches("fileA.txt"));
        assert!(!p.matches("file.txt"));
        assert!(!p.matches("file12.txt"));
    }

    #[test]
    fn test_char_class() {
        let p = Pattern::new("file[123].txt").unwrap();
        assert!(p.matches("file1.txt"));
        assert!(p.matches("file2.txt"));
        assert!(p.matches("file3.txt"));
        assert!(!p.matches("file4.txt"));
    }

    #[test]
    fn test_char_range() {
        let p = Pattern::new("file[a-z].txt").unwrap();
        assert!(p.matches("filea.txt"));
        assert!(p.matches("filem.txt"));
        assert!(p.matches("filez.txt"));
        assert!(!p.matches("fileA.txt"));
        assert!(!p.matches("file1.txt"));
    }

    #[test]
    fn test_negated_class() {
        let p = Pattern::new("file[!0-9].txt").unwrap();
        assert!(p.matches("filea.txt"));
        assert!(!p.matches("file1.txt"));
    }

    #[test]
    fn test_star_middle() {
        let p = Pattern::new("src/*.rs").unwrap();
        assert!(p.matches("src/main.rs"));
        assert!(p.matches("src/lib.rs"));
        assert!(!p.matches("src/sub/mod.rs"));
    }

    #[test]
    fn test_multiple_stars() {
        let p = Pattern::new("*/*").unwrap();
        assert!(p.matches("a/b"));
        assert!(p.matches("src/main.rs"));
        assert!(!p.matches("file.txt"));
    }

    #[test]
    fn test_escape() {
        let p = Pattern::new(r"file\*.txt").unwrap();
        assert!(p.matches("file*.txt"));
        assert!(!p.matches("file1.txt"));
    }

    #[test]
    fn test_leading_period_not_matched_by_wildcards() {
        // A leading '.' must be matched explicitly, never by * ? or [...].
        assert!(!Pattern::new("*").unwrap().matches(".hidden"));
        assert!(!Pattern::new("?hidden").unwrap().matches(".hidden"));
        assert!(!Pattern::new("[.]hidden").unwrap().matches(".hidden"));
        // An explicit leading dot does match.
        assert!(Pattern::new(".*").unwrap().matches(".hidden"));
        assert!(Pattern::new(".hidden").unwrap().matches(".hidden"));
        // The same rule applies just after a '/'.
        assert!(!Pattern::new("dir/*").unwrap().matches("dir/.hidden"));
        assert!(Pattern::new("dir/.*").unwrap().matches("dir/.hidden"));
        // A non-leading dot is matched normally.
        assert!(Pattern::new("a*").unwrap().matches("a.b"));
    }

    #[test]
    fn test_find_matching_pattern_subtree() {
        let patterns = vec![Pattern::new("dir").unwrap()];

        // Without expansion, only the directory itself matches (stored "dir/").
        assert_eq!(
            find_matching_pattern_subtree(&patterns, "dir/", false),
            Some(0)
        );
        assert_eq!(
            find_matching_pattern_subtree(&patterns, "dir/sub/f", false),
            None
        );

        // With expansion, the whole subtree matches via an ancestor.
        assert_eq!(
            find_matching_pattern_subtree(&patterns, "dir/sub/f", true),
            Some(0)
        );
        // An unrelated sibling never matches.
        assert_eq!(
            find_matching_pattern_subtree(&patterns, "dirfoo", true),
            None
        );
    }
}
