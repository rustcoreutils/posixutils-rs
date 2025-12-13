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
        Ok(Pattern { tokens })
    }

    /// Check if a string matches this pattern
    pub fn matches(&self, text: &str) -> bool {
        match_tokens(&self.tokens, text)
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

/// Match tokens against text
fn match_tokens(tokens: &[Token], text: &str) -> bool {
    match_tokens_at(tokens, text, 0)
}

/// Recursive matching with position tracking
fn match_tokens_at(tokens: &[Token], text: &str, pos: usize) -> bool {
    if tokens.is_empty() {
        return pos == text.len();
    }

    let text_chars: Vec<char> = text.chars().collect();

    match &tokens[0] {
        Token::Char(c) => {
            if pos < text_chars.len() && text_chars[pos] == *c {
                match_tokens_at(&tokens[1..], text, pos + 1)
            } else {
                false
            }
        }
        Token::Any => {
            // ? matches any single character except /
            if pos < text_chars.len() && text_chars[pos] != '/' {
                match_tokens_at(&tokens[1..], text, pos + 1)
            } else {
                false
            }
        }
        Token::Star => {
            // * matches any sequence except /
            match_star(&tokens[1..], text, pos)
        }
        Token::Class(class) => {
            if pos < text_chars.len() && class_matches(class, text_chars[pos]) {
                match_tokens_at(&tokens[1..], text, pos + 1)
            } else {
                false
            }
        }
    }
}

/// Handle star matching (greedy with backtracking)
/// Star matches any sequence except /
#[allow(clippy::needless_range_loop)]
fn match_star(remaining_tokens: &[Token], text: &str, start_pos: usize) -> bool {
    let text_chars: Vec<char> = text.chars().collect();
    let text_len = text_chars.len();

    // Try matching zero or more characters (but not /)
    for pos in start_pos..=text_len {
        if match_tokens_at(remaining_tokens, text, pos) {
            return true;
        }

        // Don't try to extend past a slash
        if pos < text_len && text_chars[pos] == '/' {
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
}
