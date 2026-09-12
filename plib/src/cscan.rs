//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Incremental lexical scanner for embedded C source.
//!
//! Utilities that copy C code through -- `yacc` actions, `lex` actions -- have
//! to know which characters are *code* and which sit inside a string literal,
//! a character literal, or a comment. A `{` inside `"}"` does not close an
//! action, and a `$1` inside `printf("costs $1")` is not a value reference.
//!
//! Every caller needs the same answer for the same reason, so they share one
//! scanner rather than each carrying its own copy of the rule. The three that
//! existed before this module had already drifted: two tracked string and
//! comment state and one tracked none, which is what let `yacc` rewrite `$1`
//! inside a string literal.
//!
//! The scanner is fed one character at a time and reports where that character
//! sits, so it serves a stream (`yacc`'s action reader), a line-at-a-time
//! caller that resumes across lines (`lex`'s brace counter), and a caller that
//! rescans finished text (`yacc`'s `$` substitution) without any of them
//! reshaping their input.

/// Where a character sits in C source.
///
/// A delimiter belongs to what it delimits: both quotes of a string literal
/// report [`CharContext::String`], and the `*/` closing a comment reports
/// [`CharContext::Comment`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CharContext {
    /// Ordinary code. Operators, brackets and `$` mean what they say here.
    Code,
    /// Inside a `"..."` literal.
    String,
    /// Inside a `'...'` literal.
    Char,
    /// Inside a `/* */` or `//` comment.
    Comment,
}

impl CharContext {
    /// Whether this is ordinary code rather than a literal or a comment.
    pub fn is_code(self) -> bool {
        self == CharContext::Code
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
    Code,
    /// In a `"` or `'` literal; the char is the delimiter that closes it.
    Literal(char),
    Block,
    Line,
}

/// A C lexical scanner fed one character at a time.
///
/// Create one per region of code and call [`CScanner::step`] for every
/// character in order. The scanner keeps no buffer, so a caller may hold it
/// across reads, lines, or any other boundary in its own input.
#[derive(Debug, Clone)]
pub struct CScanner {
    state: State,
    /// A `\` in a literal, or immediately before a newline in a `//` comment,
    /// so the next character is escaped rather than special.
    escaped: bool,
    /// A `/` in code whose meaning is not yet known, or a `*` inside a block
    /// comment that may be closing it.
    pending: bool,
}

impl Default for CScanner {
    fn default() -> Self {
        Self::new()
    }
}

impl CScanner {
    /// A scanner positioned in ordinary code.
    pub fn new() -> Self {
        Self {
            state: State::Code,
            escaped: false,
            pending: false,
        }
    }

    /// Whether the scanner is currently in ordinary code -- that is, whether
    /// every literal and comment opened so far has been closed.
    pub fn is_code(&self) -> bool {
        self.state == State::Code
    }

    /// Consume `ch` and report where it sits.
    ///
    /// The `/` that opens a comment reports [`CharContext::Code`]: a scanner
    /// reading one character at a time cannot know what a `/` opens until it
    /// sees the next one. That is invisible to callers, which ask about `{`,
    /// `}` and `$` -- none of which can introduce a comment.
    pub fn step(&mut self, ch: char) -> CharContext {
        match self.state {
            State::Code => self.step_code(ch),
            State::Literal(close) => self.step_literal(ch, close),
            State::Block => self.step_block(ch),
            State::Line => self.step_line(ch),
        }
    }

    fn step_code(&mut self, ch: char) -> CharContext {
        if self.pending {
            // The previous character was a `/`.
            self.pending = false;
            match ch {
                '*' => {
                    self.state = State::Block;
                    return CharContext::Comment;
                }
                '/' => {
                    self.state = State::Line;
                    return CharContext::Comment;
                }
                _ => {}
            }
        }

        match ch {
            '/' => {
                self.pending = true;
                CharContext::Code
            }
            '"' | '\'' => {
                self.state = State::Literal(ch);
                self.escaped = false;
                if ch == '"' {
                    CharContext::String
                } else {
                    CharContext::Char
                }
            }
            _ => CharContext::Code,
        }
    }

    fn step_literal(&mut self, ch: char, close: char) -> CharContext {
        let context = if close == '"' {
            CharContext::String
        } else {
            CharContext::Char
        };

        if self.escaped {
            self.escaped = false;
        } else if ch == '\\' {
            self.escaped = true;
        } else if ch == close {
            self.state = State::Code;
        }

        context
    }

    fn step_block(&mut self, ch: char) -> CharContext {
        if self.pending && ch == '/' {
            self.pending = false;
            self.state = State::Code;
        } else {
            self.pending = ch == '*';
        }
        CharContext::Comment
    }

    fn step_line(&mut self, ch: char) -> CharContext {
        if ch == '\n' {
            // A `//` comment spliced by a backslash-newline continues onto the
            // next line (C17 5.1.1.2, phase 2 running before phase 3).
            if self.escaped {
                self.escaped = false;
                return CharContext::Comment;
            }
            self.state = State::Code;
            // The newline ends the comment without being part of it.
            return CharContext::Code;
        }
        self.escaped = ch == '\\';
        CharContext::Comment
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Scan `src` and return one context per character.
    fn scan(src: &str) -> Vec<CharContext> {
        let mut scanner = CScanner::new();
        src.chars().map(|c| scanner.step(c)).collect()
    }

    /// The characters of `src` that sit in ordinary code.
    fn code_only(src: &str) -> String {
        let mut scanner = CScanner::new();
        src.chars().filter(|&c| scanner.step(c).is_code()).collect()
    }

    #[test]
    fn plain_code_is_all_code() {
        assert!(scan("a + b / c;").iter().all(|c| c.is_code()));
    }

    #[test]
    fn string_literal_hides_its_contents() {
        // The defect this module exists for: `$1` and `}` inside a string are
        // not a value reference and not a closing brace.
        assert_eq!(code_only(r#"f("costs $1}"); $2"#), "f(); $2");
    }

    #[test]
    fn escaped_quote_does_not_close_the_string() {
        assert_eq!(code_only(r#""a\"$1" $2"#), " $2");
    }

    #[test]
    fn escaped_backslash_at_end_of_string_closes_it() {
        // `"a\\"` ends at the fourth character; the `$1` after it is code.
        assert_eq!(code_only(r#""a\\" $1"#), " $1");
    }

    #[test]
    fn char_literal_hides_its_contents() {
        assert_eq!(code_only("c == '$' ? 1 : 2"), "c ==  ? 1 : 2");
        assert_eq!(code_only(r"c == '\'' ? $1 : 0"), "c ==  ? $1 : 0");
    }

    // The `/` introducing a comment reports Code, per the documented contract:
    // one-character lookahead does not exist, and no caller asks about `/`.
    // These expectations keep that leading `/`, which is what makes the
    // contract visible rather than accidental.

    #[test]
    fn block_comment_hides_its_contents() {
        assert_eq!(code_only("a /* $1 } */ b"), "a / b");
    }

    #[test]
    fn block_comment_with_stars_inside() {
        assert_eq!(code_only("a /** $1 **/ b"), "a / b");
    }

    #[test]
    fn line_comment_ends_at_newline() {
        assert_eq!(code_only("a // $1\nb"), "a /\nb");
    }

    #[test]
    fn spliced_line_comment_continues() {
        // `$2` sits on the next source line but inside the same comment.
        assert_eq!(code_only("a // $1 \\\n$2\nb"), "a /\nb");
    }

    #[test]
    fn division_is_not_a_comment() {
        assert_eq!(code_only("a / b / c"), "a / b / c");
    }

    #[test]
    fn quote_inside_a_comment_opens_nothing() {
        assert_eq!(code_only(r#"/* " */ $1"#), "/ $1");
    }

    #[test]
    fn comment_inside_a_string_opens_nothing() {
        assert_eq!(code_only(r#""/*" $1"#), " $1");
    }

    #[test]
    fn delimiters_belong_to_what_they_delimit() {
        assert_eq!(
            scan(r#""x""#),
            vec![CharContext::String; 3],
            "both quotes are part of the string"
        );
        assert_eq!(
            scan("/**/"),
            // The opening `/` cannot be classified until the `*` arrives.
            vec![
                CharContext::Code,
                CharContext::Comment,
                CharContext::Comment,
                CharContext::Comment
            ]
        );
    }

    #[test]
    fn is_code_tracks_unterminated_constructs() {
        let mut scanner = CScanner::new();
        assert!(scanner.is_code());
        for c in r#""abc"#.chars() {
            scanner.step(c);
        }
        assert!(!scanner.is_code(), "an unterminated string is not closed");
        scanner.step('"');
        assert!(scanner.is_code());
    }

    #[test]
    fn scanner_resumes_across_a_boundary() {
        // `lex` feeds one line at a time and keeps the scanner, so a block
        // comment opened on one line still hides the next.
        let mut scanner = CScanner::new();
        for c in "a /* $1\n".chars() {
            scanner.step(c);
        }
        let second: String = "$2 */ $3"
            .chars()
            .filter(|&c| scanner.step(c).is_code())
            .collect();
        assert_eq!(second, " $3");
    }
}
