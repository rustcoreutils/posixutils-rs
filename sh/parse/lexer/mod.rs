//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::parse::lexer::word_lexer::remove_quotes;
use crate::parse::{ParseResult, ParserError};
use std::borrow::Cow;

pub mod command_lexer;
pub mod word_lexer;

/// Every character with syntactic meaning in the shell grammar is ASCII, which
/// is what lets the lexer work on bytes: a byte >= 0x80 is by definition an
/// ordinary word character and cannot be mistaken for an operator or a quote.
pub fn is_blank(c: u8) -> bool {
    c == b' ' || c == b'\t'
}

fn is_operator(c: u8) -> bool {
    matches!(c, b'&' | b'(' | b')' | b';' | b'\n' | b'|' | b'<' | b'>')
}

trait Lexer {
    fn advance(&mut self);

    fn reached_eof(&self) -> bool;

    fn lookahead(&mut self) -> u8;

    fn line_no(&self) -> u32;

    fn next_line(&mut self) -> Cow<'_, [u8]>;

    fn next_word(&mut self) -> ParseResult<Cow<'_, [u8]>>;

    fn skip_comment(&mut self) {
        if self.lookahead() == b'#' {
            while self.lookahead() != b'\n' && !self.reached_eof() {
                self.advance();
            }
        }
    }

    fn skip_double_quoted_string(&mut self) -> ParseResult<()> {
        let quote_start_lineno = self.line_no();
        self.advance();
        self.skip_word_token(Some(b'"'), true)?;
        if self.lookahead() != b'"' {
            return Err(ParserError::new(
                quote_start_lineno,
                "missing closing '\"'",
                self.reached_eof(),
            ));
        }
        Ok(())
    }

    /// Consumes a here-document (delimiter word, body and terminator) without
    /// recording it. Used when only the extent of the surrounding construct
    /// matters, e.g. while scanning a command substitution.
    fn skip_here_document(&mut self, remove_leading_tabs: bool) -> ParseResult<()> {
        let start_lineno = self.line_no();
        // `<<` is an operator, so blanks may separate it from the delimiter.
        while is_blank(self.lookahead()) {
            self.advance();
        }
        let (_, end) = remove_quotes(self.next_word()?.as_ref())?;
        loop {
            if self.reached_eof() {
                return Err(ParserError::new(
                    start_lineno,
                    "unterminated here-document",
                    true,
                ));
            }
            let line = self.next_line();
            let line = line.strip_suffix(b"\n").unwrap_or(&line);
            let line = if remove_leading_tabs {
                {
                    let mut l = line;
                    while let Some(rest) = l.strip_prefix(b"\t") {
                        l = rest;
                    }
                    l
                }
            } else {
                line
            };
            if line == end {
                break;
            }
        }
        Ok(())
    }

    fn skip_single_quoted_string(&mut self) -> ParseResult<()> {
        let start_lineno = self.line_no();
        self.advance();
        loop {
            if self.reached_eof() {
                return Err(ParserError::new(
                    start_lineno,
                    "unterminated single quoted string",
                    true,
                ));
            }
            if self.lookahead() == b'\'' {
                break;
            }
            self.advance();
        }
        self.advance();
        Ok(())
    }

    fn skip_parameter_expansion(&mut self) -> ParseResult<()> {
        // The word in `${param:-word}` / `:?` / `:=` / `:+` (and pattern in
        // `${param#pat}` etc.) may contain blanks and operators up to the
        // matching '}', so they must not terminate the token here.
        self.skip_word_token(Some(b'}'), true)?;
        if self.lookahead() != b'}' {
            return Err(ParserError::new(
                self.line_no(),
                "missing closing '}' in parameter expansion",
                self.reached_eof(),
            ));
        }
        self.advance();
        Ok(())
    }

    /// does not skip the terminating )
    fn skip_command_substitution(&mut self) -> ParseResult<()> {
        let start_lineno = self.line_no();
        let mut open_parens = 0;
        // A `case` item's pattern ends in a `)` that never had a matching `(`,
        // so paren counting alone mistakes it for the end of the substitution
        // and `$(case a in a) echo A;; esac)` is cut short. Track how many
        // `case`s are still open; while any is, a `)` at depth zero belongs to
        // a pattern.
        let mut open_cases: u32 = 0;
        let mut word: Vec<u8> = Vec::new();
        // `case`/`esac` are reserved words only in command position, so
        // `$(echo case)` must not be counted (POSIX 2.4).
        let mut at_command_start = true;
        self.skip_comment();
        loop {
            if self.reached_eof() {
                return Err(ParserError::new(
                    start_lineno,
                    "missing terminating ')' in command expansion",
                    true,
                ));
            }
            // A blank or operator ends the current word; `case` and `esac` are
            // only reserved words when they stand alone.
            let c = self.lookahead();
            if is_blank(c) || is_operator(c) {
                if !word.is_empty() {
                    if at_command_start {
                        match word.as_slice() {
                            b"case" => open_cases += 1,
                            b"esac" => open_cases = open_cases.saturating_sub(1),
                            _ => {}
                        }
                    }
                    at_command_start = false;
                    word.clear();
                }
                if matches!(c, b';' | b'&' | b'|' | b'\n' | b'(' | b')') {
                    at_command_start = true;
                }
            }
            match c {
                b'"' => {
                    self.skip_double_quoted_string()?;
                }
                b'\'' => {
                    self.skip_single_quoted_string()?;
                    continue;
                }
                b'(' => {
                    open_parens += 1;
                }
                b')' if open_parens == 0 && open_cases == 0 => {
                    break;
                }
                b')' if open_parens == 0 => {
                    // closes a `case` item's pattern
                }
                b')' => {
                    open_parens -= 1;
                }
                b'\\' => {
                    self.advance();
                    if self.reached_eof() {
                        return Err(ParserError::new(
                            self.line_no(),
                            "missing character after '\\'",
                            true,
                        ));
                    }
                }
                b'<' => {
                    self.advance();
                    if self.lookahead() == b'<' {
                        self.advance();
                        let remove_leading_tabs = self.lookahead() == b'-';
                        if remove_leading_tabs {
                            self.advance();
                        }
                        self.skip_here_document(remove_leading_tabs)?;
                    }
                    // don't advance char
                    continue;
                }
                other if is_blank(other) || is_operator(other) => {
                    // when '#' is not inside a word it is a comment.
                    self.advance();
                    self.skip_comment();
                    // don't advance char
                    continue;
                }
                other => word.push(other),
            }
            self.advance();
        }
        Ok(())
    }

    /// does not skip terminating `
    fn skip_backquoted_command_substitution(&mut self) -> ParseResult<()> {
        loop {
            if self.reached_eof() {
                return Err(ParserError::new(
                    self.line_no(),
                    "missing closing '`' in command substitution",
                    true,
                ));
            }
            match self.lookahead() {
                b'\\' => {
                    self.advance();
                    if self.lookahead() == b'`' {
                        self.advance();
                    }
                }
                b'`' => {
                    break;
                }
                _ => self.advance(),
            }
        }
        Ok(())
    }

    fn skip_arithmetic_expansion(&mut self) -> ParseResult<()> {
        let start_lineno = self.line_no();
        let mut open_parens = 0;
        loop {
            if self.reached_eof() {
                return Err(ParserError::new(
                    start_lineno,
                    "missing closing '))' in arithmetic expansion",
                    true,
                ));
            }
            match self.lookahead() {
                b'"' => {
                    self.skip_double_quoted_string()?;
                }
                b'\'' => {
                    self.skip_single_quoted_string()?;
                    continue;
                }
                b'\\' => {
                    self.advance();
                }
                b'(' => open_parens += 1,
                b')' if open_parens == 0 => {
                    self.advance();
                    if self.lookahead() == b')' {
                        self.advance();
                        break;
                    } else {
                        return Err(ParserError::new(
                            start_lineno,
                            "missing closing '))' in arithmetic expansion",
                            true,
                        ));
                    }
                }
                b')' => open_parens -= 1,
                _ => {}
            }
            self.advance();
        }
        Ok(())
    }

    fn skip_word_token(
        &mut self,
        end: Option<u8>,
        include_spaces_and_operators: bool,
    ) -> ParseResult<()> {
        let word_start_lineno = self.line_no();
        let mut inside_double_quotes = false;
        while !self.reached_eof() {
            if !inside_double_quotes && end.is_some_and(|c| self.lookahead() == c) {
                break;
            }

            match self.lookahead() {
                b'"' => {
                    inside_double_quotes = !inside_double_quotes;
                    self.advance();
                }
                b'\'' if !inside_double_quotes => {
                    self.skip_single_quoted_string()?;
                }
                b'$' => {
                    self.advance();
                    match self.lookahead() {
                        b'(' => {
                            self.advance();
                            if self.lookahead() == b'(' {
                                self.advance();
                                self.skip_arithmetic_expansion()?;
                            } else {
                                self.skip_command_substitution()?;
                                self.advance();
                            }
                        }
                        b'{' => {
                            self.skip_parameter_expansion()?;
                        }
                        b'\'' => {
                            // $'...' dollar-single-quote: a backslash escapes the
                            // next character (so \' does not close the string).
                            self.advance();
                            while !self.reached_eof() && self.lookahead() != b'\'' {
                                if self.lookahead() == b'\\' {
                                    self.advance();
                                    if self.reached_eof() {
                                        break;
                                    }
                                }
                                self.advance();
                            }
                            self.advance();
                        }
                        _ => {}
                    }
                }
                b'`' => {
                    self.advance();
                    self.skip_backquoted_command_substitution()?;
                    self.advance();
                }
                b'\\' => {
                    self.advance();
                    self.advance();
                }
                other => {
                    if !include_spaces_and_operators
                        && !inside_double_quotes
                        && (is_operator(other) || is_blank(other))
                    {
                        break;
                    }
                    self.advance();
                }
            }
        }
        if inside_double_quotes {
            return Err(ParserError::new(
                word_start_lineno,
                "missing closing '\"'",
                self.reached_eof(),
            ));
        }
        Ok(())
    }
}
