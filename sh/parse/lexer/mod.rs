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

pub fn is_blank(c: char) -> bool {
    c == ' ' || c == '\t'
}

fn is_operator(c: char) -> bool {
    matches!(c, '&' | '(' | ')' | ';' | '\n' | '|' | '<' | '>')
}

trait Lexer {
    fn advance(&mut self);

    fn reached_eof(&self) -> bool;

    fn lookahead(&mut self) -> char;

    fn line_no(&self) -> u32;

    fn next_line(&mut self) -> Cow<'_, str>;

    fn next_word(&mut self) -> ParseResult<Cow<'_, str>>;

    fn skip_comment(&mut self) {
        if self.lookahead() == '#' {
            while self.lookahead() != '\n' && !self.reached_eof() {
                self.advance();
            }
        }
    }

    fn skip_double_quoted_string(&mut self) -> ParseResult<()> {
        let quote_start_lineno = self.line_no();
        self.advance();
        self.skip_word_token(Some('"'), true)?;
        if self.lookahead() != '"' {
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
        let (_, end) = remove_quotes(self.next_word()?.as_ref());
        loop {
            if self.reached_eof() {
                return Err(ParserError::new(
                    start_lineno,
                    "unterminated here-document",
                    true,
                ));
            }
            let line = self.next_line();
            let line = line.trim_end_matches('\n');
            let line = if remove_leading_tabs {
                line.trim_start_matches('\t')
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
            if self.lookahead() == '\'' {
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
        self.skip_word_token(Some('}'), true)?;
        if self.lookahead() != '}' {
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
        self.skip_comment();
        loop {
            if self.reached_eof() {
                return Err(ParserError::new(
                    start_lineno,
                    "missing terminating ')' in command expansion",
                    true,
                ));
            }
            match self.lookahead() {
                '"' => {
                    self.skip_double_quoted_string()?;
                }
                '\'' => {
                    self.skip_single_quoted_string()?;
                    continue;
                }
                '(' => {
                    open_parens += 1;
                }
                ')' if open_parens == 0 => {
                    break;
                }
                ')' => {
                    open_parens -= 1;
                }
                '\\' => {
                    self.advance();
                    if self.reached_eof() {
                        return Err(ParserError::new(
                            self.line_no(),
                            "missing character after '\\'",
                            true,
                        ));
                    }
                }
                '<' => {
                    self.advance();
                    if self.lookahead() == '<' {
                        self.advance();
                        let remove_leading_tabs = self.lookahead() == '-';
                        if remove_leading_tabs {
                            self.advance();
                        }
                        self.skip_here_document(remove_leading_tabs)?;
                    }
                    // don't advance char
                    continue;
                }
                other if is_blank(other) || is_operator(other) => {
                    // unquoted blanks and operators are word delimiters
                    // when '#' is not inside a word it is a comment.
                    self.advance();
                    self.skip_comment();
                    // don't advance char
                    continue;
                }
                _ => {}
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
                '\\' => {
                    self.advance();
                    if self.lookahead() == '`' {
                        self.advance();
                    }
                }
                '`' => {
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
                '"' => {
                    self.skip_double_quoted_string()?;
                }
                '\'' => {
                    self.skip_single_quoted_string()?;
                    continue;
                }
                '\\' => {
                    self.advance();
                }
                '(' => open_parens += 1,
                ')' if open_parens == 0 => {
                    self.advance();
                    if self.lookahead() == ')' {
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
                ')' => open_parens -= 1,
                _ => {}
            }
            self.advance();
        }
        Ok(())
    }

    fn skip_word_token(
        &mut self,
        end: Option<char>,
        include_spaces_and_operators: bool,
    ) -> ParseResult<()> {
        let word_start_lineno = self.line_no();
        let mut inside_double_quotes = false;
        while !self.reached_eof() {
            if !inside_double_quotes && end.is_some_and(|c| self.lookahead() == c) {
                break;
            }

            match self.lookahead() {
                '"' => {
                    inside_double_quotes = !inside_double_quotes;
                    self.advance();
                }
                '\'' if !inside_double_quotes => {
                    self.skip_single_quoted_string()?;
                }
                '$' => {
                    self.advance();
                    match self.lookahead() {
                        '(' => {
                            self.advance();
                            if self.lookahead() == '(' {
                                self.advance();
                                self.skip_arithmetic_expansion()?;
                            } else {
                                self.skip_command_substitution()?;
                                self.advance();
                            }
                        }
                        '{' => {
                            self.skip_parameter_expansion()?;
                        }
                        '\'' => {
                            // $'...' dollar-single-quote: a backslash escapes the
                            // next character (so \' does not close the string).
                            self.advance();
                            while !self.reached_eof() && self.lookahead() != '\'' {
                                if self.lookahead() == '\\' {
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
                '`' => {
                    self.advance();
                    self.skip_backquoted_command_substitution()?;
                    self.advance();
                }
                '\\' => {
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
