use crate::parse::lexer::command_lexer::CommandToken;
use crate::parse::{ParseResult, ParserError};
use std::borrow::Cow;

pub mod command_lexer;
pub mod word_lexer;

fn is_blank(c: char) -> bool {
    c == ' ' || c == '\t'
}

fn is_operator(c: char) -> bool {
    match c {
        '&' => true,
        '(' => true,
        ')' => true,
        ';' => true,
        '\n' => true,
        '|' => true,
        '<' => true,
        '>' => true,
        _ => false,
    }
}

trait Lexer {
    fn advance(&mut self);

    fn reached_eof(&self) -> bool;

    fn lookahead(&mut self) -> char;

    fn line_no(&self) -> u32;

    fn next_line(&mut self) -> Cow<str>;

    fn skip_comment(&mut self) {
        if self.lookahead() == '#' {
            while self.lookahead() != '\n' {
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

    fn skip_here_document(&mut self) -> ParseResult<()> {
        let start_lineno = self.line_no();
        let end = self.next_line().into_owned();
        loop {
            if self.reached_eof() {
                return Err(ParserError::new(
                    start_lineno,
                    "unterminated here-document",
                    true,
                ));
            }
            let line = self.next_line();
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
        Ok(())
    }

    fn skip_parameter_expansion(&mut self) -> ParseResult<()> {
        self.skip_word_token(Some('}'), false)?;
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
                }
                '(' => {
                    open_parens += 1;
                }
                ')' if open_parens == 0 => {
                    self.advance();
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
                        if self.lookahead() == '-' {
                            self.advance();
                            self.skip_here_document()?;
                        } else {
                            self.skip_here_document()?;
                        }
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
                    self.advance();
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
                    self.advance();
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
                            }
                        }
                        '{' => {
                            self.skip_parameter_expansion()?;
                        }
                        _ => {}
                    }
                }
                '`' => {
                    self.advance();
                    self.skip_backquoted_command_substitution()?;
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

fn remove_delimiter_from_here_document(here_document: Cow<str>) -> Cow<str> {
    match here_document {
        Cow::Borrowed(str) => {
            let str = &str.trim_start_matches(|c| c != '\n')[1..];
            let str = str[0..str.len() - 1].trim_end_matches(|c| c != '\n');
            str.into()
        }
        Cow::Owned(str) => {
            let str = &str.trim_start_matches(|c| c != '\n')[1..];
            let str = str[0..str.len() - 1].trim_end_matches(|c| c != '\n');
            str.to_string().into()
        }
    }
}
