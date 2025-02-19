//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::parse::{ParseResult, ParserError};
use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Clone, Debug)]
struct SourceReadState<'s> {
    current_part: usize,
    current_part_char_iter: Peekable<CharIndices<'s>>,
    current_part_char_index: usize,
    line_no: u32,
    reached_eof: bool,
}

struct SourcePart<'s> {
    text: &'s str,
    in_original_string: bool,
}

impl SourcePart<'_> {
    fn split_at(&self, mid: usize) -> (Self, Self) {
        let (p1, p2) = self.text.split_at(mid);
        (
            Self {
                text: p1,
                in_original_string: self.in_original_string,
            },
            Self {
                text: p2,
                in_original_string: self.in_original_string,
            },
        )
    }
}

struct SourceString<'s> {
    // all parts should have text, otherwise they should not be added.
    // The first part is an exception, but if its empty, `read_state.reached_eof`
    // should be set
    parts: Vec<SourcePart<'s>>,
    read_state: SourceReadState<'s>,
}

impl<'s> SourceString<'s> {
    fn new(s: &'s str) -> Self {
        Self {
            parts: vec![SourcePart {
                text: s,
                in_original_string: true,
            }],
            read_state: SourceReadState {
                current_part: 0,
                current_part_char_iter: s.char_indices().peekable(),
                current_part_char_index: 0,
                reached_eof: s.is_empty(),
                line_no: 1,
            },
        }
    }

    fn advance_char(&mut self) {
        if self.read_state.reached_eof {
            return;
        }

        // since `self.read_state.reached_eof` is false, unwrap is safe
        let (index, char) = self.read_state.current_part_char_iter.next().unwrap();
        if char == '\n' && self.parts[self.read_state.current_part].in_original_string {
            self.read_state.line_no += 1;
        }
        self.read_state.current_part_char_index = index + char.len_utf8();
        if self.read_state.current_part_char_iter.peek().is_none() {
            self.read_state.current_part += 1;
            if self.read_state.current_part == self.parts.len() {
                self.read_state.reached_eof = true;
                return;
            }
            self.read_state.current_part_char_index = 0;
            self.read_state.current_part_char_iter = self.parts[self.read_state.current_part]
                .text
                .char_indices()
                .peekable();
        }
    }

    fn insert_string_after_last_char(&mut self, string: &'s str) {
        if string.is_empty() {
            return;
        }
        let (p1, p2) = self.parts[self.read_state.current_part]
            .split_at(self.read_state.current_part_char_index);
        self.parts[self.read_state.current_part] = p1;
        self.parts.push(SourcePart {
            text: string,
            in_original_string: false,
        });
        self.parts.push(p2);
        self.parts[self.read_state.current_part + 1..].rotate_right(2);
        self.read_state.current_part_char_index = 0;
        self.read_state.current_part += 1;
        self.read_state.current_part_char_iter = self.parts[self.read_state.current_part]
            .text
            .char_indices()
            .peekable();
    }

    fn reached_eof(&self) -> bool {
        self.read_state.reached_eof
    }

    fn line_no(&self) -> u32 {
        self.read_state.line_no
    }

    fn lookahead(&mut self) -> char {
        self.read_state
            .current_part_char_iter
            .peek()
            .copied()
            .map(|(_, c)| c)
            .unwrap_or_default()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CommandToken<'src> {
    // single character control operators
    And,       // &
    LParen,    // (
    RParen,    // )
    SemiColon, // ;
    Newline,   // \n
    Pipe,      // |
    // multi character control operators
    AndIf, // &&
    OrIf,  // ||
    DSemi, // ;;
    // single character redirection operators
    Less,    // <
    Greater, // >
    // multi character redirection operators
    Clobber,   // >|
    DGreat,    // >>
    LessAnd,   // <&
    GreatAnd,  // >&
    LessGreat, // <>

    // reserved words
    Bang,   // !
    LBrace, // {
    RBrace, // }
    Case,   // case
    Do,     // do
    Done,   // done
    Elif,   // elif
    Else,   // else
    Esac,   // esac
    Fi,     // fi
    For,    // for
    If,     // if
    In,     // in
    Then,   // then
    Until,  // until
    While,  // while

    // number followed by a redirection operator
    IoNumber(u32),

    Word(Cow<'src, str>),
    HereDocument(String),

    EOF,
}

impl Display for CommandToken<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            CommandToken::And => write!(f, "'&'"),
            CommandToken::LParen => write!(f, "'('"),
            CommandToken::RParen => write!(f, "')'"),
            CommandToken::SemiColon => write!(f, "';'"),
            CommandToken::Newline => write!(f, "'\\n'"),
            CommandToken::Pipe => write!(f, "'|'"),
            CommandToken::AndIf => write!(f, "'&&'"),
            CommandToken::OrIf => write!(f, "'||'"),
            CommandToken::DSemi => write!(f, "';;'"),
            CommandToken::Less => write!(f, "'<'"),
            CommandToken::Greater => write!(f, "'>'"),
            CommandToken::Clobber => write!(f, "'>|'"),
            CommandToken::DGreat => write!(f, "'>>'"),
            CommandToken::LessAnd => write!(f, "'<&'"),
            CommandToken::GreatAnd => write!(f, "'>&'"),
            CommandToken::LessGreat => write!(f, "'<>'"),
            CommandToken::Bang => write!(f, "'!'"),
            CommandToken::LBrace => write!(f, "'{{'"),
            CommandToken::RBrace => write!(f, "'}}'"),
            CommandToken::Case => write!(f, "case"),
            CommandToken::Do => write!(f, "do"),
            CommandToken::Done => write!(f, "done"),
            CommandToken::Elif => write!(f, "elif"),
            CommandToken::Else => write!(f, "else"),
            CommandToken::Esac => write!(f, "esac"),
            CommandToken::Fi => write!(f, "fi"),
            CommandToken::For => write!(f, "for"),
            CommandToken::If => write!(f, "if"),
            CommandToken::In => write!(f, "in"),
            CommandToken::Then => write!(f, "then"),
            CommandToken::Until => write!(f, "until"),
            CommandToken::While => write!(f, "while"),
            CommandToken::IoNumber(_) => write!(f, "number"),
            CommandToken::Word(_) => write!(f, "word"),
            CommandToken::HereDocument(_) => write!(f, "here-document"),
            CommandToken::EOF => write!(f, "EOF"),
        }
    }
}

impl<'src> CommandToken<'src> {
    pub fn as_word_str(&self) -> Option<&str> {
        match self {
            CommandToken::Bang => Some("!"),
            CommandToken::LBrace => Some("{"),
            CommandToken::RBrace => Some("}"),
            CommandToken::Case => Some("case"),
            CommandToken::Do => Some("do"),
            CommandToken::Done => Some("done"),
            CommandToken::Elif => Some("elif"),
            CommandToken::Else => Some("else"),
            CommandToken::Esac => Some("esac"),
            CommandToken::Fi => Some("fi"),
            CommandToken::For => Some("for"),
            CommandToken::If => Some("if"),
            CommandToken::In => Some("in"),
            CommandToken::Then => Some("then"),
            CommandToken::Until => Some("until"),
            CommandToken::While => Some("while"),
            CommandToken::Word(word) => Some(word.as_ref()),
            _ => None,
        }
    }

    pub fn into_word_cow(self) -> Option<Cow<'src, str>> {
        match self {
            CommandToken::Bang => Some("!".into()),
            CommandToken::LBrace => Some("{".into()),
            CommandToken::RBrace => Some("}".into()),
            CommandToken::Case => Some("case".into()),
            CommandToken::Do => Some("do".into()),
            CommandToken::Done => Some("done".into()),
            CommandToken::Elif => Some("elif".into()),
            CommandToken::Else => Some("else".into()),
            CommandToken::Esac => Some("esac".into()),
            CommandToken::Fi => Some("fi".into()),
            CommandToken::For => Some("for".into()),
            CommandToken::If => Some("if".into()),
            CommandToken::In => Some("in".into()),
            CommandToken::Then => Some("then".into()),
            CommandToken::Until => Some("until".into()),
            CommandToken::While => Some("while".into()),
            CommandToken::Word(word) => Some(word.into()),
            _ => None,
        }
    }

    pub fn unwrap_here_document_contents(self) -> String {
        match self {
            CommandToken::HereDocument(contents) => contents,
            _ => unreachable!(),
        }
    }

    fn word(word: Cow<'src, str>) -> Self {
        match word.as_ref() {
            "!" => CommandToken::Bang,
            "{" => CommandToken::LBrace,
            "}" => CommandToken::RBrace,
            "case" => CommandToken::Case,
            "do" => CommandToken::Do,
            "done" => CommandToken::Done,
            "elif" => CommandToken::Elif,
            "else" => CommandToken::Else,
            "esac" => CommandToken::Esac,
            "fi" => CommandToken::Fi,
            "for" => CommandToken::For,
            "if" => CommandToken::If,
            "in" => CommandToken::In,
            "then" => CommandToken::Then,
            "until" => CommandToken::Until,
            "while" => CommandToken::While,
            _ => CommandToken::Word(word),
        }
    }
}

fn is_blank(c: char) -> bool {
    c == ' ' || c == '\t'
}

fn char_to_operator_token(c: char) -> Option<CommandToken<'static>> {
    match c {
        '&' => Some(CommandToken::And),
        '(' => Some(CommandToken::LParen),
        ')' => Some(CommandToken::RParen),
        ';' => Some(CommandToken::SemiColon),
        '\n' => Some(CommandToken::Newline),
        '|' => Some(CommandToken::Pipe),
        '<' => Some(CommandToken::Less),
        '>' => Some(CommandToken::Greater),
        _ => None,
    }
}

fn is_operator(c: char) -> bool {
    char_to_operator_token(c).is_some()
}

fn advance_and_return<Tok>(lex: &mut CommandLexer, complete_token: Tok) -> Tok {
    lex.source.advance_char();
    complete_token
}

pub struct CommandLexer<'src> {
    source: SourceString<'src>,
    prev_read_state: SourceReadState<'src>,
    last_token_id: u64,
}

impl<'src> CommandLexer<'src> {
    fn reached_eof(&mut self) -> bool {
        self.source.reached_eof()
    }

    fn skip_blanks(&mut self) {
        while is_blank(self.source.lookahead()) {
            self.source.advance_char();
        }
    }

    fn skip_comment(&mut self) {
        if self.source.lookahead() == '#' {
            while self.source.lookahead() != '\n' {
                self.source.advance_char();
            }
        }
    }

    fn next_line(&mut self) -> String {
        let mut result = String::new();
        while !self.reached_eof() {
            let lookahead = self.source.lookahead();
            result.push(lookahead);
            self.source.advance_char();
            if lookahead == '\n' {
                break;
            }
        }
        result
    }

    fn read_here_document_into(
        &mut self,
        result: &mut String,
        remove_leading_tabs: bool,
        include_delimiters: bool,
    ) -> ParseResult<()> {
        let start_lineno = self.source.line_no();
        let end = self.next_line();
        if include_delimiters {
            result.push_str(&end);
        }
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
                if include_delimiters {
                    result.push_str(&line);
                }
                break;
            }
            if remove_leading_tabs {
                result.push_str(line.trim_start_matches('\t'));
            } else {
                result.push_str(&line);
            }
        }
        Ok(())
    }

    fn read_single_quoted_string_into(&mut self, result: &mut String) -> ParseResult<()> {
        let start_lineno = self.source.line_no();
        self.source.advance_char();
        loop {
            if self.source.reached_eof() {
                return Err(ParserError::new(
                    start_lineno,
                    "unterminated single quoted string",
                    true,
                ));
            }
            result.push(self.source.lookahead());
            if self.source.lookahead() == '\'' {
                break;
            }
            self.source.advance_char();
        }
        Ok(())
    }

    fn read_parameter_expansion_into(&mut self, result: &mut String) -> ParseResult<()> {
        self.read_word_token_into(result, Some('}'), false)?;
        if self.source.lookahead() != '}' {
            return Err(ParserError::new(
                self.source.line_no(),
                "missing closing '}' in parameter expansion",
                self.reached_eof(),
            ));
        }
        result.push('}');
        self.source.advance_char();
        Ok(())
    }

    fn read_command_substitution_into(&mut self, result: &mut String) -> ParseResult<()> {
        let start_lineno = self.source.line_no();
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
            result.push(self.source.lookahead());
            match self.source.lookahead() {
                '"' => {
                    let quote_start_lineno = self.source.line_no();
                    self.source.advance_char();
                    self.read_word_token_into(result, Some('"'), true)?;
                    if self.source.lookahead() != '"' {
                        return Err(ParserError::new(
                            quote_start_lineno,
                            "missing closing '\"'",
                            self.reached_eof(),
                        ));
                    }
                    result.push('"');
                }
                '\'' => {
                    self.read_single_quoted_string_into(result)?;
                }
                '(' => {
                    open_parens += 1;
                }
                ')' if open_parens == 0 => {
                    self.source.advance_char();
                    break;
                }
                ')' => {
                    open_parens -= 1;
                }
                '\\' => {
                    self.source.advance_char();
                    if self.reached_eof() {
                        return Err(ParserError::new(
                            self.source.line_no(),
                            "missing character after '\\'",
                            true,
                        ));
                    }
                    result.push(self.source.lookahead());
                }
                '<' => {
                    self.source.advance_char();
                    if self.source.lookahead() == '<' {
                        result.push('<');
                        self.source.advance_char();
                        if self.source.lookahead() == '-' {
                            result.push('-');
                            self.source.advance_char();
                            self.read_here_document_into(result, true, true)?;
                        } else {
                            self.read_here_document_into(result, false, true)?;
                        }
                    }
                    // don't advance char
                    continue;
                }
                other if is_blank(other) || is_operator(other) => {
                    // unquoted blanks and operators are word delimiters
                    // when '#' is not inside a word it is a comment.
                    self.source.advance_char();
                    self.skip_comment();
                    // don't advance char
                    continue;
                }
                _ => {}
            }
            self.source.advance_char();
        }
        Ok(())
    }

    fn read_backquoted_command_substitution_into(
        &mut self,
        result: &mut String,
    ) -> ParseResult<()> {
        loop {
            if self.reached_eof() {
                return Err(ParserError::new(
                    self.source.line_no(),
                    "missing closing '`' in command substitution",
                    true,
                ));
            }
            result.push(self.source.lookahead());
            match self.source.lookahead() {
                '\\' => {
                    self.source.advance_char();
                    if self.source.lookahead() == '`' {
                        result.push('`');
                        self.source.advance_char();
                    }
                }
                '`' => {
                    self.source.advance_char();
                    break;
                }
                _ => self.source.advance_char(),
            }
        }
        Ok(())
    }

    fn read_arithmetic_expansion_into(&mut self, result: &mut String) -> ParseResult<()> {
        let start_lineno = self.source.line_no();
        let mut open_parens = 0;
        loop {
            if self.reached_eof() {
                return Err(ParserError::new(
                    start_lineno,
                    "missing closing '))' in arithmetic expansion",
                    true,
                ));
            }
            result.push(self.source.lookahead());
            match self.source.lookahead() {
                '"' => {
                    let quote_start_lineno = self.source.line_no();
                    self.source.advance_char();
                    self.read_word_token_into(result, Some('"'), true)?;
                    if self.source.lookahead() != '"' {
                        return Err(ParserError::new(
                            quote_start_lineno,
                            "missing closing '\"'",
                            self.reached_eof(),
                        ));
                    }
                    result.push('"');
                }
                '\'' => {
                    self.read_single_quoted_string_into(result)?;
                }
                '\\' => {
                    self.source.advance_char();
                    result.push(self.source.lookahead());
                }
                '(' => open_parens += 1,
                ')' if open_parens == 0 => {
                    self.source.advance_char();
                    if self.source.lookahead() == ')' {
                        result.push(')');
                        self.source.advance_char();
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
            self.source.advance_char();
        }
        Ok(())
    }

    fn read_word_token_into(
        &mut self,
        result: &mut String,
        end: Option<char>,
        include_spaces_and_operators: bool,
    ) -> ParseResult<()> {
        let word_start_lineno = self.source.line_no();
        let mut inside_double_quotes = false;
        while !self.source.reached_eof() {
            if !inside_double_quotes && end.is_some_and(|c| self.source.lookahead() == c) {
                break;
            }
            result.push(self.source.lookahead());
            match self.source.lookahead() {
                '"' => {
                    inside_double_quotes = !inside_double_quotes;
                    self.source.advance_char();
                }
                '\'' if !inside_double_quotes => {
                    self.read_single_quoted_string_into(result)?;
                    self.source.advance_char();
                }
                '$' => {
                    self.source.advance_char();
                    match self.source.lookahead() {
                        '(' => {
                            result.push('(');
                            self.source.advance_char();
                            if self.source.lookahead() == '(' {
                                result.push('(');
                                self.source.advance_char();
                                self.read_arithmetic_expansion_into(result)?;
                            } else {
                                self.read_command_substitution_into(result)?;
                            }
                        }
                        '{' => {
                            self.read_parameter_expansion_into(result)?;
                        }
                        _ => {}
                    }
                }
                '`' => {
                    self.source.advance_char();
                    self.read_backquoted_command_substitution_into(result)?;
                }
                '\\' => {
                    self.source.advance_char();
                    result.push(self.source.lookahead());
                    self.source.advance_char();
                }
                other => {
                    if !include_spaces_and_operators
                        && !inside_double_quotes
                        && (is_operator(other) || is_blank(other))
                    {
                        result.pop();
                        break;
                    }
                    self.source.advance_char();
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

    fn read_word_token(&mut self) -> ParseResult<String> {
        let mut result = String::new();
        self.read_word_token_into(&mut result, None, false)?;
        Ok(result)
    }

    pub fn next_token(&mut self) -> ParseResult<(CommandToken<'src>, u32)> {
        self.skip_blanks();
        self.skip_comment();

        let line_no = self.source.line_no();

        if self.reached_eof() {
            return Ok((CommandToken::EOF, line_no));
        }

        if let Some(partial_token) = char_to_operator_token(self.source.lookahead()) {
            // multi-character operators all start with a single character
            // operator
            self.source.advance_char();

            let complete_token = match partial_token {
                CommandToken::And => match self.source.lookahead() {
                    '&' => advance_and_return(self, CommandToken::AndIf),
                    _ => CommandToken::And,
                },
                CommandToken::Pipe => match self.source.lookahead() {
                    '|' => advance_and_return(self, CommandToken::OrIf),
                    _ => CommandToken::Pipe,
                },
                CommandToken::SemiColon => match self.source.lookahead() {
                    ';' => advance_and_return(self, CommandToken::DSemi),
                    _ => CommandToken::SemiColon,
                },
                CommandToken::Less => match self.source.lookahead() {
                    '&' => advance_and_return(self, CommandToken::LessAnd),
                    '>' => advance_and_return(self, CommandToken::LessGreat),
                    '<' => {
                        self.source.advance_char();
                        let mut here_document_contents = String::new();
                        if self.source.lookahead() == '-' {
                            self.source.advance_char();
                            self.read_here_document_into(&mut here_document_contents, true, false)?;
                        } else {
                            self.read_here_document_into(
                                &mut here_document_contents,
                                false,
                                false,
                            )?;
                        }
                        CommandToken::HereDocument(here_document_contents)
                    }
                    _ => CommandToken::Less,
                },
                CommandToken::Greater => match self.source.lookahead() {
                    '>' => advance_and_return(self, CommandToken::DGreat),
                    '&' => advance_and_return(self, CommandToken::GreatAnd),
                    '|' => advance_and_return(self, CommandToken::Clobber),
                    _ => CommandToken::Greater,
                },
                other => other,
            };
            return Ok((complete_token, line_no));
        }

        let token = match self.source.lookahead() {
            d if d.is_digit(10) => {
                let prev_read_state = self.source.read_state.clone();

                let mut number = d.to_digit(10).unwrap();
                self.source.advance_char();
                while let Some(d) = self.source.lookahead().to_digit(10) {
                    number = number.saturating_mul(10);
                    number = number.saturating_add(d);
                    self.source.advance_char();
                }
                if self.source.lookahead() == '>' || self.source.lookahead() == '<' {
                    CommandToken::IoNumber(number)
                } else {
                    self.source.read_state = prev_read_state;
                    CommandToken::word(self.read_word_token()?.into())
                }
            }
            _ => CommandToken::word(self.read_word_token()?.into()),
        };
        Ok((token, line_no))
    }

    /// Inserts text after the last returned token
    pub fn insert_text_at_current_position(&mut self, text: &'src str) {
        self.source.insert_string_after_last_char(text);
    }

    pub fn new(source: &'src str) -> Self {
        let source = SourceString::new(source);
        let initial_read_state = source.read_state.clone();
        Self {
            source,
            prev_read_state: initial_read_state,
            last_token_id: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_word(text: &str) {
        let mut lex = CommandLexer::new(text);
        if let CommandToken::Word(word) = lex.next_token().unwrap().0 {
            assert_eq!(lex.next_token().unwrap().0, CommandToken::EOF);
            assert_eq!(word, text);
        } else {
            panic!("not a word")
        }
    }

    fn lex_token(token: &str) -> CommandToken {
        let mut lex = CommandLexer::new(token);
        let token = lex.next_token().unwrap().0;
        assert_eq!(lex.next_token().unwrap().0, CommandToken::EOF);
        token
    }

    #[test]
    fn lex_empty_string() {
        let mut lex = CommandLexer::new("");
        assert_eq!(lex.next_token().unwrap().0, CommandToken::EOF);
    }

    #[test]
    fn lex_skip_comment() {
        let mut lex = CommandLexer::new("# this is a comment\n");
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Newline);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::EOF);
    }

    #[test]
    fn lex_operators() {
        let mut lex = CommandLexer::new("&();\n|&&||;;< > >| >><&>&<>");
        assert_eq!(lex.next_token().unwrap().0, CommandToken::And);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::LParen);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::RParen);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::SemiColon);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Newline);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Pipe);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::AndIf);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::OrIf);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::DSemi);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Less);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Greater);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Clobber);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::DGreat);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::LessAnd);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::GreatAnd);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::LessGreat);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::EOF);
    }

    #[test]
    fn test_lex_reserved_words() {
        let mut lex =
            CommandLexer::new("! { } case do done elif else esac fi for if in then until while");
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Bang);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::LBrace);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::RBrace);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Case);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Do);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Done);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Elif);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Else);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Esac);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Fi);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::For);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::If);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::In);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Then);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Until);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::While);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::EOF);
    }

    #[test]
    fn lex_name() {
        lex_word("a");
        lex_word("this_is_a_var123");
    }

    #[test]
    fn lex_parameter_expansion() {
        lex_word("$var");
        lex_word("${var}");
        lex_word("${var:-word}");
        lex_word("${var:-${param}}");
        lex_word("${var:-'}'}");
        lex_word("${var:-\"}\"}");
        lex_word("${var:-\\}}");
    }

    #[test]
    fn lex_single_quoted_word() {
        lex_word("'test ${$($(('")
    }

    #[test]
    fn lex_backslash_escaped_characters() {
        lex_word("\\;");
    }

    #[test]
    fn lex_command_substitution() {
        lex_word("$()");
        lex_word("$(command)");
        lex_word("$(command  arg1 arg2 arg3)");
        lex_word("$(ab $(cd))");
        lex_word("$(')')");
        lex_word("$(\")\")");
        lex_word("$(\\))");
        lex_word("$(ab$(cd$(ef)))");
    }

    #[test]
    fn comment_in_command_substitution() {
        assert_eq!(
            lex_token("$(#comment\necho a &#)'\")2\n)"),
            CommandToken::Word("$(\necho a &\n)".into())
        );
    }

    #[test]
    fn here_document_inside_command_substitution() {
        lex_word("$(echo <<end\nthis\nis\n\ta\ntest\nend\n)");
        assert_eq!(
            lex_token("$(echo <<-end\nthis)(\nis\n\ta))\n\t\t\t\ttest\nend\n)"),
            CommandToken::Word("$(echo <<-end\nthis)(\nis\na))\ntest\nend\n)".into())
        );
    }

    #[test]
    fn lex_backtick_command_substitution() {
        lex_word("``");
        lex_word("`cmd`");
        lex_word("`cmd arg1 arg2`");
        lex_word("`cmd arg1 \\`cmd2 arg2\\``");
        lex_word("`cmd arg1 cmd2 arg2\\``");
    }

    #[test]
    fn arithmetic_expansion() {
        lex_word("$(())");
        lex_word("$((1 + 1))");
        lex_word("$((')'\\)\")\"))");
        lex_word("$((\"$(echo \"$((1 + 1))\")\"))");
    }

    #[test]
    fn mixed_word() {
        lex_word("test${var:-$(cmd)}'quoted'$(command with args)$((1 + 1))");
    }

    #[test]
    fn here_document() {
        assert_eq!(
            lex_token("<<end\nthis\nis\n\ta\ntest\nend\n"),
            CommandToken::HereDocument("this\nis\n\ta\ntest\n".to_string())
        );
        assert_eq!(
            lex_token("<<-end\nthis\nis\n\ta\n\t\t\t\ttest\nend\n"),
            CommandToken::HereDocument("this\nis\na\ntest\n".to_string())
        )
    }

    #[test]
    fn lex_io_number() {
        let mut lex = CommandLexer::new("123>");
        assert_eq!(lex.next_token().unwrap().0, CommandToken::IoNumber(123));
        let mut lex = CommandLexer::new("123");
        assert_eq!(
            lex.next_token().unwrap().0,
            CommandToken::Word("123".into())
        );
    }
}
