//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::str::CharIndices;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ShellToken {
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
    DLess,     // >>
    DGreat,    // <<
    LessAnd,   // <&
    GreatAnd,  // >&
    DLessDash, // <<-
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

    WordStart,

    EOF,
}

impl ShellToken {
    pub fn is_reserved_word(&self) -> bool {
        match self {
            ShellToken::Bang
            | ShellToken::LBrace
            | ShellToken::RBrace
            | ShellToken::Case
            | ShellToken::Do
            | ShellToken::Done
            | ShellToken::Elif
            | ShellToken::Else
            | ShellToken::Esac
            | ShellToken::Fi
            | ShellToken::For
            | ShellToken::If
            | ShellToken::In
            | ShellToken::Then
            | ShellToken::Until
            | ShellToken::While => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WordToken {
    DoubleQuote,              // "
    SingleQuote,              // '
    Dollar,                   // $
    Backtick,                 // `
    EscapedBacktick,          // \`
    CommandSubstitutionStart, // $(
    ArithmeticExpansionStart, // $((

    Char(char),

    EOF,
}

pub enum ArithmeticToken {
    Plus,
    Minus,
    // ...
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct SourceLocation {
    pub start: usize,
    pub end: usize,
}

impl SourceLocation {
    fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    fn eof(source_len: usize) -> Self {
        Self {
            start: source_len,
            end: source_len,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Default)]
pub struct TokenId {
    id: u64,
}

pub fn is_blank(c: char) -> bool {
    c == ' ' || c == '\t'
}

fn char_to_operator_token(c: char) -> Option<ShellToken> {
    match c {
        '&' => Some(ShellToken::And),
        '(' => Some(ShellToken::LParen),
        ')' => Some(ShellToken::RParen),
        ';' => Some(ShellToken::SemiColon),
        '\n' => Some(ShellToken::Newline),
        '|' => Some(ShellToken::Pipe),
        '<' => Some(ShellToken::Less),
        '>' => Some(ShellToken::Greater),
        _ => None,
    }
}

pub fn is_operator(c: char) -> bool {
    char_to_operator_token(c).is_some()
}

fn advance_and_return<Tok>(lex: &mut Lexer, complete_token: Tok) -> Tok {
    lex.advance_char();
    complete_token
}

pub struct Lexer<'src> {
    source: &'src str,
    source_iter: CharIndices<'src>,
    current_char_index: usize,
    previous_token_iter: CharIndices<'src>,
    previous_token_lookahead: char,
    previous_token_char_index: usize,
    lookahead: char,
    last_token_id: u64,
}

impl<'src> Lexer<'src> {
    fn reached_eof(&self) -> bool {
        self.current_char_index == self.source.len()
    }

    fn advance_char(&mut self) {
        if let Some((n, c)) = self.source_iter.next() {
            self.lookahead = c;
            self.current_char_index = n;
        } else {
            self.lookahead = '\0';
            self.current_char_index = self.source.len();
        }
    }

    pub fn skip_blanks(&mut self) {
        while is_blank(self.lookahead) {
            self.advance_char();
        }
    }

    fn skip_comment(&mut self) {
        if self.lookahead == '#' {
            while self.lookahead != '\n' {
                self.advance_char();
            }
        }
    }

    fn token_id(&mut self) -> TokenId {
        let token_id = self.last_token_id;
        self.last_token_id += 1;
        TokenId { id: token_id }
    }

    pub fn next_shell_token(&mut self) -> (ShellToken, SourceLocation, TokenId) {
        self.previous_token_iter = self.source_iter.clone();
        self.previous_token_lookahead = self.lookahead;
        self.previous_token_char_index = self.current_char_index;

        self.skip_blanks();
        self.skip_comment();

        let token_id = self.token_id();

        if self.reached_eof() {
            return (
                ShellToken::EOF,
                SourceLocation::eof(self.source.len()),
                token_id,
            );
        }

        let token_start = self.current_char_index;

        if let Some(partial_token) = char_to_operator_token(self.lookahead) {
            // multi-character operators all start with a single character
            // operator
            self.advance_char();

            let complete_token = match partial_token {
                ShellToken::And => match self.lookahead {
                    '&' => advance_and_return(self, ShellToken::AndIf),
                    _ => ShellToken::And,
                },
                ShellToken::Pipe => match self.lookahead {
                    '|' => advance_and_return(self, ShellToken::OrIf),
                    _ => ShellToken::Pipe,
                },
                ShellToken::SemiColon => match self.lookahead {
                    ';' => advance_and_return(self, ShellToken::DSemi),
                    _ => ShellToken::SemiColon,
                },
                ShellToken::Less => match self.lookahead {
                    '&' => advance_and_return(self, ShellToken::LessAnd),
                    '>' => advance_and_return(self, ShellToken::LessGreat),
                    '<' => {
                        self.advance_char();
                        if self.lookahead == '-' {
                            advance_and_return(self, ShellToken::DLessDash)
                        } else {
                            ShellToken::DLess
                        }
                    }
                    _ => ShellToken::Less,
                },
                ShellToken::Greater => match self.lookahead {
                    '>' => advance_and_return(self, ShellToken::DGreat),
                    '&' => advance_and_return(self, ShellToken::GreatAnd),
                    '|' => advance_and_return(self, ShellToken::Clobber),
                    _ => ShellToken::Greater,
                },
                other => other,
            };
            return (
                complete_token,
                SourceLocation::new(token_start, self.current_char_index),
                token_id,
            );
        }

        let token = match self.lookahead {
            '!' => advance_and_return(self, ShellToken::Bang),
            '{' => advance_and_return(self, ShellToken::LBrace),
            '}' => advance_and_return(self, ShellToken::RBrace),
            d if d.is_digit(10) => {
                let start = self.current_char_index;
                let previous_iter_value = self.source_iter.clone();
                let previous_lookahead_value = self.lookahead;

                let mut number = d.to_digit(10).unwrap();
                self.advance_char();
                // TODO: make this robust to integer overflow
                while let Some(d) = self.lookahead.to_digit(10) {
                    number *= 10;
                    number += d;
                    self.advance_char();
                }
                if self.lookahead == '>' || self.lookahead == '<' {
                    ShellToken::IoNumber(number)
                } else {
                    self.current_char_index = start;
                    self.source_iter = previous_iter_value;
                    self.lookahead = previous_lookahead_value;
                    ShellToken::WordStart
                }
            }
            other if other.is_alphabetic() => {
                let start = self.current_char_index;
                let previous_iter_value = self.source_iter.clone();
                let previous_lookahead_value = self.lookahead;

                while self.lookahead.is_alphabetic() {
                    self.advance_char();
                }
                let word = &self.source[start..self.current_char_index];
                match word {
                    "case" => ShellToken::Case,
                    "do" => ShellToken::Do,
                    "done" => ShellToken::Done,
                    "elif" => ShellToken::Elif,
                    "else" => ShellToken::Else,
                    "esac" => ShellToken::Esac,
                    "fi" => ShellToken::Fi,
                    "for" => ShellToken::For,
                    "if" => ShellToken::If,
                    "in" => ShellToken::In,
                    "then" => ShellToken::Then,
                    "until" => ShellToken::Until,
                    "while" => ShellToken::While,
                    _ => {
                        self.current_char_index = start;
                        self.source_iter = previous_iter_value;
                        self.lookahead = previous_lookahead_value;
                        ShellToken::WordStart
                    }
                }
            }
            _ => ShellToken::WordStart,
        };
        let source_location = SourceLocation::new(token_start, self.current_char_index);
        (token, source_location, token_id)
    }

    pub fn next_word_token(&mut self) -> (WordToken, SourceLocation, TokenId) {
        self.previous_token_iter = self.source_iter.clone();
        self.previous_token_lookahead = self.lookahead;
        self.previous_token_char_index = self.current_char_index;

        let token_id = self.token_id();

        if self.reached_eof() {
            return (
                WordToken::EOF,
                SourceLocation::eof(self.source.len()),
                token_id,
            );
        }
        let token_start = self.current_char_index;
        let result = match self.lookahead {
            '"' => advance_and_return(self, WordToken::DoubleQuote),
            '\'' => advance_and_return(self, WordToken::SingleQuote),
            '`' => advance_and_return(self, WordToken::Backtick),
            '\\' => {
                self.advance_char();
                if self.lookahead == '`' {
                    advance_and_return(self, WordToken::EscapedBacktick)
                } else {
                    WordToken::Char('\\')
                }
            }
            '$' => {
                self.advance_char();
                if self.lookahead == '(' {
                    self.advance_char();
                    if self.lookahead == '(' {
                        advance_and_return(self, WordToken::ArithmeticExpansionStart)
                    } else {
                        WordToken::CommandSubstitutionStart
                    }
                } else {
                    WordToken::Dollar
                }
            }
            other => advance_and_return(self, WordToken::Char(other)),
        };
        let source_location = SourceLocation::new(token_start, self.current_char_index);
        (result, source_location, token_id)
    }

    pub fn next_char(&mut self) -> Option<(char, SourceLocation)> {
        if self.reached_eof() {
            None
        } else {
            let c = self.lookahead;
            self.advance_char();
            Some((
                c,
                SourceLocation::new(self.current_char_index - 1, self.current_char_index),
            ))
        }
    }

    pub fn next_line(&mut self) -> (&'src str, SourceLocation) {
        let start = self.current_char_index;
        while self.lookahead != '\n' && !self.reached_eof() {
            self.advance_char();
        }
        self.advance_char();
        let end = self.current_char_index;
        (&self.source[start..end], SourceLocation::new(start, end))
    }

    pub fn rollback_last_token(&mut self) {
        self.source_iter = self.previous_token_iter.clone();
        self.lookahead = self.previous_token_lookahead;
        self.current_char_index = self.previous_token_char_index;
    }

    pub fn new(source: &'src str) -> Self {
        let mut source_iter = source.char_indices();
        let lookahead;
        if let Some((_, c)) = source_iter.next() {
            lookahead = c;
        } else {
            lookahead = '\0';
        }
        Self {
            source,
            previous_token_iter: source_iter.clone(),
            previous_token_char_index: 0,
            previous_token_lookahead: lookahead,
            source_iter,
            current_char_index: 0,
            lookahead,
            last_token_id: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_empty_string() {
        let mut lex = Lexer::new("");
        assert_eq!(lex.next_shell_token().0, ShellToken::EOF);
    }

    #[test]
    fn lex_skip_comment() {
        let mut lex = Lexer::new("# this is a comment\n");
        assert_eq!(lex.next_shell_token().0, ShellToken::Newline);
        assert_eq!(lex.next_shell_token().0, ShellToken::EOF);
    }

    #[test]
    fn lex_operators() {
        let mut lex = Lexer::new("&();\n|&&||;;< > >| << >><&>&<<-<>");
        assert_eq!(lex.next_shell_token().0, ShellToken::And);
        assert_eq!(lex.next_shell_token().0, ShellToken::LParen);
        assert_eq!(lex.next_shell_token().0, ShellToken::RParen);
        assert_eq!(lex.next_shell_token().0, ShellToken::SemiColon);
        assert_eq!(lex.next_shell_token().0, ShellToken::Newline);
        assert_eq!(lex.next_shell_token().0, ShellToken::Pipe);
        assert_eq!(lex.next_shell_token().0, ShellToken::AndIf);
        assert_eq!(lex.next_shell_token().0, ShellToken::OrIf);
        assert_eq!(lex.next_shell_token().0, ShellToken::DSemi);
        assert_eq!(lex.next_shell_token().0, ShellToken::Less);
        assert_eq!(lex.next_shell_token().0, ShellToken::Greater);
        assert_eq!(lex.next_shell_token().0, ShellToken::Clobber);
        assert_eq!(lex.next_shell_token().0, ShellToken::DLess);
        assert_eq!(lex.next_shell_token().0, ShellToken::DGreat);
        assert_eq!(lex.next_shell_token().0, ShellToken::LessAnd);
        assert_eq!(lex.next_shell_token().0, ShellToken::GreatAnd);
        assert_eq!(lex.next_shell_token().0, ShellToken::DLessDash);
        assert_eq!(lex.next_shell_token().0, ShellToken::LessGreat);
        assert_eq!(lex.next_shell_token().0, ShellToken::EOF);
    }

    #[test]
    fn test_lex_reserved_words() {
        let mut lex = Lexer::new("! { } case do done elif else esac fi for if in then until while");
        assert_eq!(lex.next_shell_token().0, ShellToken::Bang);
        assert_eq!(lex.next_shell_token().0, ShellToken::LBrace);
        assert_eq!(lex.next_shell_token().0, ShellToken::RBrace);
        assert_eq!(lex.next_shell_token().0, ShellToken::Case);
        assert_eq!(lex.next_shell_token().0, ShellToken::Do);
        assert_eq!(lex.next_shell_token().0, ShellToken::Done);
        assert_eq!(lex.next_shell_token().0, ShellToken::Elif);
        assert_eq!(lex.next_shell_token().0, ShellToken::Else);
        assert_eq!(lex.next_shell_token().0, ShellToken::Esac);
        assert_eq!(lex.next_shell_token().0, ShellToken::Fi);
        assert_eq!(lex.next_shell_token().0, ShellToken::For);
        assert_eq!(lex.next_shell_token().0, ShellToken::If);
        assert_eq!(lex.next_shell_token().0, ShellToken::In);
        assert_eq!(lex.next_shell_token().0, ShellToken::Then);
        assert_eq!(lex.next_shell_token().0, ShellToken::Until);
        assert_eq!(lex.next_shell_token().0, ShellToken::While);
        assert_eq!(lex.next_shell_token().0, ShellToken::EOF);
    }

    #[test]
    fn lex_io_number() {
        let mut lex = Lexer::new("123>");
        assert_eq!(lex.next_shell_token().0, ShellToken::IoNumber(123));
        let mut lex = Lexer::new("123");
        assert_eq!(lex.next_shell_token().0, ShellToken::WordStart);
    }

    #[test]
    fn lex_word() {
        let mut lex = Lexer::new("\"'$`\\`$($((a \n\t");
        assert_eq!(lex.next_word_token().0, WordToken::DoubleQuote);
        assert_eq!(lex.next_word_token().0, WordToken::SingleQuote);
        assert_eq!(lex.next_word_token().0, WordToken::Dollar);
        assert_eq!(lex.next_word_token().0, WordToken::Backtick);
        assert_eq!(lex.next_word_token().0, WordToken::EscapedBacktick);
        assert_eq!(lex.next_word_token().0, WordToken::CommandSubstitutionStart);
        assert_eq!(lex.next_word_token().0, WordToken::ArithmeticExpansionStart);
        assert_eq!(lex.next_word_token().0, WordToken::Char('a'));
        assert_eq!(lex.next_word_token().0, WordToken::Char(' '));
        assert_eq!(lex.next_word_token().0, WordToken::Char('\n'));
        assert_eq!(lex.next_word_token().0, WordToken::Char('\t'));
        assert_eq!(lex.next_word_token().0, WordToken::EOF);
    }
}
