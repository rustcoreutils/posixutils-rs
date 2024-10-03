//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::str::CharIndices;

#[derive(Debug, Clone, PartialEq)]
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

    // number followed by a redirection operator
    IoNumber(u32),

    WordStart,

    EOF,
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

#[derive(Debug, Clone, PartialEq)]
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

pub struct Lexer<'src> {
    source: &'src str,
    source_iter: CharIndices<'src>,
    current_char_index: usize,
    lookahead: char,
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

    fn skip_blanks(&mut self) {
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

    pub fn next_shell_token(&mut self) -> ShellToken {
        self.skip_blanks();
        self.skip_comment();

        if self.reached_eof() {
            return ShellToken::EOF;
        }

        if let Some(partial_token) = char_to_operator_token(self.lookahead) {
            // multi-character operators all start with a single character
            // operator
            self.advance_char();
            let complete_token = match partial_token {
                ShellToken::And => match self.lookahead {
                    '&' => ShellToken::AndIf,
                    _ => ShellToken::And,
                },
                ShellToken::Pipe => match self.lookahead {
                    '|' => ShellToken::OrIf,
                    _ => ShellToken::Pipe,
                },
                ShellToken::SemiColon => match self.lookahead {
                    ';' => ShellToken::DSemi,
                    _ => ShellToken::SemiColon,
                },
                ShellToken::Less => match self.lookahead {
                    '&' => ShellToken::LessAnd,
                    '>' => ShellToken::LessGreat,
                    '<' => {
                        self.advance_char();
                        if self.lookahead == '-' {
                            ShellToken::DLessDash
                        } else {
                            ShellToken::DLess
                        }
                    }
                    _ => ShellToken::Less,
                },
                ShellToken::Greater => match self.lookahead {
                    '>' => ShellToken::DGreat,
                    '&' => ShellToken::GreatAnd,
                    '|' => ShellToken::Clobber,
                    _ => ShellToken::Greater,
                },
                other => other,
            };
            self.advance_char();
            return complete_token;
        }

        ShellToken::WordStart
    }

    pub fn next_word_token(&mut self) -> WordToken {
        if self.reached_eof() {
            return WordToken::EOF;
        }

        let result = match self.lookahead {
            '"' => WordToken::DoubleQuote,
            '\'' => WordToken::SingleQuote,
            '`' => WordToken::Backtick,
            '\\' => {
                self.advance_char();
                if self.lookahead == '`' {
                    WordToken::EscapedBacktick
                } else {
                    return WordToken::Char('\\');
                }
            }
            '$' => {
                self.advance_char();
                if self.lookahead == '(' {
                    self.advance_char();
                    if self.lookahead == '(' {
                        WordToken::ArithmeticExpansionStart
                    } else {
                        return WordToken::CommandSubstitutionStart;
                    }
                } else {
                    return WordToken::Dollar;
                }
            }
            other => WordToken::Char(other),
        };
        self.advance_char();
        result
    }

    pub fn next_char(&mut self) -> Option<char> {
        if self.reached_eof() {
            None
        } else {
            let c = self.lookahead;
            self.advance_char();
            Some(c)
        }
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
            source_iter,
            current_char_index: 0,
            lookahead,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_empty_string() {
        let mut lex = Lexer::new("");
        assert_eq!(lex.next_shell_token(), ShellToken::EOF);
    }

    #[test]
    fn lex_skip_comment() {
        let mut lex = Lexer::new("# this is a comment\n");
        assert_eq!(lex.next_shell_token(), ShellToken::Newline);
        assert_eq!(lex.next_shell_token(), ShellToken::EOF);
    }

    #[test]
    fn lex_operators() {
        let mut lex = Lexer::new("& ( ) ; \n | && || ;; < > >| << >> <& >& <<- <>");
        assert_eq!(lex.next_shell_token(), ShellToken::And);
        assert_eq!(lex.next_shell_token(), ShellToken::LParen);
        assert_eq!(lex.next_shell_token(), ShellToken::RParen);
        assert_eq!(lex.next_shell_token(), ShellToken::SemiColon);
        assert_eq!(lex.next_shell_token(), ShellToken::Newline);
        assert_eq!(lex.next_shell_token(), ShellToken::Pipe);
        assert_eq!(lex.next_shell_token(), ShellToken::AndIf);
        assert_eq!(lex.next_shell_token(), ShellToken::OrIf);
        assert_eq!(lex.next_shell_token(), ShellToken::DSemi);
        assert_eq!(lex.next_shell_token(), ShellToken::Less);
        assert_eq!(lex.next_shell_token(), ShellToken::Greater);
        assert_eq!(lex.next_shell_token(), ShellToken::Clobber);
        assert_eq!(lex.next_shell_token(), ShellToken::DLess);
        assert_eq!(lex.next_shell_token(), ShellToken::DGreat);
        assert_eq!(lex.next_shell_token(), ShellToken::LessAnd);
        assert_eq!(lex.next_shell_token(), ShellToken::GreatAnd);
        assert_eq!(lex.next_shell_token(), ShellToken::DLessDash);
        assert_eq!(lex.next_shell_token(), ShellToken::LessGreat);
        assert_eq!(lex.next_shell_token(), ShellToken::EOF);
    }

    #[test]
    fn lex_word() {
        let mut lex = Lexer::new("\"'$`\\`$($((a \n\t");
        assert_eq!(lex.next_word_token(), WordToken::DoubleQuote);
        assert_eq!(lex.next_word_token(), WordToken::SingleQuote);
        assert_eq!(lex.next_word_token(), WordToken::Dollar);
        assert_eq!(lex.next_word_token(), WordToken::Backtick);
        assert_eq!(lex.next_word_token(), WordToken::EscapedBacktick);
        assert_eq!(lex.next_word_token(), WordToken::CommandSubstitutionStart);
        assert_eq!(lex.next_word_token(), WordToken::ArithmeticExpansionStart);
        assert_eq!(lex.next_word_token(), WordToken::Char('a'));
        assert_eq!(lex.next_word_token(), WordToken::Char(' '));
        assert_eq!(lex.next_word_token(), WordToken::Char('\n'));
        assert_eq!(lex.next_word_token(), WordToken::Char('\t'));
        assert_eq!(lex.next_word_token(), WordToken::EOF);
    }
}
