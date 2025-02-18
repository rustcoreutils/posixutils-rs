//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

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

impl Display for ShellToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ShellToken::And => write!(f, "'&'"),
            ShellToken::LParen => write!(f, "'('"),
            ShellToken::RParen => write!(f, "')'"),
            ShellToken::SemiColon => write!(f, "';'"),
            ShellToken::Newline => write!(f, "'\\n'"),
            ShellToken::Pipe => write!(f, "'|'"),
            ShellToken::AndIf => write!(f, "'&&'"),
            ShellToken::OrIf => write!(f, "'||'"),
            ShellToken::DSemi => write!(f, "';;'"),
            ShellToken::Less => write!(f, "'<'"),
            ShellToken::Greater => write!(f, "'>'"),
            ShellToken::Clobber => write!(f, "'>|'"),
            ShellToken::DLess => write!(f, "'<<'"),
            ShellToken::DGreat => write!(f, "'>>'"),
            ShellToken::LessAnd => write!(f, "'<&'"),
            ShellToken::GreatAnd => write!(f, "'>&'"),
            ShellToken::DLessDash => write!(f, "'<<-'"),
            ShellToken::LessGreat => write!(f, "'<>'"),
            ShellToken::Bang => write!(f, "'!'"),
            ShellToken::LBrace => write!(f, "'{{'"),
            ShellToken::RBrace => write!(f, "'}}'"),
            ShellToken::Case => write!(f, "case"),
            ShellToken::Do => write!(f, "do"),
            ShellToken::Done => write!(f, "done"),
            ShellToken::Elif => write!(f, "elif"),
            ShellToken::Else => write!(f, "else"),
            ShellToken::Esac => write!(f, "esac"),
            ShellToken::Fi => write!(f, "fi"),
            ShellToken::For => write!(f, "for"),
            ShellToken::If => write!(f, "if"),
            ShellToken::In => write!(f, "in"),
            ShellToken::Then => write!(f, "then"),
            ShellToken::Until => write!(f, "until"),
            ShellToken::While => write!(f, "while"),
            ShellToken::IoNumber(_) => write!(f, "number"),
            ShellToken::WordStart => write!(f, "word"),
            ShellToken::EOF => write!(f, "EOF"),
        }
    }
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
    Backslash,                // \
    CommandSubstitutionStart, // $(
    ArithmeticExpansionStart, // $((

    Char(char),

    EOF,
}

impl Display for WordToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            WordToken::DoubleQuote => write!(f, "'\"'"),
            WordToken::SingleQuote => write!(f, "\'"),
            WordToken::Dollar => write!(f, "'$'"),
            WordToken::Backtick => write!(f, "'`'"),
            WordToken::EscapedBacktick => write!(f, "'\\`'"),
            WordToken::Backslash => write!(f, "'\\'"),
            WordToken::CommandSubstitutionStart => write!(f, "'$('"),
            WordToken::ArithmeticExpansionStart => write!(f, "'$(('"),
            WordToken::Char(c) => write!(f, "'{}'", c),
            WordToken::EOF => write!(f, "'EOF'"),
        }
    }
}

pub enum ArithmeticToken {
    Plus,
    Minus,
    // ...
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
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
    lex.source.advance_char();
    complete_token
}

pub struct Lexer<'src> {
    source: SourceString<'src>,
    prev_read_state: SourceReadState<'src>,
    last_token_id: u64,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Token<T> {
    pub value: T,
    pub line_no: u32,
    pub id: TokenId,
}

impl<'src> Lexer<'src> {
    fn reached_eof(&mut self) -> bool {
        self.source.reached_eof()
    }

    /// skips blank characters (space and tab)
    pub fn skip_blanks(&mut self) {
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

    fn token_id(&mut self) -> TokenId {
        let token_id = self.last_token_id;
        self.last_token_id += 1;
        TokenId { id: token_id }
    }

    fn save_state(&mut self) {
        self.prev_read_state = self.source.read_state.clone();
    }

    pub fn next_shell_token(&mut self) -> Token<ShellToken> {
        self.save_state();
        self.skip_blanks();
        self.skip_comment();

        let token_id = self.token_id();
        let line_no = self.source.line_no();

        if self.reached_eof() {
            return Token {
                value: ShellToken::EOF,
                line_no,
                id: token_id,
            };
        }

        if let Some(partial_token) = char_to_operator_token(self.source.lookahead()) {
            // multi-character operators all start with a single character
            // operator
            self.source.advance_char();

            let complete_token = match partial_token {
                ShellToken::And => match self.source.lookahead() {
                    '&' => advance_and_return(self, ShellToken::AndIf),
                    _ => ShellToken::And,
                },
                ShellToken::Pipe => match self.source.lookahead() {
                    '|' => advance_and_return(self, ShellToken::OrIf),
                    _ => ShellToken::Pipe,
                },
                ShellToken::SemiColon => match self.source.lookahead() {
                    ';' => advance_and_return(self, ShellToken::DSemi),
                    _ => ShellToken::SemiColon,
                },
                ShellToken::Less => match self.source.lookahead() {
                    '&' => advance_and_return(self, ShellToken::LessAnd),
                    '>' => advance_and_return(self, ShellToken::LessGreat),
                    '<' => {
                        self.source.advance_char();
                        if self.source.lookahead() == '-' {
                            advance_and_return(self, ShellToken::DLessDash)
                        } else {
                            ShellToken::DLess
                        }
                    }
                    _ => ShellToken::Less,
                },
                ShellToken::Greater => match self.source.lookahead() {
                    '>' => advance_and_return(self, ShellToken::DGreat),
                    '&' => advance_and_return(self, ShellToken::GreatAnd),
                    '|' => advance_and_return(self, ShellToken::Clobber),
                    _ => ShellToken::Greater,
                },
                other => other,
            };
            return Token {
                value: complete_token,
                line_no,
                id: token_id,
            };
        }

        let token = match self.source.lookahead() {
            '!' => advance_and_return(self, ShellToken::Bang),
            '{' => advance_and_return(self, ShellToken::LBrace),
            '}' => advance_and_return(self, ShellToken::RBrace),
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
                    ShellToken::IoNumber(number)
                } else {
                    self.source.read_state = prev_read_state;
                    ShellToken::WordStart
                }
            }
            other if other.is_alphabetic() => {
                let prev_read_state = self.source.read_state.clone();

                const KEYWORD_MAX_LEN: usize = 5;
                let mut buf = [b'\0'; KEYWORD_MAX_LEN];
                let mut last_char = 0;
                while self.source.lookahead().is_ascii_alphabetic() && last_char < KEYWORD_MAX_LEN {
                    buf[last_char] = self.source.lookahead() as u8;
                    last_char += 1;
                    self.source.advance_char();
                }
                match std::str::from_utf8(&buf[0..last_char]).unwrap() {
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
                        self.source.read_state = prev_read_state;
                        ShellToken::WordStart
                    }
                }
            }
            _ => ShellToken::WordStart,
        };
        Token {
            value: token,
            line_no,
            id: token_id,
        }
    }

    pub fn next_word_token(&mut self) -> Token<WordToken> {
        self.save_state();
        let token_id = self.token_id();
        let line_no = self.source.line_no();
        if self.reached_eof() {
            return Token {
                value: WordToken::EOF,
                line_no,
                id: token_id,
            };
        }
        let result = match self.source.lookahead() {
            '"' => advance_and_return(self, WordToken::DoubleQuote),
            '\'' => advance_and_return(self, WordToken::SingleQuote),
            '`' => advance_and_return(self, WordToken::Backtick),
            '\\' => {
                self.source.advance_char();
                if self.source.lookahead() == '`' {
                    advance_and_return(self, WordToken::EscapedBacktick)
                } else {
                    WordToken::Backslash
                }
            }
            '$' => {
                self.source.advance_char();
                if self.source.lookahead() == '(' {
                    self.source.advance_char();
                    if self.source.lookahead() == '(' {
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
        Token {
            value: result,
            line_no,
            id: token_id,
        }
    }

    pub fn next_char(&mut self) -> Option<char> {
        if self.reached_eof() {
            None
        } else {
            let c = self.source.lookahead();
            self.source.advance_char();
            Some(c)
        }
    }

    /// Returns the next line in the source string, including the newline character
    pub fn next_line(&mut self) -> String {
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

    /// Rolls back the last read token. Calling this method multiple times
    /// without reading a new token will have no effect.
    pub fn rollback_last_token(&mut self) {
        self.source.read_state = self.prev_read_state.clone();
    }

    /// Inserts text after the last returned token
    pub fn insert_text_at_current_position(&mut self, text: &'src str) {
        self.source.insert_string_after_last_char(text);
    }

    pub fn new(source: &'src str) -> Self {
        let source = SourceString::new(source);
        let initial_read_state = source.read_state.clone();
        let mut lexer = Self {
            source,
            prev_read_state: initial_read_state,
            last_token_id: 0,
        };
        lexer
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_empty_string() {
        let mut lex = Lexer::new("");
        assert_eq!(lex.next_shell_token().value, ShellToken::EOF);
    }

    #[test]
    fn lex_skip_comment() {
        let mut lex = Lexer::new("# this is a comment\n");
        assert_eq!(lex.next_shell_token().value, ShellToken::Newline);
        assert_eq!(lex.next_shell_token().value, ShellToken::EOF);
    }

    #[test]
    fn lex_operators() {
        let mut lex = Lexer::new("&();\n|&&||;;< > >| << >><&>&<<-<>");
        assert_eq!(lex.next_shell_token().value, ShellToken::And);
        assert_eq!(lex.next_shell_token().value, ShellToken::LParen);
        assert_eq!(lex.next_shell_token().value, ShellToken::RParen);
        assert_eq!(lex.next_shell_token().value, ShellToken::SemiColon);
        assert_eq!(lex.next_shell_token().value, ShellToken::Newline);
        assert_eq!(lex.next_shell_token().value, ShellToken::Pipe);
        assert_eq!(lex.next_shell_token().value, ShellToken::AndIf);
        assert_eq!(lex.next_shell_token().value, ShellToken::OrIf);
        assert_eq!(lex.next_shell_token().value, ShellToken::DSemi);
        assert_eq!(lex.next_shell_token().value, ShellToken::Less);
        assert_eq!(lex.next_shell_token().value, ShellToken::Greater);
        assert_eq!(lex.next_shell_token().value, ShellToken::Clobber);
        assert_eq!(lex.next_shell_token().value, ShellToken::DLess);
        assert_eq!(lex.next_shell_token().value, ShellToken::DGreat);
        assert_eq!(lex.next_shell_token().value, ShellToken::LessAnd);
        assert_eq!(lex.next_shell_token().value, ShellToken::GreatAnd);
        assert_eq!(lex.next_shell_token().value, ShellToken::DLessDash);
        assert_eq!(lex.next_shell_token().value, ShellToken::LessGreat);
        assert_eq!(lex.next_shell_token().value, ShellToken::EOF);
    }

    #[test]
    fn test_lex_reserved_words() {
        let mut lex = Lexer::new("! { } case do done elif else esac fi for if in then until while");
        assert_eq!(lex.next_shell_token().value, ShellToken::Bang);
        assert_eq!(lex.next_shell_token().value, ShellToken::LBrace);
        assert_eq!(lex.next_shell_token().value, ShellToken::RBrace);
        assert_eq!(lex.next_shell_token().value, ShellToken::Case);
        assert_eq!(lex.next_shell_token().value, ShellToken::Do);
        assert_eq!(lex.next_shell_token().value, ShellToken::Done);
        assert_eq!(lex.next_shell_token().value, ShellToken::Elif);
        assert_eq!(lex.next_shell_token().value, ShellToken::Else);
        assert_eq!(lex.next_shell_token().value, ShellToken::Esac);
        assert_eq!(lex.next_shell_token().value, ShellToken::Fi);
        assert_eq!(lex.next_shell_token().value, ShellToken::For);
        assert_eq!(lex.next_shell_token().value, ShellToken::If);
        assert_eq!(lex.next_shell_token().value, ShellToken::In);
        assert_eq!(lex.next_shell_token().value, ShellToken::Then);
        assert_eq!(lex.next_shell_token().value, ShellToken::Until);
        assert_eq!(lex.next_shell_token().value, ShellToken::While);
        assert_eq!(lex.next_shell_token().value, ShellToken::EOF);
    }

    #[test]
    fn lex_io_number() {
        let mut lex = Lexer::new("123>");
        assert_eq!(lex.next_shell_token().value, ShellToken::IoNumber(123));
        let mut lex = Lexer::new("123");
        assert_eq!(lex.next_shell_token().value, ShellToken::WordStart);
    }

    #[test]
    fn lex_word() {
        let mut lex = Lexer::new("\"'$`\\`$($((a \n\t");
        assert_eq!(lex.next_word_token().value, WordToken::DoubleQuote);
        assert_eq!(lex.next_word_token().value, WordToken::SingleQuote);
        assert_eq!(lex.next_word_token().value, WordToken::Dollar);
        assert_eq!(lex.next_word_token().value, WordToken::Backtick);
        assert_eq!(lex.next_word_token().value, WordToken::EscapedBacktick);
        assert_eq!(
            lex.next_word_token().value,
            WordToken::CommandSubstitutionStart
        );
        assert_eq!(
            lex.next_word_token().value,
            WordToken::ArithmeticExpansionStart
        );
        assert_eq!(lex.next_word_token().value, WordToken::Char('a'));
        assert_eq!(lex.next_word_token().value, WordToken::Char(' '));
        assert_eq!(lex.next_word_token().value, WordToken::Char('\n'));
        assert_eq!(lex.next_word_token().value, WordToken::Char('\t'));
        assert_eq!(lex.next_word_token().value, WordToken::EOF);
    }

    #[test]
    fn skip_blanks() {
        let mut lex = Lexer::new(" \ta");
        lex.skip_blanks();
        assert_eq!(lex.next_word_token().value, WordToken::Char('a'));
    }

    #[test]
    fn next_char() {
        let mut lex = Lexer::new("abc");
        assert_eq!(lex.next_char(), Some('a'));
        assert_eq!(lex.next_char(), Some('b'));
        assert_eq!(lex.next_char(), Some('c'));
        assert_eq!(lex.next_char(), None);
    }

    #[test]
    fn next_line() {
        let mut lex = Lexer::new("abc\ndef\n");
        assert_eq!(lex.next_line(), "abc\n");
        assert_eq!(lex.next_line(), "def\n");
        assert_eq!(lex.next_line(), "");
    }

    #[test]
    fn rollback_single_char_word_token() {
        let mut lex = Lexer::new("a$c");
        assert_eq!(
            lex.next_word_token(),
            Token {
                value: WordToken::Char('a'),
                line_no: 1,
                id: TokenId { id: 0 }
            }
        );
        lex.rollback_last_token();
        assert_eq!(
            lex.next_word_token(),
            Token {
                value: WordToken::Char('a'),
                line_no: 1,
                id: TokenId { id: 1 }
            }
        );
        assert_eq!(
            lex.next_word_token(),
            Token {
                value: WordToken::Dollar,
                line_no: 1,
                id: TokenId { id: 2 }
            }
        );
        assert_eq!(
            lex.next_word_token(),
            Token {
                value: WordToken::Char('c'),
                line_no: 1,
                id: TokenId { id: 3 }
            }
        );
        lex.rollback_last_token();
        assert_eq!(
            lex.next_word_token(),
            Token {
                value: WordToken::Char('c'),
                line_no: 1,
                id: TokenId { id: 4 }
            }
        );
        assert_eq!(
            lex.next_word_token(),
            Token {
                value: WordToken::EOF,
                line_no: 1,
                id: TokenId { id: 5 }
            }
        );
        lex.rollback_last_token();
        assert_eq!(
            lex.next_word_token(),
            Token {
                value: WordToken::EOF,
                line_no: 1,
                id: TokenId { id: 6 }
            }
        );
    }

    #[test]
    fn rollback_multi_char_word_token() {
        let mut lex = Lexer::new("$($((");
        assert_eq!(
            lex.next_word_token(),
            Token {
                value: WordToken::CommandSubstitutionStart,
                line_no: 1,
                id: TokenId { id: 0 }
            }
        );
        lex.rollback_last_token();
        assert_eq!(
            lex.next_word_token(),
            Token {
                value: WordToken::CommandSubstitutionStart,
                line_no: 1,
                id: TokenId { id: 1 }
            }
        );
        assert_eq!(
            lex.next_word_token(),
            Token {
                value: WordToken::ArithmeticExpansionStart,
                line_no: 1,
                id: TokenId { id: 2 }
            }
        );
        lex.rollback_last_token();
        assert_eq!(
            lex.next_word_token(),
            Token {
                value: WordToken::ArithmeticExpansionStart,
                line_no: 1,
                id: TokenId { id: 3 }
            }
        );
    }

    #[test]
    fn rollback_shell_token() {
        let mut lex = Lexer::new("&&()");
        assert_eq!(
            lex.next_shell_token(),
            Token {
                value: ShellToken::AndIf,
                line_no: 1,
                id: TokenId { id: 0 }
            }
        );
        lex.rollback_last_token();
        assert_eq!(
            lex.next_shell_token(),
            Token {
                value: ShellToken::AndIf,
                line_no: 1,
                id: TokenId { id: 1 }
            }
        );
        assert_eq!(
            lex.next_shell_token(),
            Token {
                value: ShellToken::LParen,
                line_no: 1,
                id: TokenId { id: 2 }
            }
        );
        assert_eq!(
            lex.next_shell_token(),
            Token {
                value: ShellToken::RParen,
                line_no: 1,
                id: TokenId { id: 3 }
            }
        );
        lex.rollback_last_token();
        assert_eq!(
            lex.next_shell_token(),
            Token {
                value: ShellToken::RParen,
                line_no: 1,
                id: TokenId { id: 4 }
            }
        );
        assert_eq!(
            lex.next_shell_token(),
            Token {
                value: ShellToken::EOF,
                line_no: 1,
                id: TokenId { id: 5 }
            }
        );
    }

    #[test]
    fn rollback_mixed_tokens() {
        let mut lex = Lexer::new("a && b");
        assert_eq!(lex.next_shell_token().value, ShellToken::WordStart);
        assert_eq!(lex.next_word_token().value, WordToken::Char('a'));
        assert_eq!(lex.next_word_token().value, WordToken::Char(' '));
        lex.rollback_last_token();
        assert_eq!(lex.next_shell_token().value, ShellToken::AndIf);
        assert_eq!(lex.next_shell_token().value, ShellToken::WordStart);
        assert_eq!(lex.next_word_token().value, WordToken::Char('b'));
    }

    #[test]
    fn insert_text() {
        let mut lex = Lexer::new("a && c");
        assert_eq!(lex.next_word_token().value, WordToken::Char('a'));
        lex.insert_text_at_current_position("d");
        assert_eq!(lex.next_word_token().value, WordToken::Char('d'));
        assert_eq!(lex.next_shell_token().value, ShellToken::AndIf);
        assert_eq!(lex.next_shell_token().value, ShellToken::WordStart);
        assert_eq!(lex.next_word_token().value, WordToken::Char('c'));
        assert_eq!(lex.next_word_token().value, WordToken::EOF);
    }
}
