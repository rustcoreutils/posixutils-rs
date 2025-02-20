//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fmt::{Display, Formatter};
use std::str::CharIndices;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WordToken<'src> {
    DoubleQuote,
    SingleQuotedString(&'src str),
    Dollar,
    Backslash,
    CommandSubstitution(&'src str),
    ArithmeticExpansion(&'src str),

    Char(char),

    EOF,
}

impl Display for WordToken<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            WordToken::DoubleQuote => write!(f, "'\"'"),
            WordToken::SingleQuotedString(str) => write!(f, "\'{str}\'"),
            WordToken::Dollar => write!(f, "'$'"),
            WordToken::Backslash => write!(f, "'\\'"),
            WordToken::CommandSubstitution(str) => write!(f, "'$({str})'"),
            WordToken::ArithmeticExpansion(str) => write!(f, "'$(({str}))'"),
            WordToken::Char(c) => write!(f, "'{}'", c),
            WordToken::EOF => write!(f, "'EOF'"),
        }
    }
}

fn advance_and_return<'a>(lex: &mut WordLexer, token: WordToken<'a>) -> WordToken<'a> {
    lex.advance_char();
    token
}

pub struct WordLexer<'src> {
    source: &'src str,
    iter: CharIndices<'src>,
    lookahead: char,
    position: usize,
    reached_eof: bool,
}

impl<'src> WordLexer<'src> {
    fn advance_char(&mut self) {
        if let Some((pos, char)) = self.iter.next() {
            self.position = pos;
            self.lookahead = char;
        } else {
            self.reached_eof = true;
            self.lookahead = '\0';
        }
    }

    pub fn next_token(&mut self) -> WordToken<'src> {
        if self.reached_eof {
            return WordToken::EOF;
        }
        let result = match self.lookahead {
            '"' => advance_and_return(self, WordToken::DoubleQuote),
            '\'' => {
                self.advance_char();
                let start = self.position;
                loop {
                    if self.reached_eof {
                        panic!("invalid word");
                    }
                    if self.lookahead == '\'' {
                        break;
                    }
                    self.advance_char();
                }
                let end = self.position;
                self.advance_char();
                WordToken::SingleQuotedString(&self.source[start..end])
            }
            '`' => todo!(),
            '\\' => advance_and_return(self, WordToken::Backslash),
            '$' => {
                self.advance_char();
                if self.lookahead == '(' {
                    self.advance_char();
                    if self.lookahead == '(' {
                        todo!()
                    } else {
                        todo!()
                    }
                } else {
                    WordToken::Dollar
                }
            }
            other => advance_and_return(self, WordToken::Char(other)),
        };
        result
    }

    pub fn new(source: &'src str) -> Self {
        let mut lexer = Self {
            source,
            iter: source.char_indices(),
            lookahead: '\0',
            position: 0,
            reached_eof: false,
        };
        lexer.advance_char();
        lexer
    }
}
