//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::parse::lexer::Lexer;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use std::str::CharIndices;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WordToken<'src> {
    DoubleQuote,
    SingleQuote,
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
            WordToken::SingleQuote => write!(f, "'"),
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
    lex.advance();
    token
}

pub struct WordLexer<'src> {
    source: &'src str,
    iter: CharIndices<'src>,
    lookahead: char,
    position: usize,
    reached_eof: bool,
}

impl Lexer for WordLexer<'_> {
    fn advance(&mut self) {
        if let Some((pos, char)) = self.iter.next() {
            self.position = pos;
            self.lookahead = char;
        } else {
            self.reached_eof = true;
            self.lookahead = '\0';
        }
    }

    fn reached_eof(&self) -> bool {
        self.reached_eof
    }

    fn lookahead(&mut self) -> char {
        self.lookahead
    }

    fn line_no(&self) -> u32 {
        0
    }

    fn next_line(&mut self) -> Cow<str> {
        let start = self.position;
        while self.lookahead != '\n' {
            self.advance()
        }
        self.source[start..self.position].into()
    }
}

impl<'src> WordLexer<'src> {
    pub fn next_token(&mut self) -> WordToken<'src> {
        if self.reached_eof {
            return WordToken::EOF;
        }
        let result = match self.lookahead {
            '"' => advance_and_return(self, WordToken::DoubleQuote),
            '\'' => advance_and_return(self, WordToken::SingleQuote),
            '`' => {
                self.advance();
                let start = self.position;
                self.skip_backquoted_command_substitution()
                    .expect("invalid word");
                let end = self.position;
                self.advance();
                WordToken::CommandSubstitution(&self.source[start..end])
            }
            '\\' => advance_and_return(self, WordToken::Backslash),
            '$' => {
                self.advance();
                if self.lookahead == '(' {
                    self.advance();
                    if self.lookahead == '(' {
                        let start = self.position;
                        self.skip_arithmetic_expansion().expect("invalid word");
                        WordToken::ArithmeticExpansion(&self.source[start..self.position])
                    } else {
                        let start = self.position;
                        self.skip_command_substitution().expect("invalid word");
                        let end = self.position;
                        self.advance();
                        WordToken::CommandSubstitution(&self.source[start..end])
                    }
                } else {
                    WordToken::Dollar
                }
            }
            other => advance_and_return(self, WordToken::Char(other)),
        };
        result
    }

    pub fn next_char(&mut self) -> Option<char> {
        if self.reached_eof {
            None
        } else {
            let c = self.lookahead;
            self.advance();
            Some(c)
        }
    }

    pub fn new(source: &'src str) -> Self {
        let mut lexer = Self {
            source,
            iter: source.char_indices(),
            lookahead: '\0',
            position: 0,
            reached_eof: false,
        };
        lexer.advance();
        lexer
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_token(s: &str) -> WordToken {
        let mut lex = WordLexer::new(s);
        let token = lex.next_token();
        assert_eq!(lex.next_token(), WordToken::EOF);
        token
    }

    #[test]
    fn lex_command_substitution() {
        assert_eq!(lex_token("$()"), WordToken::CommandSubstitution(""));
        assert_eq!(lex_token("$(cmd)"), WordToken::CommandSubstitution("cmd"));
        assert_eq!(
            lex_token("$(cmd arg1 arg2)"),
            WordToken::CommandSubstitution("cmd arg1 arg2")
        );
        assert_eq!(
            lex_token("$(\ncmd1\ncmd2\ncmd3\n)"),
            WordToken::CommandSubstitution("\ncmd1\ncmd2\ncmd3\n")
        );
        assert_eq!(
            lex_token("$(#comment\ncmd)"),
            WordToken::CommandSubstitution("#comment\ncmd")
        );
        assert_eq!(
            lex_token("$(cmd $(cmd2))"),
            WordToken::CommandSubstitution("cmd $(cmd2)")
        );
    }

    #[test]
    fn lex_backtick_command_substitution() {
        assert_eq!(lex_token("``"), WordToken::CommandSubstitution(""));
        assert_eq!(lex_token("`cmd`"), WordToken::CommandSubstitution("cmd"));
        assert_eq!(
            lex_token("`cmd arg1 arg2`"),
            WordToken::CommandSubstitution("cmd arg1 arg2")
        );
        assert_eq!(
            lex_token("`\ncmd1\ncmd2\ncmd3\n`"),
            WordToken::CommandSubstitution("\ncmd1\ncmd2\ncmd3\n")
        );
        assert_eq!(
            lex_token("`#comment\ncmd`"),
            WordToken::CommandSubstitution("#comment\ncmd")
        );
        assert_eq!(
            lex_token("`cmd $(cmd2)`"),
            WordToken::CommandSubstitution("cmd $(cmd2)")
        );
    }
}
