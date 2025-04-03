//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use std::str::CharIndices;

use crate::parse::lexer::Lexer;
use crate::parse::ParseResult;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WordToken<'src> {
    DoubleQuote,
    SingleQuote,
    Dollar,
    Backslash,
    // this needs to be a standalone token, otherwise we would get
    // `Backslash` and then we would try to lex `BacktickCommandSubstitution`
    QuotedBacktick,
    CommandSubstitution(&'src str),
    BacktickCommandSubstitution(&'src str),
    ArithmeticExpansion(&'src str),

    Char(char),

    Eof,
}

impl Display for WordToken<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            WordToken::DoubleQuote => write!(f, "'\"'"),
            WordToken::SingleQuote => write!(f, "'"),
            WordToken::Dollar => write!(f, "'$'"),
            WordToken::Backslash => write!(f, "'\\'"),
            WordToken::QuotedBacktick => write!(f, "'\\`'"),
            WordToken::CommandSubstitution(str) => write!(f, "'$({str})'"),
            WordToken::BacktickCommandSubstitution(str) => write!(f, "`{str}`"),
            WordToken::ArithmeticExpansion(str) => write!(f, "'$(({str}))'"),
            WordToken::Char(c) => write!(f, "'{}'", c),
            WordToken::Eof => write!(f, "'EOF'"),
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

    fn next_word(&mut self) -> ParseResult<Cow<str>> {
        let start = self.position;
        self.skip_word_token(None, false)?;
        Ok(Cow::from(&self.source[start..self.position]))
    }
}

impl<'src> WordLexer<'src> {
    pub fn next_token(&mut self) -> WordToken<'src> {
        if self.reached_eof {
            return WordToken::Eof;
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
                WordToken::BacktickCommandSubstitution(&self.source[start..end])
            }
            '\\' => {
                self.advance();
                match self.lookahead {
                    '`' => advance_and_return(self, WordToken::QuotedBacktick),
                    '\n' => {
                        self.advance();
                        self.next_token()
                    }
                    _ => WordToken::Backslash,
                }
            }
            '$' => {
                self.advance();
                if self.lookahead == '(' {
                    self.advance();
                    if self.lookahead == '(' {
                        self.advance();
                        let start = self.position;
                        self.skip_arithmetic_expansion().expect("invalid word");
                        WordToken::ArithmeticExpansion(&self.source[start..self.position - 1])
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

pub fn remove_quotes(word: &str) -> (bool, String) {
    let mut lex = WordLexer::new(word);
    let mut result = String::with_capacity(word.len());
    let mut is_quoted = false;
    let mut inside_double_quotes = false;
    let mut next = lex.next_token();
    loop {
        match next {
            WordToken::DoubleQuote => {
                is_quoted = true;
                inside_double_quotes = !inside_double_quotes;
            }
            WordToken::SingleQuote => {
                is_quoted = true;
                if inside_double_quotes {
                    result.push('\'')
                } else {
                    while let Some(c) = lex.next_char() {
                        if c == '\'' {
                            break;
                        } else {
                            result.push(c);
                        }
                    }
                }
            }
            WordToken::Dollar => result.push('$'),
            WordToken::Backslash => {
                is_quoted = true;
                if inside_double_quotes {
                    match lex.next_token() {
                        WordToken::Dollar => {
                            result.push('$');
                        }
                        WordToken::DoubleQuote => {
                            result.push('"');
                        }
                        WordToken::Backslash => {
                            result.push('\\');
                        }
                        _ => result.push('\\'),
                    }
                } else if let Some(c) = lex.next_char() {
                    result.push(c)
                }
            }
            WordToken::QuotedBacktick => result.push('`'),
            WordToken::CommandSubstitution(commands) => {
                result.push_str("$(");
                result.push_str(commands);
                result.push(')');
            }
            WordToken::BacktickCommandSubstitution(commands) => {
                result.push('`');
                result.push_str(commands);
                result.push('`');
            }
            WordToken::ArithmeticExpansion(expr) => {
                result.push_str("$((");
                result.push_str(expr);
                result.push_str("))");
            }
            WordToken::Char(c) => result.push(c),
            WordToken::Eof => break,
        }
        next = lex.next_token()
    }
    (is_quoted, result)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_token(s: &str) -> WordToken {
        let mut lex = WordLexer::new(s);
        let token = lex.next_token();
        assert_eq!(lex.next_token(), WordToken::Eof);
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
        assert_eq!(lex_token("``"), WordToken::BacktickCommandSubstitution(""));
        assert_eq!(
            lex_token("`cmd`"),
            WordToken::BacktickCommandSubstitution("cmd")
        );
        assert_eq!(
            lex_token("`cmd arg1 arg2`"),
            WordToken::BacktickCommandSubstitution("cmd arg1 arg2")
        );
        assert_eq!(
            lex_token("`\ncmd1\ncmd2\ncmd3\n`"),
            WordToken::BacktickCommandSubstitution("\ncmd1\ncmd2\ncmd3\n")
        );
        assert_eq!(
            lex_token("`#comment\ncmd`"),
            WordToken::BacktickCommandSubstitution("#comment\ncmd")
        );
        assert_eq!(
            lex_token("`cmd $(cmd2)`"),
            WordToken::BacktickCommandSubstitution("cmd $(cmd2)")
        );
    }

    #[test]
    fn lex_arithmetic_expansion() {
        assert_eq!(lex_token("$((1))"), WordToken::ArithmeticExpansion("1"));
        assert_eq!(
            lex_token("$((1 + 1))"),
            WordToken::ArithmeticExpansion("1 + 1")
        );
        assert_eq!(
            lex_token("$(((1) + (1)))"),
            WordToken::ArithmeticExpansion("(1) + (1)")
        );
    }
}
