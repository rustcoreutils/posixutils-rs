//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::parse::lexer::Lexer;
use crate::parse::ParseResult;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WordToken<'src> {
    DoubleQuote,
    SingleQuote,
    Dollar,
    Backslash,
    // this needs to be a standalone token, otherwise we would get
    // `Backslash` and then we would try to lex `BacktickCommandSubstitution`
    QuotedBacktick,
    CommandSubstitution(&'src [u8]),
    BacktickCommandSubstitution(&'src [u8]),
    ArithmeticExpansion(&'src [u8]),
    /// `$'...'` dollar-single-quote (POSIX.1-2024 §2.2.4); holds the raw,
    /// not-yet-unescaped content between the quotes.
    DollarSingleQuote(&'src [u8]),

    /// One byte of ordinary word text. Bytes >= 0x80 arrive here individually
    /// and are reassembled by the word parser, which never inspects them.
    Char(u8),

    Eof,
}

/// A character that, immediately after `$`, can introduce a parameter
/// expansion or special parameter. If `$` is followed by anything else (a
/// blank, `]`, `"`, EOF, ...) it is an ordinary literal `$`.
fn is_parameter_start(c: u8) -> bool {
    c.is_ascii_alphanumeric()
        || matches!(
            c,
            b'_' | b'{' | b'@' | b'*' | b'#' | b'?' | b'-' | b'!' | b'$'
        )
}

/// Processes the backslash escape sequences of a `$'...'` string per
/// POSIX.1-2024 §2.2.4 into the literal characters they denote.
pub fn unescape_dollar_single_quote(raw: &[u8]) -> Vec<u8> {
    let mut result: Vec<u8> = Vec::with_capacity(raw.len());
    let mut bytes = raw.iter().copied().peekable();
    while let Some(c) = bytes.next() {
        if c != b'\\' {
            result.push(c);
            continue;
        }
        match bytes.next() {
            Some(b'n') => result.push(b'\n'),
            Some(b't') => result.push(b'\t'),
            Some(b'r') => result.push(b'\r'),
            Some(b'a') => result.push(0x07),
            Some(b'b') => result.push(0x08),
            Some(b'e') | Some(b'E') => result.push(0x1b),
            Some(b'f') => result.push(0x0c),
            Some(b'v') => result.push(0x0b),
            Some(b'\\') => result.push(b'\\'),
            Some(b'\'') => result.push(b'\''),
            Some(b'"') => result.push(b'"'),
            Some(b'?') => result.push(b'?'),
            Some(b'x') => {
                // `\xNN` names a *byte*, so it is emitted as one rather than
                // being encoded as the character with that code point.
                let mut value: u32 = 0;
                let mut count = 0;
                while count < 2 {
                    match bytes.peek().and_then(|c| (*c as char).to_digit(16)) {
                        Some(d) => {
                            value = value * 16 + d;
                            bytes.next();
                            count += 1;
                        }
                        None => break,
                    }
                }
                if count == 0 {
                    result.push(b'\\');
                    result.push(b'x');
                } else {
                    result.push(value as u8);
                }
            }
            Some(b'c') => {
                if let Some(ctrl) = bytes.next() {
                    // control character: \cX -> the byte X & 0x1f
                    result.push(ctrl.to_ascii_uppercase() ^ 0x40);
                }
            }
            Some(d @ b'0'..=b'7') => {
                let mut value = u32::from(d - b'0');
                let mut count = 1;
                while count < 3 {
                    match bytes.peek().and_then(|c| (*c as char).to_digit(8)) {
                        Some(o) => {
                            value = value * 8 + o;
                            bytes.next();
                            count += 1;
                        }
                        None => break,
                    }
                }
                result.push(value as u8);
            }
            Some(other) => {
                // unrecognized escape: keep the backslash and the character
                result.push(b'\\');
                result.push(other);
            }
            None => result.push(b'\\'),
        }
    }
    result
}

impl Display for WordToken<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            WordToken::DoubleQuote => write!(f, "'\"'"),
            WordToken::SingleQuote => write!(f, "'"),
            WordToken::Dollar => write!(f, "'$'"),
            WordToken::Backslash => write!(f, "'\\'"),
            WordToken::QuotedBacktick => write!(f, "'\\`'"),
            WordToken::CommandSubstitution(str) => {
                write!(f, "'$({})'", String::from_utf8_lossy(str))
            }
            WordToken::BacktickCommandSubstitution(str) => {
                write!(f, "`{}`", String::from_utf8_lossy(str))
            }
            WordToken::ArithmeticExpansion(str) => {
                write!(f, "'$(({}))'", String::from_utf8_lossy(str))
            }
            WordToken::DollarSingleQuote(str) => write!(f, "$'{}'", String::from_utf8_lossy(str)),
            WordToken::Char(c) => write!(f, "'{}'", *c as char),
            WordToken::Eof => write!(f, "'EOF'"),
        }
    }
}

fn advance_and_return<'a>(lex: &mut WordLexer, token: WordToken<'a>) -> WordToken<'a> {
    lex.advance();
    token
}

pub struct WordLexer<'src> {
    source: &'src [u8],
    /// Byte cursor. Every character the grammar cares about is ASCII, so the
    /// lexer never needs to decode: a byte >= 0x80 is an ordinary word
    /// character.
    position: usize,
    lookahead: u8,
    reached_eof: bool,
}

impl Lexer for WordLexer<'_> {
    fn advance(&mut self) {
        // `position` names the lookahead byte, so it stops at the last byte
        // once the source is exhausted; `next_line` compensates.
        if self.position + 1 < self.source.len() {
            self.position += 1;
            self.lookahead = self.source[self.position];
        } else {
            self.reached_eof = true;
            self.lookahead = b'\0';
        }
    }

    fn reached_eof(&self) -> bool {
        self.reached_eof
    }

    fn lookahead(&mut self) -> u8 {
        self.lookahead
    }

    fn line_no(&self) -> u32 {
        0
    }

    fn next_line(&mut self) -> Cow<'_, [u8]> {
        let start = self.position;
        while !self.reached_eof && self.lookahead != b'\n' {
            self.advance()
        }
        // `position` is the index of the lookahead character, and it stops
        // moving once the iterator is exhausted, so at EOF the line runs to
        // the end of the source.
        let end = if self.reached_eof {
            self.source.len()
        } else {
            self.position
        };
        let line = &self.source[start..end];
        // The terminating newline belongs to this line: leaving it in place
        // would make the next call return the same (empty) line forever.
        if !self.reached_eof {
            self.advance();
        }
        line.into()
    }

    fn next_word(&mut self) -> ParseResult<Cow<'_, [u8]>> {
        let start = self.position;
        self.skip_word_token(None, false)?;
        Ok(Cow::from(&self.source[start..self.position]))
    }
}

impl<'src> WordLexer<'src> {
    pub fn next_token(&mut self) -> ParseResult<WordToken<'src>> {
        if self.reached_eof {
            return Ok(WordToken::Eof);
        }
        let result = match self.lookahead {
            b'"' => advance_and_return(self, WordToken::DoubleQuote),
            b'\'' => advance_and_return(self, WordToken::SingleQuote),
            b'`' => {
                self.advance();
                let start = self.position;
                self.skip_backquoted_command_substitution()?;
                let end = self.position;
                self.advance();
                WordToken::BacktickCommandSubstitution(&self.source[start..end])
            }
            b'\\' => {
                self.advance();
                match self.lookahead {
                    b'`' => advance_and_return(self, WordToken::QuotedBacktick),
                    b'\n' => {
                        self.advance();
                        return self.next_token();
                    }
                    _ => WordToken::Backslash,
                }
            }
            b'$' => {
                self.advance();
                if self.lookahead == b'(' {
                    self.advance();
                    if self.lookahead == b'(' {
                        self.advance();
                        let start = self.position;
                        self.skip_arithmetic_expansion()?;
                        WordToken::ArithmeticExpansion(&self.source[start..self.position - 1])
                    } else {
                        let start = self.position;
                        self.skip_command_substitution()?;
                        let end = self.position;
                        self.advance();
                        WordToken::CommandSubstitution(&self.source[start..end])
                    }
                } else if self.lookahead == b'\'' {
                    // $'...' dollar-single-quote: capture the raw content, with
                    // a backslash escaping the following character (so \' does
                    // not terminate the string).
                    self.advance();
                    let start = self.position;
                    while !self.reached_eof && self.lookahead != b'\'' {
                        if self.lookahead == b'\\' {
                            self.advance();
                            if self.reached_eof {
                                break;
                            }
                        }
                        self.advance();
                    }
                    let content = &self.source[start..self.position];
                    self.advance();
                    WordToken::DollarSingleQuote(content)
                } else if is_parameter_start(self.lookahead) {
                    WordToken::Dollar
                } else {
                    // a '$' not introducing an expansion is an ordinary character
                    WordToken::Char(b'$')
                }
            }
            other => advance_and_return(self, WordToken::Char(other)),
        };
        Ok(result)
    }

    pub fn next_char(&mut self) -> Option<u8> {
        if self.reached_eof {
            None
        } else {
            let c = self.lookahead;
            self.advance();
            Some(c)
        }
    }

    pub fn new(source: &'src [u8]) -> Self {
        Self {
            source,
            position: 0,
            lookahead: source.first().copied().unwrap_or(b'\0'),
            reached_eof: source.is_empty(),
        }
    }
}

pub fn remove_quotes(word: &[u8]) -> ParseResult<(bool, Vec<u8>)> {
    let mut lex = WordLexer::new(word);
    let mut result: Vec<u8> = Vec::with_capacity(word.len());
    let mut is_quoted = false;
    let mut inside_double_quotes = false;
    let mut next = lex.next_token()?;
    loop {
        match next {
            WordToken::DoubleQuote => {
                is_quoted = true;
                inside_double_quotes = !inside_double_quotes;
            }
            WordToken::SingleQuote => {
                is_quoted = true;
                if inside_double_quotes {
                    result.push(b'\'')
                } else {
                    while let Some(c) = lex.next_char() {
                        if c == b'\'' {
                            break;
                        } else {
                            result.push(c);
                        }
                    }
                }
            }
            WordToken::Dollar => result.push(b'$'),
            WordToken::Backslash => {
                is_quoted = true;
                if inside_double_quotes {
                    match lex.next_token()? {
                        WordToken::Dollar => {
                            result.push(b'$');
                        }
                        WordToken::DoubleQuote => {
                            result.push(b'"');
                        }
                        WordToken::Backslash => {
                            result.push(b'\\');
                        }
                        _ => result.push(b'\\'),
                    }
                } else if let Some(c) = lex.next_char() {
                    result.push(c)
                }
            }
            WordToken::QuotedBacktick => result.push(b'`'),
            WordToken::CommandSubstitution(commands) => {
                result.extend_from_slice(b"$(");
                result.extend_from_slice(commands);
                result.push(b')');
            }
            WordToken::BacktickCommandSubstitution(commands) => {
                result.push(b'`');
                result.extend_from_slice(commands);
                result.push(b'`');
            }
            WordToken::ArithmeticExpansion(expr) => {
                result.extend_from_slice(b"$((");
                result.extend_from_slice(expr);
                result.extend_from_slice(b"))");
            }
            WordToken::DollarSingleQuote(content) => {
                is_quoted = true;
                result.extend_from_slice(&unescape_dollar_single_quote(content));
            }
            WordToken::Char(c) => result.push(c),
            WordToken::Eof => break,
        }
        next = lex.next_token()?
    }
    Ok((is_quoted, result))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_token(s: &str) -> WordToken<'_> {
        let mut lex = WordLexer::new(s.as_bytes());
        let token = lex.next_token().expect("failed to lex token");
        assert_eq!(
            lex.next_token().expect("failed to lex token"),
            WordToken::Eof
        );
        token
    }

    #[test]
    fn lex_command_substitution() {
        assert_eq!(lex_token("$()"), WordToken::CommandSubstitution(b""));
        assert_eq!(lex_token("$(cmd)"), WordToken::CommandSubstitution(b"cmd"));
        assert_eq!(
            lex_token("$(cmd arg1 arg2)"),
            WordToken::CommandSubstitution(b"cmd arg1 arg2")
        );
        assert_eq!(
            lex_token("$(\ncmd1\ncmd2\ncmd3\n)"),
            WordToken::CommandSubstitution(b"\ncmd1\ncmd2\ncmd3\n")
        );
        assert_eq!(
            lex_token("$(#comment\ncmd)"),
            WordToken::CommandSubstitution(b"#comment\ncmd")
        );
        assert_eq!(
            lex_token("$(cmd $(cmd2))"),
            WordToken::CommandSubstitution(b"cmd $(cmd2)")
        );
    }

    #[test]
    fn lex_backtick_command_substitution() {
        assert_eq!(lex_token("``"), WordToken::BacktickCommandSubstitution(b""));
        assert_eq!(
            lex_token("`cmd`"),
            WordToken::BacktickCommandSubstitution(b"cmd")
        );
        assert_eq!(
            lex_token("`cmd arg1 arg2`"),
            WordToken::BacktickCommandSubstitution(b"cmd arg1 arg2")
        );
        assert_eq!(
            lex_token("`\ncmd1\ncmd2\ncmd3\n`"),
            WordToken::BacktickCommandSubstitution(b"\ncmd1\ncmd2\ncmd3\n")
        );
        assert_eq!(
            lex_token("`#comment\ncmd`"),
            WordToken::BacktickCommandSubstitution(b"#comment\ncmd")
        );
        assert_eq!(
            lex_token("`cmd $(cmd2)`"),
            WordToken::BacktickCommandSubstitution(b"cmd $(cmd2)")
        );
    }

    #[test]
    fn lex_arithmetic_expansion() {
        assert_eq!(lex_token("$((1))"), WordToken::ArithmeticExpansion(b"1"));
        assert_eq!(
            lex_token("$((1 + 1))"),
            WordToken::ArithmeticExpansion(b"1 + 1")
        );
        assert_eq!(
            lex_token("$(((1) + (1)))"),
            WordToken::ArithmeticExpansion(b"(1) + (1)")
        );
    }
}
