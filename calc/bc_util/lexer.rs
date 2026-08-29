//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Lexical analysis for `bc`.
//!
//! Implements the token rules of POSIX.1-2024, XCU `bc`, "Lexical Conventions
//! in bc" (rules 8-17). Two of those rules are load-bearing and easy to get
//! wrong:
//!
//! - Rule 9/10: the keywords are *tokens*, and LETTER is a single lowercase
//!   letter "occurring anywhere except within a keyword". Lexing `auto` as the
//!   four letters `a u t o` silently turns an `auto` declaration into four
//!   variable references.
//! - Rule 15: `++` and `--` are single INCR_DECR tokens, so `1--1` is not
//!   `1 - (-1)`; it is a syntax error.

use std::fmt;

/// A `bc` token.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    /// NUMBER, as written in the source with line continuations removed. The
    /// text is interpreted at run time, in the base held by `ibase`.
    Number(String),
    /// STRING: the characters between the quotes, which may include newlines.
    Str(String),
    /// LETTER: one lowercase letter that is not part of a keyword.
    Letter(char),

    // Keywords (rule 9).
    Auto,
    Break,
    Define,
    For,
    Ibase,
    If,
    Length,
    Obase,
    Quit,
    Return,
    Scale,
    Sqrt,
    While,

    // ASSIGN_OP (rule 11).
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    PowAssign,

    // MUL_OP (rule 13).
    Star,
    Slash,
    Percent,

    // REL_OP (rule 14).
    Eq,
    Le,
    Ge,
    Ne,
    Lt,
    Gt,

    // INCR_DECR (rule 15).
    Incr,
    Decr,

    // Single characters (rule 16).
    Newline,
    LParen,
    RParen,
    Comma,
    Plus,
    Minus,
    Semicolon,
    LBracket,
    RBracket,
    Caret,
    LBrace,
    RBrace,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Token::Number(n) => return write!(f, "number '{}'", n),
            Token::Str(_) => "string",
            Token::Letter(c) => return write!(f, "'{}'", c),
            Token::Auto => "'auto'",
            Token::Break => "'break'",
            Token::Define => "'define'",
            Token::For => "'for'",
            Token::Ibase => "'ibase'",
            Token::If => "'if'",
            Token::Length => "'length'",
            Token::Obase => "'obase'",
            Token::Quit => "'quit'",
            Token::Return => "'return'",
            Token::Scale => "'scale'",
            Token::Sqrt => "'sqrt'",
            Token::While => "'while'",
            Token::Assign => "'='",
            Token::AddAssign => "'+='",
            Token::SubAssign => "'-='",
            Token::MulAssign => "'*='",
            Token::DivAssign => "'/='",
            Token::ModAssign => "'%='",
            Token::PowAssign => "'^='",
            Token::Star => "'*'",
            Token::Slash => "'/'",
            Token::Percent => "'%'",
            Token::Eq => "'=='",
            Token::Le => "'<='",
            Token::Ge => "'>='",
            Token::Ne => "'!='",
            Token::Lt => "'<'",
            Token::Gt => "'>'",
            Token::Incr => "'++'",
            Token::Decr => "'--'",
            Token::Newline => "newline",
            Token::LParen => "'('",
            Token::RParen => "')'",
            Token::Comma => "','",
            Token::Plus => "'+'",
            Token::Minus => "'-'",
            Token::Semicolon => "';'",
            Token::LBracket => "'['",
            Token::RBracket => "']'",
            Token::Caret => "'^'",
            Token::LBrace => "'{'",
            Token::RBrace => "'}'",
        };
        f.write_str(s)
    }
}

/// A token together with where it started.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PositionedToken {
    pub token: Token,
    pub line: usize,
    pub col: usize,
}

/// A lexical error.
#[derive(Debug)]
pub struct LexError {
    pub message: String,
    pub line: usize,
    pub col: usize,
    /// True when more input could complete the token: an unterminated string
    /// or comment, or a line continuation at end of input. The REPL uses this
    /// to keep reading rather than reporting an error.
    pub incomplete: bool,
}

/// Keywords, longest first so that maximal munch picks the keyword over a
/// shorter one with the same prefix.
const KEYWORDS: &[(&str, Token)] = &[
    ("define", Token::Define),
    ("length", Token::Length),
    ("return", Token::Return),
    ("break", Token::Break),
    ("ibase", Token::Ibase),
    ("obase", Token::Obase),
    ("scale", Token::Scale),
    ("while", Token::While),
    ("auto", Token::Auto),
    ("quit", Token::Quit),
    ("sqrt", Token::Sqrt),
    ("for", Token::For),
    ("if", Token::If),
];

fn is_digit(c: u8) -> bool {
    matches!(c, b'0'..=b'9' | b'A'..=b'F')
}

struct Lexer<'a> {
    src: &'a [u8],
    text: &'a str,
    pos: usize,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    fn new(text: &'a str) -> Self {
        Lexer {
            src: text.as_bytes(),
            text,
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    fn peek_at(&self, offset: usize) -> Option<u8> {
        self.src.get(self.pos + offset).copied()
    }

    /// Advance one byte, maintaining line and column. Continuation bytes of a
    /// multi-byte character do not advance the column.
    fn bump(&mut self) {
        match self.src[self.pos] {
            b'\n' => {
                self.line += 1;
                self.col = 1;
            }
            b => {
                if b & 0xc0 != 0x80 {
                    self.col += 1;
                }
            }
        }
        self.pos += 1;
    }

    fn err(&self, message: impl Into<String>, incomplete: bool) -> LexError {
        LexError {
            message: message.into(),
            line: self.line,
            col: self.col,
            incomplete,
        }
    }

    /// A backslash immediately followed by a newline is a line continuation.
    fn at_continuation(&self) -> bool {
        self.peek() == Some(b'\\') && self.peek_at(1) == Some(b'\n')
    }

    /// Skip blanks, line continuations and comments. Returns an error only for
    /// an unterminated comment.
    fn skip_trivia(&mut self) -> Result<(), LexError> {
        loop {
            match self.peek() {
                Some(b' ') | Some(b'\t') => self.bump(),
                Some(b'\\') if self.peek_at(1) == Some(b'\n') => {
                    self.bump();
                    self.bump();
                }
                Some(b'\\') if self.peek_at(1).is_none() => {
                    // A trailing backslash may become a continuation once more
                    // input arrives.
                    return Err(self.err("incomplete line continuation", true));
                }
                Some(b'/') if self.peek_at(1) == Some(b'*') => self.skip_comment()?,
                _ => return Ok(()),
            }
        }
    }

    fn skip_comment(&mut self) -> Result<(), LexError> {
        let start_line = self.line;
        let start_col = self.col;
        self.bump(); // '/'
        self.bump(); // '*'
        loop {
            match self.peek() {
                None => {
                    return Err(LexError {
                        message: "unterminated comment".to_string(),
                        line: start_line,
                        col: start_col,
                        incomplete: true,
                    })
                }
                Some(b'*') if self.peek_at(1) == Some(b'/') => {
                    self.bump();
                    self.bump();
                    return Ok(());
                }
                _ => self.bump(),
            }
        }
    }

    /// A NUMBER: hexadecimal digits and at most one radix point, with line
    /// continuations spliced out. Digits above `ibase` are rejected at run
    /// time, not here.
    fn lex_number(&mut self) -> Token {
        let mut text = String::new();
        let mut seen_point = false;
        loop {
            match self.peek() {
                Some(c) if is_digit(c) => {
                    text.push(c as char);
                    self.bump();
                }
                Some(b'.') if !seen_point => {
                    seen_point = true;
                    text.push('.');
                    self.bump();
                }
                _ if self.at_continuation() => {
                    self.bump();
                    self.bump();
                }
                _ => break,
            }
        }
        Token::Number(text)
    }

    /// A STRING: everything up to the next quote. There are no escapes, and a
    /// newline inside a string is an ordinary character.
    fn lex_string(&mut self) -> Result<Token, LexError> {
        let start_line = self.line;
        let start_col = self.col;
        self.bump(); // opening quote
        let start = self.pos;
        loop {
            match self.peek() {
                None => {
                    return Err(LexError {
                        message: "unterminated string".to_string(),
                        line: start_line,
                        col: start_col,
                        incomplete: true,
                    })
                }
                Some(b'"') => {
                    let s = self.text[start..self.pos].to_string();
                    self.bump();
                    return Ok(Token::Str(s));
                }
                _ => self.bump(),
            }
        }
    }

    /// A keyword if one matches here (maximal munch), otherwise a single
    /// LETTER. POSIX rule 10: a letter within a keyword is not a LETTER.
    fn lex_word(&mut self) -> Token {
        let rest = &self.text[self.pos..];
        for (word, token) in KEYWORDS {
            if rest.as_bytes().starts_with(word.as_bytes()) {
                for _ in 0..word.len() {
                    self.bump();
                }
                return token.clone();
            }
        }
        let c = self.src[self.pos] as char;
        self.bump();
        Token::Letter(c)
    }

    /// One two-character token if `second` follows, otherwise `single`.
    fn two_or_one(&mut self, second: u8, two: Token, single: Token) -> Token {
        self.bump();
        if self.peek() == Some(second) {
            self.bump();
            two
        } else {
            single
        }
    }

    fn next_token(&mut self) -> Result<Option<PositionedToken>, LexError> {
        self.skip_trivia()?;
        let c = match self.peek() {
            None => return Ok(None),
            Some(c) => c,
        };
        let line = self.line;
        let col = self.col;

        let token = match c {
            b'\n' => {
                self.bump();
                Token::Newline
            }
            b'"' => self.lex_string()?,
            c if is_digit(c) => self.lex_number(),
            b'.' => self.lex_number(),
            b'a'..=b'z' => self.lex_word(),
            b'*' => self.two_or_one(b'=', Token::MulAssign, Token::Star),
            b'/' => self.two_or_one(b'=', Token::DivAssign, Token::Slash),
            b'%' => self.two_or_one(b'=', Token::ModAssign, Token::Percent),
            b'^' => self.two_or_one(b'=', Token::PowAssign, Token::Caret),
            b'<' => self.two_or_one(b'=', Token::Le, Token::Lt),
            b'>' => self.two_or_one(b'=', Token::Ge, Token::Gt),
            b'=' => self.two_or_one(b'=', Token::Eq, Token::Assign),
            // Rule 15: "++" and "--" are single tokens, so they are tried
            // before "+=" / "-=" and before the bare operator.
            b'+' => {
                self.bump();
                match self.peek() {
                    Some(b'+') => {
                        self.bump();
                        Token::Incr
                    }
                    Some(b'=') => {
                        self.bump();
                        Token::AddAssign
                    }
                    _ => Token::Plus,
                }
            }
            b'-' => {
                self.bump();
                match self.peek() {
                    Some(b'-') => {
                        self.bump();
                        Token::Decr
                    }
                    Some(b'=') => {
                        self.bump();
                        Token::SubAssign
                    }
                    _ => Token::Minus,
                }
            }
            b'!' => {
                self.bump();
                if self.peek() == Some(b'=') {
                    self.bump();
                    Token::Ne
                } else {
                    return Err(LexError {
                        message: "'!' is not a bc operator".to_string(),
                        line,
                        col,
                        incomplete: false,
                    });
                }
            }
            b'(' => {
                self.bump();
                Token::LParen
            }
            b')' => {
                self.bump();
                Token::RParen
            }
            b'[' => {
                self.bump();
                Token::LBracket
            }
            b']' => {
                self.bump();
                Token::RBracket
            }
            b'{' => {
                self.bump();
                Token::LBrace
            }
            b'}' => {
                self.bump();
                Token::RBrace
            }
            b',' => {
                self.bump();
                Token::Comma
            }
            b';' => {
                self.bump();
                Token::Semicolon
            }
            _ => {
                let ch = self.text[self.pos..].chars().next().unwrap_or('\u{fffd}');
                return Err(LexError {
                    message: format!("illegal character '{}'", ch.escape_default()),
                    line,
                    col,
                    incomplete: false,
                });
            }
        };
        Ok(Some(PositionedToken { token, line, col }))
    }
}

/// Split `text` into tokens.
pub fn tokenize(text: &str) -> Result<Vec<PositionedToken>, LexError> {
    let mut lexer = Lexer::new(text);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next_token()? {
        tokens.push(token);
    }
    Ok(tokens)
}

/// Where the input ended, for reporting an error at end of input.
pub fn end_position(text: &str) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    for c in text.chars() {
        if c == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn toks(text: &str) -> Vec<Token> {
        tokenize(text)
            .expect("lex error")
            .into_iter()
            .map(|t| t.token)
            .collect()
    }

    fn lex_err(text: &str) -> LexError {
        tokenize(text).expect_err("expected a lex error")
    }

    #[test]
    fn keywords_are_tokens_not_letters() {
        // POSIX rule 9/10. Lexing "auto" as four LETTERs is what silently
        // turned an auto declaration into variable references.
        assert_eq!(toks("auto"), vec![Token::Auto]);
        assert_eq!(toks("quit"), vec![Token::Quit]);
        assert_eq!(toks("define"), vec![Token::Define]);
        assert_eq!(toks("length"), vec![Token::Length]);
        assert_eq!(toks("return"), vec![Token::Return]);
        assert_eq!(toks("scale"), vec![Token::Scale]);
        assert_eq!(toks("sqrt"), vec![Token::Sqrt]);
        assert_eq!(toks("ibase"), vec![Token::Ibase]);
        assert_eq!(toks("obase"), vec![Token::Obase]);
        assert_eq!(toks("if"), vec![Token::If]);
        assert_eq!(toks("for"), vec![Token::For]);
        assert_eq!(toks("while"), vec![Token::While]);
        assert_eq!(toks("break"), vec![Token::Break]);
    }

    #[test]
    fn single_letters_are_letters() {
        assert_eq!(toks("a"), vec![Token::Letter('a')]);
        // 's' is a prefix of both "scale" and "sqrt" but is not a keyword.
        assert_eq!(toks("s"), vec![Token::Letter('s')]);
        assert_eq!(
            toks("s(1)"),
            vec![
                Token::Letter('s'),
                Token::LParen,
                Token::Number("1".into()),
                Token::RParen
            ]
        );
        assert_eq!(
            toks("f o o"),
            vec![Token::Letter('f'), Token::Letter('o'), Token::Letter('o')]
        );
    }

    #[test]
    fn keywords_use_maximal_munch() {
        // "autox" is the keyword followed by a letter, as a lex-generated
        // scanner would produce.
        assert_eq!(toks("autox"), vec![Token::Auto, Token::Letter('x')]);
    }

    #[test]
    fn incr_decr_is_one_token() {
        // POSIX rule 15. "1--1" must not lex as 1 - (-1).
        assert_eq!(
            toks("1--1"),
            vec![
                Token::Number("1".into()),
                Token::Decr,
                Token::Number("1".into())
            ]
        );
        assert_eq!(toks("++a"), vec![Token::Incr, Token::Letter('a')]);
        assert_eq!(toks("a--"), vec![Token::Letter('a'), Token::Decr]);
        assert_eq!(toks("a++"), vec![Token::Letter('a'), Token::Incr]);
        // A blank still separates two unary minuses.
        assert_eq!(
            toks("1 - -1"),
            vec![
                Token::Number("1".into()),
                Token::Minus,
                Token::Minus,
                Token::Number("1".into())
            ]
        );
    }

    #[test]
    fn assignment_and_relational_operators() {
        assert_eq!(toks("="), vec![Token::Assign]);
        assert_eq!(toks("=="), vec![Token::Eq]);
        assert_eq!(toks("+="), vec![Token::AddAssign]);
        assert_eq!(toks("-="), vec![Token::SubAssign]);
        assert_eq!(toks("*="), vec![Token::MulAssign]);
        assert_eq!(toks("/="), vec![Token::DivAssign]);
        assert_eq!(toks("%="), vec![Token::ModAssign]);
        assert_eq!(toks("^="), vec![Token::PowAssign]);
        assert_eq!(toks("<"), vec![Token::Lt]);
        assert_eq!(toks("<="), vec![Token::Le]);
        assert_eq!(toks(">"), vec![Token::Gt]);
        assert_eq!(toks(">="), vec![Token::Ge]);
        assert_eq!(toks("!="), vec![Token::Ne]);
    }

    #[test]
    fn numbers_splice_line_continuations() {
        assert_eq!(toks("123"), vec![Token::Number("123".into())]);
        assert_eq!(toks(".456"), vec![Token::Number(".456".into())]);
        assert_eq!(toks("123."), vec![Token::Number("123.".into())]);
        assert_eq!(toks("1\\\n23"), vec![Token::Number("123".into())]);
        assert_eq!(toks("1\\\n.23"), vec![Token::Number("1.23".into())]);
        assert_eq!(toks("1.\\\n23"), vec![Token::Number("1.23".into())]);
        assert_eq!(toks("ABCDEF"), vec![Token::Number("ABCDEF".into())]);
        // A second radix point starts a new number; the parser rejects the
        // adjacency.
        assert_eq!(
            toks("1.2.3"),
            vec![Token::Number("1.2".into()), Token::Number(".3".into())]
        );
    }

    #[test]
    fn comments_and_strings() {
        assert_eq!(
            toks("1 /* two */ + 3"),
            vec![
                Token::Number("1".into()),
                Token::Plus,
                Token::Number("3".into())
            ]
        );
        assert_eq!(toks("\"abc\""), vec![Token::Str("abc".into())]);
        assert_eq!(toks("\"\""), vec![Token::Str("".into())]);
        // Newlines are ordinary characters inside a string.
        assert_eq!(toks("\"a\nb\""), vec![Token::Str("a\nb".into())]);
    }

    #[test]
    fn unterminated_constructs_are_incomplete() {
        assert!(lex_err("\"abc").incomplete);
        assert!(lex_err("/* abc").incomplete);
        assert!(lex_err("1 +\\").incomplete);
    }

    #[test]
    fn illegal_character_is_not_incomplete() {
        let e = lex_err("@");
        assert!(!e.incomplete);
        assert!(e.message.contains("illegal character"));
        let e = lex_err("!");
        assert!(!e.incomplete);
    }

    #[test]
    fn positions_track_lines_and_columns() {
        let t = tokenize("1\n  22\n").expect("lex error");
        assert_eq!((t[0].line, t[0].col), (1, 1));
        assert_eq!(t[1].token, Token::Newline);
        assert_eq!((t[2].line, t[2].col), (2, 3));
        // A spliced continuation still advances the line counter.
        let t = tokenize("1\\\n2\na\n").expect("lex error");
        assert_eq!(t[0].token, Token::Number("12".into()));
        let a = t.iter().find(|p| p.token == Token::Letter('a')).unwrap();
        assert_eq!(a.line, 3);
    }
}
