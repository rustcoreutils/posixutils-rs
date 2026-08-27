//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::parse::lexer::word_lexer::remove_quotes;
use crate::parse::lexer::{is_blank, Lexer};
use crate::parse::{ParseResult, ParserError};
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, Default)]
struct IndexIter {
    pos: usize,
}

impl IndexIter {
    /// A byte cursor. Every character the grammar cares about is ASCII, so the
    /// lexer never decodes: a byte >= 0x80 is an ordinary word character.
    fn next(&mut self, string: &[u8]) -> Option<u8> {
        if let Some(c) = self.peek(string) {
            self.pos += 1;
            Some(c)
        } else {
            None
        }
    }

    fn peek(&self, string: &[u8]) -> Option<u8> {
        string.get(self.pos).copied()
    }
}

#[derive(Clone, Debug)]
struct SourceReadState {
    current_part: usize,
    current_part_char_iter: IndexIter,
    line_no: u32,
    reached_eof: bool,
}

struct SourcePart<'s> {
    text: Cow<'s, [u8]>,
    in_original_string: bool,
    provenance: String,
}

impl SourcePart<'_> {
    fn split_at(&self, mid: usize) -> (Self, Self) {
        match &self.text {
            Cow::Borrowed(s) => {
                let (p1, p2) = s.split_at(mid);
                (
                    Self {
                        text: p1.into(),
                        in_original_string: self.in_original_string,
                        provenance: self.provenance.clone(),
                    },
                    Self {
                        text: p2.into(),
                        in_original_string: self.in_original_string,
                        provenance: self.provenance.clone(),
                    },
                )
            }
            Cow::Owned(s) => {
                let (p1, p2) = s.split_at(mid);
                (
                    Self {
                        text: p1.to_owned().into(),
                        in_original_string: self.in_original_string,
                        provenance: self.provenance.clone(),
                    },
                    Self {
                        text: p2.to_owned().into(),
                        in_original_string: self.in_original_string,
                        provenance: self.provenance.clone(),
                    },
                )
            }
        }
    }
}

struct SourceString<'s> {
    // all parts should have text, otherwise they should not be added.
    // The first part is an exception, but if its empty, `read_state.reached_eof`
    // should be set
    parts: Vec<SourcePart<'s>>,
    read_state: SourceReadState,
}

impl<'s> SourceString<'s> {
    fn new(s: &'s [u8]) -> Self {
        Self {
            parts: vec![SourcePart {
                text: s.into(),
                in_original_string: true,
                provenance: "<src>".into(),
            }],
            read_state: SourceReadState {
                current_part: 0,
                current_part_char_iter: IndexIter::default(),
                reached_eof: s.is_empty(),
                line_no: 1,
            },
        }
    }

    fn current_str(&self) -> &[u8] {
        self.parts[self.read_state.current_part].text.as_ref()
    }

    fn peek(&self) -> Option<u8> {
        self.read_state
            .current_part_char_iter
            .peek(self.current_str())
    }

    /// Consumes exactly one character, crossing into later parts as needed.
    ///
    /// Crossing a part boundary must not be a step of its own: `lookahead`
    /// already peeks into the next part, so a caller that peeks and then
    /// advances once would otherwise see the same character twice.
    fn advance_char(&mut self) {
        while !self.read_state.reached_eof {
            if let Some(char) = self
                .read_state
                .current_part_char_iter
                .next(self.parts[self.read_state.current_part].text.as_ref())
            {
                if char == b'\n' && self.parts[self.read_state.current_part].in_original_string {
                    self.read_state.line_no += 1;
                }
                if self.read_state.current_part == self.parts.len() - 1 && self.peek().is_none() {
                    self.read_state.reached_eof = true;
                }
                return;
            }
            if self.read_state.current_part + 1 == self.parts.len() {
                self.read_state.reached_eof = true;
                return;
            }
            self.read_state.current_part += 1;
            self.read_state.current_part_char_iter = IndexIter::default();
        }
    }

    fn substr(&self, start: &SourceReadState, end: &SourceReadState) -> Cow<'s, [u8]> {
        assert!(start.current_part <= end.current_part);
        if start.current_part == end.current_part {
            match self.parts[start.current_part].text {
                Cow::Owned(ref s) => s
                    [start.current_part_char_iter.pos..end.current_part_char_iter.pos]
                    .to_owned()
                    .into(),
                Cow::Borrowed(s) => {
                    s[start.current_part_char_iter.pos..end.current_part_char_iter.pos].into()
                }
            }
        } else {
            let mut result: Vec<u8> = Vec::new();
            result.extend_from_slice(
                &self.parts[start.current_part].text[start.current_part_char_iter.pos..],
            );
            for part_idx in start.current_part + 1..end.current_part {
                result.extend_from_slice(self.parts[part_idx].text.as_ref());
            }
            result.extend_from_slice(
                &self.parts[end.current_part].text[..end.current_part_char_iter.pos],
            );
            result.into()
        }
    }

    fn insert_string_after_last_char(&mut self, string: Cow<'s, [u8]>, tag: &str) {
        if string.is_empty() {
            return;
        }
        let mut provenance = self.parts[self.read_state.current_part]
            .provenance
            .to_string();
        provenance.push(':');
        provenance.push_str(tag);

        let (p1, p2) = self.parts[self.read_state.current_part]
            .split_at(self.read_state.current_part_char_iter.pos);
        self.parts[self.read_state.current_part] = p1;
        self.parts.push(SourcePart {
            text: string,
            in_original_string: false,
            provenance,
        });
        if p2.text.is_empty() {
            self.parts[self.read_state.current_part + 1..].rotate_right(1);
        } else {
            self.parts.push(p2);
            self.parts[self.read_state.current_part + 1..].rotate_right(2);
        }

        self.read_state.reached_eof = false;
        self.read_state.current_part += 1;
        self.read_state.current_part_char_iter = IndexIter::default();
    }

    fn reached_eof(&self) -> bool {
        self.read_state.reached_eof
    }

    fn line_no(&self) -> u32 {
        self.read_state.line_no
    }

    fn lookahead(&self) -> u8 {
        if let Some(c) = self.peek() {
            c
        } else {
            if self.read_state.current_part == self.parts.len() - 1 {
                return b'\0';
            }
            self.parts[self.read_state.current_part + 1].text[0]
        }
    }

    /// Removes `\`-newline line continuations at the current position and
    /// reports whether any were removed. Token recognition happens after
    /// continuation removal (POSIX §2.2.1), so operators, IO_NUMBERs and
    /// reserved words may all be split across lines.
    fn skip_line_continuations(&mut self) -> bool {
        let mut removed = false;
        while self.lookahead() == b'\\' {
            let before_backslash = self.read_state.clone();
            self.advance_char();
            if self.lookahead() == b'\n' {
                self.advance_char();
                removed = true;
            } else {
                self.read_state = before_backslash;
                break;
            }
        }
        removed
    }

    fn currently_processing_tag(&self, tag: &str) -> bool {
        self.parts[self.read_state.current_part]
            .provenance
            .split(':')
            .any(|part| part == tag)
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
    AndIf,   // &&
    OrIf,    // ||
    DSemi,   // ;;
    SemiAnd, // ;&
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

    Word(Cow<'src, [u8]>),
    HereDocument {
        delimiter: Cow<'src, [u8]>,
        contents: Cow<'src, [u8]>,
    },
    QuotedHereDocument {
        start_delimiter: Cow<'src, [u8]>,
        end_delimiter: Cow<'src, [u8]>,
        contents: Cow<'src, [u8]>,
    },

    Eof,
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
            CommandToken::SemiAnd => write!(f, "';&'"),
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
            CommandToken::HereDocument { .. } | CommandToken::QuotedHereDocument { .. } => {
                write!(f, "here-document")
            }
            CommandToken::Eof => write!(f, "EOF"),
        }
    }
}

impl<'src> CommandToken<'src> {
    /// A reserved word can only occur in command position (POSIX 2.4), so one
    /// appearing while a simple command is being read means that command has
    /// ended.
    pub fn is_reserved_word(&self) -> bool {
        matches!(
            self,
            CommandToken::Bang
                | CommandToken::LBrace
                | CommandToken::RBrace
                | CommandToken::Case
                | CommandToken::Do
                | CommandToken::Done
                | CommandToken::Elif
                | CommandToken::Else
                | CommandToken::Esac
                | CommandToken::Fi
                | CommandToken::For
                | CommandToken::If
                | CommandToken::In
                | CommandToken::Then
                | CommandToken::Until
                | CommandToken::While
        )
    }

    /// The token's text as bytes, for the positions where any word is allowed.
    /// A reserved word maps back to its spelling; anything that is not a word
    /// yields `None`.
    pub fn as_word_bytes(&self) -> Option<&[u8]> {
        match self {
            CommandToken::Word(word) => Some(word.as_ref()),
            other => other.as_word_str().map(str::as_bytes),
        }
    }

    /// As [`Self::as_word_bytes`], but only for text. A `Word` holding bytes
    /// that are not a character yields `None`, so this is for comparisons
    /// against fixed spellings, never for deciding whether something *is* a
    /// word.
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
            CommandToken::Word(word) => std::str::from_utf8(word.as_ref()).ok(),
            _ => None,
        }
    }

    pub fn into_word_cow(self) -> Option<Cow<'src, [u8]>> {
        match self {
            CommandToken::Bang => Some(Cow::Borrowed(b"!".as_slice())),
            CommandToken::LBrace => Some(Cow::Borrowed(b"{".as_slice())),
            CommandToken::RBrace => Some(Cow::Borrowed(b"}".as_slice())),
            CommandToken::Case => Some(Cow::Borrowed(b"case".as_slice())),
            CommandToken::Do => Some(Cow::Borrowed(b"do".as_slice())),
            CommandToken::Done => Some(Cow::Borrowed(b"done".as_slice())),
            CommandToken::Elif => Some(Cow::Borrowed(b"elif".as_slice())),
            CommandToken::Else => Some(Cow::Borrowed(b"else".as_slice())),
            CommandToken::Esac => Some(Cow::Borrowed(b"esac".as_slice())),
            CommandToken::Fi => Some(Cow::Borrowed(b"fi".as_slice())),
            CommandToken::For => Some(Cow::Borrowed(b"for".as_slice())),
            CommandToken::If => Some(Cow::Borrowed(b"if".as_slice())),
            CommandToken::In => Some(Cow::Borrowed(b"in".as_slice())),
            CommandToken::Then => Some(Cow::Borrowed(b"then".as_slice())),
            CommandToken::Until => Some(Cow::Borrowed(b"until".as_slice())),
            CommandToken::While => Some(Cow::Borrowed(b"while".as_slice())),
            CommandToken::Word(word) => Some(word),
            _ => None,
        }
    }

    fn word(word: Cow<'src, [u8]>, recognize_reserved: bool) -> Self {
        if !recognize_reserved {
            return CommandToken::Word(word);
        }
        // A reserved word may be split by a line continuation (`i\`+newline+`f`),
        // which is removed before token recognition.
        let unsplit = if word.contains(&b'\\') {
            strip_line_continuations(word.as_ref())
        } else {
            None
        };
        match unsplit.as_deref().unwrap_or(word.as_ref()) {
            b"!" => CommandToken::Bang,
            b"{" => CommandToken::LBrace,
            b"}" => CommandToken::RBrace,
            b"case" => CommandToken::Case,
            b"do" => CommandToken::Do,
            b"done" => CommandToken::Done,
            b"elif" => CommandToken::Elif,
            b"else" => CommandToken::Else,
            b"esac" => CommandToken::Esac,
            b"fi" => CommandToken::Fi,
            b"for" => CommandToken::For,
            b"if" => CommandToken::If,
            b"in" => CommandToken::In,
            b"then" => CommandToken::Then,
            b"until" => CommandToken::Until,
            b"while" => CommandToken::While,
            _ => CommandToken::Word(word),
        }
    }
}

/// Removes `\`-newline line continuations from a word so that a reserved word
/// split across lines is still recognized. Returns `None` when the word carries
/// any other quoting, which would keep it an ordinary word regardless.
fn strip_line_continuations(word: &[u8]) -> Option<Vec<u8>> {
    let mut result: Vec<u8> = Vec::with_capacity(word.len());
    let mut bytes = word.iter().copied().peekable();
    while let Some(c) = bytes.next() {
        match c {
            // Peek rather than consume: a guard that calls `next()` swallows
            // the byte even when the guard fails.
            b'\\' if bytes.peek() == Some(&b'\n') => {
                bytes.next();
            }
            b'\\' | b'\'' | b'"' | b'`' | b'$' => return None,
            _ => result.push(c),
        }
    }
    Some(result)
}

fn advance_and_return<Tok>(lex: &mut CommandLexer, complete_token: Tok) -> Tok {
    lex.source.advance_char();
    complete_token
}

fn char_to_operator_token(c: u8) -> Option<CommandToken<'static>> {
    match c {
        b'&' => Some(CommandToken::And),
        b'(' => Some(CommandToken::LParen),
        b')' => Some(CommandToken::RParen),
        b';' => Some(CommandToken::SemiColon),
        b'\n' => Some(CommandToken::Newline),
        b'|' => Some(CommandToken::Pipe),
        b'<' => Some(CommandToken::Less),
        b'>' => Some(CommandToken::Greater),
        _ => None,
    }
}

pub struct CommandLexer<'src> {
    source: SourceString<'src>,
    prev_read_state: SourceReadState,
}

impl Lexer for CommandLexer<'_> {
    fn advance(&mut self) {
        self.source.advance_char()
    }

    fn reached_eof(&self) -> bool {
        self.source.reached_eof()
    }

    fn lookahead(&mut self) -> u8 {
        self.source.lookahead()
    }

    fn line_no(&self) -> u32 {
        self.source.line_no()
    }

    fn next_line(&mut self) -> Cow<'_, [u8]> {
        let start = self.source.read_state.clone();
        while !self.reached_eof() {
            let lookahead = self.lookahead();
            self.advance();
            if lookahead == b'\n' {
                break;
            }
        }
        self.source.substr(&start, &self.source.read_state)
    }

    fn next_word(&mut self) -> ParseResult<Cow<'_, [u8]>> {
        self.read_word_token()
    }
}

impl<'src> CommandLexer<'src> {
    fn skip_blanks(&mut self) {
        loop {
            if is_blank(self.source.lookahead()) {
                self.source.advance_char();
            } else if !self.source.skip_line_continuations() {
                break;
            }
        }
    }

    fn read_word_token(&mut self) -> ParseResult<Cow<'src, [u8]>> {
        let start = self.source.read_state.clone();
        self.skip_word_token(None, false)?;
        let result = self.source.substr(&start, &self.source.read_state);
        Ok(result)
    }

    /// Reads a `<<` / `<<-` redirection.
    ///
    /// Only the delimiter word lives on the `<<` line; the here-document body
    /// starts on the *next* line, and whatever else follows the delimiter
    /// (`| cmd`, `> file`, a second `<<`, …) is still part of the command. The
    /// body is therefore consumed here and the remainder of the `<<` line is
    /// pushed back so it is tokenized next — which also makes a second
    /// here-document on the same line pick up its body after the first one's
    /// terminator, as POSIX requires.
    fn read_here_document(&mut self, remove_leading_tabs: bool) -> ParseResult<CommandToken<'src>> {
        let start_lineno = self.source.line_no();

        // `<<` is an operator, so blanks may separate it from the delimiter.
        self.skip_blanks();
        let delimiter_start = self.source.read_state.clone();
        self.skip_word_token(None, false)?;
        let start_delimiter = self
            .source
            .substr(&delimiter_start, &self.source.read_state);
        if start_delimiter.is_empty() {
            return Err(ParserError::new(
                start_lineno,
                "missing here-document delimiter",
                self.reached_eof(),
            ));
        }
        let (is_quoted, end_delimiter) = remove_quotes(start_delimiter.as_ref())?;

        // Save the rest of the `<<` line; it is re-inserted below.
        let rest_of_line_start = self.source.read_state.clone();
        while !self.reached_eof() {
            let c = self.source.lookahead();
            self.source.advance_char();
            if c == b'\n' {
                break;
            }
        }
        let rest_of_line = self
            .source
            .substr(&rest_of_line_start, &self.source.read_state);

        let body_start = self.source.read_state.clone();
        let body_end;
        loop {
            if self.reached_eof() {
                return Err(ParserError::new(
                    start_lineno,
                    "unterminated here-document",
                    true,
                ));
            }
            let line_start = self.source.read_state.clone();
            let line = self.next_line();
            let line = line.strip_suffix(b"\n").unwrap_or(&line);
            // `<<-` also strips leading tabs from the terminator line.
            let line = if remove_leading_tabs {
                let mut l = line;
                while let Some(rest) = l.strip_prefix(b"\t") {
                    l = rest;
                }
                l
            } else {
                line
            };
            if line == end_delimiter.as_slice() {
                body_end = line_start;
                break;
            }
        }
        let contents = self.source.substr(&body_start, &body_end);
        self.source
            .insert_string_after_last_char(rest_of_line, "here-document");

        let contents = if remove_leading_tabs {
            let mut stripped: Vec<u8> = Vec::new();
            for line in contents.split(|&b| b == b'\n') {
                let mut l = line;
                while let Some(rest) = l.strip_prefix(b"\t") {
                    l = rest;
                }
                stripped.extend_from_slice(l);
                stripped.push(b'\n');
            }
            // `split` yields a trailing empty piece for a body ending in a
            // newline; drop the extra newline it just added.
            if contents.last() == Some(&b'\n') {
                stripped.pop();
            }
            Cow::Owned(stripped)
        } else {
            contents
        };

        if is_quoted {
            Ok(CommandToken::QuotedHereDocument {
                start_delimiter,
                end_delimiter: end_delimiter.into(),
                contents,
            })
        } else {
            Ok(CommandToken::HereDocument {
                delimiter: end_delimiter.into(),
                contents,
            })
        }
    }

    /// `recognize_reserved` implements POSIX 2.4: a reserved word is only a
    /// reserved word in specific grammatical positions, and an ordinary word
    /// everywhere else. The parser decides, since only it knows the position.
    pub fn next_token(
        &mut self,
        recognize_reserved: bool,
    ) -> ParseResult<(CommandToken<'src>, u32)> {
        self.prev_read_state = self.source.read_state.clone();
        self.skip_blanks();
        self.skip_comment();

        let line_no = self.source.line_no();

        if self.reached_eof() {
            return Ok((CommandToken::Eof, line_no));
        }

        if let Some(partial_token) = char_to_operator_token(self.source.lookahead()) {
            // multi-character operators all start with a single character
            // operator
            self.source.advance_char();
            // a continuation may split the two characters of an operator
            self.source.skip_line_continuations();

            let complete_token = match partial_token {
                CommandToken::And => match self.source.lookahead() {
                    b'&' => advance_and_return(self, CommandToken::AndIf),
                    _ => CommandToken::And,
                },
                CommandToken::Pipe => match self.source.lookahead() {
                    b'|' => advance_and_return(self, CommandToken::OrIf),
                    _ => CommandToken::Pipe,
                },
                CommandToken::SemiColon => match self.source.lookahead() {
                    b';' => advance_and_return(self, CommandToken::DSemi),
                    b'&' => advance_and_return(self, CommandToken::SemiAnd),
                    _ => CommandToken::SemiColon,
                },
                CommandToken::Less => match self.source.lookahead() {
                    b'&' => advance_and_return(self, CommandToken::LessAnd),
                    b'>' => advance_and_return(self, CommandToken::LessGreat),
                    b'<' => {
                        self.source.advance_char();
                        self.source.skip_line_continuations();
                        if self.lookahead() == b'-' {
                            self.source.advance_char();
                            self.read_here_document(true)?
                        } else {
                            self.read_here_document(false)?
                        }
                    }
                    _ => CommandToken::Less,
                },
                CommandToken::Greater => match self.source.lookahead() {
                    b'>' => advance_and_return(self, CommandToken::DGreat),
                    b'&' => advance_and_return(self, CommandToken::GreatAnd),
                    b'|' => advance_and_return(self, CommandToken::Clobber),
                    _ => CommandToken::Greater,
                },
                other => other,
            };
            return Ok((complete_token, line_no));
        }

        let token = match self.source.lookahead() {
            d if d.is_ascii_digit() => {
                let prev_read_state = self.source.read_state.clone();

                let mut number = u32::from(d - b'0');
                self.source.advance_char();
                self.source.skip_line_continuations();
                while self.source.lookahead().is_ascii_digit() {
                    let d = u32::from(self.source.lookahead() - b'0');
                    number = number.saturating_mul(10);
                    number = number.saturating_add(d);
                    self.source.advance_char();
                    self.source.skip_line_continuations();
                }
                if self.lookahead() == b'>' || self.lookahead() == b'<' {
                    CommandToken::IoNumber(number)
                } else {
                    self.source.read_state = prev_read_state;
                    CommandToken::word(self.read_word_token()?, recognize_reserved)
                }
            }
            _ => CommandToken::word(self.read_word_token()?, recognize_reserved),
        };
        Ok((token, line_no))
    }

    /// Inserts text after the last returned token
    pub fn insert_text_at_current_position(&mut self, text: Cow<'src, [u8]>, tag: &str) {
        self.source.insert_string_after_last_char(text, tag);
    }

    pub fn is_currently_processing_tag(&self, tag: &str) -> bool {
        self.source.currently_processing_tag(tag)
    }

    pub fn is_next_lparen(&mut self) -> bool {
        let rollback = self.source.read_state.clone();
        self.skip_blanks();
        let result = self.lookahead() == b'(';
        self.source.read_state = rollback;
        result
    }

    pub fn new(source: &'src [u8]) -> Self {
        let source = SourceString::new(source);
        let initial_read_state = source.read_state.clone();
        Self {
            source,
            prev_read_state: initial_read_state,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_word(text: &str) {
        let mut lex = CommandLexer::new(text.as_bytes());
        if let CommandToken::Word(word) = lex.next_token(true).unwrap().0 {
            assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Eof);
            assert_eq!(word, text.as_bytes());
        } else {
            panic!("not a word")
        }
    }

    fn lex_token(token: &str) -> CommandToken<'_> {
        let mut lex = CommandLexer::new(token.as_bytes());
        let token = lex.next_token(true).unwrap().0;
        // A here-document pushes the remainder of its line back, so a trailing
        // newline may still be pending.
        let mut next = lex.next_token(true).unwrap().0;
        if next == CommandToken::Newline {
            next = lex.next_token(true).unwrap().0;
        }
        assert_eq!(next, CommandToken::Eof);
        token
    }

    #[test]
    fn lex_empty_string() {
        let mut lex = CommandLexer::new(b"");
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Eof);
    }

    #[test]
    fn lex_skip_comment() {
        let mut lex = CommandLexer::new(b"# this is a comment\n");
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Newline);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Eof);

        let mut lex = CommandLexer::new(b"cmd arg #comment");
        assert_eq!(
            lex.next_token(true).unwrap().0,
            CommandToken::Word(b"cmd".as_slice().into())
        );
        assert_eq!(
            lex.next_token(true).unwrap().0,
            CommandToken::Word(b"arg".as_slice().into())
        );
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Eof);
    }

    #[test]
    fn lex_operators() {
        let mut lex = CommandLexer::new(b"&();\n|&&||;;< > >| >><&>&<>");
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::And);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::LParen);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::RParen);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::SemiColon);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Newline);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Pipe);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::AndIf);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::OrIf);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::DSemi);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Less);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Greater);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Clobber);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::DGreat);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::LessAnd);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::GreatAnd);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::LessGreat);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Eof);
    }

    #[test]
    fn test_lex_reserved_words() {
        let mut lex =
            CommandLexer::new(b"! { } case do done elif else esac fi for if in then until while");
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Bang);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::LBrace);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::RBrace);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Case);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Do);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Done);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Elif);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Else);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Esac);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Fi);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::For);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::If);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::In);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Then);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Until);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::While);
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Eof);
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
        lex_word("'test ${$($(('");
        lex_word("'\"abc\"'");
    }

    #[test]
    fn lex_double_quoted_word() {
        lex_word("\"abc\"");
        lex_word("\"'abc'\"");
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
        lex_word("$(#comment\necho a &#)'\")2\n)");
    }

    #[test]
    fn here_document_inside_command_substitution() {
        lex_word("$(echo <<end\nthis\nis\n\ta\ntest\nend\n)");
        lex_word("$(echo <<-end\nthis)(\nis\n\ta))\n\t\t\t\ttest\nend\n)");
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
            CommandToken::HereDocument {
                delimiter: b"end".as_slice().into(),
                contents: b"this\nis\n\ta\ntest\n".as_slice().into()
            }
        );
        assert_eq!(
            lex_token("<<-end\nthis\nis\n\ta\n\t\t\t\ttest\nend\n"),
            CommandToken::HereDocument {
                delimiter: b"end".as_slice().into(),
                contents: b"this\nis\na\ntest\n".as_slice().into()
            }
        )
    }

    #[test]
    fn quoted_here_document() {
        assert_eq!(
            lex_token("<<\\end\nthis\nis\n\ta\ntest\nend\n"),
            CommandToken::QuotedHereDocument {
                start_delimiter: b"\\end".as_slice().into(),
                end_delimiter: b"end".as_slice().into(),
                contents: b"this\nis\n\ta\ntest\n".as_slice().into()
            }
        );
        assert_eq!(
            lex_token("<<-\\end\nthis\nis\n\ta\n\t\t\t\ttest\nend\n"),
            CommandToken::QuotedHereDocument {
                start_delimiter: b"\\end".as_slice().into(),
                end_delimiter: b"end".as_slice().into(),
                contents: b"this\nis\na\ntest\n".as_slice().into()
            }
        )
    }

    #[test]
    fn lex_io_number() {
        let mut lex = CommandLexer::new(b"123>");
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::IoNumber(123));
        let mut lex = CommandLexer::new(b"123");
        assert_eq!(
            lex.next_token(true).unwrap().0,
            CommandToken::Word(b"123".as_slice().into())
        );
    }

    #[test]
    fn insert_text() {
        let mut lex = CommandLexer::new(b"a b c");
        assert_eq!(
            lex.next_token(true).unwrap().0,
            CommandToken::Word(b"a".as_slice().into())
        );
        lex.insert_text_at_current_position(b"x".as_slice().into(), "x");
        assert_eq!(
            lex.next_token(true).unwrap().0,
            CommandToken::Word(b"x".as_slice().into())
        );
        assert_eq!(
            lex.next_token(true).unwrap().0,
            CommandToken::Word(b"b".as_slice().into())
        );
        assert_eq!(
            lex.next_token(true).unwrap().0,
            CommandToken::Word(b"c".as_slice().into())
        );
        lex.insert_text_at_current_position(b"y".as_slice().into(), "y");
        assert_eq!(
            lex.next_token(true).unwrap().0,
            CommandToken::Word(b"y".as_slice().into())
        );
        assert_eq!(lex.next_token(true).unwrap().0, CommandToken::Eof);
    }
}
