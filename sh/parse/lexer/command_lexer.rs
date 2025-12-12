//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::parse::lexer::{is_blank, remove_delimiter_from_here_document, HereDocument, Lexer};
use crate::parse::ParseResult;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, Default)]
struct IndexIter {
    pos: usize,
}

impl IndexIter {
    fn next(&mut self, string: &str) -> Option<char> {
        if let Some(c) = self.peek(string) {
            self.pos += c.len_utf8();
            Some(c)
        } else {
            None
        }
    }

    fn peek(&self, string: &str) -> Option<char> {
        string[self.pos..].chars().next()
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
    text: Cow<'s, str>,
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
    fn new(s: &'s str) -> Self {
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

    fn current_str(&self) -> &str {
        self.parts[self.read_state.current_part].text.as_ref()
    }

    fn peek(&self) -> Option<char> {
        self.read_state
            .current_part_char_iter
            .peek(self.current_str())
    }

    fn advance_char(&mut self) {
        if self.read_state.reached_eof {
            return;
        }

        if let Some(char) = self
            .read_state
            .current_part_char_iter
            .next(self.parts[self.read_state.current_part].text.as_ref())
        {
            if char == '\n' && self.parts[self.read_state.current_part].in_original_string {
                self.read_state.line_no += 1;
            }
            if self.read_state.current_part == self.parts.len() - 1 && self.peek().is_none() {
                self.read_state.reached_eof = true;
            }
        } else {
            self.read_state.current_part += 1;
            self.read_state.current_part_char_iter = IndexIter::default();
        }
    }

    fn substr(&self, start: &SourceReadState, end: &SourceReadState) -> Cow<'s, str> {
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
            let mut result = String::new();
            result
                .push_str(&self.parts[start.current_part].text[start.current_part_char_iter.pos..]);
            for part_idx in start.current_part + 1..end.current_part {
                result.push_str(self.parts[part_idx].text.as_ref());
            }
            result.push_str(&self.parts[end.current_part].text[..end.current_part_char_iter.pos]);
            result.into()
        }
    }

    fn insert_string_after_last_char(&mut self, string: Cow<'s, str>, tag: &str) {
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

    fn lookahead(&self) -> char {
        if let Some(c) = self.peek() {
            c
        } else {
            if self.read_state.current_part == self.parts.len() - 1 {
                return '\0';
            }
            self.parts[self.read_state.current_part + 1]
                .text
                .as_ref()
                .chars()
                .next()
                .unwrap()
        }
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
    HereDocument {
        delimiter: Cow<'src, str>,
        contents: Cow<'src, str>,
    },
    QuotedHereDocument {
        start_delimiter: Cow<'src, str>,
        end_delimiter: Cow<'src, str>,
        contents: Cow<'src, str>,
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
            CommandToken::Word(word) => Some(word),
            _ => None,
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

fn advance_and_return<Tok>(lex: &mut CommandLexer, complete_token: Tok) -> Tok {
    lex.source.advance_char();
    complete_token
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

    fn lookahead(&mut self) -> char {
        self.source.lookahead()
    }

    fn line_no(&self) -> u32 {
        self.source.line_no()
    }

    fn next_line(&mut self) -> Cow<'_, str> {
        let start = self.source.read_state.clone();
        while !self.reached_eof() {
            let lookahead = self.lookahead();
            self.advance();
            if lookahead == '\n' {
                break;
            }
        }
        self.source.substr(&start, &self.source.read_state)
    }

    fn next_word(&mut self) -> ParseResult<Cow<'_, str>> {
        self.read_word_token()
    }
}

impl<'src> CommandLexer<'src> {
    fn skip_blanks(&mut self) {
        while is_blank(self.source.lookahead()) {
            self.source.advance_char();
        }
    }

    fn read_word_token(&mut self) -> ParseResult<Cow<'src, str>> {
        let start = self.source.read_state.clone();
        self.skip_word_token(None, false)?;
        let result = self.source.substr(&start, &self.source.read_state);
        Ok(result)
    }

    fn read_here_document(&mut self, remove_leading_tabs: bool) -> ParseResult<CommandToken<'src>> {
        let start = self.source.read_state.clone();
        let is_quoted = self.skip_here_document()?;
        let here_document = remove_delimiter_from_here_document(
            self.source.substr(&start, &self.source.read_state),
        );

        let here_document = if remove_leading_tabs {
            let mut contents = String::new();
            for line in here_document.contents.lines() {
                contents.push_str(line.trim_start_matches('\t'));
                contents.push('\n');
            }
            HereDocument {
                contents: contents.into(),
                start_delimiter: here_document.start_delimiter,
                end_delimiter: here_document.end_delimiter,
            }
        } else {
            here_document
        };
        if is_quoted {
            Ok(CommandToken::QuotedHereDocument {
                start_delimiter: here_document.start_delimiter,
                end_delimiter: here_document.end_delimiter,
                contents: here_document.contents,
            })
        } else {
            assert_eq!(here_document.start_delimiter, here_document.end_delimiter);
            Ok(CommandToken::HereDocument {
                delimiter: here_document.start_delimiter,
                contents: here_document.contents,
            })
        }
    }

    pub fn next_token(&mut self) -> ParseResult<(CommandToken<'src>, u32)> {
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
                        if self.source.lookahead() == '-' {
                            self.source.advance_char();
                            self.read_here_document(true)?
                        } else {
                            self.read_here_document(false)?
                        }
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
            d if d.is_ascii_digit() => {
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
                    CommandToken::word(self.read_word_token()?)
                }
            }
            _ => CommandToken::word(self.read_word_token()?),
        };
        Ok((token, line_no))
    }

    /// Inserts text after the last returned token
    pub fn insert_text_at_current_position(&mut self, text: Cow<'src, str>, tag: &str) {
        self.source.insert_string_after_last_char(text, tag);
    }

    pub fn is_currently_processing_tag(&self, tag: &str) -> bool {
        self.source.currently_processing_tag(tag)
    }

    pub fn is_next_lparen(&mut self) -> bool {
        let rollback = self.source.read_state.clone();
        self.skip_blanks();
        let result = self.source.lookahead() == '(';
        self.source.read_state = rollback;
        result
    }

    pub fn new(source: &'src str) -> Self {
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
        let mut lex = CommandLexer::new(text);
        if let CommandToken::Word(word) = lex.next_token().unwrap().0 {
            assert_eq!(lex.next_token().unwrap().0, CommandToken::Eof);
            assert_eq!(word, text);
        } else {
            panic!("not a word")
        }
    }

    fn lex_token(token: &str) -> CommandToken<'_> {
        let mut lex = CommandLexer::new(token);
        let token = lex.next_token().unwrap().0;
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Eof);
        token
    }

    #[test]
    fn lex_empty_string() {
        let mut lex = CommandLexer::new("");
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Eof);
    }

    #[test]
    fn lex_skip_comment() {
        let mut lex = CommandLexer::new("# this is a comment\n");
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Newline);
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Eof);

        let mut lex = CommandLexer::new("cmd arg #comment");
        assert_eq!(
            lex.next_token().unwrap().0,
            CommandToken::Word("cmd".into())
        );
        assert_eq!(
            lex.next_token().unwrap().0,
            CommandToken::Word("arg".into())
        );
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Eof);
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
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Eof);
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
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Eof);
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
                delimiter: "end".into(),
                contents: "this\nis\n\ta\ntest\n".into()
            }
        );
        assert_eq!(
            lex_token("<<-end\nthis\nis\n\ta\n\t\t\t\ttest\nend\n"),
            CommandToken::HereDocument {
                delimiter: "end".into(),
                contents: "this\nis\na\ntest\n".into()
            }
        )
    }

    #[test]
    fn quoted_here_document() {
        assert_eq!(
            lex_token("<<\\end\nthis\nis\n\ta\ntest\nend\n"),
            CommandToken::QuotedHereDocument {
                start_delimiter: "\\end".into(),
                end_delimiter: "end".into(),
                contents: "this\nis\n\ta\ntest\n".into()
            }
        );
        assert_eq!(
            lex_token("<<-\\end\nthis\nis\n\ta\n\t\t\t\ttest\nend\n"),
            CommandToken::QuotedHereDocument {
                start_delimiter: "\\end".into(),
                end_delimiter: "end".into(),
                contents: "this\nis\na\ntest\n".into()
            }
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

    #[test]
    fn insert_text() {
        let mut lex = CommandLexer::new("a b c");
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Word("a".into()));
        lex.insert_text_at_current_position("x".into(), "x");
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Word("x".into()));
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Word("b".into()));
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Word("c".into()));
        lex.insert_text_at_current_position("y".into(), "y");
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Word("y".into()));
        assert_eq!(lex.next_token().unwrap().0, CommandToken::Eof);
    }
}
