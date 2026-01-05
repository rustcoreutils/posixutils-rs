//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Lexer for yacc grammar files

use crate::diag;
use crate::error::YaccError;

/// Token types for yacc grammar files
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// Identifier (name)
    Identifier(String),
    /// C_IDENTIFIER: identifier followed by ':'
    CIdentifier(String),
    /// Number (decimal integer)
    Number(i32),
    /// Character literal (e.g., 'a')
    CharLiteral(char),
    /// %token directive
    SymbolDecl,
    /// %left
    Left,
    /// %right
    Right,
    /// %nonassoc
    Nonassoc,
    /// %type
    Type,
    /// %start
    Start,
    /// %prec
    Prec,
    /// %expect
    Expect,
    /// %expect-rr
    ExpectRr,
    /// %% (mark)
    Mark,
    /// <tag>
    Tag(String),
    /// { ... } (action or union body)
    Action(String),
    /// C code block from %{ ... %}
    CodeBlock(String),
    /// Union body from %union { ... }
    UnionBody(String),
    /// Colon ':'
    Colon,
    /// Semicolon ';'
    Semicolon,
    /// Pipe '|'
    Pipe,
    /// Error token (reserved)
    Error,
    /// Programs section (everything after second %%)
    ProgramsSection(String),
}

/// Token with position information
#[derive(Debug, Clone)]
pub struct PositionedToken {
    pub token: Token,
    pub line: usize,
}

/// Lexer state
struct Lexer<'a> {
    input: &'a str,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    pos: usize,
    line: usize,
    column: usize,
    mark_count: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer {
            input,
            chars: input.chars().peekable(),
            pos: 0,
            line: 1,
            column: 1,
            mark_count: 0,
        }
    }

    /// Create a lexical error, also logging it via diag
    fn lexical_error(&self, line: usize, column: usize, msg: String) -> YaccError {
        diag::error(diag::Position::new(line as u32, column as u16), &msg);
        YaccError::Lexical { line, column, msg }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.pos += c.len_utf8();
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(c)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) -> bool {
        if self.peek() == Some('/') {
            let saved_pos = self.pos;
            let saved_line = self.line;
            let saved_col = self.column;

            self.advance();
            if self.peek() == Some('*') {
                self.advance();
                // Skip until */
                loop {
                    match self.advance() {
                        Some('*') => {
                            if self.peek() == Some('/') {
                                self.advance();
                                return true;
                            }
                        }
                        None => return true, // Unterminated comment, let parser handle
                        _ => {}
                    }
                }
            } else {
                // Not a comment, restore position
                // We can't easily restore, so we'll handle this differently
                // For now, return false and let the caller handle '/'
                self.pos = saved_pos;
                self.line = saved_line;
                self.column = saved_col;
                // Re-create chars iterator at saved position
                self.chars = self.input[saved_pos..].chars().peekable();
                return false;
            }
        }
        false
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            self.skip_whitespace();
            if !self.skip_comment() {
                break;
            }
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' || c == '.' {
                s.push(c);
                self.advance();
            } else {
                break;
            }
        }
        s
    }

    fn read_number(&mut self) -> Result<i32, YaccError> {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                s.push(c);
                self.advance();
            } else {
                break;
            }
        }
        s.parse::<i32>().map_err(|_| {
            self.lexical_error(self.line, self.column, format!("invalid number: {}", s))
        })
    }

    fn read_char_literal(&mut self) -> Result<char, YaccError> {
        let start_line = self.line;
        let start_col = self.column;

        self.advance(); // consume opening quote
        let c = match self.advance() {
            Some('\\') => {
                // Escape sequence - ISO C standard escape sequences
                match self.advance() {
                    // Simple escape sequences
                    Some('a') => '\x07', // Alert (bell)
                    Some('b') => '\x08', // Backspace
                    Some('f') => '\x0C', // Form feed
                    Some('n') => '\n',   // Newline
                    Some('r') => '\r',   // Carriage return
                    Some('t') => '\t',   // Horizontal tab
                    Some('v') => '\x0B', // Vertical tab
                    Some('\\') => '\\',  // Backslash
                    Some('\'') => '\'',  // Single quote
                    Some('"') => '"',    // Double quote
                    Some('?') => '?',    // Question mark (avoid trigraphs)
                    // Hex escape sequence: \xHH (one or more hex digits)
                    Some('x') => {
                        let mut hex = String::new();
                        // Read hex digits (ISO C allows any number, we limit to reasonable amount)
                        while let Some(h) = self.peek() {
                            if h.is_ascii_hexdigit() {
                                hex.push(h);
                                self.advance();
                                // Limit to 2 digits for char (8-bit value)
                                if hex.len() >= 2 {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                        if hex.is_empty() {
                            return Err(self.lexical_error(
                                start_line,
                                start_col,
                                "\\x used with no following hex digits".into(),
                            ));
                        }
                        let value = u32::from_str_radix(&hex, 16).unwrap_or(0);
                        char::from_u32(value).unwrap_or('\0')
                    }
                    // Octal escape sequence: \OOO (1-3 octal digits)
                    Some(c) if ('0'..='7').contains(&c) => {
                        let mut oct = String::new();
                        oct.push(c);
                        // Read up to 2 more octal digits
                        for _ in 0..2 {
                            if let Some(o) = self.peek() {
                                if ('0'..='7').contains(&o) {
                                    oct.push(o);
                                    self.advance();
                                } else {
                                    break;
                                }
                            }
                        }
                        let value = u32::from_str_radix(&oct, 8).unwrap_or(0);
                        char::from_u32(value).unwrap_or('\0')
                    }
                    // Unknown escape - per ISO C, this should produce a diagnostic
                    // For yacc compatibility, we pass through the character
                    Some(c) => {
                        // Note: ISO C requires diagnostic for unknown escapes
                        // Traditional yacc just passes the character through
                        c
                    }
                    None => {
                        return Err(self.lexical_error(
                            start_line,
                            start_col,
                            "unterminated character literal".into(),
                        ))
                    }
                }
            }
            Some(c) => c,
            None => {
                return Err(self.lexical_error(
                    start_line,
                    start_col,
                    "unterminated character literal".into(),
                ))
            }
        };

        if self.advance() != Some('\'') {
            return Err(self.lexical_error(
                start_line,
                start_col,
                "unterminated character literal".into(),
            ));
        }

        Ok(c)
    }

    fn read_tag(&mut self) -> Result<String, YaccError> {
        let start_line = self.line;
        let start_col = self.column;

        self.advance(); // consume '<'
        let mut tag = String::new();
        loop {
            match self.advance() {
                Some('>') => break,
                Some(c) => tag.push(c),
                None => {
                    return Err(self.lexical_error(
                        start_line,
                        start_col,
                        "unterminated tag".into(),
                    ))
                }
            }
        }
        Ok(tag)
    }

    fn read_action(&mut self) -> Result<String, YaccError> {
        let start_line = self.line;
        let start_col = self.column;

        self.advance(); // consume '{'
        let mut action = String::new();
        let mut depth = 1;

        while depth > 0 {
            match self.advance() {
                Some('{') => {
                    depth += 1;
                    action.push('{');
                }
                Some('}') => {
                    depth -= 1;
                    if depth > 0 {
                        action.push('}');
                    }
                }
                Some('\'') => {
                    // Character literal in C code
                    action.push('\'');
                    loop {
                        match self.advance() {
                            Some('\\') => {
                                action.push('\\');
                                if let Some(c) = self.advance() {
                                    action.push(c);
                                }
                            }
                            Some('\'') => {
                                action.push('\'');
                                break;
                            }
                            Some(c) => action.push(c),
                            None => break,
                        }
                    }
                }
                Some('"') => {
                    // String literal in C code
                    action.push('"');
                    loop {
                        match self.advance() {
                            Some('\\') => {
                                action.push('\\');
                                if let Some(c) = self.advance() {
                                    action.push(c);
                                }
                            }
                            Some('"') => {
                                action.push('"');
                                break;
                            }
                            Some(c) => action.push(c),
                            None => break,
                        }
                    }
                }
                Some('/') => {
                    action.push('/');
                    if self.peek() == Some('*') {
                        // C comment
                        action.push(self.advance().unwrap());
                        loop {
                            match self.advance() {
                                Some('*') => {
                                    action.push('*');
                                    if self.peek() == Some('/') {
                                        action.push(self.advance().unwrap());
                                        break;
                                    }
                                }
                                Some(c) => action.push(c),
                                None => break,
                            }
                        }
                    } else if self.peek() == Some('/') {
                        // C++ comment
                        action.push(self.advance().unwrap());
                        while let Some(c) = self.advance() {
                            action.push(c);
                            if c == '\n' {
                                break;
                            }
                        }
                    }
                }
                Some(c) => action.push(c),
                None => {
                    return Err(self.lexical_error(
                        start_line,
                        start_col,
                        "unterminated action".into(),
                    ))
                }
            }
        }

        Ok(action)
    }

    fn read_code_block(&mut self) -> Result<String, YaccError> {
        // Already consumed %{
        let start_line = self.line;
        let start_col = self.column;

        let mut code = String::new();
        loop {
            match self.advance() {
                Some('%') => {
                    if self.peek() == Some('}') {
                        self.advance();
                        break;
                    } else {
                        code.push('%');
                    }
                }
                Some(c) => code.push(c),
                None => {
                    return Err(self.lexical_error(
                        start_line,
                        start_col,
                        "unterminated %{ %}".into(),
                    ))
                }
            }
        }

        Ok(code)
    }

    fn read_union_body(&mut self) -> Result<String, YaccError> {
        // Skip whitespace to find opening brace
        self.skip_whitespace_and_comments();

        if self.peek() != Some('{') {
            return Err(self.lexical_error(
                self.line,
                self.column,
                "expected '{' after %union".into(),
            ));
        }

        // read_action consumes the braces but doesn't include them in output
        // For union bodies, we need the braces for valid C syntax
        let body = self.read_action()?;
        Ok(format!("{{{}}}", body))
    }

    fn next_token(&mut self) -> Result<Option<PositionedToken>, YaccError> {
        self.skip_whitespace_and_comments();

        let line = self.line;

        let c = match self.peek() {
            Some(c) => c,
            None => return Ok(None),
        };

        let token = match c {
            '%' => {
                self.advance();
                match self.peek() {
                    Some('%') => {
                        self.advance();
                        self.mark_count += 1;
                        if self.mark_count == 2 {
                            // Read rest of input as programs section
                            let rest: String = self.chars.by_ref().collect();
                            Token::ProgramsSection(rest)
                        } else {
                            Token::Mark
                        }
                    }
                    Some('{') => {
                        self.advance();
                        let code = self.read_code_block()?;
                        Token::CodeBlock(code)
                    }
                    Some(c) if c.is_alphabetic() => {
                        let word = self.read_identifier();
                        match word.as_str() {
                            "token" => Token::SymbolDecl,
                            "left" => Token::Left,
                            "right" => Token::Right,
                            "nonassoc" => Token::Nonassoc,
                            "type" => Token::Type,
                            "start" => Token::Start,
                            "union" => {
                                let body = self.read_union_body()?;
                                Token::UnionBody(body)
                            }
                            "prec" => Token::Prec,
                            "expect" => {
                                // Check for %expect-rr by looking ahead without consuming
                                let remaining = &self.input[self.pos..];
                                if remaining.starts_with("-rr")
                                    && !remaining[3..]
                                        .chars()
                                        .next()
                                        .is_some_and(|c| c.is_alphanumeric())
                                {
                                    // Consume the "-rr" suffix
                                    self.advance(); // '-'
                                    self.advance(); // 'r'
                                    self.advance(); // 'r'
                                    Token::ExpectRr
                                } else {
                                    Token::Expect
                                }
                            }
                            _ => {
                                return Err(self.lexical_error(
                                    line,
                                    self.column,
                                    format!("unknown directive: %{}", word),
                                ))
                            }
                        }
                    }
                    _ => {
                        return Err(self.lexical_error(
                            line,
                            self.column,
                            "invalid character after '%'".into(),
                        ))
                    }
                }
            }
            ':' => {
                self.advance();
                Token::Colon
            }
            ';' => {
                self.advance();
                Token::Semicolon
            }
            '|' => {
                self.advance();
                Token::Pipe
            }
            '<' => {
                let tag = self.read_tag()?;
                Token::Tag(tag)
            }
            '{' => {
                let action = self.read_action()?;
                Token::Action(action)
            }
            '\'' => {
                let ch = self.read_char_literal()?;
                Token::CharLiteral(ch)
            }
            c if c.is_ascii_digit() => {
                let n = self.read_number()?;
                Token::Number(n)
            }
            c if c.is_alphabetic() || c == '_' || c == '.' => {
                let ident = self.read_identifier();

                // Check if this is a C_IDENTIFIER (followed by colon)
                self.skip_whitespace_and_comments();
                if self.peek() == Some(':') {
                    self.advance();
                    if ident == "error" {
                        // 'error:' is special - it's the error token used as a non-terminal
                        Token::CIdentifier(ident)
                    } else {
                        Token::CIdentifier(ident)
                    }
                } else if ident == "error" {
                    Token::Error
                } else {
                    Token::Identifier(ident)
                }
            }
            _ => {
                return Err(self.lexical_error(
                    line,
                    self.column,
                    format!("unexpected character: '{}'", c),
                ))
            }
        };

        Ok(Some(PositionedToken { token, line }))
    }
}

/// Lex the input string into a vector of tokens
pub fn lex(input: &str) -> Result<Vec<PositionedToken>, YaccError> {
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next_token()? {
        tokens.push(token);
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let input = "%token FOO BAR";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 3);
        assert!(matches!(tokens[0].token, Token::SymbolDecl));
        assert!(matches!(&tokens[1].token, Token::Identifier(s) if s == "FOO"));
        assert!(matches!(&tokens[2].token, Token::Identifier(s) if s == "BAR"));
    }

    #[test]
    fn test_c_identifier() {
        let input = "expr : term";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(&tokens[0].token, Token::CIdentifier(s) if s == "expr"));
        assert!(matches!(&tokens[1].token, Token::Identifier(s) if s == "term"));
    }

    #[test]
    fn test_char_literal() {
        let input = "'a' '\\n' '\\t'";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 3);
        assert!(matches!(tokens[0].token, Token::CharLiteral('a')));
        assert!(matches!(tokens[1].token, Token::CharLiteral('\n')));
        assert!(matches!(tokens[2].token, Token::CharLiteral('\t')));
    }

    #[test]
    fn test_escape_sequences_simple() {
        // Test all simple escape sequences from ISO C
        // Note: we use separate tests for some escapes due to Rust raw string limitations
        let input = r"'\a' '\b' '\f' '\n' '\r' '\t' '\v' '\\' '\'' '\?'";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 10);
        assert!(matches!(tokens[0].token, Token::CharLiteral('\x07'))); // \a - alert
        assert!(matches!(tokens[1].token, Token::CharLiteral('\x08'))); // \b - backspace
        assert!(matches!(tokens[2].token, Token::CharLiteral('\x0C'))); // \f - form feed
        assert!(matches!(tokens[3].token, Token::CharLiteral('\n'))); // \n - newline
        assert!(matches!(tokens[4].token, Token::CharLiteral('\r'))); // \r - carriage return
        assert!(matches!(tokens[5].token, Token::CharLiteral('\t'))); // \t - horizontal tab
        assert!(matches!(tokens[6].token, Token::CharLiteral('\x0B'))); // \v - vertical tab
        assert!(matches!(tokens[7].token, Token::CharLiteral('\\'))); // \\ - backslash
        assert!(matches!(tokens[8].token, Token::CharLiteral('\''))); // \' - single quote
        assert!(matches!(tokens[9].token, Token::CharLiteral('?'))); // \? - question mark
    }

    #[test]
    fn test_escape_sequence_double_quote() {
        // Test double quote escape separately (can't use raw string)
        let input = "'\\\"'";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 1);
        assert!(matches!(tokens[0].token, Token::CharLiteral('"'))); // \" - double quote
    }

    #[test]
    fn test_escape_sequences_octal() {
        // Test octal escape sequences
        let input = r"'\0' '\7' '\77' '\177' '\1'";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 5);
        assert!(matches!(tokens[0].token, Token::CharLiteral('\0'))); // \0 - null
        assert!(matches!(tokens[1].token, Token::CharLiteral('\x07'))); // \7 - bell (7 octal = 7)
        assert!(matches!(tokens[2].token, Token::CharLiteral('\x3F'))); // \77 - question mark (77 octal = 63)
        assert!(matches!(tokens[3].token, Token::CharLiteral('\x7F'))); // \177 - DEL (177 octal = 127)
        assert!(matches!(tokens[4].token, Token::CharLiteral('\x01'))); // \1 - SOH
    }

    #[test]
    fn test_escape_sequences_hex() {
        // Test hex escape sequences
        let input = r"'\x00' '\x41' '\x7f' '\xff' '\xA'";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 5);
        assert!(matches!(tokens[0].token, Token::CharLiteral('\0'))); // \x00 - null
        assert!(matches!(tokens[1].token, Token::CharLiteral('A'))); // \x41 - 'A'
        assert!(matches!(tokens[2].token, Token::CharLiteral('\x7F'))); // \x7f - DEL
        assert!(matches!(tokens[3].token, Token::CharLiteral('\u{FF}'))); // \xff - 255
        assert!(matches!(tokens[4].token, Token::CharLiteral('\x0A'))); // \xA - newline (single digit)
    }

    #[test]
    fn test_escape_sequence_invalid_hex() {
        // Test that \x with no hex digits produces an error
        let input = r"'\xZ'";
        let result = lex(input);
        assert!(result.is_err());
        if let Err(YaccError::Lexical { msg, .. }) = result {
            assert!(msg.contains("hex"));
        }
    }

    #[test]
    fn test_action() {
        let input = "{ printf(\"hello\"); }";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0].token, Token::Action(s) if s.contains("printf")));
    }

    #[test]
    fn test_mark() {
        let input = "%%\n%%";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::Mark));
        assert!(matches!(tokens[1].token, Token::ProgramsSection(_)));
    }

    #[test]
    fn test_precedence() {
        let input = "%left '+' '-'\n%left '*' '/'";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 6);
        assert!(matches!(tokens[0].token, Token::Left));
        assert!(matches!(tokens[3].token, Token::Left));
    }

    #[test]
    fn test_code_block() {
        let input = "%{\n#include <stdio.h>\n%}";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0].token, Token::CodeBlock(s) if s.contains("stdio.h")));
    }

    #[test]
    fn test_union() {
        let input = "%union { int ival; double dval; }";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 1);
        // Union body must include braces for valid C syntax in generated code
        assert!(
            matches!(&tokens[0].token, Token::UnionBody(s) if s.starts_with('{') && s.ends_with('}') && s.contains("ival"))
        );
    }

    #[test]
    fn test_tag() {
        let input = "%token <ival> NUM";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 3);
        assert!(matches!(tokens[0].token, Token::SymbolDecl));
        assert!(matches!(&tokens[1].token, Token::Tag(s) if s == "ival"));
        assert!(matches!(&tokens[2].token, Token::Identifier(s) if s == "NUM"));
    }

    #[test]
    fn test_number() {
        let input = "%token NUM 256";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 3);
        assert!(matches!(tokens[2].token, Token::Number(256)));
    }

    #[test]
    fn test_comment() {
        let input = "%token /* comment */ FOO";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::SymbolDecl));
        assert!(matches!(&tokens[1].token, Token::Identifier(s) if s == "FOO"));
    }

    #[test]
    fn test_expect() {
        let input = "%expect 5";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::Expect));
        assert!(matches!(tokens[1].token, Token::Number(5)));
    }

    #[test]
    fn test_expect_rr() {
        let input = "%expect-rr 3";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::ExpectRr));
        assert!(matches!(tokens[1].token, Token::Number(3)));
    }

    #[test]
    fn test_expect_zero() {
        let input = "%expect 0";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 2);
        assert!(matches!(tokens[0].token, Token::Expect));
        assert!(matches!(tokens[1].token, Token::Number(0)));
    }

    #[test]
    fn test_expect_both() {
        let input = "%expect 2\n%expect-rr 1";
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 4);
        assert!(matches!(tokens[0].token, Token::Expect));
        assert!(matches!(tokens[1].token, Token::Number(2)));
        assert!(matches!(tokens[2].token, Token::ExpectRr));
        assert!(matches!(tokens[3].token, Token::Number(1)));
    }
}
