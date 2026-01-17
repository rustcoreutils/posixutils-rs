//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Lexer module for pcc - C99 Tokenizer
// Implements C99 preprocessing token lexing (pp-number, pp-tokens)
//

use crate::diag;
use crate::strings::{StringId, StringTable};

// Re-export Position for use by other modules
pub use crate::diag::Position;

// ============================================================================
// Token Types
// ============================================================================

/// Token types for C99 preprocessing tokens
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Ident,
    Number,
    Char,
    WideChar,
    String,
    WideString,
    Special,
    StreamBegin,
    StreamEnd,
}

/// Special tokens (operators and punctuators)
/// Values >= SPECIAL_BASE are multi-character operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum SpecialToken {
    // Single character - stored as their ASCII value
    // Multi-character operators start at 256
    AddAssign = 256, // +=
    Increment,       // ++
    SubAssign,       // -=
    Decrement,       // --
    Arrow,           // ->
    MulAssign,       // *=
    DivAssign,       // /=
    ModAssign,       // %=
    Lte,             // <=
    Gte,             // >=
    Equal,           // ==
    NotEqual,        // !=
    LogicalAnd,      // &&
    AndAssign,       // &=
    LogicalOr,       // ||
    OrAssign,        // |=
    XorAssign,       // ^=
    HashHash,        // ##
    LeftShift,       // <<
    RightShift,      // >>
    DotDot,          // ..
    ShlAssign,       // <<=
    ShrAssign,       // >>=
    Ellipsis,        // ...
}

impl SpecialToken {
    pub const BASE: u32 = 256;
}

// Position is imported from crate::diag

// ============================================================================
// Identifier Interning
// ============================================================================

/// Identifier intern table - now a re-export of StringTable
/// Kept for backward compatibility during transition
pub type IdentTable = StringTable;

// ============================================================================
// Token Value
// ============================================================================

/// Token value - type-specific payload for each token kind
#[derive(Debug, Clone)]
pub enum TokenValue {
    None,
    Number(String),     // Numeric literal as string (pp-number)
    Ident(StringId),    // Identifier (interned StringId)
    Special(u32),       // Operator/punctuator
    String(String),     // String literal content
    Char(String),       // Character literal content
    WideString(String), // Wide string literal
    WideChar(String),   // Wide character literal
}

// ============================================================================
// Token
// ============================================================================

/// A C token
#[derive(Debug, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub pos: Position,
    pub value: TokenValue,
    /// Macros that should not expand this token (C preprocessor "blue painting").
    /// When a macro's expansion contains its own name, those tokens are marked.
    /// This prevents re-expansion in nested contexts per C99 6.10.3.4.
    pub no_expand: Option<std::collections::HashSet<String>>,
}

impl Token {
    pub fn new(typ: TokenType, pos: Position) -> Self {
        Self {
            typ,
            pos,
            value: TokenValue::None,
            no_expand: None,
        }
    }

    pub fn with_value(typ: TokenType, pos: Position, value: TokenValue) -> Self {
        Self {
            typ,
            pos,
            value,
            no_expand: None,
        }
    }

    /// Mark this token as not expandable for the given macro name
    pub fn mark_no_expand(&mut self, macro_name: &str) {
        if self.no_expand.is_none() {
            self.no_expand = Some(std::collections::HashSet::new());
        }
        self.no_expand
            .as_mut()
            .unwrap()
            .insert(macro_name.to_string());
    }

    /// Check if this token should not be expanded for the given macro
    pub fn is_no_expand(&self, macro_name: &str) -> bool {
        self.no_expand
            .as_ref()
            .is_some_and(|set| set.contains(macro_name))
    }
}

// ============================================================================
// Character Classification
// ============================================================================

/// Character class flags for lexer character classification
const LETTER: u8 = 1;
const DIGIT: u8 = 2;
const HEX: u8 = 4;
const EXP: u8 = 8;
const DOT: u8 = 16;
const VALID_SECOND: u8 = 32; // Can be second char of 2-char operator

/// Character classification table
fn char_class(c: u8) -> u8 {
    match c {
        b'0'..=b'9' => DIGIT | HEX,
        b'A'..=b'D' | b'F' => LETTER | HEX,
        b'E' => LETTER | HEX | EXP, // E for exponent
        b'G'..=b'O' => LETTER,
        b'P' => LETTER | EXP, // P for hex float exponent
        b'Q'..=b'Z' => LETTER,
        b'a'..=b'd' | b'f' => LETTER | HEX,
        b'e' => LETTER | HEX | EXP,
        b'g'..=b'o' => LETTER,
        b'p' => LETTER | EXP,
        b'q'..=b'z' => LETTER,
        b'_' => LETTER,
        b'.' => DOT | VALID_SECOND,
        b'=' | b'+' | b'-' | b'>' | b'<' | b'&' | b'|' | b'#' => VALID_SECOND,
        _ => 0,
    }
}

#[inline]
fn is_digit(c: u8) -> bool {
    char_class(c) & DIGIT != 0
}

#[inline]
fn is_letter_or_digit(c: u8) -> bool {
    char_class(c) & (LETTER | DIGIT) != 0
}

// ============================================================================
// Stream (Input Source)
// ============================================================================

// Stream management is now in crate::diag

/// Stream table wrapper for backward compatibility
/// Uses the global thread-local StreamRegistry from diag
pub struct StreamTable;

impl StreamTable {
    pub fn new() -> Self {
        diag::clear_streams();
        Self
    }

    pub fn add(&mut self, name: String) -> u16 {
        diag::init_stream(&name)
    }
}

impl Default for StreamTable {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Tokenizer
// ============================================================================

const EOF: i32 = -1;

/// C99 tokenizer with line splicing and position tracking
pub struct Tokenizer<'a, 'b> {
    // Input
    buffer: &'a [u8],
    offset: usize,

    // Position tracking
    stream_id: u16,
    line: u32,
    col: u16,
    newline: bool,
    whitespace: bool,

    // Interning - shared string table
    strings: &'b mut StringTable,
}

impl<'a, 'b> Tokenizer<'a, 'b> {
    pub fn new(buffer: &'a [u8], stream_id: u16, strings: &'b mut StringTable) -> Self {
        Self {
            buffer,
            offset: 0,
            stream_id,
            line: 1,
            col: 0,
            newline: true,
            whitespace: false,
            strings,
        }
    }

    /// Get current position
    fn pos(&self) -> Position {
        let mut pos = Position::new(self.stream_id, self.line, self.col);
        pos.newline = self.newline;
        pos.whitespace = self.whitespace;
        pos
    }

    /// Get next character, handling line splicing (backslash-newline)
    fn nextchar(&mut self) -> i32 {
        loop {
            if self.offset >= self.buffer.len() {
                return EOF;
            }

            let c = self.buffer[self.offset] as i32;
            self.offset += 1;

            // Handle carriage return
            if c == b'\r' as i32 {
                // Check for \r\n
                if self.offset < self.buffer.len() && self.buffer[self.offset] == b'\n' {
                    self.offset += 1;
                }
                self.line += 1;
                self.col = 0;
                self.newline = true;
                return b'\n' as i32;
            }

            // Handle newline
            if c == b'\n' as i32 {
                self.line += 1;
                self.col = 0;
                self.newline = true;
                return c;
            }

            // Handle backslash (potential line splice)
            if c == b'\\' as i32 && self.offset < self.buffer.len() {
                let next = self.buffer[self.offset];
                if next == b'\n' {
                    // Line splice: skip backslash-newline
                    self.offset += 1;
                    self.line += 1;
                    self.col = 0;
                    continue;
                } else if next == b'\r' {
                    // Line splice with \r or \r\n
                    self.offset += 1;
                    if self.offset < self.buffer.len() && self.buffer[self.offset] == b'\n' {
                        self.offset += 1;
                    }
                    self.line += 1;
                    self.col = 0;
                    continue;
                }
            }

            // Handle tab
            if c == b'\t' as i32 {
                self.col = (self.col + 8) & !7; // Round to next multiple of 8
            } else {
                self.col += 1;
            }

            return c;
        }
    }

    /// Peek at next character without consuming (handles line splicing)
    fn peekchar(&self) -> i32 {
        let mut offset = self.offset;
        loop {
            if offset >= self.buffer.len() {
                return EOF;
            }
            let c = self.buffer[offset];

            // Handle backslash (potential line splice)
            if c == b'\\' && offset + 1 < self.buffer.len() {
                let next = self.buffer[offset + 1];
                if next == b'\n' {
                    // Skip backslash-newline
                    offset += 2;
                    continue;
                } else if next == b'\r' {
                    // Skip backslash-CR or backslash-CRLF
                    offset += 2;
                    if offset < self.buffer.len() && self.buffer[offset] == b'\n' {
                        offset += 1;
                    }
                    continue;
                }
            }

            // Handle \r as \n
            if c == b'\r' {
                return b'\n' as i32;
            }

            return c as i32;
        }
    }

    /// Skip whitespace, tracking whitespace/newline flags
    fn skip_whitespace(&mut self) -> i32 {
        loop {
            let c = self.nextchar();
            if c == EOF {
                return EOF;
            }
            match c as u8 {
                b' ' | b'\t' | b'\x0C' | b'\x0B' => {
                    self.whitespace = true;
                }
                b'\n' => {
                    self.newline = true;
                    self.whitespace = true;
                }
                _ => return c,
            }
        }
    }

    /// Get a pp-number token
    /// pp-number: digit | . digit | pp-number (digit|letter|.|e[+-]|E[+-]|p[+-]|P[+-])
    fn get_number(&mut self, first: u8) -> Token {
        let pos = self.pos();
        let mut num = String::new();
        num.push(first as char);

        loop {
            // Use peek to avoid consuming characters that would affect line/col tracking
            let c = self.peekchar();
            if c == EOF {
                break;
            }
            let cu = c as u8;
            let class = char_class(cu);

            // Continue if digit, letter, or dot
            if class & (DIGIT | LETTER | DOT) != 0 {
                self.nextchar(); // Now consume it
                num.push(cu as char);

                // Handle exponent sign (e+, e-, E+, E-, p+, p-, P+, P-)
                if class & EXP != 0 {
                    let next = self.peekchar();
                    if next == b'+' as i32 || next == b'-' as i32 {
                        num.push(self.nextchar() as u8 as char);
                    }
                }
            } else {
                break;
            }
        }

        Token::with_value(TokenType::Number, pos, TokenValue::Number(num))
    }

    /// Peek at buffer to check if there's a valid UCN sequence starting at current position.
    /// If the buffer contains \uXXXX or \UXXXXXXXX (where X is hex digit), returns
    /// Some((decoded_char, bytes_consumed)) where bytes_consumed includes the backslash.
    /// Returns None if not a valid UCN.
    fn peek_ucn(&self) -> Option<(char, usize)> {
        let mut offset = self.offset;

        // Skip any line splices to find the actual backslash
        loop {
            if offset >= self.buffer.len() {
                return None;
            }
            let c = self.buffer[offset];
            if c == b'\\' && offset + 1 < self.buffer.len() {
                let next = self.buffer[offset + 1];
                if next == b'\n' {
                    offset += 2;
                    continue;
                } else if next == b'\r' {
                    offset += 2;
                    if offset < self.buffer.len() && self.buffer[offset] == b'\n' {
                        offset += 1;
                    }
                    continue;
                }
                // Found non-splice backslash
                break;
            }
            // Not a backslash at all
            return None;
        }

        // Now offset points to backslash, check for 'u' or 'U'
        if offset + 1 >= self.buffer.len() {
            return None;
        }

        let u_char = self.buffer[offset + 1];
        let expected_digits = match u_char {
            b'u' => 4,
            b'U' => 8,
            _ => return None,
        };

        // Check we have enough hex digits
        if offset + 2 + expected_digits > self.buffer.len() {
            return None;
        }

        let hex_start = offset + 2;
        let hex_end = hex_start + expected_digits;

        // Validate all characters are hex digits
        for i in hex_start..hex_end {
            if !self.buffer[i].is_ascii_hexdigit() {
                return None;
            }
        }

        // Parse the hex value
        let hex_str: String = self.buffer[hex_start..hex_end]
            .iter()
            .map(|&b| b as char)
            .collect();
        let val = u32::from_str_radix(&hex_str, 16).ok()?;
        let ch = char::from_u32(val)?;

        // Calculate bytes consumed from self.offset
        let bytes_consumed = hex_end - self.offset;
        Some((ch, bytes_consumed))
    }

    /// Try to consume a UCN sequence. If successful, returns the decoded character.
    /// Otherwise returns None and leaves position unchanged.
    fn try_consume_ucn(&mut self) -> Option<char> {
        if let Some((ch, bytes)) = self.peek_ucn() {
            // Consume the bytes by calling nextchar the right number of times
            // This properly handles line/col tracking
            for _ in 0..bytes {
                self.nextchar();
            }
            Some(ch)
        } else {
            None
        }
    }

    /// Try to consume a UCN sequence when the backslash has already been consumed.
    /// Expects the next character to be 'u' or 'U'.
    /// Returns the decoded character if successful, None otherwise.
    fn try_consume_ucn_after_backslash(&mut self) -> Option<char> {
        let c = self.peekchar();
        if c == EOF {
            return None;
        }

        let expected_digits = match c as u8 {
            b'u' => 4,
            b'U' => 8,
            _ => return None,
        };

        // Check we have enough hex digits after the u/U
        // Peek ahead without consuming
        let mut offset = self.offset;
        // Skip 'u' or 'U'
        offset += 1;

        if offset + expected_digits > self.buffer.len() {
            return None;
        }

        // Validate all characters are hex digits
        for i in 0..expected_digits {
            if !self.buffer[offset + i].is_ascii_hexdigit() {
                return None;
            }
        }

        // Now consume and parse
        self.nextchar(); // consume 'u' or 'U'

        let mut hex = String::new();
        for _ in 0..expected_digits {
            hex.push(self.nextchar() as u8 as char);
        }

        let val = u32::from_str_radix(&hex, 16).ok()?;
        char::from_u32(val)
    }

    /// Get an identifier token
    fn get_identifier(&mut self, first: u8) -> Token {
        let pos = self.pos();
        let mut name = String::new();
        name.push(first as char);

        loop {
            // Use peek to avoid consuming characters that would affect line/col tracking
            let c = self.peekchar();
            if c == EOF {
                break;
            }
            let cu = c as u8;

            // Check for UCN escape sequence (\uXXXX or \UXXXXXXXX) - C99 6.4.3
            if cu == b'\\' {
                if let Some(uc) = self.try_consume_ucn() {
                    name.push(uc);
                    continue;
                }
                // Not a valid UCN, end identifier
                break;
            }

            if is_letter_or_digit(cu) {
                self.nextchar(); // Now consume it
                name.push(cu as char);
            } else {
                // Check for L"..." or L'...' (wide string/char)
                if name == "L" && (cu == b'"' || cu == b'\'') {
                    self.nextchar(); // Consume the quote
                    return self.get_string_or_char(cu, true);
                }
                break;
            }
        }

        let id = self.strings.intern(&name);
        Token::with_value(TokenType::Ident, pos, TokenValue::Ident(id))
    }

    /// Get an identifier token starting with a UCN character (already consumed)
    fn get_identifier_from_ucn(&mut self, first_ucn: char) -> Token {
        let pos = self.pos();
        let mut name = String::new();
        name.push(first_ucn);

        loop {
            let c = self.peekchar();
            if c == EOF {
                break;
            }
            let cu = c as u8;

            // Check for UCN escape sequence
            if cu == b'\\' {
                if let Some(uc) = self.try_consume_ucn() {
                    name.push(uc);
                    continue;
                }
                break;
            }

            if is_letter_or_digit(cu) {
                self.nextchar();
                name.push(cu as char);
            } else {
                break;
            }
        }

        let id = self.strings.intern(&name);
        Token::with_value(TokenType::Ident, pos, TokenValue::Ident(id))
    }

    /// Get a string or character literal
    fn get_string_or_char(&mut self, delim: u8, wide: bool) -> Token {
        let pos = self.pos();
        let mut content = String::new();
        let mut escape = false;
        let mut want_hex = false; // Track if we just saw \x

        loop {
            let c = self.nextchar();
            if c == EOF {
                // Unterminated string/char - emit warning
                diag::warning(pos, "End of file in middle of string");
                break;
            }
            let cu = c as u8;

            // Check for \x without hex digits
            if want_hex {
                if !cu.is_ascii_hexdigit() {
                    diag::warning(pos, "\\x used with no following hex digits");
                }
                want_hex = false;
            }

            if escape {
                content.push(cu as char);
                escape = false;
                // Track if this is \x escape
                if cu == b'x' {
                    want_hex = true;
                }
                continue;
            }

            if cu == b'\\' {
                content.push(cu as char);
                escape = true;
                continue;
            }

            if cu == delim {
                // End of literal
                break;
            }

            if cu == b'\n' {
                // Error: newline in string/char literal - emit warning
                let delim_char = if delim == b'"' { '"' } else { '\'' };
                diag::warning(
                    pos,
                    &format!("missing terminating {} character", delim_char),
                );
                break;
            }

            content.push(cu as char);
        }

        // Check for trailing \x at end of string
        if want_hex {
            diag::warning(pos, "\\x used with no following hex digits");
        }

        let (typ, value) = if delim == b'"' {
            if wide {
                (TokenType::WideString, TokenValue::WideString(content))
            } else {
                (TokenType::String, TokenValue::String(content))
            }
        } else if wide {
            (TokenType::WideChar, TokenValue::WideChar(content))
        } else {
            (TokenType::Char, TokenValue::Char(content))
        };

        Token::with_value(typ, pos, value)
    }

    /// Skip a single-line comment (// ...)
    fn skip_line_comment(&mut self) {
        loop {
            let c = self.nextchar();
            if c == EOF || c == b'\n' as i32 {
                break;
            }
        }
    }

    /// Skip a block comment (/* ... */)
    fn skip_block_comment(&mut self) {
        let pos = self.pos(); // Save position for warning
                              // Track both current and next character to properly detect */
                              // This handles cases like /***/ or /**/
        let mut next = self.nextchar();
        loop {
            let curr = next;
            if curr == EOF {
                // Unterminated comment - emit warning
                diag::warning(pos, "End of file in the middle of a comment");
                break;
            }
            next = self.nextchar();
            if curr == b'*' as i32 && next == b'/' as i32 {
                break;
            }
        }
        // Reset newline flag - comments don't create new logical lines.
        // This is important for multi-line comments inside macro definitions:
        // the token after the comment should NOT have newline=true just because
        // the comment spanned multiple lines.
        self.newline = false;
    }

    /// Get a special token (operator/punctuator)
    fn get_special(&mut self, first: u8) -> Option<Token> {
        let pos = self.pos();

        // Check for string/char literals
        if first == b'"' {
            return Some(self.get_string_or_char(b'"', false));
        }
        if first == b'\'' {
            return Some(self.get_string_or_char(b'\'', false));
        }

        // Check for .digit (floating point number)
        if first == b'.' {
            let next = self.peekchar();
            if next != EOF && is_digit(next as u8) {
                return Some(self.get_number(first));
            }
        }

        // Check for comments
        if first == b'/' {
            let next = self.peekchar();
            if next == b'/' as i32 {
                self.nextchar();
                self.skip_line_comment();
                return None; // No token, continue tokenizing
            }
            if next == b'*' as i32 {
                self.nextchar();
                self.skip_block_comment();
                return None; // No token, continue tokenizing
            }
        }

        // Two-character operator lookup table
        // Format: (first, second, code)
        static TWO_CHAR_OPS: &[(u8, u8, u32)] = &[
            (b'+', b'=', SpecialToken::AddAssign as u32),
            (b'+', b'+', SpecialToken::Increment as u32),
            (b'-', b'=', SpecialToken::SubAssign as u32),
            (b'-', b'-', SpecialToken::Decrement as u32),
            (b'-', b'>', SpecialToken::Arrow as u32),
            (b'*', b'=', SpecialToken::MulAssign as u32),
            (b'/', b'=', SpecialToken::DivAssign as u32),
            (b'%', b'=', SpecialToken::ModAssign as u32),
            (b'<', b'=', SpecialToken::Lte as u32),
            (b'>', b'=', SpecialToken::Gte as u32),
            (b'=', b'=', SpecialToken::Equal as u32),
            (b'!', b'=', SpecialToken::NotEqual as u32),
            (b'&', b'&', SpecialToken::LogicalAnd as u32),
            (b'&', b'=', SpecialToken::AndAssign as u32),
            (b'|', b'|', SpecialToken::LogicalOr as u32),
            (b'|', b'=', SpecialToken::OrAssign as u32),
            (b'^', b'=', SpecialToken::XorAssign as u32),
            (b'#', b'#', SpecialToken::HashHash as u32),
            (b'<', b'<', SpecialToken::LeftShift as u32),
            (b'>', b'>', SpecialToken::RightShift as u32),
            (b'.', b'.', SpecialToken::DotDot as u32),
        ];

        // Check for two-character operators
        let next = self.peekchar();
        if next != EOF {
            let next_u8 = next as u8;
            let class = char_class(next_u8);

            if class & VALID_SECOND != 0 {
                for &(c0, c1, code) in TWO_CHAR_OPS {
                    if first == c0 && next_u8 == c1 {
                        self.nextchar(); // Consume second char

                        // Check for three-character operators
                        let third = self.peekchar();
                        if third != EOF {
                            let third_u8 = third as u8;
                            // <<= or >>=
                            if code == SpecialToken::LeftShift as u32 && third_u8 == b'=' {
                                self.nextchar();
                                return Some(Token::with_value(
                                    TokenType::Special,
                                    pos,
                                    TokenValue::Special(SpecialToken::ShlAssign as u32),
                                ));
                            }
                            if code == SpecialToken::RightShift as u32 && third_u8 == b'=' {
                                self.nextchar();
                                return Some(Token::with_value(
                                    TokenType::Special,
                                    pos,
                                    TokenValue::Special(SpecialToken::ShrAssign as u32),
                                ));
                            }
                            // ...
                            if code == SpecialToken::DotDot as u32 && third_u8 == b'.' {
                                self.nextchar();
                                return Some(Token::with_value(
                                    TokenType::Special,
                                    pos,
                                    TokenValue::Special(SpecialToken::Ellipsis as u32),
                                ));
                            }
                        }

                        return Some(Token::with_value(
                            TokenType::Special,
                            pos,
                            TokenValue::Special(code),
                        ));
                    }
                }
            }
        }

        // Single character operator
        Some(Token::with_value(
            TokenType::Special,
            pos,
            TokenValue::Special(first as u32),
        ))
    }

    /// Get one token
    fn get_one_token(&mut self, c: u8) -> Option<Token> {
        let class = char_class(c);

        if class & DIGIT != 0 {
            return Some(self.get_number(c));
        }

        if class & LETTER != 0 {
            return Some(self.get_identifier(c));
        }

        // Check for UCN starting an identifier (\uXXXX or \UXXXXXXXX) - C99 6.4.3
        // UCNs can appear at the start of an identifier
        // Note: The backslash has already been consumed by skip_whitespace
        if c == b'\\' {
            if let Some(uc) = self.try_consume_ucn_after_backslash() {
                return Some(self.get_identifier_from_ucn(uc));
            }
        }

        self.get_special(c)
    }

    /// Tokenize the entire input, returning all tokens
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        // Add stream begin token
        tokens.push(Token::new(TokenType::StreamBegin, self.pos()));

        loop {
            // Skip whitespace - this updates newline/whitespace flags
            let c = self.skip_whitespace();
            if c == EOF {
                break;
            }

            if let Some(token) = self.get_one_token(c as u8) {
                tokens.push(token);
                // Reset flags for next token (only after we've captured position)
                // Don't reset if comment was skipped - the comment may have consumed
                // newlines that affect the next token's position flags
                self.newline = false;
                self.whitespace = false;
            }
            // If get_one_token returns None (comment), continue without resetting flags
        }

        // Add stream end token
        tokens.push(Token::new(TokenType::StreamEnd, self.pos()));

        tokens
    }
}

// ============================================================================
// Token Display
// ============================================================================

/// Display a special token
pub fn show_special(value: u32) -> String {
    if value < SpecialToken::BASE {
        // Single character
        return (value as u8 as char).to_string();
    }

    match value {
        x if x == SpecialToken::AddAssign as u32 => "+=".to_string(),
        x if x == SpecialToken::Increment as u32 => "++".to_string(),
        x if x == SpecialToken::SubAssign as u32 => "-=".to_string(),
        x if x == SpecialToken::Decrement as u32 => "--".to_string(),
        x if x == SpecialToken::Arrow as u32 => "->".to_string(),
        x if x == SpecialToken::MulAssign as u32 => "*=".to_string(),
        x if x == SpecialToken::DivAssign as u32 => "/=".to_string(),
        x if x == SpecialToken::ModAssign as u32 => "%=".to_string(),
        x if x == SpecialToken::Lte as u32 => "<=".to_string(),
        x if x == SpecialToken::Gte as u32 => ">=".to_string(),
        x if x == SpecialToken::Equal as u32 => "==".to_string(),
        x if x == SpecialToken::NotEqual as u32 => "!=".to_string(),
        x if x == SpecialToken::LogicalAnd as u32 => "&&".to_string(),
        x if x == SpecialToken::AndAssign as u32 => "&=".to_string(),
        x if x == SpecialToken::LogicalOr as u32 => "||".to_string(),
        x if x == SpecialToken::OrAssign as u32 => "|=".to_string(),
        x if x == SpecialToken::XorAssign as u32 => "^=".to_string(),
        x if x == SpecialToken::HashHash as u32 => "##".to_string(),
        x if x == SpecialToken::LeftShift as u32 => "<<".to_string(),
        x if x == SpecialToken::RightShift as u32 => ">>".to_string(),
        x if x == SpecialToken::DotDot as u32 => "..".to_string(),
        x if x == SpecialToken::ShlAssign as u32 => "<<=".to_string(),
        x if x == SpecialToken::ShrAssign as u32 => ">>=".to_string(),
        x if x == SpecialToken::Ellipsis as u32 => "...".to_string(),
        _ => format!("<special:{}>", value),
    }
}

/// Format a token for display
pub fn show_token(token: &Token, strings: &StringTable) -> String {
    match token.typ {
        TokenType::StreamBegin => "<STREAM_BEGIN>".to_string(),
        TokenType::StreamEnd => "<STREAM_END>".to_string(),
        TokenType::Ident => {
            if let TokenValue::Ident(id) = &token.value {
                strings.get(*id).to_string()
            } else {
                "<ident?>".to_string()
            }
        }
        TokenType::Number => {
            if let TokenValue::Number(n) = &token.value {
                n.clone()
            } else {
                "<number?>".to_string()
            }
        }
        TokenType::String => {
            if let TokenValue::String(s) = &token.value {
                format!("\"{}\"", s)
            } else {
                "<string?>".to_string()
            }
        }
        TokenType::WideString => {
            if let TokenValue::WideString(s) = &token.value {
                format!("L\"{}\"", s)
            } else {
                "<wstring?>".to_string()
            }
        }
        TokenType::Char => {
            if let TokenValue::Char(s) = &token.value {
                format!("'{}'", s)
            } else {
                "<char?>".to_string()
            }
        }
        TokenType::WideChar => {
            if let TokenValue::WideChar(s) = &token.value {
                format!("L'{}'", s)
            } else {
                "<wchar?>".to_string()
            }
        }
        TokenType::Special => {
            if let TokenValue::Special(v) = &token.value {
                show_special(*v)
            } else {
                "<special?>".to_string()
            }
        }
    }
}

/// Format token type name
pub fn token_type_name(typ: TokenType) -> &'static str {
    match typ {
        TokenType::Ident => "IDENT",
        TokenType::Number => "NUMBER",
        TokenType::Char => "CHAR",
        TokenType::WideChar => "WCHAR",
        TokenType::String => "STRING",
        TokenType::WideString => "WSTRING",
        TokenType::Special => "SPECIAL",
        TokenType::StreamBegin => "STREAM_BEGIN",
        TokenType::StreamEnd => "STREAM_END",
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize_str(input: &str) -> (Vec<Token>, StringTable) {
        let mut strings = StringTable::new();
        let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
        let tokens = tokenizer.tokenize();
        (tokens, strings)
    }

    #[test]
    fn test_simple_tokens() {
        let (tokens, idents) = tokenize_str("int main");
        // StreamBegin, "int", "main", StreamEnd
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].typ, TokenType::StreamBegin);
        assert_eq!(tokens[1].typ, TokenType::Ident);
        assert_eq!(tokens[2].typ, TokenType::Ident);
        assert_eq!(tokens[3].typ, TokenType::StreamEnd);

        assert_eq!(show_token(&tokens[1], &idents), "int");
        assert_eq!(show_token(&tokens[2], &idents), "main");
    }

    #[test]
    fn test_numbers() {
        let (tokens, _) = tokenize_str("123 0x1F 3.14 1e10 0.5e-3");
        // Skip StreamBegin/End
        assert_eq!(tokens[1].typ, TokenType::Number);
        assert_eq!(tokens[2].typ, TokenType::Number);
        assert_eq!(tokens[3].typ, TokenType::Number);
        assert_eq!(tokens[4].typ, TokenType::Number);
        assert_eq!(tokens[5].typ, TokenType::Number);

        if let TokenValue::Number(n) = &tokens[1].value {
            assert_eq!(n, "123");
        }
        if let TokenValue::Number(n) = &tokens[2].value {
            assert_eq!(n, "0x1F");
        }
        if let TokenValue::Number(n) = &tokens[3].value {
            assert_eq!(n, "3.14");
        }
        if let TokenValue::Number(n) = &tokens[4].value {
            assert_eq!(n, "1e10");
        }
        if let TokenValue::Number(n) = &tokens[5].value {
            assert_eq!(n, "0.5e-3");
        }
    }

    #[test]
    fn test_strings() {
        let (tokens, _) = tokenize_str(r#""hello" "world""#);
        assert_eq!(tokens[1].typ, TokenType::String);
        assert_eq!(tokens[2].typ, TokenType::String);

        if let TokenValue::String(s) = &tokens[1].value {
            assert_eq!(s, "hello");
        }
        if let TokenValue::String(s) = &tokens[2].value {
            assert_eq!(s, "world");
        }
    }

    #[test]
    fn test_char_literals() {
        let (tokens, _) = tokenize_str("'a' '\\n' '\\0'");
        assert_eq!(tokens[1].typ, TokenType::Char);
        assert_eq!(tokens[2].typ, TokenType::Char);
        assert_eq!(tokens[3].typ, TokenType::Char);
    }

    #[test]
    fn test_operators() {
        let (tokens, idents) = tokenize_str("+ += ++ - -= -- -> * *= / /= % %= = ==");
        let ops: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(
            ops,
            vec![
                "+", "+=", "++", "-", "-=", "--", "->", "*", "*=", "/", "/=", "%", "%=", "=", "=="
            ]
        );
    }

    #[test]
    fn test_comparison_ops() {
        let (tokens, idents) = tokenize_str("< <= > >= == != && || !");
        let ops: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(ops, vec!["<", "<=", ">", ">=", "==", "!=", "&&", "||", "!"]);
    }

    #[test]
    fn test_bitwise_ops() {
        let (tokens, idents) = tokenize_str("& &= | |= ^ ^= ~ << >> <<= >>=");
        let ops: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(
            ops,
            vec!["&", "&=", "|", "|=", "^", "^=", "~", "<<", ">>", "<<=", ">>="]
        );
    }

    #[test]
    fn test_punctuation() {
        let (tokens, idents) = tokenize_str("( ) [ ] { } ; , . ...");
        let ops: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(
            ops,
            vec!["(", ")", "[", "]", "{", "}", ";", ",", ".", "..."]
        );
    }

    #[test]
    fn test_line_comment() {
        let (tokens, idents) = tokenize_str("a // comment\nb");
        assert_eq!(tokens.len(), 4); // StreamBegin, a, b, StreamEnd
        assert_eq!(show_token(&tokens[1], &idents), "a");
        assert_eq!(show_token(&tokens[2], &idents), "b");
    }

    #[test]
    fn test_block_comment() {
        let (tokens, idents) = tokenize_str("a /* comment */ b");
        assert_eq!(tokens.len(), 4);
        assert_eq!(show_token(&tokens[1], &idents), "a");
        assert_eq!(show_token(&tokens[2], &idents), "b");
    }

    #[test]
    fn test_line_splice() {
        let (tokens, idents) = tokenize_str("a\\\nb");
        // Line splice joins 'a' and 'b' into one identifier "ab"
        // In C, backslash-newline is deleted, so this becomes "ab"
        assert_eq!(tokens.len(), 3); // StreamBegin, ab, StreamEnd
        assert_eq!(show_token(&tokens[1], &idents), "ab");
    }

    #[test]
    fn test_wide_string() {
        let (tokens, _) = tokenize_str(r#"L"wide""#);
        assert_eq!(tokens[1].typ, TokenType::WideString);
        if let TokenValue::WideString(s) = &tokens[1].value {
            assert_eq!(s, "wide");
        }
    }

    #[test]
    fn test_wide_char() {
        let (tokens, _) = tokenize_str("L'w'");
        assert_eq!(tokens[1].typ, TokenType::WideChar);
    }

    #[test]
    fn test_position_tracking() {
        let (tokens, _) = tokenize_str("a\nb");
        // 'a' is on line 1
        assert_eq!(tokens[1].pos.line, 1);
        // 'b' is on line 2
        assert_eq!(tokens[2].pos.line, 2);
    }

    #[test]
    fn test_newline_flag_first_token() {
        // First token at start of file should have newline=true
        let (tokens, _) = tokenize_str("#define");
        // tokens[0] is STREAM_BEGIN, tokens[1] is the first real token '#'
        assert!(
            tokens[1].pos.newline,
            "First token should have newline=true"
        );
    }

    #[test]
    fn test_newline_flag_after_newline() {
        // Token after newline should have newline=true
        let (tokens, _) = tokenize_str("a\n#define");
        // tokens[0] is STREAM_BEGIN, tokens[1] is 'a', tokens[2] is '#'
        // First token's newline flag isn't constrained - just verify we can access it
        let _ = tokens[1].pos.newline;
        assert!(
            tokens[2].pos.newline,
            "Token after newline should have newline=true"
        );
    }

    #[test]
    fn test_preprocessor_tokens() {
        let (tokens, idents) = tokenize_str("#include <stdio.h>");
        let toks: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        // # include < stdio . h >
        assert_eq!(toks, vec!["#", "include", "<", "stdio", ".", "h", ">"]);
    }

    #[test]
    fn test_function_declaration() {
        let (tokens, idents) = tokenize_str("int main(void) { return 0; }");
        let toks: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(
            toks,
            vec!["int", "main", "(", "void", ")", "{", "return", "0", ";", "}"]
        );
    }

    // ========================================================================
    // Additional coverage tests for multi-char operators
    // ========================================================================

    #[test]
    fn test_hashhash_operator() {
        // ## is the preprocessor token paste operator
        let (tokens, idents) = tokenize_str("a ## b");
        let ops: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(ops, vec!["a", "##", "b"]);
    }

    #[test]
    fn test_dotdot_operator() {
        // .. is a two-character operator (range extension)
        let (tokens, idents) = tokenize_str("a .. b");
        let ops: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(ops, vec!["a", "..", "b"]);
    }

    #[test]
    fn test_ternary_operators() {
        // ? and : for ternary expressions
        let (tokens, idents) = tokenize_str("a ? b : c");
        let ops: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(ops, vec!["a", "?", "b", ":", "c"]);
    }

    #[test]
    fn test_all_two_char_operators() {
        // Comprehensive test of ALL 2-char operators
        let (tokens, idents) =
            tokenize_str("+= ++ -= -- -> *= /= %= <= >= == != && &= || |= ^= ## << >> ..");
        let ops: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(
            ops,
            vec![
                "+=", "++", "-=", "--", "->", "*=", "/=", "%=", "<=", ">=", "==", "!=", "&&", "&=",
                "||", "|=", "^=", "##", "<<", ">>", ".."
            ]
        );
    }

    #[test]
    fn test_all_three_char_operators() {
        // Test all 3-char operators
        let (tokens, idents) = tokenize_str("<<= >>= ...");
        let ops: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(ops, vec!["<<=", ">>=", "..."]);
    }

    #[test]
    fn test_three_char_in_context() {
        // 3-char operators in realistic context
        let (tokens, idents) = tokenize_str("x <<= 2; y >>= 1; void f(int a, ...)");
        let ops: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(
            ops,
            vec![
                "x", "<<=", "2", ";", "y", ">>=", "1", ";", "void", "f", "(", "int", "a", ",",
                "...", ")"
            ]
        );
    }

    // ========================================================================
    // Multi-line comment tests
    // ========================================================================

    #[test]
    fn test_multiline_block_comment() {
        let (tokens, idents) = tokenize_str("a /* this is\na multi-line\ncomment */ b");
        assert_eq!(tokens.len(), 4); // StreamBegin, a, b, StreamEnd
        assert_eq!(show_token(&tokens[1], &idents), "a");
        assert_eq!(show_token(&tokens[2], &idents), "b");
    }

    #[test]
    fn test_block_comment_with_stars() {
        // Comment with * characters inside (common in doc comments)
        let (tokens, idents) = tokenize_str("a /* ** stars ** */ b");
        assert_eq!(tokens.len(), 4);
        assert_eq!(show_token(&tokens[1], &idents), "a");
        assert_eq!(show_token(&tokens[2], &idents), "b");
    }

    #[test]
    fn test_block_comment_with_slashes() {
        // Comment with / characters inside
        let (tokens, idents) = tokenize_str("a /* // not a line comment */ b");
        assert_eq!(tokens.len(), 4);
        assert_eq!(show_token(&tokens[1], &idents), "a");
        assert_eq!(show_token(&tokens[2], &idents), "b");
    }

    #[test]
    fn test_block_comment_asterisk_not_end() {
        // * followed by non-/ should not end comment
        let (tokens, idents) = tokenize_str("a /* x * y */ b");
        assert_eq!(tokens.len(), 4);
        assert_eq!(show_token(&tokens[1], &idents), "a");
        assert_eq!(show_token(&tokens[2], &idents), "b");
    }

    #[test]
    fn test_multiline_comment_position_tracking() {
        // After a multiline comment, position should be correct
        let (tokens, _) = tokenize_str("a\n/* comment\nspanning\nlines */\nb");
        // a is on line 1, b is on line 5
        assert_eq!(tokens[1].pos.line, 1);
        assert_eq!(tokens[2].pos.line, 5);
    }

    // ========================================================================
    // Additional number format tests
    // ========================================================================

    #[test]
    fn test_hex_float_numbers() {
        // Hex floats with p/P exponent (C99 feature)
        let (tokens, _) = tokenize_str("0x1p5 0x1.0p10 0xABCp-5 0x1P+3");
        assert_eq!(tokens[1].typ, TokenType::Number);
        assert_eq!(tokens[2].typ, TokenType::Number);
        assert_eq!(tokens[3].typ, TokenType::Number);
        assert_eq!(tokens[4].typ, TokenType::Number);

        if let TokenValue::Number(n) = &tokens[1].value {
            assert_eq!(n, "0x1p5");
        }
        if let TokenValue::Number(n) = &tokens[2].value {
            assert_eq!(n, "0x1.0p10");
        }
        if let TokenValue::Number(n) = &tokens[3].value {
            assert_eq!(n, "0xABCp-5");
        }
        if let TokenValue::Number(n) = &tokens[4].value {
            assert_eq!(n, "0x1P+3");
        }
    }

    #[test]
    fn test_number_suffixes() {
        // Integer suffixes
        let (tokens, _) = tokenize_str("123L 456UL 789LL 0xFFu 42lu");
        for token in tokens.iter().skip(1).take(5) {
            assert_eq!(token.typ, TokenType::Number);
        }
        if let TokenValue::Number(n) = &tokens[1].value {
            assert_eq!(n, "123L");
        }
        if let TokenValue::Number(n) = &tokens[2].value {
            assert_eq!(n, "456UL");
        }
    }

    #[test]
    fn test_float_suffixes() {
        // Float suffixes
        let (tokens, _) = tokenize_str("3.14f 2.71F 1.0l 9.8L");
        for token in tokens.iter().skip(1).take(4) {
            assert_eq!(token.typ, TokenType::Number);
        }
    }

    #[test]
    fn test_dot_starting_number() {
        // Numbers starting with .
        let (tokens, _) = tokenize_str(".5 .123 .0e10");
        assert_eq!(tokens[1].typ, TokenType::Number);
        assert_eq!(tokens[2].typ, TokenType::Number);
        assert_eq!(tokens[3].typ, TokenType::Number);

        if let TokenValue::Number(n) = &tokens[1].value {
            assert_eq!(n, ".5");
        }
    }

    // ========================================================================
    // Edge cases and tricky sequences
    // ========================================================================

    #[test]
    fn test_operator_adjacency() {
        // Operators without spaces - maximal munch
        let (tokens, idents) = tokenize_str("a+++b"); // a ++ + b
        let ops: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(ops, vec!["a", "++", "+", "b"]);
    }

    #[test]
    fn test_operator_adjacency_minus() {
        let (tokens, idents) = tokenize_str("a---b"); // a -- - b
        let ops: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(ops, vec!["a", "--", "-", "b"]);
    }

    #[test]
    fn test_shift_vs_templates() {
        // >> should be one token (not two > >)
        let (tokens, idents) = tokenize_str("a>>b");
        let ops: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(ops, vec!["a", ">>", "b"]);
    }

    #[test]
    fn test_arrow_vs_minus_gt() {
        // -> should be one token
        let (tokens, idents) = tokenize_str("ptr->field");
        let ops: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(ops, vec!["ptr", "->", "field"]);
    }

    #[test]
    fn test_string_with_comment_chars() {
        // String containing /* and */ should not be treated as comment
        let (tokens, _) = tokenize_str(r#""/* not a comment */""#);
        assert_eq!(tokens.len(), 3); // StreamBegin, string, StreamEnd
        assert_eq!(tokens[1].typ, TokenType::String);
        if let TokenValue::String(s) = &tokens[1].value {
            assert_eq!(s, "/* not a comment */");
        }
    }

    #[test]
    fn test_char_with_quote() {
        // Character literal with escaped quote
        let (tokens, _) = tokenize_str(r#"'\''"#);
        assert_eq!(tokens[1].typ, TokenType::Char);
        if let TokenValue::Char(s) = &tokens[1].value {
            assert_eq!(s, "\\'");
        }
    }

    #[test]
    fn test_string_with_escaped_quote() {
        let (tokens, _) = tokenize_str(r#""hello \"world\"""#);
        assert_eq!(tokens[1].typ, TokenType::String);
        if let TokenValue::String(s) = &tokens[1].value {
            assert_eq!(s, "hello \\\"world\\\"");
        }
    }

    #[test]
    fn test_empty_string() {
        let (tokens, _) = tokenize_str(r#""""#);
        assert_eq!(tokens[1].typ, TokenType::String);
        if let TokenValue::String(s) = &tokens[1].value {
            assert_eq!(s, "");
        }
    }

    #[test]
    fn test_empty_char() {
        // Empty char literal (technically invalid C, but lexer should handle)
        let (tokens, _) = tokenize_str("''");
        assert_eq!(tokens[1].typ, TokenType::Char);
        if let TokenValue::Char(s) = &tokens[1].value {
            assert_eq!(s, "");
        }
    }

    #[test]
    fn test_consecutive_comments() {
        let (tokens, idents) = tokenize_str("a /* c1 */ /* c2 */ b");
        assert_eq!(tokens.len(), 4);
        assert_eq!(show_token(&tokens[1], &idents), "a");
        assert_eq!(show_token(&tokens[2], &idents), "b");
    }

    #[test]
    fn test_comment_at_eof() {
        let (tokens, idents) = tokenize_str("a /* comment */");
        assert_eq!(tokens.len(), 3); // StreamBegin, a, StreamEnd
        assert_eq!(show_token(&tokens[1], &idents), "a");
    }

    #[test]
    fn test_line_comment_at_eof() {
        let (tokens, idents) = tokenize_str("a // comment");
        assert_eq!(tokens.len(), 3);
        assert_eq!(show_token(&tokens[1], &idents), "a");
    }

    // ========================================================================
    // UCN (Universal Character Name) tests - C99 6.4.3
    // ========================================================================

    #[test]
    fn test_ucn_in_identifier() {
        // Identifier with UCN: caf\u00E9 should become "café"
        let (tokens, idents) = tokenize_str("caf\\u00E9");
        assert_eq!(tokens.len(), 3); // StreamBegin, ident, StreamEnd
        assert_eq!(tokens[1].typ, TokenType::Ident);
        assert_eq!(show_token(&tokens[1], &idents), "café");
    }

    #[test]
    fn test_ucn_identifier_start() {
        // Identifier starting with UCN: \u00E9tat -> "état"
        let (tokens, idents) = tokenize_str("\\u00E9tat");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[1].typ, TokenType::Ident);
        assert_eq!(show_token(&tokens[1], &idents), "état");
    }

    #[test]
    fn test_ucn_long_form() {
        // Long UCN form: \U00000041 = 'A'
        let (tokens, idents) = tokenize_str("test\\U00000041bc");
        assert_eq!(tokens[1].typ, TokenType::Ident);
        assert_eq!(show_token(&tokens[1], &idents), "testAbc");
    }

    #[test]
    fn test_ucn_multiple_in_identifier() {
        // Multiple UCNs in one identifier
        let (tokens, idents) = tokenize_str("\\u00E9l\\u00E8ve");
        assert_eq!(tokens[1].typ, TokenType::Ident);
        assert_eq!(show_token(&tokens[1], &idents), "élève");
    }

    #[test]
    fn test_ucn_only_identifier() {
        // Identifier consisting only of UCN
        let (tokens, idents) = tokenize_str("\\u03B1"); // Greek alpha
        assert_eq!(tokens[1].typ, TokenType::Ident);
        assert_eq!(show_token(&tokens[1], &idents), "α");
    }

    #[test]
    fn test_ucn_lowercase_hex() {
        // UCN with lowercase hex digits
        let (tokens, idents) = tokenize_str("caf\\u00e9");
        assert_eq!(show_token(&tokens[1], &idents), "café");
    }

    // ========================================================================
    // Diagnostic warning tests
    // ========================================================================

    #[test]
    fn test_unterminated_string() {
        // Unterminated string should still produce a token (warning emitted)
        let (tokens, _) = tokenize_str("\"hello");
        assert_eq!(tokens[1].typ, TokenType::String);
        if let TokenValue::String(s) = &tokens[1].value {
            assert_eq!(s, "hello");
        }
    }

    #[test]
    fn test_unterminated_char() {
        // Unterminated char should still produce a token (warning emitted)
        let (tokens, _) = tokenize_str("'a");
        assert_eq!(tokens[1].typ, TokenType::Char);
        if let TokenValue::Char(s) = &tokens[1].value {
            assert_eq!(s, "a");
        }
    }

    #[test]
    fn test_newline_in_string() {
        // Newline terminates string literal (warning emitted)
        let (tokens, _) = tokenize_str("\"hello\nworld\"");
        assert_eq!(tokens[1].typ, TokenType::String);
        if let TokenValue::String(s) = &tokens[1].value {
            assert_eq!(s, "hello");
        }
        // 'world"' becomes identifier 'world' and unterminated string
        assert_eq!(tokens[2].typ, TokenType::Ident);
    }

    #[test]
    fn test_unterminated_block_comment() {
        // Unterminated block comment (warning emitted)
        let (tokens, idents) = tokenize_str("a /* unterminated");
        assert_eq!(tokens.len(), 3); // StreamBegin, a, StreamEnd
        assert_eq!(show_token(&tokens[1], &idents), "a");
    }

    #[test]
    fn test_hex_escape_no_digits() {
        // \x without hex digits should warn
        let (tokens, _) = tokenize_str("\"\\xg\"");
        assert_eq!(tokens[1].typ, TokenType::String);
        if let TokenValue::String(s) = &tokens[1].value {
            assert_eq!(s, "\\xg"); // Raw escape preserved
        }
    }

    #[test]
    fn test_hex_escape_at_end() {
        // \x at end of string should warn
        let (tokens, _) = tokenize_str("\"\\x\"");
        assert_eq!(tokens[1].typ, TokenType::String);
        if let TokenValue::String(s) = &tokens[1].value {
            assert_eq!(s, "\\x");
        }
    }

    #[test]
    fn test_hex_escape_valid() {
        // Valid \x escape (no warning)
        let (tokens, _) = tokenize_str("\"\\x41\"");
        assert_eq!(tokens[1].typ, TokenType::String);
        if let TokenValue::String(s) = &tokens[1].value {
            assert_eq!(s, "\\x41"); // Raw escape preserved
        }
    }

    #[test]
    fn test_octal_escape_preserved() {
        // Octal escapes should be preserved as raw
        let (tokens, _) = tokenize_str("\"\\0\\377\"");
        assert_eq!(tokens[1].typ, TokenType::String);
        if let TokenValue::String(s) = &tokens[1].value {
            assert_eq!(s, "\\0\\377");
        }
    }

    #[test]
    fn test_standard_escapes_preserved() {
        // Standard escapes should be preserved as raw
        let (tokens, _) = tokenize_str("\"\\n\\t\\r\\\\\"");
        assert_eq!(tokens[1].typ, TokenType::String);
        if let TokenValue::String(s) = &tokens[1].value {
            assert_eq!(s, "\\n\\t\\r\\\\");
        }
    }

    #[test]
    fn test_token_no_expand_initially_none() {
        let token = Token::new(TokenType::Ident, Position::default());
        assert!(token.no_expand.is_none());
        assert!(!token.is_no_expand("FOO"));
    }

    #[test]
    fn test_token_mark_no_expand() {
        let mut token = Token::new(TokenType::Ident, Position::default());

        // Mark a macro as no-expand
        token.mark_no_expand("FOO");
        assert!(token.is_no_expand("FOO"));
        assert!(!token.is_no_expand("BAR"));

        // Can mark multiple macros
        token.mark_no_expand("BAR");
        assert!(token.is_no_expand("FOO"));
        assert!(token.is_no_expand("BAR"));
        assert!(!token.is_no_expand("BAZ"));
    }

    #[test]
    fn test_token_with_value_no_expand_none() {
        let token = Token::with_value(
            TokenType::Number,
            Position::default(),
            TokenValue::Number("42".to_string()),
        );
        assert!(token.no_expand.is_none());
    }

    #[test]
    fn test_multiline_comment_newline_flag() {
        // After a multiline comment, the next token should NOT have
        // newline=true just because the comment spanned lines.
        // The comment fix resets the newline flag.
        let (tokens, idents) = tokenize_str("a /* multi\nline\ncomment */ b");
        assert_eq!(tokens.len(), 4); // StreamBegin, a, b, StreamEnd

        // 'a' and 'b' should both be identifiers
        assert_eq!(show_token(&tokens[1], &idents), "a");
        assert_eq!(show_token(&tokens[2], &idents), "b");

        // 'b' should NOT have newline=true (it follows comment on same logical line)
        // The comment was on the same line as 'a', so 'b' continues that line
        assert!(
            !tokens[2].pos.newline,
            "token after multiline comment should not have newline flag"
        );
    }
}
