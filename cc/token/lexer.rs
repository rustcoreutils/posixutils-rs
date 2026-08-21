//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Lexer module for c17 - C99 Tokenizer
// Implements C99 preprocessing token lexing (pp-number, pp-tokens)
//

use crate::diag;
use crate::strings::{StringId, StringTable};
use gettextrs::gettext;

// Re-export Position for use by other modules
pub use crate::diag::Position;

// ============================================================================
// Lexer Mode
// ============================================================================

/// Lexer mode - controls how a few characters are classified
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LexerMode {
    /// C mode: `'` and `"` both open a literal.
    #[default]
    C,
    /// Assembly mode, matching GCC's `assembler-with-cpp`.
    ///
    /// Only `"` opens a literal here. GNU as spells a character constant `'a`
    /// with no closing quote, and an apostrophe in a comment is ordinary
    /// prose -- "don't" is common in real `.S` files -- so lexing `'` as C
    /// does swallows the rest of the line and mangles the output.
    ///
    /// `;` is a statement separator rather than a comment introducer, and is
    /// passed through for the assembler to interpret. `//` and `/* */` are
    /// comments in both modes; GCC strips them from assembly too.
    Assembly,
}

// ============================================================================
// Token Types
// ============================================================================

/// Apply translation phase 1 trigraph replacement to a source buffer.
///
/// C17 5.2.1.1 still mandates the nine trigraphs; they were removed in C23,
/// and POSIX's own RATIONALE (88224) notes that *not* supporting them is the
/// non-conforming choice. They are nonetheless off by default here, behind
/// `--trigraphs`, because the replacement applies everywhere including inside
/// string literals — `"What??!"` silently becomes `"What|"` — and real code is
/// far more likely to contain `??` by accident than by intent. GCC ships them
/// off by default for the same reason.
///
/// Done as a whole-buffer pre-pass rather than inside `nextchar`, because
/// phase 1 precedes line splicing (so `??/` at end of line must be able to
/// become a splice) and because `peekchar` is a separate non-mutating scanner
/// that would have to mirror the rule exactly.
///
/// Returns the original buffer untouched when it contains no trigraph.
pub fn replace_trigraphs(buf: &[u8]) -> std::borrow::Cow<'_, [u8]> {
    /// The third character of each trigraph, and what `??x` becomes.
    const TRIGRAPHS: &[(u8, u8)] = &[
        (b'=', b'#'),
        (b'(', b'['),
        (b'/', b'\\'),
        (b')', b']'),
        (b'\'', b'^'),
        (b'<', b'{'),
        (b'!', b'|'),
        (b'>', b'}'),
        (b'-', b'~'),
    ];

    // Test for a *complete* trigraph, not a bare `??`: source that pairs two
    // question marks without forming one — `"Really??"` — is exactly the
    // common case, and has nothing to replace.
    let has_trigraph = buf
        .windows(3)
        .any(|w| w[0] == b'?' && w[1] == b'?' && TRIGRAPHS.iter().any(|(c, _)| *c == w[2]));
    if !has_trigraph {
        return std::borrow::Cow::Borrowed(buf);
    }

    let mut out = Vec::with_capacity(buf.len());
    let mut i = 0;
    while i < buf.len() {
        if i + 2 < buf.len() && buf[i] == b'?' && buf[i + 1] == b'?' {
            if let Some(&(_, repl)) = TRIGRAPHS.iter().find(|(c, _)| *c == buf[i + 2]) {
                out.push(repl);
                i += 3;
                continue;
            }
        }
        out.push(buf[i]);
        i += 1;
    }
    std::borrow::Cow::Owned(out)
}

/// The encoding prefix on a character or string literal (C11 6.4.4.4, 6.4.5).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LiteralEncoding {
    /// No prefix.
    Narrow,
    /// `u8` — a string of `char`, guaranteed UTF-8. Strings only.
    Utf8,
    /// `L` — `wchar_t`.
    Wide,
    /// `u` — `char16_t`.
    Utf16,
    /// `U` — `char32_t`.
    Utf32,
}

/// Token types for C99 preprocessing tokens
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Ident,
    Number,
    Char,
    WideChar,
    /// `u'x'` — a `char16_t` constant (C11 6.4.4.4).
    Utf16Char,
    /// `U'x'` — a `char32_t` constant.
    Utf32Char,
    String,
    WideString,
    /// `u"..."` — a `char16_t` string literal (C11 6.4.5).
    Utf16String,
    /// `U"..."` — a `char32_t` string literal.
    Utf32String,
    Special,
    StreamBegin,
    StreamEnd,
    /// A `#pragma` the parser has to see, carried through the token stream
    /// because the preprocessor cannot answer it alone.
    ///
    /// `#pragma pack` changes how the *parser* lays out the structures that
    /// follow it, and "which structures follow it" is a question only the
    /// final token order answers -- an include is preprocessed into its own
    /// vector and spliced in afterwards, so neither a source position nor a
    /// token index recorded during preprocessing survives the splice. A
    /// marker in the stream does. `extract_pragma_directives` removes these
    /// before parsing and hands the parser their positions in the stream.
    Pragma,
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

/// Does this universal character name name a character C17 6.4.3p2 forbids?
///
/// A UCN "shall not specify a character whose short identifier is less than
/// 00A0 other than 0024 ($), 0040 (@), or 0060 (`), nor one in the range D800
/// through DFFF inclusive." The first half keeps a UCN from spelling a
/// character that already has a spelling -- `\u0041` for `A` -- and the second
/// excludes the UTF-16 surrogate range, which is not a character at all.
///
/// Takes the raw scalar rather than a `char` because a surrogate cannot be
/// represented as one: `char::from_u32` rejects it, and every caller used to
/// treat that failure as "not an escape" and carry on with the letter `u`.
/// Report a universal character name C17 6.4.3p2 forbids, spelled as the
/// source spells it so the message can be matched against what was written.
pub(crate) fn report_forbidden_ucn(pos: Position, val: u32) {
    let (prefix, width) = if val > 0xFFFF { ('U', 8) } else { ('u', 4) };
    crate::diag::error(
        pos,
        &format!("\\{prefix}{val:0width$x} is not a valid universal character"),
    );
}

pub(crate) fn ucn_is_forbidden(val: u32) -> bool {
    (val < 0xA0 && val != 0x24 && val != 0x40 && val != 0x60) || (0xD800..=0xDFFF).contains(&val)
}

// ============================================================================
// Translation phase 2 (line splicing)
// ============================================================================

/// Delete the run of backslash-newline splices starting at `offset`, returning
/// the offset of the first byte that survives phase 2 and how many source
/// lines were crossed to reach it.
///
/// This is the single definition of what phase 2 deletes. Every scanner below
/// -- the consuming `nextchar`, the non-consuming `peekchar`, and the UCN
/// lookahead -- goes through it, because they must agree byte for byte: the
/// three hand-written copies this replaces disagreed in three separate places,
/// each of which silently mislexed valid source rather than diagnosing it.
///
/// `splice` is false for a `.i` operand, where a surviving backslash-newline
/// is text rather than a joint and nothing is deleted.
fn skip_splices(buffer: &[u8], mut offset: usize, splice: bool) -> (usize, u32) {
    if !splice {
        return (offset, 0);
    }
    let mut lines = 0;
    // A backslash as the final byte of the buffer has no newline to join to.
    while offset + 1 < buffer.len() && buffer[offset] == b'\\' {
        match buffer[offset + 1] {
            b'\n' => offset += 2,
            b'\r' => {
                offset += 2;
                if buffer.get(offset) == Some(&b'\n') {
                    offset += 1;
                }
            }
            _ => break,
        }
        lines += 1;
    }
    (offset, lines)
}

/// Non-consuming lookahead over the source, yielding exactly the characters
/// `nextchar` would: splices deleted, every line ending normalised to `\n`.
///
/// Lookahead past a single character has to go through this rather than index
/// the buffer directly, or it sees characters the consumer will not see (and
/// vice versa) as soon as a splice lands in the middle of a token.
struct Peek<'a> {
    buffer: &'a [u8],
    offset: usize,
    splice: bool,
}

impl Peek<'_> {
    fn next(&mut self) -> Option<u8> {
        let (offset, _) = skip_splices(self.buffer, self.offset, self.splice);
        let c = *self.buffer.get(offset)?;
        self.offset = offset + 1;
        if c == b'\r' {
            if self.buffer.get(self.offset) == Some(&b'\n') {
                self.offset += 1;
            }
            return Some(b'\n');
        }
        Some(c)
    }
}

impl SpecialToken {
    pub const BASE: u32 = 256;

    /// The punctuator this code stands for, or `None` for a single-character
    /// one, which is its own ASCII code and has no variant here.
    pub fn from_code(code: u32) -> Option<Self> {
        use SpecialToken::*;
        Some(match code {
            c if c == AddAssign as u32 => AddAssign,
            c if c == Increment as u32 => Increment,
            c if c == SubAssign as u32 => SubAssign,
            c if c == Decrement as u32 => Decrement,
            c if c == Arrow as u32 => Arrow,
            c if c == MulAssign as u32 => MulAssign,
            c if c == DivAssign as u32 => DivAssign,
            c if c == ModAssign as u32 => ModAssign,
            c if c == Lte as u32 => Lte,
            c if c == Gte as u32 => Gte,
            c if c == Equal as u32 => Equal,
            c if c == NotEqual as u32 => NotEqual,
            c if c == LogicalAnd as u32 => LogicalAnd,
            c if c == AndAssign as u32 => AndAssign,
            c if c == LogicalOr as u32 => LogicalOr,
            c if c == OrAssign as u32 => OrAssign,
            c if c == XorAssign as u32 => XorAssign,
            c if c == HashHash as u32 => HashHash,
            c if c == LeftShift as u32 => LeftShift,
            c if c == RightShift as u32 => RightShift,
            c if c == DotDot as u32 => DotDot,
            c if c == ShlAssign as u32 => ShlAssign,
            c if c == ShrAssign as u32 => ShrAssign,
            c if c == Ellipsis as u32 => Ellipsis,
            _ => return None,
        })
    }

    /// How this punctuator is written in source.
    ///
    /// `#` stringification needs it (6.10.3.2p2: "the spelling of the
    /// preprocessing token"). Without one, `#x` dropped every operator of more
    /// than one character, so `S(a >> b)` came out `"a  b"`.
    pub fn spelling(self) -> &'static str {
        use SpecialToken::*;
        match self {
            AddAssign => "+=",
            Increment => "++",
            SubAssign => "-=",
            Decrement => "--",
            Arrow => "->",
            MulAssign => "*=",
            DivAssign => "/=",
            ModAssign => "%=",
            Lte => "<=",
            Gte => ">=",
            Equal => "==",
            NotEqual => "!=",
            LogicalAnd => "&&",
            AndAssign => "&=",
            LogicalOr => "||",
            OrAssign => "|=",
            XorAssign => "^=",
            HashHash => "##",
            LeftShift => "<<",
            RightShift => ">>",
            DotDot => "..",
            ShlAssign => "<<=",
            ShrAssign => ">>=",
            Ellipsis => "...",
        }
    }
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
    Number(String),  // Numeric literal as string (pp-number)
    Ident(StringId), // Identifier (interned StringId)
    Special(u32),    // Operator/punctuator
    /// String literal content, as written between the quotes.
    ///
    /// Holds one `char` per source *byte*, not one per character: the lexer
    /// reads bytes and the back end needs the byte count. A `char` here is
    /// therefore always < 0x100, and text from anywhere else has to be put in
    /// that form with [`literal_payload`] before it can go in a payload.
    /// [`payload_bytes`] reads it back out.
    String(String),
    Char(String),       // Character literal content
    WideString(String), // Wide string literal
    WideChar(String),   // Wide character literal
    /// `u"..."` content. Held as a Rust `String`; the 16-bit code units are
    /// produced at emission, so a code point outside the BMP becomes a
    /// surrogate pair there rather than being lost here.
    Utf16String(String),
    /// `U"..."` content.
    Utf32String(String),
    /// `u'x'` content.
    Utf16Char(String),
    /// `U'x'` content.
    Utf32Char(String),
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
const QUOTE: u8 = 64; // ' "
const COMMENT: u8 = 128; // /

/// Classify a single byte (mirrors the old match arms exactly, plus QUOTE and COMMENT).
const fn classify_char(c: u8) -> u8 {
    match c {
        b'0'..=b'9' => DIGIT | HEX,
        b'A'..=b'D' | b'F' => LETTER | HEX,
        b'E' => LETTER | HEX | EXP,
        b'G'..=b'O' => LETTER,
        b'P' => LETTER | EXP,
        b'Q'..=b'Z' => LETTER,
        b'a'..=b'd' | b'f' => LETTER | HEX,
        b'e' => LETTER | HEX | EXP,
        b'g'..=b'o' => LETTER,
        b'p' => LETTER | EXP,
        b'q'..=b'z' => LETTER,
        b'_' => LETTER,
        b'.' => DOT | VALID_SECOND,
        b'=' | b'+' | b'-' | b'>' | b'<' | b'&' | b'|' | b'#' => VALID_SECOND,
        b'\'' | b'"' => QUOTE,
        b'/' => COMMENT,
        _ => 0,
    }
}

/// Build the 256-byte lookup table at compile time.
const fn build_char_table() -> [u8; 256] {
    let mut table = [0u8; 256];
    let mut i: usize = 0;
    while i < 256 {
        table[i] = classify_char(i as u8);
        i += 1;
    }
    table
}

/// Compile-time character classification table.
const CHAR_TABLE: [u8; 256] = build_char_table();

/// Character classification via table lookup.
#[inline(always)]
fn char_class(c: u8) -> u8 {
    CHAR_TABLE[c as usize]
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

    // Lexer mode (C vs Assembly)
    mode: LexerMode,

    // Translation phase 2. Off for a `.i` operand: `c17 -E` already spliced,
    // so a surviving backslash-newline is text the user meant, and GCC
    // likewise reports it rather than joining the lines a second time.
    splice: bool,
}

impl<'a, 'b> Tokenizer<'a, 'b> {
    pub fn new(buffer: &'a [u8], stream_id: u16, strings: &'b mut StringTable) -> Self {
        Self::new_with_mode(buffer, stream_id, strings, LexerMode::C)
    }

    /// Create a tokenizer with a specific lexer mode
    pub fn new_with_mode(
        buffer: &'a [u8],
        stream_id: u16,
        strings: &'b mut StringTable,
        mode: LexerMode,
    ) -> Self {
        Self {
            buffer,
            offset: 0,
            stream_id,
            line: 1,
            col: 0,
            newline: true,
            whitespace: false,
            strings,
            mode,
            splice: true,
        }
    }

    /// Stop performing translation phase 2 (backslash-newline splicing).
    ///
    /// For a `.i` operand, POSIX 87982-87983 says the processing `c17 -E`
    /// already performed shall not be repeated, and splicing is part of it.
    pub fn without_splicing(mut self) -> Self {
        self.splice = false;
        self
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
        let (offset, lines) = skip_splices(self.buffer, self.offset, self.splice);
        if lines > 0 {
            self.offset = offset;
            self.line += lines;
            self.col = 0;
        }

        if self.offset >= self.buffer.len() {
            return EOF;
        }
        let c = self.buffer[self.offset];
        self.offset += 1;

        // Handle carriage return
        if c == b'\r' {
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
        if c == b'\n' {
            self.line += 1;
            self.col = 0;
            self.newline = true;
            return c as i32;
        }

        // Handle tab.
        //
        // The column is only ever reported, never computed with, so a line
        // wider than `col` can hold pins the count at the maximum. Machine-
        // generated and minified C routinely exceeds 65535 columns, where the
        // unchecked arithmetic wrapped to a nonsense column in a release build
        // and panicked outright in a debug one.
        if c == b'\t' {
            self.col = self.col.saturating_add(8) & !7; // Round to next multiple of 8
        } else {
            self.col = self.col.saturating_add(1);
        }

        c as i32
    }

    /// Peek at next character without consuming (handles line splicing)
    fn peekchar(&self) -> i32 {
        self.peek_at(self.offset).next().map_or(EOF, i32::from)
    }

    /// A lookahead reader starting at `offset`, walking the buffer the way
    /// `nextchar` does.
    fn peek_at(&self, offset: usize) -> Peek<'a> {
        Peek {
            buffer: self.buffer,
            offset,
            splice: self.splice,
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

    /// The character a UCN at the current position denotes, and how many
    /// `nextchar()` calls consume it.
    ///
    /// The count is in characters, not bytes. Phase 2 deletes splices for free
    /// inside a single `nextchar`, so a byte count over-consumes by the length
    /// of every splice the UCN spans -- silently eating the source characters
    /// that follow it.
    fn peek_ucn(&self) -> Option<(char, usize)> {
        let mut peek = self.peek_at(self.offset);
        if peek.next()? != b'\\' {
            return None;
        }
        let (ch, digits) = self.peek_ucn_after_backslash(&mut peek)?;
        Some((ch, 2 + digits))
    }

    /// Shared tail of both UCN entry points, with `peek` positioned just past
    /// the backslash. Returns the character and how many hex digits spelled it.
    fn peek_ucn_after_backslash(&self, peek: &mut Peek<'_>) -> Option<(char, usize)> {
        let digits = match peek.next()? {
            b'u' => 4,
            b'U' => 8,
            _ => return None,
        };

        let mut val: u32 = 0;
        for _ in 0..digits {
            val = val * 16 + (peek.next()? as char).to_digit(16)?;
        }

        // C17 6.4.3p2. Diagnosed here rather than folded into the `?` below,
        // because a forbidden UCN is a constraint violation and not simply
        // "no UCN here": returning `None` would leave the backslash to be
        // lexed as some other token and report something unrelated.
        if ucn_is_forbidden(val) {
            report_forbidden_ucn(self.pos(), val);
            return None;
        }

        Some((char::from_u32(val)?, digits))
    }

    /// Try to consume a UCN sequence. If successful, returns the decoded character.
    /// Otherwise returns None and leaves position unchanged.
    fn try_consume_ucn(&mut self) -> Option<char> {
        let (ch, chars) = self.peek_ucn()?;
        self.consume_chars(chars);
        Some(ch)
    }

    /// Try to consume a UCN sequence when the backslash has already been consumed.
    /// Expects the next character to be 'u' or 'U'.
    /// Returns the decoded character if successful, None otherwise.
    fn try_consume_ucn_after_backslash(&mut self) -> Option<char> {
        let mut peek = self.peek_at(self.offset);
        let (ch, digits) = self.peek_ucn_after_backslash(&mut peek)?;
        self.consume_chars(1 + digits);
        Some(ch)
    }

    /// Consume `count` characters, letting `nextchar` keep line and column in
    /// step. A lookahead count is always in characters for this reason.
    fn consume_chars(&mut self, count: usize) {
        for _ in 0..count {
            self.nextchar();
        }
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
                // An encoding prefix directly before a quote: L, u, U, u8
                // (C11 6.4.4.4 / 6.4.5). `u8` applies to strings only.
                // Assembly has no such prefixes.
                if self.mode == LexerMode::C && (cu == b'"' || cu == b'\'') {
                    let enc = match name.as_str() {
                        "L" => Some(LiteralEncoding::Wide),
                        "u" => Some(LiteralEncoding::Utf16),
                        "U" => Some(LiteralEncoding::Utf32),
                        "u8" if cu == b'"' => Some(LiteralEncoding::Utf8),
                        _ => None,
                    };
                    if let Some(enc) = enc {
                        self.nextchar(); // Consume the quote
                        return self.get_string_or_char(cu, enc);
                    }
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
    fn get_string_or_char(&mut self, delim: u8, enc: LiteralEncoding) -> Token {
        let pos = self.pos();
        let mut content = String::new();
        let mut escape = false;
        let mut want_hex = false; // Track if we just saw \x

        loop {
            let c = self.nextchar();
            if c == EOF {
                // Unterminated string/char - emit warning
                diag::warning(pos, &gettext("End of file in middle of string"));
                break;
            }
            let cu = c as u8;

            // Check for \x without hex digits
            if want_hex {
                if !cu.is_ascii_hexdigit() {
                    diag::warning(pos, &gettext("\\x used with no following hex digits"));
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
                diag::warning_args(
                    pos,
                    "missing terminating {0} character",
                    &[&delim_char.to_string()],
                );
                break;
            }

            content.push(cu as char);
        }

        // Check for trailing \x at end of string
        if want_hex {
            diag::warning(pos, &gettext("\\x used with no following hex digits"));
        }

        let (typ, value) = if delim == b'"' {
            match enc {
                // A `u8"..."` literal has type `char[]` (C11 6.4.5p6), and the
                // source is already UTF-8, so it is an ordinary narrow string.
                LiteralEncoding::Narrow | LiteralEncoding::Utf8 => {
                    (TokenType::String, TokenValue::String(content))
                }
                LiteralEncoding::Wide => (TokenType::WideString, TokenValue::WideString(content)),
                LiteralEncoding::Utf16 => {
                    (TokenType::Utf16String, TokenValue::Utf16String(content))
                }
                LiteralEncoding::Utf32 => {
                    (TokenType::Utf32String, TokenValue::Utf32String(content))
                }
            }
        } else {
            match enc {
                LiteralEncoding::Narrow | LiteralEncoding::Utf8 => {
                    (TokenType::Char, TokenValue::Char(content))
                }
                LiteralEncoding::Wide => (TokenType::WideChar, TokenValue::WideChar(content)),
                LiteralEncoding::Utf16 => (TokenType::Utf16Char, TokenValue::Utf16Char(content)),
                LiteralEncoding::Utf32 => (TokenType::Utf32Char, TokenValue::Utf32Char(content)),
            }
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
                              // Save newline state before the comment and restore it after.
                              // This matches sparse's drop_stream_comment() behavior:
                              // a comment is transparent to newline tracking, so the token
                              // after the comment inherits the newline flag from before it.
                              // This prevents multi-line comments inside macros from breaking
                              // the EOL boundary, while also preserving start-of-line status
                              // for tokens that follow a comment at the beginning of a line.
        let saved_newline = self.newline;
        let mut next = self.nextchar();
        loop {
            let curr = next;
            if curr == EOF {
                // Unterminated comment - emit warning
                diag::warning(pos, &gettext("End of file in the middle of a comment"));
                break;
            }
            next = self.nextchar();
            if curr == b'*' as i32 && next == b'/' as i32 {
                break;
            }
        }
        self.newline = saved_newline;
    }

    /// Get a special token (operator/punctuator)
    fn get_special(&mut self, first: u8, class: u8) -> Option<Token> {
        let pos = self.pos();

        // Check for string/char literals. Assembly mode admits only `"`; see
        // LexerMode::Assembly for why `'` is an ordinary punctuator there.
        if class & QUOTE != 0 && (first == b'"' || self.mode == LexerMode::C) {
            return Some(self.get_string_or_char(first, LiteralEncoding::Narrow));
        }

        // Check for .digit (floating point number)
        if first == b'.' {
            let next = self.peekchar();
            if next != EOF && is_digit(next as u8) {
                return Some(self.get_number(first));
            }
        }

        // Check for comments. Both modes strip `//` and `/* */`, as GCC does
        // for assembler-with-cpp.
        //
        // Translation phase 3 replaces each comment with one space, so the
        // token after a comment is "preceded by whitespace" even when no
        // actual space is there. That flag is what `#` stringification and -E
        // spacing read, so without it `S(a/**/b)` stringified as "ab" instead
        // of "a b".
        if class & COMMENT != 0 {
            let next = self.peekchar();
            if next == b'/' as i32 {
                self.nextchar();
                self.skip_line_comment();
                self.whitespace = true;
                return None; // No token, continue tokenizing
            }
            if next == b'*' as i32 {
                self.nextchar();
                self.skip_block_comment();
                self.whitespace = true;
                return None; // No token, continue tokenizing
            }
        }

        // C99 6.4.6 Digraphs: alternate token spellings
        // Must be checked before two-char operator table
        if first == b'<' {
            let next = self.peekchar();
            if next == b':' as i32 {
                self.nextchar();
                return Some(Token::with_value(
                    TokenType::Special,
                    pos,
                    TokenValue::Special(b'[' as u32),
                ));
            }
            if next == b'%' as i32 {
                self.nextchar();
                return Some(Token::with_value(
                    TokenType::Special,
                    pos,
                    TokenValue::Special(b'{' as u32),
                ));
            }
        }
        if first == b':' {
            let next = self.peekchar();
            if next == b'>' as i32 {
                self.nextchar();
                return Some(Token::with_value(
                    TokenType::Special,
                    pos,
                    TokenValue::Special(b']' as u32),
                ));
            }
        }
        if first == b'%' {
            let next = self.peekchar();
            if next == b'>' as i32 {
                self.nextchar();
                return Some(Token::with_value(
                    TokenType::Special,
                    pos,
                    TokenValue::Special(b'}' as u32),
                ));
            }
            if next == b':' as i32 {
                self.nextchar();
                // %:%: is the digraph for ##. Decided by looking two characters
                // ahead rather than by consuming and backing out: rewinding
                // `offset` cannot undo the line counting a splice between the
                // two halves already did, and the double count desynchronised
                // __LINE__ for the rest of the file.
                let mut peek = self.peek_at(self.offset);
                if peek.next() == Some(b'%') && peek.next() == Some(b':') {
                    self.consume_chars(2);
                    return Some(Token::with_value(
                        TokenType::Special,
                        pos,
                        TokenValue::Special(SpecialToken::HashHash as u32),
                    ));
                }
                // Just %: -> #
                return Some(Token::with_value(
                    TokenType::Special,
                    pos,
                    TokenValue::Special(b'#' as u32),
                ));
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

        self.get_special(c, class)
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

            // The flags belong to the token about to be lexed, so hand them
            // over and clear them here rather than afterwards. Lexing can set
            // `newline` again -- an unterminated literal ends at the newline it
            // ran into -- and that newline belongs to the *next* token, which
            // does start a line. Clearing afterwards swallowed it, so a `#` on
            // the following line stopped introducing a directive and one
            // diagnostic became a cascade.
            let newline = std::mem::take(&mut self.newline);
            let whitespace = std::mem::take(&mut self.whitespace);

            match self.get_one_token(c as u8) {
                Some(mut token) => {
                    token.pos.newline = newline;
                    token.pos.whitespace = whitespace;
                    tokens.push(token);
                }
                // A comment produced no token. It is transparent to
                // start-of-line status, so give back what it inherited; a `//`
                // comment additionally ran through the newline that starts the
                // next line, and that one stands.
                None => self.newline |= newline,
            }
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

/// The encoding prefix, delimiter and payload of a literal token, or `None`
/// if the token is not a literal (or its type and value disagree).
///
/// One arm per literal type, replacing eight near-identical blocks that each
/// had to be edited in step.
fn literal_parts(token: &Token) -> Option<(&'static str, u8, &str)> {
    let (prefix, delim, payload) = match (token.typ, &token.value) {
        (TokenType::String, TokenValue::String(s)) => ("", b'"', s),
        (TokenType::WideString, TokenValue::WideString(s)) => ("L", b'"', s),
        (TokenType::Utf16String, TokenValue::Utf16String(s)) => ("u", b'"', s),
        (TokenType::Utf32String, TokenValue::Utf32String(s)) => ("U", b'"', s),
        (TokenType::Char, TokenValue::Char(s)) => ("", b'\'', s),
        (TokenType::WideChar, TokenValue::WideChar(s)) => ("L", b'\'', s),
        (TokenType::Utf16Char, TokenValue::Utf16Char(s)) => ("u", b'\'', s),
        (TokenType::Utf32Char, TokenValue::Utf32Char(s)) => ("U", b'\'', s),
        _ => return None,
    };
    Some((prefix, delim, payload.as_str()))
}

/// Encode Rust text as a literal payload: one `char` per UTF-8 byte.
///
/// A literal's payload holds the literal's *source bytes*, one per `char`
/// (see [`TokenValue::String`]). Text arriving from anywhere else -- a file
/// name for `__FILE__`, an identifier being stringified -- is an ordinary
/// Rust string and has to be converted, or the two conventions mix inside one
/// payload and neither its byte count nor its spelling comes out right.
pub fn literal_payload(text: &str) -> String {
    text.bytes().map(char::from).collect()
}

/// The source bytes a literal payload stands for.
pub fn payload_bytes(payload: &str) -> impl Iterator<Item = u8> + '_ {
    payload.chars().map(|c| c as u8)
}

/// The text a literal payload holds, decoded from its source bytes.
///
/// The inverse of [`literal_payload`], for the consumers that need Rust text
/// rather than bytes: a header name to open, a symbol name, a message to
/// print. Reading a payload as if it were already text instead produced
/// mojibake -- `#include "café.h"` looked for `cafÃ©.h` and reported it
/// missing. Lossy when the bytes are not valid UTF-8; where the bytes
/// themselves matter, use [`payload_bytes`].
pub fn payload_text(payload: &str) -> String {
    let bytes: Vec<u8> = payload_bytes(payload).collect();
    String::from_utf8_lossy(&bytes).into_owned()
}

/// Append a token's source spelling to `out`, byte for byte.
///
/// Literals have to be written a byte at a time. Formatting a payload through
/// a Rust `String` UTF-8-encodes each of its `char`s, so every source byte of
/// 0x80 or more became two: a literal holding an accented letter left `c17 -E`
/// longer than it went in, and preprocessing a file then compiling it changed
/// what the string held.
pub fn write_token(out: &mut Vec<u8>, token: &Token, strings: &StringTable) {
    match literal_parts(token) {
        Some((prefix, delim, payload)) => {
            out.extend_from_slice(prefix.as_bytes());
            out.push(delim);
            out.extend(payload_bytes(payload));
            out.push(delim);
        }
        None => out.extend_from_slice(show_other_token(token, strings).as_bytes()),
    }
}

/// Format a token for display.
///
/// Lossy for a literal holding bytes that are not valid UTF-8; use
/// [`write_token`] wherever the exact source bytes matter.
pub fn show_token(token: &Token, strings: &StringTable) -> String {
    if literal_parts(token).is_none() {
        return show_other_token(token, strings);
    }
    let mut out = Vec::new();
    write_token(&mut out, token, strings);
    String::from_utf8_lossy(&out).into_owned()
}

/// Everything [`literal_parts`] declines: the non-literal token types, plus a
/// literal whose type and value disagree.
fn show_other_token(token: &Token, strings: &StringTable) -> String {
    match token.typ {
        TokenType::StreamBegin => "<STREAM_BEGIN>".to_string(),
        TokenType::StreamEnd => "<STREAM_END>".to_string(),
        TokenType::Pragma => match &token.value {
            TokenValue::String(s) => format!("<PRAGMA {s}>"),
            _ => "<PRAGMA>".to_string(),
        },
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
        TokenType::Special => {
            if let TokenValue::Special(v) = &token.value {
                show_special(*v)
            } else {
                "<special?>".to_string()
            }
        }
        // A literal type reaches here only when its value does not match.
        typ => format!("<{}?>", token_type_name(typ).to_lowercase()),
    }
}

/// Format token type name
pub fn token_type_name(typ: TokenType) -> &'static str {
    match typ {
        TokenType::Pragma => "PRAGMA",
        TokenType::Ident => "IDENT",
        TokenType::Number => "NUMBER",
        TokenType::Char => "CHAR",
        TokenType::WideChar => "WCHAR",
        TokenType::String => "STRING",
        TokenType::WideString => "WSTRING",
        TokenType::Utf16Char => "U16CHAR",
        TokenType::Utf32Char => "U32CHAR",
        TokenType::Utf16String => "U16STRING",
        TokenType::Utf32String => "U32STRING",
        TokenType::Special => "SPECIAL",
        TokenType::StreamBegin => "STREAM_BEGIN",
        TokenType::StreamEnd => "STREAM_END",
    }
}

// ============================================================================
// Token to Text Conversion (for preprocessing output)
// ============================================================================

/// Convert preprocessed tokens back to source text, byte for byte.
///
/// Handles whitespace/newline preservation based on token positions. Used for
/// outputting preprocessed assembly files. Bytes rather than a `String`
/// because a literal payload is a byte sequence; see [`write_token`].
///
/// Not to be confused with `Preprocessor::tokens_to_message`, which renders
/// tokens for a human to read in a `#error` diagnostic.
pub fn tokens_to_source_bytes(tokens: &[Token], strings: &StringTable) -> Vec<u8> {
    let mut result: Vec<u8> = Vec::new();
    let mut last_stream: u16 = 0;
    let mut last_line: u32 = 1;
    let mut last_char: Option<u8> = None;

    for token in tokens {
        // Skip stream markers
        match token.typ {
            TokenType::StreamBegin | TokenType::StreamEnd => continue,
            _ => {}
        }

        // Detect stream changes (e.g., entering/leaving #include files)
        // When stream changes, line numbers reset, so we need to handle this
        let stream_changed = token.pos.stream != last_stream;
        if stream_changed {
            last_stream = token.pos.stream;
            last_line = token.pos.line.saturating_sub(1); // Allow line sync below
        }

        let start = result.len();
        write_token(&mut result, token, strings);
        let spelling = &result[start..];
        let first_char = spelling.first().copied();

        // Handle newlines: if token is on a new line, add newline(s)
        if token.pos.newline && start > 0 {
            let spelling: Vec<u8> = result.split_off(start);
            while last_line < token.pos.line {
                result.push(b'\n');
                last_line += 1;
            }
            result.extend_from_slice(&spelling);
        } else if start > 0 {
            // Need a space if the original had whitespace, or if the adjacent
            // tokens would otherwise merge (both alphanumeric/underscore).
            let merges = |c: Option<u8>| c.is_some_and(|c| c.is_ascii_alphanumeric() || c == b'_');
            if token.pos.whitespace || (merges(last_char) && merges(first_char)) {
                result.insert(start, b' ');
            }
        }

        last_char = result.last().copied();
        last_line = token.pos.line;
    }

    // Ensure file ends with newline
    if !result.is_empty() && !result.ends_with(b"\n") {
        result.push(b'\n');
    }

    result
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {

    #[test]
    fn test_replace_trigraphs() {
        // All nine sequences of C17 5.2.1.1.
        assert_eq!(
            replace_trigraphs(b"??=??(??/??)??'??<??!??>??-").as_ref(),
            br"#[\]^{|}~"
        );
        // A buffer with no trigraph is returned untouched.
        assert!(matches!(
            replace_trigraphs(b"int main(void) { return 0; }"),
            std::borrow::Cow::Borrowed(_)
        ));
        // `??` not followed by a trigraph character stays literal — this is
        // the case that makes the feature opt-in.
        assert_eq!(replace_trigraphs(b"What??!").as_ref(), b"What|");
        assert_eq!(replace_trigraphs(b"What??x").as_ref(), b"What??x");
        assert_eq!(replace_trigraphs(b"a??").as_ref(), b"a??");
        // ...and is borrowed, not copied: a `??` that forms no trigraph must
        // not cost an allocation, or the doc's promise is only half true.
        assert!(matches!(
            replace_trigraphs(b"puts(\"Really??\");"),
            std::borrow::Cow::Borrowed(_)
        ));
        assert!(matches!(
            replace_trigraphs(b"a??"),
            std::borrow::Cow::Borrowed(_)
        ));
        // Overlapping question marks: only a complete `??x` is replaced.
        assert_eq!(replace_trigraphs(b"???=").as_ref(), b"?#");
    }

    #[test]
    fn test_literal_encoding_prefixes() {
        let mut strings = StringTable::new();
        let src = br#"u8"a" u"b" U"c" L"d" u'e' U'f' L'g' "h" 'i'"#;
        let tokens = Tokenizer::new(src, 0, &mut strings).tokenize();
        let kinds: Vec<TokenType> = tokens
            .iter()
            .filter(|t| !matches!(t.typ, TokenType::StreamBegin | TokenType::StreamEnd))
            .map(|t| t.typ)
            .collect();
        assert_eq!(
            kinds,
            vec![
                // u8"..." has type char[], so it is an ordinary narrow string.
                TokenType::String,
                TokenType::Utf16String,
                TokenType::Utf32String,
                TokenType::WideString,
                TokenType::Utf16Char,
                TokenType::Utf32Char,
                TokenType::WideChar,
                TokenType::String,
                TokenType::Char,
            ]
        );
    }

    #[test]
    fn test_u8_prefix_only_applies_to_strings() {
        // There is no `u8'x'` character constant in C11, so `u8` before a
        // quote must stay an identifier.
        let mut strings = StringTable::new();
        let tokens = Tokenizer::new(b"u8'x'", 0, &mut strings).tokenize();
        let kinds: Vec<TokenType> = tokens
            .iter()
            .filter(|t| !matches!(t.typ, TokenType::StreamBegin | TokenType::StreamEnd))
            .map(|t| t.typ)
            .collect();
        assert_eq!(kinds, vec![TokenType::Ident, TokenType::Char]);
    }
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
        // Long UCN form, with a code point it is allowed to name.
        //
        // This used to use `\U00000041` and assert the identifier `testAbc`,
        // which C17 6.4.3p2 forbids: a UCN may not name a character below
        // 00A0 other than `$`, `@` and `` ` ``, precisely so it cannot spell
        // an `A` that already has a spelling. gcc rejects that input, and so
        // does c17 now, so the long form is exercised with `\U000000E9`.
        let (tokens, idents) = tokenize_str("test\\U000000E9bc");
        assert_eq!(tokens[1].typ, TokenType::Ident);
        assert_eq!(show_token(&tokens[1], &idents), "testébc");
    }

    #[test]
    fn test_ucn_forbidden_characters() {
        // C17 6.4.3p2, in both the short and long forms, and at both ends of
        // the surrogate range -- a surrogate has no `char`, so it used to fail
        // `char::from_u32` and be taken for "not an escape" entirely.
        for src in [
            "test\\u0041bc",
            "test\\U00000041bc",
            "\\u0061bc",
            "test\\u0020bc",
            "test\\ud800bc",
            "test\\udfffbc",
        ] {
            assert!(
                ucn_is_forbidden(match src.split_once("\\u").or(src.split_once("\\U")) {
                    Some((_, rest)) => u32::from_str_radix(&rest[..4.min(rest.len())], 16)
                        .unwrap_or_else(|_| u32::from_str_radix(&rest[..8], 16).unwrap()),
                    None => unreachable!(),
                }),
                "{src} should name a forbidden character"
            );
        }
        // The three carve-outs, and everything from 00A0 up.
        for val in [0x24, 0x40, 0x60, 0xA0, 0xE9, 0xC5, 0x1F600] {
            assert!(!ucn_is_forbidden(val), "{val:#x} is permitted");
        }
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

    /// Translation phase 2 runs before phase 3, so a splice anywhere in or
    /// around a UCN is simply not there by the time the UCN is lexed. The
    /// UCN lookahead used to count *bytes* and the consumer to spend them as
    /// *characters*, so each splice silently ate that many source characters.
    #[test]
    fn test_ucn_across_line_splices() {
        // Splice immediately before the UCN: the trailing `zz` must survive.
        let (tokens, idents) = tokenize_str("caf\\\n\\u00e9zz");
        assert_eq!(tokens[1].typ, TokenType::Ident);
        assert_eq!(show_token(&tokens[1], &idents), "caf\u{e9}zz");
        assert_eq!(tokens[2].typ, TokenType::StreamEnd);

        // Splice between the backslash and the `u`.
        let (tokens, idents) = tokenize_str("caf\\\\\nu00e9zz");
        assert_eq!(show_token(&tokens[1], &idents), "caf\u{e9}zz");

        // Splice in the middle of the hex digits.
        let (tokens, idents) = tokenize_str("caf\\u00\\\ne9zz");
        assert_eq!(show_token(&tokens[1], &idents), "caf\u{e9}zz");

        // Same, for a UCN that *starts* the identifier.
        let (tokens, idents) = tokenize_str("\\u00\\\ne9tat");
        assert_eq!(tokens[1].typ, TokenType::Ident);
        assert_eq!(show_token(&tokens[1], &idents), "\u{e9}tat");

        // The long form spans more digits, so it spans more splices.
        let (tokens, idents) = tokenize_str("a\\U000\\\n000\\\ne9b");
        assert_eq!(show_token(&tokens[1], &idents), "a\u{e9}b");
    }

    /// An unterminated literal ends at the newline it ran into, so the token
    /// after it starts a line -- GCC recovers the same way and goes on to
    /// process a directive there. Clearing the flag after lexing every token
    /// swallowed that newline.
    #[test]
    fn test_unterminated_literal_yields_the_newline_it_ate() {
        let (tokens, idents) = tokenize_str("char *s = \"abc\n#define ZZZ 9\n");
        let hash = tokens
            .iter()
            .position(|t| show_token(t, &idents) == "#")
            .expect("the `#` must survive as its own token");
        assert!(tokens[hash].pos.newline);
    }

    /// A literal payload holds one `char` per source byte, so its spelling has
    /// to be written a byte at a time. Formatting one through a Rust `String`
    /// UTF-8-encoded each of those chars and doubled every byte >= 0x80.
    #[test]
    fn test_write_token_emits_source_bytes() {
        let (tokens, idents) = tokenize_str("\"caf\u{e9}\"");
        assert_eq!(tokens[1].typ, TokenType::String);

        let mut out = Vec::new();
        write_token(&mut out, &tokens[1], &idents);
        assert_eq!(out, b"\"caf\xc3\xa9\"");

        // Round-trips through show_token when the bytes are valid UTF-8.
        assert_eq!(show_token(&tokens[1], &idents), "\"caf\u{e9}\"");

        // literal_payload is the inverse: Rust text into payload form.
        assert_eq!(literal_payload("caf\u{e9}"), "caf\u{c3}\u{a9}");
        assert_eq!(
            payload_bytes(&literal_payload("caf\u{e9}")).collect::<Vec<_>>(),
            b"caf\xc3\xa9"
        );
    }

    #[test]
    fn test_column_saturates_on_very_long_line() {
        let mut src = " ".repeat(70000);
        src.push('x');
        let (tokens, idents) = tokenize_str(&src);
        assert_eq!(show_token(&tokens[1], &idents), "x");
        assert_eq!(tokens[1].pos.col, u16::MAX);
        assert_eq!(tokens[1].pos.line, 1);

        // Tabs advance to the next multiple of eight, which must saturate too.
        let mut src = "\t".repeat(70000);
        src.push('x');
        let (tokens, idents) = tokenize_str(&src);
        assert_eq!(show_token(&tokens[1], &idents), "x");
        assert!(tokens[1].pos.col >= u16::MAX - 8);
    }

    /// A `%:` whose following `%` does not complete the `%:%:` digraph used to
    /// be consumed and then rewound by hand. The rewind restored `offset` and
    /// `col` but not `line`, so a splice between the two halves was counted
    /// once on the way in and again on the way out.
    #[test]
    fn test_digraph_hash_not_hashhash_keeps_line_count() {
        let (tokens, idents) = tokenize_str("%:\\\n% x\ny");
        let spelled: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(spelled, vec!["#", "%", "x", "y"]);
        // Phase 2 deletes the splice but the physical lines still count, so
        // `x` is on line 2 and `y` on line 3 -- each crossed exactly once.
        assert_eq!(tokens[3].pos.line, 2);
        assert_eq!(tokens[4].pos.line, 3);
    }

    /// With phase 2 off (a `.i` operand) a backslash-newline is text, not a
    /// joint, so none of the above applies and the UCN does not form.
    #[test]
    fn test_ucn_splice_disabled() {
        let mut strings = StringTable::new();
        let mut tokenizer = Tokenizer::new(b"caf\\u00\\\ne9", 0, &mut strings).without_splicing();
        let tokens = tokenizer.tokenize();
        assert_eq!(tokens[1].typ, TokenType::Ident);
        assert_eq!(show_token(&tokens[1], &strings), "caf");
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

    // ========================================================================
    // Assembly mode tests
    // ========================================================================

    fn tokenize_asm(input: &str) -> (Vec<Token>, StringTable) {
        let mut strings = StringTable::new();
        let mut tokenizer =
            Tokenizer::new_with_mode(input.as_bytes(), 0, &mut strings, LexerMode::Assembly);
        let tokens = tokenizer.tokenize();
        (tokens, strings)
    }

    #[test]
    fn test_asm_semicolon_not_comment() {
        // In assembly mode, `;` is NOT treated as comment (it's a statement
        // separator in GAS/AT&T syntax). Comment handling is left to the assembler.
        let (tokens, idents) = tokenize_asm("mov eax, ebx ; move register");
        // Should get full line tokenized, including ; and subsequent identifiers
        let toks: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(
            toks,
            vec!["mov", "eax", ",", "ebx", ";", "move", "register"]
        );
    }

    #[test]
    fn test_asm_comments_are_stripped() {
        // GCC's assembler-with-cpp strips `//` and `/* */` from assembly just
        // as it does from C, so c17 does too.
        let (tokens, idents) = tokenize_asm("a // b");
        let toks: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(toks, vec!["a"]);

        let (tokens, idents) = tokenize_asm("a /* b */ c");
        let toks: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(toks, vec!["a", "c"]);
    }

    /// An apostrophe in assembly is prose or a GNU as character constant, not
    /// the start of a C literal. Lexing it as one swallowed the rest of the
    /// line, so a `.S` file whose comment said "don't" assembled to something
    /// else entirely -- or failed outright.
    #[test]
    fn test_asm_apostrophe_is_not_a_literal() {
        let (tokens, idents) = tokenize_asm("# don't panic\nmovl $7, %eax");
        let toks: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(
            toks,
            vec!["#", "don", "'", "t", "panic", "movl", "$", "7", ",", "%", "eax"]
        );

        // GNU as writes an unterminated character constant `'a`, and a
        // terminated one `'b'`; both are just punctuation plus identifiers.
        let (tokens, idents) = tokenize_asm(".byte 'a\n.byte 'b'");
        let toks: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(
            toks,
            vec![".", "byte", "'", "a", ".", "byte", "'", "b", "'"]
        );

        // `"` still opens a string, so an apostrophe inside one is content.
        let (tokens, idents) = tokenize_asm(".ascii \"it's fine\"");
        assert_eq!(tokens[3].typ, TokenType::String);
        assert_eq!(show_token(&tokens[3], &idents), "\"it's fine\"");
    }

    #[test]
    fn test_asm_semicolon_at_start_of_line() {
        // Semicolon comment at start of line
        let (tokens, _) = tokenize_asm("; This is a full line comment\nmov eax, 1");
        // First line should be completely ignored
        assert!(tokens.len() >= 4); // StreamBegin, mov, eax, ..., StreamEnd
    }

    #[test]
    fn test_asm_mode_preserves_preprocessor_directives() {
        // Assembly preprocessing should still handle # directives
        let (tokens, idents) = tokenize_asm("#define FOO 1\nmov eax, FOO");
        // Should tokenize the # directive
        let toks: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert!(toks.contains(&"#".to_string()));
        assert!(toks.contains(&"define".to_string()));
    }

    #[test]
    fn test_c_mode_semicolon_not_comment() {
        // In C mode, `;` is a statement terminator, not a comment
        let (tokens, idents) = tokenize_str("int x; int y");
        let toks: Vec<_> = tokens[1..tokens.len() - 1]
            .iter()
            .map(|t| show_token(t, &idents))
            .collect();
        assert_eq!(toks, vec!["int", "x", ";", "int", "y"]);
    }

    // ========================================================================
    // tokens_to_source_bytes tests
    // ========================================================================

    fn source_text(input: &str) -> String {
        let (tokens, strings) = tokenize_str(input);
        String::from_utf8(tokens_to_source_bytes(&tokens, &strings)).expect("not UTF-8")
    }

    #[test]
    fn test_tokens_to_source_bytes_simple() {
        // Note: semicolon doesn't have whitespace flag set, so no space before it
        assert_eq!(source_text("int x = 42;").trim(), "int x = 42;");
    }

    #[test]
    fn test_tokens_to_source_bytes_multiline() {
        let text = source_text("int x;\nint y;");
        // Should preserve the newline between statements
        assert!(text.contains('\n'));
        assert!(text.contains("int"));
    }

    #[test]
    fn test_tokens_to_source_bytes_ends_with_newline() {
        assert!(source_text("x").ends_with('\n'));
    }

    /// The reason this path deals in bytes: a literal payload is one `char`
    /// per source byte, and rendering it as a Rust string doubles every byte
    /// of 0x80 or more.
    #[test]
    fn test_tokens_to_source_bytes_keeps_literal_bytes() {
        let (tokens, strings) = tokenize_str(".ascii \"caf\u{e9}\"");
        let out = tokens_to_source_bytes(&tokens, &strings);
        assert_eq!(out, b".ascii \"caf\xc3\xa9\"\n");
    }

    // ========================================================================
    // C99 6.4.6 Digraph tests
    // ========================================================================

    #[test]
    fn test_digraph_brackets() {
        // <: and :> are digraphs for [ and ]
        let (tokens, _) = tokenize_str("<:0:>");
        // StreamBegin, [, 0, ], StreamEnd
        assert_eq!(tokens.len(), 5);
        assert!(matches!(&tokens[1].value, TokenValue::Special(c) if *c == b'[' as u32));
        assert!(matches!(&tokens[2].value, TokenValue::Number(n) if n == "0"));
        assert!(matches!(&tokens[3].value, TokenValue::Special(c) if *c == b']' as u32));
    }

    #[test]
    fn test_digraph_braces() {
        // <% and %> are digraphs for { and }
        let (tokens, _) = tokenize_str("<% %>");
        // StreamBegin, {, }, StreamEnd
        assert_eq!(tokens.len(), 4);
        assert!(matches!(&tokens[1].value, TokenValue::Special(c) if *c == b'{' as u32));
        assert!(matches!(&tokens[2].value, TokenValue::Special(c) if *c == b'}' as u32));
    }

    #[test]
    fn test_digraph_hash() {
        // %: is digraph for #
        let (tokens, _) = tokenize_str("%: define");
        // StreamBegin, #, define, StreamEnd
        assert_eq!(tokens.len(), 4);
        assert!(matches!(&tokens[1].value, TokenValue::Special(c) if *c == b'#' as u32));
    }

    #[test]
    fn test_digraph_hashhash() {
        // %:%: is digraph for ##
        let (tokens, _) = tokenize_str("%:%:");
        // StreamBegin, ##, StreamEnd
        assert_eq!(tokens.len(), 3);
        assert!(
            matches!(&tokens[1].value, TokenValue::Special(c) if *c == SpecialToken::HashHash as u32)
        );
    }

    // ========================================================================
    // Character classification table tests
    // ========================================================================

    #[test]
    fn test_char_table_digits() {
        for c in b'0'..=b'9' {
            let cl = char_class(c);
            assert_eq!(cl & DIGIT, DIGIT, "digit {}", c as char);
            assert_eq!(cl & HEX, HEX, "digit hex {}", c as char);
            assert_eq!(cl & LETTER, 0, "digit not letter {}", c as char);
        }
    }

    #[test]
    fn test_char_table_hex_letters() {
        for c in *b"ABCDFabcdf" {
            let cl = char_class(c);
            assert_eq!(cl & LETTER, LETTER, "hex letter {}", c as char);
            assert_eq!(cl & HEX, HEX, "hex flag {}", c as char);
        }
    }

    #[test]
    fn test_char_table_exp_letters() {
        for c in *b"EePp" {
            let cl = char_class(c);
            assert_eq!(cl & EXP, EXP, "exp {}", c as char);
            assert_eq!(cl & LETTER, LETTER, "exp letter {}", c as char);
        }
        // E and e are also hex
        assert_ne!(char_class(b'E') & HEX, 0);
        assert_ne!(char_class(b'e') & HEX, 0);
        // P and p are NOT hex
        assert_eq!(char_class(b'P') & HEX, 0);
        assert_eq!(char_class(b'p') & HEX, 0);
    }

    #[test]
    fn test_char_table_plain_letters() {
        // Non-hex, non-exp uppercase
        for c in b'G'..=b'O' {
            let cl = char_class(c);
            assert_eq!(cl, LETTER, "plain upper {}", c as char);
        }
        for c in b'Q'..=b'Z' {
            let cl = char_class(c);
            assert_eq!(cl, LETTER, "plain upper {}", c as char);
        }
        // Non-hex, non-exp lowercase
        for c in b'g'..=b'o' {
            let cl = char_class(c);
            assert_eq!(cl, LETTER, "plain lower {}", c as char);
        }
        for c in b'q'..=b'z' {
            let cl = char_class(c);
            assert_eq!(cl, LETTER, "plain lower {}", c as char);
        }
        assert_eq!(char_class(b'_'), LETTER);
    }

    #[test]
    fn test_char_table_dot() {
        let cl = char_class(b'.');
        assert_ne!(cl & DOT, 0);
        assert_ne!(cl & VALID_SECOND, 0);
    }

    #[test]
    fn test_char_table_valid_second() {
        for c in *b"=+-><&|#" {
            assert_ne!(
                char_class(c) & VALID_SECOND,
                0,
                "valid_second {}",
                c as char
            );
        }
    }

    #[test]
    fn test_char_table_quote() {
        assert_ne!(char_class(b'\'') & QUOTE, 0);
        assert_ne!(char_class(b'"') & QUOTE, 0);
    }

    #[test]
    fn test_char_table_comment() {
        assert_ne!(char_class(b'/') & COMMENT, 0);
    }

    #[test]
    fn test_char_table_zero_for_others() {
        // Control characters, whitespace, misc punctuation not in the table
        for c in [0u8, b' ', b'\t', b'\n', b'@', b'$', b'`', b'~', 0x80, 0xFF] {
            assert_eq!(char_class(c), 0, "zero for byte {:#x}", c);
        }
    }
}
