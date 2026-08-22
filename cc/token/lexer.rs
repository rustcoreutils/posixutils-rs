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

// Lexer Mode

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

/// How a token was written, where its type and value do not say.
///
/// Two constructs in C spell a token differently without changing anything
/// else about it, and C99 6.10.3.2p2 asks `#` for "the spelling of the
/// preprocessing token" -- so the spelling has to survive to the point where
/// `#` and `-E` read it. Kept beside the type rather than as types of their
/// own: a path that fails to carry this loses the spelling, which is what
/// every path did before, rather than losing the token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Spelling {
    /// Written the one canonical way for this token's type and value.
    #[default]
    Canonical,
    /// A string literal written `u8"..."`. C11 6.4.5p6 gives it type
    /// `char[]`, so it is a narrow string in every respect but this.
    Utf8Prefix,
    /// A punctuator written as one of the six digraphs (C99 6.4.6):
    /// `<: :> <% %> %: %:%:`. 6.4.6p3 makes them behave exactly as
    /// `[ ] { } # ##` "except for their spelling".
    Digraph,
}

/// A punctuator token that was written as a digraph. The value is the primary
/// token's, because 6.4.6p3 makes it mean exactly that; only the spelling
/// differs.
fn digraph_token(pos: Position, value: TokenValue) -> Token {
    let mut token = Token::with_value(TokenType::Special, pos, value);
    token.spelling = Spelling::Digraph;
    token
}

/// How a punctuator token is written.
///
/// A single-character punctuator's value **is** a source byte -- a non-ASCII
/// byte outside a literal lexes as its own punctuator -- while a digraph or a
/// multi-character operator has a spelling that is text. Everything that
/// builds a byte stream or a literal payload has to keep the two apart:
/// rendering the byte through a Rust `String` UTF-8-encodes it, which doubled
/// it in `-E` output, in preprocessed assembly, and in `#` stringification.
///
/// The distinction is in this type rather than in a comment because all three
/// of those were separate bugs with one cause.
pub enum Punctuator {
    /// One source byte, verbatim.
    Byte(u8),
    /// An ASCII spelling: a digraph, a multi-character operator, or the
    /// `<special:N>` placeholder for a code with neither.
    Text(String),
}

/// The digraph that spells the punctuator `code`, for a token written as one.
fn digraph_spelling(code: u32) -> Option<&'static str> {
    Some(match code {
        c if c == b'[' as u32 => "<:",
        c if c == b']' as u32 => ":>",
        c if c == b'{' as u32 => "<%",
        c if c == b'}' as u32 => "%>",
        c if c == b'#' as u32 => "%:",
        c if c == SpecialToken::HashHash as u32 => "%:%:",
        _ => return None,
    })
}

/// Where a header name (C99 6.4.7) may appear.
///
/// A header name is one preprocessing token, but only in a `#include`,
/// `#include_next` or `#import` directive and inside `__has_include(...)`.
/// Everywhere else `<` and `"` mean what they always mean, so the lexer has
/// to be told which it is looking at.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HeaderNamePos {
    /// Not in a directive that can contain one.
    No,
    /// A `#` has just started a line.
    AfterHash,
    /// Inside `#if`/`#elif`, where `__has_include(` can introduce one.
    InCondition,
    /// `__has_include` seen inside a condition; a `(` opens the header name.
    AfterHasInclude,
    /// The next token is a header name if it opens with `<` or `"`.
    Expect { in_condition: bool },
}

// Token Types

/// Drop a leading UTF-8 byte order mark.
///
/// `U+FEFF` is inside Annex D.1's `FE47-FFFD`, so the identifier tables are
/// right to admit it -- which is why gcc special-cases the mark rather than
/// changing its tables, and why this cannot be fixed by making the character
/// invalid. Left in the buffer it lexes as an identifier character, so a
/// BOM'd file's first line is never a directive (`<BOM>#define X 1` comes out
/// as text, with `X` never defined) and `<BOM>int main` fuses into one
/// identifier.
///
/// Only at the very start, and only one: a mark anywhere else is a zero-width
/// no-break space, a legal identifier character, and not this function's
/// business. gcc draws the line in the same place.
pub fn strip_bom(content: &[u8]) -> &[u8] {
    content.strip_prefix(b"\xEF\xBB\xBF").unwrap_or(content)
}

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
    /// A header name: `<stdio.h>` or `"local.h"` (C99 6.4.7).
    ///
    /// One preprocessing token, delimiters and all, and only where a header
    /// name can appear -- after `#include` and inside `__has_include`. None
    /// of the ordinary rules apply between the delimiters, so lexing the
    /// characters as tokens destroyed any header whose name contained `//`
    /// or an apostrophe.
    HeaderName,
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

// Identifier characters (C17 Annex D)

/// Annex D.1: the characters an identifier may contain beyond the basic
/// source character set.
///
/// Transcribed from GCC's `libcpp/ucnid.tab` (`[C11]` + `[C11NOSTART]`),
/// which states that it reproduces the table in ISO/IEC 9899 Annex D, itself
/// a reproduction of ISO/IEC TR 10176. Clang's independent transcription in
/// `clang/lib/Lex/UnicodeCharSets.h` agrees with it on every code point, and
/// so does this one.
///
/// Deliberately *not* Unicode's XID_Start/XID_Continue, which C23 moved to
/// and which differs here in over ten thousand code points.
static IDENT_ALLOWED: &[(u32, u32)] = &[
    (0x00A8, 0x00A8),
    (0x00AA, 0x00AA),
    (0x00AD, 0x00AD),
    (0x00AF, 0x00AF),
    (0x00B2, 0x00B5),
    (0x00B7, 0x00BA),
    (0x00BC, 0x00BE),
    (0x00C0, 0x00D6),
    (0x00D8, 0x00F6),
    (0x00F8, 0x167F),
    (0x1681, 0x180D),
    (0x180F, 0x1FFF),
    (0x200B, 0x200D),
    (0x202A, 0x202E),
    (0x203F, 0x2040),
    (0x2054, 0x2054),
    (0x2060, 0x218F),
    (0x2460, 0x24FF),
    (0x2776, 0x2793),
    (0x2C00, 0x2DFF),
    (0x2E80, 0x2FFF),
    (0x3004, 0x3007),
    (0x3021, 0x302F),
    (0x3031, 0xD7FF),
    (0xF900, 0xFD3D),
    (0xFD40, 0xFDCF),
    (0xFDF0, 0xFE44),
    (0xFE47, 0xFFFD),
    (0x10000, 0x1FFFD),
    (0x20000, 0x2FFFD),
    (0x30000, 0x3FFFD),
    (0x40000, 0x4FFFD),
    (0x50000, 0x5FFFD),
    (0x60000, 0x6FFFD),
    (0x70000, 0x7FFFD),
    (0x80000, 0x8FFFD),
    (0x90000, 0x9FFFD),
    (0xA0000, 0xAFFFD),
    (0xB0000, 0xBFFFD),
    (0xC0000, 0xCFFFD),
    (0xD0000, 0xDFFFD),
    (0xE0000, 0xEFFFD),
];

/// Annex D.2: of the characters above, those that may not appear first.
///
/// All combining marks -- an identifier beginning with one would render as
/// though it modified whatever preceded it. GCC's `[C11NOSTART]` and Clang's
/// `C11DisallowedInitialIDCharRanges` are both exactly these four ranges.
static IDENT_NOT_INITIAL: &[(u32, u32)] = &[
    (0x0300, 0x036F),
    (0x1DC0, 0x1DFF),
    (0x20D0, 0x20FF),
    (0xFE20, 0xFE2F),
];

fn in_ranges(ranges: &[(u32, u32)], val: u32) -> bool {
    ranges
        .binary_search_by(|&(lo, hi)| {
            if val < lo {
                std::cmp::Ordering::Greater
            } else if val > hi {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Equal
            }
        })
        .is_ok()
}

/// Where in an identifier a character is being used.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum IdentPos {
    /// The first character, which C17 Annex D.2 restricts further.
    Initial,
    /// Any later character.
    Continue,
}

/// Whether `ch` may appear in an identifier at `pos` (C17 Annex D).
///
/// Distinct from [`ucn_is_forbidden`], which is 6.4.3p2's rule about what a
/// universal character name may *name* -- a question that applies to string
/// literals too, and that admits characters no identifier may contain.
/// A UCN in an identifier has to satisfy both; a character written directly
/// satisfies only this one. Conflating them let `int \u00A0x;` through, which
/// gcc rejects, and let a combining mark start an identifier.
pub(crate) fn identifier_char(ch: char, pos: IdentPos) -> bool {
    let val = ch as u32;
    // The basic source character set, which the byte-level table already
    // answers for; spelled here so one predicate covers every caller.
    if val < 0x80 {
        return is_letter_or_digit(val as u8)
            && (pos == IdentPos::Continue || !is_digit(val as u8));
    }
    if !in_ranges(IDENT_ALLOWED, val) {
        return false;
    }
    pos == IdentPos::Continue || !in_ranges(IDENT_NOT_INITIAL, val)
}

/// The length in bytes of the UTF-8 character `lead` begins, or `None` if
/// `lead` is ASCII, a continuation byte, or an over-long form's lead.
fn utf8_len(lead: u8) -> Option<usize> {
    match lead {
        0xC2..=0xDF => Some(2),
        0xE0..=0xEF => Some(3),
        0xF0..=0xF4 => Some(4),
        _ => None,
    }
}

/// Decode the UTF-8 character that `lead` begins, taking its continuation
/// bytes from `peek`. Returns the character and its total length in bytes.
///
/// `std::str::from_utf8` does the validation, so over-long forms and
/// surrogates are rejected by construction rather than by hand.
fn decode_utf8(lead: u8, peek: &mut Peek<'_>) -> Option<(char, usize)> {
    let len = utf8_len(lead)?;
    let mut buf = [lead, 0, 0, 0];
    for byte in buf.iter_mut().take(len).skip(1) {
        let c = peek.next()?;
        if c & 0xC0 != 0x80 {
            return None;
        }
        *byte = c;
    }
    let ch = std::str::from_utf8(&buf[..len]).ok()?.chars().next()?;
    Some((ch, len))
}

pub(crate) fn ucn_is_forbidden(val: u32) -> bool {
    (val < 0xA0 && val != 0x24 && val != 0x40 && val != 0x60) || (0xD800..=0xDFFF).contains(&val)
}

// Translation phase 2 (line splicing)

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

// Identifier Interning

/// Identifier intern table - now a re-export of StringTable
/// Kept for backward compatibility during transition
pub type IdentTable = StringTable;

// Token Value

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
    /// A header name (C99 6.4.7), spelling and delimiters included, as in
    /// `<stdio.h>` or `"local.h"`. A payload like the literals above: one
    /// `char` per source byte.
    HeaderName(String),
}

// Token

/// A C token
#[derive(Debug, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub pos: Position,
    pub value: TokenValue,
    /// How this token was written, where its type and value do not say.
    pub spelling: Spelling,
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
            spelling: Spelling::Canonical,
            no_expand: None,
        }
    }

    pub fn with_value(typ: TokenType, pos: Position, value: TokenValue) -> Self {
        Self {
            typ,
            pos,
            value,
            spelling: Spelling::Canonical,
            no_expand: None,
        }
    }

    /// The encoding prefix this literal is written with, for the token types
    /// that do not imply one. Only `u8` qualifies: `L`, `u` and `U` are each
    /// a token type of their own.
    pub fn encoding_prefix(&self) -> &'static str {
        if self.spelling == Spelling::Utf8Prefix {
            "u8"
        } else {
            ""
        }
    }

    /// How this token's punctuator is written.
    ///
    /// See [`Punctuator`]: a single-character one is a source *byte*, and
    /// only a digraph or a longer operator has a spelling that is text.
    pub fn punctuator(&self, code: u32) -> Punctuator {
        if code < SpecialToken::BASE && self.spelling == Spelling::Canonical {
            return Punctuator::Byte(code as u8);
        }
        Punctuator::Text(match self.spelling {
            Spelling::Digraph => digraph_spelling(code)
                .map(str::to_string)
                .unwrap_or_else(|| show_special(code)),
            _ => show_special(code),
        })
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

// Character Classification

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

// Stream (Input Source)

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

// Tokenizer

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

    /// The UTF-8 character at the current position and how many `nextchar()`
    /// calls consume it, or `None` if the bytes there do not form one.
    ///
    /// Splice-aware, like every other lookahead here: phase 2 runs before
    /// phase 3, so a backslash-newline between two bytes of a character is
    /// deleted and the bytes join.
    fn peek_utf8(&self) -> Option<(char, usize)> {
        let mut peek = self.peek_at(self.offset);
        let lead = peek.next()?;
        decode_utf8(lead, &mut peek)
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
        let (ch, digits) = self.peek_ucn_after_backslash(&mut peek, false)?;
        Some((ch, 2 + digits))
    }

    /// Shared tail of both UCN entry points, with `peek` positioned just past
    /// the backslash. Returns the character and how many hex digits spelled it.
    ///
    /// Only 6.4.3p2 is checked here -- whether the UCN may name the character
    /// at all. Whether an *identifier* may contain it is Annex D's question,
    /// which the callers ask, because they are the ones that know the
    /// position.
    fn peek_ucn_after_backslash(&self, peek: &mut Peek<'_>, report: bool) -> Option<(char, usize)> {
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
            // Reported by the caller that actually consumes the backslash, not
            // by the one looking ahead over it: both see the same escape, so
            // reporting here gave two diagnostics a column apart. The consumer
            // is also where gcc points.
            if report {
                report_forbidden_ucn(self.pos(), val);
            }
            return None;
        }

        Some((char::from_u32(val)?, digits))
    }

    /// Try to consume a UCN sequence naming a character an identifier may
    /// contain at `pos`. If successful, returns the decoded character;
    /// otherwise returns None and leaves the position unchanged.
    fn try_consume_ucn(&mut self, pos: IdentPos) -> Option<char> {
        let (ch, chars) = self.peek_ucn()?;
        if !identifier_char(ch, pos) {
            return None;
        }
        self.consume_chars(chars);
        Some(ch)
    }

    /// Try to consume a UCN sequence when the backslash has already been consumed.
    /// Expects the next character to be 'u' or 'U'.
    /// Returns the decoded character if successful, None otherwise.
    fn try_consume_ucn_after_backslash(&mut self) -> Option<char> {
        let mut peek = self.peek_at(self.offset);
        let (ch, digits) = self.peek_ucn_after_backslash(&mut peek, true)?;
        if !identifier_char(ch, IdentPos::Initial) {
            return None;
        }
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

    /// Scan the rest of an identifier onto `name`, stopping at the first
    /// character that cannot continue one.
    ///
    /// Returns the character it stopped on, so the caller can decide whether
    /// it means something there -- an encoding prefix is only a prefix when a
    /// quote follows it. Shared by both entry points: the duplicate of this
    /// loop is what let the UCN over-consumption bug reach each of them
    /// independently.
    fn scan_identifier_tail(&mut self, name: &mut String) -> Option<u8> {
        loop {
            // Use peek to avoid consuming characters that would affect line/col tracking
            let c = self.peekchar();
            if c == EOF {
                return None;
            }
            let cu = c as u8;

            // Check for UCN escape sequence (\uXXXX or \UXXXXXXXX) - C99 6.4.3
            if cu == b'\\' {
                match self.try_consume_ucn(IdentPos::Continue) {
                    Some(uc) => name.push(uc),
                    // Not a valid UCN, end identifier
                    None => return Some(cu),
                }
                continue;
            }

            // A character written directly rather than as a UCN (C23, and a
            // GCC extension long before it). Annex D admits the same set
            // either way.
            if cu >= 0x80 {
                match self.peek_utf8() {
                    Some((ch, len)) if identifier_char(ch, IdentPos::Continue) => {
                        self.consume_chars(len);
                        name.push(ch);
                        continue;
                    }
                    // Not a character, or not one an identifier may contain:
                    // the identifier ends here and the bytes lex as before.
                    _ => return Some(cu),
                }
            }

            if !is_letter_or_digit(cu) {
                return Some(cu);
            }
            self.nextchar(); // Now consume it
            name.push(cu as char);
        }
    }

    /// Intern `name` as an identifier token at `pos`.
    fn ident_token(&mut self, pos: Position, name: &str) -> Token {
        let id = self.strings.intern(name);
        Token::with_value(TokenType::Ident, pos, TokenValue::Ident(id))
    }

    fn get_identifier(&mut self, first: u8) -> Token {
        let pos = self.pos();
        let mut name = String::new();
        name.push(first as char);

        // An encoding prefix directly before a quote: L, u, U, u8
        // (C11 6.4.4.4 / 6.4.5). `u8` applies to strings only.
        // Assembly has no such prefixes.
        if let Some(cu) = self.scan_identifier_tail(&mut name) {
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
        }

        self.ident_token(pos, &name)
    }

    /// Get an identifier token starting with a UCN character (already consumed)
    fn get_identifier_from_ucn(&mut self, first_ucn: char) -> Token {
        let pos = self.pos();
        let mut name = String::new();
        name.push(first_ucn);
        self.scan_identifier_tail(&mut name);
        self.ident_token(pos, &name)
    }

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

        let mut token = Token::with_value(typ, pos, value);
        // `u8` is folded into the narrow type above; the flag is what keeps
        // the spelling, which `#` and `-E` both have to reproduce.
        if enc == LiteralEncoding::Utf8 {
            token.spelling = Spelling::Utf8Prefix;
        }
        token
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
                return Some(digraph_token(pos, TokenValue::Special(b'[' as u32)));
            }
            if next == b'%' as i32 {
                self.nextchar();
                return Some(digraph_token(pos, TokenValue::Special(b'{' as u32)));
            }
        }
        if first == b':' {
            let next = self.peekchar();
            if next == b'>' as i32 {
                self.nextchar();
                return Some(digraph_token(pos, TokenValue::Special(b']' as u32)));
            }
        }
        if first == b'%' {
            let next = self.peekchar();
            if next == b'>' as i32 {
                self.nextchar();
                return Some(digraph_token(pos, TokenValue::Special(b'}' as u32)));
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
                    return Some(digraph_token(
                        pos,
                        TokenValue::Special(SpecialToken::HashHash as u32),
                    ));
                }
                // Just %: -> #
                return Some(digraph_token(pos, TokenValue::Special(b'#' as u32)));
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

        // An extended character written directly, starting an identifier.
        // The lead byte is already consumed, so only the continuation bytes
        // remain to be taken.
        if c >= 0x80 {
            let mut peek = self.peek_at(self.offset);
            if let Some((ch, len)) = decode_utf8(c, &mut peek) {
                if identifier_char(ch, IdentPos::Initial) {
                    self.consume_chars(len - 1);
                    return Some(self.get_identifier_from_ucn(ch));
                }
            }
        }

        self.get_special(c, class)
    }

    /// Lex a header name (C99 6.4.7), the opening delimiter already consumed.
    ///
    /// Between the delimiters the source is one preprocessing token: `//` is
    /// not a comment, `'` does not open a character literal, and `\` is not
    /// an escape. Lexing those as ordinary tokens destroyed the directive --
    /// `#include <sys//types.h>` lost everything after the `//`, and
    /// `#include <it's.h>` opened a literal that ate the rest of the file.
    ///
    /// Returns `None` when no closing delimiter arrives before the end of the
    /// line, leaving the characters to be lexed as before. The whole file is
    /// tokenized up front, including `#if 0` blocks full of prose, so a run
    /// that is not a header name has to lex the way it always did.
    fn try_get_header_name(&mut self, open: u8) -> Option<Token> {
        let close = if open == b'<' { b'>' } else { b'"' };

        let mut peek = self.peek_at(self.offset);
        let mut len = 0;
        loop {
            match peek.next() {
                None | Some(b'\n') => return None,
                Some(c) => {
                    len += 1;
                    if c == close {
                        break;
                    }
                }
            }
        }

        let pos = self.pos();
        let mut spelling = String::with_capacity(len + 1);
        spelling.push(open as char);
        for _ in 0..len {
            spelling.push(self.nextchar() as u8 as char);
        }
        Some(Token::with_value(
            TokenType::HeaderName,
            pos,
            TokenValue::HeaderName(spelling),
        ))
    }

    /// Advance the header-name state machine past `token`.
    fn next_header_name_pos(&self, state: HeaderNamePos, token: &Token) -> HeaderNamePos {
        use HeaderNamePos::*;

        let ident = match &token.value {
            TokenValue::Ident(id) => Some(self.strings.get(*id)),
            _ => None,
        };
        let is_punct = |c: u8| matches!(&token.value, TokenValue::Special(v) if *v == c as u32);

        // A `#` starting a line restarts the machine wherever it was.
        if token.pos.newline && is_punct(b'#') {
            return AfterHash;
        }

        match state {
            No => No,
            AfterHash => match ident {
                Some("include" | "include_next" | "import") => Expect {
                    in_condition: false,
                },
                Some("if" | "elif") => InCondition,
                _ => No,
            },
            InCondition | AfterHasInclude => match ident {
                Some("__has_include" | "__has_include_next") => AfterHasInclude,
                None if state == AfterHasInclude && is_punct(b'(') => Expect { in_condition: true },
                _ => InCondition,
            },
            // A condition may name more than one header.
            Expect { in_condition } => {
                if in_condition {
                    InCondition
                } else {
                    No
                }
            }
        }
    }

    /// Tokenize the entire input, returning all tokens
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        // Add stream begin token
        tokens.push(Token::new(TokenType::StreamBegin, self.pos()));

        let mut header_pos = HeaderNamePos::No;

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

            // C99 6.4.7: where a header name can appear, it is one token and
            // none of the ordinary rules apply inside it.
            let header = matches!(header_pos, HeaderNamePos::Expect { .. })
                && (c == b'<' as i32 || c == b'"' as i32);
            let token = if header {
                self.try_get_header_name(c as u8)
            } else {
                None
            };

            match token.or_else(|| self.get_one_token(c as u8)) {
                Some(mut token) => {
                    token.pos.newline = newline;
                    token.pos.whitespace = whitespace;
                    header_pos = self.next_header_name_pos(header_pos, &token);
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

// Token Display

pub fn show_special(value: u32) -> String {
    if value < SpecialToken::BASE {
        // Single character
        return (value as u8 as char).to_string();
    }

    match SpecialToken::from_code(value) {
        Some(punct) => punct.spelling().to_string(),
        None => format!("<special:{}>", value),
    }
}

/// The encoding prefix, delimiter and payload of a literal token, or `None`
/// if the token is not a literal (or its type and value disagree).
///
/// One arm per literal type, replacing eight near-identical blocks that each
/// had to be edited in step.
fn literal_parts(token: &Token) -> Option<(&'static str, u8, &str)> {
    let (prefix, delim, payload) = match (token.typ, &token.value) {
        (TokenType::String, TokenValue::String(s)) => (token.encoding_prefix(), b'"', s),
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
        None => match &token.value {
            // A header name already carries its own delimiters.
            TokenValue::HeaderName(h) if token.typ == TokenType::HeaderName => {
                out.extend(payload_bytes(h))
            }
            TokenValue::Special(v) if token.typ == TokenType::Special => {
                match token.punctuator(*v) {
                    Punctuator::Byte(b) => out.push(b),
                    Punctuator::Text(t) => out.extend_from_slice(t.as_bytes()),
                }
            }
            _ => out.extend_from_slice(show_other_token(token, strings).as_bytes()),
        },
    }
}

/// Format a token for display.
///
/// Lossy for a literal holding bytes that are not valid UTF-8; use
/// [`write_token`] wherever the exact source bytes matter.
pub fn show_token(token: &Token, strings: &StringTable) -> String {
    let mut out = Vec::new();
    write_token(&mut out, token, strings);
    match String::from_utf8(out) {
        Ok(text) => text,
        // A token holding bytes that are not valid UTF-8 on their own -- one
        // byte of a multi-byte character, lexed as its own punctuator. Only
        // the byte stream can put those back together, which is why
        // `write_token` exists.
        Err(e) => String::from_utf8_lossy(e.as_bytes()).into_owned(),
    }
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
                match token.punctuator(*v) {
                    Punctuator::Byte(b) => (b as char).to_string(),
                    Punctuator::Text(t) => t,
                }
            } else {
                "<special?>".to_string()
            }
        }
        TokenType::HeaderName => {
            if let TokenValue::HeaderName(h) = &token.value {
                payload_text(h)
            } else {
                "<header?>".to_string()
            }
        }
        // A literal type reaches here only when its value does not match.
        typ => format!("<{}?>", token_type_name(typ).to_lowercase()),
    }
}

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
        TokenType::HeaderName => "HEADER_NAME",
        TokenType::Special => "SPECIAL",
        TokenType::StreamBegin => "STREAM_BEGIN",
        TokenType::StreamEnd => "STREAM_END",
    }
}

// Token to Text Conversion (for preprocessing output)

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

#[cfg(test)]
#[path = "test_lexer.rs"]
mod tests;
