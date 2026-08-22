//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C literal decoding: translation phase 5 for character and string literals.
//
// This lives beside the lexer rather than inside the parser because two passes
// need it. The parser decodes a literal to build a constant; the preprocessor's
// `#if` evaluator decodes a character constant to evaluate a controlling
// expression. A second implementation is how `#if '\n' == 10` came to be false
// while the compiled `'\n' == 10` was true, so there is exactly one.
//
// The input is always a literal *payload*: the source spelling between the
// quotes, one `char` per source byte (see `TokenValue::String` in lexer.rs).
//

use crate::token::lexer::{report_forbidden_ucn, ucn_is_forbidden, Position};

/// What one escape sequence contributes to a literal.
///
/// The distinction is the whole point: every escape but a universal character
/// name denotes a single byte of the execution character set, while a UCN
/// denotes a *code point* that the execution character set then encodes. c17
/// stores a narrow literal as one `char` per byte, and returning a UCN's code
/// point as if it were a byte is how `"caf\u00e9"` became the single byte
/// 0xE9 -- five bytes and Latin-1, where gcc gives six and UTF-8.
pub(crate) enum Escaped {
    /// One byte named by an escape: `\n`, `\x41`, `\101`. It is one element
    /// of a wide literal whatever its value -- `L"\xc3\xa9"` is two wide
    /// characters, not one, because the program asked for two byte values.
    Byte(u8),
    /// One byte of the source text itself. The lexer hands over a source
    /// character one byte at a time, so a run of these is the UTF-8 of a
    /// character the programmer typed, and a wide literal wants that character
    /// rather than its bytes.
    ///
    /// Kept apart from `Byte` because after escape processing the two look
    /// identical, and collapsing them makes `L"café"` and `L"caf\xc3\xa9"`
    /// the same literal when C says they differ.
    SourceByte(u8),
    /// A code point named by a universal character name, which the execution
    /// character set encodes.
    CodePoint(char),
    /// A universal character name that names a character C17 6.4.3p2 forbids:
    /// below 00A0 other than `$`, `@` and `` ` ``, or a UTF-16 surrogate.
    /// Carries the scalar so the diagnostic can name it -- a surrogate has no
    /// `char` to carry.
    ForbiddenUcn(u32),
}

/// Parse an escape sequence starting at position i (after the backslash).
///
/// Returns what it denotes -- a byte, or a code point for a universal
/// character name -- and how many characters after the backslash it
/// consumed. See [`Escaped`] for why the two are not the same thing.
pub(crate) fn parse_escape_sequence(chars: &[char], i: usize) -> (Escaped, usize) {
    if i >= chars.len() {
        return (Escaped::Byte(b'\\'), 0);
    }

    match chars[i] {
        'n' => (Escaped::Byte(b'\n'), 1),
        't' => (Escaped::Byte(b'\t'), 1),
        'r' => (Escaped::Byte(b'\r'), 1),
        '\\' => (Escaped::Byte(b'\\'), 1),
        '\'' => (Escaped::Byte(b'\''), 1),
        '"' => (Escaped::Byte(b'"'), 1),
        'a' => (Escaped::Byte(0x07), 1), // bell
        'b' => (Escaped::Byte(0x08), 1), // backspace
        'f' => (Escaped::Byte(0x0C), 1), // form feed
        'v' => (Escaped::Byte(0x0B), 1), // vertical tab
        'x' => {
            // Hex escape \xHH - consume all hex digits
            let mut hex_chars = 0;
            while i + 1 + hex_chars < chars.len() && chars[i + 1 + hex_chars].is_ascii_hexdigit() {
                hex_chars += 1;
            }
            if hex_chars > 0 {
                let hex: String = chars[i + 1..i + 1 + hex_chars].iter().collect();
                // C allows arbitrary-length hex escapes, but only low 8 bits matter
                let val = u64::from_str_radix(&hex, 16).unwrap_or(0) as u8;
                (Escaped::Byte(val), 1 + hex_chars)
            } else {
                (Escaped::Byte(b'x'), 1) // \x with no hex digits - just 'x'
            }
        }
        'u' => {
            // UCN \uXXXX - exactly 4 hex digits (C99 6.4.3)
            if i + 4 < chars.len() && chars[i + 1..i + 5].iter().all(|c| c.is_ascii_hexdigit()) {
                let hex: String = chars[i + 1..i + 5].iter().collect();
                let val = u32::from_str_radix(&hex, 16).unwrap_or(0);
                if ucn_is_forbidden(val) {
                    (Escaped::ForbiddenUcn(val), 5)
                } else if let Some(c) = char::from_u32(val) {
                    (Escaped::CodePoint(c), 5)
                } else {
                    (Escaped::Byte(b'u'), 1) // Invalid code point
                }
            } else {
                (Escaped::Byte(b'u'), 1) // Not enough hex digits
            }
        }
        'U' => {
            // UCN \UXXXXXXXX - exactly 8 hex digits (C99 6.4.3)
            if i + 8 < chars.len() && chars[i + 1..i + 9].iter().all(|c| c.is_ascii_hexdigit()) {
                let hex: String = chars[i + 1..i + 9].iter().collect();
                let val = u32::from_str_radix(&hex, 16).unwrap_or(0);
                if ucn_is_forbidden(val) {
                    (Escaped::ForbiddenUcn(val), 9)
                } else if let Some(c) = char::from_u32(val) {
                    (Escaped::CodePoint(c), 9)
                } else {
                    (Escaped::Byte(b'U'), 1) // Invalid code point
                }
            } else {
                (Escaped::Byte(b'U'), 1) // Not enough hex digits
            }
        }
        c if c.is_ascii_digit() && c != '8' && c != '9' => {
            // Octal escape \NNN (up to 3 digits)
            let mut oct_chars = 1;
            while oct_chars < 3
                && i + oct_chars < chars.len()
                && chars[i + oct_chars].is_ascii_digit()
                && chars[i + oct_chars] != '8'
                && chars[i + oct_chars] != '9'
            {
                oct_chars += 1;
            }
            let oct: String = chars[i..i + oct_chars].iter().collect();
            // `\777` is 511, which is not a byte. C leaves it undefined and gcc
            // takes the low eight bits; parsing straight into `u8` failed and
            // silently produced NUL instead, so `'\777'` was 0 where gcc gives
            // 255.
            let val = u32::from_str_radix(&oct, 8).unwrap_or(0) as u8;
            (Escaped::Byte(val), oct_chars)
        }
        // An unknown escape stands for the character itself. In a narrow
        // literal that character came from the source one byte at a time,
        // so it is already a byte.
        c => (Escaped::Byte(c as u32 as u8), 1),
    }
}

/// Parse a character literal into its scalar value, and say whether that
/// value is a single *byte* or a *code point*.
///
/// The distinction decides whether plain `char`'s signedness applies: a
/// byte is what a `char` object would hold, so `'\x80'` is subject to it
/// (C17 6.4.4.4p10), while a code point is not a byte at all and is
/// carried through unchanged.
///
/// Only the first character of the payload is decoded, so this is the value of
/// a *single*-character constant. A multi-character constant such as `'ab'` is
/// the preprocessor's business (C17 6.10.1) and goes through
/// [`parse_string_literal`] instead.
pub(crate) fn char_literal_value(s: &str, wide: bool, pos: Position) -> (u32, bool) {
    let elements = parse_string_literal(s);
    for e in &elements {
        if let Escaped::ForbiddenUcn(val) = e {
            report_forbidden_ucn(pos, *val);
        }
    }
    if elements.is_empty() {
        return (0, false);
    }

    // A prefixed constant holds characters, not bytes: `L'é'` is the one wide
    // character U+00E9, never the first byte of its UTF-8.
    if wide {
        let units = literal_wide_chars(&elements);
        return (units.first().copied().unwrap_or(0), true);
    }

    // A lone universal character name keeps its code point. gcc makes it a
    // multi-character constant of its UTF-8 bytes, with a warning; c17 keeps
    // the code point, which is the more useful answer and is what
    // `test_char_escape_ucn_*` pin. Truncating it would also flatten
    // `'\U0001F600'` to zero.
    if elements.len() == 1 {
        if let Escaped::CodePoint(c) = elements[0] {
            return (c as u32, true);
        }
        if let Escaped::ForbiddenUcn(v) = elements[0] {
            return (v, true);
        }
    }

    let bytes: Vec<u8> = literal_bytes(&elements)
        .chars()
        .map(|c| c as u32 as u8)
        .collect();
    match bytes.len() {
        0 => (0, false),
        // One byte is what a `char` object would hold, so plain `char`'s
        // signedness applies to it (C17 6.4.4.4p10) -- which is what the
        // `false` says.
        1 => (bytes[0] as u32, false),
        // More than one: a multi-character constant has type `int`, packed
        // big-endian and wrapped, so signedness does not apply. Reading only
        // the first byte made `'ab'` compile to 97 while the preprocessor
        // evaluated it as 24930 -- the two disagreeing about the same token,
        // which having one decoder was supposed to prevent.
        _ => {
            let mut val: u32 = 0;
            for b in bytes {
                val = (val << 8) | b as u32;
            }
            (val, true)
        }
    }
}

/// Parse a string literal, converting escape sequences to their actual values.
/// This implements C99 translation phase 5 for string literals.
pub(crate) fn parse_string_literal(s: &str) -> Vec<Escaped> {
    let chars: Vec<char> = s.chars().collect();
    let mut result = Vec::with_capacity(chars.len());
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == '\\' && i + 1 < chars.len() {
            let (escaped, consumed) = parse_escape_sequence(&chars, i + 1);
            result.push(escaped);
            i += 1 + consumed;
        } else {
            // The lexer hands over one `char` per source *byte*, so a
            // character written directly in the source arrives as the
            // bytes of its UTF-8 -- marked as such, since a wide literal
            // has to put them back together.
            result.push(Escaped::SourceByte(chars[i] as u32 as u8));
            i += 1;
        }
    }

    result
}

/// The bytes a literal's elements make in the execution character set.
pub(crate) fn literal_bytes(elements: &[Escaped]) -> String {
    let mut out = String::with_capacity(elements.len());
    for e in elements {
        match e {
            Escaped::Byte(b) | Escaped::SourceByte(b) => out.push(*b as char),
            // Already diagnosed where the literal was parsed; encoded as
            // written so the rest of the literal still makes sense.
            Escaped::ForbiddenUcn(v) => out.push(*v as u8 as char),
            Escaped::CodePoint(c) => {
                let mut buf = [0u8; 4];
                for b in c.encode_utf8(&mut buf).as_bytes() {
                    out.push(*b as char);
                }
            }
        }
    }
    out
}

/// The wide elements a literal's elements make: one per character.
///
/// A universal character name already names one. A run of source bytes is
/// the UTF-8 of one, and is decoded. A byte named by an escape is one
/// element on its own, whatever its value.
pub(crate) fn literal_wide_chars(elements: &[Escaped]) -> Vec<u32> {
    let mut out = Vec::with_capacity(elements.len());
    let mut run = Vec::new();
    let flush = |run: &mut Vec<u8>, out: &mut Vec<u32>| {
        if !run.is_empty() {
            for c in String::from_utf8_lossy(run).chars() {
                out.push(c as u32);
            }
            run.clear();
        }
    };
    for e in elements {
        match e {
            Escaped::SourceByte(b) => run.push(*b),
            Escaped::Byte(b) => {
                flush(&mut run, &mut out);
                out.push(*b as u32);
            }
            // Already diagnosed where the literal was parsed.
            Escaped::ForbiddenUcn(v) => {
                flush(&mut run, &mut out);
                out.push(*v);
            }
            Escaped::CodePoint(c) => {
                flush(&mut run, &mut out);
                out.push(*c as u32);
            }
        }
    }
    flush(&mut run, &mut out);
    out
}
