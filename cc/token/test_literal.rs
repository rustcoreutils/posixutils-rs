//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Unit tests for literal decoding
//

use super::*;

fn numeric(payload: &str) -> NumericEscape {
    match parse_string_literal(payload).as_slice() {
        [Escaped::Numeric(n)] => *n,
        _ => panic!("{payload} is not one numeric escape"),
    }
}

/// C17 6.4.4.4p9 bounds an octal or hex escape by its element type. The
/// width an escape needs is its value's, so leading zeros cost nothing, and a
/// hex escape may need more than the 32 bits its unit keeps.
#[test]
fn numeric_escape_fits_by_its_value() {
    for (payload, unit, bits_needed) in [
        ("\\0", 0, 0),
        ("\\377", 0xff, 8),
        ("\\400", 0x100, 9),
        ("\\777", 0x1ff, 9),
        ("\\xff", 0xff, 8),
        ("\\x100", 0x100, 9),
        ("\\x00000000041", 0x41, 7),
        ("\\xffff", 0xffff, 16),
        ("\\x12345", 0x12345, 17),
        ("\\xffffffff", 0xffff_ffff, 32),
        ("\\x100000000", 0, 33),
        ("\\x123456789", 0x2345_6789, 33),
    ] {
        let n = numeric(payload);
        assert_eq!(n.unit, unit, "{payload}");
        assert!(n.fits(bits_needed), "{payload} fits {bits_needed} bits");
        if bits_needed > 0 {
            assert!(!n.fits(bits_needed - 1), "{payload} fits fewer bits");
        }
    }
}

/// The message names the escape's form, in gcc's words.
#[test]
fn numeric_escape_message_names_its_radix() {
    assert_eq!(
        numeric("\\400").out_of_range_message(),
        "octal escape sequence out of range"
    );
    assert_eq!(
        numeric("\\x100").out_of_range_message(),
        "hex escape sequence out of range"
    );
}

/// Simple escapes are not numeric and cannot be out of range.
#[test]
fn simple_escapes_are_units() {
    assert!(matches!(
        parse_string_literal("\\n\\q").as_slice(),
        [Escaped::Unit(10), Escaped::Unit(0x71)]
    ));
}

/// C17 6.4.4.4p1: a character constant holds at least one character.
#[test]
fn empty_character_constant_is_reported() {
    let before = crate::diag::error_count();
    let (value, _) = char_literal_value("", None, crate::token::lexer::Position::default());
    assert_eq!(value, 0);
    assert!(crate::diag::error_count() > before);
}

/// A universal character name past U+10FFFF names no character at all.
#[test]
fn ucn_beyond_the_codespace_is_forbidden() {
    assert!(crate::token::lexer::ucn_is_forbidden(0x110000));
    assert!(crate::token::lexer::ucn_is_forbidden(0xFFFF_FFFF));
    assert!(!crate::token::lexer::ucn_is_forbidden(0x10FFFF));
    assert!(!crate::token::lexer::ucn_is_forbidden(0x1F600));
}

/// C17 6.4.3p1: `\u` takes four hex digits and `\U` eight. Fewer names no
/// character; the escape keeps what it has so the rest still decodes.
#[test]
fn incomplete_ucn_keeps_its_digits() {
    for (payload, want_long, want_spelled, want_digits, rest) in [
        ("\\u12", false, 0x12, 2, None),
        ("\\u", false, 0, 0, None),
        ("\\U1234567x", true, 0x123_4567, 7, Some(b'x')),
        ("\\u12g", false, 0x12, 2, Some(b'g')),
    ] {
        let elements = parse_string_literal(payload);
        match elements[0] {
            Escaped::IncompleteUcn {
                long,
                spelled,
                digits,
            } => assert_eq!(
                (long, spelled, digits),
                (want_long, want_spelled, want_digits),
                "{payload}"
            ),
            _ => panic!("{payload} is not an incomplete UCN"),
        }
        let next = elements.get(1).map(|e| match e {
            Escaped::SourceByte(b) => *b,
            _ => panic!("{payload}: the rest is not source text"),
        });
        assert_eq!(next, rest, "{payload}");
    }
    assert!(matches!(
        parse_string_literal("\\u00e9")[..],
        [Escaped::CodePoint('\u{e9}')]
    ));
}
