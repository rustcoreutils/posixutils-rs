//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser unit tests
//

#![allow(clippy::approx_constant)]

use crate::float::IntegralRounding;
use crate::parse::ast::{
    AssignOp, BinaryOp, BlockItem, CalleeBinding, Declaration, Designator, Expr, ExprKind,
    ExternalDecl, ForInit, FpTest, FunctionDef, InlineLibraryFn, Label, LibFn, MathErrno, MemoryFn,
    Stmt, TranslationUnit, UnaryOp,
};
use crate::parse::parser::{ParseResult, Parser};
use crate::strings::{StringId, StringTable};
use crate::symbol::{Symbol, SymbolTable};
use crate::target::Target;
use crate::token::lexer::Tokenizer;
use crate::types::{TypeId, TypeKind, TypeModifiers, TypeTable};

fn parse_expr(input: &str) -> ParseResult<(Expr, TypeTable, StringTable, SymbolTable)> {
    parse_expr_with_vars(input, &[])
}

/// Parse expression with pre-declared variables
fn parse_expr_with_vars(
    input: &str,
    vars: &[&str],
) -> ParseResult<(Expr, TypeTable, StringTable, SymbolTable)> {
    parse_expr_under(input, vars, Default::default())
}

/// [`parse_expr_with_vars`], with library builtins evaluated as `policy`
/// says.
fn parse_expr_under(
    input: &str,
    vars: &[&str],
    policy: super::LibraryCallPolicy,
) -> ParseResult<(Expr, TypeTable, StringTable, SymbolTable)> {
    parse_expr_on(input, vars, policy, &Target::host())
}

/// An expression parsed for `target`. A test about a property one target
/// family has -- x87 or binary128 `long double`, `__float128` -- names that
/// target instead of taking the host's: on an arm64 Mac `long double` is
/// `double` and there is no `__float128`.
fn parse_expr_for(
    input: &str,
    target: &Target,
) -> ParseResult<(Expr, TypeTable, StringTable, SymbolTable)> {
    parse_expr_on(input, &[], Default::default(), target)
}

fn parse_expr_on(
    input: &str,
    vars: &[&str],
    policy: super::LibraryCallPolicy,
    target: &Target,
) -> ParseResult<(Expr, TypeTable, StringTable, SymbolTable)> {
    let mut strings = StringTable::new();
    let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(target);

    // Pre-declare variables
    for var_name in vars {
        let name_id = strings.intern(var_name);
        let sym = Symbol::variable(name_id, types.int_id, 0);
        let _ = symbols.declare(sym);
    }

    let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
    parser.set_library_call_policy(policy);
    parser.skip_stream_tokens();
    let expr = parser.parse_expression()?;
    Ok((expr, types, strings, symbols))
}

/// Helper to compare a StringId with a string literal
fn check_name(strings: &StringTable, id: StringId, expected: &str) {
    assert_eq!(strings.get(id), expected);
}

// Literal tests

#[test]
fn test_int_literal() {
    let (expr, _types, _strings, _symbols) = parse_expr("42").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(42)));
}

#[test]
fn test_hex_literal() {
    let (expr, _types, _strings, _symbols) = parse_expr("0xFF").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(255)));
}

#[test]
fn test_octal_literal() {
    let (expr, _types, _strings, _symbols) = parse_expr("0777").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(511)));
}

#[test]
fn test_float_literal() {
    let (expr, _types, _strings, _symbols) = parse_expr("3.14").unwrap();
    match expr.kind {
        ExprKind::FloatLit(v) => assert!((v.to_f64() - 3.14).abs() < 0.001),
        _ => panic!("Expected FloatLit"),
    }
}

#[test]
fn test_char_literal() {
    let (expr, _types, _strings, _symbols) = parse_expr("'a'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(97)));
}

#[test]
fn test_char_escape() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\n'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(10)));
}

#[test]
fn test_string_literal() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"hello\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "hello"),
        _ => panic!("Expected StringLit"),
    }
}

// Character literal escape sequence tests

#[test]
fn test_char_escape_newline() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\n'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(10)));
}

#[test]
fn test_char_escape_tab() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\t'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(9)));
}

#[test]
fn test_char_escape_carriage_return() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\r'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(13)));
}

#[test]
fn test_char_escape_backslash() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\\\'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(92)));
}

#[test]
fn test_char_escape_single_quote() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\''").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(39)));
}

#[test]
fn test_char_escape_double_quote() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\\"'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(34)));
}

#[test]
fn test_char_escape_bell() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\a'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(7)));
}

#[test]
fn test_char_escape_backspace() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\b'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(8)));
}

#[test]
fn test_char_escape_formfeed() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\f'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(12)));
}

#[test]
fn test_char_escape_vertical_tab() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\v'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(11)));
}

#[test]
fn test_char_escape_null() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\0'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(0)));
}

#[test]
fn test_char_escape_hex() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\x41'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(65)));
}

#[test]
fn test_char_escape_hex_lowercase() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\x0a'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(10)));
}

#[test]
fn test_char_escape_octal() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\101'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(65))); // octal 101 = 65 = 'A'
}

#[test]
fn test_char_escape_octal_012() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\012'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(10))); // octal 012 = 10 = '\n'
}

// UCN (Universal Character Name) escape sequence tests - C99 6.4.3

#[test]
fn test_char_escape_ucn_short() {
    // \u00E9 is 'é' (U+00E9)
    let (expr, _types, _strings, _symbols) = parse_expr("'\\u00E9'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(233)));
}

#[test]
fn test_char_escape_ucn_short_lowercase() {
    // \u00e9 is 'é' (U+00E9) - lowercase hex
    let (expr, _types, _strings, _symbols) = parse_expr("'\\u00e9'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(233)));
}

#[test]
fn test_char_escape_ucn_long() {
    // \U00000041 is 'A' (U+0041)
    let (expr, _types, _strings, _symbols) = parse_expr("'\\U00000041'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(65)));
}

#[test]
fn test_char_escape_ucn_long_emoji() {
    // \U0001F600 is '😀' (U+1F600)
    let (expr, _types, _strings, _symbols) = parse_expr("'\\U0001F600'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(128512)));
}

#[test]
fn test_string_ucn() {
    // A narrow literal holds *bytes*, one per `char`, and a UCN names a code
    // point that the execution character set encodes -- UTF-8 here. So
    // `"caf\u00E9"` is five bytes, the last two being 0xC3 0xA9.
    let (expr, _types, _strings, _symbols) = parse_expr("\"caf\\u00E9\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => {
            let bytes: Vec<u8> = s.chars().map(|c| c as u32 as u8).collect();
            assert_eq!(bytes, b"caf\xc3\xa9");
        }
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_ucn_long() {
    // Likewise for a code point outside the BMP: four UTF-8 bytes, not one
    // truncated to a byte.
    let (expr, _types, _strings, _symbols) = parse_expr("\"hello\\U0001F600world\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => {
            let bytes: Vec<u8> = s.chars().map(|c| c as u32 as u8).collect();
            assert_eq!(bytes, "hello\u{1F600}world".as_bytes());
        }
        _ => panic!("Expected StringLit"),
    }
}

// String literal escape sequence tests

#[test]
fn test_string_escape_newline() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"hello\\nworld\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "hello\nworld"),
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_escape_tab() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"hello\\tworld\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "hello\tworld"),
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_escape_carriage_return() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"hello\\rworld\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "hello\rworld"),
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_escape_backslash() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"hello\\\\world\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "hello\\world"),
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_escape_double_quote() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"hello\\\"world\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "hello\"world"),
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_escape_bell() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"\\a\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "\x07"),
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_escape_backspace() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"\\b\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "\x08"),
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_escape_formfeed() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"\\f\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "\x0C"),
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_escape_vertical_tab() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"\\v\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "\x0B"),
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_escape_null() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"hello\\0world\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => {
            assert_eq!(s.len(), 11);
            assert_eq!(s.as_bytes()[5], 0);
        }
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_escape_hex() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"\\x41\\x42\\x43\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "ABC"),
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_escape_octal() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"\\101\\102\\103\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "ABC"), // octal 101,102,103 = A,B,C
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_escape_octal_012() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"line1\\012line2\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "line1\nline2"), // octal 012 = newline
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_multiple_escapes() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"\\t\\n\\r\\\\\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "\t\n\r\\"),
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_mixed_content() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"Name:\\tJohn\\nAge:\\t30\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "Name:\tJohn\nAge:\t30"),
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_empty() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, ""),
        _ => panic!("Expected StringLit"),
    }
}

/// C17 6.3.1.8p1 runs the integer promotions on both operands before ranking
/// them, so no operand narrower than `int` can reach the "either operand is
/// unsigned" fallback. Without the promotion, `(unsigned char) - (unsigned
/// char)` was typed `unsigned int` and the arithmetic that followed was done
/// unsigned.
#[test]
fn test_usual_arithmetic_conversions_promote_first() {
    // Every sub-int pair yields plain signed int, whatever its own signedness.
    for src in [
        "(unsigned char)1 - (unsigned char)2",
        "(unsigned short)1 - (unsigned short)2",
        "(signed char)1 - (signed char)2",
        "(char)1 - (char)2",
        "(_Bool)0 - (_Bool)1",
        "(unsigned char)1 - (signed char)2",
        "(unsigned char)1 * (unsigned short)2",
    ] {
        let (expr, types, _strings, _symbols) = parse_expr(src).unwrap();
        let typ = expr.typ.unwrap();
        assert_eq!(types.kind(typ), TypeKind::Int, "{src} should be int");
        assert!(!types.is_unsigned(typ), "{src} should be signed");
    }

    // A genuinely unsigned operand of rank int or above still wins.
    for (src, kind) in [
        ("(unsigned char)1 - 2u", TypeKind::Int),
        ("(unsigned char)1 - (unsigned long)2", TypeKind::Long),
        ("(_Bool)1 - 2u", TypeKind::Int),
    ] {
        let (expr, types, _strings, _symbols) = parse_expr(src).unwrap();
        let typ = expr.typ.unwrap();
        assert_eq!(types.kind(typ), kind, "{src}");
        assert!(types.is_unsigned(typ), "{src} should stay unsigned");
    }

    // And a signed wider operand still wins.
    let (expr, types, _strings, _symbols) = parse_expr("(unsigned char)1 - 2L").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Long);
    assert!(!types.is_unsigned(expr.typ.unwrap()));
}

#[test]
fn test_integer_literal_suffixes() {
    // Plain int
    let (expr, types, _strings, _symbols) = parse_expr("42").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Int);
    assert!(!types.is_unsigned(expr.typ.unwrap()));

    // Unsigned int
    let (expr, types, _strings, _symbols) = parse_expr("42U").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Int);
    assert!(types.is_unsigned(expr.typ.unwrap()));

    // Long
    let (expr, types, _strings, _symbols) = parse_expr("42L").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Long);
    assert!(!types.is_unsigned(expr.typ.unwrap()));

    // Unsigned long (UL)
    let (expr, types, _strings, _symbols) = parse_expr("42UL").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Long);
    assert!(types.is_unsigned(expr.typ.unwrap()));

    // Unsigned long (LU)
    let (expr, types, _strings, _symbols) = parse_expr("42LU").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Long);
    assert!(types.is_unsigned(expr.typ.unwrap()));

    // Long long
    let (expr, types, _strings, _symbols) = parse_expr("42LL").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::LongLong);
    assert!(!types.is_unsigned(expr.typ.unwrap()));

    // Unsigned long long (ULL)
    let (expr, types, _strings, _symbols) = parse_expr("42ULL").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::LongLong);
    assert!(types.is_unsigned(expr.typ.unwrap()));

    // Unsigned long long (LLU)
    let (expr, types, _strings, _symbols) = parse_expr("42LLU").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::LongLong);
    assert!(types.is_unsigned(expr.typ.unwrap()));

    // Hex with suffix
    let (expr, types, _strings, _symbols) = parse_expr("0xFFLL").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::LongLong);
    assert!(!types.is_unsigned(expr.typ.unwrap()));

    let (expr, types, _strings, _symbols) = parse_expr("0xFFULL").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::LongLong);
    assert!(types.is_unsigned(expr.typ.unwrap()));
}

#[test]
fn test_identifier() {
    let (expr, _types, strings, symbols) = parse_expr_with_vars("foo", &["foo"]).unwrap();
    match expr.kind {
        ExprKind::Ident(symbol_id) => check_name(&strings, symbols.get(symbol_id).name, "foo"),
        _ => panic!("Expected Ident"),
    }
}

// Binary operator tests

#[test]
fn test_addition() {
    let (expr, _types, _strings, _symbols) = parse_expr("1 + 2").unwrap();
    match expr.kind {
        ExprKind::Binary { op, left, right } => {
            assert_eq!(op, BinaryOp::Add);
            assert!(matches!(left.kind, ExprKind::IntLit(1)));
            assert!(matches!(right.kind, ExprKind::IntLit(2)));
        }
        _ => panic!("Expected Binary"),
    }
}

#[test]
fn test_subtraction() {
    let (expr, _types, _strings, _symbols) = parse_expr("5 - 3").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Sub),
        _ => panic!("Expected Binary"),
    }
}

#[test]
fn test_multiplication() {
    let (expr, _types, _strings, _symbols) = parse_expr("2 * 3").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Mul),
        _ => panic!("Expected Binary"),
    }
}

#[test]
fn test_division() {
    let (expr, _types, _strings, _symbols) = parse_expr("10 / 2").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Div),
        _ => panic!("Expected Binary"),
    }
}

#[test]
fn test_modulo() {
    let (expr, _types, _strings, _symbols) = parse_expr("10 % 3").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Mod),
        _ => panic!("Expected Binary"),
    }
}

#[test]
fn test_precedence_mul_add() {
    // 1 + 2 * 3 should be 1 + (2 * 3)
    let (expr, _types, _strings, _symbols) = parse_expr("1 + 2 * 3").unwrap();
    match expr.kind {
        ExprKind::Binary { op, left, right } => {
            assert_eq!(op, BinaryOp::Add);
            assert!(matches!(left.kind, ExprKind::IntLit(1)));
            match right.kind {
                ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Mul),
                _ => panic!("Expected nested Binary"),
            }
        }
        _ => panic!("Expected Binary"),
    }
}

#[test]
fn test_left_associativity() {
    // 1 - 2 - 3 should be (1 - 2) - 3
    let (expr, _types, _strings, _symbols) = parse_expr("1 - 2 - 3").unwrap();
    match expr.kind {
        ExprKind::Binary { op, left, right } => {
            assert_eq!(op, BinaryOp::Sub);
            assert!(matches!(right.kind, ExprKind::IntLit(3)));
            match left.kind {
                ExprKind::Binary { op, left, right } => {
                    assert_eq!(op, BinaryOp::Sub);
                    assert!(matches!(left.kind, ExprKind::IntLit(1)));
                    assert!(matches!(right.kind, ExprKind::IntLit(2)));
                }
                _ => panic!("Expected nested Binary"),
            }
        }
        _ => panic!("Expected Binary"),
    }
}

#[test]
fn test_comparison_ops() {
    let (expr, _types, _strings, _symbols) = parse_expr("a < b").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Lt),
        _ => panic!("Expected Binary"),
    }

    let (expr, _types, _strings, _symbols) = parse_expr("a > b").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Gt),
        _ => panic!("Expected Binary"),
    }

    let (expr, _types, _strings, _symbols) = parse_expr("a <= b").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Le),
        _ => panic!("Expected Binary"),
    }

    let (expr, _types, _strings, _symbols) = parse_expr("a >= b").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Ge),
        _ => panic!("Expected Binary"),
    }
}

#[test]
fn test_equality_ops() {
    let (expr, _types, _strings, _symbols) = parse_expr("a == b").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Eq),
        _ => panic!("Expected Binary"),
    }

    let (expr, _types, _strings, _symbols) = parse_expr("a != b").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Ne),
        _ => panic!("Expected Binary"),
    }
}

#[test]
fn test_logical_ops() {
    let (expr, _types, _strings, _symbols) = parse_expr("a && b").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::LogAnd),
        _ => panic!("Expected Binary"),
    }

    let (expr, _types, _strings, _symbols) = parse_expr("a || b").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::LogOr),
        _ => panic!("Expected Binary"),
    }
}

#[test]
fn test_bitwise_ops() {
    let (expr, _types, _strings, _symbols) = parse_expr("a & b").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::BitAnd),
        _ => panic!("Expected Binary"),
    }

    let (expr, _types, _strings, _symbols) = parse_expr("a | b").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::BitOr),
        _ => panic!("Expected Binary"),
    }

    let (expr, _types, _strings, _symbols) = parse_expr("a ^ b").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::BitXor),
        _ => panic!("Expected Binary"),
    }
}

#[test]
fn test_shift_ops() {
    let (expr, _types, _strings, _symbols) = parse_expr("a << b").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Shl),
        _ => panic!("Expected Binary"),
    }

    let (expr, _types, _strings, _symbols) = parse_expr("a >> b").unwrap();
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Shr),
        _ => panic!("Expected Binary"),
    }
}

// Unary operator tests

#[test]
fn test_unary_neg() {
    let (expr, _types, strings, symbols) = parse_expr_with_vars("-x", &["x"]).unwrap();
    match expr.kind {
        ExprKind::Unary { op, operand } => {
            assert_eq!(op, UnaryOp::Neg);
            match operand.kind {
                ExprKind::Ident(symbol_id) => {
                    check_name(&strings, symbols.get(symbol_id).name, "x")
                }
                _ => panic!("Expected Ident"),
            }
        }
        _ => panic!("Expected Unary"),
    }
}

/// Unary `-` and `~` perform the integer promotions on their operand
/// (C17 6.5.3.3p3, p4), and the conversion has to be in the tree.
///
/// Recording only the promoted *result* type left the operand narrow, and
/// nothing downstream then says how it is widened: `-(signed char)200` was
/// negated as an eight-bit value moved without a sign, giving -200 where C
/// says 56.
#[test]
fn unary_minus_and_bitnot_convert_their_operand() {
    for src in [
        "-(signed char)200",
        "~(signed char)200",
        "-(short)9",
        "-(_Bool)1",
    ] {
        let (expr, types, _strings, _symbols) = parse_expr(src).unwrap();
        match expr.kind {
            ExprKind::Unary { operand, .. } => {
                assert_eq!(operand.typ, Some(types.int_id), "{src}: operand type");
                assert!(
                    matches!(operand.kind, ExprKind::Cast { cast_type, .. } if cast_type == types.int_id),
                    "{src}: the promotion must be a conversion in the tree"
                );
            }
            _ => panic!("Expected Unary for {src}"),
        }
    }
}

/// Unary `+` performs the integer promotions too (C17 6.5.3.3p2), and its
/// result is a value of the promoted type. It used to hand back the operand
/// itself, so `+(signed char)1` was a `signed char` and `+x` an lvalue.
#[test]
fn unary_plus_promotes_and_is_a_value() {
    for (src, want_int) in [
        ("+(signed char)200", true),
        ("+(short)9", true),
        ("+(_Bool)1", true),
        ("+(unsigned char)1", true),
        ("+1L", false),
        ("+1.5f", false),
    ] {
        let (expr, types, _strings, _symbols) = parse_expr(src).unwrap();
        let typ = expr.typ.unwrap();
        assert!(
            matches!(expr.kind, ExprKind::Cast { cast_type, .. } if cast_type == typ),
            "{src}: `+` yields a converted value, never its operand"
        );
        assert_eq!(typ == types.int_id, want_int, "{src}: result type");
        if src == "+1.5f" {
            assert_eq!(typ, types.float_id, "{src}: no default promotion");
        }
    }
}

/// An operand that already has its promoted type gains nothing: the
/// conversion is the promotion, not a wrapper on every unary operator.
///
/// Spelled with literal suffixes rather than casts, because a cast the
/// source wrote is the same node kind as one the promotion adds -- the
/// difference this is checking would be invisible.
#[test]
fn unary_minus_does_not_convert_what_is_already_promoted() {
    for src in ["-1", "-1L", "-1u", "-1.5", "~1", "~1UL"] {
        let (expr, _types, _strings, _symbols) = parse_expr(src).unwrap();
        match expr.kind {
            ExprKind::Unary { operand, .. } => assert!(
                !matches!(operand.kind, ExprKind::Cast { .. }),
                "{src}: no conversion should be added"
            ),
            _ => panic!("Expected Unary for {src}"),
        }
    }
}

#[test]
fn test_unary_not() {
    let (expr, _types, _strings, _symbols) = parse_expr("!x").unwrap();
    match expr.kind {
        ExprKind::Unary { op, .. } => assert_eq!(op, UnaryOp::Not),
        _ => panic!("Expected Unary"),
    }
}

#[test]
fn test_unary_bitnot() {
    let (expr, _types, _strings, _symbols) = parse_expr("~x").unwrap();
    match expr.kind {
        ExprKind::Unary { op, .. } => assert_eq!(op, UnaryOp::BitNot),
        _ => panic!("Expected Unary"),
    }
}

#[test]
fn test_unary_addr() {
    let (expr, _types, _strings, _symbols) = parse_expr("&x").unwrap();
    match expr.kind {
        ExprKind::Unary { op, .. } => assert_eq!(op, UnaryOp::AddrOf),
        _ => panic!("Expected Unary"),
    }
}

#[test]
fn test_unary_deref() {
    let (expr, _types, _strings, _symbols) = parse_expr("*p").unwrap();
    match expr.kind {
        ExprKind::Unary { op, .. } => assert_eq!(op, UnaryOp::Deref),
        _ => panic!("Expected Unary"),
    }
}

#[test]
fn test_pre_increment() {
    let (expr, _types, _strings, _symbols) = parse_expr("++x").unwrap();
    match expr.kind {
        ExprKind::Unary { op, .. } => assert_eq!(op, UnaryOp::PreInc),
        _ => panic!("Expected Unary"),
    }
}

#[test]
fn test_pre_decrement() {
    let (expr, _types, _strings, _symbols) = parse_expr("--x").unwrap();
    match expr.kind {
        ExprKind::Unary { op, .. } => assert_eq!(op, UnaryOp::PreDec),
        _ => panic!("Expected Unary"),
    }
}

// Postfix operator tests

#[test]
fn test_post_increment() {
    let (expr, _types, _strings, _symbols) = parse_expr("x++").unwrap();
    assert!(matches!(expr.kind, ExprKind::PostInc(_)));
}

#[test]
fn test_post_decrement() {
    let (expr, _types, _strings, _symbols) = parse_expr("x--").unwrap();
    assert!(matches!(expr.kind, ExprKind::PostDec(_)));
}

#[test]
fn test_array_subscript() {
    let (expr, _types, strings, symbols) = parse_expr_with_vars("arr[5]", &["arr"]).unwrap();
    match expr.kind {
        ExprKind::Index { array, index } => {
            match array.kind {
                ExprKind::Ident(symbol_id) => {
                    check_name(&strings, symbols.get(symbol_id).name, "arr")
                }
                _ => panic!("Expected Ident"),
            }
            assert!(matches!(index.kind, ExprKind::IntLit(5)));
        }
        _ => panic!("Expected Index"),
    }
}

#[test]
fn test_member_access() {
    let (expr, _types, strings, symbols) = parse_expr_with_vars("obj.field", &["obj"]).unwrap();
    match expr.kind {
        ExprKind::Member { expr, member } => {
            match expr.kind {
                ExprKind::Ident(symbol_id) => {
                    check_name(&strings, symbols.get(symbol_id).name, "obj")
                }
                _ => panic!("Expected Ident"),
            }
            check_name(&strings, member, "field");
        }
        _ => panic!("Expected Member"),
    }
}

#[test]
fn test_arrow_access() {
    let (expr, _types, strings, symbols) = parse_expr_with_vars("ptr->field", &["ptr"]).unwrap();
    match expr.kind {
        ExprKind::Arrow { expr, member } => {
            match expr.kind {
                ExprKind::Ident(symbol_id) => {
                    check_name(&strings, symbols.get(symbol_id).name, "ptr")
                }
                _ => panic!("Expected Ident"),
            }
            check_name(&strings, member, "field");
        }
        _ => panic!("Expected Arrow"),
    }
}

#[test]
fn test_function_call_no_args() {
    let (expr, _types, strings, symbols) = parse_expr_with_vars("foo()", &["foo"]).unwrap();
    match expr.kind {
        ExprKind::Call { func, args, .. } => {
            match func.kind {
                ExprKind::Ident(symbol_id) => {
                    check_name(&strings, symbols.get(symbol_id).name, "foo")
                }
                _ => panic!("Expected Ident"),
            }
            assert!(args.is_empty());
        }
        _ => panic!("Expected Call"),
    }
}

#[test]
fn test_function_call_with_args() {
    let (expr, _types, strings, symbols) = parse_expr_with_vars("foo(1, 2, 3)", &["foo"]).unwrap();
    match expr.kind {
        ExprKind::Call { func, args, .. } => {
            match func.kind {
                ExprKind::Ident(symbol_id) => {
                    check_name(&strings, symbols.get(symbol_id).name, "foo")
                }
                _ => panic!("Expected Ident"),
            }
            assert_eq!(args.len(), 3);
        }
        _ => panic!("Expected Call"),
    }
}

#[test]
fn test_chained_postfix() {
    // obj.arr[0]->next
    let (expr, _types, strings, symbols) = parse_expr_with_vars("obj.arr[0]", &["obj"]).unwrap();
    match expr.kind {
        ExprKind::Index { array, index } => {
            match array.kind {
                ExprKind::Member { expr, member } => {
                    match expr.kind {
                        ExprKind::Ident(symbol_id) => {
                            check_name(&strings, symbols.get(symbol_id).name, "obj")
                        }
                        _ => panic!("Expected Ident"),
                    }
                    check_name(&strings, member, "arr");
                }
                _ => panic!("Expected Member"),
            }
            assert!(matches!(index.kind, ExprKind::IntLit(0)));
        }
        _ => panic!("Expected Index"),
    }
}

// Assignment tests

#[test]
fn test_simple_assignment() {
    let (expr, _types, strings, symbols) = parse_expr_with_vars("x = 5", &["x"]).unwrap();
    match expr.kind {
        ExprKind::Assign { op, target, value } => {
            assert_eq!(op, AssignOp::Assign);
            match target.kind {
                ExprKind::Ident(symbol_id) => {
                    check_name(&strings, symbols.get(symbol_id).name, "x")
                }
                _ => panic!("Expected Ident"),
            }
            assert!(matches!(value.kind, ExprKind::IntLit(5)));
        }
        _ => panic!("Expected Assign"),
    }
}

#[test]
fn test_compound_assignments() {
    let (expr, _types, _strings, _symbols) = parse_expr("x += 5").unwrap();
    match expr.kind {
        ExprKind::Assign { op, .. } => assert_eq!(op, AssignOp::AddAssign),
        _ => panic!("Expected Assign"),
    }

    let (expr, _types, _strings, _symbols) = parse_expr("x -= 5").unwrap();
    match expr.kind {
        ExprKind::Assign { op, .. } => assert_eq!(op, AssignOp::SubAssign),
        _ => panic!("Expected Assign"),
    }

    let (expr, _types, _strings, _symbols) = parse_expr("x *= 5").unwrap();
    match expr.kind {
        ExprKind::Assign { op, .. } => assert_eq!(op, AssignOp::MulAssign),
        _ => panic!("Expected Assign"),
    }
}

#[test]
fn test_assignment_right_associativity() {
    // a = b = c should be a = (b = c)
    let (expr, _types, strings, symbols) =
        parse_expr_with_vars("a = b = c", &["a", "b", "c"]).unwrap();
    match expr.kind {
        ExprKind::Assign { target, value, .. } => {
            match target.kind {
                ExprKind::Ident(symbol_id) => {
                    check_name(&strings, symbols.get(symbol_id).name, "a")
                }
                _ => panic!("Expected Ident"),
            }
            match value.kind {
                ExprKind::Assign { target, .. } => match target.kind {
                    ExprKind::Ident(symbol_id) => {
                        check_name(&strings, symbols.get(symbol_id).name, "b")
                    }
                    _ => panic!("Expected Ident"),
                },
                _ => panic!("Expected nested Assign"),
            }
        }
        _ => panic!("Expected Assign"),
    }
}

// Ternary expression tests

#[test]
fn test_ternary() {
    let (expr, _types, strings, symbols) =
        parse_expr_with_vars("a ? b : c", &["a", "b", "c"]).unwrap();
    match expr.kind {
        ExprKind::Conditional {
            cond,
            then_expr,
            else_expr,
        } => {
            match cond.kind {
                ExprKind::Ident(symbol_id) => {
                    check_name(&strings, symbols.get(symbol_id).name, "a")
                }
                _ => panic!("Expected Ident"),
            }
            match then_expr.kind {
                ExprKind::Ident(symbol_id) => {
                    check_name(&strings, symbols.get(symbol_id).name, "b")
                }
                _ => panic!("Expected Ident"),
            }
            match else_expr.kind {
                ExprKind::Ident(symbol_id) => {
                    check_name(&strings, symbols.get(symbol_id).name, "c")
                }
                _ => panic!("Expected Ident"),
            }
        }
        _ => panic!("Expected Conditional"),
    }
}

#[test]
fn test_nested_ternary() {
    // a ? b : c ? d : e should be a ? b : (c ? d : e)
    let (expr, _types, _strings, _symbols) = parse_expr("a ? b : c ? d : e").unwrap();
    match expr.kind {
        ExprKind::Conditional { else_expr, .. } => {
            assert!(matches!(else_expr.kind, ExprKind::Conditional { .. }));
        }
        _ => panic!("Expected Conditional"),
    }
}

// Comma expression tests

#[test]
fn test_comma_expr() {
    let (expr, _types, _strings, _symbols) = parse_expr("a, b, c").unwrap();
    match expr.kind {
        ExprKind::Comma(exprs) => assert_eq!(exprs.len(), 3),
        _ => panic!("Expected Comma"),
    }
}

// sizeof tests

#[test]
fn test_sizeof_expr() {
    let (expr, _types, _strings, _symbols) = parse_expr("sizeof x").unwrap();
    assert!(matches!(expr.kind, ExprKind::SizeofExpr(_)));
}

#[test]
fn test_sizeof_type() {
    let (expr, types, _strings, _symbols) = parse_expr("sizeof(int)").unwrap();
    match expr.kind {
        ExprKind::SizeofType(typ, _) => assert_eq!(types.kind(typ), TypeKind::Int),
        _ => panic!("Expected SizeofType"),
    }
}

/// `sizeof` of a variably-modified type-name carries one size expression per
/// array level whose extent is absent, outermost-first, because the interned
/// type cannot hold them: `int[n]`, `int[m]` and `int[]` are one `TypeId`.
///
/// `sizeof_type_is_runtime` is what every consumer asks, so the pairing rule
/// and its refusals are asserted through it rather than by counting the `Vec`.
#[test]
fn test_sizeof_variably_modified_type_name_carries_its_dimensions() {
    use crate::parse::ast::sizeof_type_is_runtime;

    // One absent extent, one expression.
    let (expr, types, _s, _y) = parse_expr_with_vars("sizeof(int[n])", &["n"]).unwrap();
    match &expr.kind {
        ExprKind::SizeofType(typ, dims) => {
            assert_eq!(dims.len(), 1);
            assert!(sizeof_type_is_runtime(&types, *typ, dims));
        }
        _ => panic!("Expected SizeofType"),
    }

    // A constant outer level contributes no expression; the inner one does.
    for src in ["sizeof(int[3][n])", "sizeof(int[n][3])"] {
        let (expr, types, _s, _y) = parse_expr_with_vars(src, &["n"]).unwrap();
        match &expr.kind {
            ExprKind::SizeofType(typ, dims) => {
                assert_eq!(dims.len(), 1, "{src}");
                assert!(sizeof_type_is_runtime(&types, *typ, dims), "{src}");
            }
            _ => panic!("Expected SizeofType for {src}"),
        }
    }

    // Two absent extents, two expressions, in source order.
    let (expr, types, _s, _y) = parse_expr_with_vars("sizeof(int[n][m])", &["n", "m"]).unwrap();
    match &expr.kind {
        ExprKind::SizeofType(typ, dims) => {
            assert_eq!(dims.len(), 2);
            assert!(sizeof_type_is_runtime(&types, *typ, dims));
        }
        _ => panic!("Expected SizeofType"),
    }

    // Nothing variably modified: no expressions, and not a run-time size.
    for src in ["sizeof(int)", "sizeof(int[4])", "sizeof(int[3][4])"] {
        let (expr, types, _s, _y) = parse_expr(src).unwrap();
        match &expr.kind {
            ExprKind::SizeofType(typ, dims) => {
                assert!(dims.is_empty(), "{src}");
                assert!(!sizeof_type_is_runtime(&types, *typ, dims), "{src}");
            }
            _ => panic!("Expected SizeofType for {src}"),
        }
    }

    // A pointer to a variably-modified array is the pointer's size, and its
    // extent is not evaluated -- gcc agrees.
    let (expr, types, _s, _y) = parse_expr_with_vars("sizeof(int(*)[n])", &["n"]).unwrap();
    match &expr.kind {
        ExprKind::SizeofType(typ, dims) => {
            assert!(
                !sizeof_type_is_runtime(&types, *typ, dims),
                "a pointer to a VLA is not a run-time sizeof"
            );
        }
        _ => panic!("Expected SizeofType"),
    }
}

#[test]
fn test_sizeof_paren_expr() {
    // sizeof(x) where x is not a type
    let (expr, _types, _strings, _symbols) = parse_expr("sizeof(x)").unwrap();
    assert!(matches!(expr.kind, ExprKind::SizeofExpr(_)));
}

// Cast tests

#[test]
fn test_cast() {
    let (expr, types, strings, symbols) = parse_expr_with_vars("(int)x", &["x"]).unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, expr } => {
            assert_eq!(types.kind(cast_type), TypeKind::Int);
            match expr.kind {
                ExprKind::Ident(symbol_id) => {
                    check_name(&strings, symbols.get(symbol_id).name, "x")
                }
                _ => panic!("Expected Ident"),
            }
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_unsigned_char() {
    let (expr, types, _strings, _symbols) = parse_expr("(unsigned char)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::Char);
            assert!(types
                .get(cast_type)
                .modifiers
                .contains(TypeModifiers::UNSIGNED));
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_signed_int() {
    let (expr, types, _strings, _symbols) = parse_expr("(signed int)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::Int);
            assert!(types
                .get(cast_type)
                .modifiers
                .contains(TypeModifiers::SIGNED));
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_unsigned_long() {
    let (expr, types, _strings, _symbols) = parse_expr("(unsigned long)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::Long);
            assert!(types
                .get(cast_type)
                .modifiers
                .contains(TypeModifiers::UNSIGNED));
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_long_long() {
    let (expr, types, _strings, _symbols) = parse_expr("(long long)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::LongLong);
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_unsigned_long_long() {
    let (expr, types, _strings, _symbols) = parse_expr("(unsigned long long)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::LongLong);
            assert!(types
                .get(cast_type)
                .modifiers
                .contains(TypeModifiers::UNSIGNED));
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_pointer() {
    let (expr, types, _strings, _symbols) = parse_expr("(int*)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::Pointer);
            let base = types.base_type(cast_type).unwrap();
            assert_eq!(types.kind(base), TypeKind::Int);
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_void_pointer() {
    let (expr, types, _strings, _symbols) = parse_expr("(void*)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::Pointer);
            let base = types.base_type(cast_type).unwrap();
            assert_eq!(types.kind(base), TypeKind::Void);
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_unsigned_char_pointer() {
    let (expr, types, _strings, _symbols) = parse_expr("(unsigned char*)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::Pointer);
            let base = types.base_type(cast_type).unwrap();
            assert_eq!(types.kind(base), TypeKind::Char);
            assert!(types.get(base).modifiers.contains(TypeModifiers::UNSIGNED));
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_const_int() {
    // A cast yields a value, so a cast to `const int` is a cast to `int`
    // (C17 6.5.4p5, footnote 108), and the expression's type says so.
    let (expr, types, _strings, _symbols) = parse_expr("(const int)x").unwrap();
    assert_eq!(expr.typ, Some(types.int_id));
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::Int);
            assert!(!types
                .get(cast_type)
                .modifiers
                .contains(TypeModifiers::CONST));
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_double_pointer() {
    let (expr, types, _strings, _symbols) = parse_expr("(int**)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::Pointer);
            let base = types.base_type(cast_type).unwrap();
            assert_eq!(types.kind(base), TypeKind::Pointer);
            let innermost = types.base_type(base).unwrap();
            assert_eq!(types.kind(innermost), TypeKind::Int);
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_const_pointer() {
    // Test pointer qualifiers: (const int *)x
    let (expr, types, _strings, _symbols) = parse_expr("(const int *)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::Pointer);
            let base = types.base_type(cast_type).unwrap();
            assert_eq!(types.kind(base), TypeKind::Int);
            // const applies to the pointed-to int
            assert!(types.get(base).modifiers.contains(TypeModifiers::CONST));
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_pointer_to_const() {
    // Test pointer qualifiers after *: (int * const)x - const pointer to int
    // The `const` qualifies the pointer itself, which a cast drops along
    // with every other top-level qualifier (C17 6.5.4p5).
    let (expr, types, _strings, _symbols) = parse_expr("(int * const)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::Pointer);
            assert!(!types
                .get(cast_type)
                .modifiers
                .contains(TypeModifiers::CONST));
            let base = types.base_type(cast_type).unwrap();
            assert_eq!(types.kind(base), TypeKind::Int);
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_double_pointer_with_qualifiers() {
    // Test: (const int * const *)x - pointer to const pointer to const int
    let (expr, types, _strings, _symbols) = parse_expr("(const int * const *)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            // Outer is pointer (to const pointer to const int)
            assert_eq!(types.kind(cast_type), TypeKind::Pointer);

            // Middle is const pointer
            let middle = types.base_type(cast_type).unwrap();
            assert_eq!(types.kind(middle), TypeKind::Pointer);
            assert!(types.get(middle).modifiers.contains(TypeModifiers::CONST));

            // Inner is const int
            let inner = types.base_type(middle).unwrap();
            assert_eq!(types.kind(inner), TypeKind::Int);
            assert!(types.get(inner).modifiers.contains(TypeModifiers::CONST));
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_volatile_pointer() {
    // Test volatile qualifier: (volatile int *)x
    let (expr, types, _strings, _symbols) = parse_expr("(volatile int *)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::Pointer);
            let base = types.base_type(cast_type).unwrap();
            assert!(types.get(base).modifiers.contains(TypeModifiers::VOLATILE));
        }
        _ => panic!("Expected Cast"),
    }
}

#[test]
fn test_cast_int128_constant_folding() {
    // (__int128)42 should fold to Int128Lit(42)
    let (expr, types, _strings, _symbols) = parse_expr("(__int128)42").unwrap();
    match expr.kind {
        ExprKind::Int128Lit(val) => {
            assert_eq!(val, 42);
            assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Int128);
        }
        _ => panic!("Expected Int128Lit, got {:?}", expr.kind),
    }
}

#[test]
fn test_cast_int128_constant_folding_negative() {
    // (__int128)(-1) should fold to Int128Lit(-1)
    let (expr, types, _strings, _symbols) = parse_expr("(__int128)(-1)").unwrap();
    match expr.kind {
        ExprKind::Int128Lit(val) => {
            assert_eq!(val, -1);
            assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Int128);
        }
        _ => panic!("Expected Int128Lit, got {:?}", expr.kind),
    }
}

#[test]
fn test_cast_int128_non_constant_no_fold() {
    // (__int128)x should remain as Cast (non-constant expression)
    let (expr, types, _strings, _symbols) = parse_expr_with_vars("(__int128)x", &["x"]).unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::Int128);
        }
        _ => panic!("Expected Cast, got {:?}", expr.kind),
    }
}

#[test]
fn test_sizeof_compound_type() {
    let (expr, types, _strings, _symbols) = parse_expr("sizeof(unsigned long long)").unwrap();
    match expr.kind {
        ExprKind::SizeofType(typ, _) => {
            assert_eq!(types.kind(typ), TypeKind::LongLong);
            assert!(types.get(typ).modifiers.contains(TypeModifiers::UNSIGNED));
        }
        _ => panic!("Expected SizeofType"),
    }
}

#[test]
fn test_sizeof_pointer_type() {
    let (expr, types, _strings, _symbols) = parse_expr("sizeof(int*)").unwrap();
    match expr.kind {
        ExprKind::SizeofType(typ, _) => {
            assert_eq!(types.kind(typ), TypeKind::Pointer);
        }
        _ => panic!("Expected SizeofType"),
    }
}

// Parentheses tests

#[test]
fn test_parentheses() {
    let (expr, _types, _strings, _symbols) = parse_expr("(1 + 2) * 3").unwrap();
    match expr.kind {
        ExprKind::Binary { op, left, .. } => {
            assert_eq!(op, BinaryOp::Mul);
            match left.kind {
                ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Add),
                _ => panic!("Expected Binary"),
            }
        }
        _ => panic!("Expected Binary"),
    }
}

// Complex expression tests

#[test]
fn test_complex_expr() {
    // x = a + b * c - d / e
    let (expr, _types, _strings, _symbols) = parse_expr("x = a + b * c - d / e").unwrap();
    assert!(matches!(expr.kind, ExprKind::Assign { .. }));
}

#[test]
fn test_function_call_complex() {
    // foo(a + b, c * d)
    let (expr, _types, _strings, _symbols) = parse_expr("foo(a + b, c * d)").unwrap();
    match expr.kind {
        ExprKind::Call { args, .. } => {
            assert_eq!(args.len(), 2);
            assert!(matches!(
                args[0].kind,
                ExprKind::Binary {
                    op: BinaryOp::Add,
                    ..
                }
            ));
            assert!(matches!(
                args[1].kind,
                ExprKind::Binary {
                    op: BinaryOp::Mul,
                    ..
                }
            ));
        }
        _ => panic!("Expected Call"),
    }
}

#[test]
fn test_pointer_arithmetic() {
    // *p++
    let (expr, _types, _strings, _symbols) = parse_expr("*p++").unwrap();
    match expr.kind {
        ExprKind::Unary {
            op: UnaryOp::Deref,
            operand,
        } => {
            assert!(matches!(operand.kind, ExprKind::PostInc(_)));
        }
        _ => panic!("Expected Unary Deref"),
    }
}

// Statement tests

fn parse_stmt(input: &str) -> ParseResult<(Stmt, StringTable)> {
    parse_stmt_with_vars(input, &[])
}

/// Parse statement with pre-declared variables
fn parse_stmt_with_vars(input: &str, vars: &[&str]) -> ParseResult<(Stmt, StringTable)> {
    let mut strings = StringTable::new();
    let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(&Target::host());

    // Pre-declare variables
    for var_name in vars {
        let name_id = strings.intern(var_name);
        let sym = Symbol::variable(name_id, types.int_id, 0);
        let _ = symbols.declare(sym);
    }

    let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
    parser.skip_stream_tokens();
    let stmt = parser.parse_statement()?;
    Ok((stmt, strings))
}

#[test]
fn test_empty_stmt() {
    let (stmt, _strings) = parse_stmt(";").unwrap();
    assert!(matches!(stmt, Stmt::Empty));
}

#[test]
fn test_expr_stmt() {
    let (stmt, _strings) = parse_stmt("x = 5;").unwrap();
    assert!(matches!(stmt, Stmt::Expr(_)));
}

#[test]
fn test_if_stmt() {
    let (stmt, _strings) = parse_stmt_with_vars("if (x) y = 1;", &["x", "y"]).unwrap();
    match stmt {
        Stmt::If {
            cond,
            then_stmt,
            else_stmt,
        } => {
            assert!(matches!(cond.kind, ExprKind::Ident(_)));
            assert!(matches!(*then_stmt, Stmt::Expr(_)));
            assert!(else_stmt.is_none());
        }
        _ => panic!("Expected If"),
    }
}

#[test]
fn test_if_else_stmt() {
    let (stmt, _strings) = parse_stmt("if (x) y = 1; else y = 2;").unwrap();
    match stmt {
        Stmt::If { else_stmt, .. } => {
            assert!(else_stmt.is_some());
        }
        _ => panic!("Expected If"),
    }
}

#[test]
fn test_while_stmt() {
    let (stmt, _strings) = parse_stmt_with_vars("while (x) x--;", &["x"]).unwrap();
    match stmt {
        Stmt::While { cond, body } => {
            assert!(matches!(cond.kind, ExprKind::Ident(_)));
            assert!(matches!(*body, Stmt::Expr(_)));
        }
        _ => panic!("Expected While"),
    }
}

#[test]
fn test_do_while_stmt() {
    let (stmt, _strings) = parse_stmt("do x++; while (x < 10);").unwrap();
    match stmt {
        Stmt::DoWhile { body, cond } => {
            assert!(matches!(*body, Stmt::Expr(_)));
            assert!(matches!(
                cond.kind,
                ExprKind::Binary {
                    op: BinaryOp::Lt,
                    ..
                }
            ));
        }
        _ => panic!("Expected DoWhile"),
    }
}

#[test]
fn test_for_stmt_basic() {
    let (stmt, _strings) = parse_stmt("for (i = 0; i < 10; i++) x++;").unwrap();
    match stmt {
        Stmt::For {
            init,
            cond,
            post,
            body,
        } => {
            assert!(init.is_some());
            assert!(cond.is_some());
            assert!(post.is_some());
            assert!(matches!(*body, Stmt::Expr(_)));
        }
        _ => panic!("Expected For"),
    }
}

#[test]
fn test_for_stmt_with_decl() {
    let (stmt, _strings) = parse_stmt("for (int i = 0; i < 10; i++) x++;").unwrap();
    match stmt {
        Stmt::For { init, .. } => {
            assert!(matches!(init, Some(ForInit::Declaration(_))));
        }
        _ => panic!("Expected For"),
    }
}

#[test]
fn test_for_stmt_empty() {
    let (stmt, _strings) = parse_stmt("for (;;) ;").unwrap();
    match stmt {
        Stmt::For {
            init,
            cond,
            post,
            body,
        } => {
            assert!(init.is_none());
            assert!(cond.is_none());
            assert!(post.is_none());
            assert!(matches!(*body, Stmt::Empty));
        }
        _ => panic!("Expected For"),
    }
}

#[test]
fn test_for_stmt_static_error() {
    // C99: storage class specifiers are not allowed in for-loop init declarations
    let result = parse_stmt("for (static int i = 0; i < 10; i++) x++;");
    assert!(result.is_err(), "static in for-init should be an error");
    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("static"),
        "error message should mention static"
    );
}

#[test]
fn test_for_stmt_extern_error() {
    // C99: storage class specifiers are not allowed in for-loop init declarations
    let result = parse_stmt("for (extern int i; i < 10; i++) x++;");
    assert!(result.is_err(), "extern in for-init should be an error");
    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("extern"),
        "error message should mention extern"
    );
}

#[test]
fn test_return_void() {
    let (stmt, _strings) = parse_stmt("return;").unwrap();
    match stmt {
        Stmt::Return(None) => {}
        _ => panic!("Expected Return(None)"),
    }
}

#[test]
fn test_return_value() {
    let (stmt, _strings) = parse_stmt("return 42;").unwrap();
    match stmt {
        Stmt::Return(Some(ref e)) => {
            assert!(matches!(e.kind, ExprKind::IntLit(42)));
        }
        _ => panic!("Expected Return(Some(42))"),
    }
}

#[test]
fn test_break_stmt() {
    let (stmt, _strings) = parse_stmt("break;").unwrap();
    assert!(matches!(stmt, Stmt::Break(_)));
}

#[test]
fn test_continue_stmt() {
    let (stmt, _strings) = parse_stmt("continue;").unwrap();
    assert!(matches!(stmt, Stmt::Continue(_)));
}

#[test]
fn test_goto_stmt() {
    let (stmt, strings) = parse_stmt("goto label;").unwrap();
    match stmt {
        Stmt::Goto { name, .. } => check_name(&strings, name, "label"),
        _ => panic!("Expected Goto"),
    }
}

/// Basic asm is emitted verbatim, so every `%` in it is escaped for the
/// substitution extended asm shares; extended asm, even with no operands,
/// keeps its template as written.
#[test]
fn test_asm_basic_template_escapes_percent() {
    let template = |src| match parse_stmt(src).unwrap().0 {
        Stmt::Asm { template, .. } => template,
        other => panic!("expected asm: {other:?}"),
    };
    assert_eq!(
        template("__asm__(\"mov %eax, %%ebx\");"),
        "mov %%eax, %%%%ebx"
    );
    assert_eq!(
        template("__asm__(\"mov %eax, %%ebx\" ::);"),
        "mov %eax, %%ebx"
    );
    assert_eq!(
        template("__asm__(\"mov %0, %%ebx\" :: \"r\"(1));"),
        "mov %0, %%ebx"
    );
}

#[test]
fn test_labeled_stmt() {
    let (stmt, strings) = parse_stmt("label: x = 1;").unwrap();
    match stmt {
        Stmt::Labeled { labels, stmt } => {
            let [Label::Named { name, .. }] = labels.as_slice() else {
                panic!("expected one goto label: {labels:?}");
            };
            check_name(&strings, *name, "label");
            assert!(matches!(*stmt, Stmt::Expr(_)));
        }
        _ => panic!("Expected Label"),
    }
}

/// A run of labels is one labeled statement holding every label in source
/// order, not a chain of statements each labelling the next.
#[test]
fn test_consecutive_labels_are_one_list() {
    let (stmt, strings) =
        parse_stmt_with_vars("case 1: a: default: case 2 ... 3: b: x = 1;", &["x"]).unwrap();
    let Stmt::Labeled { labels, stmt } = stmt else {
        panic!("expected a labeled statement: {stmt:?}");
    };
    let [Label::Case(_, None), Label::Named { name: a, .. }, Label::Default(_), Label::Case(_, Some(_)), Label::Named { name: b, .. }] =
        labels.as_slice()
    else {
        panic!("labels out of order: {labels:?}");
    };
    check_name(&strings, *a, "a");
    check_name(&strings, *b, "b");
    assert!(matches!(*stmt, Stmt::Expr(_)), "{stmt:?}");
}

/// A statement keyword is not a goto label: `else:` is an error, not a label
/// named `else`.
#[test]
fn test_statement_keyword_is_not_a_label() {
    assert!(parse_stmt("else: x = 1;").is_err());
}

#[test]
fn test_block_stmt() {
    let (stmt, _strings) = parse_stmt("{ x = 1; y = 2; }").unwrap();
    match stmt {
        Stmt::Block(items) => {
            assert_eq!(items.len(), 2);
            assert!(
                matches!(&items[0], BlockItem::Statement(s) if matches!(s.as_ref(), Stmt::Expr(_)))
            );
            assert!(
                matches!(&items[1], BlockItem::Statement(s) if matches!(s.as_ref(), Stmt::Expr(_)))
            );
        }
        _ => panic!("Expected Block"),
    }
}

#[test]
fn test_block_with_decl() {
    let (stmt, _strings) = parse_stmt("{ int x = 1; x++; }").unwrap();
    match stmt {
        Stmt::Block(items) => {
            assert_eq!(items.len(), 2);
            assert!(matches!(items[0], BlockItem::Declaration(_)));
            assert!(matches!(items[1], BlockItem::Statement(_)));
        }
        _ => panic!("Expected Block"),
    }
}

// Declaration tests

fn parse_decl(input: &str) -> ParseResult<(Declaration, TypeTable, StringTable, SymbolTable)> {
    let mut strings = StringTable::new();
    let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(&Target::host());
    let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
    parser.skip_stream_tokens();
    // The production entry point, not a copy of it: `parse_declaration` and
    // `parse_function_def` were `#[cfg(test)]` duplicates of this and had
    // drifted from it, so these tests were checking a parser that no
    // translation unit ever ran through (#C133).
    match parser.parse_external_decl()? {
        ExternalDecl::Declaration(decl) => Ok((decl, types, strings, symbols)),
        ExternalDecl::FunctionDef(_) => {
            panic!("expected a declaration, parsed a function definition: {input}")
        }
    }
}

#[test]
fn test_simple_decl() {
    let (decl, types, strings, symbols) = parse_decl("int x;").unwrap();
    assert_eq!(decl.declarators.len(), 1);
    check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "x");
    assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Int);
}

#[test]
fn test_decl_with_init() {
    let (decl, _types, _strings, _symbols) = parse_decl("int x = 5;").unwrap();
    assert_eq!(decl.declarators.len(), 1);
    assert!(decl.declarators[0].init.is_some());
}

#[test]
fn test_multiple_declarators() {
    let (decl, _types, strings, symbols) = parse_decl("int x, y, z;").unwrap();
    assert_eq!(decl.declarators.len(), 3);
    check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "x");
    check_name(&strings, symbols.get(decl.declarators[1].symbol).name, "y");
    check_name(&strings, symbols.get(decl.declarators[2].symbol).name, "z");
}

#[test]
fn test_pointer_decl() {
    let (decl, types, _strings, _symbols) = parse_decl("int *p;").unwrap();
    assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
}

#[test]
fn test_array_decl() {
    let (decl, types, _strings, _symbols) = parse_decl("int arr[10];").unwrap();
    assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Array);
    assert_eq!(types.get(decl.declarators[0].typ).array_size, Some(10));
}

#[test]
fn test_const_decl() {
    let (decl, types, _strings, _symbols) = parse_decl("const int x = 5;").unwrap();
    assert!(types
        .get(decl.declarators[0].typ)
        .modifiers
        .contains(TypeModifiers::CONST));
}

#[test]
fn test_unsigned_decl() {
    let (decl, types, _strings, _symbols) = parse_decl("unsigned int x;").unwrap();
    assert!(types
        .get(decl.declarators[0].typ)
        .modifiers
        .contains(TypeModifiers::UNSIGNED));
}

#[test]
fn test_long_long_decl() {
    let (decl, types, _strings, _symbols) = parse_decl("long long x;").unwrap();
    assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::LongLong);
}

#[test]
fn test_extern_pointer_modifier_propagation() {
    let (decl, types, _strings, _symbols) = parse_decl("extern int *p;").unwrap();
    let ptr_typ = decl.declarators[0].typ;

    // Verify it's a pointer type
    assert_eq!(types.kind(ptr_typ), TypeKind::Pointer);

    // Verify EXTERN modifier is on the pointer type
    assert!(
        types.get(ptr_typ).modifiers.contains(TypeModifiers::EXTERN),
        "EXTERN modifier should propagate to pointer type"
    );
}

#[test]
fn test_static_pointer_modifier_propagation() {
    let (decl, types, _strings, _symbols) = parse_decl("static int *p;").unwrap();
    let ptr_typ = decl.declarators[0].typ;

    // Verify it's a pointer type
    assert_eq!(types.kind(ptr_typ), TypeKind::Pointer);

    // Verify STATIC modifier is on the pointer type
    assert!(
        types.get(ptr_typ).modifiers.contains(TypeModifiers::STATIC),
        "STATIC modifier should propagate to pointer type"
    );
}

#[test]
fn test_typedef_array_modifier_propagation() {
    let (decl, types, _strings, _symbols) = parse_decl("typedef int arr[10];").unwrap();
    let arr_typ = decl.declarators[0].typ;

    // Verify it's an array type
    assert_eq!(types.kind(arr_typ), TypeKind::Array);

    // Verify TYPEDEF modifier is on the array type
    assert!(
        types
            .get(arr_typ)
            .modifiers
            .contains(TypeModifiers::TYPEDEF),
        "TYPEDEF modifier should propagate to array type"
    );
}

// Function parsing tests

fn parse_func(input: &str) -> ParseResult<(FunctionDef, TypeTable, StringTable, SymbolTable)> {
    let mut strings = StringTable::new();
    let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(&Target::host());
    let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
    parser.skip_stream_tokens();
    // See `parse_decl`: the production entry point.
    match parser.parse_external_decl()? {
        ExternalDecl::FunctionDef(func) => Ok((func, types, strings, symbols)),
        ExternalDecl::Declaration(_) => {
            panic!("expected a function definition, parsed a declaration: {input}")
        }
    }
}

#[test]
fn test_simple_function() {
    let (func, types, strings, _symbols) = parse_func("int main() { return 0; }").unwrap();
    check_name(&strings, func.name, "main");
    assert_eq!(types.kind(func.return_type), TypeKind::Int);
    assert!(func.params.is_empty());
}

#[test]
fn test_function_with_params() {
    let (func, _types, strings, _symbols) =
        parse_func("int add(int a, int b) { return a + b; }").unwrap();
    check_name(&strings, func.name, "add");
    assert_eq!(func.params.len(), 2);
}

#[test]
fn test_void_function() {
    let (func, types, _strings, _symbols) = parse_func("void foo(void) { }").unwrap();
    assert_eq!(types.kind(func.return_type), TypeKind::Void);
    assert!(func.params.is_empty());
}

#[test]
fn test_variadic_function() {
    // Variadic functions are parsed but variadic info is not tracked in FunctionDef
    let (func, _types, strings, _symbols) =
        parse_func("int printf(char *fmt, ...) { return 0; }").unwrap();
    check_name(&strings, func.name, "printf");
}

#[test]
fn test_pointer_return() {
    let (func, types, _strings, _symbols) = parse_func("int *getptr() { return 0; }").unwrap();
    assert_eq!(types.kind(func.return_type), TypeKind::Pointer);
}

/// A grouped declarator before a pointer run -- `void (*fp)(int)` -- and after
/// one -- `char *(*fp)(int)`. They were once two near-identical 140-line
/// blocks; these pin the shapes each one owned.
#[test]
fn test_grouped_declarator_before_and_after_pointers() {
    // Before the pointer loop: the declarator starts from the specifier type.
    let (decl, types, strings, symbols) = parse_decl("void (*fp)(int);").unwrap();
    assert_eq!(decl.declarators.len(), 1);
    check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "fp");
    assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);

    // After it: the declarator starts from the pointer-derived type.
    let (decl, types, strings, symbols) = parse_decl("char *(*fp)(int);").unwrap();
    assert_eq!(decl.declarators.len(), 1);
    check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "fp");
    assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);

    // An array through a grouped declarator, and a grouped function typedef.
    let (decl, types, _strings, _symbols) = parse_decl("int (*arr)[10];").unwrap();
    assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
    let (decl, _types, strings, symbols) = parse_decl("typedef int (cmp)(int, int);").unwrap();
    check_name(
        &strings,
        symbols.get(decl.declarators[0].symbol).name,
        "cmp",
    );
}

/// A grouped declarator that turns out to be a function *definition* still
/// takes the definition path: `int (*get_op(int which))(int, int) { .. }`
/// returns a pointer to function and has a body.
#[test]
fn test_grouped_declarator_function_definition() {
    let (func, types, strings, _symbols) =
        parse_func("int (*get_op(int which))(int, int) { return 0; }").unwrap();
    check_name(&strings, func.name, "get_op");
    assert_eq!(types.kind(func.return_type), TypeKind::Pointer);
    assert_eq!(func.params.len(), 1);
}

/// A plain file-scope declaration must not be routed down the
/// function-declarator path just because that block moved into a helper: the
/// `(` guard travels with it.
#[test]
fn test_plain_declaration_is_not_a_function_declarator() {
    let (decl, types, strings, symbols) = parse_decl("int x;").unwrap();
    check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "x");
    assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Int);
}

// Translation unit tests

pub(super) fn parse_tu(
    input: &str,
) -> ParseResult<(TranslationUnit, TypeTable, StringTable, SymbolTable)> {
    parse_tu_for(input, &Target::host())
}

/// [`parse_tu`] for `target`; see [`parse_expr_for`] for when a test needs it.
fn parse_tu_for(
    input: &str,
    target: &Target,
) -> ParseResult<(TranslationUnit, TypeTable, StringTable, SymbolTable)> {
    let mut strings = StringTable::new();
    let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(target);
    let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
    let tu = parser.parse_translation_unit()?;
    Ok((tu, types, strings, symbols))
}

#[test]
fn test_simple_program() {
    let (tu, _types, _strings, _symbols) = parse_tu("int main() { return 0; }").unwrap();
    assert_eq!(tu.items.len(), 1);
    assert!(matches!(tu.items[0], ExternalDecl::FunctionDef(_)));
}

/// C17 6.7.6.2p2 confines a variably modified ordinary identifier to block
/// scope, and 6.7.6.2p1 requires an array size to be greater than zero.
#[test]
fn test_file_scope_array_size_constraints() {
    for src in [
        "int n; int bad[n];",
        "int n; static int bad[n];",
        "int n; int bad[n][2];",
        "int n; int bad[2][n];",
        "int n; int ok[2], bad[n];",
    ] {
        match parse_tu(src) {
            Err(e) => assert!(
                e.to_string().contains("file scope"),
                "{src}: wrong message: {e}"
            ),
            Ok(_) => panic!("{src} should have been rejected"),
        }
    }

    for src in [
        "int bad[-1];",
        // Block scope reaches the same check through `parse_declarator`.
        "int main(void){ int bad[-1]; return 0; }",
    ] {
        match parse_tu(src) {
            Err(e) => assert!(e.to_string().contains("negative"), "{src}: {e}"),
            Ok(_) => panic!("{src} should have been rejected"),
        }
    }

    // The forms that must keep parsing: a constant bound, an incomplete array
    // (a tentative definition), the GNU zero-length array, and a VLA where it
    // is legal.
    for src in [
        "int ok[3];",
        "int ok[];",
        "extern int ok[];",
        "int ok[0];",
        "enum { N = 4 }; int ok[N];",
        "int ok[sizeof(int)];",
        "int f(int n, int a[n]);",
        "int main(void){ int n = 4; int ok[n]; return ok[0]; }",
    ] {
        assert!(parse_tu(src).is_ok(), "{src} should parse");
    }
}

#[test]
fn test_global_var() {
    let (tu, _types, _strings, _symbols) = parse_tu("int x = 5;").unwrap();
    assert_eq!(tu.items.len(), 1);
    assert!(matches!(tu.items[0], ExternalDecl::Declaration(_)));
}

#[test]
fn test_multiple_items() {
    let (tu, _types, _strings, _symbols) = parse_tu("int x; int main() { return x; }").unwrap();
    assert_eq!(tu.items.len(), 2);
    assert!(matches!(tu.items[0], ExternalDecl::Declaration(_)));
    assert!(matches!(tu.items[1], ExternalDecl::FunctionDef(_)));
}

#[test]
fn test_function_declaration() {
    let (tu, types, strings, symbols) = parse_tu("int foo(int x);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "foo",
            );
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_struct_only_declaration() {
    // Struct definition without a variable declarator
    let (tu, _types, _strings, _symbols) = parse_tu("struct point { int x; int y; };").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            // No declarators for struct-only definition
            assert!(decl.declarators.is_empty());
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_struct_with_variable_declaration() {
    // Struct definition with a variable declarator
    let (tu, types, strings, symbols) = parse_tu("struct point { int x; int y; } p;").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "p");
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Struct);
        }
        _ => panic!("Expected Declaration"),
    }
}

// Typedef tests

#[test]
fn test_typedef_basic() {
    // Basic typedef declaration
    let (tu, types, strings, symbols) = parse_tu("typedef int myint;").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "myint",
            );
            // The type includes the TYPEDEF modifier
            assert!(types
                .get(decl.declarators[0].typ)
                .modifiers
                .contains(TypeModifiers::TYPEDEF));
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_typedef_usage() {
    // Typedef declaration followed by usage
    let (tu, types, strings, symbols) = parse_tu("typedef int myint; myint x;").unwrap();
    assert_eq!(tu.items.len(), 2);

    // First item: typedef declaration
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "myint",
            );
        }
        _ => panic!("Expected typedef Declaration"),
    }

    // Second item: variable using typedef
    match &tu.items[1] {
        ExternalDecl::Declaration(decl) => {
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "x");
            // The variable should have int type (resolved from typedef)
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Int);
        }
        _ => panic!("Expected variable Declaration"),
    }
}

#[test]
fn test_typedef_pointer() {
    // Typedef for pointer type
    let (tu, types, strings, symbols) = parse_tu("typedef int *intptr; intptr p;").unwrap();
    assert_eq!(tu.items.len(), 2);

    // Variable should have pointer type
    match &tu.items[1] {
        ExternalDecl::Declaration(decl) => {
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "p");
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
        }
        _ => panic!("Expected variable Declaration"),
    }
}

#[test]
fn test_typedef_struct() {
    // Typedef for anonymous struct
    let (tu, types, strings, symbols) =
        parse_tu("typedef struct { int x; int y; } Point; Point p;").unwrap();
    assert_eq!(tu.items.len(), 2);

    // Variable should have struct type
    match &tu.items[1] {
        ExternalDecl::Declaration(decl) => {
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "p");
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Struct);
        }
        _ => panic!("Expected variable Declaration"),
    }
}

#[test]
fn test_typedef_chained() {
    // Chained typedef: typedef of typedef
    let (tu, types, strings, symbols) =
        parse_tu("typedef int myint; typedef myint myint2; myint2 x;").unwrap();
    assert_eq!(tu.items.len(), 3);

    // Final variable should resolve to int
    match &tu.items[2] {
        ExternalDecl::Declaration(decl) => {
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "x");
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Int);
        }
        _ => panic!("Expected variable Declaration"),
    }
}

#[test]
fn test_typedef_multiple() {
    // Multiple typedefs in one declaration
    let (tu, types, strings, symbols) = parse_tu("typedef int INT, *INTPTR;").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 2);
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "INT",
            );
            check_name(
                &strings,
                symbols.get(decl.declarators[1].symbol).name,
                "INTPTR",
            );
            // INTPTR should be a pointer type
            assert_eq!(types.kind(decl.declarators[1].typ), TypeKind::Pointer);
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_typedef_in_function() {
    // Typedef used in function parameter and return type
    let (tu, types, strings, _symbols) =
        parse_tu("typedef int myint; myint add(myint a, myint b) { return a + b; }").unwrap();
    assert_eq!(tu.items.len(), 2);

    match &tu.items[1] {
        ExternalDecl::FunctionDef(func) => {
            check_name(&strings, func.name, "add");
            // Return type should resolve to int
            assert_eq!(types.kind(func.return_type), TypeKind::Int);
            // Parameters should also resolve to int
            assert_eq!(func.params.len(), 2);
            assert_eq!(types.kind(func.params[0].typ), TypeKind::Int);
            assert_eq!(types.kind(func.params[1].typ), TypeKind::Int);
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_typedef_local_variable() {
    // Typedef used as local variable type inside function body
    let (tu, _types, strings, _symbols) =
        parse_tu("typedef int myint; int main(void) { myint x; x = 42; return 0; }").unwrap();
    assert_eq!(tu.items.len(), 2);

    match &tu.items[1] {
        ExternalDecl::FunctionDef(func) => {
            check_name(&strings, func.name, "main");
            // Check that the body parsed correctly
            match &func.body {
                Stmt::Block(items) => {
                    assert!(items.len() >= 2, "Expected at least 2 block items");
                }
                _ => panic!("Expected Block statement"),
            }
        }
        _ => panic!("Expected FunctionDef"),
    }
}

// Restrict qualifier tests

#[test]
fn test_restrict_pointer_decl() {
    // Local variable with restrict qualifier
    let (tu, _types, _strings, _symbols) =
        parse_tu("int main(void) { int * restrict p; return 0; }").unwrap();
    assert_eq!(tu.items.len(), 1);
    // Just verify it parses without error
}

#[test]
fn test_restrict_function_param() {
    // Function with restrict-qualified pointer parameters
    let (tu, types, strings, _symbols) =
        parse_tu("void copy(int * restrict dest, int * restrict src) { *dest = *src; }").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::FunctionDef(func) => {
            check_name(&strings, func.name, "copy");
            assert_eq!(func.params.len(), 2);
            // Both params should be restrict-qualified pointers
            assert_eq!(types.kind(func.params[0].typ), TypeKind::Pointer);
            assert!(types
                .get(func.params[0].typ)
                .modifiers
                .contains(TypeModifiers::RESTRICT));
            assert_eq!(types.kind(func.params[1].typ), TypeKind::Pointer);
            assert!(types
                .get(func.params[1].typ)
                .modifiers
                .contains(TypeModifiers::RESTRICT));
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_restrict_with_const() {
    // Pointer with both const and restrict qualifiers
    let (tu, _types, _strings, _symbols) =
        parse_tu("int main(void) { int * const restrict p = 0; return 0; }").unwrap();
    assert_eq!(tu.items.len(), 1);
    // Just verify it parses without error - both qualifiers should be accepted
}

#[test]
fn test_restrict_global_pointer() {
    // Global pointer with restrict qualifier
    let (tu, types, strings, symbols) = parse_tu("int * restrict global_ptr;").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "global_ptr",
            );
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
            assert!(types
                .get(decl.declarators[0].typ)
                .modifiers
                .contains(TypeModifiers::RESTRICT));
        }
        _ => panic!("Expected Declaration"),
    }
}

// Volatile qualifier tests

#[test]
fn test_volatile_basic() {
    // Basic volatile variable
    let (tu, types, strings, symbols) = parse_tu("volatile int x;").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "x");
            assert!(types
                .get(decl.declarators[0].typ)
                .modifiers
                .contains(TypeModifiers::VOLATILE));
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_volatile_pointer() {
    // Pointer to volatile int
    let (tu, types, strings, symbols) = parse_tu("volatile int *p;").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "p");
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
            // The base type should be volatile
            let base_id = types.base_type(decl.declarators[0].typ).unwrap();
            assert!(types
                .get(base_id)
                .modifiers
                .contains(TypeModifiers::VOLATILE));
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_volatile_pointer_itself() {
    // Volatile pointer to int (pointer itself is volatile)
    let (tu, types, strings, symbols) = parse_tu("int * volatile p;").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "p");
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
            // The pointer type itself should be volatile
            assert!(types
                .get(decl.declarators[0].typ)
                .modifiers
                .contains(TypeModifiers::VOLATILE));
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_volatile_const_combined() {
    // Both const and volatile
    let (tu, types, strings, symbols) = parse_tu("const volatile int x;").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "x");
            assert!(types
                .get(decl.declarators[0].typ)
                .modifiers
                .contains(TypeModifiers::VOLATILE));
            assert!(types
                .get(decl.declarators[0].typ)
                .modifiers
                .contains(TypeModifiers::CONST));
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_volatile_function_param() {
    // Function with volatile pointer parameter
    let (tu, types, strings, _symbols) = parse_tu("void foo(volatile int *p) { *p = 1; }").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::FunctionDef(func) => {
            check_name(&strings, func.name, "foo");
            assert_eq!(func.params.len(), 1);
            // Parameter is pointer to volatile int
            assert_eq!(types.kind(func.params[0].typ), TypeKind::Pointer);
            let base_id = types.base_type(func.params[0].typ).unwrap();
            assert!(types
                .get(base_id)
                .modifiers
                .contains(TypeModifiers::VOLATILE));
        }
        _ => panic!("Expected FunctionDef"),
    }
}

// __attribute__ tests

#[test]
fn test_attribute_on_function_declaration() {
    // Attribute on function declaration
    let (tu, _types, strings, symbols) =
        parse_tu("void foo(void) __attribute__((noreturn));").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "foo",
            );
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_attribute_on_struct() {
    // Attribute between struct keyword and name (with variable)
    let (tu, types, strings, symbols) =
        parse_tu("struct __attribute__((packed)) foo { int x; } s;").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "s");
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Struct);
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_attribute_after_struct() {
    // Attribute after struct closing brace (with variable)
    let (tu, types, strings, symbols) =
        parse_tu("struct foo { int x; } __attribute__((aligned(16))) s;").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "s");
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Struct);
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_attribute_on_struct_only() {
    // Attribute on struct-only definition (no variable)
    let (tu, _types, _strings, _symbols) =
        parse_tu("struct __attribute__((packed)) foo { int x; };").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            // No variable declared, just the struct definition
            assert_eq!(decl.declarators.len(), 0);
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_attribute_on_variable() {
    // Attribute on variable declaration
    let (tu, _types, strings, symbols) = parse_tu("int x __attribute__((aligned(8)));").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "x");
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_attribute_multiple() {
    // Multiple attributes in one list
    let (tu, _types, strings, symbols) =
        parse_tu("void foo(void) __attribute__((noreturn, cold));").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "foo",
            );
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_attribute_with_args() {
    // Attribute with multiple arguments
    let (tu, _types, strings, symbols) =
        parse_tu("void foo(const char *fmt, ...) __attribute__((__format__(__printf__, 1, 2)));")
            .unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "foo",
            );
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_attribute_before_declaration() {
    // Attribute before declaration
    let (tu, _types, strings, symbols) =
        parse_tu("__attribute__((visibility(\"default\"))) int exported_var;").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "exported_var",
            );
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_attribute_underscore_variant() {
    // __attribute variant (single underscore pair)
    let (tu, _types, strings, symbols) =
        parse_tu("void foo(void) __attribute((noreturn));").unwrap();
    assert_eq!(tu.items.len(), 1);

    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "foo",
            );
        }
        _ => panic!("Expected Declaration"),
    }
}

// ========================================================================
// Const enforcement tests
// ========================================================================
// Note: Error detection is tested via integration tests, not unit tests,
// because the error counter is global state shared across parallel tests.
// These tests verify that const parsing works and code with const violations
// still produces a valid AST (parsing continues after reporting errors).

#[test]
fn test_const_assignment_parses() {
    // Assignment to const variable should still parse (errors are reported but parsing continues)
    let (tu, _types, _strings, _symbols) =
        parse_tu("int main(void) { const int x = 42; x = 10; return 0; }").unwrap();
    assert_eq!(tu.items.len(), 1);
    // Verify we got a function definition
    assert!(matches!(tu.items[0], ExternalDecl::FunctionDef(_)));
}

#[test]
fn test_const_pointer_deref_parses() {
    // Assignment through pointer to const should still parse
    let (tu, _types, _strings, _symbols) =
        parse_tu("int main(void) { int v = 1; const int *p = &v; *p = 2; return 0; }").unwrap();
    assert_eq!(tu.items.len(), 1);
    assert!(matches!(tu.items[0], ExternalDecl::FunctionDef(_)));
}

#[test]
fn test_const_usage_valid() {
    // Valid const usage - reading const values
    let (tu, _types, _strings, _symbols) =
        parse_tu("int main(void) { const int x = 42; int y = x + 1; return y; }").unwrap();
    assert_eq!(tu.items.len(), 1);
    assert!(matches!(tu.items[0], ExternalDecl::FunctionDef(_)));
}

#[test]
fn test_const_pointer_types() {
    // Different const pointer combinations
    let (tu, _types, _strings, _symbols) = parse_tu(
        "int main(void) { int v = 1; const int *a = &v; int * const b = &v; const int * const c = &v; return 0; }",
    )
    .unwrap();
    assert_eq!(tu.items.len(), 1);
    assert!(matches!(tu.items[0], ExternalDecl::FunctionDef(_)));
}

// Function declaration tests (prototypes)

#[test]
fn test_function_decl_no_params() {
    let (tu, types, strings, symbols) = parse_tu("int foo(void);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "foo",
            );
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
            assert!(!types.is_variadic(decl.declarators[0].typ));
            // Check return type
            if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                assert_eq!(types.kind(base_id), TypeKind::Int);
            }
            // Check params (void means empty)
            if let Some(params) = types.params(decl.declarators[0].typ) {
                assert!(params.is_empty());
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_decl_one_param() {
    let (tu, types, strings, symbols) = parse_tu("int square(int x);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "square",
            );
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
            assert!(!types.is_variadic(decl.declarators[0].typ));
            if let Some(params) = types.params(decl.declarators[0].typ) {
                assert_eq!(params.len(), 1);
                assert_eq!(types.kind(params[0]), TypeKind::Int);
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_decl_multiple_params() {
    let (tu, types, strings, symbols) = parse_tu("int add(int a, int b, int c);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "add",
            );
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
            assert!(!types.is_variadic(decl.declarators[0].typ));
            if let Some(params) = types.params(decl.declarators[0].typ) {
                assert_eq!(params.len(), 3);
                for p in params {
                    assert_eq!(types.kind(*p), TypeKind::Int);
                }
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_decl_void_return() {
    let (tu, types, strings, symbols) = parse_tu("void do_something(int x);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "do_something",
            );
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
            if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                assert_eq!(types.kind(base_id), TypeKind::Void);
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_decl_pointer_return() {
    let (tu, types, strings, symbols) = parse_tu("char *get_string(void);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "get_string",
            );
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
            if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                assert_eq!(types.kind(base_id), TypeKind::Pointer);
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_decl_pointer_param() {
    let (tu, types, strings, symbols) = parse_tu("void process(int *data, int count);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "process",
            );
            if let Some(params) = types.params(decl.declarators[0].typ) {
                assert_eq!(params.len(), 2);
                assert_eq!(types.kind(params[0]), TypeKind::Pointer);
                assert_eq!(types.kind(params[1]), TypeKind::Int);
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

// Variadic function declaration tests

#[test]
fn test_function_decl_variadic_printf() {
    // Classic printf prototype
    let (tu, types, strings, symbols) = parse_tu("int printf(const char *fmt, ...);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "printf",
            );
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
            // Should be marked as variadic
            assert!(
                types.is_variadic(decl.declarators[0].typ),
                "printf should be marked as variadic"
            );
            if let Some(params) = types.params(decl.declarators[0].typ) {
                // Only the fixed parameter (fmt) should be in params
                assert_eq!(params.len(), 1);
                assert_eq!(types.kind(params[0]), TypeKind::Pointer);
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_decl_variadic_sprintf() {
    let (tu, types, strings, symbols) =
        parse_tu("int sprintf(char *buf, const char *fmt, ...);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "sprintf",
            );
            assert!(
                types.is_variadic(decl.declarators[0].typ),
                "sprintf should be marked as variadic"
            );
            if let Some(params) = types.params(decl.declarators[0].typ) {
                // Two fixed parameters: buf and fmt
                assert_eq!(params.len(), 2);
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_decl_variadic_custom() {
    // Custom variadic function with int first param
    let (tu, types, strings, symbols) = parse_tu("int sum_ints(int count, ...);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "sum_ints",
            );
            assert!(
                types.is_variadic(decl.declarators[0].typ),
                "sum_ints should be marked as variadic"
            );
            if let Some(params) = types.params(decl.declarators[0].typ) {
                assert_eq!(params.len(), 1);
                assert_eq!(types.kind(params[0]), TypeKind::Int);
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_decl_variadic_multiple_fixed() {
    // Variadic function with multiple fixed parameters
    let (tu, types, strings, symbols) =
        parse_tu("int variadic_func(int a, double b, char *c, ...);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "variadic_func",
            );
            assert!(
                types.is_variadic(decl.declarators[0].typ),
                "variadic_func should be marked as variadic"
            );
            if let Some(params) = types.params(decl.declarators[0].typ) {
                assert_eq!(params.len(), 3);
                assert_eq!(types.kind(params[0]), TypeKind::Int);
                assert_eq!(types.kind(params[1]), TypeKind::Double);
                assert_eq!(types.kind(params[2]), TypeKind::Pointer);
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_decl_variadic_void_return() {
    // Variadic function with void return type
    let (tu, types, strings, symbols) =
        parse_tu("void log_message(const char *fmt, ...);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "log_message",
            );
            assert!(
                types.is_variadic(decl.declarators[0].typ),
                "log_message should be marked as variadic"
            );
            if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                assert_eq!(types.kind(base_id), TypeKind::Void);
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_decl_not_variadic() {
    // Make sure non-variadic functions are NOT marked as variadic
    let (tu, types, strings, symbols) = parse_tu("int regular_func(int a, int b);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "regular_func",
            );
            assert!(
                !types.is_variadic(decl.declarators[0].typ),
                "regular_func should NOT be marked as variadic"
            );
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_variadic_function_definition() {
    // Variadic function definition (not just declaration)
    let (func, _types, strings, _symbols) =
        parse_func("int my_printf(char *fmt, ...) { return 0; }").unwrap();
    check_name(&strings, func.name, "my_printf");
    // Note: FunctionDef doesn't directly expose variadic, but the function
    // body can use va_start etc. This test just ensures parsing succeeds.
    assert_eq!(func.params.len(), 1);
}

#[test]
fn test_variadic_without_named_param_warning() {
    // Variadic function without named parameter - ISO C violation
    // This should parse successfully (warning is emitted, but parsing continues)
    // The warning can be observed in stderr during the test
    let (tu, types, strings, symbols) = parse_tu("void varargs_only(...);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "varargs_only",
            );
            // Function should be marked as variadic
            assert!(types.is_variadic(decl.declarators[0].typ));
            // No named parameters
            let params = types.params(decl.declarators[0].typ);
            assert!(
                params.is_none_or(|p| p.is_empty()),
                "should have no named params"
            );
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_multiple_function_decls_mixed() {
    // Mix of variadic and non-variadic declarations
    let (tu, types, strings, symbols) = parse_tu(
        "int printf(const char *fmt, ...); int puts(const char *s); int sprintf(char *buf, const char *fmt, ...);",
    )
    .unwrap();
    assert_eq!(tu.items.len(), 3);

    // printf - variadic
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "printf",
            );
            assert!(types.is_variadic(decl.declarators[0].typ));
        }
        _ => panic!("Expected Declaration"),
    }

    // puts - not variadic
    match &tu.items[1] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "puts",
            );
            assert!(!types.is_variadic(decl.declarators[0].typ));
        }
        _ => panic!("Expected Declaration"),
    }

    // sprintf - variadic
    match &tu.items[2] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "sprintf",
            );
            assert!(types.is_variadic(decl.declarators[0].typ));
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_decl_with_struct_param() {
    // Function declaration with struct parameter
    let (tu, types, strings, symbols) =
        parse_tu("struct point { int x; int y; }; void move_point(struct point p);").unwrap();
    assert_eq!(tu.items.len(), 2);
    match &tu.items[1] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "move_point",
            );
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
            assert!(!types.is_variadic(decl.declarators[0].typ));
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_decl_array_decay() {
    // Array parameters decay to pointers in function declarations
    let (tu, types, strings, symbols) = parse_tu("void process_array(int arr[]);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "process_array",
            );
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
            // The array parameter should decay to pointer
            if let Some(params) = types.params(decl.declarators[0].typ) {
                assert_eq!(params.len(), 1);
                // Array params in function declarations become pointers
                let p_kind = types.kind(params[0]);
                assert!(p_kind == TypeKind::Pointer || p_kind == TypeKind::Array);
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

// Function pointer tests

#[test]
fn test_function_pointer_declaration() {
    // Basic function pointer: void (*fp)(int)
    let (tu, types, strings, symbols) = parse_tu("void (*fp)(int);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "fp");
            // fp should be a pointer to a function
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
            // The base type of the pointer should be a function
            if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                assert_eq!(types.kind(base_id), TypeKind::Function);
                // Function returns void
                if let Some(ret_id) = types.base_type(base_id) {
                    assert_eq!(types.kind(ret_id), TypeKind::Void);
                }
                // Function takes one int parameter
                if let Some(params) = types.params(base_id) {
                    assert_eq!(params.len(), 1);
                    assert_eq!(types.kind(params[0]), TypeKind::Int);
                }
            } else {
                panic!("Expected function pointer base type");
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_pointer_no_params() {
    // Function pointer with no parameters: int (*fp)(void)
    let (tu, types, strings, symbols) = parse_tu("int (*fp)(void);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "fp");
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
            if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                assert_eq!(types.kind(base_id), TypeKind::Function);
                // (void) means no parameters
                if let Some(params) = types.params(base_id) {
                    assert!(params.is_empty());
                }
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_pointer_multiple_params() {
    // Function pointer with multiple parameters: int (*fp)(int, char, double)
    let (tu, types, strings, symbols) = parse_tu("int (*fp)(int, char, double);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "fp");
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
            if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                assert_eq!(types.kind(base_id), TypeKind::Function);
                if let Some(params) = types.params(base_id) {
                    assert_eq!(params.len(), 3);
                    assert_eq!(types.kind(params[0]), TypeKind::Int);
                    assert_eq!(types.kind(params[1]), TypeKind::Char);
                    assert_eq!(types.kind(params[2]), TypeKind::Double);
                }
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_pointer_returning_pointer() {
    // Function pointer returning a pointer: char *(*fp)(int)
    let (tu, types, strings, symbols) = parse_tu("char *(*fp)(int);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "fp");
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
            if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                assert_eq!(types.kind(base_id), TypeKind::Function);
                // Return type is char*
                if let Some(ret_id) = types.base_type(base_id) {
                    assert_eq!(types.kind(ret_id), TypeKind::Pointer);
                    if let Some(char_id) = types.base_type(ret_id) {
                        assert_eq!(types.kind(char_id), TypeKind::Char);
                    }
                }
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_pointer_variadic() {
    // Variadic function pointer: int (*fp)(const char *, ...)
    let (tu, types, strings, symbols) = parse_tu("int (*fp)(const char *, ...);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "fp");
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
            if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                assert_eq!(types.kind(base_id), TypeKind::Function);
                assert!(types.is_variadic(base_id), "Function should be variadic");
                if let Some(params) = types.params(base_id) {
                    assert_eq!(params.len(), 1);
                    assert_eq!(types.kind(params[0]), TypeKind::Pointer);
                }
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_returning_function_pointer() {
    // Function returning function pointer: int (*get_op(int which))(int, int)
    // This declares get_op as a function taking int and returning a pointer to
    // a function (int, int) -> int
    let (tu, types, strings, symbols) = parse_tu("int (*get_op(int which))(int, int);").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            check_name(
                &strings,
                symbols.get(decl.declarators[0].symbol).name,
                "get_op",
            );

            // get_op should be a Function type
            let get_op_type = decl.declarators[0].typ;
            assert_eq!(types.kind(get_op_type), TypeKind::Function);

            // get_op takes one int parameter
            if let Some(params) = types.params(get_op_type) {
                assert_eq!(params.len(), 1);
                assert_eq!(types.kind(params[0]), TypeKind::Int);
            } else {
                panic!("Expected function parameters");
            }

            // Return type should be a pointer to a function
            if let Some(ret_id) = types.base_type(get_op_type) {
                assert_eq!(types.kind(ret_id), TypeKind::Pointer);

                // The pointer's base should be a function
                if let Some(func_id) = types.base_type(ret_id) {
                    assert_eq!(types.kind(func_id), TypeKind::Function);

                    // The inner function returns int
                    if let Some(inner_ret_id) = types.base_type(func_id) {
                        assert_eq!(types.kind(inner_ret_id), TypeKind::Int);
                    } else {
                        panic!("Expected inner function return type");
                    }

                    // The inner function takes (int, int)
                    if let Some(inner_params) = types.params(func_id) {
                        assert_eq!(inner_params.len(), 2);
                        assert_eq!(types.kind(inner_params[0]), TypeKind::Int);
                        assert_eq!(types.kind(inner_params[1]), TypeKind::Int);
                    } else {
                        panic!("Expected inner function parameters");
                    }
                } else {
                    panic!("Expected function pointer base type");
                }
            } else {
                panic!("Expected return type");
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_function_pointer_returning_struct_pointer() {
    // Function pointer returning a struct pointer: struct node *(*fp)(int)
    // This declares fp as a pointer to a function (int) -> struct node*
    let (tu, types, _strings, _symbols) =
        parse_tu("struct node { int value; }; struct node *(*fp)(int);").unwrap();
    assert_eq!(tu.items.len(), 2);
    match &tu.items[1] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            // fp should be Pointer -> Function -> Pointer -> struct node
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);

            // Base type of fp should be Function (not Pointer!)
            let func_id = types.base_type(decl.declarators[0].typ).unwrap();
            assert_eq!(
                types.kind(func_id),
                TypeKind::Function,
                "Function pointer base type should be Function, not Pointer"
            );

            // Function return type should be Pointer
            let ret_id = types.base_type(func_id).unwrap();
            assert_eq!(
                types.kind(ret_id),
                TypeKind::Pointer,
                "Function return type should be Pointer"
            );

            // Pointer base type should be Struct
            let struct_id = types.base_type(ret_id).unwrap();
            assert_eq!(
                types.kind(struct_id),
                TypeKind::Struct,
                "Pointer base type should be Struct"
            );

            // Function should take one int parameter
            if let Some(params) = types.params(func_id) {
                assert_eq!(params.len(), 1);
                assert_eq!(types.kind(params[0]), TypeKind::Int);
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_pointer_to_array_of_pointers() {
    // Pointer to array of pointers: int *(*p)[3]
    // This declares p as a pointer to an array of 3 int pointers
    let (tu, types, _strings, _symbols) = parse_tu("int *(*p)[3];").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            // p should be Pointer -> Array -> Pointer -> int
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);

            // Base type of p should be Array
            let array_id = types.base_type(decl.declarators[0].typ).unwrap();
            assert_eq!(
                types.kind(array_id),
                TypeKind::Array,
                "Pointer base type should be Array"
            );

            // Array element type should be Pointer
            let elem_id = types.base_type(array_id).unwrap();
            assert_eq!(
                types.kind(elem_id),
                TypeKind::Pointer,
                "Array element type should be Pointer"
            );

            // Pointer base type should be Int
            let int_id = types.base_type(elem_id).unwrap();
            assert_eq!(
                types.kind(int_id),
                TypeKind::Int,
                "Pointer base type should be Int"
            );
        }
        _ => panic!("Expected Declaration"),
    }
}

// Bitfield tests

#[test]
fn test_bitfield_basic() {
    // Basic bitfield parsing - include a variable declarator
    let (tu, types, strings, symbols) =
        parse_tu("struct flags { unsigned int a : 4; unsigned int b : 4; } f;").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            check_name(&strings, symbols.get(decl.declarators[0].symbol).name, "f");
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Struct);
            if let Some(composite) = types.composite(decl.declarators[0].typ) {
                assert_eq!(composite.members.len(), 2);
                // First bitfield
                check_name(&strings, composite.members[0].name, "a");
                assert_eq!(composite.members[0].bit_width, Some(4));
                assert_eq!(composite.members[0].bit_offset, Some(0));
                // Second bitfield
                check_name(&strings, composite.members[1].name, "b");
                assert_eq!(composite.members[1].bit_width, Some(4));
                assert_eq!(composite.members[1].bit_offset, Some(4));
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_bitfield_unnamed() {
    // Unnamed bitfield for padding
    let (tu, types, strings, _symbols) =
        parse_tu("struct padded { unsigned int a : 4; unsigned int : 4; unsigned int b : 8; } p;")
            .unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            if let Some(composite) = types.composite(decl.declarators[0].typ) {
                assert_eq!(composite.members.len(), 3);
                // First named bitfield
                check_name(&strings, composite.members[0].name, "a");
                assert_eq!(composite.members[0].bit_width, Some(4));
                // Unnamed padding bitfield
                check_name(&strings, composite.members[1].name, "");
                assert_eq!(composite.members[1].bit_width, Some(4));
                // Second named bitfield
                check_name(&strings, composite.members[2].name, "b");
                assert_eq!(composite.members[2].bit_width, Some(8));
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_bitfield_zero_width() {
    // Zero-width bitfield forces alignment
    let (tu, types, strings, _symbols) =
        parse_tu("struct aligned { unsigned int a : 4; unsigned int : 0; unsigned int b : 4; } x;")
            .unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            if let Some(composite) = types.composite(decl.declarators[0].typ) {
                assert_eq!(composite.members.len(), 3);
                // After zero-width bitfield, b should start at new storage unit
                check_name(&strings, composite.members[2].name, "b");
                assert_eq!(composite.members[2].bit_width, Some(4));
                // b should be at offset 0 within its storage unit
                assert_eq!(composite.members[2].bit_offset, Some(0));
                // b's byte offset should be different from a's
                assert!(composite.members[2].offset > composite.members[0].offset);
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_bitfield_mixed_with_regular() {
    // Bitfield mixed with regular member
    let (tu, types, strings, _symbols) =
        parse_tu("struct mixed { int x; unsigned int bits : 8; int y; } m;").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            if let Some(composite) = types.composite(decl.declarators[0].typ) {
                assert_eq!(composite.members.len(), 3);
                // x is regular member
                check_name(&strings, composite.members[0].name, "x");
                assert!(composite.members[0].bit_width.is_none());
                // bits is a bitfield
                check_name(&strings, composite.members[1].name, "bits");
                assert_eq!(composite.members[1].bit_width, Some(8));
                // y is regular member
                check_name(&strings, composite.members[2].name, "y");
                assert!(composite.members[2].bit_width.is_none());
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_bitfield_struct_size() {
    // Verify struct size calculation with bitfields
    let (tu, types, _strings, _symbols) =
        parse_tu("struct small { unsigned int a : 1; unsigned int b : 1; unsigned int c : 1; } s;")
            .unwrap();
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            if let Some(composite) = types.composite(decl.declarators[0].typ) {
                // Three 1-bit fields should fit in one 4-byte int
                assert_eq!(composite.size, 4);
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_bitfield_zero_width_named_error() {
    // Zero-width bitfield with a name is an error
    let result = parse_tu("struct S { int x : 0; } s;");
    match result {
        Err(e) => {
            let err_msg = e.to_string();
            assert!(
                err_msg.contains("zero width"),
                "error message should mention zero width, got: {}",
                err_msg
            );
        }
        Ok(_) => panic!("named zero-width bitfield should be an error"),
    }
}

#[test]
fn test_bitfield_too_wide_error() {
    // Bitfield width exceeding type size is an error
    let result = parse_tu("struct S { int x : 64; } s;");
    match result {
        Err(e) => {
            let err_msg = e.to_string();
            assert!(
                err_msg.contains("exceeds"),
                "error message should mention exceeds, got: {}",
                err_msg
            );
        }
        Ok(_) => panic!("bitfield width exceeding type should be an error"),
    }
}

// Enum tests

/// An empty enumerator list is an error, as in gcc and clang: C17
/// 6.7.2.2p1 does not make the list optional.
#[test]
fn test_empty_enum_is_an_error() {
    for src in ["enum E {};", "enum __attribute__((packed)) E {};"] {
        let before = crate::diag::error_count();
        let (_tu, _types, _strings, _symbols) = parse_tu(src).unwrap();
        // The count is process-wide and only grows, so a concurrent test can
        // add to it but never hide this one's error.
        assert!(crate::diag::error_count() > before, "{src}: accepted");
    }
}

/// The declared variable's enum type, its size and alignment, and the
/// integer type it is compatible with.
fn enum_of_variable(src: &str) -> (usize, usize, Option<TypeId>, TypeTable) {
    let (tu, types, _strings, _symbols) = parse_tu(src).unwrap();
    let ExternalDecl::Declaration(ref decl) = tu.items.last().unwrap() else {
        panic!("{src}: expected a declaration");
    };
    let typ = decl.declarators[0].typ;
    let compat = types.enum_compatible_type(typ);
    (types.size_bytes(typ), types.alignment(typ), compat, types)
}

/// `packed` makes an enum the smallest integer type holding its members,
/// signed iff a member is negative, as gcc does; without it the search
/// starts at `int`.
#[test]
fn test_packed_enum_underlying_type() {
    use crate::target::IntType;
    for (body, size, int) in [
        ("{ A }", 1, IntType::UChar),
        ("{ A = 255 }", 1, IntType::UChar),
        ("{ A = 256 }", 2, IntType::UShort),
        ("{ A = -128, B = 127 }", 1, IntType::SChar),
        ("{ A = -129 }", 2, IntType::Short),
        ("{ A = 65535 }", 2, IntType::UShort),
        ("{ A = 65536 }", 4, IntType::UInt),
        ("{ A = -32769 }", 4, IntType::Int),
        ("{ A = 0x100000000 }", 8, IntType::ULong),
        ("{ A = -0x100000000 }", 8, IntType::Long),
    ] {
        let src = format!("enum __attribute__((packed)) E {body} x;");
        let (got_size, align, compat, types) = enum_of_variable(&src);
        assert_eq!((got_size, align), (size, size), "{src}");
        assert_eq!(compat, Some(types.int_type_id(int)), "{src}");
        let plain = format!("enum E {body} x;");
        let (plain_size, _, _, _) = enum_of_variable(&plain);
        assert_eq!(plain_size, size.max(4), "{plain}");
    }
}

/// gcc reads an enum's `packed` between `enum` and the tag and after the
/// closing brace, tagged or not; on a declaration that is not a definition
/// it means nothing, and `aligned` on an enum is ignored.
#[test]
fn test_packed_enum_attribute_positions() {
    for (src, size) in [
        ("enum __attribute__((packed)) E { A } x;", 1),
        ("enum __attribute__((__packed__)) { A } x;", 1),
        ("enum E { A } __attribute__((packed)) x;", 1),
        ("enum { A } __attribute__((packed)) x;", 1),
        ("enum E; enum __attribute__((packed)) E { A } x;", 1),
        ("enum __attribute__((packed)) E; enum E { A } x;", 4),
        ("enum __attribute__((aligned(8))) E { A } x;", 4),
        ("enum E { A } __attribute__((aligned(8))) x;", 4),
    ] {
        let (got_size, align, _, _) = enum_of_variable(src);
        assert_eq!((got_size, align), (size, size), "{src}");
    }
}

/// An enumeration constant stays `int` in a `packed` enum narrower than
/// `int`, and takes the enumeration's type in one wider than `int`: gcc's
/// `sizeof(A)` is 4 for the first and 8 for every member of the second.
#[test]
fn test_enumerator_type_follows_enum_width() {
    for (src, size) in [
        ("enum __attribute__((packed)) E { A }; int x[sizeof(A)];", 4),
        ("enum E { B = -1, A = 0x100000000 }; int x[sizeof(B)];", 8),
        (
            "enum __attribute__((packed)) E { B = -1, A = 0x100000000 }; int x[sizeof(B)];",
            8,
        ),
        (
            "enum E { A = 0x80000000u }; int x[sizeof(A) + (A > 0 ? 0 : 99)];",
            4,
        ),
    ] {
        let (got_size, _, _, _) = enum_of_variable(src);
        assert_eq!(got_size, 4 * size, "{src}");
    }
}

// Typedef with trailing qualifiers tests (C99)
// Tests for "typedef_name const" and "typedef_name volatile" syntax

#[test]
fn test_typedef_trailing_const() {
    // Test that "typedef_name const" is parsed correctly
    // This pattern is used in real code like: z_word_t const *ptr
    let (tu, types, _strings, _symbols) =
        parse_tu("typedef unsigned long mytype; mytype const x;").unwrap();
    assert_eq!(tu.items.len(), 2);

    // Second item should be the declaration of x
    match &tu.items[1] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            let typ = decl.declarators[0].typ;
            // The type should have CONST modifier
            assert!(
                types.modifiers(typ).contains(TypeModifiers::CONST),
                "Type should have CONST modifier"
            );
            // Base type should still be unsigned long
            assert_eq!(types.kind(typ), TypeKind::Long);
            assert!(types.modifiers(typ).contains(TypeModifiers::UNSIGNED));
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_typedef_trailing_volatile() {
    // Test that "typedef_name volatile" is parsed correctly
    let (tu, types, _strings, _symbols) = parse_tu("typedef int myint; myint volatile v;").unwrap();
    assert_eq!(tu.items.len(), 2);

    match &tu.items[1] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            let typ = decl.declarators[0].typ;
            assert!(
                types.modifiers(typ).contains(TypeModifiers::VOLATILE),
                "Type should have VOLATILE modifier"
            );
            assert_eq!(types.kind(typ), TypeKind::Int);
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_typedef_trailing_const_volatile() {
    // Test both const and volatile after typedef name
    let (tu, types, _strings, _symbols) =
        parse_tu("typedef int myint; myint const volatile cv;").unwrap();
    assert_eq!(tu.items.len(), 2);

    match &tu.items[1] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            let typ = decl.declarators[0].typ;
            assert!(
                types.modifiers(typ).contains(TypeModifiers::CONST),
                "Type should have CONST modifier"
            );
            assert!(
                types.modifiers(typ).contains(TypeModifiers::VOLATILE),
                "Type should have VOLATILE modifier"
            );
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_typedef_trailing_const_pointer() {
    // Test "typedef_name const *ptr" pattern
    // This is common in real code: z_word_t const *ptr
    let (tu, types, _strings, _symbols) =
        parse_tu("typedef unsigned long word_t; word_t const *ptr;").unwrap();
    assert_eq!(tu.items.len(), 2);

    match &tu.items[1] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            let ptr_typ = decl.declarators[0].typ;
            // Should be a pointer
            assert_eq!(types.kind(ptr_typ), TypeKind::Pointer);
            // The base type (what pointer points to) should be const
            if let Some(base) = types.base_type(ptr_typ) {
                assert!(
                    types.modifiers(base).contains(TypeModifiers::CONST),
                    "Pointee type should have CONST modifier"
                );
                assert!(types.modifiers(base).contains(TypeModifiers::UNSIGNED));
            }
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_forward_declared_struct_member_access() {
    // The incomplete struct type should be resolved to the complete type
    // when the member access is performed.
    let (tu, types, _strings, _symbols) =
        parse_tu("struct S { int x; }; void f(struct S *p) { p->x; }").unwrap();
    assert_eq!(tu.items.len(), 2);

    // Verify the function body parsed correctly
    match &tu.items[1] {
        ExternalDecl::FunctionDef(func) => {
            // The function should have parsed successfully
            assert_eq!(types.kind(func.return_type), TypeKind::Void);
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_forward_declared_struct_via_pointer_param() {
    // More complex case: struct declared after function, used via pointer
    // The definition completes the type the parameter already named
    let (tu, _types, _strings, _symbols) = parse_tu(
        "struct Node; \
         int get_val(struct Node *n); \
         struct Node { int val; struct Node *next; }; \
         int get_val(struct Node *n) { return n->val; }",
    )
    .unwrap();
    // 4 items: forward decl, prototype, struct def, function def
    assert_eq!(tu.items.len(), 4);
}

#[test]
fn test_function_pointer_call() {
    // Regression test: function pointer calls should return the correct type
    let (tu, types, _strings, _symbols) = parse_tu(
        "int (*fp)(int); \
         int test(void) { return fp(42); }",
    )
    .unwrap();
    assert_eq!(tu.items.len(), 2);

    // Verify the function return type is int (from fp(42) call)
    match &tu.items[1] {
        ExternalDecl::FunctionDef(func) => {
            assert_eq!(types.kind(func.return_type), TypeKind::Int);
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_function_pointer_in_struct_call() {
    // Test calling function pointer stored in struct member
    let (tu, _types, _strings, _symbols) = parse_tu(
        "struct Ops { int (*handler)(int); }; \
         int call_handler(struct Ops *ops, int x) { return ops->handler(x); }",
    )
    .unwrap();
    assert_eq!(tu.items.len(), 2);
}

#[test]
fn test_typedef_function_pointer_call() {
    // Test typedef'd function pointer call
    let (tu, _types, _strings, _symbols) = parse_tu(
        "typedef int (*callback_t)(int); \
         callback_t cb; \
         int test(void) { return cb(10); }",
    )
    .unwrap();
    assert_eq!(tu.items.len(), 3);
}

/// A call's value has its callee's return type, unqualified, however the
/// callee is reached: a function, a pointer to one -- a variable, a typedef,
/// a member, a call returning one -- or the explicit `(*p)` form.
#[test]
fn test_call_type_is_the_callee_return_type() {
    let decls = "const short f(int); \
                 const short (*p)(int); \
                 typedef const short (*F)(int); F q; \
                 struct S { F m; } s; \
                 F get(void);";
    for call in ["f(1)", "p(1)", "(*p)(1)", "q(1)", "s.m(1)", "get()(1)"] {
        let src = format!("{decls} void g(void) {{ {call}; }}");
        let (tu, types, _, _) = parse_tu(&src).unwrap();
        let Stmt::Expr(e) = first_statement_of(&tu, 0) else {
            panic!("{call}: expected an expression statement");
        };
        assert!(matches!(e.kind, ExprKind::Call { .. }), "{call}");
        let typ = e.typ.expect("typed");
        assert_eq!(types.kind(typ), TypeKind::Short, "{call}");
        assert!(
            !types.modifiers(typ).contains(TypeModifiers::CONST),
            "{call}: the value is unqualified"
        );
    }
}

// Designated Initializer Edge Cases

#[test]
fn test_nested_field_designator() {
    // Test .x.y = 1 nested field designator
    let code = "struct Inner { int y; }; struct S { struct Inner x; }; struct S s = {.x.y = 1};";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 3);
}

#[test]
fn test_mixed_array_field_designator() {
    // Test .arr[0].field = 2 mixed designator
    let code = "struct T { int field; }; struct S { struct T arr[10]; }; struct S s = {.arr[0].field = 2};";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 3);
}

#[test]
fn test_out_of_order_array_designator() {
    // Test [5] = 1, [0] = 2 out-of-order designators
    let code = "int arr[] = {[5] = 1, [0] = 2};";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
}

#[test]
fn test_repeated_designator() {
    // Same index twice - last wins per C99
    let code = "int arr[] = {[0] = 1, [0] = 2};";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
}

// Complex Declarator Edge Cases

#[test]
fn test_function_returning_ptr_to_array() {
    // int (*foo())[10] - function returning pointer to array
    let code = "int (*foo())[10];";
    let (tu, types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            let typ = decl.declarators[0].typ;
            // Should be a function type
            assert_eq!(types.kind(typ), TypeKind::Function);
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_array_of_function_pointers() {
    // int (*arr[5])(int) - array of 5 function pointers
    let code = "int (*arr[5])(int);";
    let (tu, types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            let typ = decl.declarators[0].typ;
            // Should be an array type
            assert_eq!(types.kind(typ), TypeKind::Array);
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_complex_nested_declarator() {
    // int *(*(*fp)(int))[3] - ptr to func returning ptr to array of 3 int*
    let code = "int *(*(*fp)(int))[3];";
    let (tu, types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            let typ = decl.declarators[0].typ;
            // Should be a pointer type
            assert_eq!(types.kind(typ), TypeKind::Pointer);
        }
        _ => panic!("Expected Declaration"),
    }
}

/// C17 6.7.6: `direct-declarator` is `( declarator )` *recursively*, and
/// 5.2.4.1 requires 63 levels of it. Only one level used to parse: the
/// predicate that decides whether a `(` opens a grouped declarator or a
/// parameter list looked for `*` or an identifier, and a nested `(` is
/// neither, so `int ((q));` was rejected as "expected identifier".
#[test]
fn test_nested_parenthesized_declarators() {
    for (code, kind) in [
        ("int ((q));", TypeKind::Int),
        ("int ((*p));", TypeKind::Pointer),
        ("int (((*h)));", TypeKind::Pointer),
        ("int ((((((*deep))))));", TypeKind::Pointer),
    ] {
        let (tu, types, _strings, _symbols) = parse_tu(code).unwrap_or_else(|e| {
            panic!("{code} should parse: {e:?}");
        });
        assert_eq!(tu.items.len(), 1, "{code}");
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1, "{code}");
                assert_eq!(
                    types.kind(decl.declarators[0].typ),
                    kind,
                    "{code}: redundant parentheses must not change the type"
                );
            }
            other => panic!("{code}: expected Declaration, got {other:?}"),
        }
    }
}

/// A function declarator wrapped in redundant parentheses is still a function,
/// and the parentheses may nest.
#[test]
fn test_nested_parenthesized_function_declarator() {
    for code in [
        "void (g)(void);",
        "void ((g))(void);",
        "void (((g)))(void);",
    ] {
        let (tu, _types, _strings, _symbols) =
            parse_tu(code).unwrap_or_else(|e| panic!("{code} should parse: {e:?}"));
        assert_eq!(tu.items.len(), 1, "{code}");
    }
}

/// A parameter whose type is a *function* type, spelled without a name:
/// `int (size_t)` is a function taking `size_t`, adjusted to a pointer by
/// 6.7.6.3p8. The old inline predicate treated any identifier after `(` as a
/// grouped declarator, so this was read as a declarator named `size_t`.
#[test]
fn test_function_type_parameter_is_not_a_grouped_declarator() {
    let code = "typedef unsigned long size_t;\nvoid f(int (size_t));";
    let (tu, _types, _strings, _symbols) =
        parse_tu(code).unwrap_or_else(|e| panic!("{code} should parse: {e:?}"));
    assert_eq!(tu.items.len(), 2);
}

/// A type-name's abstract declarator (C17 6.7.7) is the ordinary declarator
/// grammar minus the identifier, so it goes through `parse_declarator`. It
/// used to have a parser of its own that knew one shape, `(*)(params)`, which
/// is why a pointer to an array could be declared but not named.
#[test]
fn test_abstract_declarators_in_type_names() {
    for src in [
        "sizeof(int (*)[3])",
        "sizeof(int (**)(void))",
        "sizeof(int (*[4])(void))",
        "sizeof(int ((*))(void))",
        "sizeof(int (*)(const char *))",
        "sizeof(int[3][4])",
        "_Generic(1, int: 11, double: 22, default: 33)",
    ] {
        let (expr, _types, _strings, _symbols) =
            parse_expr(src).unwrap_or_else(|e| panic!("{src} should parse: {e:?}"));
        assert!(
            expr.typ.is_some(),
            "{src} should have produced a typed expression"
        );
    }
}

// Array Parameter Edge Cases

#[test]
fn test_static_array_parameter() {
    // int a[static 10] - minimum size guarantee
    let code = "void foo(int a[static 10]);";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
}

#[test]
fn test_qualified_array_parameter() {
    // int a[const 10] - qualifier in array size
    let code = "void foo(int a[const 10]);";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
}

#[test]
fn test_vla_star_parameter() {
    // int a[*] - unspecified VLA size in prototype
    let code = "void foo(int n, int a[*]);";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
}

// Type Parsing Edge Cases

#[test]
fn test_multiple_type_qualifiers() {
    let code = "const volatile int x;";
    let (tu, types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            let typ = decl.declarators[0].typ;
            assert!(types.modifiers(typ).contains(TypeModifiers::CONST));
            assert!(types.modifiers(typ).contains(TypeModifiers::VOLATILE));
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_restrict_pointer() {
    let code = "int * restrict p;";
    let (tu, types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            let typ = decl.declarators[0].typ;
            assert_eq!(types.kind(typ), TypeKind::Pointer);
            assert!(types.modifiers(typ).contains(TypeModifiers::RESTRICT));
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_inline_function() {
    let code = "inline int foo(void) { return 0; }";
    let (tu, types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::FunctionDef(func) => {
            // The INLINE modifier is stored on the return type
            assert!(types
                .modifiers(func.return_type)
                .contains(TypeModifiers::INLINE));
        }
        _ => panic!("Expected FunctionDef"),
    }
}

// Escape Sequence Edge Cases

#[test]
fn test_octal_escape_boundary() {
    // \377 is max valid octal for 8-bit char
    let (expr, _types, _strings, _symbols) = parse_expr("'\\377'").unwrap();
    match expr.kind {
        // The byte the escape denotes. Not `ch` itself: an unprefixed
        // constant takes plain `char`'s signedness, so this is -1 on a
        // signed-`char` host and 255 on an unsigned one, and these tests
        // run on both.
        ExprKind::CharLit(ch) => {
            assert_eq!(ch as u8, 0o377); // 255 in decimal
        }
        _ => panic!("Expected character constant, got {:?}", expr.kind),
    }
}

/// C17 6.4.4.4p10: `'\x80'` has the value of a plain `char` holding 0x80,
/// so it follows the target's `char` signedness -- which is per OS as well as
/// per architecture: Apple arm64 is signed where AAPCS64 is unsigned. A
/// prefixed constant is the code point on every target.
#[test]
fn test_char_constant_value_follows_target_signedness() {
    use crate::target::{Arch, Os};
    for (arch, os, want) in [
        (Arch::X86_64, Os::Linux, -128),
        (Arch::X86_64, Os::MacOS, -128),
        (Arch::Aarch64, Os::Linux, 128),
        (Arch::Aarch64, Os::MacOS, -128),
    ] {
        for (src, expected) in [("'\\x80'", want), ("L'\\x80'", 128)] {
            let mut strings = StringTable::new();
            let mut tokenizer = Tokenizer::new(src.as_bytes(), 0, &mut strings);
            let tokens = tokenizer.tokenize();
            let mut symbols = SymbolTable::new();
            let mut types = TypeTable::new(&Target::new(arch, os));
            let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
            parser.skip_stream_tokens();
            let expr = parser.parse_expression().unwrap();
            let value = match expr.kind {
                ExprKind::CharLit(v) => v,
                ExprKind::IntLit(v) => v,
                other => panic!("{src}: expected a character constant, got {other:?}"),
            };
            assert_eq!(value, expected, "{src} on {arch}-{os}");
        }
    }
}

#[test]
fn test_hex_escape_single_digit() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\x0'").unwrap();
    match expr.kind {
        ExprKind::CharLit(ch) => {
            assert_eq!(ch as u8, 0);
        }
        _ => panic!("Expected character constant, got {:?}", expr.kind),
    }
}

#[test]
fn test_hex_escape_max() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\xff'").unwrap();
    match expr.kind {
        ExprKind::CharLit(ch) => {
            assert_eq!(ch as u8, 0xff);
        }
        _ => panic!("Expected character constant, got {:?}", expr.kind),
    }
}

#[test]
fn test_escape_sequences_comprehensive() {
    // Test all standard escape sequences in a string
    let (expr, _types, _strings, _symbols) = parse_expr(r#""\a\b\f\n\r\t\v\\\'\"" "#).unwrap();
    match expr.kind {
        ExprKind::StringLit(_) => {
            // Successfully parsed string with all escape sequences
        }
        _ => panic!("Expected string constant"),
    }
}

// Expression Edge Cases

#[test]
fn test_comma_expression_in_parens() {
    // Comma expression inside parentheses
    let (expr, _types, _strings, _symbols) = parse_expr("(1, 2, 3)").unwrap();
    match &expr.kind {
        ExprKind::Comma(exprs) => {
            assert_eq!(exprs.len(), 3);
        }
        _ => panic!("Expected comma expression"),
    }
}

#[test]
fn test_ternary_expression_nesting() {
    // Nested ternary expressions
    let (expr, _types, _strings, _symbols) = parse_expr("a ? b ? c : d : e").unwrap();
    match &expr.kind {
        ExprKind::Conditional { then_expr, .. } => {
            // The middle expression should also be ternary
            match &then_expr.kind {
                ExprKind::Conditional { .. } => {}
                _ => panic!("Expected nested ternary"),
            }
        }
        _ => panic!("Expected ternary expression"),
    }
}

#[test]
fn test_cast_expression_chain() {
    // Multiple cast expressions
    let (expr, types, _strings, _symbols) = parse_expr("(int)(char)(short)x").unwrap();
    match &expr.kind {
        ExprKind::Cast {
            cast_type,
            expr: inner,
        } => {
            assert_eq!(types.kind(*cast_type), TypeKind::Int);
            match &inner.kind {
                ExprKind::Cast {
                    cast_type: typ2,
                    expr: inner2,
                } => {
                    assert_eq!(types.kind(*typ2), TypeKind::Char);
                    match &inner2.kind {
                        ExprKind::Cast {
                            cast_type: typ3, ..
                        } => {
                            assert_eq!(types.kind(*typ3), TypeKind::Short);
                        }
                        _ => panic!("Expected cast expression"),
                    }
                }
                _ => panic!("Expected cast expression"),
            }
        }
        _ => panic!("Expected cast expression"),
    }
}

#[test]
fn test_sizeof_expression_vs_type() {
    // sizeof applied to expression
    let (expr1, _types, _strings, _symbols) = parse_expr("sizeof x").unwrap();
    match &expr1.kind {
        ExprKind::SizeofExpr(_) => {}
        _ => panic!("Expected sizeof expression"),
    }

    // sizeof applied to type in parentheses
    let (expr2, _types, _strings, _symbols) = parse_expr("sizeof(int)").unwrap();
    match &expr2.kind {
        ExprKind::SizeofType(..) => {}
        _ => panic!("Expected sizeof type"),
    }
}

#[test]
fn test_compound_literal_in_expression() {
    // Compound literal used in expression
    let (expr, _types, _strings, _symbols) = parse_expr("(struct { int x; }){.x = 5}.x").unwrap();
    match &expr.kind {
        ExprKind::Member { .. } => {}
        _ => panic!("Expected member access on compound literal"),
    }
}

/// A GNU cast to union is the compound literal `(U){ .member = operand }`
/// for the member whose type the operand has, inside a cast to `U` that
/// keeps the result an rvalue. Converting the operand to the union's width
/// stored a `double` operand as integer bits.
#[test]
fn test_cast_to_union_initializes_the_matching_member() {
    let (expr, types, strings, _symbols) = parse_expr("(union { long l; double d; })1.5").unwrap();
    let ExprKind::Cast { cast_type, expr } = &expr.kind else {
        panic!("expected a cast, got {:?}", expr.kind);
    };
    assert_eq!(types.kind(*cast_type), TypeKind::Union);
    let ExprKind::CompoundLiteral { typ, elements } = &expr.kind else {
        panic!("expected a compound literal, got {:?}", expr.kind);
    };
    assert_eq!(typ, cast_type);
    assert_eq!(elements.len(), 1);
    match elements[0].designators.as_slice() {
        [Designator::Field(name)] => assert_eq!(strings.get(*name), "d"),
        other => panic!("expected .d, got {other:?}"),
    }
    assert_eq!(elements[0].value.typ, Some(types.double_id));
}

// __builtin_offsetof tests

#[test]
fn test_offsetof_basic() {
    // Simple __builtin_offsetof(type, member)
    let code = "struct S { int a; int b; }; unsigned long x = __builtin_offsetof(struct S, b);";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 2);
}

#[test]
fn test_offsetof_nested_member() {
    // __builtin_offsetof with nested member path
    let code = "struct Inner { int x; }; struct Outer { struct Inner i; }; \
         unsigned long x = __builtin_offsetof(struct Outer, i.x);";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 3);
}

#[test]
fn test_offsetof_array_index() {
    // __builtin_offsetof with array index
    let code = "struct S { int arr[10]; }; unsigned long x = __builtin_offsetof(struct S, arr[5]);";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 2);
}

#[test]
fn test_offsetof_macro_style() {
    // offsetof (macro-compatible spelling)
    let code = "struct S { int a; int b; }; unsigned long x = offsetof(struct S, b);";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 2);
}

// Statement expression tests (GNU extension)

#[test]
fn test_stmt_expr_basic() {
    // Basic statement expression
    let code = "int x = ({ int a = 5; a + 3; });";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
}

#[test]
fn test_stmt_expr_with_declarations() {
    // Statement expression with multiple declarations
    let code = "int result = ({ int a = 1; int b = 2; a + b; });";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
}

#[test]
fn test_stmt_expr_nested() {
    // Nested statement expressions
    let code = "int x = ({ int a = ({ int inner = 10; inner * 2; }); a + 5; });";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
}

#[test]
fn test_stmt_expr_in_function() {
    // Statement expression inside a function
    let code = "int foo(void) { return ({ int x = 1; x + 1; }); }";
    let (tu, _types, _strings, _symbols) = parse_tu(code).unwrap();
    assert_eq!(tu.items.len(), 1);
}

// Integer literal type promotion tests (C99 6.4.4.1)

#[test]
fn test_hex_literal_type_promotion() {
    // 0x7FFFFFFF fits in int (signed)
    let (expr, types, _, _) = parse_expr("0x7FFFFFFF").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Int);
    assert!(!types.is_unsigned(expr.typ.unwrap()));

    // 0x80000000 doesn't fit in int, should be unsigned int
    let (expr, types, _, _) = parse_expr("0x80000000").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Int);
    assert!(types.is_unsigned(expr.typ.unwrap()));

    // 0xFFFFFFFF is max u32, should be unsigned int
    let (expr, types, _, _) = parse_expr("0xFFFFFFFF").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Int);
    assert!(types.is_unsigned(expr.typ.unwrap()));

    // 0x100000000 doesn't fit in u32, should be long (signed)
    let (expr, types, _, _) = parse_expr("0x100000000").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Long);
    assert!(!types.is_unsigned(expr.typ.unwrap()));
}

#[test]
fn test_octal_literal_type_promotion() {
    // 017777777777 = 0x7FFFFFFF = 2147483647 (fits in int)
    let (expr, types, _, _) = parse_expr("017777777777").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Int);
    assert!(!types.is_unsigned(expr.typ.unwrap()));

    // 020000000000 = 0x80000000 = 2147483648 (doesn't fit in int, use uint)
    let (expr, types, _, _) = parse_expr("020000000000").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Int);
    assert!(types.is_unsigned(expr.typ.unwrap()));
}

#[test]
fn test_decimal_literal_stays_signed() {
    // 2147483647 (INT_MAX) fits in int
    let (expr, types, _, _) = parse_expr("2147483647").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Int);
    assert!(!types.is_unsigned(expr.typ.unwrap()));

    // 2147483648 (INT_MAX + 1) doesn't fit in int, should be long (NOT uint)
    let (expr, types, _, _) = parse_expr("2147483648").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Long);
    assert!(
        !types.is_unsigned(expr.typ.unwrap()),
        "Decimal literal should remain signed"
    );

    // 9223372036854775807 (i64::MAX) fits in long on 64-bit systems
    // On 64-bit systems, long is 64 bits, so this fits in long (not long long)
    let (expr, types, _, _) = parse_expr("9223372036854775807").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Long);
    assert!(!types.is_unsigned(expr.typ.unwrap()));
}

// Incomplete array size from string literal tests

#[test]
fn test_incomplete_array_string_literal_size() {
    // char arr[] = "abc"; should infer size 4 (3 chars + null)
    let (tu, types, _, _) = parse_tu("char arr[] = \"abc\";").unwrap();
    assert_eq!(tu.items.len(), 1);

    // Get the declarator and check its type
    if let ExternalDecl::Declaration(decl) = &tu.items[0] {
        let typ = decl.declarators[0].typ;
        assert_eq!(types.kind(typ), TypeKind::Array);
        assert_eq!(
            types.get(typ).array_size,
            Some(4),
            "Array size should be 4 (3 chars + null terminator)"
        );
    } else {
        panic!("Expected Declaration");
    }
}

#[test]
fn test_incomplete_array_empty_string() {
    // char arr[] = ""; should infer size 1 (just null terminator)
    let (tu, types, _, _) = parse_tu("char arr[] = \"\";").unwrap();

    if let ExternalDecl::Declaration(decl) = &tu.items[0] {
        let typ = decl.declarators[0].typ;
        assert_eq!(types.kind(typ), TypeKind::Array);
        assert_eq!(
            types.get(typ).array_size,
            Some(1),
            "Array size should be 1 (just null terminator)"
        );
    } else {
        panic!("Expected Declaration");
    }
}

#[test]
fn test_incomplete_array_designator_size() {
    // int arr[] = {[10] = 1}; should infer size 11
    let (tu, types, _, _) = parse_tu("int arr[] = {[10] = 1};").unwrap();
    if let ExternalDecl::Declaration(decl) = &tu.items[0] {
        let typ = decl.declarators[0].typ;
        assert_eq!(types.kind(typ), TypeKind::Array);
        assert_eq!(
            types.get(typ).array_size,
            Some(11),
            "Array size should be 11 for {{[10] = 1}}"
        );
    } else {
        panic!("Expected Declaration");
    }
}

#[test]
fn test_incomplete_array_designator_sequence_size() {
    // int arr[] = {1, 2, [5] = 5, 6}; should infer size 7
    let (tu, types, _, _) = parse_tu("int arr[] = {1, 2, [5] = 5, 6};").unwrap();
    if let ExternalDecl::Declaration(decl) = &tu.items[0] {
        let typ = decl.declarators[0].typ;
        assert_eq!(types.kind(typ), TypeKind::Array);
        assert_eq!(
            types.get(typ).array_size,
            Some(7),
            "Array size should be 7 for {{1,2,[5]=5,6}}"
        );
    } else {
        panic!("Expected Declaration");
    }
}

// typeof operator tests (GCC extension)

#[test]
fn test_typeof_with_type() {
    // typeof(int) should be int
    let (tu, types, _, _) = parse_tu("typeof(int) x;").unwrap();
    assert_eq!(tu.items.len(), 1);
    if let ExternalDecl::Declaration(decl) = &tu.items[0] {
        let typ = decl.declarators[0].typ;
        assert_eq!(types.kind(typ), TypeKind::Int);
    } else {
        panic!("Expected Declaration");
    }
}

#[test]
fn test_typeof_with_expression() {
    // typeof(42) should be int (type of the expression)
    let (tu, types, _, _) = parse_tu("int n = 42; typeof(n) x;").unwrap();
    assert_eq!(tu.items.len(), 2);
    if let ExternalDecl::Declaration(decl) = &tu.items[1] {
        let typ = decl.declarators[0].typ;
        assert_eq!(types.kind(typ), TypeKind::Int);
    } else {
        panic!("Expected Declaration");
    }
}

#[test]
fn test_typeof_pointer() {
    // typeof(int) * should be pointer to int
    let (tu, types, _, _) = parse_tu("typeof(int) *p;").unwrap();
    assert_eq!(tu.items.len(), 1);
    if let ExternalDecl::Declaration(decl) = &tu.items[0] {
        let typ = decl.declarators[0].typ;
        assert_eq!(types.kind(typ), TypeKind::Pointer);
        let pointee = types.get(typ).base.unwrap();
        assert_eq!(types.kind(pointee), TypeKind::Int);
    } else {
        panic!("Expected Declaration");
    }
}

#[test]
fn test_dunder_typeof() {
    // __typeof__ is an alias for typeof
    let (tu, types, _, _) = parse_tu("__typeof__(long) x;").unwrap();
    assert_eq!(tu.items.len(), 1);
    if let ExternalDecl::Declaration(decl) = &tu.items[0] {
        let typ = decl.declarators[0].typ;
        assert_eq!(types.kind(typ), TypeKind::Long);
    } else {
        panic!("Expected Declaration");
    }
}

// Anonymous struct/union member tests (C11)

#[test]
fn test_anonymous_union_in_struct() {
    // Anonymous union inside struct - this should parse without error
    // The key test is that "union { ... };" without a name is accepted
    let result = parse_tu("struct foo { int x; union { int a; float b; }; int y; } s;");
    assert!(result.is_ok(), "Anonymous union in struct should parse");
}

#[test]
fn test_anonymous_struct_in_union() {
    // Anonymous struct inside union - this should parse without error
    let result = parse_tu("union bar { int i; struct { short x; short y; }; } u;");
    assert!(result.is_ok(), "Anonymous struct in union should parse");
}

// Statement expression void type tests

#[test]
fn test_stmt_expr_void_when_last_is_if() {
    // When last statement is if (not expression), result is void
    let (expr, types, _, _) = parse_expr("({ if (1) ; })").unwrap();
    match expr.kind {
        ExprKind::StmtExpr { .. } => {
            // The expression should have void type
            assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Void);
        }
        _ => panic!("Expected StmtExpr"),
    }
}

#[test]
fn test_stmt_expr_empty_is_void() {
    // Empty statement expression ({ }) has void type
    let (expr, types, _, _) = parse_expr("({ })").unwrap();
    match expr.kind {
        ExprKind::StmtExpr { .. } => {
            assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Void);
        }
        _ => panic!("Expected StmtExpr"),
    }
}

// Wide string literal tests

#[test]
fn test_wide_string_literal_basic() {
    // L"hello" should parse as WideStringLit
    let (expr, types, _, _) = parse_expr("L\"hello\"").unwrap();
    match &expr.kind {
        ExprKind::WideStringLit(s) => {
            assert_eq!(s, &"hello".chars().map(u32::from).collect::<Vec<_>>());
        }
        _ => panic!("Expected WideStringLit, got {:?}", expr.kind),
    }
    // Type should be wchar_t[N], not wchar_t*
    // C11 6.4.5: "wide string literal has type wchar_t[N]"
    let typ = expr.typ.unwrap();
    assert_eq!(types.kind(typ), TypeKind::Array);
    let elem_type = types.get(typ).base.unwrap();
    assert_eq!(elem_type, types.wchar_id);
    // Array size should be 6 (5 chars + null terminator)
    assert_eq!(types.array_size(typ), Some(6));
}

#[test]
fn test_wide_string_literal_concatenation() {
    // Adjacent wide string literals should concatenate
    let (expr, _, _, _) = parse_expr("L\"hello\" L\" world\"").unwrap();
    match &expr.kind {
        ExprKind::WideStringLit(s) => {
            assert_eq!(s, &"hello world".chars().map(u32::from).collect::<Vec<_>>());
        }
        _ => panic!("Expected WideStringLit, got {:?}", expr.kind),
    }
}

#[test]
fn test_wide_string_array_size_inference() {
    // int arr[] = L"abc"; should infer size 4 (3 chars + null)
    let (tu, types, _, _) = parse_tu("int arr[] = L\"abc\";").unwrap();
    assert_eq!(tu.items.len(), 1);

    if let ExternalDecl::Declaration(decl) = &tu.items[0] {
        let typ = decl.declarators[0].typ;
        assert_eq!(types.kind(typ), TypeKind::Array);
        assert_eq!(
            types.get(typ).array_size,
            Some(4),
            "Wide string array size should be 4 (3 chars + null terminator)"
        );
    } else {
        panic!("Expected Declaration");
    }
}

// Function parameter adjustment tests

#[test]
fn test_function_param_adjusted_to_pointer() {
    // Function parameters with function type should be adjusted to pointers
    // void foo(int fn(int)) should become void foo(int (*fn)(int))
    let (tu, types, _, _) = parse_tu("void foo(int fn(int));").unwrap();

    if let ExternalDecl::Declaration(decl) = &tu.items[0] {
        let func_typ = decl.declarators[0].typ;
        assert_eq!(types.kind(func_typ), TypeKind::Function);

        // Get function parameters
        if let Some(params) = &types.get(func_typ).params {
            assert_eq!(params.len(), 1);
            let param_typ = params[0];
            // The parameter should be a pointer to function, not function itself
            assert_eq!(
                types.kind(param_typ),
                TypeKind::Pointer,
                "Function parameter should be adjusted to pointer"
            );
            // The pointee should be a function type
            let pointee = types.get(param_typ).base.unwrap();
            assert_eq!(types.kind(pointee), TypeKind::Function);
        } else {
            panic!("Expected function type with params");
        }
    } else {
        panic!("Expected Declaration");
    }
}

#[test]
fn test_function_param_array_adjusted_to_pointer() {
    // Array parameters are adjusted to pointers (existing behavior, ensure not broken)
    let (tu, types, _, _) = parse_tu("void foo(int arr[]);").unwrap();

    if let ExternalDecl::Declaration(decl) = &tu.items[0] {
        let func_typ = decl.declarators[0].typ;
        assert_eq!(types.kind(func_typ), TypeKind::Function);

        if let Some(params) = &types.get(func_typ).params {
            let param_typ = params[0];
            assert_eq!(
                types.kind(param_typ),
                TypeKind::Pointer,
                "Array parameter should be adjusted to pointer"
            );
        }
    } else {
        panic!("Expected Declaration");
    }
}

// __FUNCTION__ and __PRETTY_FUNCTION__ parsing tests

#[test]
fn test_gcc_function_identifier_parsing() {
    // __FUNCTION__ should parse as FuncName with char* type
    let (expr, types, _, _) = parse_expr("__FUNCTION__").unwrap();
    match &expr.kind {
        ExprKind::FuncName => {}
        _ => panic!("Expected FuncName, got {:?}", expr.kind),
    }
    // Type should be char*
    let typ = expr.typ.unwrap();
    assert_eq!(types.kind(typ), TypeKind::Pointer);
}

#[test]
fn test_gcc_pretty_function_identifier_parsing() {
    // __PRETTY_FUNCTION__ should parse as FuncName with char* type
    let (expr, types, _, _) = parse_expr("__PRETTY_FUNCTION__").unwrap();
    match &expr.kind {
        ExprKind::FuncName => {}
        _ => panic!("Expected FuncName, got {:?}", expr.kind),
    }
    // Type should be char*
    let typ = expr.typ.unwrap();
    assert_eq!(types.kind(typ), TypeKind::Pointer);
}

// Long double type parsing tests

#[test]
fn test_long_double_type() {
    // Test "long double x;"
    let (decl, types, _, _) = parse_decl("long double x;").unwrap();
    let typ = decl.declarators[0].typ;
    assert_eq!(types.kind(typ), TypeKind::LongDouble);
}

#[test]
fn test_double_long_type() {
    // Test "double long x;" - alternative ordering per C standard
    let (decl, types, _, _) = parse_decl("double long x;").unwrap();
    let typ = decl.declarators[0].typ;
    assert_eq!(types.kind(typ), TypeKind::LongDouble);
}

#[test]
fn test_long_double_function_return() {
    // Test function returning long double
    let (func, types, _, _) = parse_func("long double foo(void) { return 1.0L; }").unwrap();
    assert_eq!(types.kind(func.return_type), TypeKind::LongDouble);
}

#[test]
fn test_long_double_function_param() {
    // Test function with long double parameter
    let (func, types, _, _) = parse_func("void foo(long double x) {}").unwrap();
    let param_type = func.params[0].typ;
    assert_eq!(types.kind(param_type), TypeKind::LongDouble);
}

// C11 _Atomic qualifier tests

#[test]
fn test_atomic_type_qualifier() {
    // _Atomic as type qualifier: _Atomic int x;
    let (decl, types, _, _) = parse_decl("_Atomic int x;").unwrap();
    let typ = decl.declarators[0].typ;
    assert_eq!(types.kind(typ), TypeKind::Int);
    assert!(types.get(typ).modifiers.contains(TypeModifiers::ATOMIC));
}

#[test]
fn test_atomic_type_specifier() {
    // _Atomic as type specifier: _Atomic(int) x;
    let (decl, types, _, _) = parse_decl("_Atomic(int) x;").unwrap();
    let typ = decl.declarators[0].typ;
    assert_eq!(types.kind(typ), TypeKind::Int);
    assert!(types.get(typ).modifiers.contains(TypeModifiers::ATOMIC));
}

#[test]
fn test_atomic_pointer_to_atomic() {
    // Pointer to atomic: _Atomic int *p;
    let (decl, types, _, _) = parse_decl("_Atomic int *p;").unwrap();
    let typ = decl.declarators[0].typ;
    assert_eq!(types.kind(typ), TypeKind::Pointer);
    // The base type should be atomic int
    let base = types.get(typ).base.unwrap();
    assert_eq!(types.kind(base), TypeKind::Int);
    assert!(types.get(base).modifiers.contains(TypeModifiers::ATOMIC));
}

#[test]
fn test_atomic_pointer_qualifier() {
    // Atomic pointer: int * _Atomic p;
    let (decl, types, _, _) = parse_decl("int * _Atomic p;").unwrap();
    let typ = decl.declarators[0].typ;
    assert_eq!(types.kind(typ), TypeKind::Pointer);
    assert!(types.get(typ).modifiers.contains(TypeModifiers::ATOMIC));
}

#[test]
fn test_atomic_with_const() {
    // Combined qualifiers: const _Atomic int x;
    let (decl, types, _, _) = parse_decl("const _Atomic int x;").unwrap();
    let typ = decl.declarators[0].typ;
    assert_eq!(types.kind(typ), TypeKind::Int);
    assert!(types.get(typ).modifiers.contains(TypeModifiers::ATOMIC));
    assert!(types.get(typ).modifiers.contains(TypeModifiers::CONST));
}

#[test]
fn test_atomic_specifier_with_pointer() {
    // _Atomic(int *) - atomic pointer type
    let (decl, types, _, _) = parse_decl("_Atomic(int *) p;").unwrap();
    let typ = decl.declarators[0].typ;
    assert_eq!(types.kind(typ), TypeKind::Pointer);
    assert!(types.get(typ).modifiers.contains(TypeModifiers::ATOMIC));
}

#[test]
fn test_atomic_function_param() {
    // Function with atomic parameter
    let (func, types, _, _) = parse_func("void foo(_Atomic int x) {}").unwrap();
    let param_type = func.params[0].typ;
    assert_eq!(types.kind(param_type), TypeKind::Int);
    assert!(types
        .get(param_type)
        .modifiers
        .contains(TypeModifiers::ATOMIC));
}

#[test]
fn test_atomic_local_variable() {
    // Local atomic variable
    let (tu, types, _, _symbols) =
        parse_tu("int main(void) { _Atomic int counter = 0; return 0; }").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::FunctionDef(func) => {
            // Check function body is a block
            if let crate::parse::ast::Stmt::Block(items) = &func.body {
                // Find the counter variable in locals
                let found = items.iter().any(|item| {
                    if let crate::parse::ast::BlockItem::Declaration(decl) = item {
                        let typ = decl.declarators[0].typ;
                        types.get(typ).modifiers.contains(TypeModifiers::ATOMIC)
                    } else {
                        false
                    }
                });
                assert!(found, "Expected to find atomic local variable");
            } else {
                panic!("Expected Block statement for function body");
            }
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_atomic_in_cast() {
    // _Atomic in cast expression: (_Atomic int)value
    let (tu, types, _, _) =
        parse_tu("int main(void) { int x = 42; return (_Atomic int)x; }").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::FunctionDef(func) => {
            // Verify the function parsed successfully
            assert_eq!(types.kind(func.return_type), TypeKind::Int);
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_atomic_specifier_in_cast() {
    // _Atomic(type) specifier form in cast: (_Atomic(int))value
    let (tu, types, _, _) =
        parse_tu("int main(void) { int x = 42; return (_Atomic(int))x; }").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::FunctionDef(func) => {
            assert_eq!(types.kind(func.return_type), TypeKind::Int);
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_atomic_in_sizeof() {
    // sizeof(_Atomic int)
    let (tu, _types, _, _) = parse_tu("int main(void) { return sizeof(_Atomic int); }").unwrap();
    assert_eq!(tu.items.len(), 1);
}

#[test]
fn test_atomic_specifier_in_sizeof() {
    // sizeof(_Atomic(int))
    let (tu, _types, _, _) = parse_tu("int main(void) { return sizeof(_Atomic(int)); }").unwrap();
    assert_eq!(tu.items.len(), 1);
}

// Atomic builtin tests

#[test]
fn test_atomic_load_n() {
    // __atomic_load_n(ptr, order)
    let (tu, types, _, _) =
        parse_tu("int foo(int *p) { return __atomic_load_n(p, __ATOMIC_SEQ_CST); }").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::FunctionDef(func) => {
            let ret_type = func.return_type;
            assert_eq!(types.kind(ret_type), TypeKind::Int);
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_atomic_store_n() {
    // __atomic_store_n(ptr, val, order)
    let (tu, _, _, _) =
        parse_tu("void foo(int *p, int v) { __atomic_store_n(p, v, __ATOMIC_RELEASE); }").unwrap();
    assert_eq!(tu.items.len(), 1);
    assert!(matches!(&tu.items[0], ExternalDecl::FunctionDef(_)));
}

#[test]
fn test_atomic_exchange_n() {
    // __atomic_exchange_n(ptr, val, order)
    let (tu, types, _, _) =
        parse_tu("int foo(int *p, int v) { return __atomic_exchange_n(p, v, __ATOMIC_ACQ_REL); }")
            .unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::FunctionDef(func) => {
            let ret_type = func.return_type;
            assert_eq!(types.kind(ret_type), TypeKind::Int);
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_atomic_compare_exchange_n() {
    // __atomic_compare_exchange_n(ptr, expected, desired, weak, succ, fail)
    let (tu, types, _, _) = parse_tu(
        "int foo(int *p, int *exp, int des) { return __atomic_compare_exchange_n(p, exp, des, 0, __ATOMIC_SEQ_CST, __ATOMIC_RELAXED); }",
    )
    .unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::FunctionDef(func) => {
            // CAS returns bool
            let ret_type = func.return_type;
            assert_eq!(types.kind(ret_type), TypeKind::Int);
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_atomic_fetch_add() {
    // __atomic_fetch_add(ptr, val, order)
    let (tu, types, _, _) =
        parse_tu("int foo(int *p) { return __atomic_fetch_add(p, 1, __ATOMIC_RELAXED); }").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::FunctionDef(func) => {
            let ret_type = func.return_type;
            assert_eq!(types.kind(ret_type), TypeKind::Int);
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_atomic_fetch_sub() {
    // __atomic_fetch_sub(ptr, val, order)
    let (tu, types, _, _) =
        parse_tu("int foo(int *p) { return __atomic_fetch_sub(p, 1, __ATOMIC_ACQUIRE); }").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::FunctionDef(func) => {
            let ret_type = func.return_type;
            assert_eq!(types.kind(ret_type), TypeKind::Int);
        }
        _ => panic!("Expected FunctionDef"),
    }
}

#[test]
fn test_atomic_fetch_bitwise() {
    // __atomic_fetch_and, __atomic_fetch_or, __atomic_fetch_xor
    let (tu, _, _, _) = parse_tu(
        r#"
        int foo(int *p) {
            __atomic_fetch_and(p, 0xFF, __ATOMIC_RELAXED);
            __atomic_fetch_or(p, 0x80, __ATOMIC_RELAXED);
            return __atomic_fetch_xor(p, 0x01, __ATOMIC_RELAXED);
        }
        "#,
    )
    .unwrap();
    assert_eq!(tu.items.len(), 1);
    assert!(matches!(&tu.items[0], ExternalDecl::FunctionDef(_)));
}

#[test]
fn test_atomic_thread_fence() {
    // __atomic_thread_fence(order)
    let (tu, _, _, _) =
        parse_tu("void foo(void) { __atomic_thread_fence(__ATOMIC_SEQ_CST); }").unwrap();
    assert_eq!(tu.items.len(), 1);
    assert!(matches!(&tu.items[0], ExternalDecl::FunctionDef(_)));
}

#[test]
fn test_atomic_signal_fence() {
    // __atomic_signal_fence(order)
    let (tu, _, _, _) =
        parse_tu("void foo(void) { __atomic_signal_fence(__ATOMIC_ACQUIRE); }").unwrap();
    assert_eq!(tu.items.len(), 1);
    assert!(matches!(&tu.items[0], ExternalDecl::FunctionDef(_)));
}

// C23 _Float* type tests (TS 18661-3)

#[test]
fn test_float16_type_decl() {
    let (tu, types, _, _) = parse_tu("_Float16 x;").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Float16);
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_float32_type_decl() {
    // _Float32 is alias for float
    let (tu, types, _, _) = parse_tu("_Float32 x;").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Float);
        }
        _ => panic!("Expected Declaration"),
    }
}

#[test]
fn test_float64_type_decl() {
    // _Float64 is alias for double
    let (tu, types, _, _) = parse_tu("_Float64 x;").unwrap();
    assert_eq!(tu.items.len(), 1);
    match &tu.items[0] {
        ExternalDecl::Declaration(decl) => {
            assert_eq!(decl.declarators.len(), 1);
            assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Double);
        }
        _ => panic!("Expected Declaration"),
    }
}

// C23 _Float* literal suffix tests (f16, f32, f64)

#[test]
fn test_float16_literal_suffix() {
    let (expr, types, _, _) = parse_expr("1.0f16").unwrap();
    match expr.kind {
        ExprKind::FloatLit(v) => assert!((v.to_f64() - 1.0).abs() < 0.001),
        _ => panic!("Expected FloatLit"),
    }
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Float16);
}

#[test]
fn test_float16_literal_suffix_upper() {
    let (expr, types, _, _) = parse_expr("3.14F16").unwrap();
    match expr.kind {
        ExprKind::FloatLit(v) => assert!((v.to_f64() - 3.14).abs() < 0.001),
        _ => panic!("Expected FloatLit"),
    }
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Float16);
}

#[test]
fn test_float32_literal_suffix() {
    // f32 is alias for float
    let (expr, types, _, _) = parse_expr("2.5f32").unwrap();
    match expr.kind {
        ExprKind::FloatLit(v) => assert!((v.to_f64() - 2.5).abs() < 0.001),
        _ => panic!("Expected FloatLit"),
    }
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Float);
}

#[test]
fn test_float64_literal_suffix() {
    // f64 is alias for double
    let (expr, types, _, _) = parse_expr("2.5f64").unwrap();
    match expr.kind {
        ExprKind::FloatLit(v) => assert!((v.to_f64() - 2.5).abs() < 0.001),
        _ => panic!("Expected FloatLit"),
    }
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Double);
}

/// A `_FloatN` suffix is a *floating* suffix (TS 18661-3): it does not make
/// an integer constant floating. gcc rejects `42f16` with "invalid suffix on
/// integer constant"; c17 took it for a `_Float16` 42.
#[test]
fn test_int_with_float_suffix_is_rejected() {
    for src in ["42f16", "42f32", "42f64", "42f128", "42f", "42q", "42lf"] {
        assert!(parse_expr(src).is_err(), "{src} should be rejected");
    }
}

/// A `_FloatN` suffix on a hex floating constant, after the `p` exponent.
///
/// Suffixes were found by `ends_with` on the whole spelling and the `fN` ones
/// were only looked for on decimal constants, since before a `p` they are
/// hex digits; so `0x1p0f16` was rejected outright.
#[test]
fn test_hex_float_with_float_n_suffix() {
    for (src, want) in [
        ("0x1p0f16", TypeKind::Float16),
        ("0x1.8p1F16", TypeKind::Float16),
        ("0x1p0f32", TypeKind::Float),
        ("0x1p0f64", TypeKind::Double),
    ] {
        let (expr, types, _, _) =
            parse_expr(src).unwrap_or_else(|e| panic!("{src} did not parse: {e:?}"));
        assert!(matches!(expr.kind, ExprKind::FloatLit(_)), "{src}");
        assert_eq!(types.kind(expr.typ.unwrap()), want, "{src}");
    }
}

/// Malformed suffixes are rejected rather than trimmed until something
/// parses: `1.0lf` was a `long double` and `1f` the integer 1.
#[test]
fn test_malformed_number_suffixes_are_rejected() {
    for src in [
        "1.0lf", "1.0fl", "1f", "1lL", "1uu", "1lul", "1.0ff", "2.0f32x", "1.5e+",
    ] {
        assert!(parse_expr(src).is_err(), "{src} should be rejected");
    }
}

// _Alignof expression tests (C11)

#[test]
fn test_alignof_type_int() {
    let (expr, types, _, _) = parse_expr("_Alignof(int)").unwrap();
    match expr.kind {
        ExprKind::AlignofType(tid) => assert_eq!(tid, types.int_id),
        _ => panic!("Expected AlignofType, got {:?}", expr.kind),
    }
}

#[test]
fn test_alignof_type_char() {
    let (expr, types, _, _) = parse_expr("_Alignof(char)").unwrap();
    match expr.kind {
        ExprKind::AlignofType(tid) => assert_eq!(tid, types.char_id),
        _ => panic!("Expected AlignofType, got {:?}", expr.kind),
    }
}

#[test]
fn test_alignof_type_double() {
    let (expr, types, _, _) = parse_expr("_Alignof(double)").unwrap();
    match expr.kind {
        ExprKind::AlignofType(tid) => assert_eq!(tid, types.double_id),
        _ => panic!("Expected AlignofType, got {:?}", expr.kind),
    }
}

#[test]
fn test_alignof_alias_alignof() {
    // C23 alignof keyword
    let (expr, _, _, _) = parse_expr("alignof(char)").unwrap();
    assert!(
        matches!(expr.kind, ExprKind::AlignofType(_)),
        "Expected AlignofType"
    );
}

#[test]
fn test_alignof_alias_gcc_double_underscore() {
    // GCC __alignof__
    let (expr, _, _, _) = parse_expr("__alignof__(int)").unwrap();
    assert!(
        matches!(expr.kind, ExprKind::AlignofType(_)),
        "Expected AlignofType"
    );
}

#[test]
fn test_alignof_alias_gcc_single_underscore() {
    // GCC __alignof
    let (expr, _, _, _) = parse_expr("__alignof(long)").unwrap();
    assert!(
        matches!(expr.kind, ExprKind::AlignofType(_)),
        "Expected AlignofType"
    );
}

#[test]
fn test_alignof_expr_variable() {
    // _Alignof on an expression (variable)
    let (expr, _, _, _) = parse_expr_with_vars("_Alignof(x)", &["x"]).unwrap();
    assert!(
        matches!(expr.kind, ExprKind::AlignofExpr(_)),
        "Expected AlignofExpr"
    );
}

#[test]
fn test_alignof_returns_size_t_type() {
    // _Alignof should return size_t (unsigned long on 64-bit)
    let (expr, types, _, _) = parse_expr("_Alignof(int)").unwrap();
    assert_eq!(expr.typ, Some(types.ulong_id));
}

// __builtin_nan/nanf/nanl tests

#[test]
fn test_builtin_nan() {
    let (expr, types, _, _) = parse_expr("__builtin_nan(\"\")").unwrap();
    assert!(matches!(expr.kind, ExprKind::FloatLit(v) if v.is_nan()));
    assert_eq!(expr.typ, Some(types.double_id));
}

#[test]
fn test_builtin_nanf() {
    let (expr, types, _, _) = parse_expr("__builtin_nanf(\"\")").unwrap();
    assert!(matches!(expr.kind, ExprKind::FloatLit(v) if v.is_nan()));
    assert_eq!(expr.typ, Some(types.float_id));
}

#[test]
fn test_builtin_nanl() {
    let (expr, types, _, _) = parse_expr("__builtin_nanl(\"\")").unwrap();
    assert!(matches!(expr.kind, ExprKind::FloatLit(v) if v.is_nan()));
    assert_eq!(expr.typ, Some(types.longdouble_id));
}

#[test]
fn test_builtin_nans() {
    let (expr, types, _, _) = parse_expr("__builtin_nans(\"\")").unwrap();
    assert!(matches!(expr.kind, ExprKind::FloatLit(v) if v.is_nan()));
    assert_eq!(expr.typ, Some(types.double_id));
}

/// The encoding of the floating constant `src` parses to, at `fmt`.
fn nan_bits(src: &str, fmt: crate::float::FpFormat) -> u128 {
    let (expr, _, _, _) = parse_expr(src).unwrap();
    match expr.kind {
        ExprKind::FloatLit(v) => v.to_bits(fmt),
        other => panic!("{src} parsed to {other:?}"),
    }
}

/// The string is parsed as gcc parses it -- `strtoull` with base 0 -- and
/// its value is the payload: every expectation is gcc's emitted constant.
#[test]
fn test_builtin_nan_payload_parsing() {
    use crate::float::FpFormat::{Binary32, Binary64};
    let cases = [
        // Empty, and the spellings of zero.
        ("__builtin_nan(\"\")", 0x7ff8_0000_0000_0000),
        ("__builtin_nan(\"0\")", 0x7ff8_0000_0000_0000),
        ("__builtin_nan(\"0x\")", 0x7ff8_0000_0000_0000),
        // Hexadecimal, either case of the prefix.
        ("__builtin_nan(\"0x1234\")", 0x7ff8_0000_0000_1234),
        ("__builtin_nan(\"0X10\")", 0x7ff8_0000_0000_0010),
        // Decimal.
        ("__builtin_nan(\"4660\")", 0x7ff8_0000_0000_1234),
        // Octal, from a leading zero.
        ("__builtin_nan(\"010\")", 0x7ff8_0000_0000_0008),
        // Leading white space and a sign are skipped; the sign is ignored.
        ("__builtin_nan(\" 5\")", 0x7ff8_0000_0000_0005),
        ("__builtin_nan(\"-1\")", 0x7ff8_0000_0000_0001),
        // Wider than the payload: the low bits are kept.
        (
            "__builtin_nan(\"0xffffffffffffffff\")",
            0x7fff_ffff_ffff_ffff,
        ),
        (
            "__builtin_nan(\"18446744073709551616\")",
            0x7ff8_0000_0000_0000,
        ),
        // A C string ends at its first NUL.
        ("__builtin_nan(\"12\\0abc\")", 0x7ff8_0000_0000_000c),
        // Signalling: the quiet bit clear, and an empty payload made
        // non-empty so the result is not an infinity.
        ("__builtin_nans(\"0x1234\")", 0x7ff0_0000_0000_1234),
        ("__builtin_nans(\"\")", 0x7ff4_0000_0000_0000),
    ];
    for (src, want) in cases {
        assert_eq!(nan_bits(src, Binary64), want, "{src}");
    }
    assert_eq!(nan_bits("__builtin_nanf(\"0x123\")", Binary32), 0x7fc0_0123);
    assert_eq!(
        nan_bits("__builtin_nansf(\"0x123\")", Binary32),
        0x7f80_0123
    );
    assert_eq!(nan_bits("__builtin_nansf(\"\")", Binary32), 0x7fa0_0000);
}

/// `long double` puts the payload in its own format's significand.
#[test]
fn test_builtin_nanl_payload() {
    let (expr, types, _, _) = parse_expr("__builtin_nanl(\"0x1234\")").unwrap();
    let fmt = types.fp_format(types.longdouble_id).unwrap();
    let want = match fmt {
        crate::float::FpFormat::X87Extended => 0x7fff_c000_0000_0000_1234,
        crate::float::FpFormat::Binary128 => 0x7fff_8000_0000_0000_0000_0000_0000_1234,
        _ => 0x7ff8_0000_0000_1234,
    };
    assert!(matches!(expr.kind, ExprKind::FloatLit(v) if v.to_bits(fmt) == want));
}

/// The `_FloatN` forms of the infinity and NaN builtins: each is a constant
/// of the type its suffix names -- `_Float32` and `_Float64` being `float`
/// and `double` here -- whose encoding is gcc's, payload and all.
#[test]
fn test_builtin_float_n_constants() {
    use crate::float::FpFormat::{Binary128, Binary16, Binary32, Binary64};
    type Want = fn(&TypeTable) -> crate::types::TypeId;
    let cases: &[(&str, Want, crate::float::FpFormat, u128)] = &[
        ("__builtin_inff16()", |t| t.float16_id, Binary16, 0x7c00),
        (
            "__builtin_huge_valf16()",
            |t| t.float16_id,
            Binary16,
            0x7c00,
        ),
        ("__builtin_nanf16(\"\")", |t| t.float16_id, Binary16, 0x7e00),
        (
            "__builtin_nanf16(\"0x12\")",
            |t| t.float16_id,
            Binary16,
            0x7e12,
        ),
        (
            "__builtin_nanf16(\"0xfff\")",
            |t| t.float16_id,
            Binary16,
            0x7fff,
        ),
        (
            "__builtin_nansf16(\"\")",
            |t| t.float16_id,
            Binary16,
            0x7d00,
        ),
        (
            "__builtin_nansf16(\"0x12\")",
            |t| t.float16_id,
            Binary16,
            0x7c12,
        ),
        ("__builtin_inff32()", |t| t.float_id, Binary32, 0x7f80_0000),
        (
            "__builtin_huge_valf32()",
            |t| t.float_id,
            Binary32,
            0x7f80_0000,
        ),
        (
            "__builtin_nanf32(\"0x5\")",
            |t| t.float_id,
            Binary32,
            0x7fc0_0005,
        ),
        (
            "__builtin_nansf32(\"\")",
            |t| t.float_id,
            Binary32,
            0x7fa0_0000,
        ),
        (
            "__builtin_inff64()",
            |t| t.double_id,
            Binary64,
            0x7ff0 << 48,
        ),
        (
            "__builtin_huge_valf64()",
            |t| t.double_id,
            Binary64,
            0x7ff0 << 48,
        ),
        (
            "__builtin_nanf64(\"0x5\")",
            |t| t.double_id,
            Binary64,
            0x7ff8_0000_0000_0005,
        ),
        (
            "__builtin_nansf64(\"\")",
            |t| t.double_id,
            Binary64,
            0x7ff4 << 48,
        ),
    ];
    let binary128: &[(&str, u128)] = &[
        ("__builtin_inff128()", 0x7fff << 112),
        ("__builtin_huge_valf128()", 0x7fff << 112),
        ("__builtin_nanf128(\"0x1234\")", 0x7fff8 << 108 | 0x1234),
        ("__builtin_nansf128(\"\")", 0x7fff4 << 108),
        ("__builtin_nansf128(\"0x1234\")", 0x7fff << 112 | 0x1234),
    ];
    for &(src, want, fmt, bits) in cases {
        let (expr, types, _, _) = parse_expr(src).unwrap();
        assert_eq!(expr.typ, Some(want(&types)), "{src}");
        assert_eq!(nan_bits(src, fmt), bits, "{src}");
    }
    // The `f128` forms exist where `__float128` does -- not on Apple arm64,
    // which test_builtin_float128_constant_follows_the_target covers -- so
    // they are checked on the Linux targets, not the host.
    for target in linux_targets() {
        for &(src, bits) in binary128 {
            let (expr, types, _, _) = parse_expr_for(src, &target).unwrap();
            assert_eq!(expr.typ, Some(types.float128_id), "{src}");
            let ExprKind::FloatLit(v) = expr.kind else {
                panic!("{src} parsed to {:?}", expr.kind);
            };
            assert_eq!(v.to_bits(Binary128), bits, "{src}");
        }
    }
}

/// A `_FloatN` NaN whose string is not a payload calls the library function
/// gcc calls, `nanf16` and so on, whose type is the builtin's.
#[test]
fn test_builtin_nan_float_n_of_a_malformed_string_is_a_call() {
    for (src, float16) in [
        ("__builtin_nanf16(\"abc\")", true),
        ("__builtin_nanf32(\"abc\")", false),
    ] {
        let (expr, types, _, _) = parse_expr(src).unwrap();
        assert!(matches!(expr.kind, ExprKind::Call { .. }), "{src}");
        let want = if float16 {
            types.float16_id
        } else {
            types.float_id
        };
        assert_eq!(expr.typ, Some(want), "{src}");
    }
}

/// `__builtin_nanf128` is a `_Float128` wherever the target has one -- on
/// aarch64 Linux too, where `long double` has the same format but is another
/// type -- and an error where it has none.
#[test]
fn test_builtin_float128_constant_follows_the_target() {
    use crate::target::{Arch, Os};
    for (arch, os, supported) in [
        (Arch::X86_64, Os::Linux, true),
        (Arch::Aarch64, Os::Linux, true),
        (Arch::Aarch64, Os::MacOS, false),
    ] {
        let src = "__builtin_nanf128(\"0x1\")";
        let mut strings = StringTable::new();
        let mut tokenizer = Tokenizer::new(src.as_bytes(), 0, &mut strings);
        let tokens = tokenizer.tokenize();
        let mut symbols = SymbolTable::new();
        let mut types = TypeTable::new(&Target::new(arch, os));
        let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
        parser.skip_stream_tokens();
        let result = parser.parse_expression();
        match (result, supported) {
            (Ok(expr), true) => {
                assert_eq!(expr.typ, Some(types.float128_id), "{arch}-{os}");
                let ExprKind::FloatLit(v) = expr.kind else {
                    panic!("{arch}-{os}: {:?}", expr.kind);
                };
                assert_eq!(
                    v.to_bits(crate::float::FpFormat::Binary128),
                    0x7fff8 << 108 | 1
                );
            }
            (Err(e), false) => assert!(e.to_string().contains("not supported"), "{e}"),
            (other, _) => panic!("{arch}-{os}: {other:?}"),
        }
    }
}

/// A string gcc does not fold is left to the library, as gcc leaves it:
/// `__builtin_nan` becomes a call to `nan`.
#[test]
fn test_builtin_nan_of_a_malformed_string_is_a_call() {
    for src in [
        "__builtin_nan(\"abc\")",
        "__builtin_nan(\"08\")",
        "__builtin_nan(\"12x\")",
    ] {
        let (expr, types, _, _) = parse_expr(src).unwrap();
        assert!(
            matches!(expr.kind, ExprKind::Call { .. }),
            "{src}: {:?}",
            expr.kind
        );
        assert_eq!(expr.typ, Some(types.double_id), "{src}");
    }
}

/// The first statement of the first function defined in `tu`.
fn first_statement(tu: &TranslationUnit) -> &Stmt {
    first_statement_of(tu, 0)
}

/// The first statement of the function defined `n`th (from 0) in `tu`.
fn first_statement_of(tu: &TranslationUnit, n: usize) -> &Stmt {
    let func = tu
        .items
        .iter()
        .filter_map(|item| match item {
            ExternalDecl::FunctionDef(f) => Some(f),
            _ => None,
        })
        .nth(n)
        .expect("function definition");
    let Stmt::Block(items) = &func.body else {
        panic!("function body is not a block");
    };
    let Some(BlockItem::Statement(stmt)) = items.first() else {
        panic!("expected a statement");
    };
    stmt
}

/// C17 6.5.2.3p3/p4: `s.m` and `p->m` have the *so-qualified* version of the
/// member's type -- the member's type plus the qualifiers of the object.
///
/// The parser is where that type is formed, and it took the member's declared
/// type unchanged: a member of a `volatile` object read as an ordinary `int`
/// (so DCE deleted the load) and a member of a `const` object was assignable.
#[test]
fn test_a_member_access_is_qualified_by_the_object() {
    let quals = |src: &str, decls: &str| -> TypeModifiers {
        let code = format!("struct S {{ int a; }};\nstruct N {{ struct S in; }};\n{decls}\nvoid t(void) {{ {src}; }}");
        let (tu, types, _, _) = parse_tu(&code).unwrap();
        let Stmt::Expr(expr) = first_statement(&tu) else {
            panic!("{src}: expected an expression statement");
        };
        types.qualifiers(expr.typ.expect("a typed expression"))
    };
    const NONE: TypeModifiers = TypeModifiers::empty();

    // The object's qualifiers, in each spelling that reaches a member.
    assert_eq!(
        quals("vs.a", "volatile struct S vs;"),
        TypeModifiers::VOLATILE
    );
    assert_eq!(quals("cs.a", "const struct S cs;"), TypeModifiers::CONST);
    assert_eq!(
        quals("cvs.a", "const volatile struct S cvs;"),
        TypeModifiers::CONST | TypeModifiers::VOLATILE
    );
    assert_eq!(
        quals("vsa[1].a", "volatile struct S vsa[2];"),
        TypeModifiers::VOLATILE
    );
    assert_eq!(
        quals("vn.in.a", "volatile struct N vn;"),
        TypeModifiers::VOLATILE,
        "the intermediate member is qualified too, and carries it downward"
    );
    assert_eq!(
        quals("vt.a", "typedef volatile struct S VS; VS vt;"),
        TypeModifiers::VOLATILE
    );

    // `->` takes them from the *pointee*, which is the object it names.
    assert_eq!(
        quals("vp->a", "volatile struct S *vp;"),
        TypeModifiers::VOLATILE
    );
    assert_eq!(
        quals("qp->a", "struct S *volatile qp;"),
        NONE,
        "`struct S *volatile` qualifies the pointer, not what it points at"
    );

    // `_Atomic` does not travel: there is no atomic access to one member of an
    // atomic object, and gcc does not pretend otherwise.
    assert_eq!(quals("as.a", "_Atomic struct S as;"), NONE);

    // An unqualified object leaves the member's declared type alone -- and a
    // qualifier on the *member* still reaches the access, which is the
    // direction that always worked.
    assert_eq!(quals("s.a", "struct S s;"), NONE);
    assert_eq!(
        quals("vm.v", "struct M { volatile int v; }; struct M vm;"),
        TypeModifiers::VOLATILE
    );
}

/// C17 6.3.2.1p2: converting an lvalue to a value drops the qualifiers, so an
/// arithmetic result is never qualified.
///
/// `common_type` and `integer_promote` answer with one of the operands' own
/// type ids, so `volatile int + int` came out `volatile int` -- a qualifier on
/// the type of a value, which nothing may read as "this expression touched a
/// volatile object".
#[test]
fn test_an_arithmetic_result_is_unqualified() {
    let quals = |src: &str| -> TypeModifiers {
        let code = format!(
            "struct S {{ int a; long l; }};\nvolatile struct S vs;\nvolatile int vi;\n\
             void t(void) {{ {src}; }}"
        );
        let (tu, types, _, _) = parse_tu(&code).unwrap();
        let Stmt::Expr(expr) = first_statement(&tu) else {
            panic!("{src}: expected an expression statement");
        };
        types.qualifiers(expr.typ.expect("a typed expression"))
    };
    const NONE: TypeModifiers = TypeModifiers::empty();

    // The usual arithmetic conversions, a shift (whose result is the promoted
    // *left* operand's type), and a unary operator.
    assert_eq!(quals("vs.a + 1"), NONE);
    assert_eq!(quals("1 + vs.a"), NONE);
    assert_eq!(quals("vs.l * vs.a"), NONE);
    assert_eq!(quals("vi | 1"), NONE);
    assert_eq!(quals("vs.a << 1"), NONE);
    assert_eq!(quals("-vs.a"), NONE);
    assert_eq!(quals("~vi"), NONE);
    assert_eq!(quals("1 ? vs.a : 0"), NONE);

    // The lvalue itself keeps them: that is the whole point of the rule above,
    // and the assignment check and the volatile marker both read it.
    assert_eq!(quals("vs.a"), TypeModifiers::VOLATILE);
}

// Library builtins: abs, fabs, creal, conj, ... as checked calls

/// The in-place call `expr` is, as (function, arguments), or a panic naming
/// `src` and what it was instead.
fn inline_call_args<'e>(src: &str, expr: &'e Expr) -> (InlineLibraryFn, &'e [Expr]) {
    match &expr.kind {
        ExprKind::InlineLibraryCall { func, args, .. } => (*func, args),
        other => panic!("{src}: expected an InlineLibraryCall, got {other:?}"),
    }
}

/// The in-place call of one argument `expr` is, as (function, argument).
fn inline_call<'e>(src: &str, expr: &'e Expr) -> (InlineLibraryFn, &'e Expr) {
    match inline_call_args(src, expr) {
        (func, [arg]) => (func, arg),
        (func, args) => panic!("{src}: {func:?} has {} arguments", args.len()),
    }
}

/// Each integer magnitude, bare or reserved, is computed in place at the
/// function's own type, with the argument converted to it as the prototype
/// would.
#[test]
fn test_int_abs_builtins() {
    // The spelling, the type it answers, and whether an `int` argument is
    // converted to reach it.
    type Want = fn(&TypeTable) -> crate::types::TypeId;
    let cases: &[(&str, Want, bool)] = &[
        ("abs(i)", |t| t.int_id, false),
        ("__builtin_abs(i)", |t| t.int_id, false),
        ("labs(i)", |t| t.long_id, true),
        ("__builtin_labs(i)", |t| t.long_id, true),
        ("llabs(i)", |t| t.longlong_id, true),
        ("__builtin_llabs(i)", |t| t.longlong_id, true),
        ("imaxabs(i)", |t| t.long_id, true),
        ("__builtin_imaxabs(i)", |t| t.long_id, true),
    ];
    for (src, want, converted) in cases {
        let (expr, types, _, _) = parse_expr_with_vars(src, &["i"]).unwrap();
        assert_eq!(expr.typ, Some(want(&types)), "{src}");
        let (func, arg) = inline_call(src, &expr);
        assert_eq!(func, InlineLibraryFn::IntAbs, "{src}");
        assert_eq!(arg.typ, Some(want(&types)), "{src}");
        assert_eq!(
            matches!(arg.kind, ExprKind::Cast { .. }),
            *converted,
            "{src}"
        );
    }
}

/// `fabs`, `fabsf` and `fabsl` are all computed in place, each at its own
/// type.
#[test]
fn test_fabs_builtins() {
    let (expr, types, _, _) = parse_expr_with_vars("fabs(i)", &["i"]).unwrap();
    let (func, arg) = inline_call("fabs(i)", &expr);
    assert_eq!(func, InlineLibraryFn::Fabs);
    assert_eq!(expr.typ, Some(types.double_id));
    assert_eq!(arg.typ, Some(types.double_id), "the int converts to double");

    let (expr, types, _, _) = parse_expr_with_vars("__builtin_fabsf(i)", &["i"]).unwrap();
    let (func, _) = inline_call("__builtin_fabsf(i)", &expr);
    assert_eq!(func, InlineLibraryFn::Fabs);
    assert_eq!(expr.typ, Some(types.float_id));

    let (expr, types, _, _) = parse_expr_with_vars("fabsl(i)", &["i"]).unwrap();
    let (func, arg) = inline_call("fabsl(i)", &expr);
    assert_eq!(func, InlineLibraryFn::Fabs);
    assert_eq!(expr.typ, Some(types.longdouble_id));
    assert_eq!(
        arg.typ,
        Some(types.longdouble_id),
        "the int converts to long double"
    );
}

/// `copysign`, `copysignf` and `copysignl`, bare or reserved, are computed in
/// place at their own type, each of the two arguments converted to it.
#[test]
fn test_copysign_builtins() {
    type Want = fn(&TypeTable) -> crate::types::TypeId;
    let cases: &[(&str, Want)] = &[
        ("copysign(i, f)", |t| t.double_id),
        ("__builtin_copysign(i, f)", |t| t.double_id),
        ("copysignf(i, f)", |t| t.float_id),
        ("__builtin_copysignf(i, f)", |t| t.float_id),
        ("copysignl(i, f)", |t| t.longdouble_id),
        ("__builtin_copysignl(i, f)", |t| t.longdouble_id),
    ];
    for (src, want) in cases {
        let (expr, types, _, _) = parse_expr_with_vars(src, &["i", "f"]).unwrap();
        assert_eq!(expr.typ, Some(want(&types)), "{src}");
        let (func, args) = inline_call_args(src, &expr);
        assert_eq!(func, InlineLibraryFn::CopySign, "{src}");
        assert_eq!(args.len(), 2, "{src}");
        for arg in args {
            assert_eq!(arg.typ, Some(want(&types)), "{src}: an argument");
        }
    }
}

/// `sqrt`, `sqrtf` and `sqrtl`, bare or reserved, are computed in place at
/// their own type, reporting a domain error through `errno` by default, and
/// carry the library name a call path needs.
#[test]
fn test_sqrt_builtins() {
    type Want = fn(&TypeTable) -> crate::types::TypeId;
    let cases: &[(&str, Want, &str)] = &[
        ("sqrt(i)", |t| t.double_id, "sqrt"),
        ("__builtin_sqrt(i)", |t| t.double_id, "sqrt"),
        ("sqrtf(i)", |t| t.float_id, "sqrtf"),
        ("__builtin_sqrtf(i)", |t| t.float_id, "sqrtf"),
        ("sqrtl(i)", |t| t.longdouble_id, "sqrtl"),
        ("__builtin_sqrtl(i)", |t| t.longdouble_id, "sqrtl"),
    ];
    for (src, want, callee) in cases {
        let (expr, types, strings, _) = parse_expr_with_vars(src, &["i"]).unwrap();
        assert_eq!(expr.typ, Some(want(&types)), "{src}");
        let (func, arg) = inline_call(src, &expr);
        assert_eq!(func, InlineLibraryFn::Sqrt(MathErrno::Set), "{src}");
        assert_eq!(arg.typ, Some(want(&types)), "{src}: the int converts");
        let ExprKind::InlineLibraryCall { name, .. } = expr.kind else {
            unreachable!()
        };
        check_name(&strings, name, callee);
    }
}

/// `-fno-math-errno` drops the domain-error path; `-O0` calls the library
/// for a bare spelling, and for any spelling that must still set `errno`,
/// as gcc does.
#[test]
fn test_sqrt_follows_the_library_call_policy() {
    use super::LibraryCallPolicy;
    let no_errno = LibraryCallPolicy {
        optimizing: true,
        math_errno: false,
    };
    let (expr, _, _, _) = parse_expr_under("sqrt(i)", &["i"], no_errno).unwrap();
    let (func, _) = inline_call("sqrt(i)", &expr);
    assert_eq!(func, InlineLibraryFn::Sqrt(MathErrno::Ignored));

    let is_call = |e: &Expr| matches!(e.kind, ExprKind::Call { .. });
    let o0 = |math_errno| LibraryCallPolicy {
        optimizing: false,
        math_errno,
    };
    for (src, math_errno, called) in [
        ("sqrt(i)", true, true),
        ("__builtin_sqrt(i)", true, true),
        ("sqrt(i)", false, true),
        ("__builtin_sqrt(i)", false, false),
        ("fabs(i)", true, false),
        ("copysign(i, i)", true, false),
    ] {
        let (expr, types, _, _) = parse_expr_under(src, &["i"], o0(math_errno)).unwrap();
        assert_eq!(is_call(&expr), called, "{src} errno={math_errno}");
        assert_eq!(expr.typ, Some(types.double_id), "{src}");
    }
}

/// The six roundings and their `f` forms, bare or reserved, are computed in
/// place at their own type and named for the call a target may still make.
#[test]
fn test_rounding_builtins() {
    use IntegralRounding::*;
    for (base, how) in [
        ("floor", Floor),
        ("ceil", Ceil),
        ("trunc", Trunc),
        ("round", Round),
        ("rint", Rint),
        ("nearbyint", NearbyInt),
    ] {
        for suffix in ["", "f"] {
            for prefix in ["", "__builtin_"] {
                let src = format!("{prefix}{base}{suffix}(i)");
                let (expr, types, strings, _) = parse_expr_with_vars(&src, &["i"]).unwrap();
                let want = if suffix.is_empty() {
                    types.double_id
                } else {
                    types.float_id
                };
                assert_eq!(expr.typ, Some(want), "{src}");
                let (func, arg) = inline_call(&src, &expr);
                assert_eq!(func, InlineLibraryFn::RoundToIntegral(how), "{src}");
                assert_eq!(arg.typ, Some(want), "{src}");
                let ExprKind::InlineLibraryCall { name, .. } = expr.kind else {
                    unreachable!()
                };
                check_name(&strings, name, &format!("{base}{suffix}"));
            }
        }
    }
}

/// `fmin`, `fmax`, `fma` and their `f` forms, bare or reserved, are computed
/// in place at their own type, every argument converted to it; nothing
/// narrows.
#[test]
fn test_min_max_fma_builtins() {
    for (src, func, float) in [
        ("fmin(i, f)", InlineLibraryFn::FMin, false),
        ("__builtin_fminf(i, f)", InlineLibraryFn::FMin, true),
        ("fmaxf(f, i)", InlineLibraryFn::FMax, true),
        ("__builtin_fmax(f, f)", InlineLibraryFn::FMax, false),
        ("fma(i, f, i)", InlineLibraryFn::Fma, false),
        ("__builtin_fmaf(i, i, i)", InlineLibraryFn::Fma, true),
    ] {
        let (expr, types, _, _) = parse_expr_with_vars(src, &["i", "f"]).unwrap();
        let want = if float {
            types.float_id
        } else {
            types.double_id
        };
        assert_eq!(expr.typ, Some(want), "{src}");
        let (got, args) = inline_call_args(src, &expr);
        assert_eq!(got, func, "{src}");
        assert_eq!(args.len(), func.arity(), "{src}");
        assert!(args.iter().all(|a| a.typ == Some(want)), "{src}");
    }
}

/// A `float` argument to a `double` rounding is rounded as a `float`, by
/// the `f` form, and the exact answer widened: the call is still a
/// `double`, and still a call to the function the program named, whose
/// definition would displace it. A `double` argument is not narrowed, and a
/// root never is.
#[test]
fn test_rounding_narrows_a_float_argument() {
    let decls = "float f; double d;";
    with_statement_expr(decls, "floor(f)", |p, e| {
        assert_eq!(e.typ, Some(p.types.double_id));
        let (func, arg) = inline_call("floor(f)", e);
        assert_eq!(
            func,
            InlineLibraryFn::RoundToIntegral(IntegralRounding::Floor)
        );
        assert_eq!(arg.typ, Some(p.types.float_id), "not converted to double");
        let ExprKind::InlineLibraryCall { name, narrowed, .. } = e.kind else {
            unreachable!()
        };
        assert_eq!(name, crate::kw::FLOOR, "the function called");
        let narrowed = narrowed.expect("computed by floorf");
        assert_eq!(narrowed.name, crate::kw::FLOORF);
        assert_eq!(narrowed.typ, p.types.float_id);
    });
    with_statement_expr(decls, "__builtin_rint(d)", |p, e| {
        assert_eq!(e.typ, Some(p.types.double_id));
        inline_call("__builtin_rint(d)", e);
        assert!(matches!(
            e.kind,
            ExprKind::InlineLibraryCall { narrowed: None, .. }
        ));
    });
    with_statement_expr(decls, "sqrt(f)", |p, e| {
        let (_, arg) = inline_call("sqrt(f)", e);
        assert_eq!(arg.typ, Some(p.types.double_id), "a root is not narrowed");
    });
}

/// At `-O0` a bare rounding is a call to the function it names, whatever
/// its argument, and a `__builtin_` one is computed in place.
#[test]
fn test_rounding_at_o0() {
    let o0 = super::LibraryCallPolicy {
        optimizing: false,
        math_errno: true,
    };
    let called = |src: &str, vars: &[&str]| {
        let (expr, types, strings, symbols) = parse_expr_under(src, vars, o0).unwrap();
        assert_eq!(expr.typ, Some(types.double_id), "{src}");
        let e = match &expr.kind {
            ExprKind::Cast { expr, .. } => expr.as_ref(),
            _ => &expr,
        };
        let ExprKind::Call { func, .. } = &e.kind else {
            panic!("{src} at -O0 is not a call: {:?}", e.kind);
        };
        let ExprKind::Ident(sym) = func.kind else {
            panic!("{src} at -O0 names no function");
        };
        strings.get(symbols.get(sym).name).to_string()
    };
    assert_eq!(called("ceil(i)", &["i"]), "ceil");
    // The call the program wrote, as gcc makes it: a `float` argument is
    // narrowed only where c17 computes the answer itself.
    assert_eq!(called("ceil(f)", &["f"]), "ceil");
    // Unless the answer is a constant, which it is at every level -- and it
    // is the constant *itself*, not the in-place form: at -O0 nothing folds
    // that form afterwards, and a back end with no instruction for the
    // function lowers it back to the library call, which is how `fmin` came
    // to answer one thing at -O0 and another at -O2. A root's domain error
    // and a direction-dependent `rint` are not constants.
    for (src, want) in [
        ("ceil(2.5)", 3.0),
        ("sqrt(4.0)", 2.0),
        ("fmin(1.0, 2.0)", 1.0),
    ] {
        let (expr, _, _, _) = parse_expr_under(src, &[], o0).unwrap();
        let ExprKind::FloatLit(v) = expr.kind else {
            panic!("{src} at -O0 is not a constant: {:?}", expr.kind);
        };
        assert_eq!(v.to_f64(), want, "{src}");
    }
    assert_eq!(called("sqrt(-1.0)", &[]), "sqrt");
    assert_eq!(called("rint(2.5)", &[]), "rint");
    let (expr, _, _, _) = parse_expr_under("__builtin_ceil(i)", &["i"], o0).unwrap();
    inline_call("__builtin_ceil(i)", &expr);
}

/// A definition of `sqrt` or a rounding above the call displaces the
/// builtin -- an old-style one taking nothing included, whose call takes
/// nothing -- while a definition of `abs` does not.
#[test]
fn test_definition_displaces_a_late_expanded_builtin() {
    fn returned(src: &str) -> ExprKind {
        let (tu, _, _, _) = parse_tu(src).unwrap();
        let Stmt::Return(Some(e)) = first_statement_of(&tu, 1) else {
            panic!("{src}: expected a return");
        };
        e.kind.clone()
    }
    let is_call = |k: &ExprKind| matches!(k, ExprKind::Call { .. });
    assert!(is_call(&returned(
        "double sqrt(double x) { return x; } double f(double y) { return sqrt(y); }"
    )));
    assert!(is_call(&returned(
        "float rintf() { return 1.0f; } float f(void) { return rintf(); }"
    )));
    // A `float` argument asks about the function called, not its `f` form:
    // `floor` is displaced, and `floorf` displaces nothing.
    assert!(is_call(&returned(
        "double floor(double x) { return x; } double f(float y) { return floor(y); }"
    )));
    assert!(matches!(
        returned("float floorf(float x) { return x; } double f(float y) { return floor(y); }"),
        ExprKind::InlineLibraryCall { .. }
    ));
    assert!(matches!(
        returned("int abs(int v) { return v; } int f(int y) { return abs(y); }"),
        ExprKind::InlineLibraryCall { .. }
    ));
    // A weak definition may be replaced at link time; gcc keeps the builtin.
    assert!(matches!(
        returned(
            "__attribute__((weak)) double sqrt(double x) { return x; }\n\
             double f(double y) { return sqrt(y); }"
        ),
        ExprKind::InlineLibraryCall { .. }
    ));
}

/// A declaration of `copysign` keeps the builtin only if both parameters, not
/// just the first, match the library's.
#[test]
fn test_copysign_incompatible_declaration_displaces_builtin() {
    fn returned_is_copysign(src: &str) -> bool {
        let (tu, _, _, _) = parse_tu(src).unwrap();
        matches!(
            first_statement(&tu),
            Stmt::Return(Some(e)) if matches!(
                e.kind,
                ExprKind::InlineLibraryCall { func: InlineLibraryFn::CopySign, .. }
            )
        )
    }
    assert!(returned_is_copysign(
        "double copysign(double, double); double f(void) { return copysign(1, 2); }"
    ));
    assert!(returned_is_copysign(
        "double copysign(); double f(void) { return copysign(1.0, 2.0); }"
    ));
    assert!(!returned_is_copysign(
        "double copysign(double, int); double f(void) { return copysign(1, 2); }"
    ));
    assert!(!returned_is_copysign(
        "double copysign(double); double f(void) { return copysign(1); }"
    ));
    assert!(!returned_is_copysign(
        "double copysign(double, double, ...); double f(void) { return copysign(1, 2); }"
    ));
}

/// A declaration of `abs` with a type incompatible with `int abs(int)` makes
/// it an ordinary function; the compatible declaration keeps the builtin.
#[test]
fn test_int_abs_incompatible_declaration_displaces_builtin() {
    fn returned_is_int_abs(src: &str) -> bool {
        let (tu, _, _, _) = parse_tu(src).unwrap();
        matches!(
            first_statement(&tu),
            Stmt::Return(Some(e)) if matches!(
                e.kind,
                ExprKind::InlineLibraryCall { func: InlineLibraryFn::IntAbs, .. }
            )
        )
    }
    assert!(!returned_is_int_abs(
        "struct S { int a; }; struct S abs(int); struct S f(void) { return abs(1); }"
    ));
    assert!(!returned_is_int_abs(
        "long abs(long); long f(void) { return abs(1); }"
    ));
    assert!(returned_is_int_abs(
        "int abs(int); int f(void) { return abs(1); }"
    ));
    assert!(returned_is_int_abs(
        "extern int abs(const int); int f(void) { return abs(1); }"
    ));
    assert!(returned_is_int_abs(
        "int abs(); int f(void) { return abs(1); }"
    ));
}

/// A bare name that is not being called is an ordinary identifier: here the
/// variable `abs`, which also displaces the builtin.
#[test]
fn test_int_abs_bare_name_not_called_is_an_identifier() {
    let (expr, _, _, _) = parse_expr_with_vars("abs + 1", &["abs"]).unwrap();
    assert!(!matches!(expr.kind, ExprKind::InlineLibraryCall { .. }));
}

/// Each complex accessor, bare or reserved, computes its half or the
/// conjugate in place, of its argument converted to the complex type its
/// suffix names.
#[test]
fn test_complex_accessor_builtins() {
    type Want = fn(&TypeTable) -> crate::types::TypeId;
    use InlineLibraryFn::{ComplexImag, ComplexReal, Conjugate};
    let cases: &[(&str, InlineLibraryFn, Want, Want)] = &[
        (
            "creal(z)",
            ComplexReal,
            |t| t.double_id,
            |t| t.complex_double_id,
        ),
        (
            "__builtin_cimag(z)",
            ComplexImag,
            |t| t.double_id,
            |t| t.complex_double_id,
        ),
        (
            "crealf(z)",
            ComplexReal,
            |t| t.float_id,
            |t| t.complex_float_id,
        ),
        (
            "cimagl(z)",
            ComplexImag,
            |t| t.longdouble_id,
            |t| t.complex_longdouble_id,
        ),
        (
            "conj(z)",
            Conjugate,
            |t| t.complex_double_id,
            |t| t.complex_double_id,
        ),
        (
            "__builtin_conjf(z)",
            Conjugate,
            |t| t.complex_float_id,
            |t| t.complex_float_id,
        ),
    ];
    for (src, want_func, want_typ, want_arg) in cases {
        let code = format!("double _Complex z; void t(void) {{ {src}; }}");
        let (tu, types, _, _) = parse_tu(&code).unwrap();
        let Stmt::Expr(expr) = first_statement(&tu) else {
            panic!("{src}: expected an expression statement");
        };
        let (func, arg) = inline_call(src, expr);
        assert_eq!(func, *want_func, "{src}");
        assert_eq!(expr.typ, Some(want_typ(&types)), "{src}");
        assert_eq!(arg.typ, Some(want_arg(&types)), "{src}");
        // Only a precision other than `double` needs a conversion.
        let converted = want_arg(&types) != types.complex_double_id;
        assert_eq!(
            matches!(arg.kind, ExprKind::Cast { .. }),
            converted,
            "{src}"
        );
    }
}

/// A `creal` declared with some other type is the program's own function.
#[test]
fn test_complex_accessor_incompatible_declaration_displaces_builtin() {
    let code = "struct S { int a; }; struct S creal(int);\n\
                int t(void) { return creal(1).a; }";
    assert!(parse_tu(code).is_ok());
}

/// `memcpy`, `memset`, `memmove`, `mempcpy` and `bcopy`, bare or reserved,
/// are block memory nodes whose arguments are converted as the prototype
/// says, and whose constant length is folded to a `size_t` literal.
#[test]
fn test_memory_builtins() {
    use MemoryFn::{Copy, CopyToEnd, Move, MoveSourceFirst, Set};
    let decls = "char *d; const char *s; int c; unsigned char n;";
    let cases = [
        ("memcpy(d, s, 16)", Copy),
        ("__builtin_memcpy(d, s, 8 + 8)", Copy),
        ("memset(d, c, 16)", Set),
        ("__builtin_memset(d, 'x', 16)", Set),
        ("memmove(d, s, 16)", Move),
        ("__builtin_memmove(d, s, 16)", Move),
        ("mempcpy(d, s, 16)", CopyToEnd),
        ("__builtin_mempcpy(d, s, 16)", CopyToEnd),
        ("bcopy(s, d, 16)", MoveSourceFirst),
        ("__builtin_bcopy(s, d, 16)", MoveSourceFirst),
    ];
    for (stmt, want) in cases {
        with_statement_expr(decls, stmt, |p, e| {
            let t = &*p.types;
            let ExprKind::InlineLibraryCall {
                func: InlineLibraryFn::Memory(mem),
                args,
                ..
            } = &e.kind
            else {
                panic!("{stmt}: expected a block memory call, got {:?}", e.kind);
            };
            assert_eq!(*mem, want, "{stmt}");
            let [first, second, n] = args.as_slice() else {
                panic!("{stmt}: {} arguments", args.len());
            };
            let (ret, first_typ, second_typ) = match mem {
                Copy | Move | CopyToEnd => (t.void_ptr_id, t.void_ptr_id, t.const_void_ptr_id),
                Set => (t.void_ptr_id, t.void_ptr_id, t.int_id),
                MoveSourceFirst => (t.void_id, t.const_void_ptr_id, t.void_ptr_id),
            };
            assert_eq!(e.typ, Some(ret), "{stmt}");
            assert_eq!(first.typ, Some(first_typ), "{stmt}");
            assert_eq!(second.typ, Some(second_typ), "{stmt}");
            assert_eq!(n.typ, Some(t.ulong_id), "{stmt}");
            assert!(
                matches!(n.kind, ExprKind::IntLit(16)),
                "{stmt}: {:?}",
                n.kind
            );
        });
    }
    // A length known only at run time is converted, not folded.
    with_statement_expr(decls, "memcpy(d, s, n)", |p, e| {
        let ExprKind::InlineLibraryCall {
            func: InlineLibraryFn::Memory(MemoryFn::Copy),
            args,
            ..
        } = &e.kind
        else {
            panic!("expected memcpy, got {:?}", e.kind);
        };
        let n = &args[2];
        assert_eq!(n.typ, Some(p.types.ulong_id));
        assert!(matches!(n.kind, ExprKind::Cast { .. }), "{:?}", n.kind);
    });
}

/// The declaration `<string.h>` writes keeps `memcpy` a builtin, `restrict`
/// and all; one of another type, or a non-weak definition, makes it the
/// program's own function.
#[test]
fn test_memory_builtin_declarations() {
    fn is_memcpy(decl: &str) -> bool {
        let src = format!("{decl}\nvoid t(char *d, char *s) {{ memcpy(d, s, 4); }}");
        let (tu, _, _, _) = parse_tu(&src).unwrap();
        // `t`, which follows any definition `decl` makes.
        let t = tu
            .items
            .iter()
            .filter(|item| matches!(item, ExternalDecl::FunctionDef(_)))
            .count()
            - 1;
        matches!(
            first_statement_of(&tu, t),
            Stmt::Expr(e) if matches!(
                e.kind,
                ExprKind::InlineLibraryCall { func: InlineLibraryFn::Memory(MemoryFn::Copy), .. }
            )
        )
    }
    assert!(is_memcpy(""));
    assert!(is_memcpy(
        "void *memcpy(void *restrict, const void *restrict, unsigned long);"
    ));
    assert!(is_memcpy("void *memcpy();"));
    assert!(!is_memcpy("char *memcpy(char *, char *, int);"));
    assert!(!is_memcpy("void *memcpy(void *, void *, unsigned long);"));
    assert!(!is_memcpy(
        "void *memcpy(void *, const void *, unsigned long, ...);"
    ));
    assert!(!is_memcpy(
        "void *memcpy(void *d, const void *s, unsigned long n) { return d; }"
    ));
    assert!(is_memcpy(
        "__attribute__((weak)) void *memcpy(void *d, const void *s, unsigned long n) { return d; }"
    ));
}

/// `bcopy` keeps its builtin under the declaration `<strings.h>` writes,
/// which returns `void` and takes the source first, and loses it to any
/// other declaration or to a definition; `mempcpy` likewise.
#[test]
fn test_bcopy_and_mempcpy_declarations() {
    fn is_builtin(decl: &str, call: &str) -> bool {
        let src = format!("{decl}\nvoid t(char *d, char *s) {{ {call}; }}");
        let (tu, _, _, _) = parse_tu(&src).unwrap();
        let t = tu
            .items
            .iter()
            .filter(|item| matches!(item, ExternalDecl::FunctionDef(_)))
            .count()
            - 1;
        matches!(
            first_statement_of(&tu, t),
            Stmt::Expr(e) if matches!(
                e.kind,
                ExprKind::InlineLibraryCall { func: InlineLibraryFn::Memory(_), .. }
            )
        )
    }
    let bcopy = "bcopy(s, d, 4)";
    assert!(is_builtin("", bcopy));
    assert!(is_builtin(
        "void bcopy(const void *, void *, unsigned long);",
        bcopy
    ));
    assert!(!is_builtin("void bcopy(char *, char *, int);", bcopy));
    assert!(!is_builtin(
        "void *bcopy(const void *, void *, unsigned long);",
        bcopy
    ));
    assert!(!is_builtin(
        "void bcopy(const void *s, void *d, unsigned long n) {}",
        bcopy
    ));
    let mempcpy = "mempcpy(d, s, 4)";
    assert!(is_builtin(
        "void *mempcpy(void *restrict, const void *restrict, unsigned long);",
        mempcpy
    ));
    assert!(!is_builtin("char *mempcpy(char *, char *, int);", mempcpy));
    assert!(!is_builtin(
        "void *mempcpy(void *d, const void *s, unsigned long n) { return d; }",
        mempcpy
    ));
}

/// Parse `decls` and a function `t` whose one statement is `stmt`, then hand
/// that statement's expression to `f` along with the parser that built it.
fn with_statement_expr<R>(decls: &str, stmt: &str, f: impl FnOnce(&mut Parser, &Expr) -> R) -> R {
    let src = format!("{decls}\nvoid t(void) {{ {stmt}; }}");
    let mut strings = StringTable::new();
    let mut tokenizer = Tokenizer::new(src.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(&Target::host());
    let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
    let tu = parser.parse_translation_unit().unwrap();
    let Stmt::Expr(expr) = first_statement(&tu) else {
        panic!("{stmt}: expected an expression statement");
    };
    f(&mut parser, expr)
}

/// A library builtin's result is a value: `creal(z)` is not the object
/// `__real__ z` designates, even though it reads the same half.
#[test]
fn test_library_builtin_result_is_not_an_lvalue() {
    let decls = "double _Complex z; int i; double d;";
    for stmt in [
        "creal(z)",
        "__builtin_cimag(z)",
        "conj(z)",
        "abs(i)",
        "fabs(d)",
    ] {
        let lvalue = with_statement_expr(decls, stmt, |p, e| p.is_lvalue(e));
        assert!(!lvalue, "`{stmt}` is an lvalue");
    }
    for stmt in ["__real__ z", "__imag__ z"] {
        let lvalue = with_statement_expr(decls, stmt, |p, e| p.is_lvalue(e));
        assert!(lvalue, "`{stmt}` is not an lvalue");
    }
}

/// A null pointer constant is an integer constant expression with the value
/// 0, or one cast to `void *` (C17 6.3.2.3p3) -- no other cast, and no
/// integer constant that a pointer was converted to reach (6.6p6).
#[test]
fn test_null_pointer_constants() {
    let decls = "int *p; enum { Z };";
    for (stmt, null) in [
        ("0", true),
        ("0L", true),
        ("'\\0'", true),
        ("1 - 1", true),
        ("Z", true),
        ("(long)0", true),
        ("(int)0.0", true),
        ("(void *)0", true),
        ("(void *)(1 - 1)", true),
        ("sizeof p - sizeof p", true),
        ("1", false),
        ("(void *)1", false),
        ("(char *)0", false),
        ("(int *)0", false),
        ("(const void *)0", false),
        ("(int)(char *)0", false),
        ("(int)(long)(char *)0", false),
        ("(void *)(long)(char *)0", false),
        ("p", false),
    ] {
        let got = with_statement_expr(decls, stmt, |p, e| p.is_null_pointer_constant(e));
        assert_eq!(got, null, "{stmt}");
    }
}

/// `return` is checked by the parser against the enclosing function's
/// declared return type, with the simple-assignment constraints.
#[test]
fn test_return_is_checked_against_the_declared_type() {
    for src in [
        "int f(void) { return; }",
        "void f(void) { return 1; }",
        "void h(void); int f(void) { return h(); }",
        "struct A { int x; }; struct B { int x; }; struct A f(struct B b) { return b; }",
    ] {
        let before = crate::diag::error_count();
        let _ = parse_tu(src);
        assert!(crate::diag::error_count() > before, "{src}: accepted");
    }
    for src in [
        "int *f(void) { return 5; }",
        "int *f(void) { return (char *)0; }",
        "int *f(void) { return (int)(char *)0; }",
    ] {
        let before = crate::diag::warning_count();
        let _ = parse_tu(src);
        assert!(crate::diag::warning_count() > before, "{src}: not warned");
    }
}

/// An argument the prototype rejects, or the wrong number of them, is
/// reported by the ordinary call checks, and the call is not lowered: a zero
/// of the return type stands in for it, so nothing converts a structure to
/// an `int`.
#[test]
fn test_library_builtin_bad_arguments_are_not_lowered() {
    let decls = "struct S { int a; } s; int *p;";
    type Want = fn(&TypeTable) -> crate::types::TypeId;
    let cases: &[(&str, Want)] = &[
        ("abs(s)", |t| t.int_id),
        ("__builtin_fabsf(p)", |t| t.float_id),
        ("creal(s)", |t| t.double_id),
        ("conj(s)", |t| t.complex_double_id),
        ("abs(1, 2)", |t| t.int_id),
        ("abs()", |t| t.int_id),
        ("floor(s)", |t| t.double_id),
    ];
    for (stmt, want) in cases {
        with_statement_expr(decls, stmt, |p, e| {
            assert_eq!(e.typ, Some(want(p.types)), "{stmt}");
            assert!(
                !matches!(
                    e.kind,
                    ExprKind::InlineLibraryCall { .. } | ExprKind::Call { .. }
                ),
                "`{stmt}` was lowered: {:?}",
                e.kind
            );
        });
    }
}

/// The checks are the ordinary call's: they say a call is sound exactly when
/// the arguments fit the prototype, whoever asks.
#[test]
fn test_check_call_answers_soundness() {
    let decls = "struct S { int a; } s; int *p; int i; int F(int);";
    let sound = |call: &str| {
        with_statement_expr(decls, call, |p, e| {
            let ExprKind::Call { func, args, .. } = &e.kind else {
                panic!("{call}: expected a call");
            };
            let func_type = p.resolved_function_type(func);
            p.check_call(func_type, None, args, e.pos)
        })
    };
    assert!(sound("F(i)"));
    assert!(sound("F(p)"), "a pointer to int is a warning, not an error");
    assert!(!sound("F(s)"));
    assert!(!sound("F(1, 2)"));
    assert!(!sound("F()"));
}

/// The one argument of a bit builtin's node: the population count under
/// `parity`'s mask, and the argument of the `ffs` call.
fn bit_builtin_operand(e: &Expr) -> &Expr {
    match &e.kind {
        ExprKind::Bswap16 { arg }
        | ExprKind::Bswap32 { arg }
        | ExprKind::Bswap64 { arg }
        | ExprKind::Ctz { arg }
        | ExprKind::Ctzl { arg }
        | ExprKind::Ctzll { arg }
        | ExprKind::Clz { arg }
        | ExprKind::Clzl { arg }
        | ExprKind::Clzll { arg }
        | ExprKind::Clrsb { arg }
        | ExprKind::Clrsbl { arg }
        | ExprKind::Clrsbll { arg }
        | ExprKind::Popcount { arg }
        | ExprKind::Popcountl { arg }
        | ExprKind::Popcountll { arg } => arg,
        ExprKind::Binary {
            op: BinaryOp::BitAnd,
            left,
            ..
        } => bit_builtin_operand(left),
        ExprKind::Call { args, .. } if args.len() == 1 => &args[0],
        other => panic!("not a bit builtin: {other:?}"),
    }
}

/// A bit builtin's argument converts to its parameter type, as in a call
/// through gcc's prototype for it (C17 6.5.2.2p7): the node reads a
/// converted value, never the argument's own bits.
#[test]
fn test_bit_builtins_convert_their_argument() {
    use crate::types::TypeId;
    let decls = "double d; unsigned u;";
    type Want = fn(&TypeTable) -> TypeId;
    // (call, parameter type, result type)
    let cases: &[(&str, Want, Want)] = &[
        ("__builtin_bswap16(d)", |t| t.ushort_id, |t| t.ushort_id),
        ("__builtin_bswap32(d)", |t| t.uint_id, |t| t.uint_id),
        (
            "__builtin_bswap64(d)",
            |t| t.ulonglong_id,
            |t| t.ulonglong_id,
        ),
        ("__builtin_ctz(d)", |t| t.uint_id, |t| t.int_id),
        ("__builtin_ctzl(d)", |t| t.ulong_id, |t| t.int_id),
        ("__builtin_ctzll(d)", |t| t.ulonglong_id, |t| t.int_id),
        ("__builtin_clz(d)", |t| t.uint_id, |t| t.int_id),
        ("__builtin_clzll(u)", |t| t.ulonglong_id, |t| t.int_id),
        ("__builtin_clrsb(d)", |t| t.int_id, |t| t.int_id),
        ("__builtin_clrsbl(u)", |t| t.long_id, |t| t.int_id),
        ("__builtin_popcount(d)", |t| t.uint_id, |t| t.int_id),
        ("__builtin_popcountl(d)", |t| t.ulong_id, |t| t.int_id),
        ("__builtin_parity(d)", |t| t.uint_id, |t| t.int_id),
        ("__builtin_parityll(u)", |t| t.ulonglong_id, |t| t.int_id),
        ("__builtin_ffs(d)", |t| t.int_id, |t| t.int_id),
        ("__builtin_ffsl(u)", |t| t.long_id, |t| t.int_id),
        ("__builtin_ffsll(d)", |t| t.longlong_id, |t| t.int_id),
    ];
    for (stmt, param, ret) in cases {
        with_statement_expr(decls, stmt, |p, e| {
            assert_eq!(e.typ, Some(ret(p.types)), "{stmt}: result type");
            let arg = bit_builtin_operand(e);
            let want = param(p.types);
            assert_eq!(arg.typ, Some(want), "{stmt}: operand type");
            let ExprKind::Cast { cast_type, .. } = arg.kind else {
                panic!("{stmt}: operand not converted: {:?}", arg.kind);
            };
            assert_eq!(cast_type, want, "{stmt}");
        });
    }
    // An argument of the parameter type is passed as it is.
    with_statement_expr(decls, "__builtin_ctz(u)", |_, e| {
        assert!(matches!(bit_builtin_operand(e).kind, ExprKind::Ident(_)));
    });
}

/// An argument no assignment converts -- a structure -- is an error, as in
/// an ordinary call, and the call is not built: a zero of the result type
/// stands in for it.
#[test]
fn test_bit_builtins_reject_a_structure_argument() {
    let decls = "struct S { int a; } s;";
    for stmt in [
        "__builtin_ctz(s)",
        "__builtin_parity(s)",
        "__builtin_bswap16(s)",
        "__builtin_ffs(s)",
        "__builtin_clz(1, 2)",
    ] {
        let before = crate::diag::error_count();
        with_statement_expr(decls, stmt, |_, e| {
            // The count is process-wide and only grows, so a concurrent test
            // can add to it but never hide this one's error.
            assert!(crate::diag::error_count() > before, "{stmt}: accepted");
            assert!(
                matches!(&e.kind, ExprKind::IntLit(0))
                    || matches!(&e.kind, ExprKind::Cast { expr, .. }
                        if matches!(expr.kind, ExprKind::IntLit(0))),
                "{stmt}: built {:?}",
                e.kind
            );
        });
    }
}

/// A bit builtin of an integer constant expression is one itself, as in gcc,
/// and reads its argument converted to the parameter type: `ctz(-1)` counts
/// the zeros of `UINT_MAX`, `clrsb(-1)` the sign bits of a signed -1. `ctz`
/// and `clz` of 0 are gcc's folded value, the operand width.
#[test]
fn test_bit_builtins_of_constants_are_constant_expressions() {
    for (call, want) in [
        ("__builtin_bswap16(0x12345)", 0x4523),
        ("__builtin_bswap32(0x12345678)", 0x7856_3412),
        (
            "__builtin_bswap64(0x0102030405060708)",
            0x0807_0605_0403_0201,
        ),
        ("__builtin_bswap64(0xff)", 0xff00_0000_0000_0000),
        ("__builtin_ctz(-1)", 0),
        ("__builtin_ctz(1u << 31)", 31),
        ("__builtin_ctzll(1ull << 40)", 40),
        ("__builtin_ctz(0)", 32),
        ("__builtin_ctzl(0)", 64),
        ("__builtin_clz(1)", 31),
        ("__builtin_clz(0x100000000)", 32),
        ("__builtin_clzll(0)", 64),
        ("__builtin_clrsb(0)", 31),
        ("__builtin_clrsb(-1)", 31),
        ("__builtin_clrsb(1)", 30),
        ("__builtin_clrsbll(-5)", 60),
        ("__builtin_popcount(-1)", 32),
        ("__builtin_popcountll(-1)", 64),
        ("__builtin_parity(7)", 1),
        ("__builtin_parityl(3)", 0),
        ("__builtin_ffs(0)", 0),
        ("__builtin_ffs(8)", 4),
        ("__builtin_ffsll(1ll << 40)", 41),
    ] {
        with_statement_expr("", call, |p, e| {
            assert_eq!(p.eval_const_expr(e), Some(want), "{call}");
        });
    }
    // A variable argument is no constant, and `ffs` of one is still a call.
    with_statement_expr("int i;", "__builtin_ffs(i)", |p, e| {
        assert_eq!(p.eval_const_expr(e), None);
        assert!(matches!(e.kind, ExprKind::Call { .. }), "{:?}", e.kind);
    });
}

// __builtin_flt_rounds test

#[test]
fn test_builtin_flt_rounds() {
    // __builtin_flt_rounds() returns 1 (round to nearest, IEEE 754 default)
    let (expr, types, _, _) = parse_expr("__builtin_flt_rounds()").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(1)));
    assert_eq!(expr.typ, Some(types.int_id));
}

// __builtin_expect test

#[test]
fn test_builtin_expect() {
    // __builtin_expect(expr, expected) returns expr
    let (expr, _, _, _) = parse_expr("__builtin_expect(42, 1)").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(42)));
}

#[test]
fn test_builtin_expect_with_expression() {
    // __builtin_expect returns the first argument unchanged
    let (expr, _, strings, symbols) =
        parse_expr_with_vars("__builtin_expect(x, 0)", &["x"]).unwrap();
    match expr.kind {
        ExprKind::Ident(sym_id) => {
            check_name(&strings, symbols.get(sym_id).name, "x");
        }
        _ => panic!("Expected Ident"),
    }
}

// Wide character literal test

#[test]
fn test_wide_char_literal() {
    let (expr, types, _, _) = parse_expr("L'A'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(65)));
    assert_eq!(expr.typ, Some(types.wchar_id));
}

/// An escape in a prefixed literal is bounded by the element type, not by a
/// byte (C17 6.4.4.4p9): `L'\x1234'` is 0x1234 where it was 0x34, and a
/// `wchar_t` unit takes the target's signedness -- `L'\xffffffff'` is -1
/// where `wchar_t` is `int` and 4294967295 where it is `unsigned int`.
#[test]
fn test_prefixed_escapes_keep_their_width() {
    use crate::target::{Arch, Os};
    for (arch, os, all_ones) in [
        (Arch::X86_64, Os::Linux, -1i64),
        (Arch::Aarch64, Os::Linux, 0xffff_ffff),
        (Arch::Aarch64, Os::MacOS, -1),
    ] {
        let parse = |src: &str| {
            let mut strings = StringTable::new();
            let mut tokenizer = Tokenizer::new(src.as_bytes(), 0, &mut strings);
            let tokens = tokenizer.tokenize();
            let mut symbols = SymbolTable::new();
            let mut types = TypeTable::new(&Target::new(arch, os));
            let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
            parser.skip_stream_tokens();
            parser.parse_expression().unwrap().kind
        };
        for (src, want) in [
            ("L'\\x1234'", 0x1234),
            ("L'\\777'", 0o777),
            ("L'\\xffffffff'", all_ones),
            ("u'\\x1234'", 0x1234),
            // Out of range: diagnosed, then the low bits, as gcc keeps.
            ("u'\\x12345'", 0x2345),
            ("U'\\xffffffff'", 0xffff_ffff),
        ] {
            assert!(
                matches!(parse(src), ExprKind::CharLit(v) if v == want),
                "{src} on {arch}-{os}: {:?}",
                parse(src)
            );
        }
        match parse("L\"\\x1234\\xffffffff\\U0001F600\"") {
            ExprKind::WideStringLit(u) => assert_eq!(u, [0x1234, 0xffff_ffff, 0x1f600]),
            other => panic!("{other:?}"),
        }
        // A character beyond the BMP is a surrogate pair in UTF-16; an
        // escaped unit is not a character and is never encoded.
        match parse("u\"\\xd800\\U0001F600\\x12345\"") {
            ExprKind::Utf16StringLit(u) => assert_eq!(u, [0xd800, 0xd83d, 0xde00, 0x2345]),
            other => panic!("{other:?}"),
        }
        match parse("U\"\\xffffffff\\777\"") {
            ExprKind::Utf32StringLit(u) => assert_eq!(u, [0xffff_ffff, 0o777]),
            other => panic!("{other:?}"),
        }
        // A plain piece takes the run's prefix (6.4.5p5), so its escape is a
        // `char16_t` unit, bounded and truncated as one.
        match parse("\"\\x12345\" u\"a\"") {
            ExprKind::Utf16StringLit(u) => assert_eq!(u, [0x2345, 0x61]),
            other => panic!("{other:?}"),
        }
        // Out of range for `char`: diagnosed, then the low eight bits.
        assert!(matches!(parse("'\\x141'"), ExprKind::CharLit(0x41)));
    }
}

/// A prefixed literal takes the target's `wchar_t`, `char16_t` or
/// `char32_t` -- and `wchar_t` is `unsigned int` under AAPCS64, which Linux
/// follows, where it was `int` on every target.
#[test]
fn test_prefixed_literal_types_follow_the_target() {
    use crate::target::{Arch, Os};
    for (arch, os, wchar_unsigned) in [
        (Arch::X86_64, Os::Linux, false),
        (Arch::X86_64, Os::MacOS, false),
        (Arch::Aarch64, Os::Linux, true),
        (Arch::Aarch64, Os::FreeBSD, true),
        (Arch::Aarch64, Os::MacOS, false),
    ] {
        for src in ["L'A'", "L\"ab\"", "u'A'", "u\"ab\"", "U'A'", "U\"ab\""] {
            let mut strings = StringTable::new();
            let mut tokenizer = Tokenizer::new(src.as_bytes(), 0, &mut strings);
            let tokens = tokenizer.tokenize();
            let mut symbols = SymbolTable::new();
            let mut types = TypeTable::new(&Target::new(arch, os));
            let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
            parser.skip_stream_tokens();
            let expr = parser.parse_expression().unwrap();
            let typ = expr.typ.unwrap();
            let elem = types.base_type(typ).unwrap_or(typ);
            let (kind, unsigned) = match src.as_bytes()[0] {
                b'L' => (TypeKind::Int, wchar_unsigned),
                b'u' => (TypeKind::Short, true),
                _ => (TypeKind::Int, true),
            };
            assert_eq!(types.kind(elem), kind, "{src} on {arch}-{os}");
            assert_eq!(types.is_unsigned(elem), unsigned, "{src} on {arch}-{os}");
        }
    }
}

#[test]
fn test_wide_char_escape() {
    let (expr, _, _, _) = parse_expr("L'\\n'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit(10)));
}

// Hex float suffix fix test (f16/f32/f64 are hex digits, not suffixes)

#[test]
fn test_hex_float_not_f16_suffix() {
    // 0x1f16 should be parsed as hex integer 0x1f16, not 0x1 with f16 suffix
    let (expr, types, _, _) = parse_expr("0x1f16").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(0x1f16)));
    // Should be int or long, not float16
    let kind = types.kind(expr.typ.unwrap());
    assert!(matches!(kind, TypeKind::Int | TypeKind::Long));
}

#[test]
fn test_hex_float_not_f32_suffix() {
    // 0xABCf32 should be hex integer, not hex with f32 suffix
    let (expr, types, _, _) = parse_expr("0xABCf32").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(0xabcf32)));
    let kind = types.kind(expr.typ.unwrap());
    assert!(matches!(kind, TypeKind::Int | TypeKind::Long));
}

#[test]
fn test_hex_float_not_f64_suffix() {
    // 0x123f64 should be hex integer
    let (expr, types, _, _) = parse_expr("0x123f64").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(0x123f64)));
    let kind = types.kind(expr.typ.unwrap());
    assert!(matches!(kind, TypeKind::Int | TypeKind::Long));
}

#[test]
fn test_hex_float_with_exponent() {
    // 0x1.0p5 is a valid hex float (uses p exponent, not e)
    let (expr, types, _, _) = parse_expr("0x1.0p5").unwrap();
    assert!(matches!(expr.kind, ExprKind::FloatLit(_)));
    assert_eq!(expr.typ, Some(types.double_id));
}

/// A hex float names a binary value exactly, so a long significand must not be
/// mangled by the accumulator that reads it.
///
/// The significand was read into a `u64` and divided by `1u64 << (4 * digits)`.
/// At 16 fraction digits that shift is 64, which wraps to a shift of 0 in
/// release builds -- so `0x1.0000000000000002p0` came out as **3.0** instead of
/// a value just above 1. Wrong by a factor of three, silently, in a C99
/// feature the conformance matrix listed as passing.
#[test]
fn test_hex_float_long_significand_is_not_mangled() {
    // 16 fraction digits: exactly the width that used to wrap.
    let (expr, _types, _, _) = parse_expr("0x1.0000000000000002p+0").unwrap();
    match expr.kind {
        ExprKind::FloatLit(v) => {
            assert!(
                (v.to_f64() - 1.0).abs() < 1e-15,
                "0x1.0000000000000002p0 is just above 1.0, got {v}"
            );
        }
        other => panic!("expected FloatLit, got {other:?}"),
    }

    // 31 significand digits, all set: 2^124 - 1, which rounds to 2^124 at
    // double precision. Exercises a significand far wider than the mantissa.
    let (expr, _types, _, _) = parse_expr("0xfffffffffffffffffffffffffffffffp0").unwrap();
    match expr.kind {
        ExprKind::FloatLit(v) => assert_eq!(v.to_f64(), f64::powi(2.0, 124), "got {v}"),
        other => panic!("expected FloatLit, got {other:?}"),
    }
}

/// Exponents beyond double's range must not be reached through an infinity.
#[test]
fn test_hex_float_extreme_exponents() {
    for (src, want) in [
        ("0x1p-1074", f64::from_bits(1)), // smallest subnormal
        ("0x1p-1080", 0.0),               // underflows to zero
        ("0x1p+2000", f64::INFINITY),     // overflows to infinity
    ] {
        let (expr, _types, _, _) = parse_expr(src).unwrap();
        match expr.kind {
            ExprKind::FloatLit(v) => assert_eq!(v.to_f64(), want, "{src}"),
            other => panic!("expected FloatLit for {src}, got {other:?}"),
        }
    }
}

/// C17 6.7.6.1: `_Atomic` is a type qualifier, so it belongs in the qualifier
/// run after a `*` exactly like `const`.
///
/// Three copies of that loop had drifted apart and only the one in
/// `parse_declarator` listed `_Atomic`, so `int *_Atomic p;` parsed inside a
/// function and failed at file scope -- and worse, `int *_Atomic;` was
/// *accepted*, because `_Atomic` fell through to the name position and became
/// the identifier. Both file-scope paths (`parse_external_decl` and
/// `parse_function_def`) now share one helper with the declarator's.
#[test]
fn test_atomic_is_a_pointer_qualifier() {
    for src in [
        "int *_Atomic p;",
        "int *const _Atomic p;",
        "int *_Atomic const p;",
        "int *restrict _Atomic p;",
        "static int *_Atomic p;",
    ] {
        let parsed = parse_decl(src);
        assert!(parsed.is_ok(), "{src} should parse: {:?}", parsed.err());
    }
}

/// The qualifier must not be usable as the declared name.
#[test]
fn test_atomic_is_not_an_identifier() {
    assert!(
        parse_decl("int *_Atomic;").is_err(),
        "`int *_Atomic;` names nothing and must be rejected, not treated as a \
         declaration of a variable called _Atomic"
    );
}

// Alignment tests

#[test]
fn test_alignas_on_variable() {
    let (decl, _types, _strings, _symbols) = parse_decl("_Alignas(16) int x;").unwrap();
    assert_eq!(decl.declarators.len(), 1);
    assert_eq!(decl.declarators[0].explicit_align, Some(16));
}

#[test]
fn test_alignas_zero_no_effect() {
    let (decl, _types, _strings, _symbols) = parse_decl("_Alignas(0) int x;").unwrap();
    assert_eq!(decl.declarators.len(), 1);
    assert_eq!(decl.declarators[0].explicit_align, None);
}

#[test]
fn test_multiple_alignas_strictest_wins() {
    let (decl, _types, _strings, _symbols) = parse_decl("_Alignas(8) _Alignas(16) int x;").unwrap();
    assert_eq!(decl.declarators.len(), 1);
    assert_eq!(decl.declarators[0].explicit_align, Some(16));
}

#[test]
fn test_alignas_below_natural_alignment_error() {
    let result = parse_decl("_Alignas(1) int x;");
    assert!(result.is_err());
}

#[test]
fn test_attr_aligned_on_variable() {
    let (decl, _types, _strings, _symbols) =
        parse_decl("int __attribute__((aligned(16))) x;").unwrap();
    assert_eq!(decl.declarators.len(), 1);
    assert_eq!(decl.declarators[0].explicit_align, Some(16));
}

#[test]
fn test_attr_aligned_no_args_defaults_to_16() {
    let (decl, _types, _strings, _symbols) = parse_decl("int __attribute__((aligned)) x;").unwrap();
    assert_eq!(decl.declarators.len(), 1);
    assert_eq!(decl.declarators[0].explicit_align, Some(16));
}

#[test]
fn test_attr_aligned_on_struct_member() {
    let (tu, types, _strings, _symbols) =
        parse_tu("struct S { char a; int __attribute__((aligned(16))) b; char c; }; struct S x;")
            .unwrap();
    // The variable `x` of type `struct S` should have alignment 16
    if let ExternalDecl::Declaration(ref decl) = tu.items[1] {
        let typ = decl.declarators[0].typ;
        assert_eq!(types.alignment(typ), 16);
    }
}

#[test]
fn test_attr_aligned_on_struct_tag() {
    let (tu, types, _strings, _symbols) =
        parse_tu("struct __attribute__((aligned(32))) S { int x; int y; }; struct S var;").unwrap();
    // The variable type should have alignment 32
    if let ExternalDecl::Declaration(ref decl) = tu.items[1] {
        let typ = decl.declarators[0].typ;
        assert_eq!(types.alignment(typ), 32);
    }
}

#[test]
fn test_attr_aligned_on_typedef() {
    let (tu, types, _strings, _symbols) =
        parse_tu("typedef int __attribute__((aligned(16))) aligned_int_t; aligned_int_t x;")
            .unwrap();
    // The variable's type should have alignment 16
    if let ExternalDecl::Declaration(ref decl) = tu.items[1] {
        let typ = decl.declarators[0].typ;
        assert_eq!(types.alignment(typ), 16);
    }
}

#[test]
fn test_combined_alignas_and_attr_aligned() {
    // _Alignas(16) + __attribute__((aligned(32))) — strictest (32) wins
    let (decl, _types, _strings, _symbols) =
        parse_decl("_Alignas(16) int __attribute__((aligned(32))) x;").unwrap();
    assert_eq!(decl.declarators[0].explicit_align, Some(32));
}

#[test]
fn test_aligned_typedef_as_struct_member() {
    let (tu, types, _strings, _symbols) = parse_tu(
        "typedef int __attribute__((aligned(16))) ai_t; \
         struct S { char a; ai_t b; char c; }; struct S x;",
    )
    .unwrap();
    // struct S should inherit alignment 16 from typedef member
    if let ExternalDecl::Declaration(ref decl) = tu.items[2] {
        let typ = decl.declarators[0].typ;
        assert_eq!(types.alignment(typ), 16);
    }
}

/// The layout of the type of the last declaration's first declarator:
/// member offsets, size and alignment.
fn last_struct_layout(src: &str) -> (Vec<usize>, usize, usize) {
    let (tu, types, _strings, _symbols) = parse_tu(src).unwrap();
    let Some(ExternalDecl::Declaration(decl)) = tu.items.last() else {
        panic!("{src}: the last item is not a declaration");
    };
    let typ = decl.declarators[0].typ;
    let composite = types.composite(typ).expect("a struct or union");
    let offsets = composite.members.iter().map(|m| m.offset).collect();
    (offsets, types.size_bytes(typ), types.alignment(typ))
}

/// `packed` on a member drops that member's alignment to 1 wherever the
/// member's declaration writes it, and an `aligned` alongside raises it back.
/// Written among the specifiers it reaches every declarator, as `aligned`
/// does; after a declarator, that declarator alone. Every layout is gcc's.
#[test]
fn test_packed_on_struct_member() {
    for (src, offsets, size, align) in [
        ("struct { char a; int b __attribute__((packed)); } x;", &[0, 1][..], 5, 1),
        ("struct { char a; __attribute__((packed)) int b; } x;", &[0, 1], 5, 1),
        ("struct { char a; int __attribute__((packed)) b; } x;", &[0, 1], 5, 1),
        ("struct { char a; int b __attribute__((packed)), c; } x;", &[0, 1, 8], 12, 4),
        ("struct { char a; __attribute__((packed)) int b, c; } x;", &[0, 1, 5], 9, 1),
        ("struct { char a; __attribute__((aligned(8))) int b, c; } x;", &[0, 8, 16], 24, 8),
        ("struct { char a; int b __attribute__((packed, aligned(2))); } x;", &[0, 2], 6, 2),
        ("struct { char a; int b __attribute__((aligned(1))); } x;", &[0, 4], 8, 4),
        (
            "struct { char a; struct { char x; int y; } s __attribute__((packed)); } x;",
            &[0, 1],
            9,
            1,
        ),
        // A struct-level `packed` leaves a member's own `aligned` in force.
        (
            "struct __attribute__((packed)) { char a; int b __attribute__((aligned(4))); char c; } x;",
            &[0, 4, 8],
            12,
            4,
        ),
        // gcc ignores `packed` on an anonymous member.
        (
            "struct { char a; __attribute__((packed)) struct { char x; int y; }; } x;",
            &[0, 4],
            12,
            4,
        ),
        ("union { char a; int b __attribute__((packed)); } x;", &[0, 0], 4, 1),
    ] {
        assert_eq!(
            last_struct_layout(src),
            (offsets.to_vec(), size, align),
            "{src}"
        );
    }
}

/// `packed` reaches no declaration but the member it is written on: not the
/// struct declared after an object that wrote it, where gcc ignores it, nor
/// the member whose array bound holds a type-name that wrote it.
#[test]
fn test_packed_reaches_only_its_member() {
    for src in [
        "int g __attribute__((packed)); struct T { char a; int b; } t;",
        "struct { char a; int b[sizeof(int __attribute__((packed)))]; } x;",
    ] {
        let (offsets, _, align) = last_struct_layout(src);
        assert_eq!((offsets[1], align), (4, 4), "{src}");
    }
}

/// An `_Alignas` keyword is its own member declaration's: the next member's
/// bit-field must not be rejected for it.
#[test]
fn test_alignas_does_not_reach_the_next_bitfield() {
    let (offsets, size, align) =
        last_struct_layout("struct { _Alignas(8) int a; int b:3; char c; } x;");
    assert_eq!((offsets[2], size, align), (5, 8, 8));
}

/// The argument is an integer constant expression, not one numeric token:
/// a hex or suffixed literal, arithmetic, `sizeof` and a parenthesised value
/// all fold.
#[test]
fn test_attr_aligned_constant_expressions() {
    for (src, align) in [
        ("int __attribute__((aligned(0x40))) x;", 64),
        ("int __attribute__((aligned(16UL))) x;", 16),
        ("int __attribute__((aligned(2 * sizeof(int)))) x;", 8),
        ("int __attribute__((aligned((0x20)))) x;", 32),
        ("int x __attribute__((__aligned__(1 << 5)));", 32),
    ] {
        let (decl, _types, _strings, _symbols) = parse_decl(src).unwrap();
        assert_eq!(decl.declarators[0].explicit_align, Some(align), "{src}");
    }
}

/// An enumerator in `aligned` is its value, not a name the attribute ignores.
#[test]
fn test_attr_aligned_enumerator() {
    let (tu, _types, _strings, _symbols) =
        parse_tu("enum { A = 32 }; int __attribute__((aligned(A))) x;").unwrap();
    let ExternalDecl::Declaration(ref decl) = tu.items[1] else {
        panic!("Expected Declaration");
    };
    assert_eq!(decl.declarators[0].explicit_align, Some(32));
}

/// A `vector_size` past 2^31 bytes still aligns to 16, as gcc's does.
///
/// The width was rounded to a power of two and then cast to `u32` before the
/// cap was applied, so 2^32 bytes and more came out with alignment 0.
#[test]
fn test_attr_vector_size_wide_alignment() {
    if let Err(e) = parse_tu(
        "typedef long V __attribute__((vector_size(8589934592L)));\n\
         _Static_assert(_Alignof(V) == 16, \"align\");\n\
         _Static_assert(sizeof(V) == 8589934592UL, \"size\");",
    ) {
        panic!("should have parsed: {e}");
    }
}

/// `vector_size` folds its argument the same way.
#[test]
fn test_attr_vector_size_constant_expression() {
    let (tu, types, _strings, _symbols) = parse_tu(
        "typedef int V __attribute__((vector_size(2 * sizeof(int)))); \
         typedef float W __attribute__((vector_size(sizeof(float) * 4))); \
         V v; W w;",
    )
    .unwrap();
    for (item, size) in [(2, 8), (3, 16)] {
        let ExternalDecl::Declaration(ref decl) = tu.items[item] else {
            panic!("Expected Declaration");
        };
        assert_eq!(types.size_bytes(decl.declarators[0].typ), size);
    }
}

#[test]
fn test_attr_aligned_nonpow2_ignored() {
    // Non-power-of-2 is diagnosed and not applied
    let (decl, _types, _strings, _symbols) =
        parse_decl("int __attribute__((aligned(3))) x;").unwrap();
    assert_eq!(decl.declarators[0].explicit_align, None);
}

#[test]
fn test_int128_decl() {
    let (decl, types, _strings, _symbols) = parse_decl("__int128 x;").unwrap();
    assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Int128);
    assert!(!types.is_unsigned(decl.declarators[0].typ));
}

#[test]
fn test_int128_t_decl() {
    let (decl, types, _strings, _symbols) = parse_decl("__int128_t x;").unwrap();
    assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Int128);
    assert!(!types.is_unsigned(decl.declarators[0].typ));
}

#[test]
fn test_uint128_t_decl() {
    let (decl, types, _strings, _symbols) = parse_decl("__uint128_t x;").unwrap();
    assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Int128);
    assert!(types.is_unsigned(decl.declarators[0].typ));
}

#[test]
fn test_unsigned_int128_decl() {
    let (decl, types, _strings, _symbols) = parse_decl("unsigned __int128 x;").unwrap();
    assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Int128);
    assert!(types.is_unsigned(decl.declarators[0].typ));
}

#[test]
fn test_signed_int128_decl() {
    let (decl, types, _strings, _symbols) = parse_decl("signed __int128 x;").unwrap();
    assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Int128);
    assert!(!types.is_unsigned(decl.declarators[0].typ));
}

#[test]
fn test_int128_sizeof() {
    let (_, types, _, _) = parse_decl("__int128 x;").unwrap();
    assert_eq!(types.size_bits(types.int128_id), 128);
    assert_eq!(types.size_bits(types.uint128_id), 128);
    assert_eq!(types.alignment(types.int128_id), 16);
    assert_eq!(types.alignment(types.uint128_id), 16);
}

#[test]
fn test_int128_struct_member() {
    let code = "struct s { __uint128_t v[32]; } x;";
    let (decl, types, _strings, _symbols) = parse_decl(code).unwrap();
    let typ = decl.declarators[0].typ;
    assert_eq!(types.kind(typ), TypeKind::Struct);
    // 32 * 16 = 512 bytes
    assert_eq!(types.size_bytes(typ), 512);
}

// C11 `_Generic` type-generic selection (C17 6.5.1.1)

/// `_Generic` is resolved at parse time and the *selected* association's
/// expression is returned verbatim -- there is no `ExprKind::Generic`. These
/// tests assert exactly that, since it is the property everything downstream
/// (constant folding, `cflow`/`cxref` visitors, the linearizer's exhaustive
/// match) relies on.
#[test]
fn test_generic_selects_matching_association() {
    let (expr, _types, _strings, _symbols) =
        parse_expr("_Generic(1, int: 11, double: 22, default: 33)").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(11)));
}

#[test]
fn test_generic_falls_back_to_default() {
    let (expr, _types, _strings, _symbols) =
        parse_expr("_Generic(1.0f, int: 11, double: 22, default: 33)").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(33)));
}

#[test]
fn test_generic_selects_by_float_type() {
    let (expr, _types, _strings, _symbols) =
        parse_expr("_Generic(1.0, int: 11, double: 22, default: 33)").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(22)));
}

/// The result carries the selected expression's own type, not the controlling
/// expression's.
#[test]
fn test_generic_result_type_is_the_selected_arm() {
    let (expr, types, _strings, _symbols) =
        parse_expr("_Generic(1, int: 1.5, default: 2)").unwrap();
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Double);
}

#[test]
fn test_generic_nested() {
    let (expr, _types, _strings, _symbols) =
        parse_expr("_Generic(1, int: _Generic(1.0, double: 7, default: 8), default: 9)").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(7)));
}

/// A `default`-only selection is legal and always chosen.
#[test]
fn test_generic_default_only() {
    let (expr, _types, _strings, _symbols) = parse_expr("_Generic(1, default: 5)").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(5)));
}

/// The controlling expression contributes its type after lvalue conversion,
/// so a qualified type selects the unqualified association (6.5.1.1p2).
#[test]
fn test_generic_strips_qualifiers_from_the_controlling_type() {
    let (expr, _types, _strings, _symbols) =
        parse_expr("_Generic((const int)1, int: 11, default: 22)").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(11)));
}

/// `default` need not come last, and association order does not matter.
#[test]
fn test_generic_default_may_precede_associations() {
    let (expr, _types, _strings, _symbols) =
        parse_expr("_Generic(1, default: 33, int: 11)").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(11)));
}

/// C17 6.7.2p2 lists the declaration specifiers as a *set*: `int long` names
/// the same type as `long int`, and `int short` the same as `short int`.
///
/// The specifier tally settled `base_kind` on whichever of the two arrived
/// first, so `long` reaching an already-`int` tally left the kind at `Int`
/// and produced a four-byte `long`. Only this order broke; every test in the
/// suite spelled the size first.
#[test]
fn test_size_specifier_after_int_still_names_the_size() {
    for (src, want) in [
        ("long int x;", TypeKind::Long),
        ("int long x;", TypeKind::Long),
        ("short int x;", TypeKind::Short),
        ("int short x;", TypeKind::Short),
        ("long long int x;", TypeKind::LongLong),
        ("int long long x;", TypeKind::LongLong),
        ("long int long x;", TypeKind::LongLong),
        ("unsigned int long x;", TypeKind::Long),
        ("int unsigned long x;", TypeKind::Long),
    ] {
        let (decl, types, _strings, _symbols) = parse_decl(src).unwrap();
        let typ = decl.declarators[0].typ;
        assert_eq!(types.kind(typ), want, "{src}");
    }
}

/// A size named twice in either order is still `long long`, and the
/// unsignedness of the spelling survives the promotion of the kind.
#[test]
fn test_size_specifier_order_preserves_signedness() {
    let (decl, types, _strings, _symbols) = parse_decl("int unsigned long x;").unwrap();
    let typ = decl.declarators[0].typ;
    assert_eq!(types.kind(typ), TypeKind::Long);
    assert!(types.modifiers(typ).contains(TypeModifiers::UNSIGNED));
}

/// A `switch` body that is not a compound statement must still contain the
/// statement its `case` label prefixes.
///
/// When a `case` label was a marker carrying the value and not the labeled
/// statement, `switch (x) case 1: return 2;` gave the marker the entire body
/// and made the `return` a *sibling* of the switch -- reached whatever the
/// value of `x`.
#[test]
fn test_switch_with_non_compound_body_keeps_its_labelled_statement() {
    let (func, _types, _strings, _symbols) =
        parse_func("int f(int x) { switch (x) case 1: return 2; return 0; }").unwrap();

    let Stmt::Block(items) = &func.body else {
        panic!("function body is not a block");
    };
    // The switch and the trailing `return 0;` -- and nothing else. The
    // labelled statement must not escape the switch, which is what this has
    // always been about; it now stays because the label *holds* it rather
    // than because a synthetic block was wrapped around the pair.
    assert_eq!(
        items.len(),
        2,
        "the labelled statement escaped the switch: {items:#?}"
    );

    let BlockItem::Statement(first) = &items[0] else {
        panic!("first item is not a statement")
    };
    let Stmt::Switch { body, .. } = &**first else {
        panic!("first item is not a switch: {first:#?}");
    };
    // One statement, as C17 6.8.4 says: the label, carrying its own.
    let Stmt::Labeled {
        labels,
        stmt: labelled,
    } = &**body
    else {
        panic!("switch body is not the case label: {body:#?}");
    };
    assert!(
        matches!(labels.as_slice(), [Label::Case(..)]),
        "{labels:#?}"
    );
    assert!(matches!(**labelled, Stmt::Return(Some(_))), "{labelled:#?}");
}

/// The converse: a body that opens a block, and one that carries no label, are
/// shaped exactly as before. Without these the fix could pass by wrapping
/// every switch body, which would churn the lowering of every existing switch.
#[test]
fn test_switch_bodies_that_need_no_wrapping_are_unchanged() {
    let (func, _types, _strings, _symbols) =
        parse_func("int f(int x) { switch (x) { case 1: return 2; } return 0; }").unwrap();
    let Stmt::Block(items) = &func.body else {
        panic!("not a block")
    };
    let BlockItem::Statement(first) = &items[0] else {
        panic!("first item is not a statement")
    };
    let Stmt::Switch { body, .. } = &**first else {
        panic!("not a switch")
    };
    let Stmt::Block(inner) = &**body else {
        panic!("a braced body should stay a block")
    };
    // One item now -- the label and the statement it carries -- where the
    // label used to be a marker with the statement beside it.
    assert_eq!(inner.len(), 1, "{inner:#?}");

    // No label at all: the single statement is returned verbatim, not wrapped.
    let (func, _types, _strings, _symbols) =
        parse_func("int f(int x) { int n = 0; switch (x) n = 5; return n; }").unwrap();
    let Stmt::Block(items) = &func.body else {
        panic!("not a block")
    };
    let BlockItem::Statement(second) = &items[1] else {
        panic!("second item is not a statement")
    };
    let Stmt::Switch { body, .. } = &**second else {
        panic!("not a switch")
    };
    assert!(
        matches!(**body, Stmt::Expr(_)),
        "an unlabelled body gained a wrapper: {body:#?}"
    );
}

// Libc-alias builtins: the type of the synthesized declaration
//
// These lower to an ordinary call to the same-named library function. When no
// header has declared it -- which is the case the torture suite exercises --
// c17 synthesizes the declaration, and the return type it picks is what the
// call expression carries. A pointer-returning entry typed `int` here
// truncates the returned address to 32 bits at run time.

/// Every pointer-returning libc alias must parse to a pointer-typed call.
#[test]
fn test_library_builtin_pointer_returns() {
    for (call, base) in [
        ("__builtin_strcpy(0, 0)", TypeKind::Char),
        ("__builtin_strncpy(0, 0, 0)", TypeKind::Char),
        ("__builtin_stpcpy(0, 0)", TypeKind::Char),
        ("__builtin_strcat(0, 0)", TypeKind::Char),
        ("__builtin_strncat(0, 0, 0)", TypeKind::Char),
        ("__builtin_strchr(0, 0)", TypeKind::Char),
        ("__builtin_strrchr(0, 0)", TypeKind::Char),
        ("__builtin_strstr(0, 0)", TypeKind::Char),
        ("__builtin_malloc(0)", TypeKind::Void),
        ("__builtin_calloc(0, 0)", TypeKind::Void),
        ("__builtin_realloc(0, 0)", TypeKind::Void),
    ] {
        let (expr, types, _, _) = parse_expr(call).unwrap();
        assert!(
            matches!(expr.kind, ExprKind::Call { .. }),
            "{call} did not lower to a call"
        );
        let typ = expr.typ.unwrap_or_else(|| panic!("{call} has no type"));
        assert_eq!(
            types.kind(typ),
            TypeKind::Pointer,
            "{call} returns {:?}, not a pointer -- a 64-bit address would be truncated",
            types.kind(typ)
        );
        assert_eq!(
            types.kind(types.get(typ).base.unwrap()),
            base,
            "{call} points at the wrong type"
        );
    }
}

/// The integer- and void-returning aliases, for the same reason in reverse:
/// widening `int` to a pointer would be just as wrong.
#[test]
fn test_library_builtin_scalar_returns() {
    for (call, want) in [
        ("__builtin_memcmp(0, 0, 1)", TypeKind::Int),
        ("__builtin_strncmp(0, 0, 1)", TypeKind::Int),
        ("__builtin_printf(0)", TypeKind::Int),
        ("__builtin_sprintf(0, 0)", TypeKind::Int),
        ("__builtin_snprintf(0, 0, 0)", TypeKind::Int),
        ("__builtin_puts(0)", TypeKind::Int),
        ("__builtin_abort()", TypeKind::Void),
        ("__builtin_exit(0)", TypeKind::Void),
        ("__builtin_free(0)", TypeKind::Void),
    ] {
        let (expr, types, _, _) = parse_expr(call).unwrap();
        assert!(
            matches!(expr.kind, ExprKind::Call { .. }),
            "{call} did not lower to a call"
        );
        let typ = expr.typ.unwrap_or_else(|| panic!("{call} has no type"));
        assert_eq!(types.kind(typ), want, "{call} has the wrong return type");
    }
}

/// The printf family is variadic *after* a fixed format argument. Declaring it
/// variadic from argument zero misplaces that argument on Apple arm64, where
/// variadic arguments go on the stack and fixed ones stay in registers.
#[test]
fn test_library_builtin_printf_family_fixed_arity() {
    for (call, fixed) in [
        ("__builtin_printf(0)", 1usize),
        ("__builtin_sprintf(0, 0)", 2),
        ("__builtin_snprintf(0, 0, 0)", 3),
    ] {
        let (expr, types, _, _) = parse_expr(call).unwrap();
        let ExprKind::Call { func, .. } = &expr.kind else {
            panic!("{call} did not lower to a call");
        };
        let ftyp = func.typ.unwrap();
        let info = types.get(ftyp);
        assert!(info.variadic, "{call} must be variadic");
        assert_eq!(
            info.params.as_ref().map(|p| p.len()),
            Some(fixed),
            "{call} has the wrong fixed-argument count"
        );
    }
}

// __complex__ / __complex: gcc's spellings of _Complex
//
// Without them, `__complex__ float f(void)` parses as a declaration naming no
// type and draws the implicit-int diagnostic, which blames the wrong thing.

/// All three spellings produce the same type.
#[test]
fn test_gnu_complex_spellings_agree() {
    let mut seen: Vec<(&str, TypeKind, u32)> = Vec::new();
    for decl in [
        "_Complex double v;",
        "__complex__ double v;",
        "__complex double v;",
    ] {
        let (d, types, _, _) = parse_decl(decl).unwrap_or_else(|e| panic!("{decl}: {e:?}"));
        let typ = d.declarators[0].typ;
        seen.push((decl, types.kind(typ), types.size_bits(typ)));
    }
    let (first_spelling, first_kind, first_bits) = seen[0];
    for (spelling, kind, bits) in &seen[1..] {
        assert_eq!(
            (*kind, *bits),
            (first_kind, first_bits),
            "{spelling} gave a different type than {first_spelling}"
        );
    }
    // And it really is complex, not a bare double that happened to match.
    assert_eq!(first_bits, 128, "_Complex double should be two doubles");
}

/// The GNU spellings carry the COMPLEX modifier in a type-name position too
/// (a cast or a `sizeof`), not only in a declaration.
#[test]
fn test_gnu_complex_in_type_name() {
    for spelling in ["_Complex float", "__complex__ float", "__complex float"] {
        let src = format!("sizeof({spelling})");
        let (expr, types, _, _) = parse_expr(&src)
            .unwrap_or_else(|e| panic!("{spelling} failed to parse as a type name: {e:?}"));
        let ExprKind::SizeofType(typ, _) = expr.kind else {
            panic!("sizeof({spelling}) gave {:?}", expr.kind);
        };
        assert_eq!(
            types.size_bits(typ),
            64,
            "{spelling} in a type name is not a complex float"
        );
    }
}

/// `_Imaginary` is a type specifier c17 provides no type for (C17 6.7.2p2;
/// the types are Annex G's): every use is diagnosed, and the declaration goes
/// on as `_Complex` so nothing after it trips over the same mistake.
///
/// It used to be tagged only as a reserved name, so `_Imaginary double x;`
/// was not a declaration at all: it drew implicit-int and then "keyword
/// cannot be used as a name", neither of which names the actual problem.
#[test]
fn test_imaginary_is_diagnosed_and_recovers_as_complex() {
    for decl in [
        "_Imaginary double v;",
        "double _Imaginary v;",
        "_Imaginary v;",
    ] {
        let before = crate::diag::error_count();
        let (d, types, _, _) = parse_decl(decl).unwrap_or_else(|e| panic!("{decl}: {e:?}"));
        // The count is process-wide and only grows, so a concurrent test can
        // add to it but never hide this one's error.
        assert!(
            crate::diag::error_count() > before,
            "{decl}: _Imaginary was accepted without a diagnostic"
        );
        let typ = d.declarators[0].typ;
        assert!(
            types.modifiers(typ).contains(TypeModifiers::COMPLEX),
            "{decl}: did not recover as a complex type"
        );
    }
    // In a type-name (a cast, `sizeof`) it is always the specifier.
    let before = crate::diag::error_count();
    let (expr, types, _, _) =
        parse_expr("sizeof(float _Imaginary)").unwrap_or_else(|e| panic!("{e:?}"));
    assert!(crate::diag::error_count() > before);
    let ExprKind::SizeofType(typ, _) = expr.kind else {
        panic!("sizeof(float _Imaginary) gave {:?}", expr.kind);
    };
    assert_eq!(types.size_bits(typ), 64);
}

/// Where only a declarator's name can stand, `_Imaginary` is that name, as a
/// typedef name would be: `int _Imaginary;` misuses a keyword, and must say
/// so rather than complain that c17 lacks imaginary types.
#[test]
fn test_imaginary_in_name_position_is_a_keyword_misused() {
    for src in [
        "int _Imaginary;",
        "int _Imaginary = 0;",
        "int _Imaginary[2];",
        "int _Imaginary, y;",
        "void g(int _Imaginary);",
        "int _Imaginary(void);",
        "struct S { int _Imaginary : 3; };",
    ] {
        match parse_tu(src) {
            Err(e) => assert!(
                e.to_string()
                    .contains("'_Imaginary' is a keyword and cannot be used as a name"),
                "{src}: wrong message: {e}"
            ),
            Ok(_) => panic!("{src}: accepted a keyword as a name"),
        }
    }
}

/// `_Complex` applies to the integer types too, and doubles their size.
///
/// The `COMPLEX` size multiplier reached only the floating kinds, so
/// `sizeof(_Complex int)` was 4 -- the width of one half -- and every write
/// of an imaginary part landed one object past the end of the storage.
#[test]
fn test_complex_integer_type_sizes() {
    for (spelling, want_bits) in [
        ("_Complex signed char", 16),
        ("_Complex unsigned char", 16),
        ("_Complex short", 32),
        ("_Complex unsigned short", 32),
        ("_Complex int", 64),
        ("_Complex unsigned", 64),
        ("_Complex unsigned int", 64),
        ("_Complex long", 128),
        ("_Complex unsigned long", 128),
        ("_Complex long long", 128),
        // The GNU spellings mean the same thing here as for floating bases.
        ("__complex__ int", 64),
        ("__complex int", 64),
        // And the floating ones are unchanged.
        ("_Complex float", 64),
        ("_Complex double", 128),
    ] {
        let src = format!("sizeof({spelling})");
        let (expr, types, _, _) =
            parse_expr(&src).unwrap_or_else(|e| panic!("{spelling} did not parse: {e:?}"));
        let ExprKind::SizeofType(typ, _) = expr.kind else {
            panic!("sizeof({spelling}) gave {:?}", expr.kind);
        };
        assert_eq!(
            types.size_bits(typ),
            want_bits,
            "{spelling} is the wrong width"
        );
        assert!(types.is_complex(typ), "{spelling} is not complex");
    }
}

/// An imaginary constant with an integer value is a `_Complex int`.
///
/// These were rejected outright while c17 had no complex integer type --
/// deliberately, rather than being given a floating type, which would have
/// changed what the program computes. Both markers are accepted, and the
/// literal becomes a complex value with a zero real part whose *own* type
/// matches the base's family: giving an integer's real half a `FloatLit` made
/// the constant folder carry an integer complex as two floats.
#[test]
fn test_imaginary_integer_constants() {
    for (src, want_bits) in [
        ("2i", 64),
        ("2j", 64),
        ("2I", 64),
        ("200i", 64),
        ("2uli", 128),
        ("2lli", 128),
    ] {
        let (expr, types, _, _) =
            parse_expr(src).unwrap_or_else(|e| panic!("{src} did not parse: {e:?}"));
        let typ = expr.typ.unwrap_or_else(|| panic!("{src} has no type"));
        assert!(
            types.is_complex_integer(typ),
            "{src} should be a complex *integer*"
        );
        assert_eq!(types.size_bits(typ), want_bits, "{src} width");
        let ExprKind::BuiltinComplex { real, imag } = &expr.kind else {
            panic!("{src} gave {:?}", expr.kind);
        };
        // The real half is an integer zero, not a floating one.
        assert!(
            matches!(real.kind, ExprKind::IntLit(0)),
            "{src}: real half is {:?}",
            real.kind
        );
        assert!(
            matches!(imag.kind, ExprKind::IntLit(_)),
            "{src}: imaginary half is {:?}",
            imag.kind
        );
    }

    // The control: the same numbers without a marker stay real.
    for src in ["2", "200", "2ul", "2ll"] {
        let (expr, types, _, _) =
            parse_expr(src).unwrap_or_else(|e| panic!("{src} did not parse: {e:?}"));
        let typ = expr.typ.unwrap_or_else(|| panic!("{src} has no type"));
        assert!(!types.is_complex(typ), "{src} should not be complex");
    }

    // A floating imaginary constant still gets a floating complex type and a
    // floating zero.
    for src in ["1.0i", "1.0fi", "1.0li"] {
        let (expr, types, _, _) =
            parse_expr(src).unwrap_or_else(|e| panic!("{src} did not parse: {e:?}"));
        let typ = expr.typ.unwrap_or_else(|| panic!("{src} has no type"));
        assert!(
            types.is_complex_float(typ),
            "{src} should be a complex float"
        );
        let ExprKind::BuiltinComplex { real, .. } = &expr.kind else {
            panic!("{src} gave {:?}", expr.kind);
        };
        assert!(
            matches!(real.kind, ExprKind::FloatLit(_)),
            "{src}: real half is {:?}",
            real.kind
        );
    }
}

/// The GNU imaginary marker on a hexadecimal floating constant.
///
/// Only the suffix after the binary exponent may carry the marker; before the
/// `p`, `a`-`f` are digits, so `0xfp0i` is `15i` and not a `float`. The marker
/// combines with `f`/`l` on either side, as it does on a decimal constant.
#[test]
fn test_imaginary_hex_float_constants() {
    for (src, want, base) in [
        ("0x1.8p1i", 3.0, 'd'),
        ("0xfp0i", 15.0, 'd'),
        ("0xFp0I", 15.0, 'd'),
        ("0x1p0fi", 1.0, 'f'),
        ("0x1p-1if", 0.5, 'f'),
        ("0x2p-1iL", 1.0, 'l'),
        ("0x1.8p0Li", 1.5, 'l'),
        ("0xAp0j", 10.0, 'd'),
        ("0x1P+2i", 4.0, 'd'),
    ] {
        let (expr, types, _, _) =
            parse_expr(src).unwrap_or_else(|e| panic!("{src} did not parse: {e:?}"));
        let typ = expr.typ.unwrap_or_else(|| panic!("{src} has no type"));
        assert!(
            types.is_complex_float(typ),
            "{src} should be a complex float"
        );
        let ExprKind::BuiltinComplex { real, imag } = &expr.kind else {
            panic!("{src} gave {:?}", expr.kind);
        };
        assert!(
            matches!(real.kind, ExprKind::FloatLit(v) if v.to_f64() == 0.0),
            "{src}: real half is {:?}",
            real.kind
        );
        assert!(
            matches!(imag.kind, ExprKind::FloatLit(v) if v.to_f64() == want),
            "{src}: imaginary half is {:?}",
            imag.kind
        );
        let want_base = match base {
            'f' => types.float_id,
            'l' => types.longdouble_id,
            _ => types.double_id,
        };
        assert_eq!(imag.typ, Some(want_base), "{src} base type");
    }

    // The control: without a marker, a hex float stays real, and its `f`
    // digits are still digits.
    for (src, want) in [("0xfp0", 15.0), ("0x1.8p1", 3.0), ("0xfp0f", 15.0)] {
        let (expr, types, _, _) =
            parse_expr(src).unwrap_or_else(|e| panic!("{src} did not parse: {e:?}"));
        let typ = expr.typ.unwrap_or_else(|| panic!("{src} has no type"));
        assert!(!types.is_complex(typ), "{src} should not be complex");
        assert!(
            matches!(expr.kind, ExprKind::FloatLit(v) if v.to_f64() == want),
            "{src} gave {:?}",
            expr.kind
        );
    }
}

/// The imaginary marker with every `_FloatN` suffix c17 has a type for, in
/// both orders, as gcc accepts it.
///
/// The marker used to be found in "the trailing run of letters", and the
/// digits of `f16` ended that run, so `2.0if16` was rejected while
/// `2.0f16i` was accepted.
#[test]
fn test_imaginary_marker_with_float_n_suffixes() {
    for (suffix, want) in [
        ("f16", TypeKind::Float16),
        ("F16", TypeKind::Float16),
        ("f32", TypeKind::Float),
        ("f64", TypeKind::Double),
        ("f", TypeKind::Float),
        ("l", TypeKind::LongDouble),
        ("", TypeKind::Double),
    ] {
        for marker in ["i", "j", "I", "J"] {
            for src in [
                format!("2.5{marker}{suffix}"),
                format!("2.5{suffix}{marker}"),
                format!("0x1.4p1{marker}{suffix}"),
                format!("0x1.4p1{suffix}{marker}"),
                format!("25e-1{marker}{suffix}"),
            ] {
                let (expr, types, _, _) =
                    parse_expr(&src).unwrap_or_else(|e| panic!("{src} did not parse: {e:?}"));
                let typ = expr.typ.unwrap();
                assert!(types.is_complex_float(typ), "{src} should be complex");
                let ExprKind::BuiltinComplex { real, imag } = &expr.kind else {
                    panic!("{src} gave {:?}", expr.kind);
                };
                assert!(
                    matches!(real.kind, ExprKind::FloatLit(v) if v.to_f64() == 0.0),
                    "{src}: real half is {:?}",
                    real.kind
                );
                assert!(
                    matches!(imag.kind, ExprKind::FloatLit(v) if v.to_f64() == 2.5),
                    "{src}: imaginary half is {:?}",
                    imag.kind
                );
                assert_eq!(types.kind(imag.typ.unwrap()), want, "{src} base type");
            }
        }
    }
}

/// `f128`/`q` with the marker in both orders, where the target has binary128.
#[test]
fn test_imaginary_marker_with_binary128_suffixes() {
    for src in ["2.0if128", "2.0f128i", "2.0iq", "2.0qi", "0x1p1iF128"] {
        let (expr, types, _, _) = parse_expr_for(src, &x86_64_linux())
            .unwrap_or_else(|e| panic!("{src} did not parse: {e:?}"));
        let ExprKind::BuiltinComplex { imag, .. } = &expr.kind else {
            panic!("{src} gave {:?}", expr.kind);
        };
        assert_eq!(types.kind(imag.typ.unwrap()), TypeKind::Float128, "{src}");
    }
}

/// Where gcc places the marker, and where it does not.
///
/// It may stand between an integer's `u` and its `l`s and on a hex integer,
/// but only once, and never inside another suffix: not between the `f` and
/// the digits of `f16`, not inside `ll`.
#[test]
fn test_imaginary_marker_placement() {
    for (src, want_bits) in [
        ("1uil", 128),
        ("1liu", 128),
        ("1ill", 128),
        ("1llui", 128),
        ("0x1i", 64),
        ("0xfi", 64),
        ("0b1i", 64),
    ] {
        let (expr, types, _, _) =
            parse_expr(src).unwrap_or_else(|e| panic!("{src} did not parse: {e:?}"));
        let typ = expr.typ.unwrap();
        assert!(types.is_complex_integer(typ), "{src} should be complex int");
        assert_eq!(types.size_bits(typ), want_bits, "{src} width");
    }
    for src in [
        "2.0fi16", "2.0f1i6", "2.0f16ij", "2.0iif", "1lil", "1.0ii", "2.0Lif", "1if16", "1f16i",
    ] {
        assert!(parse_expr(src).is_err(), "{src} should be rejected");
    }
}

/// An object no `i32` frame displacement can reach is refused where it is
/// declared.
///
/// C17 sets no limit here; `TypeTable::MAX_STACK_OBJECT_BYTES` is c17's, because
/// both backends address a local and a stacked argument by a signed 32-bit
/// displacement. Before this check every conversion in `arch/` and `abi/` was a
/// bare `as i32` and wrapped: `char a[3000000000];` in a function got an
/// eight-byte slot and a `subq $32, %rsp` frame, with no diagnostic.
///
/// One rule, asked from four places, because an automatic object, a prototyped
/// parameter, a K&R parameter and a compound literal are four different
/// productions bound by four different functions. The three convergence points
/// below them (`Function::add_local`, `Linearizer::insert_local`,
/// `SymbolTable::declare`) all lack a `Position`, and the first two also see
/// compiler temporaries that have no declaration to point at.
#[test]
fn test_stack_object_larger_than_a_frame_slot_is_rejected() {
    for src in [
        // An automatic object, in each of the scopes that can declare one.
        "int f(void){ char a[3000000000]; return a[0]; }",
        "int f(void){ int a[600000000]; return a[0]; }",
        "int f(void){ struct S { char x[3000000000]; } s; return s.x[0]; }",
        "int f(void){ register char a[3000000000]; return a[0]; }",
        "int f(void){ { { char a[3000000000]; return a[0]; } } }",
        "int f(void){ for (char a[3000000000];;) return a[0]; }",
        // A by-value parameter, prototyped and unnamed and nested.
        "struct S { char x[3000000000]; }; int f(struct S s);",
        "struct S { char x[3000000000]; }; void f(struct S);",
        "struct S { char x[3000000000]; }; int f(struct S s){ return s.x[0]; }",
        "struct S { char x[3000000000]; }; int (*fp)(struct S);",
        "struct S { char x[3000000000]; }; int f(int, struct S, int);",
        // A K&R parameter, whose real type arrives after the identifier list.
        "struct S { char x[3000000000]; }; int f(a) struct S a; { return a.x[0]; }",
        // A compound literal inside a function: automatic storage duration by
        // C17 6.5.2.5p5, and not a declaration.
        "struct S { char x[3000000000]; };\n\
         void sink(struct S *); void f(void){ sink(&(struct S){0}); }",
    ] {
        match parse_tu(src) {
            Err(e) => assert!(
                e.to_string().contains("maximum stack object size"),
                "{src}\nwas rejected, but not for its size: {e}"
            ),
            Ok(_) => panic!("{src}\nshould have been rejected"),
        }
    }
}

/// Static storage duration is not the frame's problem.
///
/// The guard that keeps the new ceiling from becoming a second, tighter
/// `max_object_bytes`. `array_parameter_decays` and the pointer case are the two
/// that break if the parameter check is moved before the C17 6.7.5.3 adjustment:
/// that parameter is a `char *`. A variable length array has no static extent to
/// measure, so the rule declines to answer: its extent is subtracted from the
/// stack pointer at run time, and the frame holds only a pointer to it.
#[test]
fn test_static_object_larger_than_a_frame_slot_is_accepted() {
    for src in [
        "char big[3000000000];",
        "int f(void){ static char big[3000000000]; return big[0]; }",
        "int f(void){ extern char big[3000000000]; return big[0]; }",
        "_Thread_local char big[3000000000];",
        "int f(char a[3000000000]){ return a[0]; }",
        "struct S { char x[3000000000]; }; int f(struct S *p){ return p->x[0]; }",
        "typedef char T[3000000000]; unsigned long f(void){ return sizeof(T); }",
        "int f(int n){ char a[n]; return a[0]; }",
        "int f(int n){ char a[n][3000000000]; return a[0][0]; }",
        "int f(void){ char a[1000000000]; return a[0]; }",
    ] {
        if let Err(e) = parse_tu(src) {
            panic!("{src}\nshould have parsed: {e}");
        }
    }
}

/// An object may be as large as `PTRDIFF_MAX`, gcc's bound, and `sizeof` and
/// every spelling of a member offset fold exactly up to it.
///
/// The bound was `u64::MAX / 8` because struct layout accumulated its bits in
/// a `usize`, so gcc.c-torture `991014-1` -- a struct of 2^63 - 496 bytes --
/// was refused. Each `_Static_assert` below is gcc's answer.
#[test]
fn test_object_up_to_ptrdiff_max_is_accepted() {
    let src = "\
        struct H { short buf[(1L << 62) - 256]; int a, b, c, d; };\n\
        union U { int a; char buf[(1L << 62) - 256]; };\n\
        struct B { char pad[5000000000L]; int x; unsigned f : 3, g : 5; long y; };\n\
        struct E { char buf[9223372036854775807L]; };\n\
        _Static_assert(sizeof(struct H) == 9223372036854775312UL, \"H\");\n\
        _Static_assert(sizeof(union U) == 4611686018427387648UL, \"U\");\n\
        _Static_assert(sizeof(struct E) == 9223372036854775807UL, \"E\");\n\
        _Static_assert(__builtin_offsetof(struct H, d) == 9223372036854775308UL, \"d\");\n\
        _Static_assert((unsigned long)&((struct H *)0)->b == 9223372036854775300UL, \"b\");\n\
        _Static_assert(__builtin_offsetof(struct B, y) == 5000000008UL, \"y\");\n\
        _Static_assert(sizeof(struct B) == 5000000016UL, \"B\");\n\
        _Static_assert(sizeof(struct B[3]) == 15000000048UL, \"B[3]\");\n\
        _Static_assert(sizeof(char[9223372036854775807L]) == 9223372036854775807UL, \"max\");\n";
    if let Err(e) = parse_tu(src) {
        panic!("should have parsed: {e}");
    }
}

/// One byte past `PTRDIFF_MAX` is refused, in each of the shapes that can
/// reach it, and the message names the bound.
#[test]
fn test_object_past_ptrdiff_max_is_rejected() {
    for (src, needle) in [
        (
            "char a[9223372036854775808UL];",
            "size of array is too large",
        ),
        ("int a[2305843009213693952L];", "size of array is too large"),
        (
            "char a[4000000000L][4000000000L];",
            "size of array is too large",
        ),
        (
            "struct H { short buf[(1L << 62) - 256]; int a; }; struct H h[2];",
            "size of array is too large",
        ),
        (
            "struct S { char a[9223372036854775807L]; int b; };",
            "type 'struct S' is too large",
        ),
        (
            "struct S { char a[9223372036854775807L]; char b; };",
            "type 'struct S' is too large",
        ),
        (
            "struct S { char a[9223372036854775807L]; char b[9223372036854775807L]; \
             char c[9223372036854775807L]; };",
            "type 'struct S' is too large",
        ),
        (
            "union U { char a[9223372036854775807L]; int b; };",
            "type 'union U' is too large",
        ),
        (
            "struct { char a[9223372036854775807L]; int b; } s;",
            "type 'struct <anonymous>' is too large",
        ),
    ] {
        match parse_tu(src) {
            Err(e) => {
                let e = e.to_string();
                assert!(e.contains(needle), "{src}\nwrong message: {e}");
                assert!(
                    e.contains("maximum object size of 9223372036854775807 bytes"),
                    "{src}\nmessage does not name the bound: {e}"
                );
            }
            Ok(_) => panic!("{src}\nshould have been rejected"),
        }
    }
}

/// The level of `__builtin_frame_address`/`__builtin_return_address` is folded
/// at parse time, so the AST carries the number of frames to walk rather than
/// an expression the backend would have to evaluate.
#[test]
fn test_frame_builtin_level_is_a_parsed_constant() {
    let (expr, ..) = parse_expr("__builtin_return_address(1 + 1)").unwrap();
    assert!(matches!(expr.kind, ExprKind::ReturnAddress { level: 2 }));
    let (expr, ..) = parse_expr("__builtin_frame_address(0)").unwrap();
    assert!(matches!(expr.kind, ExprKind::FrameAddress { level: 0 }));
}

/// gcc rejects a level that is not a non-negative integer constant.
#[test]
fn test_frame_builtin_level_must_be_constant() {
    for src in [
        "__builtin_return_address(n)",
        "__builtin_frame_address(n)",
        "__builtin_return_address(-1)",
    ] {
        match parse_expr_with_vars(src, &["n"]) {
            Err(err) => assert!(
                err.message.contains("invalid argument"),
                "{src}: {}",
                err.message
            ),
            Ok(_) => panic!("{src} should be rejected"),
        }
    }
}

/// `aligned` on a function reaches the definition's attributes from every
/// place it can be written -- a prototype's trailing attribute, the position
/// before the specifiers, the definition -- and the largest wins.
#[test]
fn test_function_aligned_attribute_is_gathered() {
    let src = "void a(void) __attribute__((aligned(256)));\n\
               void a(void) {}\n\
               __attribute__((aligned(64))) void b(void) {}\n\
               void c(void) __attribute__((aligned(128)));\n\
               __attribute__((aligned(32))) void c(void) {}\n\
               void d(void) {}\n";
    let (tu, _types, strings, _symbols) = parse_tu(src).unwrap();
    let aligns: Vec<(String, Option<u32>)> = tu
        .items
        .iter()
        .filter_map(|item| match item {
            ExternalDecl::FunctionDef(f) => Some((strings.get(f.name).to_string(), f.attrs.align)),
            _ => None,
        })
        .collect();
    assert_eq!(
        aligns,
        [
            ("a".to_string(), Some(256)),
            ("b".to_string(), Some(64)),
            ("c".to_string(), Some(128)),
            ("d".to_string(), None),
        ]
    );
}

/// `__alignof__` of an aligned function is a constant the parser folds.
#[test]
fn test_alignof_of_an_aligned_function() {
    let src = "void f(void) __attribute__((aligned(512)));\n\
               unsigned long n = __alignof__(f);\n";
    let (tu, ..) = parse_tu(src).unwrap();
    let init = tu.items.iter().find_map(|item| match item {
        ExternalDecl::Declaration(d) => d.declarators.first()?.init.clone(),
        _ => None,
    });
    let init = init.expect("n has an initializer");
    assert!(
        matches!(init.kind, ExprKind::IntLit(512)),
        "{:?}",
        init.kind
    );
}

/// `__builtin_signbit` reads the sign at its argument's own type, a
/// `_Float16` widened to `float`; `__builtin_signbitf` and
/// `__builtin_signbitl` convert theirs to the type they name. Each is a bit
/// test, never a call.
#[test]
fn test_signbit_is_type_generic() {
    let (tu, types, _, _) = parse_tu(
        "float f; double d; long double l; _Float16 h;\n\
         int a = 0; void t(void) { a = __builtin_signbit(f); a = __builtin_signbit(d); \
         a = __builtin_signbit(l); a = __builtin_signbit(h); a = __builtin_signbitf(d); \
         a = __builtin_signbitl(f); }\n",
    )
    .unwrap();
    let body = tu
        .items
        .iter()
        .find_map(|i| match i {
            ExternalDecl::FunctionDef(f) => Some(&f.body),
            _ => None,
        })
        .unwrap();
    let Stmt::Block(items) = body else {
        panic!("expected a block")
    };
    let rhs: Vec<&Expr> = items
        .iter()
        .filter_map(|i| match i {
            BlockItem::Statement(s) => match s.as_ref() {
                Stmt::Expr(e) => match &e.kind {
                    ExprKind::Assign { value, .. } => Some(value.as_ref()),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        })
        .collect();
    let want = [
        types.float_id,
        types.double_id,
        types.longdouble_id,
        types.float_id,
        types.float_id,
        types.longdouble_id,
    ];
    assert_eq!(rhs.len(), want.len());
    for (i, (e, typ)) in rhs.iter().zip(want).enumerate() {
        assert_eq!(e.typ, Some(types.int_id), "#{i}");
        match &e.kind {
            ExprKind::FpTest {
                test: FpTest::SignBit,
                arg,
            } => assert_eq!(arg.typ, Some(typ), "#{i}: the operand's type"),
            other => panic!("#{i}: expected a sign-bit test, got {other:?}"),
        }
    }
}

/// `vector_size` marks its type, so a vector is not the same type as a plain
/// array of its elements -- which is what lets a value use of one be told
/// apart and refused.
#[test]
fn test_vector_size_type_is_marked() {
    let (decl, types, _strings, symbols) =
        parse_decl("int __attribute__((vector_size(8))) x;").unwrap();
    let typ = symbols.get(decl.declarators[0].symbol).typ;
    assert!(types.is_vector(typ));
    let (decl, types, _strings, symbols) = parse_decl("int y[2];").unwrap();
    assert!(!types.is_vector(symbols.get(decl.declarators[0].symbol).typ));
}

/// `mode` and `vector_size` are recognised by their `SUPPORTED_ATTR` tag like
/// every other attribute: both spellings apply, and a spelling the table does
/// not list is warned about as ignored and is ignored.
#[test]
fn test_attr_mode_and_vector_size_follow_the_tag() {
    for (src, size, vector) in [
        ("int __attribute__((mode(QI))) x;", 1, false),
        ("int __attribute__((__mode__(__QI__))) x;", 1, false),
        ("int __attribute__((__mode(QI))) x;", 4, false),
        ("int __attribute__((vector_size(8))) x;", 8, true),
        ("int __attribute__((__vector_size__(8))) x;", 8, true),
        ("int __attribute__((__vector_size(8))) x;", 4, false),
    ] {
        let (decl, types, _strings, symbols) = parse_decl(src).unwrap();
        let typ = symbols.get(decl.declarators[0].symbol).typ;
        assert_eq!(types.size_bytes(typ), size, "{src}");
        assert_eq!(types.is_vector(typ), vector, "{src}");
    }
}

/// Two tagless definitions with the same members are distinct types, while a
/// qualified variant of one stays compatible with it (C17 6.7.2.3p5).
#[test]
fn test_tagless_composites_have_identity() {
    let (tu, types, _strings, symbols) =
        parse_tu("struct { int x; } a;\nstruct { int x; } b;\ntypedef struct { int x; } T;\nconst T c;\nT d;\n")
            .unwrap();
    let typ = |i: usize| match &tu.items[i] {
        ExternalDecl::Declaration(decl) => symbols.get(decl.declarators[0].symbol).typ,
        _ => panic!("item {i} is not a declaration"),
    };
    let (a, b) = (typ(0), typ(1));
    assert!(
        !types.types_compatible(a, b),
        "two tagless definitions must be distinct"
    );
    // `types_compatible` ignores top-level qualifiers.
    let (c, d) = (typ(3), typ(4));
    assert!(
        types.types_compatible(c, d),
        "const T and T share a definition"
    );
}

/// Every declarator of a list gets the attributes written among the
/// specifiers, and only its own trailing ones -- gcc's rule. A function
/// declarator after the first, or at block scope, has its attributes
/// recorded like the first one's: `g` and `k` below lost their `aligned`,
/// `a2` lost the specifiers' one, and `wb` its `weak`.
#[test]
fn test_attributes_follow_their_declarator_in_a_list() {
    let src = "void f(void), g(void) __attribute__((aligned(32)));\n\
               __attribute__((aligned(16))) void a1(void), a2(void);\n\
               void b1(void) __attribute__((aligned(64))), b2(void);\n\
               __attribute__((weak)) int wa, wb;\n\
               void f(void) {} void g(void) {} void a1(void) {} void a2(void) {}\n\
               void b1(void) {} void b2(void) {}\n\
               void outer(void) { void k(void) __attribute__((aligned(128))), m(void); }\n\
               void k(void) {} void m(void) {}\n";
    let (tu, _types, strings, symbols) = parse_tu(src).unwrap();
    let aligns: std::collections::BTreeMap<String, Option<u32>> = tu
        .items
        .iter()
        .filter_map(|item| match item {
            ExternalDecl::FunctionDef(f) => Some((strings.get(f.name).to_string(), f.attrs.align)),
            _ => None,
        })
        .collect();
    let expect: std::collections::BTreeMap<String, Option<u32>> = [
        ("f", None),
        ("g", Some(32)),
        ("a1", Some(16)),
        ("a2", Some(16)),
        ("b1", Some(64)),
        ("b2", None),
        ("outer", None),
        ("k", Some(128)),
        ("m", None),
    ]
    .into_iter()
    .map(|(n, a)| (n.to_string(), a))
    .collect();
    assert_eq!(aligns, expect);

    let weak: Vec<(String, bool)> = tu
        .items
        .iter()
        .filter_map(|item| match item {
            ExternalDecl::Declaration(d) => Some(d.declarators.iter()),
            _ => None,
        })
        .flatten()
        .map(|d| {
            let name = strings.get(symbols.get(d.symbol).name).to_string();
            (name, d.symbol_attrs.weak)
        })
        .filter(|(n, _)| n.starts_with('w'))
        .collect();
    assert_eq!(weak, [("wa".to_string(), true), ("wb".to_string(), true)]);
}

/// `alias("target")` is a symbol attribute like `weak`: it reaches the
/// declarator it is written on and no other, in either spelling.
#[test]
fn test_alias_attribute_reaches_its_declarator() {
    let (tu, _types, strings, symbols) = parse_tu(
        "int a;\n\
         extern int b __attribute__((alias(\"a\"))), c;\n\
         int f(void) __attribute__((__alias__(\"g\")));\n",
    )
    .unwrap();
    let got: Vec<(String, Option<String>)> = tu
        .items
        .iter()
        .filter_map(|item| match item {
            ExternalDecl::Declaration(d) => Some(d.declarators.iter()),
            _ => None,
        })
        .flatten()
        .map(|d| {
            let name = strings.get(symbols.get(d.symbol).name).to_string();
            (name, d.symbol_attrs.alias.clone())
        })
        .collect();
    let want: Vec<(String, Option<String>)> =
        [("a", None), ("b", Some("a")), ("c", None), ("f", Some("g"))]
            .into_iter()
            .map(|(n, t)| (n.to_string(), t.map(str::to_string)))
            .collect();
    assert_eq!(got, want);
}

/// The expression `fname`'s body returns in its first statement.
fn returned_expr<'t>(tu: &'t TranslationUnit, strings: &StringTable, fname: &str) -> &'t Expr {
    let body = tu
        .items
        .iter()
        .find_map(|item| match item {
            ExternalDecl::FunctionDef(f) if strings.get(f.name) == fname => Some(&f.body),
            _ => None,
        })
        .unwrap_or_else(|| panic!("no definition of {fname}"));
    let Stmt::Block(items) = body else {
        panic!("{fname}: body is not a block");
    };
    let Some(BlockItem::Statement(stmt)) = items.first() else {
        panic!("{fname}: expected a statement");
    };
    let Stmt::Return(Some(expr)) = &**stmt else {
        panic!("{fname}: expected a return statement");
    };
    expr
}

/// The binding and library tag of the call `fname` returns.
fn returned_call(
    tu: &TranslationUnit,
    strings: &StringTable,
    fname: &str,
) -> (CalleeBinding, Option<LibFn>) {
    let ExprKind::Call { binding, known, .. } = &returned_expr(tu, strings, fname).kind else {
        panic!("{fname}: expected a call");
    };
    (*binding, *known)
}

/// A library function spelled `__builtin_X` is a call to the library's `X`;
/// the same function called by its own name is a call to whatever the unit
/// declares, which may be an inline definition. Both are known to call it.
#[test]
fn test_library_builtin_call_binds_to_the_library() {
    let (tu, _types, strings, _symbols) = parse_tu(
        "char *strncpy(char *, const char *, unsigned long);\n\
         char *lib(char *d) { return __builtin_strncpy(d, d, 1); }\n\
         char *own(char *d) { return strncpy(d, d, 1); }\n",
    )
    .unwrap();
    assert_eq!(
        returned_call(&tu, &strings, "lib"),
        (CalleeBinding::Library, Some(LibFn::Strncpy))
    );
    assert_eq!(
        returned_call(&tu, &strings, "own"),
        (CalleeBinding::Declared, Some(LibFn::Strncpy))
    );
}

/// A call to a library function the optimizer knows is tagged with it
/// where the name still means that function: declared with the library's
/// prototype, by its old spelling (`index`), with a `FILE *` of the
/// program's own, and even past a definition in the unit, since defining a
/// reserved name is undefined (C17 7.1.3p2) and gcc folds `__printf_chk`
/// past one.
#[test]
fn test_known_library_call_is_tagged() {
    let (tu, _types, strings, _symbols) = parse_tu(
        "unsigned long strlen(const char *);\n\
         char *index(const char *, int);\n\
         struct F;\n\
         int fputs(const char *restrict, struct F *restrict);\n\
         int __printf_chk(int flag, const char *fmt, ...) { return flag; }\n\
         unsigned long len(void) { return strlen(\"ab\"); }\n\
         char *ix(const char *s) { return index(s, 'a'); }\n\
         int put(struct F *f) { return fputs(\"x\", f); }\n\
         int chk(void) { return __printf_chk(1, \"%d\", 2); }\n",
    )
    .unwrap();
    let known = |f| returned_call(&tu, &strings, f).1;
    assert_eq!(known("len"), Some(LibFn::Strlen));
    assert_eq!(known("ix"), Some(LibFn::Strchr));
    assert_eq!(known("put"), Some(LibFn::Fputs));
    assert_eq!(known("chk"), Some(LibFn::PrintfChk));
}

/// Where the name is the program's, the call is an ordinary one: a
/// prototype that is not the library's (a different parameter, or a `...`
/// the library does not have), and a call through a pointer rather than by
/// name.
#[test]
fn test_known_library_call_is_not_tagged_where_the_name_is_the_programs() {
    let (tu, _types, strings, _symbols) = parse_tu(
        "int strlen(int);\n\
         int puts(const char *, ...);\n\
         unsigned long strnlen(const char *, unsigned long);\n\
         int len(void) { return strlen(3); }\n\
         int put(void) { return puts(\"x\", 1); }\n\
         unsigned long ptr(void) { return (&strnlen)(\"ab\", 1); }\n\
         unsigned long own(void) { return strnlen(\"ab\", 1); }\n",
    )
    .unwrap();
    let known = |f| returned_call(&tu, &strings, f).1;
    assert_eq!(known("len"), None);
    assert_eq!(known("put"), None);
    assert_eq!(known("ptr"), None);
    assert_eq!(known("own"), Some(LibFn::Strnlen));
}

/// The declaration a builtin reaches for must be a function.
///
/// `int __clear_cache;` binds a reserved name to an object, so there is no
/// function of that name to call. Reusing the object's symbol made
/// `__builtin___clear_cache` a call through its own storage; declining
/// leaves the diagnostic the caller already had. This is the test
/// `builtin_is_shadowed` ends with, applied where a builtin declares the
/// library function rather than where a bare call looks one up.
#[test]
fn test_library_builtin_declines_a_name_bound_to_an_object() {
    let body = "void f(char *a, char *b) { __builtin___clear_cache(a, b); }\n";
    assert!(
        parse_tu(&format!("int __clear_cache;\n{body}")).is_err(),
        "`int __clear_cache;` is an object, so __builtin___clear_cache has \
         no function to call and must not reuse the object's symbol"
    );
    // The control: with the name left alone, the builtin still declares it.
    assert!(
        parse_tu(body).is_ok(),
        "with nothing shadowing it, __builtin___clear_cache must still parse"
    );
}

/// `__builtin_puts` with no declaration of `puts` in scope declares it with
/// the library's own prototype -- so its arguments are checked and
/// converted, and the call is known.
#[test]
fn test_undeclared_known_builtin_gets_the_library_prototype() {
    let (tu, types, strings, symbols) =
        parse_tu("int f(void) { return __builtin_puts(\"hi\"); }\n").unwrap();
    assert_eq!(
        returned_call(&tu, &strings, "f"),
        (CalleeBinding::Library, Some(LibFn::Puts))
    );
    let ExprKind::Call { func, .. } = &returned_expr(&tu, &strings, "f").kind else {
        unreachable!("returned_call found a call");
    };
    let ExprKind::Ident(puts) = func.kind else {
        panic!("expected a call by name");
    };
    let ft = types.get(symbols.get(puts).typ);
    assert_eq!(ft.base, Some(types.int_id));
    assert_eq!(ft.params.as_deref(), Some(&[types.const_char_ptr_id][..]));
    assert!(!ft.variadic);
}

/// `strncmp` or `memcmp` of the constant length 0 is 0 as it is parsed, at
/// every level, with each argument still evaluated; any other length, and
/// a function that is not one of the two, stays a call.
#[test]
fn test_zero_length_compare_is_zero_with_its_arguments_evaluated() {
    let (tu, _types, strings, _symbols) = parse_tu(
        "typedef unsigned long size_t;\n\
         int strncmp(const char *, const char *, size_t);\n\
         int memcmp(const void *, const void *, size_t);\n\
         int n(const char *p) { return strncmp(p++, \"x\", 0); }\n\
         int m(const char *p) { return __builtin_memcmp(p, p, 2 - 2); }\n\
         int one(const char *p) { return strncmp(p, \"x\", 1); }\n",
    )
    .unwrap();
    for f in ["n", "m"] {
        let ExprKind::Comma(parts) = &returned_expr(&tu, &strings, f).kind else {
            panic!("{f}: expected the arguments, then 0");
        };
        assert_eq!(parts.len(), 4, "{f}");
        assert!(matches!(parts[3].kind, ExprKind::IntLit(0)), "{f}");
    }
    assert_eq!(returned_call(&tu, &strings, "one").1, Some(LibFn::Strncmp));
}

/// A statement expression whose last statement is a labeled expression
/// statement takes that expression's value, as gcc does (compile/pr17913).
/// The labels stay where they were, labelling an empty statement.
#[test]
fn test_stmt_expr_labeled_last_statement_has_its_value() {
    let (expr, types, _, _) = parse_expr("({ a: b: 5; })").unwrap();
    let ExprKind::StmtExpr { stmts, result } = &expr.kind else {
        panic!("expected StmtExpr");
    };
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Int);
    assert!(matches!(result.kind, ExprKind::IntLit(5)));
    let [BlockItem::Statement(stmt)] = stmts.as_slice() else {
        panic!("expected one labeled statement: {stmts:#?}");
    };
    let Stmt::Labeled { labels, stmt } = &**stmt else {
        panic!("expected a labeled statement: {stmt:#?}");
    };
    assert!(matches!(
        labels.as_slice(),
        [Label::Named { .. }, Label::Named { .. }]
    ));
    assert!(matches!(**stmt, Stmt::Empty));
}

/// A `case` or `default` label in front of the last statement is a label like
/// any other: the statement expression keeps its value, so the one error such
/// a program earns is "switch jumps into statement expression", not a second
/// one about a `void` value.
#[test]
fn test_stmt_expr_case_labeled_last_statement_has_its_value() {
    for src in ["({ case 1: 5; })", "({ default: 5; })"] {
        let (expr, types, _, _) = parse_expr(src).unwrap();
        let ExprKind::StmtExpr { stmts, result } = &expr.kind else {
            panic!("expected StmtExpr");
        };
        assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Int, "{src}");
        assert!(matches!(result.kind, ExprKind::IntLit(5)), "{src}");
        let [BlockItem::Statement(label)] = stmts.as_slice() else {
            panic!("{src}: expected one label");
        };
        assert!(
            matches!(&**label, Stmt::Labeled { labels, stmt }
                if matches!(labels.as_slice(), [Label::Case(..) | Label::Default(_)])
                    && matches!(**stmt, Stmt::Empty)),
            "{src}"
        );
    }
}

/// `Expr::defines_label` finds a label in a statement expression at any
/// depth, and nothing else counts as one.
#[test]
fn test_expr_defines_label() {
    let (expr, _, _, _) = parse_expr("1 ? 2 : ({ a: 3; })").unwrap();
    let ExprKind::Conditional {
        then_expr,
        else_expr,
        ..
    } = &expr.kind
    else {
        panic!("expected Conditional");
    };
    assert!(!then_expr.defines_label());
    assert!(else_expr.defines_label());

    let nested = "(1, -({ int x = ({ if (1) { b: ; } 1; }); x; }))";
    assert!(parse_expr(nested).unwrap().0.defines_label());
    for src in ["({ 1; })", "({ int y = 2; y; })", "1 + 2"] {
        assert!(!parse_expr(src).unwrap().0.defines_label(), "{src}");
    }
}

/// The alignment of a type-name written with `__attribute__((aligned(N)))`,
/// wherever the attribute stands in it.
#[test]
fn test_type_name_attributes() {
    for (src, want) in [
        ("_Alignof(int __attribute__((aligned(16))))", 16),
        ("_Alignof(__attribute__((aligned(8))) int)", 8),
        ("_Alignof(const __attribute__((aligned(16))) long)", 16),
        ("_Alignof(int __attribute__((aligned(16))) *)", 16),
        ("_Alignof(int * __attribute__((aligned(16))))", 16),
        ("_Alignof(int __attribute__((aligned(2))))", 2),
        ("_Alignof(int __attribute__((packed)))", 4),
    ] {
        let (expr, types, _, _) = parse_expr(src).unwrap();
        let ExprKind::AlignofType(typ) = expr.kind else {
            panic!("{src}: expected AlignofType, got {:?}", expr.kind);
        };
        assert_eq!(types.alignment(typ), want, "{src}");
    }
}

/// A type-name's attributes are its own. Before, the type-name wrote them to
/// the enclosing declaration's slots, and a struct's member list read the
/// enclosing declaration's alignment as its first member's.
#[test]
fn test_type_name_attributes_do_not_reach_the_declaration() {
    let (decl, _, _, _) =
        parse_decl("char c = sizeof(int * __attribute__((aligned(64))));").unwrap();
    assert_eq!(decl.declarators[0].explicit_align, None);

    let (decl, types, _, _) = parse_decl("_Alignas(16) struct { char a; char b; } x;").unwrap();
    assert_eq!(decl.declarators[0].explicit_align, Some(16));
    assert_eq!(types.size_bytes(decl.declarators[0].typ), 2);
}

/// `aligned` on a reference to an existing tag is the declaration's, as gcc
/// has it: it aligns `z`, and the struct itself keeps its alignment.
#[test]
fn test_aligned_on_a_tag_reference_aligns_the_declaration() {
    let (tu, types, _, _) =
        parse_tu("struct S { int a; }; struct S __attribute__((aligned(32))) z;").unwrap();
    let ExternalDecl::Declaration(decl) = &tu.items[1] else {
        panic!("expected a declaration");
    };
    assert_eq!(decl.declarators[0].explicit_align, Some(32));
    assert_eq!(types.alignment(decl.declarators[0].typ), 4);
}

/// The qualifiers written before a `typeof` belong to the type it names, in a
/// type-name as in a declaration. The type-name copy of the specifier loop
/// returned the operand's type the moment it had parsed it, so the `const`
/// in `(const typeof(int) *)` was read and then dropped.
#[test]
fn test_type_name_keeps_qualifiers_before_typeof() {
    let (expr, types, _, _) = parse_expr("(const typeof(int) *)0").unwrap();
    let ExprKind::Cast { cast_type, .. } = expr.kind else {
        panic!("expected a cast");
    };
    let pointee = types.base_type(cast_type).expect("a pointer");
    assert_eq!(types.kind(pointee), TypeKind::Int);
    assert!(types.modifiers(pointee).contains(TypeModifiers::CONST));
}

/// C17 6.7p1 lets the declaration specifiers come in any order, so a
/// specifier may follow `typeof(..)`, `_Atomic(..)` or an enum specifier as
/// it may follow `int`. Each of those arms returned as soon as it had parsed
/// its type, and the specifier after it was read as the declarator's name.
#[test]
fn test_specifiers_may_follow_a_complete_type_specifier() {
    let (decl, types, _, _) = parse_decl("typeof(int) const x = 1;").unwrap();
    let typ = decl.declarators[0].typ;
    assert_eq!(types.kind(typ), TypeKind::Int);
    assert!(types.modifiers(typ).contains(TypeModifiers::CONST));

    let (decl, types, _, _) = parse_decl("_Atomic(int) const y = 1;").unwrap();
    let typ = decl.declarators[0].typ;
    assert!(types
        .modifiers(typ)
        .contains(TypeModifiers::CONST | TypeModifiers::ATOMIC));

    let (decl, _, _, _) = parse_decl("enum E { A } static e;").unwrap();
    assert!(decl.declarators[0]
        .storage_class
        .contains(TypeModifiers::STATIC));
}

/// A typedef name is a type specifier only where no other type specifier has
/// been given (C17 6.7.2p2 lists no combination that includes one), so in
/// `unsigned T;` the typedef name is the identifier being declared -- an
/// `unsigned int` that hides the typedef. It was taken as the type.
#[test]
fn test_typedef_name_after_a_type_specifier_is_the_declarator() {
    let (tu, types, strings, symbols) = parse_tu("typedef char T; unsigned T;").unwrap();
    let ExternalDecl::Declaration(decl) = &tu.items[1] else {
        panic!("expected a declaration");
    };
    let declared = &decl.declarators[0];
    let sym = symbols.get(declared.symbol);
    assert_eq!(strings.get(sym.name), "T");
    assert_eq!(types.kind(declared.typ), TypeKind::Int);
    assert!(types.is_unsigned(declared.typ));
}

/// `typeof(int[n++])`'s extent is evaluated once for the declaration: an
/// unnamed typedef ahead of the declarators carries the size expression, and
/// each declarator names the evaluated extent rather than repeating `n++`.
#[test]
fn test_typeof_extent_is_bound_once_per_declaration() {
    let (tu, _types, _strings, _symbols) =
        parse_tu("void f(int n) { typeof(int[n++]) a, b; }").unwrap();
    let ExternalDecl::FunctionDef(func) = &tu.items[0] else {
        panic!("expected a function definition");
    };
    let Stmt::Block(items) = &func.body else {
        panic!("expected a block");
    };
    let BlockItem::Declaration(decl) = &items[0] else {
        panic!("expected a declaration");
    };
    let [hidden, a, b] = decl.declarators.as_slice() else {
        panic!("expected the unnamed typedef and two declarators");
    };
    assert!(hidden.storage_class.contains(TypeModifiers::TYPEDEF));
    let [size] = hidden.vla_sizes.as_slice() else {
        panic!("the unnamed typedef carries the one size expression");
    };
    assert!(matches!(size.kind, ExprKind::PostInc(_)));
    for d in [a, b] {
        let [extent] = d.vla_sizes.as_slice() else {
            panic!("one extent per declarator");
        };
        assert!(matches!(extent.kind, ExprKind::VmTypedefExtent(sym, 0) if sym == hidden.symbol));
    }
}

/// The declarators of every file-scope position -- first, grouped, later in
/// the list -- and block scope's are bound by one path, so each gets the
/// same symbol kind and the same function type. A grouped or later function
/// declarator was bound as a *variable*, and only the first one's type
/// carried `noreturn`.
#[test]
fn test_every_declarator_position_binds_the_same_way() {
    let src = "void a(void) __attribute__((noreturn));\n\
               void (b)(void) __attribute__((noreturn));\n\
               int x, c(void) __attribute__((noreturn));\n\
               _Noreturn void d(void), (e)(void);\n\
               void outer(void) { void k(void) __attribute__((noreturn)); }\n";
    let (tu, types, strings, symbols) = parse_tu(src).unwrap();
    let mut seen = Vec::new();
    let mut check = |decl: &Declaration| {
        for d in &decl.declarators {
            let sym = symbols.get(d.symbol);
            let name = strings.get(sym.name).to_string();
            if name == "x" {
                continue;
            }
            assert_eq!(sym.kind, crate::symbol::SymbolKind::Function, "{name}");
            assert!(types.get(d.typ).noreturn, "{name} must be noreturn");
            seen.push(name);
        }
    };
    for item in &tu.items {
        match item {
            ExternalDecl::Declaration(decl) => check(decl),
            ExternalDecl::FunctionDef(func) => {
                let Stmt::Block(items) = &func.body else {
                    panic!("expected a block");
                };
                for item in items {
                    if let BlockItem::Declaration(decl) = item {
                        check(decl);
                    }
                }
            }
        }
    }
    assert_eq!(seen, ["a", "b", "c", "d", "e", "k"]);
}

/// A later typedef declarator gets its trailing alignment, and a later or
/// grouped declarator completes an earlier `extern` array.
#[test]
fn test_later_declarators_align_and_complete() {
    let (_tu, types, strings, symbols) = parse_tu(
        "typedef int A, B __attribute__((aligned(16)));\n\
         extern int p[]; extern int q[];\n\
         int z, p[4];\n\
         int (q)[5];\n",
    )
    .unwrap();
    let find = |name: &str| {
        let id = strings.lookup(name).expect("interned");
        symbols
            .lookup(id, crate::symbol::Namespace::Ordinary)
            .unwrap_or_else(|| panic!("no symbol {name}"))
            .typ
    };
    assert_eq!(types.get(find("B")).explicit_align, Some(16));
    assert_eq!(types.get(find("A")).explicit_align, None);
    assert_eq!(types.get(find("p")).array_size, Some(4));
    assert_eq!(types.get(find("q")).array_size, Some(5));
}

/// `ms_abi` belongs to the function *type*, as gcc has it: a definition, a
/// prototype, a pointer to a function, a typedef and a parameter each carry
/// it, and `sysv_abi` names the x86-64 default. Read off the types the
/// declarations bound.
#[test]
fn test_calling_convention_is_part_of_the_function_type() {
    use crate::abi::CallingConv::{Win64, C};
    let target = Target::new(crate::target::Arch::X86_64, crate::target::Os::Linux);
    let (tu, types, strings, symbols) = parse_tu_for(
        &"int f(int a, int b) MS;\n\
          MS int g(void) { return 0; }\n\
          int h(void) { return 0; }\n\
          __attribute__((sysv_abi)) int s(void);\n\
          MS long (*fp)(long);\n\
          long (MS *gp)(long);\n\
          typedef MS long fn_t(long);\n\
          fn_t *tp;\n\
          void take(MS long (*cb)(long));\n\
          MS long (*ret_ms(void))(long);\n"
            .replace("MS", "__attribute__((ms_abi))"),
        &target,
    )
    .unwrap();
    let find = |name: &str| {
        let id = strings.lookup(name).expect("interned");
        symbols
            .lookup(id, crate::symbol::Namespace::Ordinary)
            .unwrap_or_else(|| panic!("no symbol {name}"))
            .typ
    };
    let conv = |t: TypeId| crate::abi::CallingConv::of_callee(t, &types);
    for (name, want) in [
        ("f", Win64),
        ("g", Win64),
        ("h", C),
        ("s", C),
        ("fp", Win64),
        ("gp", Win64),
        ("tp", Win64),
        // The attribute is the function's, not the pointer it returns.
        ("ret_ms", Win64),
    ] {
        assert_eq!(conv(find(name)), want, "{name}");
    }
    let returned = types.base_type(find("ret_ms")).unwrap();
    assert_eq!(conv(returned), C, "ret_ms returns a System V pointer");
    let take = types.get(find("take")).params.clone().unwrap();
    assert_eq!(conv(take[0]), Win64, "parameter");
    assert_eq!(conv(find("take")), C);
    // A definition is compiled under its type's convention.
    let defs: Vec<(String, crate::abi::CallingConv)> = tu
        .items
        .iter()
        .filter_map(|item| match item {
            ExternalDecl::FunctionDef(f) => Some((strings.get(f.name).to_string(), f.calling_conv)),
            _ => None,
        })
        .collect();
    assert_eq!(defs, [("g".to_string(), Win64), ("h".to_string(), C)]);
    // Two types differing only in convention are distinct.
    let ms_fn = types.base_type(find("fp")).unwrap();
    let mut types = types;
    let mut sysv = types.get(ms_fn).clone();
    sysv.conv = C;
    let sysv_fn = types.intern(sysv);
    assert_ne!(ms_fn, sysv_fn);
    assert!(!types.types_compatible(ms_fn, sysv_fn));
    assert_eq!(
        types.format_type(types.pointer_to(ms_fn), None),
        "long (__attribute__((ms_abi)) *)(long)"
    );
}

/// gcc rejects a redeclaration that changes a function's calling
/// convention, and the two attributes on one declaration, as conflicts; a
/// pointer to a pointer to a function is not a function type, so the
/// attribute warns there and is ignored.
#[test]
fn test_calling_convention_conflicts() {
    let target = Target::new(crate::target::Arch::X86_64, crate::target::Os::Linux);
    for src in [
        "int f(int a, int b) __attribute__((ms_abi));\nint f(int a, int b) { return a - b; }\n",
        "__attribute__((ms_abi)) int f(int);\n__attribute__((sysv_abi)) int f(int);\n",
        "__attribute__((ms_abi, sysv_abi)) int f(int);\n",
        "__attribute__((ms_abi)) int f(int) __attribute__((sysv_abi));\n",
    ] {
        let before = crate::diag::error_count();
        let _ = parse_tu_for(src, &target);
        assert!(crate::diag::error_count() > before, "{src}: accepted");
    }
    let (_, types, strings, symbols) =
        parse_tu_for("__attribute__((ms_abi)) long (**pp)(long);\n", &target).unwrap();
    let id = strings.lookup("pp").unwrap();
    let pp = symbols
        .lookup(id, crate::symbol::Namespace::Ordinary)
        .unwrap()
        .typ;
    let func = types.base_type(types.base_type(pp).unwrap()).unwrap();
    assert_eq!(types.get(func).conv, crate::abi::CallingConv::C);
}

/// On aarch64 neither attribute exists, as for gcc there: the type keeps the
/// native convention.
#[test]
fn test_calling_convention_attributes_do_not_exist_on_aarch64() {
    let target = Target::new(crate::target::Arch::Aarch64, crate::target::Os::Linux);
    let (_, types, strings, symbols) = parse_tu_for(
        "__attribute__((ms_abi)) long f(long);\n__attribute__((sysv_abi)) long g(long);\n",
        &target,
    )
    .unwrap();
    for name in ["f", "g"] {
        let id = strings.lookup(name).unwrap();
        let typ = symbols
            .lookup(id, crate::symbol::Namespace::Ordinary)
            .unwrap()
            .typ;
        assert_eq!(types.get(typ).conv, crate::abi::CallingConv::C, "{name}");
    }
}

/// Each `va_start` belongs to one convention, and `__builtin_ms_va_list`
/// is a `char *` to everything but `va_arg`: assignable both ways without a
/// diagnostic, and named by its own spelling.
#[test]
fn test_ms_va_builtins() {
    let target = Target::new(crate::target::Arch::X86_64, crate::target::Os::Linux);
    let ok = "__attribute__((ms_abi)) int f(int n, ...) {\n\
              __builtin_ms_va_list ap, ap2; char *p;\n\
              __builtin_ms_va_start(ap, n);\n\
              __builtin_ms_va_copy(ap2, ap);\n\
              p = ap; ap2 = p;\n\
              int r = __builtin_va_arg(ap, int);\n\
              __builtin_ms_va_end(ap);\n\
              return r;\n}\n\
              int g(__builtin_ms_va_list ap) { return __builtin_va_arg(ap, int); }\n";
    let (_, types, _, _) = parse_tu_for(ok, &target).unwrap();
    assert!(types.is_ms_va_list(types.ms_va_list_id));
    assert!(!types.is_ms_va_list(types.char_ptr_id));
    assert!(types.types_compatible(types.ms_va_list_id, types.char_ptr_id));
    assert_eq!(
        types.format_type(types.ms_va_list_id, None),
        "__builtin_ms_va_list"
    );
    for src in [
        // The System V `va_start` in an `ms_abi` function...
        "__attribute__((ms_abi)) int f(int n, ...) { __builtin_va_list ap; \
         __builtin_va_start(ap, n); return 0; }",
        // ...the Microsoft one in a System V function...
        "int f(int n, ...) { __builtin_ms_va_list ap; __builtin_ms_va_start(ap, n); return 0; }",
        // ...and on a list of the wrong type.
        "__attribute__((ms_abi)) int f(int n, ...) { char *ap; \
         __builtin_ms_va_start(ap, n); return 0; }",
    ] {
        let before = crate::diag::error_count();
        let _ = parse_tu_for(src, &target);
        assert!(crate::diag::error_count() > before, "{src}: accepted");
    }
}

/// A floating comparison or a cast to an integer in an integer constant
/// expression is decided exactly, at the operands' common type: through
/// `f64` the first two were false, `0.1f != 0.1` was false, and
/// `(_Bool)0.5` was 0. Each `_Static_assert` is gcc's answer.
#[test]
fn test_constant_expression_floating_folds_are_exact() {
    let src = "\
        _Static_assert(0x1p62L + 1.0L > 0x1p62L, \"order\");\n\
        _Static_assert((long long)(0x1p62L + 1.0L) == 4611686018427387905LL, \"cast\");\n\
        _Static_assert((unsigned long long)(0x1p63L + 3.0L) == 9223372036854775811ULL, \"u\");\n\
        _Static_assert(0.1f != 0.1, \"common type\");\n\
        _Static_assert((1LL << 60) + 1 == 0x1p60, \"integer operand rounds\");\n\
        _Static_assert((int)0.99999999999999999999 == 1, \"literal is a double\");\n\
        _Static_assert((_Bool)0.5 == 1, \"bool\");\n\
        _Static_assert((int)((1 / 2) + 0.5) == 0, \"integer subexpression\");\n\
        _Static_assert(__builtin_nan(\"\") != __builtin_nan(\"\"), \"unordered ne\");\n\
        _Static_assert(!(__builtin_nan(\"\") == __builtin_nan(\"\")), \"unordered eq\");\n\
        _Static_assert(__builtin_constant_p(0x1p62L + 1.0L), \"constant\");\n";
    // `0x1p62L + 1.0L` needs a `long double` wider than `double`: x87 on
    // x86-64, binary128 on aarch64 Linux -- not Apple arm64.
    for target in linux_targets() {
        if let Err(e) = parse_tu_for(src, &target) {
            panic!("{target:?}: should have parsed: {e}");
        }
    }
}

/// A conversion C leaves undefined is not an integer constant expression:
/// gcc makes `int a[(int)1e300 > 0];` a VLA for the same reason.
#[test]
fn test_out_of_range_float_cast_is_not_an_integer_constant() {
    for src in [
        "enum { E = (int)1e300 };",
        "_Static_assert((int)__builtin_nan(\"\") == 0, \"\");",
        "_Static_assert((unsigned)-1.0 == 0, \"\");",
    ] {
        assert!(parse_tu(src).is_err(), "{src} should be rejected");
    }
}

/// `==` and `!=` take a complex operand, and fold: both operands convert to
/// the common complex type and compare half by half. Each answer is gcc's.
#[test]
fn test_complex_equality_is_a_constant_expression() {
    let src = "\
        _Static_assert((_Complex float)(0.5) == 0.5, \"real promotes\");\n\
        _Static_assert((1.0f + 2.0fi) == (1.0L + 2.0iL), \"common type\");\n\
        _Static_assert((0.1f + 0i) != 0.1, \"float half widens exactly\");\n\
        _Static_assert(3 == (3 + 0i), \"complex integer\");\n\
        _Static_assert(__builtin_complex(__builtin_nan(\"\"), 0.0) != __builtin_complex(__builtin_nan(\"\"), 0.0), \"NaN half\");\n\
        _Static_assert(__real__ (3 + 4i) == 3 && __imag__ (3 + 4i) == 4, \"halves\");\n\
        _Static_assert(!(0.0 + 0.0i) && (0.0 + 1.0i), \"truth\");\n\
        _Static_assert((_Bool)(0.0 + 1.0i) && (int)(3.75 + 2.5i) == 3, \"conversion\");\n\
        _Static_assert(((0.0 + 1.0i) ? 7 : 8) == 7, \"condition\");\n";
    if let Err(e) = parse_tu(src) {
        panic!("should have parsed: {e}");
    }
}

/// `__builtin_constant_p` of something the parser cannot fold is 0 at once
/// at `-O0`, where no optimizer will run to prove it constant, and deferred
/// once optimizing. A constant operand answers 1 at every level.
#[test]
fn test_constant_p_at_o0_answers_zero() {
    let o0 = super::LibraryCallPolicy {
        optimizing: false,
        math_errno: true,
    };
    let (expr, _, _, _) = parse_expr_under("__builtin_constant_p(n)", &["n"], o0).unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(0)), "{:?}", expr.kind);
    let (expr, _, _, _) = parse_expr_under("__builtin_constant_p(3)", &[], o0).unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(1)), "{:?}", expr.kind);
    let (expr, _, _, _) =
        parse_expr_under("__builtin_constant_p(n)", &["n"], Default::default()).unwrap();
    assert!(
        matches!(expr.kind, ExprKind::ConstantP(_)),
        "{:?}",
        expr.kind
    );
}

/// x86-64 Linux: x87 `long double`, and `__float128`.
fn x86_64_linux() -> Target {
    Target::new(crate::target::Arch::X86_64, crate::target::Os::Linux)
}

/// The Linux targets, whose `long double` is wider than `double`.
fn linux_targets() -> [Target; 2] {
    [
        x86_64_linux(),
        Target::new(crate::target::Arch::Aarch64, crate::target::Os::Linux),
    ]
}

/// Unary `+` and `-` leave a GNU complex integer's type alone.
///
/// `integer_promote` switches on `kind()`, which answers a complex type's
/// base kind, so `_Complex short` looked like a `short` and promoted to
/// `int` -- and the conversion that carries out the promotion then kept only
/// the real half, so `+z` came out `(3, 0)`. gcc and clang give `+z` the
/// type of `z`. The real `short` beside it is the control: 6.5.3.3p2 still
/// promotes that one.
#[test]
fn test_unary_plus_leaves_a_complex_integer_alone() {
    for op in ["+", "-"] {
        let (expr, types, _strings, _symbols) =
            parse_expr(&format!("{op}(_Complex short)1")).unwrap();
        let t = expr.typ.expect("a unary arithmetic operator has a type");
        assert!(
            types.is_complex_integer(t),
            "`{op}` on a complex integer keeps both halves' type"
        );
        assert_eq!(
            types.size_bits(t),
            types.size_bits(types.short_id) * 2,
            "`{op}` does not widen it to _Complex int either"
        );
    }

    // The control: a real narrow operand still promotes.
    let (expr, types, _strings, _symbols) = parse_expr("+(short)1").unwrap();
    assert_eq!(expr.typ, Some(types.int_id));
}

/// Where each element of an array's initializer list lands, and where the
/// cursor goes next: a positional element takes the cursor, an index moves it
/// past itself, and a range moves it past its *high* end.
#[test]
fn test_array_slot_follows_the_cursor() {
    use crate::parse::ast::{array_slot, Designator};
    let mut cursor = 0;
    assert_eq!(array_slot(&[], &mut cursor), (0, 0, None));
    assert_eq!(cursor, 1);
    assert_eq!(
        array_slot(&[Designator::IndexRange(4, 6)], &mut cursor),
        (4, 6, Some(0))
    );
    assert_eq!(cursor, 7);
    assert_eq!(array_slot(&[], &mut cursor), (7, 7, None));
    assert_eq!(
        array_slot(&[Designator::Index(2)], &mut cursor),
        (2, 2, Some(0))
    );
    assert_eq!(cursor, 3);
}

// Attribute declarations: `__attribute__((...));` standing alone

/// The block items of a function body.
fn body_items(func: &FunctionDef) -> &[BlockItem] {
    match &func.body {
        Stmt::Block(items) => items,
        other => panic!("function body is not a block: {other:?}"),
    }
}

/// The block items of the `switch` that is the first statement of `func`.
fn switch_items(func: &FunctionDef) -> &[BlockItem] {
    let BlockItem::Statement(stmt) = &body_items(func)[0] else {
        panic!("first item is not a statement");
    };
    let Stmt::Switch { body, .. } = stmt.as_ref() else {
        panic!("first statement is not a switch: {stmt:?}");
    };
    match body.as_ref() {
        Stmt::Block(items) => items,
        other => panic!("switch body is not a block: {other:?}"),
    }
}

/// `__attribute__((fallthrough));` is a null statement in either spelling,
/// whether it follows a statement or is itself the labeled statement -- not
/// a declaration that declares nothing.
#[test]
fn test_fallthrough_attribute_is_a_null_statement() {
    let (func, _, _, _) = parse_func(
        "int f(int x) { switch (x) { case 1: x++; __attribute__((fallthrough));\n\
         case 2: __attribute__((__fallthrough__)); default: break; } return x; }",
    )
    .unwrap();
    let items = switch_items(&func);
    assert_eq!(items.len(), 4, "{items:?}");
    // `case 1: x++;` is one item, holding the statement it labels.
    assert!(matches!(&items[1], BlockItem::Statement(s) if matches!(**s, Stmt::Empty)));
    let BlockItem::Statement(case2) = &items[2] else {
        panic!("case 2 is not a statement");
    };
    let Stmt::Labeled {
        labels,
        stmt: labeled,
    } = case2.as_ref()
    else {
        panic!("not a case: {case2:?}");
    };
    assert!(matches!(labels.as_slice(), [Label::Case(..)]), "{labels:?}");
    assert!(matches!(**labeled, Stmt::Empty), "{labeled:?}");
}

/// Outside every `switch` the fallthrough statement is an error, as in gcc,
/// in a block and as the body of an `if`.
#[test]
fn test_fallthrough_attribute_outside_a_switch_is_an_error() {
    for src in [
        "void f(void) { __attribute__((fallthrough)); }",
        "void f(int x) { if (x) __attribute__((fallthrough)); }",
        "void f(void) { for (;;) { __attribute__((fallthrough)); break; } }",
    ] {
        let before = crate::diag::error_count();
        parse_func(src).unwrap_or_else(|e| panic!("{src}: {e:?}"));
        assert!(crate::diag::error_count() > before, "{src}: no error");
    }
}

/// Any other attribute standing alone is an empty declaration: accepted, and
/// it leaves nothing pending for the declaration after it.
#[test]
fn test_other_attribute_statement_is_empty() {
    let (func, _, _, _) =
        parse_func("void f(void) { __attribute__((aligned(64))); int y; }").unwrap();
    let items = body_items(&func);
    assert!(matches!(&items[0], BlockItem::Statement(s) if matches!(**s, Stmt::Empty)));
    let BlockItem::Declaration(decl) = &items[1] else {
        panic!("second item is not a declaration: {:?}", items[1]);
    };
    assert_eq!(decl.declarators[0].explicit_align, None);
}

/// A declaration that begins with an attribute is still a declaration, in a
/// block and at file scope, and a lone attribute list at file scope is an
/// empty declaration that the next one parses after.
#[test]
fn test_attribute_led_declarations_still_declare() {
    let (func, _, _, _) =
        parse_func("void g(void) { __attribute__((unused)) int x; x = 1; }").unwrap();
    let BlockItem::Declaration(decl) = &body_items(&func)[0] else {
        panic!("attribute-led local is not a declaration");
    };
    assert_eq!(decl.declarators.len(), 1);

    let (tu, _, _, _) = parse_tu(
        "__attribute__((unused));\n\
         __attribute__((constructor)) void f(void) {}\n\
         __attribute__((unused)) static int z;\n",
    )
    .unwrap();
    assert_eq!(tu.items.len(), 3);
    assert!(matches!(&tu.items[0], ExternalDecl::Declaration(d) if d.declarators.is_empty()));
    assert!(matches!(tu.items[1], ExternalDecl::FunctionDef(_)));
    assert!(matches!(&tu.items[2], ExternalDecl::Declaration(d) if d.declarators.len() == 1));
}

/// The lookahead sees an attribute declaration only when nothing but
/// attribute lists stands before the `;`.
#[test]
fn test_at_attribute_declaration_lookahead() {
    for (src, expected) in [
        ("__attribute__((fallthrough));", true),
        ("__attribute__((a)) __attribute((b(1, (2))));", true),
        ("__attribute__((unused)) int x;", false),
        ("__attribute__((fallthrough)) x++;", false),
        ("int x;", false),
        (";", false),
    ] {
        let mut strings = StringTable::new();
        let tokens = Tokenizer::new(src.as_bytes(), 0, &mut strings).tokenize();
        let mut symbols = SymbolTable::new();
        let mut types = TypeTable::new(&Target::host());
        let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types, Vec::new());
        parser.skip_stream_tokens();
        assert_eq!(parser.at_attribute_declaration(), expected, "{src}");
    }
}

/// A function designator decays before the additive operators type it, as an
/// array does: `f - g` is a `ptrdiff_t` and `f + 1` a pointer to `f`'s type,
/// the same as the operators give a function pointer.
#[test]
fn test_additive_operators_decay_a_function_designator() {
    let decls = "int f(int); int g(int); int (*fp)(int);";
    for stmt in ["f - g", "fp - f", "f - fp", "fp - fp"] {
        with_statement_expr(decls, stmt, |p, e| {
            assert_eq!(e.typ, Some(p.types.long_id), "{stmt}: result type");
        });
    }
    for stmt in ["f + 1", "1 + f", "f - 1", "fp + 1"] {
        with_statement_expr(decls, stmt, |p, e| {
            let t = e.typ.expect("typed");
            assert_eq!(p.types.kind(t), TypeKind::Pointer, "{stmt}");
            let pointee = p.types.base_type(t).unwrap();
            assert_eq!(p.types.kind(pointee), TypeKind::Function, "{stmt}");
        });
    }
}

/// Indexing wants a pointer to a complete object type (C17 6.5.2.1p1), so a
/// pointer to a function is refused on either side of the brackets, though
/// gcc's arithmetic on one is accepted.
#[test]
fn test_subscripting_a_function_pointer_is_rejected() {
    let decls = "int f(int); int (*fp)(int);";
    for stmt in ["fp[0]", "0[fp]", "(&f)[1]"] {
        let before = crate::diag::error_count();
        with_statement_expr(decls, stmt, |_, _| {
            // The count is process-wide and only grows, so a concurrent test
            // can add to it but never hide this one's error.
            assert!(crate::diag::error_count() > before, "{stmt}: accepted");
        });
    }
}

/// `__builtin_assume_aligned` is a call through gcc's prototype
/// `void *(const void *, size_t, ...)`: its result is a `void *` -- whatever
/// the pointer's pointee and qualifiers -- and every argument that is more
/// than a literal is kept, for its side effects.
#[test]
fn test_assume_aligned_yields_void_ptr_and_keeps_its_arguments() {
    let decls = "const char *p; int k; long n;";
    // (call, operands kept beside the pointer)
    for (stmt, kept) in [
        ("__builtin_assume_aligned(p, 16)", 0),
        ("__builtin_assume_aligned(p, 16, 4)", 0),
        ("__builtin_assume_aligned(p, n)", 1),
        ("__builtin_assume_aligned(p, 4, k++)", 1),
        ("__builtin_assume_aligned(p, n, k++)", 2),
    ] {
        with_statement_expr(decls, stmt, |p, e| {
            assert_eq!(e.typ, Some(p.types.void_ptr_id), "{stmt}: result type");
            let ptr = match &e.kind {
                ExprKind::Comma(parts) => {
                    assert_eq!(parts.len(), kept + 1, "{stmt}: operands");
                    parts.last().unwrap()
                }
                _ => {
                    assert_eq!(kept, 0, "{stmt}: operands dropped");
                    e
                }
            };
            assert!(
                matches!(ptr.kind, ExprKind::Cast { cast_type, .. }
                    if cast_type == p.types.void_ptr_id),
                "{stmt}: pointer not converted: {:?}",
                ptr.kind
            );
        });
    }
}

/// What gcc rejects in a call to `__builtin_assume_aligned`: an argument
/// the prototype cannot convert, too few or too many arguments, and a
/// misalignment that is not an integer. A null `void *` stands in.
#[test]
fn test_assume_aligned_rejects_bad_arguments() {
    let decls = "struct S { int a; } s; char *p; double d;";
    for stmt in [
        "__builtin_assume_aligned(s, 16)",
        "__builtin_assume_aligned(p, s)",
        "__builtin_assume_aligned(p)",
        "__builtin_assume_aligned(p, 16, 0, 1)",
        "__builtin_assume_aligned(p, 16, d)",
        "__builtin_assume_aligned(p, 16, p)",
    ] {
        let before = crate::diag::error_count();
        with_statement_expr(decls, stmt, |p, e| {
            // The count is process-wide and only grows, so a concurrent test
            // can add to it but never hide this one's error.
            assert!(crate::diag::error_count() > before, "{stmt}: accepted");
            assert_eq!(e.typ, Some(p.types.void_ptr_id), "{stmt}: result type");
            assert!(
                matches!(&e.kind, ExprKind::Cast { expr, .. }
                    if matches!(expr.kind, ExprKind::IntLit(0))),
                "{stmt}: built {:?}",
                e.kind
            );
        });
    }
}

/// An argument a builtin requires to be an integer constant is one only if
/// it is an integer constant expression (C17 6.6p6): an enumerator, a cast
/// of a floating constant and `sizeof` are; a floating constant, a `const`
/// object and a variable are not. The range decides in from out.
#[test]
fn test_constant_argument_is_an_integer_constant_expression() {
    use super::builtin_args::ConstantArgument::{InRange, NotConstant, OutOfRange};
    let decls = "enum E { A = 2 }; const int ci = 1; int i; double d;";
    for (stmt, expected) in [
        ("3", InRange(3)),
        ("A", InRange(2)),
        ("(int)1.0", InRange(1)),
        ("sizeof(int) - 3", InRange(1)),
        ("4", OutOfRange(4)),
        ("-1", OutOfRange(-1)),
        ("1.0", NotConstant),
        ("ci", NotConstant),
        ("i", NotConstant),
        ("d", NotConstant),
    ] {
        let got = with_statement_expr(decls, stmt, |p, e| p.constant_argument(e, 0..=3));
        assert_eq!(got, expected, "{stmt}");
    }
}

/// What gcc calls integral: the integer types, `_Bool` and enumerations --
/// not a complex integer, a floating type or a pointer.
#[test]
fn test_is_integral_is_gccs_integral_type() {
    let decls = "enum E { A } e; _Bool b; char c; __int128 w; unsigned long u;\
                 _Complex int ci; double d; int *p;";
    for (stmt, integral) in [
        ("e", true),
        ("b", true),
        ("c", true),
        ("w", true),
        ("u", true),
        ("ci", false),
        ("d", false),
        ("p", false),
    ] {
        let got = with_statement_expr(decls, stmt, |p, e| p.is_integral(e.typ.unwrap()));
        assert_eq!(got, integral, "{stmt}");
    }
}

/// The classification builtins take any real floating argument and nothing
/// else; a rejection is an error.
#[test]
fn test_require_floating_argument_takes_real_floating_only() {
    let decls = "float f; long double ld; _Float16 h; const double cd = 0; int i;\
                 enum E { A } e; _Bool b; char *p; _Complex double z; struct S { int a; } s;";
    for (stmt, floating) in [
        ("f", true),
        ("ld", true),
        ("h", true),
        ("cd", true),
        ("i", false),
        ("e", false),
        ("b", false),
        ("p", false),
        ("z", false),
        ("s", false),
    ] {
        let before = crate::diag::error_count();
        let got = with_statement_expr(decls, stmt, |p, e| {
            p.require_floating_argument(e, crate::kw::BUILTIN_ISNAN)
        });
        assert_eq!(got, floating, "{stmt}");
        if !floating {
            assert!(crate::diag::error_count() > before, "{stmt}: not reported");
        }
    }
}

/// A count is judged against a minimum and an optional maximum, as every
/// call's is.
#[test]
fn test_check_argument_count_bounds() {
    for (given, min, max, ok) in [
        (2, 2, Some(2), true),
        (1, 2, Some(2), false),
        (3, 2, Some(2), false),
        (5, 1, None, true),
        (0, 1, None, false),
        (3, 0, Some(3), true),
        (4, 0, Some(3), false),
    ] {
        let got = with_statement_expr("", "0", |p, _| {
            p.check_argument_count(
                Some(crate::kw::BUILTIN_FFS),
                given,
                min,
                max,
                p.current_pos(),
            )
        });
        assert_eq!(got, ok, "{given} of {min}..{max:?}");
    }
}

/// A builtin call rejected by its checks is not built: a zero of its type
/// stands in, so nothing downstream sees an operand it cannot handle.
#[test]
fn test_rejected_builtin_calls_are_a_typed_zero() {
    let decls = "int i; double d; _Bool b; char *p; struct S { int a; } s;\
                 struct I; __builtin_va_list ap;";
    for (stmt, typ) in [
        ("__builtin_isnan(i)", TypeKind::Int),
        ("__builtin_fpclassify(i, 1, 2, 3, 4, d)", TypeKind::Int),
        ("__builtin_complex(1, 2)", TypeKind::Double),
        ("__builtin_complex(1.0f, 2.0)", TypeKind::Double),
        ("__builtin_add_overflow(1, 2, &d)", TypeKind::Int),
        ("__builtin_add_overflow(1, 2, &b)", TypeKind::Int),
        ("__builtin_mul_overflow_p(1, 2, b)", TypeKind::Int),
        ("__builtin_object_size(p, i)", TypeKind::Long),
        ("__builtin_prefetch(p, i)", TypeKind::Void),
        ("__atomic_fetch_add(&b, 1, 0)", TypeKind::Bool),
        ("__atomic_load_n(i, 0)", TypeKind::Int),
        ("__atomic_always_lock_free(i, 0)", TypeKind::Bool),
        ("__builtin_alloca(s)", TypeKind::Pointer),
        ("__builtin_va_arg(ap, void)", TypeKind::Int),
        ("__builtin_va_arg(ap, struct I)", TypeKind::Int),
        ("__builtin_va_arg(ap, int[])", TypeKind::Int),
        ("__builtin_va_arg(ap, int(void))", TypeKind::Int),
    ] {
        let before = crate::diag::error_count();
        with_statement_expr(decls, stmt, |p, e| {
            assert!(crate::diag::error_count() > before, "{stmt}: accepted");
            assert_eq!(p.types.kind(e.typ.unwrap()), typ, "{stmt}: type");
            let zero = match &e.kind {
                ExprKind::Cast { expr, .. } => expr,
                _ => e,
            };
            assert!(
                matches!(zero.kind, ExprKind::IntLit(0)),
                "{stmt}: built {:?}",
                e.kind
            );
        });
    }
}

/// `va_arg` takes any complete object type (C17 7.16.1.1p2) -- an array, a
/// pointer to a variably modified one, a defined tag, a type the default
/// argument promotions change (gcc only warns) -- and yields a value of it.
#[test]
fn test_va_arg_of_a_complete_object_type_is_built() {
    let decls = "struct S { int a; } s; enum E { A }; struct I; __builtin_va_list ap; int n;";
    for (stmt, typ) in [
        ("__builtin_va_arg(ap, int[3])", TypeKind::Array),
        ("__builtin_va_arg(ap, int(*)[n])", TypeKind::Pointer),
        ("__builtin_va_arg(ap, struct S)", TypeKind::Struct),
        ("__builtin_va_arg(ap, struct I *)", TypeKind::Pointer),
        ("__builtin_va_arg(ap, enum E)", TypeKind::Enum),
        ("__builtin_va_arg(ap, char)", TypeKind::Char),
        ("__builtin_va_arg(ap, float)", TypeKind::Float),
    ] {
        with_statement_expr(decls, stmt, |p, e| {
            // A variably modified type-name's extents wrap the value.
            let value = match &e.kind {
                ExprKind::VmTypeName { expr, .. } => expr,
                _ => e,
            };
            assert!(
                matches!(value.kind, ExprKind::VaArg { .. }),
                "{stmt}: built {:?}",
                e.kind
            );
            assert_eq!(p.types.kind(e.typ.unwrap()), typ, "{stmt}: type");
        });
    }
}

/// The suffixed classification builtins and the typed checked arithmetic
/// have gcc's prototypes, so their arguments convert: `__builtin_isnanf(i)`
/// tests a `float`, `__builtin_sadd_overflow(1.5, ...)` adds `int`s.
#[test]
fn test_prototyped_generic_builtins_convert_their_arguments() {
    let decls = "int i; long double ld;";
    with_statement_expr(decls, "__builtin_isnanf(i)", |p, e| {
        let ExprKind::FpTest { arg, .. } = &e.kind else {
            panic!("built {:?}", e.kind);
        };
        assert_eq!(arg.typ, Some(p.types.float_id));
    });
    with_statement_expr(decls, "__builtin_signbitl(i)", |p, e| {
        let ExprKind::FpTest { arg, .. } = &e.kind else {
            panic!("built {:?}", e.kind);
        };
        assert_eq!(arg.typ, Some(p.types.longdouble_id));
    });
    with_statement_expr(decls, "__builtin_sadd_overflow(1.5, ld, &i)", |p, e| {
        let ExprKind::CheckedArith { a, b, .. } = &e.kind else {
            panic!("built {:?}", e.kind);
        };
        assert_eq!(a.typ, Some(p.types.int_id));
        assert_eq!(b.typ, Some(p.types.int_id));
    });
}

// ============================================================================
// Operand constraints of the operators (C17 6.5.3.3, 6.5.5-6.5.15)
// ============================================================================

/// A table of types for the operand-rule tests: one of each shape an operand
/// can have once arrays and functions have decayed.
struct OperandTypes {
    types: TypeTable,
    structure: crate::types::TypeId,
    union: crate::types::TypeId,
    complex_int: crate::types::TypeId,
    long_ptr: crate::types::TypeId,
}

fn operand_types() -> OperandTypes {
    use crate::types::{CompositeType, Type};
    let mut types = TypeTable::new(&Target::host());
    let structure = types.intern(Type::struct_type(CompositeType::incomplete(None)));
    let union = types.intern(Type::union_type(CompositeType::incomplete(None)));
    let complex_int = types.make_complex(types.int_id);
    let long_ptr = types.intern(Type::pointer(types.long_id));
    OperandTypes {
        types,
        structure,
        union,
        complex_int,
        long_ptr,
    }
}

/// Which types each operand class admits. A structure or union is in none of
/// them; a complex integer is not an integer, though `is_integer` says so.
#[test]
fn operand_class_admits_its_types_and_no_aggregate() {
    use super::operand_rule::OperandClass::*;
    let t = operand_types();
    let ty = &t.types;
    // (type, Integer, IntegerOrComplex, Real, Arithmetic, Scalar)
    let table = [
        (ty.int_id, [true, true, true, true, true]),
        (ty.bool_id, [true, true, true, true, true]),
        (ty.double_id, [false, false, true, true, true]),
        (ty.complex_double_id, [false, true, false, true, true]),
        (t.complex_int, [false, true, false, true, true]),
        (ty.int_ptr_id, [false, false, false, false, true]),
        (t.structure, [false, false, false, false, false]),
        (t.union, [false, false, false, false, false]),
        (ty.void_id, [false, false, false, false, false]),
    ];
    for (typ, expected) in table {
        for (class, want) in [Integer, IntegerOrComplex, Real, Arithmetic, Scalar]
            .into_iter()
            .zip(expected)
        {
            assert_eq!(
                class.admits(ty, typ),
                want,
                "{:?} of {}",
                class,
                ty.format_type(typ, None)
            );
        }
    }
}

/// The unary operators' classes, as gcc applies them: `~` takes a complex
/// operand (its conjugate), and `++` a complex one too.
#[test]
fn unary_operator_classes() {
    use super::operand_rule::{OperandClass, UnaryOperator};
    for (op, class, name) in [
        (UnaryOperator::Plus, OperandClass::Arithmetic, "unary plus"),
        (
            UnaryOperator::Minus,
            OperandClass::Arithmetic,
            "unary minus",
        ),
        (
            UnaryOperator::Complement,
            OperandClass::IntegerOrComplex,
            "bit-complement",
        ),
        (
            UnaryOperator::Not,
            OperandClass::Scalar,
            "unary exclamation mark",
        ),
        (UnaryOperator::Increment, OperandClass::Scalar, "increment"),
        (UnaryOperator::Decrement, OperandClass::Scalar, "decrement"),
    ] {
        assert_eq!(op.operand_class(), class);
        assert_eq!(op.name(), name);
    }
}

/// Every binary operator against representative operand pairs, with gcc's
/// verdict for each.
#[test]
fn binary_operand_verdicts() {
    use super::operand_rule::{binary_operand_verdict, Operand, OperandVerdict::*};
    let t = operand_types();
    let ty = &t.types;
    let v = |typ| Operand {
        typ,
        null_constant: false,
    };
    let zero = Operand {
        typ: ty.int_id,
        null_constant: true,
    };
    let (int, dbl, cplx, ptr, s) = (
        v(ty.int_id),
        v(ty.double_id),
        v(ty.complex_double_id),
        v(ty.int_ptr_id),
        v(t.structure),
    );
    let (lptr, vptr) = (v(t.long_ptr), v(ty.void_ptr_id));
    use BinaryOp::*;
    let table = [
        // Aggregates satisfy no operator.
        (Add, s, int, Invalid),
        (Add, int, s, Invalid),
        (Mul, s, s, Invalid),
        (Eq, s, s, Invalid),
        (Lt, int, s, Invalid),
        (LogAnd, int, v(t.union), Invalid),
        (BitAnd, s, int, Invalid),
        // Integer-only operators.
        (Mod, dbl, int, Invalid),
        (Mod, cplx, int, Invalid),
        (Shl, int, cplx, Invalid),
        (BitXor, v(t.complex_int), int, Invalid),
        (Shr, ptr, int, Invalid),
        (BitOr, v(ty.bool_id), int, Valid),
        // Arithmetic, complex included.
        (Mul, cplx, dbl, Valid),
        (Mul, ptr, int, Invalid),
        (Eq, cplx, int, Valid),
        (Lt, cplx, int, Invalid),
        // Pointer forms of + and -.
        (Add, ptr, int, Valid),
        (Add, int, ptr, Valid),
        (Add, ptr, ptr, Invalid),
        (Add, ptr, dbl, Invalid),
        (Sub, ptr, int, Valid),
        (Sub, int, ptr, Invalid),
        (Sub, ptr, ptr, Valid),
        (Sub, ptr, lptr, Invalid),
        (Sub, ptr, vptr, Invalid),
        // Comparisons with pointers.
        (Lt, ptr, ptr, Valid),
        (Lt, ptr, lptr, DistinctPointers),
        (Lt, ptr, vptr, DistinctPointers),
        (Eq, ptr, vptr, Valid),
        (Eq, ptr, lptr, DistinctPointers),
        (Eq, ptr, int, PointerInteger),
        (Gt, int, ptr, PointerInteger),
        (Eq, ptr, zero, Valid),
        (Gt, ptr, zero, Valid),
        (Eq, ptr, dbl, Invalid),
        // Logical operators take any scalar.
        (LogOr, ptr, cplx, Valid),
        (LogAnd, s, int, Invalid),
    ];
    for (op, l, r, want) in table {
        assert_eq!(
            binary_operand_verdict(ty, op, l, r),
            want,
            "{} {} {}",
            ty.format_type(l.typ, None),
            op.spelling(),
            ty.format_type(r.typ, None)
        );
    }
}

/// C17 6.7.6.2p1: an array's element type shall be neither incomplete nor a
/// function type, wherever the array type is formed -- a declaration, a
/// parameter, a member, a pointer to it, a type-name -- and the type is still
/// formed, so parsing carries on.
#[test]
fn test_array_of_incomplete_element_type_is_reported() {
    for src in [
        "struct I; extern struct I a[2];",
        "struct I; struct I (*q)[2]; struct I { int x; };",
        "struct I; void f(struct I a[]);",
        "struct I; struct S { struct I m[2]; };",
        "enum E; extern enum E a[2];",
        "extern int a[3][];",
        "extern int (a[2])[];",
        "extern void x[2];",
        "void (a[2]);",
        "int (a[2])(void);",
        "int n = sizeof(struct I[2]);",
        "int n = sizeof(void[2]);",
        "int n = sizeof(int[2](void));",
    ] {
        let before = crate::diag::error_count();
        parse_tu(src).unwrap_or_else(|e| panic!("{src}: {e:?}"));
        assert!(crate::diag::error_count() > before, "{src}: accepted");
    }
}

/// A suffix reads left to right from the identifier: `a[2](void)` is an
/// array of two functions, not a function returning an array, and a grouped
/// declarator's extents apply over the outer suffix.
#[test]
fn test_array_suffix_derivation_order() {
    let (decl, types, _, _) = parse_decl("int a[2](void);").unwrap();
    let typ = decl.declarators[0].typ;
    assert_eq!(types.kind(typ), TypeKind::Array);
    assert_eq!(
        types.kind(types.base_type(typ).unwrap()),
        TypeKind::Function
    );

    let (decl, types, _, _) = parse_decl("int (a[2])[3];").unwrap();
    let typ = decl.declarators[0].typ;
    assert_eq!(types.get(typ).array_size, Some(2));
    let elem = types.base_type(typ).unwrap();
    assert_eq!(types.get(elem).array_size, Some(3));
    assert_eq!(types.kind(types.base_type(elem).unwrap()), TypeKind::Int);
}

/// The type of a conditional expression whose arms are pointers, or a pointer
/// and something else (C17 6.5.15p6), spelled as gcc's `_Generic` reports it.
///
/// A null pointer constant takes the other arm's type, even when it is
/// `(void *)0` -- which is a pointer, but is not what the arm contributes.
/// Compatible pointees merge to their composite type, so an array of unknown
/// size meets `[3]` as `[3]` and a function without a prototype meets
/// `(void)` as `(void)`. Pointers to incompatible types give `void *`, which
/// is gcc's answer, and a pointer beside a nonzero integer stays a pointer.
#[test]
fn test_conditional_pointer_result_types() {
    let decls = "int c; int *p; char *cp; const int *cip; volatile int *vip; \
                 void *vp; const void *cvp; int (*fp)(void); int (*fnp)(); \
                 const int (*cap)[]; int (*a3)[3]; unsigned *up;";
    for (expr, want) in [
        ("c ? p : (void *)0", "int *"),
        ("c ? (void *)0 : p", "int *"),
        ("c ? cip : (void *)0", "const int *"),
        ("c ? fp : (void *)0", "int (*)(void)"),
        ("c ? p : 0", "int *"),
        ("c ? p : 0L", "int *"),
        ("c ? p : (const void *)0", "const void *"),
        ("c ? p : (char *)0", "void *"),
        ("c ? p : cp", "void *"),
        ("c ? cip : cp", "void *"),
        ("c ? p : up", "void *"),
        ("c ? p : 1", "int *"),
        ("c ? 5 : vip", "volatile int *"),
        ("c ? p : vp", "void *"),
        ("c ? cip : vp", "const void *"),
        ("c ? cvp : vip", "const volatile void *"),
        ("c ? cip : vip", "const volatile int *"),
        ("c ? cap : a3", "const int (*)[3]"),
        ("c ? a3 : cap", "const int (*)[3]"),
        ("c ? fnp : fp", "int (*)(void)"),
        ("c ? fp : fnp", "int (*)(void)"),
        ("c ? fp : vp", "void *"),
    ] {
        let src = format!("{decls} __typeof__({expr}) r;");
        let (tu, types, _, _) = parse_tu(&src).unwrap();
        let Some(ExternalDecl::Declaration(decl)) = tu.items.last() else {
            panic!("{expr}: expected a declaration");
        };
        let typ = decl.declarators[0].typ;
        assert_eq!(types.format_type(typ, None), want, "{expr}");
    }
}

/// C17 6.5.15p3 admits only these pairs of arms; any other is an error, which
/// leaves the conditional untyped so no enclosing operator reports it again.
/// The accepted pairs are proved in `cc/tests/diagnostics`, where a stray
/// error from a concurrent test cannot reach the count.
#[test]
fn test_conditional_mismatched_arms_are_errors() {
    for src in [
        "struct S { int a; } s; struct T { int a; } t; int c; void f(void) { c ? s : t; }",
        "struct S { int a; } s; int c; void f(void) { c ? s : 1; }",
        "int *p; int c; void f(void) { c ? 1.0 : p; }",
        "int *p; int c; void f(void) { c ? p : 0.0; }",
    ] {
        let before = crate::diag::error_count();
        parse_tu(src).unwrap_or_else(|e| panic!("{src}: {e:?}"));
        assert!(crate::diag::error_count() > before, "{src}: accepted");
    }
}
