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

use crate::parse::ast::{
    AssignOp, BinaryOp, BlockItem, Declaration, Expr, ExprKind, ExternalDecl, ForInit, FunctionDef,
    Stmt, TranslationUnit, UnaryOp,
};
use crate::parse::parser::{ParseResult, Parser};
use crate::strings::{StringId, StringTable};
use crate::symbol::{Symbol, SymbolTable};
use crate::target::Target;
use crate::token::lexer::Tokenizer;
use crate::types::{TypeKind, TypeModifiers, TypeTable};

fn parse_expr(input: &str) -> ParseResult<(Expr, TypeTable, StringTable, SymbolTable)> {
    parse_expr_with_vars(input, &[])
}

/// Parse expression with pre-declared variables
fn parse_expr_with_vars(
    input: &str,
    vars: &[&str],
) -> ParseResult<(Expr, TypeTable, StringTable, SymbolTable)> {
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

    let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types);
    parser.skip_stream_tokens();
    let expr = parser.parse_expression()?;
    Ok((expr, types, strings, symbols))
}

/// Helper to compare a StringId with a string literal
fn check_name(strings: &StringTable, id: StringId, expected: &str) {
    assert_eq!(strings.get(id), expected);
}

// ========================================================================
// Literal tests
// ========================================================================

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
        ExprKind::FloatLit(v) => assert!((v - 3.14).abs() < 0.001),
        _ => panic!("Expected FloatLit"),
    }
}

#[test]
fn test_char_literal() {
    let (expr, _types, _strings, _symbols) = parse_expr("'a'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('a')));
}

#[test]
fn test_char_escape() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\n'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('\n')));
}

#[test]
fn test_string_literal() {
    let (expr, _types, _strings, _symbols) = parse_expr("\"hello\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "hello"),
        _ => panic!("Expected StringLit"),
    }
}

// ========================================================================
// Character literal escape sequence tests
// ========================================================================

#[test]
fn test_char_escape_newline() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\n'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('\n')));
}

#[test]
fn test_char_escape_tab() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\t'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('\t')));
}

#[test]
fn test_char_escape_carriage_return() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\r'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('\r')));
}

#[test]
fn test_char_escape_backslash() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\\\'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('\\')));
}

#[test]
fn test_char_escape_single_quote() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\''").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('\'')));
}

#[test]
fn test_char_escape_double_quote() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\\"'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('"')));
}

#[test]
fn test_char_escape_bell() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\a'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('\x07')));
}

#[test]
fn test_char_escape_backspace() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\b'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('\x08')));
}

#[test]
fn test_char_escape_formfeed() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\f'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('\x0C')));
}

#[test]
fn test_char_escape_vertical_tab() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\v'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('\x0B')));
}

#[test]
fn test_char_escape_null() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\0'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('\0')));
}

#[test]
fn test_char_escape_hex() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\x41'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('A')));
}

#[test]
fn test_char_escape_hex_lowercase() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\x0a'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('\n')));
}

#[test]
fn test_char_escape_octal() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\101'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('A'))); // octal 101 = 65 = 'A'
}

#[test]
fn test_char_escape_octal_012() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\012'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('\n'))); // octal 012 = 10 = '\n'
}

// ========================================================================
// UCN (Universal Character Name) escape sequence tests - C99 6.4.3
// ========================================================================

#[test]
fn test_char_escape_ucn_short() {
    // \u00E9 is 'é' (U+00E9)
    let (expr, _types, _strings, _symbols) = parse_expr("'\\u00E9'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('é')));
}

#[test]
fn test_char_escape_ucn_short_lowercase() {
    // \u00e9 is 'é' (U+00E9) - lowercase hex
    let (expr, _types, _strings, _symbols) = parse_expr("'\\u00e9'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('é')));
}

#[test]
fn test_char_escape_ucn_long() {
    // \U00000041 is 'A' (U+0041)
    let (expr, _types, _strings, _symbols) = parse_expr("'\\U00000041'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('A')));
}

#[test]
fn test_char_escape_ucn_long_emoji() {
    // \U0001F600 is '😀' (U+1F600)
    let (expr, _types, _strings, _symbols) = parse_expr("'\\U0001F600'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('😀')));
}

#[test]
fn test_string_ucn() {
    // "caf\u00E9" should become "café"
    let (expr, _types, _strings, _symbols) = parse_expr("\"caf\\u00E9\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "café"),
        _ => panic!("Expected StringLit"),
    }
}

#[test]
fn test_string_ucn_long() {
    // Test long UCN in string
    let (expr, _types, _strings, _symbols) = parse_expr("\"hello\\U0001F600world\"").unwrap();
    match expr.kind {
        ExprKind::StringLit(s) => assert_eq!(s, "hello😀world"),
        _ => panic!("Expected StringLit"),
    }
}

// ========================================================================
// String literal escape sequence tests
// ========================================================================

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

// ========================================================================
// Binary operator tests
// ========================================================================

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

// ========================================================================
// Unary operator tests
// ========================================================================

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

// ========================================================================
// Postfix operator tests
// ========================================================================

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
        ExprKind::Call { func, args } => {
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
        ExprKind::Call { func, args } => {
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

// ========================================================================
// Assignment tests
// ========================================================================

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

// ========================================================================
// Ternary expression tests
// ========================================================================

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

// ========================================================================
// Comma expression tests
// ========================================================================

#[test]
fn test_comma_expr() {
    let (expr, _types, _strings, _symbols) = parse_expr("a, b, c").unwrap();
    match expr.kind {
        ExprKind::Comma(exprs) => assert_eq!(exprs.len(), 3),
        _ => panic!("Expected Comma"),
    }
}

// ========================================================================
// sizeof tests
// ========================================================================

#[test]
fn test_sizeof_expr() {
    let (expr, _types, _strings, _symbols) = parse_expr("sizeof x").unwrap();
    assert!(matches!(expr.kind, ExprKind::SizeofExpr(_)));
}

#[test]
fn test_sizeof_type() {
    let (expr, types, _strings, _symbols) = parse_expr("sizeof(int)").unwrap();
    match expr.kind {
        ExprKind::SizeofType(typ) => assert_eq!(types.kind(typ), TypeKind::Int),
        _ => panic!("Expected SizeofType"),
    }
}

#[test]
fn test_sizeof_paren_expr() {
    // sizeof(x) where x is not a type
    let (expr, _types, _strings, _symbols) = parse_expr("sizeof(x)").unwrap();
    assert!(matches!(expr.kind, ExprKind::SizeofExpr(_)));
}

// ========================================================================
// Cast tests
// ========================================================================

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
    let (expr, types, _strings, _symbols) = parse_expr("(const int)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::Int);
            assert!(types
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
    let (expr, types, _strings, _symbols) = parse_expr("(int * const)x").unwrap();
    match expr.kind {
        ExprKind::Cast { cast_type, .. } => {
            assert_eq!(types.kind(cast_type), TypeKind::Pointer);
            // const applies to the pointer itself
            assert!(types
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
fn test_sizeof_compound_type() {
    let (expr, types, _strings, _symbols) = parse_expr("sizeof(unsigned long long)").unwrap();
    match expr.kind {
        ExprKind::SizeofType(typ) => {
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
        ExprKind::SizeofType(typ) => {
            assert_eq!(types.kind(typ), TypeKind::Pointer);
        }
        _ => panic!("Expected SizeofType"),
    }
}

// ========================================================================
// Parentheses tests
// ========================================================================

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

// ========================================================================
// Complex expression tests
// ========================================================================

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

// ========================================================================
// Statement tests
// ========================================================================

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

    let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types);
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
    assert!(matches!(stmt, Stmt::Break));
}

#[test]
fn test_continue_stmt() {
    let (stmt, _strings) = parse_stmt("continue;").unwrap();
    assert!(matches!(stmt, Stmt::Continue));
}

#[test]
fn test_goto_stmt() {
    let (stmt, strings) = parse_stmt("goto label;").unwrap();
    match stmt {
        Stmt::Goto(name) => check_name(&strings, name, "label"),
        _ => panic!("Expected Goto"),
    }
}

#[test]
fn test_labeled_stmt() {
    let (stmt, strings) = parse_stmt("label: x = 1;").unwrap();
    match stmt {
        Stmt::Label { name, stmt } => {
            check_name(&strings, name, "label");
            assert!(matches!(*stmt, Stmt::Expr(_)));
        }
        _ => panic!("Expected Label"),
    }
}

#[test]
fn test_block_stmt() {
    let (stmt, _strings) = parse_stmt("{ x = 1; y = 2; }").unwrap();
    match stmt {
        Stmt::Block(items) => {
            assert_eq!(items.len(), 2);
            assert!(matches!(items[0], BlockItem::Statement(Stmt::Expr(_))));
            assert!(matches!(items[1], BlockItem::Statement(Stmt::Expr(_))));
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

// ========================================================================
// Declaration tests
// ========================================================================

fn parse_decl(input: &str) -> ParseResult<(Declaration, TypeTable, StringTable, SymbolTable)> {
    let mut strings = StringTable::new();
    let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(&Target::host());
    let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types);
    parser.skip_stream_tokens();
    let decl = parser.parse_declaration()?;
    Ok((decl, types, strings, symbols))
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
    // Bug fix: "extern int *p" - EXTERN should be on the pointer type, not just int
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
    // Bug fix: "static int *p" - STATIC should be on the pointer type, not just int
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
    // Bug fix: "typedef int arr[10]" - TYPEDEF should be on the array type, not just int
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

// ========================================================================
// Function parsing tests
// ========================================================================

fn parse_func(input: &str) -> ParseResult<(FunctionDef, TypeTable, StringTable, SymbolTable)> {
    let mut strings = StringTable::new();
    let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(&Target::host());
    let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types);
    parser.skip_stream_tokens();
    let func = parser.parse_function_def()?;
    Ok((func, types, strings, symbols))
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

// ========================================================================
// Translation unit tests
// ========================================================================

fn parse_tu(input: &str) -> ParseResult<(TranslationUnit, TypeTable, StringTable, SymbolTable)> {
    let mut strings = StringTable::new();
    let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(&Target::host());
    let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types);
    let tu = parser.parse_translation_unit()?;
    Ok((tu, types, strings, symbols))
}

#[test]
fn test_simple_program() {
    let (tu, _types, _strings, _symbols) = parse_tu("int main() { return 0; }").unwrap();
    assert_eq!(tu.items.len(), 1);
    assert!(matches!(tu.items[0], ExternalDecl::FunctionDef(_)));
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

// ========================================================================
// Typedef tests
// ========================================================================

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

// ========================================================================
// Restrict qualifier tests
// ========================================================================

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

// ========================================================================
// Volatile qualifier tests
// ========================================================================

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

// ========================================================================
// __attribute__ tests
// ========================================================================

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

// ========================================================================
// Function declaration tests (prototypes)
// ========================================================================

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

// ========================================================================
// Variadic function declaration tests
// ========================================================================

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

// ========================================================================
// Function pointer tests
// ========================================================================

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
    // Regression test for issue where outer pointers in grouped declarators
    // were applied after the function type instead of before (to the return type)
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
    // Regression test for issue where type chain was incorrectly built
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

// ========================================================================
// Bitfield tests
// ========================================================================

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

// ========================================================================
// Enum tests
// ========================================================================

#[test]
fn test_empty_enum_warning() {
    // Empty enum is a GNU extension - should parse successfully with a warning
    // The warning is emitted to stderr during the test
    let (tu, types, _strings, _symbols) = parse_tu("enum E {};").unwrap();
    assert_eq!(tu.items.len(), 1);
    if let ExternalDecl::Declaration(decl) = &tu.items[0] {
        // This is a type declaration (enum only, no variable)
        // The enum should have no constants
        let typ = decl.declarators.first().map(|d| d.typ);
        if let Some(typ_id) = typ {
            if let Some(composite) = types.composite(typ_id) {
                assert!(composite.enum_constants.is_empty());
            }
        }
    }
}

// ========================================================================
// Typedef with trailing qualifiers tests (C99)
// Tests for "typedef_name const" and "typedef_name volatile" syntax
// ========================================================================

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

// ========================================================================
// Bug fix regression tests
// ========================================================================

#[test]
fn test_forward_declared_struct_member_access() {
    // Regression test: forward-declared struct pointer member access
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
    // This tests that resolve_struct_type works for arrow operator
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
    // Previously, the parser only handled TypeKind::Function, not pointers to functions
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

// ========================================================================
// Designated Initializer Edge Cases
// ========================================================================

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

// ========================================================================
// Complex Declarator Edge Cases
// ========================================================================

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

// ========================================================================
// Array Parameter Edge Cases
// ========================================================================

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

// ========================================================================
// Type Parsing Edge Cases
// ========================================================================

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

// ========================================================================
// Escape Sequence Edge Cases
// ========================================================================

#[test]
fn test_octal_escape_boundary() {
    // \377 is max valid octal for 8-bit char
    let (expr, _types, _strings, _symbols) = parse_expr("'\\377'").unwrap();
    match expr.kind {
        ExprKind::CharLit(ch) => {
            assert_eq!(ch as u32, 0o377); // 255 in decimal
        }
        _ => panic!("Expected character constant, got {:?}", expr.kind),
    }
}

#[test]
fn test_hex_escape_single_digit() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\x0'").unwrap();
    match expr.kind {
        ExprKind::CharLit(ch) => {
            assert_eq!(ch as u32, 0);
        }
        _ => panic!("Expected character constant, got {:?}", expr.kind),
    }
}

#[test]
fn test_hex_escape_max() {
    let (expr, _types, _strings, _symbols) = parse_expr("'\\xff'").unwrap();
    match expr.kind {
        ExprKind::CharLit(ch) => {
            assert_eq!(ch as u32, 0xff);
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

// ========================================================================
// Expression Edge Cases
// ========================================================================

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
        ExprKind::SizeofType(_) => {}
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

// ========================================================================
// __builtin_offsetof tests
// ========================================================================

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

// ========================================================================
// Statement expression tests (GNU extension)
// ========================================================================

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

// ========================================================================
// Integer literal type promotion tests (C99 6.4.4.1)
// ========================================================================

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

// ========================================================================
// Incomplete array size from string literal tests
// ========================================================================

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

// ========================================================================
// typeof operator tests (GCC extension)
// ========================================================================

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

// ========================================================================
// Anonymous struct/union member tests (C11)
// ========================================================================

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

// ========================================================================
// Statement expression void type tests
// ========================================================================

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

// ========================================================================
// Wide string literal tests
// ========================================================================

#[test]
fn test_wide_string_literal_basic() {
    // L"hello" should parse as WideStringLit
    let (expr, types, _, _) = parse_expr("L\"hello\"").unwrap();
    match &expr.kind {
        ExprKind::WideStringLit(s) => {
            assert_eq!(s, "hello");
        }
        _ => panic!("Expected WideStringLit, got {:?}", expr.kind),
    }
    // Type should be wchar_t[N] (int[N] on this platform), not wchar_t*
    // C11 6.4.5: "wide string literal has type wchar_t[N]"
    let typ = expr.typ.unwrap();
    assert_eq!(types.kind(typ), TypeKind::Array);
    let elem_type = types.get(typ).base.unwrap();
    assert_eq!(types.kind(elem_type), TypeKind::Int);
    // Array size should be 6 (5 chars + null terminator)
    assert_eq!(types.array_size(typ), Some(6));
}

#[test]
fn test_wide_string_literal_concatenation() {
    // Adjacent wide string literals should concatenate
    let (expr, _, _, _) = parse_expr("L\"hello\" L\" world\"").unwrap();
    match &expr.kind {
        ExprKind::WideStringLit(s) => {
            assert_eq!(s, "hello world");
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

// ========================================================================
// Function parameter adjustment tests
// ========================================================================

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

// ========================================================================
// __FUNCTION__ and __PRETTY_FUNCTION__ parsing tests
// ========================================================================

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

// ========================================================================
// Long double type parsing tests
// ========================================================================

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

// ========================================================================
// C11 _Atomic qualifier tests
// ========================================================================

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

// =======================================================================
// Atomic builtin tests
// =======================================================================

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

// ========================================================================
// C23 _Float* type tests (TS 18661-3)
// ========================================================================

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

// ========================================================================
// C23 _Float* literal suffix tests (f16, f32, f64)
// ========================================================================

#[test]
fn test_float16_literal_suffix() {
    let (expr, types, _, _) = parse_expr("1.0f16").unwrap();
    match expr.kind {
        ExprKind::FloatLit(v) => assert!((v - 1.0).abs() < 0.001),
        _ => panic!("Expected FloatLit"),
    }
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Float16);
}

#[test]
fn test_float16_literal_suffix_upper() {
    let (expr, types, _, _) = parse_expr("3.14F16").unwrap();
    match expr.kind {
        ExprKind::FloatLit(v) => assert!((v - 3.14).abs() < 0.001),
        _ => panic!("Expected FloatLit"),
    }
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Float16);
}

#[test]
fn test_float32_literal_suffix() {
    // f32 is alias for float
    let (expr, types, _, _) = parse_expr("2.5f32").unwrap();
    match expr.kind {
        ExprKind::FloatLit(v) => assert!((v - 2.5).abs() < 0.001),
        _ => panic!("Expected FloatLit"),
    }
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Float);
}

#[test]
fn test_float64_literal_suffix() {
    // f64 is alias for double
    let (expr, types, _, _) = parse_expr("2.5f64").unwrap();
    match expr.kind {
        ExprKind::FloatLit(v) => assert!((v - 2.5).abs() < 0.001),
        _ => panic!("Expected FloatLit"),
    }
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Double);
}

#[test]
fn test_int_with_float16_suffix() {
    // Integer with f16 suffix becomes float literal
    let (expr, types, _, _) = parse_expr("42f16").unwrap();
    match expr.kind {
        ExprKind::FloatLit(v) => assert!((v - 42.0).abs() < 0.001),
        _ => panic!("Expected FloatLit"),
    }
    assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Float16);
}

// ========================================================================
// _Alignof expression tests (C11)
// ========================================================================

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

// ========================================================================
// __builtin_nan/nanf/nanl tests
// ========================================================================

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
    // Signaling NaN variant (same implementation, returns NaN)
    let (expr, types, _, _) = parse_expr("__builtin_nans(\"\")").unwrap();
    assert!(matches!(expr.kind, ExprKind::FloatLit(v) if v.is_nan()));
    assert_eq!(expr.typ, Some(types.double_id));
}

// ========================================================================
// __builtin_flt_rounds test
// ========================================================================

#[test]
fn test_builtin_flt_rounds() {
    // __builtin_flt_rounds() returns 1 (round to nearest, IEEE 754 default)
    let (expr, types, _, _) = parse_expr("__builtin_flt_rounds()").unwrap();
    assert!(matches!(expr.kind, ExprKind::IntLit(1)));
    assert_eq!(expr.typ, Some(types.int_id));
}

// ========================================================================
// __builtin_expect test
// ========================================================================

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

// ========================================================================
// Wide character literal test
// ========================================================================

#[test]
fn test_wide_char_literal() {
    let (expr, types, _, _) = parse_expr("L'A'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('A')));
    // wchar_t is int on most Unix systems
    assert_eq!(expr.typ, Some(types.int_id));
}

#[test]
fn test_wide_char_escape() {
    let (expr, _, _, _) = parse_expr("L'\\n'").unwrap();
    assert!(matches!(expr.kind, ExprKind::CharLit('\n')));
}

// ========================================================================
// Hex float suffix fix test (f16/f32/f64 are hex digits, not suffixes)
// ========================================================================

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
