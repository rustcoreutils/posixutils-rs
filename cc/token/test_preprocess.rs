//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Unit tests for the preprocessor. Attached as a child module by `preprocess.rs`, so it
// still reaches that module's private items exactly as an inline
// `mod tests` did.
//

use super::*;
use crate::token::lexer::Tokenizer;

fn preprocess_str(input: &str) -> (Vec<Token>, IdentTable) {
    let target = Target::host();
    let mut strings = IdentTable::new();
    let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let result = preprocess_with_defines(
        tokens,
        &target,
        &mut strings,
        "<test>",
        &PreprocessConfig::default(),
    );
    (result, strings)
}

fn get_token_strings(tokens: &[Token], idents: &IdentTable) -> Vec<String> {
    tokens
        .iter()
        .filter_map(|t| match &t.typ {
            TokenType::Ident => {
                if let TokenValue::Ident(id) = &t.value {
                    idents.get_opt(*id).map(|s| s.to_string())
                } else {
                    None
                }
            }
            TokenType::Number => {
                if let TokenValue::Number(n) = &t.value {
                    Some(n.clone())
                } else {
                    None
                }
            }
            TokenType::String => {
                if let TokenValue::String(s) = &t.value {
                    Some(format!("\"{}\"", s))
                } else {
                    None
                }
            }
            TokenType::Special => {
                if let TokenValue::Special(code) = &t.value {
                    if *code < 256 {
                        Some((*code as u8 as char).to_string())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        })
        .collect()
}

#[test]
fn test_simple_define() {
    let (tokens, idents) = preprocess_str("#define FOO 42\nFOO");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"42".to_string()));
}

#[test]
fn test_undef() {
    let (tokens, idents) = preprocess_str("#define FOO 42\n#undef FOO\nFOO");
    let strs = get_token_strings(&tokens, &idents);
    // FOO should not be expanded after undef
    assert!(strs.contains(&"FOO".to_string()));
}

#[test]
fn test_ifdef_true() {
    let (tokens, idents) = preprocess_str("#define FOO\n#ifdef FOO\nyes\n#endif");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_ifdef_false() {
    let (tokens, idents) = preprocess_str("#ifdef FOO\nyes\n#endif\nno");
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"yes".to_string()));
    assert!(strs.contains(&"no".to_string()));
}

#[test]
fn test_ifndef_true() {
    let (tokens, idents) = preprocess_str("#ifndef FOO\nyes\n#endif");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_ifndef_false() {
    let (tokens, idents) = preprocess_str("#define FOO\n#ifndef FOO\nyes\n#endif\nno");
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"yes".to_string()));
    assert!(strs.contains(&"no".to_string()));
}

#[test]
fn test_ifdef_else() {
    let (tokens, idents) = preprocess_str("#ifdef FOO\nyes\n#else\nno\n#endif");
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"yes".to_string()));
    assert!(strs.contains(&"no".to_string()));
}

#[test]
fn test_nested_ifdef() {
    let (tokens, idents) =
        preprocess_str("#define A\n#ifdef A\n#ifdef B\ninner\n#endif\nouter\n#endif");
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"inner".to_string())); // B not defined
    assert!(strs.contains(&"outer".to_string())); // A is defined
}

#[test]
fn test_if_true() {
    let (tokens, idents) = preprocess_str("#if 1\nyes\n#endif");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_if_false() {
    let (tokens, idents) = preprocess_str("#if 0\nyes\n#endif\nno");
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"yes".to_string()));
    assert!(strs.contains(&"no".to_string()));
}

#[test]
fn test_if_defined() {
    let (tokens, idents) = preprocess_str("#define FOO\n#if defined(FOO)\nyes\n#endif");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_elif() {
    let (tokens, idents) = preprocess_str("#if 0\none\n#elif 1\ntwo\n#else\nthree\n#endif");
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"one".to_string()));
    assert!(strs.contains(&"two".to_string()));
    assert!(!strs.contains(&"three".to_string()));
}

#[test]
fn test_predefined_stdc() {
    let target = Target::host();
    let pp = Preprocessor::new(&target, "test.c", &SystemSearch::default());
    assert!(pp.is_defined("__STDC__"));
    assert!(pp.is_defined("__STDC_VERSION__"));
}

#[test]
fn test_predefined_arch() {
    let target = Target::host();
    let pp = Preprocessor::new(&target, "test.c", &SystemSearch::default());

    // Should have either x86_64 or aarch64 defined
    assert!(pp.is_defined("__x86_64__") || pp.is_defined("__aarch64__"));
}

#[test]
fn test_line_macro() {
    let (tokens, _idents) = preprocess_str("__LINE__");
    // Should have a number token
    assert!(tokens.iter().any(|t| t.typ == TokenType::Number));
}

#[test]
fn test_counter_macro() {
    let (tokens, _idents) = preprocess_str("__COUNTER__ __COUNTER__ __COUNTER__");
    let nums: Vec<_> = tokens
        .iter()
        .filter_map(|t| {
            if let TokenValue::Number(n) = &t.value {
                Some(n.clone())
            } else {
                None
            }
        })
        .collect();
    // Should have 0, 1, 2
    assert_eq!(nums, vec!["0", "1", "2"]);
}

#[test]
fn test_deeply_nested_conditionals() {
    let input = r#"
#define A
#ifdef A
    level1
    #ifdef B
        level2a
    #else
        level2b
        #ifdef A
            level3
        #endif
    #endif
#endif
"#;
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);

    assert!(strs.contains(&"level1".to_string()));
    assert!(!strs.contains(&"level2a".to_string())); // B not defined
    assert!(strs.contains(&"level2b".to_string())); // else branch
    assert!(strs.contains(&"level3".to_string())); // A still defined
}

#[test]
fn test_else_basic() {
    // Ensure #else works correctly when condition is false
    let (tokens, idents) = preprocess_str("#if 0\nyes\n#else\nno\n#endif");
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"yes".to_string()));
    assert!(strs.contains(&"no".to_string()));
}

#[test]
fn test_endif_basic() {
    // Ensure #endif properly closes conditional blocks
    let (tokens, idents) = preprocess_str("#ifdef FOO\nskipped\n#endif\nafter");
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"skipped".to_string()));
    assert!(strs.contains(&"after".to_string()));
}

#[test]
fn test_include_skipped_in_false_branch() {
    // #include in a false branch should be skipped
    let (tokens, idents) = preprocess_str("#if 0\n#include <stdio.h>\n#endif\ncode");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"code".to_string()));
    // No error from trying to include stdio.h
}

#[test]
fn test_error_skipped_in_false_branch() {
    // #error in a false branch should not trigger
    let (tokens, idents) = preprocess_str("#if 0\n#error This should not trigger\n#endif\ncode");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"code".to_string()));
}

#[test]
fn test_warning_skipped_in_false_branch() {
    // #warning in a false branch should not trigger
    let (tokens, idents) = preprocess_str("#if 0\n#warning This should not trigger\n#endif\ncode");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"code".to_string()));
}

#[test]
fn test_pragma_ignored() {
    // #pragma should be silently ignored
    let (tokens, idents) = preprocess_str("#pragma once\ncode");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"code".to_string()));
}

#[test]
fn test_line_directive_consumed() {
    // #line should be consumed and not pass through as tokens
    let (tokens, idents) = preprocess_str("#line 100\ncode");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"code".to_string()));
}

#[test]
fn test_define_with_value() {
    // Test #define with a specific value
    let (tokens, idents) = preprocess_str("#define VALUE 123\nVALUE");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"123".to_string()));
}

#[test]
fn test_define_empty() {
    // Test #define without value (flag-style macro)
    let (tokens, idents) = preprocess_str("#define FLAG\n#ifdef FLAG\nyes\n#endif");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_undef_removes_macro() {
    // Verify #undef removes a macro so #ifdef fails
    let (tokens, idents) = preprocess_str("#define FOO\n#undef FOO\n#ifdef FOO\nyes\n#endif\nno");
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"yes".to_string()));
    assert!(strs.contains(&"no".to_string()));
}

#[test]
fn test_function_like_macro() {
    let (tokens, idents) = preprocess_str("#define ADD(a, b) a + b\nADD(1, 2)");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"1".to_string()));
    assert!(strs.contains(&"+".to_string()));
    assert!(strs.contains(&"2".to_string()));
}

#[test]
fn test_if_logical_and() {
    let (tokens, idents) = preprocess_str("#if 1 && 1\nyes\n#endif");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));

    let (tokens, idents) = preprocess_str("#if 1 && 0\nyes\n#endif\nno");
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"yes".to_string()));
    assert!(strs.contains(&"no".to_string()));
}

#[test]
fn test_if_logical_or() {
    let (tokens, idents) = preprocess_str("#if 0 || 1\nyes\n#endif");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));

    let (tokens, idents) = preprocess_str("#if 0 || 0\nyes\n#endif\nno");
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"yes".to_string()));
    assert!(strs.contains(&"no".to_string()));
}

#[test]
fn test_if_not() {
    let (tokens, idents) = preprocess_str("#if !0\nyes\n#endif");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));

    let (tokens, idents) = preprocess_str("#if !1\nyes\n#endif\nno");
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"yes".to_string()));
    assert!(strs.contains(&"no".to_string()));
}

#[test]
fn test_if_comparison() {
    let (tokens, idents) = preprocess_str("#if 5 > 3\nyes\n#endif");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));

    let (tokens, idents) = preprocess_str("#if 5 < 3\nyes\n#endif\nno");
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"yes".to_string()));
    assert!(strs.contains(&"no".to_string()));
}

// ========================================================================
// Header guard tests
// ========================================================================

#[test]
fn test_header_guard_basic() {
    // Simulates typical header guard pattern
    let input = r#"
#ifndef MY_HEADER_H
#define MY_HEADER_H
first_include
#endif
#ifndef MY_HEADER_H
#define MY_HEADER_H
second_include
#endif
"#;
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"first_include".to_string()));
    assert!(!strs.contains(&"second_include".to_string()));
}

#[test]
fn test_header_guard_ifdef_style() {
    // Alternative header guard using #ifdef
    let input = r#"
#ifdef GUARD
#else
#define GUARD
first
#endif
#ifdef GUARD
second
#endif
"#;
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"first".to_string()));
    assert!(strs.contains(&"second".to_string()));
}

// ========================================================================
// Multiple elif chain tests
// ========================================================================

#[test]
fn test_multiple_elif_first() {
    let input = "#if 1\none\n#elif 1\ntwo\n#elif 1\nthree\n#else\nfour\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"one".to_string()));
    assert!(!strs.contains(&"two".to_string()));
    assert!(!strs.contains(&"three".to_string()));
    assert!(!strs.contains(&"four".to_string()));
}

#[test]
fn test_multiple_elif_middle() {
    let input = "#if 0\none\n#elif 0\ntwo\n#elif 1\nthree\n#else\nfour\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"one".to_string()));
    assert!(!strs.contains(&"two".to_string()));
    assert!(strs.contains(&"three".to_string()));
    assert!(!strs.contains(&"four".to_string()));
}

#[test]
fn test_multiple_elif_else() {
    let input = "#if 0\none\n#elif 0\ntwo\n#elif 0\nthree\n#else\nfour\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"one".to_string()));
    assert!(!strs.contains(&"two".to_string()));
    assert!(!strs.contains(&"three".to_string()));
    assert!(strs.contains(&"four".to_string()));
}

// ========================================================================
// Defined operator tests
// ========================================================================

#[test]
fn test_defined_without_parens() {
    let input = "#define FOO\n#if defined FOO\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_defined_not_defined() {
    let input = "#if defined(BAR)\nyes\n#endif\nno";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"yes".to_string()));
    assert!(strs.contains(&"no".to_string()));
}

#[test]
fn test_defined_negated() {
    let input = "#if !defined(FOO)\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_defined_in_complex_expr() {
    let input = "#define A\n#if defined(A) && !defined(B)\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

// ========================================================================
// Macro expansion tests
// ========================================================================

#[test]
fn test_multi_token_macro() {
    let input = "#define EXPR 1 + 2 + 3\nEXPR";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"1".to_string()));
    assert!(strs.contains(&"2".to_string()));
    assert!(strs.contains(&"3".to_string()));
}

#[test]
fn test_nested_macro_expansion() {
    let input = "#define A B\n#define B 42\nA";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"42".to_string()));
}

#[test]
fn test_macro_in_if_expr() {
    let input = "#define VAL 5\n#if VAL > 3\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_macro_redefinition() {
    // An incompatible redefinition is diagnosed (C17 6.10.3p2) but is not
    // fatal: the standard requires only a diagnostic, and rejecting would
    // break a great deal of code that redefines a macro benignly. The
    // later definition wins, as it always has.
    //
    // This test used to assert the silent override *as intended*, which is
    // why the constraint went unimplemented.
    let input = "#define X 1\n#define X 2\nX";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"2".to_string()));
    assert!(!strs.contains(&"1".to_string()));
}

#[test]
fn test_macro_redefinition_conflict_detection() {
    // Build a macro as if it came from a `#define` directive: the
    // implementation-predefined flag exempts a macro from the constraint,
    // which is not what is under test here.
    fn obj(name: &str, value: &str) -> Macro {
        let mut m = Macro::predefined(name, Some(value));
        m.predefined = false;
        m
    }

    // Identical replacement lists are permitted.
    assert!(macro_redefinition_conflict(&obj("A", "1"), &obj("A", "1")).is_none());
    // Differing ones are not.
    assert!(macro_redefinition_conflict(&obj("A", "1"), &obj("A", "2")).is_some());

    // Object-like versus function-like.
    let mut fnlike = obj("A", "1");
    fnlike.is_function = true;
    fnlike.params = vec![MacroParam {
        name: "x".into(),
        index: 0,
    }];
    assert!(macro_redefinition_conflict(&obj("A", "1"), &fnlike).is_some());

    // Same shape, differently spelled parameters.
    let mut renamed = fnlike.clone();
    renamed.params = vec![MacroParam {
        name: "y".into(),
        index: 0,
    }];
    assert!(macro_redefinition_conflict(&fnlike, &renamed).is_some());

    // A parameter list that matches is fine.
    assert!(macro_redefinition_conflict(&fnlike, &fnlike.clone()).is_none());
}

#[test]
fn test_replacement_lists_ignore_leading_whitespace() {
    // Whitespace before the first replacement token is not a separation
    // *within* the list. Without this, every compilation against glibc
    // warned: we predefine __GLIBC__ with no leading space, while
    // features.h writes `#define __GLIBC__ 2` with one.
    let a = vec![MacroToken {
        typ: TokenType::Number,
        value: MacroTokenValue::Number("2".into()),
        whitespace: false,
        spelling: Spelling::Canonical,
    }];
    let b = vec![MacroToken {
        typ: TokenType::Number,
        value: MacroTokenValue::Number("2".into()),
        whitespace: true,
        spelling: Spelling::Canonical,
    }];
    assert!(replacement_lists_identical(&a, &b));

    // But whitespace between tokens still counts.
    let two = |ws: bool| {
        vec![
            MacroToken {
                typ: TokenType::Number,
                value: MacroTokenValue::Number("1".into()),
                whitespace: false,
                spelling: Spelling::Canonical,
            },
            MacroToken {
                typ: TokenType::Number,
                value: MacroTokenValue::Number("2".into()),
                whitespace: ws,
                spelling: Spelling::Canonical,
            },
        ]
    };
    assert!(!replacement_lists_identical(&two(true), &two(false)));
}

// ========================================================================
// Arithmetic in #if expressions
// ========================================================================

#[test]
fn test_if_addition() {
    let input = "#if 2 + 3 == 5\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_if_subtraction() {
    let input = "#if 10 - 3 == 7\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_if_multiplication() {
    let input = "#if 3 * 4 == 12\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_if_division() {
    let input = "#if 12 / 4 == 3\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_if_modulo() {
    let input = "#if 10 % 3 == 1\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_if_parentheses() {
    let input = "#if (2 + 3) * 2 == 10\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

// ========================================================================
// Comparison operators in #if
// ========================================================================

#[test]
fn test_if_equal() {
    let input = "#if 5 == 5\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_if_not_equal() {
    let input = "#if 5 != 3\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_if_less_equal() {
    let input = "#if 3 <= 3\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_if_greater_equal() {
    let input = "#if 5 >= 5\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

// ========================================================================
// Bitwise operators in #if
// ========================================================================

#[test]
fn test_if_bitwise_and() {
    let input = "#if 0xFF & 0x0F == 0x0F\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_if_bitwise_or() {
    let input = "#if (0xF0 | 0x0F) == 0xFF\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

// ========================================================================
// Edge cases
// ========================================================================

#[test]
fn test_empty_if_block() {
    let input = "#if 1\n#endif\nafter";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"after".to_string()));
}

#[test]
fn test_empty_else_block() {
    let input = "#if 0\nskipped\n#else\n#endif\nafter";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"skipped".to_string()));
    assert!(strs.contains(&"after".to_string()));
}

#[test]
fn test_consecutive_conditionals() {
    let input = "#if 1\nfirst\n#endif\n#if 1\nsecond\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"first".to_string()));
    assert!(strs.contains(&"second".to_string()));
}

#[test]
fn test_undefined_macro_is_zero() {
    // Undefined macros evaluate to 0 in #if expressions
    let input = "#if UNDEFINED\nyes\n#endif\nno";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"yes".to_string()));
    assert!(strs.contains(&"no".to_string()));
}

#[test]
fn test_ternary_in_if() {
    let input = "#if 1 ? 1 : 0\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

// Tests for nested conditional skipping (bug fix)
#[test]
fn test_nested_if_else_in_skipped_block() {
    // When outer #ifndef is false (guard defined), inner #if/#else should not activate
    let input = r#"
#define GUARD
#ifndef GUARD
outer_skipped
#if 0
inner_if_skipped
#else
inner_else_should_also_skip
#endif
#endif
after
"#;
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"outer_skipped".to_string()));
    assert!(!strs.contains(&"inner_if_skipped".to_string()));
    assert!(!strs.contains(&"inner_else_should_also_skip".to_string()));
    assert!(strs.contains(&"after".to_string()));
}

#[test]
fn test_nested_elif_in_skipped_block() {
    // When outer block is skipped, nested #elif should not activate
    let input = r#"
#define GUARD
#ifndef GUARD
#if 0
a
#elif 1
b_should_not_appear
#else
c
#endif
#endif
done
"#;
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"a".to_string()));
    assert!(!strs.contains(&"b_should_not_appear".to_string()));
    assert!(!strs.contains(&"c".to_string()));
    assert!(strs.contains(&"done".to_string()));
}

#[test]
fn test_deeply_nested_skipped_conditionals() {
    // Multiple levels of nesting inside a skipped block
    let input = r#"
#if 0
level1
#if 1
level2_should_skip
#if 1
level3_should_skip
#else
level3_else_should_skip
#endif
#endif
#endif
visible
"#;
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"level1".to_string()));
    assert!(!strs.contains(&"level2_should_skip".to_string()));
    assert!(!strs.contains(&"level3_should_skip".to_string()));
    assert!(!strs.contains(&"level3_else_should_skip".to_string()));
    assert!(strs.contains(&"visible".to_string()));
}

// Tests for token pasting in object-like macros (bug fix)
#[test]
fn test_token_paste_object_macro() {
    let input = "#define CONCAT a ## b\nCONCAT";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"ab".to_string()));
}

#[test]
fn test_token_paste_object_macro_numbers() {
    let input = "#define NUM 1 ## 2 ## 3\nNUM";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"123".to_string()));
}

#[test]
fn test_token_paste_object_macro_mixed() {
    let input = "#define PREFIX foo ## 123\nPREFIX";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"foo123".to_string()));
}

// Tests for token pasting in function-like macros
#[test]
fn test_token_paste_function_macro() {
    let input = "#define CONCAT(a, b) a ## b\nCONCAT(foo, bar)";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"foobar".to_string()));
}

#[test]
fn test_token_paste_function_macro_prefix() {
    let input = "#define MAKE_ID(x) id_ ## x\nMAKE_ID(test)";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"id_test".to_string()));
}

#[test]
fn test_token_paste_function_macro_suffix() {
    let input = "#define MAKE_FUNC(x) x ## _func\nMAKE_FUNC(my)";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"my_func".to_string()));
}

#[test]
fn test_token_paste_creates_identifier() {
    // Pasting should create a new identifier that can be used
    let input = r#"
#define PASTE(a, b) a ## b
#define foobar 42
PASTE(foo, bar)
"#;
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    // foobar should expand to 42
    assert!(strs.contains(&"42".to_string()));
}

// ========================================================================
// Tests for __INCLUDE_LEVEL__ macro
// ========================================================================

#[test]
fn test_include_level_macro() {
    // At the top level, __INCLUDE_LEVEL__ should be 0
    let (tokens, _idents) = preprocess_str("__INCLUDE_LEVEL__");
    let nums: Vec<_> = tokens
        .iter()
        .filter_map(|t| {
            if let TokenValue::Number(n) = &t.value {
                Some(n.clone())
            } else {
                None
            }
        })
        .collect();
    assert!(nums.contains(&"0".to_string()));
}

// ========================================================================
// Tests for __BASE_FILE__ macro
// ========================================================================

#[test]
fn test_base_file_macro() {
    // __BASE_FILE__ should return the base filename
    let (tokens, _idents) = preprocess_str("__BASE_FILE__");
    // Should have a string token
    assert!(tokens.iter().any(|t| t.typ == TokenType::String));
}

// ========================================================================
// Tests for ternary operator in #if expressions
// ========================================================================

#[test]
fn test_ternary_true_branch() {
    let (tokens, idents) = preprocess_str("#if 1 ? 1 : 0\nyes\n#endif");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_ternary_false_branch() {
    let (tokens, idents) = preprocess_str("#if 0 ? 1 : 0\nyes\n#endif\nno");
    let strs = get_token_strings(&tokens, &idents);
    assert!(!strs.contains(&"yes".to_string()));
    assert!(strs.contains(&"no".to_string()));
}

#[test]
fn test_ternary_nested() {
    // Nested ternary: 1 ? (0 ? 1 : 2) : 3 = 2
    let input = "#if (1 ? (0 ? 1 : 2) : 3) == 2\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_ternary_with_expressions() {
    // ((5 > 3) ? 10 : 20) == 10 = 1 (true)
    let input = "#if ((5 > 3) ? 10 : 20) == 10\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

#[test]
fn test_ternary_with_defined() {
    let input = "#define FOO\n#if defined(FOO) ? 1 : 0\nyes\n#endif";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"yes".to_string()));
}

// ========================================================================
// Tests for GNU ,##__VA_ARGS__ comma suppression
// ========================================================================

#[test]
fn test_va_args_basic() {
    // Basic variadic macro with arguments
    let input = "#define DEBUG(fmt, ...) fmt __VA_ARGS__\nDEBUG(hello, world)";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"hello".to_string()));
    assert!(strs.contains(&"world".to_string()));
}

#[test]
fn test_va_args_comma_suppression_with_args() {
    // ,##__VA_ARGS__ with arguments - comma should remain
    let input = "#define DEBUG(fmt, ...) fmt, ##__VA_ARGS__\nDEBUG(hello, world)";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"hello".to_string()));
    assert!(strs.contains(&",".to_string()));
    assert!(strs.contains(&"world".to_string()));
}

#[test]
fn test_va_args_comma_suppression_no_args() {
    // ,##__VA_ARGS__ without variadic arguments - comma should be suppressed
    let input = "#define DEBUG(fmt, ...) fmt, ##__VA_ARGS__\nDEBUG(hello)";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"hello".to_string()));
    // The comma should be suppressed when __VA_ARGS__ is empty
    // Count commas - should be 0 or fewer than with args
    let comma_count = strs.iter().filter(|s| *s == ",").count();
    assert_eq!(
        comma_count, 0,
        "Comma should be suppressed when VA_ARGS is empty"
    );
}

#[test]
fn test_va_args_comma_suppression_multiple_args() {
    // ,##__VA_ARGS__ with multiple variadic arguments
    let input = "#define DEBUG(fmt, ...) fmt, ##__VA_ARGS__\nDEBUG(hello, a, b, c)";
    let (tokens, idents) = preprocess_str(input);
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"hello".to_string()));
    assert!(strs.contains(&"a".to_string()));
    assert!(strs.contains(&"b".to_string()));
    assert!(strs.contains(&"c".to_string()));
}

#[test]
fn test_chained_paste_expansion() {
    // Token paste creates function-like macro name, arguments come from outside.
    // This tests the case: CALL(ADD)(10, 32) where ## creates ADD_func,
    // and then (10, 32) from outside should trigger ADD_func expansion.
    let (tokens, idents) = preprocess_str(
        "#define ADD_func(x, y) ((x) + (y))\n\
             #define CONCAT(a, b) a ## b\n\
             #define CALL(name) CONCAT(name, _func)\n\
             CALL(ADD)(10, 32)",
    );
    let strs = get_token_strings(&tokens, &idents);
    // Should expand to ((10) + (32))
    assert!(strs.contains(&"10".to_string()));
    assert!(strs.contains(&"32".to_string()));
    assert!(strs.contains(&"+".to_string()));
    // Should NOT contain ADD_func as unexpanded identifier
    assert!(!strs.contains(&"ADD_func".to_string()));
}

// ========================================================================
// _Pragma operator tests (C99)
// ========================================================================

#[test]
fn test_pragma_operator_basic() {
    // _Pragma("...") should be silently consumed
    let (tokens, idents) = preprocess_str("_Pragma(\"GCC diagnostic ignored\") int x;");
    let strs = get_token_strings(&tokens, &idents);
    // _Pragma should be consumed, only "int x ;" should remain
    assert!(strs.contains(&"int".to_string()));
    assert!(strs.contains(&"x".to_string()));
    assert!(!strs.contains(&"_Pragma".to_string()));
}

#[test]
fn test_pragma_operator_from_macro() {
    // _Pragma from macro expansion
    let (tokens, idents) = preprocess_str(
        "#define PRAGMA(x) _Pragma(#x)\n\
             #define DISABLE_WARNING(w) PRAGMA(GCC diagnostic ignored #w)\n\
             DISABLE_WARNING(-Wsign-compare)\n\
             int y;",
    );
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"int".to_string()));
    assert!(strs.contains(&"y".to_string()));
    // _Pragma should be consumed
    assert!(!strs.contains(&"_Pragma".to_string()));
}

#[test]
fn test_pragma_operator_multiple() {
    // Multiple _Pragma operators
    let (tokens, idents) =
        preprocess_str("_Pragma(\"once\") _Pragma(\"GCC diagnostic push\") int z;");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"int".to_string()));
    assert!(strs.contains(&"z".to_string()));
    assert!(!strs.contains(&"_Pragma".to_string()));
}

// ========================================================================
// Include guard detection tests
// ========================================================================

#[test]
fn test_detect_include_guard_ifndef_define() {
    // Standard include guard pattern: #ifndef X / #define X
    let content = b"#ifndef FOO_H\n#define FOO_H\n// content\n#endif\n";
    assert_eq!(
        Preprocessor::detect_include_guard(content),
        Some("FOO_H".to_string())
    );
}

#[test]
fn test_detect_include_guard_with_leading_comment() {
    // Include guard with leading block comment
    let content = b"/* Header file */\n#ifndef MY_HEADER_H\n#define MY_HEADER_H\n#endif\n";
    assert_eq!(
        Preprocessor::detect_include_guard(content),
        Some("MY_HEADER_H".to_string())
    );
}

#[test]
fn test_detect_include_guard_with_line_comment() {
    // Include guard with leading line comment
    let content = b"// Header file\n#ifndef GUARD_H\n#define GUARD_H\n#endif\n";
    assert_eq!(
        Preprocessor::detect_include_guard(content),
        Some("GUARD_H".to_string())
    );
}

#[test]
fn test_detect_include_guard_no_define() {
    // Not a guard - #ifndef without matching #define
    let content = b"#ifndef FOO_H\n#error \"Use other header\"\n#endif\n";
    assert_eq!(Preprocessor::detect_include_guard(content), None);
}

#[test]
fn test_detect_include_guard_different_macro() {
    // Not a guard - #define defines different macro
    let content = b"#ifndef FOO_H\n#define BAR_H\n#endif\n";
    assert_eq!(Preprocessor::detect_include_guard(content), None);
}

#[test]
fn test_detect_include_guard_if_not_defined() {
    // Alternative pattern: #if !defined(X)
    let content = b"#if !defined(MYGUARD)\n#define MYGUARD\n#endif\n";
    assert_eq!(
        Preprocessor::detect_include_guard(content),
        Some("MYGUARD".to_string())
    );
}

#[test]
fn test_detect_include_guard_no_guard() {
    // Not a guard - just regular code
    let content = b"int x = 1;\n";
    assert_eq!(Preprocessor::detect_include_guard(content), None);
}

#[test]
fn test_detect_include_guard_conditional_default_with_content() {
    // NOT a guard - conditional default definition followed by more content
    // This pattern: #ifndef X / #define X default / #endif / more macros
    let content = b"#ifndef FOO\n#define FOO 1\n#endif\n#define BAR 2\n";
    assert_eq!(Preprocessor::detect_include_guard(content), None);
}

#[test]
fn test_detect_include_guard_if_defined_conditional_default() {
    // NOT a guard - #if !defined() pattern with content after #endif
    let content = b"#if !defined(DEFAULT_VAL)\n#define DEFAULT_VAL 42\n#endif\n#define OTHER 1\n";
    assert_eq!(Preprocessor::detect_include_guard(content), None);
}

// ========================================================================
// bool/true/false predefined macro tests
// ========================================================================

#[test]
fn test_bool_macro_expands_to_bool_with_stdbool() {
    // bool should expand to _Bool after including stdbool.h
    let (tokens, idents) = preprocess_str("#include <stdbool.h>\nbool x;");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"_Bool".to_string()));
}

#[test]
fn test_true_macro_expands_to_1_with_stdbool() {
    // true should expand to 1 after including stdbool.h
    let (tokens, idents) = preprocess_str("#include <stdbool.h>\nint x = true;");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"1".to_string()));
}

#[test]
fn test_false_macro_expands_to_0_with_stdbool() {
    // false should expand to 0 after including stdbool.h
    let (tokens, idents) = preprocess_str("#include <stdbool.h>\nint x = false;");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"0".to_string()));
}

#[test]
fn test_bool_not_predefined_without_stdbool() {
    // bool should NOT be predefined without stdbool.h (matches GCC/Clang)
    let (tokens, idents) = preprocess_str("bool x;");
    let strs = get_token_strings(&tokens, &idents);
    // bool should remain unexpanded (as an identifier)
    assert!(strs.contains(&"bool".to_string()));
    assert!(!strs.contains(&"_Bool".to_string()));
}

// ========================================================================
// Blue-painting tests (C99 6.10.3.4 - recursive macro prevention)
// ========================================================================

#[test]
fn test_blue_painting_object_like_macro() {
    // A macro that expands to itself should not infinitely recurse
    // FOO expands to "FOO + 1", but the FOO in the expansion should not re-expand
    let (tokens, idents) = preprocess_str("#define FOO FOO + 1\nFOO");
    let strs = get_token_strings(&tokens, &idents);
    // Should get "FOO + 1", not infinite recursion
    assert!(strs.contains(&"FOO".to_string()));
    assert!(strs.contains(&"+".to_string()));
    assert!(strs.contains(&"1".to_string()));
}

#[test]
fn test_blue_painting_function_like_macro() {
    // Function-like macro that references itself
    let (tokens, idents) = preprocess_str("#define F(x) F(x + 1)\nF(0)");
    let strs = get_token_strings(&tokens, &idents);
    // Should get "F(0 + 1)", the inner F should not re-expand
    assert!(strs.contains(&"F".to_string()));
    assert!(strs.contains(&"0".to_string()));
    assert!(strs.contains(&"+".to_string()));
    assert!(strs.contains(&"1".to_string()));
}

#[test]
fn test_blue_painting_indirect_recursion() {
    // A -> B -> A should not infinitely recurse
    let (tokens, idents) = preprocess_str("#define A B\n#define B A\nA");
    let strs = get_token_strings(&tokens, &idents);
    // A -> B -> A (blue painted, stops)
    // Result should contain "A"
    assert!(strs.contains(&"A".to_string()));
}

// ========================================================================
// Predefined macro tokenization tests
// ========================================================================

#[test]
fn test_predefined_macro_tokenization_parentheses() {
    // Predefined macros like __DBL_MIN_EXP__ have values like "(-1021)"
    // These should be tokenized as separate tokens: (, -, 1021, )
    let mac = Macro::predefined("__TEST__", Some("(-1021)"));
    assert_eq!(mac.body.len(), 4); // (, -, 1021, )
    assert_eq!(mac.body[0].typ, TokenType::Special); // (
    assert_eq!(mac.body[1].typ, TokenType::Special); // -
    assert_eq!(mac.body[2].typ, TokenType::Number); // 1021
    assert_eq!(mac.body[3].typ, TokenType::Special); // )
}

#[test]
fn test_predefined_macro_tokenization_simple_number() {
    // Simple numeric value should be a single token
    let mac = Macro::predefined("__TEST__", Some("42"));
    assert_eq!(mac.body.len(), 1);
    assert_eq!(mac.body[0].typ, TokenType::Number);
}

#[test]
fn test_predefined_macro_tokenization_float() {
    // Float with exponent
    let mac = Macro::predefined("__TEST__", Some("1.0e-37"));
    assert_eq!(mac.body.len(), 1);
    assert_eq!(mac.body[0].typ, TokenType::Number);
}

#[test]
fn test_predefined_macro_expansion_with_parens() {
    // When a predefined macro with parenthesized value is used, it should expand correctly
    let code = "#define __TEST__ (-1021)\nint x = __TEST__;";
    let (tokens, idents) = preprocess_str(code);
    let strs = get_token_strings(&tokens, &idents);
    // Should contain the individual tokens
    assert!(strs.contains(&"(".to_string()));
    assert!(strs.contains(&"-".to_string()));
    assert!(strs.contains(&"1021".to_string()));
    assert!(strs.contains(&")".to_string()));
}

// ========================================================================
// Wide string/char in macro expansion tests
// ========================================================================

#[test]
fn test_wide_string_in_macro() {
    let (tokens, _) = preprocess_str("#define WSTR L\"hello\"\nWSTR");
    // Find the wide string token
    let wide_string_count = tokens
        .iter()
        .filter(|t| t.typ == TokenType::WideString)
        .count();
    assert_eq!(wide_string_count, 1, "should have one wide string token");
}

#[test]
fn test_wide_char_in_macro() {
    let (tokens, _) = preprocess_str("#define WCHAR L'x'\nWCHAR");
    // Find the wide char token
    let wide_char_count = tokens
        .iter()
        .filter(|t| t.typ == TokenType::WideChar)
        .count();
    assert_eq!(wide_char_count, 1, "should have one wide char token");
}

// ========================================================================
// Stringify, paste, include, and #if edge-case tests
// ========================================================================

/// 6.10.3.1p2 makes `__VA_ARGS__` the whole variadic token sequence,
/// commas included. Substitution took its first element alone, so
/// `V(1,2,3)` stringified to `"1"` -- silently, and the idiom is common
/// in logging macros.
#[test]
fn test_stringify_all_va_args() {
    for (code, want) in [
        ("#define V(...) #__VA_ARGS__\nV(1,2,3)", "\"1,2,3\""),
        ("#define V(...) #__VA_ARGS__\nV(1)", "\"1\""),
        ("#define W(a, ...) #__VA_ARGS__\nW(k, 1,2)", "\"1,2\""),
        // A comma inside parentheses is not a separator.
        ("#define V(...) #__VA_ARGS__\nV(f(1,2),3)", "\"f(1,2),3\""),
        // Spacing that the source has is kept; spacing it lacks is not
        // invented, which is what made `V(1,2,3)` come out `"1 , 2 , 3"`
        // once every argument survived.
        ("#define V(...) #__VA_ARGS__\nV(a, b)", "\"a, b\""),
    ] {
        let (tokens, idents) = preprocess_str(code);
        let strs = get_token_strings(&tokens, &idents);
        assert!(
            strs.contains(&want.to_string()),
            "expected {want:?} from {code:?}, got: {strs:?}"
        );
    }
}

/// 6.4.6p3: a digraph behaves as its primary token "except for their
/// spelling", and 6.10.3.2p2 asks `#` for that spelling. The punctuator
/// renderer used the primary token's, so `S(<:1:>)` came out `"[1]"`.
#[test]
fn test_stringify_keeps_digraph_spelling() {
    for (code, want) in [
        ("#define S(x) #x\nS(<:1:>)", "\"<:1:>\""),
        ("#define S(x) #x\nS(%:%:)", "\"%:%:\""),
        ("#define S(x) #x\nS(<% %>)", "\"<% %>\""),
        // The primary tokens still spell themselves.
        ("#define S(x) #x\nS([1])", "\"[1]\""),
        // And a multi-character punctuator is not dropped.
        ("#define S(x) #x\nS(a >> b)", "\"a >> b\""),
    ] {
        let (tokens, idents) = preprocess_str(code);
        let strs = get_token_strings(&tokens, &idents);
        assert!(
            strs.contains(&want.to_string()),
            "expected {want:?} from {code:?}, got: {strs:?}"
        );
    }
}

#[test]
fn test_stringify_empty_va_args() {
    // #__VA_ARGS__ with zero variadic args should produce an empty string ""
    let code = "#define S(...) #__VA_ARGS__\nS()";
    let (tokens, idents) = preprocess_str(code);
    let strs = get_token_strings(&tokens, &idents);
    assert!(
        strs.contains(&"\"\"".to_string()),
        "expected empty string \"\\\"\\\"\", got: {:?}",
        strs
    );
}

#[test]
fn test_include_macro_expanded_filename() {
    // The preprocessor should macro-expand the argument to #include.
    // Use angle-bracket include via a macro-expanded name.
    // stdbool.h is a builtin header that defines bool as _Bool.
    let code = "#include <stdbool.h>\n#define MYBOOL bool\nMYBOOL x;";
    let (tokens, idents) = preprocess_str(code);
    let strs = get_token_strings(&tokens, &idents);
    // After #include <stdbool.h>, bool is defined as _Bool.
    // The macro MYBOOL expands to bool, which then expands to _Bool.
    assert!(
        strs.contains(&"_Bool".to_string()),
        "expected _Bool from macro chain through stdbool.h, got: {:?}",
        strs
    );
}

#[test]
fn test_if_multichar_constant() {
    // Multi-character constants pack big-endian: 'ab' == ('a'<<8)+'b'
    let code = "#if 'ab' == (('a'<<8)+'b')\nyes\n#else\nno\n#endif";
    let (tokens, idents) = preprocess_str(code);
    let strs = get_token_strings(&tokens, &idents);
    assert!(
        strs.contains(&"yes".to_string()),
        "expected 'yes' for multi-char constant packing, got: {:?}",
        strs
    );
}

#[test]
fn test_paste_empty_arg() {
    // When the first argument is empty, a##b should produce just "hello".
    let code = "#define P(a,b) a##b\nP(,hello)";
    let (tokens, idents) = preprocess_str(code);
    let strs = get_token_strings(&tokens, &idents);
    assert!(
        strs.contains(&"hello".to_string()),
        "expected 'hello' from paste with empty arg, got: {:?}",
        strs
    );
}

#[test]
fn test_paste_start_of_body() {
    // ## at the start of a macro body is a constraint violation per
    // C99 6.10.3.3p1, but our preprocessor should handle it without
    // panicking. Just verify it completes.
    let code = "#define BAD(x) ##x\nBAD(hello)";
    let (_tokens, _idents) = preprocess_str(code);
    // If we get here without panicking, the test passes.
}

#[test]
fn test_line_directive_sets_line() {
    // #line 100 should make __LINE__ report 100
    let (tokens, _idents) = preprocess_str("#line 100\n__LINE__");
    let nums: Vec<_> = tokens
        .iter()
        .filter_map(|t| {
            if let TokenValue::Number(n) = &t.value {
                Some(n.clone())
            } else {
                None
            }
        })
        .collect();
    assert!(
        nums.contains(&"100".to_string()),
        "Expected __LINE__ to be 100, got {:?}",
        nums
    );
}

#[test]
fn test_line_directive_sets_file() {
    // #line 200 "fake.c" should make __FILE__ report "fake.c"
    let (tokens, _idents) = preprocess_str("#line 200 \"fake.c\"\n__FILE__");
    let strs: Vec<_> = tokens
        .iter()
        .filter_map(|t| {
            if let TokenValue::String(s) = &t.value {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();
    assert!(
        strs.contains(&"fake.c".to_string()),
        "Expected __FILE__ to be 'fake.c', got {:?}",
        strs
    );
}

#[test]
fn test_line_directive_skipped_in_false_branch() {
    // #line inside #if 0 should have no effect
    let (tokens, _idents) = preprocess_str("#if 0\n#line 999 \"wrong.c\"\n#endif\n__LINE__");
    let nums: Vec<_> = tokens
        .iter()
        .filter_map(|t| {
            if let TokenValue::Number(n) = &t.value {
                Some(n.clone())
            } else {
                None
            }
        })
        .collect();
    // __LINE__ should NOT be 999
    assert!(
        !nums.contains(&"999".to_string()),
        "__LINE__ should not be 999 in false branch"
    );
}

// ========================================================================
// C99 compliance gap tests
// ========================================================================

#[test]
fn test_line_directive_macro_expansion() {
    // #line should macro-expand its tokens before parsing
    let code = "#define LINENUM 100\n#line LINENUM\n__LINE__";
    let (tokens, _idents) = preprocess_str(code);
    let nums: Vec<_> = tokens
        .iter()
        .filter_map(|t| {
            if let TokenValue::Number(n) = &t.value {
                Some(n.clone())
            } else {
                None
            }
        })
        .collect();
    assert!(
        nums.contains(&"100".to_string()),
        "Expected __LINE__ to be 100 after #line with macro, got {:?}",
        nums
    );
}

#[test]
fn test_pragma_stdc_fp_contract() {
    // #pragma STDC FP_CONTRACT ON should be recognized without error
    let (tokens, idents) = preprocess_str("#pragma STDC FP_CONTRACT ON\ncode");
    let strs = get_token_strings(&tokens, &idents);
    assert!(
        strs.contains(&"code".to_string()),
        "code after #pragma STDC should pass through, got: {:?}",
        strs
    );
}

#[test]
fn test_pragma_stdc_fenv_access() {
    let (tokens, idents) = preprocess_str("#pragma STDC FENV_ACCESS OFF\ncode");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"code".to_string()));
}

#[test]
fn test_pragma_stdc_cx_limited_range() {
    let (tokens, idents) = preprocess_str("#pragma STDC CX_LIMITED_RANGE DEFAULT\ncode");
    let strs = get_token_strings(&tokens, &idents);
    assert!(strs.contains(&"code".to_string()));
}

#[test]
fn test_stringify_string_literal() {
    // #x with x being "hello" should produce token with content \"hello\"
    // (C99 6.10.3.2p2: \ before each " and \ including string delimiters)
    let code = "#define S(x) #x\nS(\"hello\")";
    let (tokens, _idents) = preprocess_str(code);
    let strings: Vec<_> = tokens
        .iter()
        .filter_map(|t| {
            if let TokenValue::String(s) = &t.value {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();
    // Token content should be: \"hello\" (escaped delimiters)
    assert!(
        strings.iter().any(|s| s == "\\\"hello\\\""),
        "expected stringified string with escaped delimiters, got: {:?}",
        strings
    );
}

#[test]
fn test_stringify_char_literal() {
    // #x with x being 'a' should produce "'a'"
    let code = "#define S(x) #x\nS('a')";
    let (tokens, _idents) = preprocess_str(code);
    let strings: Vec<_> = tokens
        .iter()
        .filter_map(|t| {
            if let TokenValue::String(s) = &t.value {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();
    assert!(
        strings.iter().any(|s| s.contains("'a'")),
        "expected stringified char literal, got: {:?}",
        strings
    );
}

#[test]
fn test_pragma_operator_destringify() {
    // _Pragma with escaped content should not crash
    let code = "_Pragma(\"GCC diagnostic ignored \\\"warn\\\"\") int x;";
    let (tokens, idents) = preprocess_str(code);
    let strs = get_token_strings(&tokens, &idents);
    assert!(
        strs.contains(&"x".to_string()),
        "code after _Pragma should pass through, got: {:?}",
        strs
    );
}
