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
    preprocess_str_for(input, &Target::host())
}

fn preprocess_str_for(input: &str, target: &Target) -> (Vec<Token>, IdentTable) {
    let mut strings = IdentTable::new();
    let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    let (result, _) = preprocess_collecting(
        tokens,
        target,
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

/// The version macros spell `GNUC_VERSION`, which the driver's
/// `-dumpversion` and `-dumpfullversion` print.
#[test]
fn test_predefined_gnuc_version_is_gnuc_version() {
    let (tokens, idents) =
        preprocess_str("__GNUC__ __GNUC_MINOR__ __GNUC_PATCHLEVEL__ __VERSION__");
    let strs = get_token_strings(&tokens, &idents);
    assert_eq!(strs[..3], GNUC_VERSION);
    let version = format!(
        "\"c17 {} (gcc compatible {})\"",
        env!("CARGO_PKG_VERSION"),
        GNUC_VERSION.join(".")
    );
    assert_eq!(strs[3], version);
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

// Header guard tests

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

// Multiple elif chain tests

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

// Defined operator tests

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

// Macro expansion tests

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

// Arithmetic in #if expressions

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

// Comparison operators in #if

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

// Bitwise operators in #if

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

// Edge cases

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

// Tests for __INCLUDE_LEVEL__ macro

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

// Tests for __BASE_FILE__ macro

#[test]
fn test_base_file_macro() {
    // __BASE_FILE__ should return the base filename
    let (tokens, _idents) = preprocess_str("__BASE_FILE__");
    // Should have a string token
    assert!(tokens.iter().any(|t| t.typ == TokenType::String));
}

/// `-fmacro-prefix-map` rewrites `__FILE__`, `__BASE_FILE__`, and a name a
/// `#line` gave, by the last matching map; the payload is the mapped name's
/// bytes, one `char` each, for both macros.
#[test]
fn test_file_macros_follow_the_macro_prefix_map() {
    let mut map = crate::prefix_map::PrefixMap::default();
    map.push("/src", "/OLD");
    map.push("/src/d\u{e9}", "/N\u{e9}");
    let config = PreprocessConfig {
        macro_prefix_map: map,
        ..Default::default()
    };
    let input = "__FILE__ __BASE_FILE__\n#line 9 \"/src/x.c\"\n__FILE__ \"/src/y.c\"\n";
    let mut idents = IdentTable::new();
    let tokens = Tokenizer::new(input.as_bytes(), 0, &mut idents).tokenize();
    let (out, _) = preprocess_collecting(
        tokens,
        &Target::host(),
        &mut idents,
        "/src/d\u{e9}/t.c",
        &config,
    );
    let payloads: Vec<String> = out
        .iter()
        .filter_map(|t| match &t.value {
            TokenValue::String(s) => Some(payload_text(s)),
            _ => None,
        })
        .collect();
    // An ordinary string literal is not a file name and is left alone.
    assert_eq!(
        payloads,
        ["/N\u{e9}/t.c", "/N\u{e9}/t.c", "/OLD/x.c", "/src/y.c"]
    );
}

// Tests for ternary operator in #if expressions

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

// Tests for GNU ,##__VA_ARGS__ comma suppression

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

// _Pragma operator tests (C99)

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

// Include guard detection tests

/// Tokenize a header and ask whether it is exactly one guarded group.
///
/// The scan reads tokens rather than raw bytes, so comments and whitespace are
/// already gone by the time it looks.
fn guard_of(content: &str) -> Option<String> {
    let mut strings = IdentTable::new();
    let tokens = Tokenizer::new(content.as_bytes(), 0, &mut strings).tokenize();
    Preprocessor::guard_of(&tokens, &strings)
}

#[test]
fn test_guard_ifndef_define() {
    // Standard include guard pattern: #ifndef X / #define X
    assert_eq!(
        guard_of("#ifndef FOO_H\n#define FOO_H\nint content;\n#endif\n"),
        Some("FOO_H".to_string())
    );
}

#[test]
fn test_guard_with_leading_comment() {
    assert_eq!(
        guard_of("/* Header file */\n#ifndef MY_HEADER_H\n#define MY_HEADER_H\n#endif\n"),
        Some("MY_HEADER_H".to_string())
    );
    assert_eq!(
        guard_of("// Header file\n#ifndef GUARD_H\n#define GUARD_H\n#endif\n"),
        Some("GUARD_H".to_string())
    );
}

#[test]
fn test_guard_no_define() {
    // Not a guard -- #ifndef without a matching #define. This is the
    // <sys/cdefs.h> sentinel shape, which must keep being re-read.
    assert_eq!(
        guard_of("#ifndef FOO_H\n#error \"Use other header\"\n#endif\n"),
        None
    );
}

#[test]
fn test_guard_different_macro() {
    // Not a guard -- the #define names something else.
    assert_eq!(guard_of("#ifndef FOO_H\n#define BAR_H\n#endif\n"), None);
}

#[test]
fn test_guard_if_not_defined() {
    // The `#if !defined(X)` spelling, with and without parentheses.
    assert_eq!(
        guard_of("#if !defined(MYGUARD)\n#define MYGUARD\n#endif\n"),
        Some("MYGUARD".to_string())
    );
    assert_eq!(
        guard_of("#if !defined MYGUARD\n#define MYGUARD\n#endif\n"),
        Some("MYGUARD".to_string())
    );
}

#[test]
fn test_guard_no_guard() {
    assert_eq!(guard_of("int x = 1;\n"), None);
    assert_eq!(guard_of(""), None);
}

#[test]
fn test_guard_conditional_default_with_content() {
    // NOT a guard: the conditional-default idiom, where something follows the
    // #endif. Treating it as one would delete that something.
    assert_eq!(
        guard_of("#ifndef FOO\n#define FOO 1\n#endif\n#define BAR 2\n"),
        None
    );
    assert_eq!(
        guard_of("#if !defined(DEFAULT_VAL)\n#define DEFAULT_VAL 42\n#endif\n#define OTHER 1\n"),
        None
    );
}

/// A header with code after its `#endif` is not one guarded group: skipping it
/// on re-inclusion would delete that code.
#[test]
fn test_guard_rejects_code_after_endif() {
    assert_eq!(
        guard_of("#ifndef G\n#define G\nint body;\n#endif\nint outside;\n"),
        None
    );
}

/// Nesting has to be counted, or an inner `#endif` looks like the guard's.
#[test]
fn test_guard_counts_nesting() {
    assert_eq!(
        guard_of("#ifndef G\n#define G\n#if 1\nint a;\n#endif\nint b;\n#endif\n"),
        Some("G".to_string())
    );
    // Same shape, but with something after the *outer* #endif.
    assert_eq!(
        guard_of("#ifndef G\n#define G\n#if 1\nint a;\n#endif\n#endif\nint after;\n"),
        None
    );
}

/// A guard whose group never closes is not a guard.
#[test]
fn test_guard_rejects_unterminated() {
    assert_eq!(guard_of("#ifndef G\n#define G\nint body;\n"), None);
}

// bool/true/false predefined macro tests

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

// Blue-painting tests (C99 6.10.3.4 - recursive macro prevention)

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

// Predefined macro tokenization tests

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

// Wide string/char in macro expansion tests

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

// Stringify, paste, include, and #if edge-case tests

/// 6.10.3.1p2 makes `__VA_ARGS__` the whole variadic token sequence, commas
/// included, and `#__VA_ARGS__` stringifies all of it.
#[test]
fn test_stringify_all_va_args() {
    for (code, want) in [
        ("#define V(...) #__VA_ARGS__\nV(1,2,3)", "\"1,2,3\""),
        ("#define V(...) #__VA_ARGS__\nV(1)", "\"1\""),
        ("#define W(a, ...) #__VA_ARGS__\nW(k, 1,2)", "\"1,2\""),
        // A comma inside parentheses is not a separator.
        ("#define V(...) #__VA_ARGS__\nV(f(1,2),3)", "\"f(1,2),3\""),
        // Spacing that the source has is kept; spacing it lacks is not
        // invented.
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
/// spelling", and 6.10.3.2p2 asks `#` for that spelling.
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

/// A character constant in `#if` evaluates to its escape's value, not to the
/// *source spelling* between the quotes: `'\n'` is 10, and `#if '\0'` is false.
#[test]
fn test_if_character_escapes() {
    for (expr, want_yes) in [
        ("'\\n' == 10", true),
        ("'\\0' == 0", true),
        ("'\\0'", false),
        ("'\\t' == 9", true),
        ("'\\x41' == 65", true),
        ("'\\101' == 65", true),
        ("'\\\\' == 92", true),
        ("'\\'' == 39", true),
        ("'A' == 65", true),
    ] {
        let code = format!(
            "#if {}
yes
#else
no
#endif",
            expr
        );
        let (tokens, idents) = preprocess_str(&code);
        let strs = get_token_strings(&tokens, &idents);
        let want = if want_yes { "yes" } else { "no" };
        assert!(
            strs.contains(&want.to_string()),
            "#if {} should take the {} branch, got: {:?}",
            expr,
            want,
            strs
        );
    }
}

/// C17 6.4.4.4p10: an ordinary character constant has plain `char`'s
/// signedness, so the answer differs by target and `#if` must agree with what
/// the compiled program would compute.
///
/// `__CHAR_UNSIGNED__` must agree with it, since `<limits.h>` reads that
/// macro alone -- per OS as well as per architecture, because Apple arm64
/// makes plain `char` signed where AAPCS64 makes it unsigned.
#[test]
fn test_if_char_signedness_follows_target() {
    use crate::target::{Arch, CharSignedness, Os};
    let code = "#if '\\xff' < 0
signed
#else
unsigned
#endif
#ifdef __CHAR_UNSIGNED__
macro_unsigned
#else
macro_signed
#endif";
    for (arch, os) in [
        (Arch::X86_64, Os::Linux),
        (Arch::X86_64, Os::MacOS),
        (Arch::Aarch64, Os::Linux),
        (Arch::Aarch64, Os::MacOS),
    ] {
        let target = Target::new(arch, os);
        let (tokens, idents) = preprocess_str_for(code, &target);
        let strs = get_token_strings(&tokens, &idents);
        let want = match target.plain_char {
            CharSignedness::Signed => ["signed", "macro_signed"],
            CharSignedness::Unsigned => ["unsigned", "macro_unsigned"],
        };
        assert_eq!(strs, want, "{arch}-{os}");
    }
}

/// A prefixed constant holds one wide character, not the bytes of an
/// encoding: `L'\n'` is 10, never a packed pair.
#[test]
fn test_if_prefixed_character_constants() {
    for expr in ["L'\\n' == 10", "u'\\x41' == 65", "U'A' == 65", "L'A' == 65"] {
        let code = format!(
            "#if {}
yes
#else
no
#endif",
            expr
        );
        let (tokens, idents) = preprocess_str(&code);
        let strs = get_token_strings(&tokens, &idents);
        assert!(
            strs.contains(&"yes".to_string()),
            "#if {} should be true, got: {:?}",
            expr,
            strs
        );
    }
}

/// A prefixed constant has its type's signedness in `#if` (C17 6.10.1p4):
/// `char16_t` and `char32_t` are unsigned everywhere and `wchar_t` is
/// unsigned on aarch64 Linux, so `X'\0' - 1 > 0` there -- the test glibc's
/// <bits/wchar.h> uses to find WCHAR_MIN. Answers match gcc's on both Linux
/// targets. The macros say the same as the constants.
#[test]
fn test_if_prefixed_constants_have_their_types_signedness() {
    use crate::target::{Arch, Os};
    let code = "#if L'\\0' - 1 > 0
w_unsigned
#endif
#if u'\\0' - 1 > 0
u_unsigned
#endif
#if U'\\0' - 1 > 0
U_unsigned
#endif
#if __WCHAR_MIN__ == 0 && __WCHAR_MAX__ == 0xffffffffU
wmacro_unsigned
#endif
#if __WCHAR_MIN__ < 0 && __WCHAR_MAX__ == 0x7fffffff
wmacro_signed
#endif";
    for (arch, os, wchar_unsigned) in [
        (Arch::X86_64, Os::Linux, false),
        (Arch::X86_64, Os::MacOS, false),
        (Arch::Aarch64, Os::Linux, true),
        (Arch::Aarch64, Os::FreeBSD, true),
        (Arch::Aarch64, Os::MacOS, false),
    ] {
        let target = Target::new(arch, os);
        let (tokens, idents) = preprocess_str_for(code, &target);
        let strs = get_token_strings(&tokens, &idents);
        let want: &[&str] = if wchar_unsigned {
            &["w_unsigned", "u_unsigned", "U_unsigned", "wmacro_unsigned"]
        } else {
            &["u_unsigned", "U_unsigned", "wmacro_signed"]
        };
        assert_eq!(strs, want, "{arch}-{os}");
    }
}

/// An escape in a prefixed constant keeps the width of its type in `#if`
/// too, and a `wchar_t` one its signedness: `L'\xffffffff'` is -1 where
/// `wchar_t` is `int` and 4294967295 where it is `unsigned int`, as gcc says
/// on both Linux targets. Escapes were cut to a byte, so `L'\x1234'` was 0x34.
#[test]
fn test_if_prefixed_escapes_keep_their_width() {
    use crate::target::{Arch, Os};
    let code = "#if L'\\x1234' == 0x1234 && u'\\x1234' == 0x1234 && U'\\x10000' == 0x10000
wide
#endif
#if L'\\xffffffff' < 0
negative
#endif
#if L'\\xffffffff' == 4294967295
all_ones
#endif";
    for (arch, os, want) in [
        (Arch::X86_64, Os::Linux, ["wide", "negative"]),
        (Arch::Aarch64, Os::Linux, ["wide", "all_ones"]),
        (Arch::Aarch64, Os::MacOS, ["wide", "negative"]),
    ] {
        let target = Target::new(arch, os);
        let (tokens, idents) = preprocess_str_for(code, &target);
        assert_eq!(get_token_strings(&tokens, &idents), want, "{arch}-{os}");
    }
}

/// The predefined `__INTN_C(c)` macros paste their suffix onto the argument,
/// as gcc's do, and the empty-suffix ones hand it back untouched.
#[test]
fn test_predefined_constant_fn_macros_paste() {
    use crate::target::{Arch, Os};
    let code = "__INT64_C(5) __UINT32_C(7) __INT8_C(9) __UINTMAX_C(0x10)";
    for (os, int64) in [(Os::Linux, "5L"), (Os::MacOS, "5LL")] {
        let target = Target::new(Arch::Aarch64, os);
        let (tokens, idents) = preprocess_str_for(code, &target);
        let strs = get_token_strings(&tokens, &idents);
        assert_eq!(strs, [int64, "7U", "9", "0x10UL"], "{os}");
    }
}

/// gcc packs a multi-character constant big-endian into `int` and lets it
/// wrap, so a five-byte constant keeps only its last four bytes.
#[test]
fn test_if_multichar_constant_wraps_at_int() {
    for expr in ["'abcd' == 0x61626364", "'abcde' == 0x62636465"] {
        let code = format!(
            "#if {}
yes
#else
no
#endif",
            expr
        );
        let (tokens, idents) = preprocess_str(&code);
        let strs = get_token_strings(&tokens, &idents);
        assert!(
            strs.contains(&"yes".to_string()),
            "#if {} should be true, got: {:?}",
            expr,
            strs
        );
    }
}

/// A `defined` that comes *out* of an expansion, with an operand that is
/// itself a macro. The operand must not be expanded on the way through, or
/// the evaluator sees `defined(1)`.
#[test]
fn test_if_defined_from_macro_expansion() {
    let code =
        "#define FOO 1\n#define D defined(FOO)\n#if D\nyes\n#else\nno\n#endif".replace("\\n", "\n");
    let (tokens, idents) = preprocess_str(&code);
    let strs = get_token_strings(&tokens, &idents);
    assert!(
        strs.contains(&"yes".to_string()),
        "expected 'yes' for defined() produced by expansion, got: {:?}",
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

/// A `#line` or linemarker name is a string literal, so its escapes are
/// interpreted; `__FILE__` spells the name back with `"` and `\` escaped,
/// as `__BASE_FILE__` does the main file's.
#[test]
fn test_file_macros_escape_quote_and_backslash() {
    let input = "__BASE_FILE__\n#line 7 \"a\\\\b\\\"c\\x41\"\n__FILE__\n# 9 \"x\\\\y\"\n__FILE__\n";
    let mut idents = IdentTable::new();
    let tokens = Tokenizer::new(input.as_bytes(), 0, &mut idents).tokenize();
    let (out, _) = preprocess_collecting(
        tokens,
        &Target::new(crate::target::Arch::X86_64, crate::target::Os::Linux),
        &mut idents,
        "q\"d\\e/f.c",
        &PreprocessConfig::default(),
    );
    let spelled: Vec<String> = out
        .iter()
        .filter_map(|t| match &t.value {
            TokenValue::String(s) => Some(payload_text(s)),
            _ => None,
        })
        .collect();
    assert_eq!(
        spelled,
        ["q\\\"d\\\\e/f.c", "a\\\\b\\\"cA", "x\\\\y"],
        "file-name macros must spell their names escaped"
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

// C99 compliance gap tests

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

// The search chain: `-I`, then the bundled headers, then the system
// directories, with `#include_next` resuming just past the current file.

/// A temporary tree of headers: `iq` stands for a `-iquote` directory, `q`
/// for a `-I` one, `sys` for a system one, and each file is written as given.
struct SearchTree {
    dir: plib::tmp::TempDir,
}

impl SearchTree {
    fn new(files: &[(&str, &str)]) -> Self {
        let dir = plib::tmp::Builder::new()
            .prefix("c17_search_chain_")
            .tempdir()
            .unwrap();
        for sub in ["iq", "q", "sys"] {
            std::fs::create_dir(dir.path().join(sub)).unwrap();
        }
        for (path, text) in files {
            std::fs::write(dir.path().join(path), text).unwrap();
        }
        SearchTree { dir }
    }

    fn path(&self, sub: &str) -> String {
        self.dir.path().join(sub).to_string_lossy().into_owned()
    }

    /// Preprocess `input` with `iq` as the only `-iquote` directory, `q` as the
    /// only `-I` directory and `sys` as the only system directory, returning
    /// the token spellings and the headers depended on.
    fn preprocess(&self, input: &str) -> (Vec<String>, Vec<(PathBuf, bool)>) {
        let iquote = [self.path("iq")];
        let include_paths = [self.path("q")];
        let isystem = [self.path("sys")];
        let config = PreprocessConfig {
            include_paths: &include_paths,
            search: SystemSearch {
                iquote: &iquote,
                isystem: &isystem,
                no_std_inc: true,
                ..Default::default()
            },
            collect_dependencies: true,
            ..Default::default()
        };
        let mut idents = IdentTable::new();
        let tokens = Tokenizer::new(input.as_bytes(), 0, &mut idents).tokenize();
        let (out, outcome) =
            preprocess_collecting(tokens, &Target::host(), &mut idents, "<test>", &config);
        (get_token_strings(&out, &idents), outcome.dependencies)
    }
}

#[test]
fn test_search_pos_order_is_the_search_order() {
    assert!(SearchPos::IQuote(7) < SearchPos::Quote(0));
    assert!(SearchPos::Quote(7) < SearchPos::Bundled);
    assert!(SearchPos::Bundled < SearchPos::System(0));
    assert!(SearchPos::System(0) < SearchPos::System(1));
    assert_eq!(SearchPos::after(None), SearchPos::IQuote(0));
    assert_eq!(
        SearchPos::after(Some(SearchPos::IQuote(0))),
        SearchPos::IQuote(1)
    );
    assert_eq!(
        SearchPos::after(Some(SearchPos::Quote(2))),
        SearchPos::Quote(3)
    );
    assert_eq!(
        SearchPos::after(Some(SearchPos::Bundled)),
        SearchPos::System(0)
    );
    assert_eq!(
        SearchPos::after(Some(SearchPos::System(4))),
        SearchPos::System(5)
    );
}

/// `-iquote` directories are searched for the `"..."` form only, after the
/// including file's own directory and ahead of `-I`; `#include_next` from a
/// header found there goes on to the rest of the chain.
#[test]
fn test_iquote_serves_quote_includes_only() {
    let tree = SearchTree::new(&[
        ("iq/h.h", "#define WHERE iquote\n"),
        ("q/h.h", "#define WHERE dash_i\n"),
        ("iq/n.h", "#include_next \"n.h\"\nFIRST\n"),
        ("q/n.h", "SECOND\n"),
    ]);
    let (strs, deps) = tree.preprocess("#include \"h.h\"\nWHERE");
    assert_eq!(strs, ["iquote"]);
    assert_eq!(deps, [(Path::new(&tree.path("iq")).join("h.h"), false)]);
    let (strs, _) = tree.preprocess("#include <h.h>\nWHERE");
    assert_eq!(strs, ["dash_i"]);
    let (strs, _) = tree.preprocess("#include \"n.h\"\n");
    assert_eq!(strs, ["SECOND", "FIRST"]);
}

/// The bundled <limits.h> forwards to the system's, which, like glibc's, would
/// forward back to the compiler's under `__GNUC__` unless `_GCC_LIMITS_H_` is
/// defined, and fills in `LLONG_MIN` its own way when it is missing; like
/// Apple's, it spells `INT_MAX` as a number. The system's limits arrive,
/// nothing recurses, and the compiler's sizes stand.
#[test]
fn test_bundled_limits_h_forwards_to_the_system_header() {
    let tree = SearchTree::new(&[(
        "sys/limits.h",
        "#ifndef SYS_LIMITS\n#define SYS_LIMITS 1\n#define MB_LEN_MAX 6\n\
         #define INT_MAX 2147483647\n#define LINE_MAX 2048\n#endif\n\
         #if defined __GNUC__ && !defined _GCC_LIMITS_H_\n#include_next <limits.h>\n#endif\n\
         #ifndef LLONG_MIN\n#define LLONG_MIN (-LLONG_MAX-1)\n#endif\n",
    )]);
    let (strs, _) =
        tree.preprocess("#include <limits.h>\nLINE_MAX MB_LEN_MAX INT_MAX LLONG_MIN SYS_LIMITS");
    assert_eq!(
        strs,
        [
            "2048",
            "6",
            "0x7fffffff",
            "(",
            "-",
            "0x7fffffffffffffffLL",
            "-",
            "1LL",
            ")",
            "1"
        ],
        "the system's limits must arrive and the compiler's sizes stand"
    );
}

/// With no system <limits.h> to forward to, the bundled one still stands on
/// its own.
#[test]
fn test_bundled_limits_h_without_a_system_header() {
    let tree = SearchTree::new(&[]);
    let (strs, _) = tree.preprocess("#include <limits.h>\nCHAR_BIT MB_LEN_MAX LINE_MAX");
    assert_eq!(strs, ["8", "16", "LINE_MAX"]);
}

/// A `-I` header that forwards, as gnulib's replacement <limits.h> does,
/// reaches the bundled one, and through it the system's.
#[test]
fn test_include_next_from_dash_i_reaches_the_bundled_header() {
    let tree = SearchTree::new(&[
        ("q/limits.h", "#include_next <limits.h>\n#define FROM_Q 1\n"),
        ("sys/limits.h", "#define LINE_MAX 2048\n"),
    ]);
    let (strs, _) = tree.preprocess("#include <limits.h>\nFROM_Q CHAR_BIT LINE_MAX");
    assert_eq!(strs, ["1", "8", "2048"]);
}

/// A system header's own `#include <limits.h>` starts the search over, so it
/// reaches the bundled header too, and that header's `#include_next` finds
/// the system one although the includer came from the last system directory.
#[test]
fn test_limits_h_included_from_a_system_header() {
    let tree = SearchTree::new(&[
        ("sys/wrap.h", "#include <limits.h>\n"),
        ("sys/limits.h", "#define LINE_MAX 2048\n"),
    ]);
    let (strs, _) = tree.preprocess("#include <wrap.h>\nCHAR_BIT LINE_MAX");
    assert_eq!(strs, ["8", "2048"]);
}

/// `__has_include_next` asks what `#include_next` would find, which is never
/// the current file itself.
#[test]
fn test_has_include_next_searches_past_the_current_file() {
    let probe = "#if __has_include_next(<a.h>)\nNEXT_YES\n#else\nNEXT_NO\n#endif\n";
    let tree = SearchTree::new(&[("q/a.h", probe)]);
    let (strs, _) = tree.preprocess("#include <a.h>\n");
    assert_eq!(strs, ["NEXT_NO"]);

    let tree = SearchTree::new(&[("q/a.h", probe), ("sys/a.h", "")]);
    let (strs, _) = tree.preprocess("#include <a.h>\n");
    assert_eq!(strs, ["NEXT_YES"]);
}

/// A header found in a system directory is a system header for diagnostics
/// too -- the warnings `-w` would hide are not shown, and `-Werror` and
/// `-pedantic-errors` do not reach it -- and so is one it includes from
/// beside itself, or a bundled one. A `-I` header is not, however spelled.
#[test]
fn test_headers_from_system_directories_are_system_streams() {
    let tree = SearchTree::new(&[
        ("q/mine.h", ""),
        ("sys/theirs.h", "#include \"beside.h\"\n"),
        ("sys/beside.h", ""),
    ]);
    crate::diag::clear_streams();
    tree.preprocess("#include <mine.h>\n#include \"theirs.h\"\n#include <stddef.h>\n");
    let system = |suffix: &str| {
        let name = crate::diag::get_all_stream_names()
            .into_iter()
            .find(|n| n.ends_with(suffix))
            .unwrap_or_else(|| panic!("no stream for {suffix}"));
        crate::diag::stream_is_system(crate::diag::find_or_add_stream(&name))
    };
    assert!(!system("q/mine.h"));
    assert!(system("sys/theirs.h"));
    assert!(system("sys/beside.h"));
    assert!(system("<builtin:stddef.h>"));
}

/// A header found through `-I` is the project's, which `-MM` lists; only one
/// found in a system directory is a system header, however it was spelled.
#[test]
fn test_dash_i_header_is_not_a_system_dependency() {
    let tree = SearchTree::new(&[("q/mine.h", ""), ("sys/theirs.h", "")]);
    let (_, deps) = tree.preprocess("#include <mine.h>\n#include \"theirs.h\"\n");
    let deps: Vec<_> = deps
        .iter()
        .map(|(p, sys)| (p.file_name().unwrap().to_string_lossy().into_owned(), *sys))
        .collect();
    assert_eq!(
        deps,
        [
            ("mine.h".to_string(), false),
            ("theirs.h".to_string(), true)
        ]
    );
}

/// `__has_builtin` answers for the `_Float128` constants only where the type
/// exists, in a directive and in running text alike; the `_Float16` ones
/// exist everywhere.
#[test]
fn test_has_builtin_float128_constants_follow_the_target() {
    use crate::target::{Arch, Os};
    let code = "#if __has_builtin(__builtin_nanf128)\nYES\n#else\nNO\n#endif\n\
                __has_builtin(__builtin_inff128) __has_builtin(__builtin_inff16)\n";
    for (os, want) in [
        (Os::Linux, ["YES", "1", "1"]),
        (Os::MacOS, ["NO", "0", "1"]),
    ] {
        let target = Target::new(Arch::Aarch64, os);
        let (tokens, idents) = preprocess_str_for(code, &target);
        assert_eq!(get_token_strings(&tokens, &idents), want, "{os:?}");
    }
}

/// Whatever a directive or `_Pragma` at the end of the file leaves unfinished,
/// the end of the stream survives preprocessing: the parser stops there, and
/// one that never saw it looped allocating without bound.
#[test]
fn test_stream_end_survives_unfinished_constructs() {
    for src in [
        "int x;\n#define X \\\n",
        "int x;\n#undef X \\",
        "int x;\n#error e \\\n",
        "int x;\n_Pragma",
        "int x;\n_Pragma(",
        "int x;\n_Pragma(\"once\"",
    ] {
        let (tokens, _) = preprocess_str(src);
        assert!(
            tokens.iter().any(|t| t.typ == TokenType::StreamEnd),
            "{src:?}: the end of the stream was consumed"
        );
    }
}

/// A malformed `_Pragma` operand is gcc's error and consumes nothing that is
/// not part of the operator.
#[test]
fn test_malformed_pragma_operator_keeps_following_tokens() {
    let before = crate::diag::error_count();
    let (tokens, idents) = preprocess_str("_Pragma(x) int y;");
    assert!(crate::diag::error_count() > before);
    assert_eq!(
        get_token_strings(&tokens, &idents),
        ["x", ")", "int", "y", ";"]
    );
}

/// `#__VA_ARGS__` keeps the spacing of the commas that separated the
/// variadic arguments (C17 6.10.3.1p2), and a `##` result keeps its left
/// operand's spacing.
#[test]
fn test_stringify_and_paste_spacing() {
    let (tokens, idents) = preprocess_str(
        "#define H(...) #__VA_ARGS__\n#define S(x) #x\n#define F(n) S(a[b##n])\nH(a , b) F(1)",
    );
    assert_eq!(
        get_token_strings(&tokens, &idents),
        ["\"a , b\"", "\"a[b1]\""]
    );
}

/// `#if` evaluates only the arm of `?:` it takes, and converts both arms to
/// their common type (C17 6.5.15p4-5).
#[test]
fn test_if_conditional_operator() {
    let (tokens, idents) = preprocess_str(
        "#if 1 ? 2 : (1/0)\nA\n#endif\n#if (1 ? -1 : 0u) > 0\nB\n#endif\n#if 0 ? -1 : 0u\nC\n#endif\n",
    );
    assert_eq!(get_token_strings(&tokens, &idents), ["A", "B"]);
}

/// `__PIC__`/`__pic__` and `__PIE__`/`__pie__` describe the position
/// independence the code is generated with.
#[test]
fn test_pic_macros_follow_the_configuration() {
    use crate::target::{PicLevel, PositionIndependence as P};
    let expand = |position: P, target: &Target| {
        let config = PreprocessConfig {
            position,
            isa: Default::default(),
            ..Default::default()
        };
        let mut idents = IdentTable::new();
        let tokens = Tokenizer::new(b"__PIC__ __pic__ __PIE__ __pie__", 0, &mut idents).tokenize();
        let (out, _) = preprocess_collecting(tokens, target, &mut idents, "<test>", &config);
        get_token_strings(&out, &idents)
    };
    let linux = Target::from_triple("x86_64-unknown-linux-gnu").unwrap();
    assert_eq!(
        expand(P::Pie(PicLevel::Large), &linux),
        ["2", "2", "2", "2"]
    );
    // `-fpie` and `-fpic` say 1, as gcc does.
    assert_eq!(
        expand(P::Pie(PicLevel::Small), &linux),
        ["1", "1", "1", "1"]
    );
    assert_eq!(
        expand(P::Pic(PicLevel::Small), &linux),
        ["1", "1", "__PIE__", "__pie__"]
    );
    assert_eq!(
        expand(P::Pic(PicLevel::Large), &linux),
        ["2", "2", "__PIE__", "__pie__"]
    );
    assert_eq!(
        expand(P::Absolute, &linux),
        ["__PIC__", "__pic__", "__PIE__", "__pie__"]
    );
    // Mach-O code is position independent whatever was asked.
    let darwin = Target::from_triple("aarch64-apple-darwin").unwrap();
    assert_eq!(
        expand(P::Absolute, &darwin),
        ["2", "2", "__PIE__", "__pie__"]
    );
}

/// `#line` maps the positions of the tokens after it -- what diagnostics and
/// debug info read -- the same way a `# N "file"` linemarker does, and the
/// mapping of a file survives an `#include` in it.
#[test]
fn test_line_directive_maps_token_positions() {
    let (tokens, idents) =
        preprocess_str("#line 77 \"renamed.c\"\nint b = __LINE__;\n#line 5\nint c = __LINE__;\n");
    let strings = get_token_strings(&tokens, &idents);
    assert_eq!(
        strings,
        ["int", "b", "=", "77", ";", "int", "c", "=", "5", ";"]
    );
    let b = tokens
        .iter()
        .find(|t| matches!(&t.value, TokenValue::Ident(id) if idents.get_opt(*id) == Some("b")))
        .expect("b");
    assert_eq!(b.pos.line, 77);
    assert_eq!(crate::diag::stream_name(b.pos.stream), "renamed.c");
}

/// `#pragma scalar_storage_order` reaches the parser as a layout marker, in
/// either spelling, and a body naming no order is dropped with a warning.
#[test]
fn test_storage_order_pragma_becomes_a_layout_marker() {
    let (mut tokens, _) = preprocess_str(
        "#pragma scalar_storage_order big-endian\n\
         int a;\n\
         _Pragma(\"scalar_storage_order little-endian\")\n\
         #pragma scalar_storage_order default\n\
         #pragma scalar_storage_order sideways\n\
         int b;\n",
    );
    let pragmas = extract_pragma_directives(&mut tokens);
    let orders: Vec<LayoutPragma> = pragmas.iter().map(|(_, p)| *p).collect();
    assert_eq!(
        orders,
        [
            LayoutPragma::StorageOrder(StorageOrderPragma::Order(ByteOrder::BigEndian)),
            LayoutPragma::StorageOrder(StorageOrderPragma::Order(ByteOrder::LittleEndian)),
            LayoutPragma::StorageOrder(StorageOrderPragma::Default),
        ]
    );
    // The first stands before `int a`, the others after it.
    assert!(pragmas[0].0 < pragmas[1].0);
    // And each one spells itself back for `-E`.
    assert_eq!(
        orders[0].to_pragma_text(),
        "#pragma scalar_storage_order big-endian"
    );
    assert_eq!(
        orders[2].to_pragma_text(),
        "#pragma scalar_storage_order default"
    );
}

/// Preprocess `src` and return every diagnostic it produced.
fn preprocess_diagnostics(src: &str) -> Vec<String> {
    crate::diag::capture_diagnostics();
    preprocess_str(src);
    crate::diag::take_captured_diagnostics()
}

/// Whether any diagnostic line contains `needle`.
fn mentions(lines: &[String], needle: &str) -> bool {
    lines.iter().any(|l| l.contains(needle))
}

/// C17 6.10p1: a `#` that starts a line in an active group begins a
/// directive, and one whose name is none of the directives is invalid. gcc
/// errors, naming the token that follows the `#`.
#[test]
fn test_invalid_directive_is_an_error() {
    for (src, spelled) in [
        ("#foo\nint x;\n", "#foo"),
        ("#foo bar baz\nint x;\n", "#foo"),
        ("#Define X 1\nint x;\n", "#Define"),
        ("#!foo\nint x;\n", "#!"),
        ("#+\nint x;\n", "#+"),
        ("#\"x\"\nint x;\n", "#\"x\""),
    ] {
        let lines = preprocess_diagnostics(src);
        let want = format!("error: invalid preprocessing directive {spelled}");
        assert!(
            lines.iter().any(|l| l.ends_with(&want)),
            "{src:?}: expected {want:?}, got {lines:?}"
        );
    }
}

/// A linemarker's line number must be a number: gcc's error names the token.
#[test]
fn test_linemarker_needs_a_positive_integer() {
    let lines = preprocess_diagnostics("# 12abc\nint x;\n");
    assert!(
        mentions(&lines, "error: \"12abc\" after # is not a positive integer"),
        "{lines:?}"
    );
}

/// The null directive and a linemarker are valid, and a skipped group is
/// only scanned for the directives that nest: gcc says nothing about any of
/// these.
#[test]
fn test_valid_and_skipped_directives_are_quiet() {
    let lines = preprocess_diagnostics(
        "#\n# 33 \"file.c\"\n#123\n#if 0\n#foo\n#!\n#+ bar\n# 12abc\n#endif\nint x;\n",
    );
    assert!(lines.is_empty(), "{lines:?}");
}

/// In assembly `#` also introduces a comment, so a line naming no directive
/// is prose. gcc passes it through without a word.
#[test]
fn test_assembly_leaves_unknown_directives_alone() {
    crate::diag::capture_diagnostics();
    let out = preprocess_asm_file(
        b"# save the frame pointer\n#! odd\n#foo bar\n\tnop\n",
        &Target::host(),
        "t.S",
        &AsmPreprocessConfig::default(),
    );
    let lines = crate::diag::take_captured_diagnostics();
    assert!(out.is_ok(), "{lines:?}");
    assert!(lines.is_empty(), "{lines:?}");
}

/// C17 6.10.1p4 evaluates `#if` in `intmax_t`, where signed overflow is
/// undefined. gcc's default pedwarn "integer overflow in preprocessor
/// expression" covers `+`, `-`, `*`, `/`, unary `-` and a signed left shift
/// that loses bits -- and nothing in the unsigned domain, `%`, a right shift,
/// or an operand that is not evaluated.
#[test]
fn test_if_signed_overflow_is_diagnosed_as_gcc_does() {
    const MAX: &str = "9223372036854775807";
    let cases: &[(String, bool)] = &[
        (format!("{MAX} + 1"), true),
        (format!("-{MAX} - 2"), true),
        (format!("{MAX} * 2"), true),
        (format!("-{MAX} * 2"), true),
        ("4294967296 * 4294967296".to_string(), true),
        (format!("(-{MAX}-1) * -1"), true),
        (format!("(-{MAX}-1) / -1"), true),
        (format!("-(-{MAX}-1)"), true),
        ("1 << 63".to_string(), true),
        ("3 << 62".to_string(), true),
        ("2 << 62".to_string(), true),
        ("1 << 64".to_string(), true),
        ("-1 << 64".to_string(), true),
        ("4 >> -62".to_string(), true),
        ("1 >> -64".to_string(), true),
        ("1 << 18446744073709551615u".to_string(), true),
        (format!("-{MAX} - 1"), false),
        (format!("{MAX} * -1"), false),
        (format!("(-{MAX}-1) % -1"), false),
        (format!("{MAX} + 1u"), false),
        ("18446744073709551615u + 1".to_string(), false),
        ("0u - 1".to_string(), false),
        ("-1 + 0u".to_string(), false),
        ("~0 + 1".to_string(), false),
        ("1 << 62".to_string(), false),
        ("-1 << 1".to_string(), false),
        ("-1 << 63".to_string(), false),
        ("0 << 64".to_string(), false),
        ("1u << 63".to_string(), false),
        ("1u << 64".to_string(), false),
        ("1 >> 64".to_string(), false),
        ("-1 >> 70".to_string(), false),
        ("1 << -1".to_string(), false),
        (format!("0 && {MAX} + 1"), false),
        (format!("1 || {MAX} * 2"), false),
        (format!("0 ? ({MAX} + 1) : 1"), false),
    ];
    let faults: Vec<String> = cases
        .iter()
        .filter_map(|(expr, overflows)| {
            let lines = preprocess_diagnostics(&format!("#if {expr}\n#endif\nint x;\n"));
            let warned = mentions(
                &lines,
                "warning: integer overflow in preprocessor expression",
            );
            let other = lines
                .iter()
                .any(|l| !l.contains("integer overflow in preprocessor expression"));
            (warned != *overflows || other).then(|| format!("#if {expr}: {lines:?}"))
        })
        .collect();
    assert!(faults.is_empty(), "{}", faults.join("\n"));
}

/// The value of an out-of-range shift is gcc's: a negative count shifts the
/// other way, and a count of 64 or more shifts every bit out.
#[test]
fn test_if_shift_values_match_gcc() {
    for cond in [
        "(1 << -1) == 0",
        "(4 >> -1) == 8",
        "(1u << 64) == 0",
        "(1 << 64) == 0",
        "(1 >> 64) == 0",
        "(-1 >> 70) == -1",
        "(-1 << 64) == 0",
        "(1 << 63) < 0",
        "!((4 >> -62) < 0)",
        "(1 >> -64) == 0",
        "(1 << 18446744073709551615u) == 0",
    ] {
        let (tokens, idents) = preprocess_str(&format!("#if {cond}\nyes\n#else\nno\n#endif\n"));
        assert_eq!(get_token_strings(&tokens, &idents), ["yes"], "#if {cond}");
    }
}
