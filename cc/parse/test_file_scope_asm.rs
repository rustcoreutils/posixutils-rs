//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU basic asm at file scope: `__asm__("...");` between external
// declarations, kept as an item of the translation unit in source order.
//

use super::ast::ExternalDecl;
use super::test_parser::parse_tu;

/// The text of every file-scope asm in `src`, in order.
fn asm_texts(src: &str) -> Vec<String> {
    let (tu, ..) = parse_tu(src).expect("parse");
    tu.items
        .into_iter()
        .filter_map(|item| match item {
            ExternalDecl::Asm { text, .. } => Some(text),
            _ => None,
        })
        .collect()
}

/// The parse error `src` draws.
fn error_of(src: &str) -> String {
    match parse_tu(src) {
        Ok(_) => panic!("{src:?} should not parse"),
        Err(e) => e.message,
    }
}

/// The asm is an item of its own, between the declarations around it.
#[test]
fn file_scope_asm_is_an_item_in_source_order() {
    let (tu, ..) = parse_tu(
        "int a = 1;\n\
         __asm__(\".symver a_impl, a@@V2\");\n\
         int f(void) { return a; }\n",
    )
    .expect("parse");
    let [ExternalDecl::Declaration(_), ExternalDecl::Asm { text, .. }, ExternalDecl::FunctionDef(_)] =
        tu.items.as_slice()
    else {
        panic!("{:#?}", tu.items);
    };
    assert_eq!(text, ".symver a_impl, a@@V2");
}

/// `asm`, `__asm` and `__asm__` all introduce it; the literals concatenate,
/// escapes are decoded, and `%` is left alone, since nothing is substituted
/// into basic asm.
#[test]
fn file_scope_asm_spellings_and_concatenation() {
    assert_eq!(
        asm_texts(
            "asm(\"a\");\n\
             __asm(\"b\" \"\\n\\t\" \"c\");\n\
             __asm__(\"movl %eax, %%ebx\\x21\");\n"
        ),
        ["a", "b\n\tc", "movl %eax, %%ebx!"]
    );
}

/// A UTF-8 character in the text is one character of it.
#[test]
fn file_scope_asm_text_is_decoded() {
    assert_eq!(asm_texts("__asm__(\"# caf\u{e9}\");"), ["# caf\u{e9}"]);
}

/// Operands are for an asm statement inside a function; gcc reads the first
/// `:` at file scope as a missing `)`.
#[test]
fn file_scope_extended_asm_is_an_error() {
    for src in [
        "__asm__(\"nop\" : : );",
        "int x; __asm__(\"nop\" : \"=r\"(x));",
        "__asm__(\"nop\" ::: \"memory\");",
    ] {
        assert_eq!(error_of(src), "expected ')' before ':' token", "{src}");
    }
}

/// gcc takes no qualifier on a file-scope asm: `volatile`, `inline` and
/// `goto` are all an error where the `(` should be.
#[test]
fn file_scope_asm_qualifiers_are_errors() {
    for q in ["volatile", "__volatile__", "inline", "__inline__", "goto"] {
        assert_eq!(
            error_of(&format!("__asm__ {q} (\"nop\");")),
            format!("expected '(' before '{q}'"),
        );
    }
}

/// The operand must be a narrow string literal.
#[test]
fn file_scope_asm_operand_must_be_a_narrow_string() {
    assert_eq!(
        error_of("__asm__(L\"nop\");"),
        "a wide string is invalid in this context"
    );
    assert!(error_of("__asm__(1);").starts_with("expected string literal"));
    assert!(error_of("__asm__();").starts_with("expected string literal"));
    assert!(error_of("__asm__(\"nop\")").contains("';'"));
}
