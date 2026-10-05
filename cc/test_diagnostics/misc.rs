//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compile-only cases of the misc suite (tests/misc), in process.
//

use crate::test_compile::{
    compile_expect_error, compile_expect_no_diagnostic, compile_expect_ok, compile_expect_warning,
    compile_expect_warning_with,
};

// ============================================================================
// tests/misc/float_n.rs:
// C23's _FloatN and _FloatNx types (TS 18661-3), which c17 has as gcc does:
// distinct types in the formats of the standard ones. glibc relies on that
// once the compiler claims gcc 7 -- its <math.h> lists `float:` and
// `_Float32:` in one `_Generic`.
// ============================================================================

#[test]
fn float_n_specifier_combinations() {
    compile_expect_error(
        "float_n_long",
        "long _Float64 x;\n",
        "both 'long' and '_Float64' in declaration specifiers",
    );
    compile_expect_error(
        "float_n_unsigned",
        "unsigned _Float32x x;\n",
        "both 'unsigned' and '_Float32x' in declaration specifiers",
    );
}

// ============================================================================
// tests/misc/no_current_block.rs:
// Valid C for which the linearizer holds no current basic block.
//
// `Linearizer::current_bb` is legitimately `None` in two situations the
// language allows: after a `goto`, and inside a `switch` body before the first
// `case` label. Code in either place is unreachable but well-formed, and the
// standard requires it to be translated, not rejected -- and certainly not to
// crash the compiler.
//
// Every lowering that ends a block has to read `current_bb` back afterwards.
// The ones that reached for `.unwrap()` instead turned each of these programs
// into an internal compiler error. `current_or_unreachable_bb()` is the
// accessor that starts a fresh unreachable block rather than panicking.
//
// These are compile-only where the construct is genuinely unreachable, because
// there is no answer to assert; the two that are reachable check the answer as
// well.
// ============================================================================

/// A statement before a `switch`'s first `case` has no enclosing block.
///
/// Each of these ended in a panic, one per lowering: the ternary, the two
/// short-circuit operators, the complex ternary, both spellings of the GNU
/// elvis operator, complex integer division, and the atomic CAS loop.
#[test]
fn misc_a_statement_before_the_first_case_does_not_ice() {
    for (tag, body) in [
        ("ternary", "g() ? g() : g();"),
        ("logical_and", "(void)(g() && g());"),
        ("logical_or", "(void)(g() || g());"),
        ("elvis", "(void)(g() ?: g());"),
        ("nested_ternary", "(void)(g() ? (g() ? g() : g()) : g());"),
        ("and_in_ternary", "(void)(g() ? (g() && g()) : g());"),
    ] {
        compile_expect_ok(
            &format!("nocur_switch_{tag}"),
            &format!(
                "int g(void);\n\
                 int f(int x) {{ switch (x) {{ {body} case 1: return 1; }} return 0; }}\n"
            ),
        );
    }

    compile_expect_ok(
        "nocur_switch_complex_ternary",
        "int g2(void);\n_Complex double h(void);\n\
         int f(int x) { switch (x) { (void)(g2() ? h() : h()); case 1: return 1; } return 0; }\n",
    );
    compile_expect_ok(
        "nocur_switch_complex_elvis",
        "_Complex double h(void);\n\
         int f(int x) { switch (x) { (void)(h() ?: h()); case 1: return 1; } return 0; }\n",
    );
    compile_expect_ok(
        "nocur_switch_complex_int_div",
        "_Complex int ci(void);\n\
         int f(int x) { switch (x) { ci() / ci(); case 1: return 1; } return 0; }\n",
    );
    compile_expect_ok(
        "nocur_switch_atomic_nand",
        "_Atomic int a;\n\
         int f(int x) { switch (x) { __atomic_fetch_nand(&a, 1, 5); case 1: return 1; } \
         return 0; }\n",
    );
}

/// The same shapes after a `goto`, which is the other way to have no block.
#[test]
fn misc_a_statement_after_a_goto_does_not_ice() {
    for (tag, body) in [
        ("ternary", "x = g() ? g() : g();"),
        ("logical_and", "x = g() && g();"),
        ("logical_or", "x = g() || g();"),
        ("elvis", "x = g() ?: g();"),
    ] {
        compile_expect_ok(
            &format!("nocur_goto_{tag}"),
            &format!("int g(void);\nint f(int x) {{ goto L; {body} L: return x; }}\n"),
        );
    }
}

/// A `goto` out of one arm leaves *that arm* without a block, which is a
/// different site from the condition's.
///
/// These are why the fix belongs in the shared diamond builder rather than on
/// the condition alone: the helper reads the block back at the end of each arm
/// too.
#[test]
fn misc_a_goto_out_of_a_conditional_arm_does_not_ice() {
    for (tag, init) in [
        ("then", "x ? ({ goto L; g(); }) : g()"),
        ("else", "x ? g() : ({ goto L; g(); })"),
        ("and", "x && ({ goto L; g(); })"),
        ("or", "x || ({ goto L; g(); })"),
        ("elvis", "g() ?: ({ goto L; g(); })"),
        ("both", "x ? ({ goto L; g(); }) : ({ goto L; g(); })"),
    ] {
        compile_expect_ok(
            &format!("nocur_arm_{tag}"),
            &format!("int g(void);\nint f(int x) {{ int y = {init}; L: return y; }}\n"),
        );
    }
}

// ============================================================================
// tests/misc/statement_attributes.rs:
// Attributes written as statements
// ============================================================================

/// Where gcc is silent: directly before a `case` or `default`, before an
/// ordinary label, and before a brace a `case` may be reached through.
#[test]
fn misc_fallthrough_statement_before_a_label_is_silent() {
    let code = r#"
int f(int x) {
    switch (x) {
    case 1: x++; __attribute__((fallthrough));
    case 2: x++; __attribute__((fallthrough));
    lab: case 3: x++; __attribute__((fallthrough));
    { case 4: x++; }
    case 5: x++; __attribute__((fallthrough));
    default: x++;
    }
    return x;
}
"#;
    compile_expect_no_diagnostic("fallthrough_stmt_quiet", code, "fallthrough");
}

/// A fallthrough statement with a statement after it falls into no label,
/// which gcc warns about in these words.
#[test]
fn misc_fallthrough_statement_not_before_a_label_warns() {
    let code = r#"
int f(int x) {
    switch (x) {
    case 1: __attribute__((fallthrough)); x++;
    case 2: x++;
    }
    return x;
}
"#;
    compile_expect_warning(
        "fallthrough_stmt_stray",
        code,
        "attribute 'fallthrough' not preceding a case label or default label",
    );
}

/// Outside every `switch` there is nothing to fall into, and gcc rejects it.
#[test]
fn misc_fallthrough_statement_outside_a_switch_is_rejected() {
    compile_expect_error(
        "fallthrough_stmt_no_switch",
        "int f(int x) { if (x) __attribute__((fallthrough)); return x; }\n",
        "invalid use of attribute 'fallthrough'",
    );
}

/// An attribute on a statement that is not null is not the fallthrough
/// statement: gcc reads it as a declaration and rejects it, and so does c17.
#[test]
fn misc_fallthrough_attribute_on_a_statement_is_rejected() {
    compile_expect_error(
        "fallthrough_on_stmt",
        "int f(int x) { switch (x) { case 1: x++; __attribute__((fallthrough)) x++;\n\
         case 2: x++; } return x; }\n",
        "",
    );
}

/// Beside `fallthrough`, another recognised attribute applies to nothing:
/// warned about in gcc's words, under `-Wattributes`.
#[test]
fn misc_fallthrough_statement_ignores_other_attributes() {
    let code = r#"
int f(int x) {
    switch (x) {
    case 1: x++; __attribute__((fallthrough, unused));
    case 2: x++;
    }
    return x;
}
"#;
    compile_expect_warning(
        "fallthrough_stmt_unused",
        code,
        "'unused' attribute ignored",
    );
    let stderr = compile_expect_warning_with(
        "fallthrough_stmt_unused_quiet",
        code,
        &["-Wno-attributes".to_string()],
    );
    assert!(!stderr.contains("attribute ignored"), "{stderr}");
}

/// Any other attribute list alone before `;` is an empty declaration -- in a
/// block and at file scope -- which gcc accepts with a warning.
#[test]
fn misc_attribute_declaration_is_empty() {
    compile_expect_warning(
        "attr_decl_block",
        "int f(int x) { __attribute__((unused)); return x; }\n",
        "empty declaration",
    );
    compile_expect_warning(
        "attr_decl_file",
        "__attribute__((unused));\nint f(int x) { return x; }\n",
        "empty declaration",
    );
}
