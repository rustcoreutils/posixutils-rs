//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
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
//

use crate::common::{compile_and_run, compile_expect_ok};

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

/// Unreachable code before the first `case` is discarded, and the reachable
/// part of the same function still gives the right answer.
#[test]
fn misc_an_unreachable_statement_does_not_change_the_answer() {
    let code = r#"
int calls;
int g(void) { calls++; return 3; }

int f(int x)
{
    switch (x) {
        g() ? g() : g();          /* unreachable: never selected */
        (void)(g() && g());
    case 1:
        return 1;
    default:
        return 2;
    }
}

int main(void)
{
    if (f(1) != 1) return 1;
    if (f(0) != 2) return 2;
    /* Nothing before the first case may run. */
    if (calls != 0) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("nocur_answer", code, &[]), 0);
}
