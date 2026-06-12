//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::expr::eval_numeric;
use super::preprocess_with_loader;

fn run(s: &str) -> String {
    preprocess_with_loader(s, |_| None)
}

#[test]
fn plain_text_is_passed_through() {
    // A page with no roff programmability is emitted essentially unchanged.
    let input = ".TH CAT 1\n.SH NAME\ncat \\- concatenate\n";
    assert_eq!(run(input), ".TH CAT 1\n.SH NAME\ncat \\- concatenate\n");
}

#[test]
fn mdoc_page_is_identity() {
    let input = ".Dd July 4, 2024\n.Dt CAT 1\n.Os\n.Sh NAME\n.Nm cat\n.Nd concatenate\n";
    assert_eq!(run(input), input);
}

#[test]
fn full_line_comment_dropped() {
    assert_eq!(run(".\\\" a comment\ntext\n"), "text\n");
}

#[test]
fn trailing_comment_stripped() {
    assert_eq!(run("hello \\\" trailing\n"), "hello\n");
    // An escaped backslash before the quote is not a comment.
    assert_eq!(run("a\\\\\"b\n"), "a\\\\\"b\n");
}

#[test]
fn string_register_define_and_interpolate() {
    assert_eq!(run(".ds GW groff\nuse \\*(GW now\n"), "use groff now\n");
    assert_eq!(run(".ds X y\n\\*[X]\\*(GW\n"), "y\n");
}

#[test]
fn string_append() {
    assert_eq!(run(".ds A foo\n.as A bar\n\\*A\n"), "foobar\n");
}

#[test]
fn number_register_and_interpolate() {
    assert_eq!(run(".nr N 40\nwidth \\n(N\n"), "width 40\n");
    assert_eq!(run(".nr N 5\n.nr N +3\n\\n[N]\n"), "8\n");
}

#[test]
fn if_numeric_true_and_false() {
    assert_eq!(run(".if 1 yes\n"), "yes\n");
    assert_eq!(run(".if 0 no\nafter\n"), "after\n");
    assert_eq!(run(".nr W 80\n.if \\n(W>40 wide\n"), "wide\n");
}

#[test]
fn if_nroff_troff_conditions() {
    // `n` (nroff) is always true here; `t` (troff) always false.
    assert_eq!(run(".if n nroff\n"), "nroff\n");
    assert_eq!(run(".if t troff\nx\n"), "x\n");
}

#[test]
fn if_negation() {
    assert_eq!(run(".if !t yes\n"), "yes\n");
    assert_eq!(run(".if !1 no\nz\n"), "z\n");
}

#[test]
fn string_comparison_condition() {
    assert_eq!(run(".if 'a'a' eq\n"), "eq\n");
    assert_eq!(run(".if 'a'b' ne\nq\n"), "q\n");
    // With register interpolation in the operands.
    assert_eq!(run(".ds S linux\n.if '\\*S'linux' match\n"), "match\n");
}

#[test]
fn ie_el_pair() {
    assert_eq!(run(".ie t A\n.el B\n"), "B\n");
    assert_eq!(run(".ie n A\n.el B\n"), "A\n");
}

#[test]
fn if_block_braces() {
    let input = ".if 1 \\{\\\n.ds X a\n\\*X\\}\n";
    // The block body runs; X is defined and interpolated.
    assert_eq!(run(input), "a\n");
}

#[test]
fn if_block_skipped_when_false() {
    let input = ".if 0 \\{\\\nshould not appear\n.\\}\nafter\n";
    assert_eq!(run(input), "after\n");
}

#[test]
fn register_defined_condition() {
    assert_eq!(run(".nr X 1\n.if rX has\n"), "has\n");
    assert_eq!(run(".if rY no\nz\n"), "z\n");
}

#[test]
fn macro_definition_and_call() {
    let input = ".de GREET\nHello \\$1 from \\$2\n..\n.GREET world here\n";
    assert_eq!(run(input), "Hello world from here\n");
}

#[test]
fn macro_append() {
    let input = ".de M\nfirst\n..\n.am M\nsecond\n..\n.M\n";
    assert_eq!(run(input), "first\nsecond\n");
}

#[test]
fn macro_star_args() {
    let input = ".de ALL\ngot \\$# args: \\$*\n..\n.ALL a b c\n";
    assert_eq!(run(input), "got 3 args: a b c\n");
}

#[test]
fn macro_quoted_args() {
    let input = ".de J\n[\\$1][\\$2]\n..\n.J \"a b\" c\n";
    assert_eq!(run(input), "[a b][c]\n");
}

#[test]
fn nested_macro_call() {
    let input = ".de INNER\nin\n..\n.de OUTER\n.INNER\nout\n..\n.OUTER\n";
    assert_eq!(run(input), "in\nout\n");
}

#[test]
fn ignore_block() {
    assert_eq!(run(".ig\nhidden\nlines\n..\nvisible\n"), "visible\n");
}

#[test]
fn rename_and_remove() {
    assert_eq!(run(".ds A x\n.rn A B\n\\*B\\*A\n"), "x\n");
    assert_eq!(run(".ds A x\n.rm A\n[\\*A]end\n"), "[]end\n");
}

#[test]
fn so_inclusion_via_loader() {
    let out = preprocess_with_loader(".so inc.man\nafter\n", |target| {
        if target == "inc.man" {
            Some(".SH INCLUDED\nbody\n".to_string())
        } else {
            None
        }
    });
    assert_eq!(out, ".SH INCLUDED\nbody\nafter\n");
}

#[test]
fn so_without_loader_is_dropped() {
    assert_eq!(run(".so other\nkept\n"), "kept\n");
}

#[test]
fn width_escape_resolves_to_cell_count() {
    assert_eq!(run(".if \\w'hello'=5 yes\n"), "yes\n");
}

#[test]
fn while_block_loop() {
    let input = ".nr i 0\n.while \\n[i]<3 \\{\\\n\\n[i]\n.nr i +1\n\\}\n";
    assert_eq!(run(input), "0\n1\n2\n");
}

#[test]
fn while_single_line_body() {
    let input = ".nr i 0\n.while \\n[i]<2 .nr i +1\ndone \\n[i]\n";
    assert_eq!(run(input), "done 2\n");
}

#[test]
fn while_false_runs_zero_times() {
    assert_eq!(run(".if 1 a\n.while 0 never\nb\n"), "a\nb\n");
}

#[test]
fn diversion_capture_and_replay() {
    let input = ".di SAVE\ncaptured line\n.di\nbefore\n.SAVE\nafter\n";
    assert_eq!(run(input), "before\ncaptured line\nafter\n");
}

#[test]
fn diversion_append() {
    let input = ".di D\none\n.di\n.da D\ntwo\n.di\n.D\n";
    assert_eq!(run(input), "one\ntwo\n");
}

#[test]
fn traps_and_environments_dropped() {
    let input = ".wh 0 NL\n.ev 1\n.dt 5 X\nkept\n.ch NL 1\n.evc 0\n";
    assert_eq!(run(input), "kept\n");
}

#[test]
fn tbl_region_is_laid_out() {
    let out = run(".TS\nl l.\nName\tValue\n.TE\nafter\n");
    assert!(out.contains(".nf"), "{out}");
    assert!(out.contains("Name  Value"), "{out}");
    assert!(out.contains(".fi"), "{out}");
    assert!(out.contains("after"), "{out}");
}

#[test]
fn eqn_region_is_linearized() {
    let out = run(".EQ\nx over y\n.EN\nrest\n");
    assert!(out.contains("x/y"), "{out}");
    assert!(out.contains("rest"), "{out}");
}

// ── expression evaluator ─────────────────────────────────────────────────

#[test]
fn expr_left_to_right_no_precedence() {
    // roff has no precedence: 1+2*3 == (1+2)*3 == 9.
    assert_eq!(eval_numeric("1+2*3"), Some(9));
    assert_eq!(eval_numeric("2+3"), Some(5));
    assert_eq!(eval_numeric("10-2-3"), Some(5));
}

#[test]
fn expr_parens_group() {
    assert_eq!(eval_numeric("1+(2*3)"), Some(7));
    assert_eq!(eval_numeric("(2+3)*4"), Some(20));
}

#[test]
fn expr_comparisons_and_logic() {
    assert_eq!(eval_numeric("5>3"), Some(1));
    assert_eq!(eval_numeric("5<3"), Some(0));
    assert_eq!(eval_numeric("4=4"), Some(1));
    assert_eq!(eval_numeric("1&1"), Some(1));
    assert_eq!(eval_numeric("0:1"), Some(1));
    assert_eq!(eval_numeric("3<>4"), Some(1));
}

#[test]
fn expr_unit_suffix_ignored() {
    assert_eq!(eval_numeric("78n"), Some(78));
    assert_eq!(eval_numeric("1i"), Some(1));
}

#[test]
fn expr_division_by_zero_is_zero() {
    assert_eq!(eval_numeric("5/0"), Some(0));
}

#[test]
fn expr_malformed_is_none() {
    assert_eq!(eval_numeric("abc"), None);
    assert_eq!(eval_numeric(""), None);
}
