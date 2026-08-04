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

/// Body-text width used by the tests; only tbl text blocks are sensitive to it.
const TEST_LINE_LENGTH: usize = 72;

fn run(s: &str) -> String {
    preprocess_with_loader(s, TEST_LINE_LENGTH, |_| None)
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
    // A leading `-` decrements the existing value (not an absolute negative).
    assert_eq!(run(".nr N 10\n.nr N -3\n\\n[N]\n"), "7\n");
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
    let out = preprocess_with_loader(".so inc.man\nafter\n", TEST_LINE_LENGTH, |target| {
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

// ---------------------------------------------------------------------------
// Recursion and overflow guards (#M1, #M7).
//
// These inputs previously overflowed the native stack, which aborts the process
// (SIGABRT) rather than unwinding — so a `should_panic` test cannot express
// them. Each test simply has to *return*.
// ---------------------------------------------------------------------------

#[test]
fn self_referential_string_register_terminates() {
    // `.ds A \*[A]` then `\*A`: expansion of A names A. Before the guard this
    // recursed until the stack was exhausted.
    let out = run(".ds A \\*[A]\n\\*A\n");
    assert!(!out.contains("\\*"), "register left unexpanded: {out:?}");
}

#[test]
fn mutually_referential_string_registers_terminate() {
    // A names B and B names A: a depth cap alone would still expand this pair
    // 64 times; the in-progress name set is what actually cuts the cycle.
    let out = run(".ds A x\\*B\n.ds B y\\*A\n\\*A\n");
    assert_eq!(out, "xy\n");
}

#[test]
fn deep_string_register_chain_terminates() {
    // An acyclic but deep chain: r0 -> r1 -> ... -> rN. No name repeats, so the
    // in-progress set never fires and only the depth cap bounds this. N is large
    // enough that the pre-fix code exhausted the stack.
    const N: usize = 20_000;
    let mut src = String::new();
    for i in 0..N {
        src.push_str(&format!(".ds r{i} \\*[r{}]\n", i + 1));
    }
    src.push_str(&format!(".ds r{N} end\n\\*[r0]\n"));
    let out = run(&src);
    assert!(!out.is_empty());
}

#[test]
fn self_referential_width_escape_terminates() {
    let out = run(".ds W \\w'\\*[W]'\n\\*W\n");
    assert!(!out.is_empty());
}

#[test]
fn expr_deep_nesting_is_rejected_not_crashed() {
    // 5000 open parens: `term()` and `expr()` are mutually recursive, one native
    // frame per level.
    let deep = format!("{}1{}", "(".repeat(5000), ")".repeat(5000));
    assert_eq!(eval_numeric(&deep), None);
    // Deeply nested unary operators recurse through the same path.
    assert_eq!(eval_numeric(&format!("{}1", "-".repeat(5000))), None);
    assert_eq!(eval_numeric(&format!("{}1", "!".repeat(5000))), None);
    // Nesting within the supported depth still evaluates.
    assert_eq!(eval_numeric("((((((1+2))))))"), Some(3));
}

#[test]
fn expr_arithmetic_overflow_saturates() {
    // Under `overflow-checks` (a debug build) these panicked.
    assert_eq!(eval_numeric("9223372036854775807+1"), Some(i64::MAX));
    assert_eq!(eval_numeric("9223372036854775807*2"), Some(i64::MAX));
    assert_eq!(eval_numeric("-9223372036854775807-2"), Some(i64::MIN));
    assert_eq!(eval_numeric("5%0"), Some(0));
}

// ---------------------------------------------------------------------------
// `.so` include-cycle guard (#M5).
// ---------------------------------------------------------------------------

#[test]
fn so_self_include_terminates() {
    // A file that sources itself. Inclusion is flattened onto the line queue,
    // so this is not caught by any recursion limit; before the guard it spun
    // until the multi-million-line budget tripped, taking minutes.
    let out = preprocess_with_loader("hello\n.so self\n", TEST_LINE_LENGTH, |t| {
        (t == "self").then(|| "hello\n.so self\n".to_string())
    });
    assert_eq!(out.lines().filter(|l| *l == "hello").count(), 2);
}

#[test]
fn so_mutual_include_cycle_terminates() {
    let out = preprocess_with_loader(".so a\n", TEST_LINE_LENGTH, |t| match t {
        "a" => Some("A\n.so b\n".to_string()),
        "b" => Some("B\n.so a\n".to_string()),
        _ => None,
    });
    assert!(out.contains('A') && out.contains('B'));
    assert_eq!(out.lines().count(), 2, "cycle re-expanded: {out:?}");
}

// ---------------------------------------------------------------------------
// Global output budgets (#M6, #M8).
// ---------------------------------------------------------------------------

#[test]
fn string_register_doubling_chain_is_bounded() {
    // The classic billion-laughs shape, in roff: each register doubles the next.
    // It is acyclic and only as deep as the chain, so neither the cycle set nor
    // the depth cap sees it — 30 lines expanded to 400 MB of output and 2 GB
    // resident. All of it lands on a single line, so a line budget cannot see it
    // either.
    let mut src = String::from(".ds L0 xxxxxxxxxx\n");
    for i in 1..=30 {
        src.push_str(&format!(".ds L{i} \\*[L{}]\\*[L{}]\n", i - 1, i - 1));
    }
    src.push_str("\\*[L30]\n");
    let out = run(&src);
    assert!(
        out.len() <= 2 << 20,
        "expansion not bounded: {} bytes",
        out.len()
    );
}

#[test]
fn while_loop_budget_is_global_not_per_iteration() {
    // A `.while` whose body invokes an endlessly self-recursive macro. Each
    // iteration used to get its own two-million-line allowance, so the loop's
    // iteration cap multiplied it: 100,000 x 2,000,000 lines. The counter is now
    // on the interpreter, not the loop, so the whole page shares one allowance.
    let src = ".de R\nx\n.R\n..\n.nr i 0\n\
               .while \\n[i]<100000 \\{\\\n.R\n.nr i +1\n\\}\n";
    let out = run(src);
    assert!(
        out.len() <= 80 << 20,
        "output not bounded: {} bytes",
        out.len()
    );
}

#[test]
fn request_arguments_do_not_include_a_trailing_comment() {
    // `.de NAME END` takes a custom terminator. A trailing `\"` comment reaching
    // the arguments made `\"` the terminator, which no line matched — so the
    // definition swallowed the rest of the page. pod2man writes `.de CQ
    // \" put $1 in typewriter font`, so this hit every perl-derived page.
    let out = run(".de CQ \\\" put $1 in typewriter font\n.ft CW\n..\n.SH NAME\nkept\n");
    assert_eq!(out, ".SH NAME\nkept\n");
    // Other requests must not see the comment either.
    assert_eq!(run(".ds A foo \\\" note\n[\\*A]\n"), "[foo]\n");
    assert_eq!(run(".nr N 40 \\\" note\n\\n(N\n"), "40\n");
}

// ---------------------------------------------------------------------------
// Conditional and macro-expansion correctness (#M10, #M11).
// ---------------------------------------------------------------------------

#[test]
fn parenthesised_condition_is_not_split_at_the_first_space() {
    // The expression ends at whitespace *outside* parentheses. Splitting at the
    // first whitespace anywhere took `(5` — truthy — and printed `> 99)` as the
    // body. `.if (a:(b)) \{\` is the standard groff idiom; every pod2man page
    // opens with one.
    assert_eq!(run(".if (5 > 99) body\nafter\n"), "after\n");
    assert_eq!(run(".if (5 < 99) body\n"), "body\n");
    assert_eq!(run(".if (0:(1==1)) yes\n"), "yes\n");
    assert_eq!(run(".if (1&(0)) no\nz\n"), "z\n");
}

#[test]
fn doubled_backslash_in_a_macro_body_does_not_leak() {
    // A macro body is written with doubled escapes so they act at call time.
    // Copy mode reduces `\\` to `\` when the body is stored; without it the
    // leading backslash of `\\$1` was emitted verbatim before every argument.
    assert_eq!(run(".de G\nHello \\\\$1\n..\n.G world\n"), "Hello world\n");
    // The single-backslash spelling keeps working.
    assert_eq!(run(".de H\nHi \\$1\n..\n.H there\n"), "Hi there\n");
    // A deferred register interpolation resolves at call time, not at
    // definition time.
    assert_eq!(run(".de S\nval \\\\n(X\n..\n.nr X 7\n.S\n"), "val 7\n");
}

#[test]
fn argument_count_register_is_available_in_a_macro_body() {
    assert_eq!(run(".de C\ngot \\\\n(.$\n..\n.C a b c\n"), "got 3\n");
    assert_eq!(run(".de D\ngot \\\\n[.$]\n..\n.D x\n"), "got 1\n");
    assert_eq!(run(".de E\ngot \\\\n(.$\n..\n.E\n"), "got 0\n");
}

#[test]
fn width_escape_counts_terminal_cells() {
    // `\w'…'` is a width in cells: an East Asian wide character is two.
    assert_eq!(run(".if \\w'日本'=4 yes\n"), "yes\n");
    assert_eq!(run(".if \\w'ab'=2 yes\n"), "yes\n");
}
