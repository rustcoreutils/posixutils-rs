//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{
    locale_matching, os_bytes, run_test, run_test_os, run_test_with_env, utf8_locale, TestPlan,
    TestPlanOs,
};

// success: result is neither null nor zero (exit 0)
fn expr_test(args: &[&str], expected_output: &str) {
    expr_test_status(args, expected_output, "", 0);
}

// general form: assert stdout, stderr, and exit code
fn expr_test_status(args: &[&str], expected_out: &str, expected_err: &str, code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("expr"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_out),
        expected_err: String::from(expected_err),
        expected_exit_code: code,
    });
}

// Operator precedence: '*' binds tighter than '+', comparisons bind looser
// than arithmetic, and '|'/'&' are lowest. Values verified against GNU expr.
#[test]
fn expr_precedence() {
    expr_test(&["2", "+", "3", "*", "4"], "14\n");
    expr_test(&["2", "*", "3", "+", "4"], "10\n");
    expr_test(&["10", "-", "2", "-", "3"], "5\n"); // left-associative
    expr_test(&["1", "+", "2", "*", "3", "+", "4"], "11\n");
    expr_test(&["(", "2", "+", "3", ")", "*", "4"], "20\n");
}

#[test]
fn expr_logops() {
    expr_test(&["4", "|", "5", "+", "1"], "4\n");
    expr_test(&["0", "|", "5", "+", "1"], "6\n");
    expr_test(&["4", "&", "5", "+", "1"], "4\n");
    expr_test(&["4", "&", "0", "+", "1"], "4\n");
    expr_test(&["0", "%", "5", "+", "1"], "1\n");
    // '|' returns expr2 when expr1 is null or zero and expr2 is not null.
    expr_test_status(&["0", "|", "0"], "0\n", "", 1);
    expr_test(&["", "|", "abc"], "abc\n");
    // A string-valued "0" (here produced by a ':' capture) is zero for '|'/'&'.
    expr_test(&["0abc", ":", "\\(0\\)", "|", "5"], "5\n");
    expr_test_status(&["0abc", ":", "\\(0\\)", "&", "5"], "0\n", "", 1);
}

#[test]
fn expr_intops() {
    expr_test(&["4", "+", "4", "+", "1"], "9\n");
    expr_test(&["4", "-", "4", "+", "1"], "1\n");
    expr_test(&["4", "*", "4", "+", "1"], "17\n");
    expr_test(&["4", "/", "4", "+", "1"], "2\n");
    expr_test(&["4", "%", "4", "+", "1"], "1\n");
}

#[test]
fn expr_cmpint() {
    // '+' binds tighter than the comparison, so the rhs is 5 + 1 == 6.
    expr_test(&["4", "<", "5", "+", "1"], "1\n");
    expr_test_status(&["4", ">", "5", "+", "1"], "0\n", "", 1);
    expr_test(&["4", "<=", "5", "+", "1"], "1\n");
    expr_test_status(&["4", ">=", "5", "+", "1"], "0\n", "", 1);
    expr_test_status(&["4", "=", "5", "+", "1"], "0\n", "", 1);
    expr_test(&["4", "!=", "5", "+", "1"], "1\n");
}

#[test]
fn expr_cmpstr() {
    expr_test(&["aaa", "<", "bbb"], "1\n");
    expr_test_status(&["aaa", ">", "bbb"], "0\n", "", 1);
    expr_test(&["aaa", "<=", "bbb"], "1\n");
    expr_test_status(&["aaa", ">=", "bbb"], "0\n", "", 1);
    expr_test_status(&["aaa", "=", "bbb"], "0\n", "", 1);
    expr_test(&["aaa", "!=", "bbb"], "1\n");
}

// POSIX EXIT STATUS: 0 when the result is neither null nor zero, else 1.
#[test]
fn expr_exit_status() {
    expr_test(&["1"], "1\n");
    expr_test_status(&["0"], "0\n", "", 1);
    expr_test_status(&[""], "\n", "", 1);
    expr_test(&["abc"], "abc\n");
    expr_test(&["0.0"], "0.0\n"); // only the literal "0" counts as zero
}

// Invalid expressions: diagnostic to stderr, exit status 2.
#[test]
fn expr_invalid() {
    expr_test_status(&["1", "+"], "", "expr: syntax error: missing argument\n", 2);
    expr_test_status(&["6", "/", "0"], "", "expr: division by zero\n", 2);
    expr_test_status(&["abc", "+", "1"], "", "expr: non-integer argument\n", 2);
}

// "--" delimits end of options, protecting a leading-minus operand.
#[test]
fn expr_dashdash() {
    expr_test(&["--", "-3", "+", "1"], "-2\n");
}

// ':' matching operator — POSIX BRE, anchored at start, character-count
// length, and "\1" back-reference. Values verified against GNU expr.
#[test]
fn expr_match() {
    // anchored char-count length
    expr_test(&["abcd", ":", "ab"], "2\n");
    // no anchored match -> 0 (exit 1)
    expr_test_status(&["abcd", ":", "bc"], "0\n", "", 1);
    // Multibyte: the count is in characters, and LC_CTYPE decides what a
    // character is. The runner forces LC_ALL=C, where each byte is one.
    expr_test(&["éé", ":", ".*"], "4\n");
    // back-reference capture
    expr_test(&["abc", ":", "a\\(b\\)c"], "b\n");
    expr_test(&["hello", ":", "h\\(.*\\)o"], "ell\n");
    // canonical pathname example from the spec
    expr_test(&["//abc/file", ":", ".*/\\(.*\\)"], "file\n");
    // subexpression present but matches the null string -> null (exit 1)
    expr_test_status(&["abc", ":", "a\\(x*\\)"], "\n", "", 1);
    // subexpression present but no match at all -> null (exit 1)
    expr_test_status(&["abc", ":", "x\\(y\\)"], "\n", "", 1);
}

/// LC_CTYPE decides what a character is, so the same operand counts
/// differently in a single-byte and a multi-byte locale. Verified against GNU
/// expr, which gives 4 and 2 respectively.
#[test]
fn expr_match_length_follows_lc_ctype() {
    // The runner forces LC_ALL=C when the test names no locale of its own.
    expr_test(&["éé", ":", ".*"], "4\n");
    let Some(locale) = utf8_locale() else {
        return; // no UTF-8 locale installed on this host
    };
    expr_test_locale(&locale, &["éé", ":", ".*"], "2\n", 0);
}

/// Match offsets come from regexec and are byte offsets. Slicing them into
/// text aborted the process whenever a pattern matched part of a character,
/// which in a single-byte locale is any `.` against a multibyte operand.
#[test]
fn expr_match_on_partial_characters() {
    expr_test(&["日本語", ":", "."], "1\n");
    expr_test(&["日本語", ":", ".."], "2\n");
    expr_test(&["日本語", ":", ".*"], "9\n");
    // The capture is the bytes matched; asserting it needs a byte-exact
    // comparison, so it lives with the other byte-string cases below.
}

/// A backslash is an ordinary character inside a bracket expression
/// (XBD 9.3.5), so `[\(]` is not a subexpression and the operator returns a
/// match length rather than a capture.
#[test]
fn expr_match_bracket_expression_is_not_a_subexpression() {
    expr_test(&["(x", ":", "[\\(]"], "1\n");
    expr_test(&["(x", ":", "[\\(]x"], "2\n");
    expr_test(&["ab", ":", "[a\\(]b"], "2\n");
    expr_test(&["ab", ":", "[[:alpha:]]b"], "2\n");
    // A real subexpression alongside a bracket expression still captures.
    expr_test(&["a(b", ":", "\\(a[\\(]b\\)"], "a(b\n");
}

/// POSIX identifies an argument as an integer only where the operator needs
/// one; everywhere else the operand keeps the spelling it was given.
#[test]
fn expr_operands_keep_their_text() {
    expr_test(&["007"], "007\n");
    // Still the integer zero, so the status is 1 even though the text is kept.
    expr_test_status(&["00"], "00\n", "", 1);
    expr_test_status(&["--", "-0"], "-0\n", "", 1);
    // ':' compares strings, so the operand is three characters, not one.
    expr_test(&["007", ":", ".*"], "3\n");
    // '|' yields the operand as written.
    expr_test(&["0", "|", "007"], "007\n");
    // Arithmetic and integer comparison still see the value.
    expr_test(&["007", "+", "1"], "8\n");
    expr_test(&["007", "=", "7"], "1\n");
    // A mixed comparison compares the text as given.
    expr_test_status(&["007", "=", "7abc"], "0\n", "", 1);
}

/// POSIX: "an (optional) unary minus followed by digits". A leading plus does
/// not make an integer, so `+5` is a string and `+0` is not zero.
#[test]
fn expr_leading_plus_is_not_an_integer() {
    expr_test_status(&["+5", "+", "1"], "", "expr: non-integer argument\n", 2);
    expr_test(&["+5"], "+5\n");
    // A string comparison, so not equal.
    expr_test_status(&["+5", "=", "5"], "0\n", "", 1);
    // Neither null nor zero, so the exit status is 0.
    expr_test(&["+0"], "+0\n");
}

/// An operand too large to represent is an integer, and saying it is not one
/// describes the input wrongly.
#[test]
fn expr_integer_out_of_range() {
    expr_test_status(
        &["99999999999999999999999999999999999999999", "+", "1"],
        "",
        "expr: integer out of range\n",
        2,
    );
    // x % -1 is zero for every x, though the matching division overflows.
    expr_test_status(
        &["--", "-170141183460469231731687303715884105728", "%", "-1"],
        "0\n",
        "",
        1,
    );
}

/// POSIX operands are byte strings and need not be text; decoding argv as
/// UTF-8 aborted the process on one that was not.
#[test]
fn expr_non_utf8_operands() {
    // Printed back unchanged.
    run_test_os(TestPlanOs {
        cmd: String::from("expr"),
        args: vec![os_bytes(b"a\xffb")],
        stdin_data: Vec::new(),
        expected_out: b"a\xffb\n".to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
    // Matched as bytes.
    run_test_os(TestPlanOs {
        cmd: String::from("expr"),
        args: vec![os_bytes(b"a\xffb"), os_bytes(b":"), os_bytes(b".*")],
        stdin_data: Vec::new(),
        expected_out: b"3\n".to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
    // A capture returns the bytes, not a lossy replacement.
    run_test_os(TestPlanOs {
        cmd: String::from("expr"),
        args: vec![os_bytes(b"a\xffb"), os_bytes(b":"), os_bytes(b"\\(a.\\)")],
        stdin_data: Vec::new(),
        expected_out: b"a\xff\n".to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
    // Compared as bytes.
    run_test_os(TestPlanOs {
        cmd: String::from("expr"),
        args: vec![os_bytes(b"a\xffb"), os_bytes(b"="), os_bytes(b"a\xffb")],
        stdin_data: Vec::new(),
        expected_out: b"1\n".to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
    // A capture that lands inside a multibyte character returns those bytes,
    // rather than aborting or substituting a replacement character.
    run_test_os(TestPlanOs {
        cmd: String::from("expr"),
        args: vec![
            os_bytes("日本語".as_bytes()),
            os_bytes(b":"),
            os_bytes(b"\\(..\\)"),
        ],
        stdin_data: Vec::new(),
        expected_out: b"\xe6\x97\n".to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}

/// POSIX: "returns the evaluation of expr1 if it is neither null nor zero;
/// otherwise, returns the evaluation of expr2 if it is not null; otherwise,
/// zero." A null expr2 yields zero, not the null string. Verified against GNU.
#[test]
fn expr_or_with_a_null_right_operand() {
    expr_test_status(&["", "|", ""], "0\n", "", 1);
    expr_test_status(&["0", "|", ""], "0\n", "", 1);
    expr_test_status(&["00", "|", ""], "0\n", "", 1);
    // A zero expr2 is still returned; only a null one becomes zero.
    expr_test_status(&["", "|", "0"], "0\n", "", 1);
    expr_test(&["", "|", "abc"], "abc\n");
    expr_test(&["4", "|", "5"], "4\n");
}

/// Deep nesting must report a limit rather than abort on a guard page.
#[test]
fn expr_deep_nesting_is_bounded() {
    const DEPTH: usize = 5000;
    let mut args: Vec<&str> = vec!["("; DEPTH];
    args.push("1");
    args.extend(std::iter::repeat_n(")", DEPTH));
    expr_test_status(&args, "", "expr: expression nested too deeply\n", 2);
    // Ordinary nesting still evaluates.
    expr_test(&["(", "(", "2", "+", "3", ")", "*", "4", ")"], "20\n");
}

// Run one comparison under an explicit locale.
fn expr_test_locale(locale: &str, args: &[&str], expected_out: &str, code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_env(
        TestPlan {
            cmd: String::from("expr"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::from(expected_out),
            expected_err: String::new(),
            expected_exit_code: code,
        },
        &[("LC_ALL", locale)],
    );
}

// String comparison must use the collating sequence of the current locale
// (POSIX 94588-94590), not byte order.
//
// The discriminator is a case-crossing pair. In the C locale the collating
// sequence is byte order, so 'B' (0x42) sorts before 'a' (0x61) and `a < B` is
// false. Under glibc's en_US.UTF-8 the primary weight is the letter rather than
// the byte, so `a < B` is true. A byte-wise implementation returns the same
// answer under both locales, which is what this catches.
//
// Self-skips when no suitable locale is installed: with only C/POSIX available
// there is no second collating sequence to compare against.
#[test]
fn expr_string_compare_honors_lc_collate() {
    let Some(locale) = locale_matching(&["en_US.UTF-8", "en_US.utf8"]) else {
        return;
    };

    // C locale: byte order.
    expr_test_locale("C", &["a", "<", "B"], "0\n", 1);
    expr_test_locale("C", &["B", "<", "a"], "1\n", 0);

    // Collating locale: the opposite answer for the same operands.
    expr_test_locale(&locale, &["a", "<", "B"], "1\n", 0);
    expr_test_locale(&locale, &["B", "<", "a"], "0\n", 1);

    // The remaining relational operators route through the same comparison,
    // so they flip together. `cmpop` falls back to string compare whenever
    // either operand is non-numeric, which is why this is not just `<`.
    expr_test_locale("C", &["a", ">", "B"], "1\n", 0);
    expr_test_locale(&locale, &["a", ">", "B"], "0\n", 1);
    expr_test_locale("C", &["a", ">=", "B"], "1\n", 0);
    expr_test_locale(&locale, &["a", "<=", "B"], "1\n", 0);

    // Equality is locale-independent for these operands, in both locales.
    expr_test_locale("C", &["a", "=", "a"], "1\n", 0);
    expr_test_locale(&locale, &["a", "=", "a"], "1\n", 0);
    expr_test_locale(&locale, &["a", "!=", "b"], "1\n", 0);

    // Integer comparison must not be affected: `cmpop` only reaches `cmpstr`
    // when an operand is not an integer.
    expr_test_locale(&locale, &["10", ">", "9"], "1\n", 0);
    expr_test_locale(&locale, &["9", "<", "10"], "1\n", 0);
}
