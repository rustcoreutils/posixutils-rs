//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};

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
    // '|' returns expr2 when expr1 is null or zero, regardless of expr2.
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
    // multibyte: length is in characters, not bytes
    expr_test(&["éé", ":", ".*"], "2\n");
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
