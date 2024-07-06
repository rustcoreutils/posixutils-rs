//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test, TestPlan};

fn expr_test(args: &[&str], expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("expr"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn expr_logops() {
    expr_test(&["4", "|", "5", "+", "1"], "5\n");
    expr_test(&["0", "|", "5", "+", "1"], "6\n");
    expr_test(&["4", "&", "5", "+", "1"], "5\n");
    expr_test(&["4", "&", "0", "+", "1"], "1\n");
    expr_test(&["0", "%", "5", "+", "1"], "1\n");
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
    expr_test(&["4", "<", "5", "+", "1"], "2\n");
    expr_test(&["4", ">", "5", "+", "1"], "1\n");
    expr_test(&["4", "<=", "5", "+", "1"], "2\n");
    expr_test(&["4", ">=", "5", "+", "1"], "1\n");
    expr_test(&["4", "=", "5", "+", "1"], "1\n");
    expr_test(&["4", "!=", "5", "+", "1"], "2\n");
}

#[test]
fn expr_cmpstr() {
    expr_test(&["aaa", "<", "bbb", "+", "1"], "2\n");
    expr_test(&["aaa", ">", "bbb", "+", "1"], "1\n");
    expr_test(&["aaa", "<=", "bbb", "+", "1"], "2\n");
    expr_test(&["aaa", ">=", "bbb", "+", "1"], "1\n");
    expr_test(&["aaa", "=", "bbb", "+", "1"], "1\n");
    expr_test(&["aaa", "!=", "bbb", "+", "1"], "2\n");
}
