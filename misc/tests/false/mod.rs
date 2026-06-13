//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};

fn truefalse_test(cmd: &str, expected_exit_code: i32) {
    truefalse_test_args(cmd, &[], expected_exit_code);
}

fn truefalse_test_args(cmd: &str, args: &[&str], expected_exit_code: i32) {
    run_test(TestPlan {
        cmd: cmd.to_string(),
        args: args.iter().map(|s| String::from(*s)).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    });
}

#[test]
fn false_exit_code() {
    truefalse_test("false", 1);
}

#[test]
fn false_ignores_arguments() {
    // POSIX: false exits non-zero (1..=125) regardless of arguments.
    truefalse_test_args("false", &["--"], 1);
    truefalse_test_args("false", &["foo"], 1);
}
