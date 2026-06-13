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
fn true_exit_code() {
    truefalse_test("true", 0);
}

#[test]
fn true_ignores_arguments() {
    // POSIX: true must accept and discard a leading "--"; arguments do not
    // change the zero exit status.
    truefalse_test_args("true", &["--"], 0);
    truefalse_test_args("true", &["foo", "bar"], 0);
}
