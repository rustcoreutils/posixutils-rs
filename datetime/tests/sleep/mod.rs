//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};

fn sleep_plan(args: &[&str], expected_exit_code: i32) -> TestPlan {
    TestPlan {
        cmd: String::from("sleep"),
        args: args.iter().map(|s| String::from(*s)).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    }
}

// Regression for #S1: POSIX's `time` operand is a non-negative integer, so
// `sleep 0` must succeed (and return promptly). The pre-fix clap range
// rejected it.
#[test]
fn test_sleep_zero_succeeds() {
    run_test_with_checker(sleep_plan(&["0"], 0), |_, output| {
        assert!(output.status.success(), "`sleep 0` should exit 0");
        assert!(output.stdout.is_empty());
    });
}

#[test]
fn test_sleep_one_succeeds() {
    run_test_with_checker(sleep_plan(&["1"], 0), |_, output| {
        assert!(output.status.success(), "`sleep 1` should exit 0");
    });
}

#[test]
fn test_sleep_non_numeric_fails() {
    run_test_with_checker(sleep_plan(&["abc"], 2), |_, output| {
        assert!(!output.status.success(), "non-numeric operand should fail");
    });
}

#[test]
fn test_sleep_negative_fails() {
    run_test_with_checker(sleep_plan(&["-1"], 2), |_, output| {
        assert!(!output.status.success(), "negative operand should fail");
    });
}
