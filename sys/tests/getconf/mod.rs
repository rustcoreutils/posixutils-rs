//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test_with_checker};
use std::process::Output;

fn run_getconf_test(args: Vec<&str>, expected_exit_code: i32, check_fn: fn(&TestPlan, &Output)) {
    let plan = TestPlan {
        cmd: "getconf".to_string(),
        args: args.iter().map(|&s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    };

    run_test_with_checker(plan, check_fn);
}

fn check_output_is_positive_integer(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let value: i64 = stdout.trim().parse().expect("Output is not a valid number");
    assert!(
        value > 0,
        "Expected a positive integer, but got '{}'",
        value
    );
}

#[test]
fn sysconf_arg_max() {
    run_getconf_test(vec!["ARG_MAX"], 0, check_output_is_positive_integer);
}

#[test]
fn pathconf_link_max() {
    run_getconf_test(vec!["LINK_MAX", "/"], 0, check_output_is_positive_integer);
}
