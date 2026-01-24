//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::process::Output;

use plib::testing::{run_test_base, TestPlan};

fn get_output(plan: TestPlan) -> Output {
    run_test_base(&plan.cmd, &plan.args, plan.stdin_data.as_bytes())
}

fn run_test_time(
    args: &[&str],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    let output = get_output(TestPlan {
        cmd: String::from("time"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });

    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(stderr.contains(expected_error));
}

#[test]
fn simple_test() {
    run_test_time(&["--", "ls", "-l"], "", "User time", 0);
}

#[test]
fn p_test() {
    run_test_time(&["-p", "--", "ls", "-l"], "", "user", 0);
}

#[test]
fn parse_error_test() {
    run_test_time(&[], "", "not provided", 0);
}

#[test]
fn command_error_test() {
    run_test_time(&["-s", "ls", "-l"], "", "unexpected argument '-s' found", 0);
}
