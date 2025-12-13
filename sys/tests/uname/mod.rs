//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};
use std::process::Output;

fn run_uname_test(args: Vec<&str>, expected_exit_code: i32, check_fn: fn(&TestPlan, &Output)) {
    let plan = TestPlan {
        cmd: "uname".to_string(),
        args: args.iter().map(|&s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    };
    run_test_with_checker(plan, check_fn);
}

// Checker functions (cross-platform, verify structure not values)
fn check_output_nonempty_single_line(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(!stdout.trim().is_empty(), "Expected non-empty output");
    assert_eq!(stdout.lines().count(), 1, "Expected single line output");
}

fn check_output_has_five_fields(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let fields: Vec<&str> = stdout.trim().split_whitespace().collect();
    // -a produces: sysname nodename release version machine
    // Note: version field may contain spaces, so we check at least 5 parts
    assert!(
        fields.len() >= 5,
        "Expected at least 5 fields with -a, got {}",
        fields.len()
    );
}

fn check_output_has_two_fields(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let fields: Vec<&str> = stdout.trim().split_whitespace().collect();
    assert!(
        fields.len() >= 2,
        "Expected at least 2 fields, got {}",
        fields.len()
    );
}

#[test]
fn uname_no_args() {
    // Default is -s (sysname)
    run_uname_test(vec![], 0, check_output_nonempty_single_line);
}

#[test]
fn uname_sysname() {
    run_uname_test(vec!["-s"], 0, check_output_nonempty_single_line);
}

#[test]
fn uname_nodename() {
    run_uname_test(vec!["-n"], 0, check_output_nonempty_single_line);
}

#[test]
fn uname_release() {
    run_uname_test(vec!["-r"], 0, check_output_nonempty_single_line);
}

#[test]
fn uname_version() {
    run_uname_test(vec!["-v"], 0, check_output_nonempty_single_line);
}

#[test]
fn uname_machine() {
    run_uname_test(vec!["-m"], 0, check_output_nonempty_single_line);
}

#[test]
fn uname_all() {
    run_uname_test(vec!["-a"], 0, check_output_has_five_fields);
}

#[test]
fn uname_combined_options() {
    // -sr should produce 2 fields (sysname and release)
    run_uname_test(vec!["-s", "-r"], 0, check_output_has_two_fields);
}
