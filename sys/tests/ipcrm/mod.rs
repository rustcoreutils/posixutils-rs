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

fn run_ipcrm_test(args: Vec<&str>, expected_exit_code: i32, check_fn: fn(&TestPlan, &Output)) {
    let plan = TestPlan {
        cmd: "ipcrm".to_string(),
        args: args.iter().map(|&s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    };

    run_test_with_checker(plan, check_fn);
}

fn check_empty_stdout(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.is_empty(),
        "Expected no stdout output, got: {}",
        stdout
    );
}

fn check_error_on_stderr(_: &TestPlan, output: &Output) {
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.is_empty(),
        "Expected error message on stderr, got none"
    );
}

#[test]
fn ipcrm_no_args() {
    // Running ipcrm without arguments should succeed with no output
    // (no IPC objects to remove is not an error)
    run_ipcrm_test(vec![], 0, check_empty_stdout);
}

#[test]
fn ipcrm_invalid_semid() {
    // Removing a non-existent semaphore ID should fail
    // Using a very high ID that almost certainly doesn't exist
    run_ipcrm_test(vec!["-s", "999999"], 1, check_error_on_stderr);
}

#[test]
fn ipcrm_invalid_shmid() {
    // Removing a non-existent shared memory ID should fail
    run_ipcrm_test(vec!["-m", "999999"], 1, check_error_on_stderr);
}

#[cfg(not(target_os = "macos"))]
#[test]
fn ipcrm_invalid_msgid() {
    // Removing a non-existent message queue ID should fail (Linux only)
    run_ipcrm_test(vec!["-q", "999999"], 1, check_error_on_stderr);
}

#[test]
fn ipcrm_invalid_semkey() {
    // Removing a non-existent semaphore by key should fail
    run_ipcrm_test(vec!["-S", "999999"], 1, check_error_on_stderr);
}

#[test]
fn ipcrm_invalid_shmkey() {
    // Removing a non-existent shared memory by key should fail
    run_ipcrm_test(vec!["-M", "999999"], 1, check_error_on_stderr);
}

#[cfg(not(target_os = "macos"))]
#[test]
fn ipcrm_invalid_msgkey() {
    // Removing a non-existent message queue by key should fail (Linux only)
    run_ipcrm_test(vec!["-Q", "999999"], 1, check_error_on_stderr);
}

#[test]
fn ipcrm_multiple_invalid_ids() {
    // Multiple invalid IDs should all fail
    run_ipcrm_test(
        vec!["-s", "999998", "-s", "999999"],
        1,
        check_error_on_stderr,
    );
}

#[test]
fn ipcrm_mixed_types() {
    // Mix of invalid semaphore and shared memory IDs
    run_ipcrm_test(
        vec!["-s", "999999", "-m", "999999"],
        1,
        check_error_on_stderr,
    );
}
