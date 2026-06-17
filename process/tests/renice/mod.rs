//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};

fn renice_exit(args: Vec<&str>, expected_exit: i32) {
    let plan = TestPlan {
        cmd: String::from("renice"),
        args: args.into_iter().map(String::from).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: expected_exit,
    };
    run_test_with_checker(plan, |_, output| {
        assert_eq!(output.status.code(), Some(expected_exit));
    });
}

// PID 0 = the renice process itself; +0 increment is a privilege-free no-op.
// #RN3: ID 0 must be accepted (was rejected as "Invalid ID").
#[test]
fn renice_id_zero_accepted() {
    renice_exit(vec!["-n", "0", "-p", "0"], 0);
}

// #RN1: multiple IDs must be accepted (was: single ID only).
#[test]
fn renice_multiple_ids() {
    renice_exit(vec!["-n", "0", "-p", "0", "0"], 0);
}

// #RN4: increment +20 must be accepted (range was -20..20, exclusive).
#[test]
fn renice_range_plus_twenty() {
    renice_exit(vec!["-n", "20", "-p", "0"], 0);
}

// #RN2: a per-ID failure (nonexistent PID) must not abort; the valid ID is
// still processed and the exit status is non-zero.
#[test]
fn renice_partial_failure_continues() {
    renice_exit(vec!["-n", "0", "-p", "0", "999999"], 1);
}

// A non-numeric process ID is a per-ID error -> non-zero exit.
#[test]
fn renice_invalid_id() {
    renice_exit(vec!["-n", "0", "-p", "abc"], 1);
}

// `-u` with an unknown user name -> non-zero exit (username resolution path).
#[test]
fn renice_unknown_user() {
    renice_exit(vec!["-n", "0", "-u", "nonexistent_user_xyz123"], 1);
}

// `-n` is required.
#[test]
fn renice_missing_increment() {
    renice_exit(vec!["-p", "0"], 2);
}
