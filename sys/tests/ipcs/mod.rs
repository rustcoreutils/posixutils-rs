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

fn run_ipcs_test(args: Vec<&str>, expected_exit_code: i32, check_fn: fn(&TestPlan, &Output)) {
    let plan = TestPlan {
        cmd: "ipcs".to_string(),
        args: args.iter().map(|&s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    };

    run_test_with_checker(plan, check_fn);
}

fn check_output_contains_ipc_status(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    // POSIX requires the first line to be "IPC status from <source> as of <date>"
    assert!(
        stdout.contains("IPC status from"),
        "Expected 'IPC status from' in output, got: {}",
        stdout
    );
}

fn check_output_contains_queues_section(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should contain either message queue info or "not in system" message
    assert!(
        stdout.contains("Message Queue") || stdout.contains("message queue"),
        "Expected message queue section in output, got: {}",
        stdout
    );
}

fn check_output_contains_shm_section(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should contain either shared memory info or "not in system" message
    assert!(
        stdout.contains("Shared Memory") || stdout.contains("shared memory"),
        "Expected shared memory section in output, got: {}",
        stdout
    );
}

fn check_output_contains_sem_section(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should contain either semaphore info or "not in system" message
    assert!(
        stdout.contains("Semaphore") || stdout.contains("semaphore"),
        "Expected semaphore section in output, got: {}",
        stdout
    );
}

fn check_output_nonempty(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(!stdout.is_empty(), "Expected non-empty output");
}

#[test]
fn ipcs_no_args() {
    // Running ipcs without arguments should show all facilities
    run_ipcs_test(vec![], 0, check_output_contains_ipc_status);
}

#[test]
fn ipcs_message_queues_only() {
    run_ipcs_test(vec!["-q"], 0, check_output_contains_queues_section);
}

#[test]
fn ipcs_shared_memory_only() {
    run_ipcs_test(vec!["-m"], 0, check_output_contains_shm_section);
}

#[test]
fn ipcs_semaphores_only() {
    run_ipcs_test(vec!["-s"], 0, check_output_contains_sem_section);
}

#[test]
fn ipcs_all_options() {
    // -a enables all print options (-b, -c, -o, -p, -t)
    run_ipcs_test(vec!["-a"], 0, check_output_nonempty);
}

#[test]
fn ipcs_max_size_option() {
    run_ipcs_test(vec!["-b"], 0, check_output_nonempty);
}

#[test]
fn ipcs_creator_option() {
    run_ipcs_test(vec!["-c"], 0, check_output_nonempty);
}

#[test]
fn ipcs_outstanding_option() {
    run_ipcs_test(vec!["-o"], 0, check_output_nonempty);
}

#[test]
fn ipcs_pid_option() {
    run_ipcs_test(vec!["-p"], 0, check_output_nonempty);
}

#[test]
fn ipcs_time_option() {
    run_ipcs_test(vec!["-t"], 0, check_output_nonempty);
}

#[test]
fn ipcs_combined_facility_options() {
    // Combine multiple facility options
    run_ipcs_test(vec!["-q", "-m"], 0, check_output_nonempty);
}

#[test]
fn ipcs_combined_print_options() {
    // Combine multiple print options
    run_ipcs_test(vec!["-b", "-c", "-t"], 0, check_output_nonempty);
}
