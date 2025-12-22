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

fn run_who_test(args: Vec<&str>, expected_exit_code: i32, check_fn: fn(&TestPlan, &Output)) {
    let plan = TestPlan {
        cmd: "who".to_string(),
        args: args.iter().map(|&s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    };
    run_test_with_checker(plan, check_fn);
}

// Checker functions
fn check_exit_success(_: &TestPlan, output: &Output) {
    assert!(output.status.success(), "Expected successful exit");
}

fn check_has_column_headings(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.lines().collect();

    // Should have at least a header line
    assert!(!lines.is_empty(), "Expected output with headings");

    // First line should contain column headers
    let first_line = lines[0].to_uppercase();
    assert!(
        first_line.contains("NAME") || first_line.contains("LINE") || first_line.contains("TIME"),
        "Expected header line to contain NAME, LINE, or TIME"
    );
}

fn check_summary_format(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.lines().collect();

    // Summary format should have at least the "# users=" line
    assert!(
        lines.iter().any(|line| line.contains("# users=")),
        "Expected '# users=' line in summary output"
    );
}

fn check_output_has_terminal_state(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);

    // With -T, output should include terminal state characters (+, -, ?)
    // Skip if no output (no logged in users)
    if !stdout.trim().is_empty() {
        let lines: Vec<&str> = stdout.lines().filter(|l| !l.trim().is_empty()).collect();
        if !lines.is_empty() {
            // At least one line should potentially have terminal state indicators
            // The format should be: NAME STATE LINE TIME
            // We can't guarantee specific output, but we can check structure
            for line in lines {
                // Each line should have multiple fields
                let fields: Vec<&str> = line.split_whitespace().collect();
                assert!(fields.len() >= 2, "Expected multiple fields in output");
            }
        }
    }
}

#[test]
fn who_no_args() {
    // Default behavior: show logged in users
    run_who_test(vec![], 0, check_exit_success);
}

#[test]
fn who_short_format() {
    // -s is the default, explicit test
    run_who_test(vec!["-s"], 0, check_exit_success);
}

#[test]
fn who_heading() {
    // -H should print column headings
    run_who_test(vec!["-H"], 0, check_has_column_headings);
}

#[test]
fn who_summary() {
    // -q should show summary format
    run_who_test(vec!["-q"], 0, check_summary_format);
}

#[test]
fn who_boot() {
    // -b should show boot time
    run_who_test(vec!["-b"], 0, check_exit_success);
}

#[test]
fn who_dead() {
    // -d should show dead processes
    run_who_test(vec!["-d"], 0, check_exit_success);
}

#[test]
fn who_login() {
    // -l should show login processes
    run_who_test(vec!["-l"], 0, check_exit_success);
}

#[test]
fn who_process() {
    // -p should show active processes spawned by init
    run_who_test(vec!["-p"], 0, check_exit_success);
}

#[test]
fn who_runlevel() {
    // -r should show current runlevel
    run_who_test(vec!["-r"], 0, check_exit_success);
}

#[test]
fn who_time() {
    // -t should show last system clock change
    run_who_test(vec!["-t"], 0, check_exit_success);
}

#[test]
fn who_terminals() {
    // -T should show terminal state
    run_who_test(vec!["-T"], 0, check_output_has_terminal_state);
}

#[test]
fn who_users() {
    // -u should show idle time for users
    run_who_test(vec!["-u"], 0, check_exit_success);
}

#[test]
fn who_all() {
    // -a should enable all options
    run_who_test(vec!["-a"], 0, check_exit_success);
}

#[test]
fn who_combined_options() {
    // Test combining options
    run_who_test(vec!["-H", "-b"], 0, check_has_column_headings);
}

#[test]
fn who_current_terminal() {
    // -m should show only current terminal
    run_who_test(vec!["-m"], 0, check_exit_success);
}

#[test]
fn who_userproc() {
    // --userproc should show normal user processes
    run_who_test(vec!["--userproc"], 0, check_exit_success);
}
