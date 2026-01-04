//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_base, run_test_with_checker, TestPlan};
use std::process::Output;

// Exit code constants
const EXIT_SUCCESS: i32 = 0;

// Field count constants (POSIX uname output)
const FIELD_COUNT_TWO: usize = 2;
const FIELD_COUNT_THREE: usize = 3;
const FIELD_COUNT_ALL: usize = 5; // sysname, nodename, release, version, machine
const LINE_COUNT_ONE: usize = 1;

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

// Helper to run uname and get raw output for comparison tests
fn get_uname_output(args: &[&str]) -> String {
    let str_args: Vec<String> = args.iter().map(|&s| s.to_string()).collect();
    let output = run_test_base("uname", &str_args, &[]);
    String::from_utf8_lossy(&output.stdout).to_string()
}

// ============================================
// Checker functions (cross-platform, verify structure not values)
// ============================================

fn check_output_nonempty_single_line(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(!stdout.trim().is_empty(), "Expected non-empty output");
    assert_eq!(
        stdout.lines().count(),
        LINE_COUNT_ONE,
        "Expected single line output"
    );
}

fn check_output_has_five_fields(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let fields: Vec<&str> = stdout.trim().split_whitespace().collect();
    // -a produces: sysname nodename release version machine
    // Note: version field may contain spaces, so we check at least 5 parts
    assert!(
        fields.len() >= FIELD_COUNT_ALL,
        "Expected at least {} fields with -a, got {}",
        FIELD_COUNT_ALL,
        fields.len()
    );
}

fn check_output_has_two_fields(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let fields: Vec<&str> = stdout.trim().split_whitespace().collect();
    assert!(
        fields.len() >= FIELD_COUNT_TWO,
        "Expected at least {} fields, got {}",
        FIELD_COUNT_TWO,
        fields.len()
    );
}

fn check_output_has_three_fields(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let fields: Vec<&str> = stdout.trim().split_whitespace().collect();
    assert!(
        fields.len() >= FIELD_COUNT_THREE,
        "Expected at least {} fields, got {}",
        FIELD_COUNT_THREE,
        fields.len()
    );
}

fn check_output_ends_with_newline(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.ends_with('\n'),
        "Output should end with newline, got: {:?}",
        stdout
    );
}

fn check_no_trailing_spaces(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Remove trailing newline and check for trailing spaces
    let without_newline = stdout.trim_end_matches('\n');
    assert!(
        !without_newline.ends_with(' '),
        "Output should not have trailing spaces before newline, got: {:?}",
        stdout
    );
}

// ============================================
// Basic option tests (default behavior)
// ============================================

#[test]
fn uname_no_args() {
    // Default is -s (sysname)
    run_uname_test(vec![], EXIT_SUCCESS, check_output_nonempty_single_line);
}

#[test]
fn uname_sysname() {
    run_uname_test(vec!["-s"], EXIT_SUCCESS, check_output_nonempty_single_line);
}

#[test]
fn uname_nodename() {
    run_uname_test(vec!["-n"], EXIT_SUCCESS, check_output_nonempty_single_line);
}

#[test]
fn uname_release() {
    run_uname_test(vec!["-r"], EXIT_SUCCESS, check_output_nonempty_single_line);
}

#[test]
fn uname_version() {
    run_uname_test(vec!["-v"], EXIT_SUCCESS, check_output_nonempty_single_line);
}

#[test]
fn uname_machine() {
    run_uname_test(vec!["-m"], EXIT_SUCCESS, check_output_nonempty_single_line);
}

#[test]
fn uname_all() {
    run_uname_test(vec!["-a"], EXIT_SUCCESS, check_output_has_five_fields);
}

#[test]
fn uname_combined_options() {
    // -sr should produce 2 fields (sysname and release)
    run_uname_test(vec!["-s", "-r"], EXIT_SUCCESS, check_output_has_two_fields);
}

// ============================================
// Long option tests
// ============================================

#[test]
fn uname_long_all() {
    run_uname_test(vec!["--all"], EXIT_SUCCESS, check_output_has_five_fields);
}

#[test]
fn uname_long_machine() {
    run_uname_test(
        vec!["--machine"],
        EXIT_SUCCESS,
        check_output_nonempty_single_line,
    );
}

#[test]
fn uname_long_node() {
    run_uname_test(
        vec!["--node"],
        EXIT_SUCCESS,
        check_output_nonempty_single_line,
    );
}

#[test]
fn uname_long_release() {
    run_uname_test(
        vec!["--release"],
        EXIT_SUCCESS,
        check_output_nonempty_single_line,
    );
}

#[test]
fn uname_long_system() {
    run_uname_test(
        vec!["--system"],
        EXIT_SUCCESS,
        check_output_nonempty_single_line,
    );
}

#[test]
fn uname_long_osversion() {
    run_uname_test(
        vec!["--osversion"],
        EXIT_SUCCESS,
        check_output_nonempty_single_line,
    );
}

// ============================================
// Option ordering tests (POSIX order: s, n, r, v, m)
// ============================================

#[test]
fn uname_order_mn_vs_nm() {
    // Both -m -n and -n -m should produce output in POSIX order: nodename machine
    let output_mn = get_uname_output(&["-m", "-n"]);
    let output_nm = get_uname_output(&["-n", "-m"]);
    assert_eq!(
        output_mn, output_nm,
        "Option order should not affect output order"
    );
}

#[test]
fn uname_order_sr_vs_rs() {
    // Both -s -r and -r -s should produce output in POSIX order: sysname release
    let output_sr = get_uname_output(&["-s", "-r"]);
    let output_rs = get_uname_output(&["-r", "-s"]);
    assert_eq!(
        output_sr, output_rs,
        "Option order should not affect output order"
    );
}

#[test]
fn uname_order_reverse_all() {
    // -m -v -r -n -s should produce same output as -a (order: s, n, r, v, m)
    let output_reverse = get_uname_output(&["-m", "-v", "-r", "-n", "-s"]);
    let output_all = get_uname_output(&["-a"]);
    assert_eq!(
        output_reverse, output_all,
        "Reverse order options should produce same output as -a"
    );
}

// ============================================
// Equivalence tests
// ============================================

#[test]
fn uname_all_vs_combined() {
    // -a output should equal -s -n -r -v -m output
    let output_all = get_uname_output(&["-a"]);
    let output_combined = get_uname_output(&["-s", "-n", "-r", "-v", "-m"]);
    assert_eq!(
        output_all, output_combined,
        "-a should equal -s -n -r -v -m"
    );
}

#[test]
fn uname_combined_short() {
    // -snrvm as single argument should work
    let output_combined = get_uname_output(&["-snrvm"]);
    let output_all = get_uname_output(&["-a"]);
    assert_eq!(output_combined, output_all, "-snrvm should equal -a");
}

#[test]
fn uname_short_vs_long() {
    // Short and long options should produce same output
    let short = get_uname_output(&["-s"]);
    let long = get_uname_output(&["--system"]);
    assert_eq!(short, long, "-s should equal --system");
}

#[test]
fn uname_default_vs_s() {
    // No args should equal -s
    let no_args = get_uname_output(&[]);
    let with_s = get_uname_output(&["-s"]);
    assert_eq!(no_args, with_s, "Default (no args) should equal -s");
}

// ============================================
// Various combinations
// ============================================

#[test]
fn uname_sn() {
    // -s -n should produce 2 fields (sysname nodename)
    run_uname_test(vec!["-s", "-n"], EXIT_SUCCESS, check_output_has_two_fields);
}

#[test]
fn uname_rv() {
    // -r -v should produce 2 fields (release version)
    run_uname_test(vec!["-r", "-v"], EXIT_SUCCESS, check_output_has_two_fields);
}

#[test]
fn uname_mrs() {
    // -m -r -s should produce 3 fields (sysname release machine)
    run_uname_test(
        vec!["-m", "-r", "-s"],
        EXIT_SUCCESS,
        check_output_has_three_fields,
    );
}

#[test]
fn uname_nrv() {
    // -n -r -v should produce 3 fields (nodename release version)
    run_uname_test(
        vec!["-n", "-r", "-v"],
        EXIT_SUCCESS,
        check_output_has_three_fields,
    );
}

// ============================================
// Output format tests
// ============================================

#[test]
fn uname_output_ends_with_newline() {
    run_uname_test(vec!["-s"], EXIT_SUCCESS, check_output_ends_with_newline);
}

#[test]
fn uname_all_output_ends_with_newline() {
    run_uname_test(vec!["-a"], EXIT_SUCCESS, check_output_ends_with_newline);
}

#[test]
fn uname_no_trailing_spaces() {
    run_uname_test(vec!["-s"], EXIT_SUCCESS, check_no_trailing_spaces);
}

#[test]
fn uname_all_no_trailing_spaces() {
    run_uname_test(vec!["-a"], EXIT_SUCCESS, check_no_trailing_spaces);
}

// ============================================
// Mixed short and long options
// ============================================

#[test]
fn uname_mixed_short_long() {
    // Mix of short and long options should work
    run_uname_test(
        vec!["-s", "--release"],
        EXIT_SUCCESS,
        check_output_has_two_fields,
    );
}

#[test]
fn uname_mixed_long_short() {
    // Mix of long and short options should work
    run_uname_test(
        vec!["--system", "-r"],
        EXIT_SUCCESS,
        check_output_has_two_fields,
    );
}
