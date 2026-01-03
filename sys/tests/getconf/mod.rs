//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};
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

fn check_output_is_nonnegative_integer(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let value: i64 = stdout.trim().parse().expect("Output is not a valid number");
    assert!(
        value >= 0,
        "Expected a non-negative integer, but got '{}'",
        value
    );
}

fn check_output_is_integer_or_undefined(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let trimmed = stdout.trim();
    if trimmed == "undefined" {
        return; // Valid output
    }
    let _value: i64 = trimmed
        .parse()
        .expect("Output is neither a valid number nor 'undefined'");
}

fn check_has_stderr(_: &TestPlan, output: &Output) {
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.is_empty(),
        "Expected error message on stderr, but got empty"
    );
}

fn check_output_is_nonempty_string(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.trim().is_empty(),
        "Expected non-empty string output"
    );
}

// ============================================
// Sysconf variable tests
// ============================================

#[test]
fn sysconf_arg_max() {
    run_getconf_test(vec!["ARG_MAX"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_ngroups_max() {
    run_getconf_test(vec!["NGROUPS_MAX"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_open_max() {
    run_getconf_test(vec!["OPEN_MAX"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_page_size() {
    run_getconf_test(vec!["PAGE_SIZE"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_pagesize() {
    run_getconf_test(vec!["PAGESIZE"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_posix_version() {
    run_getconf_test(vec!["_POSIX_VERSION"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_child_max() {
    run_getconf_test(vec!["CHILD_MAX"], 0, check_output_is_integer_or_undefined);
}

#[test]
fn sysconf_host_name_max() {
    run_getconf_test(vec!["HOST_NAME_MAX"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_login_name_max() {
    run_getconf_test(vec!["LOGIN_NAME_MAX"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_line_max() {
    run_getconf_test(vec!["LINE_MAX"], 0, check_output_is_positive_integer);
}

// Test POSIX2_* compatibility aliases (required by POSIX for backwards compat)
#[test]
fn sysconf_posix2_version() {
    run_getconf_test(
        vec!["POSIX2_VERSION"],
        0,
        check_output_is_integer_or_undefined,
    );
}

#[test]
fn sysconf_posix2_c_bind() {
    run_getconf_test(
        vec!["POSIX2_C_BIND"],
        0,
        check_output_is_integer_or_undefined,
    );
}

#[test]
fn sysconf_posix2_line_max() {
    run_getconf_test(vec!["POSIX2_LINE_MAX"], 0, check_output_is_positive_integer);
}

// Test _SC_ prefixed names
#[test]
fn sysconf_sc_arg_max() {
    run_getconf_test(vec!["_SC_ARG_MAX"], 0, check_output_is_positive_integer);
}

// ============================================
// Pathconf variable tests
// ============================================

#[test]
fn pathconf_link_max() {
    run_getconf_test(vec!["LINK_MAX", "/"], 0, check_output_is_positive_integer);
}

#[test]
fn pathconf_name_max() {
    run_getconf_test(vec!["NAME_MAX", "/"], 0, check_output_is_positive_integer);
}

#[test]
fn pathconf_path_max() {
    run_getconf_test(vec!["PATH_MAX", "/"], 0, check_output_is_positive_integer);
}

#[test]
fn pathconf_pipe_buf() {
    run_getconf_test(vec!["PIPE_BUF", "/"], 0, check_output_is_positive_integer);
}

#[test]
fn pathconf_chown_restricted() {
    run_getconf_test(
        vec!["CHOWN_RESTRICTED", "/"],
        0,
        check_output_is_nonnegative_integer,
    );
}

#[test]
fn pathconf_no_trunc() {
    run_getconf_test(
        vec!["NO_TRUNC", "/"],
        0,
        check_output_is_nonnegative_integer,
    );
}

#[test]
fn pathconf_filesizebits() {
    run_getconf_test(
        vec!["FILESIZEBITS", "/"],
        0,
        check_output_is_integer_or_undefined,
    );
}

#[test]
fn pathconf_tmp_dir() {
    run_getconf_test(
        vec!["NAME_MAX", "/tmp"],
        0,
        check_output_is_positive_integer,
    );
}

// ============================================
// Confstr variable tests
// ============================================

#[test]
fn confstr_path() {
    run_getconf_test(vec!["PATH"], 0, check_output_is_nonempty_string);
}

#[cfg(target_os = "macos")]
#[test]
fn confstr_darwin_user_dir() {
    run_getconf_test(vec!["DARWIN_USER_DIR"], 0, check_output_is_nonempty_string);
}

#[cfg(target_os = "linux")]
#[test]
fn confstr_gnu_libc_version() {
    run_getconf_test(vec!["GNU_LIBC_VERSION"], 0, check_output_is_nonempty_string);
}

// ============================================
// Error handling tests
// ============================================

#[test]
fn error_invalid_variable() {
    run_getconf_test(
        vec!["INVALID_NONEXISTENT_VARIABLE_XYZ"],
        1,
        check_has_stderr,
    );
}

#[test]
fn error_invalid_pathname() {
    run_getconf_test(
        vec!["NAME_MAX", "/nonexistent/path/that/does/not/exist"],
        1,
        check_has_stderr,
    );
}

// ============================================
// -v specification tests
// ============================================

#[test]
fn v_specification_valid_posix_v7() {
    // Valid specification should be accepted (as no-op)
    run_getconf_test(
        vec!["-v", "POSIX_V7_LP64_OFF64", "ARG_MAX"],
        0,
        check_output_is_positive_integer,
    );
}

#[test]
fn v_specification_valid_posix_v6() {
    run_getconf_test(
        vec!["-v", "POSIX_V6_LP64_OFF64", "ARG_MAX"],
        0,
        check_output_is_positive_integer,
    );
}

#[test]
fn v_specification_invalid() {
    run_getconf_test(vec!["-v", "INVALID_SPEC", "ARG_MAX"], 1, check_has_stderr);
}

#[test]
fn v_specification_with_pathconf() {
    run_getconf_test(
        vec!["-v", "POSIX_V7_LP64_OFF64", "NAME_MAX", "/"],
        0,
        check_output_is_positive_integer,
    );
}

// ============================================
// Output format tests
// ============================================

#[test]
fn output_ends_with_newline() {
    let plan = TestPlan {
        cmd: "getconf".to_string(),
        args: vec!["ARG_MAX".to_string()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };

    run_test_with_checker(plan, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.ends_with('\n'),
            "Output should end with newline, got: {:?}",
            stdout
        );
    });
}
