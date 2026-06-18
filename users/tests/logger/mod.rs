//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};

fn logger_test(args: &[&str]) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();
    run_test(TestPlan {
        cmd: String::from("logger"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

/// Assert `logger <args>` exits with `code` (priority parsing happens before
/// any syslog connection, so the error cases are deterministic offline).
fn logger_exit(args: &[&str], code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("logger"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: code,
        },
        move |_plan, output| {
            assert_eq!(output.status.code(), Some(code));
        },
    );
}

// POSIX notes that logger is "difficult to test" since the output format
// and location of logged messages is unspecified. We test observable behavior:
// exit codes, no stdout, and no stderr on success.

#[test]
fn test_logger_basic() {
    logger_test(&["test message from posixutils"]);
}

#[test]
fn test_logger_multiple_args() {
    // POSIX: multiple string arguments are concatenated with spaces
    logger_test(&["hello", "world", "from", "logger"]);
}

#[test]
fn test_logger_special_chars() {
    logger_test(&["message with special: !@#$%^&*()_+-=[]{}|;':\",./<>?"]);
}

#[test]
fn test_logger_unicode() {
    logger_test(&["unicode message: \u{1F600} \u{1F4BB} \u{2764}"]);
}

#[test]
fn test_logger_empty_string() {
    // Empty string is valid - logs empty message
    logger_test(&[""]);
}

#[test]
fn test_logger_no_args() {
    // No arguments - logs empty message
    logger_test(&[]);
}

#[test]
fn test_logger_long_message() {
    let long_msg = "a".repeat(1000);
    logger_test(&[&long_msg]);
}

#[test]
fn test_logger_multiword_quoted() {
    // Test that quoted arguments work correctly
    logger_test(&["this is a single argument with spaces"]);
}

#[test]
fn test_logger_newlines() {
    // Test embedded newlines (implementation may handle differently)
    logger_test(&["line1\nline2\nline3"]);
}

// ============================================================================
// Option parsing (#LG1/#LG3/#LG4) — operands are no longer logged verbatim
// ============================================================================

#[test]
fn test_logger_unknown_facility_errors() {
    // -p with an unknown facility is rejected before any syslog connection.
    logger_exit(&["-p", "nosuchfac.info", "hello"], 1);
}

#[test]
fn test_logger_unknown_level_errors() {
    // -p with an unknown level is rejected.
    logger_exit(&["-p", "user.nosuchlevel", "hello"], 1);
}

#[test]
fn test_logger_options_not_logged_as_body() {
    // -t/-p/-i must be parsed as options, not concatenated into the message
    // body. A valid invocation succeeds (exit 0) when syslog is reachable.
    logger_test(&["-i", "-t", "mytag", "-p", "user.info", "hello", "world"]);
}

#[test]
fn test_logger_bare_level_priority() {
    // A bare level (no facility) defaults the facility to user.
    logger_test(&["-p", "warning", "a warning message"]);
}
