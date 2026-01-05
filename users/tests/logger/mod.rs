//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test};

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
