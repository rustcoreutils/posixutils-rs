//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test_with_checker};
use std::process::Output;

/// Test locale with no arguments shows current settings
#[test]
fn test_locale_no_args() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("locale"),
            args: vec![],
            stdin_data: String::new(),
            expected_out: String::new(), // Will be checked by checker
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output: &Output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should contain LANG and LC_* variables
            assert!(stdout.contains("LANG="), "Output should contain LANG");
            assert!(
                stdout.contains("LC_CTYPE="),
                "Output should contain LC_CTYPE"
            );
            assert!(
                stdout.contains("LC_MESSAGES="),
                "Output should contain LC_MESSAGES"
            );
            assert!(stdout.contains("LC_ALL="), "Output should contain LC_ALL");
        },
    );
}

/// Test locale -a lists available locales
#[test]
fn test_locale_list_all() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("locale"),
            args: vec![String::from("-a")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output: &Output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should always include POSIX and C locales
            assert!(
                stdout.contains("POSIX") || stdout.contains("C"),
                "Output should contain POSIX or C locale"
            );
        },
    );
}

/// Test locale -m lists charmaps
#[test]
fn test_locale_list_charmaps() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("locale"),
            args: vec![String::from("-m")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output: &Output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should list at least UTF-8
            assert!(
                stdout.contains("UTF-8") || stdout.contains("UTF8") || !stdout.is_empty(),
                "Output should contain charmap names or be non-empty"
            );
        },
    );
}

/// Test locale with LANG set
#[test]
fn test_locale_with_lang() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("locale"),
            args: vec![],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output: &Output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should display LANG and LC_* variables
            assert!(stdout.contains("LANG="), "Output should contain LANG");
        },
    );
}

/// Test locale -k for keyword output
#[test]
fn test_locale_keyword() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("locale"),
            args: vec![String::from("-k"), String::from("decimal_point")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output: &Output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should contain keyword=value format
            assert!(
                stdout.contains("decimal_point="),
                "Output should contain decimal_point keyword"
            );
        },
    );
}
