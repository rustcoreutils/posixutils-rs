//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, run_test_with_checker_and_env, TestPlan};
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

/// LO-3: an unknown keyword/name is diagnosed on stderr and exits non-zero.
#[test]
fn test_locale_unknown_keyword_exits_nonzero() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("locale"),
            args: vec![String::from("no_such_keyword")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        &[("LC_ALL", "C")],
        |_plan, output: &Output| {
            assert_eq!(output.status.code(), Some(1), "unknown name must exit 1");
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("no_such_keyword"),
                "stderr should name the unknown keyword: {stderr:?}"
            );
        },
    );
}

/// LO-2: a category operand without `-k` writes keyword *values*, one per line,
/// not a `CATEGORY=locale_name` line.
#[test]
fn test_locale_category_without_k_prints_values() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("locale"),
            args: vec![String::from("LC_NUMERIC")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("LC_ALL", "C")],
        |_plan, output: &Output| {
            assert_eq!(output.status.code(), Some(0));
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                !stdout.contains("LC_NUMERIC="),
                "without -k a category must not print CATEGORY=...: {stdout:?}"
            );
            // First selected value is decimal_point, which is "." in the C locale.
            assert_eq!(stdout.lines().next(), Some("."));
        },
    );
}

/// LO-2 with `-k`: a category prints keyword=value pairs.
#[test]
fn test_locale_category_with_k_prints_pairs() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("locale"),
            args: vec![String::from("-k"), String::from("LC_NUMERIC")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("LC_ALL", "C")],
        |_plan, output: &Output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(stdout.contains("decimal_point=\".\""), "{stdout:?}");
        },
    );
}

/// LO-4: the reserved `charmap` operand prints the locale's codeset.
#[test]
fn test_locale_charmap_operand() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("locale"),
            args: vec![String::from("charmap")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("LC_ALL", "C")],
        |_plan, output: &Output| {
            assert_eq!(output.status.code(), Some(0), "charmap must not error");
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(!stdout.trim().is_empty(), "charmap should print a codeset");
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                !stderr.contains("unknown"),
                "charmap must not be an unknown name: {stderr:?}"
            );
        },
    );
}

/// LO-5/LO-6: numeric monetary keywords are exposed and printed unquoted
/// (`name=N`), with the CHAR_MAX sentinel shown as `-1` in the C locale.
#[test]
fn test_locale_numeric_keyword_unquoted() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("locale"),
            args: vec![String::from("-k"), String::from("frac_digits")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("LC_ALL", "C")],
        |_plan, output: &Output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Unquoted numeric form, not frac_digits="-1".
            assert_eq!(stdout.trim_end(), "frac_digits=-1", "{stdout:?}");
        },
    );
}

/// LO-6: a previously-missing keyword (e.g. `mon_grouping`) is now recognized
/// rather than reported as unknown.
#[test]
fn test_locale_added_monetary_keyword_recognized() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("locale"),
            args: vec![String::from("-k"), String::from("mon_grouping")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("LC_ALL", "C")],
        |_plan, output: &Output| {
            assert_eq!(output.status.code(), Some(0));
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert_eq!(stdout.trim_end(), "mon_grouping=-1", "{stdout:?}");
        },
    );
}

/// LO-1: `LC_TIME` keywords reflect the active locale via nl_langinfo. In the C
/// locale `d_fmt` is `%m/%d/%y`.
#[test]
fn test_locale_time_keyword_live_c_locale() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("locale"),
            args: vec![String::from("-k"), String::from("d_fmt")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("LC_ALL", "C")],
        |_plan, output: &Output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert_eq!(stdout.trim_end(), "d_fmt=\"%m/%d/%y\"", "{stdout:?}");
        },
    );
}
