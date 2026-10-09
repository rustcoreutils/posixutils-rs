//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};

fn sleep_plan(args: &[&str], expected_exit_code: i32) -> TestPlan {
    TestPlan {
        cmd: String::from("sleep"),
        args: args.iter().map(|s| String::from(*s)).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    }
}

// Regression for #S1: POSIX's `time` operand is a non-negative integer, so
// `sleep 0` must succeed (and return promptly). The pre-fix clap range
// rejected it.
#[test]
fn test_sleep_zero_succeeds() {
    run_test_with_checker(sleep_plan(&["0"], 0), |_, output| {
        assert!(output.status.success(), "`sleep 0` should exit 0");
        assert!(output.stdout.is_empty());
    });
}

#[test]
fn test_sleep_one_succeeds() {
    run_test_with_checker(sleep_plan(&["1"], 0), |_, output| {
        assert!(output.status.success(), "`sleep 1` should exit 0");
    });
}

#[test]
fn test_sleep_non_numeric_fails() {
    run_test_with_checker(sleep_plan(&["abc"], 2), |_, output| {
        assert!(!output.status.success(), "non-numeric operand should fail");
    });
}

/// A fractional operand (`0.01`, `.5`, `1.`), as GNU and BSD sleep accept: the sleep lasts at
/// least that long. Anything else that is not a decimal number is refused.
#[test]
fn test_sleep_fraction() {
    for operand in ["0.01", ".05", "0.", "0.000000000001"] {
        run_test_with_checker(sleep_plan(&[operand], 0), |_, output| {
            assert!(output.status.success(), "`sleep {operand}` should exit 0");
            assert!(output.stdout.is_empty() && output.stderr.is_empty());
        });
    }
    let started = std::time::Instant::now();
    run_test_with_checker(sleep_plan(&["0.3"], 0), |_, output| {
        assert!(output.status.success());
    });
    assert!(started.elapsed() >= std::time::Duration::from_millis(300));
}

#[test]
fn test_sleep_rejects_malformed_numbers() {
    for operand in [
        ".", "", "1.2.3", "1e2", "0x10", "1s", "inf", " 1", "1,5", "+1",
    ] {
        run_test_with_checker(sleep_plan(&[operand], 2), |_, output| {
            let err = String::from_utf8_lossy(&output.stderr);
            assert!(
                err.contains(&format!("invalid time interval '{operand}'")),
                "`sleep {operand:?}`: {err}"
            );
            assert_eq!(output.status.code(), Some(2));
        });
    }
}

#[test]
fn test_sleep_negative_fails() {
    run_test_with_checker(sleep_plan(&["-1"], 2), |_, output| {
        assert!(!output.status.success(), "negative operand should fail");
    });
}
