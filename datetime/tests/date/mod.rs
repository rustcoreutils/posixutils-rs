//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker_and_env, run_test_with_env, TestPlan};

fn date_plan(args: &[&str]) -> TestPlan {
    TestPlan {
        cmd: String::from("date"),
        args: args.iter().map(|s| String::from(*s)).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    }
}

#[test]
fn test_tz_utc() {
    let mut plan = date_plan(&["+%Z"]);
    plan.expected_out = String::from("UTC\n");
    run_test_with_env(plan, &[("TZ", "UTC")]);
}

#[test]
fn test_tz_utc_flag() {
    run_test_with_checker_and_env(date_plan(&["-u", "+%Z"]), &[], |_, output| {
        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        let tz = stdout.trim();
        assert!(
            tz == "UTC" || tz == "GMT",
            "Expected UTC or GMT, got: '{}'",
            tz
        );
    });
}

#[test]
fn test_tz_named() {
    run_test_with_checker_and_env(
        date_plan(&["+%Z"]),
        &[("TZ", "America/New_York")],
        |_, output| {
            assert!(output.status.success());
            let stdout = String::from_utf8_lossy(&output.stdout);
            let tz = stdout.trim();
            assert!(
                tz == "EST" || tz == "EDT",
                "Expected EST or EDT, got: '{}'",
                tz
            );
        },
    );
}

#[test]
fn test_format_year() {
    run_test_with_checker_and_env(date_plan(&["+%Y"]), &[("TZ", "UTC")], |_, output| {
        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        let year = stdout.trim();
        assert_eq!(year.len(), 4, "Expected 4-digit year, got: '{}'", year);
        assert!(
            year.chars().all(|c| c.is_ascii_digit()),
            "Expected digits, got: '{}'",
            year
        );
    });
}

#[test]
fn test_format_date() {
    run_test_with_checker_and_env(date_plan(&["+%Y-%m-%d"]), &[("TZ", "UTC")], |_, output| {
        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        let date = stdout.trim();
        // Match NNNN-NN-NN pattern
        let parts: Vec<&str> = date.split('-').collect();
        assert_eq!(parts.len(), 3, "Expected YYYY-MM-DD, got: '{}'", date);
        assert_eq!(parts[0].len(), 4);
        assert_eq!(parts[1].len(), 2);
        assert_eq!(parts[2].len(), 2);
    });
}

#[test]
fn test_default_format_utc() {
    run_test_with_checker_and_env(date_plan(&[]), &[("TZ", "UTC")], |_, output| {
        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("UTC"),
            "Default output with TZ=UTC should contain 'UTC', got: '{}'",
            stdout.trim()
        );
    });
}

#[test]
fn test_locale_abbrev_weekday_and_month_c() {
    run_test_with_checker_and_env(
        date_plan(&["+%a %b"]),
        &[("TZ", "UTC"), ("LANG", "C")],
        |_, output| {
            assert!(output.status.success());
            let stdout = String::from_utf8_lossy(&output.stdout);
            let trimmed = stdout.trim();
            let parts: Vec<&str> = trimmed.split_whitespace().collect();
            assert_eq!(
                parts.len(),
                2,
                "Expected '<weekday> <month>', got: '{}'",
                trimmed
            );
            let weekday = parts[0];
            let month = parts[1];
            let valid_weekdays = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
            let valid_months = [
                "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
            ];
            assert!(
                valid_weekdays.contains(&weekday),
                "Expected weekday abbreviation per C locale, got: '{}'",
                weekday
            );
            assert!(
                valid_months.contains(&month),
                "Expected month abbreviation per C locale, got: '{}'",
                month
            );
        },
    );
}
