//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
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

// Regression for the #D2 follow-up (Copilot): a format whose output exceeds
// the internal 64 KiB strftime buffer must be reported as an error, not
// silently truncated to a bare newline.
#[test]
fn test_format_exceeds_buffer_errors() {
    let huge = format!("+{}", "x".repeat(70_000));
    run_test_with_checker_and_env(date_plan(&[&huge]), &[("TZ", "UTC")], |_, output| {
        assert!(
            !output.status.success(),
            "an over-long format should exit non-zero, got success"
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("exceeds internal buffer limit"),
            "expected buffer-limit diagnostic, got stderr: {stderr}"
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

// #D2: unusual `+format` operands. A bare `+` is an empty format — it must
// print just the terminating newline, not error or echo the `+`. The suite only
// ever fed well-formed formats.
#[test]
fn test_format_empty_prints_only_a_newline() {
    let mut plan = date_plan(&["+"]);
    plan.expected_out = String::from("\n");
    run_test_with_env(plan, &[("TZ", "UTC0")]);
}

// `%%` is a literal percent; text outside conversions passes through verbatim.
#[test]
fn test_format_literal_text_and_percent() {
    let mut plan = date_plan(&["+%%"]);
    plan.expected_out = String::from("%\n");
    run_test_with_env(plan, &[("TZ", "UTC0")]);

    let mut plan = date_plan(&["+no conversions here"]);
    plan.expected_out = String::from("no conversions here\n");
    run_test_with_env(plan, &[("TZ", "UTC0")]);
}

// `%n` and `%t` are a <newline> and a <tab> (spec 91600-91603), and a
// conversion may be embedded in surrounding literal text.
#[test]
fn test_format_newline_tab_and_embedded_conversion() {
    let mut plan = date_plan(&["+%n"]);
    plan.expected_out = String::from("\n\n");
    run_test_with_env(plan, &[("TZ", "UTC0")]);

    let mut plan = date_plan(&["+%t"]);
    plan.expected_out = String::from("\t\n");
    run_test_with_env(plan, &[("TZ", "UTC0")]);

    // The year is the one field that is stable enough to pin exactly.
    run_test_with_checker_and_env(
        date_plan(&["+start[%Y]end"]),
        &[("TZ", "UTC0")],
        |_, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.starts_with("start[") && stdout.trim_end().ends_with("]end"),
                "literal text around a conversion must pass through, got {stdout:?}"
            );
        },
    );
}

// #D4: the `-u` *set* form. Setting the clock needs privilege, so an
// unprivileged run must fail cleanly with a diagnostic and a non-zero status —
// not panic, and not silently succeed. This exercises the set-time branch,
// which no test reached before.
#[test]
fn test_utc_set_form_fails_cleanly_without_privilege() {
    // Root would actually set the system clock; never do that in a test.
    if unsafe { libc::geteuid() } == 0 {
        return;
    }

    run_test_with_checker_and_env(
        date_plan(&["-u", "010203042026"]),
        &[("TZ", "UTC0")],
        |_, output| {
            assert_eq!(
                output.status.code(),
                Some(1),
                "an unprivileged set must exit non-zero"
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("failed to set time"),
                "a diagnostic is required, got {stderr:?}"
            );
            assert!(
                output.stdout.is_empty(),
                "the set form must not write to stdout, got {:?}",
                String::from_utf8_lossy(&output.stdout)
            );
        },
    );
}
