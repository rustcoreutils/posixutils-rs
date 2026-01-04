//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};

fn cal_test_error(args: &[&str]) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("cal"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_, output| {
            assert!(!output.status.success(), "Expected command to fail");
        },
    );
}

fn cal_test_contains(args: &[&str], checks: &[&str]) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("cal"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            assert!(output.status.success(), "Expected command to succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            for check in checks {
                assert!(
                    stdout.contains(check),
                    "Expected output to contain '{}', got:\n{}",
                    check,
                    stdout
                );
            }
        },
    );
}

fn cal_test_not_contains(args: &[&str], checks: &[&str]) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("cal"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            assert!(output.status.success(), "Expected command to succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            for check in checks {
                assert!(
                    !stdout.contains(check),
                    "Expected output NOT to contain '{}', got:\n{}",
                    check,
                    stdout
                );
            }
        },
    );
}

// ============================================================================
// Basic functionality tests
// ============================================================================

#[test]
fn test_cal_basic_month_year() {
    // January 2024 - known Gregorian date
    cal_test_contains(&["1", "2024"], &["January 2024", "Su Mo Tu We Th Fr Sa"]);
}

#[test]
fn test_cal_full_year() {
    // Display full year
    cal_test_contains(&["2024"], &["January 2024", "December 2024"]);
}

#[test]
fn test_cal_output_header() {
    cal_test_contains(&["1", "2024"], &["Su Mo Tu We Th Fr Sa"]);
}

// ============================================================================
// September 1752 - the calendar transition (CRITICAL)
// ============================================================================

#[test]
fn test_cal_september_1752_has_gap() {
    // September 1752 should have days 1, 2, then 14-30
    // Days 3-13 do not exist
    cal_test_contains(
        &["9", "1752"],
        &["September 1752", " 1 ", " 2 ", "14", "30"],
    );
}

#[test]
fn test_cal_september_1752_no_day_3() {
    // Day 3 should not appear in September 1752
    // We check that " 3 " does not appear (with spaces to avoid matching "13", "23", "30")
    let str_args: Vec<String> = vec!["9".to_string(), "1752".to_string()];

    run_test_with_checker(
        TestPlan {
            cmd: String::from("cal"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Check days 3-13 don't exist as standalone day numbers
            // Skip header lines and check day rows
            let lines: Vec<&str> = stdout.lines().collect();
            for line in lines.iter().skip(2) {
                // Parse day numbers from this line
                for token in line.split_whitespace() {
                    if let Ok(day) = token.parse::<u32>() {
                        assert!(
                            !(3..=13).contains(&day),
                            "Day {} should not exist in September 1752",
                            day
                        );
                    }
                }
            }
        },
    );
}

#[test]
fn test_cal_september_1752_day_count() {
    // September 1752 should have exactly 19 days (1, 2, 14-30)
    let str_args: Vec<String> = vec!["9".to_string(), "1752".to_string()];

    run_test_with_checker(
        TestPlan {
            cmd: String::from("cal"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Count numeric day values (skip month name and day headers)
            let day_count: usize = stdout
                .lines()
                .skip(2)
                .flat_map(|line| line.split_whitespace())
                .filter(|s| s.parse::<u32>().is_ok())
                .count();
            assert_eq!(day_count, 19, "September 1752 should have 19 days");
        },
    );
}

// ============================================================================
// Julian calendar leap year tests (before September 1752)
// ============================================================================

#[test]
fn test_cal_feb_1700_julian_leap() {
    // February 1700 - Julian leap year (divisible by 4)
    // Under Gregorian rules 1700 would NOT be a leap year (div by 100, not 400)
    cal_test_contains(&["2", "1700"], &["29"]);
}

#[test]
fn test_cal_feb_100_julian_leap() {
    // February 100 - Julian leap year
    cal_test_contains(&["2", "100"], &["29"]);
}

#[test]
fn test_cal_feb_1600_julian_leap() {
    // February 1600 - Julian leap year (before transition)
    cal_test_contains(&["2", "1600"], &["29"]);
}

// ============================================================================
// Gregorian calendar leap year tests (after September 1752)
// ============================================================================

#[test]
fn test_cal_feb_1900_gregorian_no_leap() {
    // February 1900 - Gregorian non-leap (divisible by 100, not 400)
    cal_test_not_contains(&["2", "1900"], &["29"]);
}

#[test]
fn test_cal_feb_2000_gregorian_leap() {
    // February 2000 - Gregorian leap year (divisible by 400)
    cal_test_contains(&["2", "2000"], &["29"]);
}

#[test]
fn test_cal_feb_2024_gregorian_leap() {
    // February 2024 - Regular Gregorian leap year
    cal_test_contains(&["2", "2024"], &["29"]);
}

#[test]
fn test_cal_feb_2023_no_leap() {
    // February 2023 - Non-leap year
    cal_test_not_contains(&["2", "2023"], &["29"]);
}

// ============================================================================
// Day-of-week verification tests
// ============================================================================

#[test]
fn test_cal_jan_1_year_1_saturday() {
    // January 1, year 1 should be Saturday (last column)
    let str_args: Vec<String> = vec!["1".to_string(), "1".to_string()];

    run_test_with_checker(
        TestPlan {
            cmd: String::from("cal"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let lines: Vec<&str> = stdout.lines().collect();
            // Day 1 should be on Saturday (last column in first data row)
            if lines.len() > 2 {
                let first_week = lines[2];
                // Check that "1" appears at the end (Saturday position)
                assert!(
                    first_week.trim().ends_with("1"),
                    "Jan 1, year 1 should be Saturday. Got: '{}'",
                    first_week
                );
            }
        },
    );
}

// ============================================================================
// Error handling tests
// ============================================================================

#[test]
fn test_cal_invalid_month_13() {
    cal_test_error(&["13", "2024"]);
}

#[test]
fn test_cal_invalid_month_0() {
    cal_test_error(&["0", "2024"]);
}

#[test]
fn test_cal_invalid_year_0() {
    cal_test_error(&["1", "0"]);
}

#[test]
fn test_cal_invalid_year_10000() {
    cal_test_error(&["1", "10000"]);
}

#[test]
fn test_cal_non_numeric_month() {
    cal_test_error(&["abc", "2024"]);
}

#[test]
fn test_cal_non_numeric_year() {
    cal_test_error(&["1", "xyz"]);
}

// ============================================================================
// Boundary tests
// ============================================================================

#[test]
fn test_cal_year_1_minimum() {
    // Year 1 (minimum valid year)
    cal_test_contains(&["1"], &["January 1", "December 1"]);
}

#[test]
fn test_cal_year_9999_maximum() {
    // Year 9999 (maximum valid year)
    cal_test_contains(&["9999"], &["January 9999", "December 9999"]);
}

// ============================================================================
// Month name tests
// ============================================================================

#[test]
fn test_cal_all_month_names() {
    let months = [
        ("1", "January"),
        ("2", "February"),
        ("3", "March"),
        ("4", "April"),
        ("5", "May"),
        ("6", "June"),
        ("7", "July"),
        ("8", "August"),
        ("9", "September"),
        ("10", "October"),
        ("11", "November"),
        ("12", "December"),
    ];
    for (num, name) in months {
        cal_test_contains(&[num, "2024"], &[name]);
    }
}

// ============================================================================
// Days in month tests
// ============================================================================

#[test]
fn test_cal_31_day_months() {
    // January, March, May, July, August, October, December have 31 days
    for month in ["1", "3", "5", "7", "8", "10", "12"] {
        cal_test_contains(&[month, "2023"], &["31"]);
    }
}

#[test]
fn test_cal_30_day_months() {
    // April, June, September, November have 30 days
    for month in ["4", "6", "9", "11"] {
        cal_test_contains(&[month, "2023"], &["30"]);
        cal_test_not_contains(&[month, "2023"], &["31"]);
    }
}

#[test]
fn test_cal_february_non_leap() {
    // February in non-leap year has 28 days
    cal_test_contains(&["2", "2023"], &["28"]);
    cal_test_not_contains(&["2", "2023"], &["29"]);
}

#[test]
fn test_cal_february_leap() {
    // February in leap year has 29 days
    cal_test_contains(&["2", "2024"], &["29"]);
}
