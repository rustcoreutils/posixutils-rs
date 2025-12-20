//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker_and_env, run_test_with_env, TestPlan};

/// Test that lp fails when no destination is specified
#[test]
fn lp_no_destination_error() {
    // Use empty strings to clear the environment variables in the subprocess
    run_test_with_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from("lp: no destination specified\n"),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
    );
}

/// Test that lp fails with invalid URI
#[test]
fn lp_invalid_uri_error() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec!["-d".to_string(), "not-a-uri".to_string()],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("invalid destination URI"),
                "Expected invalid URI error, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test that -m option is accepted (stub implementation)
#[test]
fn lp_m_option_accepted() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-m".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // -m should be accepted; any failure should be printer error, not option error
            assert!(
                !stderr.contains("-m option not supported"),
                "Expected -m to be accepted, but got: {}",
                stderr
            );
            assert!(
                stderr.contains("printer error"),
                "Expected printer error (no printer available), got: {}",
                stderr
            );
        },
    );
}

/// Test that -w option is accepted (stub implementation)
#[test]
fn lp_w_option_accepted() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-w".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // -w should be accepted; any failure should be printer error, not option error
            assert!(
                !stderr.contains("-w option not supported"),
                "Expected -w to be accepted, but got: {}",
                stderr
            );
            assert!(
                stderr.contains("printer error"),
                "Expected printer error (no printer available), got: {}",
                stderr
            );
        },
    );
}

/// Test that lp fails when file does not exist
#[test]
fn lp_file_not_found() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
                "/nonexistent/file/path.txt".to_string(),
            ],
            stdin_data: String::from(""),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("cannot open") && stderr.contains("/nonexistent/file/path.txt"),
                "Expected cannot open error, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test LPDEST environment variable is used when -d is not specified
#[test]
fn lp_lpdest_env_used() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", "not-ipp-uri"), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Should fail because the URI from LPDEST is not valid ipp://
            assert!(
                stderr.contains("invalid destination URI"),
                "Expected invalid URI error from LPDEST, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test PRINTER environment variable is used when -d and LPDEST are not set
#[test]
fn lp_printer_env_used() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "not-ipp-uri")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Should fail because the URI from PRINTER is not valid ipp://
            assert!(
                stderr.contains("invalid destination URI"),
                "Expected invalid URI error from PRINTER, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test that -d takes precedence over LPDEST
#[test]
fn lp_d_overrides_lpdest() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec!["-d".to_string(), "not-a-uri".to_string()],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[
            ("LPDEST", "ipp://should-not-be-used/ipp/print"),
            ("PRINTER", ""),
        ],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Should fail with the -d value, not the LPDEST value
            assert!(
                stderr.contains("invalid destination URI"),
                "Expected invalid URI error from -d, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}
