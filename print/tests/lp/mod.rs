//
// Copyright (c) 2024-2026 Jeff Garzik
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

/// Test that a destination name containing a path separator is rejected
#[test]
fn lp_invalid_name_rejected() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec!["-d".to_string(), "bad/name".to_string()],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("invalid destination name"),
                "Expected invalid name error, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test that a bare printer name resolves to a localhost IPP URI
#[test]
fn lp_bare_name_resolves_to_localhost() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec!["-d".to_string(), "myprinter".to_string()],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // The bare name is resolved, not rejected; the connection then fails.
            assert!(
                !stderr.contains("invalid destination"),
                "Expected bare name to resolve, got: {}",
                stderr
            );
            assert!(
                stderr.contains("printer error")
                    && stderr.contains("ipp://localhost/printers/myprinter"),
                "Expected printer error against resolved localhost URI, got: {}",
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
        &[("LPDEST", "lpdest-printer"), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // LPDEST is read and resolved to a localhost URI carrying its name.
            assert!(
                stderr.contains("printer error") && stderr.contains("lpdest-printer"),
                "Expected printer error referencing LPDEST name, got: {}",
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
        &[("LPDEST", ""), ("PRINTER", "printer-name")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // PRINTER is read and resolved to a localhost URI carrying its name.
            assert!(
                stderr.contains("printer error") && stderr.contains("printer-name"),
                "Expected printer error referencing PRINTER name, got: {}",
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
            args: vec!["-d".to_string(), "dprinter".to_string()],
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
            // -d wins: the resolved URI carries the -d name, not the LPDEST host.
            assert!(
                stderr.contains("dprinter") && !stderr.contains("should-not-be-used"),
                "Expected -d to take precedence over LPDEST, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test that -c option is accepted (copy mode)
#[test]
fn lp_c_option_accepted() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-c".to_string(),
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
            // -c should be accepted; any failure should be printer error, not option error
            assert!(
                !stderr.contains("error: unexpected argument"),
                "Expected -c to be accepted, but got: {}",
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

/// Test that -n copies with valid value is accepted
#[test]
fn lp_n_copies_accepted() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-n".to_string(),
                "5".to_string(),
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
            // -n 5 should be accepted; any failure should be printer error, not argument error
            assert!(
                !stderr.contains("error: invalid value"),
                "Expected -n 5 to be accepted, but got: {}",
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

/// Test that -n 0 is rejected by clap validation
#[test]
fn lp_n_zero_rejected() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-n".to_string(),
                "0".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 2,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // -n 0 should be rejected with an argument error, not printer error
            assert!(
                stderr.contains("error:") && stderr.contains("0"),
                "Expected argument validation error for -n 0, got: {}",
                stderr
            );
        },
    );
}

/// Test that -n above i32::MAX is rejected by clap validation
#[test]
fn lp_n_copies_overflow_rejected() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-n".to_string(),
                "3000000000".to_string(),
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
            ],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 2,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Above i32::MAX must be rejected with a validation error, not sent.
            assert!(
                stderr.contains("error:"),
                "Expected validation error for -n 3000000000, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(2));
        },
    );
}

/// Test that a malformed -o option (no '=') produces a warning
#[test]
fn lp_o_malformed_warned() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-o".to_string(),
                "noequals".to_string(),
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
            assert!(
                stderr.contains("malformed -o option") && stderr.contains("noequals"),
                "Expected malformed -o warning, got: {}",
                stderr
            );
        },
    );
}

/// Test that -t title option is accepted
#[test]
fn lp_t_title_accepted() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-t".to_string(),
                "My Print Job".to_string(),
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
            // -t should be accepted; any failure should be printer error
            assert!(
                !stderr.contains("error: unexpected argument"),
                "Expected -t to be accepted, but got: {}",
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

/// Test that single -o option is accepted
#[test]
fn lp_o_option_single() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-o".to_string(),
                "media=letter".to_string(),
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
            // -o should be accepted; any failure should be printer error
            assert!(
                !stderr.contains("error: unexpected argument"),
                "Expected -o to be accepted, but got: {}",
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

/// Test that multiple -o options are accepted
#[test]
fn lp_o_option_multiple() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-o".to_string(),
                "media=letter".to_string(),
                "-o".to_string(),
                "sides=two-sided-long-edge".to_string(),
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
            // Multiple -o should be accepted; any failure should be printer error
            assert!(
                !stderr.contains("error: unexpected argument"),
                "Expected multiple -o to be accepted, but got: {}",
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

/// Test that combined options work together
#[test]
fn lp_combined_options() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-c".to_string(),
                "-m".to_string(),
                "-w".to_string(),
                "-s".to_string(),
                "-n".to_string(),
                "3".to_string(),
                "-t".to_string(),
                "Test Title".to_string(),
                "-o".to_string(),
                "media=a4".to_string(),
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
            // All options should be accepted together; failure should be printer error
            assert!(
                !stderr.contains("error: unexpected argument"),
                "Expected all options to be accepted, but got: {}",
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

/// Test that LPDEST takes precedence over PRINTER
#[test]
fn lp_lpdest_over_printer() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", "lpdest-value"), ("PRINTER", "printer-value")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // LPDEST wins: the resolved URI carries the LPDEST name, not PRINTER.
            assert!(
                stderr.contains("lpdest-value") && !stderr.contains("printer-value"),
                "Expected LPDEST to take precedence over PRINTER, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test that empty LPDEST falls back to PRINTER
#[test]
fn lp_empty_lpdest_fallback() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![],
            stdin_data: String::from("test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "printer-fallback")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // LPDEST is empty, so PRINTER is used and resolved to a localhost URI.
            assert!(
                stderr.contains("printer error") && stderr.contains("printer-fallback"),
                "Expected printer error referencing PRINTER fallback name, got: {}",
                stderr
            );
            assert_eq!(output.status.code(), Some(1));
        },
    );
}

/// Test that stdin is handled when '-' is passed as file argument
#[test]
fn lp_stdin_dash_argument() {
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("lp"),
            args: vec![
                "-d".to_string(),
                "ipp://localhost/ipp/print".to_string(),
                "-".to_string(),
            ],
            stdin_data: String::from("stdin test data"),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 1,
        },
        &[("LPDEST", ""), ("PRINTER", "")],
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // '-' should be accepted as stdin; failure should be printer error
            assert!(
                !stderr.contains("cannot open"),
                "Expected '-' to be handled as stdin, but got: {}",
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
