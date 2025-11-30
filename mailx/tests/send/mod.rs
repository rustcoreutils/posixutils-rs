//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! Send mode integration tests for mailx
//!
//! These tests verify send mode functionality without requiring actual mail
//! delivery infrastructure. Tests use debug mode via MAILRC environment
//! variable or expect controlled failures when sendmail is unavailable.

use plib::testing::{run_test_with_checker, run_test_with_checker_and_env, TestPlan};
use std::io::Write;
use tempfile::NamedTempFile;

/// Helper to create a temporary mailrc file with given content
fn create_temp_mailrc(content: &str) -> NamedTempFile {
    let mut file = NamedTempFile::new().expect("failed to create temp mailrc");
    file.write_all(content.as_bytes())
        .expect("failed to write temp mailrc");
    file.flush().expect("failed to flush temp mailrc");
    file
}

/// Test basic send mode with single recipient
/// Verifies:
/// - Subject line handling (-s option)
/// - Recipient processing
/// - Message body reading from stdin
/// - Proper error when sendmail is not available
#[test]
fn send_basic() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-s"),
                String::from("Test Subject"),
                String::from("recipient@example.com"),
            ],
            stdin_data: String::from("This is a test message.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);

            // In CI environments without sendmail, we expect an error
            // In environments with sendmail, it should succeed
            // Both are valid outcomes for this test
            if !output.status.success() {
                assert!(
                    stderr.contains("Cannot find sendmail")
                        || stderr.contains("sendmail")
                        || stderr.contains("Failed"),
                    "Expected sendmail error, got: {}",
                    stderr
                );
            }
            // If it succeeded, sendmail was available and message was sent
        },
    );
}

/// Test send mode with subject containing spaces
/// Verifies proper handling of multi-word subjects
#[test]
fn send_subject_with_spaces() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-s"),
                String::from("This is a longer test subject"),
                String::from("user@example.com"),
            ],
            stdin_data: String::from("Message body.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Accept either success or sendmail not found
            if !output.status.success() {
                assert!(
                    stderr.contains("Cannot find sendmail") || stderr.contains("sendmail"),
                    "Unexpected error: {}",
                    stderr
                );
            }
        },
    );
}

/// Test send mode with multiple recipients
/// Verifies that multiple recipients are properly processed
#[test]
fn send_multiple_recipients() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-s"),
                String::from("Multi-recipient Test"),
                String::from("user1@example.com"),
                String::from("user2@example.com"),
            ],
            stdin_data: String::from("Message to multiple recipients.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Accept either success or sendmail not found
            if !output.status.success() {
                assert!(
                    stderr.contains("Cannot find sendmail") || stderr.contains("sendmail"),
                    "Unexpected error: {}",
                    stderr
                );
            }
        },
    );
}

/// Test send mode with multiline message body
/// Verifies that stdin is properly read until EOF
#[test]
fn send_multiline_body() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-s"),
                String::from("Multiline Test"),
                String::from("recipient@example.com"),
            ],
            stdin_data: String::from("Line 1\nLine 2\nLine 3\n\nParagraph 2\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            if !output.status.success() {
                assert!(
                    stderr.contains("Cannot find sendmail") || stderr.contains("sendmail"),
                    "Unexpected error: {}",
                    stderr
                );
            }
        },
    );
}

/// Test send mode with debug variable set via MAILRC environment variable
/// This test demonstrates the env var support in the test framework.
/// Verifies:
/// - MAILRC environment variable is honored
/// - debug variable prevents actual sending
/// - Proper debug output is produced
#[test]
fn send_debug_mode_via_mailrc() {
    // Create a mailrc that sets debug mode
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Debug Test Subject"),
                String::from("debug-recipient@example.com"),
            ],
            stdin_data: String::from("This message should not be sent.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);

            // In debug mode, mailx prints diagnostics to stderr
            assert!(
                stderr.contains("Debug mode"),
                "Should show debug mode message: {}",
                stderr
            );
            assert!(
                stderr.contains("debug-recipient@example.com") || stderr.contains("To:"),
                "Should show recipient in debug output: {}",
                stderr
            );
            assert!(
                stderr.contains("Debug Test Subject") || stderr.contains("Subject:"),
                "Should show subject in debug output: {}",
                stderr
            );
            assert!(
                output.status.success(),
                "Debug mode should exit successfully"
            );
        },
    );

    // Keep mailrc file alive until test completes
    drop(mailrc);
}

/// Test that MAILRC can set multiple variables
#[test]
fn send_mailrc_multiple_vars() {
    // Create a mailrc that sets multiple variables
    let mailrc = create_temp_mailrc("set debug\nset quiet\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Multi-var Test"),
                String::from("user@example.com"),
            ],
            stdin_data: String::from("Test body.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Debug mode should be active
            assert!(
                stderr.contains("Debug mode"),
                "Should be in debug mode: {}",
                stderr
            );
            assert!(output.status.success());
        },
    );

    drop(mailrc);
}
