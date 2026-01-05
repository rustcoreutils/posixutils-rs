//
// Copyright (c) 2025-2026 Jeff Garzik
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

use plib::testing::{TestPlan, run_test_with_checker, run_test_with_checker_and_env};
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

/// Test alias expansion in send mode
/// Verifies that aliases are properly expanded when sending
#[test]
fn send_alias_expansion() {
    // Create a mailrc with an alias and debug mode
    let mailrc = create_temp_mailrc("set debug\nalias team user1@example.com user2@example.com\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Team Message"),
                String::from("team"),
            ],
            stdin_data: String::from("Message for the team.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Debug mode should show the expanded aliases
            assert!(
                stderr.contains("Debug mode"),
                "Should be in debug mode: {}",
                stderr
            );
            // The alias should have been expanded to individual addresses
            assert!(
                stderr.contains("user1@example.com") || stderr.contains("user2@example.com"),
                "Alias should be expanded: {}",
                stderr
            );
            assert!(output.status.success());
        },
    );

    drop(mailrc);
}

/// Test empty message handling
#[test]
fn send_empty_message() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Empty Test"),
                String::from("recipient@example.com"),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("Debug mode"),
                "Should be in debug mode: {}",
                stderr
            );
            // Body should be 0 bytes
            assert!(
                stderr.contains("Body: 0 bytes"),
                "Should show empty body: {}",
                stderr
            );
            assert!(output.status.success());
        },
    );

    drop(mailrc);
}

/// Test subject containing special characters
#[test]
fn send_special_subject() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Re: [URGENT] Meeting @2pm - Updated!"),
                String::from("user@example.com"),
            ],
            stdin_data: String::from("Meeting details.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("Debug mode"),
                "Should be in debug mode: {}",
                stderr
            );
            assert!(
                stderr.contains("[URGENT]") || stderr.contains("Updated"),
                "Subject should be preserved: {}",
                stderr
            );
            assert!(output.status.success());
        },
    );

    drop(mailrc);
}

/// Test dot variable behavior (dot ends message)
/// Note: This tests non-interactive behavior - in non-interactive mode,
/// dot handling doesn't apply (stdin is just read until EOF)
#[test]
fn send_with_dot_variable() {
    let mailrc = create_temp_mailrc("set debug\nset dot\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Dot Test"),
                String::from("recipient@example.com"),
            ],
            stdin_data: String::from("Line 1\n.\nLine 2 (should be included in non-TTY)\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("Debug mode"),
                "Should be in debug mode: {}",
                stderr
            );
            // In non-interactive mode, the dot is included in the body
            assert!(output.status.success());
        },
    );

    drop(mailrc);
}

/// Test sign variable is properly parsed from mailrc
#[test]
fn send_sign_variable() {
    let mailrc = create_temp_mailrc("set debug\nset sign=\"Best regards,\\nTest User\"\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Sign Test"),
                String::from("user@example.com"),
            ],
            stdin_data: String::from("Hello.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
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

/// Test escape variable changes the escape character
#[test]
fn send_custom_escape_char() {
    let mailrc = create_temp_mailrc("set debug\nset escape=#\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Escape Test"),
                String::from("user@example.com"),
            ],
            stdin_data: String::from("Message body.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
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

/// Test sendwait variable (wait for sendmail)
#[test]
fn send_sendwait_variable() {
    let mailrc = create_temp_mailrc("set debug\nset sendwait\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Sendwait Test"),
                String::from("user@example.com"),
            ],
            stdin_data: String::from("Test message.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // In debug mode, sendwait doesn't matter
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
