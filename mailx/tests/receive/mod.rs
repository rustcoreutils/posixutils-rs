//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! Receive mode integration tests for mailx
//!
//! These tests verify receive mode functionality using temporary mbox files,
//! allowing them to run without system mail access or special privileges.

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::io::Write;
use tempfile::NamedTempFile;

/// Sample mbox content with two test messages
const TEST_MBOX: &str = r#"From sender@example.com Fri Nov 29 10:00:00 2024
From: sender@example.com
To: user@localhost
Subject: First Test Message
Date: Fri, 29 Nov 2024 10:00:00 +0000

This is the body of the first test message.
It has multiple lines.

From another@example.org Fri Nov 29 11:00:00 2024
From: another@example.org
To: user@localhost
Subject: Second Test Message
Date: Fri, 29 Nov 2024 11:00:00 +0000

This is the second message body.
"#;

/// Helper to create a temporary mbox file
fn create_temp_mbox(content: &str) -> NamedTempFile {
    let mut file = NamedTempFile::new().expect("failed to create temp mbox");
    file.write_all(content.as_bytes())
        .expect("failed to write temp mbox");
    file.flush().expect("failed to flush temp mbox");
    file
}

/// Test headers display (-H option)
/// Verifies:
/// - Reading mbox format files
/// - Parsing message headers
/// - Header summary output format
#[test]
fn receive_headers_only() {
    let mbox = create_temp_mbox(TEST_MBOX);
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-H"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);

            // Verify header output contains our test messages
            assert!(
                stdout.contains("sender@example.com") || stdout.contains("sender"),
                "Should show first sender in headers: {}",
                stdout
            );
            assert!(
                stdout.contains("another@example.org") || stdout.contains("another"),
                "Should show second sender in headers: {}",
                stdout
            );
            assert!(
                stdout.contains("First Test Message"),
                "Should show first subject: {}",
                stdout
            );
            assert!(
                stdout.contains("Second Test Message"),
                "Should show second subject: {}",
                stdout
            );
            assert!(output.status.success(), "Should exit successfully");
        },
    );
}

/// Test print command in receive mode
/// Verifies:
/// - Interactive command processing via stdin
/// - Message printing with headers
/// - quit command
#[test]
fn receive_print_message() {
    let mbox = create_temp_mbox(TEST_MBOX);
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"), // Suppress initial header summary
                String::from("-f"),
                String::from(mbox_path),
            ],
            // Commands piped via stdin
            stdin_data: String::from("print 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);

            // Verify message 1 was printed
            assert!(
                stdout.contains("From: sender@example.com"),
                "Should show From header: {}",
                stdout
            );
            assert!(
                stdout.contains("Subject: First Test Message"),
                "Should show Subject header: {}",
                stdout
            );
            assert!(
                stdout.contains("first test message"),
                "Should show message body: {}",
                stdout
            );
            assert!(output.status.success(), "Should exit successfully");
        },
    );
}

/// Test from command (display header summary)
/// Verifies:
/// - The from command displays message summary
/// - Message numbering
#[test]
fn receive_from_command() {
    let mbox = create_temp_mbox(TEST_MBOX);
    let mbox_path = mbox.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("from *\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);

            // from * should show all messages
            assert!(
                stdout.contains("1") && stdout.contains("2"),
                "Should show message numbers: {}",
                stdout
            );
            assert!(output.status.success(), "Should exit successfully");
        },
    );
}

/// Test check for mail presence (-e option)
/// Verifies:
/// - -e returns 0 when mail exists
/// - -e returns 1 when no mail
#[test]
fn receive_check_mail_exists() {
    let mbox = create_temp_mbox(TEST_MBOX);
    let mbox_path = mbox.path().to_str().unwrap();

    // With mail present, -e should return 0
    run_test(TestPlan {
        cmd: String::from("mailx"),
        args: vec![
            String::from("-n"),
            String::from("-e"),
            String::from("-f"),
            String::from(mbox_path),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

/// Test check for mail when empty (-e option)
#[test]
fn receive_check_mail_empty() {
    let mbox = create_temp_mbox("");
    let mbox_path = mbox.path().to_str().unwrap();

    // With no mail, -e should return 1
    run_test(TestPlan {
        cmd: String::from("mailx"),
        args: vec![
            String::from("-n"),
            String::from("-e"),
            String::from("-f"),
            String::from(mbox_path),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    });
}
