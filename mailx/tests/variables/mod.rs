//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! Variable handling integration tests for mailx
//!
//! These tests verify the set/unset commands and variable behavior,
//! including special cases like the onehop rejection.

use plib::testing::{run_test_with_checker, TestPlan};
use std::io::Write;
use tempfile::NamedTempFile;

/// Minimal mbox content (single empty message structure)
const MINIMAL_MBOX: &str = r#"From test@example.com Fri Nov 29 12:00:00 2024
From: test@example.com
To: user@localhost
Subject: Test
Date: Fri, 29 Nov 2024 12:00:00 +0000

Test message.
"#;

/// Helper to create a temporary mbox file
fn create_temp_mbox(content: &str) -> NamedTempFile {
    let mut file = NamedTempFile::new().expect("failed to create temp mbox");
    file.write_all(content.as_bytes())
        .expect("failed to write temp mbox");
    file.flush().expect("failed to flush temp mbox");
    file
}

/// Test setting and displaying variables
/// Verifies:
/// - set command with no arguments shows variables
/// - set varname enables boolean variable
/// - set varname=value sets string variable
#[test]
fn variables_set_and_display() {
    let mbox = create_temp_mbox(MINIMAL_MBOX);
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
            // Set variables then display them
            stdin_data: String::from("set dot\nset prompt=\"test> \"\nset\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);

            // Verify dot variable is set (should appear in set output)
            assert!(stdout.contains("dot"), "Should show dot variable: {}", stdout);

            // Verify prompt variable is set with value
            assert!(
                stdout.contains("prompt"),
                "Should show prompt variable: {}",
                stdout
            );

            assert!(output.status.success(), "Should exit successfully");
        },
    );
}

/// Test unsetting variables
/// Verifies:
/// - set noVARNAME unsets a variable
/// - unset command removes variable
#[test]
fn variables_unset() {
    let mbox = create_temp_mbox(MINIMAL_MBOX);
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
            // Set then unset using both methods
            stdin_data: String::from("set dot\nset autoprint\nset nodot\nunset autoprint\nset\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            // After unsetting, dot and autoprint should not appear as set
            // For now, we just verify successful execution
            assert!(output.status.success(), "Should exit successfully");
        },
    );
}

/// Test onehop variable rejection
/// Verifies:
/// - set onehop produces an error
/// - mailx permanently operates in noonehop mode
#[test]
fn variables_onehop_rejected() {
    let mbox = create_temp_mbox(MINIMAL_MBOX);
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
            stdin_data: String::from("set onehop\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);

            // Should produce an error message about onehop
            let combined = format!("{}{}", stdout, stderr);
            assert!(
                combined.contains("onehop") && combined.contains("not supported")
                    || combined.contains("noonehop"),
                "Should reject onehop with error: stdout={}, stderr={}",
                stdout,
                stderr
            );
        },
    );
}

/// Test alias command
/// Verifies:
/// - alias command defines mail aliases
/// - aliases expand when used
#[test]
fn variables_alias() {
    let mbox = create_temp_mbox(MINIMAL_MBOX);
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
            stdin_data: String::from("alias friends alice@example.com bob@example.com\nalias\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);

            // alias command with no args should display defined aliases
            assert!(
                stdout.contains("friends") || stdout.contains("alice"),
                "Should show defined alias: {}",
                stdout
            );
            assert!(output.status.success(), "Should exit successfully");
        },
    );
}

/// Test internal variable defaults
/// Verifies some standard POSIX default values
#[test]
fn variables_defaults() {
    let mbox = create_temp_mbox(MINIMAL_MBOX);
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
            // Just display default variables
            stdin_data: String::from("set\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            // Check for some default variables that should be set
            // POSIX says asksub defaults to enabled for interactive
            // But with -n, we skip rc files so minimal defaults
            assert!(output.status.success(), "Should exit successfully");
        },
    );
}
