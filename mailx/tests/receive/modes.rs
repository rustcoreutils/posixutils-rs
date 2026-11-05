//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Receive Mode entry points: `-H`, `-e`, and the first commands.

use crate::common::{copy_test_data, create_temp_mbox, test_data_path};
use plib::testing::{run_test, run_test_with_checker, TestPlan};

/// Test headers display (-H option)
/// Verifies:
/// - Reading mbox format files
/// - Parsing message headers
/// - Header summary output format
#[test]
fn receive_headers_only() {
    let mbox_path = test_data_path("testdata.mbox");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-H"),
                String::from("-f"),
                mbox_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);

            // Verify header output contains our test messages (alice, bob, charlie)
            assert!(
                stdout.contains("alice") || stdout.contains("Alice"),
                "Should show first sender in headers: {}",
                stdout
            );
            assert!(
                stdout.contains("bob") || stdout.contains("Bob"),
                "Should show second sender in headers: {}",
                stdout
            );
            assert!(
                stdout.contains("Meeting Tomorrow"),
                "Should show first subject: {}",
                stdout
            );
            assert!(
                stdout.contains("Project Update"),
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
    let mbox = copy_test_data("testdata.mbox");
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

            // Verify message 1 was printed (alice's meeting message)
            assert!(
                stdout.contains("alice@example.com") || stdout.contains("Alice Smith"),
                "Should show From header: {}",
                stdout
            );
            assert!(
                stdout.contains("Meeting Tomorrow"),
                "Should show Subject header: {}",
                stdout
            );
            assert!(
                stdout.contains("meeting tomorrow") || stdout.contains("2pm"),
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
    let mbox = copy_test_data("testdata.mbox");
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
    let mbox = copy_test_data("testdata.mbox");
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
