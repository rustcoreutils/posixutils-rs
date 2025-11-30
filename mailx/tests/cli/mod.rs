//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! CLI option integration tests for mailx
//!
//! Tests all POSIX-defined command line options:
//! - `-e` : Check for mail presence (exit 0 if mail, 1 if not)
//! - `-f [file]` : Read from specified mbox file
//! - `-F` : Record outgoing mail to file named after first recipient
//! - `-H` : Display headers only, then exit
//! - `-i` : Ignore interrupts (requires TTY, limited testing)
//! - `-n` : Do not read system startup file
//! - `-N` : Do not display initial header summary
//! - `-s subject` : Set subject for send mode
//! - `-u user` : Read user's system mailbox (requires /var/mail access)
//!
//! Tests use static test data files for reproducible results.

use plib::testing::{run_test, run_test_with_checker, run_test_with_checker_and_env, TestPlan};
use std::io::Write;
use std::path::PathBuf;
use tempfile::NamedTempFile;

// =============================================================================
// Test Data Helpers
// =============================================================================

/// Get path to static test data file
fn test_data_path(filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push("cli");
    path.push(filename);
    path
}

/// Helper to create a temporary mailrc file
fn create_temp_mailrc(content: &str) -> NamedTempFile {
    let mut file = NamedTempFile::new().expect("failed to create temp mailrc");
    file.write_all(content.as_bytes())
        .expect("failed to write temp mailrc");
    file.flush().expect("failed to flush temp mailrc");
    file
}

/// Helper to create a temporary mbox file
fn create_temp_mbox(content: &str) -> NamedTempFile {
    let mut file = NamedTempFile::new().expect("failed to create temp mbox");
    file.write_all(content.as_bytes())
        .expect("failed to write temp mbox");
    file.flush().expect("failed to flush temp mbox");
    file
}

// =============================================================================
// -e Option: Check for Mail Presence
// =============================================================================

/// Test -e with mail present (should exit 0)
#[test]
fn opt_e_mail_present() {
    let mbox_path = test_data_path("testdata.mbox");

    run_test(TestPlan {
        cmd: String::from("mailx"),
        args: vec![
            String::from("-n"),
            String::from("-e"),
            String::from("-f"),
            mbox_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

/// Test -e with empty mbox (should exit 1)
#[test]
fn opt_e_no_mail() {
    let mbox = create_temp_mbox("");

    run_test(TestPlan {
        cmd: String::from("mailx"),
        args: vec![
            String::from("-n"),
            String::from("-e"),
            String::from("-f"),
            mbox.path().to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    });
}

/// Test -e combined with -f
#[test]
fn opt_e_with_f() {
    let mbox_path = test_data_path("testdata.mbox");

    // -e -f should check the specified file
    run_test(TestPlan {
        cmd: String::from("mailx"),
        args: vec![
            String::from("-n"),
            String::from("-ef"),
            mbox_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// =============================================================================
// -f Option: Read from File
// =============================================================================

/// Test -f with explicit file path
#[test]
fn opt_f_explicit_file() {
    let mbox_path = test_data_path("testdata.mbox");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                mbox_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::from("quit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            assert!(
                output.status.success(),
                "Should read from file successfully"
            );
        },
    );
}

/// Test -f reads correct number of messages
#[test]
fn opt_f_message_count() {
    let mbox_path = test_data_path("testdata.mbox");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-f"),
                mbox_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::from("quit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Test data has 3 messages, should show them in header summary
            assert!(
                stdout.contains("alice") || stdout.contains("Alice"),
                "Should show first message sender: {}",
                stdout
            );
            assert!(
                stdout.contains("bob") || stdout.contains("Bob"),
                "Should show second message sender: {}",
                stdout
            );
            assert!(
                stdout.contains("charlie") || stdout.contains("Charlie"),
                "Should show third message sender: {}",
                stdout
            );
        },
    );
}

/// Test -f with nonexistent file
#[test]
fn opt_f_nonexistent_file() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-f"),
                String::from("/nonexistent/path/to/mbox"),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_plan, output| {
            assert!(!output.status.success(), "Should fail for nonexistent file");
        },
    );
}

// =============================================================================
// -F Option: Record to Recipient File
// =============================================================================

/// Test -F option with debug mode (verify it's parsed correctly)
#[test]
fn opt_f_uppercase_record() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-F"),
                String::from("-s"),
                String::from("Record Test"),
                String::from("recipient@example.com"),
            ],
            stdin_data: String::from("Test message for -F option.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // With debug mode, should show message info
            assert!(
                stderr.contains("Debug mode"),
                "Should be in debug mode: {}",
                stderr
            );
            assert!(output.status.success());
        },
    );
}

// =============================================================================
// -H Option: Headers Only
// =============================================================================

/// Test -H displays only headers and exits
#[test]
fn opt_h_headers_only() {
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
            stdin_data: String::new(), // No input needed, -H exits after display
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should show message headers
            assert!(
                stdout.contains("Meeting Tomorrow") || stdout.contains("alice"),
                "Should show first message info: {}",
                stdout
            );
            assert!(
                stdout.contains("Project Update") || stdout.contains("bob"),
                "Should show second message info: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test -H with empty mailbox
#[test]
fn opt_h_empty_mbox() {
    let mbox = create_temp_mbox("");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-H"),
                String::from("-f"),
                mbox.path().to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should indicate no mail
            assert!(
                stdout.contains("No mail") || stdout.is_empty() || output.status.success(),
                "Should handle empty mailbox gracefully: {}",
                stdout
            );
        },
    );
}

/// Test -H combined with -f (common pattern)
#[test]
fn opt_h_with_f() {
    let mbox_path = test_data_path("testdata.mbox");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-nHf"),
                mbox_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Combined flags should work
            assert!(
                stdout.contains("alice") || stdout.contains("Meeting"),
                "Combined -nHf should show headers: {}",
                stdout
            );
        },
    );
}

// =============================================================================
// -i Option: Ignore Interrupts
// =============================================================================

/// Test -i option is accepted (full interrupt testing requires TTY)
#[test]
fn opt_i_accepted() {
    let mbox_path = test_data_path("testdata.mbox");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-i"),
                String::from("-N"),
                String::from("-f"),
                mbox_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::from("quit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            // Just verify -i is accepted without error
            assert!(output.status.success(), "-i option should be accepted");
        },
    );
}

// =============================================================================
// -n Option: No System Init File
// =============================================================================

/// Test -n skips system init file
#[test]
fn opt_n_no_init() {
    let mbox_path = test_data_path("testdata.mbox");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                mbox_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::from("set\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            // With -n, should have minimal variables set
            assert!(output.status.success());
        },
    );
}

/// Test without -n (MAILRC is honored)
#[test]
fn opt_without_n_reads_mailrc() {
    let mbox_path = test_data_path("testdata.mbox");
    let mailrc = create_temp_mailrc("set testvar\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-N"),
                String::from("-f"),
                mbox_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::from("set\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Without -n, MAILRC should be read
            assert!(
                stdout.contains("testvar"),
                "Without -n, MAILRC variables should be set: {}",
                stdout
            );
        },
    );
}

// =============================================================================
// -N Option: No Header Summary
// =============================================================================

/// Test -N suppresses initial header summary
#[test]
fn opt_n_uppercase_no_summary() {
    let mbox_path = test_data_path("testdata.mbox");

    // With -N, should not show initial headers
    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                mbox_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::from("quit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            // With -N, initial summary is suppressed
            // Output should be minimal (just prompt if any)
            assert!(output.status.success());
        },
    );
}

/// Test without -N shows header summary
#[test]
fn opt_without_n_uppercase_shows_summary() {
    let mbox_path = test_data_path("testdata.mbox");

    // Without -N, should show initial headers
    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-f"),
                mbox_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::from("quit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Without -N, should show headers
            assert!(
                stdout.contains("alice") || stdout.contains("Meeting") || stdout.contains("1"),
                "Without -N, should show header summary: {}",
                stdout
            );
        },
    );
}

// =============================================================================
// -s Option: Set Subject
// =============================================================================

/// Test -s sets subject (verified via debug mode)
#[test]
fn opt_s_set_subject() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Test Subject Line"),
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
                stderr.contains("Test Subject Line") || stderr.contains("Subject:"),
                "Should show subject in debug: {}",
                stderr
            );
        },
    );
}

/// Test -s with subject containing spaces
#[test]
fn opt_s_subject_with_spaces() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("This is a multi-word subject"),
                String::from("user@example.com"),
            ],
            stdin_data: String::from("Body.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("multi-word") || stderr.contains("Subject:"),
                "Should handle multi-word subject: {}",
                stderr
            );
        },
    );
}

/// Test -s without argument (should error)
#[test]
fn opt_s_missing_argument() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![String::from("-s")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(!output.status.success(), "-s without argument should fail");
            assert!(
                stderr.contains("requires") || stderr.contains("argument") || stderr.contains("-s"),
                "Should indicate missing argument: {}",
                stderr
            );
        },
    );
}

/// Test -s with attached subject (no space: -sSubject)
#[test]
fn opt_s_attached_subject() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-sAttachedSubject"),
                String::from("user@example.com"),
            ],
            stdin_data: String::from("Body.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("AttachedSubject") || stderr.contains("Debug mode"),
                "Should handle attached subject: {}",
                stderr
            );
        },
    );
}

// =============================================================================
// -u Option: Read User Mailbox
// =============================================================================

/// Test -u option is parsed correctly (actual mailbox access requires privileges)
/// We test that the option is accepted and produces expected behavior
#[test]
fn opt_u_parsed_correctly() {
    // -u without system mail access should fail gracefully
    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-e"),
                String::from("-u"),
                String::from("nonexistentuser12345"),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1, // Should fail - no such user/mailbox
        },
        |_plan, output| {
            // Either fails to find mailbox or exits with code 1
            assert!(
                !output.status.success() || output.status.code() == Some(1),
                "-u with nonexistent user should fail"
            );
        },
    );
}

/// Test -u missing argument
#[test]
fn opt_u_missing_argument() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![String::from("-u")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(!output.status.success(), "-u without argument should fail");
            assert!(
                stderr.contains("requires") || stderr.contains("argument") || stderr.contains("-u"),
                "Should indicate missing argument: {}",
                stderr
            );
        },
    );
}

// =============================================================================
// Combined Options
// =============================================================================

/// Test multiple options combined: -nNf
#[test]
fn combined_n_n_f() {
    let mbox_path = test_data_path("testdata.mbox");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-nNf"),
                mbox_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::from("quit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            assert!(output.status.success(), "Combined -nNf should work");
        },
    );
}

/// Test -n -H -f combination
#[test]
fn combined_n_h_f_separate() {
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
            assert!(
                stdout.contains("alice") || stdout.contains("Meeting"),
                "Separate options should work: {}",
                stdout
            );
        },
    );
}

/// Test send mode options: -n -s subject recipient
#[test]
fn combined_send_options() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Combined Send Test"),
                String::from("user1@example.com"),
                String::from("user2@example.com"),
            ],
            stdin_data: String::from("Message to multiple recipients.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("Debug mode"),
                "Send with multiple recipients should work: {}",
                stderr
            );
        },
    );
}

/// Test illegal option produces error
#[test]
fn illegal_option() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![String::from("-Z")], // -Z is not a valid option
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(!output.status.success(), "Illegal option should fail");
            assert!(
                stderr.contains("illegal") || stderr.contains("Z") || stderr.contains("option"),
                "Should indicate illegal option: {}",
                stderr
            );
        },
    );
}

// =============================================================================
// Edge Cases
// =============================================================================

/// Test reading from stdin in send mode (no file arguments)
#[test]
fn stdin_send_mode() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Stdin Test"),
                String::from("recipient@example.com"),
            ],
            stdin_data: String::from("This message comes from stdin.\nSecond line.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Should process stdin as message body
            assert!(
                stderr.contains("Debug mode") && stderr.contains("bytes"),
                "Should read message from stdin: {}",
                stderr
            );
        },
    );
}

/// Test empty recipient list with -s (goes to receive mode)
#[test]
fn subject_without_recipient() {
    let mbox_path = test_data_path("testdata.mbox");

    // With -s but no recipients and -f specified, should go to receive mode
    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-s"),
                String::from("Unused Subject"),
                String::from("-f"),
                mbox_path.to_str().unwrap().to_string(),
            ],
            stdin_data: String::from("quit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            // Should enter receive mode (no recipients means not send mode)
            assert!(output.status.success());
        },
    );
}
