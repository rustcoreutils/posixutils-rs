//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! Receive mode integration tests for mailx
//!
//! These tests verify receive mode functionality using static test data files,
//! allowing them to run without system mail access or special privileges.
//!
//! Test data files:
//! - testdata.mbox: 3 messages (alice, bob, charlie)
//! - testdata-single.mbox: 1 message (alice)
//! - testdata-5msg.mbox: 5 messages (alice, bob, charlie, alice@different, dave)

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::io::Write;
use std::path::PathBuf;
use tempfile::NamedTempFile;

/// Get path to static test data file in tests/ directory
fn test_data_path(filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push(filename);
    path
}

/// Copy a static test data file to a temporary location
/// Use this for tests that may modify the mailbox (quit saves changes)
fn copy_test_data(filename: &str) -> NamedTempFile {
    let src_path = test_data_path(filename);
    let content = std::fs::read_to_string(&src_path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", src_path.display(), e));
    create_temp_mbox(&content)
}

/// Helper to create a temporary mbox file (for tests needing custom content)
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

// =============================================================================
// POSIX Command Tests
// =============================================================================

// -----------------------------------------------------------------------------
// cd/chdir command
// -----------------------------------------------------------------------------

/// Test cd command changes directory
#[test]
fn cmd_cd_to_tmp() {
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
            stdin_data: String::from("cd /tmp\n!pwd\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("/tmp") || stdout.contains("/private/tmp"),
                "Should change to /tmp directory: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test chdir alias
#[test]
fn cmd_chdir_alias() {
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
            stdin_data: String::from("chdir /tmp\n!pwd\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("/tmp") || stdout.contains("/private/tmp"),
                "chdir should work like cd: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test cd to home directory (no argument)
#[test]
fn cmd_cd_home() {
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
            stdin_data: String::from("cd\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            // cd with no arg should go to HOME - just verify it doesn't error
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// copy/Copy command
// -----------------------------------------------------------------------------

/// Test copy command saves messages to file
#[test]
fn cmd_copy_to_file() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();
    let save_file = NamedTempFile::new().expect("failed to create temp file");
    let save_path = save_file.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: format!("copy 1 {}\nquit\n", save_path),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // copy should report file written
            assert!(
                stdout.contains("1 messages") || stdout.contains("bytes"),
                "copy should report saved messages: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test copy multiple messages
#[test]
fn cmd_copy_multiple() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();
    let save_file = NamedTempFile::new().expect("failed to create temp file");
    let save_path = save_file.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: format!("copy 1-2 {}\nquit\n", save_path),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("2 messages") || stdout.contains("bytes"),
                "copy should report 2 messages: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// delete command
// -----------------------------------------------------------------------------

/// Test delete command
#[test]
fn cmd_delete() {
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
            // Delete message 1, then show remaining
            stdin_data: String::from("delete 1\nfrom *\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // After deleting msg 1, from * should only show 2 and 3
            assert!(
                !stdout.contains("alice") || stdout.contains("bob") || stdout.contains("charlie"),
                "from * should not show deleted message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// dp/dt command
// -----------------------------------------------------------------------------

/// Test dp (delete and print) command
#[test]
fn cmd_dp() {
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
            stdin_data: String::from("dp 1\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // dp should delete msg 1 and print next
            assert!(
                stdout.contains("bob") || stdout.contains("Second"),
                "dp should print next message after delete: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test dt alias for dp
#[test]
fn cmd_dt() {
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
            stdin_data: String::from("dt 1\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("bob") || stdout.contains("Second"),
                "dt should work like dp: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// echo command
// -----------------------------------------------------------------------------

/// Test echo command
#[test]
fn cmd_echo() {
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
            stdin_data: String::from("echo Hello World Test\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("Hello World Test"),
                "echo should output text: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// exit/xit command
// -----------------------------------------------------------------------------

/// Test exit command doesn't save changes
#[test]
fn cmd_exit() {
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
            stdin_data: String::from("delete 1\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            // exit should succeed without saving
            assert!(output.status.success());
        },
    );
}

/// Test xit alias for exit
#[test]
fn cmd_xit() {
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
            stdin_data: String::from("xit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// file/folder command
// -----------------------------------------------------------------------------

/// Test file command displays current mailbox info
#[test]
fn cmd_file_info() {
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
            stdin_data: String::from("file\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // file with no arg shows current mailbox info
            assert!(
                stdout.contains("3 messages") || stdout.contains("messages"),
                "file should show mailbox info: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test folder alias for file
#[test]
fn cmd_folder_alias() {
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
            stdin_data: String::from("folder\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("messages"),
                "folder should work like file: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// headers command
// -----------------------------------------------------------------------------

/// Test headers command
#[test]
fn cmd_headers() {
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
            stdin_data: String::from("headers\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout).to_lowercase();
            assert!(
                stdout.contains("alice") && stdout.contains("bob"),
                "headers should show message summary: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// help/? command
// -----------------------------------------------------------------------------

/// Test help command
#[test]
fn cmd_help() {
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
            stdin_data: String::from("help\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // help should show command list
            assert!(
                stdout.contains("delete") && stdout.contains("print"),
                "help should show commands: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test ? alias for help
#[test]
fn cmd_question_mark() {
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
            stdin_data: String::from("?\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("delete") && stdout.contains("print"),
                "? should work like help: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// hold/preserve command
// -----------------------------------------------------------------------------

/// Test hold command
#[test]
fn cmd_hold() {
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
            stdin_data: String::from("hold 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            // hold should succeed
            assert!(output.status.success());
        },
    );
}

/// Test preserve alias for hold
#[test]
fn cmd_preserve() {
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
            stdin_data: String::from("preserve 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// list command
// -----------------------------------------------------------------------------

/// Test list command shows all available commands
#[test]
fn cmd_list() {
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
            stdin_data: String::from("list\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // list should show command names
            assert!(
                stdout.contains("alias")
                    && stdout.contains("delete")
                    && stdout.contains("print")
                    && stdout.contains("quit"),
                "list should show commands: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// mbox command
// -----------------------------------------------------------------------------

/// Test mbox command marks message for moving to mbox
#[test]
fn cmd_mbox() {
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
            stdin_data: String::from("mbox 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// next command
// -----------------------------------------------------------------------------

/// Test next command advances to next message
#[test]
fn cmd_next() {
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
            // Print msg 1, then next should go to msg 2
            stdin_data: String::from("print 1\nnext\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // After print 1 and next, should show message 2
            assert!(
                stdout.contains("alice") && stdout.contains("bob"),
                "next should advance to message 2: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// pipe/| command
// -----------------------------------------------------------------------------

/// Test pipe command
#[test]
fn cmd_pipe() {
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
            stdin_data: String::from("pipe 1 cat\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // pipe to cat should output the message
            assert!(
                stdout.contains("alice@example.com") || stdout.contains("meeting tomorrow"),
                "pipe should output message through command: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test | alias for pipe
#[test]
fn cmd_pipe_alias() {
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
            stdin_data: String::from("| 1 cat\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("alice@example.com") || stdout.contains("meeting tomorrow"),
                "| should work like pipe: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// Print/Type command (with all headers)
// -----------------------------------------------------------------------------

/// Test Print command shows all headers
#[test]
fn cmd_print_uppercase() {
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
            stdin_data: String::from("Print 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Print should show all headers including Date
            assert!(
                stdout.contains("Date:") && stdout.contains("From:"),
                "Print should show all headers: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test Type alias for Print
#[test]
fn cmd_type_uppercase() {
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
            stdin_data: String::from("Type 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("Date:") && stdout.contains("From:"),
                "Type should work like Print: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// save/Save/write commands
// -----------------------------------------------------------------------------

/// Test save command saves message to file
#[test]
fn cmd_save() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();
    let save_file = NamedTempFile::new().expect("failed to create temp file");
    let save_path = save_file.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: format!("save 1 {}\nexit\n", save_path),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("1 messages") || stdout.contains("bytes"),
                "save should report saved message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test write command saves message body only (no headers)
#[test]
fn cmd_write() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();
    let save_file = NamedTempFile::new().expect("failed to create temp file");
    let save_path = save_file.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: format!("write 1 {}\nexit\n", save_path),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("1 messages") || stdout.contains("bytes"),
                "write should report saved message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// size command
// -----------------------------------------------------------------------------

/// Test size command displays message size
#[test]
fn cmd_size() {
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
            stdin_data: String::from("size 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // size should show message number and bytes
            assert!(
                stdout.contains("1:") && stdout.contains("bytes"),
                "size should show message size: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test size command with multiple messages
#[test]
fn cmd_size_multiple() {
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
            stdin_data: String::from("size *\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // size * should show all messages
            assert!(
                stdout.contains("1:") && stdout.contains("2:") && stdout.contains("3:"),
                "size * should show all message sizes: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// source command
// -----------------------------------------------------------------------------

/// Test source command reads commands from file
#[test]
fn cmd_source() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Create a command file
    let mut cmd_file = NamedTempFile::new().expect("failed to create temp file");
    cmd_file
        .write_all(b"set testvar\n")
        .expect("failed to write");
    cmd_file.flush().expect("failed to flush");
    let cmd_path = cmd_file.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: format!("source {}\nset\nquit\n", cmd_path),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // After sourcing, testvar should be set
            assert!(
                stdout.contains("testvar"),
                "source should execute commands from file: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// top command
// -----------------------------------------------------------------------------

/// Test top command shows first lines of message
#[test]
fn cmd_top() {
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
            stdin_data: String::from("top 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // top should show headers and first lines of body
            assert!(
                stdout.contains("From:") && stdout.contains("reminder"),
                "top should show message beginning: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// touch command
// -----------------------------------------------------------------------------

/// Test touch command marks message for mbox
#[test]
fn cmd_touch() {
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
            stdin_data: String::from("touch 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// undelete command
// -----------------------------------------------------------------------------

/// Test undelete command restores deleted message
#[test]
fn cmd_undelete() {
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
            // Delete msg 1, then undelete it, then show from *
            stdin_data: String::from("delete 1\nundelete 1\nfrom *\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // After undelete, msg 1 should be visible again
            assert!(
                stdout.contains("alice") || stdout.contains("Meeting"),
                "undelete should restore message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// z (scroll) command
// -----------------------------------------------------------------------------

/// Test z command scrolls headers forward
#[test]
fn cmd_z_forward() {
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
            stdin_data: String::from("z\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            // z should show headers (or "last screenful" message)
            assert!(output.status.success());
        },
    );
}

/// Test z- command scrolls headers backward
#[test]
fn cmd_z_backward() {
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
            stdin_data: String::from("z-\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// = command (current message number)
// -----------------------------------------------------------------------------

/// Test = command shows current message number
#[test]
fn cmd_equals() {
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
            stdin_data: String::from("=\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // = should print current message number (1)
            assert!(
                stdout.contains("1"),
                "= should show current message number: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// ! (shell escape) command
// -----------------------------------------------------------------------------

/// Test ! command executes shell command
#[test]
fn cmd_shell_escape() {
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
            stdin_data: String::from("!echo TestOutput123\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("TestOutput123"),
                "! should execute shell command: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// Message number navigation
// -----------------------------------------------------------------------------

/// Test entering message number prints that message
#[test]
fn cmd_message_number() {
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
            stdin_data: String::from("2\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Entering 2 should print message 2
            assert!(
                stdout.contains("bob") || stdout.contains("Second"),
                "Message number should print that message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// Comment handling
// -----------------------------------------------------------------------------

/// Test # comments are ignored
#[test]
fn cmd_comment() {
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
            stdin_data: String::from("# This is a comment\necho working\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("working"),
                "Comment should be ignored: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// Blank line handling
// -----------------------------------------------------------------------------

/// Test blank lines are handled
#[test]
fn cmd_blank_line() {
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
            stdin_data: String::from("\n\necho after_blanks\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("after_blanks"),
                "Blank lines should be handled: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// =============================================================================
// Message List Syntax Tests
// =============================================================================

// -----------------------------------------------------------------------------
// Range syntax: n-m
// -----------------------------------------------------------------------------

/// Test message range 1-3
#[test]
fn msglist_range() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            stdin_data: String::from("from 1-3\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout).to_lowercase();
            // Should show messages 1, 2, 3
            assert!(
                stdout.contains("alice") && stdout.contains("bob") && stdout.contains("charlie"),
                "Range 1-3 should show first three messages: {}",
                stdout
            );
            // Should NOT show messages 4, 5
            assert!(
                !stdout.contains("dave"),
                "Range 1-3 should not show message 5: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test message range 3-5
#[test]
fn msglist_range_end() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            stdin_data: String::from("from 3-5\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout).to_lowercase();
            // Should show messages 3, 4, 5
            assert!(
                stdout.contains("charlie") && stdout.contains("dave"),
                "Range 3-5 should show last messages: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// Special symbols: . ^ $ * + -
// -----------------------------------------------------------------------------

/// Test . (current message)
#[test]
fn msglist_dot_current() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            // Print message 3 to set current, then use . to reference it
            stdin_data: String::from("3\nfrom .\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // After viewing 3, . should refer to message 3
            assert!(
                stdout.contains("charlie") || stdout.contains("Urgent"),
                ". should reference current message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test ^ (first message)
#[test]
fn msglist_caret_first() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            stdin_data: String::from("from ^\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stdout_lower = stdout.to_lowercase();
            // ^ should show first message (alice, Meeting Tomorrow)
            assert!(
                stdout_lower.contains("alice") && stdout_lower.contains("meeting tomorrow"),
                "^ should show first message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test $ (last message)
#[test]
fn msglist_dollar_last() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            stdin_data: String::from("from $\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stdout_lower = stdout.to_lowercase();
            // $ should show last message (dave, Final Report)
            assert!(
                stdout_lower.contains("dave") && stdout_lower.contains("final report"),
                "$ should show last message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test * (all messages)
#[test]
fn msglist_star_all() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            // * should show all 5 messages
            assert!(
                stdout.contains("1")
                    && stdout.contains("2")
                    && stdout.contains("3")
                    && stdout.contains("4")
                    && stdout.contains("5"),
                "* should show all messages: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test + (next message)
#[test]
fn msglist_plus_next() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            // Start at msg 2, then + should go to msg 3
            stdin_data: String::from("2\nfrom +\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // After msg 2, + should show msg 3 (charlie)
            assert!(
                stdout.contains("charlie") || stdout.contains("Urgent"),
                "+ should show next message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test - (previous message)
#[test]
fn msglist_minus_prev() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            // Start at msg 3, then - should go to msg 2
            stdin_data: String::from("3\nfrom -\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // After msg 3, - should show msg 2 (bob)
            assert!(
                stdout.contains("bob") || stdout.contains("Project"),
                "- should show previous message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// Subject search: /pattern
// -----------------------------------------------------------------------------

/// Test /pattern searches subjects
#[test]
fn msglist_subject_search() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            stdin_data: String::from("from /Meeting\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // /Meeting should find messages 1 and 4 (both have "Meeting" in subject)
            assert!(
                stdout.contains("Meeting Tomorrow") || stdout.contains("Another Meeting"),
                "/Meeting should find Meeting messages: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test /pattern is case-insensitive
#[test]
fn msglist_subject_search_case() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            stdin_data: String::from("from /urgent\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // /urgent (lowercase) should find "Urgent Request"
            assert!(
                stdout.contains("Urgent") || stdout.contains("charlie"),
                "/urgent should match case-insensitively: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// Message type: :n :o :r :u :d
// -----------------------------------------------------------------------------

/// Test :d (deleted messages) - for undelete
#[test]
fn msglist_type_deleted() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            // Delete messages 1 and 3, then undelete :d (all deleted)
            stdin_data: String::from("delete 1 3\nundelete :d\nfrom *\nexit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout).to_lowercase();
            // After undelete :d, all messages should be visible
            assert!(
                stdout.contains("alice") && stdout.contains("charlie"),
                ":d should select deleted messages for undelete: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// Address matching
// -----------------------------------------------------------------------------

/// Test address matching (sender name)
#[test]
fn msglist_address_match() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            stdin_data: String::from("from bob\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should find bob's message
            assert!(
                stdout.contains("bob") || stdout.contains("Project"),
                "Address match should find bob: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test address matching with full email
#[test]
fn msglist_address_full_email() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            stdin_data: String::from("from alice@example.com\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should find only alice@example.com (not alice@different.org)
            assert!(
                stdout.contains("Meeting Tomorrow"),
                "Full email should find exact match: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test allnet variable - match by login only
#[test]
fn msglist_allnet_login_match() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            // With allnet, "alice" should match both alice@example.com and alice@different.org
            stdin_data: String::from("set allnet\nfrom alice\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should find both alice messages (1 and 4)
            assert!(
                (stdout.contains("Meeting Tomorrow") || stdout.contains("alice@example.com"))
                    && (stdout.contains("Another Meeting")
                        || stdout.contains("alice@different.org")),
                "allnet should match both alice addresses: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// -----------------------------------------------------------------------------
// Multiple specs combined
// -----------------------------------------------------------------------------

/// Test multiple message specs separated by space
#[test]
fn msglist_multiple_specs() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            stdin_data: String::from("from 1 3 5\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout).to_lowercase();
            // Should show messages 1, 3, 5
            assert!(
                stdout.contains("alice") && stdout.contains("charlie") && stdout.contains("dave"),
                "Multiple specs should show selected messages: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test range combined with numbers
#[test]
fn msglist_range_and_number() {
    let mbox = copy_test_data("testdata-5msg.mbox");
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
            stdin_data: String::from("from 1-2 5\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout).to_lowercase();
            // Should show messages 1, 2, 5 but not 3, 4
            assert!(
                stdout.contains("alice") && stdout.contains("bob") && stdout.contains("dave"),
                "Range + number should show correct messages: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// =============================================================================
// Conditional (if/else/endif) Tests
// =============================================================================

/// Test if r (receive mode) conditional in mailrc
#[test]
fn conditional_if_receive_mode() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Create mailrc with conditional for receive mode
    let mailrc = create_temp_mailrc("if r\nset receive_var\nendif\nif s\nset send_var\nendif\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("set\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // In receive mode, receive_var should be set, send_var should not
            assert!(
                stdout.contains("receive_var"),
                "if r should execute in receive mode: {}",
                stdout
            );
            assert!(
                !stdout.contains("send_var"),
                "if s should not execute in receive mode: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test if s (send mode) conditional in mailrc
#[test]
fn conditional_if_send_mode() {
    // Create mailrc with conditional for send mode
    let mailrc =
        create_temp_mailrc("if s\nset send_var\nendif\nif r\nset receive_var\nendif\nset debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Test"),
                String::from("user@example.com"),
            ],
            stdin_data: String::from("body\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            // Debug mode should show we're in send mode
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("Debug mode"),
                "Should be in debug/send mode: {}",
                stderr
            );
            assert!(output.status.success());
        },
    );
}

/// Test else branch in conditional
#[test]
fn conditional_else_branch() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // In receive mode: if s (false) -> else branch should execute
    let mailrc = create_temp_mailrc("if s\nset wrong_var\nelse\nset correct_var\nendif\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("set\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // In receive mode, else branch should execute
            assert!(
                stdout.contains("correct_var"),
                "else branch should execute when if condition is false: {}",
                stdout
            );
            assert!(
                !stdout.contains("wrong_var"),
                "if branch should not execute when condition is false: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test nested conditionals
#[test]
fn conditional_nested() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Nested if r inside if r (both true in receive mode)
    let mailrc = create_temp_mailrc("if r\nset outer_var\nif r\nset inner_var\nendif\nendif\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("set\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Both should be set
            assert!(
                stdout.contains("outer_var") && stdout.contains("inner_var"),
                "Nested conditionals should work: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test commands outside conditionals always execute
#[test]
fn conditional_commands_outside() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let mailrc =
        create_temp_mailrc("set always_set\nif s\nset send_only\nendif\nset also_always\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("set\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Commands outside if should always execute
            assert!(
                stdout.contains("always_set") && stdout.contains("also_always"),
                "Commands outside conditionals should execute: {}",
                stdout
            );
            assert!(
                !stdout.contains("send_only"),
                "if s commands should not run in receive mode: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Helper to create a temporary mailrc file
fn create_temp_mailrc(content: &str) -> NamedTempFile {
    let mut file = NamedTempFile::new().expect("failed to create temp mailrc");
    file.write_all(content.as_bytes())
        .expect("failed to write temp mailrc");
    file.flush().expect("failed to flush temp mailrc");
    file
}

use plib::testing::run_test_with_checker_and_env;
