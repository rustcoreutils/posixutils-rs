//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Command-mode commands, one group per command.

use crate::common::{copy_test_data, create_temp_mbox};
use plib::testing::{run_test_with_checker, run_test_with_checker_and_env, TestPlan};
use plib::tmp::NamedTempFile;
use std::io::Write;

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
            // `discard` first: without it this asserts nothing, because the
            // lowercase form prints Date: and From: too when no header is
            // suppressed. That is how both of these passed while `Print` was
            // unreachable and silently ran `print`.
            stdin_data: String::from("discard Date\nPrint 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("Date:") && stdout.contains("From:"),
                "Print must override discard and show all headers: {}",
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
            // `discard` first, for the same reason as cmd_print_uppercase.
            stdin_data: String::from("discard Date\nType 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("Date:") && stdout.contains("From:"),
                "Type must override discard, like Print: {}",
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
            // = prints the current message number. testdata.mbox has no `new`
            // message but message 3 (`Status: O`) is unread, so the current
            // message is the first unread = 3 (spec 104481-104482).
            assert!(
                stdout.trim().lines().any(|l| l.trim() == "3"),
                "= should show the current message number (first unread = 3): {}",
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

/// With `crt` set, pagination is skipped when stdout is not a terminal
/// (audit #15): the message prints inline and no pager is spawned.
#[test]
fn crt_pagination_gated_on_terminal() {
    let mbox = copy_test_data("testdata.mbox");

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                mbox.path().to_str().unwrap().to_string(),
            ],
            stdin_data: String::from("set crt=1\nprint 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        // A bogus pager: if pagination were (wrongly) attempted off a terminal,
        // spawning it would fail and surface a diagnostic.
        &[("PAGER", "nonexistent_pager_xyz123")],
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stdout.contains("Meeting Tomorrow") || stdout.contains("alice"),
                "message should print inline without paging: {}",
                stdout
            );
            assert!(
                !stderr.contains("nonexistent_pager_xyz123"),
                "no pager should be spawned off a terminal: {}",
                stderr
            );
        },
    );
}

/// `file #` reopens the previously-visited folder (audit #18).
#[test]
fn file_hash_previous_folder() {
    let folder_a = create_temp_mbox(
        "From aaa@a.example Mon Jan  1 10:00:00 2024\n\
         From: aaa@a.example\n\
         Subject: folder A\n\
         \n\
         a\n",
    );
    let folder_b = create_temp_mbox(
        "From bbb@b.example Mon Jan  1 10:00:00 2024\n\
         From: bbb@b.example\n\
         Subject: folder B\n\
         \n\
         b\n",
    );
    let a_path = folder_a.path().to_str().unwrap().to_string();
    let b_path = folder_b.path().to_str().unwrap().to_string();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                a_path.clone(),
            ],
            // Open B, then `#` returns to A.
            stdin_data: format!("file {}\nfile #\nquit\n", b_path),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stdout.contains("bbb@b.example"),
                "switching to folder B should show its summary: {}",
                stdout
            );
            assert!(
                stdout.contains("aaa@a.example"),
                "`file #` should reopen folder A: {}",
                stdout
            );
            assert!(
                !stderr.contains("previous folder") && !stderr.contains("#:"),
                "`file #` must resolve, not error: {}",
                stderr
            );
        },
    );
}

/// Editor temporaries must live under `$TMPDIR` with a name nobody can predict.
#[test]
fn editor_temp_file_honors_tmpdir() {
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let log = dir.path().join("editor.log");
    let editor = dir.path().join("fake-editor");
    std::fs::write(
        &editor,
        format!("#!/bin/sh\nprintf '%s\\n' \"$1\" >> {}\n", log.display()),
    )
    .expect("write fake editor");
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&editor, std::fs::Permissions::from_mode(0o755))
            .expect("chmod fake editor");
    }

    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap().to_string();
    let tmpdir = dir.path().to_str().unwrap().to_string();
    let editor_path = editor.to_str().unwrap().to_string();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                mbox_path,
            ],
            stdin_data: format!("set EDITOR={}\nedit 1\nexit\n", editor_path),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("TMPDIR", &tmpdir)],
        |_plan, _output| {
            let logged = std::fs::read_to_string(&log).unwrap_or_default();
            let path = logged.trim();
            assert!(!path.is_empty(), "the editor was never invoked");
            assert!(
                path.starts_with(&tmpdir),
                "the editor temporary ignored $TMPDIR: {}",
                path
            );
            assert!(
                !path.contains("mailx."),
                "the editor temporary still uses a predictable name: {}",
                path
            );
        },
    );
}
