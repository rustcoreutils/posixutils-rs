//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Command-mode commands, one group per command.

use crate::common::{copy_test_data, create_temp_mailrc, create_temp_mbox, mailx, mailx_env};
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

    let output = mailx(&["-n", "-N", "-f", mbox_path], "cd /tmp\n!pwd\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("/tmp") || stdout.contains("/private/tmp"),
        "Should change to /tmp directory: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test chdir alias
#[test]
fn cmd_chdir_alias() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "chdir /tmp\n!pwd\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("/tmp") || stdout.contains("/private/tmp"),
        "chdir should work like cd: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test cd to home directory (no argument)
#[test]
fn cmd_cd_home() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "cd\nquit\n");
    // cd with no arg should go to HOME - just verify it doesn't error
    assert!(output.status.success());
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

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        format!("copy 1 {}\nquit\n", save_path),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // copy should report file written
    assert!(
        stdout.contains("1 messages") && stdout.contains("bytes"),
        "copy should report saved messages: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test copy multiple messages
#[test]
fn cmd_copy_multiple() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();
    let save_file = NamedTempFile::new().expect("failed to create temp file");
    let save_path = save_file.path().to_str().unwrap();

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        format!("copy 1-2 {}\nquit\n", save_path),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("2 messages") && stdout.contains("bytes"),
        "copy should report 2 messages: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// delete command
// -----------------------------------------------------------------------------

/// Test delete command
#[test]
fn cmd_delete() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Delete message 1, then show remaining
    let output = mailx(&["-n", "-N", "-f", mbox_path], "delete 1\nfrom *\nexit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // After deleting msg 1, from * should only show 2 and 3
    assert!(
        !stdout.contains("alice@example.com")
            && stdout.contains("bob@example.org")
            && stdout.contains("charlie@example.net"),
        "from * should not show deleted message: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// dp/dt command
// -----------------------------------------------------------------------------

/// Test dp (delete and print) command
#[test]
fn cmd_dp() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "dp 1\nexit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // dp should delete msg 1 and print next
    assert!(
        stdout.contains("Subject: Project Update") && stdout.contains("The project is on track."),
        "dp should print next message after delete: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test dt alias for dp
#[test]
fn cmd_dt() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "dt 1\nexit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Subject: Project Update") && stdout.contains("The project is on track."),
        "dt should work like dp: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// echo command
// -----------------------------------------------------------------------------

/// Test echo command
#[test]
fn cmd_echo() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "echo Hello World Test\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Hello World Test"),
        "echo should output text: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// exit/xit command
// -----------------------------------------------------------------------------

/// Test exit command doesn't save changes
#[test]
fn cmd_exit() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "delete 1\nexit\n");
    // exit should succeed without saving
    assert!(output.status.success());
}

/// Test xit alias for exit
#[test]
fn cmd_xit() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "xit\n");
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// file/folder command
// -----------------------------------------------------------------------------

/// Test file command displays current mailbox info
#[test]
fn cmd_file_info() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "file\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // file with no arg shows current mailbox info
    assert!(
        stdout.contains("3 messages"),
        "file should show mailbox info: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test folder alias for file
#[test]
fn cmd_folder_alias() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "folder\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("messages"),
        "folder should work like file: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// headers command
// -----------------------------------------------------------------------------

/// Test headers command
#[test]
fn cmd_headers() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "headers\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout).to_lowercase();
    assert!(
        stdout.contains("alice") && stdout.contains("bob"),
        "headers should show message summary: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// help/? command
// -----------------------------------------------------------------------------

/// Test help command
#[test]
fn cmd_help() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "help\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // help should show command list
    assert!(
        stdout.contains("delete") && stdout.contains("print"),
        "help should show commands: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test ? alias for help
#[test]
fn cmd_question_mark() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "?\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("delete") && stdout.contains("print"),
        "? should work like help: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// hold/preserve command
// -----------------------------------------------------------------------------

/// Test hold command
#[test]
fn cmd_hold() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "hold 1\nquit\n");
    // hold should succeed
    assert!(output.status.success());
}

/// Test preserve alias for hold
#[test]
fn cmd_preserve() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "preserve 1\nquit\n");
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// list command
// -----------------------------------------------------------------------------

/// Test list command shows all available commands
#[test]
fn cmd_list() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "list\nquit\n");
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
}

// -----------------------------------------------------------------------------
// mbox command
// -----------------------------------------------------------------------------

/// Test mbox command marks message for moving to mbox
#[test]
fn cmd_mbox() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "mbox 1\nquit\n");
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// next command
// -----------------------------------------------------------------------------

/// Test next command advances to next message
#[test]
fn cmd_next() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Print msg 1, then next should go to msg 2
    let output = mailx(&["-n", "-N", "-f", mbox_path], "print 1\nnext\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // After print 1 and next, should show message 2
    assert!(
        stdout.contains("alice") && stdout.contains("bob"),
        "next should advance to message 2: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// pipe/| command
// -----------------------------------------------------------------------------

/// Test pipe command
#[test]
fn cmd_pipe() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "pipe 1 cat\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // pipe to cat should output the message
    assert!(
        stdout.contains("alice@example.com") && stdout.contains("meeting tomorrow"),
        "pipe should output message through command: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test | alias for pipe
#[test]
fn cmd_pipe_alias() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "| 1 cat\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("alice@example.com") && stdout.contains("meeting tomorrow"),
        "| should work like pipe: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// Print/Type command (with all headers)
// -----------------------------------------------------------------------------

/// Test Print command shows all headers
#[test]
fn cmd_print_uppercase() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // `discard` first: without it this asserts nothing, because the
    // lowercase form prints Date: and From: too when no header is
    // suppressed. That is how both of these passed while `Print` was
    // unreachable and silently ran `print`.
    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "discard Date\nPrint 1\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Date:") && stdout.contains("From:"),
        "Print must override discard and show all headers: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test Type alias for Print
#[test]
fn cmd_type_uppercase() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // `discard` first, for the same reason as cmd_print_uppercase.
    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "discard Date\nType 1\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Date:") && stdout.contains("From:"),
        "Type must override discard, like Print: {}",
        stdout
    );
    assert!(output.status.success());
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

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        format!("save 1 {}\nexit\n", save_path),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("1 messages") && stdout.contains("bytes"),
        "save should report saved message: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test write command saves message body only (no headers)
#[test]
fn cmd_write() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();
    let save_file = NamedTempFile::new().expect("failed to create temp file");
    let save_path = save_file.path().to_str().unwrap();

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        format!("write 1 {}\nexit\n", save_path),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("1 messages") && stdout.contains("bytes"),
        "write should report saved message: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// size command
// -----------------------------------------------------------------------------

/// Test size command displays message size
#[test]
fn cmd_size() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "size 1\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // size should show message number and bytes
    assert!(
        stdout.contains("1:") && stdout.contains("bytes"),
        "size should show message size: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test size command with multiple messages
#[test]
fn cmd_size_multiple() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "size *\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // size * should show all messages
    assert!(
        stdout.contains("1:") && stdout.contains("2:") && stdout.contains("3:"),
        "size * should show all message sizes: {}",
        stdout
    );
    assert!(output.status.success());
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

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        format!("source {}\nset\nquit\n", cmd_path),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // After sourcing, testvar should be set
    assert!(
        stdout.contains("testvar"),
        "source should execute commands from file: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// top command
// -----------------------------------------------------------------------------

/// Test top command shows first lines of message
#[test]
fn cmd_top() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "top 1\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // top should show headers and first lines of body
    assert!(
        stdout.contains("From:") && stdout.contains("reminder"),
        "top should show message beginning: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// touch command
// -----------------------------------------------------------------------------

/// Test touch command marks message for mbox
#[test]
fn cmd_touch() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "touch 1\nquit\n");
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// undelete command
// -----------------------------------------------------------------------------

/// Test undelete command restores deleted message
#[test]
fn cmd_undelete() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Delete msg 1, then undelete it, then show from *
    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "delete 1\nundelete 1\nfrom *\nexit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // After undelete, msg 1 should be visible again
    assert!(
        stdout.contains("alice") && stdout.contains("Meeting"),
        "undelete should restore message: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// z (scroll) command
// -----------------------------------------------------------------------------

/// Test z command scrolls headers forward
#[test]
fn cmd_z_forward() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "z\nquit\n");
    // z should show headers (or "last screenful" message)
    assert!(output.status.success());
}

/// Test z- command scrolls headers backward
#[test]
fn cmd_z_backward() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "z-\nquit\n");
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// = command (current message number)
// -----------------------------------------------------------------------------

/// Test = command shows current message number
#[test]
fn cmd_equals() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "=\nquit\n");
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
}

// -----------------------------------------------------------------------------
// ! (shell escape) command
// -----------------------------------------------------------------------------

/// Test ! command executes shell command
#[test]
fn cmd_shell_escape() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "!echo TestOutput123\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("TestOutput123"),
        "! should execute shell command: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// Message number navigation
// -----------------------------------------------------------------------------

/// Test entering message number prints that message
#[test]
fn cmd_message_number() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "2\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Entering 2 should print message 2
    assert!(
        stdout.contains("Subject: Project Update") && stdout.contains("The project is on track."),
        "Message number should print that message: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// Comment handling
// -----------------------------------------------------------------------------

/// Test # comments are ignored
#[test]
fn cmd_comment() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "# This is a comment\necho working\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("working"),
        "Comment should be ignored: {}",
        stdout
    );
    assert!(output.status.success());
}

// -----------------------------------------------------------------------------
// Blank line handling
// -----------------------------------------------------------------------------

/// Test blank lines are handled
#[test]
fn cmd_blank_line() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "\n\necho after_blanks\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("after_blanks"),
        "Blank lines should be handled: {}",
        stdout
    );
    assert!(output.status.success());
}

/// With `crt` set, pagination is skipped when stdout is not a terminal:
/// the message prints inline and no pager is spawned.
#[test]
fn crt_pagination_gated_on_terminal() {
    let mbox = copy_test_data("testdata.mbox");

    let output = mailx_env(
        &["-n", "-N", "-f", mbox.path().to_str().unwrap()],
        "set crt=1\nprint 1\nquit\n", // A bogus pager: if pagination were (wrongly) attempted off a terminal,
        // spawning it would fail and surface a diagnostic.
        &[("PAGER", "nonexistent_pager_xyz123")],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stdout.contains("Meeting Tomorrow") && stdout.contains("alice"),
        "message should print inline without paging: {}",
        stdout
    );
    assert!(
        !stderr.contains("nonexistent_pager_xyz123"),
        "no pager should be spawned off a terminal: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `file #` reopens the previously-visited folder.
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

    // Open B, then `#` returns to A.
    let output = mailx(
        &["-n", "-N", "-f", &a_path],
        format!("file {}\nfile #\nquit\n", b_path),
    );
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
    assert_eq!(output.status.code(), Some(0), "exit status");
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

    let output = mailx_env(
        &["-n", "-N", "-f", &mbox_path],
        format!("set EDITOR={}\nedit 1\nexit\n", editor_path),
        &[("TMPDIR", &tmpdir)],
    );
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
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// The command `pipe` runs is its last argument, quoted to keep its blanks
/// (spec: "the command is given as a single argument").
#[test]
fn pipe_takes_a_quoted_command() {
    let mbox = copy_test_data("testdata.mbox");
    let output = mailx(
        &["-n", "-N", "-f", mbox.path().to_str().unwrap()],
        "pipe 1 \"cat -n\"\nexit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("     1\tFrom: Alice Smith <alice@example.com>"),
        "message 1 should be numbered by cat -n: {}\n{}",
        stdout,
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `copy` with no file copies to mbox, as `save` does, but leaves the
/// message unmarked, so quitting does not remove it from the mailbox.
#[test]
fn copy_without_a_file_copies_to_mbox() {
    let mbox = copy_test_data("testdata.mbox");
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let secondary = dir.path().join("mbox");
    let output = mailx_env(
        &["-n", "-N", "-f", mbox.path().to_str().unwrap()],
        "copy 2\nfrom 2\nquit\n",
        &[("MBOX", secondary.to_str().unwrap())],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let copied = std::fs::read_to_string(&secondary).unwrap_or_default();
    assert!(
        copied.contains("Subject: Project Update"),
        "copy should write message 2 to mbox: {}\n{}",
        copied,
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        !stdout
            .lines()
            .any(|l| l.starts_with(">*") || l.starts_with(" *")),
        "copy must not mark the message saved: {}",
        stdout
    );
    let kept = std::fs::read_to_string(mbox.path()).unwrap();
    assert!(
        kept.contains("Subject: Project Update"),
        "message 2 left the mailbox"
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `autoprint` has no effect on `dp`: the next message is written once.
#[test]
fn dp_prints_once_with_autoprint() {
    let mbox = copy_test_data("testdata.mbox");
    let output = mailx(
        &["-n", "-N", "-f", mbox.path().to_str().unwrap()],
        "set autoprint\ndp 1\nexit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout.matches("Subject: Project Update").count(),
        1,
        "dp should print message 2 exactly once: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `dp` on the last message has no following message to write; it reports
/// the end of the mailbox rather than writing the one before.
#[test]
fn dp_of_the_last_message_reports_the_end() {
    let mbox = copy_test_data("testdata.mbox");
    let output = mailx(
        &["-n", "-N", "-f", mbox.path().to_str().unwrap()],
        "dp 3\nexit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("At EOF"),
        "dp 3 should report EOF: {}",
        stdout
    );
    assert!(
        !stdout.contains("Subject:"),
        "dp 3 must not print another message: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `edit` gives the editor a copy: POSIX says it "does not modify the
/// contents of those messages in the mailbox".
#[test]
fn edit_leaves_the_message_unchanged() {
    use std::os::unix::fs::PermissionsExt;

    let mbox = copy_test_data("testdata.mbox");
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let ran = dir.path().join("ran");
    let editor = dir.path().join("editor");
    std::fs::write(
        &editor,
        format!(
            "#!/bin/sh\nsed 's/Meeting/CHANGED/' \"$1\" > \"$1.new\" && mv \"$1.new\" \"$1\"\ntouch {}\n",
            ran.display()
        ),
    )
    .unwrap();
    std::fs::set_permissions(&editor, std::fs::Permissions::from_mode(0o755)).unwrap();

    let output = mailx_env(
        &["-n", "-N", "-f", mbox.path().to_str().unwrap()],
        "edit 1\nprint 1\nquit\n",
        &[("EDITOR", editor.to_str().unwrap())],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(ran.exists(), "the editor did not run");
    assert!(
        stdout.contains("Subject: Meeting Tomorrow") && !stdout.contains("CHANGED"),
        "the edit reached the message: {}",
        stdout
    );
    let after = std::fs::read(mbox.path()).unwrap();
    assert!(
        !String::from_utf8_lossy(&after).contains("CHANGED"),
        "the edit reached the mailbox"
    );
    assert!(
        String::from_utf8_lossy(&after).contains("Subject: Meeting Tomorrow"),
        "the message is gone from the mailbox"
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `headers n` makes the message at the top of n's page current.
#[test]
fn headers_with_a_message_makes_its_page_top_current() {
    let mbox = copy_test_data("testdata.mbox");
    let mailrc = create_temp_mailrc("set screen=2\n");
    let output = mailx_env(
        &["-N", "-f", mbox.path().to_str().unwrap()],
        "headers 2\n=\nheaders 3\n=\nexit\n",
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let numbers: Vec<&str> = stdout
        .lines()
        .filter(|l| l.parse::<usize>().is_ok())
        .collect();
    assert_eq!(numbers, ["1", "3"], "{}", stdout);
}
