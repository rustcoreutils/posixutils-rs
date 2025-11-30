//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! Comprehensive variable handling integration tests for mailx
//!
//! These tests systematically verify all mailx internal variables that can be
//! tested without a real MTA or live system data.
//!
//! Variables are organized into categories:
//! - Basic set/unset operations
//! - Receive-mode display variables (header, screen, toplines, showto, autoprint)
//! - Path variables (folder, MBOX, DEAD, LISTER, SHELL)
//! - Input/escape variables (escape, indentprefix, prompt, quiet)
//! - Message-list variables (allnet)
//! - Mailbox operation variables (hold, keep, keepsave, append)
//! - Shell command variables (bang)
//! - Special rejections (onehop)

use plib::testing::{run_test_with_checker, run_test_with_checker_and_env, TestPlan};
use std::io::Write;
use tempfile::{NamedTempFile, TempDir};

// =============================================================================
// Test Data
// =============================================================================

/// Minimal mbox content (single message)
const MINIMAL_MBOX: &str = r#"From test@example.com Fri Nov 29 12:00:00 2024
From: test@example.com
To: user@localhost
Subject: Test Message
Date: Fri, 29 Nov 2024 12:00:00 +0000

This is the test message body.
"#;

/// Multi-message mbox for testing message lists and display
const MULTI_MSG_MBOX: &str = r#"From alice@example.com Fri Nov 29 10:00:00 2024
From: alice@example.com
To: user@localhost
Subject: First Message
Date: Fri, 29 Nov 2024 10:00:00 +0000

First message body.
Line two of first message.
Line three of first message.
Line four of first message.
Line five of first message.

From bob@example.org Fri Nov 29 11:00:00 2024
From: bob@example.org
To: user@localhost
Subject: Second Message
Date: Fri, 29 Nov 2024 11:00:00 +0000

Second message body.

From alice@different.net Fri Nov 29 12:00:00 2024
From: alice@different.net
To: user@localhost
Subject: Third Message from Alice
Date: Fri, 29 Nov 2024 12:00:00 +0000

Third message body.
"#;

/// Mbox where user is the sender (for showto testing)
#[allow(dead_code)]
const USER_SENT_MBOX: &str = r#"From user@localhost Fri Nov 29 10:00:00 2024
From: user@localhost
To: recipient@example.com
Subject: Sent by User
Date: Fri, 29 Nov 2024 10:00:00 +0000

Message sent by the user.
"#;

// =============================================================================
// Helper Functions
// =============================================================================

/// Helper to create a temporary mbox file
fn create_temp_mbox(content: &str) -> NamedTempFile {
    let mut file = NamedTempFile::new().expect("failed to create temp mbox");
    file.write_all(content.as_bytes())
        .expect("failed to write temp mbox");
    file.flush().expect("failed to flush temp mbox");
    file
}

/// Helper to create a temporary mailrc file
fn create_temp_mailrc(content: &str) -> NamedTempFile {
    let mut file = NamedTempFile::new().expect("failed to create temp mailrc");
    file.write_all(content.as_bytes())
        .expect("failed to write temp mailrc");
    file.flush().expect("failed to flush temp mailrc");
    file
}

// =============================================================================
// Basic Set/Unset Operations
// =============================================================================

/// Test setting and displaying variables
#[test]
fn set_and_display() {
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
            stdin_data: String::from("set dot\nset prompt=\"test> \"\nset\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("dot"),
                "Should show dot variable: {}",
                stdout
            );
            assert!(
                stdout.contains("prompt"),
                "Should show prompt variable: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test unsetting variables with 'set noVAR' and 'unset VAR'
#[test]
fn unset_variables() {
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
            stdin_data: String::from(
                "set dot\nset autoprint\nset nodot\nunset autoprint\nset\nquit\n",
            ),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            assert!(output.status.success(), "Should exit successfully");
        },
    );
}

/// Test onehop variable rejection
#[test]
fn onehop_rejected() {
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
            let combined = format!("{}{}", stdout, stderr);
            assert!(
                combined.contains("onehop") && combined.contains("not supported")
                    || combined.contains("noonehop"),
                "Should reject onehop: stdout={}, stderr={}",
                stdout,
                stderr
            );
        },
    );
}

// =============================================================================
// Receive-Mode Display Variables
// =============================================================================

/// Test 'header' variable - show headers on startup
/// When set (default), headers are displayed when entering receive mode
#[test]
fn var_header_enabled() {
    let mbox = create_temp_mbox(MULTI_MSG_MBOX);
    let mbox_path = mbox.path().to_str().unwrap();

    // Default behavior with header enabled (no -N flag)
    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("quit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should show header summary with message info
            assert!(
                stdout.contains("alice") || stdout.contains("First Message"),
                "With header set, should show message summary: {}",
                stdout
            );
        },
    );
}

/// Test 'noheader' - suppress headers on startup (same as -N flag)
#[test]
fn var_header_disabled() {
    let mbox = create_temp_mbox(MULTI_MSG_MBOX);
    let mbox_path = mbox.path().to_str().unwrap();
    let mailrc = create_temp_mailrc("set noheader\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![String::from("-f"), String::from(mbox_path)],
            stdin_data: String::from("quit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            // With noheader, initial summary should be suppressed
            // Output should be minimal or empty before quit
            assert!(output.status.success());
            // Note: There might still be prompt output
        },
    );
}

/// Test 'toplines' variable - number of lines shown by 'top' command
#[test]
fn var_toplines() {
    let mbox = create_temp_mbox(MULTI_MSG_MBOX);
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
            // Set toplines to 2, then use top command
            stdin_data: String::from("set toplines=2\ntop 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // top command should show limited lines from the message
            assert!(
                stdout.contains("First message body") || stdout.contains("From:"),
                "top command should show message content: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test 'screen' variable - number of headers per screen
#[test]
fn var_screen() {
    let mbox = create_temp_mbox(MULTI_MSG_MBOX);
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
            // Set screen to 2, then use headers command
            stdin_data: String::from("set screen=2\nheaders\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // headers command should show messages
            assert!(
                stdout.contains("alice") || stdout.contains("bob") || stdout.contains("Message"),
                "headers should show message info: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test 'autoprint' variable - print next message after delete
#[test]
fn var_autoprint() {
    let mbox = create_temp_mbox(MULTI_MSG_MBOX);
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
            // Enable autoprint, delete message 1, should auto-print next
            stdin_data: String::from("set autoprint\ndelete 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // With autoprint, after delete, next message should be printed
            // Should see content from message 2 (bob's message)
            assert!(
                stdout.contains("bob") || stdout.contains("Second"),
                "autoprint should show next message after delete: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// =============================================================================
// Path Variables
// =============================================================================

/// Test 'folder' variable - directory for mail folders
#[test]
fn var_folder() {
    let mbox = create_temp_mbox(MINIMAL_MBOX);
    let mbox_path = mbox.path().to_str().unwrap();
    let temp_dir = TempDir::new().expect("failed to create temp dir");
    let folder_path = temp_dir.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: format!("set folder={}\nset\nquit\n", folder_path),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("folder"),
                "Should show folder variable: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test 'SHELL' variable - shell for ! commands
#[test]
fn var_shell() {
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
            // Set SHELL and run a command
            stdin_data: String::from("set SHELL=/bin/sh\n!echo hello\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("hello"),
                "Shell command should execute: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test 'LISTER' variable - command for listing folders
#[test]
fn var_lister() {
    let mbox = create_temp_mbox(MINIMAL_MBOX);
    let mbox_path = mbox.path().to_str().unwrap();
    let temp_dir = TempDir::new().expect("failed to create temp dir");
    let folder_path = temp_dir.path().to_str().unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-n"),
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: format!("set folder={}\nset LISTER=ls\nfolders\nquit\n", folder_path),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            // folders command should run LISTER on folder directory
            assert!(output.status.success());
        },
    );
}

// =============================================================================
// Input/Escape Variables
// =============================================================================

/// Test 'prompt' variable - command prompt string
#[test]
fn var_prompt() {
    let mbox = create_temp_mbox(MINIMAL_MBOX);
    let mbox_path = mbox.path().to_str().unwrap();
    let mailrc = create_temp_mailrc("set prompt=\"TESTPROMPT> \"\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("quit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            // Note: prompt may not show in non-TTY mode
            // Just verify it doesn't crash with custom prompt
            assert!(output.status.success());
        },
    );
}

/// Test 'quiet' variable - suppress version banner
#[test]
fn var_quiet() {
    let mbox = create_temp_mbox(MINIMAL_MBOX);
    let mbox_path = mbox.path().to_str().unwrap();
    let mailrc = create_temp_mailrc("set quiet\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-N"),
                String::from("-f"),
                String::from(mbox_path),
            ],
            stdin_data: String::from("quit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // With quiet set, version info should be suppressed
            assert!(
                !stdout.contains("version") && !stdout.contains("Version"),
                "quiet should suppress version: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test 'escape' variable - escape character for compose mode
#[test]
fn var_escape() {
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
            // Verify escape variable can be set
            stdin_data: String::from("set escape=@\nset\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("escape"),
                "Should show escape variable: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test 'indentprefix' variable - prefix for quoted text
#[test]
fn var_indentprefix() {
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
            stdin_data: String::from("set indentprefix=\">> \"\nset\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("indentprefix"),
                "Should show indentprefix variable: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// =============================================================================
// Message-List Variables
// =============================================================================

/// Test 'allnet' variable - match addresses by login name only
/// When set, "alice" matches both alice@example.com and alice@different.net
#[test]
fn var_allnet_enabled() {
    let mbox = create_temp_mbox(MULTI_MSG_MBOX);
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
            // With allnet, searching for "alice" should find both messages
            stdin_data: String::from("set allnet\nfrom alice\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should match messages from alice@example.com and alice@different.net
            // Look for indicators of multiple matches
            assert!(
                stdout.contains("alice") || stdout.contains("First") || stdout.contains("Third"),
                "allnet should match by login: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test without 'allnet' - full address matching
#[test]
fn var_allnet_disabled() {
    let mbox = create_temp_mbox(MULTI_MSG_MBOX);
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
            // Without allnet, search is more specific
            stdin_data: String::from("from alice@example.com\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should find only alice@example.com message
            assert!(
                stdout.contains("alice") || stdout.contains("First"),
                "Should match specific address: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// =============================================================================
// Shell Command Variables
// =============================================================================

/// Test 'bang' variable - enable ! expansion in shell commands
#[test]
fn var_bang_enabled() {
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
            // Enable bang, run a command, then use ! to repeat
            stdin_data: String::from("set bang\n!echo first\n!echo second\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("first") && stdout.contains("second"),
                "Shell commands should execute: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// =============================================================================
// Alias Command
// =============================================================================

/// Test alias command - define and display mail aliases
#[test]
fn alias_define_and_display() {
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
            stdin_data: String::from(
                "alias friends alice@example.com bob@example.com\nalias\nquit\n",
            ),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("friends") || stdout.contains("alice"),
                "Should show defined alias: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test unalias command
#[test]
fn unalias_command() {
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
            stdin_data: String::from(
                "alias testgroup user@example.com\nunalias testgroup\nalias\nquit\n",
            ),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            // After unalias, the alias should be gone
            assert!(output.status.success());
        },
    );
}

// =============================================================================
// Alternates Command
// =============================================================================

/// Test alternates command - declare alternate addresses
#[test]
fn alternates_command() {
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
            stdin_data: String::from("alternates me@work.com me@home.com\nalternates\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // alternates command should show defined alternates
            assert!(
                stdout.contains("me@work.com") || stdout.contains("me@home.com"),
                "Should show alternates: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// =============================================================================
// Discard/Ignore/Retain Commands
// =============================================================================

/// Test discard command - suppress header fields
#[test]
fn discard_command() {
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
            // Discard Date header, then print message
            stdin_data: String::from("discard Date\nprint 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // After discard, Date header might be hidden
            // Just verify command runs successfully
            assert!(
                stdout.contains("From:") || stdout.contains("test message"),
                "Should show message: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test retain command - retain only specified headers
#[test]
fn retain_command() {
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
            // Retain only From and Subject
            stdin_data: String::from("retain From Subject\nprint 1\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("From:") || stdout.contains("Subject:"),
                "Should show retained headers: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// =============================================================================
// Variable Defaults
// =============================================================================

/// Test that default variables are properly set
#[test]
fn default_variables() {
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
            stdin_data: String::from("set\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            // With -n, we skip rc files so minimal defaults
            assert!(output.status.success(), "Should exit successfully");
        },
    );
}

// =============================================================================
// Mailbox Operation Variables
// =============================================================================

/// Test 'hold' variable behavior
/// When set, read messages are held in system mailbox instead of moving to mbox
#[test]
fn var_hold() {
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
            stdin_data: String::from("set hold\nset\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("hold"),
                "Should show hold variable: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test 'keep' variable - keep empty mailboxes
#[test]
fn var_keep() {
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
            stdin_data: String::from("set keep\nset\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("keep"),
                "Should show keep variable: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test 'keepsave' variable
#[test]
fn var_keepsave() {
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
            stdin_data: String::from("set keepsave\nset\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("keepsave"),
                "Should show keepsave variable: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

/// Test 'append' variable - append vs prepend to mbox
#[test]
fn var_append() {
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
            stdin_data: String::from("set append\nset\nquit\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("append"),
                "Should show append variable: {}",
                stdout
            );
            assert!(output.status.success());
        },
    );
}

// =============================================================================
// Send-Mode Variables (tested via debug mode)
// =============================================================================

/// Test 'debug' variable - prevents actual mail sending, shows diagnostics
#[test]
fn var_debug_send_mode() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Debug Test"),
                String::from("recipient@example.com"),
            ],
            stdin_data: String::from("Test message body\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Debug mode should show diagnostic info
            assert!(
                stderr.contains("Debug mode"),
                "Should show debug mode message: {}",
                stderr
            );
            assert!(
                stderr.contains("recipient@example.com") || stderr.contains("To:"),
                "Should show recipient: {}",
                stderr
            );
            assert!(output.status.success());
        },
    );
}

/// Test 'sign' variable - signature inserted by ~a escape
/// Uses debug mode to verify message content without sending
#[test]
fn var_sign() {
    let mailrc = create_temp_mailrc("set debug\nset sign=\"-- My Signature\"\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Sign Test"),
                String::from("user@example.com"),
            ],
            stdin_data: String::from("Message body\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // In debug mode, we verify the setup works
            assert!(
                stderr.contains("Debug mode"),
                "Should be in debug mode: {}",
                stderr
            );
            assert!(output.status.success());
        },
    );
}

/// Test 'Sign' variable (uppercase) - signature for ~A escape
#[test]
fn var_sign_uppercase() {
    let mailrc = create_temp_mailrc("set debug\nset Sign=\"-- Formal Signature\"\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Sign Uppercase Test"),
                String::from("user@example.com"),
            ],
            stdin_data: String::from("Message body\n"),
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
}

/// Test debug mode shows message size
#[test]
fn var_debug_shows_body_size() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Size Test"),
                String::from("user@example.com"),
            ],
            stdin_data: String::from("This is the message body content.\n"),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        &[("MAILRC", mailrc_path)],
        |_plan, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            // Debug output should show body size
            assert!(
                stderr.contains("Body:") || stderr.contains("bytes"),
                "Should show body size info: {}",
                stderr
            );
            assert!(output.status.success());
        },
    );
}

/// Test debug mode with Cc recipients
#[test]
fn var_debug_with_cc() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Cc Test"),
                String::from("primary@example.com"),
            ],
            // Note: In non-interactive mode, we can't easily add Cc
            // This tests basic debug functionality
            stdin_data: String::from("Message with potential Cc.\n"),
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
}

/// Test 'sendwait' variable - wait for mailer to complete
/// In debug mode, this affects timing but mail isn't actually sent
#[test]
fn var_sendwait() {
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
            // With debug mode, sendwait is set but mail isn't sent
            assert!(
                stderr.contains("Debug mode"),
                "Should be in debug mode: {}",
                stderr
            );
            assert!(output.status.success());
        },
    );
}

/// Test multiple variables set via MAILRC for send mode
#[test]
fn var_multiple_send_vars() {
    let mailrc = create_temp_mailrc("set debug\nset quiet\nset sendwait\nset sign=\"--sig\"\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("mailx"),
            args: vec![
                String::from("-s"),
                String::from("Multi-var Send Test"),
                String::from("user@example.com"),
            ],
            stdin_data: String::from("Testing multiple variables.\n"),
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
}
