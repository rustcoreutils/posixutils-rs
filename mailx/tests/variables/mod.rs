//
// Copyright (c) 2025-2026 Jeff Garzik
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
//!
//! Test data files:
//! - testdata.mbox: 3 messages (alice, bob, charlie)
//! - testdata-single.mbox: 1 message (alice)
//! - testdata-5msg.mbox: 5 messages

use crate::common::{copy_test_data, create_temp_mailrc, mailx, mailx_env};
use plib::tmp::TempDir;

// =============================================================================
// Helper Functions
// =============================================================================

// =============================================================================
// Basic Set/Unset Operations
// =============================================================================

/// Test setting and displaying variables
#[test]
fn set_and_display() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "set dot\nset prompt=\"test> \"\nset\nquit\n",
    );
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
}

/// Test unsetting variables with 'set noVAR' and 'unset VAR'
#[test]
fn unset_variables() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "set dot\nset autoprint\nset nodot\nunset autoprint\nset\nquit\n",
    );
    assert!(output.status.success(), "Should exit successfully");
}

/// Test onehop variable rejection
#[test]
fn onehop_rejected() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "set onehop\nquit\n");
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
    assert_eq!(output.status.code(), Some(0), "exit status");
}

// =============================================================================
// Receive-Mode Display Variables
// =============================================================================

/// Test 'header' variable - show headers on startup
/// When set (default), headers are displayed when entering receive mode
#[test]
fn var_header_enabled() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Default behavior with header enabled (no -N flag)
    let output = mailx(&["-n", "-f", mbox_path], "quit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should show header summary with message info
    assert!(
        stdout.contains("alice") && stdout.contains("Meeting Tomorrow"),
        "With header set, should show message summary: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// Test 'noheader' - suppress headers on startup (same as -N flag)
#[test]
fn var_header_disabled() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();
    let mailrc = create_temp_mailrc("set noheader\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(&["-f", mbox_path], "quit\n", &[("MAILRC", mailrc_path)]);
    // With noheader, initial summary should be suppressed
    // Output should be minimal or empty before quit
    assert!(output.status.success());
    // Note: There might still be prompt output
}

/// Test 'toplines' variable - number of lines shown by 'top' command
#[test]
fn var_toplines() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Set toplines to 2, then use top command
    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "set toplines=2\ntop 1\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // The body begins "Hi,", "", "Just a reminder ..."; two lines stop short.
    assert!(
        stdout.contains("Hi,") && !stdout.contains("reminder"),
        "top should show only the first two body lines: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test 'screen' variable - number of headers per screen
#[test]
fn var_screen() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Set screen to 2, then use headers command
    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "set screen=2\nheaders\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stdout_lower = stdout.to_lowercase();
    // headers command should show messages
    assert!(
        stdout_lower.contains("alice")
            || stdout_lower.contains("bob")
            || stdout_lower.contains("meeting"),
        "headers should show message info: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test 'autoprint' variable - print next message after delete
#[test]
fn var_autoprint() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Enable autoprint, delete message 1, should auto-print next
    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "set autoprint\ndelete 1\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // With autoprint, after delete, next message should be printed
    // Should see content from message 2 (bob's message)
    assert!(
        stdout.contains("Subject: Project Update") && stdout.contains("The project is on track."),
        "autoprint should show next message after delete: {}",
        stdout
    );
    assert!(output.status.success());
}

// =============================================================================
// Path Variables
// =============================================================================

/// Test 'folder' variable - directory for mail folders
#[test]
fn var_folder() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();
    let temp_dir = TempDir::new().expect("failed to create temp dir");
    let folder_path = temp_dir.path().to_str().unwrap();

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        format!("set folder={}\nset\nquit\n", folder_path),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("folder"),
        "Should show folder variable: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test 'SHELL' variable - shell for ! commands
#[test]
fn var_shell() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Set SHELL and run a command
    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "set SHELL=/bin/sh\n!echo hello\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("hello"),
        "Shell command should execute: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test 'LISTER' variable - command for listing folders
#[test]
fn var_lister() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();
    let temp_dir = TempDir::new().expect("failed to create temp dir");
    let folder_path = temp_dir.path().to_str().unwrap();

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        format!("set folder={}\nset LISTER=ls\nfolders\nquit\n", folder_path),
    );
    // folders command should run LISTER on folder directory
    assert!(output.status.success());
}

// =============================================================================
// Input/Escape Variables
// =============================================================================

/// Test 'prompt' variable - command prompt string
#[test]
fn var_prompt() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();
    let mailrc = create_temp_mailrc("set prompt=\"TESTPROMPT> \"\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-N", "-f", mbox_path],
        "quit\n",
        &[("MAILRC", mailrc_path)],
    );
    // Note: prompt may not show in non-TTY mode
    // Just verify it doesn't crash with custom prompt
    assert!(output.status.success());
}

/// Test 'quiet' variable - suppress version banner
#[test]
fn var_quiet() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();
    let mailrc = create_temp_mailrc("set quiet\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-N", "-f", mbox_path],
        "quit\n",
        &[("MAILRC", mailrc_path)],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // With quiet set, version info should be suppressed
    assert!(
        !stdout.contains("version") && !stdout.contains("Version"),
        "quiet should suppress version: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test 'escape' variable - escape character for compose mode
#[test]
fn var_escape() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Verify escape variable can be set
    let output = mailx(&["-n", "-N", "-f", mbox_path], "set escape=@\nset\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("escape"),
        "Should show escape variable: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test 'indentprefix' variable - prefix for quoted text
#[test]
fn var_indentprefix() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "set indentprefix=\">> \"\nset\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("indentprefix"),
        "Should show indentprefix variable: {}",
        stdout
    );
    assert!(output.status.success());
}

// =============================================================================
// Message-List Variables
// =============================================================================

/// Test 'allnet' variable - match addresses by login name only
/// When set, "alice" matches both alice@example.com and alice@different.net
#[test]
fn var_allnet_enabled() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // With allnet, searching for "alice" should find both messages
    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "set allnet\nfrom alice\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should match messages from alice@example.com
    assert!(
        stdout.contains("alice") && stdout.contains("Meeting"),
        "allnet should match by login: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test without 'allnet' - full address matching
#[test]
fn var_allnet_disabled() {
    let mbox = copy_test_data("testdata.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Without allnet, search is more specific
    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "from alice@example.com\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should find only alice@example.com message
    assert!(
        stdout.contains("alice") && stdout.contains("Meeting"),
        "Should match specific address: {}",
        stdout
    );
    assert!(output.status.success());
}

// =============================================================================
// Shell Command Variables
// =============================================================================

/// Test 'bang' variable - enable ! expansion in shell commands
#[test]
fn var_bang_enabled() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Enable bang, run a command, then use ! to repeat
    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "set bang\n!echo first\n!echo second\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("first") && stdout.contains("second"),
        "Shell commands should execute: {}",
        stdout
    );
    assert!(output.status.success());
}

// =============================================================================
// Alias Command
// =============================================================================

/// Test alias command - define and display mail aliases
#[test]
fn alias_define_and_display() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "alias friends alice@example.com bob@example.com\nalias\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("friends") && stdout.contains("alice"),
        "Should show defined alias: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test unalias command
#[test]
fn unalias_command() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "alias testgroup user@example.com\nunalias testgroup\nalias\nquit\n",
    );
    // After unalias, the alias should be gone
    assert!(output.status.success());
}

// =============================================================================
// Alternates Command
// =============================================================================

/// Test alternates command - declare alternate addresses
#[test]
fn alternates_command() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "alternates me@work.com me@home.com\nalternates\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // alternates command should show defined alternates
    assert!(
        stdout.contains("me@work.com") && stdout.contains("me@home.com"),
        "Should show alternates: {}",
        stdout
    );
    assert!(output.status.success());
}

// =============================================================================
// Discard/Ignore/Retain Commands
// =============================================================================

/// Test discard command - suppress header fields
#[test]
fn discard_command() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Discard Date header, then print message
    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "discard Date\nprint 1\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Subject: Meeting Tomorrow") && !stdout.contains("Date:"),
        "discard should hide only the Date header: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test retain command - retain only specified headers
#[test]
fn retain_command() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    // Retain only From and Subject
    let output = mailx(
        &["-n", "-N", "-f", mbox_path],
        "retain From Subject\nprint 1\nquit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("From:") && stdout.contains("Subject:"),
        "Should show retained headers: {}",
        stdout
    );
    assert!(output.status.success());
}

// =============================================================================
// Variable Defaults
// =============================================================================

/// Test that default variables are properly set
#[test]
fn default_variables() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "set\nquit\n");
    // With -n, we skip rc files so minimal defaults
    assert!(output.status.success(), "Should exit successfully");
}

// =============================================================================
// Mailbox Operation Variables
// =============================================================================

/// Test 'hold' variable behavior
/// When set, read messages are held in system mailbox instead of moving to mbox
#[test]
fn var_hold() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "set hold\nset\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("hold"),
        "Should show hold variable: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test 'keep' variable - keep empty mailboxes
#[test]
fn var_keep() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "set keep\nset\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("keep"),
        "Should show keep variable: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test 'keepsave' variable
#[test]
fn var_keepsave() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "set keepsave\nset\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("keepsave"),
        "Should show keepsave variable: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test 'append' variable - append vs prepend to mbox
#[test]
fn var_append() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap();

    let output = mailx(&["-n", "-N", "-f", mbox_path], "set append\nset\nquit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("append"),
        "Should show append variable: {}",
        stdout
    );
    assert!(output.status.success());
}

// =============================================================================
// Send-Mode Variables (tested via debug mode)
// =============================================================================

/// Test 'debug' variable - prevents actual mail sending, shows diagnostics
#[test]
fn var_debug_send_mode() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "Debug Test", "recipient@example.com"],
        "Test message body\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    // Debug mode should show diagnostic info
    assert!(
        stderr.contains("Debug mode"),
        "Should show debug mode message: {}",
        stderr
    );
    assert!(
        stderr.contains("recipient@example.com") && stderr.contains("To:"),
        "Should show recipient: {}",
        stderr
    );
    assert!(output.status.success());
}

/// Test 'sign' variable - signature inserted by ~a escape
/// Uses debug mode to verify message content without sending
#[test]
fn var_sign() {
    let mailrc = create_temp_mailrc("set debug\nset sign=\"-- My Signature\"\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "Sign Test", "user@example.com"],
        "Message body\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    // In debug mode, we verify the setup works
    assert!(
        stderr.contains("Debug mode"),
        "Should be in debug mode: {}",
        stderr
    );
    assert!(output.status.success());
}

/// Test 'Sign' variable (uppercase) - signature for ~A escape
#[test]
fn var_sign_uppercase() {
    let mailrc = create_temp_mailrc("set debug\nset Sign=\"-- Formal Signature\"\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "Sign Uppercase Test", "user@example.com"],
        "Message body\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Debug mode"),
        "Should be in debug mode: {}",
        stderr
    );
    assert!(output.status.success());
}

/// Test debug mode shows message size
#[test]
fn var_debug_shows_body_size() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "Size Test", "user@example.com"],
        "This is the message body content.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    // Debug output should show body size
    assert!(
        stderr.contains("Body:") && stderr.contains("bytes"),
        "Should show body size info: {}",
        stderr
    );
    assert!(output.status.success());
}

/// Test debug mode with Cc recipients
#[test]
fn var_debug_with_cc() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    // Note: In non-interactive mode, we can't easily add Cc
    // This tests basic debug functionality
    let output = mailx_env(
        &["-s", "Cc Test", "primary@example.com"],
        "Message with potential Cc.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Debug mode"),
        "Should be in debug mode: {}",
        stderr
    );
    assert!(output.status.success());
}

/// Test 'sendwait' variable - wait for mailer to complete
/// In debug mode, this affects timing but mail isn't actually sent
#[test]
fn var_sendwait() {
    let mailrc = create_temp_mailrc("set debug\nset sendwait\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "Sendwait Test", "user@example.com"],
        "Test message.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    // With debug mode, sendwait is set but mail isn't sent
    assert!(
        stderr.contains("Debug mode"),
        "Should be in debug mode: {}",
        stderr
    );
    assert!(output.status.success());
}

/// Test multiple variables set via MAILRC for send mode
#[test]
fn var_multiple_send_vars() {
    let mailrc = create_temp_mailrc("set debug\nset quiet\nset sendwait\nset sign=\"--sig\"\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "Multi-var Send Test", "user@example.com"],
        "Testing multiple variables.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Debug mode"),
        "Should be in debug mode: {}",
        stderr
    );
    assert!(output.status.success());
}

/// A `set` value may contain blanks when quoted (spec 104965, 104694-104699).
#[test]
fn set_value_may_contain_blanks() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    let output = mailx(
        &["-n", "-N", "-f", &mbox_path],
        "set indentprefix=\"> \"\nset\nexit\n",
    );
    let out = String::from_utf8_lossy(&output.stdout);
    assert!(
        out.contains("indentprefix=\"> \""),
        "a quoted set value lost its trailing blank: {}",
        out
    );
    assert!(
        !out.lines().any(|l| l.trim() == "\""),
        "the set tokenizer produced a junk variable named `\"`: {}",
        out
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `set noX` unsets `X`, and must not strip `no` from an unrelated name.
#[test]
fn set_no_prefix_only_strips_a_known_boolean() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    let output = mailx(&["-n", "-N", "-f", &mbox_path], "set notify\nset\nexit\n");
    let out = String::from_utf8_lossy(&output.stdout);
    assert!(
        out.lines().any(|l| l.trim() == "notify"),
        "`set notify` was read as `set no tify`: {}",
        out
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `set noX` clears X for any variable that is set, not just a known boolean.
#[test]
fn set_no_prefix_clears_valued_and_user_variables() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    let output = mailx(
        &["-n", "-N", "-f", &mbox_path],
        "set crt=1\nset nocrt\nset myvar=1\nset nomyvar\nset notify\nset\nexit\n",
    );
    let out = String::from_utf8_lossy(&output.stdout);
    for gone in ["crt=", "myvar=", "nocrt", "nomyvar"] {
        assert!(
            !out.contains(gone),
            "`set no...` left `{}` behind: {}",
            gone,
            out
        );
    }
    assert!(
        out.lines().any(|l| l.trim() == "notify"),
        "`set notify` was read as `set no tify`: {}",
        out
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// A variable has one value: `set name` after `set name=value` replaces the
/// value with null rather than adding a second, boolean, `name`.
#[test]
fn a_variable_holds_one_value() {
    let mbox = copy_test_data("testdata-single.mbox");
    let output = mailx(
        &["-n", "-N", "-f", mbox.path().to_str().unwrap()],
        "set color=red\nset color\nset\nset shade\nset shade=blue\nset\nexit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let listings: Vec<&str> = stdout
        .lines()
        .filter(|l| l.starts_with("color") || l.starts_with("shade"))
        .collect();
    // First listing: `color` (null). Second: `color` and `shade="blue"`.
    assert_eq!(
        listings,
        ["color", "color", "shade=\"blue\""],
        "each variable should be listed once, with its latest value: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// A variable that is tested for being set counts as set whatever value it
/// was given: `set autoprint=yes` turns autoprint on.
#[test]
fn a_valued_variable_is_set() {
    let mbox = copy_test_data("testdata.mbox");
    let output = mailx(
        &["-n", "-N", "-f", mbox.path().to_str().unwrap()],
        "set autoprint=yes\ndelete 1\nexit\n",
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Subject: Project Update"),
        "autoprint=yes should print the next message after delete: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// `set escape` gives `escape` a null value, which disables command escapes
/// just as `set escape=` does (`name` and `name=` are equivalent).
#[test]
fn set_escape_without_value_disables_escapes() {
    let mbox = copy_test_data("testdata.mbox");
    let mailrc = create_temp_mailrc("set debug\nset dot\nset escape\n");
    let output = mailx_env(
        &["-N", "-f", mbox.path().to_str().unwrap()],
        "mail recipient@example.com\n~x\nbody\n.\nquit\n",
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Debug mode"),
        "with escapes disabled ~x is text, so the message is sent: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}
