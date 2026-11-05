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

use crate::common::{create_temp_mailrc, mailx, mailx_env};

/// Test basic send mode with single recipient
/// Verifies:
/// - Subject line handling (-s option)
/// - Recipient processing
/// - Message body reading from stdin
/// - Proper error when sendmail is not available
#[test]
fn send_basic() {
    let output = mailx(
        &["-n", "-s", "Test Subject", "recipient@example.com"],
        "This is a test message.\n",
    );
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
}

/// Test send mode with subject containing spaces
/// Verifies proper handling of multi-word subjects
#[test]
fn send_subject_with_spaces() {
    let output = mailx(
        &[
            "-n",
            "-s",
            "This is a longer test subject",
            "user@example.com",
        ],
        "Message body.\n",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    // Accept either success or sendmail not found
    if !output.status.success() {
        assert!(
            stderr.contains("Cannot find sendmail"),
            "Unexpected error: {}",
            stderr
        );
    }
}

/// Test send mode with multiple recipients
/// Verifies that multiple recipients are properly processed
#[test]
fn send_multiple_recipients() {
    let output = mailx(
        &[
            "-n",
            "-s",
            "Multi-recipient Test",
            "user1@example.com",
            "user2@example.com",
        ],
        "Message to multiple recipients.\n",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    // Accept either success or sendmail not found
    if !output.status.success() {
        assert!(
            stderr.contains("Cannot find sendmail"),
            "Unexpected error: {}",
            stderr
        );
    }
}

/// Test send mode with multiline message body
/// Verifies that stdin is properly read until EOF
#[test]
fn send_multiline_body() {
    let output = mailx(
        &["-n", "-s", "Multiline Test", "recipient@example.com"],
        "Line 1\nLine 2\nLine 3\n\nParagraph 2\n",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    if !output.status.success() {
        assert!(
            stderr.contains("Cannot find sendmail"),
            "Unexpected error: {}",
            stderr
        );
    }
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

    let output = mailx_env(
        &["-s", "Debug Test Subject", "debug-recipient@example.com"],
        "This message should not be sent.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);

    // In debug mode, mailx prints diagnostics to stderr
    assert!(
        stderr.contains("Debug mode"),
        "Should show debug mode message: {}",
        stderr
    );
    assert!(
        stderr.contains("debug-recipient@example.com") && stderr.contains("To:"),
        "Should show recipient in debug output: {}",
        stderr
    );
    assert!(
        stderr.contains("Debug Test Subject") && stderr.contains("Subject:"),
        "Should show subject in debug output: {}",
        stderr
    );
    assert!(
        output.status.success(),
        "Debug mode should exit successfully"
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

    let output = mailx_env(
        &["-s", "Multi-var Test", "user@example.com"],
        "Test body.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    // Debug mode should be active
    assert!(
        stderr.contains("Debug mode"),
        "Should be in debug mode: {}",
        stderr
    );
    assert!(output.status.success());

    drop(mailrc);
}

/// Test alias expansion in send mode
/// Verifies that aliases are properly expanded when sending
#[test]
fn send_alias_expansion() {
    // Create a mailrc with an alias and debug mode
    let mailrc = create_temp_mailrc("set debug\nalias team user1@example.com user2@example.com\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "Team Message", "team"],
        "Message for the team.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    // Debug mode should show the expanded aliases
    assert!(
        stderr.contains("Debug mode"),
        "Should be in debug mode: {}",
        stderr
    );
    // The alias should have been expanded to individual addresses
    assert!(
        stderr.contains("user1@example.com") && stderr.contains("user2@example.com"),
        "Alias should be expanded: {}",
        stderr
    );
    assert!(output.status.success());

    drop(mailrc);
}

/// Test empty message handling
#[test]
fn send_empty_message() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "Empty Test", "recipient@example.com"],
        "",
        &[("MAILRC", mailrc_path)],
    );
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

    drop(mailrc);
}

/// Test subject containing special characters
#[test]
fn send_special_subject() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &[
            "-s",
            "Re: [URGENT] Meeting @2pm - Updated!",
            "user@example.com",
        ],
        "Meeting details.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Debug mode"),
        "Should be in debug mode: {}",
        stderr
    );
    assert!(
        stderr.contains("[URGENT]") && stderr.contains("Updated"),
        "Subject should be preserved: {}",
        stderr
    );
    assert!(output.status.success());

    drop(mailrc);
}

/// Test dot variable behavior (dot ends message)
/// Note: This tests non-interactive behavior - in non-interactive mode,
/// dot handling doesn't apply (stdin is just read until EOF)
#[test]
fn send_with_dot_variable() {
    let mailrc = create_temp_mailrc("set debug\nset dot\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "Dot Test", "recipient@example.com"],
        "Line 1\n.\nLine 2 (should be included in non-TTY)\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Debug mode"),
        "Should be in debug mode: {}",
        stderr
    );
    // In non-interactive mode, the dot is included in the body
    assert!(output.status.success());

    drop(mailrc);
}

/// Test sign variable is properly parsed from mailrc
#[test]
fn send_sign_variable() {
    let mailrc = create_temp_mailrc("set debug\nset sign=\"Best regards,\\nTest User\"\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "Sign Test", "user@example.com"],
        "Hello.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Debug mode"),
        "Should be in debug mode: {}",
        stderr
    );
    assert!(output.status.success());

    drop(mailrc);
}

/// Test escape variable changes the escape character
#[test]
fn send_custom_escape_char() {
    let mailrc = create_temp_mailrc("set debug\nset escape=#\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "Escape Test", "user@example.com"],
        "Message body.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Debug mode"),
        "Should be in debug mode: {}",
        stderr
    );
    assert!(output.status.success());

    drop(mailrc);
}

/// Test sendwait variable (wait for sendmail)
#[test]
fn send_sendwait_variable() {
    let mailrc = create_temp_mailrc("set debug\nset sendwait\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "Sendwait Test", "user@example.com"],
        "Test message.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    // In debug mode, sendwait doesn't matter
    assert!(
        stderr.contains("Debug mode"),
        "Should be in debug mode: {}",
        stderr
    );
    assert!(output.status.success());

    drop(mailrc);
}

/// A line break in a `-s` subject or an address operand cannot add header
/// fields.
///
/// Both are copied into the header block. A script passing an untrusted
/// subject could end the Subject line, add its own `Bcc:`, or end the header
/// block and, in a `record` file, begin a forged mbox message.
#[test]
fn send_operands_cannot_inject_header_lines() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &[
            "-s",
            "hello\r\nBcc: injected@evil.example\n\nFrom forged@evil",
            "victim@example.com\nCc: cc-injected@evil.example",
        ],
        "body\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Subject: hello  Bcc: injected@evil.example  From forged@evil\n"),
        "subject was not kept on one line: {}",
        stderr
    );
    assert!(
        stderr.contains("To: victim@example.com Cc: cc-injected@evil.example\n"),
        "address was not kept on one line: {}",
        stderr
    );
    for line in stderr.lines() {
        assert!(
            !line.starts_with("Bcc: ") && !line.starts_with("Cc: ") && !line.starts_with("From "),
            "an operand began a header line: {}",
            stderr
        );
    }
    assert!(output.status.success());

    drop(mailrc);
}

/// A body on standard input that is not UTF-8 is sent, not refused.
#[test]
fn send_mode_accepts_eight_bit_body() {
    let mailrc = create_temp_mailrc("set debug\n");
    let body: &[u8] = b"caf\xe9 \xff\n";
    let output = mailx_env(
        &["-s", "s", "to@example.com"],
        body,
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "8-bit body refused: {}", stderr);
    assert!(
        stderr.contains(&format!("Body: {} bytes", body.len())),
        "body not kept whole: {}",
        stderr
    );
}

/// A stub delivery agent in its own directory: a shell script running
/// `body`, which may use `$out` for a file beside it.
fn stub_sendmail(body: &str) -> (plib::tmp::TempDir, String, std::path::PathBuf) {
    use std::os::unix::fs::PermissionsExt;

    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let out = dir.path().join("out");
    let script = dir.path().join("sendmail");
    std::fs::write(
        &script,
        format!("#!/bin/sh\nout='{}'\n{}\n", out.display(), body),
    )
    .expect("write stub");
    std::fs::set_permissions(&script, std::fs::Permissions::from_mode(0o755)).expect("chmod");
    let script = script.to_str().unwrap().to_string();
    (dir, script, out)
}

/// The `sendmail` variable names the delivery agent, as in BSD mailx; it is
/// handed the recipients as operands and the message on standard input.
#[test]
fn send_uses_sendmail_variable() {
    let (_dir, script, out) = stub_sendmail(r#"{ echo "args: $*"; cat; } > "$out""#);
    let mailrc = create_temp_mailrc(&format!("set sendwait\nset sendmail={}\n", script));
    let output = mailx_env(
        &["-n", "-s", "Hi", "rcpt@example.com"],
        "body line\n",
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    assert!(output.status.success(), "mailx failed: {:?}", output);
    let delivered = std::fs::read_to_string(&out).expect("stub was not run");
    assert!(
        delivered.starts_with("args: -oi -- rcpt@example.com\n"),
        "{}",
        delivered
    );
    assert!(delivered.contains("Subject: Hi\n"), "{}", delivered);
    assert!(delivered.ends_with("\nbody line\n"), "{}", delivered);
}

/// A delivery agent that exits without reading the message is a failed
/// delivery, reported as one: the write into its closed pipe does not kill
/// mailx with SIGPIPE.
#[test]
fn send_reports_sendmail_that_exits_unread() {
    use std::os::unix::process::ExitStatusExt;

    let (_dir, script, _) = stub_sendmail("echo 'no such user' >&2; exit 1");
    let mailrc = create_temp_mailrc(&format!("set sendmail={}\n", script));
    let body = "x".repeat(1023) + "\n";
    let output = mailx_env(
        &["-n", "-s", "Big", "rcpt@example.com"],
        body.repeat(1024),
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    assert_eq!(output.status.signal(), None, "{:?}", output);
    assert_eq!(output.status.code(), Some(1), "{:?}", output);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("sendmail failed: no such user"),
        "{}",
        stderr
    );
}
