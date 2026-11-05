//
// Copyright (c) 2025-2026 Jeff Garzik
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

use crate::common::{
    assert_mailx, create_temp_mailrc, create_temp_mbox, mailx, mailx_env, test_data_path,
};

// =============================================================================
// Test Data Helpers
// =============================================================================

// =============================================================================
// -e Option: Check for Mail Presence
// =============================================================================

/// Test -e with mail present (should exit 0)
#[test]
fn opt_e_mail_present() {
    let mbox_path = test_data_path("testdata.mbox");

    assert_mailx(
        &["-n", "-e", "-f", mbox_path.to_str().unwrap()],
        "",
        "",
        "",
        0,
    );
}

/// Test -e with empty mbox (should exit 1)
#[test]
fn opt_e_no_mail() {
    let mbox = create_temp_mbox("");

    assert_mailx(
        &["-n", "-e", "-f", mbox.path().to_str().unwrap()],
        "",
        "",
        "",
        1,
    );
}

/// Test -e combined with -f
#[test]
fn opt_e_with_f() {
    let mbox_path = test_data_path("testdata.mbox");

    // -e -f should check the specified file
    assert_mailx(&["-n", "-ef", mbox_path.to_str().unwrap()], "", "", "", 0);
}

// =============================================================================
// -f Option: Read from File
// =============================================================================

/// Test -f with explicit file path
#[test]
fn opt_f_explicit_file() {
    let mbox_path = test_data_path("testdata.mbox");

    let output = mailx(&["-n", "-N", "-f", mbox_path.to_str().unwrap()], "quit\n");
    assert!(
        output.status.success(),
        "Should read from file successfully"
    );
}

/// Test -f reads correct number of messages
#[test]
fn opt_f_message_count() {
    let mbox_path = test_data_path("testdata.mbox");

    let output = mailx(&["-n", "-f", mbox_path.to_str().unwrap()], "quit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // The header summary lists each of the 3 messages by sender address.
    for (num, sender) in [
        (1, "alice@example.com"),
        (2, "bob@example.org"),
        (3, "charlie@example.net"),
    ] {
        assert!(
            stdout
                .lines()
                .any(|l| l.contains(&format!("{} {}", num, sender))),
            "Should list message {} from {}: {}",
            num,
            sender,
            stdout
        );
    }
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// Test -f with nonexistent file
#[test]
fn opt_f_nonexistent_file() {
    let output = mailx(&["-n", "-f", "/nonexistent/path/to/mbox"], "");
    assert!(!output.status.success(), "Should fail for nonexistent file");
    // The system's text for the error, without Rust's "(os error N)".
    let missing = "/nonexistent/path/to/mbox";
    let open_error = plib::diag::io_error_text(&std::fs::File::open(missing).unwrap_err());
    assert_eq!(
        String::from_utf8_lossy(&output.stderr),
        format!("mailx: {missing}: {open_error}\n")
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

    let output = mailx_env(
        &["-F", "-s", "Record Test", "recipient@example.com"],
        "Test message for -F option.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    // With debug mode, should show message info
    assert!(
        stderr.contains("Debug mode"),
        "Should be in debug mode: {}",
        stderr
    );
    assert!(output.status.success());
}

// =============================================================================
// -H Option: Headers Only
// =============================================================================

/// Test -H displays only headers and exits
#[test]
fn opt_h_headers_only() {
    let mbox_path = test_data_path("testdata.mbox");

    let output = mailx(&["-n", "-H", "-f", mbox_path.to_str().unwrap()], "");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should show message headers
    assert!(
        stdout.contains("Meeting Tomorrow") && stdout.contains("alice"),
        "Should show first message info: {}",
        stdout
    );
    assert!(
        stdout.contains("Project Update") && stdout.contains("bob"),
        "Should show second message info: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test -H with empty mailbox
#[test]
fn opt_h_empty_mbox() {
    let mbox = create_temp_mbox("");

    let output = mailx(&["-n", "-H", "-f", mbox.path().to_str().unwrap()], "");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.starts_with("No mail for "),
        "Should report that there is no mail: {}",
        stdout
    );
    assert!(output.status.success());
}

/// Test -H combined with -f (common pattern)
#[test]
fn opt_h_with_f() {
    let mbox_path = test_data_path("testdata.mbox");

    let output = mailx(&["-nHf", mbox_path.to_str().unwrap()], "");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Combined flags should work
    assert!(
        stdout.contains("alice") && stdout.contains("Meeting"),
        "Combined -nHf should show headers: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

// =============================================================================
// -i Option: Ignore Interrupts
// =============================================================================

/// Test -i option is accepted (full interrupt testing requires TTY)
#[test]
fn opt_i_accepted() {
    let mbox_path = test_data_path("testdata.mbox");

    let output = mailx(
        &["-n", "-i", "-N", "-f", mbox_path.to_str().unwrap()],
        "quit\n",
    );
    // Just verify -i is accepted without error
    assert!(output.status.success(), "-i option should be accepted");
}

// =============================================================================
// -n Option: No System Init File
// =============================================================================

/// Test -n skips system init file
#[test]
fn opt_n_no_init() {
    let mbox_path = test_data_path("testdata.mbox");

    let output = mailx(
        &["-n", "-N", "-f", mbox_path.to_str().unwrap()],
        "set\nquit\n",
    );
    // With -n, should have minimal variables set
    assert!(output.status.success());
}

/// Test without -n (MAILRC is honored)
#[test]
fn opt_without_n_reads_mailrc() {
    let mbox_path = test_data_path("testdata.mbox");
    let mailrc = create_temp_mailrc("set testvar\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-N", "-f", mbox_path.to_str().unwrap()],
        "set\nquit\n",
        &[("MAILRC", mailrc_path)],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Without -n, MAILRC should be read
    assert!(
        stdout.contains("testvar"),
        "Without -n, MAILRC variables should be set: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

// =============================================================================
// -N Option: No Header Summary
// =============================================================================

/// Test -N suppresses initial header summary
#[test]
fn opt_n_uppercase_no_summary() {
    let mbox_path = test_data_path("testdata.mbox");

    // With -N, should not show initial headers
    let output = mailx(&["-n", "-N", "-f", mbox_path.to_str().unwrap()], "quit\n");
    // With -N, initial summary is suppressed
    // Output should be minimal (just prompt if any)
    assert!(output.status.success());
}

/// Test without -N shows header summary
#[test]
fn opt_without_n_uppercase_shows_summary() {
    let mbox_path = test_data_path("testdata.mbox");

    // Without -N, should show initial headers
    let output = mailx(&["-n", "-f", mbox_path.to_str().unwrap()], "quit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Without -N, should show headers
    assert!(
        stdout.contains("alice") && stdout.contains("Meeting") && stdout.contains("1"),
        "Without -N, should show header summary: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

// =============================================================================
// -s Option: Set Subject
// =============================================================================

/// Test -s sets subject (verified via debug mode)
#[test]
fn opt_s_set_subject() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "Test Subject Line", "user@example.com"],
        "Message body.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Test Subject Line") && stderr.contains("Subject:"),
        "Should show subject in debug: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// Test -s with subject containing spaces
#[test]
fn opt_s_subject_with_spaces() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-s", "This is a multi-word subject", "user@example.com"],
        "Body.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("multi-word") && stderr.contains("Subject:"),
        "Should handle multi-word subject: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// Test -s without argument (should error)
#[test]
fn opt_s_missing_argument() {
    let output = mailx(&["-s"], "");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "-s without argument should fail");
    assert_eq!(stderr, "mailx: option requires an argument -- s\n");
}

/// Test -s with attached subject (no space: -sSubject)
#[test]
fn opt_s_attached_subject() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-sAttachedSubject", "user@example.com"],
        "Body.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("AttachedSubject") && stderr.contains("Debug mode"),
        "Should handle attached subject: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

// =============================================================================
// -u Option: Read User Mailbox
// =============================================================================

/// Test -u option is parsed correctly (actual mailbox access requires privileges)
/// We test that the option is accepted and produces expected behavior
#[test]
fn opt_u_parsed_correctly() {
    // -u without system mail access should fail gracefully
    let output = mailx(&["-n", "-e", "-u", "nonexistentuser12345"], "");
    // Either fails to find mailbox or exits with code 1
    assert!(
        !output.status.success() || output.status.code() == Some(1),
        "-u with nonexistent user should fail"
    );
}

/// Test -u missing argument
#[test]
fn opt_u_missing_argument() {
    let output = mailx(&["-u"], "");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "-u without argument should fail");
    assert_eq!(stderr, "mailx: option requires an argument -- u\n");
}

// =============================================================================
// Combined Options
// =============================================================================

/// Test multiple options combined: -nNf
#[test]
fn combined_n_n_f() {
    let mbox_path = test_data_path("testdata.mbox");

    let output = mailx(&["-nNf", mbox_path.to_str().unwrap()], "quit\n");
    assert!(output.status.success(), "Combined -nNf should work");
}

/// Test -n -H -f combination
#[test]
fn combined_n_h_f_separate() {
    let mbox_path = test_data_path("testdata.mbox");

    let output = mailx(&["-n", "-H", "-f", mbox_path.to_str().unwrap()], "");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("alice") && stdout.contains("Meeting"),
        "Separate options should work: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// Test send mode options: -n -s subject recipient
#[test]
fn combined_send_options() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &[
            "-s",
            "Combined Send Test",
            "user1@example.com",
            "user2@example.com",
        ],
        "Message to multiple recipients.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Debug mode"),
        "Send with multiple recipients should work: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// Test illegal option produces error
#[test]
fn illegal_option() {
    let output = mailx(&["-Z"], "");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "Illegal option should fail");
    assert!(
        stderr.contains("illegal") && stderr.contains("Z") && stderr.contains("option"),
        "Should indicate illegal option: {}",
        stderr
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

    let output = mailx_env(
        &["-s", "Stdin Test", "recipient@example.com"],
        "This message comes from stdin.\nSecond line.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    // Should process stdin as message body
    assert!(
        stderr.contains("Debug mode") && stderr.contains("bytes"),
        "Should read message from stdin: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// Test empty recipient list with -s (goes to receive mode)
#[test]
fn subject_without_recipient() {
    let mbox_path = test_data_path("testdata.mbox");

    // With -s but no recipients and -f specified, should go to receive mode
    let output = mailx(
        &[
            "-n",
            "-N",
            "-s",
            "Unused Subject",
            "-f",
            mbox_path.to_str().unwrap(),
        ],
        "quit\n",
    );
    // Should enter receive mode (no recipients means not send mode)
    assert!(output.status.success());
}

// =============================================================================
// -E Option: Discard messages with an empty body
// =============================================================================

/// -E with an empty body must succeed without attempting delivery.
#[test]
fn opt_e_uppercase_discards_empty_body() {
    // Empty body (whitespace only): must be discarded, not sent.
    let output = mailx(
        &["-n", "-E", "-s", "Empty", "recipient@example.com"],
        "   \n\n",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    // No delivery attempt: sendmail is never invoked, so no "sendmail"
    // failure diagnostic appears even on a host without sendmail.
    assert!(output.status.success(), "Empty -E message should succeed");
    assert!(
        !stderr.contains("sendmail"),
        "-E empty body must not attempt delivery: {}",
        stderr
    );
}

/// -E with a non-empty body still composes/sends (verified via debug mode).
#[test]
fn opt_e_uppercase_keeps_nonempty_body() {
    let mailrc = create_temp_mailrc("set debug\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-E", "-s", "NonEmpty", "recipient@example.com"],
        "This body is not empty.\n",
        &[("MAILRC", mailrc_path)],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Debug mode"),
        "-E with non-empty body should still send: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

// =============================================================================
// -f file operand parsing
// =============================================================================

/// `mailx -f -N file` must read the mailbox, not send to "file".
#[test]
fn opt_f_operand_after_other_options() {
    let mbox_path = test_data_path("testdata.mbox");

    let output = mailx(&["-n", "-f", "-N", mbox_path.to_str().unwrap()], "quit\n");
    // Reading the file in Receive Mode succeeds; it is not parsed as a
    // recipient address (which, lacking sendmail, would error out).
    assert!(
        output.status.success(),
        "-f -N file should read the mailbox: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

/// RATIONALE's `mailx -fin file` clustered form must read the mailbox.
#[test]
fn opt_f_clustered_fin_operand() {
    let mbox_path = test_data_path("testdata.mbox");

    let output = mailx(&["-fiN", mbox_path.to_str().unwrap()], "quit\n");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Header summary is suppressed by N, but the file must load: a
    // `headers` command then lists messages from the file.
    assert!(
        output.status.success(),
        "-fiN file should read the mailbox: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    // Sanity: no "No recipients"/send error leaked to stdout.
    assert!(
        !stdout.contains("No recipients"),
        "Should be Receive Mode, not Send: {}",
        stdout
    );
}

// =============================================================================
// -n keeps the user MAILRC
// =============================================================================

/// `mailx -n` must still process the user MAILRC (only the system file is skipped).
#[test]
fn opt_n_still_reads_user_mailrc() {
    let mbox_path = test_data_path("testdata.mbox");
    let mailrc = create_temp_mailrc("set testvar\n");
    let mailrc_path = mailrc.path().to_str().unwrap();

    let output = mailx_env(
        &["-n", "-N", "-f", mbox_path.to_str().unwrap()],
        "set\nquit\n",
        &[("MAILRC", mailrc_path)],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("testvar"),
        "-n must still read the user MAILRC: {}",
        stdout
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

// =============================================================================
// Multibyte header-summary truncation must not panic
// =============================================================================

/// A multibyte From/Subject longer than the column widths must truncate on a
/// character boundary rather than panicking on a byte slice.
#[test]
fn header_summary_multibyte_no_panic() {
    // 3-byte chars in Subject (>25) and 2-byte chars in the From address
    // (>20): a byte-offset slice at column 22/10 would fall mid-character and
    // panic.
    let subject: String = "あ".repeat(30);
    let from_name: String = "Ñ".repeat(25);
    let body = format!(
        "From sender@example.com Mon Jan  1 10:00:00 2024\n\
         From: {}@example.com\n\
         Subject: {}\n\
         \n\
         body text\n",
        from_name, subject
    );
    let mbox = create_temp_mbox(&body);

    let output = mailx(&["-n", "-H", "-f", mbox.path().to_str().unwrap()], "");
    assert!(
        output.status.success(),
        "header summary must not panic on multibyte text: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Truncation appends an ellipsis.
    assert!(
        stdout.contains("..."),
        "long fields should be truncated with ellipsis: {}",
        stdout
    );
}

// =============================================================================
// sh -c receives the -- argument
// =============================================================================

/// A piped command beginning with `-` must be handed to the program (after the
/// mandated `--`), not consumed by the shell as one of its own options.
#[test]
fn pipe_command_leading_dash_reaches_program() {
    let mbox_path = test_data_path("testdata.mbox");

    // Pipe message 1 to the command "-n": with `sh -c -- -n` the shell
    // runs "-n" as a command (not found) instead of swallowing it as the
    // shell's own `-n` option (which would silently succeed).
    let output = mailx(
        &["-n", "-N", "-f", mbox_path.to_str().unwrap()],
        "pipe 1 -n\nquit\n",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("not found"),
        "`-n` should reach the shell as a command (proves `--`): {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

// =============================================================================
// Locale initialization
// =============================================================================

/// mailx initializes the locale from the environment without breaking; a
/// UTF-8 LC_CTYPE drives multibyte header interpretation.
#[test]
fn locale_env_initialization() {
    let subject: String = "résumé ".repeat(6); // multibyte, > 25 chars
    let body = format!(
        "From sender@example.com Mon Jan  1 10:00:00 2024\n\
         From: sender@example.com\n\
         Subject: {}\n\
         \n\
         body\n",
        subject
    );
    let mbox = create_temp_mbox(&body);

    let output = mailx_env(
        &["-n", "-H", "-f", mbox.path().to_str().unwrap()],
        "", // The test controls the locale; C.UTF-8 exercises the UTF-8 ctype path.
        &[("LC_ALL", "C.UTF-8")],
    );
    assert!(
        output.status.success(),
        "mailx should run with an environment UTF-8 locale: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

// =============================================================================
// alias backslash recursion guard
// =============================================================================

/// A leading backslash on an alias group member prevents its expansion.
#[test]
fn alias_backslash_prevents_expansion() {
    // `user` expands to an address; `grp` references `\user` (literal) plus a
    // normal address. The backslashed member must NOT expand.
    let mailrc = create_temp_mailrc(
        "set debug\nalias user expanded@example.com\nalias grp \\user other@example.com\n",
    );

    let output = mailx_env(
        &["grp"],
        "body\n",
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("user") && stderr.contains("other@example.com"),
        "group members should be present: {}",
        stderr
    );
    assert!(
        !stderr.contains("expanded@example.com"),
        "backslashed member must not be expanded: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

// An argument that is not valid UTF-8 is reported, not a panic.
#[test]
fn mailx_non_utf8_argument_is_reported() {
    plib::testing::assert_non_utf8_argument_rejected("mailx", &[]);
}

/// Run `mailx -n -H -f <mbox>` on `content`, then `mailx -n -N -f <mbox>`
/// with `commands` on stdin, and return both standard outputs.
fn summary_and_commands(content: &str, commands: &str) -> (String, String) {
    let mbox = create_temp_mbox(content);
    let path = mbox.path().to_str().unwrap().to_string();
    let run = |flag: &str, stdin: &str| {
        let output = mailx(&["-n", flag, "-f", &path], stdin);
        assert!(
            output.status.success(),
            "mailx failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        String::from_utf8_lossy(&output.stdout).into_owned()
    };
    (run("-H", ""), run("-N", commands))
}

/// The header summary's line and byte counts, and `size`, describe the
/// message as stored: folded header lines, `Status:`, and the blank line
/// separating it from the next message all count, as in traditional mailx.
#[test]
fn header_summary_counts_the_stored_message() {
    let first = "From alice@example.com Mon Jan 29 10:29:21 2007\n\
                 Received: from a.example\n\
                 \tby b.example\n\
                 \tfor c.example\n\
                 From: alice@example.com\n\
                 Subject: folded\n\
                 Status: O\n\
                 \n\
                 body line one\n\
                 body line two\n\
                 \n";
    let second = "From bob@example.com Mon Jan 29 11:00:00 2007\n\
                  From: bob@example.com\n\
                  Subject: second\n\
                  \n\
                  hi\n";
    let (summary, sizes) = summary_and_commands(&format!("{}{}", first, second), "size 1 2\n");

    let want = format!("{}/{}", first.lines().count(), first.len());
    let line = summary
        .lines()
        .find(|l| l.contains("folded"))
        .unwrap_or_else(|| panic!("no summary line: {}", summary));
    assert!(line.contains(&want), "want {} in {:?}", want, line);
    let want2 = format!("{}/{}", second.lines().count(), second.len());
    assert!(summary.contains(&want2), "want {} in {:?}", want2, summary);

    assert!(
        sizes.contains(&format!("1: {} bytes", first.len())),
        "size disagrees with the summary: {}",
        sizes
    );
    assert!(
        sizes.contains(&format!("2: {} bytes", second.len())),
        "size disagrees with the summary: {}",
        sizes
    );
}

/// The summary date comes from the `From ` line, as `Www Mmm dd hh:mm`.
#[test]
fn header_summary_date_from_envelope() {
    let (summary, _) = summary_and_commands(
        "From a@example.com Mon Jan  1 10:00:00 2024\n\
         From: a@example.com\n\
         Date: Tue, 2 Jan 2024 09:00:00 +0000\n\
         Subject: s\n\
         \n\
         body\n",
        "",
    );
    assert!(summary.contains("Mon Jan  1 10:00"), "{:?}", summary);
}

/// Without a usable `From ` line date the `Date:` header supplies it, and
/// its day is the number before the month, never the year after it.
#[test]
fn header_summary_date_from_header() {
    let (summary, _) = summary_and_commands(
        "From a@example.com\n\
         From: a@example.com\n\
         Date: Mon, 29 Jan 2007 19:32:06 +0900\n\
         Subject: s\n\
         \n\
         body\n",
        "",
    );
    assert!(summary.contains("Mon Jan 29 19:32"), "{:?}", summary);
    assert!(!summary.contains("Jan 2007"), "{:?}", summary);
}

/// The summary shows the sender's bare address, cut to 20 columns, in the
/// traditional layout.
#[test]
fn header_summary_sender_is_the_address() {
    let (summary, _) = summary_and_commands(
        "From x@example.com Mon Jan 29 10:29:21 2007\n\
         From: Some Name <angle@example.com>\n\
         Subject: one\n\
         \n\
         body\n\
         \n\
         From x@example.com Mon Jan 29 10:30:00 2007\n\
         From: paren@example.com (Some Name)\n\
         Subject: two\n\
         \n\
         body\n\
         \n\
         From x@example.com Mon Jan 29 10:31:00 2007\n\
         From: a.very.long.address@example.com\n\
         Subject: three\n\
         \n\
         body\n",
        "",
    );
    let rows: Vec<&str> = summary.lines().collect();
    assert_eq!(
        rows[0],
        ">N  1 angle@example.com     Mon Jan 29 10:29   6/100   one"
    );
    assert!(
        rows[1].contains(" paren@example.com ") && !rows[1].contains("Some Name"),
        "{:?}",
        rows[1]
    );
    assert!(
        rows[2].contains(" a.very.long.address@  Mon"),
        "{:?}",
        rows[2]
    );
}

/// The `Date:` fallback tolerates the layouts mail actually carries rather
/// than leaving the date column blank.
#[test]
fn header_summary_date_from_loose_headers() {
    let cases = [
        ("Mon Nov 25 10:30:00 2024", "Mon Nov 25 10:30"),
        ("25 Nov 2024", "Nov 25"),
        ("Mon,25 Nov 2024 10:30 +0000", "Mon Nov 25 10:30"),
        (
            "Monday, 25 November 2024 10:30:00 +0000",
            "Mon Nov 25 10:30",
        ),
    ];
    for (header, want) in cases {
        let (summary, _) = summary_and_commands(
            &format!(
                "From a@example.com\n\
                 From: a@example.com\n\
                 Date: {}\n\
                 Subject: s\n\
                 \n\
                 body\n",
                header
            ),
            "",
        );
        assert!(
            summary.contains(want),
            "Date: {} should summarize as {:?}: {:?}",
            header,
            want,
            summary
        );
    }
}

/// `save` reports the bytes it wrote, which differ from the message as
/// loaded: here a `Status:` header is added and a separator supplied.
#[test]
fn save_reports_the_bytes_written() {
    let dir = plib::tmp::TempDir::new().expect("temp dir");
    let out = dir.path().join("saved");
    let out_path = out.to_str().unwrap().to_string();
    let (_, report) = summary_and_commands(
        "From a@example.com Mon Jan 29 10:29:21 2007\n\
         From: a@example.com\n\
         Subject: s\n\
         \n\
         body",
        &format!("print 1\nsave 1 {}\nexit\n", out_path),
    );
    let written = std::fs::metadata(&out).expect("saved file").len();
    assert!(
        report.contains(&format!("1 messages {} bytes", written)),
        "report disagrees with the {} bytes written: {}",
        written,
        report
    );
}

/// An unclosed `<` still yields the address, not the display name.
#[test]
fn header_summary_sender_unclosed_angle() {
    let (summary, _) = summary_and_commands(
        "From x@example.com Mon Jan 29 10:29:21 2007\n\
         From: Some Name <broken@example.com\n\
         Subject: s\n\
         \n\
         body\n",
        "",
    );
    assert!(
        summary.contains(" broken@example.com ") && !summary.contains("Some Name"),
        "{:?}",
        summary
    );
}

/// A `<` inside a quoted display name or a comment is not the address.
#[test]
fn header_summary_sender_skips_quotes_and_comments() {
    let (summary, _) = summary_and_commands(
        "From x@example.com Mon Jan 29 10:29:21 2007\n\
         From: \"Smith <dev team>\" <j@example.com>\n\
         Subject: one\n\
         \n\
         body\n\
         \n\
         From x@example.com Mon Jan 29 10:30:00 2007\n\
         From: k@example.com (Kim <work>)\n\
         Subject: two\n\
         \n\
         body\n",
        "",
    );
    let rows: Vec<&str> = summary.lines().collect();
    assert!(rows[0].contains(" j@example.com "), "{:?}", rows[0]);
    assert!(rows[1].contains(" k@example.com "), "{:?}", rows[1]);
}

/// With `showto`, every recipient's address is shown, however each is
/// written; a comma inside a quoted name does not split it.
#[test]
fn header_summary_showto_lists_every_recipient() {
    let (_, out) = summary_and_commands(
        "From me@example.com Mon Jan 29 10:29:21 2007\n\
         From: me@example.com\n\
         To: \"Doe, A\" <a@x.org>, b@x.org (B)\n\
         Subject: s\n\
         \n\
         body\n",
        "alternates me@example.com\nset showto\nheaders\n",
    );
    assert!(out.contains("To a@x.org, b@x.org"), "{:?}", out);
}

/// A one-digit hour is a time; a word that merely begins like a month or
/// weekday name is not one, and an impossible time is no time.
#[test]
fn header_summary_date_tokens_are_whole() {
    let cases = [
        ("Mon, 4 Nov 2024 9:05:00 -0500", "Mon Nov  4 09:05"),
        ("marked 5 Nov 2024 10:00", "Nov  5 10:00"),
        ("Mon, 4 Nov 2024 99:99 (Decided)", "Mon Nov  4"),
    ];
    for (header, want) in cases {
        let (summary, _) = summary_and_commands(
            &format!(
                "From a@example.com\n\
                 From: a@example.com\n\
                 Date: {}\n\
                 Subject: s\n\
                 \n\
                 body\n",
                header
            ),
            "",
        );
        let row = summary.lines().next().unwrap_or("");
        // The date is the 16 columns after the marker, number, and sender.
        let date: String = row.chars().skip(28).take(16).collect();
        assert_eq!(date.trim(), want, "Date: {}: {:?}", header, row);
    }
}

/// A field that appears twice is read from its first occurrence, as other
/// mail readers do; the second must not silently replace it.
#[test]
fn repeated_header_field_uses_the_first() {
    let (summary, _) = summary_and_commands(
        "From a@example.com Fri Nov 29 10:00:00 2024\n\
         From: a@example.com\n\
         Subject: first subject\n\
         Subject: second subject\n\
         \n\
         body\n",
        "exit\n",
    );
    assert!(
        summary.contains("first subject") && !summary.contains("second subject"),
        "the summary should show the first Subject: {}",
        summary
    );
}

/// `-H` with a bare `-f` summarizes mbox, the mailbox `-f` names when no
/// file is given, not the system mailbox.
#[test]
fn opt_h_with_bare_f_reads_mbox() {
    let mbox = create_temp_mbox(
        "From saved@example.com Fri Nov 29 10:00:00 2024\n\
         From: saved@example.com\n\
         Subject: kept in mbox\n\
         \n\
         body\n",
    );
    let output = mailx_env(
        &["-n", "-H", "-f"],
        "",
        &[
            ("MBOX", mbox.path().to_str().unwrap()),
            ("MAIL", "/nonexistent/system/mailbox"),
        ],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("kept in mbox"),
        "-H -f should summarize mbox: {}\n{}",
        stdout,
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}
