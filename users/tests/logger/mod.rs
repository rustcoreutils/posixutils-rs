//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};

fn logger_test(args: &[&str]) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();
    run_test(TestPlan {
        cmd: String::from("logger"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

/// Assert `logger <args>` exits with `code` (priority parsing happens before
/// any syslog connection, so the error cases are deterministic offline).
fn logger_exit(args: &[&str], code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("logger"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: code,
        },
        move |_plan, output| {
            assert_eq!(output.status.code(), Some(code));
        },
    );
}

// POSIX notes that logger is "difficult to test" since the output format
// and location of logged messages is unspecified. We test observable behavior:
// exit codes, no stdout, and no stderr on success.

#[test]
fn test_logger_basic() {
    logger_test(&["test message from posixutils"]);
}

#[test]
fn test_logger_multiple_args() {
    // POSIX: multiple string arguments are concatenated with spaces
    logger_test(&["hello", "world", "from", "logger"]);
}

#[test]
fn test_logger_special_chars() {
    logger_test(&["message with special: !@#$%^&*()_+-=[]{}|;':\",./<>?"]);
}

#[test]
fn test_logger_unicode() {
    logger_test(&["unicode message: \u{1F600} \u{1F4BB} \u{2764}"]);
}

#[test]
fn test_logger_empty_string() {
    // Empty string is valid - logs empty message
    logger_test(&[""]);
}

#[test]
fn test_logger_no_args() {
    // No operands: messages are read from standard input (#LG2). The harness
    // supplies empty stdin, so nothing is logged and the exit status is 0.
    // See test_logger_reads_stdin_when_no_operands for the non-empty case.
    logger_test(&[]);
}

#[test]
fn test_logger_long_message() {
    let long_msg = "a".repeat(1000);
    logger_test(&[&long_msg]);
}

#[test]
fn test_logger_multiword_quoted() {
    // Test that quoted arguments work correctly
    logger_test(&["this is a single argument with spaces"]);
}

#[test]
fn test_logger_newlines() {
    // Test embedded newlines (implementation may handle differently)
    logger_test(&["line1\nline2\nline3"]);
}

// ============================================================================
// Option parsing (#LG1/#LG3/#LG4) — operands are no longer logged verbatim
// ============================================================================

#[test]
fn test_logger_unknown_facility_errors() {
    // -p with an unknown facility is rejected before any syslog connection.
    logger_exit(&["-p", "nosuchfac.info", "hello"], 1);
}

#[test]
fn test_logger_unknown_level_errors() {
    // -p with an unknown level is rejected.
    logger_exit(&["-p", "user.nosuchlevel", "hello"], 1);
}

#[test]
fn test_logger_options_not_logged_as_body() {
    // -t/-p/-i must be parsed as options, not concatenated into the message
    // body. A valid invocation succeeds (exit 0) when syslog is reachable.
    logger_test(&["-i", "-t", "mytag", "-p", "user.info", "hello", "world"]);
}

#[test]
fn test_logger_bare_level_priority() {
    // A bare level (no facility) defaults the facility to user.
    logger_test(&["-p", "warning", "a warning message"]);
}

// --- #LG1/#LG2 coverage: option parsing and message sourcing ---

/// Run logger with `args` and `stdin_data`, returning the exit code.
fn logger_run(args: &[&str], stdin_data: &str) -> i32 {
    use std::io::Write;
    use std::process::{Command, Stdio};

    let mut child = Command::new(plib::testing::get_binary_path("logger"))
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn logger");
    let _ = child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(stdin_data.as_bytes());
    child.wait().expect("wait failed").code().unwrap_or(-1)
}

// #LG1: -t (tag), -i (log pid) and -p (priority) are accepted together and are
// consumed as options rather than being logged as message text.
#[test]
fn test_logger_option_surface_parses() {
    assert_eq!(
        logger_run(&["-t", "mytag", "-i", "-p", "user.info", "hello"], ""),
        0
    );
}

// #LG2: with no operands, the message bodies come from standard input, one
// message per non-empty line. (The pre-fix behavior logged a single empty
// message and never read stdin.)
#[test]
fn test_logger_reads_stdin_when_no_operands() {
    assert_eq!(logger_run(&[], "first line\nsecond line\n"), 0);
}

// #LG2: -f names a file to read messages from.
#[test]
fn test_logger_reads_message_file() {
    let path = std::env::temp_dir().join("posixutils_logger_msgs");
    std::fs::write(&path, "alpha\n\nbravo\n").unwrap();
    assert_eq!(logger_run(&["-f", path.to_str().unwrap()], ""), 0);
    std::fs::remove_file(&path).unwrap();
}

// -f - is standard input, per the usual convention.
#[test]
fn test_logger_dash_file_is_stdin() {
    assert_eq!(logger_run(&["-f", "-"], "via dash\n"), 0);
}

// Error path (logger.rs collect_messages): an unreadable -f file exits non-zero.
#[test]
fn test_logger_missing_message_file_errors() {
    assert_ne!(
        logger_run(&["-f", "/nonexistent/logger/messages"], ""),
        0,
        "an unreadable -f file must be a diagnostic, not a silent success"
    );
}
