//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{get_binary_path, run_test, run_test_u8, TestPlan, TestPlanU8};
use std::ffi::OsStr;
use std::fs::File;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::process::Command;

fn echo_test(args: &[&str], expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("echo"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

fn echo_test_bytes(args: &[&str], expected_output: &[u8]) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_u8(TestPlanU8 {
        cmd: String::from("echo"),
        args: str_args,
        stdin_data: Vec::new(),
        expected_out: expected_output.to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_echo_no_args() {
    // No arguments - only newline is written
    echo_test(&[], "\n");
}

#[test]
fn test_echo_basic() {
    echo_test(&["hello"], "hello\n");
    echo_test(&["big", "brown", "bear"], "big brown bear\n");
    echo_test(&["hello", "world"], "hello world\n");
}

#[test]
fn test_echo_suppress_newline_n() {
    // -n as first argument suppresses newline (BSD behavior, implementation-defined)
    echo_test(&["-n", "foo", "bar"], "foo bar");
    echo_test(&["-n", "hello"], "hello");
    echo_test(&["-n"], "");
}

#[test]
fn test_echo_suppress_newline_c() {
    // \c suppresses newline and ignores following characters
    echo_test(&["foo", "bar\\c"], "foo bar");
    echo_test(&["hello\\c", "world"], "hello");
    echo_test(&["abc\\cdef"], "abc");
}

#[test]
fn test_echo_escape_alert() {
    // \a - alert (bell)
    echo_test(&["hello\\aworld"], "hello\x07world\n");
}

#[test]
fn test_echo_escape_backspace() {
    // \b - backspace
    echo_test(&["hello\\bworld"], "hello\x08world\n");
}

#[test]
fn test_echo_escape_formfeed() {
    // \f - form-feed
    echo_test(&["hello\\fworld"], "hello\x0cworld\n");
}

#[test]
fn test_echo_escape_newline() {
    // \n - newline
    echo_test(&["hello\\nworld"], "hello\nworld\n");
}

#[test]
fn test_echo_escape_carriage_return() {
    // \r - carriage-return
    echo_test(&["hello\\rworld"], "hello\rworld\n");
}

#[test]
fn test_echo_escape_tab() {
    // \t - tab
    echo_test(&["hello\\tworld"], "hello\tworld\n");
}

#[test]
fn test_echo_escape_vertical_tab() {
    // \v - vertical-tab
    echo_test(&["hello\\vworld"], "hello\x0bworld\n");
}

#[test]
fn test_echo_escape_backslash() {
    // \\ - literal backslash
    echo_test(&["hello\\\\world"], "hello\\world\n");
}

#[test]
fn test_echo_octal_escape_zero() {
    // \0 with no digits - NUL character
    echo_test(&["hello\\0world"], "hello\x00world\n");
}

#[test]
fn test_echo_octal_escape_one_digit() {
    // \0 followed by 1 octal digit
    echo_test(&["\\07"], "\x07\n"); // bell
    echo_test(&["\\01"], "\x01\n");
}

#[test]
fn test_echo_octal_escape_two_digits() {
    // \0 followed by 2 octal digits
    echo_test(&["\\012"], "\n\n"); // \012 = newline
    echo_test(&["\\041"], "!\n"); // \041 = '!'
}

#[test]
fn test_echo_octal_escape_three_digits() {
    // \0 followed by 3 octal digits
    echo_test(&["\\0101"], "A\n"); // \0101 = 'A' (65 decimal)
    echo_test(&["\\0141"], "a\n"); // \0141 = 'a' (97 decimal)
                                   // \0377 = 255 (max byte) - use byte comparison since 0xFF is not valid UTF-8
    echo_test_bytes(&["\\0377"], &[0xff, b'\n']);
}

#[test]
fn test_echo_octal_non_octal_terminates() {
    // Non-octal digit terminates octal sequence
    echo_test(&["\\018"], "\x018\n"); // '8' is not octal, so \01 = 1, then '8'
    echo_test(&["\\0789"], "\x0789\n"); // \07 = 7, '8' and '9' are literal
}

#[test]
fn test_echo_unknown_escape() {
    // Sequences POSIX does not define are emitted verbatim, backslash included
    // (matching every other widespread implementation).
    echo_test(&["hello\\xworld"], "hello\\xworld\n");
    echo_test(&["\\q"], "\\q\n");
    echo_test(&["\\1"], "\\1\n"); // \1 without leading 0 is not a sequence
}

#[test]
fn test_echo_trailing_backslash() {
    // Trailing backslash is preserved
    echo_test(&["hello\\"], "hello\\\n");
}

#[test]
fn test_echo_double_dash() {
    // POSIX: "--" shall be recognized as a string operand, not option terminator
    echo_test(&["--"], "--\n");
    echo_test(&["--", "hello"], "-- hello\n");
    echo_test(&["hello", "--", "world"], "hello -- world\n");
}

#[test]
fn test_echo_multiple_escapes() {
    // Multiple escape sequences in one argument
    echo_test(&["\\t\\t\\t"], "\t\t\t\n");
    echo_test(&["a\\nb\\nc"], "a\nb\nc\n");
}

#[test]
fn test_echo_mixed_content() {
    // Mix of regular text and escapes
    echo_test(&["Name:\\tJohn\\nAge:\\t30"], "Name:\tJohn\nAge:\t30\n");
}

#[test]
fn test_echo_empty_string() {
    // Empty string argument
    echo_test(&[""], "\n");
    echo_test(&["", ""], " \n");
}

#[test]
fn test_echo_octal_escape_out_of_range() {
    // Audit #E3: three octal digits can name a value above 377.  The
    // accumulator must not overflow; the low 8 bits are kept.
    echo_test_bytes(&["\\0777"], &[0xff, b'\n']);
    echo_test_bytes(&["\\0400"], &[0x00, b'\n']);
    // A fourth octal digit is an ordinary character.
    echo_test_bytes(&["\\01011"], b"A1\n");
}

#[test]
fn test_echo_non_utf8_operand() {
    // Audit #E1: operands are byte strings.  A non-UTF-8 operand must be
    // written through unchanged, not abort the process.
    let arg = OsStr::from_bytes(&[0xff, 0xfe]);
    let output = Command::new(get_binary_path("echo"))
        .arg(arg)
        .output()
        .expect("failed to run echo");

    assert_eq!(output.status.code(), Some(0));
    assert_eq!(output.stdout, vec![0xff, 0xfe, b'\n']);
    assert!(output.stderr.is_empty());
}

#[test]
fn test_echo_non_utf8_operand_mixed() {
    // Byte-faithful operands survive joining and escape expansion.
    let output = Command::new(get_binary_path("echo"))
        .arg("-n")
        .arg(OsStr::from_bytes(b"a\xffb"))
        .arg(OsStr::from_bytes(b"\\tc\xfe"))
        .output()
        .expect("failed to run echo");

    assert_eq!(output.status.code(), Some(0));
    assert_eq!(output.stdout, b"a\xffb \tc\xfe".to_vec());
}

#[test]
fn test_echo_write_error_exit_status() {
    // Audit #E2: a failed write must be diagnosed and must not exit 0, even
    // when the output does not end in a <newline> (the buffered case).
    if !Path::new("/dev/full").exists() {
        return;
    }

    for args in [&["hello"][..], &["-n", "hello"][..]] {
        let devfull = File::options()
            .write(true)
            .open("/dev/full")
            .expect("failed to open /dev/full");

        let output = Command::new(get_binary_path("echo"))
            .args(args)
            .stdout(devfull)
            .output()
            .expect("failed to run echo");

        assert_eq!(
            output.status.code(),
            Some(1),
            "echo {args:?} > /dev/full should exit 1"
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.starts_with("echo: "),
            "diagnostic should be prefixed with the utility name, got {stderr:?}"
        );
    }
}
