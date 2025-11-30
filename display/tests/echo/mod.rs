//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};

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
    echo_test(&["\\0377"], "\u{ff}\n"); // \0377 = 255 (max byte)
}

#[test]
fn test_echo_octal_non_octal_terminates() {
    // Non-octal digit terminates octal sequence
    echo_test(&["\\018"], "\x018\n"); // '8' is not octal, so \01 = 1, then '8'
    echo_test(&["\\0789"], "\x0789\n"); // \07 = 7, '8' and '9' are literal
}

#[test]
fn test_echo_unknown_escape() {
    // Unknown escape sequences - the character after backslash is preserved
    echo_test(&["hello\\xworld"], "helloxworld\n");
    echo_test(&["\\q"], "q\n");
    echo_test(&["\\1"], "1\n"); // \1 without leading 0 is unknown
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
