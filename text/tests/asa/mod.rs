//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};

fn asa_test(test_data: &str, expected_output: &str) {
    run_test(TestPlan {
        cmd: String::from("asa"),
        args: vec![],
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

// Test empty input
#[test]
fn asa_empty() {
    asa_test("", "");
}

// Test basic space control character (normal single-spacing)
#[test]
fn asa_space_single_line() {
    asa_test(" hello\n", "hello\n");
}

#[test]
fn asa_space_multiple_lines() {
    asa_test(" line1\n line2\n line3\n", "line1\nline2\nline3\n");
}

// Test '0' control character (double-spacing - blank line before)
#[test]
fn asa_zero_first_line() {
    // '0' as first line: outputs blank line, then content
    asa_test("0hello\n", "\nhello\n");
}

#[test]
fn asa_zero_second_line() {
    // '0' on second line: previous line, blank line, then content
    asa_test(" line1\n0line2\n", "line1\n\nline2\n");
}

#[test]
fn asa_zero_multiple() {
    asa_test("0first\n0second\n", "\nfirst\n\nsecond\n");
}

// Test '1' control character (form-feed/new page)
#[test]
fn asa_one_first_line() {
    // '1' as first line: form-feed, then content
    asa_test("1page1\n", "\x0cpage1\n");
}

#[test]
fn asa_one_second_line() {
    // '1' on second line: previous line ends, form-feed, then content
    asa_test(" line1\n1page2\n", "line1\n\x0cpage2\n");
}

// Test '+' control character (overprint - carriage return)
#[test]
fn asa_plus_overprint() {
    // '+' causes overprint: carriage return instead of newline
    asa_test(" line1\n+over\n", "line1\rover\n");
}

#[test]
fn asa_plus_multiple_overprint() {
    // Multiple overprints on same logical line
    asa_test(" base\n+mid\n+top\n", "base\rmid\rtop\n");
}

#[test]
fn asa_plus_first_line() {
    // POSIX: '+' as first character in input is equivalent to space
    asa_test("+first\n", "first\n");
}

#[test]
fn asa_plus_first_line_then_normal() {
    // '+' as first, then normal lines
    asa_test("+first\n line2\n", "first\nline2\n");
}

// Test '-' control character (triple-spacing - non-POSIX extension)
#[test]
fn asa_dash_first_line() {
    asa_test("-content\n", "\n\ncontent\n");
}

#[test]
fn asa_dash_second_line() {
    asa_test(" line1\n-line2\n", "line1\n\n\nline2\n");
}

// Test other/unknown control characters (treated as space)
#[test]
fn asa_other_char() {
    // Unknown control chars treated as space (normal single-spacing)
    asa_test("Xhello\n", "hello\n");
}

#[test]
fn asa_digit_as_control() {
    // '2' is not a special control, treated as space
    asa_test("2hello\n", "hello\n");
}

// Test empty content (control char only)
#[test]
fn asa_space_empty_content() {
    asa_test(" \n", "\n");
}

#[test]
fn asa_zero_empty_content() {
    asa_test("0\n", "\n\n");
}

// Test mixed control characters
#[test]
fn asa_mixed_controls() {
    asa_test(
        " line1\n0double\n1newpage\n+over\n line2\n",
        "line1\n\ndouble\n\x0cnewpage\rover\nline2\n",
    );
}

// Test content without trailing newline (EOF without newline)
#[test]
fn asa_no_trailing_newline() {
    asa_test(" hello", "hello\n");
}

#[test]
fn asa_plus_no_trailing_newline() {
    asa_test(" line1\n+over", "line1\rover\n");
}

// Test lines with only control character (no content, no newline)
#[test]
fn asa_control_only_no_newline() {
    asa_test(" ", "\n");
}

// Test complex FORTRAN-style output simulation
#[test]
fn asa_fortran_style_report() {
    // Simulate a simple FORTRAN report with page header and data
    let input = "1REPORT TITLE\n \n DATA LINE 1\n DATA LINE 2\n0SECTION 2\n DATA LINE 3\n";
    let expected = "\x0cREPORT TITLE\n\nDATA LINE 1\nDATA LINE 2\n\nSECTION 2\nDATA LINE 3\n";
    asa_test(input, expected);
}

// Test multi-byte UTF-8 content (control char is ASCII, content is UTF-8)
#[test]
fn asa_utf8_content() {
    asa_test(" héllo wörld\n", "héllo wörld\n");
}

#[test]
fn asa_cjk_content() {
    asa_test(" 日本語テスト\n", "日本語テスト\n");
}

// Test multi-byte UTF-8 as control character (non-standard, treated as space per POSIX)
#[test]
fn asa_multibyte_control_char() {
    // Multi-byte char as control: treated as unknown, like space
    asa_test("éhello\n", "hello\n");
}

#[test]
fn asa_cjk_control_char() {
    // 3-byte CJK char as control: treated as unknown, like space
    asa_test("日test\n", "test\n");
}
