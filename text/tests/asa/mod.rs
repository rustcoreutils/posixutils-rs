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

// ===== FILE ARGUMENT TESTS =====

fn asa_test_with_args(args: &[&str], expected_output: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();
    run_test(TestPlan {
        cmd: String::from("asa"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code,
    });
}

// Test single file argument
#[test]
fn asa_single_file() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let file1 = format!("{}/tests/asa/file1.txt", project_root);
    let expected = "Line 1 from file1\nLine 2 from file1\n\nLine 3 with double-space\n";
    asa_test_with_args(&[file1.as_str()], expected, 0);
}

// Test multiple file arguments (files processed sequentially)
#[test]
fn asa_multiple_files() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let file1 = format!("{}/tests/asa/file1.txt", project_root);
    let file2 = format!("{}/tests/asa/file2.txt", project_root);
    let expected = concat!(
        "Line 1 from file1\n",
        "Line 2 from file1\n",
        "\n",
        "Line 3 with double-space\n",
        "\x0c",
        "Page 1 from file2\n",
        "Normal line\r",
        "Overprint this\n"
    );
    asa_test_with_args(&[file1.as_str(), file2.as_str()], expected, 0);
}

// Test stdin with "-" argument
#[test]
fn asa_stdin_dash() {
    run_test(TestPlan {
        cmd: String::from("asa"),
        args: vec![String::from("-")],
        stdin_data: String::from(" test from stdin\n"),
        expected_out: String::from("test from stdin\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

// ===== EDGE CASE TESTS (POSIX COMPLIANCE) =====

// Test lines with only newline (control char missing)
#[test]
fn asa_line_with_only_newline() {
    // Empty line (just newline): first char is newline itself
    // The implementation should handle this gracefully
    asa_test("\n", "\n");
}

// Test very long lines (POSIX doesn't specify line length limits for asa)
#[test]
fn asa_long_line() {
    let long_content = "x".repeat(5000);
    let input = format!(" {}\n", long_content);
    let expected = format!("{}\n", long_content);
    asa_test(&input, &expected);
}

// Test tab character as control (treated as unknown, like space)
#[test]
fn asa_tab_control() {
    asa_test("\ttext\n", "text\n");
}

// Test newline as control character (edge case)
// First line is empty (just newline), second line has space control with "text"
// Empty first line should output just a newline, then second line outputs normally
#[test]
fn asa_newline_control() {
    asa_test("\n text\n", "\ntext\n");
}

// Test carriage return within content (not as control)
#[test]
fn asa_embedded_carriage_return() {
    asa_test(" text\rwith\rcr\n", "text\rwith\rcr\n");
}

// Test form feed within content (not as control)
#[test]
fn asa_embedded_form_feed() {
    asa_test(" text\x0cwith\x0cff\n", "text\x0cwith\x0cff\n");
}

// ===== SEQUENTIAL FILE PROCESSING =====

// Test that each file starts as if it's independent
// (first line of second file should be treated as first line)
#[test]
fn asa_file_independence() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let file2 = format!("{}/tests/asa/file2.txt", project_root);
    // file2.txt starts with '1' (form-feed) as first line
    // Should output form-feed at start since it's the first line of the file
    let expected = "\x0cPage 1 from file2\nNormal line\rOverprint this\n";
    asa_test_with_args(&[file2.as_str()], expected, 0);
}

// ===== SPECIAL CHARACTER COMBINATIONS =====

// Test all standard control characters in sequence
#[test]
fn asa_all_controls_sequence() {
    let input = " space\n0zero\n1one\n+plus\n";
    let expected = "space\n\nzero\n\x0cone\rplus\n";
    asa_test(input, expected);
}

// Test repeated form-feeds
#[test]
fn asa_multiple_form_feeds() {
    asa_test("1page1\n1page2\n1page3\n", "\x0cpage1\n\x0cpage2\n\x0cpage3\n");
}

// Test alternating double-space and normal
#[test]
fn asa_alternating_spacing() {
    asa_test(" normal\n0double\n normal\n0double\n", "normal\n\ndouble\nnormal\n\ndouble\n");
}
