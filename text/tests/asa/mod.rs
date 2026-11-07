//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_u8, run_test_with_env, utf8_locale, TestPlan, TestPlanU8};

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

/// `asa` over bytes, in the C locale the test runner forces.
fn asa_test_bytes(test_data: &[u8], expected_output: &[u8]) {
    run_test_u8(TestPlanU8 {
        cmd: String::from("asa"),
        args: vec![],
        stdin_data: test_data.to_vec(),
        expected_out: expected_output.to_vec(),
        expected_err: vec![],
        expected_exit_code: 0,
    });
}

/// `asa` in a UTF-8 locale, or nothing where the host has none.
fn asa_test_utf8(test_data: &str, expected_output: &str) {
    let Some(locale) = utf8_locale() else {
        return;
    };
    run_test_with_env(
        TestPlan {
            cmd: String::from("asa"),
            args: vec![],
            stdin_data: String::from(test_data),
            expected_out: String::from(expected_output),
            expected_err: String::from(""),
            expected_exit_code: 0,
        },
        &[("LC_ALL", &locale)],
    );
}

// A multibyte character as the control character is none POSIX names, so it
// acts as a space: the whole character goes, in a locale where it is one.
#[test]
fn asa_multibyte_control_char() {
    asa_test_utf8("éhello\n", "hello\n");
}

#[test]
fn asa_cjk_control_char() {
    asa_test_utf8("日test\n", "test\n");
}

// In the POSIX locale every byte is a character (XBD 6.2), so a file with
// bytes that are not UTF-8 is a text file, and passes through whole.
#[test]
fn asa_non_utf8_bytes_in_posix_locale() {
    asa_test_bytes(b" ok1\n caf\xe9\n ok3\n", b"ok1\ncaf\xe9\nok3\n");
}

// The first *character* is removed, and in the POSIX locale that is one
// byte, even of what would be a UTF-8 sequence elsewhere.
#[test]
fn asa_first_character_is_one_byte_in_posix_locale() {
    asa_test_bytes(b"\xe6\x97\xa5test\n", b"\x97\xa5test\n");
}

#[test]
fn asa_nul_control_char() {
    asa_test_bytes(b"\0a\0b\n x\n", b"a\0b\nx\n");
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
    asa_test(
        "1page1\n1page2\n1page3\n",
        "\x0cpage1\n\x0cpage2\n\x0cpage3\n",
    );
}

// Test alternating double-space and normal
#[test]
fn asa_alternating_spacing() {
    asa_test(
        " normal\n0double\n normal\n0double\n",
        "normal\n\ndouble\nnormal\n\ndouble\n",
    );
}

// ---------------------------------------------------------------------------
// Operands, `-`, `--` and error paths (audit "Test coverage signal")
// ---------------------------------------------------------------------------

use std::io::Write;

/// Run `asa` with `args` against real files, returning (stdout, stderr, code).
fn asa_run(args: &[&str]) -> (String, String, i32) {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_asa"))
        .args(args)
        .output()
        .expect("failed to run asa");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

fn tmp_file(name: &str, content: &str) -> plib::testing::TempFile {
    plib::testing::TempFile::new(name, content)
}

#[test]
fn asa_nonexistent_file_errors() {
    let (_, stderr, code) = asa_run(&["no-such-file-xyz"]);
    assert_eq!(code, 1, "a missing operand must exit non-zero");
    assert!(
        stderr.contains("no-such-file-xyz"),
        "the diagnostic must name the file, got {stderr:?}"
    );
}

#[test]
fn asa_continues_past_a_missing_file() {
    // POSIX CONSEQUENCES OF ERRORS is Default, but the historical behavior is
    // to process the remaining operands and still exit non-zero.
    let good = tmp_file("good", " kept\n");
    let (stdout, _, code) = asa_run(&["no-such-file-xyz", good.to_str().unwrap()]);
    assert_eq!(code, 1);
    assert!(
        stdout.contains("kept"),
        "later operands must still be processed, got {stdout:?}"
    );
}

#[test]
fn asa_dash_interleaved_with_files() {
    // `-` is stdin even when it is not the sole operand, and order is preserved.
    let a = tmp_file("a", " first\n");
    let b = tmp_file("b", " third\n");
    let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_asa"))
        .args([a.to_str().unwrap(), "-", b.to_str().unwrap()])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("spawn asa");
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(b" second\n")
        .unwrap();
    let out = child.wait_with_output().expect("wait asa");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(stdout, "first\nsecond\nthird\n", "got {stdout:?}");
}

/// `asa args` run in `dir`, for operands named relative to it.
fn asa_run_in(dir: &std::path::Path, args: &[&str]) -> (String, i32) {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_asa"))
        .args(args)
        .current_dir(dir)
        .env("LC_ALL", "C")
        .output()
        .expect("failed to run asa");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

#[test]
fn asa_double_dash_ends_options() {
    // A file whose name begins with `-` is reachable only after `--`: named
    // relative to its directory, since an absolute path never looks like an
    // option and would pass with or without `--`.
    let f = tmp_file("-dashname", " body\n");
    let dir = f.parent().unwrap();
    assert_eq!(asa_run_in(dir, &["--", "-dashname"]), ("body\n".into(), 0));
    let (stdout, code) = asa_run_in(dir, &["-dashname"]);
    assert_eq!(
        (stdout.as_str(), code),
        ("", 2),
        "without `--` it is an option"
    );
}

// A directory operand is an error that does not stop the others.
#[cfg(unix)]
#[test]
fn asa_directory_operand_continues() {
    let f = tmp_file("good", " body\n");
    let dir = f.parent().unwrap();
    let (stdout, stderr, code) = asa_run(&[dir.to_str().unwrap(), f.to_str().unwrap()]);
    assert_eq!(code, 1);
    assert_eq!(stdout, "body\n");
    assert!(stderr.contains(dir.to_str().unwrap()), "stderr {stderr:?}");
}

// Each file starts afresh: a '+' first in the second file is a space too.
#[test]
fn asa_plus_first_in_a_later_file() {
    let a = tmp_file("a", " a\n");
    let b = tmp_file("b", "+b\n");
    let (stdout, _, code) = asa_run(&[a.to_str().unwrap(), b.to_str().unwrap()]);
    assert_eq!(code, 0);
    assert_eq!(stdout, "a\nb\n");
}

// A write that fails is reported as a write error and exits 1, even when
// it is the last one, made only when the output is flushed at the end.
#[cfg(target_os = "linux")]
#[test]
fn asa_write_error_on_full_device() {
    plib::testing::assert_write_error_on_full_device("asa", &[], b" x\n", 1);
}

// Once standard output fails every later file would fail the same way:
// one diagnostic, not one per operand.
#[cfg(target_os = "linux")]
#[test]
fn asa_write_error_is_reported_once() {
    let a = tmp_file("a", " first\n");
    let b = tmp_file("b", " second\n");
    let full = std::fs::OpenOptions::new()
        .write(true)
        .open("/dev/full")
        .expect("open /dev/full");
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_asa"))
        .args([a.to_str().unwrap(), b.to_str().unwrap()])
        .env("LC_ALL", "C")
        .stdout(full)
        .output()
        .expect("run asa");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(1), "stderr {stderr:?}");
    assert_eq!(stderr.lines().count(), 1, "stderr {stderr:?}");
    assert!(stderr.contains("write error"), "stderr {stderr:?}");
}

// Into a closed pipe asa dies by SIGPIPE, as a filter should.
#[cfg(unix)]
#[test]
fn asa_dies_by_sigpipe_on_a_closed_pipe() {
    let big = " line of a long report\n".repeat(200_000);
    let f = tmp_file("big", &big);
    plib::testing::assert_dies_by_sigpipe("asa", &[f.to_str().unwrap()]);
}
