//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;

use plib::testing::{run_test_with_checker, TestPlan};
use plib::tmp::{tempdir, TempDir};

/// Fresh temp directory for a test's output files, removed when dropped;
/// returns (dir, prefix-string).
fn tmp_prefix() -> (TempDir, String) {
    let dir = tempdir().unwrap();
    let prefix = dir.path().join("seg_").to_str().unwrap().to_string();
    (dir, prefix)
}

fn run_split(args: &[&str], stdin: &str, expected_exit: i32) {
    let str_args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("split"),
            args: str_args,
            stdin_data: String::from(stdin),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: expected_exit,
        },
        |_, output| assert_eq!(output.status.code(), Some(expected_exit)),
    );
}

#[test]
fn split_lines_from_stdin_dash() {
    let (tmp, prefix) = tmp_prefix();
    let dir = tmp.path();
    run_split(&["-l", "1", "-", &prefix], "l1\nl2\nl3\n", 0);
    assert_eq!(fs::read_to_string(dir.join("seg_aa")).unwrap(), "l1\n");
    assert_eq!(fs::read_to_string(dir.join("seg_ab")).unwrap(), "l2\n");
    assert_eq!(fs::read_to_string(dir.join("seg_ac")).unwrap(), "l3\n");
}

#[test]
fn split_by_bytes() {
    let (tmp, prefix) = tmp_prefix();
    let dir = tmp.path();
    run_split(&["-b", "2", "-", &prefix], "abcde", 0);
    assert_eq!(fs::read_to_string(dir.join("seg_aa")).unwrap(), "ab");
    assert_eq!(fs::read_to_string(dir.join("seg_ab")).unwrap(), "cd");
    assert_eq!(fs::read_to_string(dir.join("seg_ac")).unwrap(), "e");
}

#[test]
fn split_partial_last_line() {
    // A trailing partial line (no newline) goes into the last file.
    let (tmp, prefix) = tmp_prefix();
    let dir = tmp.path();
    run_split(&["-l", "2", "-", &prefix], "a\nb\nc", 0);
    assert_eq!(fs::read_to_string(dir.join("seg_aa")).unwrap(), "a\nb\n");
    assert_eq!(fs::read_to_string(dir.join("seg_ab")).unwrap(), "c");
}

#[test]
fn split_empty_input_no_files() {
    let (tmp, prefix) = tmp_prefix();
    let dir = tmp.path();
    run_split(&["-l", "1", "-", &prefix], "", 0);
    let count = fs::read_dir(dir).unwrap().count();
    assert_eq!(count, 0, "empty input must not create output files");
}

/// Runs split and hands the raw stderr to `check`.
fn run_split_stderr(args: &[&str], stdin: &str, expected_exit: i32, check: fn(&str)) {
    let str_args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("split"),
            args: str_args,
            stdin_data: String::from(stdin),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: expected_exit,
        },
        move |_, output| {
            assert_eq!(output.status.code(), Some(expected_exit));
            check(&String::from_utf8_lossy(&output.stderr));
        },
    );
}

/// Every diagnostic is one `split: <message>` line -- not the `Debug` of a
/// boxed error, and not the same message twice.
fn assert_one_diagnostic(stderr: &str, needle: &str) {
    let lines: Vec<&str> = stderr.lines().collect();
    assert_eq!(lines.len(), 1, "expected exactly one line, got {stderr:?}");
    assert!(
        lines[0].starts_with("split: "),
        "diagnostic must be prefixed with the utility name: {stderr:?}"
    );
    assert!(
        lines[0].contains(needle),
        "diagnostic must name the failure ({needle:?}): {stderr:?}"
    );
}

#[test]
fn split_exhaustion_diagnostic_is_one_named_line() {
    let (_dir, prefix) = tmp_prefix();
    let input: String = (0..27).map(|i| format!("line{i}\n")).collect();
    run_split_stderr(&["-l", "1", "-a", "1", "-", &prefix], &input, 1, |stderr| {
        assert_one_diagnostic(stderr, "suffixes exhausted")
    });
}

#[test]
fn split_zero_byte_count_is_rejected() {
    // A zero boundary made every write advance by zero bytes, so the loop
    // opened a fresh output file per pass and never consumed the input: 676
    // empty files and then "output suffixes exhausted". `-l 0` is already
    // refused by clap's `1..` range; `-b` parses its own operand and was not.
    let (tmp, prefix) = tmp_prefix();
    let dir = tmp.path();
    run_split_stderr(&["-b", "0", "-", &prefix], "a\nb\n", 1, |stderr| {
        assert_one_diagnostic(stderr, "byte count")
    });
    assert_eq!(
        fs::read_dir(dir).unwrap().count(),
        0,
        "a rejected byte count must create no files"
    );
}

#[test]
fn split_zero_byte_count_with_suffix_is_rejected() {
    // The multiplier is applied after the parse, so `0k` is zero too.
    let (tmp, prefix) = tmp_prefix();
    let dir = tmp.path();
    run_split_stderr(&["-b", "0k", "-", &prefix], "a\nb\n", 1, |stderr| {
        assert_one_diagnostic(stderr, "byte count")
    });
    assert_eq!(fs::read_dir(dir).unwrap().count(), 0);
}

#[test]
fn split_name_too_long_diagnostic_is_one_named_line() {
    // This path printed the message itself *and* returned an Err that was
    // printed again, so it emitted two lines.
    let (tmp, _) = tmp_prefix();
    let dir = tmp.path();
    let long_prefix = dir.join("p".repeat(260));
    run_split_stderr(
        &["-l", "1", "-", long_prefix.to_str().unwrap()],
        "data\n",
        1,
        |stderr| assert_one_diagnostic(stderr, "too long"),
    );
}

#[test]
fn split_invalid_byte_count_diagnostic_is_one_named_line() {
    let (_dir, prefix) = tmp_prefix();
    run_split_stderr(&["-b", "12x", "-", &prefix], "data\n", 1, |stderr| {
        assert_one_diagnostic(stderr, "byte count")
    });
}

#[test]
fn split_uses_every_suffix_before_exhausting() {
    // The suffix odometer carried left through every 'z' and returned None
    // without ever yielding the all-'z' value, so `-a 1` stopped at `y` and
    // produced 25 files where 26 are available.
    let (tmp, prefix) = tmp_prefix();
    let dir = tmp.path();
    let input: String = (0..26).map(|i| format!("line{i}\n")).collect();
    run_split(&["-l", "1", "-a", "1", "-", &prefix], &input, 0);

    for (i, ch) in ('a'..='z').enumerate() {
        let path = dir.join(format!("seg_{ch}"));
        assert_eq!(
            fs::read_to_string(&path).unwrap_or_default(),
            format!("line{i}\n"),
            "suffix '{ch}' must be used: {}",
            path.display()
        );
    }
}

#[test]
fn split_suffixes_exhausted_errors() {
    // One line past the last suffix: the 26 files still get written, and the
    // 27th is the error. The iterator must stay exhausted rather than wrapping
    // back to the first suffix and overwriting `seg_a`.
    let (tmp, prefix) = tmp_prefix();
    let dir = tmp.path();
    let input: String = (0..27).map(|i| format!("line{i}\n")).collect();
    run_split(&["-l", "1", "-a", "1", "-", &prefix], &input, 1);

    assert_eq!(
        fs::read_to_string(dir.join("seg_a")).unwrap(),
        "line0\n",
        "the first output file must not be overwritten by a wrapped suffix"
    );
    assert_eq!(fs::read_to_string(dir.join("seg_z")).unwrap(), "line25\n");
    assert_eq!(
        fs::read_dir(dir).unwrap().count(),
        26,
        "exactly the 26 available suffixes are used"
    );
}

#[test]
fn split_name_too_long_errors() {
    let (tmp, _) = tmp_prefix();
    let dir = tmp.path();
    let long_prefix = dir.join("p".repeat(260));
    run_split(
        &["-l", "1", "-", long_prefix.to_str().unwrap()],
        "data\n",
        1,
    );
    // No files created.
    let count = fs::read_dir(dir).unwrap().count();
    assert_eq!(count, 0);
}

/// `split` must name the *input* file it could not open.
#[test]
fn test_split_error_names_the_input_file() {
    let out = std::process::Command::new(plib::testing::get_binary_path("split"))
        .arg("/nonexistent_split_probe_zz")
        .output()
        .expect("spawn split");
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();

    assert!(
        stderr.starts_with("split: "),
        "every diagnostic must name the utility: {stderr:?}"
    );
    assert!(
        stderr.contains("/nonexistent_split_probe_zz"),
        "the diagnostic must name the file it could not open: {stderr:?}"
    );
    assert!(
        !stderr.contains("(os error"),
        "Rust's errno parenthetical must not reach the user: {stderr:?}"
    );
    assert_ne!(out.status.code(), Some(0));
}

/// ...and must name the *output* file when that is what failed, which is why
/// the name has to be captured per origin rather than once in main.
#[test]
fn test_split_error_names_the_output_file() {
    let dir = plib::tmp::tempdir().expect("tempdir");
    let input = dir.path().join("in.txt");
    std::fs::write(&input, "one\ntwo\nthree\n").unwrap();

    let out = std::process::Command::new(plib::testing::get_binary_path("split"))
        .arg(&input)
        .arg("/nonexistent_dir_split_zz/prefix")
        .output()
        .expect("spawn split");
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();

    assert!(
        stderr.contains("/nonexistent_dir_split_zz/prefix"),
        "the output file is what failed, so that is what must be named: {stderr:?}"
    );
    assert!(
        !stderr.contains(input.to_str().unwrap()),
        "the readable input must not be blamed: {stderr:?}"
    );
    assert_ne!(out.status.code(), Some(0));
}

// XBD 12.2, Guideline 7: an option-argument may begin with '-'. Each option
// below used to have the word after it refused as an unknown option.
#[test]
fn option_argument_may_begin_with_hyphen() {
    for opt in ["-a", "-l", "-b"] {
        plib::testing::assert_hyphen_option_argument("split", &[opt, "-zq", "--help"]);
    }
}
