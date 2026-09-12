//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;
use std::path::PathBuf;

use plib::testing::{run_test_with_checker, TestPlan};

/// Fresh temp directory for a test's output files; returns (dir, prefix-string).
fn tmp_prefix(tag: &str) -> (PathBuf, String) {
    let dir = std::env::temp_dir().join(format!("posixutils_split_{tag}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    let prefix = dir.join("seg_").to_str().unwrap().to_string();
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
    let (dir, prefix) = tmp_prefix("lines_dash");
    run_split(&["-l", "1", "-", &prefix], "l1\nl2\nl3\n", 0);
    assert_eq!(fs::read_to_string(dir.join("seg_aa")).unwrap(), "l1\n");
    assert_eq!(fs::read_to_string(dir.join("seg_ab")).unwrap(), "l2\n");
    assert_eq!(fs::read_to_string(dir.join("seg_ac")).unwrap(), "l3\n");
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn split_by_bytes() {
    let (dir, prefix) = tmp_prefix("bytes");
    run_split(&["-b", "2", "-", &prefix], "abcde", 0);
    assert_eq!(fs::read_to_string(dir.join("seg_aa")).unwrap(), "ab");
    assert_eq!(fs::read_to_string(dir.join("seg_ab")).unwrap(), "cd");
    assert_eq!(fs::read_to_string(dir.join("seg_ac")).unwrap(), "e");
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn split_partial_last_line() {
    // A trailing partial line (no newline) goes into the last file.
    let (dir, prefix) = tmp_prefix("partial");
    run_split(&["-l", "2", "-", &prefix], "a\nb\nc", 0);
    assert_eq!(fs::read_to_string(dir.join("seg_aa")).unwrap(), "a\nb\n");
    assert_eq!(fs::read_to_string(dir.join("seg_ab")).unwrap(), "c");
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn split_empty_input_no_files() {
    let (dir, prefix) = tmp_prefix("empty");
    run_split(&["-l", "1", "-", &prefix], "", 0);
    let count = fs::read_dir(&dir).unwrap().count();
    assert_eq!(count, 0, "empty input must not create output files");
    fs::remove_dir_all(&dir).unwrap();
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
    let (dir, prefix) = tmp_prefix("exhausted_msg");
    let input: String = (0..27).map(|i| format!("line{i}\n")).collect();
    run_split_stderr(&["-l", "1", "-a", "1", "-", &prefix], &input, 1, |stderr| {
        assert_one_diagnostic(stderr, "suffixes exhausted")
    });
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn split_name_too_long_diagnostic_is_one_named_line() {
    // This path printed the message itself *and* returned an Err that was
    // printed again, so it emitted two lines.
    let (dir, _) = tmp_prefix("namemax_msg");
    let long_prefix = dir.join("p".repeat(260));
    run_split_stderr(
        &["-l", "1", "-", long_prefix.to_str().unwrap()],
        "data\n",
        1,
        |stderr| assert_one_diagnostic(stderr, "too long"),
    );
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn split_invalid_byte_count_diagnostic_is_one_named_line() {
    let (dir, prefix) = tmp_prefix("badbytes_msg");
    run_split_stderr(&["-b", "12x", "-", &prefix], "data\n", 1, |stderr| {
        assert_one_diagnostic(stderr, "byte count")
    });
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn split_uses_every_suffix_before_exhausting() {
    // The suffix odometer carried left through every 'z' and returned None
    // without ever yielding the all-'z' value, so `-a 1` stopped at `y` and
    // produced 25 files where 26 are available.
    let (dir, prefix) = tmp_prefix("suffix_last");
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
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn split_suffixes_exhausted_errors() {
    // One line past the last suffix: the 26 files still get written, and the
    // 27th is the error. The iterator must stay exhausted rather than wrapping
    // back to the first suffix and overwriting `seg_a`.
    let (dir, prefix) = tmp_prefix("suffix_exhausted");
    let input: String = (0..27).map(|i| format!("line{i}\n")).collect();
    run_split(&["-l", "1", "-a", "1", "-", &prefix], &input, 1);

    assert_eq!(
        fs::read_to_string(dir.join("seg_a")).unwrap(),
        "line0\n",
        "the first output file must not be overwritten by a wrapped suffix"
    );
    assert_eq!(fs::read_to_string(dir.join("seg_z")).unwrap(), "line25\n");
    assert_eq!(
        fs::read_dir(&dir).unwrap().count(),
        26,
        "exactly the 26 available suffixes are used"
    );
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn split_name_too_long_errors() {
    let (dir, _) = tmp_prefix("namemax");
    let long_prefix = dir.join("p".repeat(260));
    run_split(
        &["-l", "1", "-", long_prefix.to_str().unwrap()],
        "data\n",
        1,
    );
    // No files created.
    let count = fs::read_dir(&dir).unwrap().count();
    assert_eq!(count, 0);
    fs::remove_dir_all(&dir).unwrap();
}
