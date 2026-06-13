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
