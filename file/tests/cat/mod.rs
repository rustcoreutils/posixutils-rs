//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use plib::tmp::tempdir;

fn cat_test(args: &[&str], stdin: &str, expected_out: &str) {
    let str_args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    run_test(TestPlan {
        cmd: String::from("cat"),
        args: str_args,
        stdin_data: String::from(stdin),
        expected_out: String::from(expected_out),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn cat_no_args_reads_stdin() {
    cat_test(&[], "hello\nworld\n", "hello\nworld\n");
}

#[test]
fn cat_dash_is_stdin() {
    cat_test(&["-"], "abc\n", "abc\n");
}

#[test]
fn cat_u_flag_is_noop() {
    cat_test(&["-u"], "data\n", "data\n");
}

#[test]
fn cat_multiple_dash_reads_stdin_once() {
    // The first '-' consumes all of stdin; the second sees EOF. stdin is not
    // closed/reopened, so the result is the full input followed by nothing.
    cat_test(&["-", "-"], "line\n", "line\n");
}

#[test]
fn cat_file_operand() {
    let dir = tempdir().unwrap();
    let f = dir.path().join("file");
    fs::write(&f, "file contents\n").unwrap();
    cat_test(&[f.to_str().unwrap()], "", "file contents\n");
}

#[test]
fn cat_dash_among_files() {
    let dir = tempdir().unwrap();
    let f = dir.path().join("among");
    fs::write(&f, "A\n").unwrap();
    // file, then stdin (-), in order.
    cat_test(&[f.to_str().unwrap(), "-"], "B\n", "A\nB\n");
}

#[test]
fn cat_missing_file_sets_exit_and_continues() {
    let dir = tempdir().unwrap();
    let good = dir.path().join("good");
    fs::write(&good, "ok\n").unwrap();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("cat"),
            args: vec![
                String::from("/no/such/file/xyz"),
                good.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_, output| {
            // The good file is still written, and the exit status is non-zero.
            assert_eq!(String::from_utf8_lossy(&output.stdout), "ok\n");
            assert_eq!(output.status.code(), Some(1));
            assert!(String::from_utf8_lossy(&output.stderr).contains("/no/such/file/xyz"));
        },
    );
}

/// `cat` of a large file into a closed pipe must die by SIGPIPE.
/// See `plib::testing::assert_dies_by_sigpipe`.
#[test]
fn test_cat_dies_by_sigpipe_on_a_closed_pipe() {
    let dir = tempdir().unwrap();
    let big = dir.path().join("big");
    let body: String = (0..200_000).map(|n| format!("line {n}\n")).collect();
    std::fs::write(&big, body).unwrap();

    plib::testing::assert_dies_by_sigpipe("cat", &[big.to_str().unwrap()]);
}

/// Started with SIGPIPE ignored, `cat` into a closed pipe reports the write
/// error and exits 1 instead of dying by the signal it was told to ignore.
/// See `plib::testing::assert_epipe_when_sigpipe_ignored`.
#[test]
fn test_cat_reports_epipe_when_sigpipe_is_ignored() {
    let dir = tempdir().unwrap();
    let f = dir.path().join("f");
    std::fs::write(&f, "hello\n").unwrap();

    plib::testing::assert_epipe_when_sigpipe_ignored("cat", &[f.to_str().unwrap()], 1);
}
