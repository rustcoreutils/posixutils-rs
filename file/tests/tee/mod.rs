//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;
use std::path::PathBuf;

use plib::testing::{run_test_with_checker, TestPlan};

fn tmp_path(name: &str) -> PathBuf {
    std::env::temp_dir().join(format!("posixutils_tee_{name}"))
}

/// Run `tee` with the given args and stdin, assert stdout/exit, then return so
/// the caller can inspect the written files.
fn run_tee(args: &[&str], stdin: &str, expected_stdout: &str, expected_exit: i32) {
    let str_args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("tee"),
            args: str_args,
            stdin_data: String::from(stdin),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: expected_exit,
        },
        |_, output| {
            assert_eq!(String::from_utf8_lossy(&output.stdout), expected_stdout);
            assert_eq!(output.status.code(), Some(expected_exit));
        },
    );
}

#[test]
fn tee_copies_stdin_to_stdout_and_file() {
    let f = tmp_path("basic");
    let _ = fs::remove_file(&f);
    run_tee(
        &[f.to_str().unwrap()],
        "hello\nworld\n",
        "hello\nworld\n",
        0,
    );
    assert_eq!(fs::read_to_string(&f).unwrap(), "hello\nworld\n");
    let _ = fs::remove_file(&f);
}

#[test]
fn tee_no_files_passthrough_to_stdout() {
    run_tee(&[], "passthrough\n", "passthrough\n", 0);
}

#[test]
fn tee_positional_multiple_files() {
    let f1 = tmp_path("multi1");
    let f2 = tmp_path("multi2");
    let _ = fs::remove_file(&f1);
    let _ = fs::remove_file(&f2);
    run_tee(
        &[f1.to_str().unwrap(), f2.to_str().unwrap()],
        "data\n",
        "data\n",
        0,
    );
    assert_eq!(fs::read_to_string(&f1).unwrap(), "data\n");
    assert_eq!(fs::read_to_string(&f2).unwrap(), "data\n");
    let _ = fs::remove_file(&f1);
    let _ = fs::remove_file(&f2);
}

#[test]
fn tee_append() {
    let f = tmp_path("append");
    fs::write(&f, "first\n").unwrap();
    run_tee(&["-a", f.to_str().unwrap()], "second\n", "second\n", 0);
    assert_eq!(fs::read_to_string(&f).unwrap(), "first\nsecond\n");
    let _ = fs::remove_file(&f);
}

#[test]
fn tee_truncates_without_append() {
    let f = tmp_path("trunc");
    fs::write(&f, "old content here\n").unwrap();
    run_tee(&[f.to_str().unwrap()], "new\n", "new\n", 0);
    assert_eq!(fs::read_to_string(&f).unwrap(), "new\n");
    let _ = fs::remove_file(&f);
}
