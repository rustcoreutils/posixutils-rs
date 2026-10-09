//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};
use std::os::unix::fs::OpenOptionsExt;

fn nice_exit(args: Vec<&str>, expected_exit: i32) {
    let plan = TestPlan {
        cmd: String::from("nice"),
        args: args.into_iter().map(String::from).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: expected_exit,
    };
    run_test_with_checker(plan, |_, output| {
        assert_eq!(output.status.code(), Some(expected_exit));
    });
}

// Default increment: utility runs and its status is propagated.
#[test]
fn nice_default_runs() {
    nice_exit(vec!["true"], 0);
}

#[test]
fn nice_status_passthrough() {
    nice_exit(vec!["false"], 1);
}

// `-n 5` accepted; utility runs (raising the nice value needs no privilege).
#[test]
fn nice_positive_increment() {
    nice_exit(vec!["-n", "5", "true"], 0);
}

// #NC3: a large increment (40) must be accepted, not rejected at parse time.
// The kernel clamps the resulting nice value.
#[test]
fn nice_large_increment_accepted() {
    nice_exit(vec!["-n", "40", "true"], 0);
}

// #NC1: a negative increment without privilege must NOT prevent invoking the
// utility (the nice change silently fails but the utility still runs, exit 0).
#[test]
fn nice_negative_increment_still_runs() {
    nice_exit(vec!["-n", "-10", "true"], 0);
}

// #NC2: utility not found -> 127 (not 1).
#[test]
fn nice_not_found() {
    nice_exit(vec!["/nonexistent/utility/xyz"], 127);
}

// #NC2: utility found but not executable -> 126 (not 1).
#[test]
fn nice_not_executable() {
    let dir = plib::tmp::tempdir().unwrap();
    let path = dir.path().join("noexec");
    {
        let _f = std::fs::OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .mode(0o644)
            .open(&path)
            .unwrap();
    }
    nice_exit(vec![path.to_str().unwrap()], 126);
}

// XBD 12.2 Guideline 9: once the utility name is read, every later word is
// the utility's. `nice echo -n 5 x` took `-n 5` as nice's own increment and
// ran `echo x`. Both GNU and BSD echo honour -n, so the output has no newline.
#[test]
fn options_after_utility_belong_to_the_utility() {
    plib::testing::run_test(TestPlan {
        cmd: String::from("nice"),
        args: ["echo", "-n", "5", "x"].map(String::from).to_vec(),
        stdin_data: String::new(),
        expected_out: String::from("5 x"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// The utility's arguments are passed through byte for byte; clap rejected
// one that was not valid UTF-8.
#[test]
fn nice_passes_non_utf8_arguments() {
    use plib::testing::{get_binary_path, os_bytes};
    let output = std::process::Command::new(get_binary_path("nice"))
        .args(["sh", "-c", "printf '%s' \"$1\"", "sh"])
        .arg(os_bytes(b"arg\xfe"))
        .output()
        .unwrap();
    assert!(output.status.success(), "{output:?}");
    assert_eq!(output.stdout, b"arg\xfe");
}
