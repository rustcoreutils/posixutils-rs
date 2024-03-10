//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::Write;
use std::process::{Command, Stdio};

fn wc_test(args: &[&str], test_data: &str, expected_output: &str) {
    let test_bin_path = std::env::current_dir()
        .unwrap()
        .parent()
        .unwrap() // Move up to the workspace root from the current package directory
        .join("target/release/wc"); // Adjust the path to the binary

    let mut command = Command::new(test_bin_path);
    let mut child = command
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("failed to spawn wc");

    let stdin = child.stdin.as_mut().expect("failed to get stdin");
    stdin
        .write_all(test_data.as_bytes())
        .expect("failed to write to stdin");

    let output = child.wait_with_output().expect("failed to wait for child");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout, expected_output);
    assert!(output.status.success());
    assert_eq!(output.status.code(), Some(0));
}

#[test]
fn test_wc_empty() {
    wc_test(&["-c"], "", "0\n");
    wc_test(&["-l"], "", "0\n");
    wc_test(&["-w"], "", "0\n");
}

#[test]
fn test_wc_one() {
    wc_test(&["-c"], "x", "1\n");
    wc_test(&["-l"], "x", "0\n");
    wc_test(&["-w"], "x", "1\n");
}

#[test]
fn test_wc_two() {
    wc_test(&["-c"], "x y\n", "4\n");
    wc_test(&["-l"], "x y\n", "1\n");
    wc_test(&["-w"], "x y\n", "2\n");
}
