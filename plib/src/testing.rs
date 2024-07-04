//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::Write;
use std::process::{Command, Output, Stdio};

pub struct TestPlan {
    pub cmd: String,
    pub args: Vec<String>,
    pub stdin_data: String,
    pub expected_out: String,
    pub expected_err: String,
    pub expected_exit_code: i32,
}

pub struct TestPlanU8 {
    pub cmd: String,
    pub args: Vec<String>,
    pub stdin_data: Vec<u8>,
    pub expected_out: Vec<u8>,
    pub expected_err: Vec<u8>,
    pub expected_exit_code: i32,
}

fn run_test_base(cmd: &str, args: &Vec<String>, stdin_data: &[u8]) -> Output {
    let relpath = if cfg!(debug_assertions) {
        format!("target/debug/{}", cmd)
    } else {
        format!("target/release/{}", cmd)
    };
    let test_bin_path = std::env::current_dir()
        .unwrap()
        .parent()
        .unwrap() // Move up to the workspace root from the current package directory
        .join(relpath); // Adjust the path to the binary

    let mut command = Command::new(test_bin_path);
    let mut child = command
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn head");

    let stdin = child.stdin.as_mut().expect("failed to get stdin");
    stdin
        .write_all(stdin_data)
        .expect("failed to write to stdin");

    let output = child.wait_with_output().expect("failed to wait for child");
    output
}

pub fn run_test(plan: TestPlan) {
    let output = run_test_base(&plan.cmd, &plan.args, plan.stdin_data.as_bytes());

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout, plan.expected_out);

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(stderr, plan.expected_err);

    assert_eq!(output.status.code(), Some(plan.expected_exit_code));
    if plan.expected_exit_code == 0 {
        assert!(output.status.success());
    }
}

pub fn run_test_u8(plan: TestPlanU8) {
    let output = run_test_base(&plan.cmd, &plan.args, &plan.stdin_data);

    assert_eq!(output.stdout, plan.expected_out);

    assert_eq!(output.stderr, plan.expected_err);

    assert_eq!(output.status.code(), Some(plan.expected_exit_code));
    if plan.expected_exit_code == 0 {
        assert!(output.status.success());
    }
}

pub fn run_test_with_checker<F: FnMut(&TestPlan, &Output)>(plan: TestPlan, mut checker: F) {
    let output = run_test_base(&plan.cmd, &plan.args, plan.stdin_data.as_bytes());
    checker(&plan, &output);
}
