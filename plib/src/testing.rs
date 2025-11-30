//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::Write;
use std::process::{Command, Output, Stdio};
use std::thread;
use std::time::Duration;

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

/// Run a test command with environment variables
///
/// This is the core test runner that supports setting environment variables.
/// Use this when tests need specific environment configuration.
pub fn run_test_base_with_env(
    cmd: &str,
    args: &Vec<String>,
    stdin_data: &[u8],
    env_vars: &[(&str, &str)],
) -> Output {
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
    command
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    // Set environment variables
    for (key, value) in env_vars {
        command.env(key, value);
    }

    let mut child = command
        .spawn()
        .unwrap_or_else(|_| panic!("failed to spawn command {cmd}"));

    // Separate the mutable borrow of stdin from the child process
    if let Some(mut stdin) = child.stdin.take() {
        let chunk_size = 1024; // Arbitrary chunk size, adjust if needed
        for chunk in stdin_data.chunks(chunk_size) {
            // Write each chunk
            if let Err(e) = stdin.write_all(chunk) {
                eprintln!("Error writing to stdin: {}", e);
                break;
            }
            // Flush after writing each chunk
            if let Err(e) = stdin.flush() {
                eprintln!("Error flushing stdin: {}", e);
                break;
            }

            // Sleep briefly to avoid CPU spinning
            thread::sleep(Duration::from_millis(10));
        }
        // Explicitly drop stdin to close the pipe
        drop(stdin);
    }

    // Ensure we wait for the process to complete after writing to stdin
    let output = child.wait_with_output().expect("failed to wait for child");
    output
}

/// Run a test command (without custom environment variables)
pub fn run_test_base(cmd: &str, args: &Vec<String>, stdin_data: &[u8]) -> Output {
    run_test_base_with_env(cmd, args, stdin_data, &[])
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

/// Run a test with custom environment variables
///
/// Like `run_test` but allows setting environment variables for the subprocess.
pub fn run_test_with_env(plan: TestPlan, env_vars: &[(&str, &str)]) {
    let output = run_test_base_with_env(
        &plan.cmd,
        &plan.args,
        plan.stdin_data.as_bytes(),
        env_vars,
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout, plan.expected_out);

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(stderr, plan.expected_err);

    assert_eq!(output.status.code(), Some(plan.expected_exit_code));
    if plan.expected_exit_code == 0 {
        assert!(output.status.success());
    }
}

/// Run a test with custom environment variables and a checker function
///
/// Like `run_test_with_checker` but allows setting environment variables.
pub fn run_test_with_checker_and_env<F: FnMut(&TestPlan, &Output)>(
    plan: TestPlan,
    env_vars: &[(&str, &str)],
    mut checker: F,
) {
    let output = run_test_base_with_env(
        &plan.cmd,
        &plan.args,
        plan.stdin_data.as_bytes(),
        env_vars,
    );
    checker(&plan, &output);
}
