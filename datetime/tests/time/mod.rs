//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{
    io::Write,
    process::{Command, Output, Stdio},
};

use plib::testing::TestPlan;

fn run_test_base(cmd: &str, args: &Vec<String>, stdin_data: &[u8]) -> Output {
    // Determine the target directory - cargo-llvm-cov uses a custom target dir
    // When built with cargo-llvm-cov, cfg(coverage) is set and target is in llvm-cov-target subdir
    let target_dir = std::env::var("CARGO_TARGET_DIR")
        .or_else(|_| std::env::var("CARGO_LLVM_COV_TARGET_DIR"))
        .unwrap_or_else(|_| {
            if cfg!(coverage) {
                String::from("target/llvm-cov-target")
            } else {
                String::from("target")
            }
        });

    let profile = if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    };

    let relpath = format!("{}/{}/{}", target_dir, profile, cmd);
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

    child.wait_with_output().expect("failed to wait for child")
}

fn get_output(plan: TestPlan) -> Output {
    let output = run_test_base(&plan.cmd, &plan.args, plan.stdin_data.as_bytes());

    output
}

fn run_test_time(
    args: &[&str],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    let output = get_output(TestPlan {
        cmd: String::from("time"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });

    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(stderr.contains(expected_error));
}

#[test]
fn simple_test() {
    run_test_time(&["--", "ls", "-l"], "", "User time", 0);
}

#[test]
fn p_test() {
    run_test_time(&["-p", "--", "ls", "-l"], "", "user", 0);
}

#[test]
fn parse_error_test() {
    run_test_time(&[], "", "not provided", 0);
}

#[test]
fn command_error_test() {
    run_test_time(&["-s", "ls", "-l"], "", "unexpected argument '-s' found", 0);
}
