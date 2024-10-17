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

use plib::testing::{run_test, TestPlan};

fn time_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("time"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
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

fn get_output(plan: TestPlan) -> Output {
    let output = run_test_base(&plan.cmd, &plan.args, plan.stdin_data.as_bytes());

    output
}

fn run_test_time_error_only(
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
fn test_smoke_help() {
    time_test(
        &["--help"],
        "\
time - time a simple command or give resource usage

Usage: time [OPTIONS] <UTILITY> [ARGUMENT]...

Arguments:
  <UTILITY>      The utility to be invoked
  [ARGUMENT]...  Arguments for the utility

Options:
  -p, --posix    Write timing output to standard error in POSIX format
  -h, --help     Print help
  -V, --version  Print version
",
        "",
        0,
    );
}

#[test]
fn simple_test() {
    run_test_time_error_only(&["--", "ls", "-l"], "", "User time", 0);
}

#[test]
fn p_test() {
    run_test_time_error_only(&["-p", "--", "ls", "-l"], "", "user", 0);
}

#[test]
fn parse_error_test() {
    run_test_time_error_only(&[], "", "not provided", 0);
}

#[test]
fn command_error_test() {
    run_test_time_error_only(&["-s", "ls", "-l"], "", "unexpected argument '-s' found", 0);
}
