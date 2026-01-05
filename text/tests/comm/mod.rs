//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test, run_test_with_checker};
use std::fs;
use std::path::PathBuf;

fn get_test_file_path(filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push("comm");
    path.push(filename);
    path
}

fn run_comm_test(args: Vec<&str>, expected_output_filename: &str) {
    let expected_output = fs::read_to_string(get_test_file_path(expected_output_filename))
        .expect("Failed to read expected output file");

    let args: Vec<String> = args.iter().map(|&s| s.into()).collect();

    run_test(TestPlan {
        cmd: String::from("comm"),
        args,
        expected_out: expected_output,
        expected_err: String::new(),
        stdin_data: String::new(),
        expected_exit_code: 0,
    });
}
#[test]
fn comm_basic_functionality() {
    run_comm_test(
        vec!["tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.0",
    );
}

#[test]
fn comm_suppress_first_column() {
    run_comm_test(
        vec!["-1", "tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.1",
    );
}

#[test]
fn comm_suppress_second_column() {
    run_comm_test(
        vec!["-2", "tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.2",
    );
}

#[test]
fn comm_suppress_third_column() {
    run_comm_test(
        vec!["-3", "tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.3",
    );
}

#[test]
fn comm_suppress_columns_12() {
    run_comm_test(
        vec!["-12", "tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.12",
    );
}

#[test]
fn comm_suppress_columns_13() {
    run_comm_test(
        vec!["-13", "tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.13",
    );
}

#[test]
fn comm_suppress_columns_23() {
    run_comm_test(
        vec!["-23", "tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.23",
    );
}

#[test]
fn comm_suppress_columns_123() {
    run_comm_test(
        vec!["-123", "tests/comm/comm.file1", "tests/comm/comm.file2"],
        "comm.123",
    );
}

#[test]
fn comm_suppress_columns_separate_flags() {
    // Test that -1 -2 -3 works the same as -123
    run_comm_test(
        vec![
            "-1",
            "-2",
            "-3",
            "tests/comm/comm.file1",
            "tests/comm/comm.file2",
        ],
        "comm.123",
    );
}

#[test]
fn comm_empty_both_files() {
    run_comm_test(
        vec!["tests/comm/comm.empty", "tests/comm/comm.empty"],
        "comm.empty_both",
    );
}

#[test]
fn comm_empty_file1() {
    run_comm_test(
        vec!["tests/comm/comm.empty", "tests/comm/comm.file2"],
        "comm.empty_file1",
    );
}

#[test]
fn comm_empty_file2() {
    run_comm_test(
        vec!["tests/comm/comm.file1", "tests/comm/comm.empty"],
        "comm.empty_file2",
    );
}

#[test]
fn comm_stdin_file1() {
    // Test using "-" for file1 (stdin)
    let expected_output =
        fs::read_to_string(get_test_file_path("comm.0")).expect("Failed to read expected output");
    let file1_content =
        fs::read_to_string(get_test_file_path("comm.file1")).expect("Failed to read file1");

    run_test(TestPlan {
        cmd: String::from("comm"),
        args: vec![String::from("-"), String::from("tests/comm/comm.file2")],
        expected_out: expected_output,
        expected_err: String::new(),
        stdin_data: file1_content,
        expected_exit_code: 0,
    });
}

#[test]
fn comm_stdin_file2() {
    // Test using "-" for file2 (stdin)
    let expected_output =
        fs::read_to_string(get_test_file_path("comm.0")).expect("Failed to read expected output");
    let file2_content =
        fs::read_to_string(get_test_file_path("comm.file2")).expect("Failed to read file2");

    run_test(TestPlan {
        cmd: String::from("comm"),
        args: vec![String::from("tests/comm/comm.file1"), String::from("-")],
        expected_out: expected_output,
        expected_err: String::new(),
        stdin_data: file2_content,
        expected_exit_code: 0,
    });
}

#[test]
fn comm_missing_file_error() {
    // Test error handling for missing file
    // Note: error message text varies by OS, so we only check that stderr is non-empty
    run_test_with_checker(
        TestPlan {
            cmd: String::from("comm"),
            args: vec![
                String::from("tests/comm/nonexistent_file"),
                String::from("tests/comm/comm.file2"),
            ],
            expected_out: String::new(),
            expected_err: String::new(), // Not used by checker
            stdin_data: String::new(),
            expected_exit_code: 1,
        },
        |_plan, output| {
            assert!(output.stdout.is_empty(), "stdout should be empty");
            assert!(
                !output.stderr.is_empty(),
                "stderr should contain error message"
            );
            assert_eq!(output.status.code(), Some(1), "exit code should be 1");
        },
    );
}
