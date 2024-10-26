//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib_testing::{run_test, TestPlan};
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
