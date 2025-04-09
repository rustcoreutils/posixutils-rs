//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib_testing::{run_test, TestPlan};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

fn get_test_file_path(filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests/fold");
    path.push(filename);
    path
}

fn run_fold_test(args: Vec<&str>, input_filename: &str, expected_output_filename: &str) {
    let input_file_path = get_test_file_path(input_filename);
    let input_data = match File::open(input_file_path) {
        Ok(mut file) => {
            let mut data = String::new();
            file.read_to_string(&mut data).unwrap();
            data
        }
        Err(e) => {
            panic!("Error opening file: {}", e);
        }
    };

    let expected_output_file_path = get_test_file_path(expected_output_filename);

    let mut expected_output = String::new();
    File::open(expected_output_file_path)
        .unwrap()
        .read_to_string(&mut expected_output)
        .unwrap();

    let args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    run_test(TestPlan {
        cmd: String::from("fold"),
        args,
        expected_out: expected_output,
        expected_err: String::new(),
        expected_exit_code: 0,
        stdin_data: input_data,
    });
}

#[test]
fn fold_default_behavior() {
    run_fold_test(vec![], "input1.txt", "output_default.txt");
}

#[test]
fn fold_custom_width() {
    run_fold_test(vec!["-w", "40"], "input1.txt", "output_width40.txt");
}

#[test]
fn fold_bytes_mode() {
    run_fold_test(vec!["-b"], "input1.txt", "output_bytes.txt");
}

#[test]
fn fold_spaces_mode() {
    run_fold_test(vec!["-s"], "input2.txt", "output_spaces.txt");
}

#[test]
fn fold_bytes_and_spaces_mode() {
    run_fold_test(vec!["-b", "-s"], "input2.txt", "output_bytes_spaces.txt");
}
