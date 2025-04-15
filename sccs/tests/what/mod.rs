//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test};
use std::path::PathBuf;

fn test_file_path(file_name: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push(file_name);
    path
}

fn generate_test_plan(args: Vec<&str>, expected_out: &str) -> TestPlan {
    TestPlan {
        cmd: String::from("what"),
        args: args.iter().map(|&s| String::from(s)).collect(),
        stdin_data: String::new(),
        expected_out: String::from(expected_out),
        expected_err: String::new(),
        expected_exit_code: 0,
    }
}

#[test]
fn single_identification() {
    let file_path = test_file_path("single_identification.txt");
    let plan = generate_test_plan(
        vec![file_path.to_str().unwrap()],
        "@(#)single_identification\n",
    );
    run_test(plan);
}

#[test]
fn multiple_identifications() {
    let file_path = test_file_path("multiple_identifications.txt");
    let plan = generate_test_plan(
        vec![file_path.to_str().unwrap()],
        "@(#)first_identification\n@(#)second_identification\n",
    );
    run_test(plan);
}

#[test]
fn single_identification_flag() {
    let file_path = test_file_path("single_identification_flag.txt");
    let plan = generate_test_plan(
        vec!["-s", file_path.to_str().unwrap()],
        "@(#)first_identification\n",
    );
    run_test(plan);
}

#[test]
fn no_identification() {
    let file_path = test_file_path("no_identification.txt");
    let plan = generate_test_plan(vec![file_path.to_str().unwrap()], "");
    run_test(plan);
}

#[test]
fn empty_file() {
    let file_path = test_file_path("empty_file.txt");
    let plan = generate_test_plan(vec![file_path.to_str().unwrap()], "");
    run_test(plan);
}

#[test]
fn special_characters() {
    let file_path = test_file_path("special_characters.txt");
    let plan = generate_test_plan(
        vec![file_path.to_str().unwrap()],
        "@(#)special\n@(#)another\n@(#)back\n@(#)null\n",
    );
    run_test(plan);
}
