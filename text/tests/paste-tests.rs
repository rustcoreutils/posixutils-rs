//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

fn get_test_file_path(filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests/paste");
    path.push(filename);
    path
}

fn run_paste_test(args: Vec<&str>, expected_output_filename: &str) {
    let expected_output_file_path = get_test_file_path(expected_output_filename);

    let mut expected_output = String::new();
    File::open(expected_output_file_path)
        .unwrap()
        .read_to_string(&mut expected_output)
        .unwrap();

    let args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    run_test(TestPlan {
        cmd: String::from("paste"),
        args,
        expected_out: expected_output,
        expected_err: String::new(),
        stdin_data: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn paste_default_behavior() {
    run_paste_test(
        vec!["tests/paste/input1.txt", "tests/paste/input2.txt"],
        "output_default.txt",
    );
}

#[test]
fn paste_serial_mode() {
    run_paste_test(
        vec!["-s", "tests/paste/input1.txt", "tests/paste/input2.txt"],
        "output_serial.txt",
    );
}

#[test]
fn paste_custom_delimiter() {
    run_paste_test(
        vec![
            "-d",
            ",",
            "tests/paste/input1.txt",
            "tests/paste/input2.txt",
        ],
        "output_custom_delim.txt",
    );
}

#[test]
fn paste_serial_custom_delimiter() {
    run_paste_test(
        vec![
            "-s",
            "-d",
            ",",
            "tests/paste/input1.txt",
            "tests/paste/input2.txt",
        ],
        "output_serial_custom_delim.txt",
    );
}
