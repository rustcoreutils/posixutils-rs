//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test, TestPlan};
use std::fs;
use std::path::PathBuf;

fn get_test_file_path(filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push("ed");
    path.push(filename);
    path
}

fn copy_test_file(src: &str, dest: &str) {
    let src_path = get_test_file_path(src);
    let dest_path = get_test_file_path(dest);
    fs::copy(src_path, dest_path).expect("Failed to copy test file");
}

struct TempFile {
    path: PathBuf,
}

impl TempFile {
    fn new(filename: &str) -> TempFile {
        let path = get_test_file_path(filename);
        TempFile { path }
    }
}

impl Drop for TempFile {
    fn drop(&mut self) {
        fs::remove_file(&self.path).expect("Failed to remove temporary file");
    }
}

fn run_ed_test(input_file: &str, stdin_file: &str, expected_output_file: &str) {
    let stdin_data = fs::read_to_string(get_test_file_path(stdin_file))
        .expect("Failed to read stdin input file");

    let expected_output = fs::read_to_string(get_test_file_path(expected_output_file))
        .expect("Failed to read expected output file");

    run_test(TestPlan {
        cmd: String::from("ed"),
        args: vec![get_test_file_path(input_file).to_string_lossy().to_string()],
        expected_out: expected_output,
        expected_err: String::new(),
        stdin_data,
        expected_exit_code: 0,
    });
}

#[test]
fn ed_print_basic() {
    run_ed_test("input1.txt", "print1.ed", "print1.txt");
}

#[test]
fn ed_insert_basic() {
    run_ed_test("input1.txt", "insert1.ed", "insert1.txt");
}

#[test]
fn ed_write_basic() {
    // Copy the original input file to a temporary file for the write test
    let _temp_file = TempFile::new("temporary1.txt");
    copy_test_file("input1.txt", "temporary1.txt");

    let stdin_data = fs::read_to_string(get_test_file_path("write1.ed"))
        .expect("Failed to read stdin input file");

    // No expected output in stdout for write command, as it writes to the file
    let expected_output = String::from("64\n");

    run_test(TestPlan {
        cmd: String::from("ed"),
        args: vec![get_test_file_path("temporary1.txt")
            .to_string_lossy()
            .to_string()],
        expected_out: expected_output,
        expected_err: String::new(),
        stdin_data,
        expected_exit_code: 0,
    });

    // Compare the temporary1.txt file with the expected output file write1.txt
    let temporary_output = fs::read_to_string(get_test_file_path("temporary1.txt"))
        .expect("Failed to read temporary output file");
    let expected_file_output = fs::read_to_string(get_test_file_path("write1.txt"))
        .expect("Failed to read expected output file");

    assert_eq!(
        temporary_output, expected_file_output,
        "The output file does not match the expected output file."
    );
}

#[test]
fn ed_delete_single_line() {
    run_ed_test("input1.txt", "delete_1.ed", "delete_1.txt");
}

#[test]
fn ed_delete_multiple_lines() {
    run_ed_test("input1.txt", "delete_2_to_4.ed", "delete_2_to_4.txt");
}

// #[test]
// fn ed_delete_all_lines() {
//     run_ed_test("input1.txt", "delete_all.ed", "delete_all.txt");
// }

#[test]
fn ed_delete_first_line() {
    run_ed_test("input1.txt", "delete_1.ed", "delete_1.txt");
}

#[test]
fn ed_delete_last_line() {
    run_ed_test("input1.txt", "delete_10.ed", "delete_10.txt");
}
