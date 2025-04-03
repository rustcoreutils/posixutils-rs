//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs::File;
use std::io::Write;
use std::os::unix::fs::symlink;

use plib::testing::{run_test, TestPlan};
use tempfile::tempdir;

#[test]
fn test_readlink_valid_symlink() {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("file.txt");
    let symlink_path = dir.path().join("symlink.txt");

    let mut file = File::create(&file_path).unwrap();
    writeln!(file, "Hello, world!").unwrap();
    symlink(&file_path, &symlink_path).unwrap();

    run_test(TestPlan {
        cmd: String::from("readlink"),
        args: vec![symlink_path.to_str().unwrap().to_string()],
        stdin_data: String::new(),
        expected_out: format!("{}\n", file_path.to_str().unwrap()),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_readlink_valid_symlink_no_newline() {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("file.txt");
    let symlink_path = dir.path().join("symlink.txt");

    let mut file = File::create(&file_path).unwrap();
    writeln!(file, "Hello, world!").unwrap();
    symlink(&file_path, &symlink_path).unwrap();

    run_test(TestPlan {
        cmd: String::from("readlink"),
        args: vec![
            String::from("-n"),
            symlink_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: format!("{}", file_path.to_str().unwrap()),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_readlink_non_existent_file() {
    let dir = tempdir().unwrap();
    let non_existent_path = dir.path().join("non_existent.txt");

    run_test(TestPlan {
        cmd: String::from("readlink"),
        args: vec![
            "-v".to_owned(),
            non_existent_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: format!(
            "readlink: {}: No such file or directory\n",
            non_existent_path.to_str().unwrap()
        ),
        expected_exit_code: 1,
    });
}

#[test]
fn test_readlink_not_a_symlink() {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("file.txt");

    let mut file = File::create(&file_path).unwrap();
    writeln!(file, "Hello, world!").unwrap();

    run_test(TestPlan {
        cmd: String::from("readlink"),
        args: vec!["-v".to_owned(), file_path.to_str().unwrap().to_string()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: format!(
            "readlink: {}: Not a symbolic link\n",
            file_path.to_str().unwrap()
        ),
        expected_exit_code: 1,
    });
}
