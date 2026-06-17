//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};
use std::fs::File;
use std::io::Write;
use std::os::unix::fs::symlink;
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
        expected_out: file_path.to_str().unwrap().to_string(),
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

// Audit #RL1: readlink on a non-symlink writes a diagnostic to stderr and exits 1, even without -v.
#[test]
fn test_readlink_not_symlink_diagnoses() {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("file.txt");
    File::create(&file_path).unwrap();

    let out = std::process::Command::new(env!("CARGO_BIN_EXE_readlink"))
        .arg(&file_path)
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("Not a symbolic link"),
        "expected a diagnostic: {stderr:?}"
    );
}
