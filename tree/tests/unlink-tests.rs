//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test, run_test_with_checker, TestPlan};
use std::fs;
use tempfile::tempdir;

#[test]
fn test_remove_existing_file() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let file_path = temp_dir.path().join("testfile.txt");

    // Create the file to be unlinked
    fs::write(&file_path, b"test").expect("Unable to create test file");

    let test_plan = TestPlan {
        cmd: String::from("unlink"),
        args: vec![file_path.to_str().unwrap().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0, // We expect success
    };

    run_test(test_plan);

    // Ensure the file has been removed
    assert!(!file_path.exists());
}

#[test]
fn test_remove_non_existing_file() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let file_path = temp_dir.path().join("non_existing.txt");

    let test_plan = TestPlan {
        cmd: String::from("unlink"),
        args: vec![file_path.to_str().unwrap().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: format!(
            "unlink: {}: No such file or directory (os error 2)\n",
            file_path.to_str().unwrap()
        ),
        expected_exit_code: 1, // We expect failure
    };

    run_test(test_plan);
}

#[test]
fn test_remove_directory() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let dir_path = temp_dir.path().join("testdir");

    // Create the directory to be unlinked
    fs::create_dir_all(&dir_path).expect("Unable to create test directory");

    let test_plan = TestPlan {
        cmd: String::from("unlink"),
        args: vec![dir_path.to_str().unwrap().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(), // We will check this in the checker function
        expected_exit_code: 1,       // We expect failure
    };

    run_test_with_checker(test_plan, |_plan, output| {
        assert_eq!(output.status.code(), Some(1));
        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("unlink: "));
    });

    // Ensure the directory still exists
    assert!(dir_path.exists());

    // Clean up
    fs::remove_dir_all(dir_path).expect("Unable to remove test directory");
}
