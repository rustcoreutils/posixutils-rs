//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test_with_checker};
use std::fs;
use std::path::Path;
use std::process::Output;
use tempfile::{TempDir, tempdir};

fn setup_test_env() -> (TempDir, String) {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let dir_path = temp_dir.path().join("testdir");
    (temp_dir, dir_path.to_str().unwrap().to_string())
}

fn run_mkdir_test(args: Vec<&str>, expected_exit_code: i32, expected_err_substr: &str) {
    let plan = TestPlan {
        cmd: String::from("mkdir"),
        args: args.iter().map(|&s| s.into()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    };

    run_test_with_checker(plan, move |_, output: &Output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains(expected_err_substr),
            "Expected substring not found in stderr: '{}'",
            stderr
        );
    });
}

#[test]
fn test_create_single_directory() {
    let (_temp_dir, dir_path) = setup_test_env();

    run_mkdir_test(vec![&dir_path], 0, "");

    // Ensure the directory has been created
    assert!(Path::new(&dir_path).exists());

    // Clean up
    fs::remove_dir(&dir_path).expect("Unable to remove test directory");
}

#[test]
fn test_directory_already_exists() {
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    run_mkdir_test(vec![&dir_path], 1, "File exists");

    // Ensure the directory still exists
    assert!(Path::new(&dir_path).exists());

    // Clean up
    fs::remove_dir(&dir_path).expect("Unable to remove test directory");
}

#[test]
fn test_invalid_mode() {
    let (_temp_dir, dir_path) = setup_test_env();

    run_mkdir_test(vec!["-m", "invalid", &dir_path], 1, "invalid mode string");

    // Ensure the directory has not been created
    assert!(!Path::new(&dir_path).exists());
}

#[test]
fn test_set_directory_mode() {
    let (_temp_dir, dir_path) = setup_test_env();

    run_mkdir_test(vec!["-m", "755", &dir_path], 0, "");

    // Ensure the directory has been created
    assert!(Path::new(&dir_path).exists());

    // Check the directory permissions
    let metadata = fs::metadata(&dir_path).expect("Unable to get directory metadata");
    let permissions = metadata.permissions();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        assert_eq!(permissions.mode() & 0o777, 0o755);
    }

    // Clean up
    fs::remove_dir(&dir_path).expect("Unable to remove test directory");
}
