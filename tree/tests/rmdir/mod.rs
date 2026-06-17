//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};
use std::fs;
use std::path::Path;
use std::process::Output;
use tempfile::{tempdir, TempDir};

fn setup_test_env() -> (TempDir, String) {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let dir_path = temp_dir.path().join("testdir");
    (temp_dir, dir_path.to_str().unwrap().to_string())
}

fn run_rmdir_test(args: Vec<&str>, expected_exit_code: i32, expected_err_substr: &str) {
    let plan = TestPlan {
        cmd: String::from("rmdir"),
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
fn rmdir_remove_existing_directory() {
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    run_rmdir_test(vec![&dir_path], 0, "");

    // Ensure the directory has been removed
    assert!(!Path::new(&dir_path).exists());
}

#[test]
fn rmdir_remove_non_empty_directory() {
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");
    let file_path = Path::new(&dir_path).join("file.txt");
    fs::write(&file_path, b"test").expect("Unable to create test file");

    run_rmdir_test(vec![&dir_path], 1, "Directory not empty");

    // Ensure the directory still exists
    assert!(Path::new(&dir_path).exists());

    // Clean up
    fs::remove_file(file_path).expect("Unable to remove test file");
    fs::remove_dir(&dir_path).expect("Unable to remove test directory");
}

#[test]
fn rmdir_remove_non_existent_directory() {
    let (_temp_dir, dir_path) = setup_test_env();

    run_rmdir_test(vec![&dir_path], 1, "No such file or directory");

    // Ensure the directory still does not exist
    assert!(!Path::new(&dir_path).exists());
}

#[test]
fn rmdir_remove_directory_with_parents() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let parent_dir = temp_dir.path().join("parent");
    let dir_path = parent_dir.join("testdir");

    fs::create_dir_all(&dir_path).expect("Unable to create test directories");

    run_rmdir_test(vec!["-p", dir_path.to_str().unwrap()], 0, "");

    // Ensure the directories have been removed
    assert!(!dir_path.exists());
    assert!(!parent_dir.exists());
}

// Audit #RD1: `rmdir -p` on a non-empty parent reports the parent that actually failed (not the
// original operand), removes the empty leaf chain, and exits 1.
#[test]
fn test_rmdir_p_names_failing_parent() {
    let test_dir = &format!(
        "{}/test_rmdir_p_names_failing_parent",
        env!("CARGO_TARGET_TMPDIR")
    );
    let a = &format!("{test_dir}/a");
    let c = &format!("{test_dir}/a/b/c");
    fs::create_dir_all(c).unwrap();
    fs::File::create(format!("{a}/keep")).unwrap(); // makes `a` non-empty

    let out = std::process::Command::new(env!("CARGO_BIN_EXE_rmdir"))
        .args(["-p", c])
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains(&format!("rmdir: {a}: ")),
        "diagnostic should name the failing parent: {stderr}"
    );
    assert!(!Path::new(c).exists(), "empty leaf chain removed");
    assert!(Path::new(a).exists(), "non-empty parent kept");

    fs::remove_dir_all(test_dir).unwrap();
}
