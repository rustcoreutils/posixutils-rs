//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib_testing::{run_test_with_checker, TestPlan};
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process::Output;

fn get_test_file_path(filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push(filename);
    path
}

fn run_link_test(
    source: &str,
    target: &str,
    expected_err: &str,
    expected_exit_code: i32,
    checker: impl FnMut(&TestPlan, &Output),
) {
    let source_path = get_test_file_path(source);
    let target_path = get_test_file_path(target);

    // Clean up before the test
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&target_path);

    // Create source file
    let mut file = fs::File::create(&source_path).expect("Unable to create source file");
    writeln!(file, "Hello, world!").expect("Unable to write to source file");

    let args = vec![
        source_path.to_str().unwrap().to_string(),
        target_path.to_str().unwrap().to_string(),
    ];

    run_test_with_checker(
        TestPlan {
            cmd: "link".to_string(),
            args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: expected_err.to_string(),
            expected_exit_code,
        },
        checker,
    );

    // Clean up after the test
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&target_path);
}

#[test]
fn link_create() {
    let source_filename = "source1.txt";
    let target_filename = "target1.txt";

    run_link_test(source_filename, target_filename, "", 0, |_, output| {
        // Check that the link was created
        assert!(
            output.status.success(),
            "Link creation failed with output: {:?}",
            output
        );
        let source_path = get_test_file_path(source_filename);
        let target_path = get_test_file_path(target_filename);

        // Ensure the target is a link to the source
        assert!(
            fs::metadata(&target_path).is_ok(),
            "Target file does not exist"
        );
        assert_eq!(fs::read_to_string(&source_path).unwrap(), "Hello, world!\n");
        assert_eq!(fs::read_to_string(&target_path).unwrap(), "Hello, world!\n");
    });
}

#[test]
fn link_already_exists() {
    let source_filename = "source2.txt";
    let target_filename = "target2.txt";

    let source_path = get_test_file_path(source_filename);
    let target_path = get_test_file_path(target_filename);

    // Clean up before the test
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&target_path);

    // Create source file
    let mut file = fs::File::create(&source_path).expect("Unable to create source file");
    writeln!(file, "Hello, world!").expect("Unable to write to source file");

    // Create the target file to trigger the "already exists" error
    let _ = fs::File::create(&target_path).expect("Unable to create target file");

    let args = vec![
        source_path.to_str().unwrap().to_string(),
        target_path.to_str().unwrap().to_string(),
    ];

    run_test_with_checker(
        TestPlan {
            cmd: "link".to_string(),
            args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: format!(
                "link: {}: File exists (os error 17)\n",
                target_path.to_str().unwrap()
            ),
            expected_exit_code: 1,
        },
        |_, output| {
            assert_eq!(
                output.status.code(),
                Some(1),
                "Link creation failed with unexpected exit code: {:?}",
                output
            );
        },
    );

    // Clean up after the test
    let _ = fs::remove_file(&source_path);
    let _ = fs::remove_file(&target_path);
}
