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

fn run_mkfifo_test(args: Vec<&str>, expected_exit_code: i32) {
    let plan = TestPlan {
        cmd: String::from("mkfifo"),
        args: args.iter().map(|&s| s.into()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    };

    run_test_with_checker(plan, |_, output: &Output| {
        assert_eq!(output.status.code(), Some(expected_exit_code));
    });
}

#[test]
fn test_create_single_fifo() {
    let fifo_path = "/tmp/posixutils_mkfifo_test_1";
    let _ = fs::remove_file(fifo_path);

    run_mkfifo_test(vec![fifo_path], 0);

    assert!(Path::new(fifo_path).exists());

    #[cfg(unix)]
    {
        use std::os::unix::fs::FileTypeExt;
        let metadata = fs::metadata(fifo_path).expect("Unable to get FIFO metadata");
        assert!(metadata.file_type().is_fifo());
    }

    fs::remove_file(fifo_path).expect("Unable to remove test FIFO");
}

#[test]
fn test_fifo_already_exists() {
    let fifo_path = "/tmp/posixutils_mkfifo_test_2";
    let _ = fs::remove_file(fifo_path);

    run_mkfifo_test(vec![fifo_path], 0);
    assert!(Path::new(fifo_path).exists());

    run_mkfifo_test(vec![fifo_path], 1);

    fs::remove_file(fifo_path).expect("Unable to remove test FIFO");
}

#[test]
fn test_invalid_mode() {
    let fifo_path = "/tmp/posixutils_mkfifo_test_3";
    let _ = fs::remove_file(fifo_path);

    run_mkfifo_test(vec!["-m", "invalid", fifo_path], 1);

    assert!(!Path::new(fifo_path).exists());
}

#[test]
fn test_set_fifo_mode_absolute() {
    let fifo_path = "/tmp/posixutils_mkfifo_test_4";
    let _ = fs::remove_file(fifo_path);

    run_mkfifo_test(vec!["-m", "644", fifo_path], 0);

    assert!(Path::new(fifo_path).exists());

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let metadata = fs::metadata(fifo_path).expect("Unable to get FIFO metadata");
        let permissions = metadata.permissions();
        assert_eq!(permissions.mode() & 0o777, 0o644);
    }

    fs::remove_file(fifo_path).expect("Unable to remove test FIFO");
}

#[test]
fn test_set_fifo_mode_symbolic_plus() {
    let fifo_path = "/tmp/posixutils_mkfifo_test_5";
    let _ = fs::remove_file(fifo_path);

    run_mkfifo_test(vec!["-m", "+x", fifo_path], 0);

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let metadata = fs::metadata(fifo_path).expect("Unable to get FIFO metadata");
        let permissions = metadata.permissions();
        assert_eq!(
            permissions.mode() & 0o777,
            0o777,
            "+x should produce rwxrwxrwx"
        );
    }

    fs::remove_file(fifo_path).expect("Unable to remove test FIFO");
}

#[test]
fn test_set_fifo_mode_symbolic_minus() {
    let fifo_path = "/tmp/posixutils_mkfifo_test_6";
    let _ = fs::remove_file(fifo_path);

    run_mkfifo_test(vec!["-m", "-w", fifo_path], 0);

    assert!(Path::new(fifo_path).exists());

    #[cfg(unix)]
    {
        use std::os::unix::fs::FileTypeExt;
        let metadata = fs::metadata(fifo_path).expect("Unable to get FIFO metadata");
        assert!(metadata.file_type().is_fifo());
    }

    fs::remove_file(fifo_path).expect("Unable to remove test FIFO");
}

#[test]
fn test_set_fifo_mode_symbolic_who_specified() {
    let fifo_path = "/tmp/posixutils_mkfifo_test_7";
    let _ = fs::remove_file(fifo_path);

    run_mkfifo_test(vec!["-m", "a-w", fifo_path], 0);

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let metadata = fs::metadata(fifo_path).expect("Unable to get FIFO metadata");
        let permissions = metadata.permissions();
        assert_eq!(
            permissions.mode() & 0o777,
            0o444,
            "a-w should produce r--r--r--"
        );
    }

    fs::remove_file(fifo_path).expect("Unable to remove test FIFO");
}

#[test]
fn test_create_multiple_fifos() {
    let fifo1 = "/tmp/posixutils_mkfifo_test_8a";
    let fifo2 = "/tmp/posixutils_mkfifo_test_8b";
    let _ = fs::remove_file(fifo1);
    let _ = fs::remove_file(fifo2);

    run_mkfifo_test(vec![fifo1, fifo2], 0);

    assert!(Path::new(fifo1).exists());
    assert!(Path::new(fifo2).exists());

    fs::remove_file(fifo1).expect("Unable to remove test FIFO");
    fs::remove_file(fifo2).expect("Unable to remove test FIFO");
}
