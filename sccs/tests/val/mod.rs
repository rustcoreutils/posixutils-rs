//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::fs;
use std::process::Output;
use tempfile::TempDir;

fn create_sccs_file(tmp: &TempDir, name: &str, content: &str) -> std::path::PathBuf {
    let sfile = tmp.path().join(format!("s.{}", name));

    run_test_with_checker(
        TestPlan {
            cmd: String::from("admin"),
            args: vec![sfile.to_string_lossy().into(), "-i".into()],
            stdin_data: String::from(content),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "admin should succeed");
        },
    );

    sfile
}

#[test]
fn val_valid_file() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "valid", "test content\n");

    run_test(TestPlan {
        cmd: String::from("val"),
        args: vec![sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn val_invalid_sid() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    // SID 2.1 doesn't exist
    run_test_with_checker(
        TestPlan {
            cmd: String::from("val"),
            args: vec!["-r2.1".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0, // Non-zero, but we check below
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(
                !output.status.success(),
                "val should fail for non-existent SID"
            );
        },
    );
}

#[test]
fn val_valid_sid() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    // SID 1.1 exists
    run_test(TestPlan {
        cmd: String::from("val"),
        args: vec!["-r1.1".into(), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn val_not_sccs_file() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("notsccs");
    fs::write(&file, "not an sccs file\n").unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("val"),
            args: vec![file.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(
                !output.status.success(),
                "val should fail for non-SCCS file"
            );
        },
    );
}

#[test]
fn val_no_file_argument() {
    // Missing file argument must yield exit bit 0x80 (128), not clap's exit 2.
    run_test_with_checker(
        TestPlan {
            cmd: String::from("val"),
            args: vec![],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert_eq!(output.status.code(), Some(0x80), "missing file => 0x80");
        },
    );
}

#[test]
fn val_unknown_keyletter() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    // An unknown keyletter must yield exit bit 0x40 (64), not clap's exit 2.
    run_test_with_checker(
        TestPlan {
            cmd: String::from("val"),
            args: vec!["-Q".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert_eq!(output.status.code(), Some(0x40), "unknown option => 0x40");
        },
    );
}

#[test]
fn val_duplicate_keyletter() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    // A duplicated keyletter must yield exit bit 0x40 (64).
    run_test_with_checker(
        TestPlan {
            cmd: String::from("val"),
            args: vec![
                "-r1.1".into(),
                "-r1.1".into(),
                sfile.to_string_lossy().into(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert_eq!(output.status.code(), Some(0x40), "duplicate option => 0x40");
        },
    );
}

#[test]
fn val_stdin_discrepancy_format() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "x", "content\n");
    let line = format!("-y nomatch {}", sfile.to_string_lossy());

    // POSIX stdin format: "%s\n\n %s: %s\n", <input>, <pathname>, <string>
    run_test_with_checker(
        TestPlan {
            cmd: String::from("val"),
            args: vec!["-".into()],
            stdin_data: format!("{}\n", line),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let expected = format!("{}\n\n {}: %Y%, -y mismatch\n", line, sfile.display());
            assert_eq!(stdout, expected, "stdin discrepancy format");
            assert_eq!(output.status.code(), Some(0x02), "-y mismatch => 0x02");
        },
    );
}

#[test]
fn val_silent_mode() {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("notsccs");
    fs::write(&file, "not an sccs file\n").unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("val"),
            args: vec!["-s".into(), file.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(!output.status.success(), "val should fail");
            // Silent mode - no output
            assert!(output.stdout.is_empty(), "no stdout in silent mode");
        },
    );
}
