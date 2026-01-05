//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test, run_test_with_checker};
use std::fs;
use std::path::PathBuf;
use std::process::Output;
use tempfile::TempDir;

fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(name)
}

#[test]
fn admin_check_valid_file() {
    // Test -h (check) on a valid SCCS file
    let fixture = fixture_path("s.simple");
    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec!["-h".into(), fixture.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn admin_check_all_fixtures() {
    // Test -h on all fixtures
    for name in &["s.simple", "s.multi", "s.keywords", "s.branched"] {
        let fixture = fixture_path(name);
        run_test(TestPlan {
            cmd: String::from("admin"),
            args: vec!["-h".into(), fixture.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        });
    }
}

#[test]
fn admin_create_new_file_stdin() {
    // Test creating new SCCS file from stdin
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.newfile");

    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![sfile.to_string_lossy().into(), "-i".into()],
        stdin_data: String::from("hello\nworld\n"),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    // Verify file was created
    assert!(sfile.exists(), "SCCS file should be created");

    // Verify with -h
    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec!["-h".into(), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn admin_create_new_file_from_file() {
    // Test creating new SCCS file from an existing file
    let tmp = TempDir::new().unwrap();
    let input = tmp.path().join("input.txt");
    let sfile = tmp.path().join("s.fromfile");

    // Create input file
    fs::write(&input, "line one\nline two\nline three\n").unwrap();

    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![
            sfile.to_string_lossy().into(),
            format!("-i{}", input.to_string_lossy()),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    // Verify file was created
    assert!(sfile.exists(), "SCCS file should be created");
}

#[test]
fn admin_error_file_exists() {
    // Test error when creating file that already exists
    let fixture = fixture_path("s.simple");

    let plan = TestPlan {
        cmd: String::from("admin"),
        args: vec!["-n".into(), fixture.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(!output.status.success(), "should fail when file exists");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("file already exists"),
            "error message should mention file exists"
        );
    });
}

#[test]
fn admin_error_invalid_name() {
    // Test error for invalid SCCS file name (not starting with s.)
    let plan = TestPlan {
        cmd: String::from("admin"),
        args: vec!["-n".into(), "invalid.txt".into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(!output.status.success(), "should fail for invalid name");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("s."),
            "error message should mention s. requirement"
        );
    });
}

#[test]
fn admin_recompute_checksum() {
    // Test -z (recompute checksum)
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.checksum");

    // Create a new SCCS file first
    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![sfile.to_string_lossy().into(), "-i".into()],
        stdin_data: String::from("test content\n"),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    // Make file writable for -z
    let perms = std::os::unix::fs::PermissionsExt::from_mode(0o644);
    fs::set_permissions(&sfile, perms).ok();

    // Recompute checksum
    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec!["-z".into(), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    // Verify with -h
    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec!["-h".into(), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn admin_create_with_initial_sid() {
    // Test creating file with initial SID
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.withsid");

    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![sfile.to_string_lossy().into(), "-i".into(), "-r2.1".into()],
        stdin_data: String::from("content\n"),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(sfile.exists());
}

#[test]
fn admin_create_with_comment() {
    // Test creating file with custom comment
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.withcomment");

    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![
            sfile.to_string_lossy().into(),
            "-i".into(),
            "-yMy custom comment".into(),
        ],
        stdin_data: String::from("content\n"),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(sfile.exists());

    // Verify comment is in file
    let content = fs::read_to_string(&sfile).unwrap();
    assert!(
        content.contains("My custom comment"),
        "Comment should be in file"
    );
}
