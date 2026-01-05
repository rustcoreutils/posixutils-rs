//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test_with_checker};
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

fn get_for_editing(sfile: &std::path::Path) {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("get"),
            args: vec!["-e".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "get -e should succeed");
        },
    );
}

#[test]
fn unget_removes_pfile_and_gfile() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");
    let pfile = tmp.path().join("p.test");
    let gfile = tmp.path().join("test");

    // Get for editing
    get_for_editing(&sfile);
    assert!(pfile.exists(), "p-file should exist");
    assert!(gfile.exists(), "g-file should exist");

    // Unget
    run_test_with_checker(
        TestPlan {
            cmd: String::from("unget"),
            args: vec![sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "unget should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(stdout.contains("1.2"), "should print the SID");
        },
    );

    // Both files should be removed
    assert!(!pfile.exists(), "p-file should be removed");
    assert!(!gfile.exists(), "g-file should be removed");
}

#[test]
fn unget_keep_gfile() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "keep", "content\n");
    let pfile = tmp.path().join("p.keep");
    let gfile = tmp.path().join("keep");

    // Get for editing
    get_for_editing(&sfile);
    assert!(gfile.exists(), "g-file should exist");

    // Unget with -n (keep g-file)
    run_test_with_checker(
        TestPlan {
            cmd: String::from("unget"),
            args: vec!["-n".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "unget -n should succeed");
        },
    );

    // p-file removed but g-file kept
    assert!(!pfile.exists(), "p-file should be removed");
    assert!(gfile.exists(), "g-file should be kept with -n");
}

#[test]
fn unget_no_pending_edit() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "noedit", "content\n");

    // Unget without pending edit should fail
    run_test_with_checker(
        TestPlan {
            cmd: String::from("unget"),
            args: vec![sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(
                !output.status.success(),
                "unget should fail without pending edit"
            );
        },
    );
}

#[test]
fn unget_silent_mode() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "silent", "content\n");

    get_for_editing(&sfile);

    // Unget with -s (silent)
    run_test_with_checker(
        TestPlan {
            cmd: String::from("unget"),
            args: vec!["-s".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "unget -s should succeed");
            assert!(
                output.stdout.is_empty(),
                "silent mode should produce no output"
            );
        },
    );
}
