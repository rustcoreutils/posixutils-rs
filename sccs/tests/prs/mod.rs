//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};
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
fn prs_default_output() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec![sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "prs should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should contain filename, delta info, MRs, COMMENTS
            assert!(stdout.contains("s.test"), "should show filename");
            assert!(stdout.contains("D 1.1"), "should show delta type and SID");
            assert!(stdout.contains("MRs:"), "should show MRs label");
            assert!(stdout.contains("COMMENTS:"), "should show COMMENTS label");
        },
    );
}

#[test]
fn prs_dataspec() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec!["-d:I:".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::from("1.1"),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "prs should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(stdout.trim() == "1.1", "should show SID");
        },
    );
}

#[test]
fn prs_delta_type() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec!["-d:DT:".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::from("D"),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "prs should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(stdout.trim() == "D", "delta type should be D");
        },
    );
}

#[test]
fn prs_line_counts() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "line1\nline2\nline3\n");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec!["-d:Li:/:Ld:/:Lu:".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "prs should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            // First delta has 3 lines inserted, 0 deleted, 0 unchanged
            assert!(
                stdout.contains("3/0/0") || stdout.contains("00003/00000/00000"),
                "should show line counts"
            );
        },
    );
}

#[test]
fn prs_specific_sid() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec![
                "-r1.1".into(),
                "-d:I:".into(),
                sfile.to_string_lossy().into(),
            ],
            stdin_data: String::new(),
            expected_out: String::from("1.1"),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "prs should succeed");
        },
    );
}

#[test]
fn prs_filename() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("prs"),
            args: vec!["-d:F:".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "prs should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(stdout.contains("s.test"), "should show filename");
        },
    );
}
