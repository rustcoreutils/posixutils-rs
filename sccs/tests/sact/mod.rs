//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test, run_test_with_checker};
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
fn sact_no_pending_edit() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    // No pending edit - should produce no output
    run_test(TestPlan {
        cmd: String::from("sact"),
        args: vec![sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn sact_with_pending_edit() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");

    // Get for editing to create p-file
    get_for_editing(&sfile);

    // sact should show the pending edit
    run_test_with_checker(
        TestPlan {
            cmd: String::from("sact"),
            args: vec![sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "sact should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(stdout.contains("1.1"), "should show old SID");
            assert!(stdout.contains("1.2"), "should show new SID");
        },
    );
}
