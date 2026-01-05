//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test, run_test_with_checker};
use std::path::PathBuf;
use std::process::Output;
use tempfile::TempDir;

fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(name)
}

// Helper to run get tests that check stdout only, allowing any stderr
fn run_get_test_stdout_only(args: Vec<String>, expected_out: &str) {
    let plan = TestPlan {
        cmd: String::from("get"),
        args,
        stdin_data: String::new(),
        expected_out: String::from(expected_out),
        expected_err: String::new(), // ignored by checker
        expected_exit_code: 0,
    };

    run_test_with_checker(plan, |plan: &TestPlan, output: &Output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert_eq!(stdout, plan.expected_out, "stdout mismatch");
        assert!(output.status.success(), "command should succeed");
    });
}

#[test]
fn get_simple_to_stdout() {
    // Test -p (print to stdout)
    let fixture = fixture_path("s.simple");
    run_get_test_stdout_only(
        vec!["-p".into(), fixture.to_string_lossy().into()],
        "line1\nline2\nline3\n",
    );
}

#[test]
fn get_multi_latest_version() {
    // Test getting latest version of multi-delta file
    let fixture = fixture_path("s.multi");
    run_get_test_stdout_only(
        vec!["-p".into(), fixture.to_string_lossy().into()],
        "line1\nmodified-line2\nline3\nline4\n",
    );
}

#[test]
fn get_specific_version() {
    // Test -r (specific SID)
    let fixture = fixture_path("s.multi");
    run_get_test_stdout_only(
        vec![
            "-p".into(),
            "-r1.1".into(),
            fixture.to_string_lossy().into(),
        ],
        "line1\nline2\nline3\n",
    );
}

#[test]
fn get_version_1_2() {
    // Test getting version 1.2
    let fixture = fixture_path("s.multi");
    run_get_test_stdout_only(
        vec![
            "-p".into(),
            "-r1.2".into(),
            fixture.to_string_lossy().into(),
        ],
        "line1\nline2\nline3\nline4\n",
    );
}

#[test]
fn get_keywords_expanded() {
    // Test keyword expansion
    let fixture = fixture_path("s.keywords");
    run_get_test_stdout_only(
        vec!["-p".into(), fixture.to_string_lossy().into()],
        "@(#)keywords\t1.1\n1.1\nkeywords\n@(#)\n",
    );
}

#[test]
fn get_keywords_suppressed() {
    // Test -k (suppress keyword expansion)
    let fixture = fixture_path("s.keywords");
    run_get_test_stdout_only(
        vec!["-p".into(), "-k".into(), fixture.to_string_lossy().into()],
        "%W%\n%I%\n%M%\n%Z%\n",
    );
}

#[test]
fn get_branched_trunk() {
    // Test getting trunk version from branched file
    let fixture = fixture_path("s.branched");
    run_get_test_stdout_only(
        vec!["-p".into(), fixture.to_string_lossy().into()],
        "line1\nline2\nline3\ntrunk-1.2\n",
    );
}

#[test]
fn get_branched_version() {
    // Test getting branch version
    let fixture = fixture_path("s.branched");
    run_get_test_stdout_only(
        vec![
            "-p".into(),
            "-r1.1.1.1".into(),
            fixture.to_string_lossy().into(),
        ],
        "line1\nline2\nline3\nbranch-1.1.1.1\n",
    );
}

#[test]
fn get_for_editing() {
    // Test -e (get for editing) - creates p-file
    let tmp = TempDir::new().unwrap();
    let sfile = tmp.path().join("s.editme");
    let pfile = tmp.path().join("p.editme");
    let gfile = tmp.path().join("editme");

    run_test(TestPlan {
        cmd: String::from("admin"),
        args: vec![sfile.to_string_lossy().into(), "-i".into()],
        stdin_data: String::from("content\n"),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    // Get for editing - use checker to verify g-file and p-file exist
    let plan = TestPlan {
        cmd: String::from("get"),
        args: vec!["-e".into(), sfile.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(output.status.success(), "get -e should succeed");
    });

    // Verify g-file was created
    assert!(gfile.exists(), "g-file should be created");

    // Verify p-file was created
    assert!(pfile.exists(), "p-file should be created");
}

#[test]
fn get_silent_mode() {
    // Test -s (silent mode) - no version info in stderr
    let fixture = fixture_path("s.simple");
    run_test(TestPlan {
        cmd: String::from("get"),
        args: vec!["-p".into(), "-s".into(), fixture.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::from("line1\nline2\nline3\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn get_error_file_not_found() {
    // Test error when file doesn't exist
    let plan = TestPlan {
        cmd: String::from("get"),
        args: vec!["-p".into(), "s.nonexistent".into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1,
    };

    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            !output.status.success(),
            "get on nonexistent file should fail"
        );
    });
}

#[test]
fn get_version_1_1_from_branched() {
    // Test getting base version from branched file
    let fixture = fixture_path("s.branched");
    run_get_test_stdout_only(
        vec![
            "-p".into(),
            "-r1.1".into(),
            fixture.to_string_lossy().into(),
        ],
        "line1\nline2\nline3\n",
    );
}
