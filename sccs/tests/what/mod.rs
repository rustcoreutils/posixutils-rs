//
// Copyright (c) 2025-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};
use std::path::PathBuf;
use std::process::Output;

fn test_file_path(file_name: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push(file_name);
    path
}

#[test]
fn single_identification() {
    let file_path = test_file_path("single_identification.txt");
    let plan = TestPlan {
        cmd: String::from("what"),
        args: vec![file_path.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            output.status.success(),
            "what should succeed when matches found"
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("single_identification.txt:"),
            "should show filename"
        );
        assert!(
            stdout.contains("\tsingle_identification\n"),
            "should show identification with tab"
        );
    });
}

#[test]
fn multiple_identifications() {
    let file_path = test_file_path("multiple_identifications.txt");
    let plan = TestPlan {
        cmd: String::from("what"),
        args: vec![file_path.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            output.status.success(),
            "what should succeed when matches found"
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("multiple_identifications.txt:"),
            "should show filename"
        );
        assert!(
            stdout.contains("\tfirst_identification\n"),
            "should show first identification"
        );
        assert!(
            stdout.contains("\tsecond_identification\n"),
            "should show second identification"
        );
    });
}

#[test]
fn single_identification_flag() {
    let file_path = test_file_path("single_identification_flag.txt");
    let plan = TestPlan {
        cmd: String::from("what"),
        args: vec!["-s".into(), file_path.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            output.status.success(),
            "what should succeed when matches found"
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("single_identification_flag.txt:"),
            "should show filename"
        );
        assert!(
            stdout.contains("\tfirst_identification\n"),
            "should show first identification"
        );
        // With -s, should only show one match
        assert!(
            !stdout.contains("second_identification"),
            "should not show second with -s"
        );
    });
}

#[test]
fn no_identification() {
    let file_path = test_file_path("no_identification.txt");
    let plan = TestPlan {
        cmd: String::from("what"),
        args: vec![file_path.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1, // No matches found
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            !output.status.success(),
            "what should fail when no matches found"
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Should still print filename even with no matches
        assert!(
            stdout.contains("no_identification.txt:"),
            "should show filename"
        );
    });
}

#[test]
fn empty_file() {
    let file_path = test_file_path("empty_file.txt");
    let plan = TestPlan {
        cmd: String::from("what"),
        args: vec![file_path.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 1, // No matches found
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            !output.status.success(),
            "what should fail when no matches found"
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("empty_file.txt:"), "should show filename");
    });
}

#[test]
fn special_characters() {
    let file_path = test_file_path("special_characters.txt");
    let plan = TestPlan {
        cmd: String::from("what"),
        args: vec![file_path.to_string_lossy().into()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    run_test_with_checker(plan, |_plan: &TestPlan, output: &Output| {
        assert!(
            output.status.success(),
            "what should succeed when matches found"
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("special_characters.txt:"),
            "should show filename"
        );
        assert!(stdout.contains("\tspecial\n"), "should show special");
        assert!(stdout.contains("\tanother\n"), "should show another");
        assert!(stdout.contains("\tback\n"), "should show back");
        assert!(stdout.contains("\tnull\n"), "should show null");
    });
}

#[test]
fn what_binary_input_does_not_abort() {
    // Regression for the UTF-8 abort bug: `what` must scan arbitrary binary
    // input (its primary use case) and never error out on non-UTF-8 bytes.
    use std::io::Write;
    let dir = tempfile::TempDir::new().unwrap();
    let path = dir.path().join("bin.o");
    // Non-UTF-8 bytes (0xff, 0xfe) surrounding a valid @(#) identification.
    let mut data = vec![0xffu8, 0x00, 0xfe, b'x'];
    data.extend_from_slice(b"@(#)ident-here\" trailing");
    data.extend_from_slice(&[0x80, 0x81, 0xff]);
    std::fs::File::create(&path)
        .unwrap()
        .write_all(&data)
        .unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("what"),
            args: vec![path.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(
                output.status.success(),
                "what must exit 0 on a binary file containing an ident"
            );
            let out = String::from_utf8_lossy(&output.stdout);
            assert!(
                out.contains("\tident-here"),
                "expected the @(#) ident, got: {out:?}"
            );
        },
    );
}
