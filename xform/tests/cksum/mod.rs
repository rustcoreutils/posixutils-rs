//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};

fn cksum_test(test_data: &str, expected_output: &str) {
    run_test(TestPlan {
        cmd: String::from("cksum"),
        args: Vec::new(),
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn cksum_basic() {
    cksum_test("foo\n", "3915528286 4\n");
}

use plib::testing::{run_test_u8, run_test_with_checker, TestPlanU8};
use std::fs;
use std::path::PathBuf;

fn temp_path(name: &str) -> PathBuf {
    std::env::temp_dir().join(format!("posixutils_cksum_{}", name))
}

#[test]
fn cksum_empty_input() {
    // No bytes: CRC of the appended length 0 only.
    cksum_test("", "4294967295 0\n");
}

#[test]
fn cksum_binary_stdin_non_utf8() {
    // cksum must operate on raw bytes, not text.
    run_test_u8(TestPlanU8 {
        cmd: String::from("cksum"),
        args: Vec::new(),
        stdin_data: vec![0x00, 0x01, 0x02, 0xFF, 0xFE],
        expected_out: b"916924328 5\n".to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn cksum_named_file_operand() {
    // A named operand prints the pathname after the checksum and size.
    let path = temp_path("named.txt");
    fs::write(&path, b"foo\n").unwrap();

    let arg = path.to_str().unwrap().to_string();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("cksum"),
            args: vec![arg.clone()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert_eq!(stdout, format!("3915528286 4 {}\n", arg));
        },
    );

    let _ = fs::remove_file(&path);
}

#[test]
fn cksum_multiple_file_operands() {
    // Multiple operands are processed in order, each on its own line.
    let a = temp_path("multi_a.txt");
    let b = temp_path("multi_b.txt");
    fs::write(&a, b"foo\n").unwrap();
    fs::write(&b, b"hello\n").unwrap();

    let arg_a = a.to_str().unwrap().to_string();
    let arg_b = b.to_str().unwrap().to_string();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("cksum"),
            args: vec![arg_a.clone(), arg_b.clone()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert_eq!(
                stdout,
                format!("3915528286 4 {}\n3015617425 6 {}\n", arg_a, arg_b)
            );
        },
    );

    let _ = fs::remove_file(&a);
    let _ = fs::remove_file(&b);
}

#[test]
fn cksum_per_file_error_exit_1() {
    // A per-file error (unreadable operand) yields a diagnostic and exit 1.
    run_test_with_checker(
        TestPlan {
            cmd: String::from("cksum"),
            args: vec![String::from("/nonexistent/cksum/path")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_plan, output| {
            assert!(output.stdout.is_empty(), "no stdout on error");
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.starts_with("cksum: "),
                "diagnostic should carry the cksum: prefix, got: {}",
                stderr
            );
        },
    );
}
