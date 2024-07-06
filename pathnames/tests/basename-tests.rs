//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test, TestPlan};

#[test]
fn test_basename_basic() {
    run_test(TestPlan {
        cmd: String::from("basename"),
        args: vec![String::from("/usr/bin/env")],
        stdin_data: String::new(),
        expected_out: String::from("env\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_basename_trailing_slash() {
    run_test(TestPlan {
        cmd: String::from("basename"),
        args: vec![String::from("/usr/bin/")],
        stdin_data: String::new(),
        expected_out: String::from("bin\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_basename_with_suffix() {
    run_test(TestPlan {
        cmd: String::from("basename"),
        args: vec![String::from("/usr/bin/env"), String::from("v")],
        stdin_data: String::new(),
        expected_out: String::from("en\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_basename_current_directory() {
    run_test(TestPlan {
        cmd: String::from("basename"),
        args: vec![String::from(".")],
        stdin_data: String::new(),
        expected_out: String::from(".\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_basename_empty_path() {
    run_test(TestPlan {
        cmd: String::from("basename"),
        args: vec![String::from("")],
        stdin_data: String::new(),
        expected_out: String::from("\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}
