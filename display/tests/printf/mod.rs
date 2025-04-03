//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test};

#[test]
fn test_basic_string_output() {
    run_test(TestPlan {
        cmd: String::from("printf"),
        args: vec![String::from("Hello, %s!"), String::from("World")],
        expected_out: String::from("Hello, World!"),
        expected_err: String::new(),
        stdin_data: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_integer_output() {
    run_test(TestPlan {
        cmd: String::from("printf"),
        args: vec![String::from("The answer is %d."), String::from("42")],
        expected_out: String::from("The answer is 42."),
        expected_err: String::new(),
        stdin_data: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_octal_output() {
    run_test(TestPlan {
        cmd: String::from("printf"),
        args: vec![String::from("%o"), String::from("8")],
        expected_out: String::from("10"),
        expected_err: String::new(),
        stdin_data: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_hex_output() {
    run_test(TestPlan {
        cmd: String::from("printf"),
        args: vec![String::from("%x"), String::from("255")],
        expected_out: String::from("ff"),
        expected_err: String::new(),
        stdin_data: String::new(),
        expected_exit_code: 0,
    });
}
