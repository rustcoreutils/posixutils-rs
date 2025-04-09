//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib_testing::{run_test, TestPlan};

#[test]
fn dirname_basic() {
    run_test(TestPlan {
        cmd: String::from("dirname"),
        args: vec![String::from("/usr/bin/env")],
        stdin_data: String::new(),
        expected_out: String::from("/usr/bin\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn dirname_root() {
    run_test(TestPlan {
        cmd: String::from("dirname"),
        args: vec![String::from("/")],
        stdin_data: String::new(),
        expected_out: String::from("/\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn dirname_no_directory_component() {
    run_test(TestPlan {
        cmd: String::from("dirname"),
        args: vec![String::from("filename")],
        stdin_data: String::new(),
        expected_out: String::from(".\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn dirname_trailing_slash() {
    run_test(TestPlan {
        cmd: String::from("dirname"),
        args: vec![String::from("/usr/bin/")],
        stdin_data: String::new(),
        expected_out: String::from("/usr\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn dirname_empty_path() {
    run_test(TestPlan {
        cmd: String::from("dirname"),
        args: vec![String::from("")],
        stdin_data: String::new(),
        expected_out: String::from(".\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn dirname_special_characters() {
    run_test(TestPlan {
        cmd: String::from("dirname"),
        args: vec![String::from("/path/to/some$special&chars*file")],
        stdin_data: String::new(),
        expected_out: String::from("/path/to\n"),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}
