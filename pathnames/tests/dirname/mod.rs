//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};

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

fn dirname_case(arg: &str, out: &str) {
    run_test(TestPlan {
        cmd: String::from("dirname"),
        args: vec![String::from(arg)],
        stdin_data: String::new(),
        expected_out: String::from(out),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// A non-final ".." component must be preserved (no pathname resolution).
#[test]
fn dirname_preserves_nonfinal_dotdot() {
    dirname_case("a/../b", "a/..\n");
}

// Redundant <slash> elision is permitted; the directory portion is unchanged.
#[test]
fn dirname_redundant_slashes() {
    dirname_case("/usr//bin", "/usr\n");
}

// FUTURE DIRECTIONS: an embedded <newline> in the output is treated as an error.
#[test]
fn dirname_newline_is_error() {
    run_test(TestPlan {
        cmd: String::from("dirname"),
        args: vec![String::from("foo\nbar/baz")],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::from("dirname: result contains a newline character\n"),
        expected_exit_code: 1,
    });
}
