//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};

fn expand_test_noargs(test_data: &str, expected_output: &str) {
    run_test(TestPlan {
        cmd: String::from("expand"),
        args: Vec::new(),
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn expand_basic() {
    expand_test_noargs("", "");
    expand_test_noargs("a\tb\tc\n", "a       b       c\n");
}

#[test]
fn expand_dash_operand_reads_stdin() {
    // A "-" operand reads standard input rather than a file named "-".
    run_test(TestPlan {
        cmd: String::from("expand"),
        args: vec![String::from("-")],
        stdin_data: String::from("a\tb\tc\n"),
        expected_out: String::from("a       b       c\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}
