//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};

fn truefalse_test(cmd: &str, expected_exit_code: i32) {
    run_test(TestPlan {
        cmd: cmd.to_string(),
        args: Vec::new(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    });
}

#[test]
fn false_exit_code() {
    truefalse_test("false", 1);
}
