//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The diagnostics the shell writes, and the exit statuses that go with
//! them.

use plib::testing::{run_test, TestPlan};

fn sh_c(script: &str, expected_out: &str, expected_err: &str, expected_exit_code: i32) {
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec!["-c".to_string(), script.to_string()],
        stdin_data: String::new(),
        expected_out: expected_out.to_string(),
        expected_err: expected_err.to_string(),
        expected_exit_code,
    });
}

// A built-in utility's diagnostic is one line, with no blank line after it.
#[test]
fn builtin_diagnostic_is_one_line() {
    sh_c("cd ''; echo $?", "1\n", "cd: empty directory operand\n", 0);
    sh_c(
        "set -o nosuchopt; echo $?",
        "",
        "set: invalid option 'nosuchopt'\n",
        1,
    );
}
