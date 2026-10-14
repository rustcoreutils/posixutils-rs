//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `break`, `continue` and `return` in the conditions of `if`, `while` and
//! `until`: they take effect at once, and a loop's condition is part of the
//! loop, as in dash and bash.

use plib::testing::{run_test, TestPlan};

fn sh_c(script: &str, expected_out: &str) {
    run_test(TestPlan {
        cmd: "sh".to_string(),
        args: vec!["-c".to_string(), script.to_string()],
        stdin_data: String::new(),
        expected_out: expected_out.to_string(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// `return` in an `if` condition returned nothing: the condition's status
// chose a branch, and the function went on after the `if`.
#[test]
fn return_in_an_if_condition_returns() {
    sh_c(
        "f() { if return 3; then echo t; else echo e; fi; echo no; }; f; echo $?",
        "3\n",
    );
}

#[test]
fn return_in_a_loop_condition_returns() {
    sh_c(
        "f() { while return 4; do echo body; done; echo no; }; f; echo $?",
        "4\n",
    );
    sh_c(
        "f() { until return 0; do echo body; done; echo no; }; f; echo $?",
        "0\n",
    );
    sh_c(
        "f() { while :; do while return 6; do :; done; done; echo no; }; f; echo $?",
        "6\n",
    );
}

// The condition ran outside the loop, so `break` there left the loop around
// it, or was ignored at top level, and the body ran anyway.
#[test]
fn break_in_a_loop_condition_leaves_that_loop() {
    sh_c(
        "for i in 1 2; do while break; do echo b; done; echo in$i; done",
        "in1\nin2\n",
    );
    sh_c(
        "for i in 1 2; do until break 2; do :; done; echo no; done; echo $?",
        "0\n",
    );
    sh_c(
        "false; while true; do false; while break; do :; done; echo $?; break; done",
        "0\n",
    );
}

#[test]
fn continue_in_a_loop_condition_runs_the_condition_again() {
    sh_c(
        "i=0; while i=$((i+1)); [ $i -lt 3 ] && continue; [ $i -lt 5 ]; do echo b$i; done; echo $i",
        "b3\nb4\n5\n",
    );
}

#[test]
fn break_in_an_if_condition_leaves_the_loop() {
    sh_c(
        "for i in 1 2; do if break; then echo t; fi; echo in$i; done; echo end",
        "end\n",
    );
}
