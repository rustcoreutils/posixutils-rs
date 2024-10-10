//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::process::Output;

use plib::{run_test_with_checker, TestPlan};

fn test_checker_man(plan: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains(&plan.expected_out));

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains(&plan.expected_err));

    assert_eq!(output.status.code(), Some(plan.expected_exit_code));
    if plan.expected_exit_code == 0 {
        assert!(output.status.success());
    }
}

fn run_test_man(args: &[&str], expected_out: &str, expected_err: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("man"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::from(expected_out),
            expected_err: String::from(expected_err),
            expected_exit_code,
        },
        test_checker_man,
    );
}

const LS: &'static str = "ls";
const MAN: &'static str = "man";
const INVALID_NAME: &'static str = "invalid_name";
const INVALID_NAME_MAN_ERROR: &'static str =
    "man: system documentation for \"invalid_name\" not found\n";
const INVALID_NAME_APROPOS_ERROR: &'static str = "invalid_name: nothing appropriate";

#[test]
fn test_one_manpage() {
    run_test_man(&[LS], "LS(1)", "", 0);
}

#[test]
fn test_one_page_not_found() {
    run_test_man(&[INVALID_NAME], "", INVALID_NAME_MAN_ERROR, 1);
}

#[test]
fn test_multiple_nampages() {
    run_test_man(&[LS, MAN], "LS(1)", "", 0);
    run_test_man(&[LS, MAN], "MAN(1)", "", 0);
}

#[test]
fn test_multiple_nampages_one_not_found() {
    run_test_man(&[LS, INVALID_NAME], "LS(1)", INVALID_NAME_MAN_ERROR, 1);
}

#[test]
fn test_empty_names() {
    run_test_man(&[], "", "man: no names specified\n", 1);
}

#[test]
fn test_k() {
    run_test_man(&["-k", "user"], "ls", "", 0);
}

#[test]
fn test_k_invalid_name() {
    run_test_man(&["-k", "invalid_name"], "", &INVALID_NAME_APROPOS_ERROR, 1);
}

#[test]
fn test_k_multiple_nampages() {
    run_test_man(&["-k", LS, MAN], "ls", "", 0);
    run_test_man(&["-k", LS, MAN], "man", "", 0);
}

#[test]
fn test_k_multiple_nampages_one_not_found() {
    run_test_man(
        &["-k", LS, INVALID_NAME],
        "ls",
        INVALID_NAME_APROPOS_ERROR,
        1,
    );
}

#[test]
fn test_k_empty_names() {
    run_test_man(&["-k"], "", "man: no names specified\n", 1);
}
