//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib_testing::{run_test, TestPlan};

fn xargs_test(test_data: &str, expected_output: &str, args: Vec<&str>) {
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: args.into_iter().map(String::from).collect(),
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn xargs_basic() {
    xargs_test("one two three\n", "one two three\n", vec!["echo"]);
}

#[test]
fn xargs_with_maxnum() {
    xargs_test(
        "one two three\n",
        "one\ntwo\nthree\n",
        vec!["-n", "1", "echo"],
    );
}

#[test]
fn xargs_with_maxsize() {
    xargs_test(
        "one two three four five\n",
        "one\ntwo\nthree\nfour\nfive\n",
        vec!["-s", "11", "echo"],
    );
}

#[test]
fn xargs_with_eofstr() {
    xargs_test(
        "one two three STOP four five\n",
        "one two three\n",
        vec!["-E", "STOP", "echo"],
    );
}

#[test]
fn xargs_with_null_delimiter() {
    xargs_test("one\0two\0three\0", "one two three\n", vec!["-0", "echo"]);
}

#[test]
fn xargs_with_null_delimiter_trailing_non_null() {
    xargs_test("one\0two\0three", "one two three\n", vec!["-0", "echo"]);
}

#[test]
fn xargs_trace() {
    run_test(TestPlan {
        cmd: String::from("xargs"),
        args: vec!["-t".to_string(), "echo".to_string()],
        stdin_data: String::from("one two three\n"),
        expected_err: String::from("echo one two three\n"),
        expected_out: String::from("one two three\n"),
        expected_exit_code: 0,
    });
}
