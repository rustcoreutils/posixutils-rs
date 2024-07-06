//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test, TestPlan};

fn tail_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("tail"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_tail() {
    tail_test(&["-n2"], "a\nb\nc\n", "b\nc\n");
}

#[test]
fn test_tail_1() {
    tail_test(&["-c+2"], "abcd", "bcd");
}

#[test]
fn test_tail_2() {
    tail_test(&["-c+8"], "abcd", "");
}

#[test]
fn test_tail_3() {
    tail_test(&["-c-1"], "abcd", "d");
}

#[test]
fn test_tail_4() {
    tail_test(&["-c-9"], "abcd", "abcd");
}

#[test]
fn test_tail_5() {
    tail_test(
        &["-c-12"],
        &("x".to_string() + &"y".repeat(12) + "z"),
        &("y".repeat(11) + "z"),
    );
}

#[test]
fn test_tail_6() {
    tail_test(&["-n-1"], "x\n", "x\n");
}

#[test]
fn test_tail_7() {
    tail_test(&["-n-1"], "x\ny\n", "y\n");
}

#[test]
fn test_tail_8() {
    tail_test(&["-n-1"], "x\ny\n", "y\n");
}

#[test]
fn test_tail_9() {
    tail_test(&["-n+1"], "x\ny\n", "x\ny\n");
}

#[test]
fn test_tail_10() {
    tail_test(&["-n+2"], "x\ny\n", "y\n");
}

#[test]
fn test_tail_11() {
    tail_test(
        &["-c+10"],
        &("x".to_string() + &"y".repeat(10) + "z\n"),
        "yyz\n",
    );
}

#[test]
fn test_tail_12() {
    tail_test(
        &["-n+10"],
        &("x\n".to_string() + &"y\n".repeat(10) + "z\n"),
        "y\ny\nz\n",
    );
}

#[test]
fn test_tail_13() {
    tail_test(
        &["-n-10"],
        &("x\n".to_string() + &"y\n".repeat(10) + "z\n"),
        &("y\n".repeat(9) + "z\n"),
    );
}

#[test]
fn test_tail_14() {
    let input = &("x\n".repeat(512 * 10 / 2 + 1));
    let expected_output = &("x\n".repeat(10));
    tail_test(&["-n-10"], input, expected_output);
}

#[test]
fn test_tail_15() {
    tail_test(&["-c2"], "abcd\n", "d\n");
}

#[test]
fn test_tail_16() {
    tail_test(
        &["-n-10"],
        &("x\n".to_string() + &"y\n".repeat(10) + "z\n"),
        &("y\n".repeat(9) + "z\n"),
    );
}

#[test]
fn test_tail_17() {
    tail_test(
        &["-n+10"],
        &("x\n".to_string() + &"y\n".repeat(10) + "z\n"),
        "y\ny\nz\n",
    );
}

#[test]
fn test_tail_18() {
    tail_test(&["-n+0"], &("y\n".repeat(5)), &("y\n".repeat(5)));
}

#[test]
fn test_tail_19() {
    tail_test(&["-n+1"], &("y\n".repeat(5)), &("y\n".repeat(5)));
}

#[test]
fn test_tail_20() {
    tail_test(&["-n-1"], &("y\n".repeat(5)), "y\n");
}
