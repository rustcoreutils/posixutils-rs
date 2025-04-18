//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_u8, TestPlan, TestPlanU8};

fn tail_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args = args.iter().map(|st| (*st).to_owned()).collect::<Vec<_>>();

    run_test(TestPlan {
        cmd: "tail".to_owned(),
        args: str_args,
        stdin_data: test_data.to_owned(),
        expected_out: expected_output.to_owned(),
        expected_err: String::new(),
        expected_exit_code: 0_i32,
    });
}

fn tail_test_failure(args: &[&str], expected_stderr: &str) {
    let str_args = args.iter().map(|st| (*st).to_owned()).collect::<Vec<_>>();

    run_test(TestPlan {
        cmd: "tail".to_owned(),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: expected_stderr.to_owned(),
        expected_exit_code: 1_i32,
    });
}

fn tail_test_binary(args: &[&str], test_data: &[u8], expected_output: &[u8]) {
    let str_args = args.iter().map(|st| (*st).to_owned()).collect::<Vec<_>>();

    run_test_u8(TestPlanU8 {
        cmd: "tail".to_owned(),
        args: str_args,
        stdin_data: test_data.to_vec(),
        expected_out: expected_output.to_vec(),
        expected_err: Vec::<u8>::new(),
        expected_exit_code: 0_i32,
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

#[test]
fn test_tail_input_containing_non_utf_8() {
    const INPUT: &[u8] = b"\
\xFF
\xFF
0
1
2
3
4
5
6
7
8
9
";

    const EXPECTED_OUTPUT: &[u8] = b"\
0
1
2
3
4
5
6
7
8
9
";

    tail_test_binary(&[], INPUT, EXPECTED_OUTPUT);
}

#[test]
fn test_tail_zero_bytes_and_zero_lines() {
    const INPUT: &str = "\
0
1
2
3
4
5
6
7
8
9
";

    tail_test(&["-c", "0"], INPUT, "");
    tail_test(&["-c", "-0"], INPUT, "");

    tail_test(&["-n", "0"], INPUT, "");
    tail_test(&["-n", "-0"], INPUT, "");
}

#[test]
fn test_tail_c_and_n() {
    tail_test_failure(
        &["-c", "1", "-n", "2"],
        "tail: options '-c' and '-n' cannot be used together\n",
    );
    tail_test_failure(
        &["-n", "3", "-c", "4"],
        "tail: options '-c' and '-n' cannot be used together\n",
    );
}
