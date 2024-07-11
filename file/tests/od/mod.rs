//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test, TestPlan};

fn od_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("od"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_od_0() {
    od_test(&["-tc", "-An"], "\x07", "\\a \n");
}

#[test]
fn test_od_1() {
    od_test(&["-c", "-j1", "-An"], "a", "");
}

#[test]
fn test_od_2() {
    od_test(&["-c", "-j3", "-An"], "abc", "");
}

#[test]
fn test_od_3() {
    od_test(&["-tc", "-j3", "-An"], "abcd", "d \n");
}

#[test]
fn test_od_4() {
    od_test(
        &[],
        "Hello, World!",
        "0000000 62510 66154 26157 53440 71157 62154 041 \n0000015 ",
    );
}

#[test]
fn test_od_5() {
    od_test(
        &["-b"],
        "Hello, World!",
        "0000000 110 145 154 154 157 054 040 127 157 162 154 144 041 \n0000015 ",
    );
}

#[test]
fn test_od_6() {
    od_test(
        &["-c"],
        "Hello, World!",
        "0000000 H e l l o ,   W o r l d ! \n0000015 ",
    );
}

#[test]
fn test_od_7() {
    od_test(
        &["-d"],
        "Hello, World!",
        "0000000 25928 27756 11375 22304 29295 25708 33 \n0000015 ",
    );
}

#[test]
fn test_od_8() {
    od_test(
        &["-Ad"],
        "Hello, World!",
        "0000000 62510 66154 26157 53440 71157 62154 041 \n0000013 ",
    );
}

#[test]
fn test_od_9() {
    od_test(
        &["-An"],
        "Hello, World!",
        "62510 66154 26157 53440 71157 62154 041 \n",
    );
}

#[test]
fn test_od_10() {
    od_test(
        &["-Ao"],
        "Hello, World!",
        "0000000 62510 66154 26157 53440 71157 62154 041 \n0000015 ",
    );
}

#[test]
fn test_od_11() {
    od_test(
        &["-Ax"],
        "Hello, World!",
        "0000000 62510 66154 26157 53440 71157 62154 041 \n000000d ",
    );
}

#[test]
fn test_od_12() {
    od_test(
        &["-j7"],
        "Hello, World!",
        "0000000 67527 66162 20544 \n0000006 ",
    );
}

#[test]
fn test_od_13() {
    od_test(
        &["-N5"],
        "Hello, World!",
        "0000000 62510 66154 157 \n0000005 ",
    );
}

#[test]
fn test_od_14() {
    od_test(
        &["-o"],
        "Hello, World!",
        "0000000 62510 66154 26157 53440 71157 62154 041 \n0000015 ",
    );
}

#[test]
fn test_od_15() {
    od_test(
        &["-s"],
        "Hello, World!",
        "0000000 25928 27756 11375 22304 29295 25708 33 \n0000015 ",
    );
}

#[test]
fn test_od_16() {
    od_test(
        &["-tf4"],
        "Hello, World!",
        "0000000 1.1431391224375825e27 1.7611270127616e14 1.7446709643352771e22 0e0 \n0000015 ",
    );
}

#[test]
fn test_od_17() {
    od_test(
        &["-td2"],
        "Hello, World!",
        "0000000 25928 27756 11375 22304 29295 25708 33 \n0000015 ",
    );
}

#[test]
fn test_od_18() {
    od_test(
        &["-v"],
        "Hello, World!",
        "0000000 62510 66154 26157 53440 71157 62154 041 \n0000015 ",
    );
}

#[test]
fn test_od_19() {
    od_test(
        &["-x"],
        "Hello, World!",
        "0000000 6548 6c6c 2c6f 5720 726f 646c 0021 \n0000015 ",
    );
}

#[test]
fn test_od_20() {
    od_test(
        &["-Ax", "-td4", "-v"],
        "Hello, World!",
        "0000000 1819043144 1461726319 1684828783 33 \n000000d ",
    );
}

#[test]
fn test_od_21() {
    od_test(
        &["-j0x10"],
        "Hello, World!123456",
        "0000000 32464 066 \n0000003 ",
    );
}

#[test]
fn test_od_22() {
    od_test(
        &["-tu4"],
        "Hello, World!1",
        "0000000 1819043144 1461726319 1684828783 12577 \n0000016 ",
    );
}
