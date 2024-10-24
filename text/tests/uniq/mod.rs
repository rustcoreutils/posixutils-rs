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

fn uniq_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("uniq"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_uniq_2() {
    uniq_test(&[], "a\na\n", "a\n");
}

#[test]
fn uniq_3() {
    uniq_test(&[], "a\na", "a\n");
}

#[test]
fn uniq_4() {
    uniq_test(&[], "a\nb", "a\nb\n");
}

#[test]
fn uniq_5() {
    uniq_test(&[], "a\na\nb", "a\nb\n");
}

#[test]
fn uniq_6() {
    uniq_test(&[], "b\na\na\n", "b\na\n");
}

#[test]
fn uniq_7() {
    uniq_test(&[], "a\nb\nc\n", "a\nb\nc\n");
}

#[test]
fn uniq_8() {
    uniq_test(&[], "ö\nv\n", "ö\nv\n");
}

#[test]
fn uniq_9() {
    uniq_test(&["-u"], "a\na\n", "");
}

#[test]
fn uniq_10() {
    uniq_test(&["-u"], "a\nb\n", "a\nb\n");
}

#[test]
fn uniq_11() {
    uniq_test(&["-u"], "a\nb\na\n", "a\nb\na\n");
}

#[test]
fn uniq_12() {
    uniq_test(&["-u"], "a\na\n", "");
}

#[test]
fn uniq_13() {
    uniq_test(&["-u"], "a\na\n", "");
}

#[test]
fn uniq_20() {
    uniq_test(&["-d"], "a\na\n", "a\n");
}

#[test]
fn uniq_21() {
    uniq_test(&["-d"], "a\nb\n", "");
}

#[test]
fn uniq_22() {
    uniq_test(&["-d"], "a\nb\na\n", "");
}

#[test]
fn uniq_23() {
    uniq_test(&["-d"], "a\na\nb\n", "a\n");
}

#[test]
fn uniq_24() {
    uniq_test(&["-f", "1"], "a a\nb a\n", "a a\n");
}

#[test]
fn uniq_25() {
    uniq_test(&["-f", "1"], "a a\nb b\n", "a a\nb b\n");
}

#[test]
fn uniq_26() {
    uniq_test(&["-f", "1"], "a a a\nb a c\n", "a a a\nb a c\n");
}

#[test]
fn uniq_27() {
    uniq_test(&["-f", "1"], "b a\na a\n", "b a\n");
}

#[test]
fn uniq_28() {
    uniq_test(&["-f", "2"], "a a c\nb a c\n", "a a c\n");
}

#[test]
fn uniq_29() {
    uniq_test(&["-s", "1"], "aaa\naaa\n", "aaa\n");
}

#[test]
fn uniq_30() {
    uniq_test(&["-s", "2"], "baa\naaa\n", "baa\n");
}

#[test]
fn uniq_31() {
    uniq_test(&["-f", "1", "-s", "1"], "a aaa\nb ab\n", "a aaa\nb ab\n");
}

#[test]
fn uniq_32() {
    uniq_test(&["-f", "1", "-s", "1"], "a aaa\nb aaa\n", "a aaa\n");
}

#[test]
fn uniq_33() {
    uniq_test(&["-f", "1", "-s", "1"], "a aaa\nb ab\n", "a aaa\nb ab\n");
}

#[test]
fn uniq_34() {
    uniq_test(&["-f", "1", "-s", "1"], "a aaa\nb aaa\n", "a aaa\n");
}

#[test]
fn uniq_35() {
    uniq_test(&["-s", "0"], "abc\nabcd\n", "abc\nabcd\n");
}

#[test]
fn uniq_36() {
    uniq_test(&["-s", "0"], "abc\n", "abc\n");
}

#[test]
fn uniq_37() {
    uniq_test(&[], "a\0a\na\n", "a\0a\na\n");
}

#[test]
fn uniq_38() {
    uniq_test(&[], "a\ta\na a\n", "a\ta\na a\n");
}

#[test]
fn uniq_39() {
    uniq_test(&["-f", "1"], "a\ta\na a\n", "a\ta\na a\n");
}

#[test]
fn uniq_40() {
    uniq_test(&["-f", "2"], "a\ta a\na a a\n", "a\ta a\n");
}

#[test]
fn uniq_41() {
    uniq_test(&["-f", "1"], "a\ta\na\ta\n", "a\ta\n");
}

#[test]
fn uniq_42() {
    uniq_test(&["-c"], "a\nb\n", "1 a\n1 b\n");
}

#[test]
fn uniq_43() {
    uniq_test(&["-c"], "a\na\n", "2 a\n");
}
