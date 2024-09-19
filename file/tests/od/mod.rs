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
    od_test(
        &["-tc", "-An"],
        "\x07",
        r#"  \a
"#,
    );
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
    od_test(&["-tc", "-j3", "-An"], "abcd", "   d\n");
}

#[test]
fn test_od_4() {
    od_test(
        &[],
        "Hello, World!",
        "\
0000000 062510 066154 026157 053440 071157 062154 000041
0000015
",
    );
}

#[test]
fn test_od_5() {
    od_test(
        &["-b"],
        "Hello, World!",
        "\
0000000 110 145 154 154 157 054 040 127 157 162 154 144 041
0000015
",
    );
}

#[test]
fn test_od_6() {
    od_test(
        &["-c"],
        "Hello, World!",
        "\
0000000   H   e   l   l   o   ,       W   o   r   l   d   !
0000015
",
    );
}

#[test]
fn test_od_7() {
    od_test(
        &["-d"],
        "Hello, World!",
        "\
0000000 25928 27756 11375 22304 29295 25708    33
0000015
",
    );
}

#[test]
fn test_od_8() {
    od_test(
        &["-Ad"],
        "Hello, World!",
        "\
0000000 062510 066154 026157 053440 071157 062154 000041
0000013
",
    );
}

#[test]
fn test_od_9() {
    od_test(
        &["-An"],
        "Hello, World!",
        " 062510 066154 026157 053440 071157 062154 000041\n",
    );
}

#[test]
fn test_od_10() {
    od_test(
        &["-Ao"],
        "Hello, World!",
        "\
0000000 062510 066154 026157 053440 071157 062154 000041
0000015
",
    );
}

#[test]
fn test_od_11() {
    od_test(
        &["-Ax"],
        "Hello, World!",
        "\
000000 062510 066154 026157 053440 071157 062154 000041
00000d
",
    );
}

#[test]
fn test_od_12() {
    od_test(
        &["-j7"],
        "Hello, World!",
        "\
0000007 067527 066162 020544
0000015
",
    );
}

#[test]
fn test_od_13() {
    od_test(
        &["-N5"],
        "Hello, World!",
        "\
0000000 062510 066154 000157
0000005
",
    );
}

#[test]
fn test_od_14() {
    od_test(
        &["-o"],
        "Hello, World!",
        "\
0000000 062510 066154 026157 053440 071157 062154 000041
0000015
",
    );
}

#[test]
fn test_od_15() {
    od_test(
        &["-s"],
        "Hello, World!",
        "\
0000000  25928  27756  11375  22304  29295  25708     33
0000015
",
    );
}

// TODO
// Does not match other implementations
#[test]
fn test_od_16() {
    od_test(
        &["-tf4"],
        "Hello, World!",
        "\
0000000 1.1431391224375825e27 1.7611270127616e14 1.7446709643352771e22 0e0
0000015
",
    );
}

#[test]
fn test_od_17() {
    od_test(
        &["-td2"],
        "Hello, World!",
        "\
0000000  25928  27756  11375  22304  29295  25708     33
0000015
",
    );
}

#[test]
fn test_od_18() {
    od_test(
        &["-v"],
        "Hello, World!",
        "\
0000000 062510 066154 026157 053440 071157 062154 000041
0000015
",
    );
}

#[test]
fn test_od_19() {
    od_test(
        &["-x"],
        "Hello, World!",
        "\
0000000 6548 6c6c 2c6f 5720 726f 646c 0021
0000015
",
    );
}

#[test]
fn test_od_20() {
    od_test(
        &["-Ax", "-td4", "-v"],
        "Hello, World!",
        "\
000000  1819043144  1461726319  1684828783          33
00000d
",
    );
}

#[test]
fn test_od_21() {
    od_test(
        &["-j0x10"],
        "Hello, World!123456",
        "\
0000020 032464 000066
0000023
",
    );
}

#[test]
fn test_od_22() {
    od_test(
        &["-tu4"],
        "Hello, World!1",
        "\
0000000 1819043144 1461726319 1684828783      12577
0000016
",
    );
}

// POSIX does not specify that '-A n' should result in a leading space being printed, or that it should not.
// Given that most other implementations of `od` print one or more leading spaces, for compatibility, one
// leading space will be printed.
//
// Busybox, GNU Core Utilities, uutils's coreutils: 1 leading space
// Toybox: 2 leading spaces
//
// Also, none of these implementations print trailing spaces
#[test]
fn test_od_a_n_t_x_one() {
    od_test(
        &["-A", "n", "-t", "x1"],
        "This is a suite of Rust-native core command line utilities",
        " 54 68 69 73 20 69 73 20 61 20 73 75 69 74 65 20
 6f 66 20 52 75 73 74 2d 6e 61 74 69 76 65 20 63
 6f 72 65 20 63 6f 6d 6d 61 6e 64 20 6c 69 6e 65
 20 75 74 69 6c 69 74 69 65 73
",
    );
}

#[test]
fn test_od_a_x_t_a() {
    od_test(
        &["-A", "x", "-t", "a"],
        "Hello, World!",
        "\
000000   H   e   l   l   o   ,  sp   W   o   r   l   d   !
00000d
",
    );
}
