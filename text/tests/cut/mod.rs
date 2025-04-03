//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib_testing::{run_test, TestPlan};

fn cut_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("cut"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_cut_0() {
    cut_test(&["-c", "1-3", "-"], "abcdef", "abc\n");
}

#[test]
fn test_cut_1() {
    cut_test(
        &["-d", ":", "-f", "1,3", "-"],
        "field1:field2:field3",
        "field1:field3\n",
    );
}

#[test]
fn test_cut_2() {
    cut_test(&["-d", ":", "-f", "1,3-", "-"], "a:b:c\n", "a:c\n");
}

#[test]
fn test_cut_3() {
    cut_test(&["-d", ":", "-f", "2-", "-"], "a:b:c\n", "b:c\n");
}

#[test]
fn test_cut_4() {
    cut_test(&["-d", ":", "-f", "4", "-"], "a:b:c\n", "\n");
}

#[test]
fn test_cut_5() {
    cut_test(&["-d", ":", "-f", "4", "-"], "", "");
}

#[test]
fn test_cut_6() {
    cut_test(&["-c", "4", "-"], "123\n", "\n");
}

#[test]
fn test_cut_7() {
    cut_test(&["-c", "4", "-"], "123", "\n");
}

#[test]
fn test_cut_8() {
    cut_test(&["-c", "4", "-"], "123\n1", "\n\n");
}

#[test]
fn test_cut_9() {
    cut_test(&["-c", "4", "-"], "", "");
}

#[test]
fn test_cut_a() {
    cut_test(&["-s", "-d", ":", "-f", "3-", "-"], "a:b:c\n", "c\n");
}

#[test]
fn test_cut_b() {
    cut_test(&["-s", "-d", ":", "-f", "2,3", "-"], "a:b:c\n", "b:c\n");
}

#[test]
fn test_cut_c() {
    cut_test(&["-s", "-d", ":", "-f", "1,3", "-"], "a:b:c\n", "a:c\n");
}

#[test]
fn test_cut_e() {
    cut_test(&["-s", "-d", ":", "-f", "3-", "-"], "a:b:c:\n", "c:\n");
}

#[test]
fn test_cut_f() {
    cut_test(&["-s", "-d", ":", "-f", "3-4", "-"], "a:b:c:\n", "c:\n");
}

#[test]
fn test_cut_h() {
    cut_test(&["-s", "-d", ":", "-f", "2,3", "-"], "abc\n", "");
}

#[test]
fn test_cut_i() {
    cut_test(&["-d", ":", "-f", "1-3", "-"], ":::\n", "::\n");
}

#[test]
fn test_cut_j() {
    cut_test(&["-d", ":", "-f", "1-4", "-"], ":::\n", ":::\n");
}

#[test]
fn test_cut_k() {
    cut_test(&["-d", ":", "-f", "2-3", "-"], ":::\n", ":\n");
}

#[test]
fn test_cut_l() {
    cut_test(&["-d", ":", "-f", "2-4", "-"], ":::\n", "::\n");
}

#[test]
fn test_cut_m() {
    cut_test(&["-s", "-d", ":", "-f", "1-3", "-"], ":::\n", "::\n");
}

#[test]
fn test_cut_n() {
    cut_test(&["-s", "-d", ":", "-f", "1-4", "-"], ":::\n", ":::\n");
}

#[test]
fn test_cut_o() {
    cut_test(&["-s", "-d", ":", "-f", "2-3", "-"], ":::\n", ":\n");
}

#[test]
fn test_cut_p() {
    cut_test(&["-s", "-d", ":", "-f", "2-4", "-"], ":::\n", "::\n");
}

#[test]
fn test_cut_q() {
    cut_test(&["-s", "-d", ":", "-f", "2-4", "-"], ":::\n:\n", "::\n\n");
}

#[test]
fn test_cut_r() {
    cut_test(&["-s", "-d", ":", "-f", "2-4", "-"], ":::\n:1\n", "::\n1\n");
}

#[test]
fn test_cut_s() {
    cut_test(
        &["-s", "-d", ":", "-f", "1-4", "-"],
        ":::\n:a\n",
        ":::\n:a\n",
    );
}

#[test]
fn test_cut_t() {
    cut_test(&["-s", "-d", ":", "-f", "3-", "-"], ":::\n:1\n", ":\n\n");
}

#[test]
fn test_cut_u() {
    cut_test(&["-s", "-f", "3-", "-"], "", "");
}

#[test]
fn test_cut_v() {
    cut_test(&["-f", "3-", "-"], "", "");
}

#[test]
fn test_cut_w() {
    cut_test(&["-b", "1", "-"], "", "");
}

#[test]
fn test_cut_x() {
    cut_test(&["-s", "-d", ":", "-f", "2-4", "-"], ":\n", "\n");
}

#[test]
fn test_cut_newline_1() {
    cut_test(&["-f", "1-", "-"], "a\nb", "a\nb\n");
}

#[test]
fn test_cut_newline_2() {
    cut_test(&["-f", "1-", "-"], "", "");
}

#[test]
fn test_cut_newline_3() {
    cut_test(&["-d", ":", "-f", "1", "-"], "a:1\nb:2\n", "a\nb\n");
}

#[test]
fn test_cut_newline_4() {
    cut_test(&["-d", ":", "-f", "1", "-"], "a:1\nb:2", "a\nb\n");
}

#[test]
fn test_cut_newline_5() {
    cut_test(&["-d", ":", "-f", "2", "-"], "a:1\nb:2\n", "1\n2\n");
}

#[test]
fn test_cut_newline_6() {
    cut_test(&["-d", ":", "-f", "2", "-"], "a:1\nb:2", "1\n2\n");
}

#[test]
fn test_cut_newline_7() {
    cut_test(&["-s", "-d", ":", "-f", "1", "-"], "a:1\nb:2", "a\nb\n");
}

#[test]
fn test_cut_newline_8() {
    cut_test(&["-s", "-d", ":", "-f", "1", "-"], "a:1\nb:2\n", "a\nb\n");
}

#[test]
fn test_cut_newline_9() {
    cut_test(&["-s", "-d", ":", "-f", "1", "-"], "a1\nb2", "");
}

#[test]
fn test_cut_newline_10() {
    cut_test(
        &["-s", "-d", ":", "-f", "1,2", "-"],
        "a:1\nb:2",
        "a:1\nb:2\n",
    );
}

#[test]
fn test_cut_newline_11() {
    cut_test(
        &["-s", "-d", ":", "-f", "1,2", "-"],
        "a:1\nb:2\n",
        "a:1\nb:2\n",
    );
}

#[test]
fn test_cut_newline_12() {
    cut_test(&["-s", "-d", ":", "-f", "1", "-"], "a:1\nb:", "a\nb\n");
}

#[test]
fn test_cut_newline_13() {
    cut_test(&["-d", ":", "-f", "1-", "-"], "a1:\n:", "a1:\n:\n");
}

#[test]
fn test_cut_newline_14() {
    cut_test(&["-d", "\n", "-f", "1-", "-"], "\nb", "\nb\n");
}

#[test]
fn test_out_delim_1() {
    cut_test(&["-d", ":", "-c", "1-3,5-", "-"], "abcdefg\n", "abc:efg\n");
}

#[test]
fn test_out_delim_2() {
    cut_test(
        &["-d", ":", "-c", "1-3,2,5-", "-"],
        "abcdefg\n",
        "abc:efg\n",
    );
}

#[test]
fn test_out_delim_3() {
    cut_test(
        &["-d", ":", "-c", "1-3,2-4,6", "-"],
        "abcdefg\n",
        "abcd:f\n",
    );
}

#[test]
fn test_out_delim_3a() {
    cut_test(
        &["-d", ":", "-c", "1-3,2-4,6-", "-"],
        "abcdefg\n",
        "abcd:fg\n",
    );
}

#[test]
fn test_out_delim_4() {
    cut_test(&["-d", ":", "-c", "4-,2-3", "-"], "abcdefg\n", "bc:defg\n");
}

#[test]
fn test_out_delim_5() {
    cut_test(&["-d", ":", "-c", "2-3,4-", "-"], "abcdefg\n", "bc:defg\n");
}

#[test]
fn test_out_delim_6() {
    cut_test(&["-d", ":", "-c", "2,1-3", "-"], "abc\n", "abc\n");
}

#[test]
fn test_od_abut() {
    cut_test(&["-d", ":", "-b", "1-2,3-4", "-"], "abcd\n", "ab:cd\n");
}

#[test]
fn test_od_overlap() {
    cut_test(&["-d", ":", "-b", "1-2,2", "-"], "abc\n", "ab\n");
}

#[test]
fn test_od_overlap2() {
    cut_test(&["-d", ":", "-b", "1-2,2-", "-"], "abc\n", "abc\n");
}

#[test]
fn test_od_overlap3() {
    cut_test(&["-d", ":", "-b", "1-3,2-", "-"], "abcd\n", "abcd\n");
}

#[test]
fn test_od_overlap4() {
    cut_test(&["-d", ":", "-b", "1-3,2-3", "-"], "abcd\n", "abc\n");
}

#[test]
fn test_od_overlap5() {
    cut_test(&["-d", ":", "-b", "1-3,1-4", "-"], "abcde\n", "abcd\n");
}

#[test]
fn test_stdin_no_hyphen() {
    cut_test(
        &["-c", "1-12,41-"],
        "081ca869c86b0b885be83db4335de7522b930913 41 truefalse/src/false.rs\n",
        "081ca869c86b 41 truefalse/src/false.rs\n",
    );
}
