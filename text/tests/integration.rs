//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test, TestPlan};

fn expand_test_noargs(test_data: &str, expected_output: &str) {
    run_test(TestPlan {
        cmd: String::from("expand"),
        args: Vec::new(),
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

fn head_test(test_data: &str, expected_output: &str) {
    run_test(TestPlan {
        cmd: String::from("head"),
        args: Vec::new(),
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

fn wc_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("wc"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

fn nl_test(args: &[&str], test_data: &str, expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("nl"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn test_expand_basic() {
    expand_test_noargs("", "");
    expand_test_noargs("a\tb\tc\n", "a       b       c\n");
}

#[test]
fn test_head_basic() {
    head_test("a\nb\nc\nd\n", "a\nb\nc\nd\n");
    head_test(
        "1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n",
        "1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n",
    );
    head_test(
        "1\n2\n3\n4\n5\n6\n7\n8\n9\n0\na\n",
        "1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n",
    );
}

#[test]
fn test_wc_empty() {
    wc_test(&["-c"], "", "0\n");
    wc_test(&["-l"], "", "0\n");
    wc_test(&["-w"], "", "0\n");
}

#[test]
fn test_wc_one() {
    wc_test(&["-c"], "x", "1\n");
    wc_test(&["-l"], "x", "0\n");
    wc_test(&["-w"], "x", "1\n");
}

#[test]
fn test_wc_two() {
    wc_test(&["-c"], "x y\n", "4\n");
    wc_test(&["-l"], "x y\n", "1\n");
    wc_test(&["-w"], "x y\n", "2\n");
}

#[test]
fn test_nl_justification() {
    nl_test(&["-n", "ln"], "a", "1     \ta\n");
    nl_test(&["-n", "rn"], "b", "     1\tb\n");
    nl_test(&["-n", "rz"], "c", "000001\tc\n");
}

#[test]
fn test_nl_newlines_at_end() {
    nl_test(&[], "a\n\n", "     1\ta\n       \n");
}

#[test]
fn test_nl_starting_number() {
    nl_test(&["-v", "2"], "a", "     2\ta\n");
}

#[test]
fn test_nl_number_increment() {
    let input = "\\:\\:\\:\nheader\n\\:\\:\nbody\n\\:\nfooter";
    // Without -p, the counter resets on delimiters
    nl_test(
        &["-h", "a", "-f", "a"],
        input,
        "\n     1\theader\n\n     1\tbody\n\n     1\tfooter\n",
    );

    // With -p, the counter increments even when encountering delimiters
    nl_test(
        &["-h", "a", "-f", "a", "-p"],
        input,
        "\n     1\theader\n\n     2\tbody\n\n     3\tfooter\n",
    );

    nl_test(
        &["-h", "a", "-f", "a", "-p", "-i", "2"],
        input,
        "\n     1\theader\n\n     3\tbody\n\n     5\tfooter\n",
    );
}

#[test]
fn test_nl_delimiter() {
    // Single character delimiter should be appended with the default second
    // character, ':'
    nl_test(
        &["-h", "a", "-f", "a", "-d", "?"],
        "?:?:?:\nheader\n?:?:\nbody\n?:\nfooter",
        "\n     1\theader\n\n     1\tbody\n\n     1\tfooter\n",
    );

    nl_test(
        &["-h", "a", "-f", "a", "-d", "?!"],
        "?!?!?!\nheader\n?!?!\nbody\n?!\nfooter",
        "\n     1\theader\n\n     1\tbody\n\n     1\tfooter\n",
    );
}

#[test]
fn test_nl_regex() {
    // NOTE: The implementation has better regex support than the reference.
    // `nl -b p.+ng` would fail to match the words ending with "ng" in the
    // original whereas it would in this Rust implementation. Might be
    // considered a bug?
    nl_test(
        &["-b", "p.*ng"],
        "something\nanything\neverything\ncat\ndog",
        "     1\tsomething\n     2\tanything\n     3\teverything\n       cat\n       dog\n",
    );
}
