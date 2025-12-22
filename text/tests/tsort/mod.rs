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

fn tsort_test(
    args: &[&str],
    test_data: &str,
    expected_output: &str,
    expected_exit_code: i32,
    expected_err: &str,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("tsort"),
        args: str_args,
        stdin_data: String::from(test_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_err),
        expected_exit_code,
    });
}

#[test]
fn test_basic() {
    tsort_test(&[], "a b\nc d\nb c\n", "a\nb\nc\nd\n", 0, "");
}

#[test]
fn test_simple_chain() {
    tsort_test(&[], "a b\nb c\nc d\n", "a\nb\nc\nd\n", 0, "");
}

#[test]
fn test_multiple_dependencies() {
    tsort_test(&[], "a b\na c\nb d\nc d\n", "a\nb\nc\nd\n", 0, "");
}

#[test]
fn test_self_loop() {
    tsort_test(&[], "a a\n", "a\n", 0, "");
}

#[test]
fn test_empty_input() {
    tsort_test(&[], "", "", 0, "");
}

#[test]
fn test_single_pair() {
    tsort_test(&[], "a b\n", "a\nb\n", 0, "");
}

#[test]
fn test_whitespace_separated_chain() {
    // Single line with chain dependencies: a->b->c->d
    tsort_test(&[], "a b b c c d\n", "a\nb\nc\nd\n", 0, "");
}

#[test]
fn test_multiline_tokens() {
    // Chain dependencies across lines
    tsort_test(&[], "a b\nb c\n", "a\nb\nc\n", 0, "");
}

#[test]
fn test_odd_number_of_tokens() {
    tsort_test(
        &[],
        "a b c\n",
        "",
        1,
        "stdin: input contains an odd number of tokens\n",
    );
}

#[test]
fn test_simple_cycle() {
    tsort_test(
        &[],
        "a b\nb a\n",
        "a\nb\n",
        1,
        "stdin: input contains a loop:\nstdin: a\nstdin: b\n",
    );
}

#[test]
fn test_three_way_cycle() {
    tsort_test(
        &[],
        "a b\nb c\nc a\n",
        "a\nb\nc\n",
        1,
        "stdin: input contains a loop:\nstdin: a\nstdin: b\nstdin: c\n",
    );
}

#[test]
fn test_partial_cycle() {
    // d->e has no cycle, a->b->c->a forms a cycle
    tsort_test(
        &[],
        "a b\nb c\nc a\nd e\n",
        "d\ne\na\nb\nc\n",
        1,
        "stdin: input contains a loop:\nstdin: a\nstdin: b\nstdin: c\n",
    );
}

#[test]
fn test_complex_graph_chain() {
    // Clear chain: d->c->b->a
    tsort_test(&[], "d c\nc b\nb a\n", "d\nc\nb\na\n", 0, "");
}

#[test]
fn test_two_independent_items() {
    // Single pair
    tsort_test(&[], "a b\n", "a\nb\n", 0, "");
}

#[test]
fn test_duplicate_pairs() {
    // Same dependency specified multiple times
    tsort_test(&[], "a b\na b\nb c\n", "a\nb\nc\n", 0, "");
}

#[test]
fn test_long_string_tokens() {
    tsort_test(
        &[],
        "very_long_token_name another_long_token\n",
        "very_long_token_name\nanother_long_token\n",
        0,
        "",
    );
}

#[test]
fn test_numeric_tokens() {
    tsort_test(&[], "1 2\n2 3\n3 4\n", "1\n2\n3\n4\n", 0, "");
}

#[test]
fn test_mixed_tokens() {
    tsort_test(
        &[],
        "file1.c file1.o\nfile1.o prog\n",
        "file1.c\nfile1.o\nprog\n",
        0,
        "",
    );
}
