//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test, TestPlan};

fn echo_test(args: &[&str], expected_output: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("echo"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
    });
}

#[test]
fn test_echo_basic() {
    echo_test(&["big", "brown", "bear"], "big brown bear\n");

    echo_test(&["-n", "foo", "bar"], "foo bar");
    echo_test(&["foo", "bar\\c"], "foo bar");
}
