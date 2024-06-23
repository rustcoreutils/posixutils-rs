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

fn test_test(args: &[&str], expected_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("test"),
        args: str_args,
        stdin_data: String::from(""),
        expected_out: String::from(""),
        expected_err: String::from(""),
        expected_exit_code: expected_code,
    });
}

#[test]
fn test_intops() {
    test_test(&["20", "-eq", "20"], 0);
    test_test(&["20", "-eq", "21"], 1);

    test_test(&["20", "-ne", "20"], 1);
    test_test(&["20", "-ne", "21"], 0);

    test_test(&["20", "-gt", "10"], 0);
    test_test(&["10", "-gt", "20"], 1);

    test_test(&["20", "-ge", "10"], 0);
    test_test(&["10", "-ge", "20"], 1);
    test_test(&["20", "-ge", "20"], 0);

    test_test(&["20", "-lt", "10"], 1);
    test_test(&["10", "-lt", "20"], 0);

    test_test(&["20", "-le", "10"], 1);
    test_test(&["10", "-le", "20"], 0);
    test_test(&["20", "-le", "20"], 0);
}

#[test]
fn test_strops() {
    test_test(&["a", "=", "a"], 0);
    test_test(&["a", "=", "b"], 1);

    test_test(&["a", "!=", "a"], 1);
    test_test(&["a", "!=", "b"], 0);

    test_test(&["a", "<", "b"], 0);
    test_test(&["b", "<", "a"], 1);

    test_test(&["a", ">", "b"], 1);
    test_test(&["b", ">", "a"], 0);
}

#[test]
fn test_str_basic() {
    test_test(&[], 1);

    test_test(&[""], 1);
    test_test(&["a"], 0);

    test_test(&["-z", ""], 0);
    test_test(&["-z", "a"], 1);

    test_test(&["-n", ""], 1);
    test_test(&["-n", "a"], 0);
}

