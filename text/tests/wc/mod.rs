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

#[test]
fn wc_empty() {
    wc_test(&["-c"], "", "0 (stdin)\n");
    wc_test(&["-l"], "", "0 (stdin)\n");
    wc_test(&["-w"], "", "0 (stdin)\n");
}

#[test]
fn wc_one() {
    wc_test(&["-c"], "x", "1 (stdin)\n");
    wc_test(&["-l"], "x", "0 (stdin)\n");
    wc_test(&["-w"], "x", "1 (stdin)\n");
}

#[test]
fn wc_two() {
    wc_test(&["-c"], "x y\n", "4 (stdin)\n");
    wc_test(&["-l"], "x y\n", "1 (stdin)\n");
    wc_test(&["-w"], "x y\n", "2 (stdin)\n");
}
