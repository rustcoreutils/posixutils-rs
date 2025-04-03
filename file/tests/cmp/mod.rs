//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib_testing::{run_test, TestPlan};

fn run_test_helper(
    args: &[&str],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("cmp"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

#[test]
fn cmp_same() {
    let mut files = vec![String::from("tests/cmp/lorem_ipsum.txt")];
    let indices = [0, 45, 90, 135, 180, 225, 270, 315, 360, 405, 450];
    for i in indices {
        files.push(format!("tests/cmp/lorem_ipsum_{i}.txt"));
    }

    for file in &files {
        run_test_helper(&[file, file], "", "", 0);
    }
}

#[test]
fn cmp_different() {
    let original = "tests/cmp/lorem_ipsum.txt";

    let indices = [0, 45, 90, 135, 180, 225, 270, 315, 360, 405, 450];
    let bytes = [1, 46, 91, 136, 181, 226, 271, 316, 361, 406, 451];
    let lines = [1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 7];

    for i in 0..indices.len() {
        let modified = format!("tests/cmp/lorem_ipsum_{}.txt", indices[i]);
        run_test_helper(
            &[original, &modified],
            &format!(
                "{original} {modified} differ: char {}, line {}\n",
                bytes[i], lines[i]
            ),
            "",
            1,
        );
    }
}

#[test]
fn cmp_different_silent() {
    let original = "tests/cmp/lorem_ipsum.txt";

    let indices = [0, 45, 90, 135, 180, 225, 270, 315, 360, 405, 450];

    for i in 0..indices.len() {
        let modified = format!("tests/cmp/lorem_ipsum_{}.txt", indices[i]);
        run_test_helper(&["-s", original, &modified], "", "", 1);
    }
}

#[test]
fn cmp_different_less_verbose() {
    let original = "tests/cmp/lorem_ipsum.txt";

    let indices = [0, 45, 90, 135, 180, 225, 270, 315, 360, 405, 450];
    let bytes = [1, 46, 91, 136, 181, 226, 271, 316, 361, 406, 451];
    let chars_original = ['L', 's', ' ', ' ', 'a', 'o', 'r', ' ', 'a', ' ', '.'];

    for i in 0..indices.len() {
        let modified = format!("tests/cmp/lorem_ipsum_{}.txt", indices[i]);
        run_test_helper(
            &["-l", original, &modified],
            &format!(
                "{} {:o} {:o}\n",
                bytes[i], chars_original[i] as u8, '?' as u8
            ),
            "",
            1,
        );
    }
}

#[test]
fn cmp_eof() {
    let original = "tests/cmp/lorem_ipsum.txt";
    let truncated = "tests/cmp/lorem_ipsum_trunc.txt";

    // Status code must be 1. From the specification:
    //
    // "...this includes the case where one file is identical to the first part
    // of the other."
    run_test_helper(
        &[original, truncated],
        "",
        &format!("cmp: EOF on {truncated}\n"),
        1,
    );
}
