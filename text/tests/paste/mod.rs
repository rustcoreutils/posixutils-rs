//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, TestPlanU8, run_test, run_test_u8};
use std::fs;
use std::path::PathBuf;

fn get_test_file_path(filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests/paste");
    path.push(filename);
    path
}

fn run_paste_test(args: Vec<&str>, expected_output_filename: &str) {
    let expected_output_file_path = get_test_file_path(expected_output_filename);

    let expected_out = fs::read_to_string(expected_output_file_path).unwrap();

    let args = args.into_iter().map(ToOwned::to_owned).collect();

    run_test(TestPlan {
        cmd: "paste".to_owned(),
        args,
        expected_out,
        expected_err: String::new(),
        stdin_data: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn paste_default_behavior() {
    run_paste_test(
        vec!["tests/paste/input1.txt", "tests/paste/input2.txt"],
        "output_default.txt",
    );
}

#[test]
fn paste_serial_mode() {
    run_paste_test(
        vec!["-s", "tests/paste/input1.txt", "tests/paste/input2.txt"],
        "output_serial.txt",
    );
}

#[test]
fn paste_custom_delimiter() {
    run_paste_test(
        vec![
            "-d",
            ",",
            "tests/paste/input1.txt",
            "tests/paste/input2.txt",
        ],
        "output_custom_delim.txt",
    );
}

#[test]
fn paste_serial_custom_delimiter() {
    run_paste_test(
        vec![
            "-s",
            "-d",
            ",",
            "tests/paste/input1.txt",
            "tests/paste/input2.txt",
        ],
        "output_serial_custom_delim.txt",
    );
}

// Test case for:
//
// thread 'main' panicked at text/./paste.rs:133:16:
// index out of bounds: the len is 0 but the index is 18446744073709551615
// note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
#[test]
fn paste_simple_multi_line_input() {
    let args = ["-d", "", "-s", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect();

    run_test(TestPlan {
        cmd: "paste".to_owned(),
        args,
        expected_out: "Line 1Line 2Line 3\n".to_owned(),
        expected_err: String::new(),
        stdin_data: "\
Line 1
Line 2
Line 3
"
        .to_owned(),
        expected_exit_code: 0,
    });
}

#[test]
fn paste_multiple_stdin() {
    let args = ["-d", "ABC", "--", "-", "-", "-", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect();

    run_test(TestPlan {
        cmd: "paste".to_owned(),
        args,
        expected_out: "\
Line 1ALine 2BLine 3CLine 4
Line 5ALine 6BLine 7CLine 8
Line 9ABC
"
        .to_owned(),
        expected_err: String::new(),
        stdin_data: "\
Line 1
Line 2
Line 3
Line 4
Line 5
Line 6
Line 7
Line 8
Line 9
"
        .to_owned(),
        expected_exit_code: 0,
    });
}

#[test]
fn paste_multiple_stdin_serial_test_one() {
    let args = ["-d", "ABC", "-s", "--", "-", "-", "-", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect();

    run_test(TestPlan {
        cmd: "paste".to_owned(),
        args,
        expected_out: "\
Line 1ALine 2BLine 3CLine 4ALine 5BLine 6CLine 7ALine 8BLine 9



"
        .to_owned(),
        expected_err: String::new(),
        stdin_data: "\
Line 1
Line 2
Line 3
Line 4
Line 5
Line 6
Line 7
Line 8
Line 9
"
        .to_owned(),
        expected_exit_code: 0,
    });
}

// This implementation was improperly truncating the second "e"
#[test]
fn paste_multiple_stdin_serial_test_two() {
    let args = ["-d", "!", "-s", "--", "-", "-", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect();

    run_test(TestPlan {
        cmd: "paste".to_owned(),
        args,
        expected_out: "\
O!K!1!234!here


"
        .to_owned(),
        expected_err: String::new(),
        stdin_data: "\
O
K
1
234
here"
            .to_owned(),
        expected_exit_code: 0,
    });
}

#[test]
fn paste_non_utf8_input() {
    const INPUT: &[u8] = b"String with non-UTF-8 bytes: \xFF\x00\xFF.\n";

    let args = ["--", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();

    run_test_u8(TestPlanU8 {
        args,
        cmd: "paste".to_owned(),
        expected_err: Vec::<u8>::new(),
        expected_exit_code: 0,
        expected_out: INPUT.to_vec(),
        stdin_data: INPUT.to_vec(),
    });
}

#[test]
fn paste_backslash_zero_delimiter() {
    let args = ["-d", r#"\0\!\0"#, "-s", "--", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();

    run_test(TestPlan {
        args,
        cmd: "paste".to_owned(),
        expected_err: String::new(),
        expected_exit_code: 0,
        expected_out: "\
AB!CDE!F
"
        .to_owned(),
        stdin_data: "\
A
B
C
D
E
F
"
        .to_owned(),
    });
}

#[test]
fn paste_trailing_unescaped_backslash_delimiter() {
    let args = ["-d", r#"\\\"#, "-s", "--", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();

    run_test(TestPlan {
        args,
        cmd: "paste".to_owned(),
        expected_err: r#"paste: delimiter list ends with an unescaped backslash: \\\
"#
        .to_owned(),
        expected_exit_code: 1,
        expected_out: String::new(),
        stdin_data: String::new(),
    });
}

#[test]
fn paste_trailing_escaped_backslash_delimiter() {
    let args = ["-d", r#"\\!\@\\"#, "-s", "--", "-"]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();

    run_test(TestPlan {
        args,
        cmd: "paste".to_owned(),
        expected_err: String::new(),
        expected_exit_code: 0,
        expected_out: r#"A\B!C@D\E\F!G@H\I\J
"#
        .to_owned(),
        stdin_data: "\
A
B
C
D
E
F
G
H
I
J
"
        .to_owned(),
    });
}

#[test]
fn paste_custom_delimiters() {
    run_paste_test(
        vec![
            "-d",
            "!@#",
            "--",
            "tests/paste/input1.txt",
            "tests/paste/input2.txt",
            "tests/paste/input1.txt",
            "tests/paste/input2.txt",
            "tests/paste/input2.txt",
        ],
        "output_paste_custom_delimiters.txt",
    );
}

#[test]
fn paste_custom_delimiters_serial() {
    run_paste_test(
        vec![
            "-d",
            "!@#",
            "-s",
            "--",
            "tests/paste/input1.txt",
            "tests/paste/input2.txt",
        ],
        "output_paste_custom_delimiters_serial.txt",
    );
}
