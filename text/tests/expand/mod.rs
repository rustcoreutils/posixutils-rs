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

#[test]
fn expand_basic() {
    expand_test_noargs("", "");
    expand_test_noargs("a\tb\tc\n", "a       b       c\n");
}

#[test]
fn expand_dash_operand_reads_stdin() {
    // A "-" operand reads standard input rather than a file named "-".
    run_test(TestPlan {
        cmd: String::from("expand"),
        args: vec![String::from("-")],
        stdin_data: String::from("a\tb\tc\n"),
        expected_out: String::from("a       b       c\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

fn expand_args_test(args: &[&str], input: &str, expected: &str) {
    run_test(TestPlan {
        cmd: String::from("expand"),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: String::from(input),
        expected_out: String::from(expected),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn expand_rejects_zero_tabsize() {
    // A tab size of zero must be rejected, not panic on modulo-by-zero.
    run_test(TestPlan {
        cmd: String::from("expand"),
        args: vec!["-t".to_string(), "0".to_string()],
        stdin_data: String::from("a\tb\n"),
        expected_out: String::new(),
        expected_err: String::from("tab size must be a positive integer\n"),
        expected_exit_code: 1,
    });
}

#[test]
fn expand_rejects_zero_in_list() {
    run_test(TestPlan {
        cmd: String::from("expand"),
        args: vec!["-t".to_string(), "0,8".to_string()],
        stdin_data: String::from("a\tb\n"),
        expected_out: String::new(),
        expected_err: String::from("tab stop must be a positive integer\n"),
        expected_exit_code: 1,
    });
}

#[test]
fn expand_multi_stop_list() {
    // -t 4,8 (0-based stops): a leading tab fills to column 4 (4 spaces); after
    // "    a" (col 5) the next tab fills to column 8 (3 spaces). Matches GNU.
    expand_args_test(&["-t", "4,8"], "\ta\tb\n", "    a   b\n");
}

#[test]
fn expand_multi_stop_resets_each_line() {
    // The stop position is recomputed from the column at every newline, so
    // line 2 expands identically to line 1.
    expand_args_test(&["-t", "4,8"], "\ta\n\tc\n", "    a\n    c\n");
}

#[test]
fn expand_tab_at_col1_list_stop() {
    // -t 4,8: a leading tab advances to column 4 — four spaces (GNU/POSIX).
    expand_args_test(&["-t", "4,8"], "\tX\n", "    X\n");
}

/// A usable UTF-8 locale name, or `None` if neither is installed.
fn utf8_locale() -> Option<String> {
    let avail = std::process::Command::new("locale")
        .arg("-a")
        .output()
        .ok()?;
    let list = String::from_utf8_lossy(&avail.stdout).to_lowercase();
    for name in ["C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8"] {
        if list.contains(&name.to_lowercase()) {
            return Some(name.to_string());
        }
    }
    None
}

fn run_expand_locale(args: &[&str], input: &str, locale: &str) -> String {
    use std::io::Write;
    use std::process::{Command, Stdio};
    let mut child = Command::new(env!("CARGO_BIN_EXE_expand"))
        .args(args)
        .env("LC_ALL", locale)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(input.as_bytes())
        .unwrap();
    let out = child.wait_with_output().unwrap();
    String::from_utf8_lossy(&out.stdout).to_string()
}

#[test]
fn expand_multibyte_column_width() {
    // "é" (U+00E9) is one column wide, so a following tab advances to column 8
    // (7 spaces), not column 9 as a byte-oriented count would produce. Skipped
    // where no UTF-8 locale is available.
    let Some(loc) = utf8_locale() else {
        return;
    };
    let out = run_expand_locale(&[], "\u{e9}\tX\n", &loc);
    assert_eq!(out, "\u{e9}       X\n");
}
