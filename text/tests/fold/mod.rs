//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

fn get_test_file_path(filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests/fold");
    path.push(filename);
    path
}

fn run_fold_test(args: Vec<&str>, input_filename: &str, expected_output_filename: &str) {
    let input_file_path = get_test_file_path(input_filename);
    let input_data = match File::open(input_file_path) {
        Ok(mut file) => {
            let mut data = String::new();
            file.read_to_string(&mut data).unwrap();
            data
        }
        Err(e) => {
            panic!("Error opening file: {}", e);
        }
    };

    let expected_output_file_path = get_test_file_path(expected_output_filename);

    let mut expected_output = String::new();
    File::open(expected_output_file_path)
        .unwrap()
        .read_to_string(&mut expected_output)
        .unwrap();

    let args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    run_test(TestPlan {
        cmd: String::from("fold"),
        args,
        expected_out: expected_output,
        expected_err: String::new(),
        expected_exit_code: 0,
        stdin_data: input_data,
    });
}

#[test]
fn fold_default_behavior() {
    run_fold_test(vec![], "input1.txt", "output_default.txt");
}

#[test]
fn fold_custom_width() {
    run_fold_test(vec!["-w", "40"], "input1.txt", "output_width40.txt");
}

#[test]
fn fold_bytes_mode() {
    run_fold_test(vec!["-b"], "input1.txt", "output_bytes.txt");
}

#[test]
fn fold_spaces_mode() {
    run_fold_test(vec!["-s"], "input2.txt", "output_spaces.txt");
}

#[test]
fn fold_bytes_and_spaces_mode() {
    run_fold_test(vec!["-b", "-s"], "input2.txt", "output_bytes_spaces.txt");
}

#[test]
fn fold_dash_operand_reads_stdin() {
    // A "-" operand reads standard input rather than a file named "-".
    run_test(TestPlan {
        cmd: String::from("fold"),
        args: vec![String::from("-")],
        stdin_data: String::from("hello\n"),
        expected_out: String::from("hello\n"),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

fn fold_args_test(args: &[&str], input: &str, expected: &str) {
    run_test(TestPlan {
        cmd: String::from("fold"),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: String::from(input),
        expected_out: String::from(expected),
        expected_err: String::from(""),
        expected_exit_code: 0,
    });
}

#[test]
fn fold_spaces_breaks_at_last_blank() {
    // -s breaks after the last blank within the width. Regression for the
    // reversed-index bug, which split at the wrong byte position.
    fold_args_test(
        &["-w", "10", "-s"],
        "aaaa bbbb cccc\n",
        "aaaa bbbb \ncccc\n",
    );
}

#[test]
fn fold_hard_break_no_spaces() {
    fold_args_test(&["-w", "5"], "abcdefghij\n", "abcde\nfghij\n");
}

#[test]
fn fold_spaces_no_blank_falls_back_to_hard_break() {
    // With -s but no blank within the width, fold still breaks at the width.
    fold_args_test(&["-w", "3", "-s"], "abcdef\n", "abc\ndef\n");
}

#[test]
fn fold_tab_advances_to_stop() {
    // A tab counts to the next 8-column stop; with -w 8 the tab fills the line
    // and the following character folds to the next line.
    fold_args_test(&["-w", "8"], "\tX\n", "\t\nX\n");
}

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

fn run_fold_locale(args: &[&str], input: &[u8], locale: &str) -> Vec<u8> {
    use std::io::Write;
    use std::process::{Command, Stdio};
    let mut child = Command::new(env!("CARGO_BIN_EXE_fold"))
        .args(args)
        .env("LC_ALL", locale)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    child.stdin.as_mut().unwrap().write_all(input).unwrap();
    child.wait_with_output().unwrap().stdout
}

#[test]
fn fold_wide_character_width() {
    // Two double-width CJK characters with -w 2: each occupies a full line and
    // is never split mid-character. Skipped where no UTF-8 locale is available.
    let Some(loc) = utf8_locale() else {
        return;
    };
    // 世 (U+4E16) 界 (U+754C), each 2 columns.
    let out = run_fold_locale(&["-w", "2"], "世界\n".as_bytes(), &loc);
    assert_eq!(out, "世\n界\n".as_bytes());
}
