//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The file operands are one input stream (POSIX sed, INPUT FILES): line
//! numbers run on across files, `$` is the last line of the last file, and
//! the hold space carries over.

use plib::testing::{run_test, TempFile, TestPlan};

fn sed_files(args: &[&str], output: &str, err: &str, code: i32) {
    run_test(TestPlan {
        cmd: String::from("sed"),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: String::from(output),
        expected_err: String::from(err),
        expected_exit_code: code,
    });
}

fn two_files() -> (TempFile, TempFile) {
    (TempFile::new("f1", "a\nb\n"), TempFile::new("f2", "c\nd\n"))
}

fn path(file: &TempFile) -> String {
    file.path().display().to_string()
}

#[test]
fn line_numbers_run_on_across_files() {
    let (f1, f2) = two_files();
    sed_files(&["-n", "=", &path(&f1), &path(&f2)], "1\n2\n3\n4\n", "", 0);
}

#[test]
fn last_line_is_the_last_of_the_last_file() {
    let (f1, f2) = two_files();
    sed_files(&["-n", "$p", &path(&f1), &path(&f2)], "d\n", "", 0);
}

#[test]
fn hold_space_carries_across_files() {
    let (f1, f2) = two_files();
    sed_files(
        &["-n", "H;${x;p}", &path(&f1), &path(&f2)],
        "\na\nb\nc\nd\n",
        "",
        0,
    );
}

#[test]
fn range_spans_files() {
    let (f1, f2) = two_files();
    sed_files(&["-n", "/b/,/c/p", &path(&f1), &path(&f2)], "b\nc\n", "", 0);
}

// A file whose last line has no <newline> is followed by the next file's
// first line on a line of its own, as GNU sed does.
#[test]
fn unterminated_line_before_another_file() {
    let f1 = TempFile::new("f1", "a");
    let f2 = TempFile::new("f2", "b\n");
    sed_files(&["p", &path(&f1), &path(&f2)], "a\na\nb\nb\n", "", 0);
}

// An unreadable operand is reported, the rest are still read, and the exit
// status says something went wrong (GNU uses 2).
#[test]
fn unreadable_file_is_reported_and_skipped() {
    let f1 = TempFile::new("f1", "a\n");
    sed_files(
        &["p", "/nonexistent/sed-input", &path(&f1)],
        "a\na\n",
        "sed: can't read /nonexistent/sed-input: No such file or directory\n",
        2,
    );
}

// An operand after `--` is a file, even when it is spelled "-e".
#[test]
fn e_after_double_dash_is_an_operand() {
    sed_files(
        &["p", "--", "-e"],
        "",
        "sed: can't read -e: No such file or directory\n",
        2,
    );
}
