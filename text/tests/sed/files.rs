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

use plib::testing::{open_error_text, run_test, TempFile, TestPlan};

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

// The last line of the input, when it has no <newline>, is written without
// one; but any output after it starts on a line of its own, as in GNU sed.
// `p` ran the two copies together as "aa".
#[test]
fn unterminated_last_line_owes_a_newline_to_more_output() {
    let f1 = TempFile::new("f1", "a");
    let f2 = TempFile::new("f2", "x\n");
    let (p1, p2) = (path(&f1), path(&f2));
    sed_files(&["p", &p1], "a\na", "", 0);
    sed_files(&["-n", "p;p", &p1], "a\na", "", 0);
    sed_files(&["s/a//;p", &p1], "\n", "", 0);
    sed_files(&["", &p1], "a", "", 0);
    // Deferred `r` output pays the debt too, even for an unreadable file,
    // but owes nothing when no line was written.
    sed_files(&[&format!("$r {p2}"), &p1], "a\nx\n", "", 0);
    sed_files(&["-n", &format!("$r {p2}"), &p1], "x\n", "", 0);
    sed_files(&["$r /nonexistent/sed-input", &p1], "a\n", "", 0);
}

// Under -i each file is an output of its own, so the newline one file owes
// is not written into the next.
#[test]
fn in_place_unterminated_line_owes_nothing_to_the_next_file() {
    let dir = plib::tmp::TempDir::new().unwrap();
    std::fs::write(dir.path().join("f1"), "a").unwrap();
    std::fs::write(dir.path().join("f2"), "b\n").unwrap();
    let out = std::process::Command::new(plib::testing::get_binary_path("sed"))
        .args(["-i", "p", "f1", "f2"])
        .current_dir(dir.path())
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(0));
    let read = |name: &str| std::fs::read_to_string(dir.path().join(name)).unwrap();
    assert_eq!(read("f1"), "a\na");
    assert_eq!(read("f2"), "b\nb\n");
}

// An unreadable operand is reported, the rest are still read, and the exit
// status says something went wrong (GNU uses 2).
#[test]
fn unreadable_file_is_reported_and_skipped() {
    let f1 = TempFile::new("f1", "a\n");
    let missing = "/nonexistent/sed-input";
    sed_files(
        &["p", missing, &path(&f1)],
        "a\na\n",
        &format!("sed: can't read {missing}: {}\n", open_error_text(missing)),
        2,
    );
}

// An operand after `--` is a file, even when it is spelled "-e".
#[test]
fn e_after_double_dash_is_an_operand() {
    sed_files(
        &["p", "--", "-e"],
        "",
        &format!("sed: can't read -e: {}\n", open_error_text("-e")),
        2,
    );
}
