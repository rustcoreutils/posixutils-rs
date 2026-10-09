//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The GNU extension `-s`/`--separate`: each file operand is a stream of its
//! own, with its own line numbers and its own `$`, as under `-i`.

use plib::testing::{run_test, TempFile, TestPlan};
use plib::tmp::TempDir;
use std::fs;

fn sed_separate(args: &[&str], stdin: &str, output: &str, err: &str, code: i32) {
    run_test(TestPlan {
        cmd: String::from("sed"),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: String::from(stdin),
        expected_out: String::from(output),
        expected_err: String::from(err),
        expected_exit_code: code,
    });
}

fn two_files() -> (TempFile, TempFile) {
    (
        TempFile::new("s1", "a1\na2\n"),
        TempFile::new("s2", "b1\nb2\nb3\n"),
    )
}

fn path(file: &TempFile) -> String {
    file.path().display().to_string()
}

#[test]
fn last_line_of_each_file() {
    let (f1, f2) = two_files();
    let (p1, p2) = (path(&f1), path(&f2));
    sed_separate(&["-s", "-n", "$p", &p1, &p2], "", "a2\nb3\n", "", 0);
    sed_separate(&["--separate", "-n", "$p", &p1, &p2], "", "a2\nb3\n", "", 0);
    // Without -s the files are one stream.
    sed_separate(&["-n", "$p", &p1, &p2], "", "b3\n", "", 0);
}

#[test]
fn line_numbers_restart() {
    let (f1, f2) = two_files();
    let (p1, p2) = (path(&f1), path(&f2));
    sed_separate(&["-sn", "1p", &p1, &p2], "", "a1\nb1\n", "", 0);
    sed_separate(
        &["-s", "=", &p1, &p2],
        "",
        "1\na1\n2\na2\n1\nb1\n2\nb2\n3\nb3\n",
        "",
        0,
    );
    sed_separate(
        &["-s", "$a\\\nEND", &p1, &p2],
        "",
        "a1\na2\nEND\nb1\nb2\nb3\nEND\n",
        "",
        0,
    );
}

// Standard input is a stream like any other, and `q` ends the whole run.
#[test]
fn standard_input_and_quit() {
    let (f1, f2) = two_files();
    let (p1, p2) = (path(&f1), path(&f2));
    sed_separate(&["-s", "-n", "$p", "-", &p2], "x\ny\n", "y\nb3\n", "", 0);
    sed_separate(&["-s", "2q", &p1, &p2], "", "a1\na2\n", "", 0);
}

// A final line without a newline stays the last line of its own file, and
// gets a newline only when more output follows, as in GNU sed.
#[test]
fn unterminated_last_line() {
    let f1 = TempFile::new("s3", "c1");
    let (_, f2) = two_files();
    let (p1, p2) = (path(&f1), path(&f2));
    sed_separate(&["-s", "-n", "$p", &p1, &p2], "", "c1\nb3\n", "", 0);
    sed_separate(&["-s", "$!d", &p1, &p2], "", "c1\nb3\n", "", 0);
    sed_separate(&["-s", "-n", "$p", &p2, &p1], "", "b3\nc1", "", 0);
    sed_separate(&["-s", "-n", "/b3/p", &p1, &p2], "", "b3\n", "", 0);
}

// A file that cannot be read is reported and skipped, and the status is 2.
#[test]
fn unreadable_file_is_skipped() {
    let (f1, f2) = two_files();
    let dir = TempDir::new().unwrap();
    let missing = dir.path().join("nosuch").display().to_string();
    let err = fs::File::open(&missing).expect_err("nosuch must not exist");
    sed_separate(
        &["-s", "-n", "$p", &path(&f1), &missing, &path(&f2)],
        "",
        "a2\nb3\n",
        &format!(
            "sed: can't read {missing}: {}\n",
            plib::diag::io_error_text(&err)
        ),
        2,
    );
}

// sysvinit's man page rule.
#[test]
fn in_place_separate() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("a.8"), "@VERSION@ x\n").unwrap();
    fs::write(dir.path().join("b.8"), "y @VERSION@\n").unwrap();
    let out = std::process::Command::new(plib::testing::get_binary_path("sed"))
        .args([
            "--in-place=.orig",
            "--separate",
            "s/@VERSION@/3.14/g",
            "a.8",
            "b.8",
        ])
        .current_dir(dir.path())
        .output()
        .unwrap();
    assert_eq!(
        (
            out.status.code(),
            out.stdout.as_slice(),
            out.stderr.as_slice()
        ),
        (Some(0), &b""[..], &b""[..])
    );
    let read = |name: &str| fs::read_to_string(dir.path().join(name)).unwrap();
    assert_eq!(read("a.8"), "3.14 x\n");
    assert_eq!(read("b.8"), "y 3.14\n");
    assert_eq!(read("a.8.orig"), "@VERSION@ x\n");
    assert_eq!(read("b.8.orig"), "y @VERSION@\n");
}
