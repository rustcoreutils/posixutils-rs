//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU's one-line `a`, `i` and `c`: the text follows the command on its own
//! line (`a text`, `a\text`), and the POSIX `a\` <newline> form is unchanged.
//! Expectations are GNU sed 4.9's output.

use super::sed_test;
use plib::testing::get_binary_path;
use std::fs;
use std::process::Command;

const INPUT: &str = "l1\nl2\nl3\n";

fn ok(args: &[&str], expected: &str) {
    sed_test(args, INPUT, expected, "", 0);
}

// perl's debian/rules and binutils' gprofng build use these two.
#[test]
fn debian_uses() {
    ok(&["$i #define X 1"], "l1\nl2\n#define X 1\nl3\n");
    ok(&["1 i /* C */"], "/* C */\nl1\nl2\nl3\n");
}

#[test]
fn each_command() {
    ok(&["2a text"], "l1\nl2\ntext\nl3\n");
    ok(&["2i text"], "l1\ntext\nl2\nl3\n");
    ok(&["2c text"], "l1\ntext\nl3\n");
    ok(&["2,3c two"], "l1\ntwo\n");
    ok(&["$!a x"], "l1\nx\nl2\nx\nl3\n");
}

// Blanks after the letter are skipped; the rest of the line is the text,
// trailing blanks included, and a `\` keeps leading blanks.
#[test]
fn blanks() {
    ok(&["1a    spaced   text  "], "l1\nspaced   text  \nl2\nl3\n");
    ok(&["1a\ttab"], "l1\ntab\nl2\nl3\n");
    ok(&["1a\\  lead"], "l1\n  lead\nl2\nl3\n");
    ok(&["1a \\  lead"], "l1\n  lead\nl2\nl3\n");
    ok(&["1a\\text"], "l1\ntext\nl2\nl3\n");
}

// `;`, `}` and `#` belong to the text; only the <newline> ends it.
#[test]
fn text_runs_to_end_of_line() {
    ok(&["1a foo; p"], "l1\nfoo; p\nl2\nl3\n");
    ok(&["1a foo }"], "l1\nfoo }\nl2\nl3\n");
    ok(&["1a#notcomment"], "l1\n#notcomment\nl2\nl3\n");
    ok(&["1a x\np"], "l1\nl1\nx\nl2\nl2\nl3\nl3\n");
    ok(&["1d;a x"], "l2\nx\nl3\nx\n");
    ok(&["2{a foo\n}"], "l1\nl2\nfoo\nl3\n");
}

#[test]
fn backslashes() {
    ok(&["1a t1\\\nt2"], "l1\nt1\nt2\nl2\nl3\n");
    ok(&["1a foo\\"], "l1\nfoo\nl2\nl3\n");
    ok(&["1a foo\\\\"], "l1\nfoo\\\nl2\nl3\n");
    ok(&["1a foo\\\\bar"], "l1\nfoo\\bar\nl2\nl3\n");
    ok(&["1a foo\\bar"], "l1\nfoobar\nl2\nl3\n");
    ok(&["1a foo\\tbar"], "l1\nfoo\tbar\nl2\nl3\n");
    ok(&["1a foo\\nbar"], "l1\nfoo\nbar\nl2\nl3\n");
    ok(&["1a \\x41"], "l1\nx41\nl2\nl3\n");
}

// The POSIX form keeps POSIX's rule: a `\` before any other character is
// removed.
#[test]
fn posix_form_unchanged() {
    ok(&["1a\\\ntext"], "l1\ntext\nl2\nl3\n");
    ok(&["1a\\\n  text"], "l1\n  text\nl2\nl3\n");
    ok(&["1a\\\nt1\\\nt2"], "l1\nt1\nt2\nl2\nl3\n");
    ok(&["1a\\\nfoo\\tbar"], "l1\nfootbar\nl2\nl3\n");
    ok(&["1i\\\nmulti\\\nline"], "multi\nline\nl1\nl2\nl3\n");
}

#[test]
fn missing_text() {
    sed_test(
        &["a"],
        INPUT,
        "",
        "sed: text must be separated with '\\' (line: 0, col: 2)\n",
        1,
    );
    sed_test(
        &["a  "],
        INPUT,
        "",
        "sed: text must be separated with '\\' (line: 0, col: 4)\n",
        1,
    );
    sed_test(
        &["a\np"],
        INPUT,
        "",
        "sed: text must be separated with '\\' (line: 0, col: 2)\n",
        1,
    );
}

// The -e chunks are joined by <newline>s, as POSIX says.
#[test]
fn e_chunks() {
    ok(
        &["-e", "1a foo", "-e", "p"],
        "l1\nl1\nfoo\nl2\nl2\nl3\nl3\n",
    );
    ok(
        &["-e", "1a hello", "-e", "2i bye"],
        "l1\nhello\nbye\nl2\nl3\n",
    );
    ok(&["-e", "2{", "-e", "a foo", "-e", "}"], "l1\nl2\nfoo\nl3\n");
    ok(&["-e", "1a x\\", "-e", "y"], "l1\nx\ny\nl2\nl3\n");
    ok(&["-e", "1a\\", "-e", "text"], "l1\ntext\nl2\nl3\n");
    ok(
        &["-e", "1a\\", "-e", "text\\", "-e", "more"],
        "l1\ntext\nmore\nl2\nl3\n",
    );
    ok(&["-e", "$a\\", "-e", "  lead"], "l1\nl2\nl3\n  lead\n");
    // A `\` ending the whole script continues nothing.
    ok(&["-e", "1a\\", "-e", "foo\\"], "l1\nfoo\nl2\nl3\n");
}

#[test]
fn script_file_and_in_place() {
    let td = plib::tmp::tempdir().unwrap();
    let script = td.path().join("script");
    fs::write(&script, "1a one\n2i\\\ntwo\n$c three\n").unwrap();
    ok(
        &["-f", script.to_str().unwrap()],
        "l1\none\ntwo\nl2\nthree\n",
    );

    let file = td.path().join("config.h");
    fs::write(&file, INPUT).unwrap();
    let out = Command::new(get_binary_path("sed"))
        .args(["-i", "$i #define BUILD \"today\"", file.to_str().unwrap()])
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(0), "{:?}", out);
    assert_eq!(
        fs::read_to_string(&file).unwrap(),
        "l1\nl2\n#define BUILD \"today\"\nl3\n"
    );
}

#[test]
fn separate_streams() {
    let td = plib::tmp::tempdir().unwrap();
    let file = td.path().join("in");
    fs::write(&file, INPUT).unwrap();
    let f = file.to_str().unwrap();
    ok(
        &["-s", "$a end", f, f],
        "l1\nl2\nl3\nend\nl1\nl2\nl3\nend\n",
    );
}
