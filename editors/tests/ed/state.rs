//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! ed state carried between commands: the remembered RE and the marks.

use super::parse::{ed, ok};

#[test]
fn test_null_re_in_substitute_is_last_address_re() {
    // POSIX: "A null RE shall be equivalent to the last RE encountered" --
    // from an address as much as from an earlier `s`.
    assert_eq!(ok("foo\nbar\n", "/bar/s//X/\n,p\nQ\n"), "foo\nX\n");
    assert_eq!(ok("foo\nbar\n", "?bar?s//X/\n,p\nQ\n"), "foo\nX\n");
}

#[test]
fn test_null_re_in_substitute_inside_global() {
    assert_eq!(ok("a\nc\na\n", "g/a/s//b/\n,p\nQ\n"), "b\nc\nb\n");
}

#[test]
fn test_null_re_in_substitute_after_global() {
    // `g/x/` prints both lines and leaves `x` as the remembered RE.
    assert_eq!(ok("x\na\nx\n", "g/x/\n1s//y/\n,p\nQ\n"), "x\nx\ny\na\nx\n");
}

#[test]
fn test_null_re_with_nothing_remembered_is_an_error() {
    let (out, code, _) = ed("a\n", "s//b/\nQ\n");
    assert_eq!(out, "?\n");
    assert_ne!(code, 0);
}

#[test]
fn test_undo_restores_marks() {
    // `1d` moves the mark on `two` up a line; `u` must move it back.
    assert_eq!(ok("one\ntwo\nthree\n", "2ka\n1d\nu\n'ap\nQ\n"), "two\n");
}

#[test]
fn test_undo_restores_mark_on_deleted_line() {
    assert_eq!(ok("one\ntwo\nthree\n", "2ka\n2d\nu\n'ap\nQ\n"), "two\n");
}

#[test]
fn test_undo_of_undo_restores_marks_again() {
    assert_eq!(
        ok("one\ntwo\nthree\n", "3ka\n1d\nu\nu\n'ap\nQ\n"),
        "three\n"
    );
}

#[test]
fn test_undo_keeps_mark_set_after_the_change() {
    // `k` changes no text, so `u` undoes the `1d` before it -- and the mark
    // set in between still names its line.
    assert_eq!(ok("x\ny\n", "1d\n1ka\nu\n'ap\nQ\n"), "y\n");
    assert_eq!(
        ok("one\ntwo\nthree\n", "3ka\n1d\n1ka\nu\n'ap\nQ\n"),
        "two\n"
    );
}

#[test]
fn test_undo_drops_mark_on_line_the_change_made() {
    let (out, code, _) = ed("a\n", "$a\nb\n.\n2ka\nu\n'ap\nQ\n");
    assert_eq!(out, "?\n");
    assert_ne!(code, 0);
}

#[test]
fn test_marks_follow_moved_lines() {
    assert_eq!(ok("a\nb\nc\n", "1ka\n1m$\n'an\nQ\n"), "3\ta\n");
}

#[test]
fn test_repeat_substitute_needs_an_earlier_substitute() {
    let (out, code, file) = ed("foo bar\n", "/foo/\n&\nw\nq\n");
    assert_eq!(out, "foo bar\n?\n");
    assert_eq!(file, "foo bar\n");
    assert_ne!(code, 0);
}

#[test]
fn test_invalid_re_does_not_replace_remembered_re() {
    let (out, _, _) = ed("foo bar\n", "/foo/\ns/\\(/x/\ns//Z/\n,p\nQ\n");
    assert_eq!(out, "foo bar\n?\nZ bar\n");
}

#[test]
fn test_bad_substitute_flags_do_not_replace_remembered_substitute() {
    let (out, _, _) = ed("ab\nab\n", "1s/a/X/\n2s/b/Y/q\n2&\n,p\nQ\n");
    assert_eq!(out, "?\nXb\nXb\n");
}

#[test]
fn test_unmatched_address_re_is_still_remembered() {
    // POSIX: the last RE *encountered*, matched or not.
    let (out, _, _) = ed("a\n", "/zzz/\na\nzzz\n.\n2s//y/\n,p\nQ\n");
    assert_eq!(out, "?\na\ny\n");
}

#[test]
fn test_failed_percent_replacement_does_not_remember_re() {
    // `%` with no earlier replacement fails the `s` before its RE counts.
    let (out, _, _) = ed("foo\nbar\n", "2\ns/bar/%/\n1s//Z/\n,p\nQ\n");
    assert_eq!(out, "bar\n?\n?\nfoo\nbar\n");
}
