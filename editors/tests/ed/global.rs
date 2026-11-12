//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! g/v/G/V: each pass acts on the line that was marked, wherever earlier
//! passes moved it, and skips a marked line an earlier pass deleted.

use super::parse::ok;

#[test]
fn test_global_move_to_end() {
    assert_eq!(ok("a1\nb\na2\nb\n", "g/a/m$\n,p\nQ\n"), "b\nb\na1\na2\n");
    assert_eq!(ok("x1\nx2\ny\n", "g/x/m$\n,p\nQ\n"), "y\nx1\nx2\n");
}

#[test]
fn test_global_move_to_start() {
    assert_eq!(ok("b\na1\nb\na2\n", "g/a/m0\n,p\nQ\n"), "a2\na1\nb\nb\n");
}

#[test]
fn test_global_skips_line_deleted_by_earlier_pass() {
    assert_eq!(ok("a\na\nb\n", "g/a/+1d\n,p\nQ\n"), "a\nb\n");
}

#[test]
fn test_global_range_delete() {
    assert_eq!(ok("x\ny\nx\ny\nz\n", "g/x/.,+1d\n,p\nQ\n"), "z\n");
}

#[test]
fn test_global_copies_are_not_marked() {
    assert_eq!(ok("a\nb\n", "g/a/t$\n,p\nQ\n"), "a\nb\na\n");
}

#[test]
fn test_global_delete_previous_line() {
    assert_eq!(ok("p\na\nq\na\n", "g/a/-1d\n,p\nQ\n"), "a\na\n");
}

#[test]
fn test_v_move_to_end() {
    assert_eq!(ok("k1\nx\nk2\n", "v/x/m0\n,p\nQ\n"), "k2\nk1\nx\n");
}

#[test]
fn test_interactive_global_follows_moved_lines() {
    // Each prompt shows the marked line it will act on.
    assert_eq!(
        ok("x1\nx2\ny\n", "G/x/\nm$\nm$\n,p\nQ\n"),
        "x1\nx2\ny\nx1\nx2\n"
    );
}

#[test]
fn test_interactive_global_skips_deleted_line() {
    assert_eq!(ok("a\na\nb\n", "G/a/\n+1d\n,p\nQ\n"), "a\na\nb\n");
}

#[test]
fn test_global_sets_current_line_to_last_command() {
    assert_eq!(ok("a\nb\na\nc\n", "g/a/\n.=\nQ\n"), "a\na\n3\n");
}

#[test]
fn test_interactive_global_null_command_sets_current_line() {
    assert_eq!(ok("a\nb\na\nc\n", "G/a/\n\n\n.=\nQ\n"), "a\na\n3\n");
}

#[test]
fn test_interactive_global_ampersand_is_per_invocation() {
    // The `&` in the second G has nothing of its own to repeat.
    let (out, code, _) = super::parse::ed("a\nb\n", "G/a/\ns/a/X/\nG/b/\n&\n,p\nQ\n");
    assert_eq!(out, "a\nb\n?\nX\nb\n");
    assert_ne!(code, 0);
}

#[test]
fn test_interactive_global_failed_command_ends_it() {
    // After the failure the empty line is an ed command again: the null
    // command, which prints the next line.
    let (out, _, _) = super::parse::ed("a\nb\na\nb\n", "G/b/\nzz\n\nQ\n");
    assert_eq!(out, "b\n?\na\n");
}

#[test]
fn test_write_empty_buffer_empties_the_file() {
    let (out, code, file) = super::parse::ed("a\nb\n", "1,2d\nw\nq\n");
    assert_eq!((out.as_str(), code, file.as_str()), ("", 0, ""));
}
