//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Programs ex runs (`!`, `r !`, `[range]!`) and `%`/`#` in file names.

use super::addresses::{ex, ex_env, ok};
use super::argv::run;
use plib::tmp::TempDir;
use std::fs;

#[test]
fn test_read_keeps_output_of_a_failing_program() {
    // 95279-95280: the output goes in whatever the exit status.
    assert_eq!(ok("a\n", "r !echo x; exit 1\n%p\nq!\n"), "a\nx\n");
}

#[test]
fn test_read_takes_standard_error_too() {
    assert_eq!(
        ok("a\n", "r !echo out; echo err >&2\n%p\nq!\n"),
        "a\nout\nerr\n"
    );
}

#[test]
fn test_read_leaves_the_last_line_current_in_ex() {
    // 95287-95289.
    assert_eq!(ok("a\nb\n", "1r !printf 'x\\ny\\n'\n.p\nq!\n"), "y\n");
}

#[test]
fn test_filter_keeps_output_and_stderr_of_a_failing_program() {
    assert_eq!(
        ok("a\nb\n", "1!echo X; echo E >&2; exit 3\n%p\nq!\n"),
        "X\nE\nb\n"
    );
}

#[test]
fn test_filter_saves_lines_to_the_unnamed_buffer() {
    // 95614-95615, in line mode: `put` brings them back as lines.
    assert_eq!(ok("a\nb\n", "1!tr a A\n$put\n%p\nq!\n"), "A\nb\na\n");
}

#[test]
fn test_filter_leaves_the_last_line_read_current() {
    assert_eq!(
        ok("a\nb\nc\n", "1,2!printf 'x\\ny\\nz\\n'\n.p\nq!\n"),
        "z\n"
    );
}

#[test]
fn test_shell_option_is_read_when_the_command_runs() {
    let (out, _, _, _) = ex("a\n", "set shell=/bin/false\nset shell?\n!echo hi\nq!\n");
    assert!(
        out.starts_with("shell=/bin/false\n"),
        "the option took: {out:?}"
    );
    assert!(!out.contains("hi"), "{out:?}");
}

#[test]
fn test_empty_shell_variable_means_sh() {
    let (out, _, _, _) = ex_env("a\n", "!echo hi\nq!\n", &[("SHELL", "")]);
    assert!(out.contains("hi"), "{out:?}");
}

#[test]
fn test_no_blank_line_before_program_output_under_s() {
    assert_eq!(ok("a\n", "!echo hi\nq!\n"), "hi\n");
    assert_eq!(ok("a\n", "w !cat\nq!\n"), "a\n");
}

#[test]
fn test_percent_and_hash_in_file_arguments() {
    // 94666-94669.
    let (_, err, code, _) = ex("a\n", "w %.bak\ne %.bak\nf\nq!\n");
    assert_eq!(code, 0, "{err}");
    let (out, _, _, _) = ex("a\n", "w %.bak\ne %.bak\ne! #\n.p\nq!\n");
    assert_eq!(out, "a\n");
    // Escaped, they are the characters.
    let (out, err, code, _) = ex("a\n", "w \\%x\ne \\%x\n.p\nq!\n");
    assert_eq!((out.as_str(), code), ("a\n", 0), "{err}");
}

#[test]
fn test_hash_without_an_alternate_is_an_error() {
    let (_, _, code, _) = ex("a\n", "w #x\nq!\n");
    assert_ne!(code, 0);
}

#[test]
fn test_file_command_name_is_the_shells_percent() {
    assert_eq!(
        ok("a\n", "f other\n!echo %\nq!\n").lines().last(),
        Some("other")
    );
}

#[test]
fn test_odd_length_sections_option_is_an_error() {
    let (_, _, code, _) = ex("a\n", "set sections=abc\nq!\n");
    assert_ne!(code, 0);
}

/// A directory holding files `a` (`A`) and `b` (`B`).
fn two_files() -> TempDir {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("a"), "A\n").unwrap();
    fs::write(dir.path().join("b"), "B\n").unwrap();
    dir
}

#[test]
fn test_next_makes_the_previous_file_the_alternate() {
    let dir = two_files();
    let (out, err, _) = run(dir.path(), &["-s", "a", "b"], "n\n!echo alt=#\nq\n");
    assert_eq!(out, "alt=a\n", "{err}");
}

#[test]
fn test_edit_again_keeps_the_alternate() {
    // 94359-94364: only a file argument changes the names.
    let dir = two_files();
    let (out, err, _) = run(dir.path(), &["-s", "a"], "e b\ne!\ne #\n.p\nq\n");
    assert_eq!(out, "A\n", "{err}");
}

#[test]
fn test_write_to_a_name_makes_it_the_alternate() {
    // 94372-94377 rule 4b.
    let dir = two_files();
    let (out, err, _) = run(dir.path(), &["-s", "a"], "w x\n!echo #\nq\n");
    assert_eq!(out, "x\n", "{err}");
    // And `:r` of a name, with no current pathname, makes it current.
    // (After the warning that the buffer is modified, 95607-95608.)
    let (out, err, _) = run(dir.path(), &["-s"], "r b\n!echo %\nq!\n");
    assert_eq!(out.lines().last(), Some("b"), "{err}");
}
