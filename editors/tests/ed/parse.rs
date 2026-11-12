//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! ed command-line parsing: print suffixes, trailing text after a command,
//! addresses on commands that take none, and the `s` delimiter rules.

use plib::testing::get_binary_path;
use plib::tmp::TempDir;
use std::fs;
use std::io::Write;
use std::process::{Command, Stdio};

/// Run `ed -s f` on a file holding `content`, with `script` on stdin.
/// Returns (stdout, exit code, the file afterwards).
fn ed(content: &str, script: &str) -> (String, i32, String) {
    let dir = TempDir::new().unwrap();
    let path = dir.path().join("f");
    fs::write(&path, content).unwrap();
    let mut child = Command::new(get_binary_path("ed"))
        .args(["-s", "f"])
        .current_dir(dir.path())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn ed");
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(script.as_bytes())
        .unwrap();
    let out = child.wait_with_output().expect("wait for ed");
    (
        String::from_utf8(out.stdout).unwrap(),
        out.status.code().unwrap_or(-1),
        fs::read_to_string(&path).unwrap(),
    )
}

/// The script's stdout, asserting a clean exit.
fn ok(content: &str, script: &str) -> String {
    let (out, code, _) = ed(content, script);
    assert_eq!(code, 0, "ed failed; stdout: {out:?}");
    out
}

#[test]
fn test_unterminated_replacement_prints() {
    // POSIX: `s/s1/s2` is `s/s1/s2/p`.
    assert_eq!(ok("a\n", "s/a/b\nQ\n"), "b\n");
}

#[test]
fn test_text_after_substitute_flags_is_an_error() {
    let (out, code, _) = ed("a\n", "s/a/b/g/x\n,p\nQ\n");
    assert_eq!(out, "?\na\n");
    assert_ne!(code, 0);
}

#[test]
fn test_substitute_count_zero_is_an_error() {
    let (out, code, _) = ed("a\n", "s/a/b/0\n,p\nQ\n");
    assert_eq!(out, "?\na\n");
    assert_ne!(code, 0);
}

#[test]
fn test_no_address_commands_reject_addresses() {
    for cmd in ["5u", "1h", "1H", "1P", "1!true", ",q", "1Q", "2f", "1e"] {
        let (out, code, _) = ed("a\nb\nc\nd\ne\n", &format!("{cmd}\nQ\n"));
        assert!(out.starts_with('?'), "{cmd} was accepted: {out:?}");
        assert_ne!(code, 0, "{cmd}");
    }
}

#[test]
fn test_change_semicolon_range_from_last_line() {
    // `1;+1` is lines 1-2 whatever the current line is; resolving `+1` from
    // `$` rejected it and the replacement text ran as commands.
    assert_eq!(ok("a\nb\nc\n", "1;+1c\nY\n.\n,p\nQ\n"), "Y\nc\n");
}

#[test]
fn test_junk_after_command_is_an_error() {
    for cmd in ["1dx", "1ix", "1pq", "uu", "1kab", "jx", "1zq"] {
        let (out, code, file) = ed("a\nb\nc\n", &format!("{cmd}\nw\nq\n"));
        assert!(out.starts_with('?'), "{cmd} was accepted: {out:?}");
        assert_eq!(file, "a\nb\nc\n", "{cmd} changed the buffer");
        assert_ne!(code, 0, "{cmd}");
    }
}

#[test]
fn test_filename_commands_need_a_blank() {
    // `fromstdin` is not `f romstdin`.
    let (out, code, _) = ed("a\n", "fromstdin\nf\nQ\n");
    assert_eq!(out, "?\nf\n");
    assert_ne!(code, 0);
}

#[test]
fn test_print_suffixes() {
    assert_eq!(ok("a\nb\nc\n", "1dp\nQ\n"), "b\n");
    assert_eq!(ok("a\nb\nc\n", "1pn\nQ\n"), "1\ta\n");
    assert_eq!(ok("a\tb\n", "1nl\nQ\n"), "1\ta\\tb$\n");
    assert_eq!(ok("a\nb\n", "1,2jp\nQ\n"), "ab\n");
    assert_eq!(ok("a\nb\nc\n", "1m$n\nQ\n"), "3\ta\n");
    assert_eq!(ok("a\nb\n", "2t0p\nQ\n"), "b\n");
    assert_eq!(ok("a\nb\n", "2kap\n'ap\nQ\n"), "b\nb\n");
    assert_eq!(ok("a\nb\n", "$ap\nz\n.\nQ\n"), "z\n");
    assert_eq!(ok("a\nb\n", "1ip\nz\n.\nQ\n"), "z\n");
    assert_eq!(ok("a\nb\n", "1cn\nz\n.\nQ\n"), "1\tz\n");
    // `u` restores the current line too: `$`, before the `1d`.
    assert_eq!(ok("a\nb\n", "1d\nup\nQ\n"), "b\n");
    assert_eq!(ok("a\nb\nc\n", "1z2p\nQ\n"), "a\nb\nb\n");
}

#[test]
fn test_global_unterminated_substitute_prints_each_line() {
    // The last line of a command list has nothing to continue onto, so the
    // unterminated `s` is `s/a/b/p` on every matched line.
    assert_eq!(ok("a\nx\na\n", "g/a/s/a/b\nQ\n"), "b\nb\n");
}

#[test]
fn test_escaped_delimiter_is_literal() {
    // POSIX: an escaped delimiter is a literal character, "losing any
    // special meaning" -- even when the delimiter is special to the RE or the
    // replacement.
    let (out, _, _) = ed("axb\n", "s.a\\.b.X.\n,p\nQ\n");
    assert_eq!(out, "?\naxb\n", "`\\.` matched any character");
    assert_eq!(ok("abc\n", "s&a&[\\&]&\n,p\nQ\n"), "[&]bc\n");
    assert_eq!(ok("a.b\n", "s.a\\.b.X.\n,p\nQ\n"), "X\n");
    assert_eq!(ok("a/b\n", "s/a\\/b/X/\n,p\nQ\n"), "X\n");
    assert_eq!(ok("a.b\naxb\n", "g.a\\.b.p\nQ\n"), "a.b\n");
}

#[test]
fn test_backslash_is_not_a_delimiter() {
    let (out, code, _) = ed("b\n", "s\\b\\X\n,p\nQ\n");
    assert_eq!(out, "?\nb\n");
    assert_ne!(code, 0);
}

#[test]
fn test_blank_before_suffix_is_accepted_everywhere() {
    assert_eq!(ok("a\nb\n", "2m0 p\nQ\n"), "b\n");
    assert_eq!(ok("a\nb\nc\n", "1z2 p\nQ\n"), "a\nb\nb\n");
    assert_eq!(ok("a\nb\n", "2ka p\nQ\n"), "b\n");
    assert_eq!(ok("a\nb\nc\n", "1d p\nQ\n"), "b\n");
}

#[test]
fn test_global_keeps_trailing_blank_of_unterminated_replacement() {
    assert_eq!(ok("a\n", "g/a/s/a/b \n,l\nQ\n"), "b \nb $\n");
}

#[test]
fn test_list_folds_long_lines() {
    let line = "x".repeat(80);
    let out = ok(&format!("{line}\n"), "l\nQ\n");
    assert_eq!(out, format!("{}\\\n{}$\n", "x".repeat(72), "x".repeat(8)));
}
