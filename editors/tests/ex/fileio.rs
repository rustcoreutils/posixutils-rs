//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! ex file round-trips: reading a file and writing it back must not change
//! the file's bytes.

use plib::testing::get_binary_path;
use plib::tmp::TempDir;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

/// Run `ex -s file` in `dir` with `script` on stdin; return ex's stdout.
fn ex_stdout(dir: &Path, name: &str, script: &str) -> String {
    let mut child = Command::new(get_binary_path("ex"))
        .args(["-s", name])
        .current_dir(dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn ex");
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(script.as_bytes())
        .unwrap();
    let out = child.wait_with_output().expect("wait for ex");
    assert!(
        out.status.success(),
        "ex failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8(out.stdout).unwrap()
}

/// Run `ex -s file` in `dir` with `script` on stdin; return the file's bytes
/// afterwards.
fn ex_on(dir: &Path, name: &str, script: &str) -> Vec<u8> {
    ex_stdout(dir, name, script);
    fs::read(dir.join(name)).unwrap()
}

/// Write `content` to a file, open it in ex, `wq`, and return the result.
fn roundtrip(content: &[u8]) -> Vec<u8> {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), content).unwrap();
    ex_on(dir.path(), "f", "wq\n")
}

#[test]
fn test_ex_roundtrip_leading_blank_lines() {
    assert_eq!(roundtrip(b"\n\na\n"), b"\n\na\n");
}

#[test]
fn test_ex_roundtrip_trailing_blank_line() {
    assert_eq!(roundtrip(b"a\n\n"), b"a\n\n");
}

#[test]
fn test_ex_roundtrip_all_blank_lines() {
    assert_eq!(roundtrip(b"\n\n\n"), b"\n\n\n");
}

#[test]
fn test_ex_roundtrip_crlf() {
    assert_eq!(roundtrip(b"a\r\nb\r\n"), b"a\r\nb\r\n");
}

#[test]
fn test_ex_read_keeps_cr_and_blank_lines() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"x\n").unwrap();
    fs::write(dir.path().join("g"), b"c\r\n\n").unwrap();
    assert_eq!(ex_on(dir.path(), "f", "r g\nwq\n"), b"x\nc\r\n\n");
}

#[test]
fn test_ex_yank_put_keeps_cr() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"a\r\nb\n").unwrap();
    assert_eq!(ex_on(dir.path(), "f", "1y\n$put\nwq\n"), b"a\r\nb\na\r\n");
}

#[test]
fn test_ex_read_shell_output_keeps_cr() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"x\n").unwrap();
    assert_eq!(
        ex_on(dir.path(), "f", "r !printf 'a\\r\\n'\nwq\n"),
        b"x\na\r\n"
    );
}

#[test]
fn test_ex_delete_undo_keeps_cr() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"a\r\nb\n").unwrap();
    assert_eq!(ex_on(dir.path(), "f", "1d\nu\nwq\n"), b"a\r\nb\n");
}

#[test]
fn test_ex_filter_undo_restores_lines() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"x\n").unwrap();
    assert_eq!(
        ex_on(dir.path(), "f", "1,1!printf 'p\\nq\\n'\nu\nwq\n"),
        b"x\n"
    );
}

#[test]
fn test_ex_filter_undo_sort() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"c\nb\na\n").unwrap();
    assert_eq!(ex_on(dir.path(), "f", "1,3!sort\nu\nwq\n"), b"c\nb\na\n");
}

#[test]
fn test_ex_read_empty_shell_output_then_undo() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"a\nb\n").unwrap();
    assert_eq!(ex_on(dir.path(), "f", "$d\n1r !true\nu\nwq\n"), b"a\nb\n");
}

#[test]
fn test_ex_execute_register_keeps_quoted_cr() {
    // `:@a` runs register a's lines as ex commands, ending each the way a
    // sourced line ends: the `^V`-quoted `\r` of `map Q :wq^V^M` is kept.
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"map Q :wq\x16\r\nx\n").unwrap();
    let out = ex_stdout(dir.path(), "f", "1y a\n1d\n@a\nmap\nq!\n");
    assert!(out.trim_end().ends_with(":wq^M"), "map listing: {out:?}");
}

#[test]
fn test_ex_read_file_is_undoable() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"a\n").unwrap();
    fs::write(dir.path().join("g"), b"b\nc\n").unwrap();
    assert_eq!(ex_on(dir.path(), "f", "r g\nu\nwq\n"), b"a\n");
}
