//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! ed edits bytes: text, commands, patterns and pathnames that are not
//! UTF-8 pass through unchanged, and a last line without a newline gets one.

use plib::testing::get_binary_path;
use plib::tmp::TempDir;
use std::ffi::OsStr;
use std::fs;
use std::io::Write;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::process::{Command, Stdio};

/// Run ed with `args` in `dir`, `script` on stdin; return (stdout, exit code).
fn run(dir: &Path, args: &[&OsStr], script: &[u8]) -> (Vec<u8>, i32) {
    run_env(dir, args, script, &[])
}

/// `run`, with extra environment variables.
fn run_env(dir: &Path, args: &[&OsStr], script: &[u8], env: &[(&str, &str)]) -> (Vec<u8>, i32) {
    let mut child = Command::new(get_binary_path("ed"))
        .args(args)
        .envs(env.iter().copied())
        .current_dir(dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn ed");
    child.stdin.as_mut().unwrap().write_all(script).unwrap();
    let out = child.wait_with_output().expect("wait for ed");
    (out.stdout, out.status.code().unwrap_or(-1))
}

/// `ed -s f` on a file holding `content`; (stdout, exit code, file after).
fn ed(content: &[u8], script: &[u8]) -> (Vec<u8>, i32, Vec<u8>) {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), content).unwrap();
    let (out, code) = run(dir.path(), &[OsStr::new("-s"), OsStr::new("f")], script);
    (out, code, fs::read(dir.path().join("f")).unwrap())
}

#[test]
fn test_non_utf8_file_roundtrips() {
    let content = b"a\xffb\n\xfe\xed\n";
    let (out, code, file) = ed(content, b"w\nq\n");
    assert_eq!((out.as_slice(), code), (&b""[..], 0));
    assert_eq!(file, content);
}

#[test]
fn test_non_utf8_file_lists_as_octal() {
    let (out, _, _) = ed(b"a\xffb\n", b",l\nQ\n");
    assert_eq!(out, b"a\\377b$\n");
}

#[test]
fn test_non_utf8_pattern_and_replacement() {
    let (_, code, file) = ed(b"a\xffb\n", b"s/\xff/\xfe\xfe/\nw\nq\n");
    assert_eq!(code, 0);
    assert_eq!(file, b"a\xfe\xfeb\n");
}

#[test]
fn test_non_utf8_global_and_address() {
    let (_, _, file) = ed(b"x\n\xff1\ny\n\xff2\n", b"g/\xff/d\nw\nq\n");
    assert_eq!(file, b"x\ny\n");
    let (out, _, _) = ed(b"x\n\xff1\n", b"/\xff/p\nQ\n");
    assert_eq!(out, b"\xff1\n");
}

#[test]
fn test_non_utf8_input_text() {
    let (_, _, file) = ed(b"a\n", b"a\n\xfe\x80\n.\nw\nq\n");
    assert_eq!(file, b"a\n\xfe\x80\n");
}

#[test]
fn test_non_utf8_read_file_and_command_output() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"a\n").unwrap();
    fs::write(dir.path().join("g"), b"\xff\n").unwrap();
    run(
        dir.path(),
        &[OsStr::new("-s"), OsStr::new("f")],
        b"r g\nr !printf '\\376\\n'\nw\nq\n",
    );
    assert_eq!(fs::read(dir.path().join("f")).unwrap(), b"a\n\xff\n\xfe\n");
}

#[test]
fn test_non_utf8_pathname() {
    let dir = TempDir::new().unwrap();
    let name = OsStr::from_bytes(b"n\xffm");
    fs::write(dir.path().join(name), b"old\n").unwrap();
    run(
        dir.path(),
        &[OsStr::new("-s")],
        b"e n\xffm\ns/old/new/\nw\nf\nq\n",
    );
    assert_eq!(fs::read(dir.path().join(name)).unwrap(), b"new\n");
}

#[test]
fn test_missing_final_newline_is_supplied() {
    // Text appended after the last line must not be glued onto it.
    let (_, _, file) = ed(b"a", b"a\nb\n.\nw\nq\n");
    assert_eq!(file, b"a\nb\n");
    let (_, _, file) = ed(b"x\ny", b"$m0\nw\nq\n");
    assert_eq!(file, b"y\nx\n");
}

#[test]
fn test_byte_count_is_the_file_size() {
    // The count `e` prints is what the file holds, newline supplied or not.
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"ab").unwrap();
    let (out, _) = run(dir.path(), &[OsStr::new("f")], b"Q\n");
    assert_eq!(out, b"2\n");
    fs::write(dir.path().join("g"), b"\xff\n").unwrap();
    let (out, _) = run(dir.path(), &[OsStr::new("g")], b"Q\n");
    assert_eq!(out, b"2\n");
}

#[test]
fn test_empty_matches_step_one_character() {
    let (_, _, file) = ed(b"abc\n", b"s/x*/-/g\nw\nq\n");
    assert_eq!(file, b"-a-b-c-\n");
    let (_, _, file) = ed(b"aXX\n", b"s/\\(\\)/[\\1]/g\nw\nq\n");
    assert_eq!(file, b"[]a[]X[]X[]\n");
    let (_, _, file) = ed(b"abc\n", b"s/x*/-/2\nw\nq\n");
    assert_eq!(file, b"a-bc\n");
}

#[test]
fn test_empty_match_after_a_match_is_not_another() {
    let (_, _, file) = ed(b"ab\n", b"s/b*$/X/g\nw\nq\n");
    assert_eq!(file, b"aX\n");
}

#[test]
fn test_multibyte_delimiter() {
    // A delimiter is one character of the locale's encoding.
    let utf8 = if cfg!(target_os = "macos") {
        "en_US.UTF-8"
    } else {
        "C.UTF-8"
    };
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"abc\nd\n").unwrap();
    let (out, code) = run_env(
        dir.path(),
        &[OsStr::new("-s"), OsStr::new("f")],
        "g§b§p\ns§b§X§\nw\nq\n".as_bytes(),
        &[("LC_ALL", utf8)],
    );
    assert_eq!((out.as_slice(), code), (&b"abc\n"[..], 0));
    assert_eq!(fs::read(dir.path().join("f")).unwrap(), b"aXc\nd\n");
}

#[test]
fn test_nul_bytes_roundtrip() {
    // POSIX text files hold no NUL, so no RE is promised to match one, but
    // a line holding one must still come back unchanged.
    let content = b"a\0b\nc\n";
    let (_, code, file) = ed(content, b"$s/c/d/\nw\nq\n");
    assert_eq!((code, file.as_slice()), (0, &b"a\0b\nd\n"[..]));
}

#[test]
fn test_global_remembers_re_even_when_list_is_bad() {
    let (out, _, _) = ed(b"foo\nbar\n", b"g/foo/x\n//p\nQ\n");
    assert_eq!(out, b"?\nfoo\n");
}
