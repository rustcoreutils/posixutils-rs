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
        .env("TMPDIR", dir)
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
fn test_ex_read_shell_output_folds_crlf() {
    // 95283-95284: a <carriage-return><newline> pair in program output is
    // one <newline>; a lone <carriage-return> is text.  (A file `:r` reads
    // keeps its CRs: see above.)
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"x\n").unwrap();
    assert_eq!(
        ex_on(dir.path(), "f", "r !printf 'a\\r\\nb\\rc\\n'\nwq\n"),
        b"x\na\nb\rc\n"
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

/// `ex -s` in `dir` with raw `args` and a raw `script`: (stdout, status).
fn ex_raw(dir: &Path, args: &[&std::ffi::OsStr], script: &[u8]) -> (Vec<u8>, i32) {
    let mut child = Command::new(get_binary_path("ex"))
        .arg("-s")
        .args(args)
        .current_dir(dir)
        .env("TMPDIR", dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn ex");
    child.stdin.as_mut().unwrap().write_all(script).unwrap();
    let out = child.wait_with_output().expect("wait for ex");
    (out.stdout, out.status.code().unwrap_or(-1))
}

/// `ex -s f` on a file holding `content`: (stdout, status, file after).
fn ex_bytes(content: &[u8], script: &[u8]) -> (Vec<u8>, i32, Vec<u8>) {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), content).unwrap();
    let (out, code) = ex_raw(dir.path(), &[std::ffi::OsStr::new("f")], script);
    (out, code, fs::read(dir.path().join("f")).unwrap())
}

#[test]
fn test_ex_roundtrip_non_utf8() {
    let content = b"a\xffb\n\xfe\xed\n\xc3\n";
    let (_, code, file) = ex_bytes(content, b"wq\n");
    assert_eq!((code, file.as_slice()), (0, &content[..]));
}

#[test]
fn test_ex_roundtrip_reserved_private_use_characters() {
    // The characters the escape scheme reserves, genuinely in a file.
    let content = "x\u{10FF80}\u{10FFFF}y\n".as_bytes();
    let (_, _, file) = ex_bytes(content, b"wq\n");
    assert_eq!(file, content);
}

#[test]
fn test_ex_print_and_list_non_utf8() {
    let (out, _, _) = ex_bytes(b"a\xffb\n", b"p\nl\nq\n");
    assert_eq!(out, b"a\xffb\na\\377b$\n");
}

#[test]
fn test_ex_substitute_non_utf8_pattern() {
    let (_, code, file) = ex_bytes(b"a\xffb\n", b"s/\xff/\xfe\xfe/\nwq\n");
    assert_eq!((code, file.as_slice()), (0, &b"a\xfe\xfeb\n"[..]));
}

#[test]
fn test_ex_read_and_filter_non_utf8() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"\xf0\n").unwrap();
    fs::write(dir.path().join("g"), b"\xff\n").unwrap();
    ex_raw(
        dir.path(),
        &[std::ffi::OsStr::new("f")],
        b"r g\nr !printf '\\376\\n'\n1,$!cat\nwq\n",
    );
    assert_eq!(
        fs::read(dir.path().join("f")).unwrap(),
        b"\xf0\n\xff\n\xfe\n"
    );
}

#[test]
fn test_ex_non_utf8_pathname_argument() {
    use std::os::unix::ffi::OsStrExt;
    let dir = TempDir::new().unwrap();
    let name = std::ffi::OsStr::from_bytes(b"n\xffm");
    fs::write(dir.path().join(name), b"old\n").unwrap();
    let (_, code) = ex_raw(dir.path(), &[name], b"s/old/new/\nwq\n");
    assert_eq!(code, 0);
    assert_eq!(fs::read(dir.path().join(name)).unwrap(), b"new\n");
}

#[test]
fn test_ex_write_to_non_utf8_pathname() {
    use std::os::unix::ffi::OsStrExt;
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), b"t\n").unwrap();
    ex_raw(dir.path(), &[std::ffi::OsStr::new("f")], b"w o\xffut\nq\n");
    let out = dir.path().join(std::ffi::OsStr::from_bytes(b"o\xffut"));
    assert_eq!(fs::read(out).unwrap(), b"t\n");
}

#[test]
fn test_ex_byte_locale_regex_over_bytes_and_multibyte() {
    // In a byte locale `.` is one byte: it must match each byte that is not
    // UTF-8 whole, and never panic inside a multibyte character.
    let run = |content: &[u8], script: &[u8]| {
        let dir = TempDir::new().unwrap();
        fs::write(dir.path().join("f"), content).unwrap();
        let mut child = Command::new(get_binary_path("ex"))
            .args(["-s", "f"])
            .env("LC_ALL", "C")
            .current_dir(dir.path())
            .env("TMPDIR", dir.path())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();
        child.stdin.as_mut().unwrap().write_all(script).unwrap();
        let out = child.wait_with_output().unwrap();
        assert!(
            out.status.success(),
            "ex: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        fs::read(dir.path().join("f")).unwrap()
    };
    assert_eq!(run(b"a\xffb\n", b"s/./X/g\nwq\n"), b"XXX\n");
    assert_eq!(run(b"a\xffb\n", b"s/[\xff]/X/\nwq\n"), b"aXb\n");
    assert_eq!(run(b"a\xffb\n", b"s/[^a]/X/g\nwq\n"), b"aXX\n");
    // `é` is two bytes in this locale; no match may split it into a panic.
    let out = run("caf\u{e9}\n".as_bytes(), b"s/./X/g\nwq\n");
    assert!(out.starts_with(b"XXX"), "{out:?}");
}

#[test]
fn test_ex_non_utf8_current_pathname_is_kept() {
    // `:e!`, `:r`, `:n` and `%` all name the file being edited, bytes and all.
    use std::os::unix::ffi::OsStrExt;
    let dir = TempDir::new().unwrap();
    let a = std::ffi::OsStr::from_bytes(b"a\xff");
    let b = std::ffi::OsStr::from_bytes(b"b\xfe");
    fs::write(dir.path().join(a), b"one\n").unwrap();
    fs::write(dir.path().join(b), b"two\n").unwrap();
    let (out, code) = ex_raw(dir.path(), &[a, b], b"e!\np\nr\n$p\nn!\np\n!cat %\nq\n");
    assert_eq!(code, 0, "{}", String::from_utf8_lossy(&out));
    assert_eq!(out, b"one\none\ntwo\ntwo\n");
}
