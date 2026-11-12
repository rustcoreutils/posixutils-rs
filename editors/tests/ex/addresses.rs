//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! ex addresses and errors: a search address is an address like any
//! other, a lone address prints its line, and a command that fails is an
//! error a script stops on.

use plib::testing::get_binary_path;
use plib::tmp::TempDir;
use std::fs;
use std::io::Write;
use std::process::{Command, Stdio};

/// `ex -s f` on a file holding `content`, `script` on stdin:
/// (stdout, stderr, exit code, the file afterwards).
pub(super) fn ex(content: &str, script: &str) -> (String, String, i32, String) {
    ex_env(content, script, &[])
}

/// `ex`, with extra environment variables.
pub(super) fn ex_env(
    content: &str,
    script: &str,
    env: &[(&str, &str)],
) -> (String, String, i32, String) {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), content).unwrap();
    let mut child = Command::new(get_binary_path("ex"))
        .args(["-s", "f"])
        .current_dir(dir.path())
        .env("HOME", dir.path())
        .env_remove("EXINIT")
        .envs(env.iter().copied())
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
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
        fs::read_to_string(dir.path().join("f")).unwrap(),
    )
}

/// The stdout of a script that must succeed.
pub(super) fn ok(content: &str, script: &str) -> String {
    let (out, err, code, _) = ex(content, script);
    assert_eq!(code, 0, "ex failed: {err}");
    out
}

const ABCD: &str = "a\nb\nc\nd\n";

#[test]
fn test_command_after_search_address_runs() {
    assert_eq!(ok(ABCD, "/b/d\n%p\nq!\n"), "a\nc\nd\n");
    assert_eq!(ok(ABCD, "/b/,/c/d\n%p\nq!\n"), "a\nd\n");
    assert_eq!(ok(ABCD, "$\n?b?s/b/X/\n%p\nq!\n"), "d\na\nX\nc\nd\n");
    assert_eq!(ok(ABCD, "/c/-1p\nq!\n"), "b\n");
}

#[test]
fn test_lone_search_address_moves_and_prints() {
    assert_eq!(ok(ABCD, "/c/\n.=\nq!\n"), "c\n3\n");
}

#[test]
fn test_lone_address_prints_its_line() {
    // POSIX 94561: the implied command is print.
    assert_eq!(ok(ABCD, "2\nq!\n"), "b\n");
    assert_eq!(ok(ABCD, "$\nq!\n"), "d\n");
    // The implied print is of every addressed line.
    assert_eq!(ok(ABCD, "1,3\nq!\n"), "a\nb\nc\n");
}

#[test]
fn test_empty_line_prints_next_line() {
    // POSIX 94566-94569: no address and no command is `.+1p`.
    assert_eq!(ok(ABCD, "1\n\n\nq!\n"), "a\nb\nc\n");
}

#[test]
fn test_failed_substitute_is_an_error_and_stops_the_script() {
    // POSIX 95361: no match "shall be an error"; with stdin not a terminal
    // the script stops before `w` (96009-96011).
    let (_, _, code, file) = ex("a\nb\n", "s/zzz/y/\n1d\nw\nq\n");
    assert_ne!(code, 0);
    assert_eq!(file, "a\nb\n");
}

#[test]
fn test_repeat_substitute_with_none_is_an_error() {
    let (_, _, code, file) = ex("a\n", "&\nw\nq\n");
    assert_ne!(code, 0);
    assert_eq!(file, "a\n");
}

#[test]
fn test_address_offset_overflow_is_an_error() {
    let (out, _, code, _) = ex(ABCD, ".+18446744073709551615p\nq!\n");
    assert_ne!(code, 0);
    assert_eq!(out, "");
}

#[test]
fn test_error_with_piped_stdin_stops_even_without_s() {
    // POSIX 96009-96011 is keyed to standard input not being a terminal,
    // not to -s: the `w` after the failed command must not run.
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), "a\nb\n").unwrap();
    let mut child = Command::new(get_binary_path("ex"))
        .arg("f")
        .current_dir(dir.path())
        .env("HOME", dir.path())
        .env_remove("EXINIT")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(b"1d\nnosuchcommand\nw\nq\n")
        .unwrap();
    let out = child.wait_with_output().unwrap();
    assert_ne!(out.status.code(), Some(0));
    assert_eq!(fs::read_to_string(dir.path().join("f")).unwrap(), "a\nb\n");
}

#[test]
fn test_global_with_no_commands_prints_each_line() {
    // POSIX 95011-95012, and 94561: the implied command is print.
    assert_eq!(ok("a\nxb\nc\nxd\n", "g/x/\nq!\n"), "xb\nxd\n");
}

#[test]
fn test_search_address_honours_nowrapscan() {
    let (_, err, code, _) = ex(ABCD, "set nowrapscan\n$\n/a/p\nq!\n");
    assert_ne!(code, 0, "wrapped to line 1 despite nowrapscan");
    assert!(err.contains("not found"), "{err}");
    assert_eq!(ok(ABCD, "$\n/a/p\nq!\n"), "d\na\n");
}

#[test]
fn test_empty_search_address_with_no_previous_re() {
    let (_, err, code, _) = ex(ABCD, "//p\nq!\n");
    assert_ne!(code, 0);
    assert!(err.contains("No previous search"), "{err}");
}

#[test]
fn test_destination_search_address_is_the_last_re() {
    // After `t` the copy is current; `2s//X/` then uses the destination RE.
    assert_eq!(ok(ABCD, "1t/b/\n2s//X/\n%p\nq!\n"), "a\nX\na\nc\nd\n");
}
