//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `!command`, `e !command` and `r !command`: the child gets ed's standard
//! error, its exit status does not decide whether its output is read, and
//! its output is read byte for byte.

use plib::testing::get_binary_path;
use plib::tmp::TempDir;
use std::fs;
use std::io::Write;
use std::process::{Command, Stdio};

/// Run `ed -s f` on a file holding `content`; return (stdout, stderr).
fn ed_streams(content: &str, script: &str) -> (String, String) {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("f"), content).unwrap();
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
        String::from_utf8(out.stderr).unwrap(),
    )
}

#[test]
fn test_read_command_output_despite_nonzero_exit() {
    // `diff` and `grep` exit 1 as a normal answer; their output still counts.
    let (out, _) = ed_streams("a\n", "r !sh -c 'echo x; exit 1'\n,p\nQ\n");
    assert_eq!(out, "a\nx\n");
}

#[test]
fn test_edit_command_output_despite_nonzero_exit() {
    let (out, _) = ed_streams("a\n", "e !sh -c 'echo y; exit 1'\n,p\nQ\n");
    assert_eq!(out, "y\n");
}

#[test]
fn test_read_command_stderr_goes_to_stderr() {
    let (out, err) = ed_streams("a\n", "r !sh -c 'echo err >&2; echo out'\n,p\nQ\n");
    assert_eq!(out, "a\nout\n");
    assert_eq!(err, "err\n");
}

#[test]
fn test_shell_escape_stderr_goes_to_stderr() {
    let (out, err) = ed_streams("a\n", "!ls /nonexistent-ed-test-path\nQ\n");
    assert_eq!(out, "");
    assert!(err.contains("nonexistent-ed-test-path"), "stderr: {err:?}");
}

#[test]
fn test_shell_escape_output_comes_before_later_ed_output() {
    // The child writes to ed's stdout directly, so ed's own buffered output
    // must be flushed before it runs.
    let (out, _) = ed_streams("a\nb\n", "1p\n!echo mid\n2p\nQ\n");
    assert_eq!(out, "a\nmid\nb\n");
}

#[test]
fn test_read_command_output_keeps_cr() {
    let (out, _) = ed_streams("a\n", "r !printf 'b\\r\\n'\n,l\nQ\n");
    assert_eq!(out, "a$\nb\\r$\n");
}

#[test]
fn test_edit_command_output_without_final_newline() {
    let (out, _) = ed_streams("a\n", "e !printf 'x\\ny'\n,p\nQ\n");
    assert_eq!(out, "x\ny\n");
}

#[test]
fn test_edit_command_that_cannot_run_keeps_the_buffer() {
    let (out, _) = ed_streams("a\n", "e !nonexistent-ed-test-command\n,p\nQ\n");
    assert_eq!(out, "?\na\n");
}

#[test]
fn test_write_command_output_comes_after_earlier_ed_output() {
    let (out, _) = ed_streams("a\nb\n", "1p\n2w !cat\nQ\n");
    assert_eq!(out, "a\nb\n");
}

#[test]
fn test_shell_child_gets_default_sigquit() {
    // ed ignores SIGQUIT; the command it runs must not inherit that.
    if !std::path::Path::new("/proc/self/status").exists() {
        return; // the ignored-signal mask is read from Linux's procfs
    }
    let (out, _) = ed_streams(
        "a\n",
        "r !awk '/^SigIgn/{print $2}' /proc/self/status\n2p\nQ\n",
    );
    let mask = u64::from_str_radix(out.trim(), 16).unwrap();
    assert_eq!(
        mask & (1 << (libc::SIGQUIT - 1)),
        0,
        "SIGQUIT ignored in child"
    );
}
