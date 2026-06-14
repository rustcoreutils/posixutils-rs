//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::get_binary_path;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::process::{Command, Stdio};
use tempfile::TempDir;

/// Run a built SCCS binary with the given args and cwd, feeding `stdin`.
fn run(cmd: &str, args: &[&str], cwd: &Path, stdin: &str) -> std::process::Output {
    use std::io::Write;
    let bin = get_binary_path(cmd);
    let mut child = Command::new(bin)
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin.as_bytes())
        .unwrap();
    child.wait_with_output().expect("wait")
}

/// Create an s-file (s.NAME) with a single delta 1.1 from `content`.
/// `admin -i` (with no value) reads the initial body from standard input.
fn create_sccs_file(tmp: &TempDir, name: &str, content: &str) -> std::path::PathBuf {
    let sname = format!("s.{}", name);
    let out = run("admin", &["-i", &sname], tmp.path(), content);
    assert!(
        out.status.success(),
        "admin failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    tmp.path().join(&sname)
}

#[test]
fn rmdel_removes_leaf_delta() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "f.txt", "alpha\nbeta\n");

    // Remove the only (leaf) delta 1.1.
    let out = run("rmdel", &["-r1.1", "s.f.txt"], tmp.path(), "");
    assert!(
        out.status.success(),
        "rmdel should succeed (author removing own leaf delta): {}",
        String::from_utf8_lossy(&out.stderr)
    );

    // The s-file must still exist.
    assert!(sfile.exists(), "s-file should still exist after rmdel");
}

#[test]
fn rmdel_preserves_readonly_mode() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "g.txt", "one\ntwo\nthree\n");

    let out = run("rmdel", &["-r1.1", "s.g.txt"], tmp.path(), "");
    assert!(
        out.status.success(),
        "rmdel should succeed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    // After rewrite, the s-file must remain read-only r--r--r-- (0o444).
    let mode = std::fs::metadata(&sfile).unwrap().permissions().mode() & 0o777;
    assert_eq!(
        mode, 0o444,
        "rewritten s-file should be r--r--r-- (0o444), got {:o}",
        mode
    );
}

#[test]
fn rmdel_unknown_sid_fails() {
    let tmp = TempDir::new().unwrap();
    create_sccs_file(&tmp, "h.txt", "data\n");

    // SID 9.9 does not exist -> non-zero exit, diagnostic, s-file untouched.
    let out = run("rmdel", &["-r9.9", "s.h.txt"], tmp.path(), "");
    assert!(
        !out.status.success(),
        "rmdel should fail for a non-existent SID"
    );
}

#[test]
fn rmdel_not_an_sccs_file_fails() {
    let tmp = TempDir::new().unwrap();
    std::fs::write(tmp.path().join("plain.txt"), "hello\n").unwrap();

    let out = run("rmdel", &["-r1.1", "plain.txt"], tmp.path(), "");
    assert!(
        !out.status.success(),
        "rmdel should fail for a non-SCCS file"
    );
}
