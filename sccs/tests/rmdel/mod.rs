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

/// Build an s-file with `n` deltas (1.1 .. 1.n) in `tmp`.
fn create_multi_delta(tmp: &TempDir, name: &str, n: usize) -> std::path::PathBuf {
    let sfile = create_sccs_file(tmp, name, "v1\n");
    for i in 2..=n {
        let out = run("get", &["-e", &format!("s.{name}")], tmp.path(), "");
        assert!(
            out.status.success(),
            "get -e failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        std::fs::write(tmp.path().join(name), format!("v{i}\n")).unwrap();
        let out = run(
            "delta",
            &["-y", "next", &format!("s.{name}")],
            tmp.path(),
            "",
        );
        assert!(
            out.status.success(),
            "delta failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
    sfile
}

/// The delta-table type letters, newest first: `('1.3', 'R')`, ...
fn delta_types(sfile: &Path) -> Vec<(String, char)> {
    let data = std::fs::read(sfile).unwrap();
    let text = String::from_utf8_lossy(&data);
    text.lines()
        .filter_map(|l| l.strip_prefix("\u{1}d "))
        .filter_map(|rest| {
            let mut f = rest.split_whitespace();
            let ty = f.next()?.chars().next()?;
            let sid = f.next()?.to_string();
            Some((sid, ty))
        })
        .collect()
}

/// #R1: removing a leaf marks its delta-table entry `R` rather than deleting
/// the entry, and the rewritten file must still validate — the checksum is
/// recomputed over the reweave.
///
/// Cross-checked against CSSC 1.4.1: its `val` accepts our post-`rmdel` s-file
/// and its `prs -e` reports the same surviving deltas.
#[test]
fn rmdel_marks_removed_delta_r_and_file_stays_valid() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_multi_delta(&tmp, "multi", 3);

    assert_eq!(
        delta_types(&sfile),
        vec![
            ("1.3".to_string(), 'D'),
            ("1.2".to_string(), 'D'),
            ("1.1".to_string(), 'D'),
        ],
        "three deltas, all D, before removal"
    );

    let out = run("rmdel", &["-r", "1.3", "s.multi"], tmp.path(), "");
    assert!(
        out.status.success(),
        "rmdel of a leaf must succeed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    assert_eq!(
        delta_types(&sfile),
        vec![
            ("1.3".to_string(), 'R'),
            ("1.2".to_string(), 'D'),
            ("1.1".to_string(), 'D'),
        ],
        "the removed delta is marked R, not deleted"
    );

    // Our own val must accept the rewritten file (checksum recomputed).
    let out = run("val", &["s.multi"], tmp.path(), "");
    assert_eq!(
        out.status.code(),
        Some(0),
        "the rewritten s-file must still validate: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    // And prs must no longer list the removed delta.
    let out = run("prs", &["-e", "-d:I:", "s.multi"], tmp.path(), "");
    let listed = String::from_utf8_lossy(&out.stdout);
    assert!(
        !listed.contains("1.3"),
        "removed delta still listed: {listed:?}"
    );
    assert!(listed.contains("1.2") && listed.contains("1.1"));
}

/// Only a leaf may be removed: a delta with successors, including the initial
/// delta of a multi-delta file, must be refused with a diagnostic and a
/// non-zero exit — and the file must be left untouched.
#[test]
fn rmdel_refuses_non_leaf_and_initial_deltas() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_multi_delta(&tmp, "chain", 3);
    let before = std::fs::read(&sfile).unwrap();

    for sid in ["1.2", "1.1"] {
        let out = run("rmdel", &["-r", sid, "s.chain"], tmp.path(), "");
        assert_eq!(
            out.status.code(),
            Some(1),
            "rmdel -r {sid} must fail (not a leaf)"
        );
        assert!(
            !out.stderr.is_empty(),
            "a diagnostic is required for -r {sid}"
        );
    }

    assert_eq!(
        std::fs::read(&sfile).unwrap(),
        before,
        "a refused rmdel must not modify the s-file"
    );
}

/// A delta that is checked out for editing cannot be removed, and an already
/// removed one cannot be removed twice.
#[test]
fn rmdel_refuses_edited_and_already_removed_deltas() {
    let tmp = TempDir::new().unwrap();
    create_multi_delta(&tmp, "locked", 2);

    // Remove the leaf once: succeeds.
    let out = run("rmdel", &["-r", "1.2", "s.locked"], tmp.path(), "");
    assert!(out.status.success());

    // Twice: refused.
    let out = run("rmdel", &["-r", "1.2", "s.locked"], tmp.path(), "");
    assert_eq!(out.status.code(), Some(1), "already-removed must fail");

    // A SID that does not exist at all is also an error.
    let out = run("rmdel", &["-r", "9.9", "s.locked"], tmp.path(), "");
    assert_eq!(out.status.code(), Some(1), "unknown SID must fail");

    // A delta pending in the p-file cannot be removed.
    let tmp2 = TempDir::new().unwrap();
    create_sccs_file(&tmp2, "pend", "a\n");
    let out = run("get", &["-e", "s.pend"], tmp2.path(), "");
    assert!(out.status.success());
    let out = run("rmdel", &["-r", "1.2", "s.pend"], tmp2.path(), "");
    assert_eq!(
        out.status.code(),
        Some(1),
        "a delta being edited must not be removable: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Directory and `-` (stdin list) operands both expand, and an error on an
/// expanded operand still sets the exit status.
#[test]
fn rmdel_directory_and_stdin_operands() {
    // Directory: a removable leaf inside it is removed.
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path().join("proj");
    std::fs::create_dir(&dir).unwrap();

    let sfile = create_multi_delta(&tmp, "indir", 2);
    std::fs::rename(&sfile, dir.join("s.indir")).unwrap();

    let out = run("rmdel", &["-r", "1.2", "proj"], tmp.path(), "");
    assert!(
        out.status.success(),
        "rmdel <dir> should succeed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        delta_types(&dir.join("s.indir"))
            .iter()
            .find(|(sid, _)| sid == "1.2")
            .map(|(_, t)| *t),
        Some('R'),
        "the delta inside the directory should be marked R"
    );

    // An error on a directory-expanded operand must still be reported.
    let out = run("rmdel", &["-r", "1.1", "proj"], tmp.path(), "");
    assert_eq!(
        out.status.code(),
        Some(1),
        "an error inside a directory operand must set the exit status"
    );

    // `-` reads the pathname list from stdin.
    let tmp2 = TempDir::new().unwrap();
    create_multi_delta(&tmp2, "viastdin", 2);
    let out = run("rmdel", &["-r", "1.2", "-"], tmp2.path(), "s.viastdin\n");
    assert!(
        out.status.success(),
        "rmdel - should succeed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        delta_types(&tmp2.path().join("s.viastdin"))
            .iter()
            .find(|(sid, _)| sid == "1.2")
            .map(|(_, t)| *t),
        Some('R')
    );
}
