//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::common::run_test_with_checker;
use plib::testing::TestPlan;
use std::process::Output;
use tempfile::TempDir;

fn create_sccs_file(tmp: &TempDir, name: &str, content: &str) -> std::path::PathBuf {
    let sfile = tmp.path().join(format!("s.{}", name));

    run_test_with_checker(
        TestPlan {
            cmd: String::from("admin"),
            args: vec![sfile.to_string_lossy().into(), "-i".into()],
            stdin_data: String::from(content),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "admin should succeed");
        },
    );

    sfile
}

fn get_for_editing(sfile: &std::path::Path) {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("get"),
            args: vec!["-e".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "get -e should succeed");
        },
    );
}

#[test]
fn unget_removes_pfile_and_gfile() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "test", "content\n");
    let pfile = tmp.path().join("p.test");
    let gfile = tmp.path().join("test");

    // Get for editing
    get_for_editing(&sfile);
    assert!(pfile.exists(), "p-file should exist");
    assert!(gfile.exists(), "g-file should exist");

    // Unget
    run_test_with_checker(
        TestPlan {
            cmd: String::from("unget"),
            args: vec![sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "unget should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(stdout.contains("1.2"), "should print the SID");
        },
    );

    // Both files should be removed
    assert!(!pfile.exists(), "p-file should be removed");
    assert!(!gfile.exists(), "g-file should be removed");
}

#[test]
fn unget_keep_gfile() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "keep", "content\n");
    let pfile = tmp.path().join("p.keep");
    let gfile = tmp.path().join("keep");

    // Get for editing
    get_for_editing(&sfile);
    assert!(gfile.exists(), "g-file should exist");

    // Unget with -n (keep g-file)
    run_test_with_checker(
        TestPlan {
            cmd: String::from("unget"),
            args: vec!["-n".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "unget -n should succeed");
        },
    );

    // p-file removed but g-file kept
    assert!(!pfile.exists(), "p-file should be removed");
    assert!(gfile.exists(), "g-file should be kept with -n");
}

#[test]
fn unget_no_pending_edit() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "noedit", "content\n");

    // Unget without pending edit should fail
    run_test_with_checker(
        TestPlan {
            cmd: String::from("unget"),
            args: vec![sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(
                !output.status.success(),
                "unget should fail without pending edit"
            );
        },
    );
}

#[test]
fn unget_silent_mode() {
    let tmp = TempDir::new().unwrap();
    let sfile = create_sccs_file(&tmp, "silent", "content\n");

    get_for_editing(&sfile);

    // Unget with -s (silent)
    run_test_with_checker(
        TestPlan {
            cmd: String::from("unget"),
            args: vec!["-s".into(), sfile.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "unget -s should succeed");
            assert!(
                output.stdout.is_empty(),
                "silent mode should produce no output"
            );
        },
    );
}

#[test]
fn unget_multifile_pathname_header() {
    // When more than one file is named, each SID is preceded by a "\n%s:\n"
    // pathname header (POSIX STDOUT format).
    let tmp = TempDir::new().unwrap();
    let s1 = create_sccs_file(&tmp, "one", "content\n");
    let s2 = create_sccs_file(&tmp, "two", "content\n");
    get_for_editing(&s1);
    get_for_editing(&s2);

    run_test_with_checker(
        TestPlan {
            cmd: String::from("unget"),
            args: vec![s1.to_string_lossy().into(), s2.to_string_lossy().into()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan: &TestPlan, output: &Output| {
            assert!(output.status.success(), "multi-file unget should succeed");
            let out = String::from_utf8_lossy(&output.stdout);
            // Each SID line is preceded by a blank line + "<path>:".
            assert!(
                out.contains(&format!("\n{}:\n", s1.display())),
                "missing header for first file: {out:?}"
            );
            assert!(
                out.contains(&format!("\n{}:\n", s2.display())),
                "missing header for second file: {out:?}"
            );
        },
    );
}

fn sccs_run(cmd: &str, args: &[&str], cwd: &std::path::Path, stdin: &str) -> Output {
    use std::io::Write;
    use std::process::{Command, Stdio};

    let mut child = Command::new(plib::testing::get_binary_path(cmd))
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| panic!("spawn {cmd}: {e}"));
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin.as_bytes())
        .unwrap();
    child.wait_with_output().expect("wait")
}

/// Multiple operands are each prefixed with the `"\n%s:\n"` pathname header,
/// and each reports the SID whose edit was cancelled. The existing tests all
/// use a single operand.
#[test]
fn unget_multi_file_uses_the_pathname_header() {
    let tmp = TempDir::new().unwrap();
    let a = create_sccs_file(&tmp, "m1", "a\n");
    let b = create_sccs_file(&tmp, "m2", "b\n");
    get_for_editing(&a);
    get_for_editing(&b);

    let out = sccs_run(
        "unget",
        &[a.to_str().unwrap(), b.to_str().unwrap()],
        tmp.path(),
        "",
    );
    assert!(out.status.success(), "unget of two files should succeed");

    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains(&format!("\n{}:\n", a.display())),
        "missing header for the first operand: {stdout:?}"
    );
    assert!(
        stdout.contains(&format!("\n{}:\n", b.display())),
        "missing header for the second operand: {stdout:?}"
    );

    // Both p-files are gone.
    for f in [&a, &b] {
        let p = f.with_file_name(format!(
            "p.{}",
            f.file_name()
                .unwrap()
                .to_string_lossy()
                .strip_prefix("s.")
                .unwrap()
        ));
        assert!(!p.exists(), "p-file should be removed: {p:?}");
    }
}

/// A directory operand expands to the SCCS files inside it.
#[test]
fn unget_directory_operand_expands() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path().join("proj");
    std::fs::create_dir(&dir).unwrap();

    let src = create_sccs_file(&tmp, "dirmod", "x\n");
    get_for_editing(&src);
    std::fs::rename(&src, dir.join("s.dirmod")).unwrap();
    std::fs::rename(src.with_file_name("p.dirmod"), dir.join("p.dirmod")).unwrap();

    let out = sccs_run("unget", &[dir.to_str().unwrap()], tmp.path(), "");
    assert!(
        out.status.success(),
        "unget <dir> should succeed: {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        !dir.join("p.dirmod").exists(),
        "the p-file inside the directory should be removed"
    );
}

/// A lone `-` operand reads the pathname list from standard input.
#[test]
fn unget_dash_operand_reads_pathnames_from_stdin() {
    let tmp = TempDir::new().unwrap();
    let a = create_sccs_file(&tmp, "s1", "a\n");
    let b = create_sccs_file(&tmp, "s2", "b\n");
    get_for_editing(&a);
    get_for_editing(&b);

    let list = format!("{}\n{}\n", a.display(), b.display());
    let out = sccs_run("unget", &["-"], tmp.path(), &list);
    assert!(
        out.status.success(),
        "unget - should succeed: {:?}",
        String::from_utf8_lossy(&out.stderr)
    );

    assert!(!a.with_file_name("p.s1").exists());
    assert!(!b.with_file_name("p.s2").exists());
}

/// `-r` selects which of several pending edits to cancel, leaving the others
/// in place. Two concurrent edits need both the `b` (branch) and `j` (joint
/// edit) flags: without `b`, `get -e -b` is required to be ignored
/// (spec 99079-99081), so there is only ever one entry to pick from.
#[test]
fn unget_r_selects_one_of_several_pending_edits() {
    let tmp = TempDir::new().unwrap();
    let s = create_sccs_file(&tmp, "multi", "a\n");

    let out = sccs_run(
        "admin",
        &["-fb", "-fj", s.to_str().unwrap()],
        tmp.path(),
        "",
    );
    assert!(out.status.success(), "admin -fb -fj should succeed");

    get_for_editing(&s);
    let out = sccs_run("get", &["-e", "-b", s.to_str().unwrap()], tmp.path(), "");
    assert!(out.status.success(), "get -e -b should branch");

    let pfile = s.with_file_name("p.multi");
    let before = std::fs::read_to_string(&pfile).unwrap();
    assert_eq!(
        before.lines().count(),
        2,
        "expected two pending edits, got {before:?}"
    );
    assert!(before.contains(" 1.2 "), "trunk edit present: {before:?}");
    assert!(
        before.contains(" 1.1.1.1 "),
        "branch edit present: {before:?}"
    );

    // Cancel only the branch edit.
    let out = sccs_run(
        "unget",
        &["-r", "1.1.1.1", s.to_str().unwrap()],
        tmp.path(),
        "",
    );
    assert!(
        out.status.success(),
        "unget -r 1.1.1.1 failed: {:?}",
        String::from_utf8_lossy(&out.stderr)
    );

    let after = std::fs::read_to_string(&pfile).unwrap();
    assert_eq!(
        after.lines().count(),
        1,
        "one edit should remain: {after:?}"
    );
    assert!(
        after.contains(" 1.2 "),
        "the trunk edit must survive: {after:?}"
    );
    assert!(
        !after.contains(" 1.1.1.1 "),
        "the branch edit must be gone: {after:?}"
    );
}

/// An `-r` SID that is not checked out is an error, and must leave every
/// pending edit untouched.
#[test]
fn unget_r_with_unknown_sid_is_an_error() {
    let tmp = TempDir::new().unwrap();
    let s = create_sccs_file(&tmp, "one", "a\n");
    get_for_editing(&s);

    let pfile = s.with_file_name("p.one");
    let before = std::fs::read_to_string(&pfile).unwrap();

    let out = sccs_run("unget", &["-r", "9.9", s.to_str().unwrap()], tmp.path(), "");
    assert_eq!(out.status.code(), Some(1), "unknown SID must fail");
    assert!(!out.stderr.is_empty(), "a diagnostic is required");
    assert_eq!(
        std::fs::read_to_string(&pfile).unwrap(),
        before,
        "a failed unget must not modify the p-file"
    );
}
