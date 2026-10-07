//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The GNU options dpkg-source passes to patch.
//!
//! Unpacking a "3.0 (quilt)" source package runs, for each patch,
//!   patch -t -F 0 -N -p1 -u -V never -E -b -B .pc/NAME/ --reject-file=-
//! and a "1.0" package's .diff.gz runs
//!   patch -t -F 0 -N -p1 -u -V never -b -z .dpkg-orig
//! Expected results are GNU patch 2.7.6's.

use super::{cleanup_test_dir, setup_test_dir};
use plib::testing::{run_test_with_checker, TestPlan};
use std::fs;
use std::path::Path;

/// Run patch in `dir` (through -d) reading the patch from stdin; return the
/// exit code and stderr.
fn run_in(dir: &Path, args: &[&str], patch: &str) -> (i32, String) {
    let mut full = vec![String::from("-d"), dir.to_str().unwrap().to_string()];
    full.extend(args.iter().map(|s| s.to_string()));
    let mut code = 0;
    let mut err = String::new();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("patch"),
            args: full,
            stdin_data: patch.to_string(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            code = output.status.code().unwrap_or(-1);
            err = String::from_utf8_lossy(&output.stderr).to_string();
        },
    );
    (code, err)
}

fn quilt_args(name: &str) -> Vec<String> {
    [
        "-t",
        "-F",
        "0",
        "-N",
        "-p1",
        "-u",
        "-V",
        "never",
        "-E",
        "-b",
        "-B",
        &format!(".pc/{}/", name),
        "--reject-file=-",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect()
}

fn read(dir: &Path, rel: &str) -> String {
    fs::read_to_string(dir.join(rel)).unwrap_or_else(|e| panic!("{}: {}", rel, e))
}

const MODIFY_PATCH: &str = "--- a/sub/m.txt\n+++ b/sub/m.txt\n@@ -1,3 +1,3 @@\n a\n-b\n+B\n c\n";

// The whole quilt push: a modification, a creation in a directory that does
// not exist yet, and a deletion. Each file's original goes under the -B
// prefix, a created file's as an empty placeholder (quilt and dpkg-source
// restore a file by deleting it when its backup is empty).
#[test]
fn test_patch_dpkg_quilt_push() {
    let dir = setup_test_dir("dpkg_quilt_push");
    fs::create_dir_all(dir.join("sub")).unwrap();
    fs::write(dir.join("sub/m.txt"), "a\nb\nc\n").unwrap();
    fs::write(dir.join("gone.txt"), "x\n").unwrap();
    let patch = format!(
        "{}{}{}",
        MODIFY_PATCH,
        "--- /dev/null\n+++ b/new/dir/n.txt\n@@ -0,0 +1 @@\n+n\n",
        "--- a/gone.txt\n+++ /dev/null\n@@ -1 +0,0 @@\n-x\n"
    );
    let args = quilt_args("p1");
    let args: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let (code, err) = run_in(&dir, &args, &patch);
    assert_eq!(code, 0, "stderr: {}", err);

    assert_eq!(read(&dir, "sub/m.txt"), "a\nB\nc\n");
    assert_eq!(read(&dir, "new/dir/n.txt"), "n\n");
    assert!(!dir.join("gone.txt").exists(), "deleted file must be gone");

    assert_eq!(read(&dir, ".pc/p1/sub/m.txt"), "a\nb\nc\n");
    assert_eq!(read(&dir, ".pc/p1/gone.txt"), "x\n");
    assert_eq!(
        read(&dir, ".pc/p1/new/dir/n.txt"),
        "",
        "placeholder is empty"
    );
    assert!(
        !dir.join("sub/m.txt.orig").exists(),
        "-B replaces the suffix"
    );

    cleanup_test_dir(&dir);
}

// -F 0 allows no fuzz: a hunk whose outer context does not match is rejected,
// and --reject-file=- discards the rejects instead of writing a .rej file.
#[test]
fn test_patch_dpkg_fuzz_zero_rejects_to_nowhere() {
    let dir = setup_test_dir("dpkg_fuzz_zero");
    fs::create_dir_all(dir.join("sub")).unwrap();
    fs::write(dir.join("sub/m.txt"), "a\nb\nc\nd\n").unwrap();
    let patch = "--- a/sub/m.txt\n+++ b/sub/m.txt\n@@ -1,4 +1,4 @@\n X\n b\n-c\n+C\n d\n";
    let args = quilt_args("p2");
    let args: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let (code, err) = run_in(&dir, &args, patch);
    assert_eq!(code, 1, "stderr: {}", err);
    assert_eq!(read(&dir, "sub/m.txt"), "a\nb\nc\nd\n");
    assert!(!dir.join("sub/m.txt.rej").exists(), "rejects are discarded");
    assert!(!dir.join("-").exists(), "'-' is not a file name here");

    cleanup_test_dir(&dir);
}

// -F 1 allows one line of fuzz.
#[test]
fn test_patch_fuzz_option_one() {
    let dir = setup_test_dir("fuzz_one");
    fs::write(dir.join("m.txt"), "a\nb\nc\nd\n").unwrap();
    let patch = "--- a/m.txt\n+++ b/m.txt\n@@ -1,4 +1,4 @@\n X\n b\n-c\n+C\n d\n";
    let (code, err) = run_in(&dir, &["-F", "1", "-p1"], patch);
    assert_eq!(code, 0, "stderr: {}", err);
    assert_eq!(read(&dir, "m.txt"), "a\nb\nC\nd\n");
    cleanup_test_dir(&dir);
}

// The 1.0 (.diff.gz) invocation: -z names the backup suffix. dpkg-source
// unlinks FILE.dpkg-orig for every file the diff touches, so a file the diff
// creates needs one too.
#[test]
fn test_patch_dpkg_v1_suffix_backups() {
    let dir = setup_test_dir("dpkg_v1");
    fs::create_dir_all(dir.join("sub")).unwrap();
    fs::write(dir.join("sub/m.txt"), "a\nb\nc\n").unwrap();
    let patch = format!(
        "{}{}",
        MODIFY_PATCH, "--- a/debian/rules\n+++ b/debian/rules\n@@ -0,0 +1 @@\n+r\n"
    );
    let (code, err) = run_in(
        &dir,
        &[
            "-t",
            "-F",
            "0",
            "-N",
            "-p1",
            "-u",
            "-V",
            "never",
            "-b",
            "-z",
            ".dpkg-orig",
        ],
        &patch,
    );
    assert_eq!(code, 0, "stderr: {}", err);
    assert_eq!(read(&dir, "sub/m.txt"), "a\nB\nc\n");
    assert_eq!(read(&dir, "sub/m.txt.dpkg-orig"), "a\nb\nc\n");
    assert_eq!(read(&dir, "debian/rules"), "r\n");
    assert_eq!(read(&dir, "debian/rules.dpkg-orig"), "");
    cleanup_test_dir(&dir);
}

// -E removes a file the patch leaves empty, even without /dev/null.
#[test]
fn test_patch_remove_empty_files() {
    let dir = setup_test_dir("remove_empty");
    fs::create_dir_all(dir.join("k")).unwrap();
    fs::write(dir.join("k/e.txt"), "a\n").unwrap();
    fs::write(dir.join("k/keep"), "z\n").unwrap();
    let patch = "--- a/k/e.txt\n+++ b/k/e.txt\n@@ -1 +0,0 @@\n-a\n";
    let (code, err) = run_in(&dir, &["-p1", "-E", "-b"], patch);
    assert_eq!(code, 0, "stderr: {}", err);
    assert!(!dir.join("k/e.txt").exists(), "emptied file is removed");
    assert_eq!(read(&dir, "k/e.txt.orig"), "a\n");
    cleanup_test_dir(&dir);
}

// Without -E an emptied file stays, empty.
#[test]
fn test_patch_keeps_empty_file_without_e() {
    let dir = setup_test_dir("keep_empty");
    fs::write(dir.join("e.txt"), "a\n").unwrap();
    let patch = "--- a/e.txt\n+++ b/e.txt\n@@ -1 +0,0 @@\n-a\n";
    let (code, err) = run_in(&dir, &["-p1"], patch);
    assert_eq!(code, 0, "stderr: {}", err);
    assert_eq!(read(&dir, "e.txt"), "");
    cleanup_test_dir(&dir);
}

// -B alone and -z alone each ask for a backup; together the name is
// PREFIX + FILE + SUFFIX.
#[test]
fn test_patch_backup_prefix_and_suffix() {
    let patch = "--- a/m.txt\n+++ b/m.txt\n@@ -1,2 +1,2 @@\n a\n-b\n+B\n";
    for (args, backup) in [
        (vec!["-p1", "-B", "pre/"], "pre/m.txt"),
        (vec!["-p1", "-z", ".zz"], "m.txt.zz"),
        (
            vec!["-p1", "-b", "-B", "pre/", "-z", ".suf"],
            "pre/m.txt.suf",
        ),
        (vec!["-p1", "-b", "-V", "simple"], "m.txt.orig"),
    ] {
        let dir = setup_test_dir("backup_names");
        fs::write(dir.join("m.txt"), "a\nb\n").unwrap();
        let (code, err) = run_in(&dir, &args, patch);
        assert_eq!(code, 0, "{:?} stderr: {}", args, err);
        assert_eq!(read(&dir, "m.txt"), "a\nB\n");
        assert_eq!(read(&dir, backup), "a\nb\n", "{:?}", args);
        cleanup_test_dir(&dir);
    }
}

// Only the simple backup method exists here; any other -V is refused rather
// than silently given simple backups.
#[test]
fn test_patch_version_control_other_than_simple_refused() {
    let dir = setup_test_dir("vc_refused");
    fs::write(dir.join("m.txt"), "a\nb\n").unwrap();
    let patch = "--- a/m.txt\n+++ b/m.txt\n@@ -1,2 +1,2 @@\n a\n-b\n+B\n";
    let (code, err) = run_in(&dir, &["-p1", "-b", "-V", "numbered"], patch);
    assert_eq!(code, 2, "stderr: {}", err);
    assert!(err.contains("numbered"), "stderr: {}", err);
    assert_eq!(read(&dir, "m.txt"), "a\nb\n");
    cleanup_test_dir(&dir);
}

// -t never asks; a patch that looks reversed is taken as reversed (GNU's
// batch answer), so applying it again undoes it.
#[test]
fn test_patch_batch_assumes_reversed() {
    let dir = setup_test_dir("batch_reversed");
    fs::write(dir.join("m.txt"), "a\nB\nc\n").unwrap();
    let patch = "--- a/m.txt\n+++ b/m.txt\n@@ -1,3 +1,3 @@\n a\n-b\n+B\n c\n";
    let (code, err) = run_in(&dir, &["-t", "-p1"], patch);
    assert_eq!(code, 0, "stderr: {}", err);
    assert_eq!(read(&dir, "m.txt"), "a\nb\nc\n");
    cleanup_test_dir(&dir);
}

// --reject-file=FILE is the long form of -r.
#[test]
fn test_patch_reject_file_long_form() {
    let dir = setup_test_dir("reject_long");
    fs::write(dir.join("m.txt"), "q\n").unwrap();
    let patch = "--- a/m.txt\n+++ b/m.txt\n@@ -1,2 +1,2 @@\n a\n-b\n+B\n";
    let (code, err) = run_in(&dir, &["-p1", "--reject-file=R.rej"], patch);
    assert_eq!(code, 1, "stderr: {}", err);
    assert!(read(&dir, "R.rej").contains("+ B"));
    assert!(!dir.join("m.txt.rej").exists());
    cleanup_test_dir(&dir);
}

// Removing a file also removes the directories it leaves empty, up to (not
// including) the working directory, as GNU patch does. Debian glibc's patches
// delete the only file in advisories/, and GNU leaves no advisories/ behind.
#[test]
fn test_patch_removal_prunes_empty_directories() {
    let dir = setup_test_dir("prune_dirs");
    fs::create_dir_all(dir.join("d1/d2")).unwrap();
    fs::write(dir.join("d1/d2/f.txt"), "x\n").unwrap();
    fs::create_dir_all(dir.join("k")).unwrap();
    fs::write(dir.join("k/e.txt"), "a\n").unwrap();
    fs::write(dir.join("k/keep"), "z\n").unwrap();
    let patch = format!(
        "{}{}",
        "--- a/d1/d2/f.txt\n+++ /dev/null\n@@ -1 +0,0 @@\n-x\n",
        "--- a/k/e.txt\n+++ b/k/e.txt\n@@ -1 +0,0 @@\n-a\n"
    );
    let args = quilt_args("p9");
    let args: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let (code, err) = run_in(&dir, &args, &patch);
    assert_eq!(code, 0, "stderr: {}", err);
    assert!(!dir.join("d1").exists(), "d1/d2 and d1 are left empty");
    assert!(!dir.join("k/e.txt").exists());
    assert!(dir.join("k/keep").exists(), "k still holds a file");
    assert_eq!(read(&dir, ".pc/p9/d1/d2/f.txt"), "x\n");
    assert!(dir.exists(), "the working directory itself stays");
    cleanup_test_dir(&dir);
}
