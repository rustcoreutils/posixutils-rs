//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::fs;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};

static TEST_COUNTER: AtomicU64 = AtomicU64::new(0);

fn setup_test_dir(name: &str) -> PathBuf {
    let id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let test_dir =
        PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("patch_test_{}_{}", name, id));
    let _ = fs::remove_dir_all(&test_dir);
    fs::create_dir_all(&test_dir).expect("Failed to create test directory");
    test_dir
}

fn cleanup_test_dir(test_dir: &PathBuf) {
    let _ = fs::remove_dir_all(test_dir);
}

// Test applying a unified diff
#[test]
fn test_patch_unified_simple() {
    let test_dir = setup_test_dir("unified_simple");

    let original = test_dir.join("test.txt");
    fs::write(&original, "line 1\nline 2\nline 3\nline 4\nline 5\n").unwrap();

    let patch_file = test_dir.join("test.patch");
    fs::write(
        &patch_file,
        "--- test.txt\n+++ test.txt\n@@ -1,5 +1,5 @@\n line 1\n-line 2\n+line 2 modified\n line 3\n line 4\n line 5\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            original.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&original).unwrap();
    assert_eq!(content, "line 1\nline 2 modified\nline 3\nline 4\nline 5\n");

    cleanup_test_dir(&test_dir);
}

// Test applying a unified diff with multiple hunks
#[test]
fn test_patch_unified_multiple_hunks() {
    let test_dir = setup_test_dir("multiple_hunks");

    let original = test_dir.join("multi.txt");
    fs::write(
        &original,
        "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline 9\nline 10\n",
    )
    .unwrap();

    let patch_file = test_dir.join("multi.patch");
    fs::write(
        &patch_file,
        "--- multi.txt\n+++ multi.txt\n@@ -1,3 +1,3 @@\n-line 1\n+LINE 1\n line 2\n line 3\n@@ -8,3 +8,3 @@\n line 8\n-line 9\n+LINE 9\n line 10\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            original.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&original).unwrap();
    assert_eq!(
        content,
        "LINE 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nLINE 9\nline 10\n"
    );

    cleanup_test_dir(&test_dir);
}

// Test backup option (-b)
#[test]
fn test_patch_backup() {
    let test_dir = setup_test_dir("backup");

    let original = test_dir.join("backup_test.txt");
    fs::write(&original, "original content\n").unwrap();

    let patch_file = test_dir.join("backup.patch");
    fs::write(
        &patch_file,
        "--- backup_test.txt\n+++ backup_test.txt\n@@ -1 +1 @@\n-original content\n+modified content\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-b"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            original.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&original).unwrap();
    assert_eq!(content, "modified content\n");

    let backup = test_dir.join("backup_test.txt.orig");
    assert!(backup.exists(), "Backup file should exist");
    let backup_content = fs::read_to_string(&backup).unwrap();
    assert_eq!(backup_content, "original content\n");

    cleanup_test_dir(&test_dir);
}

// Test -p option (strip path components)
#[test]
fn test_patch_strip_path() {
    let test_dir = setup_test_dir("strip_path");

    let original = test_dir.join("strip.txt");
    fs::write(&original, "content\n").unwrap();

    let patch_file = test_dir.join("strip.patch");
    fs::write(
        &patch_file,
        "--- a/b/c/strip.txt\n+++ a/b/c/strip.txt\n@@ -1 +1 @@\n-content\n+new content\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            original.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&original).unwrap();
    assert_eq!(content, "new content\n");

    cleanup_test_dir(&test_dir);
}

// Test reverse patch (-R)
#[test]
fn test_patch_reverse() {
    let test_dir = setup_test_dir("reverse");

    let original = test_dir.join("reverse.txt");
    fs::write(&original, "line 2 modified\n").unwrap();

    let patch_file = test_dir.join("reverse.patch");
    fs::write(
        &patch_file,
        "--- reverse.txt\n+++ reverse.txt\n@@ -1 +1 @@\n-line 2\n+line 2 modified\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-R"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            original.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&original).unwrap();
    assert_eq!(content, "line 2\n");

    cleanup_test_dir(&test_dir);
}

// Test output to different file (-o)
#[test]
fn test_patch_output_file() {
    let test_dir = setup_test_dir("output_file");

    let original = test_dir.join("original.txt");
    fs::write(&original, "original\n").unwrap();

    let output = test_dir.join("output.txt");

    let patch_file = test_dir.join("output.patch");
    fs::write(
        &patch_file,
        "--- original.txt\n+++ original.txt\n@@ -1 +1 @@\n-original\n+modified\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-o"),
            output.to_str().unwrap().to_string(),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            original.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let orig_content = fs::read_to_string(&original).unwrap();
    assert_eq!(orig_content, "original\n");

    let out_content = fs::read_to_string(&output).unwrap();
    assert_eq!(out_content, "modified\n");

    cleanup_test_dir(&test_dir);
}

// Test adding lines
#[test]
fn test_patch_add_lines() {
    let test_dir = setup_test_dir("add_lines");

    let original = test_dir.join("add.txt");
    fs::write(&original, "line 1\nline 3\n").unwrap();

    let patch_file = test_dir.join("add.patch");
    fs::write(
        &patch_file,
        "--- add.txt\n+++ add.txt\n@@ -1,2 +1,3 @@\n line 1\n+line 2\n line 3\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            original.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&original).unwrap();
    assert_eq!(content, "line 1\nline 2\nline 3\n");

    cleanup_test_dir(&test_dir);
}

// Test deleting lines
#[test]
fn test_patch_delete_lines() {
    let test_dir = setup_test_dir("delete_lines");

    let original = test_dir.join("delete.txt");
    fs::write(&original, "line 1\nline 2\nline 3\n").unwrap();

    let patch_file = test_dir.join("delete.patch");
    fs::write(
        &patch_file,
        "--- delete.txt\n+++ delete.txt\n@@ -1,3 +1,2 @@\n line 1\n-line 2\n line 3\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            original.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&original).unwrap();
    assert_eq!(content, "line 1\nline 3\n");

    cleanup_test_dir(&test_dir);
}

// Test normal diff format
#[test]
fn test_patch_normal_diff() {
    let test_dir = setup_test_dir("normal_diff");

    let original = test_dir.join("normal.txt");
    fs::write(&original, "line 1\nline 2\nline 3\n").unwrap();

    let patch_file = test_dir.join("normal.patch");
    fs::write(&patch_file, "2c2\n< line 2\n---\n> LINE 2\n").unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-n"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            original.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&original).unwrap();
    assert_eq!(content, "line 1\nLINE 2\nline 3\n");

    cleanup_test_dir(&test_dir);
}

// Test patch from stdin
#[test]
fn test_patch_stdin() {
    let test_dir = setup_test_dir("stdin");

    let original = test_dir.join("stdin.txt");
    fs::write(&original, "hello\n").unwrap();

    let patch_content = "--- stdin.txt\n+++ stdin.txt\n@@ -1 +1 @@\n-hello\n+goodbye\n";

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![original.to_str().unwrap().to_string()],
        stdin_data: String::from(patch_content),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&original).unwrap();
    assert_eq!(content, "goodbye\n");

    cleanup_test_dir(&test_dir);
}

// Test invalid argument combinations
#[test]
fn test_patch_invalid_args() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("patch"),
            args: vec![String::from("-c"), String::from("-u")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 2,
        },
        |_, output| {
            assert_eq!(output.status.code(), Some(2));
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("-c") || stderr.contains("-u") || stderr.contains("one of"),
                "Error should mention conflicting options"
            );
        },
    );
}

// Test empty patch (no changes)
#[test]
fn test_patch_empty() {
    let test_dir = setup_test_dir("empty");

    let original = test_dir.join("empty.txt");
    fs::write(&original, "content\n").unwrap();

    let patch_file = test_dir.join("empty.patch");
    fs::write(&patch_file, "").unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            original.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&original).unwrap();
    assert_eq!(content, "content\n");

    cleanup_test_dir(&test_dir);
}
