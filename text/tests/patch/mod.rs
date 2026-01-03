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

// Test context diff format (-c)
#[test]
fn test_patch_context_simple() {
    let test_dir = setup_test_dir("context_simple");

    let original = test_dir.join("context.txt");
    fs::write(&original, "line 1\nline 2\nline 3\nline 4\nline 5\n").unwrap();

    let patch_file = test_dir.join("context.patch");
    // Context diff format
    fs::write(
        &patch_file,
        "*** context.txt\t2024-01-01 00:00:00
--- context.txt\t2024-01-01 00:00:01
***************
*** 1,5 ****
  line 1
! line 2
  line 3
  line 4
  line 5
--- 1,5 ----
  line 1
! line 2 modified
  line 3
  line 4
  line 5
",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-c"),
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

// Test context diff with add and delete
#[test]
fn test_patch_context_add_delete() {
    let test_dir = setup_test_dir("context_add_delete");

    let original = test_dir.join("ctx_ad.txt");
    fs::write(&original, "alpha\nbeta\ngamma\n").unwrap();

    let patch_file = test_dir.join("ctx_ad.patch");
    fs::write(
        &patch_file,
        "*** ctx_ad.txt\t2024-01-01 00:00:00
--- ctx_ad.txt\t2024-01-01 00:00:01
***************
*** 1,3 ****
  alpha
- beta
  gamma
--- 1,3 ----
  alpha
+ delta
  gamma
",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-c"),
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
    assert_eq!(content, "alpha\ndelta\ngamma\n");

    cleanup_test_dir(&test_dir);
}

// Test ed script format (-e)
#[test]
fn test_patch_ed_simple() {
    let test_dir = setup_test_dir("ed_simple");

    let original = test_dir.join("ed.txt");
    fs::write(&original, "line 1\nline 2\nline 3\n").unwrap();

    let patch_file = test_dir.join("ed.patch");
    // Ed script: change line 2
    fs::write(&patch_file, "2c\nLINE 2\n.\n").unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-e"),
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

// Test ed script with multiple commands
#[test]
fn test_patch_ed_multiple_commands() {
    let test_dir = setup_test_dir("ed_multiple");

    let original = test_dir.join("ed_multi.txt");
    fs::write(&original, "one\ntwo\nthree\nfour\nfive\n").unwrap();

    let patch_file = test_dir.join("ed_multi.patch");
    // Ed script: delete line 4, add after line 2 (in reverse order as ed expects)
    fs::write(&patch_file, "4d\n2a\ninserted\n.\n").unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-e"),
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
    assert_eq!(content, "one\ntwo\ninserted\nthree\nfive\n");

    cleanup_test_dir(&test_dir);
}

// Test directory option (-d)
#[test]
fn test_patch_directory() {
    let test_dir = setup_test_dir("directory");
    let subdir = test_dir.join("subdir");
    fs::create_dir_all(&subdir).unwrap();

    let original = subdir.join("dir_test.txt");
    fs::write(&original, "original\n").unwrap();

    let patch_file = test_dir.join("dir.patch");
    fs::write(
        &patch_file,
        "--- dir_test.txt\n+++ dir_test.txt\n@@ -1 +1 @@\n-original\n+changed\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-d"),
            subdir.to_str().unwrap().to_string(),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            String::from("dir_test.txt"),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&original).unwrap();
    assert_eq!(content, "changed\n");

    cleanup_test_dir(&test_dir);
}

// Test ifdef directive (-D)
#[test]
fn test_patch_ifdef() {
    let test_dir = setup_test_dir("ifdef");

    let original = test_dir.join("ifdef.txt");
    fs::write(&original, "before\nold line\nafter\n").unwrap();

    let patch_file = test_dir.join("ifdef.patch");
    fs::write(
        &patch_file,
        "--- ifdef.txt\n+++ ifdef.txt\n@@ -1,3 +1,3 @@\n before\n-old line\n+new line\n after\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-D"),
            String::from("FEATURE_X"),
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
    // Should contain #ifdef/#ifndef wrappers
    assert!(content.contains("#ifdef FEATURE_X"), "Should have #ifdef");
    assert!(content.contains("#ifndef FEATURE_X"), "Should have #ifndef");
    assert!(content.contains("new line"), "Should have new line");
    assert!(content.contains("old line"), "Should have old line");

    cleanup_test_dir(&test_dir);
}

// Test loose whitespace matching (-l)
#[test]
fn test_patch_loose_whitespace() {
    let test_dir = setup_test_dir("loose_ws");

    let original = test_dir.join("ws.txt");
    // File has tabs instead of spaces
    fs::write(&original, "line\tone\nline\ttwo\nline\tthree\n").unwrap();

    let patch_file = test_dir.join("ws.patch");
    // Patch uses spaces
    fs::write(
        &patch_file,
        "--- ws.txt\n+++ ws.txt\n@@ -1,3 +1,3 @@\n line one\n-line two\n+line TWO\n line three\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-l"),
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
    assert!(
        content.contains("line TWO"),
        "Patch should apply with loose whitespace matching"
    );

    cleanup_test_dir(&test_dir);
}

// Test ignore already-applied patches (-N)
#[test]
fn test_patch_ignore_applied() {
    let test_dir = setup_test_dir("ignore_applied");

    let original = test_dir.join("applied.txt");
    // File already has the "patched" content
    fs::write(&original, "modified content\n").unwrap();

    let patch_file = test_dir.join("applied.patch");
    // Patch tries to apply the same change
    fs::write(
        &patch_file,
        "--- applied.txt\n+++ applied.txt\n@@ -1 +1 @@\n-original content\n+modified content\n",
    )
    .unwrap();

    // Without -N, this would fail with "already applied" error
    // With -N, it should succeed (ignore already applied)
    run_test_with_checker(
        TestPlan {
            cmd: String::from("patch"),
            args: vec![
                String::from("-N"),
                String::from("-i"),
                patch_file.to_str().unwrap().to_string(),
                original.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            // Should succeed with -N
            assert!(
                output.status.success() || output.status.code() == Some(0),
                "Should succeed with -N flag ignoring already-applied patch"
            );
        },
    );

    cleanup_test_dir(&test_dir);
}

// Test custom reject file (-r)
#[test]
fn test_patch_reject_file() {
    let test_dir = setup_test_dir("reject_file");

    let original = test_dir.join("rej.txt");
    fs::write(&original, "line A\nline B\nline C\n").unwrap();

    let patch_file = test_dir.join("rej.patch");
    // This patch won't match (line X doesn't exist)
    fs::write(
        &patch_file,
        "--- rej.txt\n+++ rej.txt\n@@ -1,3 +1,3 @@\n line A\n-line X\n+line Y\n line C\n",
    )
    .unwrap();

    let custom_rej = test_dir.join("custom.rej");

    run_test_with_checker(
        TestPlan {
            cmd: String::from("patch"),
            args: vec![
                String::from("-r"),
                custom_rej.to_str().unwrap().to_string(),
                String::from("-i"),
                patch_file.to_str().unwrap().to_string(),
                original.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_, output| {
            assert_eq!(
                output.status.code(),
                Some(1),
                "Should exit 1 for rejected hunk"
            );
        },
    );

    // Check that custom reject file was created
    assert!(custom_rej.exists(), "Custom reject file should be created");

    cleanup_test_dir(&test_dir);
}

// Test reject file generation for failed hunks
#[test]
fn test_patch_failed_hunk() {
    let test_dir = setup_test_dir("failed_hunk");

    let original = test_dir.join("fail.txt");
    fs::write(&original, "alpha\nbeta\ngamma\n").unwrap();

    let patch_file = test_dir.join("fail.patch");
    // This hunk won't match
    fs::write(
        &patch_file,
        "--- fail.txt\n+++ fail.txt\n@@ -1,3 +1,3 @@\n alpha\n-WRONG\n+delta\n gamma\n",
    )
    .unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("patch"),
            args: vec![
                String::from("-i"),
                patch_file.to_str().unwrap().to_string(),
                original.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_, output| {
            assert_eq!(
                output.status.code(),
                Some(1),
                "Should exit 1 for rejected hunk"
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("FAILED") || stderr.contains("Hunk"),
                "Should report failed hunk"
            );
        },
    );

    // Check that .rej file was created
    let rej_file = test_dir.join("fail.txt.rej");
    assert!(rej_file.exists(), "Reject file should be created");

    cleanup_test_dir(&test_dir);
}

// Test fuzzy matching with offset
#[test]
fn test_patch_offset() {
    let test_dir = setup_test_dir("offset");

    let original = test_dir.join("offset.txt");
    // File has extra lines at the beginning
    fs::write(
        &original,
        "extra 1\nextra 2\nextra 3\nline 1\nline 2\nline 3\n",
    )
    .unwrap();

    let patch_file = test_dir.join("offset.patch");
    // Patch expects line 2 at position 2, but it's at position 5
    fs::write(
        &patch_file,
        "--- offset.txt\n+++ offset.txt\n@@ -1,3 +1,3 @@\n line 1\n-line 2\n+line 2 modified\n line 3\n",
    )
    .unwrap();

    run_test_with_checker(
        TestPlan {
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
        },
        |_, output| {
            assert!(output.status.success(), "Patch should succeed with offset");
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(stderr.contains("offset"), "Should report offset in stderr");
        },
    );

    let content = fs::read_to_string(&original).unwrap();
    assert!(
        content.contains("line 2 modified"),
        "Patch should be applied"
    );

    cleanup_test_dir(&test_dir);
}

// Test Index: header for filename detection
#[test]
fn test_patch_index_header() {
    let test_dir = setup_test_dir("index_header");

    let original = test_dir.join("indexed.txt");
    fs::write(&original, "content\n").unwrap();

    let patch_file = test_dir.join("index.patch");
    // Use Index: header instead of --- +++ headers
    fs::write(
        &patch_file,
        "Index: indexed.txt\n--- indexed.txt\n+++ indexed.txt\n@@ -1 +1 @@\n-content\n+new content\n",
    )
    .unwrap();

    // Don't specify the file, let patch find it from Index: header
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

// Test -p0 (use full path)
#[test]
fn test_patch_strip_path_p0() {
    let test_dir = setup_test_dir("strip_p0");
    let subdir = test_dir.join("a").join("b");
    fs::create_dir_all(&subdir).unwrap();

    let original = subdir.join("file.txt");
    fs::write(&original, "hello\n").unwrap();

    let patch_file = test_dir.join("p0.patch");
    let full_path = original.to_str().unwrap();
    fs::write(
        &patch_file,
        format!(
            "--- {}\n+++ {}\n@@ -1 +1 @@\n-hello\n+goodbye\n",
            full_path, full_path
        ),
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-p0"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&original).unwrap();
    assert_eq!(content, "goodbye\n");

    cleanup_test_dir(&test_dir);
}

// Test -p1 (strip first component, common for git patches)
#[test]
fn test_patch_strip_path_p1() {
    let test_dir = setup_test_dir("strip_p1");

    let original = test_dir.join("target.txt");
    fs::write(&original, "old\n").unwrap();

    let patch_file = test_dir.join("p1.patch");
    // a/target.txt -> target.txt with -p1
    fs::write(
        &patch_file,
        "--- a/target.txt\n+++ b/target.txt\n@@ -1 +1 @@\n-old\n+new\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-p1"),
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
    assert_eq!(content, "new\n");

    cleanup_test_dir(&test_dir);
}

// Test new file creation via patch
#[test]
fn test_patch_new_file() {
    let test_dir = setup_test_dir("new_file");

    let new_file = test_dir.join("created.txt");
    assert!(!new_file.exists(), "File should not exist before patch");

    let patch_file = test_dir.join("new.patch");
    // Patch to create a new file (old is /dev/null)
    fs::write(
        &patch_file,
        format!(
            "--- /dev/null\n+++ {}\n@@ -0,0 +1,2 @@\n+line one\n+line two\n",
            new_file.to_str().unwrap()
        ),
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-p0"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(new_file.exists(), "File should be created");
    let content = fs::read_to_string(&new_file).unwrap();
    assert_eq!(content, "line one\nline two\n");

    cleanup_test_dir(&test_dir);
}

// Test file not found error
#[test]
fn test_patch_file_not_found() {
    let test_dir = setup_test_dir("not_found");

    let patch_file = test_dir.join("nf.patch");
    fs::write(
        &patch_file,
        "--- nonexistent.txt\n+++ nonexistent.txt\n@@ -1 +1 @@\n-old\n+new\n",
    )
    .unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("patch"),
            args: vec![
                String::from("-i"),
                patch_file.to_str().unwrap().to_string(),
                test_dir
                    .join("nonexistent.txt")
                    .to_str()
                    .unwrap()
                    .to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 2,
        },
        |_, output| {
            assert_eq!(
                output.status.code(),
                Some(2),
                "Should exit 2 for file not found"
            );
        },
    );

    cleanup_test_dir(&test_dir);
}

// Test -R with -e is invalid
#[test]
fn test_patch_reverse_ed_error() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("patch"),
            args: vec![String::from("-R"), String::from("-e")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 2,
        },
        |_, output| {
            assert_eq!(
                output.status.code(),
                Some(2),
                "Should exit 2 for invalid argument combo"
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("-R") || stderr.contains("ed"),
                "Should mention -R or ed scripts"
            );
        },
    );
}
