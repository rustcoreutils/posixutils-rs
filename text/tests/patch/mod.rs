//
// Copyright (c) 2025-2026 Jeff Garzik
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
    // A change hunk (delete + add) must use the #else form, matching GNU
    // `patch -D`: #ifndef DEFINE / old / #else / new / #endif.
    assert_eq!(
        content,
        "before\n#ifndef FEATURE_X\nold line\n#else\nnew line\n#endif\nafter\n"
    );

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
    // -p0 keeps the name verbatim; it must be relative, since an absolute name
    // taken from the patch is refused as unsafe.
    fs::write(
        &patch_file,
        "--- a/b/file.txt\n+++ a/b/file.txt\n@@ -1 +1 @@\n-hello\n+goodbye\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-d"),
            test_dir.to_str().unwrap().to_string(),
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
    // Patch to create a new file (old is /dev/null). The name is relative:
    // an absolute name taken from the patch is refused as unsafe.
    fs::write(
        &patch_file,
        "--- /dev/null\n+++ created.txt\n@@ -0,0 +1,2 @@\n+line one\n+line two\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-d"),
            test_dir.to_str().unwrap().to_string(),
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

// #1: a patch whose last line is marked "\ No newline at end of file" must
// produce a file with no trailing newline (matching GNU patch).
#[test]
fn test_patch_no_newline_at_eof() {
    let test_dir = setup_test_dir("no_newline_eof");

    let original = test_dir.join("nl.txt");
    fs::write(&original, "line 1\nline 2\nline 3\n").unwrap();

    let patch_file = test_dir.join("nl.patch");
    fs::write(
        &patch_file,
        "--- nl.txt\n+++ nl.txt\n@@ -1,3 +1,3 @@\n line 1\n line 2\n-line 3\n+line 3 changed\n\\ No newline at end of file\n",
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

    // Last line must NOT have a trailing newline.
    let content = fs::read_to_string(&original).unwrap();
    assert_eq!(content, "line 1\nline 2\nline 3 changed");
    assert!(
        !content.ends_with('\n'),
        "Output must have no trailing newline"
    );

    cleanup_test_dir(&test_dir);
}

// #2: a deletion patch (new file is /dev/null) must remove the target rather
// than leaving an empty file behind.
#[test]
fn test_patch_delete_file() {
    let test_dir = setup_test_dir("delete_file");

    let original = test_dir.join("doomed.txt");
    fs::write(&original, "a\nb\n").unwrap();

    let patch_file = test_dir.join("del.patch");
    fs::write(
        &patch_file,
        "--- doomed.txt\n+++ /dev/null\n@@ -1,2 +0,0 @@\n-a\n-b\n",
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

    assert!(
        !original.exists(),
        "Deletion patch should remove the target file"
    );

    cleanup_test_dir(&test_dir);
}

// #4: -o outfile receiving multiple patched files must concatenate the
// successive versions rather than truncating each time.
#[test]
fn test_patch_output_concat() {
    let test_dir = setup_test_dir("output_concat");

    let f1 = test_dir.join("f1.txt");
    let f2 = test_dir.join("f2.txt");
    fs::write(&f1, "a\n").unwrap();
    fs::write(&f2, "b\n").unwrap();

    // One patch file with two file sections.
    let patch_file = test_dir.join("multi.patch");
    fs::write(
        &patch_file,
        "--- f1.txt\n+++ f1.txt\n@@ -1 +1 @@\n-a\n+A\n--- f2.txt\n+++ f2.txt\n@@ -1 +1 @@\n-b\n+B\n",
    )
    .unwrap();

    let output = test_dir.join("out.txt");

    // Run from the test directory so the relative paths resolve.
    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-d"),
            test_dir.to_str().unwrap().to_string(),
            String::from("-o"),
            output.to_str().unwrap().to_string(),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let out_content = fs::read_to_string(&output).unwrap();
    assert_eq!(
        out_content, "A\nB\n",
        "Successive -o versions must concatenate"
    );

    cleanup_test_dir(&test_dir);
}

// #5: -b must back up each file only the first time it is patched, so the
// .orig holds the true original after a multi-patch run on the same file.
#[test]
fn test_patch_backup_once() {
    let test_dir = setup_test_dir("backup_once");

    let target = test_dir.join("b.txt");
    fs::write(&target, "a\n").unwrap();

    // Two sections both patching b.txt: a -> B -> C.
    let patch_file = test_dir.join("b2.patch");
    fs::write(
        &patch_file,
        "--- b.txt\n+++ b.txt\n@@ -1 +1 @@\n-a\n+B\n--- b.txt\n+++ b.txt\n@@ -1 +1 @@\n-B\n+C\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-b"),
            String::from("-d"),
            test_dir.to_str().unwrap().to_string(),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&target).unwrap();
    assert_eq!(content, "C\n", "Both hunks should apply in sequence");

    let backup = test_dir.join("b.txt.orig");
    assert!(backup.exists(), "Backup should exist");
    let backup_content = fs::read_to_string(&backup).unwrap();
    assert_eq!(
        backup_content, "a\n",
        ".orig must hold the ORIGINAL content, not an intermediate version"
    );

    cleanup_test_dir(&test_dir);
}

// #7: -p must count a sequence of adjacent slashes as a single slash.
#[test]
fn test_patch_strip_path_double_slash() {
    let test_dir = setup_test_dir("strip_double_slash");

    let subdir = test_dir.join("sub");
    fs::create_dir_all(&subdir).unwrap();
    let target = subdir.join("strip.txt");
    fs::write(&target, "content\n").unwrap();

    // The header path has a "//" run that must collapse to one slash so that
    // -p2 yields "sub/strip.txt" (not "foo/sub/strip.txt").
    let patch_file = test_dir.join("pp.patch");
    fs::write(
        &patch_file,
        "--- //foo/sub/strip.txt\n+++ //foo/sub/strip.txt\n@@ -1 +1 @@\n-content\n+new content\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-d"),
            test_dir.to_str().unwrap().to_string(),
            String::from("-p2"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(&target).unwrap();
    assert_eq!(content, "new content\n");

    cleanup_test_dir(&test_dir);
}

// A context diff whose section lines carry non-ASCII payloads must parse.
// The section-line splitter used to slice at byte index 2, which panicked
// (exit 101) whenever a line began with a multi-byte character.
#[test]
fn test_patch_context_non_ascii_line() {
    let test_dir = setup_test_dir("context_non_ascii");

    let target = test_dir.join("utf8.txt");
    fs::write(&target, "a\nb\nc\n").unwrap();

    let patch_file = test_dir.join("utf8.patch");
    fs::write(
        &patch_file,
        "*** utf8.txt\t2024-01-01 00:00:00\n\
         --- utf8.txt\t2024-01-01 00:00:01\n\
         ***************\n\
         *** 1,3 ****\n\
         \x20 a\n\
         ! b\n\
         \x20 c\n\
         --- 1,3 ----\n\
         \x20 a\n\
         ! B\n\
         \x20 c\n\
         \u{2192} trailing prose line\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-c"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(fs::read_to_string(&target).unwrap(), "a\nB\nc\n");

    cleanup_test_dir(&test_dir);
}

// A context-diff section line whose payload is empty may reach us as a bare
// "+", "-" or "!" when a mailer has stripped the trailing separator space.
#[test]
fn test_patch_context_stripped_separator() {
    let test_dir = setup_test_dir("context_stripped_sep");

    let target = test_dir.join("strip.txt");
    fs::write(&target, "a\nb\n").unwrap();

    let patch_file = test_dir.join("strip.patch");
    // The added blank line is written "+" with no trailing space.
    fs::write(
        &patch_file,
        "*** strip.txt\n--- strip.txt\n***************\n\
         *** 1,2 ****\n  a\n  b\n--- 1,3 ----\n  a\n+\n  b\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-c"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(fs::read_to_string(&target).unwrap(), "a\n\nb\n");

    cleanup_test_dir(&test_dir);
}

// A normal-diff range whose end precedes its start is malformed. The count
// arithmetic used to underflow, aborting with a capacity-overflow panic.
#[test]
fn test_patch_normal_reversed_range() {
    let test_dir = setup_test_dir("normal_rev_range");

    let target = test_dir.join("rev.txt");
    fs::write(&target, "a\nb\nc\nd\ne\n").unwrap();

    let patch_file = test_dir.join("rev.patch");
    fs::write(&patch_file, "5,2c3,1\n< a\n---\n> A\n").unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("patch"),
            args: vec![
                String::from("-n"),
                String::from("-i"),
                patch_file.to_str().unwrap().to_string(),
                target.to_str().unwrap().to_string(),
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
                "Malformed range should be a clean error, not a panic"
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("malformed range"),
                "Should name the malformed range, got: {}",
                stderr
            );
        },
    );

    assert_eq!(fs::read_to_string(&target).unwrap(), "a\nb\nc\nd\ne\n");

    cleanup_test_dir(&test_dir);
}

// Same malformed range, in an ed script.
#[test]
fn test_patch_ed_reversed_range() {
    let test_dir = setup_test_dir("ed_rev_range");

    let target = test_dir.join("edrev.txt");
    fs::write(&target, "a\nb\nc\n").unwrap();

    let patch_file = test_dir.join("edrev.patch");
    fs::write(&patch_file, "5,2c\nX\n.\n").unwrap();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("patch"),
            args: vec![
                String::from("-e"),
                String::from("-i"),
                patch_file.to_str().unwrap().to_string(),
                target.to_str().unwrap().to_string(),
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
                "Malformed range should be a clean error, not a panic"
            );
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("malformed range"),
                "Should name the malformed range, got: {}",
                stderr
            );
        },
    );

    assert_eq!(fs::read_to_string(&target).unwrap(), "a\nb\nc\n");

    cleanup_test_dir(&test_dir);
}

// Helper: run patch and return its exit code plus stderr, for tests that only
// care about a property of the result rather than exact diagnostic text.
fn run_patch_capture(args: Vec<String>) -> (i32, String) {
    let mut code = 0;
    let mut err = String::new();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("patch"),
            args,
            stdin_data: String::new(),
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

// A fuzzy match ignores the outer context lines, so it must not write the
// patch's copy of them over the file. Only the verified window is replaced.
#[test]
fn test_patch_fuzz_preserves_outer_context() {
    let test_dir = setup_test_dir("fuzz_outer_ctx");

    let target = test_dir.join("fuzz1.txt");
    fs::write(&target, "c1\nc2\nXXX\nc3\nc4\n").unwrap();

    let patch_file = test_dir.join("fuzz1.patch");
    // The first and last context lines disagree with the file (z1/z4 vs c1/c4).
    fs::write(
        &patch_file,
        "--- fuzz1.txt\n+++ fuzz1.txt\n@@ -1,5 +1,5 @@\n z1\n c2\n-XXX\n+YYY\n c3\n z4\n",
    )
    .unwrap();

    let (code, err) = run_patch_capture(vec![
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
        target.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 0, "should apply with fuzz, stderr: {}", err);
    assert!(err.contains("fuzz 1"), "should report fuzz 1, got: {}", err);

    assert_eq!(
        fs::read_to_string(&target).unwrap(),
        "c1\nc2\nYYY\nc3\nc4\n",
        "ignored context must be left as the file has it"
    );

    cleanup_test_dir(&test_dir);
}

// Same, with two ignored context lines at each end.
#[test]
fn test_patch_fuzz2_preserves_outer_context() {
    let test_dir = setup_test_dir("fuzz2_outer_ctx");

    let target = test_dir.join("fuzz2.txt");
    fs::write(&target, "q1\nq2\nc3\nXXX\nc5\nq6\nq7\n").unwrap();

    let patch_file = test_dir.join("fuzz2.patch");
    fs::write(
        &patch_file,
        "--- fuzz2.txt\n+++ fuzz2.txt\n@@ -1,7 +1,7 @@\n z1\n z2\n c3\n-XXX\n+YYY\n c5\n z6\n z7\n",
    )
    .unwrap();

    let (code, err) = run_patch_capture(vec![
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
        target.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 0, "should apply with fuzz 2, stderr: {}", err);
    assert!(err.contains("fuzz 2"), "should report fuzz 2, got: {}", err);

    assert_eq!(
        fs::read_to_string(&target).unwrap(),
        "q1\nq2\nc3\nYYY\nc5\nq6\nq7\n"
    );

    cleanup_test_dir(&test_dir);
}

// POSIX allows a rescan to ignore lines of *context*. Deleted lines are not
// context and must still match exactly, so a hunk with no context (the shape
// "diff -U0" produces) can never be placed by fuzz.
#[test]
fn test_patch_fuzz_does_not_trim_deletes() {
    let test_dir = setup_test_dir("fuzz_no_trim_del");

    let target = test_dir.join("del.txt");
    fs::write(&target, "A\nb\nc\nd\ne\n").unwrap();

    let patch_file = test_dir.join("del.patch");
    // "a" does not match the file's "A"; there is no context to give up.
    fs::write(
        &patch_file,
        "--- del.txt\n+++ del.txt\n@@ -1,3 +1,1 @@\n-a\n-b\n-c\n+X\n",
    )
    .unwrap();

    let (code, err) = run_patch_capture(vec![
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
        target.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 1, "hunk must be rejected, stderr: {}", err);

    assert_eq!(
        fs::read_to_string(&target).unwrap(),
        "A\nb\nc\nd\ne\n",
        "a hunk whose deletes do not match must not modify the file"
    );
    assert!(test_dir.join("del.txt.rej").exists());

    cleanup_test_dir(&test_dir);
}

// A normal diff records no context at all, so no fuzz window can be built.
#[test]
fn test_patch_normal_diff_gets_no_fuzz() {
    let test_dir = setup_test_dir("normal_no_fuzz");

    let target = test_dir.join("nfz.txt");
    fs::write(&target, "A\nb\nc\n").unwrap();

    let patch_file = test_dir.join("nfz.patch");
    fs::write(&patch_file, "1,3c1\n< a\n< b\n< c\n---\n> X\n").unwrap();

    let (code, _) = run_patch_capture(vec![
        String::from("-n"),
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
        target.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 1, "hunk must be rejected");
    assert_eq!(fs::read_to_string(&target).unwrap(), "A\nb\nc\n");

    cleanup_test_dir(&test_dir);
}

// POSIX describes exactly two rescans, so three ignored context lines per side
// is beyond what a fuzzy match may do.
#[test]
fn test_patch_fuzz_capped_at_two() {
    let test_dir = setup_test_dir("fuzz_cap");

    let target = test_dir.join("cap.txt");
    fs::write(&target, "q1\nq2\nq3\nc4\nXXX\nc6\nq7\nq8\nq9\n").unwrap();

    let patch_file = test_dir.join("cap.patch");
    fs::write(
        &patch_file,
        "--- cap.txt\n+++ cap.txt\n@@ -1,9 +1,9 @@\n z1\n z2\n z3\n c4\n-XXX\n+YYY\n c6\n z7\n z8\n z9\n",
    )
    .unwrap();

    let (code, _) = run_patch_capture(vec![
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
        target.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 1, "fuzz 3 would be needed; hunk must be rejected");
    assert_eq!(
        fs::read_to_string(&target).unwrap(),
        "q1\nq2\nq3\nc4\nXXX\nc6\nq7\nq8\nq9\n"
    );

    cleanup_test_dir(&test_dir);
}

// A hunk whose old-side lines are all blank is still content-matched: it must
// be verified against the file, and it must contribute to the running offset
// that later hunks are placed by. Deriving "is this an ed script?" from the
// old-side text got both wrong.
#[test]
fn test_patch_blank_line_delete_then_add() {
    let test_dir = setup_test_dir("blank_delete");

    let target = test_dir.join("blank.txt");
    fs::write(&target, "a\n\n\nb\nc\n").unwrap();

    let patch_file = test_dir.join("blank.patch");
    fs::write(
        &patch_file,
        "--- blank.txt\n+++ blank.txt\n@@ -2,2 +1,0 @@\n-\n-\n@@ -4,0 +3 @@\n+ZZZ\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(
        fs::read_to_string(&target).unwrap(),
        "a\nb\nZZZ\nc\n",
        "the blank-line delete must shift the following hunk"
    );

    cleanup_test_dir(&test_dir);
}

// The same two insertions, expressed in unified, normal and context format,
// must land in the same place. Each format spells a zero-count side
// differently; the parsers normalize them to one convention.
fn twenty_lines() -> String {
    (1..=20).map(|i| format!("l{:02}\n", i)).collect()
}

fn expected_with_insertions() -> String {
    let mut s = String::new();
    for i in 1..=20 {
        s.push_str(&format!("l{:02}\n", i));
        if i == 5 {
            s.push_str("n1\nn2\n");
        }
        if i == 15 {
            s.push_str("n3\nn4\n");
        }
    }
    s
}

#[test]
fn test_patch_unified_zero_context_additions() {
    let test_dir = setup_test_dir("add_unified");

    let target = test_dir.join("add.txt");
    fs::write(&target, twenty_lines()).unwrap();

    let patch_file = test_dir.join("add.patch");
    fs::write(
        &patch_file,
        "--- add.txt\n+++ add.txt\n@@ -5,0 +6,2 @@\n+n1\n+n2\n@@ -15,0 +18,2 @@\n+n3\n+n4\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-u"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(
        fs::read_to_string(&target).unwrap(),
        expected_with_insertions()
    );

    cleanup_test_dir(&test_dir);
}

#[test]
fn test_patch_normal_zero_context_additions() {
    let test_dir = setup_test_dir("add_normal");

    let target = test_dir.join("add.txt");
    fs::write(&target, twenty_lines()).unwrap();

    let patch_file = test_dir.join("add.patch");
    fs::write(&patch_file, "5a6,7\n> n1\n> n2\n15a18,19\n> n3\n> n4\n").unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-n"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(
        fs::read_to_string(&target).unwrap(),
        expected_with_insertions()
    );

    cleanup_test_dir(&test_dir);
}

#[test]
fn test_patch_context_zero_context_additions() {
    let test_dir = setup_test_dir("add_context");

    let target = test_dir.join("add.txt");
    fs::write(&target, twenty_lines()).unwrap();

    let patch_file = test_dir.join("add.patch");
    // An insertion has an empty old section, spelled as a bare line number.
    fs::write(
        &patch_file,
        "*** add.txt\t2024-01-01 00:00:00\n\
         --- add.txt\t2024-01-01 00:00:01\n\
         ***************\n*** 5 ****\n--- 6,7 ----\n+ n1\n+ n2\n\
         ***************\n*** 15 ****\n--- 18,19 ----\n+ n3\n+ n4\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-c"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(
        fs::read_to_string(&target).unwrap(),
        expected_with_insertions()
    );

    cleanup_test_dir(&test_dir);
}

// An ed script's line numbers are absolute: they already describe the file as
// it stands when each command runs, so no cumulative offset may leak in.
#[test]
fn test_patch_ed_positional_multi() {
    let test_dir = setup_test_dir("ed_positional");

    let target = test_dir.join("edp.txt");
    fs::write(&target, "one\ntwo\nthree\nfour\nfive\nsix\n").unwrap();

    let patch_file = test_dir.join("edp.patch");
    fs::write(&patch_file, "5c\nFIVE\n.\n2d\n").unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-e"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(
        fs::read_to_string(&target).unwrap(),
        "one\nthree\nfour\nFIVE\nsix\n"
    );

    cleanup_test_dir(&test_dir);
}

// A synthesized #endif carries its own newline, so it overrides the hunk's
// "no newline at end of file" marker.
#[test]
fn test_patch_ifdef_no_newline_at_eof() {
    let test_dir = setup_test_dir("ifdef_eof");

    let target = test_dir.join("dnl.txt");
    fs::write(&target, "a\nb").unwrap();

    let patch_file = test_dir.join("dnl.patch");
    fs::write(
        &patch_file,
        "--- dnl.txt\n+++ dnl.txt\n@@ -1,2 +1,2 @@\n a\n-b\n\
         \\ No newline at end of file\n+B\n\\ No newline at end of file\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-D"),
            String::from("FEATURE"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(
        fs::read_to_string(&target).unwrap(),
        "a\n#ifndef FEATURE\nb\n#else\nB\n#endif\n"
    );

    cleanup_test_dir(&test_dir);
}

// -D must guard a pure addition too. The pure-addition path used to return
// before the -D writer ran, inserting the new lines raw.
#[test]
fn test_patch_ifdef_pure_addition() {
    let test_dir = setup_test_dir("ifdef_pure_add");

    let target = test_dir.join("dadd.txt");
    fs::write(&target, "a\nb\nc\n").unwrap();

    let patch_file = test_dir.join("dadd.patch");
    fs::write(
        &patch_file,
        "--- dadd.txt\n+++ dadd.txt\n@@ -1,0 +2 @@\n+NEW\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-D"),
            String::from("FEATURE"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(
        fs::read_to_string(&target).unwrap(),
        "a\n#ifdef FEATURE\nNEW\n#endif\nb\nc\n"
    );

    cleanup_test_dir(&test_dir);
}

// A rejected hunk's context-format header must convert a normalized zero-count
// side back to the line *after* which the change goes.
#[test]
fn test_patch_reject_zero_new_count_header() {
    let test_dir = setup_test_dir("reject_zero_count");

    let target = test_dir.join("rj.txt");
    fs::write(&target, "a\nb\nc\n").unwrap();

    let patch_file = test_dir.join("rj.patch");
    // A pure deletion that does not match, so the hunk is rejected.
    fs::write(&patch_file, "--- rj.txt\n+++ rj.txt\n@@ -2 +1,0 @@\n-ZZZ\n").unwrap();

    let (code, _) = run_patch_capture(vec![
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
        target.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 1);
    assert_eq!(fs::read_to_string(&target).unwrap(), "a\nb\nc\n");

    let rej = fs::read_to_string(test_dir.join("rj.txt.rej")).unwrap();
    assert!(
        rej.contains("--- 1 ----"),
        "zero-count new side should print line 1, got:\n{}",
        rej
    );

    cleanup_test_dir(&test_dir);
}

// git format-patch puts the commit message before the diff. Detection used to
// look at only the first 20 lines, so any message longer than that made the
// whole patch unreadable.
#[test]
fn test_patch_long_preamble() {
    let test_dir = setup_test_dir("long_preamble");

    let target = test_dir.join("t.txt");
    fs::write(&target, "a\nb\nc\n").unwrap();

    let mut patch_text = String::from(
        "From 1234567890abcdef Mon Sep 17 00:00:00 2001\n\
         From: Some One <someone@example.com>\n\
         Subject: [PATCH] a change with a long message\n\n",
    );
    for i in 1..=22 {
        patch_text.push_str(&format!("commit message line {}\n", i));
    }
    patch_text.push_str(
        "\n---\n t.txt | 2 +-\n 1 file changed\n\n\
         diff --git a/t.txt b/t.txt\n\
         index 1234567..89abcde 100644\n\
         --- a/t.txt\n+++ b/t.txt\n@@ -1,3 +1,3 @@\n a\n-b\n+B\n c\n",
    );

    let patch_file = test_dir.join("long.patch");
    fs::write(&patch_file, patch_text).unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-d"),
            test_dir.to_str().unwrap().to_string(),
            String::from("-p1"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(fs::read_to_string(&target).unwrap(), "a\nB\nc\n");

    cleanup_test_dir(&test_dir);
}

// Not every diff separates the file name from the timestamp with a tab.
#[test]
fn test_patch_single_space_timestamp() {
    let test_dir = setup_test_dir("space_timestamp");

    let target = test_dir.join("ts.txt");
    fs::write(&target, "a\nb\nc\n").unwrap();

    let patch_file = test_dir.join("ts.patch");
    fs::write(
        &patch_file,
        "--- ts.txt 2024-01-01 10:00:00.000000000 +0000\n\
         +++ ts.txt 2024-01-02 10:00:00.000000000 +0000\n\
         @@ -1,3 +1,3 @@\n a\n-b\n+B\n c\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-d"),
            test_dir.to_str().unwrap().to_string(),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(fs::read_to_string(&target).unwrap(), "a\nB\nc\n");

    cleanup_test_dir(&test_dir);
}

// diff writes "< " even for an empty line; a mailer that strips trailing
// whitespace leaves a bare "<". That used to end the hunk early, leaving it
// with no operations at all, which was then reported as applied.
#[test]
fn test_patch_normal_stripped_marker() {
    let test_dir = setup_test_dir("normal_stripped");

    let target = test_dir.join("bare.txt");
    fs::write(&target, "a\n\nb\n").unwrap();

    let patch_file = test_dir.join("bare.patch");
    fs::write(&patch_file, "2d1\n<\n").unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-n"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(fs::read_to_string(&target).unwrap(), "a\nb\n");

    cleanup_test_dir(&test_dir);
}

// A hunk that supplies fewer lines than its header declares was truncated
// somewhere; applying it as though it were complete loses data silently.
#[test]
fn test_patch_normal_truncated_hunk() {
    let test_dir = setup_test_dir("normal_truncated");

    let target = test_dir.join("short.txt");
    fs::write(&target, "a\nb\nc\n").unwrap();

    let patch_file = test_dir.join("short.patch");
    // Declares two old lines and one new; supplies one old and none.
    fs::write(&patch_file, "1,2c1\n< a\n").unwrap();

    let (code, err) = run_patch_capture(vec![
        String::from("-n"),
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
        target.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 2, "truncated hunk must be an error, stderr: {}", err);
    assert!(
        err.contains("declares"),
        "should report the count mismatch, got: {}",
        err
    );
    assert_eq!(fs::read_to_string(&target).unwrap(), "a\nb\nc\n");

    cleanup_test_dir(&test_dir);
}

// A git section that changes no text carries no hunk. That is well-formed
// input, not garbage: name what is skipped rather than rejecting the patch.
#[test]
fn test_patch_git_rename_only() {
    let test_dir = setup_test_dir("git_rename");

    let target = test_dir.join("old.txt");
    fs::write(&target, "a\n").unwrap();

    let patch_file = test_dir.join("rename.patch");
    fs::write(
        &patch_file,
        "diff --git a/old.txt b/new.txt\n\
         similarity index 100%\n\
         rename from old.txt\n\
         rename to new.txt\n",
    )
    .unwrap();

    let (code, err) = run_patch_capture(vec![
        String::from("-d"),
        test_dir.to_str().unwrap().to_string(),
        String::from("-p1"),
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 0, "a rename-only patch is not an error");
    assert!(
        err.contains("rename of old.txt to new.txt"),
        "should name the skipped rename, got: {}",
        err
    );
    // Neither POSIX nor GNU patch performs the rename.
    assert!(target.exists());
    assert!(!test_dir.join("new.txt").exists());

    cleanup_test_dir(&test_dir);
}

// A '\r' is part of a line's content, so a patch written against LF text does
// not match a CRLF file. It must reject rather than apply and, as a side
// effect of str::lines() dropping the '\r', rewrite every line ending in the
// file.
#[test]
fn test_patch_crlf_file_lf_patch_rejects() {
    let test_dir = setup_test_dir("crlf_lf");

    let target = test_dir.join("crlf.txt");
    fs::write(&target, "a\r\nb\r\nc\r\n").unwrap();

    let patch_file = test_dir.join("crlf.patch");
    fs::write(
        &patch_file,
        "--- crlf.txt\n+++ crlf.txt\n@@ -1,3 +1,3 @@\n a\n-b\n+B\n c\n",
    )
    .unwrap();

    let (code, _) = run_patch_capture(vec![
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
        target.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 1, "hunk must be rejected");
    assert_eq!(
        fs::read(&target).unwrap(),
        b"a\r\nb\r\nc\r\n",
        "every line ending must survive untouched"
    );

    cleanup_test_dir(&test_dir);
}

// A patch whose content lines carry the file's CRLF applies, and the endings
// round-trip.
#[test]
fn test_patch_crlf_file_crlf_patch_applies() {
    let test_dir = setup_test_dir("crlf_crlf");

    let target = test_dir.join("crlf.txt");
    fs::write(&target, "a\r\nb\r\nc\r\n").unwrap();

    let patch_file = test_dir.join("crlf.patch");
    // diff metadata in LF, content lines carrying the file's CRLF.
    fs::write(
        &patch_file,
        "--- crlf.txt\n+++ crlf.txt\n@@ -1,3 +1,3 @@\n a\r\n-b\r\n+B\r\n c\r\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(fs::read(&target).unwrap(), b"a\r\nB\r\nc\r\n");

    cleanup_test_dir(&test_dir);
}

// A file with mixed endings must keep each line's own ending across a hunk
// that touches only part of it.
#[test]
fn test_patch_mixed_line_endings_preserved() {
    let test_dir = setup_test_dir("mixed_eol");

    let target = test_dir.join("mixed.txt");
    fs::write(&target, "a\nb\r\nc\nd\r\n").unwrap();

    let patch_file = test_dir.join("mixed.patch");
    fs::write(
        &patch_file,
        "--- mixed.txt\n+++ mixed.txt\n@@ -3,2 +3,2 @@\n-c\n+C\n d\r\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(fs::read(&target).unwrap(), b"a\nb\r\nC\nd\r\n");

    cleanup_test_dir(&test_dir);
}

// A patch covering several files may already be applied to one of them. That
// file is skipped, but the rest must still be patched, and the run reports the
// reject rather than an error.
#[test]
fn test_patch_reversed_file_skips_not_aborts() {
    let test_dir = setup_test_dir("reversed_continue");

    let a = test_dir.join("A.txt");
    let b = test_dir.join("B.txt");
    fs::write(&a, "new\n").unwrap(); // already applied
    fs::write(&b, "bold\n").unwrap();

    let patch_file = test_dir.join("two.patch");
    fs::write(
        &patch_file,
        "--- A.txt\n+++ A.txt\n@@ -1,1 +1,1 @@\n-old\n+new\n\
         --- B.txt\n+++ B.txt\n@@ -1,1 +1,1 @@\n-bold\n+bnew\n",
    )
    .unwrap();

    let (code, _) = run_patch_capture(vec![
        String::from("-d"),
        test_dir.to_str().unwrap().to_string(),
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 1, "rejects give exit 1, not an error");

    assert_eq!(fs::read_to_string(&a).unwrap(), "new\n", "A is left alone");
    assert_eq!(
        fs::read_to_string(&b).unwrap(),
        "bnew\n",
        "B must still be patched"
    );
    assert!(test_dir.join("A.txt.rej").exists());

    cleanup_test_dir(&test_dir);
}

// POSIX: rejected hunks are appended to the reject file. With -r naming one
// file for the whole run, each section's rejects must survive.
#[test]
fn test_patch_reject_file_accumulates() {
    let test_dir = setup_test_dir("reject_accum");

    fs::write(test_dir.join("r1.txt"), "zzz\n").unwrap();
    fs::write(test_dir.join("r2.txt"), "zzz\n").unwrap();

    let patch_file = test_dir.join("two.patch");
    fs::write(
        &patch_file,
        "--- r1.txt\n+++ r1.txt\n@@ -1,1 +1,1 @@\n-nomatch1\n+x1\n\
         --- r2.txt\n+++ r2.txt\n@@ -1,1 +1,1 @@\n-nomatch2\n+x2\n",
    )
    .unwrap();

    let (code, _) = run_patch_capture(vec![
        String::from("-d"),
        test_dir.to_str().unwrap().to_string(),
        String::from("-r"),
        String::from("all.rej"),
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 1);

    let rej = fs::read_to_string(test_dir.join("all.rej")).unwrap();
    assert!(
        rej.contains("nomatch1"),
        "first file's rejects were truncated away:\n{}",
        rej
    );
    assert!(rej.contains("nomatch2"), "second file's rejects missing");
    assert!(
        rej.contains("r1.txt") && rej.contains("r2.txt"),
        "each group of rejects should name its file:\n{}",
        rej
    );

    cleanup_test_dir(&test_dir);
}

// -f means "do not ask any questions", so an unresolvable target must fail
// rather than prompt on the controlling terminal.
#[test]
fn test_patch_force_does_not_prompt() {
    let test_dir = setup_test_dir("force_no_prompt");

    let patch_file = test_dir.join("missing.patch");
    fs::write(
        &patch_file,
        "--- nosuch_abc.txt\n+++ nosuch_abc.txt\n@@ -1,1 +1,1 @@\n-a\n+A\n",
    )
    .unwrap();

    let (code, err) = run_patch_capture(vec![
        String::from("-d"),
        test_dir.to_str().unwrap().to_string(),
        String::from("-f"),
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 2);
    assert!(
        err.contains("could not determine target file"),
        "got: {}",
        err
    );

    cleanup_test_dir(&test_dir);
}

// A patch is untrusted input: a name that walks up out of the directory being
// patched must be refused, even when the file it names exists.
#[test]
fn test_patch_refuses_parent_dir_traversal() {
    let test_dir = setup_test_dir("traversal_existing");
    let subdir = test_dir.join("sub");
    fs::create_dir_all(&subdir).unwrap();

    let victim = test_dir.join("VICTIM.txt");
    fs::write(&victim, "victim\n").unwrap();

    let patch_file = test_dir.join("esc.patch");
    fs::write(
        &patch_file,
        "--- ../VICTIM.txt\n+++ ../VICTIM.txt\n@@ -1,1 +1,1 @@\n-victim\n+PWNED\n",
    )
    .unwrap();

    let (_, err) = run_patch_capture(vec![
        String::from("-d"),
        subdir.to_str().unwrap().to_string(),
        String::from("-p0"),
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
    ]);
    assert!(
        err.contains("dangerous file name"),
        "should refuse the name, got: {}",
        err
    );
    assert_eq!(
        fs::read_to_string(&victim).unwrap(),
        "victim\n",
        "must not write outside the directory being patched"
    );

    cleanup_test_dir(&test_dir);
}

// The new-file path returns a name without checking that it exists, so it is
// the one that would create a whole tree wherever the patch says.
#[test]
fn test_patch_refuses_traversal_for_new_file() {
    let test_dir = setup_test_dir("traversal_new");
    let subdir = test_dir.join("sub");
    fs::create_dir_all(&subdir).unwrap();

    let patch_file = test_dir.join("esc.patch");
    fs::write(
        &patch_file,
        "--- /dev/null\n+++ ../ESCAPE/deep/evil.txt\n@@ -0,0 +1,1 @@\n+pwned\n",
    )
    .unwrap();

    let (_, err) = run_patch_capture(vec![
        String::from("-d"),
        subdir.to_str().unwrap().to_string(),
        String::from("-p0"),
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
    ]);
    assert!(err.contains("dangerous file name"), "got: {}", err);
    assert!(
        !test_dir.join("ESCAPE").exists(),
        "must not create a directory tree outside the patch directory"
    );

    cleanup_test_dir(&test_dir);
}

// An absolute name from the patch reaches anywhere the user can write.
#[test]
fn test_patch_refuses_absolute_name() {
    let test_dir = setup_test_dir("absolute_name");
    let subdir = test_dir.join("sub");
    fs::create_dir_all(&subdir).unwrap();

    let evil = test_dir.join("ABS_EVIL.txt");
    let patch_file = test_dir.join("abs.patch");
    fs::write(
        &patch_file,
        format!(
            "--- /dev/null\n+++ {}\n@@ -0,0 +1,1 @@\n+pwned\n",
            evil.to_str().unwrap()
        ),
    )
    .unwrap();

    let (_, err) = run_patch_capture(vec![
        String::from("-d"),
        subdir.to_str().unwrap().to_string(),
        String::from("-p0"),
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
    ]);
    assert!(err.contains("dangerous file name"), "got: {}", err);
    assert!(
        !evil.exists(),
        "must not write an absolute path from a patch"
    );

    cleanup_test_dir(&test_dir);
}

// The refusal applies to names the *patch* supplies. A name the user gives on
// the command line is their own instruction and must still work.
#[test]
fn test_patch_explicit_operand_reaches_any_path() {
    let test_dir = setup_test_dir("explicit_operand");
    let subdir = test_dir.join("sub");
    fs::create_dir_all(&subdir).unwrap();

    let outside = test_dir.join("OUT.txt");
    fs::write(&outside, "victim\n").unwrap();

    let patch_file = test_dir.join("op.patch");
    fs::write(
        &patch_file,
        "--- ../OUT.txt\n+++ ../OUT.txt\n@@ -1,1 +1,1 @@\n-victim\n+OK\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-d"),
            subdir.to_str().unwrap().to_string(),
            String::from("-p0"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            outside.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(fs::read_to_string(&outside).unwrap(), "OK\n");

    cleanup_test_dir(&test_dir);
}

// diff omits a section body entirely when that side holds only context, so an
// empty body is not an empty side. Reading it as one both mis-sized the hunk
// (poisoning the running offset for every later hunk) and dropped every
// context line, leaving nothing to match the file against.
#[test]
fn test_patch_context_omitted_section_bodies() {
    let test_dir = setup_test_dir("context_omitted");

    let target = test_dir.join("f.txt");
    let original: String = (1..=40).map(|i| format!("{}\n", i)).collect();
    fs::write(&target, &original).unwrap();

    let patch_file = test_dir.join("c.patch");
    // Real `diff -c` output: line 5 removed, INSERTED added after line 25.
    // Hunk 1's new section body is omitted, hunk 2's old section body is.
    fs::write(
        &patch_file,
        "*** f.txt\t2024-01-01 00:00:00\n\
         --- f.txt\t2024-01-01 00:00:01\n\
         ***************\n\
         *** 2,8 ****\n  2\n  3\n  4\n- 5\n  6\n  7\n  8\n\
         --- 2,7 ----\n\
         ***************\n\
         *** 23,28 ****\n\
         --- 22,28 ----\n  23\n  24\n  25\n+ INSERTED\n  26\n  27\n  28\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-c"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let expected: String = (1..=40)
        .filter(|i| *i != 5)
        .flat_map(|i| {
            if i == 25 {
                vec![format!("{}\n", i), String::from("INSERTED\n")]
            } else {
                vec![format!("{}\n", i)]
            }
        })
        .collect();
    assert_eq!(fs::read_to_string(&target).unwrap(), expected);

    cleanup_test_dir(&test_dir);
}

// The two section bodies are aligned by their context lines, one to one. The
// merge used to advance the new section's index twice per context line, so a
// hunk with more than one leading context line applied at the wrong offset.
#[test]
fn test_patch_context_consecutive_context_lines() {
    let test_dir = setup_test_dir("context_consecutive");

    let target = test_dir.join("f.txt");
    fs::write(&target, "c1\nc2\nc3\nOLD\nc4\nc5\nc6\n").unwrap();

    let patch_file = test_dir.join("c.patch");
    fs::write(
        &patch_file,
        "*** f.txt\n--- f.txt\n***************\n\
         *** 1,7 ****\n  c1\n  c2\n  c3\n! OLD\n  c4\n  c5\n  c6\n\
         --- 1,7 ----\n  c1\n  c2\n  c3\n! NEW\n  c4\n  c5\n  c6\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-c"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(
        fs::read_to_string(&target).unwrap(),
        "c1\nc2\nc3\nNEW\nc4\nc5\nc6\n"
    );

    cleanup_test_dir(&test_dir);
}

// A reject file must be readable by patch. A unified-style "--- "/"+++ " header
// above context-format hunks made our own detector read the file as a unified
// diff containing no hunks at all, so re-applying it silently did nothing.
#[test]
fn test_patch_reject_file_is_reapplicable() {
    let test_dir = setup_test_dir("reject_reapply");

    let target = test_dir.join("t.txt");
    fs::write(&target, "a\nb\nc\n").unwrap();

    let patch_file = test_dir.join("t.patch");
    fs::write(
        &patch_file,
        "--- t.txt\n+++ t.txt\n@@ -1,3 +1,3 @@\n a\n-NOMATCH\n+B\n c\n",
    )
    .unwrap();

    let (code, _) = run_patch_capture(vec![
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
        target.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 1);

    let rej = test_dir.join("t.txt.rej");
    assert!(rej.exists());

    // Feeding the reject file back in must report the failed hunk, not exit 0
    // having quietly parsed nothing.
    let (code, err) = run_patch_capture(vec![
        String::from("-i"),
        rej.to_str().unwrap().to_string(),
        target.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 1, "reject file should be readable, stderr: {}", err);
    assert!(err.contains("FAILED"), "got: {}", err);

    cleanup_test_dir(&test_dir);
}

// An ed script's text block is unprefixed file content, so it can contain a
// line that reads as another format's command. The format whose marker comes
// first is the one that describes the file.
#[test]
fn test_patch_ed_text_block_does_not_hijack_detection() {
    let test_dir = setup_test_dir("ed_hijack");

    let target = test_dir.join("f.txt");
    fs::write(&target, "alpha\nbeta\ngamma\n").unwrap();

    let patch_file = test_dir.join("e.patch");
    // Appends the literal text "1c1", which reads as a normal-diff command.
    fs::write(&patch_file, "2a\n1c1\n.\n").unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(
        fs::read_to_string(&target).unwrap(),
        "alpha\nbeta\n1c1\ngamma\n"
    );

    cleanup_test_dir(&test_dir);
}

// A patch that applies nothing must leave the file completely alone: not
// rewritten with identical bytes, and not backed up under -b.
#[test]
fn test_patch_declined_patch_does_not_touch_file() {
    let test_dir = setup_test_dir("declined_untouched");

    let target = test_dir.join("w.txt");
    fs::write(&target, "new\n").unwrap();
    let before = fs::metadata(&target).unwrap().modified().unwrap();

    let patch_file = test_dir.join("u.patch");
    // Already applied, so it is detected as reversed and declined.
    fs::write(
        &patch_file,
        "--- w.txt\n+++ w.txt\n@@ -1,1 +1,1 @@\n-old\n+new\n",
    )
    .unwrap();

    let (code, _) = run_patch_capture(vec![
        String::from("-b"),
        String::from("-i"),
        patch_file.to_str().unwrap().to_string(),
        target.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 1);

    assert_eq!(fs::read_to_string(&target).unwrap(), "new\n");
    assert_eq!(
        fs::metadata(&target).unwrap().modified().unwrap(),
        before,
        "an unapplied patch must not rewrite the file"
    );
    assert!(
        !test_dir.join("w.txt.orig").exists(),
        "no backup for a patch that changed nothing"
    );
    assert!(test_dir.join("w.txt.rej").exists());

    cleanup_test_dir(&test_dir);
}

// A section body is capped at the length its range declares, but the
// "\ No newline at end of file" marker follows that last content line. Applying
// the cap first left the marker unconsumed, so the new-section header was never
// reached and the whole hunk was silently dropped.
#[test]
fn test_patch_context_no_newline_marker_after_full_section() {
    let test_dir = setup_test_dir("context_nonl_cap");

    let target = test_dir.join("f.txt");
    fs::write(&target, "a\nb\nc\nd").unwrap(); // no trailing newline

    let patch_file = test_dir.join("c.patch");
    // The old section declares 4 lines and is followed immediately by the
    // no-newline marker; a second hunk proves parsing continued past it.
    fs::write(
        &patch_file,
        "*** f.txt\n--- f.txt\n***************\n\
         *** 1,4 ****\n  a\n  b\n  c\n- d\n\\ No newline at end of file\n\
         --- 1,4 ----\n  a\n  b\n  c\n+ D\n\\ No newline at end of file\n",
    )
    .unwrap();

    run_test(TestPlan {
        cmd: String::from("patch"),
        args: vec![
            String::from("-c"),
            String::from("-i"),
            patch_file.to_str().unwrap().to_string(),
            target.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert_eq!(fs::read(&target).unwrap(), b"a\nb\nc\nD");

    cleanup_test_dir(&test_dir);
}

// A hunk that removes nothing and adds nothing must leave the file exactly as
// it is. Sitting at end of file, such a hunk used to flip the trailing-newline
// marker, adding a newline to a file it had not otherwise touched.
#[test]
fn test_patch_noop_hunk_at_eof_leaves_file_alone() {
    let test_dir = setup_test_dir("noop_eof");

    // An ed "a" command with an empty text block, plain and under -D, and the
    // degenerate unified spelling of the same thing.
    let cases: [(&str, &str, Vec<String>); 3] = [
        ("ed.txt", "2a\n.\n", vec![String::from("-e")]),
        (
            "ed_d.txt",
            "2a\n.\n",
            vec![
                String::from("-e"),
                String::from("-D"),
                String::from("FEATURE"),
            ],
        ),
        (
            "uni.txt",
            "--- uni.txt\n+++ uni.txt\n@@ -2,0 +3,0 @@\n",
            vec![],
        ),
    ];

    for (name, patch_text, extra) in cases {
        let target = test_dir.join(name);
        fs::write(&target, "a\nb").unwrap(); // no trailing newline

        let patch_file = test_dir.join(format!("{}.patch", name));
        fs::write(&patch_file, patch_text).unwrap();

        let mut args = extra;
        args.push(String::from("-i"));
        args.push(patch_file.to_str().unwrap().to_string());
        args.push(target.to_str().unwrap().to_string());
        let (code, err) = run_patch_capture(args);

        assert_eq!(code, 0, "{}: stderr: {}", name, err);
        assert_eq!(
            fs::read(&target).unwrap(),
            b"a\nb",
            "{}: a no-op hunk must not add a trailing newline",
            name
        );
    }

    cleanup_test_dir(&test_dir);
}
