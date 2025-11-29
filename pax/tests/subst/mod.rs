//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Substitution option tests (-s)

use crate::common::*;
use std::fs::{self, File};
use std::io::Write;
use tempfile::TempDir;

#[test]
fn test_subst_basic_list() {
    // Test -s option with list mode - replace "file" with "FILE"
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("file.txt")).unwrap();
    writeln!(f, "test content").unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "file.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List with substitution
    let output = run_pax(&["-f", archive.to_str().unwrap(), "-s", "/file/FILE/"]);
    assert_success(&output, "pax list");

    let stdout = stdout_str(&output);
    assert!(
        stdout.contains("FILE.txt"),
        "substitution not applied to list output: {}",
        stdout
    );
    assert!(
        !stdout.contains("file.txt"),
        "original name should not appear: {}",
        stdout
    );
}

#[test]
fn test_subst_basic_extract() {
    // Test -s option with extract mode - replace "file" with "renamed"
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("file.txt")).unwrap();
    writeln!(f, "test content").unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "file.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract with substitution
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(
        &[
            "-r",
            "-f",
            archive.to_str().unwrap(),
            "-s",
            "/file/renamed/",
        ],
        &dst_dir,
    );
    assert_success(&output, "pax extract");

    // Verify renamed file exists
    assert!(
        dst_dir.join("renamed.txt").exists(),
        "renamed.txt should exist"
    );
    assert!(
        !dst_dir.join("file.txt").exists(),
        "file.txt should NOT exist"
    );

    let content = fs::read_to_string(dst_dir.join("renamed.txt")).unwrap();
    assert!(content.contains("test content"), "content mismatch");
}

#[test]
fn test_subst_basic_write() {
    // Test -s option with write mode - add prefix to paths
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("file.txt")).unwrap();
    writeln!(f, "test content").unwrap();

    // Create archive with substitution to add prefix
    let output = run_pax_in_dir(
        &[
            "-w",
            "-f",
            archive.to_str().unwrap(),
            "-s",
            "/^/prefix\\//",
            "file.txt",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List archive to verify prefix was added
    let output = run_pax(&["-f", archive.to_str().unwrap()]);
    assert_success(&output, "pax list");

    let stdout = stdout_str(&output);
    assert!(
        stdout.contains("prefix/file.txt"),
        "prefix should be added: {}",
        stdout
    );
}

#[test]
fn test_subst_global_flag() {
    // Test -s option with 'g' flag for global replacement
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file with multiple 'a's in name
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("aaa.txt")).unwrap();
    writeln!(f, "test").unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "aaa.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List with global substitution
    let output = run_pax(&["-f", archive.to_str().unwrap(), "-s", "/a/X/g"]);
    assert_success(&output, "pax list");

    let stdout = stdout_str(&output);
    assert!(
        stdout.contains("XXX.txt"),
        "global replacement should replace all 'a': {}",
        stdout
    );
}

#[test]
fn test_subst_non_global() {
    // Test -s option without 'g' flag - only first occurrence
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file with multiple 'a's in name
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("aaa.txt")).unwrap();
    writeln!(f, "test").unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "aaa.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List without global flag
    let output = run_pax(&["-f", archive.to_str().unwrap(), "-s", "/a/X/"]);
    assert_success(&output, "pax list");

    let stdout = stdout_str(&output);
    assert!(
        stdout.contains("Xaa.txt"),
        "non-global should only replace first 'a': {}",
        stdout
    );
}

#[test]
fn test_subst_empty_result_skips_file() {
    // Test that substitution resulting in empty string skips the file
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create multiple source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("skip.txt")).unwrap();
    writeln!(f, "to be skipped").unwrap();

    let mut f = File::create(src_dir.join("keep.txt")).unwrap();
    writeln!(f, "to be kept").unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &[
            "-w",
            "-f",
            archive.to_str().unwrap(),
            "skip.txt",
            "keep.txt",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract with substitution that makes "skip.txt" empty
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(
        &["-r", "-f", archive.to_str().unwrap(), "-s", "/skip\\.txt//"],
        &dst_dir,
    );
    assert_success(&output, "pax extract");

    // Verify skip.txt was skipped
    assert!(
        !dst_dir.join("skip.txt").exists(),
        "skip.txt should be skipped"
    );
    // Verify keep.txt was extracted
    assert!(dst_dir.join("keep.txt").exists(), "keep.txt should exist");
}

#[test]
fn test_subst_alternate_delimiter() {
    // Test -s option with alternate delimiter (#)
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("old.txt")).unwrap();
    writeln!(f, "test").unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "old.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List with alternate delimiter
    let output = run_pax(&["-f", archive.to_str().unwrap(), "-s", "#old#new#"]);
    assert_success(&output, "pax list");

    let stdout = stdout_str(&output);
    assert!(
        stdout.contains("new.txt"),
        "alternate delimiter should work: {}",
        stdout
    );
}

#[test]
fn test_subst_multiple_s_options() {
    // Test multiple -s options (first match wins)
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("file.txt")).unwrap();
    writeln!(f, "test").unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "file.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List with multiple substitutions - first match wins
    let output = run_pax(&[
        "-f",
        archive.to_str().unwrap(),
        "-s",
        "/file/FIRST/",
        "-s",
        "/file/SECOND/",
    ]);
    assert_success(&output, "pax list");

    let stdout = stdout_str(&output);
    assert!(
        stdout.contains("FIRST.txt"),
        "first -s should win: {}",
        stdout
    );
    assert!(
        !stdout.contains("SECOND"),
        "second -s should not be used: {}",
        stdout
    );
}

#[test]
fn test_subst_suffix_removal() {
    // Test removing file extension with -s
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("document.txt")).unwrap();
    writeln!(f, "test").unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "document.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List with suffix removal using $ anchor
    let output = run_pax(&["-f", archive.to_str().unwrap(), "-s", "/\\.txt$//"]);
    assert_success(&output, "pax list");

    let stdout = stdout_str(&output);
    assert!(
        stdout.contains("document") && !stdout.contains(".txt"),
        "suffix should be removed: {}",
        stdout
    );
}

#[test]
fn test_subst_backreference() {
    // Test BRE backreferences with \( \) grouping
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file with format "name_version.txt"
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("hello_world.txt")).unwrap();
    writeln!(f, "test").unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "hello_world.txt"],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List with BRE backreference to swap parts
    // In BRE: \(...\) for grouping, \1 \2 for backreferences
    let output = run_pax(&[
        "-f",
        archive.to_str().unwrap(),
        "-s",
        "/\\([^_]*\\)_\\([^.]*\\)/\\2_\\1/",
    ]);
    assert_success(&output, "pax list");

    let stdout = stdout_str(&output);
    assert!(
        stdout.contains("world_hello.txt"),
        "backreference swap should work: {}",
        stdout
    );
}
