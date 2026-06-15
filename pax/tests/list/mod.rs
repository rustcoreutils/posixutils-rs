//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! List mode tests

use crate::common::*;
use std::fs::{self, File};
use std::io::Write;
use tempfile::TempDir;

#[test]
fn test_list_mode() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    create_test_files(&src_dir);

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List archive contents
    let output = run_pax(&["-f", archive.to_str().unwrap()]);
    assert_success(&output, "pax list");

    let listing = stdout_str(&output);
    assert!(listing.contains("file.txt"), "Missing file.txt in listing");
    assert!(
        listing.contains("subdir/nested.txt") || listing.contains("subdir"),
        "Missing subdir in listing"
    );
}

#[test]
fn test_verbose_list() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    create_test_files(&src_dir);

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // List archive with verbose mode
    let output = run_pax(&["-v", "-f", archive.to_str().unwrap()]);
    assert_success(&output, "pax verbose list");

    let listing = stdout_str(&output);
    // Verbose output should contain permission strings like "rw-"
    assert!(
        listing.contains("r") && listing.contains("-"),
        "Verbose listing missing permission info"
    );
}

#[test]
fn test_pattern_matching() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    create_test_files(&src_dir);

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // Extract only .txt files
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap(), "*.txt"], &dst_dir);
    assert_success(&output, "pax pattern extract");

    // file.txt should be extracted
    assert!(
        dst_dir.join("file.txt").exists(),
        "file.txt should be extracted"
    );
}

#[test]
fn test_no_clobber() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("file.txt")).unwrap();
    writeln!(f, "Original content").unwrap();

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // Create destination with existing file
    fs::create_dir(&dst_dir).unwrap();
    let mut f = File::create(dst_dir.join("file.txt")).unwrap();
    writeln!(f, "Existing content").unwrap();

    // Extract with -k (no clobber)
    let output = run_pax_in_dir(&["-r", "-k", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax no-clobber extract");

    // Original file should be preserved
    let content = fs::read_to_string(dst_dir.join("file.txt")).unwrap();
    assert!(
        content.contains("Existing"),
        "File was overwritten despite -k"
    );
}

#[test]
fn test_pax_list() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.pax");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    create_test_files(&src_dir);

    // Create archive using pax format
    let output = run_pax_in_dir(
        &["-w", "-x", "pax", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List contents
    let output = run_pax(&["-f", archive.to_str().unwrap()]);
    assert_success(&output, "pax list");

    let listing = stdout_str(&output);
    assert!(listing.contains("file.txt"), "Missing file.txt");
}

#[test]
fn test_pax_verbose_list() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.pax");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    create_test_files(&src_dir);

    // Create archive using pax format
    run_pax_in_dir(
        &["-w", "-x", "pax", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // Verbose list
    let output = run_pax(&["-v", "-f", archive.to_str().unwrap()]);
    assert_success(&output, "pax verbose list");

    let listing = stdout_str(&output);
    // Should have permissions
    assert!(listing.contains("r") || listing.contains("-"));
}

/// A directory-name pattern selects the whole subtree by default; `-d` restricts
/// the match to the directory member itself.
#[test]
fn test_list_directory_subtree_and_dash_d() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("c.tar");

    fs::create_dir_all(src_dir.join("dir/sub")).unwrap();
    File::create(src_dir.join("dir/sub/f"))
        .unwrap()
        .write_all(b"a")
        .unwrap();
    File::create(src_dir.join("dir/g"))
        .unwrap()
        .write_all(b"b")
        .unwrap();

    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "dir"],
        &src_dir,
    );

    // Default: the directory pattern selects the whole hierarchy.
    let out = run_pax(&["-f", archive.to_str().unwrap(), "dir"]);
    assert_success(&out, "list dir subtree");
    let listing = stdout_str(&out);
    assert!(
        listing.contains("dir/sub/f"),
        "subtree should be listed: {listing}"
    );
    assert!(
        listing.contains("dir/g"),
        "subtree should be listed: {listing}"
    );

    // With -d, only the directory member itself matches.
    let out = run_pax(&["-d", "-f", archive.to_str().unwrap(), "dir"]);
    assert_success(&out, "list dir with -d");
    let listing = stdout_str(&out);
    assert!(
        listing.lines().any(|l| l == "dir/" || l == "dir"),
        "the directory itself should be listed: {listing:?}"
    );
    assert!(
        !listing.contains("dir/sub/f") && !listing.contains("dir/g"),
        "-d must not list the subtree: {listing}"
    );
}
