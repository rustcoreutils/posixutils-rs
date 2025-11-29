//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Append mode tests (-a)

use crate::common::*;
use std::fs::{self, File};
use std::io::Write;
use tempfile::TempDir;

#[test]
fn test_append_mode_basic() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("file1.txt")).unwrap();
    writeln!(f, "First file").unwrap();

    // Create initial archive with file1.txt
    let output = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "file1.txt",
        ],
        &src_dir,
    );
    assert_success(&output, "pax create");

    // Create second file
    let mut f = File::create(src_dir.join("file2.txt")).unwrap();
    writeln!(f, "Second file").unwrap();

    // Append file2.txt to archive
    let output = run_pax_in_dir(
        &["-w", "-a", "-f", archive.to_str().unwrap(), "file2.txt"],
        &src_dir,
    );
    assert_success(&output, "pax append");

    // List archive - should have both files
    let output = run_pax(&["-f", archive.to_str().unwrap()]);
    assert_success(&output, "pax list");

    let listing = stdout_str(&output);
    assert!(
        listing.contains("file1.txt"),
        "file1.txt should be in archive"
    );
    assert!(
        listing.contains("file2.txt"),
        "file2.txt should be appended"
    );
}

#[test]
fn test_append_mode_extract() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("original.txt")).unwrap();
    writeln!(f, "Original content").unwrap();

    // Create initial archive
    let output = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "original.txt",
        ],
        &src_dir,
    );
    assert_success(&output, "pax create");

    // Create appended file
    let mut f = File::create(src_dir.join("appended.txt")).unwrap();
    writeln!(f, "Appended content").unwrap();

    // Append to archive
    let output = run_pax_in_dir(
        &["-w", "-a", "-f", archive.to_str().unwrap(), "appended.txt"],
        &src_dir,
    );
    assert_success(&output, "pax append");

    // Extract archive
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax extract");

    // Verify both files were extracted with correct content
    let content1 = fs::read_to_string(dst_dir.join("original.txt")).unwrap();
    assert!(
        content1.contains("Original"),
        "Original file content mismatch"
    );

    let content2 = fs::read_to_string(dst_dir.join("appended.txt")).unwrap();
    assert!(
        content2.contains("Appended"),
        "Appended file content mismatch"
    );
}

#[test]
fn test_append_mode_multiple() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("file1.txt")).unwrap();
    writeln!(f, "File 1").unwrap();

    // Create initial archive
    run_pax_in_dir(
        &[
            "-w",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "file1.txt",
        ],
        &src_dir,
    );

    // Create and append multiple files in sequence
    for i in 2..=5 {
        let filename = format!("file{}.txt", i);
        let mut f = File::create(src_dir.join(&filename)).unwrap();
        writeln!(f, "File {}", i).unwrap();

        let output = run_pax_in_dir(
            &["-w", "-a", "-f", archive.to_str().unwrap(), &filename],
            &src_dir,
        );
        assert_success(&output, &format!("pax append {}", i));
    }

    // List archive - should have all 5 files
    let output = run_pax(&["-f", archive.to_str().unwrap()]);
    let listing = stdout_str(&output);

    for i in 1..=5 {
        let filename = format!("file{}.txt", i);
        assert!(
            listing.contains(&filename),
            "{} should be in archive",
            filename
        );
    }
}

#[test]
fn test_append_mode_directory() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source structure
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("file1.txt")).unwrap();
    writeln!(f, "Initial file").unwrap();

    // Create initial archive
    run_pax_in_dir(
        &[
            "-w",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "file1.txt",
        ],
        &src_dir,
    );

    // Create a directory with files to append
    let subdir = src_dir.join("subdir");
    fs::create_dir(&subdir).unwrap();
    let mut f = File::create(subdir.join("nested.txt")).unwrap();
    writeln!(f, "Nested file").unwrap();

    // Append directory
    let output = run_pax_in_dir(
        &["-w", "-a", "-f", archive.to_str().unwrap(), "subdir"],
        &src_dir,
    );
    assert_success(&output, "pax append directory");

    // List archive
    let output = run_pax(&["-f", archive.to_str().unwrap()]);
    let listing = stdout_str(&output);

    assert!(
        listing.contains("file1.txt"),
        "file1.txt should be in archive"
    );
    assert!(
        listing.contains("subdir") || listing.contains("nested.txt"),
        "subdir or nested.txt should be in archive"
    );
}

#[test]
fn test_append_mode_nonexistent_creates() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("new.tar");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("newfile.txt")).unwrap();
    writeln!(f, "New file").unwrap();

    // Verify archive doesn't exist
    assert!(!archive.exists(), "Archive should not exist yet");

    // Append to non-existent archive (should create it)
    let output = run_pax_in_dir(
        &["-w", "-a", "-f", archive.to_str().unwrap(), "newfile.txt"],
        &src_dir,
    );
    assert_success(&output, "pax append to new archive");

    // Verify archive was created
    assert!(archive.exists(), "Archive should be created");

    // List archive
    let output = run_pax(&["-f", archive.to_str().unwrap()]);
    let listing = stdout_str(&output);
    assert!(
        listing.contains("newfile.txt"),
        "newfile.txt should be in archive"
    );
}

#[test]
fn test_append_mode_pax_format() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.pax");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("pax1.txt")).unwrap();
    writeln!(f, "PAX file 1").unwrap();

    // Create initial pax archive
    let output = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "pax",
            "-f",
            archive.to_str().unwrap(),
            "pax1.txt",
        ],
        &src_dir,
    );
    assert_success(&output, "pax create");

    // Create second file
    let mut f = File::create(src_dir.join("pax2.txt")).unwrap();
    writeln!(f, "PAX file 2").unwrap();

    // Append to pax archive
    let output = run_pax_in_dir(
        &["-w", "-a", "-f", archive.to_str().unwrap(), "pax2.txt"],
        &src_dir,
    );
    assert_success(&output, "pax append to pax archive");

    // List archive
    let output = run_pax(&["-f", archive.to_str().unwrap()]);
    let listing = stdout_str(&output);

    assert!(
        listing.contains("pax1.txt"),
        "pax1.txt should be in archive"
    );
    assert!(listing.contains("pax2.txt"), "pax2.txt should be appended");
}

#[test]
fn test_append_verbose() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("first.txt")).unwrap();
    writeln!(f, "First").unwrap();

    // Create initial archive
    run_pax_in_dir(
        &[
            "-w",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "first.txt",
        ],
        &src_dir,
    );

    // Create file to append
    let mut f = File::create(src_dir.join("second.txt")).unwrap();
    writeln!(f, "Second").unwrap();

    // Append with verbose
    let output = run_pax_in_dir(
        &[
            "-w",
            "-a",
            "-v",
            "-f",
            archive.to_str().unwrap(),
            "second.txt",
        ],
        &src_dir,
    );
    assert_success(&output, "pax append verbose");

    // Verbose output should be on stderr
    let stderr = stderr_str(&output);
    assert!(
        stderr.contains("second.txt"),
        "Verbose output should show appended file"
    );
}
