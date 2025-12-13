//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Option tests (listopt, format options)

use crate::common::*;
use std::fs::{self, File};
use std::io::Write;
use tempfile::TempDir;

#[test]
fn test_option_listopt_filename() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("myfile.txt")).unwrap();
    writeln!(f, "Hello").unwrap();

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // List with custom format showing just filename
    let output = run_pax(&["-f", archive.to_str().unwrap(), "-o", "listopt=%f"]);
    assert_success(&output, "pax list with listopt");

    let listing = stdout_str(&output);
    assert!(
        listing.contains("myfile.txt"),
        "Listing should contain filename"
    );
}

#[test]
fn test_option_listopt_size() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file with known content
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("sized.txt")).unwrap();
    write!(f, "12345").unwrap(); // exactly 5 bytes

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // List with custom format showing size
    let output = run_pax(&["-f", archive.to_str().unwrap(), "-o", "listopt=%f:%s"]);
    assert_success(&output, "pax list with listopt");

    let listing = stdout_str(&output);
    assert!(
        listing.contains("sized.txt:5"),
        "Listing should show filename and size (got: {})",
        listing
    );
}

#[test]
fn test_option_listopt_path_and_mode() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source with subdirectory
    fs::create_dir(&src_dir).unwrap();
    let subdir = src_dir.join("subdir");
    fs::create_dir(&subdir).unwrap();
    let mut f = File::create(subdir.join("nested.txt")).unwrap();
    writeln!(f, "Nested").unwrap();

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // List with custom format showing full path and mode
    let output = run_pax(&["-f", archive.to_str().unwrap(), "-o", "listopt=%M %F"]);
    assert_success(&output, "pax list with listopt");

    let listing = stdout_str(&output);
    // Should have the full path with directory
    assert!(
        listing.contains("subdir/nested.txt"),
        "Listing should show full path (got: {})",
        listing
    );
    // Should have mode characters (r, w, x, or -)
    assert!(
        listing.contains("rw") || listing.contains("r-"),
        "Listing should show mode bits"
    );
}

#[test]
fn test_option_listopt_owner_group() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("owned.txt")).unwrap();
    writeln!(f, "Owner test").unwrap();

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // List with custom format showing owner/group
    let output = run_pax(&["-f", archive.to_str().unwrap(), "-o", "listopt=%u/%g %f"]);
    assert_success(&output, "pax list with listopt");

    let listing = stdout_str(&output);
    // Should have owner/group (at least a slash separator)
    assert!(
        listing.contains("/"),
        "Listing should show owner/group separator"
    );
    assert!(
        listing.contains("owned.txt"),
        "Listing should show filename"
    );
}

#[test]
fn test_option_listopt_with_literal() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("literal.txt")).unwrap();
    writeln!(f, "Literal test").unwrap();

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // List with custom format including literal text
    let output = run_pax(&[
        "-f",
        archive.to_str().unwrap(),
        "-o",
        "listopt=FILE: %f SIZE: %s bytes",
    ]);
    assert_success(&output, "pax list with listopt");

    let listing = stdout_str(&output);
    assert!(
        listing.contains("FILE:") && listing.contains("SIZE:") && listing.contains("bytes"),
        "Listing should include literal text (got: {})",
        listing
    );
}

#[test]
fn test_option_cpio_format() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.cpio");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("cpio_test.txt")).unwrap();
    writeln!(f, "CPIO format test").unwrap();

    // Create archive with cpio format using -x option
    let output = run_pax_in_dir(
        &["-w", "-x", "cpio", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write cpio format");

    // Extract and verify
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read cpio format");

    let content = fs::read_to_string(dst_dir.join("cpio_test.txt")).unwrap();
    assert!(content.contains("CPIO format test"));
}

#[test]
fn test_option_times() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("times_test.txt")).unwrap();
    writeln!(f, "Times test").unwrap();

    // Create archive with -o times option (pax format to use extended headers)
    let output = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "pax",
            "-o",
            "times",
            "-f",
            archive.to_str().unwrap(),
            ".",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write with times option");

    // Extract and verify (file content should be preserved)
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read with times");

    let content = fs::read_to_string(dst_dir.join("times_test.txt")).unwrap();
    assert!(content.contains("Times test"));
}

#[test]
fn test_option_linkdata() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source files - a file and a hard link to it
    fs::create_dir(&src_dir).unwrap();
    let file1 = src_dir.join("original.txt");
    let mut f = File::create(&file1).unwrap();
    writeln!(f, "Original content for linkdata test").unwrap();
    drop(f);

    // Create hard link
    let file2 = src_dir.join("hardlink.txt");
    fs::hard_link(&file1, &file2).unwrap();

    // Create archive with -o linkdata option
    let output = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "pax",
            "-o",
            "linkdata",
            "-f",
            archive.to_str().unwrap(),
            "original.txt",
            "hardlink.txt",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write with linkdata option");

    // Extract and verify both files have content
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read with linkdata");

    // Both files should have the same content
    let content1 = fs::read_to_string(dst_dir.join("original.txt")).unwrap();
    let content2 = fs::read_to_string(dst_dir.join("hardlink.txt")).unwrap();
    assert!(content1.contains("Original content for linkdata test"));
    assert!(content2.contains("Original content for linkdata test"));
}

#[test]
fn test_option_invalid_bypass() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file with valid UTF-8 name
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("valid_name.txt")).unwrap();
    writeln!(f, "Valid filename test").unwrap();

    // Create archive with -o invalid=bypass (default behavior, shouldn't affect valid names)
    let output = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "pax",
            "-o",
            "invalid=bypass",
            "-f",
            archive.to_str().unwrap(),
            ".",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write with invalid=bypass option");

    // Verify the archive was created
    assert!(archive.exists(), "Archive should be created");
}

#[test]
fn test_option_invalid_write() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("test_file.txt")).unwrap();
    writeln!(f, "Test file content").unwrap();

    // Create archive with -o invalid=write
    let output = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "pax",
            "-o",
            "invalid=write",
            "-f",
            archive.to_str().unwrap(),
            ".",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write with invalid=write option");

    // Extract and verify
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read after invalid=write");

    let content = fs::read_to_string(dst_dir.join("test_file.txt")).unwrap();
    assert!(content.contains("Test file content"));
}

#[test]
fn test_option_global_keyword_value() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("global_test.txt")).unwrap();
    writeln!(f, "Global keyword test").unwrap();

    // Create archive with -o keyword=value (global extended header)
    let output = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "pax",
            "-o",
            "comment=This is a test archive",
            "-f",
            archive.to_str().unwrap(),
            ".",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write with global keyword=value");

    // Extract and verify file content is preserved
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read with global header");

    let content = fs::read_to_string(dst_dir.join("global_test.txt")).unwrap();
    assert!(content.contains("Global keyword test"));
}

#[test]
fn test_option_per_file_keyword_override() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("perfile_test.txt")).unwrap();
    writeln!(f, "Per-file keyword test").unwrap();

    // Create archive with -o keyword:=value (per-file extended header)
    let output = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "pax",
            "-o",
            "gname:=testgroup",
            "-f",
            archive.to_str().unwrap(),
            ".",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write with per-file keyword:=value");

    // Extract and verify file content is preserved
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read with per-file header");

    let content = fs::read_to_string(dst_dir.join("perfile_test.txt")).unwrap();
    assert!(content.contains("Per-file keyword test"));
}

#[test]
fn test_option_delete_pattern() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file with long path (to trigger extended header)
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("delete_test.txt")).unwrap();
    writeln!(f, "Delete pattern test").unwrap();

    // Create archive with -o delete=mtime (delete mtime from extended headers)
    let output = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "pax",
            "-o",
            "times",
            "-o",
            "delete=atime",
            "-f",
            archive.to_str().unwrap(),
            ".",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write with delete pattern");

    // Extract and verify file content is preserved
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read after delete pattern");

    let content = fs::read_to_string(dst_dir.join("delete_test.txt")).unwrap();
    assert!(content.contains("Delete pattern test"));
}

#[test]
fn test_option_multiple_combined() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("combined.txt")).unwrap();
    writeln!(f, "Combined options test").unwrap();

    // Create archive with multiple -o options combined
    let output = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "pax",
            "-o",
            "times,comment=test comment,gname:=override",
            "-f",
            archive.to_str().unwrap(),
            ".",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write with multiple combined options");

    // Extract and verify
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read with combined options");

    let content = fs::read_to_string(dst_dir.join("combined.txt")).unwrap();
    assert!(content.contains("Combined options test"));
}
