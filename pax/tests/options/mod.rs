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
