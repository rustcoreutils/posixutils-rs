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
fn test_option_listopt_mode_precision() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("mode.txt")).unwrap();
    writeln!(f, "Mode test").unwrap();

    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    let output = run_pax(&["-f", archive.to_str().unwrap(), "-o", "listopt=%.1M"]);
    assert_success(&output, "pax list with listopt=%.1M");

    let listing = stdout_str(&output);
    let lines: Vec<&str> = listing.lines().filter(|line| !line.is_empty()).collect();
    assert!(!lines.is_empty(), "Listing should contain entries");
    for line in lines {
        assert_eq!(line.len(), 1, "Listing should be single char");
        assert!(
            matches!(
                line.chars().next(),
                Some('-')
                    | Some('d')
                    | Some('l')
                    | Some('b')
                    | Some('c')
                    | Some('p')
                    | Some('s')
                    | Some('h')
            ),
            "Listing should be entry type character (got: {})",
            line
        );
    }
}

#[test]
fn test_option_listopt_mode_precision_stdin() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("mode_stdin.txt")).unwrap();
    writeln!(f, "Mode stdin test").unwrap();

    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    let archive_data = fs::read(&archive).expect("Failed to read archive");
    let output = run_pax_with_stdin_bytes(&["-o", "listopt=%.1M"], &archive_data);
    assert_success(&output, "pax list with listopt=%.1M via stdin");

    let listing = stdout_str(&output);
    let lines: Vec<&str> = listing.lines().filter(|line| !line.is_empty()).collect();
    assert!(!lines.is_empty(), "Listing should contain entries");
    for line in lines {
        assert_eq!(line.len(), 1, "Listing should be single char");
        assert!(
            matches!(
                line.chars().next(),
                Some('-')
                    | Some('d')
                    | Some('l')
                    | Some('b')
                    | Some('c')
                    | Some('p')
                    | Some('s')
                    | Some('h')
            ),
            "Listing should be entry type character (got: {})",
            line
        );
    }
}

#[test]
fn test_option_listopt_oversized_width_precision() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("oversized.txt")).unwrap();
    writeln!(f, "Oversized format test").unwrap();

    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // Test with extremely large width that would cause OOM without clamping
    let output = run_pax(&[
        "-f",
        archive.to_str().unwrap(),
        "-o",
        "listopt=%9999999999999999p",
    ]);
    assert_success(&output, "pax list with oversized width should not OOM");

    let listing = stdout_str(&output);
    // Should complete without OOM and produce reasonable output
    assert!(!listing.is_empty(), "Listing should not be empty");
    // Width should be clamped to MAX_FORMAT_FIELD_SIZE (4096)
    // Each line should not exceed 4096 + reasonable path length
    for line in listing.lines().filter(|l| !l.is_empty()) {
        assert!(
            line.len() <= 8192,
            "Line should not exceed reasonable length (got {})",
            line.len()
        );
    }

    // Test with extremely large precision
    let output = run_pax(&[
        "-f",
        archive.to_str().unwrap(),
        "-o",
        "listopt=%.9999999999999999M",
    ]);
    assert_success(&output, "pax list with oversized precision should not OOM");

    let listing = stdout_str(&output);
    assert!(!listing.is_empty(), "Listing should not be empty");
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

/// Test that %M shows correct file type character for regular files (issue #531)
/// Previously showed `?rw-r--r--` instead of `-rw-r--r--` because tar stores
/// file type in typeflag, not in mode bits.
#[test]
fn test_option_listopt_mode_symbolic_regular_file() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create a regular file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("regular.txt")).unwrap();
    writeln!(f, "Regular file test").unwrap();

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // List with %M - should show '-' for regular file, not '?'
    let output = run_pax(&["-f", archive.to_str().unwrap(), "-o", "listopt=%M %f"]);
    assert_success(&output, "pax list with listopt=%M");

    let listing = stdout_str(&output);
    // Regular file should start with '-', not '?'
    assert!(
        listing.contains("-rw") || listing.contains("-r-"),
        "Regular file mode should start with '-', not '?'. Got: {}",
        listing
    );
    assert!(
        !listing.contains("?rw") && !listing.contains("?r-"),
        "Regular file mode should NOT start with '?'. Got: {}",
        listing
    );
}

/// Test that %M shows correct file type character for directories (issue #531)
#[test]
fn test_option_listopt_mode_symbolic_directory() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create a directory structure
    fs::create_dir(&src_dir).unwrap();
    fs::create_dir(src_dir.join("subdir")).unwrap();
    let mut f = File::create(src_dir.join("subdir").join("file.txt")).unwrap();
    writeln!(f, "test").unwrap();

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // List with %M - should show 'd' for directory
    let output = run_pax(&["-f", archive.to_str().unwrap(), "-o", "listopt=%M %f"]);
    assert_success(&output, "pax list with listopt=%M for directory");

    let listing = stdout_str(&output);
    // Directory should start with 'd'
    assert!(
        listing.contains("drwx") || listing.contains("dr-x"),
        "Directory mode should start with 'd'. Got: {}",
        listing
    );
}

/// Test that %D format specifier works (issue #531)
/// Previously showed literal `%D` instead of device major,minor
#[test]
fn test_option_listopt_device_specifier() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create a regular file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("device_test.txt")).unwrap();
    writeln!(f, "Device test").unwrap();

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // List with %D - should show "major,minor" format, not literal "%D"
    let output = run_pax(&["-f", archive.to_str().unwrap(), "-o", "listopt=%D %f"]);
    assert_success(&output, "pax list with listopt=%D");

    let listing = stdout_str(&output);
    // For regular files, devmajor and devminor are 0, so should show "0,0"
    assert!(
        listing.contains("0,0"),
        "Device specifier should show 'major,minor' format (0,0 for regular files), not literal '%D'. Got: {}",
        listing
    );
    assert!(
        !listing.contains("%D"),
        "Should NOT show literal '%D'. Got: {}",
        listing
    );
}

/// Test %M and %D together in a format string (issue #531)
#[test]
fn test_option_listopt_mode_and_device_combined() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create a regular file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("combined.txt")).unwrap();
    writeln!(f, "Combined test").unwrap();

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // List with %M %D %s %f - comprehensive format
    let output = run_pax(&["-f", archive.to_str().unwrap(), "-o", "listopt=%M %D %s %f"]);
    assert_success(&output, "pax list with listopt=%M %D %s %f");

    let listing = stdout_str(&output);

    // Should have correct mode (- for regular file)
    assert!(
        listing.contains("-rw") || listing.contains("-r-"),
        "Should show '-' for regular file mode. Got: {}",
        listing
    );

    // Should have device info (0,0 for regular files)
    assert!(
        listing.contains("0,0"),
        "Should show device info. Got: {}",
        listing
    );

    // Should have size and filename
    assert!(
        listing.contains("combined.txt"),
        "Should show filename. Got: {}",
        listing
    );
}

#[test]
fn test_first_match_option() {
    // Test -n flag: select only the first archive member that matches each pattern
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source files - multiple files matching *.txt pattern
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("a.txt")).unwrap();
    writeln!(f, "File A").unwrap();
    let mut f = File::create(src_dir.join("b.txt")).unwrap();
    writeln!(f, "File B").unwrap();
    let mut f = File::create(src_dir.join("c.txt")).unwrap();
    writeln!(f, "File C").unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List with -n and pattern - should only show first match
    let output = run_pax(&["-n", "-f", archive.to_str().unwrap(), "*.txt"]);
    assert_success(&output, "pax list with -n");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let matching_lines: Vec<&str> = stdout.lines().filter(|l| l.ends_with(".txt")).collect();

    // With -n, only one .txt file should be listed (the first match)
    assert_eq!(
        matching_lines.len(),
        1,
        "Expected exactly 1 .txt file with -n flag, got {}: {:?}",
        matching_lines.len(),
        matching_lines
    );

    // Extract with -n - should only extract first match
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(
        &["-r", "-n", "-f", archive.to_str().unwrap(), "*.txt"],
        &dst_dir,
    );
    assert_success(&output, "pax read with -n");

    // Count extracted .txt files - should be exactly 1
    let mut txt_count = 0;
    for entry in fs::read_dir(&dst_dir).unwrap() {
        let entry = entry.unwrap();
        if entry
            .path()
            .extension()
            .map(|e| e == "txt")
            .unwrap_or(false)
        {
            txt_count += 1;
        }
    }

    assert_eq!(
        txt_count, 1,
        "Expected exactly 1 .txt file extracted with -n flag, got {}",
        txt_count
    );
}
