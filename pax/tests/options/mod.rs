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
use plib::tmp::TempDir;
use std::fs::{self, File};
use std::io::Write;

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
fn test_option_invalid_unimplemented_actions_are_refused() {
    // POSIX scopes -o invalid= to a value in an extended header record that
    // the destination cannot hold. `bypass` -- skip the member -- is what pax
    // does, so it is accepted. The other four each have to create or rename a
    // file and none is implemented; accepting one and doing nothing is the
    // failure worth avoiding, so each is refused by name.
    let temp = TempDir::new().unwrap();
    let archive = temp.path().join("test.tar");

    let ok = run_pax_in_dir(
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
        temp.path(),
    );
    assert_success(&ok, "invalid=bypass is what pax does");

    for action in ["write", "rename", "UTF-8", "binary"] {
        let output = run_pax_in_dir(
            &[
                "-w",
                "-x",
                "pax",
                "-o",
                &format!("invalid={action}"),
                "-f",
                archive.to_str().unwrap(),
                ".",
            ],
            temp.path(),
        );
        assert_failure(&output, &format!("invalid={action} should be refused"));
        assert!(
            stderr_str(&output).contains(action),
            "the diagnostic must name the action: {}",
            stderr_str(&output)
        );
    }
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
/// Previously showed literal `%D` instead of a device rendering.
///
/// POSIX rule 10 gives `D` as the device of a block/character special file;
/// where that does not apply and no keyword was given -- a regular file under a
/// bare `%D` -- the conversion is equivalent to a single <space>. (The
/// keyword form, `%(size)D`, is covered by
/// `test_option_listopt_device_conversion_falls_back_to_keyword`.)
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
    assert!(
        !listing.contains("%D"),
        "Should NOT show literal '%D'. Got: {}",
        listing
    );
    let line = listing
        .lines()
        .find(|l| l.ends_with("device_test.txt"))
        .unwrap_or_else(|| panic!("no line for the test file. Got: {}", listing));
    // One space from %D, one from the literal space in "%D %f".
    assert_eq!(
        line, "  device_test.txt",
        "a regular file's bare %D must render as a single space. Got: {:?}",
        line
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

    // %D is not applicable to a regular file and carries no keyword, so it
    // contributes a single space between the mode and the size (POSIX rule 10).
    assert!(
        !listing.contains("%D"),
        "Should NOT show literal '%D'. Got: {}",
        listing
    );
    assert!(
        listing.lines().any(|l| l.ends_with("  14 combined.txt")),
        "Should show mode, the space from %D, size and name. Got: {}",
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

// --- Exit status: diagnose-and-continue (POSIX CONSEQUENCES OF ERRORS) ---

/// An unmatched pattern operand in list mode must be diagnosed and yield a
/// non-zero exit status, while the matched members are still listed.
#[test]
fn test_exit_status_unmatched_list_pattern() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("present.txt")).unwrap();
    writeln!(f, "hello").unwrap();

    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    let output = run_pax(&["-f", archive.to_str().unwrap(), "present.txt", "nosuchfile"]);

    assert_failure(&output, "list with an unmatched pattern");
    let stderr = stderr_str(&output);
    assert!(
        stderr.contains("nosuchfile") && stderr.contains("not found"),
        "expected a 'not found' diagnostic for the unmatched pattern, got: {stderr}"
    );
    // The matched member is still listed.
    let stdout = stdout_str(&output);
    assert!(
        stdout.contains("present.txt"),
        "the matched member should still be listed, got: {stdout}"
    );
}

/// An unmatched pattern operand in read (extract) mode must be diagnosed and
/// yield a non-zero exit status, while the matched members are still extracted.
#[test]
fn test_exit_status_unmatched_read_pattern() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");
    let archive = temp.path().join("test.tar");

    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("present.txt")).unwrap();
    writeln!(f, "hello").unwrap();

    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(
        &[
            "-r",
            "-f",
            archive.to_str().unwrap(),
            "present.txt",
            "nosuchfile",
        ],
        &dst_dir,
    );

    assert_failure(&output, "extract with an unmatched pattern");
    let stderr = stderr_str(&output);
    assert!(
        stderr.contains("nosuchfile") && stderr.contains("not found"),
        "expected a 'not found' diagnostic, got: {stderr}"
    );
    // The matched member was still extracted.
    assert!(
        dst_dir.join("present.txt").exists(),
        "the matched member should still be extracted"
    );
}

/// A non-existent file operand in write mode must be diagnosed and yield a
/// non-zero exit status, while the valid operands are still archived.
#[test]
fn test_exit_status_missing_write_operand() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("real.txt")).unwrap();
    writeln!(f, "hello").unwrap();

    let output = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "real.txt",
            "ghost.txt",
        ],
        &src_dir,
    );

    assert_failure(&output, "write with a missing file operand");
    let stderr = stderr_str(&output);
    assert!(
        stderr.contains("ghost.txt"),
        "expected a diagnostic naming the missing operand, got: {stderr}"
    );

    // The valid operand was still archived: listing the archive shows it.
    let list = run_pax(&["-f", archive.to_str().unwrap()]);
    assert_success(&list, "list archive built despite missing operand");
    assert!(
        stdout_str(&list).contains("real.txt"),
        "the valid operand should have been archived"
    );
}

/// Extracting over a pre-existing directory blocks one member but the rest of
/// the archive must still extract, with a non-zero exit status overall.
#[test]
fn test_exit_status_extract_continues_after_failure() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");
    let archive = temp.path().join("test.tar");

    // Two regular files; "a.txt" sorts/stores before "b.txt".
    fs::create_dir(&src_dir).unwrap();
    File::create(src_dir.join("a.txt"))
        .unwrap()
        .write_all(b"aaa")
        .unwrap();
    File::create(src_dir.join("b.txt"))
        .unwrap()
        .write_all(b"bbb")
        .unwrap();

    run_pax_in_dir(
        &[
            "-w",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "a.txt",
            "b.txt",
        ],
        &src_dir,
    );

    // In the destination, pre-create "a.txt" as a non-empty *directory* so the
    // file member cannot be created over it, but "b.txt" still can.
    fs::create_dir(&dst_dir).unwrap();
    fs::create_dir(dst_dir.join("a.txt")).unwrap();
    File::create(dst_dir.join("a.txt").join("blocker"))
        .unwrap()
        .write_all(b"x")
        .unwrap();

    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);

    assert_failure(&output, "extract over a blocking directory");
    // The second member must still have been extracted.
    assert_eq!(
        fs::read(dst_dir.join("b.txt")).unwrap(),
        b"bbb",
        "the member after the failing one should still be extracted"
    );
}

// --- Phase 5: pax time fidelity + `-o` on read ---

/// `-o gname:=value` forces a gname extended record even when the entry carried
/// no gname of its own.
#[test]
fn test_pax_o_gname_override_emits_record() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("g.pax");

    fs::create_dir(&src_dir).unwrap();
    fs::write(src_dir.join("f"), b"hi").unwrap();

    let output = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "pax",
            "-o",
            "gname:=mygroup",
            "-f",
            archive.to_str().unwrap(),
            "f",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write with -o gname:=");

    let bytes = fs::read(&archive).unwrap();
    let needle = b"gname=mygroup";
    assert!(
        bytes.windows(needle.len()).any(|w| w == needle),
        "archive should contain a gname=mygroup extended record"
    );
}

/// `-o delete=mtime` on extract removes the extended mtime record so the
/// (whole-second) ustar header time is used, dropping the sub-second part that a
/// default extract preserves.
#[test]
fn test_pax_o_delete_mtime_on_extract() {
    use std::os::unix::fs::MetadataExt;
    use std::process::Command;

    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("a.pax");

    fs::create_dir(&src_dir).unwrap();
    let src_file = src_dir.join("f");
    fs::write(&src_file, b"hi").unwrap();
    // Stamp a precise sub-second mtime.
    let status = Command::new("touch")
        .args(["-d", "2020-01-01 12:00:00.123456789"])
        .arg(&src_file)
        .status()
        .unwrap();
    if !status.success() {
        eprintln!("skipping: touch with fractional time unsupported");
        return;
    }

    assert_success(
        &run_pax_in_dir(
            &["-w", "-x", "pax", "-f", archive.to_str().unwrap(), "f"],
            &src_dir,
        ),
        "pax write pax",
    );

    // Default extract preserves the nanoseconds.
    let d_default = temp.path().join("d_default");
    fs::create_dir(&d_default).unwrap();
    assert_success(
        &run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &d_default),
        "extract default",
    );
    let nsec_default = fs::metadata(d_default.join("f")).unwrap().mtime_nsec();
    assert_eq!(
        nsec_default, 123456789,
        "default extract must preserve sub-second mtime"
    );

    // delete=mtime falls back to the whole-second ustar time.
    let d_del = temp.path().join("d_del");
    fs::create_dir(&d_del).unwrap();
    assert_success(
        &run_pax_in_dir(
            &["-r", "-o", "delete=mtime", "-f", archive.to_str().unwrap()],
            &d_del,
        ),
        "extract delete=mtime",
    );
    let nsec_del = fs::metadata(d_del.join("f")).unwrap().mtime_nsec();
    assert_eq!(
        nsec_del, 0,
        "delete=mtime must drop the sub-second precision"
    );
}

/// Set a file's atime and mtime to fixed epoch seconds so listopt time
/// renderings are deterministic. Both instants are mid-year and mid-day, so the
/// year and month names are the same in every time zone the tests might run in.
#[cfg(unix)]
fn set_times(path: &std::path::Path, atime: i64, mtime: i64) {
    use std::os::unix::ffi::OsStrExt;
    let times = [
        libc::timespec {
            tv_sec: atime as libc::time_t,
            tv_nsec: 0,
        },
        libc::timespec {
            tv_sec: mtime as libc::time_t,
            tv_nsec: 0,
        },
    ];
    let c_path = std::ffi::CString::new(path.as_os_str().as_bytes()).unwrap();
    let rc = unsafe { libc::utimensat(libc::AT_FDCWD, c_path.as_ptr(), times.as_ptr(), 0) };
    assert_eq!(rc, 0, "utimensat: {}", std::io::Error::last_os_error());
}

/// 2003-06-15 12:00:00 UTC / 2003-07-16 12:00:00 UTC.
#[cfg(unix)]
const T_MTIME: i64 = 1_055_678_400;
#[cfg(unix)]
const T_ATIME: i64 = 1_058_356_800;

/// Build a one-file pax archive carrying atime/mtime/ctime records, and return
/// (archive path, file size).
#[cfg(unix)]
fn archive_with_times(temp: &TempDir) -> (std::path::PathBuf, usize) {
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("times.pax");
    fs::create_dir(&src_dir).unwrap();
    let body = b"listopt time test\n";
    fs::write(src_dir.join("timed.txt"), body).unwrap();
    set_times(&src_dir.join("timed.txt"), T_ATIME, T_MTIME);

    assert_success(
        &run_pax_in_dir(
            &[
                "-w",
                "-x",
                "pax",
                "-o",
                "times",
                "-f",
                archive.to_str().unwrap(),
                "timed.txt",
            ],
            &src_dir,
        ),
        "write pax archive with -o times",
    );
    (archive, body.len())
}

/// The zone the listing helpers below render times in.
///
/// T_ATIME and T_MTIME are instants in UTC, and pax renders a time in the
/// zone $TZ names. Without pinning it, every assertion about an hour -- and,
/// far enough east or west, about a day or month -- would hold only on a
/// machine whose local time is UTC.
#[cfg(unix)]
const LISTING_TZ: (&str, &str) = ("TZ", "UTC0");

#[cfg(unix)]
fn listopt(archive: &std::path::Path, format: &str) -> String {
    let out = run_pax_with_env(
        &[
            "-f",
            archive.to_str().unwrap(),
            "-o",
            &format!("listopt={}", format),
        ],
        &[LISTING_TZ],
    );
    assert_success(&out, "pax list with listopt");
    stdout_str(&out).trim_end().to_string()
}

/// POSIX rule 7 lists every pax extended-header keyword as usable in
/// `%(keyword)`, and the EXAMPLES section spells out `%(atime)T`. atime was
/// echoed back literally instead of being rendered.
#[cfg(unix)]
#[test]
fn test_option_listopt_atime_keyword() {
    let temp = TempDir::new().unwrap();
    let (archive, _) = archive_with_times(&temp);

    let atime = listopt(&archive, "%(atime)T");
    assert!(
        !atime.contains("%(atime)"),
        "%(atime)T must be rendered, not echoed literally (got: {})",
        atime
    );
    assert!(
        atime.contains("Jul") && atime.contains("2003"),
        "%(atime)T must render the archived access time (got: {})",
        atime
    );

    // ... and it must be the access time, not an alias for mtime.
    let mtime = listopt(&archive, "%(mtime)T");
    assert!(
        mtime.contains("Jun") && mtime.contains("2003"),
        "%(mtime)T must render the modification time (got: {})",
        mtime
    );
    assert_ne!(atime, mtime, "atime and mtime must render distinctly");
}

/// POSIX rule 8: the `T` conversion defaults to the mtime keyword and to the
/// `%b %e %H:%M %Y` subformat. Bare `%T` rendered ISO 8601 and neither form
/// emitted the year.
#[cfg(unix)]
#[test]
fn test_option_listopt_time_default_subformat() {
    let temp = TempDir::new().unwrap();
    let (archive, _) = archive_with_times(&temp);

    let bare = listopt(&archive, "%T");
    assert!(
        bare.contains("Jun") && bare.contains("2003") && bare.contains("12:00"),
        "bare %T must default to mtime in `%b %e %H:%M %Y` form (got: {})",
        bare
    );
    assert!(
        !bare.contains("2003-06"),
        "bare %T must not use ISO 8601 (got: {})",
        bare
    );
    assert_eq!(
        bare,
        listopt(&archive, "%(mtime)T"),
        "bare %T and %(mtime)T must agree"
    );
}

/// POSIX rule 8 also allows `(keyword=subformat)`, where subformat is a date
/// format. The subformat was parsed and then discarded.
#[cfg(unix)]
#[test]
fn test_option_listopt_time_subformat() {
    let temp = TempDir::new().unwrap();
    let (archive, _) = archive_with_times(&temp);

    let iso = listopt(&archive, "%(mtime=%Y-%m-%d)T");
    assert!(
        iso.starts_with("2003-06-1"),
        "the T subformat must be honored (got: {})",
        iso
    );
    let atime_year = listopt(&archive, "%(atime=%Y)T");
    assert_eq!(atime_year, "2003", "subformat must apply to atime too");
}

/// POSIX rule 10: `D` names the device of a block/character special file; when
/// that is not applicable and a keyword is given, it is equivalent to
/// `%(keyword)u`. For a regular file `%(size)D` rendered the (meaningless)
/// device pair instead of the size -- the exact spelling the spec's EXAMPLES
/// section uses.
#[cfg(unix)]
#[test]
fn test_option_listopt_device_conversion_falls_back_to_keyword() {
    let temp = TempDir::new().unwrap();
    let (archive, size) = archive_with_times(&temp);

    assert_eq!(
        listopt(&archive, "%(size)D"),
        size.to_string(),
        "%(size)D on a regular file must render the size"
    );

    // The full example from the pax EXAMPLES section must come out sensibly.
    let example = listopt(&archive, "%M %(atime)T %(size)D %(name)s");
    assert!(
        example.starts_with("-rw")
            && example.contains("Jul")
            && example.contains(&size.to_string())
            && example.ends_with("timed.txt"),
        "the POSIX EXAMPLES listopt must render every field (got: {})",
        example
    );
}

/// ctime is not a POSIX keyword, but `-o times` records one and listing can
/// report it -- the interop other implementations expect. There is no portable
/// way to set a file's ctime, so this asserts the record round-trips rather
/// than pinning an instant: `utimensat` above updated the inode, so the ctime
/// is "recently", and in particular it is neither of the fixed times.
#[cfg(unix)]
#[test]
fn test_option_listopt_ctime_keyword() {
    let temp = TempDir::new().unwrap();
    let (archive, _) = archive_with_times(&temp);

    let ctime = listopt(&archive, "%(ctime)T");
    assert!(
        !ctime.contains("%(ctime)") && !ctime.is_empty(),
        "-o times must record a ctime that %(ctime)T can render (got: {})",
        ctime
    );
    assert!(
        !ctime.contains("2003"),
        "ctime is the inode change time, not one of the archived 2003 stamps (got: {})",
        ctime
    );

    // The raw seconds form must agree with the rendered one.
    let secs = listopt(&archive, "%(ctime)d");
    assert!(
        secs.chars().all(|c| c.is_ascii_digit()) && !secs.is_empty(),
        "%(ctime)d must yield the raw seconds (got: {})",
        secs
    );
}

/// POSIX's `times` keyword is what obliges pax to write atime/mtime records, so
/// an archive written without it carries none. A `%(atime)T` against such an
/// archive names a keyword we understand that simply has no value here, so it
/// contributes nothing rather than echoing the specification back.
#[cfg(unix)]
#[test]
fn test_option_listopt_time_keyword_absent_from_archive() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("plain.pax");
    fs::create_dir(&src_dir).unwrap();
    fs::write(src_dir.join("plain.txt"), b"no times\n").unwrap();

    assert_success(
        &run_pax_in_dir(
            &[
                "-w",
                "-x",
                "pax",
                "-f",
                archive.to_str().unwrap(),
                "plain.txt",
            ],
            &src_dir,
        ),
        "write pax archive without -o times",
    );

    assert_eq!(
        listopt(&archive, "%(atime)T"),
        "",
        "an unrecorded atime must render empty"
    );
    // An unknown keyword still echoes, so the two cases stay distinguishable.
    assert_eq!(
        listopt(&archive, "%(nosuchkeyword)s"),
        "%(nosuchkeyword)s",
        "an unknown keyword must still be echoed literally"
    );
    // mtime is always available from the ustar header.
    assert!(
        listopt(&archive, "%(mtime)T").contains("20"),
        "mtime must still render without -o times"
    );
}

/// `-o delete=` and `-o keyword:=value` were honored on extract but silently
/// ignored on list: list mode built the pax reader without its options and
/// never applied the keyword overrides, so a listing could disagree with what
/// extracting the same archive would produce.
#[cfg(unix)]
#[test]
fn test_option_list_honors_keyword_override() {
    let temp = TempDir::new().unwrap();
    let (archive, _) = archive_with_times(&temp);

    // Forcing the path changes what the listing must report.
    let out = run_pax(&["-f", archive.to_str().unwrap(), "-o", "path:=forced.txt"]);
    assert_success(&out, "list with a keyword override");
    assert!(
        stdout_str(&out).lines().any(|l| l == "forced.txt"),
        "list mode must apply -o keyword:=value: {}",
        stdout_str(&out)
    );
}

/// `-o delete=atime` suppresses the record, so the listing must stop reporting
/// it -- the same keyword filtering extraction already performed.
#[cfg(unix)]
#[test]
fn test_option_list_honors_delete() {
    let temp = TempDir::new().unwrap();
    let (archive, _) = archive_with_times(&temp);

    assert!(
        !listopt_with(&archive, "%(atime)T", &["-o", "delete=atime"]).contains("Jul"),
        "-o delete=atime must remove the record from the listing too"
    );
    assert!(
        listopt(&archive, "%(atime)T").contains("Jul"),
        "and without it the atime is still reported"
    );
}

#[cfg(unix)]
fn listopt_with(archive: &std::path::Path, format: &str, extra: &[&str]) -> String {
    let mut args: Vec<String> = vec![
        "-f".into(),
        archive.to_str().unwrap().into(),
        "-o".into(),
        format!("listopt={}", format),
    ];
    args.extend(extra.iter().map(|s| s.to_string()));
    let refs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    let out = run_pax_with_env(&refs, &[LISTING_TZ]);
    assert_success(&out, "pax list with listopt");
    stdout_str(&out).trim_end().to_string()
}

/// The listopt format string for `archive`, which is given on stdin.
///
/// Hand-built fixtures reach fields a real write cannot set without root (a
/// device's major and minor) or without a 100-byte-plus pathname (`prefix`),
/// and pin the header-identity fields exactly.
fn listopt_bytes(archive: &[u8], format: &str) -> String {
    let out = run_pax_with_stdin_bytes(&["-o", &format!("listopt={}", format)], archive);
    assert_success(&out, "pax list with listopt");
    stdout_str(&out).trim_end().to_string()
}

/// POSIX listopt rule 7 requires every Field Name entry of the ustar Header
/// Block table as a `%(keyword)`. `magic`, `version`, `typeflag` and `chksum`
/// were echoed back as their own specification, because `parse_header`
/// discarded all four.
#[test]
fn test_option_listopt_ustar_header_identity_keywords() {
    let member = Ustar {
        name: b"hdr.txt",
        body: b"hi\n",
        ..Default::default()
    };
    let archive = member.archive();

    // The fixture computes its own checksum, so read the expectation out of
    // the header it built rather than hard-coding a sum of its bytes.
    let header = member.header();
    let field = std::str::from_utf8(&header[148..154]).unwrap();
    let stored = u32::from_str_radix(field, 8).expect("octal chksum field");

    assert_eq!(
        listopt_bytes(&archive, "%(magic)s|%(version)s|%(typeflag)s|%(chksum)s"),
        format!("ustar|00|0|{:o}", stored),
        "every ustar header-identity field must report what the header stored"
    );
}

/// Rule 7 admits `devmajor` and `devminor`, which `ListEntryInfo` already
/// carried for `%D` and which no keyword could reach. A hand-built character
/// special member gets there without `mknod`, so without root.
#[test]
fn test_option_listopt_device_keywords_need_no_privileges() {
    let archive = Ustar {
        name: b"chr",
        typeflag: b'3',
        devmajor: 8,
        devminor: 0,
        ..Default::default()
    }
    .archive();

    assert_eq!(
        listopt_bytes(&archive, "%(devmajor)s,%(devminor)s %D %M"),
        "8,0 8,0 crw-r--r--",
        "the device keywords must agree with the %D conversion"
    );
}

/// A pathname too long for the 100-byte `name` field lives in both halves of
/// the ustar spelling, and rule 11 makes `(prefix,name)` the fallback `%F`
/// uses -- so the halves have to rebuild exactly what `%F` prints.
#[test]
fn test_option_listopt_prefix_and_name_rebuild_the_path() {
    let prefix = vec![b'd'; 110];
    let archive = Ustar {
        name: b"deep.txt",
        prefix: &prefix,
        body: b"x\n",
        ..Default::default()
    }
    .archive();

    let dir = String::from_utf8(prefix).unwrap();
    assert_eq!(
        listopt_bytes(&archive, "%(prefix)s|%(name)s"),
        format!("{dir}|deep.txt")
    );
    assert_eq!(
        listopt_bytes(&archive, "%(prefix)s/%(name)s"),
        listopt_bytes(&archive, "%F"),
        "the two halves must reconstruct the pathname the listing reports"
    );
}

/// `name` is derived from the pathname the listing reports, not read back from
/// the stored field, so it follows the rewrites list mode applies first. A
/// stored value would disagree with `%F` on the same line.
#[test]
fn test_option_listopt_name_follows_a_path_rewrite() {
    let archive = Ustar {
        name: b"before.txt",
        body: b"x\n",
        ..Default::default()
    }
    .archive();

    let out = run_pax_with_stdin_bytes(
        &[
            "-s",
            ",^before.txt$,after.txt,",
            "-o",
            "listopt=%(name)s|%F",
        ],
        &archive,
    );
    assert_success(&out, "listopt with -s");
    assert_eq!(stdout_str(&out).trim_end(), "after.txt|after.txt");
}

/// POSIX listopt rule 7 requires every Field Name entry of the Octet-Oriented
/// cpio Archive Entry table as a `%(keyword)`, and permits the same names
/// without the leading `c_`. None of them resolved; all were echoed back.
#[test]
fn test_option_listopt_cpio_keywords() {
    let archive = CpioNewc {
        name: b"c.txt",
        body: b"hello\n",
        ino: 7,
        nlink: 2,
        mtime: 99,
        ..Default::default()
    }
    .archive();

    assert_eq!(
        listopt_bytes(
            &archive,
            "%(c_magic)s|%(c_ino)s|%(c_mode)s|%(c_nlink)s|%(c_mtime)s|\
             %(c_namesize)s|%(c_filesize)s|%(c_name)s|%(c_rdev)s"
        ),
        "070701|7|100644|2|99|6|6|c.txt|0"
    );

    // The unprefixed spellings rule 7 permits report the same values.
    assert_eq!(
        listopt_bytes(
            &archive,
            "%(ino)s|%(nlink)s|%(namesize)s|%(filesize)s|%(rdev)s"
        ),
        "7|2|6|6|0"
    );

    // A cpio header has no typeflag, version or chksum, so those report
    // nothing -- while a name in no table still echoes, so a typo is visible.
    assert_eq!(
        listopt_bytes(&archive, "[%(typeflag)s%(version)s%(chksum)s]%(c_bogus)s"),
        "[]%(c_bogus)s"
    );
}

/// The same keywords over an archive pax wrote itself, in the ODC format
/// POSIX defines and `-x cpio` selects.
#[test]
fn test_option_listopt_cpio_odc_keywords_round_trip() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.cpio");

    fs::create_dir(&src_dir).unwrap();
    fs::write(src_dir.join("odc.txt"), b"12345").unwrap();

    run_pax_in_dir(
        &[
            "-w",
            "-x",
            "cpio",
            "-f",
            archive.to_str().unwrap(),
            "odc.txt",
        ],
        &src_dir,
    );

    let out = run_pax(&[
        "-f",
        archive.to_str().unwrap(),
        "-o",
        "listopt=%(c_magic)s|%(c_mode)s|%(c_nlink)s|%(c_namesize)s|%(c_filesize)s|%(c_name)s",
    ]);
    assert_success(&out, "list an ODC archive with the cpio keywords");
    assert_eq!(
        stdout_str(&out).trim_end(),
        "070707|100644|1|8|5|odc.txt",
        "the POSIX ODC format identifies itself with 070707"
    );

    // c_ino comes from the file's own inode, so pin its shape rather than a
    // value: what matters is that the keyword resolves to a number.
    let ino = listopt(&archive, "%(c_ino)s");
    assert!(
        !ino.is_empty() && ino.bytes().all(|b| b.is_ascii_digit()),
        "%(c_ino)s must report the recorded inode (got {:?})",
        ino
    );
}

/// cpio stores a symbolic link's target as the member's data, so c_filesize
/// counts the target while the pax `size` keyword reports the entry's own
/// zero. The two keywords name different fields and must not be aliases.
#[cfg(unix)]
#[test]
fn test_option_listopt_cpio_symlink_filesize() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("link.cpio");

    fs::create_dir(&src_dir).unwrap();
    std::os::unix::fs::symlink("target", src_dir.join("l")).unwrap();

    run_pax_in_dir(
        &["-w", "-x", "cpio", "-f", archive.to_str().unwrap(), "l"],
        &src_dir,
    );

    let out = run_pax(&[
        "-f",
        archive.to_str().unwrap(),
        "-o",
        "listopt=%(size)s|%(c_filesize)s|%(linkname)s",
    ]);
    assert_success(&out, "list a cpio symlink");
    assert_eq!(stdout_str(&out).trim_end(), "0|6|target");

    // c_mode carries the file type over the permission bits. Only the type is
    // asserted: a symbolic link's permissions are 0777 on Linux and 0755 on
    // macOS, and neither is this keyword's business.
    let octal = |format: &str| {
        let text = listopt(&archive, format);
        u32::from_str_radix(&text, 8).unwrap_or_else(|_| panic!("{format} gave {text:?}"))
    };
    assert_eq!(
        octal("%(c_mode)s"),
        0o120000 | octal("%(mode)s"),
        "%(c_mode)s must be S_IFLNK over the permission bits %(mode)s reports"
    );
}

/// POSIX rule 7 requires every pax extended-header keyword, and names
/// `"%(charset)s"` as its own example. The reader dropped `charset`,
/// `hdrcharset`, `comment` and every implementation extension on the way to
/// the entry, so the listing had nothing to report and echoed the request.
#[test]
fn test_option_listopt_pax_extended_header_records() {
    let records = [
        pax_record("charset", b"ISO-IR 10646 2000 UTF-8"),
        pax_record("comment", b"hi there"),
        pax_record("hdrcharset", b"BINARY"),
        pax_record("SCHILY.fflags", b"nodump"),
    ]
    .concat();
    let archive = archive_with_ext_records(&records);

    assert_eq!(
        listopt_bytes(
            &archive,
            "%(charset)s|%(comment)s|%(hdrcharset)s|%(SCHILY.fflags)s"
        ),
        "ISO-IR 10646 2000 UTF-8|hi there|BINARY|nodump"
    );
}

/// A member that declared none of them reports nothing rather than echoing the
/// request, because the keywords are POSIX's whether or not this archive used
/// them -- and rather than POSIX's implicit UTF-8 default, which would make
/// "declared UTF-8" and "declared nothing" indistinguishable.
#[test]
fn test_option_listopt_pax_records_absent_renders_empty() {
    let archive = Ustar {
        name: b"plain.txt",
        body: b"x\n",
        ..Default::default()
    }
    .archive();

    assert_eq!(
        listopt_bytes(&archive, "[%(charset)s%(hdrcharset)s%(comment)s]"),
        "[]"
    );
    // While a name in no table at all keeps the literal echo.
    assert_eq!(
        listopt_bytes(&archive, "%(nosuchkeyword)s"),
        "%(nosuchkeyword)s"
    );
}

/// A global `g` header applies to every following member, and `-o
/// keyword=value` writes one. The record has to survive the round trip into
/// the listing, which is the whole path from the option parser through the
/// writer's global header and back out of the reader.
#[test]
fn test_option_listopt_global_comment_record_round_trips() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("global.pax");

    fs::create_dir(&src_dir).unwrap();
    fs::write(src_dir.join("g.txt"), b"x").unwrap();

    run_pax_in_dir(
        &[
            "-w",
            "-x",
            "pax",
            "-o",
            "comment=written for the test",
            "-f",
            archive.to_str().unwrap(),
            "g.txt",
        ],
        &src_dir,
    );

    assert_eq!(listopt(&archive, "%(comment)s"), "written for the test");
}

/// `-o keyword:=value` forces a record for each member, and `-o delete=`
/// removes one -- the listing must follow both, as it already does for the
/// keywords that map onto an entry field.
#[cfg(unix)]
#[test]
fn test_option_listopt_record_override_and_delete() {
    let records = [pax_record("charset", b"BINARY")].concat();
    let archive_bytes = archive_with_ext_records(&records);
    let temp = TempDir::new().unwrap();
    let archive = temp.path().join("rec.pax");
    fs::write(&archive, &archive_bytes).unwrap();

    assert_eq!(listopt(&archive, "%(charset)s"), "BINARY");
    assert_eq!(
        listopt_with(&archive, "%(charset)s", &["-o", "charset:=ISO-IR 646 1990"]),
        "ISO-IR 646 1990",
        "-o keyword:=value must force the value the listing reports"
    );
    assert_eq!(
        listopt_with(&archive, "[%(charset)s]", &["-o", "delete=charset"]),
        "[]",
        "-o delete= must remove the record from the listing too"
    );
}

/// POSIX rule 11 makes `(prefix,name)` the `%F` default for a member with no
/// `path` record, and the <comma>-separated keyword list was not parsed at
/// all: the whole specification echoed back.
#[test]
fn test_option_listopt_f_conversion_concatenates_keywords() {
    let prefix = vec![b'd'; 110];
    let archive = Ustar {
        name: b"deep.txt",
        prefix: &prefix,
        body: b"x\n",
        ..Default::default()
    }
    .archive();

    let dir = String::from_utf8(prefix).unwrap();
    assert_eq!(
        listopt_bytes(&archive, "%(prefix,name)F"),
        format!("{dir}/deep.txt")
    );
    assert_eq!(
        listopt_bytes(&archive, "%(prefix,name)F"),
        listopt_bytes(&archive, "%F"),
        "rule 11's default must rebuild exactly the pathname %F prints"
    );
}

/// POSIX rule 12: `%L` expands a symbolic link to `"%s -> %s"` of the pathname
/// and the link's contents, and is the equivalent of `%F` for anything else.
/// Bare `%L` had no handler and printed itself.
#[cfg(unix)]
#[test]
fn test_option_listopt_l_conversion_expands_a_symlink() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("l.tar");

    fs::create_dir(&src_dir).unwrap();
    fs::write(src_dir.join("real.txt"), b"x").unwrap();
    std::os::unix::fs::symlink("real.txt", src_dir.join("alias")).unwrap();

    run_pax_in_dir(
        &[
            "-w",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "alias",
            "real.txt",
        ],
        &src_dir,
    );

    let listing = listopt(&archive, "%L");
    let lines: Vec<&str> = listing.lines().collect();
    assert!(
        lines.contains(&"alias -> real.txt"),
        "a symbolic link must expand to `name -> contents`: {:?}",
        lines
    );
    assert!(
        lines.contains(&"real.txt"),
        "and anything else must render as %F does: {:?}",
        lines
    );
}

/// `-o hdrcharset=` has to be checked and still reach the archive. Validating
/// it by intercepting the keyword would stop the extended-header record being
/// written at all, which is the trap this pins.
#[test]
fn test_option_hdrcharset_is_checked_and_still_recorded() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    fs::create_dir(&src_dir).unwrap();
    fs::write(src_dir.join("h.txt"), b"x").unwrap();

    // A name pax cannot encode to is refused before anything is written.
    let out = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "pax",
            "-o",
            "hdrcharset=ISO-8859-1",
            "-f",
            temp.path().join("bad.pax").to_str().unwrap(),
            "h.txt",
        ],
        &src_dir,
    );
    assert_failure(&out, "an unsupported hdrcharset must be refused");
    assert!(
        stderr_str(&out).contains("BINARY"),
        "the refusal must name the supported values: {}",
        stderr_str(&out)
    );

    // POSIX's spelling contains spaces, which the comma tokenizer must not
    // mangle, and the record has to end up in the archive.
    for (given, recorded) in [
        ("binary", "hdrcharset=BINARY"),
        (
            "ISO-IR 10646 2000 UTF-8",
            "hdrcharset=ISO-IR 10646 2000 UTF-8",
        ),
    ] {
        let archive = temp.path().join("ok.pax");
        assert_success(
            &run_pax_in_dir(
                &[
                    "-w",
                    "-x",
                    "pax",
                    "-o",
                    &format!("hdrcharset={given}"),
                    "-f",
                    archive.to_str().unwrap(),
                    "h.txt",
                ],
                &src_dir,
            ),
            given,
        );

        let bytes = fs::read(&archive).unwrap();
        assert!(
            bytes
                .windows(recorded.len())
                .any(|w| w == recorded.as_bytes()),
            "`-o hdrcharset={given}' must record {recorded:?} in the archive"
        );
        assert_eq!(
            listopt(&archive, "%(hdrcharset)s"),
            recorded.trim_start_matches("hdrcharset="),
            "and the listing must report it"
        );
    }
}

/// Under `-o hdrcharset=BINARY` the `path` record is what carries a name's
/// bytes, so POSIX requires one for any non-ASCII name even when the ustar
/// fields could hold it: RATIONALE, "an extended header path record is always
/// required to be generated if the prefix or name fields contain non-ASCII
/// characters even when hdrcharset=binary is also in effect for that file."
///
/// The option had no effect on the write path at all, so such a member was
/// written with its name only in the ustar field, under a header declaring an
/// encoding that field was not in.
#[test]
fn test_option_hdrcharset_binary_forces_a_path_record() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    fs::create_dir(&src_dir).unwrap();
    // Short, and valid UTF-8: the ustar name field could hold it, so nothing
    // but the operator's request makes a record necessary.
    fs::write(src_dir.join("élan.txt"), b"x").unwrap();
    fs::write(src_dir.join("plain.txt"), b"y").unwrap();

    let write = |archive: &std::path::Path, extra: &[&str]| {
        let mut args = vec!["-w", "-x", "pax"];
        args.extend_from_slice(extra);
        args.extend_from_slice(&["-f", archive.to_str().unwrap(), "élan.txt", "plain.txt"]);
        assert_success(&run_pax_in_dir(&args, &src_dir), "write a pax archive");
        fs::read(archive).unwrap()
    };

    let path_records = |bytes: &[u8]| bytes.windows(5).filter(|w| *w == b"path=").count();

    // Without the option, a short UTF-8 name needs no record.
    let plain = write(&temp.path().join("plain.pax"), &[]);
    assert_eq!(
        path_records(&plain),
        0,
        "a representable name must not get a path record on its own"
    );

    // With it, the non-ASCII name gets one -- and only that one.
    let binary = write(
        &temp.path().join("binary.pax"),
        &["-o", "hdrcharset=BINARY"],
    );
    assert_eq!(
        path_records(&binary),
        1,
        "-o hdrcharset=BINARY must force a path record for the non-ASCII name"
    );
    assert!(
        binary.windows(14).any(|w| w == "path=élan.txt".as_bytes()),
        "and the record must carry the name's bytes"
    );

    // The charset itself is announced once, as a global record, rather than
    // repeated in every member's extended header.
    assert_eq!(
        binary
            .windows(18)
            .filter(|w| *w == b"hdrcharset=BINARY\n")
            .count(),
        1,
        "the declared charset belongs in one global record"
    );
    // And it reaches the listing for every member it governs.
    let listing = run_pax(&[
        "-f",
        temp.path().join("binary.pax").to_str().unwrap(),
        "-o",
        "listopt=%(hdrcharset)s",
    ]);
    assert_success(&listing, "list the BINARY archive");
    assert!(
        stdout_str(&listing).lines().all(|l| l == "BINARY"),
        "a global hdrcharset governs every member: {:?}",
        stdout_str(&listing)
    );
}

/// `hdrcharset=BINARY` says the gname, linkpath, path and uname records "are
/// unencoded binary data from the underlying system". path and linkpath were
/// already carried as bytes; uname and gname went through from_utf8_lossy, so
/// a group name with a high byte came back as U+FFFD -- irreversibly, and
/// under a header declaring the bytes had been preserved.
#[test]
fn test_option_listopt_binary_group_name_round_trips() {
    let records = [
        pax_record("hdrcharset", b"BINARY"),
        pax_record("gname", b"gr\xffup"),
        pax_record("uname", b"us\xfer"),
    ]
    .concat();
    let archive = archive_with_ext_records(&records);

    let out = run_pax_with_stdin_bytes(&["-o", "listopt=%(uname)s:%(gname)s"], &archive);
    assert_success(&out, "list a BINARY member");
    assert_eq!(
        out.stdout, b"us\xfer:gr\xffup\n",
        "a BINARY uname and gname must reach the listing byte for byte"
    );
}

/// Every `%(keyword)` POSIX rule 7 requires must resolve to a value.
///
/// A keyword this implementation does not know is echoed back as its own
/// specification (`options.rs` `KeywordValue::Unknown`), so a listing that
/// still contains `%(` is exactly the reported bug: the ustar header field
/// names, the whole cpio set with and without the `c_` prefix, and the pax
/// `charset`/`hdrcharset` records all came back literally.
///
/// Rule 7 admits a keyword the member's format has no field for -- the value
/// is then "the value from the applicable header field", of which there is
/// none -- so this asserts only that the specification is consumed, not that
/// it produced text. The per-keyword values are pinned by the tests below.
#[test]
fn test_option_listopt_posix_rule7_keywords_all_resolve() {
    // Every Field Name entry in POSIX's ustar Header Block table.
    const USTAR: &[&str] = &[
        "name", "mode", "uid", "gid", "size", "mtime", "chksum", "typeflag", "linkname", "magic",
        "version", "uname", "gname", "devmajor", "devminor", "prefix",
    ];
    // Every Field Name entry in its Octet-Oriented cpio Archive Entry table,
    // which rule 7 also permits without the leading `c_`.
    const CPIO: &[&str] = &[
        "c_magic",
        "c_dev",
        "c_ino",
        "c_mode",
        "c_uid",
        "c_gid",
        "c_nlink",
        "c_rdev",
        "c_mtime",
        "c_namesize",
        "c_filesize",
        "c_name",
        "dev",
        "ino",
        "nlink",
        "rdev",
        "namesize",
        "filesize",
    ];
    // Every keyword defined for the pax extended header.
    const PAX: &[&str] = &[
        "atime",
        "charset",
        "comment",
        "gid",
        "gname",
        "hdrcharset",
        "linkpath",
        "mtime",
        "path",
        "size",
        "uid",
        "uname",
    ];

    let ustar = Ustar {
        name: b"f.txt",
        body: b"hi\n",
        ..Default::default()
    }
    .archive();

    let mut cpio = CpioNewc {
        name: b"f.txt",
        body: b"hi\n",
        ..Default::default()
    }
    .member();
    cpio.extend_from_slice(
        &CpioNewc {
            name: b"TRAILER!!!",
            ..Default::default()
        }
        .member(),
    );

    for (format_name, archive) in [("ustar", &ustar), ("cpio", &cpio)] {
        for keyword in USTAR.iter().chain(CPIO).chain(PAX) {
            let listopt = format!("listopt=%({})s", keyword);
            let out = run_pax_with_stdin_bytes(&["-o", &listopt], archive);
            assert_success(&out, &listopt);
            let listing = stdout_str(&out);
            assert!(
                !listing.contains("%("),
                "%({})s must resolve in a {} archive rather than echo back \
                 (got {:?})",
                keyword,
                format_name,
                listing
            );
        }
    }

    // ... while a name in none of those tables keeps the literal echo, which is
    // how an operator sees a typo instead of a silently empty column.
    let out = run_pax_with_stdin_bytes(&["-o", "listopt=%(bogus)s"], &ustar);
    assert_success(&out, "listopt=%(bogus)s");
    assert_eq!(stdout_str(&out).trim_end(), "%(bogus)s");
}
