//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Multi-volume tests (-M)

use crate::common::*;
use std::fs::{self, File};
use std::io::Write;
use tempfile::TempDir;

#[test]
fn test_multi_volume_basic() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("small.txt")).unwrap();
    writeln!(f, "Small file content").unwrap();

    // Create multi-volume archive with a large tape length (so no split needed)
    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1000000",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "small.txt",
        ],
        &src_dir,
    );
    assert_success(&output, "pax multi-volume write");

    // Verify archive was created
    assert!(archive.exists(), "Archive should be created");

    // Extract using standard read mode (single volume should work normally)
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax extract");

    // Verify content
    let content = fs::read_to_string(dst_dir.join("small.txt")).unwrap();
    assert!(content.contains("Small file"), "Content mismatch");
}

#[test]
fn test_multi_volume_verbose() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("verbose.txt")).unwrap();
    writeln!(f, "Verbose test").unwrap();

    // Create multi-volume archive with verbose
    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "-v",
            "--tape-length",
            "1000000",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "verbose.txt",
        ],
        &src_dir,
    );
    assert_success(&output, "pax multi-volume write");

    // Verbose output should mention volume
    let stderr = stderr_str(&output);
    assert!(
        stderr.contains("volume") || stderr.contains("verbose.txt"),
        "Verbose output should show progress: {}",
        stderr
    );
}

#[test]
fn test_multi_volume_requires_tape_length() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("test.txt")).unwrap();
    writeln!(f, "Test").unwrap();

    // Try to create multi-volume archive without --tape-length
    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "test.txt",
        ],
        &src_dir,
    );

    // Should fail with an error about requiring tape-length
    assert_failure(&output, "pax should fail without --tape-length");
    let stderr = stderr_str(&output);
    assert!(
        stderr.contains("tape-length") || stderr.contains("volume"),
        "Error should mention tape-length requirement: {}",
        stderr
    );
}

#[test]
fn test_multi_volume_requires_archive_file() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("test.txt")).unwrap();
    writeln!(f, "Test").unwrap();

    // Try to create multi-volume archive to stdout (no -f)
    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1000000",
            "-x",
            "ustar",
            "test.txt",
        ],
        &src_dir,
    );

    // Should fail because multi-volume requires a file
    assert_failure(&output, "pax should fail without -f for multi-volume");
    let stderr = stderr_str(&output);
    assert!(
        stderr.contains("requires") || stderr.contains("archive"),
        "Error should mention archive file requirement: {}",
        stderr
    );
}

#[test]
fn test_multi_volume_cpio_not_supported() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.cpio");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("test.txt")).unwrap();
    writeln!(f, "Test").unwrap();

    // Try to create multi-volume cpio archive
    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1000000",
            "-x",
            "cpio",
            "-f",
            archive.to_str().unwrap(),
            "test.txt",
        ],
        &src_dir,
    );

    // Should fail because cpio doesn't support multi-volume
    assert_failure(&output, "pax should fail for multi-volume cpio");
    let stderr = stderr_str(&output);
    assert!(
        stderr.contains("not supported") || stderr.contains("cpio"),
        "Error should mention cpio not supported: {}",
        stderr
    );
}

#[test]
fn test_multi_volume_multiple_files() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create multiple source files
    fs::create_dir(&src_dir).unwrap();
    for i in 1..=5 {
        let mut f = File::create(src_dir.join(format!("file{}.txt", i))).unwrap();
        writeln!(f, "Content for file {}", i).unwrap();
    }

    // Create multi-volume archive with large tape length
    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1000000",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            ".",
        ],
        &src_dir,
    );
    assert_success(&output, "pax multi-volume write");

    // List archive
    let output = run_pax(&["-f", archive.to_str().unwrap()]);
    let listing = stdout_str(&output);

    // Verify all files are listed
    for i in 1..=5 {
        assert!(
            listing.contains(&format!("file{}.txt", i)),
            "file{}.txt should be in archive",
            i
        );
    }

    // Extract and verify
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax extract");

    // Verify content
    for i in 1..=5 {
        let content = fs::read_to_string(dst_dir.join(format!("file{}.txt", i))).unwrap();
        assert!(
            content.contains(&format!("Content for file {}", i)),
            "file{}.txt content mismatch",
            i
        );
    }
}

/// A member whose data does not fit in one volume must be diagnosed.
///
/// pax does not split a member across volumes: volumes contain whole members.
/// Previously the size check guarded only the 512-byte header, so the payload
/// streamed past the tape length and produced a single over-length volume that
/// silently violated the limit the user asked for.
///
/// This test replaces one that asserted `success() || code().is_some()` and so
/// could never fail.
#[test]
fn test_multi_volume_member_larger_than_volume_is_diagnosed() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("multi.tar");

    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("large.txt")).unwrap();
    for _ in 0..100 {
        writeln!(
            f,
            "This is a line of data that will be repeated to create a larger file."
        )
        .unwrap();
    }
    drop(f);
    let size = fs::metadata(src_dir.join("large.txt")).unwrap().len();
    assert!(size > 2048, "test file must exceed the tape length");

    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "-v",
            "--tape-length",
            "2048",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "large.txt",
        ],
        &src_dir,
    );

    assert_failure(&output, "a member larger than the volume");
    let stderr = stderr_str(&output);
    assert!(
        stderr.contains("large.txt") && stderr.contains("volume"),
        "the diagnostic should name the member and the volume limit: {stderr}"
    );
    // Whatever was written must not exceed the requested volume size.
    if archive.exists() {
        assert!(
            fs::metadata(&archive).unwrap().len() <= 2048,
            "a volume must never exceed --tape-length"
        );
    }
}

/// -M writes ustar headers unconditionally. Asking for a different interchange
/// format has to be refused rather than silently downgraded.
#[test]
fn test_multi_volume_rejects_non_ustar_format() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("mv.tar");
    fs::create_dir(&src_dir).unwrap();
    fs::write(src_dir.join("f.txt"), b"x").unwrap();

    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1000000",
            "-x",
            "pax",
            "-f",
            archive.to_str().unwrap(),
            "f.txt",
        ],
        &src_dir,
    );

    assert_failure(&output, "-M with -x pax");
    assert!(
        stderr_str(&output).contains("multi-volume"),
        "the diagnostic should explain that -M is ustar-only: {}",
        stderr_str(&output)
    );
}

// ==================== Multi-Volume Read Tests ====================

#[test]
fn test_multi_volume_list_basic() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("file.txt")).unwrap();
    writeln!(f, "Test content").unwrap();

    // Create multi-volume archive with large tape length (single volume)
    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1000000",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "file.txt",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List archive in multi-volume mode
    let output = run_pax(&["-M", "-f", archive.to_str().unwrap()]);
    assert_success(&output, "pax list multi-volume");

    let listing = stdout_str(&output);
    assert!(
        listing.contains("file.txt"),
        "file.txt should be in listing"
    );
}

#[test]
fn test_multi_volume_list_verbose() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("verbose.txt")).unwrap();
    writeln!(f, "Verbose test content").unwrap();

    // Create multi-volume archive
    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1000000",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "verbose.txt",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List archive in multi-volume mode with verbose
    let output = run_pax(&["-M", "-v", "-f", archive.to_str().unwrap()]);
    assert_success(&output, "pax list multi-volume verbose");

    let listing = stdout_str(&output);
    // Verbose output should have permissions and file name
    assert!(
        listing.contains("verbose.txt"),
        "verbose.txt should be in listing"
    );
    // Should have ls-style output with permissions
    assert!(
        listing.contains("-rw") || listing.contains("rw-"),
        "Verbose output should have permissions"
    );
}

#[test]
fn test_multi_volume_extract_basic() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("extract.txt")).unwrap();
    writeln!(f, "Extract test content").unwrap();

    // Create multi-volume archive
    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1000000",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "extract.txt",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract in multi-volume mode
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-M", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax extract multi-volume");

    // Verify extracted content
    let extracted = dst_dir.join("extract.txt");
    assert!(extracted.exists(), "File should be extracted");
    let content = fs::read_to_string(&extracted).unwrap();
    assert!(content.contains("Extract test content"), "Content mismatch");
}

#[test]
fn test_multi_volume_extract_multiple_files() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create multiple source files
    fs::create_dir(&src_dir).unwrap();
    for i in 1..=5 {
        let mut f = File::create(src_dir.join(format!("mv{}.txt", i))).unwrap();
        writeln!(f, "Multi-volume file {} content", i).unwrap();
    }

    // Create multi-volume archive
    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1000000",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            ".",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract in multi-volume mode
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-M", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax extract multi-volume");

    // Verify all files were extracted
    for i in 1..=5 {
        let extracted = dst_dir.join(format!("mv{}.txt", i));
        assert!(extracted.exists(), "mv{}.txt should be extracted", i);
        let content = fs::read_to_string(&extracted).unwrap();
        assert!(
            content.contains(&format!("Multi-volume file {} content", i)),
            "mv{}.txt content mismatch",
            i
        );
    }
}

#[test]
fn test_multi_volume_extract_verbose() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("verbose_extract.txt")).unwrap();
    writeln!(f, "Verbose extract test").unwrap();

    // Create multi-volume archive
    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1000000",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "verbose_extract.txt",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract with verbose
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(
        &["-r", "-M", "-v", "-f", archive.to_str().unwrap()],
        &dst_dir,
    );
    assert_success(&output, "pax extract multi-volume");

    // Verbose output should show files being extracted
    let stderr = stderr_str(&output);
    assert!(
        stderr.contains("verbose_extract.txt") || stderr.contains("volume"),
        "Verbose output should show progress"
    );
}

#[test]
fn test_multi_volume_read_requires_archive_file() {
    let temp = TempDir::new().unwrap();

    // Try to read in multi-volume mode without -f (should fail)
    let output = run_pax_in_dir(&["-M"], temp.path());

    // Should fail because multi-volume requires a file
    assert_failure(&output, "pax should fail without -f for multi-volume read");
    let stderr = stderr_str(&output);
    assert!(
        stderr.contains("requires") || stderr.contains("archive"),
        "Error should mention archive file requirement: {}",
        stderr
    );
}

#[test]
fn test_multi_volume_extract_requires_archive_file() {
    let temp = TempDir::new().unwrap();

    // Try to extract in multi-volume mode without -f (should fail)
    let output = run_pax_in_dir(&["-r", "-M"], temp.path());

    // Should fail because multi-volume requires a file
    assert_failure(
        &output,
        "pax should fail without -f for multi-volume extract",
    );
    let stderr = stderr_str(&output);
    assert!(
        stderr.contains("requires") || stderr.contains("archive"),
        "Error should mention archive file requirement: {}",
        stderr
    );
}

#[test]
fn test_multi_volume_roundtrip() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("roundtrip.tar");
    let dst_dir = temp.path().join("dest");

    // Create source files with various content
    fs::create_dir(&src_dir).unwrap();
    fs::create_dir(src_dir.join("subdir")).unwrap();

    let mut f = File::create(src_dir.join("root.txt")).unwrap();
    writeln!(f, "Root file content").unwrap();

    let mut f = File::create(src_dir.join("subdir/nested.txt")).unwrap();
    writeln!(f, "Nested file content").unwrap();

    // Create multi-volume archive
    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1000000",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            ".",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List archive to verify content
    let output = run_pax(&["-M", "-f", archive.to_str().unwrap()]);
    let listing = stdout_str(&output);
    assert!(listing.contains("root.txt"), "root.txt should be listed");
    assert!(
        listing.contains("subdir") || listing.contains("nested.txt"),
        "subdir or nested.txt should be listed"
    );

    // Extract multi-volume archive
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-M", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax extract");

    // Verify all content was extracted correctly
    assert!(dst_dir.join("root.txt").exists(), "root.txt should exist");
    assert!(
        dst_dir.join("subdir/nested.txt").exists(),
        "subdir/nested.txt should exist"
    );

    let root_content = fs::read_to_string(dst_dir.join("root.txt")).unwrap();
    assert!(
        root_content.contains("Root file content"),
        "root.txt content mismatch"
    );

    let nested_content = fs::read_to_string(dst_dir.join("subdir/nested.txt")).unwrap();
    assert!(
        nested_content.contains("Nested file content"),
        "nested.txt content mismatch"
    );
}

/// The ustar header splits a long pathname across the 100-byte name field and
/// the 155-byte prefix field. multivolume's own writer only ever wrote the name
/// field, so a path over 100 bytes was silently truncated -- while its own
/// reader parses the prefix, making the format asymmetric with itself.
#[test]
fn test_multi_volume_long_path_not_truncated() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("long.tar");
    let dst_dir = temp.path().join("dest");

    // 3 x 40-char components plus separators: >100 bytes, but splittable at a
    // '/' within the prefix field, so ustar can represent it exactly.
    let deep = format!("{}/{}/{}", "a".repeat(40), "b".repeat(40), "c".repeat(40));
    let full = src_dir.join(&deep);
    fs::create_dir_all(full.parent().unwrap()).unwrap();
    fs::write(&full, b"deep content\n").unwrap();
    assert!(deep.len() > 100, "test path must exceed the name field");

    let output = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1000000",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            &deep,
        ],
        &src_dir,
    );
    assert_success(&output, "pax -M write long path");

    let listing = stdout_str(&run_pax(&["-f", archive.to_str().unwrap()]));
    assert!(
        listing.lines().any(|l| l == deep),
        "-M must not truncate a path the ustar prefix field can hold.\nwanted: {deep}\ngot: {listing}"
    );

    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax extract long path");
    assert_eq!(
        fs::read_to_string(dst_dir.join(&deep)).unwrap(),
        "deep content\n"
    );
}
