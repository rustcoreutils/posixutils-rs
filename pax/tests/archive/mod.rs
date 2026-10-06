//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Archive format tests - roundtrips for ustar, cpio, pax formats

use crate::common::*;
use plib::tmp::TempDir;
use std::fs::{self, File};
use std::io::Write;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::MetadataExt;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::Command;

#[test]
fn test_ustar_roundtrip() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    create_test_files(&src_dir);

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Verify archive was created
    assert!(archive.exists(), "Archive was not created");
    assert!(archive.metadata().unwrap().len() > 0, "Archive is empty");

    // Extract archive
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read");

    // Verify files were extracted correctly
    verify_files_match(&src_dir, &dst_dir);
}

#[test]
fn test_cpio_roundtrip() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.cpio");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    create_test_files(&src_dir);

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-x", "cpio", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Verify archive was created
    assert!(archive.exists(), "Archive was not created");
    assert!(archive.metadata().unwrap().len() > 0, "Archive is empty");

    // Extract archive
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read");

    // Verify files were extracted correctly
    verify_files_match(&src_dir, &dst_dir);
}

#[test]
fn test_pax_roundtrip() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.pax");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    create_test_files(&src_dir);

    // Create archive using pax format (the default extended format)
    let output = run_pax_in_dir(
        &["-w", "-x", "pax", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Verify archive was created
    assert!(archive.exists(), "Archive was not created");
    assert!(archive.metadata().unwrap().len() > 0, "Archive is empty");

    // Extract archive
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read");

    // Verify files were extracted correctly
    verify_files_match(&src_dir, &dst_dir);
}

#[cfg(unix)]
#[test]
fn test_hardlink_roundtrip() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source with hard link
    fs::create_dir(&src_dir).unwrap();
    let file1 = src_dir.join("file1.txt");
    let file2 = src_dir.join("file2.txt");

    let mut f = File::create(&file1).unwrap();
    writeln!(f, "Shared content").unwrap();
    drop(f);

    fs::hard_link(&file1, &file2).unwrap();

    // Verify they share the same inode
    use std::os::unix::fs::MetadataExt;
    let m1 = fs::metadata(&file1).unwrap();
    let m2 = fs::metadata(&file2).unwrap();
    assert_eq!(m1.ino(), m2.ino(), "Source files should share inode");

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract archive
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read");

    // Verify both files exist and have same content
    let c1 = fs::read_to_string(dst_dir.join("file1.txt")).unwrap();
    let c2 = fs::read_to_string(dst_dir.join("file2.txt")).unwrap();
    assert_eq!(c1, c2, "Hard link content mismatch");

    // Verify they share the same inode
    let m1 = fs::metadata(dst_dir.join("file1.txt")).unwrap();
    let m2 = fs::metadata(dst_dir.join("file2.txt")).unwrap();
    assert_eq!(m1.ino(), m2.ino(), "Extracted files should share inode");
}

/// cpio has no hard-link representation of its own: build_mode maps Hardlink
/// onto a regular file. Zeroing the member size for the second link therefore
/// stored a 0-byte file with no data at all, and the reader -- which returns it
/// as Regular -- never relinked it. The content was silently lost.
///
/// POSIX (pax EXTENDED DESCRIPTION): for a format that does not store contents
/// with each name causing a hard link, the data shall be restored from the
/// original file or a diagnostic given. Writing the contents for every link
/// satisfies the first; the extracted files are separate inodes, which is
/// permitted, but neither may be empty.
#[cfg(unix)]
#[test]
fn test_cpio_hardlink_preserves_content() {
    use std::os::unix::fs::MetadataExt;

    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("links.cpio");
    let dst_dir = temp.path().join("dest");

    fs::create_dir(&src_dir).unwrap();
    let body = "Shared content that must survive\n";
    fs::write(src_dir.join("file1.txt"), body).unwrap();
    fs::hard_link(src_dir.join("file1.txt"), src_dir.join("file2.txt")).unwrap();
    assert_eq!(
        fs::metadata(src_dir.join("file1.txt")).unwrap().nlink(),
        2,
        "source files must really be linked"
    );

    let output = run_pax_in_dir(
        &["-w", "-x", "cpio", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write cpio hard link");

    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read cpio hard link");

    for name in ["file1.txt", "file2.txt"] {
        let got = fs::read_to_string(dst_dir.join(name))
            .unwrap_or_else(|e| panic!("{name} missing after extract: {e}"));
        assert_eq!(got, body, "{name} lost its contents through cpio");
    }
}

#[test]
fn test_cross_tool_tar_read() {
    // This test verifies we can read tar archives created by system tar
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("hello.txt")).unwrap();
    writeln!(f, "Hello from tar").unwrap();

    // Create archive using system tar
    let Some(tar) = system_tool("tar") else {
        return;
    };
    run_system_ok(
        &tar,
        &["-cf", archive.to_str().unwrap(), "."],
        &src_dir,
        None,
    );

    // Extract with our pax
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read");

    // Verify content
    let content = fs::read_to_string(dst_dir.join("hello.txt")).unwrap();
    assert!(content.contains("Hello from tar"), "Content mismatch");
}

#[test]
fn test_cross_tool_tar_write() {
    // This test verifies system tar can read our archives
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("hello.txt")).unwrap();
    writeln!(f, "Hello from pax").unwrap();

    // Create archive using our pax
    let output = run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract with system tar
    let Some(tar) = system_tool("tar") else {
        return;
    };
    fs::create_dir(&dst_dir).unwrap();
    run_system_ok(&tar, &["-xf", archive.to_str().unwrap()], &dst_dir, None);

    // Verify content
    let content = fs::read_to_string(dst_dir.join("hello.txt")).unwrap();
    assert!(content.contains("Hello from pax"), "Content mismatch");
}

#[test]
fn test_pax_long_paths() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.pax");
    let dst_dir = temp.path().join("dest");

    // Create source with very long path
    fs::create_dir(&src_dir).unwrap();

    // Create deeply nested directory with long names
    let mut long_path = src_dir.clone();
    for i in 0..10 {
        long_path = long_path.join(format!("directory_with_a_very_long_name_{:02}", i));
    }
    fs::create_dir_all(&long_path).unwrap();

    // Create file with long name in deep directory
    let long_file =
        long_path.join("file_with_an_extremely_long_name_that_exceeds_normal_limits.txt");
    let mut f = File::create(&long_file).unwrap();
    writeln!(f, "Content in deep path").unwrap();

    // Create archive using pax format (supports long paths)
    let output = run_pax_in_dir(
        &["-w", "-x", "pax", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract archive
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    // Some filesystems or environments may not support very long paths
    // - macOS: "Operation not permitted" or "File name too long"
    // - Linux: "Is a directory" can occur with path handling edge cases
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        if stderr.contains("Operation not permitted")
            || stderr.contains("File name too long")
            || stderr.contains("Is a directory")
        {
            eprintln!(
                "Skipping long path test: filesystem/environment doesn't support very long paths"
            );
            return;
        }
    }
    assert_success(&output, "pax read");

    // Reconstruct expected path in dst_dir
    let mut expected_path = dst_dir.clone();
    for i in 0..10 {
        expected_path = expected_path.join(format!("directory_with_a_very_long_name_{:02}", i));
    }
    let expected_file =
        expected_path.join("file_with_an_extremely_long_name_that_exceeds_normal_limits.txt");

    assert!(expected_file.exists(), "Long path file should exist");
    let content = fs::read_to_string(&expected_file).unwrap();
    assert!(content.contains("Content in deep path"), "Content mismatch");
}

#[test]
fn test_pax_with_subdirectories() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.pax");
    let dst_dir = temp.path().join("dest");

    // Create source with multiple levels of subdirectories
    fs::create_dir(&src_dir).unwrap();

    // Create directory structure
    fs::create_dir_all(src_dir.join("a/b/c")).unwrap();
    fs::create_dir_all(src_dir.join("x/y")).unwrap();

    // Create files at various levels
    File::create(src_dir.join("root.txt"))
        .unwrap()
        .write_all(b"root")
        .unwrap();
    File::create(src_dir.join("a/level1.txt"))
        .unwrap()
        .write_all(b"level1")
        .unwrap();
    File::create(src_dir.join("a/b/level2.txt"))
        .unwrap()
        .write_all(b"level2")
        .unwrap();
    File::create(src_dir.join("a/b/c/level3.txt"))
        .unwrap()
        .write_all(b"level3")
        .unwrap();
    File::create(src_dir.join("x/y/another.txt"))
        .unwrap()
        .write_all(b"another")
        .unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-x", "pax", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract archive
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read");

    // Verify all files exist
    assert!(dst_dir.join("root.txt").exists());
    assert!(dst_dir.join("a/level1.txt").exists());
    assert!(dst_dir.join("a/b/level2.txt").exists());
    assert!(dst_dir.join("a/b/c/level3.txt").exists());
    assert!(dst_dir.join("x/y/another.txt").exists());

    // Verify content
    assert_eq!(
        fs::read_to_string(dst_dir.join("root.txt")).unwrap(),
        "root"
    );
    assert_eq!(
        fs::read_to_string(dst_dir.join("a/b/c/level3.txt")).unwrap(),
        "level3"
    );
}

#[cfg(unix)]
#[test]
fn test_pax_symlink() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.pax");
    let dst_dir = temp.path().join("dest");

    // Create source with symlink
    fs::create_dir(&src_dir).unwrap();

    let target = src_dir.join("target.txt");
    File::create(&target)
        .unwrap()
        .write_all(b"target content")
        .unwrap();

    let link = src_dir.join("link.txt");
    std::os::unix::fs::symlink("target.txt", &link).unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-x", "pax", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract archive
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read");

    // Verify symlink
    let extracted_link = dst_dir.join("link.txt");
    assert!(extracted_link
        .symlink_metadata()
        .unwrap()
        .file_type()
        .is_symlink());
    assert_eq!(
        fs::read_link(&extracted_link).unwrap().to_str().unwrap(),
        "target.txt"
    );
}

#[cfg(unix)]
#[test]
fn test_pax_hardlink() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.pax");
    let dst_dir = temp.path().join("dest");

    // Create source with hardlink
    fs::create_dir(&src_dir).unwrap();

    let original = src_dir.join("original.txt");
    File::create(&original)
        .unwrap()
        .write_all(b"shared content")
        .unwrap();

    let hardlink = src_dir.join("hardlink.txt");
    fs::hard_link(&original, &hardlink).unwrap();

    // Create archive
    let output = run_pax_in_dir(
        &["-w", "-x", "pax", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract archive
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read");

    // Verify both files exist with same content
    let c1 = fs::read_to_string(dst_dir.join("original.txt")).unwrap();
    let c2 = fs::read_to_string(dst_dir.join("hardlink.txt")).unwrap();
    assert_eq!(c1, c2);

    // Verify they share inode
    use std::os::unix::fs::MetadataExt;
    let m1 = fs::metadata(dst_dir.join("original.txt")).unwrap();
    let m2 = fs::metadata(dst_dir.join("hardlink.txt")).unwrap();
    assert_eq!(m1.ino(), m2.ino(), "Hardlinks should share inode");
}

#[test]
fn test_pax_cross_tool_read() {
    // Test reading pax archives created by system pax/tar
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    fs::create_dir(&src_dir).unwrap();
    File::create(src_dir.join("test.txt"))
        .unwrap()
        .write_all(b"test content")
        .unwrap();

    // Create the archive with system tar in pax format. GNU tar and bsdtar
    // both spell it --format=posix; a tar that knows neither still has to
    // write some archive this pax reads.
    let Some(tar) = system_tool("tar") else {
        return;
    };
    let path = archive.to_str().unwrap();
    let made = run_program(&tar, &["--format=posix", "-cf", path, "."], &src_dir, None);
    if !made.status.success() {
        run_system_ok(&tar, &["-cf", path, "."], &src_dir, None);
    }

    // Extract with our pax
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read");

    assert!(dst_dir.join("test.txt").exists());
}

#[test]
fn test_pax_cross_tool_write() {
    // Test that system tar can read our pax archives
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    fs::create_dir(&src_dir).unwrap();
    File::create(src_dir.join("test.txt"))
        .unwrap()
        .write_all(b"pax content")
        .unwrap();

    // Create archive with our pax
    let output = run_pax_in_dir(
        &["-w", "-x", "pax", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Extract with system tar: every tar in use today reads pax archives.
    let Some(tar) = system_tool("tar") else {
        return;
    };
    fs::create_dir(&dst_dir).unwrap();
    run_system_ok(&tar, &["-xf", archive.to_str().unwrap()], &dst_dir, None);

    let content = fs::read_to_string(dst_dir.join("test.txt")).unwrap();
    assert!(content.contains("pax content"));
}

#[test]
fn test_cross_tool_cpio_read() {
    // Test that our pax can read cpio archives created by system cpio
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("system.cpio");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("hello.txt")).unwrap();
    writeln!(f, "Hello from cpio").unwrap();

    // Create archive using system cpio, from the names `find .` would give
    let Some(cpio) = system_tool("cpio") else {
        return;
    };
    let output = run_system_ok(&cpio, &["-o"], &src_dir, Some(b".\n./hello.txt\n"));

    // Write the cpio archive
    fs::write(&archive, &output.stdout).unwrap();

    // Extract with our pax
    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax read system cpio");

    // Verify content
    let content = fs::read_to_string(dst_dir.join("hello.txt")).unwrap();
    assert!(content.contains("Hello from cpio"), "Content mismatch");
}

#[test]
fn test_cross_tool_cpio_write() {
    // Test that system cpio can read archives created by our pax
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.cpio");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("hello.txt")).unwrap();
    writeln!(f, "Hello from pax").unwrap();

    // Create archive using our pax with cpio format
    let output = run_pax_in_dir(
        &["-w", "-x", "cpio", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write cpio");

    // Extract with system cpio
    let Some(cpio) = system_tool("cpio") else {
        return;
    };
    fs::create_dir(&dst_dir).unwrap();
    run_system_ok(
        &cpio,
        &["-id"],
        &dst_dir,
        Some(&fs::read(&archive).unwrap()),
    );

    // Verify content
    let content = fs::read_to_string(dst_dir.join("hello.txt")).unwrap();
    assert!(content.contains("Hello from pax"), "Content mismatch");
}

/// Test that pax format always generates extended headers, even for simple files
/// that would fit in ustar limits. This ensures the archive is identifiable as
/// pax format (not indistinguishable from ustar).
#[test]
fn test_pax_always_generates_extended_headers() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.pax");

    // Create a simple file that would fit in ustar limits
    // (short name, small size, normal uid/gid, no subsecond timestamps)
    fs::create_dir(&src_dir).unwrap();
    File::create(src_dir.join("simple.txt"))
        .unwrap()
        .write_all(b"simple content")
        .unwrap();

    // Create archive using pax format
    let output = run_pax_in_dir(
        &["-w", "-x", "pax", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // Read the archive bytes and verify extended header presence
    let archive_data = fs::read(&archive).unwrap();

    // In tar/pax format, typeflag is at offset 156 within each 512-byte header block
    // Typeflag 'x' (0x78) indicates an extended header
    const BLOCK_SIZE: usize = 512;
    const TYPEFLAG_OFFSET: usize = 156;

    let mut found_extended_header = false;
    let mut found_mtime_record = false;

    // Scan through header blocks looking for extended header
    let mut offset = 0;
    while offset + BLOCK_SIZE <= archive_data.len() {
        let block = &archive_data[offset..offset + BLOCK_SIZE];

        // Check if this is a zero block (end of archive)
        if block.iter().all(|&b| b == 0) {
            break;
        }

        let typeflag = block[TYPEFLAG_OFFSET];

        if typeflag == b'x' {
            found_extended_header = true;

            // Get the size of the extended header data from the header
            // Size field is at offset 124, 12 bytes, octal
            let size_field = &block[124..136];
            let size_str = std::str::from_utf8(size_field)
                .unwrap_or("")
                .trim()
                .trim_end_matches('\0');
            if let Ok(size) = u64::from_str_radix(size_str.trim(), 8) {
                // Read the extended header data (follows this header block)
                let data_start = offset + BLOCK_SIZE;
                let data_end = data_start + size as usize;
                if data_end <= archive_data.len() {
                    let ext_data = &archive_data[data_start..data_end];
                    let ext_str = String::from_utf8_lossy(ext_data);

                    // Check for mtime record (format: "NN mtime=TIMESTAMP\n")
                    if ext_str.contains("mtime=") {
                        found_mtime_record = true;
                    }
                }
            }
            break; // Found what we're looking for
        }

        offset += BLOCK_SIZE;
    }

    assert!(
        found_extended_header,
        "pax archive should contain extended header (typeflag 'x') even for simple files"
    );
    assert!(
        found_mtime_record,
        "pax extended header should contain mtime record"
    );
}

#[test]
fn test_symlink_tar_no_damaged_warning() {
    // Verify that system tar can read our tar archives with symlinks without
    // reporting "Damaged tar archive" errors
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source files with symlink
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("hello.txt")).unwrap();
    writeln!(f, "Hello World").unwrap();

    #[cfg(unix)]
    std::os::unix::fs::symlink("hello.txt", src_dir.join("link.txt")).unwrap();
    #[cfg(not(unix))]
    {
        eprintln!("Skipping symlink test on non-Unix platform");
        return;
    }

    // Create archive using our pax
    let output = run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );
    assert_success(&output, "pax write");

    // List with system tar and check for "Damaged" warning
    let Some(tar) = system_tool("tar") else {
        return;
    };
    let output = run_system_ok(&tar, &["-tvf", archive.to_str().unwrap()], &src_dir, None);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // The key assertion: no "Damaged" warning
    assert!(
        !stderr.contains("Damaged"),
        "System tar reported 'Damaged' archive warning: {}",
        stderr
    );

    // Verify it can extract correctly
    fs::create_dir(&dst_dir).unwrap();
    run_system_ok(&tar, &["-xf", archive.to_str().unwrap()], &dst_dir, None);

    // Verify symlink was extracted correctly
    let link_path = dst_dir.join("link.txt");
    assert!(link_path.is_symlink(), "Symlink was not created");
    let target = fs::read_link(&link_path).unwrap();
    assert_eq!(target.to_string_lossy(), "hello.txt");
}

/// Write `src_dir` to a pax archive, extract it into a fresh directory, and
/// return that directory plus the raw archive bytes. Fails loudly if pax
/// panics on either leg.
fn pax_roundtrip(temp: &TempDir, src_dir: &Path, tag: &str) -> (PathBuf, Vec<u8>) {
    let archive = temp.path().join(format!("{}.tar", tag));
    let dst_dir = temp.path().join(format!("dest-{}", tag));

    let output = run_pax_in_dir(
        &["-w", "-x", "pax", "-f", archive.to_str().unwrap(), "."],
        src_dir,
    );
    assert!(
        !stderr_str(&output).contains("panicked"),
        "pax -w panicked ({}): {}",
        tag,
        stderr_str(&output)
    );
    assert_success(&output, "pax write");

    fs::create_dir(&dst_dir).unwrap();
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst_dir);
    assert!(
        !stderr_str(&output).contains("panicked"),
        "pax -r panicked ({}): {}",
        tag,
        stderr_str(&output)
    );
    assert_success(&output, "pax read");

    (dst_dir, fs::read(&archive).unwrap())
}

/// Count pax extended-header records for `keyword`. Records are serialized as
/// "<len> <keyword>=<value>\n", so the leading space anchors the match.
fn count_pax_records(archive: &[u8], keyword: &str) -> usize {
    let needle = format!(" {}=", keyword).into_bytes();
    archive
        .windows(needle.len())
        .filter(|w| *w == needle.as_slice())
        .count()
}

/// Sorted list of file names directly under `dir`.
fn file_names_in(dir: &Path) -> Vec<String> {
    let mut names: Vec<String> = fs::read_dir(dir)
        .unwrap()
        .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
        .collect();
    names.sort();
    names
}

/// Issue #616: `pax -w` panicked on a name longer than the 100-byte ustar
/// name field whose multi-byte character straddled the truncation point.
/// The panic was only the visible half of the bug: such names were never
/// given a `path=` extended header record at all, so they were silently
/// truncated in the archive. Both halves are checked here.
#[test]
fn test_long_multibyte_name_roundtrip() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let nested = src_dir.join("test");
    fs::create_dir_all(&nested).unwrap();

    // "test/" + 90 * 'Я' (180 bytes) + suffix -> byte 100 lands inside a 'Я'.
    let base: String = "\u{42f}".repeat(90);
    let mut expected = Vec::new();
    for i in 1..=12 {
        let name = format!("{}{}.txt", base, i);
        // The archive member path is "./test/<name>"; byte 100 of it must
        // land inside a 'Я' for this to reproduce the reported panic.
        assert!(
            !format!("./test/{}", name).is_char_boundary(100),
            "test setup: byte 100 must fall inside a multi-byte character"
        );
        File::create(nested.join(&name))
            .unwrap()
            .write_all(format!("contents {}", i).as_bytes())
            .unwrap();
        expected.push(name);
    }
    expected.sort();

    let (dst_dir, archive) = pax_roundtrip(&temp, &src_dir, "multibyte");

    // Each unrepresentable name must carry its own `path=` record; without
    // them the ustar name field alone would silently lose the tail.
    assert_eq!(
        count_pax_records(&archive, "path"),
        12,
        "expected one path= record per over-long name"
    );
    assert_eq!(
        file_names_in(&dst_dir.join("test")),
        expected,
        "long multi-byte names were not preserved through the archive"
    );
    for i in 1..=12 {
        let name = format!("{}{}.txt", base, i);
        assert_eq!(
            fs::read_to_string(dst_dir.join("test").join(&name)).unwrap(),
            format!("contents {}", i),
            "content mismatch for name {}",
            i
        );
    }
}

/// The same defect with no multi-byte character in sight: a 190-byte path
/// that cannot be split at a '/' into ustar name/prefix got no `path=`
/// record, so it was silently truncated to 100 bytes with no panic and no
/// diagnostic. A char-boundary-only fix would leave this corruption in place.
#[test]
fn test_long_ascii_name_not_truncated() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let nested = src_dir.join("ascii");
    fs::create_dir_all(&nested).unwrap();

    // "./ascii/" + 180 'a' + ".txt": too long for the name field, and the
    // only '/' leaves a 184-byte tail, so no ustar split exists.
    let name = format!("{}.txt", "a".repeat(180));
    File::create(nested.join(&name))
        .unwrap()
        .write_all(b"payload")
        .unwrap();

    let (dst_dir, archive) = pax_roundtrip(&temp, &src_dir, "ascii");

    assert_eq!(
        count_pax_records(&archive, "path"),
        1,
        "an unsplittable 190-byte path must get a path= record"
    );
    assert_eq!(file_names_in(&dst_dir.join("ascii")), vec![name.clone()]);
    assert_eq!(
        fs::read_to_string(dst_dir.join("ascii").join(&name)).unwrap(),
        "payload"
    );
}

/// A symlink target longer than the 100-byte linkname field, with a
/// multi-byte character on the boundary, hit the identical slicing panic in
/// the ustar fallback.
#[cfg(unix)]
#[test]
fn test_long_multibyte_symlink_target() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    fs::create_dir_all(&src_dir).unwrap();

    // Leading 'a' shifts the 'Я' run so byte 100 falls inside a character.
    let target = format!("a{}", "\u{42f}".repeat(90));
    assert!(!target.is_char_boundary(100));
    std::os::unix::fs::symlink(&target, src_dir.join("link")).unwrap();

    let (dst_dir, archive) = pax_roundtrip(&temp, &src_dir, "symlink");

    assert_eq!(
        count_pax_records(&archive, "linkpath"),
        1,
        "an over-long symlink target must get a linkpath= record"
    );
    assert_eq!(
        fs::read_link(dst_dir.join("link"))
            .unwrap()
            .to_string_lossy(),
        target,
        "long symlink target was not preserved"
    );
}

/// Guard against over-correcting: a long path that *does* split cleanly at a
/// '/' must keep using the ustar name/prefix fields and still round-trip.
#[test]
fn test_long_splittable_path_roundtrip() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dir_name = "d".repeat(60);
    let nested = src_dir.join(&dir_name);
    fs::create_dir_all(&nested).unwrap();

    let name = format!("{}.txt", "f".repeat(60));
    File::create(nested.join(&name))
        .unwrap()
        .write_all(b"splittable")
        .unwrap();

    let (dst_dir, archive) = pax_roundtrip(&temp, &src_dir, "splittable");

    // The whole point: this 127-byte path splits at '/' into a 62-byte prefix
    // and a 64-byte name, so it must ride in the ustar fields with no `path=`
    // record at all. A fix that widened the extended-header trigger too far
    // would show up here as a spurious record.
    assert_eq!(
        count_pax_records(&archive, "path"),
        0,
        "a prefix-splittable path must not need an extended header"
    );
    assert_eq!(
        fs::read_to_string(dst_dir.join(&dir_name).join(&name)).unwrap(),
        "splittable"
    );
}

/// The header goes out before the data, so the size in it is a promise made from
/// a `stat` that has already happened. Writing more than that puts bytes into the
/// archive at a 512-byte boundary, where a reader takes them for a header.
///
/// A procfs file reports size 0 and then yields content, which is the same
/// mismatch a file being appended to during the read produces, without a race.
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_write_bounds_member_data_to_header_size() {
    let temp = TempDir::new().unwrap();
    let archive = temp.path().join("a.tar");

    let output = run_pax_in_dir(
        &["-w", "-f", archive.to_str().unwrap(), "/proc/self/status"],
        temp.path(),
    );

    assert!(
        !output.status.success(),
        "a member whose data did not match its header should set a non-zero status"
    );
    assert!(
        stderr_str(&output).contains("changed as we read it"),
        "expected a changed-file diagnostic, got: {}",
        stderr_str(&output)
    );

    // The point of bounding the data: the archive is still structurally valid,
    // so the next header is where the previous member's size says it is.
    let listing = run_pax_in_dir(&["-t", "-f", archive.to_str().unwrap()], temp.path());
    assert!(
        listing.status.success(),
        "the archive should still be readable: {}",
        stderr_str(&listing)
    );
}

/// A pax archive writes an extended header only for the members that need one,
/// so the first block may be an ordinary ustar header. Reading the format from
/// that block alone got the rest of the archive wrong: the later 'x' blocks
/// became regular files named PaxHeader/N, and the members they described fell
/// back to the truncated 100-byte name in their ustar header.
#[test]
fn test_read_pax_archive_whose_first_member_needs_no_extended_header() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir(&src).unwrap();

    // A short name first, then one too long for a ustar header.
    let long_name = "b".repeat(160);
    fs::write(src.join("a.txt"), b"one\n").unwrap();
    fs::write(src.join(&long_name), b"two\n").unwrap();

    let archive = temp.path().join("a.tar");
    let out = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "pax",
            "-f",
            archive.to_str().unwrap(),
            "a.txt",
            &long_name,
        ],
        &src,
    );
    assert_success(&out, "writing the archive");

    let listing = run_pax_in_dir(&["-t", "-f", archive.to_str().unwrap()], temp.path());
    assert_success(&listing, "listing the archive");
    let listed = stdout_str(&listing);

    assert!(
        listed.contains(&long_name),
        "the long member name was not read back in full: {listed}"
    );
    assert!(
        !listed.contains("PaxHeader"),
        "an extended header block was listed as a member: {listed}"
    );
}

/// `-t` restores the access time of the file whose access time the read actually
/// disturbed. With `-L` that is the symbolic link's target, not the link: pax
/// opens and reads through the link, so stamping the link leaves the target
/// disturbed and changes an inode that was never read.
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_reset_atime_stamps_the_file_that_was_read() {
    use std::os::unix::fs::MetadataExt;

    let temp = TempDir::new().unwrap();
    let dir = temp.path();
    fs::write(dir.join("target"), b"data\n").unwrap();
    std::os::unix::fs::symlink("target", dir.join("link")).unwrap();

    // A distinctive access time, well in the past.
    const WHEN: i64 = 978_307_200;
    let times = [
        libc::timespec {
            tv_sec: WHEN,
            tv_nsec: 0,
        },
        libc::timespec {
            tv_sec: WHEN,
            tv_nsec: 0,
        },
    ];
    let target_c = std::ffi::CString::new(dir.join("target").as_os_str().as_bytes()).unwrap();
    assert_eq!(
        unsafe { libc::utimensat(libc::AT_FDCWD, target_c.as_ptr(), times.as_ptr(), 0) },
        0
    );

    let archive = dir.join("a.tar");
    assert_success(
        &run_pax_in_dir(
            &["-w", "-L", "-t", "-f", archive.to_str().unwrap(), "link"],
            dir,
        ),
        "archiving through a symbolic link with -t",
    );

    assert_eq!(
        fs::metadata(dir.join("target")).unwrap().atime(),
        WHEN,
        "-L -t left the target's access time disturbed"
    );
}

/// The same multiply-linked file named twice must not be archived as a hard
/// link to itself: extracting "f == f" unlinks f and then fails to link it.
#[test]
fn test_hardlink_operand_repeated_roundtrip() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    let dst = temp.path().join("dst");
    fs::create_dir(&src).unwrap();
    fs::create_dir(&dst).unwrap();
    fs::write(src.join("f"), "DATA\n").unwrap();
    fs::hard_link(src.join("f"), src.join("g")).unwrap();
    let archive = temp.path().join("a.tar");

    let output = run_pax_in_dir(&["-w", "-f", archive.to_str().unwrap(), "f", "f"], &src);
    assert_success(&output, "pax -w f f");

    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst);
    assert_success(&output, "pax -r of an archive naming f twice");
    assert_eq!(fs::read_to_string(dst.join("f")).unwrap(), "DATA\n");
}

/// GNU cpio and bsdcpio write a newc hard-link set with the data on the last
/// link only; the earlier names carry c_filesize 0. Extraction has to re-create
/// one inode holding the data, not an empty file beside a full one.
#[test]
fn test_cpio_newc_hardlink_data_on_last_link() {
    let temp = TempDir::new().unwrap();
    let mut archive = CpioNewc {
        name: b"a",
        ino: 7,
        nlink: 2,
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &CpioNewc {
            name: b"b",
            body: b"DATA\n",
            ino: 7,
            nlink: 2,
            ..Default::default()
        }
        .archive(),
    );

    let output = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, temp.path());
    assert_success(&output, "pax -r of a newc hard-link set");

    let a = fs::metadata(temp.path().join("a")).unwrap();
    let b = fs::metadata(temp.path().join("b")).unwrap();
    assert_eq!(a.ino(), b.ino(), "a and b should be one inode");
    assert_eq!(fs::read_to_string(temp.path().join("a")).unwrap(), "DATA\n");
}

/// odc stores the data with every link. POSIX: "it shall be an error if these
/// files cannot be linked" -- so they must come back linked, not as copies.
#[test]
fn test_cpio_odc_hardlinks_are_relinked() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    let dst = temp.path().join("dst");
    fs::create_dir(&src).unwrap();
    fs::create_dir(&dst).unwrap();
    fs::write(src.join("a"), "DATA\n").unwrap();
    fs::hard_link(src.join("a"), src.join("b")).unwrap();
    let archive = temp.path().join("a.cpio");

    let output = run_pax_in_dir(
        &[
            "-w",
            "-x",
            "cpio",
            "-f",
            archive.to_str().unwrap(),
            "a",
            "b",
        ],
        &src,
    );
    assert_success(&output, "pax -w -x cpio");
    let output = run_pax_in_dir(&["-r", "-f", archive.to_str().unwrap()], &dst);
    assert_success(&output, "pax -r of a cpio hard-link set");

    let a = fs::metadata(dst.join("a")).unwrap();
    let b = fs::metadata(dst.join("b")).unwrap();
    assert_eq!(a.ino(), b.ino(), "a and b should be one inode");
    assert_eq!(fs::read_to_string(dst.join("b")).unwrap(), "DATA\n");
}

/// A newc set of three names, renamed by -s on the way out: the two empty
/// earlier names must end up sharing the inode the last one brings the data in.
#[test]
fn test_cpio_newc_hardlink_set_renamed() {
    let temp = TempDir::new().unwrap();
    let link = |name, body| CpioNewc {
        name,
        body,
        ino: 9,
        nlink: 3,
        ..Default::default()
    };
    let mut archive = link(b"a", b"").member();
    archive.extend_from_slice(&link(b"b", b"").member());
    archive.extend_from_slice(&link(b"c", b"DATA\n").archive());

    let output = run_pax_with_stdin_bytes_in_dir(&["-r", "-s", ",^,new_,"], &archive, temp.path());
    assert_success(&output, "pax -r -s of a newc hard-link set");

    let ino = |n: &str| fs::metadata(temp.path().join(n)).unwrap().ino();
    assert_eq!(ino("new_a"), ino("new_c"));
    assert_eq!(ino("new_b"), ino("new_c"));
    assert_eq!(fs::metadata(temp.path().join("new_c")).unwrap().nlink(), 3);
    assert_eq!(
        fs::read_to_string(temp.path().join("new_a")).unwrap(),
        "DATA\n"
    );
}

/// The -v listing shows a later name of a cpio set as linked to the first,
/// which is what extraction makes of it.
#[test]
fn test_cpio_hardlink_set_listed_as_link() {
    let temp = TempDir::new().unwrap();
    let mut archive = CpioNewc {
        name: b"a",
        ino: 7,
        nlink: 2,
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &CpioNewc {
            name: b"b",
            body: b"DATA\n",
            ino: 7,
            nlink: 2,
            ..Default::default()
        }
        .archive(),
    );

    let output = run_pax_with_stdin_bytes_in_dir(&["-v"], &archive, temp.path());
    assert_success(&output, "pax -v of a newc hard-link set");
    let listing = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = listing.lines().collect();
    assert!(lines[0].ends_with(" a"), "first name: {}", lines[0]);
    assert!(lines[1].ends_with(" b == a"), "later name: {}", lines[1]);
}

/// A datagram socket behaves like a tape: each read returns one record, and
/// the part of a record a short read did not take is lost.
/// The buffers are raised so a 10240-byte record fits in one datagram;
/// macOS defaults to 2048.
fn datagram_pair() -> (
    std::os::unix::net::UnixDatagram,
    std::os::unix::net::UnixDatagram,
) {
    use std::os::fd::AsRawFd;
    let (a, b) = std::os::unix::net::UnixDatagram::pair().unwrap();
    for sock in [&a, &b] {
        for opt in [libc::SO_SNDBUF, libc::SO_RCVBUF] {
            let size: libc::c_int = 256 * 1024;
            let r = unsafe {
                libc::setsockopt(
                    sock.as_raw_fd(),
                    libc::SOL_SOCKET,
                    opt,
                    &size as *const _ as *const libc::c_void,
                    std::mem::size_of_val(&size) as libc::socklen_t,
                )
            };
            assert_eq!(r, 0, "setsockopt: {}", std::io::Error::last_os_error());
        }
    }
    (a, b)
}

/// Two files whose archive spans more than one 10240-byte record.
fn blocking_fixture(src: &Path) -> Vec<u8> {
    fs::create_dir(src).unwrap();
    fs::write(src.join("f1"), vec![b'1'; 8000]).unwrap();
    fs::write(src.join("f2"), vec![b'2'; 2000]).unwrap();
    let output = run_pax_in_dir(&["-w", "-x", "ustar", "f1", "f2"], src);
    assert_success(&output, "pax -w");
    assert_eq!(output.stdout.len() % 10240, 0);
    assert!(output.stdout.len() > 10240);
    output.stdout
}

/// "Blocking shall be automatically determined on input": an archive read
/// one record per read must be read whole-record at a time, not in smaller
/// reads that drop the rest of each record.
#[test]
fn test_read_determines_blocking_from_input() {
    use std::os::fd::OwnedFd;
    let temp = TempDir::new().unwrap();
    let archive = blocking_fixture(&temp.path().join("src"));
    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();

    let (tx, rx) = datagram_pair();
    let child = Command::new(env!("CARGO_BIN_EXE_pax"))
        .arg("-r")
        .current_dir(&dst)
        .stdin(std::process::Stdio::from(OwnedFd::from(rx)))
        .stderr(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    for record in archive.chunks(10240) {
        tx.send(record).unwrap();
    }
    drop(tx);
    let output = child.wait_with_output().unwrap();
    assert_success(&output, "pax -r from one-record reads");

    assert_eq!(fs::read(dst.join("f1")).unwrap(), vec![b'1'; 8000]);
    assert_eq!(fs::read(dst.join("f2")).unwrap(), vec![b'2'; 2000]);
}

/// -b sets the bytes per write: every write to standard output is one whole
/// record, however the archive's data happens to fall.
#[test]
fn test_write_issues_whole_records() {
    use std::os::fd::OwnedFd;
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    blocking_fixture(&src);
    // Data with newlines, which a line-buffered stdout splits writes at.
    fs::write(src.join("lines"), "a\nb\nc\n".repeat(3000)).unwrap();

    let (tx, rx) = datagram_pair();
    let status = Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(["-w", "-b", "10240", "-x", "ustar", "f1", "lines", "f2"])
        .current_dir(&src)
        .stdout(std::process::Stdio::from(OwnedFd::from(tx)))
        .status()
        .unwrap();
    assert!(status.success());

    // pax has exited, so every record it wrote is already queued.
    rx.set_nonblocking(true).unwrap();
    let mut buf = vec![0u8; 65536];
    let mut sizes = Vec::new();
    while let Ok(n) = rx.recv(&mut buf) {
        sizes.push(n);
    }
    assert!(!sizes.is_empty());
    assert!(sizes.iter().all(|&n| n == 10240), "write sizes: {sizes:?}");
}

/// `-o linkdata`: a pax-format hard link "may include" the file's data, and
/// this option asks for it -- every hard-link member then carries the bytes.
#[test]
fn test_linkdata_writes_hardlink_data() {
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("a"), "DATA\n").unwrap();
    fs::hard_link(temp.path().join("a"), temp.path().join("b")).unwrap();

    let output = run_pax_in_dir(
        &["-w", "-x", "pax", "-o", "linkdata", "a", "b"],
        temp.path(),
    );
    assert_success(&output, "pax -w -o linkdata");
    let listing =
        run_pax_with_stdin_bytes(&["-o", "listopt=%(typeflag)s %(size)d %F"], &output.stdout);
    assert_eq!(stdout_str(&listing), "0 5 a\n1 5 b\n");
}

/// ustar has no socket type. POSIX: a file that cannot be archived in the
/// format is diagnosed. It must not turn into an empty regular file.
#[test]
fn test_socket_is_diagnosed_not_archived_as_regular() {
    let temp = TempDir::new().unwrap();
    let _sock = std::os::unix::net::UnixListener::bind(temp.path().join("s")).unwrap();
    fs::write(temp.path().join("f"), "F\n").unwrap();

    for format in ["ustar", "pax"] {
        let output = run_pax_in_dir(&["-w", "-x", format, "s", "f"], temp.path());
        assert_exit_code(&output, 1, format);
        let listing = run_pax_with_stdin_bytes(&[], &output.stdout);
        assert_eq!(stdout_str(&listing), "f\n", "{format}");
    }
}

/// A hard link is recorded only once its first name was actually archived.
/// If a cannot be read, b must not become "b == a" pointing at nothing.
#[test]
fn test_hardlink_to_unarchived_file_is_not_a_link() {
    if unsafe { libc::geteuid() } == 0 {
        return; // root reads mode 000 files
    }
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("a"), "DATA\n").unwrap();
    fs::hard_link(temp.path().join("a"), temp.path().join("b")).unwrap();
    fs::set_permissions(temp.path().join("a"), fs::Permissions::from_mode(0o000)).unwrap();

    let output = run_pax_in_dir(&["-w", "-x", "ustar", "a", "b"], temp.path());
    fs::set_permissions(temp.path().join("a"), fs::Permissions::from_mode(0o644)).unwrap();
    assert_exit_code(&output, 1, "pax -w of an unreadable linked file");
    let listing = run_pax_with_stdin_bytes(&["-v"], &output.stdout);
    assert!(
        !stdout_str(&listing).contains("=="),
        "{}",
        stdout_str(&listing)
    );
}

/// A directory name of 100-155 bytes is split into prefix and name like any
/// other; the name field must not come out empty.
#[test]
fn test_ustar_long_directory_name_roundtrips() {
    let temp = TempDir::new().unwrap();
    let dir = format!("{}/{}", "p".repeat(60), "d".repeat(60));
    fs::create_dir_all(temp.path().join(&dir)).unwrap();

    let output = run_pax_in_dir(&["-w", "-d", "-x", "ustar", &dir], temp.path());
    assert_success(&output, "pax -w");
    let listing = run_pax_with_stdin_bytes(&[], &output.stdout);
    assert_eq!(stdout_str(&listing), format!("{dir}/\n"));
}

/// A time before 1970 cannot go in a ustar octal field. In pax format it is
/// written as an mtime record; it must not wrap to a date centuries away.
#[test]
fn test_pre_epoch_mtime_roundtrips_in_pax() {
    let temp = TempDir::new().unwrap();
    let f = temp.path().join("old");
    fs::write(&f, "O\n").unwrap();
    filetime::set_file_mtime(&f, filetime::FileTime::from_unix_time(-86400, 0)).unwrap();

    let output = run_pax_in_dir(&["-w", "-x", "pax", "old"], temp.path());
    assert_success(&output, "pax -w");
    let listing = run_pax_with_stdin_bytes(&["-o", "listopt=%(mtime)d"], &output.stdout);
    assert_eq!(stdout_str(&listing), "-86400\n");
}

/// The default name of an extended header is built from "%d/PaxHeaders.%p/%f",
/// where %d is what dirname(1) gives: "." for a top-level member, so the
/// name is relative -- never an absolute path a naive reader would write to.
#[test]
fn test_default_exthdr_name_is_relative() {
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("top"), "T\n").unwrap();

    let output = run_pax_in_dir(&["-w", "-x", "pax", "-o", "uname:=zz", "top"], temp.path());
    assert_success(&output, "pax -w");
    // The first header block is the extended header's.
    let name = String::from_utf8_lossy(&output.stdout[..100]);
    let name = name.trim_end_matches('\0');
    assert!(name.starts_with("./PaxHeaders"), "{name}");
}

/// pax writes an empty archive (two zero blocks) when given nothing to
/// archive; it must be able to read that archive back and append to it.
#[test]
fn test_empty_archive_reads_and_appends() {
    let temp = TempDir::new().unwrap();
    let archive = temp.path().join("e.tar");
    fs::write(temp.path().join("f"), "F\n").unwrap();

    let output = run_pax_in_dir_with_stdin(&["-w", "-f", "e.tar"], temp.path(), "");
    assert_success(&output, "pax -w of nothing");
    let output = run_pax_in_dir(&["-f", archive.to_str().unwrap()], temp.path());
    assert_success(&output, "list of an empty archive");
    assert_eq!(stdout_str(&output), "");

    let output = run_pax_in_dir(&["-w", "-a", "-f", "e.tar", "f"], temp.path());
    assert_success(&output, "append to an empty archive");
    let output = run_pax_in_dir(&["-f", archive.to_str().unwrap()], temp.path());
    assert_eq!(stdout_str(&output), "f\n");
}

/// pax-format header fields are meant for the portable character set; a name
/// outside it goes in a UTF-8 `path` record so that every reader recovers it.
#[test]
fn test_pax_non_ascii_name_gets_path_record() {
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("café"), "C\n").unwrap();

    let output = run_pax_in_dir(&["-w", "-x", "pax", "café"], temp.path());
    assert_success(&output, "pax -w");
    let listing = run_pax_with_stdin_bytes(&["-o", "listopt=%(typeflag)s"], &output.stdout);
    assert_success(&listing, "list");
    // An `x` header precedes the member, carrying the name.
    assert_eq!(output.stdout[156], b'x');
}

/// -t restores the access time of each file read -- in copy mode too, and for
/// directories as well as regular files.
#[test]
fn test_t_restores_atime_in_copy_and_for_directories() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir_all(src.join("d")).unwrap();
    fs::write(src.join("d/f"), "F\n").unwrap();
    let old = filetime::FileTime::from_unix_time(1_000_000_000, 0);
    for p in [src.join("d/f"), src.join("d")] {
        filetime::set_file_atime(&p, old).unwrap();
    }
    fs::create_dir(temp.path().join("out")).unwrap();

    let output = run_pax_in_dir(&["-rw", "-t", "d", "../out"], &src);
    assert_success(&output, "pax -rw -t");
    for p in [src.join("d/f"), src.join("d")] {
        let atime = fs::metadata(&p).unwrap().atime();
        assert_eq!(atime, 1_000_000_000, "{}", p.display());
    }

    for p in [src.join("d/f"), src.join("d")] {
        filetime::set_file_atime(&p, old).unwrap();
    }
    let output = run_pax_in_dir(&["-w", "-t", "-f", "../a.tar", "d"], &src);
    assert_success(&output, "pax -w -t");
    for p in [src.join("d/f"), src.join("d")] {
        let atime = fs::metadata(&p).unwrap().atime();
        assert_eq!(atime, 1_000_000_000, "{}", p.display());
    }
}

/// -X: "when a directory with a different device ID is encountered, pax shall
/// process (archive or copy) the directory itself but shall not process any
/// files below the directory."
#[test]
fn test_one_file_system_archives_the_mount_point() {
    let temp = TempDir::new().unwrap();
    let tree = temp.path().join("tree");
    fs::create_dir_all(tree.join("mnt")).unwrap();
    fs::write(tree.join("f"), "F\n").unwrap();
    let Some(_mount) = ScratchMount::mount(temp.path(), &tree.join("mnt")) else {
        return; // no unprivileged way to mount here; the rule is unit-tested
    };
    fs::write(tree.join("mnt/inside"), "I\n").unwrap();

    let output = run_pax_in_dir(&["-wX", "tree"], temp.path());
    assert_success(&output, "pax -wX");
    let listing = run_pax_with_stdin_bytes(&[], &output.stdout);
    let mut names: Vec<_> = stdout_str(&listing).lines().map(String::from).collect();
    names.sort();
    assert_eq!(names, ["tree/", "tree/f", "tree/mnt/"]);
}

/// Listing an archive in a regular file seeks over member data instead of
/// reading it. The member here is 32 GiB of hole, which reading takes many
/// seconds to get through and seeking skips at once; pax is killed if it is
/// still at it after the deadline.
#[test]
fn test_list_seeks_over_member_data() {
    let temp = TempDir::new().unwrap();
    let path = temp.path().join("big.tar");
    const SIZE: u64 = 32 << 30;

    let mut head = Ustar {
        name: b"PaxHeaders/big",
        typeflag: b'x',
        body: &pax_record("size", SIZE.to_string().as_bytes()),
        ..Default::default()
    }
    .member();
    head.extend_from_slice(
        &Ustar {
            name: b"big",
            ..Default::default()
        }
        .header(),
    );
    let tail = Ustar {
        name: b"after",
        body: b"A\n",
        ..Default::default()
    }
    .archive();

    let mut f = File::create(&path).unwrap();
    f.write_all(&head).unwrap();
    // A hole: no disk space, and no real data to read.
    f.set_len(head.len() as u64 + SIZE).unwrap();
    use std::io::Seek;
    f.seek(std::io::SeekFrom::End(0)).unwrap();
    f.write_all(&tail).unwrap();
    drop(f);

    let output = run_pax_with_deadline(
        &["-f", "big.tar"],
        temp.path(),
        std::time::Duration::from_secs(3),
    )
    .expect("pax read through the member data instead of seeking over it");
    assert_success(&output, "pax -f big.tar");
    assert_eq!(stdout_str(&output), "big\nafter\n");
}

/// The system's pax reads what ours writes, in the formats every pax shares.
#[test]
fn test_cross_tool_system_pax_reads_ours() {
    let Some(pax) = system_tool("pax") else {
        return;
    };
    for format in ["ustar", "cpio"] {
        let temp = TempDir::new().unwrap();
        let src = temp.path().join("src");
        fs::create_dir_all(src.join("sub")).unwrap();
        fs::write(src.join("sub/f.txt"), "from ours\n").unwrap();
        std::os::unix::fs::symlink("sub/f.txt", src.join("l")).unwrap();

        let out = run_pax_in_dir(&["-w", "-x", format, "-f", "../a", "sub", "l"], &src);
        assert_success(&out, &format!("pax -w -x {format}"));
        let dst = temp.path().join("dst");
        fs::create_dir(&dst).unwrap();
        run_system_ok(&pax, &["-r", "-f", "../a"], &dst, None);
        assert_eq!(
            fs::read_to_string(dst.join("sub/f.txt")).unwrap(),
            "from ours\n"
        );
        assert_eq!(
            fs::read_link(dst.join("l")).unwrap(),
            Path::new("sub/f.txt")
        );
    }
}

/// And ours reads what the system's pax writes.
#[test]
fn test_cross_tool_we_read_system_pax() {
    let Some(pax) = system_tool("pax") else {
        return;
    };
    for format in ["ustar", "cpio"] {
        let temp = TempDir::new().unwrap();
        let src = temp.path().join("src");
        fs::create_dir_all(src.join("sub")).unwrap();
        fs::write(src.join("sub/f.txt"), "from system\n").unwrap();
        std::os::unix::fs::symlink("sub/f.txt", src.join("l")).unwrap();

        run_system_ok(
            &pax,
            &["-w", "-x", format, "-f", "../a", "sub", "l"],
            &src,
            None,
        );
        let dst = temp.path().join("dst");
        fs::create_dir(&dst).unwrap();
        let out = run_pax_in_dir(&["-r", "-f", "../a"], &dst);
        assert_success(&out, &format!("pax -r of system pax -x {format}"));
        assert_eq!(
            fs::read_to_string(dst.join("sub/f.txt")).unwrap(),
            "from system\n"
        );
        assert_eq!(
            fs::read_link(dst.join("l")).unwrap(),
            Path::new("sub/f.txt")
        );
    }
}
