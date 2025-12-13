//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Copy mode tests (-r -w)

use crate::common::*;
use std::fs::{self, File};
use std::io::Write;
use tempfile::TempDir;

#[test]
fn test_copy_mode_basic() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    create_test_files(&src_dir);

    // Create destination directory
    fs::create_dir(&dst_dir).unwrap();

    // Copy files using copy mode (-r -w)
    let output = run_pax_in_dir(&["-r", "-w", ".", dst_dir.to_str().unwrap()], &src_dir);
    assert_success(&output, "pax copy");

    // Verify files were copied correctly
    // The "." directory contents should be at dst_dir/.
    let copied_dot = dst_dir.join(".");
    assert!(
        copied_dot.join("file.txt").exists() || dst_dir.join("file.txt").exists(),
        "file.txt should be copied"
    );
}

#[test]
fn test_copy_mode_file() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let src_file = src_dir.join("test.txt");
    let mut f = File::create(&src_file).unwrap();
    writeln!(f, "Test content").unwrap();

    // Create destination directory
    fs::create_dir(&dst_dir).unwrap();

    // Copy single file
    let output = run_pax(&[
        "-r",
        "-w",
        src_file.to_str().unwrap(),
        dst_dir.to_str().unwrap(),
    ]);
    assert_success(&output, "pax copy");

    // Verify file was copied
    let dst_file = dst_dir.join("test.txt");
    assert!(dst_file.exists(), "test.txt should be copied");
    let content = fs::read_to_string(&dst_file).unwrap();
    assert!(content.contains("Test content"), "Content mismatch");
}

#[test]
fn test_copy_mode_directory() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");

    // Create source directory structure
    fs::create_dir(&src_dir).unwrap();
    let subdir = src_dir.join("mydir");
    fs::create_dir(&subdir).unwrap();
    let mut f = File::create(subdir.join("file1.txt")).unwrap();
    writeln!(f, "Content 1").unwrap();
    let mut f = File::create(subdir.join("file2.txt")).unwrap();
    writeln!(f, "Content 2").unwrap();

    // Create destination directory
    fs::create_dir(&dst_dir).unwrap();

    // Copy directory
    let output = run_pax(&[
        "-r",
        "-w",
        subdir.to_str().unwrap(),
        dst_dir.to_str().unwrap(),
    ]);
    assert_success(&output, "pax copy");

    // Verify directory was copied
    let dst_subdir = dst_dir.join("mydir");
    assert!(dst_subdir.is_dir(), "mydir should be copied");
    assert!(
        dst_subdir.join("file1.txt").exists(),
        "file1.txt should exist"
    );
    assert!(
        dst_subdir.join("file2.txt").exists(),
        "file2.txt should exist"
    );

    let c1 = fs::read_to_string(dst_subdir.join("file1.txt")).unwrap();
    assert!(c1.contains("Content 1"), "file1.txt content mismatch");
}

#[test]
fn test_copy_mode_verbose() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("verbose_test.txt")).unwrap();
    writeln!(f, "Verbose test").unwrap();

    // Create destination directory
    fs::create_dir(&dst_dir).unwrap();

    // Copy with verbose output
    let output = run_pax_in_dir(
        &[
            "-r",
            "-w",
            "-v",
            "verbose_test.txt",
            dst_dir.to_str().unwrap(),
        ],
        &src_dir,
    );
    assert_success(&output, "pax copy");

    // Verify verbose output on stderr
    let stderr = stderr_str(&output);
    assert!(
        stderr.contains("verbose_test.txt"),
        "Verbose output should list the file"
    );
}

#[test]
fn test_copy_mode_no_clobber() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("clobber.txt")).unwrap();
    writeln!(f, "New content").unwrap();

    // Create destination with existing file
    fs::create_dir(&dst_dir).unwrap();
    let mut f = File::create(dst_dir.join("clobber.txt")).unwrap();
    writeln!(f, "Existing content").unwrap();

    // Copy with -k (no clobber)
    let output = run_pax_in_dir(
        &["-r", "-w", "-k", "clobber.txt", dst_dir.to_str().unwrap()],
        &src_dir,
    );
    assert_success(&output, "pax copy");

    // Verify original file was preserved
    let content = fs::read_to_string(dst_dir.join("clobber.txt")).unwrap();
    assert!(
        content.contains("Existing"),
        "File was overwritten despite -k"
    );
}

#[cfg(unix)]
#[test]
fn test_copy_mode_link() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let src_file = src_dir.join("link_test.txt");
    let mut f = File::create(&src_file).unwrap();
    writeln!(f, "Link test content").unwrap();

    // Create destination directory
    fs::create_dir(&dst_dir).unwrap();

    // Copy with -l (hard link mode)
    let output = run_pax(&[
        "-r",
        "-w",
        "-l",
        src_file.to_str().unwrap(),
        dst_dir.to_str().unwrap(),
    ]);
    assert_success(&output, "pax copy");

    // Verify file exists and has same inode (hard link)
    let dst_file = dst_dir.join("link_test.txt");
    assert!(dst_file.exists(), "link_test.txt should exist");

    use std::os::unix::fs::MetadataExt;
    let src_meta = fs::metadata(&src_file).unwrap();
    let dst_meta = fs::metadata(&dst_file).unwrap();
    assert_eq!(
        src_meta.ino(),
        dst_meta.ino(),
        "Files should share the same inode (hard link)"
    );
}

#[cfg(unix)]
#[test]
fn test_copy_mode_symlink() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");

    // Create source file and symlink
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("target.txt")).unwrap();
    writeln!(f, "Target content").unwrap();
    std::os::unix::fs::symlink("target.txt", src_dir.join("symlink.txt")).unwrap();

    // Create destination directory
    fs::create_dir(&dst_dir).unwrap();

    // Copy symlink (without -L, so symlink itself is copied)
    let output = run_pax(&[
        "-r",
        "-w",
        src_dir.join("symlink.txt").to_str().unwrap(),
        dst_dir.to_str().unwrap(),
    ]);
    assert_success(&output, "pax copy");

    // Verify symlink was copied as symlink
    let dst_link = dst_dir.join("symlink.txt");
    assert!(
        dst_link.symlink_metadata().unwrap().is_symlink(),
        "Should be a symlink"
    );
    assert_eq!(
        fs::read_link(&dst_link).unwrap().to_str().unwrap(),
        "target.txt",
        "Symlink target mismatch"
    );
}

#[test]
fn test_copy_mode_multiple_files() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");

    // Create multiple source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("file1.txt")).unwrap();
    writeln!(f, "File 1").unwrap();
    let mut f = File::create(src_dir.join("file2.txt")).unwrap();
    writeln!(f, "File 2").unwrap();
    let mut f = File::create(src_dir.join("file3.txt")).unwrap();
    writeln!(f, "File 3").unwrap();

    // Create destination directory
    fs::create_dir(&dst_dir).unwrap();

    // Copy multiple files
    let output = run_pax(&[
        "-r",
        "-w",
        src_dir.join("file1.txt").to_str().unwrap(),
        src_dir.join("file2.txt").to_str().unwrap(),
        src_dir.join("file3.txt").to_str().unwrap(),
        dst_dir.to_str().unwrap(),
    ]);
    assert_success(&output, "pax copy");

    // Verify all files were copied
    assert!(dst_dir.join("file1.txt").exists(), "file1.txt should exist");
    assert!(dst_dir.join("file2.txt").exists(), "file2.txt should exist");
    assert!(dst_dir.join("file3.txt").exists(), "file3.txt should exist");
}

#[test]
fn test_copy_mode_stdin_file_list() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");

    // Create source files
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("stdin1.txt")).unwrap();
    writeln!(f, "Stdin file 1").unwrap();
    let mut f = File::create(src_dir.join("stdin2.txt")).unwrap();
    writeln!(f, "Stdin file 2").unwrap();

    // Create destination directory
    fs::create_dir(&dst_dir).unwrap();

    // Copy files from stdin list
    let file_list = "stdin1.txt\nstdin2.txt\n";
    let output = run_pax_in_dir_with_stdin(
        &["-r", "-w", dst_dir.to_str().unwrap()],
        &src_dir,
        file_list,
    );
    assert_success(&output, "pax copy from stdin");

    // Verify files were copied
    assert!(
        dst_dir.join("stdin1.txt").exists(),
        "stdin1.txt should exist"
    );
    assert!(
        dst_dir.join("stdin2.txt").exists(),
        "stdin2.txt should exist"
    );
}
