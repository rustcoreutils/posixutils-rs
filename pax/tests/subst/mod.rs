//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Substitution option tests (-s)

use crate::common::*;
use plib::tmp::TempDir;
use std::fs::{self, File};
use std::io::Write;
use std::os::unix::fs::MetadataExt;
use std::path::Path;

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

/// -s renames a hard link's target along with the member, since the target is
/// a member name too. Linking the renamed P/b to the unsubstituted "a" would
/// reach an unrelated ./a that happened to be in the extraction directory.
#[test]
fn test_subst_renames_hardlink_target() {
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("a"), "UNRELATED\n").unwrap();
    let mut archive = Ustar {
        name: b"a",
        body: b"DATA\n",
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &Ustar {
            name: b"b",
            typeflag: b'1',
            linkname: b"a",
            ..Default::default()
        }
        .archive(),
    );

    let output = run_pax_with_stdin_bytes_in_dir(&["-r", "-s", ",^,P/,"], &archive, temp.path());
    assert_success(&output, "pax -r -s of a hard link");

    let pa = fs::metadata(temp.path().join("P/a")).unwrap();
    let pb = fs::metadata(temp.path().join("P/b")).unwrap();
    assert_eq!(pa.ino(), pb.ino(), "P/b should be linked to P/a");
    assert_eq!(
        fs::read_to_string(temp.path().join("a")).unwrap(),
        "UNRELATED\n"
    );
}

/// Build d/x and d/sub/y under `root`.
fn subst_tree(root: &Path) {
    fs::create_dir_all(root.join("d/sub")).unwrap();
    fs::write(root.join("d/x"), "X\n").unwrap();
    fs::write(root.join("d/sub/y"), "Y\n").unwrap();
}

/// Copy mode is defined "as if" written to an archive and extracted, so -s
/// applies once to each source pathname -- not again to a name built from an
/// already-substituted parent, which compounded the prefix per level.
#[test]
fn test_copy_subst_applies_once_per_name() {
    let temp = TempDir::new().unwrap();
    subst_tree(temp.path());
    fs::create_dir(temp.path().join("out")).unwrap();

    let output = run_pax_in_dir(&["-rw", "-s", ",^,pre/,", "d", "out"], temp.path());
    assert_success(&output, "pax -rw -s");

    assert_eq!(
        fs::read_to_string(temp.path().join("out/pre/d/x")).unwrap(),
        "X\n"
    );
    assert_eq!(
        fs::read_to_string(temp.path().join("out/pre/d/sub/y")).unwrap(),
        "Y\n"
    );
    assert!(!temp.path().join("out/pre/pre").exists());
}

/// A name -s maps to the empty string is ignored -- that name, not the
/// hierarchy below it, whose own names are substituted on their own.
#[test]
fn test_write_subst_to_empty_keeps_the_subtree() {
    let temp = TempDir::new().unwrap();
    subst_tree(temp.path());
    let archive = temp.path().join("a.tar");

    let output = run_pax_in_dir(
        &["-w", "-s", ",^d$,,", "-f", archive.to_str().unwrap(), "d"],
        temp.path(),
    );
    assert_success(&output, "pax -w -s to empty");

    let output = run_pax_in_dir(&["-f", archive.to_str().unwrap()], temp.path());
    let mut names: Vec<String> = stdout_str(&output).lines().map(String::from).collect();
    names.sort();
    assert_eq!(names, ["d/sub/", "d/sub/y", "d/x"]);
}
