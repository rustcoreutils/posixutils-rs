//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Update mode tests (-u) and access time reset tests (-t)

use crate::common::*;
use plib::tmp::TempDir;
use std::fs::{self, File};
use std::io::Write;
use std::time::Duration;

#[cfg(unix)]
#[test]
fn test_update_mode_read_newer() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("update.txt")).unwrap();
    writeln!(f, "Original content").unwrap();
    drop(f);

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // Create destination with an OLDER file (by setting mtime to the past)
    fs::create_dir(&dst_dir).unwrap();
    let dst_file = dst_dir.join("update.txt");
    let mut f = File::create(&dst_file).unwrap();
    writeln!(f, "Older content").unwrap();
    drop(f);

    // Set the destination file's mtime to an older time
    let old_time = std::time::SystemTime::UNIX_EPOCH + Duration::from_secs(1000000000); // ~2001
    filetime::set_file_mtime(&dst_file, filetime::FileTime::from_system_time(old_time)).unwrap();

    // Extract with -u (update mode) - archive is newer, should overwrite
    let output = run_pax_in_dir(&["-r", "-u", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax update extract");

    // File should be overwritten with archive content
    let content = fs::read_to_string(&dst_file).unwrap();
    assert!(
        content.contains("Original"),
        "File should be updated with archive content (got: {})",
        content
    );
}

#[cfg(unix)]
#[test]
fn test_update_mode_read_older() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file with OLD mtime
    fs::create_dir(&src_dir).unwrap();
    let src_file = src_dir.join("update.txt");
    let mut f = File::create(&src_file).unwrap();
    writeln!(f, "Old archived content").unwrap();
    drop(f);

    // Set source file to an old mtime before archiving
    let old_time = std::time::SystemTime::UNIX_EPOCH + Duration::from_secs(1000000000);
    filetime::set_file_mtime(&src_file, filetime::FileTime::from_system_time(old_time)).unwrap();

    // Create archive (with old mtime preserved)
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // Create destination with a NEWER file (current time)
    fs::create_dir(&dst_dir).unwrap();
    let dst_file = dst_dir.join("update.txt");
    let mut f = File::create(&dst_file).unwrap();
    writeln!(f, "Newer existing content").unwrap();
    drop(f);

    // Extract with -u (update mode) - existing file is newer, should NOT overwrite
    let output = run_pax_in_dir(&["-r", "-u", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax update extract");

    // File should NOT be overwritten
    let content = fs::read_to_string(&dst_file).unwrap();
    assert!(
        content.contains("Newer existing"),
        "Newer file should be preserved (got: {})",
        content
    );
}

#[cfg(unix)]
#[test]
fn test_update_mode_copy_newer() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");

    // Create source file (current time = newer)
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("copy.txt")).unwrap();
    writeln!(f, "New source content").unwrap();

    // Create destination with an OLDER file
    fs::create_dir(&dst_dir).unwrap();
    let dst_file = dst_dir.join("copy.txt");
    let mut f = File::create(&dst_file).unwrap();
    writeln!(f, "Old dest content").unwrap();
    drop(f);

    // Set destination file to old time
    let old_time = std::time::SystemTime::UNIX_EPOCH + Duration::from_secs(1000000000);
    filetime::set_file_mtime(&dst_file, filetime::FileTime::from_system_time(old_time)).unwrap();

    // Copy with -u (update mode) - source is newer, should overwrite
    let output = run_pax_in_dir(
        &["-r", "-w", "-u", "copy.txt", dst_dir.to_str().unwrap()],
        &src_dir,
    );
    assert_success(&output, "pax copy update");

    // File should be overwritten
    let content = fs::read_to_string(&dst_file).unwrap();
    assert!(
        content.contains("New source"),
        "File should be updated (got: {})",
        content
    );
}

#[cfg(unix)]
#[test]
fn test_update_mode_copy_older() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");

    // Create source file with OLD mtime
    fs::create_dir(&src_dir).unwrap();
    let src_file = src_dir.join("copy.txt");
    let mut f = File::create(&src_file).unwrap();
    writeln!(f, "Old source content").unwrap();
    drop(f);

    // Set source to old time
    let old_time = std::time::SystemTime::UNIX_EPOCH + Duration::from_secs(1000000000);
    filetime::set_file_mtime(&src_file, filetime::FileTime::from_system_time(old_time)).unwrap();

    // Create destination with NEWER file (current time)
    fs::create_dir(&dst_dir).unwrap();
    let dst_file = dst_dir.join("copy.txt");
    let mut f = File::create(&dst_file).unwrap();
    writeln!(f, "New dest content").unwrap();

    // Copy with -u (update mode) - source is older, should NOT overwrite
    let output = run_pax_in_dir(
        &["-r", "-w", "-u", "copy.txt", dst_dir.to_str().unwrap()],
        &src_dir,
    );
    assert_success(&output, "pax copy update");

    // File should NOT be overwritten
    let content = fs::read_to_string(&dst_file).unwrap();
    assert!(
        content.contains("New dest"),
        "Newer file should be preserved (got: {})",
        content
    );
}

#[test]
fn test_update_mode_new_file() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");
    let dst_dir = temp.path().join("dest");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let mut f = File::create(src_dir.join("newfile.txt")).unwrap();
    writeln!(f, "Brand new file").unwrap();

    // Create archive
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src_dir,
    );

    // Create empty destination directory
    fs::create_dir(&dst_dir).unwrap();

    // Extract with -u (update mode) - file doesn't exist, should extract
    let output = run_pax_in_dir(&["-r", "-u", "-f", archive.to_str().unwrap()], &dst_dir);
    assert_success(&output, "pax update extract");

    // File should be created
    assert!(
        dst_dir.join("newfile.txt").exists(),
        "New file should be extracted"
    );
    let content = fs::read_to_string(dst_dir.join("newfile.txt")).unwrap();
    assert!(
        content.contains("Brand new"),
        "Content should match archive"
    );
}

#[cfg(unix)]
#[test]
fn test_reset_atime_write_mode() {
    use std::os::unix::fs::MetadataExt;

    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("test.tar");

    // Create source file
    fs::create_dir(&src_dir).unwrap();
    let src_file = src_dir.join("atime_test.txt");
    let mut f = File::create(&src_file).unwrap();
    writeln!(f, "Access time test").unwrap();
    drop(f);

    // Set a known access time in the past
    let old_time = std::time::SystemTime::UNIX_EPOCH + Duration::from_secs(1500000000);
    filetime::set_file_atime(&src_file, filetime::FileTime::from_system_time(old_time)).unwrap();

    // Get the atime before archive creation
    let meta_before = fs::metadata(&src_file).unwrap();
    let atime_before = meta_before.atime();

    // Create archive with -t (reset access time)
    let output = run_pax_in_dir(
        &[
            "-w",
            "-t",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            "atime_test.txt",
        ],
        &src_dir,
    );
    assert_success(&output, "pax write with -t");

    // Get the atime after archive creation
    let meta_after = fs::metadata(&src_file).unwrap();
    let atime_after = meta_after.atime();

    // Access time should be restored to the original
    // (or at least not significantly different from before)
    assert_eq!(
        atime_before, atime_after,
        "Access time should be restored after reading with -t"
    );
}

/// POSIX: members are "selected based on the user-specified pattern operands
/// as modified by the -c, -n, and -u options". A member -u turns away is not
/// selected, so under -n it must not use up its pattern: the newer member of
/// the same name later in the archive is the first one selected.
#[test]
fn test_update_rejection_does_not_use_up_first_match() {
    let temp = TempDir::new().unwrap();
    let file = temp.path().join("f");
    fs::write(&file, "disk\n").unwrap();
    let on_disk = std::time::SystemTime::UNIX_EPOCH + Duration::from_secs(2_000_000_000);
    filetime::set_file_mtime(&file, filetime::FileTime::from_system_time(on_disk)).unwrap();

    let mut a = Ustar {
        name: b"f",
        body: b"older\n",
        mtime: 1_000_000_000,
        ..Default::default()
    }
    .member();
    a.extend_from_slice(
        &Ustar {
            name: b"f",
            body: b"newer\n",
            mtime: 3_000_000_000,
            ..Default::default()
        }
        .archive(),
    );

    let output = run_pax_with_stdin_bytes_in_dir(&["-r", "-u", "-n", "f"], &a, temp.path());
    assert_success(&output, "pax -r -u -n");
    assert_eq!(fs::read_to_string(&file).unwrap(), "newer\n");
}

/// -u compares against the file of the member's own name: POSIX applies -s
/// only to members already selected.
#[test]
fn test_update_compares_the_name_before_substitution() {
    let temp = TempDir::new().unwrap();
    // The archived name has a newer file on disk; the renamed one has none.
    let original = temp.path().join("f");
    fs::write(&original, "disk\n").unwrap();
    let on_disk = std::time::SystemTime::UNIX_EPOCH + Duration::from_secs(2_000_000_000);
    filetime::set_file_mtime(&original, filetime::FileTime::from_system_time(on_disk)).unwrap();

    let a = Ustar {
        name: b"f",
        body: b"archived\n",
        mtime: 1_000_000_000,
        ..Default::default()
    }
    .archive();

    let output = run_pax_with_stdin_bytes_in_dir(&["-r", "-u", "-s", ",f,g,"], &a, temp.path());
    assert_success(&output, "pax -r -u -s");
    assert!(!temp.path().join("g").exists(), "-u should have rejected f");
}

/// -u compares modification times to the nanosecond when both sides carry
/// them: a pax archive's `mtime` record, and the file's own.
#[test]
fn test_update_compares_subsecond_times() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir(&src).unwrap();
    let at = |nsec| filetime::FileTime::from_unix_time(1_600_000_000, nsec);
    fs::write(src.join("f"), "archived\n").unwrap();
    filetime::set_file_mtime(src.join("f"), at(700_000_000)).unwrap();
    let output = run_pax_in_dir(&["-w", "-x", "pax", "-f", "../a.pax", "f"], &src);
    assert_success(&output, "pax -w -x pax");

    for (dest_nsec, replaced) in [(200_000_000, true), (900_000_000, false)] {
        let dest = temp.path().join(format!("dest{dest_nsec}"));
        fs::create_dir(&dest).unwrap();
        fs::write(dest.join("f"), "disk\n").unwrap();
        filetime::set_file_mtime(dest.join("f"), at(dest_nsec)).unwrap();
        let output = run_pax_in_dir(&["-r", "-u", "-f", "../a.pax"], &dest);
        assert_success(&output, "pax -r -u");
        let want = if replaced { "archived\n" } else { "disk\n" };
        assert_eq!(
            fs::read_to_string(dest.join("f")).unwrap(),
            want,
            "read, {dest_nsec}"
        );

        // Copy mode compares the same way.
        let copy = temp.path().join(format!("copy{dest_nsec}"));
        fs::create_dir(&copy).unwrap();
        fs::write(copy.join("f"), "disk\n").unwrap();
        filetime::set_file_mtime(copy.join("f"), at(dest_nsec)).unwrap();
        let output = run_pax_in_dir(&["-r", "-w", "-u", "f", copy.to_str().unwrap()], &src);
        assert_success(&output, "pax -rw -u");
        assert_eq!(
            fs::read_to_string(copy.join("f")).unwrap(),
            want,
            "copy, {dest_nsec}"
        );
    }
}
