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

    // Copy single file. The operand is relative to src_dir, so the member name
    // -- and the destination beneath dst_dir -- is just "test.txt".
    let output = run_pax_in_dir(
        &["-r", "-w", "test.txt", dst_dir.to_str().unwrap()],
        &src_dir,
    );
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

    // Copy directory, naming it relative to src_dir.
    let output = run_pax_in_dir(&["-r", "-w", "mydir", dst_dir.to_str().unwrap()], &src_dir);
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
    let output = run_pax_in_dir(
        &["-r", "-w", "-l", "link_test.txt", dst_dir.to_str().unwrap()],
        &src_dir,
    );
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
    let output = run_pax_in_dir(
        &["-r", "-w", "symlink.txt", dst_dir.to_str().unwrap()],
        &src_dir,
    );
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
    let output = run_pax_in_dir(
        &[
            "-r",
            "-w",
            "file1.txt",
            "file2.txt",
            "file3.txt",
            dst_dir.to_str().unwrap(),
        ],
        &src_dir,
    );
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

/// `-c` inverts pattern matching, but in write and copy mode the operands are
/// the files to archive, not patterns. Copy mode used to feed an empty pattern
/// list to an inverted match, so every file failed selection and `pax -r -w -c
/// src dest` copied nothing while exiting 0 -- silent data loss for a script
/// that removes the source afterwards. Write mode ignored -c outright.
#[test]
fn test_copy_mode_rejects_dash_c() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");
    fs::create_dir(&src_dir).unwrap();
    fs::create_dir(&dst_dir).unwrap();
    fs::write(src_dir.join("keep.txt"), b"payload").unwrap();

    let output = run_pax_in_dir(
        &["-r", "-w", "-c", "source", dst_dir.to_str().unwrap()],
        temp.path(),
    );

    assert_failure(&output, "copy mode with -c");
    assert!(
        stderr_str(&output).contains("-c"),
        "the diagnostic should name the option: {}",
        stderr_str(&output)
    );
    // Above all: it must not silently report success having copied nothing.
    assert!(
        !dst_dir.join("source").exists(),
        "nothing should have been copied"
    );
}

#[test]
fn test_write_mode_rejects_dash_c() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let archive = temp.path().join("out.tar");
    fs::create_dir(&src_dir).unwrap();
    fs::write(src_dir.join("f.txt"), b"x").unwrap();

    let output = run_pax_in_dir(
        &["-w", "-c", "-f", archive.to_str().unwrap(), "f.txt"],
        &src_dir,
    );
    assert_failure(&output, "write mode with -c");
    assert!(
        stderr_str(&output).contains("-c"),
        "the diagnostic should name the option: {}",
        stderr_str(&output)
    );
}

/// Set a file's atime and mtime to fixed values, nanoseconds included.
#[cfg(unix)]
fn set_times_ns(path: &std::path::Path, atime: (i64, i64), mtime: (i64, i64)) {
    use std::os::unix::ffi::OsStrExt;
    let times = [
        libc::timespec {
            tv_sec: atime.0 as libc::time_t,
            tv_nsec: atime.1 as _,
        },
        libc::timespec {
            tv_sec: mtime.0 as libc::time_t,
            tv_nsec: mtime.1 as _,
        },
    ];
    let c = std::ffi::CString::new(path.as_os_str().as_bytes()).unwrap();
    let rc = unsafe { libc::utimensat(libc::AT_FDCWD, c.as_ptr(), times.as_ptr(), 0) };
    assert_eq!(rc, 0, "utimensat: {}", std::io::Error::last_os_error());
}

/// POSIX: a copy behaves "as if the copied files were written to a pax format
/// archive file and then subsequently extracted". Extraction restores atime and
/// mtime separately and at nanosecond resolution; copy mode instead called
/// utimes with the source *mtime* in both slots and tv_usec hardcoded to 0, so
/// it clobbered the destination access time and dropped all sub-second
/// precision.
#[cfg(unix)]
#[test]
fn test_copy_mode_preserves_atime_and_subsecond_mtime() {
    use std::os::unix::fs::MetadataExt;

    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");
    fs::create_dir(&src_dir).unwrap();
    fs::create_dir(&dst_dir).unwrap();

    let src_file = src_dir.join("timed.txt");
    fs::write(&src_file, b"content").unwrap();
    // Distinct instants so an mtime-in-both-slots bug is visible.
    set_times_ns(&src_file, (1_058_356_800, 0), (1_055_678_400, 123_456_789));

    let want = fs::metadata(&src_file).unwrap();
    let (want_atime, want_mtime, want_nsec) = (want.atime(), want.mtime(), want.mtime_nsec());

    let output = run_pax_in_dir(
        &["-r", "-w", "-p", "e", "source", dst_dir.to_str().unwrap()],
        temp.path(),
    );
    assert_success(&output, "copy preserving times");

    let got = fs::metadata(dst_dir.join("source").join("timed.txt"))
        .unwrap_or_else(|e| panic!("copied file missing: {e}"));
    assert_eq!(got.mtime(), want_mtime, "mtime not preserved");
    assert_eq!(
        got.atime(),
        want_atime,
        "atime must be the source's access time, not its mtime"
    );
    if want_nsec != 0 {
        assert_eq!(
            got.mtime_nsec(),
            want_nsec,
            "sub-second mtime must survive the copy"
        );
    }
}

/// mkfifo and mknod apply the process umask, so a special file needs an
/// explicit chmod afterwards -- which extraction does and copy mode did not,
/// along with never restoring times or ownership for these types.
#[cfg(unix)]
#[test]
fn test_copy_mode_restores_fifo_mode() {
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::PermissionsExt;

    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");
    fs::create_dir(&src_dir).unwrap();
    fs::create_dir(&dst_dir).unwrap();

    let fifo = src_dir.join("pipe");
    let c = std::ffi::CString::new(fifo.as_os_str().as_bytes()).unwrap();
    if unsafe { libc::mkfifo(c.as_ptr(), 0o600) } != 0 {
        eprintln!("Skipping FIFO test: mkfifo failed");
        return;
    }
    // A mode with bits the default umask would strip.
    fs::set_permissions(&fifo, fs::Permissions::from_mode(0o666)).unwrap();

    let output = run_pax_in_dir(
        &["-r", "-w", "-p", "e", "source", dst_dir.to_str().unwrap()],
        temp.path(),
    );
    assert_success(&output, "copy a FIFO");

    let got = fs::symlink_metadata(dst_dir.join("source").join("pipe"))
        .unwrap_or_else(|e| panic!("copied FIFO missing: {e}"));
    assert_eq!(
        got.permissions().mode() & 0o777,
        0o666,
        "a copied FIFO must keep its mode, not the umask's"
    );
}

/// A directory whose archived mode denies write or search permission must still
/// receive its contents: the mode belongs on the directory only once the
/// subtree below it exists. Applying it at creation time made every child fail
/// with EACCES, and stamped a mtime that populating the directory then
/// invalidated.
#[cfg(unix)]
#[test]
fn test_copy_mode_readonly_directory_gets_its_contents() {
    use std::os::unix::fs::PermissionsExt;

    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");
    fs::create_dir(&src_dir).unwrap();
    fs::create_dir(&dst_dir).unwrap();

    let ro = src_dir.join("ro");
    fs::create_dir(&ro).unwrap();
    fs::write(ro.join("inside.txt"), b"must survive").unwrap();
    fs::set_permissions(&ro, fs::Permissions::from_mode(0o555)).unwrap();

    let output = run_pax_in_dir(
        &["-r", "-w", "-p", "e", "source", dst_dir.to_str().unwrap()],
        temp.path(),
    );

    let copied_dir = dst_dir.join("source").join("ro");
    assert_eq!(
        fs::read_to_string(copied_dir.join("inside.txt")).unwrap_or_default(),
        "must survive",
        "a read-only directory must still receive its contents: {}",
        stderr_str(&output)
    );
    assert_eq!(
        fs::metadata(&copied_dir).unwrap().permissions().mode() & 0o777,
        0o555,
        "and must end up with its archived mode"
    );

    // Leave the tree removable for TempDir's cleanup.
    fs::set_permissions(&copied_dir, fs::Permissions::from_mode(0o755)).unwrap();
    fs::set_permissions(&ro, fs::Permissions::from_mode(0o755)).unwrap();
}

/// `-s` renamed only the paths typed on the command line: the walk used a
/// second function for everything below an operand, and that one had never
/// gained the substitution step. A rename applied to a directory operand
/// therefore left every file inside it untouched.
#[test]
fn test_copy_mode_substitution_applies_below_operand() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");
    fs::create_dir_all(src_dir.join("tree/sub")).unwrap();
    fs::create_dir(&dst_dir).unwrap();
    fs::write(src_dir.join("tree/sub/keep.txt"), b"a").unwrap();
    fs::write(src_dir.join("tree/sub/drop.o"), b"b").unwrap();

    let output = run_pax_in_dir(
        &[
            "-r",
            "-w",
            "-s",
            ",\\.txt$,.renamed,",
            "tree",
            dst_dir.to_str().unwrap(),
        ],
        &src_dir,
    );
    assert_success(&output, "copy with -s");

    assert!(
        dst_dir.join("tree/sub/keep.renamed").exists(),
        "-s must rename a file found by recursion, not just an operand"
    );
    assert!(
        !dst_dir.join("tree/sub/keep.txt").exists(),
        "the original name must not also be present"
    );
}

/// A substitution to the empty string means "skip this file", and that too has
/// to reach the whole subtree.
#[test]
fn test_copy_mode_substitution_skips_below_operand() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");
    fs::create_dir_all(src_dir.join("tree")).unwrap();
    fs::create_dir(&dst_dir).unwrap();
    fs::write(src_dir.join("tree/keep.txt"), b"a").unwrap();
    fs::write(src_dir.join("tree/drop.o"), b"b").unwrap();

    let output = run_pax_in_dir(
        &[
            "-r",
            "-w",
            "-s",
            ",^.*\\.o$,,",
            "tree",
            dst_dir.to_str().unwrap(),
        ],
        &src_dir,
    );
    assert_success(&output, "copy with a deleting -s");

    assert!(dst_dir.join("tree/keep.txt").exists(), "keep.txt must copy");
    assert!(
        !dst_dir.join("tree/drop.o").exists(),
        "a member whose name substitutes to empty must be skipped"
    );
}

/// POSIX defines a copy as "as if the copied files were written to a pax format
/// archive file and then subsequently extracted". Writing `a/b/c` records the
/// member `a/b/c` and extracting it recreates that path, so the copy must too --
/// it used to name the destination after the basename alone, producing
/// `dest/c`, which no archive round trip could yield.
#[test]
fn test_copy_mode_destination_uses_member_path() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.path().join("source");
    let dst_dir = temp.path().join("dest");
    fs::create_dir_all(src_dir.join("a/b")).unwrap();
    fs::create_dir(&dst_dir).unwrap();
    fs::write(src_dir.join("a/b/c.txt"), b"nested").unwrap();

    let output = run_pax_in_dir(
        &["-r", "-w", "a/b/c.txt", dst_dir.to_str().unwrap()],
        &src_dir,
    );
    assert_success(&output, "copy a multi-component operand");

    assert_eq!(
        fs::read_to_string(dst_dir.join("a/b/c.txt")).unwrap_or_default(),
        "nested",
        "a multi-component operand keeps its path under the destination"
    );
    assert!(
        !dst_dir.join("c.txt").exists(),
        "and must not be flattened to its basename"
    );
}
