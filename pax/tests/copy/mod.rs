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
use plib::tmp::TempDir;
use std::fs::{self, File};
use std::io::Write;
use std::os::unix::fs::MetadataExt;

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

// Copy mode must stay inside the destination directory. POSIX defines a copy as
// an archive round-trip, so these are the same guarantees the extraction tests
// make: a name planted in the destination is never written through, whatever it
// points at.

/// A *dangling* symlink in the destination is not "already there": `exists()`
/// follows the link and reports false, so nothing was removed and the create
/// then followed it, writing the source data wherever it pointed.
#[test]
fn test_copy_does_not_write_through_dangling_symlink() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    let dst = temp.path().join("dst");
    let outside = temp.path().join("outside");
    fs::create_dir(&src).unwrap();
    fs::create_dir(&dst).unwrap();

    fs::write(src.join("member"), b"payload\n").unwrap();
    std::os::unix::fs::symlink(&outside, dst.join("member")).unwrap();

    let output = run_pax_in_dir(&["-r", "-w", ".", dst.to_str().unwrap()], &src);

    assert!(
        !outside.exists(),
        "copy mode wrote through a dangling symlink: {}",
        stderr_str(&output)
    );
}

/// A symlink to a directory is not the directory: treating it as "already
/// there" wrote the whole subtree through it.
#[test]
fn test_copy_does_not_descend_symlinked_destination_directory() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    let dst = temp.path().join("dst");
    let outside = temp.path().join("outside");
    fs::create_dir(&src).unwrap();
    fs::create_dir(&dst).unwrap();
    fs::create_dir(&outside).unwrap();

    fs::create_dir(src.join("sub")).unwrap();
    fs::write(src.join("sub").join("member"), b"payload\n").unwrap();
    std::os::unix::fs::symlink(&outside, dst.join("sub")).unwrap();

    let output = run_pax_in_dir(&["-r", "-w", ".", dst.to_str().unwrap()], &src);

    assert!(
        !outside.join("member").exists(),
        "copy mode wrote a subtree through a symlinked destination directory: {}",
        stderr_str(&output)
    );
}

/// Copying a directory into a subdirectory of itself must be refused rather
/// than followed until the pathname runs out of room.
#[test]
fn test_copy_into_own_subdirectory_is_refused() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    let sub = src.join("sub");
    fs::create_dir(&src).unwrap();
    fs::create_dir(&sub).unwrap();
    fs::write(src.join("f"), b"payload\n").unwrap();

    let output = run_pax_in_dir(&["-r", "-w", ".", "sub"], &src);

    // Whatever the diagnostic, it must not have recursed: a handful of levels
    // is a copy, a thousand is the runaway.
    let depth = walkdir_depth(&sub);
    assert!(
        depth <= 3,
        "copy recursed into its own destination to depth {depth}: {}",
        stderr_str(&output)
    );
}

/// Deepest nesting below `root`, counted in directory levels.
fn walkdir_depth(root: &std::path::Path) -> usize {
    fn go(p: &std::path::Path, d: usize) -> usize {
        if d > 64 {
            return d;
        }
        let Ok(entries) = fs::read_dir(p) else {
            return d;
        };
        entries
            .flatten()
            .filter(|e| e.path().is_dir())
            .map(|e| go(&e.path(), d + 1))
            .max()
            .unwrap_or(d)
    }
    go(root, 0)
}

/// A `-s` rename can produce a member name with a leading slash. The file is
/// created under the sanitized name, so a second name for the same inode must be
/// linked to *that*, resolved from the destination anchor -- handing the raw name
/// to `linkat` resolved it from the root of the filesystem instead, and the link
/// was simply never made.
#[test]
fn test_copy_hard_link_follows_the_sanitized_member_name() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    let dst = temp.path().join("dst");
    fs::create_dir(&src).unwrap();
    fs::create_dir(&dst).unwrap();

    fs::write(src.join("aaa"), b"payload\n").unwrap();
    fs::hard_link(src.join("aaa"), src.join("zzz")).unwrap();

    let output = run_pax_in_dir(
        &[
            "-r",
            "-w",
            "-s",
            ",^aaa$,/abs/aaa,",
            ".",
            dst.to_str().unwrap(),
        ],
        &src,
    );
    assert_success(&output, "copying a hard-linked pair through a -s rename");

    let renamed = dst.join("abs").join("aaa");
    let other = dst.join("zzz");
    assert!(other.exists(), "the second name was not copied");
    assert_eq!(
        fs::metadata(&renamed).unwrap().ino(),
        fs::metadata(&other).unwrap().ino(),
        "the second name is not a link to the first copy"
    );
}

/// `pax -rwl tree/f tree/g tree/s .` names each source file as its own
/// destination. linkat() reports EEXIST, and the replace-on-EEXIST path must
/// not then unlink the name -- it is the source. BSD pax says "Unable to link
/// file to itself" for each and leaves the tree alone. `pax -rwl tree .` is
/// refused one level up, at the directory.
#[test]
fn test_copy_link_onto_itself_keeps_source() {
    let temp = TempDir::new().unwrap();
    let tree = temp.path().join("tree");
    fs::create_dir(&tree).unwrap();
    fs::write(tree.join("f"), "DATA\n").unwrap();
    fs::hard_link(tree.join("f"), tree.join("g")).unwrap();
    fs::write(tree.join("s"), "x\n").unwrap();
    let ino = |n: &str| fs::metadata(tree.join(n)).unwrap().ino();
    let before = [ino("f"), ino("g"), ino("s")];

    let files = ["tree/f", "tree/g", "tree/s"];
    for (operands, diagnostics) in [(&files[..], 3), (&["tree"][..], 1)] {
        let mut args = vec!["-rwl"];
        args.extend_from_slice(operands);
        args.push(".");
        let output = run_pax_in_dir(&args, temp.path());
        assert_exit_code(&output, 1, &format!("pax {args:?}"));
        assert_eq!(
            stderr_str(&output).lines().count(),
            diagnostics,
            "{args:?}: {}",
            stderr_str(&output)
        );

        assert_eq!(fs::read_to_string(tree.join("f")).unwrap(), "DATA\n");
        assert_eq!(fs::read_to_string(tree.join("g")).unwrap(), "DATA\n");
        assert_eq!(fs::read_to_string(tree.join("s")).unwrap(), "x\n");
        assert_eq!([ino("f"), ino("g"), ino("s")], before, "{args:?}");
    }
}

/// A multiply-linked file reached twice -- `find tree | pax -rw` visits it as
/// an operand and again while walking `tree` -- must still come out as both
/// of its names, not lose one to a link of the destination onto itself.
#[test]
fn test_copy_hardlink_visited_twice() {
    let temp = TempDir::new().unwrap();
    let tree = temp.path().join("tree");
    let out = temp.path().join("out");
    fs::create_dir(&tree).unwrap();
    fs::create_dir(&out).unwrap();
    fs::write(tree.join("f"), "DATA\n").unwrap();
    fs::hard_link(tree.join("f"), tree.join("g")).unwrap();

    let output = run_pax_in_dir_with_stdin(&["-rw", "out"], temp.path(), "tree\ntree/f\ntree/g\n");
    assert_success(&output, "pax -rw of a list naming a hard link twice");

    assert_eq!(fs::read_to_string(out.join("tree/f")).unwrap(), "DATA\n");
    assert_eq!(fs::read_to_string(out.join("tree/g")).unwrap(), "DATA\n");
}

/// -l with -L: POSIX, "the hard link created in the destination file hierarchy
/// shall be to the file referenced by the symbolic link" -- not to the link.
#[test]
fn test_copy_link_with_dereference_links_the_target() {
    let temp = TempDir::new().unwrap();
    let out = temp.path().join("out");
    fs::create_dir(&out).unwrap();
    fs::write(temp.path().join("t"), "T\n").unwrap();
    std::os::unix::fs::symlink("t", temp.path().join("s")).unwrap();

    for follow in ["-L", "-H"] {
        let output = run_pax_in_dir(&["-rwl", follow, "s", "out"], temp.path());
        assert_success(&output, &format!("pax -rwl {follow}"));
        let copied = fs::symlink_metadata(out.join("s")).unwrap();
        assert!(copied.file_type().is_file(), "{follow}: not a regular file");
        let target = fs::metadata(temp.path().join("t")).unwrap();
        assert_eq!(
            copied.ino(),
            target.ino(),
            "{follow}: not linked to the target"
        );
        fs::remove_file(out.join("s")).unwrap();
    }
}

/// -l with -H/-L where the destination name already *is* the file the source
/// symbolic link refers to. linkat reports EEXIST; the "already the same file"
/// check has to follow the link the way linkat does, or it sees two different
/// inodes, unlinks the destination -- the only copy of the data -- and the
/// retried link then has nothing to link to.
#[test]
fn test_copy_link_follow_onto_the_link_target_keeps_it() {
    for follow in ["-H", "-L"] {
        let temp = TempDir::new().unwrap();
        let d = temp.path().join("D");
        fs::create_dir(&d).unwrap();
        fs::write(d.join("x"), "DATA\n").unwrap();
        std::os::unix::fs::symlink("D/x", temp.path().join("x")).unwrap();

        let output = run_pax_in_dir(&["-rwl", follow, "x", "D"], temp.path());
        assert_eq!(
            fs::read_to_string(d.join("x")).unwrap_or_default(),
            "DATA\n",
            "{follow}: the link target was destroyed: {}",
            stderr_str(&output)
        );
        assert!(
            stderr_str(&output).contains("to itself"),
            "{follow}: linking a file onto itself is diagnosed as elsewhere: {}",
            stderr_str(&output)
        );
    }
}

/// -l across devices cannot link; POSIX says the file is then copied, and
/// that expected fallback is neither diagnosed nor a failure.
#[test]
fn test_copy_link_across_devices_copies_quietly() {
    let temp = TempDir::new().unwrap();
    let mnt = temp.path().join("mnt");
    let out = temp.path().join("out");
    fs::create_dir(&mnt).unwrap();
    fs::create_dir(&out).unwrap();
    let Some(_mount) = ScratchMount::mount(temp.path(), &mnt) else {
        return; // no unprivileged way to get a second device here
    };
    fs::write(mnt.join("f"), "F\n").unwrap();

    let output = run_pax_in_dir(&["-rwl", "f", out.to_str().unwrap()], &mnt);
    assert_success(&output, "pax -rwl across devices");
    assert_eq!(stderr_str(&output), "");
    assert_eq!(fs::read_to_string(out.join("f")).unwrap(), "F\n");
}

/// -X: a directory on another device is itself copied; only what is below
/// it is not.
#[test]
fn test_copy_one_file_system_keeps_the_mount_point() {
    let temp = TempDir::new().unwrap();
    let tree = temp.path().join("tree");
    let out = temp.path().join("out");
    fs::create_dir_all(tree.join("mnt")).unwrap();
    fs::create_dir(&out).unwrap();
    fs::write(tree.join("f"), "F\n").unwrap();
    let Some(_mount) = ScratchMount::mount(temp.path(), &tree.join("mnt")) else {
        return;
    };
    fs::write(tree.join("mnt/inside"), "I\n").unwrap();

    let output = run_pax_in_dir(&["-rwX", "tree", "out"], temp.path());
    assert_success(&output, "pax -rwX");
    assert!(out.join("tree/f").is_file());
    assert!(out.join("tree/mnt").is_dir(), "mount point not copied");
    assert!(!out.join("tree/mnt/inside").exists(), "descended below it");
}

/// Copy mode's -k leaves an existing destination directory as it is, the way
/// it leaves an existing file: its contents are still copied, but its mode is
/// not replaced by the source's.
#[test]
fn test_copy_keep_existing_directory_attributes() {
    use std::os::unix::fs::PermissionsExt;
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir_all(src.join("d")).unwrap();
    fs::write(src.join("d/f"), "F\n").unwrap();
    fs::set_permissions(src.join("d"), fs::Permissions::from_mode(0o755)).unwrap();
    let dst = temp.path().join("dst");
    fs::create_dir_all(dst.join("d")).unwrap();
    fs::set_permissions(dst.join("d"), fs::Permissions::from_mode(0o700)).unwrap();

    let out = run_pax_in_dir(&["-rw", "-k", "-pp", "d", dst.to_str().unwrap()], &src);
    assert_success(&out, "pax -rw -k");
    assert_eq!(fs::read_to_string(dst.join("d/f")).unwrap(), "F\n");
    let mode = fs::metadata(dst.join("d")).unwrap().permissions().mode() & 0o7777;
    assert_eq!(mode, 0o700, "-k replaced an existing directory's mode");
}

/// -u likewise: a destination directory newer than its source keeps its own
/// attributes, while its contents are still each considered.
#[test]
fn test_copy_update_keeps_newer_directory_attributes() {
    use std::os::unix::fs::PermissionsExt;
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir_all(src.join("d")).unwrap();
    fs::write(src.join("d/f"), "F\n").unwrap();
    fs::set_permissions(src.join("d"), fs::Permissions::from_mode(0o755)).unwrap();
    // Make the source directory old, so the destination one is newer.
    let old = std::time::SystemTime::UNIX_EPOCH + std::time::Duration::from_secs(1_000_000_000);
    File::open(src.join("d"))
        .unwrap()
        .set_modified(old)
        .unwrap();
    let dst = temp.path().join("dst");
    fs::create_dir_all(dst.join("d")).unwrap();
    fs::set_permissions(dst.join("d"), fs::Permissions::from_mode(0o700)).unwrap();

    let out = run_pax_in_dir(&["-rw", "-u", "-pp", "d", dst.to_str().unwrap()], &src);
    assert_success(&out, "pax -rw -u");
    assert_eq!(fs::read_to_string(dst.join("d/f")).unwrap(), "F\n");
    let mode = fs::metadata(dst.join("d")).unwrap().permissions().mode() & 0o7777;
    assert_eq!(mode, 0o700, "-u replaced a newer directory's mode");
}

/// A source directory that only its owner may enter must not be copied to one
/// anyone may enter, not even while the copy runs: until the run ends its
/// copy, and the 0644 files in it, were open to everyone -- for good, if the
/// run never finished.
#[test]
fn test_copy_private_directory_is_not_exposed_during_the_copy() {
    use std::os::unix::fs::PermissionsExt;
    use std::process::{Command, Stdio};
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    let dst = temp.path().join("dst");
    fs::create_dir_all(src.join("home/alice")).unwrap();
    fs::create_dir(&dst).unwrap();
    fs::write(src.join("home/alice/f"), "secret\n").unwrap();
    fs::set_permissions(src.join("home/alice"), fs::Permissions::from_mode(0o700)).unwrap();

    let mut child = Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(["-rw", dst.to_str().unwrap()])
        .current_dir(&src)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .unwrap();
    let mut stdin = child.stdin.take().unwrap();
    stdin.write_all(b"home/alice\n").unwrap();
    stdin.flush().unwrap();

    // The name list is still open: pax has copied the subtree but cannot
    // have finished the run.
    let copied = dst.join("home/alice/f");
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
    while !copied.exists() && std::time::Instant::now() < deadline {
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
    let mode = fs::metadata(dst.join("home/alice"))
        .map(|m| m.permissions().mode() & 0o777)
        .ok();
    drop(stdin);
    child.wait().unwrap();

    assert!(copied.exists(), "the subtree was never copied");
    assert_eq!(
        mode.map(|m| m & 0o077),
        Some(0),
        "a 0700 directory's copy was open to others during the run: {mode:?}"
    );
    let mode = fs::metadata(dst.join("home/alice"))
        .unwrap()
        .permissions()
        .mode()
        & 0o777;
    assert_eq!(mode, 0o700);
}

/// A `find -depth` list names a directory after its contents. A directory
/// already at the destination has been written to by then -- by this run --
/// so -u must compare the source with what the destination was *before* the
/// run, or the source directory's mode and time are never applied. cpio -p
/// implies -u, which made `find . -depth | cpio -pdm` leave them behind.
#[test]
fn test_copy_update_depth_first_uses_pre_run_directory_time() {
    use std::os::unix::fs::PermissionsExt;
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir_all(src.join("a")).unwrap();
    fs::write(src.join("a/f"), "F\n").unwrap();
    fs::set_permissions(src.join("a"), fs::Permissions::from_mode(0o700)).unwrap();
    let src_time = filetime::FileTime::from_unix_time(1_577_836_800, 0); // 2020
    filetime::set_file_mtime(src.join("a"), src_time).unwrap();

    let list = b"./a/f\n./a\n.\n";
    let runs: [(&str, &[&str]); 2] = [
        ("cpio", &["-pdm", "../dst"]),
        ("pax", &["-rw", "-d", "-u", "-pe", "../dst"]),
    ];
    for (tool, args) in runs {
        let dst = temp.path().join("dst");
        let _ = fs::remove_dir_all(&dst);
        fs::create_dir_all(dst.join("a")).unwrap();
        fs::set_permissions(dst.join("a"), fs::Permissions::from_mode(0o755)).unwrap();
        // An existing directory takes the source's attributes only where
        // nobody else can create entries beside it: not under a umask of 002.
        fs::set_permissions(&dst, fs::Permissions::from_mode(0o755)).unwrap();
        let dst_time = filetime::FileTime::from_unix_time(1_546_300_800, 0); // 2019
        filetime::set_file_mtime(dst.join("a"), dst_time).unwrap();

        let out = if tool == "cpio" {
            run_cpio(args, &src, list)
        } else {
            run_program(
                std::path::Path::new(env!("CARGO_BIN_EXE_pax")),
                args,
                &src,
                Some(list),
            )
        };
        assert_success(&out, tool);
        assert_eq!(fs::read_to_string(dst.join("a/f")).unwrap(), "F\n");
        let meta = fs::metadata(dst.join("a")).unwrap();
        assert_eq!(meta.permissions().mode() & 0o777, 0o700, "{tool}: mode");
        assert_eq!(meta.mtime(), 1_577_836_800, "{tool}: mtime");
    }
}

/// -s renaming a directory to `.` puts its contents straight into the
/// destination, as extracting the same members from an archive does. The
/// whole subtree used to be dropped, silently.
#[test]
fn test_copy_subst_directory_to_dot_copies_its_contents() {
    let temp = TempDir::new().unwrap();
    fs::create_dir_all(temp.path().join("src/sub")).unwrap();
    fs::write(temp.path().join("src/sub/f"), "F\n").unwrap();
    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();

    let out = run_pax_in_dir(&["-rw", "-s", ",^src,.,", "src", "dst"], temp.path());
    assert_success(&out, "pax -rw -s to .");
    assert_eq!(fs::read_to_string(dst.join("sub/f")).unwrap(), "F\n");
}

/// `pax -rw tree .` names every file as its own destination. Copying one
/// onto itself must neither rewrite it nor split its hard links: BSD pax says
/// "file would overwrite itself" and leaves it alone.
#[test]
fn test_copy_onto_itself_leaves_the_source_alone() {
    let temp = TempDir::new().unwrap();
    let tree = temp.path().join("tree");
    fs::create_dir(&tree).unwrap();
    fs::write(tree.join("f"), "DATA\n").unwrap();
    fs::hard_link(tree.join("f"), tree.join("g")).unwrap();
    let ino = fs::metadata(tree.join("f")).unwrap().ino();

    for operands in [&["tree", "."][..], &["tree/f", "."][..]] {
        let mut args = vec!["-rw"];
        args.extend_from_slice(operands);
        let out = run_pax_in_dir(&args, temp.path());
        assert!(
            stderr_str(&out).contains("itself"),
            "{operands:?}: copying a file onto itself is not diagnosed: {}",
            stderr_str(&out)
        );
        for name in ["f", "g"] {
            let meta = fs::metadata(tree.join(name)).unwrap();
            assert_eq!(meta.ino(), ino, "{operands:?}: {name} was replaced");
            assert_eq!(meta.nlink(), 2, "{operands:?}: {name}'s link was split");
        }
        assert_eq!(fs::read_to_string(tree.join("f")).unwrap(), "DATA\n");
    }
}

/// A directory replaces a non-directory already at its destination name, as
/// it does when extracted from an archive.
#[test]
fn test_copy_directory_replaces_existing_file() {
    let temp = TempDir::new().unwrap();
    fs::create_dir_all(temp.path().join("src/d")).unwrap();
    fs::write(temp.path().join("src/d/f"), "F\n").unwrap();
    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();
    fs::write(dst.join("d"), "old\n").unwrap();

    let out = run_pax_in_dir(&["-rw", "d", "../dst"], &temp.path().join("src"));
    assert_success(&out, "pax -rw over a file");
    assert_eq!(fs::read_to_string(dst.join("d/f")).unwrap(), "F\n");
}

/// A directory that maps onto itself is not copied, but -s can still send
/// its contents elsewhere, each under its own name, as a round trip through
/// an archive does. The whole subtree used to be skipped.
#[test]
fn test_copy_directory_onto_itself_still_copies_renamed_children() {
    let temp = TempDir::new().unwrap();
    fs::create_dir_all(temp.path().join("src/sub")).unwrap();
    fs::write(temp.path().join("src/x"), "X\n").unwrap();
    fs::write(temp.path().join("src/sub/y"), "Y\n").unwrap();

    let out = run_pax_in_dir(&["-rw", "-s", ",^src/,dst/,", "src", "."], temp.path());
    assert_success(&out, "pax -rw -s with the directory mapped onto itself");
    assert_eq!(
        fs::read_to_string(temp.path().join("dst/x")).unwrap(),
        "X\n"
    );
    assert_eq!(
        fs::read_to_string(temp.path().join("dst/sub/y")).unwrap(),
        "Y\n"
    );
}

/// Without -s or -i everything below a directory that maps onto itself maps
/// onto itself too: one diagnostic says so, not one per file.
#[test]
fn test_copy_directory_onto_itself_is_diagnosed_once() {
    let temp = TempDir::new().unwrap();
    fs::create_dir_all(temp.path().join("src/sub")).unwrap();
    fs::write(temp.path().join("src/x"), "X\n").unwrap();
    fs::write(temp.path().join("src/sub/y"), "Y\n").unwrap();

    let out = run_pax_in_dir(&["-rw", "src", "."], temp.path());
    assert_failure(&out, "pax -rw src .");
    let err = stderr_str(&out);
    assert_eq!(err.matches("itself").count(), 1, "{err}");
    assert_eq!(
        fs::read_to_string(temp.path().join("src/x")).unwrap(),
        "X\n"
    );
}

/// Under -H or -L the walk follows a symbolic link operand, but the link
/// itself is still the source: copying it into its own directory names it as
/// its own destination. It used to be unlinked and replaced -- by an empty
/// directory, or under -l by nothing at all.
#[test]
fn test_copy_followed_link_onto_itself_keeps_the_link() {
    let temp = TempDir::new().unwrap();
    let sub = temp.path().join("sub");
    fs::create_dir(&sub).unwrap();
    fs::write(sub.join("file"), "payload\n").unwrap();
    fs::write(temp.path().join("f"), "F\n").unwrap();
    std::os::unix::fs::symlink("sub", temp.path().join("ld")).unwrap();
    std::os::unix::fs::symlink("f", temp.path().join("lf")).unwrap();

    let runs: [(&str, &[&str]); 8] = [
        ("pax", &["-rw", "-H", "ld", "."]),
        ("pax", &["-rw", "-L", "ld", "."]),
        ("pax", &["-rw", "-H", "lf", "."]),
        ("pax", &["-rw", "-L", "lf", "."]),
        ("pax", &["-rw", "-l", "-H", "lf", "."]),
        ("pax", &["-rw", "-l", "-L", "lf", "."]),
        ("cpio", &["-pL", "."]),
        ("cpio", &["-plL", "."]),
    ];
    for (tool, args) in runs {
        let out = if tool == "cpio" {
            run_cpio(args, temp.path(), b"ld\nlf\n")
        } else {
            run_pax_in_dir(args, temp.path())
        };
        let ctx = format!("{tool} {args:?}");
        assert!(
            stderr_str(&out).contains("itself"),
            "{ctx}: {}",
            stderr_str(&out)
        );
        for (link, target) in [("ld", "sub"), ("lf", "f")] {
            let meta = fs::symlink_metadata(temp.path().join(link)).unwrap();
            assert!(meta.is_symlink(), "{ctx}: {link} was replaced");
            assert_eq!(
                fs::read_link(temp.path().join(link)).unwrap(),
                std::path::Path::new(target),
                "{ctx}"
            );
        }
        assert_eq!(fs::read_to_string(sub.join("file")).unwrap(), "payload\n");
        assert_eq!(fs::read_to_string(temp.path().join("f")).unwrap(), "F\n");
    }
}
