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
use plib::tmp::TempDir;
use std::fs::{self, File};
use std::io::Write;

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

/// A symbolic link in a multi-volume archive carries no data: its header must
/// say so and nothing may follow it. The writer stored the target's length as
/// the size and the target itself as a data block, which no reader -- ours
/// included -- reads back as the next member's header.
#[test]
fn test_multi_volume_symlink_round_trip() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir(&src).unwrap();
    fs::write(src.join("f"), "hi\n").unwrap();
    std::os::unix::fs::symlink("f", src.join("l")).unwrap();
    fs::write(src.join("g"), "after\n").unwrap();
    let archive = temp.path().join("vol.tar");
    let archive = archive.to_str().unwrap();

    let out = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1000000",
            "-f",
            archive,
            "f",
            "l",
            "g",
        ],
        &src,
    );
    assert_success(&out, "pax -w -M");

    for reader in [&["-r", "-M", "-f", archive][..], &["-r", "-f", archive][..]] {
        let dst = TempDir::new().unwrap();
        let out = run_pax_in_dir(reader, dst.path());
        assert_success(&out, &format!("pax {:?}", reader));
        assert_eq!(
            fs::read_link(dst.path().join("l")).unwrap(),
            std::path::Path::new("f")
        );
        assert_eq!(fs::read_to_string(dst.path().join("g")).unwrap(), "after\n");
    }
}

// ============================================================================
// Volume sets: how one volume leads to the next
// ============================================================================

/// Write `count` files of `size` bytes into `dir`, named `f0000`..., and return
/// their names one per line, in order.
fn many_files(dir: &std::path::Path, count: usize, size: usize) -> String {
    let mut names = String::new();
    for i in 0..count {
        let name = format!("f{i:04}");
        fs::write(dir.join(&name), vec![b'a' + (i % 26) as u8; size]).unwrap();
        names.push_str(&name);
        names.push('\n');
    }
    names
}

/// `pax -w -M` of `files` in `dir` into `vol.tar` with volumes of `length`
/// bytes, every volume change accepted by `true`.
fn write_volume_set(dir: &std::path::Path, length: &str, files: &[&str]) {
    let mut args = vec![
        "-w",
        "-M",
        "--tape-length",
        length,
        "--new-volume-script",
        "true",
        "-f",
        "vol.tar",
    ];
    args.extend_from_slice(files);
    let out = run_pax_in_dir(&args, dir);
    assert_success(&out, "pax -w -M");
}

/// `pax -M` listing `vol.tar` in `dir`, with `script` run for a volume that
/// is not there.
fn list_volume_set(dir: &std::path::Path, script: &str) -> std::process::Output {
    run_pax_in_dir(&["-M", "--new-volume-script", script, "-f", "vol.tar"], dir)
}

/// The names in a listing, in order.
fn listed(out: &std::process::Output) -> Vec<String> {
    stdout_str(out).lines().map(str::to_string).collect()
}

/// The volume script is a child of pax. When the names to archive are pax's
/// standard input, a script that reads its own standard input -- `read
/// answer` -- took them from under pax, and those files were silently never
/// archived. More than one buffer's worth of names, so some are still unread
/// when the volume changes.
#[test]
fn test_multi_volume_script_does_not_consume_the_name_list() {
    let temp = TempDir::new().unwrap();
    let names = many_files(temp.path(), 2000, 1);
    assert!(names.len() > 8192);

    let out = run_pax_in_dir_with_stdin(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1048576",
            "--new-volume-script",
            "cat > /dev/null",
            "-f",
            "vol.tar",
        ],
        temp.path(),
        &names,
    );
    assert_success(&out, "pax -w -M from a name list");
    assert!(temp.path().join("vol.tar.2").exists());

    let out = list_volume_set(temp.path(), "false");
    assert_success(&out, "pax -M list");
    assert_eq!(stdout_str(&out), names);
}

/// A volume that is not the last has no end-of-archive indicator, so the
/// archive is not over when it ends. A next volume that is not there was
/// taken for the end of the archive: everything on it and after it was
/// silently missing, exit status 0. The script is run for it, and if it is
/// still not there that is an error.
#[test]
fn test_multi_volume_missing_volume_is_not_the_end() {
    let temp = TempDir::new().unwrap();
    let names = many_files(temp.path(), 15, 3000);
    let files: Vec<&str> = names.lines().collect();
    write_volume_set(temp.path(), "20480", &files);
    assert!(temp.path().join("vol.tar.3").exists());
    assert!(!temp.path().join("vol.tar.4").exists());
    fs::rename(temp.path().join("vol.tar.3"), temp.path().join("saved")).unwrap();

    let out = list_volume_set(temp.path(), "true");
    assert_failure(&out, "a missing volume");
    assert!(
        stderr_str(&out).contains("vol.tar.3"),
        "the diagnostic names the volume: {}",
        stderr_str(&out)
    );

    // A script that mounts the volume -- here, puts it in place -- lets the
    // read carry on.
    let out = list_volume_set(temp.path(), "cp saved \"$TAR_ARCHIVE\"");
    assert_success(&out, "pax -M with the volume supplied");
    assert_eq!(listed(&out), files);
}

/// The end-of-archive indicator on the last volume ends the archive. A
/// volume from an earlier, longer run under the same name used to be read on
/// as the next one.
#[test]
fn test_multi_volume_stale_volume_is_not_read() {
    let temp = TempDir::new().unwrap();
    let names = many_files(temp.path(), 15, 3000);
    let files: Vec<&str> = names.lines().collect();
    write_volume_set(temp.path(), "20480", &files);
    assert!(temp.path().join("vol.tar.3").exists());

    write_volume_set(temp.path(), "20480", &["f0001"]);
    let out = list_volume_set(temp.path(), "false");
    assert_success(&out, "pax -M list");
    assert_eq!(listed(&out), ["f0001"]);
    // Not silently: it may be the rest of a set whose every volume ends
    // with the indicator, as pax used to write them.
    let stderr = stderr_str(&out);
    assert!(
        stderr.contains("vol.tar.2") && stderr.contains("not read"),
        "{stderr}"
    );
    assert!(!stderr.contains("vol.tar.3"), "{stderr}");

    // A set that ends where it should says nothing.
    fs::remove_file(temp.path().join("vol.tar.2")).unwrap();
    let out = list_volume_set(temp.path(), "false");
    assert_success(&out, "pax -M list");
    assert_eq!(stderr_str(&out), "");
}

/// A tar header for a GNU multi-volume set: `typeflag` with the "ustar  "
/// magic GNU tar writes, and `offset` in the old GNU `offset` field (369).
fn gnu_header(name: &[u8], typeflag: u8, size: u64, offset: Option<u64>) -> Vec<u8> {
    let mut h = Ustar {
        name,
        typeflag,
        size: Some(size),
        ..Default::default()
    }
    .header();
    h[257..265].copy_from_slice(b"ustar  \0");
    if let Some(offset) = offset {
        h[369..381].copy_from_slice(format!("{offset:011o}\0").as_bytes());
    }
    reseal_header(&mut h);
    h.to_vec()
}

fn member(name: &[u8], body: &[u8]) -> Vec<u8> {
    Ustar {
        name,
        body,
        ..Default::default()
    }
    .member()
}

/// A volume that ends between members without an end-of-archive indicator
/// is followed by another, as GNU tar writes them. Its end was taken for the
/// end of the whole archive.
#[test]
fn test_multi_volume_volume_without_trailer_continues() {
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("vol.tar"), member(b"x1", b"one\n")).unwrap();
    let mut second = member(b"x2", b"two\n");
    second.extend_from_slice(&ustar_trailer());
    fs::write(temp.path().join("vol.tar.2"), second).unwrap();

    let out = list_volume_set(temp.path(), "false");
    assert_success(&out, "pax -M list");
    assert_eq!(listed(&out), ["x1", "x2"]);
}

/// GNU tar splits a member between volumes: the first holds its header and
/// the start of its data, the next starts -- after a volume label, when one
/// was asked for -- with an 'M' header and the rest. Neither header is a
/// member; the data is one member's.
#[test]
fn test_multi_volume_reads_gnu_split_member() {
    let temp = TempDir::new().unwrap();
    let big: Vec<u8> = (0..2048u32).map(|i| (i * 7) as u8).collect();

    let mut first = gnu_header(b"MyLabel", b'V', 0, None);
    first.extend(member(b"a", b"AAAA\n"));
    first.extend(gnu_header(b"big", b'0', 2048, None));
    first.extend_from_slice(&big[..1024]);
    fs::write(temp.path().join("vol.tar"), first).unwrap();

    let mut second = gnu_header(b"MyLabel Volume 2", b'V', 0, None);
    second.extend(gnu_header(b"big", b'M', 1024, Some(1024)));
    second.extend_from_slice(&big[1024..]);
    second.extend(member(b"c", b"CCCC\n"));
    second.extend_from_slice(&ustar_trailer());
    fs::write(temp.path().join("vol.tar.2"), second).unwrap();

    let out = list_volume_set(temp.path(), "false");
    assert_success(&out, "pax -M list");
    assert_eq!(listed(&out), ["a", "big", "c"]);

    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();
    let out = run_pax_in_dir(
        &[
            "-r",
            "-M",
            "--new-volume-script",
            "false",
            "-f",
            "../vol.tar",
        ],
        &dst,
    );
    assert_success(&out, "pax -r -M");
    assert_eq!(fs::read(dst.join("big")).unwrap(), big);
    assert_eq!(fs::read(dst.join("c")).unwrap(), b"CCCC\n");
}

/// A continuation header on the first volume continues nothing: the volumes
/// were given out of order.
#[test]
fn test_multi_volume_first_volume_continuation_is_refused() {
    let temp = TempDir::new().unwrap();
    let mut vol = gnu_header(b"big", b'M', 512, Some(1024));
    vol.extend_from_slice(&[7u8; 512]);
    vol.extend_from_slice(&ustar_trailer());
    fs::write(temp.path().join("vol.tar"), vol).unwrap();

    let out = list_volume_set(temp.path(), "false");
    assert_failure(&out, "a set starting with a continuation");
    assert!(stdout_str(&out).is_empty());
}

/// The multi-volume reader was a header loop of its own, without what the
/// reader of a single archive knows: a GNU long-name record is not a member,
/// and a lone zero block is not the end of the archive. A one-volume set reads
/// exactly as the archive it is.
#[test]
fn test_multi_volume_reads_like_a_single_archive() {
    let temp = TempDir::new().unwrap();
    let long = [b"d/".as_slice(), &[b'n'; 120]].concat();
    let mut record = long.clone();
    record.push(0);
    let mut with_long_name = gnu_header(b"././@LongLink", b'L', record.len() as u64, None);
    pad_to_block(&mut record);
    with_long_name.extend(record);
    with_long_name.extend(member(&long[..100], b"LONG\n"));
    with_long_name.extend(member(b"after", b"after\n"));
    with_long_name.extend_from_slice(&ustar_trailer());

    let mut lone_zero = member(b"l1", b"one\n");
    lone_zero.extend_from_slice(&[0u8; 512]);
    lone_zero.extend(member(b"l2", b"two\n"));
    lone_zero.extend_from_slice(&ustar_trailer());

    for archive in [with_long_name, lone_zero] {
        fs::write(temp.path().join("vol.tar"), archive).unwrap();
        let single = run_pax_in_dir(&["-f", "vol.tar"], temp.path());
        let multi = list_volume_set(temp.path(), "false");
        assert!(!stdout_str(&multi).contains("@LongLink"));
        assert_eq!(stdout_str(&multi), stdout_str(&single));
        assert_eq!(stderr_str(&multi), stderr_str(&single));
        assert_eq!(multi.status.code(), single.status.code());
    }
}

/// A member whose header is refused is refused before the volume is changed
/// for it: the change made a new volume that nothing was ever written to.
#[test]
fn test_multi_volume_refused_member_does_not_change_volume() {
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("f1"), vec![b'1'; 9000]).unwrap();
    fs::write(temp.path().join("f2"), vec![b'2'; 9000]).unwrap();
    // A target too long for the ustar linkname field.
    std::os::unix::fs::symlink("t".repeat(150), temp.path().join("z")).unwrap();

    let out = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "20480",
            "--new-volume-script",
            "true",
            "-f",
            "vol.tar",
            "f1",
            "f2",
            "z",
        ],
        temp.path(),
    );
    assert_failure(&out, "a member ustar cannot hold");
    assert!(stderr_str(&out).contains('z'), "{}", stderr_str(&out));
    assert!(!temp.path().join("vol.tar.2").exists());
    let out = list_volume_set(temp.path(), "false");
    assert_success(&out, "pax -M list");
    assert_eq!(listed(&out), ["f1", "f2"]);
}

/// -M writes uncompressed volumes; -z was silently ignored.
#[test]
fn test_multi_volume_refuses_gzip() {
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("f"), "x").unwrap();
    let out = run_pax_in_dir(
        &[
            "-w",
            "-z",
            "-M",
            "--tape-length",
            "100000",
            "-f",
            "vol.tar",
            "f",
        ],
        temp.path(),
    );
    assert_failure(&out, "-z with -M");
    assert!(stderr_str(&out).contains("-z"), "{}", stderr_str(&out));
}

/// The previous volume is closed before the script is run for the next:
/// the script may be what takes it away, to a tape or another host.
#[test]
fn test_multi_volume_previous_volume_closed_before_script() {
    let Some(_) = system_tool("lsof") else {
        return;
    };
    let temp = TempDir::new().unwrap();
    let names = many_files(temp.path(), 8, 3000);
    let files: Vec<&str> = names.lines().collect();
    let mut args = vec![
        "-w",
        "-M",
        "--tape-length",
        "20480",
        "--new-volume-script",
        "lsof -p $PPID 2>/dev/null | grep -E '/vol\\.tar(\\.[0-9]+)?$' >> held; true",
        "-f",
        "vol.tar",
    ];
    args.extend_from_slice(&files);
    let out = run_pax_in_dir(&args, temp.path());
    assert_success(&out, "pax -w -M");
    assert!(temp.path().join("vol.tar.2").exists());
    assert_eq!(
        fs::read_to_string(temp.path().join("held")).unwrap(),
        "",
        "pax held a volume open while the script ran"
    );
}

/// Volume names are the archive name's bytes with `.N` after them. They were
/// made from a lossy UTF-8 rendering of it, so an archive name that is not
/// UTF-8 put its later volumes under a different name.
#[test]
fn test_multi_volume_non_utf8_archive_name() {
    use std::ffi::OsStr;
    use std::os::unix::ffi::OsStrExt;
    let temp = TempDir::new().unwrap();
    let names = many_files(temp.path(), 8, 3000);
    let name = OsStr::from_bytes(b"vol\xff");
    if plib::testing::create_non_utf8(temp.path(), b"vol\xff", |p| fs::write(p, "")).is_none() {
        return;
    }
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(["-w", "-M", "--tape-length", "20480"])
        .args(["--new-volume-script", "true", "-f"])
        .arg(name)
        .args(names.lines())
        .current_dir(temp.path())
        .output()
        .unwrap();
    assert_success(&out, "pax -w -M");
    assert!(temp.path().join(OsStr::from_bytes(b"vol\xff.2")).exists());

    let out = std::process::Command::new(env!("CARGO_BIN_EXE_pax"))
        .args(["-M", "--new-volume-script", "false", "-f"])
        .arg(name)
        .current_dir(temp.path())
        .output()
        .unwrap();
    assert_success(&out, "pax -M list");
    assert_eq!(stdout_str(&out), names);
}

/// A volume being written is the archive, and is not archived into itself --
/// any volume, not only the first, and whether the walk or a name operand
/// reaches it. The single-volume writer always skipped its archive; -M
/// never did, and stored a copy of a volume as a member.
#[test]
fn test_multi_volume_does_not_archive_its_own_volumes() {
    let temp = TempDir::new().unwrap();
    fs::write(temp.path().join("a"), vec![b'a'; 12000]).unwrap();
    fs::write(temp.path().join("b"), vec![b'b'; 12000]).unwrap();
    let out = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "20480",
            "--new-volume-script",
            "true",
            "-f",
            "vol.tar",
            "a",
            "b",
            "vol.tar",
            "vol.tar.2",
        ],
        temp.path(),
    );
    let err = stderr_str(&out);
    assert!(temp.path().join("vol.tar.2").exists(), "two volumes: {err}");
    assert!(
        err.contains("vol.tar: file is the archive")
            && err.contains("vol.tar.2: file is the archive"),
        "{err}"
    );
    let out = list_volume_set(temp.path(), "false");
    assert_eq!(listed(&out), vec!["a", "b"]);

    // The walk of a directory holding the archive finds it too.
    let dir = temp.path().join("d");
    fs::create_dir(&dir).unwrap();
    fs::write(dir.join("f"), "F\n").unwrap();
    let out = run_pax_in_dir(
        &[
            "-w",
            "-M",
            "--tape-length",
            "1048576",
            "-f",
            "./arch.tar",
            ".",
        ],
        &dir,
    );
    assert!(
        stderr_str(&out).contains("file is the archive"),
        "{}",
        stderr_str(&out)
    );
    let out = run_pax_in_dir(&["-M", "-f", "arch.tar"], &dir);
    assert_eq!(listed(&out), vec!["./", "./f"]);
}
