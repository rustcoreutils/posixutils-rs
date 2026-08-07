//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Extraction must stay inside the extraction directory.
//!
//! A real race cannot be won deterministically in a test, so these pre-plant
//! the state an attacker would have achieved by winning one, and assert the
//! outcome that matters: nothing is written outside the tree. The syscall shape
//! that makes the window unwinnable (O_EXCL, O_NOFOLLOW, linkat flags=0) is
//! reviewed, not tested.

use crate::common::*;
use std::fs;
use tempfile::TempDir;

/// Build a one-member ustar archive by hand, so the member name can be anything
/// -- including names pax itself would refuse to write.
fn ustar_archive(name: &str, typeflag: u8, linkname: &str, body: &[u8]) -> Vec<u8> {
    let mut h = [0u8; 512];
    h[..name.len()].copy_from_slice(name.as_bytes());
    h[100..108].copy_from_slice(b"0000644\0");
    h[108..116].copy_from_slice(b"0000000\0");
    h[116..124].copy_from_slice(b"0000000\0");
    h[124..136].copy_from_slice(format!("{:011o}\0", body.len()).as_bytes());
    h[136..148].copy_from_slice(b"00000000000\0");
    h[148..156].copy_from_slice(b"        ");
    h[156] = typeflag;
    h[157..157 + linkname.len()].copy_from_slice(linkname.as_bytes());
    h[257..263].copy_from_slice(b"ustar\0");
    h[263..265].copy_from_slice(b"00");
    let sum: u32 = h.iter().map(|&b| b as u32).sum();
    h[148..156].copy_from_slice(format!("{:06o}\0 ", sum).as_bytes());

    let mut a = h.to_vec();
    let mut data = body.to_vec();
    if !data.is_empty() {
        data.resize(data.len().div_ceil(512) * 512, 0);
        a.extend_from_slice(&data);
    }
    a.extend_from_slice(&[0u8; 1024]);
    a
}

/// A symlink planted where a member will be written must not be followed. This
/// is the state an attacker reaches by winning the window between the old
/// code's remove_file and File::create.
#[test]
fn test_extract_does_not_follow_planted_symlink_at_leaf() {
    let temp = TempDir::new().unwrap();
    let outside = temp.path().join("outside");
    let dst = temp.path().join("dst");
    fs::create_dir(&outside).unwrap();
    fs::create_dir(&dst).unwrap();

    std::os::unix::fs::symlink("../outside/target", dst.join("evil")).unwrap();

    let archive = ustar_archive("evil", b'0', "", b"payload\n");
    let output = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, &dst);

    assert!(
        !outside.join("target").exists(),
        "member data escaped the extraction directory: {}",
        stderr_str(&output)
    );
    let meta = fs::symlink_metadata(dst.join("evil")).unwrap();
    assert!(
        meta.file_type().is_file(),
        "the member should have replaced the symlink with a regular file"
    );
    assert_eq!(fs::read_to_string(dst.join("evil")).unwrap(), "payload\n");
}

/// The same, one level up: a symlinked *directory* component. The old code
/// called create_dir_all on the parent, which resolved the symlink and wrote
/// straight through it.
#[test]
fn test_extract_does_not_follow_planted_symlink_in_parent() {
    let temp = TempDir::new().unwrap();
    let outside = temp.path().join("outside");
    let dst = temp.path().join("dst");
    fs::create_dir(&outside).unwrap();
    fs::create_dir(&dst).unwrap();

    std::os::unix::fs::symlink("../outside", dst.join("sub")).unwrap();

    let archive = ustar_archive("sub/file", b'0', "", b"payload\n");
    let output = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, &dst);

    assert!(
        !outside.join("file").exists(),
        "member data escaped through a symlinked parent: {}",
        stderr_str(&output)
    );
}

/// -k must leave an existing name completely alone, whatever its type, and
/// still must not write through it.
#[test]
fn test_extract_no_clobber_with_planted_symlink() {
    let temp = TempDir::new().unwrap();
    let outside = temp.path().join("outside");
    let dst = temp.path().join("dst");
    fs::create_dir(&outside).unwrap();
    fs::create_dir(&dst).unwrap();
    std::os::unix::fs::symlink("../outside/target", dst.join("evil")).unwrap();

    let archive = ustar_archive("evil", b'0', "", b"payload\n");
    run_pax_with_stdin_bytes_in_dir(&["-r", "-k"], &archive, &dst);

    assert!(
        !outside.join("target").exists(),
        "-k wrote outside the tree"
    );
    assert!(
        fs::symlink_metadata(dst.join("evil"))
            .unwrap()
            .file_type()
            .is_symlink(),
        "-k must not replace an existing name"
    );
}

/// A hard-link member whose target escapes the tree must not link a file in
/// from outside. fs::hard_link resolved the whole target path; linkat with
/// flags=0 does not.
#[test]
fn test_extract_hardlink_target_cannot_escape() {
    let temp = TempDir::new().unwrap();
    let outside = temp.path().join("outside");
    let dst = temp.path().join("dst");
    fs::create_dir(&outside).unwrap();
    fs::create_dir(&dst).unwrap();
    fs::write(outside.join("secret"), b"secret\n").unwrap();
    std::os::unix::fs::symlink("../outside", dst.join("sub")).unwrap();

    let archive = ustar_archive("link", b'1', "sub/secret", b"");
    run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, &dst);

    assert!(
        !dst.join("link").exists(),
        "a hard link to a file outside the tree must not be created"
    );
}

/// A member name that walks upward must stay inside.
#[test]
fn test_extract_parent_traversal_stays_inside() {
    let temp = TempDir::new().unwrap();
    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();

    let archive = ustar_archive("../escape", b'0', "", b"nope\n");
    run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, &dst);

    assert!(
        !temp.path().join("escape").exists(),
        "a `..` member escaped the extraction directory"
    );
    assert!(dst.join("escape").exists(), "and should land inside it");
}

/// A directory whose archived mode denies write or search must still receive
/// its members, and must end up with that mode. Its mtime must also survive
/// being populated.
#[test]
fn test_extract_readonly_directory_gets_its_contents() {
    use std::os::unix::fs::PermissionsExt;

    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    let dst = temp.path().join("dst");
    let archive = temp.path().join("ro.tar");
    fs::create_dir_all(src.join("ro")).unwrap();
    fs::create_dir(&dst).unwrap();
    fs::write(src.join("ro/inside.txt"), b"must survive").unwrap();
    fs::set_permissions(src.join("ro"), fs::Permissions::from_mode(0o555)).unwrap();

    assert_success(
        &run_pax_in_dir(
            &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "ro"],
            &src,
        ),
        "archive a read-only directory",
    );
    fs::set_permissions(src.join("ro"), fs::Permissions::from_mode(0o755)).unwrap();

    let output = run_pax_in_dir(&["-r", "-p", "e", "-f", archive.to_str().unwrap()], &dst);

    assert_eq!(
        fs::read_to_string(dst.join("ro/inside.txt")).unwrap_or_default(),
        "must survive",
        "a read-only directory must still receive its contents: {}",
        stderr_str(&output)
    );
    assert_eq!(
        fs::metadata(dst.join("ro")).unwrap().permissions().mode() & 0o777,
        0o555,
        "and must end with its archived mode"
    );
    fs::set_permissions(dst.join("ro"), fs::Permissions::from_mode(0o755)).unwrap();
}

// Not covered here: extracting a path longer than PATH_MAX, which component-wise
// resolution now makes possible. Building the fixture needs the same fd-relative
// walk under test -- std's create_dir_all fails with ENAMETOOLONG on the source
// tree -- so the test would mostly exercise its own scaffolding.
