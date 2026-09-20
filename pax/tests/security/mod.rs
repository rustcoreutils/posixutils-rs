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
use plib::tmp::TempDir;
use std::fs;

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

    let archive = Ustar {
        name: b"evil",
        body: b"payload\n",
        ..Default::default()
    }
    .archive();
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

    let archive = Ustar {
        name: b"sub/file",
        body: b"payload\n",
        ..Default::default()
    }
    .archive();
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

    let archive = Ustar {
        name: b"evil",
        body: b"payload\n",
        ..Default::default()
    }
    .archive();
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

    let archive = Ustar {
        name: b"link",
        typeflag: b'1',
        linkname: b"sub/secret",
        ..Default::default()
    }
    .archive();
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

    let archive = Ustar {
        name: b"../escape",
        body: b"nope\n",
        ..Default::default()
    }
    .archive();
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

/// Directory attributes are applied after the whole archive is extracted, so an
/// archive can arrange for the name to be a symlink by then: a directory member
/// `d/`, then a symlink member `d`. Applying the archived mode by name chmods
/// whatever the link points at, anywhere on the system.
#[test]
fn test_extract_does_not_chmod_through_planted_symlink() {
    use std::os::unix::fs::PermissionsExt;

    let temp = TempDir::new().unwrap();
    let outside = temp.path().join("outside");
    let dst = temp.path().join("dst");
    fs::create_dir(&outside).unwrap();
    fs::create_dir(&dst).unwrap();

    let victim = outside.join("victim");
    fs::write(&victim, b"secret\n").unwrap();
    fs::set_permissions(&victim, fs::Permissions::from_mode(0o600)).unwrap();

    let mut archive = Ustar {
        name: b"d/",
        typeflag: b'5',
        mode: 0o777,
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &Ustar {
            name: b"d",
            typeflag: b'2',
            linkname: b"../outside/victim",
            mode: 0o777,
            ..Default::default()
        }
        .member(),
    );
    archive.extend_from_slice(&ustar_trailer());

    let output = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, &dst);

    assert_eq!(
        fs::metadata(&victim).unwrap().permissions().mode() & 0o7777,
        0o600,
        "an archive changed the mode of a file outside the extraction directory: {}",
        stderr_str(&output)
    );
}

/// A set-user-ID member must not exist as a set-user-ID file before its
/// contents are complete. The window itself is a race and is asserted at the
/// unit level (`AttrPolicy::creation_mode`); what this pins is that closing it
/// did not cost the preservation `-p e` asks for.
#[test]
fn test_extract_preserves_setuid_bit_on_the_finished_file() {
    use std::os::unix::fs::PermissionsExt;

    let temp = TempDir::new().unwrap();
    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();

    let archive = Ustar {
        name: b"suid",
        mode: 0o4755,
        body: b"contents\n",
        ..Default::default()
    }
    .archive();
    let output = run_pax_with_stdin_bytes_in_dir(&["-r", "-p", "e"], &archive, &dst);

    let mode = fs::metadata(dst.join("suid")).unwrap().permissions().mode();
    assert_eq!(
        mode & 0o7777,
        0o4755,
        "-p e must restore the archived set-user-ID mode: {}",
        stderr_str(&output)
    );
    assert_eq!(
        fs::read_to_string(dst.join("suid")).unwrap(),
        "contents\n",
        "and the contents must be complete"
    );
}

/// Without `-p o`/`-p e` the set-id bits are dropped entirely: the file is
/// about to belong to whoever ran pax, not to the user the archive names.
#[test]
fn test_extract_drops_setuid_without_preserve() {
    use std::os::unix::fs::PermissionsExt;

    let temp = TempDir::new().unwrap();
    let dst = temp.path().join("dst");
    fs::create_dir(&dst).unwrap();

    let archive = Ustar {
        name: b"sgid",
        mode: 0o6755,
        body: b"contents\n",
        ..Default::default()
    }
    .archive();
    run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, &dst);

    let mode = fs::metadata(dst.join("sgid")).unwrap().permissions().mode();
    assert_eq!(
        mode & 0o7000,
        0,
        "set-id bits must not survive extraction without -p o or -p e"
    );
}
