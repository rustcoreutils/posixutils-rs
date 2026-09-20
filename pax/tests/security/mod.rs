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
use std::os::unix::ffi::OsStrExt;

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

// ---------------------------------------------------------------------------
// Parser differentials: a member visible to one tool and not another is how
// content gets past a scanner. POSIX: "No data logical records are stored for
// types 1, 2, or 5."
// ---------------------------------------------------------------------------

/// A directory header with a non-zero size field. POSIX defines that field for
/// a directory as a size *limit*, not a data length, and says a system that
/// does not implement limiting should ignore it -- so the blocks that follow
/// are the next member's header. Honouring the field stepped over them and the
/// member vanished, while GNU tar read it.
#[test]
fn test_directory_size_field_does_not_hide_the_next_member() {
    let mut archive = Ustar {
        name: b"d/",
        typeflag: b'5',
        size: Some(512),
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &Ustar {
            name: b"hidden.txt",
            ..Default::default()
        }
        .member(),
    );
    archive.extend_from_slice(&ustar_trailer());

    let output = run_pax_with_stdin_bytes(&[], &archive);
    let listing = stdout_str(&output);
    assert!(
        listing.contains("hidden.txt"),
        "a member behind a directory size field was not seen: {listing}"
    );
    // A directory size limit is legal, so nothing is diagnosed.
    assert_success(&output, "list past a directory size field");
}

/// The same for a symbolic link, where POSIX requires the size field to be
/// zero -- so a non-zero one is malformed and says so.
#[test]
fn test_symlink_size_field_does_not_hide_the_next_member() {
    let mut archive = Ustar {
        name: b"ln",
        typeflag: b'2',
        linkname: b"target",
        size: Some(512),
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &Ustar {
            name: b"hidden.txt",
            ..Default::default()
        }
        .member(),
    );
    archive.extend_from_slice(&ustar_trailer());

    let output = run_pax_with_stdin_bytes(&[], &archive);
    let listing = stdout_str(&output);
    assert!(
        listing.contains("hidden.txt"),
        "a member behind a symlink size field was not seen: {listing}"
    );
    assert!(
        stderr_str(&output).contains("POSIX requires to be zero"),
        "a non-zero symlink size field must be diagnosed: {}",
        stderr_str(&output)
    );
}

/// A hard link *may* carry the linked file's contents in pax interchange
/// format -- POSIX says so explicitly, and that is `-o linkdata`. So its size
/// field is honoured, and a member hidden behind one is NOT seen.
///
/// This pins a deliberate divergence from GNU tar, which reads those blocks as
/// headers. It is here so the divergence stays a decision rather than becoming
/// an accident: the alternative is to ignore the field and lose the ability to
/// read a bsdtar archive written with linkdata.
#[test]
fn test_hardlink_size_field_is_honoured_in_pax_format() {
    let mut archive = Ustar {
        name: b"a.txt",
        body: b"aa\n",
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &Ustar {
            name: b"b.txt",
            typeflag: b'1',
            linkname: b"a.txt",
            size: Some(512),
            ..Default::default()
        }
        .member(),
    );
    archive.extend_from_slice(
        &Ustar {
            name: b"behind.txt",
            ..Default::default()
        }
        .member(),
    );
    archive.extend_from_slice(&ustar_trailer());

    let listing = stdout_str(&run_pax_with_stdin_bytes(&[], &archive));
    assert!(listing.contains("a.txt") && listing.contains("b.txt"));
    assert!(
        !listing.contains("behind.txt"),
        "typeflag 1 data is the linked file's contents in pax format, so the \
         blocks after it are data and not a header: {listing}"
    );
}

/// The end-of-archive indicator is *two* zero blocks. Stopping at one hid
/// every member after it and looked like a clean end of archive; GNU tar stops
/// there too, but says so.
#[test]
fn test_lone_zero_block_is_diagnosed() {
    let mut archive = Ustar {
        name: b"first.txt",
        body: b"1\n",
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(&[0u8; 512]);
    archive.extend_from_slice(
        &Ustar {
            name: b"after.txt",
            body: b"2\n",
            ..Default::default()
        }
        .member(),
    );
    archive.extend_from_slice(&ustar_trailer());

    let output = run_pax_with_stdin_bytes(&[], &archive);
    assert!(
        stderr_str(&output).contains("lone zero block"),
        "a lone zero block must not look like a clean end of archive: {}",
        stderr_str(&output)
    );
    assert_failure(&output, "list an archive truncated by a lone zero block");
}

/// An archive that genuinely ends after a single zero block -- the second
/// block being end-of-file rather than data -- is not truncated and must still
/// read cleanly.
#[test]
fn test_archive_ending_in_a_single_zero_block_reads_cleanly() {
    let mut archive = Ustar {
        name: b"only.txt",
        body: b"1\n",
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(&[0u8; 512]);

    let output = run_pax_with_stdin_bytes(&[], &archive);
    assert_success(&output, "list an archive ending in one zero block");
    assert!(stdout_str(&output).contains("only.txt"));
    assert!(
        !stderr_str(&output).contains("lone zero block"),
        "end of file after one zero block is a clean end, not a truncation: {}",
        stderr_str(&output)
    );
}

// ---------------------------------------------------------------------------
// The source side of write and copy mode, now walked from directory
// descriptors rather than by re-resolving a pathname on every operation.
//
// These pin behaviour rather than prove the fix. The defect was a race, and the
// syscall shape that closes it -- one component per openat, O_NOFOLLOW on each,
// and a (dev, ino) re-check after the open -- is observable under strace but
// not assertable from a test that does not win a race. What these catch is the
// traversal rewrite silently changing what gets archived: a symbolic link
// descended instead of copied, or a FIFO opened instead of stat'ed, would each
// turn up here.
// ---------------------------------------------------------------------------

/// A symbolic link where a directory would be is copied, not descended --
/// before the rewrite and after it. This is here so that the rewrite cannot
/// quietly start following one.
#[test]
fn test_write_does_not_descend_a_symlinked_source_directory() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    let outside = temp.path().join("outside");
    fs::create_dir_all(src.join("keep")).unwrap();
    fs::create_dir(&outside).unwrap();
    fs::write(src.join("keep/inside.txt"), b"mine\n").unwrap();
    fs::write(outside.join("secret.txt"), b"not mine\n").unwrap();

    // `sub` is what a directory would have been.
    std::os::unix::fs::symlink("../outside", src.join("sub")).unwrap();

    let archive = temp.path().join("a.tar");
    run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src,
    );

    let listing = stdout_str(&run_pax(&["-f", archive.to_str().unwrap()]));
    assert!(
        !listing.contains("secret.txt"),
        "the walk followed a symbolic link out of the source tree: {listing}"
    );
    assert!(
        listing.contains("keep/inside.txt"),
        "and must still have archived the real subtree: {listing}"
    );
}

/// The same for copy mode's source side.
#[test]
fn test_copy_does_not_descend_a_symlinked_source_directory() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    let outside = temp.path().join("outside");
    let dst = temp.path().join("dst");
    fs::create_dir_all(src.join("keep")).unwrap();
    fs::create_dir(&outside).unwrap();
    fs::create_dir(&dst).unwrap();
    fs::write(src.join("keep/inside.txt"), b"mine\n").unwrap();
    fs::write(outside.join("secret.txt"), b"not mine\n").unwrap();
    std::os::unix::fs::symlink("../outside", src.join("sub")).unwrap();

    run_pax_in_dir(&["-r", "-w", ".", dst.to_str().unwrap()], &src);

    // `sub` is copied as the symbolic link it is, not descended into. Note
    // that `dst/sub/secret.txt` *resolves* -- through the copied link, to the
    // original file -- so the property to assert is what `dst/sub` is, not
    // what can be reached through it.
    let sub = fs::symlink_metadata(dst.join("sub")).expect("the link should be copied");
    assert!(
        sub.file_type().is_symlink(),
        "the source symlink was descended and recreated as a real directory"
    );
    assert_eq!(
        fs::read_link(dst.join("sub")).unwrap(),
        std::path::Path::new("../outside"),
        "and it must be the same link, copied rather than resolved"
    );
    assert!(
        dst.join("keep/inside.txt").exists(),
        "and must still have copied the real subtree"
    );
}

/// A FIFO in the source tree must not stop the walk.
///
/// A FIFO is archived from its metadata and never opened, so this passes both
/// before and after the rewrite. It is here because the rewrite introduced an
/// `openat` of source files that `O_NOFOLLOW` alone does not make safe -- a
/// FIFO is not a symbolic link, and a blocking open of one with no writer never
/// returns -- so `open_source_file` opens non-blocking and checks what it
/// actually got. If that check is ever dropped, and something starts opening
/// non-regular files, this is what hangs.
#[test]
fn test_write_does_not_block_on_a_fifo_substituted_for_a_file() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir(&src).unwrap();
    fs::write(src.join("a.txt"), b"first\n").unwrap();

    // Pre-plant the outcome: a FIFO where a regular file is expected. Opening
    // it for reading with no writer would block forever.
    let fifo = src.join("b.fifo");
    let c = std::ffi::CString::new(fifo.as_os_str().as_bytes()).unwrap();
    assert_eq!(unsafe { libc::mkfifo(c.as_ptr(), 0o644) }, 0);

    fs::write(src.join("c.txt"), b"last\n").unwrap();

    let archive = temp.path().join("a.tar");
    let output = run_pax_in_dir(
        &["-w", "-x", "ustar", "-f", archive.to_str().unwrap(), "."],
        &src,
    );

    // A FIFO is archived from its metadata and never opened, so this finishes.
    let listing = stdout_str(&run_pax(&["-f", archive.to_str().unwrap()]));
    for want in ["a.txt", "b.fifo", "c.txt"] {
        assert!(
            listing.contains(want),
            "{want} missing; the walk did not get past the FIFO: {listing}\n{}",
            stderr_str(&output)
        );
    }
}

/// `-L` asks for the target, not the link. A dangling link has no target, and
/// the traversal falls back to the link's own metadata when it cannot stat one
/// -- so without this check the link is archived as a link, which is the
/// opposite of what was asked for, with a zero exit status.
#[test]
fn test_dangling_symlink_is_diagnosed_when_dereferencing() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir(&src).unwrap();
    std::os::unix::fs::symlink("/nonexistent/target", src.join("dangling")).unwrap();
    fs::write(src.join("real.txt"), b"ok\n").unwrap();

    let archive = temp.path().join("a.tar");
    let output = run_pax_in_dir(
        &[
            "-w",
            "-L",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            ".",
        ],
        &src,
    );

    assert!(
        stderr_str(&output).contains("dangling"),
        "a link that cannot be followed must be diagnosed: {}",
        stderr_str(&output)
    );
    assert_failure(&output, "archive a dangling link with -L");

    let listing = stdout_str(&run_pax(&["-f", archive.to_str().unwrap()]));
    assert!(
        !listing.contains("dangling"),
        "and must not be stored as the link it is: {listing}"
    );
    assert!(listing.contains("real.txt"), "the rest is still archived");
}

/// The same for `-H`, which dereferences only what is named on the command
/// line -- so a dangling link found *below* an operand is stored as a link,
/// and is not an error.
#[test]
fn test_dangling_symlink_below_an_operand_is_kept_under_cli_dereference() {
    let temp = TempDir::new().unwrap();
    let src = temp.path().join("src");
    fs::create_dir(&src).unwrap();
    std::os::unix::fs::symlink("/nonexistent/target", src.join("dangling")).unwrap();

    let archive = temp.path().join("a.tar");
    let output = run_pax_in_dir(
        &[
            "-w",
            "-H",
            "-x",
            "ustar",
            "-f",
            archive.to_str().unwrap(),
            ".",
        ],
        &src,
    );
    assert_success(&output, "-H does not dereference below the operand");

    let listing = stdout_str(&run_pax(&["-f", archive.to_str().unwrap()]));
    assert!(
        listing.contains("dangling"),
        "the link itself should be archived: {listing}"
    );
}
