//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Regression tests for the directory-descent symlink-swap / TOCTOU hardening (ftw audit #F1).
//!
//! `traverse_directory` invokes the `file_handler` callback *between* the non-following `fstatat`
//! of a directory entry and the `openat` used to descend into it. That window lets these tests
//! deterministically simulate an attacker replacing a directory with a symlink (or a different
//! directory) mid-walk, with no threads or timing required: the swap is performed inside the
//! handler for the target entry, just before the engine attempts the descent.

use std::cell::RefCell;
use std::collections::HashSet;
use std::fs;
use std::os::unix;
use std::path::Path;

use ftw::{traverse_directory, TraverseDirectoryOpts};

fn basename(entry: &ftw::Entry) -> String {
    String::from_utf8_lossy(entry.file_name().to_bytes()).into_owned()
}

/// A directory entry that is a real directory at `fstatat` time but is swapped for a symlink to an
/// out-of-tree directory before the descent `openat` must NOT be followed: with `O_NOFOLLOW` the
/// descent fails and the out-of-tree contents are never visited.
#[test]
fn descent_refuses_dir_swapped_for_symlink() {
    let tmp = tempfile::Builder::new()
        .prefix("ftw_race_symlink")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let base = tmp.path();

    let root = base.join("root");
    let outside = base.join("outside");
    fs::create_dir(&root).unwrap();
    fs::create_dir(&outside).unwrap();
    fs::write(outside.join("SECRET.txt"), b"should never be visited").unwrap();

    // `swapme` is an *empty* directory so it can be `rmdir`'d inside the handler.
    let swapme = root.join("swapme");
    fs::create_dir(&swapme).unwrap();
    fs::write(root.join("keep.txt"), b"normal sibling").unwrap();

    let visited: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
    let errors = RefCell::new(0usize);
    let swapped = RefCell::new(false);

    let root_for_handler = root.clone();
    let outside_for_handler = outside.clone();

    traverse_directory(
        &root,
        |entry| {
            let name = basename(&entry);
            visited.borrow_mut().insert(name.clone());

            let is_dir = entry
                .metadata()
                .map(|m| m.file_type() == ftw::FileType::Directory)
                .unwrap_or(false);

            // The swap happens after ftw has already stat'd `swapme` as a directory but before it
            // opens it for descent.
            if name == "swapme" && is_dir && !*swapped.borrow() {
                let target = root_for_handler.join("swapme");
                fs::remove_dir(&target).unwrap();
                unix::fs::symlink(&outside_for_handler, &target).unwrap();
                *swapped.borrow_mut() = true;
            }
            Ok(true)
        },
        |_entry| Ok(()),
        |_entry, _err| {
            *errors.borrow_mut() += 1;
        },
        TraverseDirectoryOpts::default(),
    );

    let visited = visited.into_inner();
    assert!(*swapped.borrow(), "the swap must have run");
    assert!(
        visited.contains("swapme"),
        "the entry itself is still processed"
    );
    assert!(
        visited.contains("keep.txt"),
        "unrelated siblings are still walked"
    );
    assert!(
        !visited.contains("SECRET.txt"),
        "descent followed the swapped-in symlink out of the tree: {visited:?}"
    );
    assert!(
        *errors.borrow() > 0,
        "the refused descent must report an error"
    );

    // Sanity: confirm `swapme` really is a symlink now (the swap was effective).
    assert!(fs::symlink_metadata(root.join("swapme"))
        .unwrap()
        .file_type()
        .is_symlink());
}

/// A directory entry that is replaced with a *different real directory* (same filesystem, new
/// inode) before the descent. `O_NOFOLLOW` cannot catch this (it is a genuine directory), but the
/// post-open `(dev, ino)` re-verification must: the decoy's contents are never visited.
#[test]
fn descent_refuses_dir_swapped_for_other_dir() {
    let tmp = tempfile::Builder::new()
        .prefix("ftw_race_devino")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let base = tmp.path();

    let root = base.join("root");
    let decoy = base.join("decoy");
    fs::create_dir(&root).unwrap();
    fs::create_dir(&decoy).unwrap();
    fs::write(decoy.join("DECOY.txt"), b"should never be visited").unwrap();

    let swapme = root.join("swapme");
    fs::create_dir(&swapme).unwrap();

    let visited: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
    let errors = RefCell::new(0usize);
    let swapped = RefCell::new(false);

    let root_for_handler = root.clone();
    let decoy_for_handler = decoy.clone();

    traverse_directory(
        &root,
        |entry| {
            let name = basename(&entry);
            visited.borrow_mut().insert(name.clone());

            let is_dir = entry
                .metadata()
                .map(|m| m.file_type() == ftw::FileType::Directory)
                .unwrap_or(false);

            if name == "swapme" && is_dir && !*swapped.borrow() {
                let target = root_for_handler.join("swapme");
                fs::remove_dir(&target).unwrap();
                // Move the decoy directory into `swapme`'s place: a real directory with a
                // different inode than the one ftw stat'd.
                fs::rename(&decoy_for_handler, &target).unwrap();
                *swapped.borrow_mut() = true;
            }
            Ok(true)
        },
        |_entry| Ok(()),
        |_entry, _err| {
            *errors.borrow_mut() += 1;
        },
        TraverseDirectoryOpts::default(),
    );

    let visited = visited.into_inner();
    assert!(*swapped.borrow(), "the swap must have run");
    assert!(
        !visited.contains("DECOY.txt"),
        "descent entered a directory whose (dev, ino) changed under it: {visited:?}"
    );
    assert!(
        *errors.borrow() > 0,
        "the dev/ino mismatch must report an error"
    );
}

/// Following symlinks is opt-in; a non-following walk must still see and report the symlinks
/// themselves (this guards against the hardening accidentally hiding entries).
#[test]
fn nonfollowing_walk_still_lists_symlink_entries() {
    let tmp = tempfile::Builder::new()
        .prefix("ftw_race_listsym")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let root = tmp.path().join("root");
    fs::create_dir(&root).unwrap();
    fs::write(root.join("a.txt"), b"a").unwrap();
    unix::fs::symlink(Path::new("a.txt"), root.join("link")).unwrap();

    let visited: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
    traverse_directory(
        &root,
        |entry| {
            visited.borrow_mut().insert(basename(&entry));
            Ok(true)
        },
        |_entry| Ok(()),
        |_entry, _err| {},
        TraverseDirectoryOpts::default(),
    );

    let visited = visited.into_inner();
    assert!(visited.contains("a.txt"));
    assert!(
        visited.contains("link"),
        "symlink entry should still be listed: {visited:?}"
    );
}
