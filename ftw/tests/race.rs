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
use std::io;
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
    let tmp = plib::tmp::Builder::new()
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
        |_entry, _exit| Ok(()),
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
    let tmp = plib::tmp::Builder::new()
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
        |_entry, _exit| Ok(()),
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

/// Ask `Entry::is_empty_dir` about `swapme` after the handler has replaced it with whatever
/// `swap` puts there. Returns the answer.
fn is_empty_dir_after_swap(tag: &str, swap: impl Fn(&Path, &Path)) -> io::Result<bool> {
    let tmp = plib::tmp::Builder::new()
        .prefix(tag)
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let base = tmp.path();
    let root = base.join("root");
    let outside = base.join("outside");
    fs::create_dir(&root).unwrap();
    fs::create_dir(&outside).unwrap();
    fs::create_dir(root.join("swapme")).unwrap();

    let mut answer = None;
    traverse_directory(
        &root,
        |entry| {
            if basename(&entry) == "swapme" {
                swap(&root.join("swapme"), &outside);
                answer = Some(entry.is_empty_dir());
                return Ok(false);
            }
            Ok(true)
        },
        |_entry, _exit| Ok(()),
        |_entry, _err| {},
        TraverseDirectoryOpts::default(),
    );
    answer.expect("the walk never reached swapme")
}

/// `is_empty_dir` opens the entry it was handed. An empty directory swapped for a symlink to an
/// (also empty) directory outside the tree must not be followed: `find -empty -delete` would
/// otherwise act on what the link points to.
#[test]
fn is_empty_dir_refuses_dir_swapped_for_symlink() {
    let answer = is_empty_dir_after_swap("ftw_race_empty_symlink", |swapme, outside| {
        fs::remove_dir(swapme).unwrap();
        unix::fs::symlink(outside, swapme).unwrap();
    });
    assert!(
        answer.is_err(),
        "is_empty_dir followed a symlink swapped in for the directory: {answer:?}"
    );
}

/// The same for a swap to a different real directory, which only the `(dev, ino)` check catches.
#[test]
fn is_empty_dir_refuses_dir_swapped_for_other_dir() {
    let answer = is_empty_dir_after_swap("ftw_race_empty_other", |swapme, outside| {
        fs::remove_dir(swapme).unwrap();
        fs::rename(outside, swapme).unwrap();
    });
    assert!(
        answer.is_err(),
        "is_empty_dir read a directory whose (dev, ino) changed under it: {answer:?}"
    );
}

/// `is_empty_dir` on a symbolic link the walk followed reads the directory it points to.
#[test]
fn is_empty_dir_reads_a_followed_symlink() {
    let tmp = plib::tmp::Builder::new()
        .prefix("ftw_empty_followed")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let root = tmp.path().join("root");
    fs::create_dir_all(root.join("empty")).unwrap();
    fs::create_dir_all(root.join("full/x")).unwrap();
    unix::fs::symlink("empty", root.join("to_empty")).unwrap();
    unix::fs::symlink("full", root.join("to_full")).unwrap();

    let mut answers = Vec::new();
    traverse_directory(
        &root,
        |entry| {
            let name = basename(&entry);
            if name.starts_with("to_") {
                answers.push((name, entry.is_empty_dir().unwrap()));
                return Ok(false);
            }
            Ok(true)
        },
        |_entry, _exit| Ok(()),
        |entry, err| panic!("unexpected error on {}: {:?}", entry.path(), err.kind()),
        TraverseDirectoryOpts {
            follow_symlinks: true,
            ..Default::default()
        },
    );
    answers.sort();
    assert_eq!(
        answers,
        [
            ("to_empty".to_string(), true),
            ("to_full".to_string(), false)
        ]
    );
}

/// Following symlinks is opt-in; a non-following walk must still see and report the symlinks
/// themselves (this guards against the hardening accidentally hiding entries).
#[test]
fn nonfollowing_walk_still_lists_symlink_entries() {
    let tmp = plib::tmp::Builder::new()
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
        |_entry, _exit| Ok(()),
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

/// A walk that does not follow symbolic links still resolves their targets. `cp -P` and `mv`
/// recreate a link from `Entry::read_link`, so leaving it unset here made them fail on the one
/// configuration that needs it most.
#[test]
fn symlink_target_available_without_following() {
    let tmp_dir = plib::tmp::Builder::new()
        .prefix("symlink_target_available_without_following")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let root = tmp_dir.path();

    fs::write(root.join("real"), b"x").unwrap();
    std::os::unix::fs::symlink("real", root.join("to_real")).unwrap();
    // Dangling too: the target is still readable even though it resolves to nothing.
    std::os::unix::fs::symlink("nowhere", root.join("dangling")).unwrap();

    let mut targets: Vec<(String, Option<String>)> = Vec::new();
    traverse_directory(
        root,
        |entry| {
            if entry.is_symlink() == Some(true) {
                targets.push((
                    entry.file_name().to_string_lossy().to_string(),
                    entry
                        .read_link()
                        .map(|link| link.to_string_lossy().to_string()),
                ));
            }
            Ok(true)
        },
        |_entry, _exit| Ok(()),
        |_entry, _err| {},
        TraverseDirectoryOpts::default(),
    );

    targets.sort();
    assert_eq!(
        targets,
        [
            ("dangling".to_string(), Some("nowhere".to_string())),
            ("to_real".to_string(), Some("real".to_string())),
        ]
    );
}

/// In descriptor-conserving mode a directory is reopened by path from an ancestor's descriptor
/// every time the walk comes back to it. A directory swapped for a different real directory
/// between two visits must be refused on the reopen, exactly as on a first descent: the walk must
/// not go on to enumerate the decoy as if it were the directory it stat'ed.
#[test]
fn deferred_reopen_refuses_dir_swapped_for_other_dir() {
    let tmp = plib::tmp::Builder::new()
        .prefix("ftw_race_deferred")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let base = tmp.path();

    let root = base.join("root");
    let decoy = base.join("decoy");
    fs::create_dir_all(root.join("a/b")).unwrap();
    fs::write(root.join("a/x"), b"x").unwrap();
    fs::create_dir(&decoy).unwrap();
    fs::write(decoy.join("DECOY.txt"), b"should never be visited").unwrap();

    let visited: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
    let errors = RefCell::new(0usize);
    let swapped = RefCell::new(false);

    traverse_directory(
        &root,
        |entry| {
            let name = basename(&entry);
            visited.borrow_mut().insert(name.clone());
            // `a` has been opened and is being enumerated; when the walk returns to it after `b`
            // it reopens `root/a` by path, which is now the decoy.
            if name == "b" && !*swapped.borrow() {
                fs::rename(root.join("a"), base.join("a_moved")).unwrap();
                fs::rename(&decoy, root.join("a")).unwrap();
                *swapped.borrow_mut() = true;
            }
            Ok(true)
        },
        |_entry, _exit| Ok(()),
        |_entry, _err| {
            *errors.borrow_mut() += 1;
        },
        TraverseDirectoryOpts {
            // Conserve descriptors from the first level on.
            caller_fds_per_level: 4096,
            ..Default::default()
        },
    );

    let visited = visited.into_inner();
    assert!(*swapped.borrow(), "the swap must have run");
    assert!(
        !visited.contains("DECOY.txt"),
        "a reopened deferred directory was not checked against its (dev, ino): {visited:?}"
    );
    assert!(*errors.borrow() > 0, "the refused reopen must be reported");
}

/// A deferred directory whose path from its anchor is longer than `PATH_MAX` is reopened one
/// component at a time. A FIFO swapped in for one of those components must be refused, not
/// opened: an `O_RDONLY` open of a FIFO blocks until a writer appears, which hung every walk.
#[test]
fn deferred_long_path_reopen_refuses_fifo_component() {
    use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
    use std::os::unix::ffi::OsStrExt;
    use std::sync::mpsc;
    use std::time::Duration;

    let tmp = plib::tmp::Builder::new()
        .prefix("ftw_race_long_fifo")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let base = tmp.path().to_path_buf();
    let root = base.join("root");
    fs::create_dir(&root).unwrap();

    // Twenty 250-byte components: far past PATH_MAX from `root`, so made through descriptors.
    const DEPTH: usize = 20;
    let names: Vec<String> = (0..DEPTH)
        .map(|i| format!("{i:02}{}", "d".repeat(248)))
        .collect();
    let mut dir = fs::File::open(&root).unwrap();
    for name in &names {
        let c = std::ffi::CString::new(name.as_str()).unwrap();
        assert_eq!(
            unsafe { libc::mkdirat(dir.as_raw_fd(), c.as_ptr(), 0o755) },
            0
        );
        let fd = unsafe {
            libc::openat(
                dir.as_raw_fd(),
                c.as_ptr(),
                libc::O_RDONLY | libc::O_DIRECTORY,
            )
        };
        assert!(fd >= 0);
        dir = fs::File::from(unsafe { OwnedFd::from_raw_fd(fd) });
    }
    drop(dir);

    // The second component, which the reopen of the deepest directory walks through.
    let swapped_component = root.join(&names[0]).join(&names[1]);
    let last_name = names[DEPTH - 1].clone();
    let (tx, rx) = mpsc::channel();
    let walker = {
        let swapped_component = swapped_component.clone();
        let base = base.clone();
        std::thread::spawn(move || {
            let mut swapped = false;
            let mut errors = 0usize;
            traverse_directory(
                &root,
                |entry| {
                    if !swapped && entry.file_name().to_bytes() == last_name.as_bytes() {
                        fs::rename(&swapped_component, base.join("moved")).unwrap();
                        let c = std::ffi::CString::new(swapped_component.as_os_str().as_bytes())
                            .unwrap();
                        assert_eq!(unsafe { libc::mkfifo(c.as_ptr(), 0o600) }, 0);
                        swapped = true;
                    }
                    Ok(true)
                },
                |_entry, _exit| Ok(()),
                |_entry, _err| errors += 1,
                TraverseDirectoryOpts {
                    // Conserve descriptors from the first level on.
                    caller_fds_per_level: 4096,
                    ..Default::default()
                },
            );
            tx.send((swapped, errors)).unwrap();
        })
    };

    match rx.recv_timeout(Duration::from_secs(20)) {
        Ok((swapped, errors)) => {
            walker.join().unwrap();
            assert!(swapped, "the swap must have run");
            assert!(errors > 0, "the refused reopen must be reported");
        }
        Err(_) => {
            // Release the walker blocked in the FIFO's open before failing.
            let c = std::ffi::CString::new(swapped_component.as_os_str().as_bytes()).unwrap();
            let fd = unsafe { libc::open(c.as_ptr(), libc::O_WRONLY | libc::O_NONBLOCK) };
            if fd >= 0 {
                unsafe { libc::close(fd) };
            }
            panic!("the walk hung opening a FIFO swapped in for a long-path component");
        }
    }
}

/// Walk `root` after the handler for the starting point itself has replaced it using `swap`.
/// Returns the names visited and the number of errors reported.
fn walk_after_root_swap(tag: &str, swap: impl Fn(&Path, &Path)) -> (HashSet<String>, usize) {
    let tmp = plib::tmp::Builder::new()
        .prefix(tag)
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .unwrap();
    let base = tmp.path();
    let root = base.join("root");
    let outside = base.join("outside");
    fs::create_dir(&root).unwrap();
    fs::create_dir(&outside).unwrap();
    fs::write(outside.join("SECRET.txt"), b"should never be visited").unwrap();

    let mut visited = HashSet::new();
    let mut errors = 0;
    let mut swapped = false;
    traverse_directory(
        &root,
        |entry| {
            if !swapped {
                swap(&root, &outside);
                swapped = true;
            }
            visited.insert(basename(&entry));
            Ok(true)
        },
        |_entry, _exit| Ok(()),
        |_entry, _err| errors += 1,
        TraverseDirectoryOpts::default(),
    );
    assert!(swapped);
    (visited, errors)
}

/// The starting point is stat'ed without following (no -H/-L) and then opened: a directory
/// swapped for a symlink in between must not be followed.
#[test]
fn root_open_refuses_dir_swapped_for_symlink() {
    let (visited, errors) = walk_after_root_swap("ftw_race_root_symlink", |root, outside| {
        fs::remove_dir(root).unwrap();
        unix::fs::symlink(outside, root).unwrap();
    });
    assert!(!visited.contains("SECRET.txt"), "followed: {visited:?}");
    assert!(errors > 0);
}

/// The same for a swap to a different real directory, caught by the `(dev, ino)` check.
#[test]
fn root_open_refuses_dir_swapped_for_other_dir() {
    let (visited, errors) = walk_after_root_swap("ftw_race_root_other", |root, outside| {
        fs::remove_dir(root).unwrap();
        fs::rename(outside, root).unwrap();
    });
    assert!(!visited.contains("SECRET.txt"), "entered: {visited:?}");
    assert!(errors > 0);
}
