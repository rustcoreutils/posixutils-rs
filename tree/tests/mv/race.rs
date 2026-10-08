//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A cross-filesystem `mv` racing someone who changes the source or destination tree while the
//! copy runs. Each race is staged deterministically: mv is held in the middle of its copy by a
//! write lease this process takes on a source file (the kernel makes mv's open of it wait until
//! the lease is released), the change is made while mv waits, and the lease is then released.
//!
//! Leases and the second filesystem (`/dev/shm`, or `OTHER_PARTITION_TMPDIR`) are Linux-only.

#![cfg(target_os = "linux")]

use std::fs;
use std::io::Read;
use std::os::fd::AsRawFd;
use std::os::unix::fs::{symlink, MetadataExt};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use plib::testing::get_binary_path;

/// A fresh directory on the source filesystem.
fn scratch(tag: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("mv_race_{tag}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    dir
}

/// A fresh directory on a second filesystem, or `None` (the test is then skipped) when there is
/// no second filesystem to move to.
fn other_fs(tag: &str) -> Option<PathBuf> {
    let root = Path::new(option_env!("OTHER_PARTITION_TMPDIR").unwrap_or("/dev/shm"));
    let here = fs::metadata(env!("CARGO_TARGET_TMPDIR")).unwrap().dev();
    match fs::metadata(root) {
        Ok(md) if md.dev() != here => {}
        _ => {
            eprintln!("skipping: no second filesystem at {}", root.display());
            return None;
        }
    }
    let dir = root.join(format!("mv_race_{tag}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir(&dir).unwrap();
    Some(dir)
}

/// A write lease on `path`, held by this process: anyone else's open of the file waits until it
/// is released.
struct Lease(fs::File);

impl Lease {
    fn take(path: &Path) -> Self {
        // The kernel signals the lease holder with SIGIO when the lease must break; the default
        // action would end the test run.
        unsafe { libc::signal(libc::SIGIO, libc::SIG_IGN) };
        let holder = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open(path)
            .unwrap();
        // A write lease is refused while anything else holds the inode, which pending writeback
        // of the file just written can do for a moment.
        holder.sync_all().unwrap();
        let deadline = Instant::now() + Duration::from_secs(10);
        while unsafe { libc::fcntl(holder.as_raw_fd(), libc::F_SETLEASE, libc::F_WRLCK) } != 0 {
            let e = std::io::Error::last_os_error();
            assert!(
                e.raw_os_error() == Some(libc::EAGAIN) && Instant::now() < deadline,
                "cannot take a lease: {e}"
            );
            std::thread::sleep(Duration::from_millis(10));
        }
        Lease(holder)
    }

    /// Whether someone's open is waiting for the lease: the kernel then reports the type the
    /// lease is being broken to instead of `F_WRLCK`.
    fn is_broken(&self) -> bool {
        unsafe { libc::fcntl(self.0.as_raw_fd(), libc::F_GETLEASE) != libc::F_WRLCK }
    }

    fn release(self) {
        unsafe { libc::fcntl(self.0.as_raw_fd(), libc::F_SETLEASE, libc::F_UNLCK) };
    }
}

/// Run `mv args` in `base` while `leased` is under a write lease; once mv is waiting to open it,
/// run `meanwhile`, then release the lease. Returns mv's exit status and stderr.
fn mv_paused_on(
    base: &Path,
    args: &[&Path],
    leased: &Path,
    meanwhile: impl FnOnce(),
) -> (Option<i32>, String) {
    let lease = Lease::take(leased);
    let mut child = Command::new(get_binary_path("mv"))
        .args(args)
        .current_dir(base)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to execute mv");
    let deadline = Instant::now() + Duration::from_secs(30);
    while !lease.is_broken() {
        if let Some(status) = child.try_wait().unwrap() {
            let mut stderr = String::new();
            child
                .stderr
                .take()
                .unwrap()
                .read_to_string(&mut stderr)
                .unwrap();
            panic!("mv finished ({status}) without opening the leased file: {stderr}");
        }
        assert!(Instant::now() < deadline, "mv never opened the leased file");
        std::thread::sleep(Duration::from_millis(5));
    }
    meanwhile();
    lease.release();
    let out = child.wait_with_output().unwrap();
    (
        out.status.code(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

/// `base/att/x/` holding `f`, and `base/victim/x/precious` that mv must never touch.
fn att_and_victim(base: &Path) {
    fs::create_dir_all(base.join("att/x")).unwrap();
    fs::write(base.join("att/x/f"), b"moved").unwrap();
    fs::create_dir_all(base.join("victim/x")).unwrap();
    fs::write(base.join("victim/x/precious"), b"precious").unwrap();
}

/// While mv copies, the source's parent is renamed away and replaced by a symbolic link to
/// another directory holding an entry of the same name.
fn swap_att_for_victim(base: &Path) {
    fs::rename(base.join("att"), base.join("att.real")).unwrap();
    symlink("victim", base.join("att")).unwrap();
}

/// After a cross-filesystem copy, mv removes the source it copied -- found through the directory
/// it held, not by resolving the operand again, which now leads to someone else's directory.
#[test]
fn mv_removes_the_copied_source_not_what_its_path_names_now() {
    let Some(other) = other_fs("parent_swap") else {
        return;
    };
    let base = scratch("parent_swap");
    att_and_victim(&base);
    let target = other.join("x");

    let (status, stderr) = mv_paused_on(
        &base,
        &[Path::new("att/x"), &target],
        &base.join("att/x/f"),
        || swap_att_for_victim(&base),
    );

    assert!(
        base.join("victim/x/precious").exists(),
        "mv removed a directory swapped in for its source's parent; stderr: {stderr}"
    );
    assert_eq!(status, Some(0), "stderr: {stderr}");
    assert_eq!(fs::read(target.join("f")).unwrap(), b"moved");
    assert!(
        !base.join("att.real/x").exists(),
        "the copied source was left behind"
    );

    let _ = fs::remove_dir_all(&base);
    let _ = fs::remove_dir_all(&other);
}

/// The same with several operands, where every removal waits until all of them are copied.
#[test]
fn mv_removes_each_copied_operand_through_the_directory_it_held() {
    let Some(other) = other_fs("parent_swap_multi") else {
        return;
    };
    let base = scratch("parent_swap_multi");
    att_and_victim(&base);
    fs::create_dir(base.join("y")).unwrap();
    fs::write(base.join("y/f"), b"y").unwrap();

    let (status, stderr) = mv_paused_on(
        &base,
        &[Path::new("att/x"), Path::new("y"), &other],
        &base.join("y/f"),
        || swap_att_for_victim(&base),
    );

    assert!(
        base.join("victim/x/precious").exists(),
        "mv removed a directory swapped in for its source's parent; stderr: {stderr}"
    );
    assert_eq!(status, Some(0), "stderr: {stderr}");
    assert_eq!(fs::read(other.join("x/f")).unwrap(), b"moved");
    assert_eq!(fs::read(other.join("y/f")).unwrap(), b"y");
    assert!(!base.join("att.real/x").exists());
    assert!(!base.join("y").exists());

    let _ = fs::remove_dir_all(&base);
    let _ = fs::remove_dir_all(&other);
}

/// Only what the copy duplicated is removed from the source: an entry added after the copy, one
/// written to since, and one replaced by another file are all left where they are, reported,
/// and the exit status says the move was not completed. The directories holding them stay.
#[test]
fn mv_leaves_source_entries_added_or_changed_after_the_copy() {
    let Some(other) = other_fs("changed_source") else {
        return;
    };
    let base = scratch("changed_source");
    fs::create_dir_all(base.join("x/sub")).unwrap();
    for name in ["copied", "appended", "replaced", "sub/copied"] {
        fs::write(base.join("x").join(name), name).unwrap();
    }
    fs::create_dir(base.join("y")).unwrap();
    fs::write(base.join("y/f"), b"y").unwrap();

    let (status, stderr) = mv_paused_on(
        &base,
        &[Path::new("x"), Path::new("y"), &other],
        &base.join("y/f"),
        || {
            fs::write(base.join("x/sub/added"), b"added").unwrap();
            let mut appended = fs::OpenOptions::new()
                .append(true)
                .open(base.join("x/appended"))
                .unwrap();
            std::io::Write::write_all(&mut appended, b" and more").unwrap();
            fs::remove_file(base.join("x/replaced")).unwrap();
            fs::write(base.join("x/replaced"), b"new file").unwrap();
        },
    );

    assert_eq!(status, Some(1), "stderr: {stderr}");
    assert_eq!(
        fs::read(base.join("x/sub/added")).unwrap(),
        b"added",
        "an entry added after the copy was removed"
    );
    assert_eq!(
        fs::read(base.join("x/appended")).unwrap(),
        b"appended and more"
    );
    assert_eq!(fs::read(base.join("x/replaced")).unwrap(), b"new file");
    assert!(!base.join("x/copied").exists(), "a copied entry was left");
    assert!(
        !base.join("x/sub/copied").exists(),
        "a copied entry was left"
    );
    assert!(
        !base.join("y").exists(),
        "the other operand was not removed"
    );
    for name in ["x/sub/added", "x/appended", "x/replaced"] {
        assert!(
            stderr.contains(&format!(
                "mv: not removing '{name}': it changed during the move\n"
            )),
            "{name} not reported; stderr: {stderr}"
        );
    }
    assert!(
        !stderr.contains("Directory not empty"),
        "a directory left only because of a reported entry is reported again: {stderr}"
    );

    let _ = fs::remove_dir_all(&base);
    let _ = fs::remove_dir_all(&other);
}

/// `f` and `g`, hard links to one file, with `y` between them; mv pauses while copying `y`.
fn hard_links_around_y(base: &Path) {
    fs::write(base.join("f"), b"moved").unwrap();
    fs::hard_link(base.join("f"), base.join("g")).unwrap();
    fs::create_dir(base.join("y")).unwrap();
    fs::write(base.join("y/z"), b"y").unwrap();
}

/// Moving two hard links of one file, mv makes the second a hard link to the copy of the
/// first. Someone who can write the destination directory renames a file of their own over the
/// first copy meanwhile: the second name must not become a link to their file (the source is
/// then removed, so its data would be lost).
#[test]
fn mv_never_links_a_later_name_to_a_file_renamed_over_the_first_copy() {
    let Some(other) = other_fs("link_renamed_over") else {
        return;
    };
    let base = scratch("link_renamed_over");
    hard_links_around_y(&base);

    let (status, stderr) = mv_paused_on(
        &base,
        &[Path::new("f"), Path::new("y"), Path::new("g"), &other],
        &base.join("y/z"),
        || {
            fs::write(other.join("planted"), b"attacker").unwrap();
            fs::rename(other.join("planted"), other.join("f")).unwrap();
        },
    );

    assert_eq!(
        fs::read(other.join("g")).unwrap(),
        b"moved",
        "the second name was linked to a file renamed over the first copy; stderr: {stderr}"
    );
    assert_eq!(status, Some(0), "stderr: {stderr}");
    assert!(!base.join("g").exists());

    let _ = fs::remove_dir_all(&base);
    let _ = fs::remove_dir_all(&other);
}

/// The target directory operand is resolved once: replaced by a symbolic link to another
/// directory in the middle of the move, it still receives every operand, hard links included.
#[test]
fn mv_moves_every_operand_into_the_target_directory_it_opened() {
    let Some(other) = other_fs("target_dir_swap") else {
        return;
    };
    let base = scratch("target_dir_swap");
    hard_links_around_y(&base);
    let dest = other.join("d");
    fs::create_dir(&dest).unwrap();
    fs::create_dir(other.join("victim")).unwrap();
    fs::write(other.join("victim/f"), b"attacker").unwrap();

    let (status, stderr) = mv_paused_on(
        &base,
        &[Path::new("f"), Path::new("y"), Path::new("g"), &dest],
        &base.join("y/z"),
        || {
            fs::rename(&dest, other.join("d.real")).unwrap();
            symlink("victim", &dest).unwrap();
        },
    );

    assert!(
        !other.join("victim/g").exists(),
        "an operand was moved into a directory swapped in for the target; stderr: {stderr}"
    );
    assert_eq!(status, Some(0), "stderr: {stderr}");
    for name in ["f", "g", "y/z"] {
        assert!(other.join("d.real").join(name).exists(), "{name} not moved");
    }
    assert_eq!(fs::read(other.join("d.real/g")).unwrap(), b"moved");

    let _ = fs::remove_dir_all(&base);
    let _ = fs::remove_dir_all(&other);
}

/// A destination that is a dangling symbolic link is replaced, as rename(2) replaces it on one
/// filesystem: the copy across filesystems creates the destination itself, and never writes
/// through anything found at its name (here, creating the file the link names).
#[test]
fn mv_replaces_a_dangling_symlink_destination_across_filesystems() {
    let Some(other) = other_fs("dangling_dest") else {
        return;
    };
    let base = scratch("dangling_dest");
    fs::write(base.join("f"), b"moved").unwrap();
    let target = other.join("link");
    symlink(other.join("elsewhere"), &target).unwrap();

    let out = Command::new(get_binary_path("mv"))
        .args([base.join("f"), target.clone()])
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute mv");
    let stderr = String::from_utf8_lossy(&out.stderr);

    assert!(
        !other.join("elsewhere").exists(),
        "mv wrote through a dangling destination link; stderr: {stderr}"
    );
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert!(fs::symlink_metadata(&target).unwrap().is_file());
    assert_eq!(fs::read(&target).unwrap(), b"moved");
    assert!(!base.join("f").exists());

    let _ = fs::remove_dir_all(&base);
    let _ = fs::remove_dir_all(&other);
}
