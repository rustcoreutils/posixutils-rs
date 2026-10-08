//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Read mode against a writer of the extraction directory who replaces what
//! pax has just made, staged deterministically through `race_hook`.

use super::*;
use crate::modes::race_hook::{self, Point};
use plib::tmp::TempDir;
use std::os::unix::fs::{MetadataExt, PermissionsExt};

/// An archive that is just a list of members with no data.
struct Members(std::vec::IntoIter<ArchiveEntry>);

impl ArchiveReader for Members {
    fn read_entry(&mut self) -> PaxResult<Option<ArchiveEntry>> {
        Ok(self.0.next())
    }
    fn read_data(&mut self, _buf: &mut [u8]) -> PaxResult<usize> {
        Ok(0)
    }
    fn skip_data(&mut self) -> PaxResult<()> {
        Ok(())
    }
}

/// A file outside the extraction directory, on the same filesystem, mode
/// 0600 and an mtime of 1,000,000,000 -- what the swapped-in hard link leads
/// to.
fn victim(tmp: &TempDir) -> (PathBuf, CString) {
    let path = tmp.path().join("victim");
    std::fs::write(&path, "private").unwrap();
    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600)).unwrap();
    let when = filetime::FileTime::from_unix_time(1_000_000_000, 0);
    filetime::set_file_times(&path, when, when).unwrap();
    let c = CString::new(path.as_os_str().as_bytes()).unwrap();
    (path, c)
}

/// `-p e`, as the demonstrated attack used.
fn preserve_everything() -> ReadOptions {
    ReadOptions {
        preserve_owner: true,
        preserve_perms: true,
        preserve_mtime: true,
        preserve_atime: true,
        ..Default::default()
    }
}

/// Extract `entry` below `dest` while a writer swaps the node pax makes for a
/// hard link to `victim`.
fn extract_with_swap(dest: &Path, entry: ArchiveEntry, victim: CString, options: &ReadOptions) {
    let tree = DirTree::open_path(dest).unwrap();
    let mut pending = PendingDirs::default();
    let mut archive = Members(vec![entry].into_iter());
    let hook = move |point, dirfd, name: &CStr| {
        if point == Point::Made {
            race_hook::swap_for_hard_link(dirfd, name, &victim);
        }
    };
    let _ = race_hook::with_hook(hook, || {
        extract_members(&mut archive, options, &tree, &mut pending)
    });
    pending.apply(&tree, &policy_of(options));
}

/// The member's owner is the caller's own, so the chown succeeds and the
/// set-user-ID bit would be kept.
fn own_member(path: &str, entry_type: EntryType, mode: u32) -> ArchiveEntry {
    ArchiveEntry {
        path: PathBuf::from(path),
        mode,
        uid: unsafe { libc::getuid() },
        gid: unsafe { libc::getgid() },
        mtime: 0,
        entry_type,
        ..Default::default()
    }
}

/// A FIFO archived 4777 and replaced, right after it is made, by a hard link
/// to another file: that file must keep its own mode and times.
#[test]
fn fifo_swapped_for_a_hard_link_lends_nothing_to_its_target() {
    let tmp = TempDir::new().unwrap();
    let dest = tmp.path().join("dest");
    std::fs::create_dir(&dest).unwrap();
    let (victim, victim_c) = victim(&tmp);

    let entry = own_member("f", EntryType::Fifo, 0o4777);
    extract_with_swap(&dest, entry, victim_c, &preserve_everything());

    let md = std::fs::metadata(&victim).unwrap();
    assert_eq!(
        md.mode() & 0o7777,
        0o600,
        "the FIFO's mode reached the victim"
    );
    assert_eq!(
        md.mtime(),
        1_000_000_000,
        "the FIFO's times reached the victim"
    );
}

/// A symbolic link replaced the same way: its owner and times must not
/// reach the file the hard link leads to.
#[test]
fn symlink_swapped_for_a_hard_link_lends_nothing_to_its_target() {
    let tmp = TempDir::new().unwrap();
    let dest = tmp.path().join("dest");
    std::fs::create_dir(&dest).unwrap();
    let (victim, victim_c) = victim(&tmp);

    let mut entry = own_member("l", EntryType::Symlink, 0o777);
    entry.link_target = Some(PathBuf::from("anywhere"));
    extract_with_swap(&dest, entry, victim_c, &preserve_everything());

    let md = std::fs::metadata(&victim).unwrap();
    assert_eq!(
        md.mtime(),
        1_000_000_000,
        "the link's times reached the victim"
    );
    assert_eq!(md.mode() & 0o7777, 0o600);
}

/// A private directory (0700, with a file in it) outside the extraction
/// directory: what a writer renames over the directory pax has just made.
fn private_dir(tmp: &TempDir) -> (PathBuf, CString) {
    let path = tmp.path().join("private");
    std::fs::create_dir(&path).unwrap();
    std::fs::write(path.join("secret"), "secret").unwrap();
    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o700)).unwrap();
    let c = CString::new(path.as_os_str().as_bytes()).unwrap();
    (path, c)
}

/// An extraction directory anyone may write, so that someone other than pax
/// could rename entries in it.
fn shared_dest(tmp: &TempDir) -> PathBuf {
    let dest = tmp.path().join("dest");
    std::fs::create_dir(&dest).unwrap();
    std::fs::set_permissions(&dest, std::fs::Permissions::from_mode(0o777)).unwrap();
    dest
}

/// Extract `entries` below `dest` while a writer renames `victim` over the
/// directory pax makes at `swapped`, once.
fn extract_with_dir_swap(dest: &Path, entries: Vec<ArchiveEntry>, swapped: &CStr, victim: CString) {
    let tree = DirTree::open_path(dest).unwrap();
    let mut pending = PendingDirs::default();
    let mut archive = Members(entries.into_iter());
    let swapped = swapped.to_owned();
    let mut done = false;
    let hook = move |point, dirfd, name: &CStr| {
        if point == Point::MadeDir && name == swapped.as_c_str() && !done {
            done = true;
            race_hook::swap_for_directory(dirfd, name, &victim);
        }
    };
    let options = preserve_everything();
    let _ = race_hook::with_hook(hook, || {
        extract_members(&mut archive, &options, &tree, &mut pending)
    });
    pending.apply(&tree, &policy_of(&options));
}

/// A directory member archived 0777, its new directory replaced by a private
/// one right after the mkdir: the private one must not be opened up.
#[test]
fn directory_swapped_after_mkdir_is_not_stamped() {
    let tmp = TempDir::new().unwrap();
    let dest = shared_dest(&tmp);
    let (_, victim_c) = private_dir(&tmp);

    let entry = own_member("d", EntryType::Directory, 0o777);
    extract_with_dir_swap(&dest, vec![entry], c"d", victim_c);

    let md = std::fs::metadata(dest.join("d")).unwrap();
    assert!(dest.join("d/secret").exists(), "the swap did not happen");
    assert_eq!(
        md.mode() & 0o7777,
        0o700,
        "the private directory was opened up"
    );
}

/// The same for a directory made only to hold a member below it, and named by
/// a later member (`find -depth` order).
#[test]
fn intermediate_directory_swapped_after_mkdir_is_not_stamped() {
    let tmp = TempDir::new().unwrap();
    let dest = shared_dest(&tmp);
    let (_, victim_c) = private_dir(&tmp);

    let entries = vec![
        own_member("a/f", EntryType::Fifo, 0o600),
        own_member("a", EntryType::Directory, 0o777),
    ];
    extract_with_dir_swap(&dest, entries, c"a", victim_c);

    let md = std::fs::metadata(dest.join("a")).unwrap();
    assert!(dest.join("a/secret").exists(), "the swap did not happen");
    assert_eq!(
        md.mode() & 0o7777,
        0o700,
        "the private directory was opened up"
    );
}

/// Without a swap, both kinds of directory take the archived mode, in a
/// shared extraction directory as anywhere else; an existing directory is
/// merged into and takes it too.
#[test]
fn made_and_existing_directories_take_their_mode() {
    let tmp = TempDir::new().unwrap();
    let dest = shared_dest(&tmp);
    std::fs::create_dir(dest.join("e")).unwrap();
    std::fs::write(dest.join("e/kept"), "").unwrap();
    let tree = DirTree::open_path(&dest).unwrap();
    let mut pending = PendingDirs::default();
    let entries = vec![
        own_member("d", EntryType::Directory, 0o751),
        own_member("a/f", EntryType::Fifo, 0o600),
        own_member("a", EntryType::Directory, 0o753),
        own_member("e", EntryType::Directory, 0o705),
    ];
    let mut archive = Members(entries.into_iter());
    let options = preserve_everything();
    extract_members(&mut archive, &options, &tree, &mut pending).unwrap();
    pending.apply(&tree, &policy_of(&options));

    let mode = |p: &str| std::fs::metadata(dest.join(p)).unwrap().mode() & 0o7777;
    assert_eq!(mode("d"), 0o751);
    assert_eq!(mode("a"), 0o753);
    assert_eq!(mode("e"), 0o705);
    assert!(dest.join("e/kept").exists());
}

/// Without any swap the FIFO and the link still get everything asked for.
#[test]
fn fifo_and_symlink_take_their_attributes() {
    let tmp = TempDir::new().unwrap();
    let tree = DirTree::open_path(tmp.path()).unwrap();
    let mut pending = PendingDirs::default();
    let mut fifo = own_member("f", EntryType::Fifo, 0o4751);
    fifo.mtime = 12345;
    let mut link = own_member("l", EntryType::Symlink, 0o777);
    link.link_target = Some(PathBuf::from("f"));
    link.mtime = 23456;
    let mut archive = Members(vec![fifo, link].into_iter());
    extract_members(&mut archive, &preserve_everything(), &tree, &mut pending).unwrap();

    let md = std::fs::symlink_metadata(tmp.path().join("f")).unwrap();
    assert_eq!(md.mode() & 0o7777, 0o4751);
    assert_eq!(md.mtime(), 12345);
    let md = std::fs::symlink_metadata(tmp.path().join("l")).unwrap();
    assert!(md.file_type().is_symlink());
    assert_eq!(md.mtime(), 23456);
}

/// Without `-p p` the FIFO's mode is the archived one less the umask; with
/// it, the archived one exactly.
#[test]
fn fifo_mode_follows_the_umask_unless_preserved() {
    let tmp = TempDir::new().unwrap();
    let tree = DirTree::open_path(tmp.path()).unwrap();
    let mut pending = PendingDirs::default();
    let options = ReadOptions {
        preserve_perms: false,
        umask: 0o022,
        ..Default::default()
    };
    let mut archive = Members(vec![own_member("f", EntryType::Fifo, 0o777)].into_iter());
    extract_members(&mut archive, &options, &tree, &mut pending).unwrap();
    let md = std::fs::symlink_metadata(tmp.path().join("f")).unwrap();
    assert_eq!(md.mode() & 0o7777, 0o755);

    let options = ReadOptions {
        preserve_perms: true,
        umask: 0o077,
        ..Default::default()
    };
    let mut archive = Members(vec![own_member("g", EntryType::Fifo, 0o777)].into_iter());
    extract_members(&mut archive, &options, &tree, &mut pending).unwrap();
    let md = std::fs::symlink_metadata(tmp.path().join("g")).unwrap();
    assert_eq!(md.mode() & 0o7777, 0o777);
}
