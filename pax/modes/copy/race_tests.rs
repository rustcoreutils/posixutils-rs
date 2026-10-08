//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Copy mode against a writer of the destination directory who replaces what
//! pax has just made, staged deterministically through `race_hook`.

use super::*;
use crate::modes::race_hook::{self, Point};
use plib::tmp::TempDir;
use std::os::unix::fs::PermissionsExt;

/// A file outside the destination, on the same filesystem, mode 0600 and an
/// mtime of 1,000,000,000 -- what the swapped-in hard link leads to.
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
fn preserve_everything() -> CopyOptions {
    CopyOptions {
        preserve_owner: true,
        preserve_perms: true,
        preserve_mtime: true,
        preserve_atime: true,
        ..Default::default()
    }
}

/// Copy `operand` into `dest` while a writer swaps every node pax makes for a
/// hard link to `victim`.
fn copy_with_swap(operand: &Path, dest: &Path, victim: CString, options: &CopyOptions) {
    let hook = move |point, dirfd, name: &CStr| {
        if point == Point::Made {
            race_hook::swap_for_hard_link(dirfd, name, &victim);
        }
    };
    let operand = operand.to_path_buf();
    race_hook::with_hook(hook, || {
        copy_files(&mut std::iter::once(operand), dest, options).unwrap();
    });
}

/// A FIFO of mode 4777 replaced, right after it is made, by a hard link to
/// another file: that file must keep its own mode and times.
#[test]
fn fifo_swapped_for_a_hard_link_lends_nothing_to_its_target() {
    let tmp = TempDir::new().unwrap();
    let src = tmp.path().join("src");
    let dest = tmp.path().join("dest");
    std::fs::create_dir(&src).unwrap();
    std::fs::create_dir(&dest).unwrap();
    let fifo = src.join("f");
    let fifo_c = CString::new(fifo.as_os_str().as_bytes()).unwrap();
    assert_eq!(unsafe { libc::mkfifo(fifo_c.as_ptr(), 0o600) }, 0);
    std::fs::set_permissions(&fifo, std::fs::Permissions::from_mode(0o4777)).unwrap();
    let (victim, victim_c) = victim(&tmp);

    copy_with_swap(&fifo, &dest, victim_c, &preserve_everything());

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

/// A symbolic link replaced the same way: its times must not reach the file
/// the hard link leads to.
#[test]
fn symlink_swapped_for_a_hard_link_lends_nothing_to_its_target() {
    let tmp = TempDir::new().unwrap();
    let src = tmp.path().join("src");
    let dest = tmp.path().join("dest");
    std::fs::create_dir(&src).unwrap();
    std::fs::create_dir(&dest).unwrap();
    let link = src.join("l");
    std::os::unix::fs::symlink("anywhere", &link).unwrap();
    let (victim, victim_c) = victim(&tmp);

    copy_with_swap(&link, &dest, victim_c, &preserve_everything());

    let md = std::fs::metadata(&victim).unwrap();
    assert_eq!(
        md.mtime(),
        1_000_000_000,
        "the link's times reached the victim"
    );
}

/// A source directory of mode 0777 whose new copy is replaced, right after
/// the mkdir, by a private directory (0700, with a file in it) renamed over
/// it: the private one must not be opened up.
#[test]
fn directory_swapped_after_mkdir_is_not_stamped() {
    let tmp = TempDir::new().unwrap();
    let src = tmp.path().join("src");
    let dir = src.join("d");
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::set_permissions(&dir, std::fs::Permissions::from_mode(0o777)).unwrap();
    let private = tmp.path().join("private");
    std::fs::create_dir(&private).unwrap();
    std::fs::write(private.join("secret"), "secret").unwrap();
    std::fs::set_permissions(&private, std::fs::Permissions::from_mode(0o700)).unwrap();
    let private_c = CString::new(private.as_os_str().as_bytes()).unwrap();

    // The directory the copy is made in already exists, and anyone may
    // write it, so someone other than pax could rename entries in it.
    let dest = tmp.path().join("dest");
    let parent = dest.join(member_name(&src));
    std::fs::create_dir_all(&parent).unwrap();
    std::fs::set_permissions(&parent, std::fs::Permissions::from_mode(0o777)).unwrap();

    let mut done = false;
    let hook = move |point, dirfd, name: &CStr| {
        if point == Point::MadeDir && name == c"d" && !done {
            done = true;
            race_hook::swap_for_directory(dirfd, name, &private_c);
        }
    };
    race_hook::with_hook(hook, || {
        let operand = dir.clone();
        copy_files(&mut std::iter::once(operand), &dest, &preserve_everything()).unwrap();
    });

    let copied = parent.join("d");
    assert!(copied.join("secret").exists(), "the swap did not happen");
    let md = std::fs::metadata(&copied).unwrap();
    assert_eq!(
        md.mode() & 0o7777,
        0o700,
        "the private directory was opened up"
    );
}

/// `-l -H`: a symbolic link operand retargeted, after the walk followed it,
/// at a private file. The copy must not be left a hard link to that file.
#[test]
fn link_through_a_retargeted_symlink_leaves_no_link_to_the_new_target() {
    use std::os::unix::fs::MetadataExt;
    let tmp = TempDir::new().unwrap();
    let src = tmp.path().join("src");
    let dest = tmp.path().join("dest");
    std::fs::create_dir(&src).unwrap();
    std::fs::create_dir(&dest).unwrap();
    std::fs::write(src.join("a"), "A").unwrap();
    std::fs::write(src.join("secret"), "S").unwrap();
    std::fs::set_permissions(src.join("secret"), std::fs::Permissions::from_mode(0o600)).unwrap();
    let link = src.join("s");
    std::os::unix::fs::symlink("a", &link).unwrap();
    let secret_ino = std::fs::metadata(src.join("secret")).unwrap().ino();
    let secret_path = src.join("secret");

    // An operand's name is its whole path, relative to the working directory.
    let swapped = std::rc::Rc::new(std::cell::Cell::new(false));
    let seen = std::rc::Rc::clone(&swapped);
    let hook = move |point, dirfd, name: &CStr| {
        let last = name.to_bytes().rsplit(|&b| b == b'/').next();
        if point == Point::Linking && last == Some(b"s".as_slice()) {
            seen.set(true);
            assert_eq!(unsafe { libc::unlinkat(dirfd, name.as_ptr(), 0) }, 0);
            let secret = CString::new(secret_path.as_os_str().as_bytes()).unwrap();
            let r = unsafe { libc::symlinkat(secret.as_ptr(), dirfd, name.as_ptr()) };
            assert_eq!(r, 0);
        }
    };
    let options = CopyOptions {
        link: true,
        cli_dereference: true,
        ..Default::default()
    };
    race_hook::with_hook(hook, || {
        let operand = link.clone();
        copy_files(&mut std::iter::once(operand), &dest, &options).unwrap();
    });

    assert!(swapped.get(), "the swap did not happen");
    let copied = dest.join(member_name(&link));
    if let Ok(md) = std::fs::symlink_metadata(&copied) {
        assert_ne!(
            md.ino(),
            secret_ino,
            "the copy is a hard link to the new target"
        );
    }
}

/// Without any swap the FIFO keeps its mode, set-user-ID included, and its
/// times.
#[test]
fn fifo_takes_its_attributes() {
    let tmp = TempDir::new().unwrap();
    let src = tmp.path().join("src");
    let dest = tmp.path().join("dest");
    std::fs::create_dir(&src).unwrap();
    std::fs::create_dir(&dest).unwrap();
    let fifo = src.join("f");
    let fifo_c = CString::new(fifo.as_os_str().as_bytes()).unwrap();
    assert_eq!(unsafe { libc::mkfifo(fifo_c.as_ptr(), 0o600) }, 0);
    std::fs::set_permissions(&fifo, std::fs::Permissions::from_mode(0o4751)).unwrap();
    let when = filetime::FileTime::from_unix_time(12345, 0);
    // Not `set_file_times`, which opens the FIFO and waits for a writer.
    filetime::set_symlink_file_times(&fifo, when, when).unwrap();

    let operand = fifo.clone();
    copy_files(&mut std::iter::once(operand), &dest, &preserve_everything()).unwrap();

    let md = std::fs::symlink_metadata(dest.join(member_name(&fifo))).unwrap();
    assert_eq!(md.mode() & 0o7777, 0o4751);
    assert_eq!(md.mtime(), 12345);
}
