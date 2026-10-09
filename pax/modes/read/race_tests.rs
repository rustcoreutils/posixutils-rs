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

/// Once a directory is found renamed over the one pax made, nothing more is
/// extracted into it: a later member below it is refused too.
#[test]
fn nothing_is_extracted_into_a_directory_found_in_place_of_a_made_one() {
    let tmp = TempDir::new().unwrap();
    let dest = shared_dest(&tmp);
    let (_, victim_c) = private_dir(&tmp);

    let entries = vec![
        own_member("d", EntryType::Directory, 0o777),
        own_member("d/f", EntryType::Fifo, 0o600),
    ];
    extract_with_dir_swap(&dest, entries, c"d", victim_c);

    assert!(dest.join("d/secret").exists(), "the swap did not happen");
    assert!(
        std::fs::symlink_metadata(dest.join("d/f")).is_err(),
        "a member was extracted into the directory renamed over pax's"
    );
}

/// A directory made only to hold a member below it, on a filesystem where its
/// owner could not be verified (`MadeTrust::ParentOwnerOnly`), gets no
/// attributes from the member naming it later, as one made for that member
/// would not either.
#[test]
fn unverified_intermediate_directory_is_not_stamped() {
    use crate::modes::made::MadeTrust;
    let tmp = TempDir::new().unwrap();
    let tree = DirTree::open_path(tmp.path()).unwrap();
    let mut pending = PendingDirs::default();
    let entries = vec![
        own_member("a/f", EntryType::Fifo, 0o600),
        own_member("a", EntryType::Directory, 0o751),
    ];
    let mut archive = Members(entries.into_iter());
    let options = preserve_everything();
    race_hook::with_dir_trust(MadeTrust::ParentOwnerOnly, || {
        extract_members(&mut archive, &options, &tree, &mut pending).unwrap();
    });
    pending.apply(&tree, &policy_of(&options));

    let md = std::fs::metadata(tmp.path().join("a")).unwrap();
    assert!(tmp.path().join("a/f").exists());
    assert_ne!(
        md.mode() & 0o7777,
        0o751,
        "the unverified directory was stamped"
    );
}

/// A directory member made where its owner could not be verified gets no
/// attributes -- and says so, rather than dropping them in silence.
#[test]
fn unverified_directory_member_is_diagnosed() {
    use crate::modes::made::MadeTrust;
    let tmp = TempDir::new().unwrap();
    let tree = DirTree::open_path(tmp.path()).unwrap();
    let mut pending = PendingDirs::default();
    let mut links = super::Links::new();
    let entry = own_member("d", EntryType::Directory, 0o751);
    let mut archive = Members(Vec::new().into_iter());
    let options = preserve_everything();
    let r = race_hook::with_dir_trust(MadeTrust::ParentOwnerOnly, || {
        let pending = &mut pending;
        extract_entry(&mut archive, &entry, &options, &mut links, &tree, pending)
    });

    assert!(r.is_err(), "withholding the attributes went unreported");
    assert!(tmp.path().join("d").is_dir());
    pending.apply(&tree, &policy_of(&options));
    // The member's mtime is 0: stamped, the directory would carry it.
    let md = std::fs::metadata(tmp.path().join("d")).unwrap();
    assert_ne!(md.mtime(), 0, "the unverified directory was stamped");
}

/// A directory member made where its owner could not be verified stays
/// unverified when the same name comes again -- a duplicate member, an
/// appended archive -- and gets no attributes then either.
#[test]
fn unverified_directory_named_twice_is_withheld_both_times() {
    use crate::modes::made::MadeTrust;
    let tmp = TempDir::new().unwrap();
    let tree = DirTree::open_path(tmp.path()).unwrap();
    let mut pending = PendingDirs::default();
    let mut links = super::Links::new();
    let entry = own_member("d", EntryType::Directory, 0o751);
    let mut archive = Members(Vec::new().into_iter());
    let options = preserve_everything();
    let results = race_hook::with_dir_trust(MadeTrust::ParentOwnerOnly, || {
        let mut extract = || {
            let pending = &mut pending;
            extract_entry(&mut archive, &entry, &options, &mut links, &tree, pending)
        };
        [extract().is_err(), extract().is_err()]
    });

    assert_eq!(results, [true, true], "withholding went unreported");
    pending.apply(&tree, &policy_of(&options));
    // The member's mtime is 0: stamped, the directory would carry it.
    let md = std::fs::metadata(tmp.path().join("d")).unwrap();
    assert_ne!(md.mtime(), 0, "the unverified directory was stamped");
}

/// In a shared extraction directory -- sticky, like /tmp -- both kinds of
/// directory this run makes and verifies take the archived mode. One that
/// was already there is merged into but keeps its own: anyone could have
/// created that name first.
#[test]
fn made_directories_take_their_mode_and_an_existing_one_keeps_its_own() {
    let tmp = TempDir::new().unwrap();
    let dest = shared_dest(&tmp);
    std::fs::set_permissions(&dest, std::fs::Permissions::from_mode(0o1777)).unwrap();
    std::fs::create_dir(dest.join("e")).unwrap();
    std::fs::set_permissions(dest.join("e"), std::fs::Permissions::from_mode(0o700)).unwrap();
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
    assert_eq!(mode("e"), 0o700);
    assert!(dest.join("e/kept").exists());
}

/// A terminal found where a link set's first name was made is not pinned --
/// and must not become the controlling terminal of a pax that has none in
/// the open before the identity check refuses it.
#[cfg(target_os = "linux")]
#[test]
fn pin_file_never_adopts_a_terminal() {
    if !race_hook::in_new_session() {
        race_hook::rerun_in_new_session(
            "modes::read::race_tests::pin_file_never_adopts_a_terminal",
        );
        return;
    }
    let (_master, pts, slave) = race_hook::open_pty();
    assert!(!race_hook::has_controlling_tty());
    assert!(pin_file(pts.as_fd(), &slave, (0, 0)).is_none());
    assert!(
        !race_hook::has_controlling_tty(),
        "pinning the file made it the controlling terminal"
    );
}

/// A directory this run made for one member, renamed by someone else to the
/// name of a later member, is not that member's directory: it is met as one
/// found existing, and in a shared destination keeps its own attributes
/// rather than taking the later member's.
#[test]
fn a_made_directory_renamed_to_a_later_members_name_is_found() {
    let tmp = TempDir::new().unwrap();
    let dest = shared_dest(&tmp);
    let tree = DirTree::open_path(&dest).unwrap();
    let mut pending = PendingDirs::default();
    let options = preserve_everything();

    let mut extract = |member| {
        let mut archive = Members(vec![member].into_iter());
        extract_members(&mut archive, &options, &tree, &mut pending).unwrap();
    };
    extract(own_member("a", EntryType::Directory, 0o700));
    std::fs::rename(dest.join("a"), dest.join("b")).unwrap();
    extract(own_member("b", EntryType::Directory, 0o777));
    pending.apply(&tree, &policy_of(&options));

    let mode = std::fs::metadata(dest.join("b")).unwrap().mode() & 0o7777;
    assert_eq!(mode, 0o700, "it took the later member's mode");
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

/// An archive of `members` whose data cannot be read: every regular member
/// fails after its file is created.
#[cfg(target_os = "linux")]
struct DataFails(std::vec::IntoIter<ArchiveEntry>);

#[cfg(target_os = "linux")]
impl ArchiveReader for DataFails {
    fn read_entry(&mut self) -> PaxResult<Option<ArchiveEntry>> {
        Ok(self.0.next())
    }
    fn read_data(&mut self, _buf: &mut [u8]) -> PaxResult<usize> {
        Err(PaxError::Io(std::io::Error::other("data unreadable")))
    }
    fn skip_data(&mut self) -> PaxResult<()> {
        Ok(())
    }
}

/// A tar link member `g` naming the member `target`.
fn link_member(target: &str) -> ArchiveEntry {
    let mut entry = own_member("g", EntryType::Hardlink, 0o644);
    entry.link_target = Some(PathBuf::from(target));
    entry
}

/// Extract `members` in order below `dest` with `archive`, a writer renaming
/// the file `planted` over `target` just before the link to it is made.
#[cfg(target_os = "linux")]
fn extract_linked_while_planted<R: ArchiveReader>(
    dest: &Path,
    archive: &mut R,
    target: &'static CStr,
) {
    let tree = DirTree::open_path(dest).unwrap();
    let mut pending = PendingDirs::default();
    let mut links = super::Links::new();
    let options = ReadOptions::default();
    let path = dest.to_path_buf();
    let hook = move |point, _: libc::c_int, name: &CStr| {
        if point == Point::Linking && name == target {
            std::fs::write(path.join("planted"), "planted\n").unwrap();
            let target = std::ffi::OsStr::from_bytes(target.to_bytes());
            std::fs::rename(path.join("planted"), path.join(target)).unwrap();
        }
    };
    race_hook::with_hook(hook, || {
        while let Some(entry) = archive.read_entry().unwrap() {
            let _ = extract_entry(archive, &entry, &options, &mut links, &tree, &mut pending);
        }
    });
}

/// A link member naming a symbolic link member is linked to that very link,
/// the one this run made, or not at all -- never to whatever is at its name
/// by then. Only regular files were recorded; a symbolic link was linked by
/// name, whatever the name held. (A symbolic link cannot be linked through
/// its pin, so with its name taken the member fails.)
#[cfg(target_os = "linux")]
#[test]
fn link_member_to_a_symlink_member_links_only_the_link_made() {
    let tmp = TempDir::new().unwrap();
    let mut symlink = own_member("s", EntryType::Symlink, 0o777);
    symlink.link_target = Some(PathBuf::from("made-target"));
    let mut archive = Members(vec![symlink, link_member("s")].into_iter());
    extract_linked_while_planted(tmp.path(), &mut archive, c"s");
    let g = tmp.path().join("g");
    if std::fs::symlink_metadata(&g).is_ok() {
        assert_eq!(
            std::fs::read_link(&g).ok(),
            Some(PathBuf::from("made-target")),
            "g is not the symbolic link made"
        );
    }
}

/// Without a writer in the way, the link member is linked to the symbolic
/// link made: by name, checked by identity and ctime.
#[cfg(target_os = "linux")]
#[test]
fn link_member_to_a_symlink_member_is_linked() {
    let tmp = TempDir::new().unwrap();
    let tree = DirTree::open_path(tmp.path()).unwrap();
    let mut pending = PendingDirs::default();
    let mut links = super::Links::new();
    let options = ReadOptions::default();
    let mut symlink = own_member("s", EntryType::Symlink, 0o777);
    symlink.link_target = Some(PathBuf::from("made-target"));
    let mut archive = Members(vec![symlink, link_member("s")].into_iter());
    while let Some(entry) = archive.read_entry().unwrap() {
        extract_entry(
            &mut archive,
            &entry,
            &options,
            &mut links,
            &tree,
            &mut pending,
        )
        .unwrap();
    }
    let g = std::fs::symlink_metadata(tmp.path().join("g")).unwrap();
    let s = std::fs::symlink_metadata(tmp.path().join("s")).unwrap();
    assert_eq!((g.dev(), g.ino()), (s.dev(), s.ino()));
}

/// A regular member whose data fails after its file is made still leaves
/// that file, and a link member naming it is linked to it -- not to what is
/// at its name by then.
#[cfg(target_os = "linux")]
#[test]
fn link_member_to_a_failed_member_links_the_file_made() {
    let tmp = TempDir::new().unwrap();
    let mut file = own_member("f", EntryType::Regular, 0o644);
    file.size = 10;
    let mut archive = DataFails(vec![file, link_member("f")].into_iter());
    extract_linked_while_planted(tmp.path(), &mut archive, c"f");
    let g = std::fs::read_to_string(tmp.path().join("g")).unwrap_or_default();
    assert_ne!(g, "planted\n", "g was linked to the planted file");
}

/// -i renames a member as it is extracted; a later link member still names
/// it by its name in the archive, and is linked to it at the name it was
/// given -- the file this run made -- not to whatever is at the old name.
#[test]
fn link_member_follows_a_target_renamed_by_i() {
    let tmp = TempDir::new().unwrap();
    let tree = DirTree::open_path(tmp.path()).unwrap();
    let mut pending = PendingDirs::default();
    let mut links = super::Links::new();
    let options = ReadOptions::default();
    // The member `f`, renamed to `renamed` at the prompt.
    let file = own_member("renamed", EntryType::Regular, 0o644);
    links.alias(Path::new("f"), Path::new("renamed"));
    let mut archive = Members(vec![file, link_member("f")].into_iter());
    while let Some(entry) = archive.read_entry().unwrap() {
        let _ = extract_entry(
            &mut archive,
            &entry,
            &options,
            &mut links,
            &tree,
            &mut pending,
        );
    }
    let g = std::fs::metadata(tmp.path().join("g")).ok();
    let made = std::fs::metadata(tmp.path().join("renamed")).unwrap();
    assert_eq!(
        g.map(|g| (g.dev(), g.ino())),
        Some((made.dev(), made.ino()))
    );
}

/// Under -k a link member whose name is taken leaves that file alone, and
/// its name is not the file it names: a later link member naming it is
/// linked to the file that was there, not to the earlier member's target.
#[test]
fn link_member_kept_by_k_is_not_recorded_as_its_target() {
    let tmp = TempDir::new().unwrap();
    std::fs::write(tmp.path().join("g"), "was here\n").unwrap();
    let tree = DirTree::open_path(tmp.path()).unwrap();
    let mut pending = PendingDirs::default();
    let mut links = super::Links::new();
    let options = ReadOptions {
        no_clobber: true,
        ..Default::default()
    };
    let file = own_member("f", EntryType::Regular, 0o644);
    let mut h = link_member("g");
    h.path = PathBuf::from("h");
    let mut archive = Members(vec![file, link_member("f"), h].into_iter());
    while let Some(entry) = archive.read_entry().unwrap() {
        let _ = extract_entry(
            &mut archive,
            &entry,
            &options,
            &mut links,
            &tree,
            &mut pending,
        );
    }
    let id = |name: &str| {
        let md = std::fs::metadata(tmp.path().join(name)).unwrap();
        (md.dev(), md.ino())
    };
    assert_ne!(id("g"), id("f"), "-k replaced g");
    assert_eq!(id("h"), id("g"), "h is not the file g holds");
}

/// Extract `archive` below `dest` under `options`, a writer putting the
/// file `planted` at `target` just before any link to it is made.
#[cfg(target_os = "linux")]
fn extract_with_plant_at_link<R: ArchiveReader>(
    dest: &Path,
    archive: &mut R,
    options: &ReadOptions,
    target: &'static str,
) {
    let tree = DirTree::open_path(dest).unwrap();
    let mut pending = PendingDirs::default();
    let mut links = super::Links::new();
    let path = dest.to_path_buf();
    let hook = move |point, _: libc::c_int, name: &CStr| {
        if point == Point::Linking && name.to_bytes() == target.as_bytes() {
            let at = path.join(target);
            if at.is_dir() {
                std::fs::remove_dir_all(&at).unwrap();
            }
            std::fs::write(path.join("planted"), "planted\n").unwrap();
            std::fs::rename(path.join("planted"), &at).unwrap();
        }
    };
    race_hook::with_hook(hook, || {
        while let Some(entry) = archive.read_entry().unwrap() {
            let _ = extract_entry(archive, &entry, options, &mut links, &tree, &mut pending);
        }
    });
}

/// A target member whose file could not be made -- a non-empty directory in
/// its way -- leaves no file of this run's at its name, and a link member
/// naming it fails: linked by name it took whatever was put there by then.
#[cfg(target_os = "linux")]
#[test]
fn link_member_to_a_target_that_failed_is_refused() {
    let tmp = TempDir::new().unwrap();
    std::fs::create_dir(tmp.path().join("a")).unwrap();
    std::fs::write(tmp.path().join("a/inside"), "x").unwrap();
    let file = own_member("a", EntryType::Regular, 0o644);
    let mut archive = Members(vec![file, link_member("a")].into_iter());
    extract_with_plant_at_link(tmp.path(), &mut archive, &ReadOptions::default(), "a");
    let g = std::fs::read_to_string(tmp.path().join("g")).unwrap_or_default();
    assert_ne!(g, "planted\n", "g was linked to what was put at the name");
}

/// Under -k a second member of a name already extracted is skipped, and
/// leaves the first one's file -- and its pin -- in place: a link member
/// naming it is linked to that file.
#[cfg(target_os = "linux")]
#[test]
fn link_member_after_a_k_duplicate_links_the_first_file() {
    let tmp = TempDir::new().unwrap();
    let file = own_member("a", EntryType::Regular, 0o644);
    let again = own_member("a", EntryType::Regular, 0o644);
    let mut archive = Members(vec![file, again, link_member("a")].into_iter());
    let options = ReadOptions {
        no_clobber: true,
        ..Default::default()
    };
    extract_with_plant_at_link(tmp.path(), &mut archive, &options, "a");
    let g = std::fs::read_to_string(tmp.path().join("g")).unwrap_or_default();
    assert_ne!(g, "planted\n", "g was linked to what was put at the name");
}
