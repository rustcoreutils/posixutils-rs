//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Telling an object a utility has just made from one swapped in for it, and
//! acting on the made one only.
//!
//! `mkdirat`, `mkfifoat`, `mknodat` and `symlinkat` return no descriptor, so a
//! utility that goes on to give the new object an owner, mode or times must
//! first reopen it -- and anyone else who can rename entries in the parent
//! directory can have put something else at the name by then. cp and pax both
//! pin the object, check what they pinned with `made_by_us`, and change it
//! only through the pin (`chmod_pinned`, `self/fd/N` under `procfs_dir`).

#[cfg(target_os = "linux")]
use gettextrs::gettext;
use std::cell::OnceCell;
use std::collections::HashMap;
use std::ffi::{CStr, CString, OsStr};
#[cfg(target_os = "linux")]
use std::fs::File;
use std::io;
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd, RawFd};
use std::os::unix::ffi::OsStrExt;
use std::path::{Component, Path};
use std::rc::{Rc, Weak};
use std::sync::Mutex;

/// Open flags for a directory that is only ever walked through, used as the `dirfd` of an `*at`
/// call, or `fstat`ed (`ChainTrust`).
///
/// Reaching a name below a directory takes search permission only, so opening it for reading
/// refuses a directory the user may search and write but not list (mode 0300) where `mkdir` or
/// `open` by name would have succeeded. `O_PATH` (Linux) and `O_SEARCH` (macOS, FreeBSD) open
/// it for exactly that. Elsewhere `O_RDONLY` is the only option there is -- on NetBSD too,
/// whose `O_SEARCH` is defined but not known here to open a directory for search alone.
#[cfg(target_os = "linux")]
pub const SEARCH_ONLY: libc::c_int = libc::O_PATH;
#[cfg(any(target_os = "macos", target_os = "freebsd"))]
pub const SEARCH_ONLY: libc::c_int = libc::O_SEARCH;
#[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "freebsd")))]
pub const SEARCH_ONLY: libc::c_int = libc::O_RDONLY;

/// How a filesystem keeps file owners, as far as its type says.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FsOwners {
    /// Each object's owner is stored and reported as it is (ext4, xfs, btrfs, tmpfs, ...), and
    /// the answer wherever the type cannot be read.
    Stored,
    /// Owners may be mapped -- reported from a mount option, an id map or a squash rule rather
    /// than from who made the object -- or may be stored for real, depending on the mount:
    /// NFS (root_squash or not), FUSE (sshfs with or without idmap, mergerfs, ceph-fuse, ...),
    /// cifs/smb (`uid=` or unix extensions), ntfs3 (mount options, or the per-file WSL owner
    /// and mode it stores).
    MayBeMapped,
    /// No owner is stored at all; every object reports the mount's owner: msdos/vfat, exfat and
    /// the classic ntfs driver.
    None,
}

/// How the filesystem holding `fd` keeps owners, from its `fstatfs` type.
#[cfg(target_os = "linux")]
pub fn fs_owners(fd: RawFd) -> FsOwners {
    const OWNERLESS_FS: [u64; 3] = [
        0x4d44,      // MSDOS_SUPER_MAGIC (msdos, vfat)
        0x2011_bab0, // EXFAT_SUPER_MAGIC
        0x5346_544e, // NTFS_SB_MAGIC
    ];
    const MAYBE_MAPPED_FS: [u64; 6] = [
        0x7366_746e, // ntfs3: stores a WSL owner and mode per file ($LXUID, $LXGID, $LXMOD)
        0xff53_4d42, // CIFS_SUPER_MAGIC
        0xfe53_4d42, // SMB2_SUPER_MAGIC
        0x517b,      // SMB_SUPER_MAGIC
        0x6969,      // NFS_SUPER_MAGIC
        0x6573_5546, // FUSE_SUPER_MAGIC (fuse and fuseblk)
    ];
    let mut st = std::mem::MaybeUninit::<libc::statfs>::uninit();
    if unsafe { libc::fstatfs(fd, st.as_mut_ptr()) } != 0 {
        return FsOwners::Stored;
    }
    let st = unsafe { st.assume_init() };
    // `f_type` is a signed word whose width varies by architecture; the magic numbers are its
    // low 32 bits.
    let f_type = u64::from(st.f_type as u32);
    if OWNERLESS_FS.contains(&f_type) {
        FsOwners::None
    } else if MAYBE_MAPPED_FS.contains(&f_type) {
        FsOwners::MayBeMapped
    } else {
        FsOwners::Stored
    }
}

/// Without the filesystem type, owners are taken to be stored: only the caller's own are
/// trusted.
#[cfg(not(target_os = "linux"))]
pub fn fs_owners(_fd: RawFd) -> FsOwners {
    FsOwners::Stored
}

/// How far a utility trusts an object `made_by_us` accepted.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MadeTrust {
    /// Owned by the effective user, or on a filesystem that stores no owners: owner and mode
    /// may be applied in full.
    Full,
    /// Accepted only because it is owned like its parent, on a filesystem that may store owners
    /// for real: used, but given no owner and no mode.
    ParentOwnerOnly,
}

/// What `fstat` (on a descriptor the caller holds for it) reports about an object the caller
/// has just made.
#[derive(Clone, Copy, Debug)]
pub struct MadeObject {
    pub uid: u32,
    pub nlink: u64,
    pub is_dir: bool,
    /// How the object's filesystem keeps owners (`fs_owners`).
    pub owners: FsOwners,
}

/// Whether `made`, found where the caller (running as `euid`) has just made an object in a
/// directory owned by `parent_uid` (`None` when it holds no descriptor for that directory), can
/// be the object it made rather than one swapped in by someone else -- and if so, how far it is
/// trusted.
///
/// Owned by the effective user: accepted, in full. Otherwise only in a parent that user does
/// not own, and only when the object is owned like that parent, on a filesystem whose type says
/// owners may not be what each creator was:
/// - msdos/vfat, exfat and the classic ntfs driver store no owner at all, so every object
///   reports the mount's owner and nothing about ownership can be learned or conferred:
///   accepted in full.
/// - NFS, FUSE, cifs/smb and ntfs3 may map owners (root_squash, sshfs without idmap, `uid=`)
///   -- or may store them for real. In the second case someone who can write the parent but
///   does not own it can rename in an object of the parent owner's, which cannot be told from
///   the caller's own. Such an object is accepted, so that work on these filesystems is
///   possible, but only as `ParentOwnerOnly`: it is given no owner and no mode.
///
/// On every other filesystem only the effective user is accepted: no one can give a file away,
/// a directory cannot be hard-linked, and a hard link to one of the user's nodes fails the link
/// count. The parent's owner, who could plant an object of their own anywhere there, controls
/// every entry of that directory already.
///
/// Link count: a fresh symbolic link or special file has exactly one; a fresh directory has
/// two (itself and its `.`), or one on filesystems that do not count directory links (btrfs,
/// some FUSE).
pub fn made_by_us(made: MadeObject, parent_uid: Option<u32>, euid: u32) -> Option<MadeTrust> {
    let nlink_ok = if made.is_dir {
        made.nlink <= 2
    } else {
        made.nlink == 1
    };
    if !nlink_ok {
        return None;
    }
    if made.uid == euid {
        return Some(MadeTrust::Full);
    }
    let owned_like_parent =
        parent_uid.is_some_and(|parent_uid| parent_uid != euid && made.uid == parent_uid);
    match (made.owners, owned_like_parent) {
        (FsOwners::None, true) => Some(MadeTrust::Full),
        (FsOwners::MayBeMapped, true) => Some(MadeTrust::ParentOwnerOnly),
        _ => None,
    }
}

/// Whether anyone but `euid` can rename entries in the directory `parent`: its owner, when that
/// is someone else, and anyone with group or other write permission on it when it is not
/// sticky.
///
/// Only what `st_mode` shows is seen. Write permission a POSIX ACL grants to named users or
/// groups shows there (in the group bits, the ACL mask); write permission a macOS or NFSv4 ACL
/// grants does not, and is not taken into account. That is a residual: in a directory such an
/// ACL lets others write, a directory just made is not checked for having been renamed over.
pub fn others_can_rename(parent: &libc::stat, euid: u32) -> bool {
    // Cast needed: `mode_t` is u16 on macOS and u32 on Linux. S_ISVTX is 0o1000,
    // S_IWGRP|S_IWOTH 0o022 (fixed by POSIX).
    #[allow(clippy::unnecessary_cast)]
    let mode = parent.st_mode as u32;
    parent.st_uid != euid || (mode & 0o022 != 0 && mode & 0o1000 == 0)
}

/// Which of a source's attributes the user asked to have preserved on a directory: its mode
/// (pax `-p p`, tar `-p`, cp `-p`) and its owner (pax `-p o`, tar `--same-owner`, cp `-p`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Preserve {
    pub mode: bool,
    pub owner: bool,
}

/// What a directory found already existing -- not one the caller made and verified -- is to
/// be given (`ChainTrust::found_dir`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FoundDir {
    /// Neither mode nor owner was asked for: its times, as by default, and nothing else.
    TimesOnly,
    /// What was asked for -- mode, owner, or both -- and its times.
    AsRequested,
    /// Nothing at all: someone else could have created its name, and the caller reports that.
    LeaveAlone,
}

/// The trust a chain of directories, walked down from the anchor the user named, hands to the
/// directories found existing in its last one: whether they may take a source's mode or owner.
///
/// An existing directory takes a source's mode or owner only when that was asked for, as
/// libarchive does ("we don't change perms on existing dirs unless _EXTRACT_PERM is
/// specified"); otherwise only its times (`found_dir`). And even then only where nobody but the
/// effective user could have created its name -- in its parent, and in every directory above
/// it, up to the anchor: anyone who can create entries anywhere on the way can create a name
/// there before the caller does -- renaming to it a directory of their choosing, the user's own
/// private one included, or a directory holding one -- and the sticky bit does not stop that.
/// Who owns the directory found proves nothing. Giving such a directory a mode would open it
/// up; giving it an owner would give it away.
///
/// So the trust is carried down the chain, one directory at a time, from descriptors: the
/// anchor hands it to its entries when nobody else can create entries in it (`anchor`); a
/// directory found existing hands it on when it was handed it and nobody else can create
/// entries in it either (`found`); a directory the caller made and verified is safe itself, and
/// hands it on when nobody else can create entries in it (`made`).
///
/// It is worked out only when asked for: when a directory found existing is to be given a mode
/// or owner (`found_dir`), and then only as far up the chain as it has not been yet, from the
/// top down, stopping at the first directory others can create entries in. Each link is made
/// with the `fstat` of the directory's held descriptor, taken then -- what the eager check
/// took, at the moment the caller relies on the directory -- and the costly part, whether a
/// group-writable directory's group is the user's private one (`is_private_group`, and its ACL),
/// waits for the question. That part reads the ACL through the same held descriptor when it is
/// asked; only the directory's owner (the user, or it would not have got that far) or root can
/// change it in between, so reading it later is reading it then. A link holds the descriptor
/// weakly: one its holder has closed by then counts as one others may write -- which, in pax,
/// is a group-writable directory deeper than the levels it keeps open.
#[derive(Clone)]
pub struct ChainTrust(Rc<Link>);

/// A directory the user named by a path, as the anchor of a chain (`ChainTrust::named`).
pub struct NamedAnchor {
    /// The trust it hands its entries.
    pub hands: ChainTrust,
    /// Where its name's last component is a symbolic link, the trust of the directory holding
    /// the link, which the named directory is found in; `None` where the user named the
    /// directory itself.
    through_link: Option<ChainTrust>,
    /// The directories holding the links followed, held while either trust may be asked.
    _holders: Vec<Rc<OwnedFd>>,
}

/// The most symbolic links one named path may follow: Linux's own limit before ELOOP.
const MAX_LINKS: usize = 40;

/// One step of resolving a path (`link_holders`).
enum Step {
    /// Back to `/` (an absolute path, or link target).
    Root,
    /// `..`: back to the directory the walk came from.
    Up,
    /// An entry of the directory the walk is in.
    Name(CString),
}

/// The steps of `path`, last first, to be popped.
fn steps_of(path: &Path) -> Option<Vec<Step>> {
    let mut steps = Vec::new();
    for component in path.components() {
        steps.push(match component {
            Component::RootDir => Step::Root,
            Component::ParentDir => Step::Up,
            Component::Normal(name) => Step::Name(CString::new(name.as_bytes()).ok()?),
            Component::CurDir | Component::Prefix(_) => continue,
        });
    }
    steps.reverse();
    Some(steps)
}

/// Resolve `path` as the kernel would, but one component at a time from held descriptors --
/// each directory opened `O_NOFOLLOW` in the one before it, so the walk holds every directory
/// it passes through and `..` goes back to the one it came from -- and say which directories
/// held the symbolic links it followed, in order: a link's target is resolved the same way,
/// from the directory holding it. `None` when `path` cannot be resolved so (a missing
/// component, more than `MAX_LINKS` links, an unreadable link), or does not reach the directory
/// open on `dir`.
fn link_holders(path: &Path, dir: RawFd) -> Option<Vec<Rc<OwnedFd>>> {
    let flags = SEARCH_ONLY | libc::O_DIRECTORY | libc::O_CLOEXEC;
    let open = |at: RawFd, name: &CStr, extra: libc::c_int| {
        let fd = unsafe { libc::openat(at, name.as_ptr(), flags | extra) };
        (fd >= 0).then(|| Rc::new(unsafe { OwnedFd::from_raw_fd(fd) }))
    };
    let mut steps = steps_of(path)?;
    let mut walked = vec![open(libc::AT_FDCWD, c".", 0)?];
    let mut holders = Vec::new();
    while let Some(step) = steps.pop() {
        let here = walked.last()?.as_raw_fd();
        match step {
            Step::Root => walked = vec![open(libc::AT_FDCWD, c"/", 0)?],
            Step::Up if walked.len() > 1 => {
                walked.pop();
            }
            Step::Up => walked = vec![open(here, c"..", 0)?],
            Step::Name(name) => {
                let st = lstat_at(here, &name).ok()?;
                if st.st_mode & libc::S_IFMT != libc::S_IFLNK {
                    walked.push(open(here, &name, libc::O_NOFOLLOW)?);
                    continue;
                }
                if holders.len() == MAX_LINKS {
                    return None;
                }
                holders.push(Rc::clone(walked.last()?));
                let target = read_link_at(here, &name)?;
                // Popped first: the target's steps, then the rest of the path.
                steps.extend(steps_of(Path::new(OsStr::from_bytes(&target)))?);
            }
        }
    }
    let reached = fstat(walked.last()?.as_raw_fd()).ok()?;
    let opened = fstat(dir).ok()?;
    ((reached.st_dev, reached.st_ino) == (opened.st_dev, opened.st_ino)).then_some(holders)
}

/// The target of the symbolic link `name` in `dirfd`; `None` when it cannot be read whole.
fn read_link_at(dirfd: RawFd, name: &CStr) -> Option<Vec<u8>> {
    let mut buf = vec![0u8; libc::PATH_MAX as usize];
    let n = unsafe { libc::readlinkat(dirfd, name.as_ptr(), buf.as_mut_ptr().cast(), buf.len()) };
    let n = usize::try_from(n).ok()?;
    if n >= buf.len() {
        return None;
    }
    buf.truncate(n);
    Some(buf)
}

impl NamedAnchor {
    /// What the named directory itself may be given, existing, `requested` being which of
    /// mode and owner the user asked to preserve: what was asked, as the user named it; or,
    /// reached through a link, what a directory found in the link's directory may be.
    pub fn named_dir(&self, requested: Preserve) -> FoundDir {
        match &self.through_link {
            Some(trust) => trust.found_dir(requested),
            None if requested.mode || requested.owner => FoundDir::AsRequested,
            None => FoundDir::TimesOnly,
        }
    }
}

/// One directory of a `ChainTrust`.
struct Link {
    /// What the directory this one is in hands it; `None` where a chain starts (`anchor`,
    /// `made`, `unlocated`).
    above: Option<ChainTrust>,
    /// Who else can create entries in it, from its `fstat`.
    writers: DirWriters,
    /// The directory, for its ACL; `None` for `unlocated`.
    dir: Option<Weak<dyn AsRawFd>>,
    /// Whether directories found existing in this one may take what was asked for, once
    /// worked out.
    entries_safe: OnceCell<bool>,
}

impl Drop for Link {
    /// Unlinks the chain above one link at a time, so that dropping a deep one does not recurse
    /// as deep.
    fn drop(&mut self) {
        let mut above = self.above.take();
        while let Some(ChainTrust(link)) = above {
            above = match Rc::try_unwrap(link) {
                Ok(mut link) => link.above.take(),
                Err(_) => None,
            };
        }
    }
}

impl std::fmt::Debug for ChainTrust {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ChainTrust")
            .field("writers", &self.0.writers)
            .field("entries_safe", &self.0.entries_safe.get())
            .finish_non_exhaustive()
    }
}

impl ChainTrust {
    /// The link for the directory held as `dir`, below `above`.
    fn link<T: AsRawFd + 'static>(above: Option<ChainTrust>, dir: &Rc<T>) -> io::Result<Self> {
        let euid = unsafe { libc::geteuid() };
        let writers = dir_writers(&fstat(dir.as_raw_fd())?, euid);
        let dir: Weak<T> = Rc::downgrade(dir);
        let dir: Weak<dyn AsRawFd> = dir;
        Ok(ChainTrust(Rc::new(Link {
            above,
            writers,
            dir: Some(dir),
            entries_safe: OnceCell::new(),
        })))
    }

    /// The trust the anchor -- the directory the user named, held as `dir` -- hands its
    /// entries.
    pub fn anchor<T: AsRawFd + 'static>(dir: &Rc<T>) -> io::Result<Self> {
        Self::link(None, dir)
    }

    /// The trust handed where the caller cannot tell which directory a directory found is in --
    /// reached through a symbolic link, say, and so in none it can judge: none.
    pub fn unlocated() -> Self {
        ChainTrust(Rc::new(Link {
            above: None,
            writers: DirWriters::Others,
            dir: None,
            entries_safe: OnceCell::from(false),
        }))
    }

    /// The trust a directory found existing, held as `dir`, in a directory that handed it
    /// `self`, hands its own entries.
    pub fn found<T: AsRawFd + 'static>(&self, dir: &Rc<T>) -> io::Result<Self> {
        Self::link(Some(self.clone()), dir)
    }

    /// The trust a directory the caller made and verified (`verify_made_dir`), held as `dir`,
    /// hands its entries, wherever it is.
    pub fn made<T: AsRawFd + 'static>(dir: &Rc<T>) -> io::Result<Self> {
        Self::link(None, dir)
    }

    /// The anchor for a directory the user named as `path`, opened (following links, as named)
    /// and held as `dir`.
    ///
    /// The user named the directory, and where `path` reaches it through no symbolic link it is
    /// trusted as `anchor`. A link, anywhere on the way, leads wherever its owner chose: the
    /// directory is then as if found in the directories holding the links followed, and may
    /// itself be given only what a directory found in all of them may
    /// (`NamedAnchor::named_dir`), handing its entries no more than that and its own (`found`).
    /// A link planted in a directory others can write leads nowhere the user vouched for.
    ///
    /// `path` is resolved here one component at a time from held descriptors
    /// (`link_holders`), and must reach the very directory opened; anything that cannot be
    /// resolved so, or is not, trusts nothing.
    pub fn named<T: AsRawFd + 'static>(path: &Path, dir: &Rc<T>) -> io::Result<NamedAnchor> {
        let Some(holders) = link_holders(path, dir.as_raw_fd()) else {
            return Ok(NamedAnchor {
                hands: Self::unlocated(),
                through_link: Some(Self::unlocated()),
                _holders: Vec::new(),
            });
        };
        let Some((first, rest)) = holders.split_first() else {
            return Ok(NamedAnchor {
                hands: Self::anchor(dir)?,
                through_link: None,
                _holders: holders,
            });
        };
        let mut in_holders = Self::anchor(first)?;
        for holder in rest {
            in_holders = in_holders.found(holder)?;
        }
        Ok(NamedAnchor {
            hands: in_holders.found(dir)?,
            through_link: Some(in_holders),
            _holders: holders,
        })
    }

    /// What a directory found existing in a directory of this trust may be given, `requested`
    /// being which of mode and owner the user asked to preserve. Only when one of them was
    /// asked for is the trust worked out.
    pub fn found_dir(&self, requested: Preserve) -> FoundDir {
        if !requested.mode && !requested.owner {
            FoundDir::TimesOnly
        } else if self.entries_safe() {
            FoundDir::AsRequested
        } else {
            FoundDir::LeaveAlone
        }
    }

    /// Whether directories found existing in this one may take what was asked for: the links
    /// not yet worked out, from the top down, each safe only below a safe one.
    fn entries_safe(&self) -> bool {
        let mut unknown = Vec::new();
        let mut known = true;
        let mut link = Some(self);
        while let Some(ChainTrust(this)) = link {
            if let Some(&safe) = this.entries_safe.get() {
                known = safe;
                break;
            }
            unknown.push(this);
            link = this.above.as_ref();
        }
        for this in unknown.into_iter().rev() {
            known = known && this.own_entries_safe();
            let _ = this.entries_safe.set(known);
        }
        known
    }
}

impl Link {
    /// Whether nobody but the user can create entries in this directory itself.
    fn own_entries_safe(&self) -> bool {
        match self.writers {
            DirWriters::User => true,
            DirWriters::Others => false,
            DirWriters::UserAndGroup { gid, euid } => {
                is_private_group(gid, euid)
                    && self
                        .dir
                        .as_ref()
                        .and_then(Weak::upgrade)
                        .is_some_and(|dir| !acl_may_name_others(dir.as_raw_fd()))
            }
        }
    }
}

/// Who can create entries in a directory, as far as its `fstat` shows.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum DirWriters {
    /// The effective user alone: it is theirs and grants no group or other write permission.
    User,
    /// Someone else too: it is someone else's, or grants other write permission.
    Others,
    /// The user, and members of its group `gid`: it is the user's (`euid`), grants group write
    /// permission and no other. Nobody else when that group is the user's private one and no
    /// ACL names anyone else (`nobody_else_can_create`).
    UserAndGroup { gid: u32, euid: u32 },
}

/// `DirWriters` for the directory `st`, the effective user being `euid`.
fn dir_writers(st: &libc::stat, euid: u32) -> DirWriters {
    // Cast needed: `mode_t` is u16 on macOS and u32 on Linux. S_IWGRP is 0o020 and S_IWOTH
    // 0o002 (fixed by POSIX).
    #[allow(clippy::unnecessary_cast)]
    let mode = st.st_mode as u32;
    if st.st_uid != euid || mode & 0o002 != 0 {
        DirWriters::Others
    } else if mode & 0o020 == 0 {
        DirWriters::User
    } else {
        DirWriters::UserAndGroup {
            gid: st.st_gid,
            euid,
        }
    }
}

/// Whether nobody but the effective user can create entries in the directory open on `fd`: it
/// is owned by that user and grants no other write permission, and no group write permission
/// either unless its group is the user's private group (`is_private_group`) and no ACL names
/// anyone else (`acl_may_name_others`) -- the user's alone, so the directories a umask of 002
/// leaves group-writable, as Debian-style user private groups intend, count as the user's. A
/// sticky directory others may write counts as one they can create entries in.
///
/// Only what `st_mode` shows is seen otherwise. Write permission a POSIX ACL grants to named
/// users or groups shows there (in the group bits, the ACL mask), and so counts as others'
/// unless the ACL is read and names nobody; write permission a macOS or NFSv4 ACL grants does
/// not show, and is not taken into account. That is a residual: below a directory such an ACL
/// lets others write, a directory found existing -- possibly one of theirs renamed there -- is
/// given the mode or owner asked for.
pub fn nobody_else_can_create(fd: RawFd) -> io::Result<bool> {
    let euid = unsafe { libc::geteuid() };
    Ok(match dir_writers(&fstat(fd)?, euid) {
        DirWriters::User => true,
        DirWriters::Others => false,
        DirWriters::UserAndGroup { gid, euid } => {
            is_private_group(gid, euid) && !acl_may_name_others(fd)
        }
    })
}

/// Whether the access ACL of the directory open on `fd` may grant anyone but its owner and its
/// owning group: it has a named user or named group entry, or it cannot be read for certain.
/// No ACL, or a filesystem without ACLs, grants nobody else.
///
/// Read as the `system.posix_acl_access` attribute. An `O_PATH` descriptor takes no `fgetxattr`
/// (EBADF); the directory is then reopened for reading through `self/fd/N` under a `/proc`
/// verified to be procfs (`procfs_dir`), which names the same inode -- and one the user may
/// not read counts as one that may.
#[cfg(target_os = "linux")]
fn acl_may_name_others(fd: RawFd) -> bool {
    const ACCESS_ACL: &CStr = c"system.posix_acl_access";
    let read = |fd: RawFd, buf: &mut [u8]| {
        let n =
            unsafe { libc::fgetxattr(fd, ACCESS_ACL.as_ptr(), buf.as_mut_ptr().cast(), buf.len()) };
        usize::try_from(n).map_err(|_| io::Error::last_os_error())
    };
    let mut buf = [0u8; 1024];
    let mut result = read(fd, &mut buf);
    if result
        .as_ref()
        .is_err_and(|e| e.raw_os_error() == Some(libc::EBADF))
    {
        let reopened = procfs_dir().and_then(|proc_dir| {
            let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC;
            let name = proc_fd_name(fd);
            let fd = unsafe { libc::openat(proc_dir.as_raw_fd(), name.as_ptr(), flags) };
            if fd < 0 {
                return Err(io::Error::last_os_error());
            }
            Ok(unsafe { File::from_raw_fd(fd) })
        });
        result = reopened.and_then(|dir| read(dir.as_raw_fd(), &mut buf));
    }
    match result {
        Ok(len) => acl_names_others(&buf[..len]),
        Err(e) => !matches!(
            e.raw_os_error(),
            Some(libc::ENODATA) | Some(libc::EOPNOTSUPP)
        ),
    }
}

/// No ACL can be read here: assume one may (the private-group rule does not apply here anyway).
#[cfg(not(target_os = "linux"))]
fn acl_may_name_others(_fd: RawFd) -> bool {
    true
}

/// Whether the `system.posix_acl_access` attribute `xattr` has an entry beyond the owner, the
/// owning group, the mask and others -- or is not one this reads for certain. Its format is
/// the kernel's: a little-endian 32-bit version (2), then 8-byte entries of a 16-bit tag, a
/// 16-bit permission set and a 32-bit id.
#[cfg(any(target_os = "linux", test))]
fn acl_names_others(xattr: &[u8]) -> bool {
    const USER_OBJ: u16 = 0x01;
    const GROUP_OBJ: u16 = 0x04;
    const MASK: u16 = 0x10;
    const OTHER: u16 = 0x20;
    let Some((version, entries)) = xattr.split_first_chunk::<4>() else {
        return true;
    };
    if u32::from_le_bytes(*version) != 2 || entries.len() % 8 != 0 {
        return true;
    }
    entries.as_chunks::<8>().0.iter().any(|entry| {
        let tag = u16::from_le_bytes([entry[0], entry[1]]);
        !matches!(tag, USER_OBJ | GROUP_OBJ | MASK | OTHER)
    })
}

/// Whether the group `gid` is the private group of the user `euid`, by the user-private-group
/// convention (`useradd`, `adduser`): nobody else is in it, so its write permission is the
/// user's own. All of these must hold (`group_is_private`):
/// - it is the user's primary group (`getpwuid`);
/// - its name (`getgrgid`) is the user's name, byte for byte;
/// - every member it lists resolves (`getpwnam`, on the name's bytes) to the user's uid.
///
/// Three lookups by key, once per group and user in a process; nothing is enumerated. Anything
/// that cannot be read counts as not private. It applies only on Linux, the one system where
/// an ACL that lets others write is seen too (`nobody_else_can_create`); elsewhere no group is
/// private.
///
/// Residuals:
/// - An account an administrator gave the same primary gid on purpose: the convention says a
///   private group is nobody else's primary group, and this does not enumerate the accounts
///   to check (with a directory service that would be a sweep of it, and one that does not
///   enumerate would hide some anyway).
/// - Groups granted outside every database: `pam_group` (`/etc/security/group.conf`) and
///   systemd's `SupplementaryGroups=` hand processes a gid no database records as theirs.
/// - Processes of someone who was a member before the group was changed keep the gid for as
///   long as they run.
/// - A group password lets anyone who knows it `newgrp` into the group; with shadow groups it
///   is out of the user's reach to read, and is not considered.
pub fn is_private_group(gid: u32, euid: u32) -> bool {
    PRIVATE_GROUP_QUERIES.with(|queries| queries.set(queries.get() + 1));
    if !cfg!(target_os = "linux") {
        return false;
    }
    static ANSWERS: Mutex<Option<HashMap<(u32, u32), bool>>> = Mutex::new(None);
    let mut answers = ANSWERS.lock().unwrap_or_else(|e| e.into_inner());
    let answers = answers.get_or_insert_with(HashMap::new);
    *answers
        .entry((gid, euid))
        .or_insert_with(|| read_private_group(gid, euid))
}

thread_local! {
    /// How many times this thread has asked `is_private_group`, cached or not.
    static PRIVATE_GROUP_QUERIES: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// How many times this thread has asked whether a group is private (`is_private_group`): for
/// tests that a run which asks nothing that needs it -- no mode or owner to preserve -- looks
/// nothing up.
pub fn private_group_queries() -> usize {
    PRIVATE_GROUP_QUERIES.with(|queries| queries.get())
}

/// `is_private_group`, read from the databases (under its lock: the lookups return storage the
/// next one reuses).
fn read_private_group(gid: u32, euid: u32) -> bool {
    let Some((user_name, user_gid)) = user_entry(euid) else {
        return false;
    };
    let Some((group_name, member_uids)) = group_entry(gid) else {
        return false;
    };
    let user = UserEntry {
        uid: euid,
        gid: user_gid,
        name: user_name.as_bytes(),
    };
    group_is_private(gid, &user, group_name.as_bytes(), member_uids)
}

/// What the passwd database holds for a user: its uid, primary gid and name.
#[derive(Clone, Copy, Debug)]
pub struct UserEntry<'a> {
    pub uid: u32,
    pub gid: u32,
    pub name: &'a [u8],
}

/// The name and primary gid of the user `uid` (`getpwuid`), the name as the database holds it.
fn user_entry(uid: u32) -> Option<(CString, u32)> {
    let passwd = unsafe { libc::getpwuid(uid) };
    if passwd.is_null() {
        return None;
    }
    let passwd = unsafe { &*passwd };
    let name = unsafe { CStr::from_ptr(passwd.pw_name) }.to_owned();
    Some((name, passwd.pw_gid))
}

/// The name of the group `gid` (`getgrgid`), as the database holds it, and the uid each member
/// it lists resolves to (`getpwnam` on the name's bytes), `None` for a name that resolves to
/// nobody; `None` altogether when the group cannot be read.
fn group_entry(gid: u32) -> Option<(CString, Vec<Option<u32>>)> {
    // Copied out first: `getgrgid` and `getpwnam` each return storage the next call reuses.
    let (group_name, names) = unsafe {
        let group = libc::getgrgid(gid);
        if group.is_null() {
            return None;
        }
        let group_name = CStr::from_ptr((*group).gr_name).to_owned();
        let mut names = Vec::new();
        let mut member = (*group).gr_mem;
        // read_unaligned: macOS does not align the member array.
        while !member.is_null() {
            let name = std::ptr::read_unaligned(member as *const *const libc::c_char);
            if name.is_null() {
                break;
            }
            names.push(CStr::from_ptr(name).to_owned());
            member = member.add(1);
        }
        (group_name, names)
    };
    let uid_of = |name: &CStr| {
        let passwd = unsafe { libc::getpwnam(name.as_ptr()) };
        (!passwd.is_null()).then(|| unsafe { (*passwd).pw_uid })
    };
    Some((group_name, names.iter().map(|name| uid_of(name)).collect()))
}

/// The rule `is_private_group` follows, for the group `gid` named `group_name` and the user
/// `user`, given the uid each member the group lists resolves to (`None` for one that resolves
/// to nobody): `gid` is the user's primary gid, `group_name` is the user's name byte for byte,
/// and every member is the user. Members are compared by uid, so a second name for the user is
/// the user.
pub fn group_is_private(
    gid: u32,
    user: &UserEntry<'_>,
    group_name: &[u8],
    member_uids: impl IntoIterator<Item = Option<u32>>,
) -> bool {
    user.gid == gid
        && group_name == user.name
        && member_uids
            .into_iter()
            .all(|member| member == Some(user.uid))
}

/// Check a directory the caller has just made with `mkdirat` in `parent_fd` and then opened as
/// `dir_fd` (`O_DIRECTORY | O_NOFOLLOW`): between the two, anyone else who can rename entries
/// in the parent could have renamed a directory of their choosing over it, and the caller would
/// fill it and, preserving attributes, give it an owner and mode.
///
/// Only the parent's owner, and anyone with group or other write permission on it when it is
/// not sticky, can do that (`others_can_rename`); when that is nobody but the effective user,
/// there is nothing to check. Otherwise the directory must be what a fresh `mkdirat` yields:
/// empty, with the owner and link count `made_by_us` accepts. `None` when it is not; how far it
/// is trusted otherwise.
///
/// An operand resolved from the working directory has no parent descriptor (`AT_FDCWD`); its
/// parent is then read as the opened directory's own `..`, which names wherever that directory
/// actually is. Every other fact comes from the descriptors, never from a name.
pub fn verify_made_dir(parent_fd: RawFd, dir_fd: RawFd) -> io::Result<Option<MadeTrust>> {
    let euid = unsafe { libc::geteuid() };
    let parent = if parent_fd == libc::AT_FDCWD {
        lstat_at(dir_fd, c"..")?
    } else {
        fstat(parent_fd)?
    };
    if !others_can_rename(&parent, euid) {
        return Ok(Some(MadeTrust::Full));
    }
    let st = fstat(dir_fd)?;
    let made = MadeObject {
        uid: st.st_uid,
        // Cast needed: `nlink_t` is u16 on macOS and u64 on Linux.
        #[allow(clippy::unnecessary_cast)]
        nlink: st.st_nlink as u64,
        is_dir: st.st_mode & libc::S_IFMT == libc::S_IFDIR,
        owners: fs_owners(dir_fd),
    };
    let Some(trust) = made_by_us(made, Some(parent.st_uid), euid) else {
        return Ok(None);
    };
    let check = || ftw::is_empty_dir_fd(dir_fd);
    if !made.is_dir || !empty_lending_read(dir_fd, &st, euid, check)? {
        return Ok(None);
    }
    Ok(Some(trust))
}

/// `check` -- whether the directory open on `dir_fd`, with `st`, is empty -- and, only if it
/// fails with EACCES on a directory the effective user `euid` owns, again with the owner's read
/// and search permission lent through the descriptor, the mode put back afterwards.
///
/// Reading a directory takes its owner's read and search permission, which a umask (0400, say)
/// may have withheld from one just made. They are lent only then: a chmod by a user outside the
/// directory's group clears its S_ISGID bit, and putting the mode back cannot set it again.
/// That is the residual, for a directory made under a umask denying its owner read, in a
/// set-group-ID parent of a group the user is not in: it loses S_ISGID.
pub fn empty_lending_read(
    dir_fd: RawFd,
    st: &libc::stat,
    euid: u32,
    mut check: impl FnMut() -> io::Result<bool>,
) -> io::Result<bool> {
    let err = match check() {
        Err(e) if e.raw_os_error() == Some(libc::EACCES) && st.st_uid == euid => e,
        answered => return answered,
    };
    let mode = st.st_mode & 0o7777;
    if mode & 0o500 == 0o500 {
        return Err(err);
    }
    chmod_fd(dir_fd, mode | 0o700)?;
    let empty = check();
    chmod_fd(dir_fd, mode)?;
    empty
}

/// Set the mode of the file open on `fd`, which may be held for search only (`O_PATH` on
/// Linux, where `fchmod` refuses it: `chmod_pinned`).
#[cfg(target_os = "linux")]
pub fn chmod_fd(fd: RawFd, mode: libc::mode_t) -> io::Result<()> {
    chmod_pinned(fd, mode)
}

/// Elsewhere a search-only descriptor (`O_SEARCH`) takes `fchmod`.
#[cfg(not(target_os = "linux"))]
pub fn chmod_fd(fd: RawFd, mode: libc::mode_t) -> io::Result<()> {
    if unsafe { libc::fchmod(fd, mode) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

/// `fstat` of a descriptor.
fn fstat(fd: RawFd) -> io::Result<libc::stat> {
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::fstat(fd, &mut st) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(st)
}

/// `fstatat` with `AT_SYMLINK_NOFOLLOW`.
fn lstat_at(dirfd: RawFd, name: &CStr) -> io::Result<libc::stat> {
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    let flags = libc::AT_SYMLINK_NOFOLLOW;
    if unsafe { libc::fstatat(dirfd, name.as_ptr(), &mut st, flags) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(st)
}

/// The `fchmodat2` system call number (Linux 6.6 and later), where it is the generic 452.
/// `libc` exports `SYS_fchmodat2` for x86_64 but not for aarch64-linux-gnu, so the number is
/// spelled here. Not on x32, whose numbers carry `__X32_SYSCALL_BIT`, nor on the architectures
/// with tables of their own (alpha, mips, ...): there only the procfs path is used.
#[cfg(target_os = "linux")]
const SYS_FCHMODAT2: Option<libc::c_long> = if cfg!(any(
    all(target_arch = "x86_64", target_pointer_width = "64"),
    target_arch = "x86",
    target_arch = "aarch64",
    target_arch = "arm",
    target_arch = "riscv64",
    target_arch = "loongarch64",
    target_arch = "powerpc64",
    target_arch = "s390x"
)) {
    Some(452)
} else {
    None
};

/// Set the mode of the inode an `O_PATH` descriptor pins, which `fchmod` refuses (EBADF).
///
/// First `fchmodat2(fd, "", mode, AT_EMPTY_PATH)` (Linux 6.6 and later), which acts on that
/// inode directly. Where it does not exist (ENOSYS), does not take `AT_EMPTY_PATH` (EINVAL), or
/// is refused by a seccomp filter that does not know it (EPERM: older runc, systemd's
/// `SystemCallFilter=`), `fchmodat` on `self/fd/N` relative to a `/proc` descriptor verified to
/// be procfs, which names the same inode; a genuine EPERM comes back from that call too. With
/// neither, the mode is not set and the failure is reported: a by-name fallback could act on
/// whatever the name holds by then.
#[cfg(target_os = "linux")]
pub fn chmod_pinned(fd: RawFd, mode: libc::mode_t) -> io::Result<()> {
    if let Some(sys_fchmodat2) = SYS_FCHMODAT2 {
        let ret = unsafe {
            libc::syscall(
                sys_fchmodat2,
                fd,
                c"".as_ptr(),
                libc::c_uint::from(mode),
                libc::AT_EMPTY_PATH,
            )
        };
        if ret == 0 {
            return Ok(());
        }
        let e = io::Error::last_os_error();
        if !matches!(
            e.raw_os_error(),
            Some(libc::ENOSYS) | Some(libc::EINVAL) | Some(libc::EPERM)
        ) {
            return Err(e);
        }
    }
    let proc_dir = procfs_dir()?;
    let pinned = proc_fd_name(fd);
    if unsafe { libc::fchmodat(proc_dir.as_raw_fd(), pinned.as_ptr(), mode, 0) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

/// `/proc`, opened and verified to be procfs (`PROC_SUPER_MAGIC`), so that `self/fd/N` names
/// exactly the inode open on descriptor N rather than whatever else is mounted or planted there.
#[cfg(target_os = "linux")]
pub fn procfs_dir() -> io::Result<File> {
    const PROC_SUPER_MAGIC: u32 = 0x9fa0;
    let fd = unsafe {
        libc::open(
            c"/proc".as_ptr(),
            libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC,
        )
    };
    if fd == -1 {
        return Err(io::Error::last_os_error());
    }
    let dir = unsafe { File::from_raw_fd(fd) };
    let mut st = std::mem::MaybeUninit::<libc::statfs>::uninit();
    if unsafe { libc::fstatfs(dir.as_raw_fd(), st.as_mut_ptr()) } != 0 {
        return Err(io::Error::last_os_error());
    }
    // `f_type` is a signed word whose width varies by architecture; the magic is its low bits.
    if unsafe { st.assume_init() }.f_type as u32 != PROC_SUPER_MAGIC {
        return Err(io::Error::other(gettext("/proc is not a procfs mount")));
    }
    Ok(dir)
}

/// `self/fd/N`, relative to `procfs_dir`.
#[cfg(target_os = "linux")]
pub fn proc_fd_name(fd: RawFd) -> CString {
    CString::new(format!("self/fd/{fd}")).expect("a formatted number has no NUL")
}

/// Set the times of the symbolic link `name` in `dirfd` by name, with `AT_SYMLINK_NOFOLLOW`,
/// if `lstat` shows the name still holds the link `pinned` -- its `(st_dev, st_ino)` -- and
/// return `true`; return `false`, changing nothing, if it does not.
///
/// For kernels before Linux 5.8, whose `utimensat` refuses `AT_EMPTY_PATH`, so a link pinned
/// with `O_PATH` cannot take its times through the pin, and its `self/fd/N` entry is followed
/// through the link. The residual is a replacement between the `lstat` and the `utimensat`: a
/// hard link to another file swapped in at that moment takes the link's times -- a wrong
/// mtime, at worst.
pub fn utimens_link_if_still(
    dirfd: RawFd,
    name: &CStr,
    pinned: (u64, u64),
    times: &[libc::timespec; 2],
) -> io::Result<bool> {
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    let nofollow = libc::AT_SYMLINK_NOFOLLOW;
    if unsafe { libc::fstatat(dirfd, name.as_ptr(), &mut st, nofollow) } != 0 {
        return Err(io::Error::last_os_error());
    }
    // Casts needed: `dev_t` is i32 on macOS and u64 on Linux.
    #[allow(clippy::unnecessary_cast)]
    let id = (st.st_dev as u64, st.st_ino as u64);
    if id != pinned || st.st_mode & libc::S_IFMT != libc::S_IFLNK {
        return Ok(false);
    }
    if unsafe { libc::utimensat(dirfd, name.as_ptr(), times.as_ptr(), nofollow) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::{
        acl_names_others, dir_writers, empty_lending_read, group_entry, group_is_private,
        is_private_group, made_by_us, others_can_rename, user_entry, utimens_link_if_still,
        verify_made_dir, ChainTrust, DirWriters, FoundDir, FsOwners, MadeObject, MadeTrust,
        Preserve, UserEntry,
    };
    use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
    use std::os::unix::fs::{MetadataExt, PermissionsExt};
    use std::path::Path;
    use std::rc::Rc;

    /// A parent directory of `uid` with permission bits `mode`.
    fn parent(uid: u32, mode: libc::mode_t) -> libc::stat {
        let mut st: libc::stat = unsafe { std::mem::zeroed() };
        st.st_uid = uid;
        st.st_mode = libc::S_IFDIR | mode;
        st
    }

    /// A directory found existing may have been created at its name by anyone who can create
    /// entries beside it.
    #[test]
    fn who_can_create_in_a_parent() {
        use DirWriters::{Others, User, UserAndGroup};
        let group = |gid, euid| UserAndGroup { gid, euid };
        // Nobody but the user can.
        assert_eq!(dir_writers(&parent(US, 0o755), US), User);
        assert_eq!(dir_writers(&parent(0, 0o755), 0), User);
        // A sticky directory others may write -- /tmp, root extracting into it too.
        assert_eq!(dir_writers(&parent(US, 0o1777), US), Others);
        assert_eq!(dir_writers(&parent(0, 0o1777), 0), Others);
        // Other write permission, whatever the group's.
        assert_eq!(dir_writers(&parent(US, 0o757), US), Others);
        assert_eq!(dir_writers(&parent(US, 0o777), US), Others);
        // Group write permission alone: the group's members, unless it is the user's private
        // group -- asked only later, of the directory's own group.
        let mut st = parent(US, 0o775);
        st.st_gid = 4242;
        assert_eq!(dir_writers(&st, US), group(4242, US));
        st.st_mode = libc::S_IFDIR | 0o2775;
        assert_eq!(dir_writers(&st, US), group(4242, US));
        // Someone else's directory: its owner can.
        assert_eq!(dir_writers(&parent(OTHER, 0o755), US), Others);
        assert_eq!(dir_writers(&parent(OTHER, 0o775), US), Others);
    }

    /// A group is the user's private one, by the user-private-group convention, only when it
    /// is the user's primary group, bears the user's name, and lists nobody else.
    #[test]
    fn what_makes_a_group_private() {
        let us = UserEntry {
            uid: US,
            gid: 500,
            name: b"us",
        };
        // Members are given as the uid each name resolves to, `None` for one that does not.
        assert!(group_is_private(500, &us, b"us", []));
        assert!(group_is_private(500, &us, b"us", [Some(US)]));
        // A second name for the user's own uid is the user.
        assert!(group_is_private(500, &us, b"us", [Some(US), Some(US)]));
        // Not the user's primary group.
        assert!(!group_is_private(600, &us, b"us", []));
        // Not named after the user, byte for byte.
        assert!(!group_is_private(500, &us, b"users", []));
        assert!(!group_is_private(500, &us, b"Us", []));
        assert!(!group_is_private(500, &us, b"us ", []));
        let lossy = UserEntry {
            name: b"\xffus",
            ..us
        };
        assert!(!group_is_private(500, &lossy, b"\xfeus", []));
        assert!(group_is_private(500, &lossy, b"\xffus", []));
        // Another member listed.
        assert!(!group_is_private(500, &us, b"us", [Some(US), Some(OTHER)]));
        assert!(!group_is_private(500, &us, b"us", [Some(OTHER)]));
        // A member whose name resolves to nobody cannot be shown to be the user.
        assert!(!group_is_private(500, &us, b"us", [None]));
    }

    /// An access ACL names someone else when it has any entry but the owner's, the owning
    /// group's, the mask and others'; one that cannot be read for certain counts as naming.
    #[test]
    fn which_access_acls_name_others() {
        let acl = |tags: &[u16]| {
            let mut xattr = 2u32.to_le_bytes().to_vec();
            for &tag in tags {
                xattr.extend(tag.to_le_bytes());
                xattr.extend(7u16.to_le_bytes());
                xattr.extend(u32::MAX.to_le_bytes());
            }
            xattr
        };
        // Owner, owning group, others; with a mask.
        assert!(!acl_names_others(&acl(&[0x01, 0x04, 0x20])));
        assert!(!acl_names_others(&acl(&[0x01, 0x04, 0x10, 0x20])));
        // A named user, a named group, a tag not known.
        assert!(acl_names_others(&acl(&[0x01, 0x02, 0x04, 0x10, 0x20])));
        assert!(acl_names_others(&acl(&[0x01, 0x04, 0x08, 0x10, 0x20])));
        assert!(acl_names_others(&acl(&[0x01, 0x04, 0x40, 0x20])));
        // Not the format read here.
        let mut other_version = acl(&[0x01, 0x04, 0x20]);
        other_version[0] = 3;
        assert!(acl_names_others(&other_version));
        let mut torn = acl(&[0x01, 0x04, 0x20]);
        torn.pop();
        assert!(acl_names_others(&torn));
        assert!(acl_names_others(&[2, 0]));
    }

    /// The test user's own primary group, read from the real databases, agrees with the rule
    /// applied to what those databases list -- and an unknown user or group is never private.
    #[test]
    fn private_group_lookup_follows_the_databases() {
        let euid = unsafe { libc::geteuid() };
        assert!(!is_private_group(u32::MAX - 1, euid));
        assert!(!is_private_group(0, u32::MAX - 1));
        let Some((user_name, user_gid)) = user_entry(euid) else {
            return;
        };
        let Some((group_name, members)) = group_entry(user_gid) else {
            assert!(!is_private_group(user_gid, euid));
            return;
        };
        let user = UserEntry {
            uid: euid,
            gid: user_gid,
            name: user_name.as_bytes(),
        };
        let expected = cfg!(target_os = "linux")
            && group_is_private(user_gid, &user, group_name.as_bytes(), members);
        assert_eq!(is_private_group(user_gid, euid), expected);
        // Asked again, the answer is the one read.
        assert_eq!(is_private_group(user.gid, euid), expected);
    }

    /// A found directory gets its times only unless mode or owner was asked for; then what was
    /// asked for only where nobody else could have created its name -- in its parent, and in
    /// every directory above it up to the anchor. A directory the caller made restarts the
    /// trust below it.
    #[test]
    fn a_found_directory_gets_what_was_asked_only_down_a_trusted_chain() {
        let tmp = crate::tmp::TempDir::new().unwrap();
        let root = tmp.path();
        // anchor 0755 / g 0777 (someone else can create here, whatever its group) / x 0755 / d
        std::fs::create_dir_all(root.join("g/x/d")).unwrap();
        for (dir, mode) in [("", 0o755), ("g", 0o777), ("g/x", 0o755)] {
            let path = root.join(dir);
            std::fs::set_permissions(path, std::fs::Permissions::from_mode(mode)).unwrap();
        }
        let none = Preserve {
            mode: false,
            owner: false,
        };
        let mode = Preserve {
            mode: true,
            owner: false,
        };
        let owner = Preserve {
            mode: false,
            owner: true,
        };
        let fd = |path: &str| Rc::new(std::fs::File::open(root.join(path)).unwrap());
        let (root_fd, g_fd, x_fd) = (fd(""), fd("g"), fd("g/x"));
        let anchor = ChainTrust::anchor(&root_fd).unwrap();
        let g = anchor.found(&g_fd).unwrap();
        let x = g.found(&x_fd).unwrap();

        // Nothing is worked out until a mode or owner is asked for.
        assert_eq!(x.found_dir(none), FoundDir::TimesOnly);
        assert!(x.0.entries_safe.get().is_none() && anchor.0.entries_safe.get().is_none());

        // `g` itself: found in the anchor, which only the user can write.
        assert_eq!(anchor.found_dir(mode), FoundDir::AsRequested);
        // `x`: found in `g`, which others can write.
        assert_eq!(g.found_dir(mode), FoundDir::LeaveAlone);
        // `d`: its parent `x` is the user's alone, but `x` may be anyone's
        // directory renamed into `g`.
        assert_eq!(x.found_dir(mode), FoundDir::LeaveAlone);
        assert_eq!(x.found_dir(owner), FoundDir::LeaveAlone);
        assert_eq!(x.found_dir(none), FoundDir::TimesOnly);
        // Had the caller made and verified `x`, what it finds in it is safe.
        let made_x = ChainTrust::made(&x_fd).unwrap();
        assert_eq!(made_x.found_dir(mode), FoundDir::AsRequested);
        assert_eq!(made_x.found_dir(owner), FoundDir::AsRequested);
        // A directory found in no directory the caller can locate takes nothing asked for, and
        // hands that on.
        let unlocated = ChainTrust::unlocated();
        assert_eq!(unlocated.found_dir(mode), FoundDir::LeaveAlone);
        assert_eq!(unlocated.found_dir(none), FoundDir::TimesOnly);
        let below = unlocated.found(&root_fd).unwrap();
        assert_eq!(below.found_dir(mode), FoundDir::LeaveAlone);
    }

    /// A directory the user names is trusted as named -- unless its name's last component is a
    /// symbolic link in a directory others can write, or its name no longer holds it.
    #[test]
    fn a_named_directory_reached_through_a_link_is_found_in_the_links_directory() {
        let tmp = crate::tmp::TempDir::new().unwrap();
        let home = tmp.path().join("home");
        let open = tmp.path().join("open");
        std::fs::create_dir_all(&home).unwrap();
        std::fs::create_dir(&open).unwrap();
        std::os::unix::fs::symlink(&home, open.join("l")).unwrap();
        let mode = Preserve {
            mode: true,
            owner: false,
        };
        let held = Rc::new(std::fs::File::open(&home).unwrap());
        for (open_mode, through) in [
            (0o777, FoundDir::LeaveAlone),
            (0o755, FoundDir::AsRequested),
        ] {
            std::fs::set_permissions(&open, std::fs::Permissions::from_mode(open_mode)).unwrap();
            for path in [open.join("l"), open.join("l/"), open.join("l/.").join("")] {
                // `l/.` too: a trailing `.` is no component of its own.
                let named = ChainTrust::named(&path, &held).unwrap();
                assert_eq!(named.named_dir(mode), through, "{path:?} in {open_mode:o}");
                assert_eq!(
                    named.hands.found_dir(mode),
                    through,
                    "{path:?} in {open_mode:o}"
                );
            }
        }
        std::fs::set_permissions(&open, std::fs::Permissions::from_mode(0o755)).unwrap();
        // Named directly.
        let named = ChainTrust::named(&home, &held).unwrap();
        assert_eq!(named.named_dir(mode), FoundDir::AsRequested);
        assert_eq!(named.hands.found_dir(mode), FoundDir::AsRequested);
        // A name that holds another directory than the one opened.
        std::fs::rename(&home, tmp.path().join("moved")).unwrap();
        std::fs::create_dir(&home).unwrap();
        let named = ChainTrust::named(&home, &held).unwrap();
        assert_eq!(named.named_dir(mode), FoundDir::LeaveAlone);
        assert_eq!(named.hands.found_dir(mode), FoundDir::LeaveAlone);
    }

    /// A link anywhere in a named path -- in its last component, in one before it, met again
    /// inside another link's target, or left by a `..` -- leads wherever its owner chose: the
    /// directory reached is trusted only where every directory holding a link the path follows
    /// is the user's alone. A path through no link is trusted as named.
    #[test]
    fn a_named_path_is_trusted_only_through_links_the_user_vouches_for() {
        let tmp = crate::tmp::TempDir::new().unwrap();
        let root = tmp.path();
        // home (0755) / sub, x; open (0777) / d -> ../home/sub, m -> ../home;
        // safe (0755) / l -> ../home, e -> ../open/m; loop (0755) / a -> b, b -> a.
        for dir in ["home/sub", "home/x", "open", "safe", "loop"] {
            std::fs::create_dir_all(root.join(dir)).unwrap();
        }
        let link = |target: &str, at: &str| std::os::unix::fs::symlink(target, root.join(at));
        link("../home/sub", "open/d").unwrap();
        link("../home", "open/m").unwrap();
        link("../home", "safe/l").unwrap();
        link("../open/m", "safe/e").unwrap();
        link("b", "loop/a").unwrap();
        link("a", "loop/b").unwrap();
        for (dir, mode) in [
            ("home", 0o755),
            ("open", 0o777),
            ("safe", 0o755),
            ("loop", 0o755),
        ] {
            std::fs::set_permissions(root.join(dir), std::fs::Permissions::from_mode(mode))
                .unwrap();
        }
        let mode = Preserve {
            mode: true,
            owner: false,
        };
        let held = |path: &Path| Rc::new(std::fs::File::open(path).unwrap());
        let judge = |named: &Path, opened: &Path| {
            let dir = held(opened);
            let named = ChainTrust::named(named, &dir).unwrap();
            (named.named_dir(mode), named.hands.found_dir(mode))
        };
        let trusted = (FoundDir::AsRequested, FoundDir::AsRequested);
        let untrusted = (FoundDir::LeaveAlone, FoundDir::LeaveAlone);
        let home = root.join("home");
        let cases = [
            // No link: as named.
            (root.join("home"), home.clone(), trusted),
            (root.join("home/sub/.."), home.clone(), trusted),
            (root.join("home/./x"), home.join("x"), trusted),
            // A link planted in the last component, a middle one, or left by `..`.
            (root.join("open/m"), home.clone(), untrusted),
            (root.join("open/m/"), home.clone(), untrusted),
            (root.join("open/m/x"), home.join("x"), untrusted),
            (root.join("open/d/.."), home.clone(), untrusted),
            // A link the user's own directory holds, alone or leading to one planted.
            (root.join("safe/l"), home.clone(), trusted),
            (root.join("safe/l/x"), home.join("x"), trusted),
            (root.join("safe/e"), home.clone(), untrusted),
            (root.join("safe/e/x"), home.join("x"), untrusted),
            // A loop of links, and a path that is not the directory opened.
            (root.join("loop/a"), home.clone(), untrusted),
            (root.join("home/x"), home.clone(), untrusted),
        ];
        for (named, opened, expected) in cases {
            assert_eq!(judge(&named, &opened), expected, "{named:?}");
        }
        // `/`, `.` and a relative name with no slash (this crate's `src`): no link, as named.
        assert_eq!(
            judge(Path::new("/"), Path::new("/")).0,
            FoundDir::AsRequested
        );
        assert_eq!(
            judge(Path::new("."), Path::new(".")).0,
            FoundDir::AsRequested
        );
        assert_eq!(
            judge(Path::new("src"), Path::new("src")).0,
            FoundDir::AsRequested
        );
        std::fs::set_permissions(root.join("open"), std::fs::Permissions::from_mode(0o755))
            .unwrap();
    }

    /// A link holds its directory's descriptor weakly. A group-writable one, whose group needs
    /// asking about, counts as one others may write once its holder has closed it; one the
    /// mode alone settles does not need it.
    #[test]
    fn a_closed_group_writable_directory_is_not_trusted() {
        let tmp = crate::tmp::TempDir::new().unwrap();
        let mode = Preserve {
            mode: true,
            owner: true,
        };
        for (perm, open, closed) in [(0o755, true, true), (0o775, private_here(), false)] {
            std::fs::set_permissions(tmp.path(), std::fs::Permissions::from_mode(perm)).unwrap();
            let held = Rc::new(std::fs::File::open(tmp.path()).unwrap());
            let trust = ChainTrust::anchor(&held).unwrap();
            let again = ChainTrust::anchor(&held).unwrap();
            assert_eq!(
                trust.found_dir(mode) == FoundDir::AsRequested,
                open,
                "{perm:o}"
            );
            drop(held);
            assert_eq!(
                again.found_dir(mode) == FoundDir::AsRequested,
                closed,
                "{perm:o}"
            );
        }
        std::fs::set_permissions(tmp.path(), std::fs::Permissions::from_mode(0o700)).unwrap();
    }

    /// Whether a directory this test makes is of the user's private group, with no ACL.
    fn private_here() -> bool {
        let tmp = crate::tmp::TempDir::new().unwrap();
        std::fs::set_permissions(tmp.path(), std::fs::Permissions::from_mode(0o775)).unwrap();
        let dir = std::fs::File::open(tmp.path()).unwrap();
        super::nobody_else_can_create(dir.as_raw_fd()).unwrap()
    }

    /// Only the parent's owner, or anyone allowed to write a parent that is not sticky, can
    /// rename entries in it.
    #[test]
    fn who_can_rename_in_a_parent() {
        assert!(!others_can_rename(&parent(US, 0o755), US));
        assert!(!others_can_rename(&parent(US, 0o1777), US));
        assert!(others_can_rename(&parent(US, 0o775), US));
        assert!(others_can_rename(&parent(US, 0o777), US));
        assert!(others_can_rename(&parent(OTHER, 0o755), US));
    }

    /// `name` below the open directory `dir`, opened for search only (it may deny reading).
    fn open_search(dir: &std::fs::File, name: &std::ffi::CStr) -> OwnedFd {
        #[cfg(target_os = "linux")]
        let search = libc::O_PATH;
        #[cfg(not(target_os = "linux"))]
        let search = libc::O_SEARCH;
        let flags = search | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC;
        let fd = unsafe { libc::openat(dir.as_raw_fd(), name.as_ptr(), flags) };
        assert!(fd >= 0, "{}", std::io::Error::last_os_error());
        unsafe { OwnedFd::from_raw_fd(fd) }
    }

    /// In a parent others can rename entries in, a made directory must be empty -- also when a
    /// umask left it unreadable to its owner -- and anything else is refused.
    #[test]
    fn a_made_directory_is_verified_empty_where_others_can_rename() {
        let tmp = crate::tmp::TempDir::new().unwrap();
        std::fs::set_permissions(tmp.path(), std::fs::Permissions::from_mode(0o777)).unwrap();
        let parent = std::fs::File::open(tmp.path()).unwrap();
        std::fs::create_dir(tmp.path().join("fresh")).unwrap();
        std::fs::create_dir(tmp.path().join("full")).unwrap();
        std::fs::write(tmp.path().join("full/f"), "").unwrap();
        // As under umask 0400.
        for name in ["fresh", "full"] {
            let path = tmp.path().join(name);
            std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o300)).unwrap();
        }

        let verify = |name| {
            let dir = open_search(&parent, name);
            verify_made_dir(parent.as_raw_fd(), dir.as_raw_fd())
        };
        assert_eq!(verify(c"fresh").unwrap(), Some(MadeTrust::Full));
        assert_eq!(verify(c"full").unwrap(), None);
        // The lent permission was put back.
        let mode = std::fs::metadata(tmp.path().join("fresh")).unwrap().mode();
        assert_eq!(mode & 0o7777, 0o300);
        for name in ["fresh", "full"] {
            let path = tmp.path().join(name);
            std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o700)).unwrap();
        }
    }

    /// Read permission is lent for the emptiness check only when the check fails for want of
    /// it: a check that succeeds as things are (root, reading past the mode) leaves the
    /// directory's mode alone, since a chmod by someone outside its group drops S_ISGID for
    /// good.
    #[test]
    fn read_is_lent_only_when_the_check_needs_it() {
        let tmp = crate::tmp::TempDir::new().unwrap();
        let path = tmp.path().join("d");
        std::fs::create_dir(&path).unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o300)).unwrap();
        let fd = open_search(&std::fs::File::open(tmp.path()).unwrap(), c"d");
        let st = {
            let mut st: libc::stat = unsafe { std::mem::zeroed() };
            assert_eq!(unsafe { libc::fstat(fd.as_raw_fd(), &mut st) }, 0);
            st
        };
        let ctime = || std::fs::metadata(&path).unwrap().ctime_nsec();
        let before = ctime();
        let euid = unsafe { libc::geteuid() };

        // Readable as things are: no chmod.
        std::thread::sleep(std::time::Duration::from_millis(10));
        assert!(empty_lending_read(fd.as_raw_fd(), &st, euid, || Ok(true)).unwrap());
        assert_eq!(
            ctime(),
            before,
            "the mode was changed for a check that needed nothing"
        );

        // Refused for want of read: lent, and put back.
        let mut calls = 0;
        let check = || {
            calls += 1;
            if calls == 1 {
                Err(std::io::Error::from_raw_os_error(libc::EACCES))
            } else {
                Ok(true)
            }
        };
        assert!(empty_lending_read(fd.as_raw_fd(), &st, euid, check).unwrap());
        let mode = std::fs::metadata(&path).unwrap().mode() & 0o7777;
        assert_eq!(mode, 0o300);
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o700)).unwrap();
    }

    const US: u32 = 1000;
    const OTHER: u32 = 2000;
    const FULL: Option<MadeTrust> = Some(MadeTrust::Full);
    const PARENT_OWNER_ONLY: Option<MadeTrust> = Some(MadeTrust::ParentOwnerOnly);

    fn dir(uid: u32, nlink: u64) -> MadeObject {
        MadeObject {
            uid,
            nlink,
            is_dir: true,
            owners: FsOwners::Stored,
        }
    }

    fn node(uid: u32, nlink: u64) -> MadeObject {
        MadeObject {
            uid,
            nlink,
            is_dir: false,
            owners: FsOwners::Stored,
        }
    }

    /// On NFS, FUSE or cifs/smb, which may map owners or store them.
    fn maybe_mapped(made: MadeObject) -> MadeObject {
        MadeObject {
            owners: FsOwners::MayBeMapped,
            ..made
        }
    }

    /// On msdos/vfat, exfat or ntfs, which store no owner.
    fn ownerless(made: MadeObject) -> MadeObject {
        MadeObject {
            owners: FsOwners::None,
            ..made
        }
    }

    /// The ordinary case: the caller's own object in its own or anyone's directory, on any
    /// filesystem.
    #[test]
    fn trusts_what_we_made() {
        assert_eq!(made_by_us(dir(US, 2), Some(US), US), FULL);
        assert_eq!(made_by_us(dir(US, 2), Some(OTHER), US), FULL);
        assert_eq!(made_by_us(node(US, 1), Some(OTHER), US), FULL);
        assert_eq!(made_by_us(node(US, 1), None, US), FULL);
        assert_eq!(made_by_us(maybe_mapped(dir(US, 2)), Some(OTHER), US), FULL);
    }

    /// A filesystem with no owners reports the mount's owner for everything: what the caller
    /// makes is owned like its parent, and there is no owner an attacker's object could carry
    /// instead.
    #[test]
    fn trusts_an_owner_reported_by_an_ownerless_filesystem() {
        assert_eq!(made_by_us(ownerless(dir(4242, 2)), Some(4242), US), FULL);
        assert_eq!(made_by_us(ownerless(node(4242, 1)), Some(4242), US), FULL);
    }

    /// NFS, FUSE or cifs/smb owned like the parent: accepted, so that work where owners are
    /// mapped (root_squash, sshfs without idmap) is possible, but given no owner and no mode,
    /// because where owners are stored the parent's owner's object may have been renamed in by
    /// someone else.
    #[test]
    fn accepts_but_does_not_trust_an_owner_that_may_be_stored() {
        assert_eq!(
            made_by_us(maybe_mapped(dir(4242, 2)), Some(4242), US),
            PARENT_OWNER_ONLY
        );
        assert_eq!(
            made_by_us(maybe_mapped(node(4242, 1)), Some(4242), US),
            PARENT_OWNER_ONLY
        );
        assert_eq!(
            made_by_us(maybe_mapped(dir(65534, 2)), Some(65534), 0),
            PARENT_OWNER_ONLY
        );
    }

    /// Where owners are stored, an object owned by the parent's owner (who is not the caller's
    /// user) was made by that owner.
    #[test]
    fn refuses_a_parent_owners_object_where_owners_are_stored() {
        assert_eq!(made_by_us(dir(OTHER, 2), Some(OTHER), US), None);
        assert_eq!(made_by_us(node(OTHER, 1), Some(OTHER), US), None);
    }

    /// The parent-owner arm needs the parent's owner; with no parent descriptor only the
    /// caller's own user is accepted.
    #[test]
    fn refuses_a_foreign_owner_without_a_parent() {
        assert_eq!(made_by_us(maybe_mapped(dir(4242, 2)), None, US), None);
        assert_eq!(made_by_us(ownerless(dir(4242, 2)), None, US), None);
    }

    /// Filesystems that report 1 for every directory's link count (btrfs, some FUSE).
    #[test]
    fn accepts_a_directory_link_count_of_one() {
        assert_eq!(made_by_us(dir(US, 1), Some(US), US), FULL);
    }

    /// Someone else's object in a directory the caller owns: an attacker in a shared directory.
    #[test]
    fn refuses_another_users_object() {
        assert_eq!(made_by_us(dir(OTHER, 2), Some(US), US), None);
        assert_eq!(made_by_us(node(OTHER, 1), Some(US), US), None);
        assert_eq!(made_by_us(maybe_mapped(dir(OTHER, 2)), Some(US), US), None);
        assert_eq!(made_by_us(ownerless(dir(OTHER, 2)), Some(US), US), None);
        // In someone else's directory, an object owned by a third user.
        assert_eq!(made_by_us(dir(3000, 2), Some(OTHER), US), None);
        assert_eq!(
            made_by_us(maybe_mapped(dir(3000, 2)), Some(OTHER), US),
            None
        );
    }

    /// A hard link to an existing node (the caller's own, or a mapped owner's) is not a fresh
    /// one, and a directory with subdirectories is not a fresh one.
    #[test]
    fn refuses_link_counts_a_fresh_object_cannot_have() {
        assert_eq!(made_by_us(node(US, 2), Some(US), US), None);
        assert_eq!(
            made_by_us(maybe_mapped(node(4242, 2)), Some(4242), US),
            None
        );
        assert_eq!(made_by_us(dir(US, 3), Some(US), US), None);
    }

    /// A made symbolic link's times by name reach only the pinned link, never a hard link to
    /// another file swapped in for it.
    #[test]
    fn link_times_by_name_reach_only_the_pinned_link() {
        use std::os::fd::AsRawFd;
        use std::os::unix::fs::MetadataExt;

        let tmp = crate::tmp::TempDir::new().unwrap();
        let dir = tmp.path();
        std::os::unix::fs::symlink("anywhere", dir.join("l")).unwrap();
        let dirfd = std::fs::File::open(dir).unwrap();
        let link = std::fs::symlink_metadata(dir.join("l")).unwrap();
        let pinned = (link.dev(), link.ino());
        let at = |sec| libc::timespec {
            tv_sec: sec,
            tv_nsec: 0,
        };

        let times = [at(12345), at(12345)];
        assert!(utimens_link_if_still(dirfd.as_raw_fd(), c"l", pinned, &times).unwrap());
        let md = std::fs::symlink_metadata(dir.join("l")).unwrap();
        assert_eq!(md.mtime(), 12345);

        let victim = dir.join("victim");
        std::fs::write(&victim, "").unwrap();
        std::fs::remove_file(dir.join("l")).unwrap();
        std::fs::hard_link(&victim, dir.join("l")).unwrap();
        let before = std::fs::metadata(&victim).unwrap().mtime();
        let times = [at(1), at(1)];
        assert!(!utimens_link_if_still(dirfd.as_raw_fd(), c"l", pinned, &times).unwrap());
        assert_eq!(std::fs::metadata(&victim).unwrap().mtime(), before);
    }
}
