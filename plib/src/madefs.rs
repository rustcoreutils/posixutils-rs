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
use std::ffi::CStr;
#[cfg(target_os = "linux")]
use std::ffi::CString;
#[cfg(target_os = "linux")]
use std::fs::File;
use std::io;
use std::os::fd::RawFd;
#[cfg(target_os = "linux")]
use std::os::fd::{AsRawFd, FromRawFd};

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
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ChainTrust {
    /// Whether directories found existing in this one may take what was asked for.
    entries_safe: bool,
}

impl ChainTrust {
    /// The trust the anchor -- the directory the user named, open on `anchor_fd` -- hands its
    /// entries.
    pub fn anchor(anchor_fd: RawFd) -> io::Result<Self> {
        Ok(ChainTrust {
            entries_safe: nobody_else_can_create_in(anchor_fd)?,
        })
    }

    /// The trust a directory found existing, open on `dir_fd`, in a directory that handed it
    /// `self`, hands its own entries.
    pub fn found(self, dir_fd: RawFd) -> io::Result<Self> {
        Ok(ChainTrust {
            entries_safe: self.entries_safe && nobody_else_can_create_in(dir_fd)?,
        })
    }

    /// The trust a directory the caller made and verified (`verify_made_dir`), open on
    /// `dir_fd`, hands its entries, wherever it is.
    pub fn made(dir_fd: RawFd) -> io::Result<Self> {
        Ok(ChainTrust {
            entries_safe: nobody_else_can_create_in(dir_fd)?,
        })
    }

    /// What a directory found existing in a directory of this trust may be given, `requested`
    /// being which of mode and owner the user asked to preserve.
    pub fn found_dir(self, requested: Preserve) -> FoundDir {
        if !requested.mode && !requested.owner {
            FoundDir::TimesOnly
        } else if self.entries_safe {
            FoundDir::AsRequested
        } else {
            FoundDir::LeaveAlone
        }
    }
}

/// `nobody_else_can_create` for the directory open on `fd`.
fn nobody_else_can_create_in(fd: RawFd) -> io::Result<bool> {
    let euid = unsafe { libc::geteuid() };
    Ok(nobody_else_can_create(&fstat(fd)?, euid))
}

/// Whether nobody but `euid` can create entries in the directory `parent`: it is owned by
/// `euid` and grants no group or other write permission. A sticky directory others may write
/// counts as one they can create entries in.
///
/// Only what `st_mode` shows is seen. Write permission a POSIX ACL grants to named users or
/// groups shows there (in the group bits, the ACL mask); write permission a macOS or NFSv4 ACL
/// grants does not, and is not taken into account. That is a residual: below a directory such
/// an ACL lets others write, a directory found existing -- possibly one of theirs renamed
/// there -- is given the mode or owner asked for.
pub fn nobody_else_can_create(parent: &libc::stat, euid: u32) -> bool {
    // Cast needed: `mode_t` is u16 on macOS and u32 on Linux. S_IWGRP|S_IWOTH is 0o022 (fixed
    // by POSIX).
    #[allow(clippy::unnecessary_cast)]
    let mode = parent.st_mode as u32;
    parent.st_uid == euid && mode & 0o022 == 0
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
    let check = || is_empty_dir_fd(dir_fd);
    if !made.is_dir || !empty_lending_read(dir_fd, &st, euid, check)? {
        return Ok(None);
    }
    Ok(Some(trust))
}

/// Whether the directory open on `dir_fd` lists nothing but `.` and `..`.
///
/// It is read through a new open of `.` relative to `dir_fd` -- the same directory, which no
/// rename can swap -- so `dir_fd` itself (which may be held for search only) is left alone.
pub fn is_empty_dir_fd(dir_fd: RawFd) -> io::Result<bool> {
    let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC;
    let fd = unsafe { libc::openat(dir_fd, c".".as_ptr(), flags) };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    let dir = unsafe { libc::fdopendir(fd) };
    if dir.is_null() {
        let e = io::Error::last_os_error();
        unsafe { libc::close(fd) };
        return Err(e);
    }
    let empty = lists_nothing(dir);
    unsafe { libc::closedir(dir) };
    empty
}

/// Whether the open directory stream `dir` holds nothing but `.` and `..`.
fn lists_nothing(dir: *mut libc::DIR) -> io::Result<bool> {
    loop {
        errno::set_errno(errno::Errno(0));
        let entry = unsafe { libc::readdir(dir) };
        if entry.is_null() {
            let e = io::Error::last_os_error();
            return match e.raw_os_error() {
                Some(0) | None => Ok(true),
                Some(_) => Err(e),
            };
        }
        let name = unsafe { CStr::from_ptr((*entry).d_name.as_ptr()) }.to_bytes();
        if name != b"." && name != b".." {
            return Ok(false);
        }
    }
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
        empty_lending_read, made_by_us, nobody_else_can_create, others_can_rename,
        utimens_link_if_still, verify_made_dir, ChainTrust, FoundDir, FsOwners, MadeObject,
        MadeTrust, Preserve,
    };
    use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
    use std::os::unix::fs::{MetadataExt, PermissionsExt};

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
        // Nobody but the user can.
        assert!(nobody_else_can_create(&parent(US, 0o755), US));
        assert!(nobody_else_can_create(&parent(0, 0o755), 0));
        // A sticky directory others may write -- /tmp, root extracting into it too.
        assert!(!nobody_else_can_create(&parent(US, 0o1777), US));
        assert!(!nobody_else_can_create(&parent(0, 0o1777), 0));
        // Group or other write permission.
        assert!(!nobody_else_can_create(&parent(US, 0o775), US));
        assert!(!nobody_else_can_create(&parent(US, 0o757), US));
        // Someone else's directory: its owner can.
        assert!(!nobody_else_can_create(&parent(OTHER, 0o755), US));
    }

    /// A found directory gets its times only unless mode or owner was asked for; then what was
    /// asked for only where nobody else could have created its name -- in its parent, and in
    /// every directory above it up to the anchor. A directory the caller made restarts the
    /// trust below it.
    #[test]
    fn a_found_directory_gets_what_was_asked_only_down_a_trusted_chain() {
        let tmp = crate::tmp::TempDir::new().unwrap();
        let root = tmp.path();
        // anchor 0755 / g 0775 (someone else can create here) / x 0755 / d
        std::fs::create_dir_all(root.join("g/x/d")).unwrap();
        for (dir, mode) in [("", 0o755), ("g", 0o775), ("g/x", 0o755)] {
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
        let fd = |path: &str| std::fs::File::open(root.join(path)).unwrap();
        let anchor = ChainTrust::anchor(fd("").as_raw_fd()).unwrap();
        let g = anchor.found(fd("g").as_raw_fd()).unwrap();
        let x = g.found(fd("g/x").as_raw_fd()).unwrap();

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
        let made_x = ChainTrust::made(fd("g/x").as_raw_fd()).unwrap();
        assert_eq!(made_x.found_dir(mode), FoundDir::AsRequested);
        assert_eq!(made_x.found_dir(owner), FoundDir::AsRequested);
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
