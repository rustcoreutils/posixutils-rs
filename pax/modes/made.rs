//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Acting on a node pax has just made, and only on it.
//!
//! `mkfifoat`, `mknodat` and `symlinkat` return no descriptor, and a FIFO or
//! device cannot be opened just to change its attributes: the open blocks, or
//! acts on the device. Applying owner, mode and times by name afterwards
//! reaches whatever the name holds by then, and anyone who can write the
//! directory can make that a hard link to another file -- which then takes the
//! member's owner, mode (set-user-ID included) and times.
//!
//! So the node is pinned right after it is made and checked to be what a fresh
//! create yields -- the type asked for, one link, owned by pax's effective user
//! -- and every change goes through the pin. The same checks, by owner, link
//! count and emptiness, tell a directory pax has just made from one renamed
//! over it.
//!
//! The trust rules mirror cp's (`tree/common/copy.rs`, `made_by_us`).

use std::ffi::CStr;
use std::io;
use std::os::fd::{AsRawFd, BorrowedFd};

/// How a filesystem keeps file owners, as far as its type says.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum FsOwners {
    /// Each object's owner is stored and reported as it is (ext4, xfs, btrfs,
    /// tmpfs, ...), and the answer wherever the type cannot be read.
    Stored,
    /// Owners may be mapped -- reported from a mount option, an id map or a
    /// squash rule rather than from who made the object -- or may be stored
    /// for real, depending on the mount: NFS, FUSE, cifs/smb, ntfs3.
    MayBeMapped,
    /// No owner is stored at all; every object reports the mount's owner:
    /// msdos/vfat, exfat and the classic ntfs driver.
    None,
}

/// How the filesystem holding `fd` keeps owners, from its `fstatfs` type.
#[cfg(target_os = "linux")]
pub(crate) fn fs_owners(fd: BorrowedFd<'_>) -> FsOwners {
    const OWNERLESS_FS: [u64; 3] = [
        0x4d44,      // MSDOS_SUPER_MAGIC (msdos, vfat)
        0x2011_bab0, // EXFAT_SUPER_MAGIC
        0x5346_544e, // NTFS_SB_MAGIC
    ];
    const MAYBE_MAPPED_FS: [u64; 6] = [
        0x7366_746e, // ntfs3
        0xff53_4d42, // CIFS_SUPER_MAGIC
        0xfe53_4d42, // SMB2_SUPER_MAGIC
        0x517b,      // SMB_SUPER_MAGIC
        0x6969,      // NFS_SUPER_MAGIC
        0x6573_5546, // FUSE_SUPER_MAGIC (fuse and fuseblk)
    ];
    let mut st = std::mem::MaybeUninit::<libc::statfs>::uninit();
    if unsafe { libc::fstatfs(fd.as_raw_fd(), st.as_mut_ptr()) } != 0 {
        return FsOwners::Stored;
    }
    let st = unsafe { st.assume_init() };
    // `f_type` is a signed word whose width varies by architecture; the magic
    // numbers are its low 32 bits.
    let f_type = u64::from(st.f_type as u32);
    if OWNERLESS_FS.contains(&f_type) {
        FsOwners::None
    } else if MAYBE_MAPPED_FS.contains(&f_type) {
        FsOwners::MayBeMapped
    } else {
        FsOwners::Stored
    }
}

/// How the filesystem holding `fd` keeps owners, from its `fstatfs` type
/// name.
#[cfg(target_os = "macos")]
pub(crate) fn fs_owners(fd: BorrowedFd<'_>) -> FsOwners {
    const OWNERLESS_FS: [&[u8]; 3] = [b"msdos", b"exfat", b"ntfs"];
    const MAYBE_MAPPED_FS: [&[u8]; 6] = [
        b"nfs", b"smbfs", b"afpfs", b"webdav", b"macfuse", b"osxfuse",
    ];
    let mut st = std::mem::MaybeUninit::<libc::statfs>::uninit();
    if unsafe { libc::fstatfs(fd.as_raw_fd(), st.as_mut_ptr()) } != 0 {
        return FsOwners::Stored;
    }
    let st = unsafe { st.assume_init() };
    let name = unsafe { CStr::from_ptr(st.f_fstypename.as_ptr()) }.to_bytes();
    if OWNERLESS_FS.contains(&name) {
        FsOwners::None
    } else if MAYBE_MAPPED_FS.contains(&name) {
        FsOwners::MayBeMapped
    } else {
        FsOwners::Stored
    }
}

/// Without the filesystem type, owners are taken to be stored: only pax's
/// own are trusted.
#[cfg(not(any(target_os = "linux", target_os = "macos")))]
pub(crate) fn fs_owners(_fd: BorrowedFd<'_>) -> FsOwners {
    FsOwners::Stored
}

/// How far pax trusts an object `made_by_us` accepted.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum MadeTrust {
    /// Owned by pax's effective user, or on a filesystem that stores no
    /// owners: every attribute applies.
    Full,
    /// Accepted only because it is owned like its parent, on a filesystem
    /// that may store owners for real: pax uses it, but gives it no owner and
    /// no mode.
    ParentOwnerOnly,
}

/// What `fstat` (on a descriptor pax holds for it) reports about an object
/// pax has just made.
#[derive(Clone, Copy)]
pub(crate) struct MadeObject {
    pub uid: u32,
    pub nlink: u64,
    pub is_dir: bool,
    pub owners: FsOwners,
}

impl MadeObject {
    /// `st` of an object on a filesystem keeping owners as `owners`.
    fn of(st: &libc::stat, owners: FsOwners) -> Self {
        // Cast needed: `nlink_t` is u16 on macOS and u64 on Linux.
        #[allow(clippy::unnecessary_cast)]
        let nlink = st.st_nlink as u64;
        MadeObject {
            uid: st.st_uid,
            nlink,
            is_dir: st.st_mode & libc::S_IFMT == libc::S_IFDIR,
            owners,
        }
    }
}

/// Whether `made`, found where pax has just made an object in a directory
/// owned by `parent_uid`, can be the object pax made rather than one swapped
/// in by someone else -- and if so, how far it is trusted.
///
/// Owned by pax's effective user: accepted, in full. Otherwise only in a
/// parent pax's user does not own, and only when the object is owned like
/// that parent, on a filesystem whose type says owners may not be what each
/// creator was: in full where no owner is stored at all, and as
/// `ParentOwnerOnly` where owners may be mapped or may be real. On every other
/// filesystem only pax's effective user is accepted: no one can give a file
/// away, a directory cannot be hard-linked, and a hard link to one of pax's
/// user's nodes fails the link count.
///
/// Link count: a fresh symbolic link or special file has exactly one; a fresh
/// directory has two (itself and its `.`), or one on filesystems that do not
/// count directory links (btrfs, some FUSE).
pub(crate) fn made_by_us(made: MadeObject, parent_uid: u32, euid: u32) -> Option<MadeTrust> {
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
    let owned_like_parent = parent_uid != euid && made.uid == parent_uid;
    match (made.owners, owned_like_parent) {
        (FsOwners::None, true) => Some(MadeTrust::Full),
        (FsOwners::MayBeMapped, true) => Some(MadeTrust::ParentOwnerOnly),
        _ => None,
    }
}

/// The error for a name that no longer holds what pax made there.
pub(crate) fn replaced() -> io::Error {
    io::Error::other("replaced after it was made")
}

/// `fstat` of a descriptor.
fn fstat(fd: BorrowedFd<'_>) -> io::Result<libc::stat> {
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::fstat(fd.as_raw_fd(), &mut st) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(st)
}

/// `Ok(())` for a successful libc call's return value, the error otherwise.
pub(crate) fn cvt(r: libc::c_int) -> io::Result<()> {
    if r != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

/// How far a node of type `made_type` (`S_IFIFO`, ...), found with `st` on a
/// filesystem keeping owners as `owners`, is trusted to be the one pax just
/// made in `dirfd`.
fn check_node(
    st: &libc::stat,
    made_type: libc::mode_t,
    dirfd: BorrowedFd<'_>,
    owners: FsOwners,
) -> io::Result<MadeTrust> {
    if st.st_mode & libc::S_IFMT != made_type {
        return Err(replaced());
    }
    let parent_uid = fstat(dirfd)?.st_uid;
    let euid = unsafe { libc::geteuid() };
    made_by_us(MadeObject::of(st, owners), parent_uid, euid).ok_or_else(replaced)
}

#[cfg(target_os = "linux")]
pub(crate) use linux::MadeNode;
#[cfg(not(target_os = "linux"))]
pub(crate) use other::MadeNode;

/// Linux: the node is pinned by an `O_PATH | O_NOFOLLOW` descriptor, which
/// opens nothing on a FIFO or device and refuses nothing either; every check
/// reads it and every change goes through it.
#[cfg(target_os = "linux")]
mod linux {
    use super::*;
    use std::ffi::CString;
    use std::fs::File;
    use std::marker::PhantomData;
    use std::os::fd::{AsFd, FromRawFd, OwnedFd};

    /// A FIFO, device or symbolic link pax has just made, held by descriptor.
    pub(crate) struct MadeNode<'a> {
        fd: OwnedFd,
        symlink: bool,
        trust: MadeTrust,
        _dir: PhantomData<BorrowedFd<'a>>,
    }

    impl<'a> MadeNode<'a> {
        /// Pin `name` below `dirfd`, just made as a node of type `made_type`,
        /// and check it is that node.
        pub(crate) fn pin(
            dirfd: BorrowedFd<'a>,
            name: &'a CStr,
            made_type: libc::mode_t,
        ) -> io::Result<Self> {
            let flags = libc::O_PATH | libc::O_NOFOLLOW | libc::O_CLOEXEC;
            let fd = unsafe { libc::openat(dirfd.as_raw_fd(), name.as_ptr(), flags) };
            if fd < 0 {
                return Err(io::Error::last_os_error());
            }
            let fd = unsafe { OwnedFd::from_raw_fd(fd) };
            let st = fstat(fd.as_fd())?;
            let trust = check_node(&st, made_type, dirfd, fs_owners(fd.as_fd()))?;
            Ok(MadeNode {
                fd,
                symlink: made_type == libc::S_IFLNK,
                trust,
                _dir: PhantomData,
            })
        }

        pub(crate) fn trust(&self) -> MadeTrust {
            self.trust
        }

        pub(crate) fn chown(&self, uid: libc::uid_t, gid: libc::gid_t) -> io::Result<()> {
            let fd = self.fd.as_raw_fd();
            cvt(unsafe { libc::fchownat(fd, c"".as_ptr(), uid, gid, libc::AT_EMPTY_PATH) })
        }

        /// The mode of a FIFO or device; never called for a symbolic link.
        pub(crate) fn chmod(&self, mode: libc::mode_t) -> io::Result<()> {
            chmod_pinned(self.fd.as_fd(), mode)
        }

        /// `utimensat` with `AT_EMPTY_PATH` (Linux 5.8 and later). Before
        /// that, a FIFO or device through `/proc/self/fd`, which names exactly
        /// the pinned inode; a symbolic link has no such route, since that
        /// path is followed to the link and then through it.
        pub(crate) fn utimens(&self, times: &[libc::timespec; 2]) -> io::Result<()> {
            let fd = self.fd.as_raw_fd();
            let r =
                unsafe { libc::utimensat(fd, c"".as_ptr(), times.as_ptr(), libc::AT_EMPTY_PATH) };
            let err = match cvt(r) {
                Ok(()) => return Ok(()),
                Err(e) => e,
            };
            if err.raw_os_error() != Some(libc::EINVAL) || self.symlink {
                return Err(err);
            }
            let proc_dir = procfs_dir()?;
            let pinned = proc_fd_name(fd);
            cvt(unsafe {
                libc::utimensat(proc_dir.as_raw_fd(), pinned.as_ptr(), times.as_ptr(), 0)
            })
        }
    }

    /// The `fchmodat2` system call number (Linux 6.6 and later), the generic
    /// 452, spelled here because `libc` does not export it for every target.
    /// Not on x32, nor on the architectures with tables of their own: there
    /// only the procfs path is used.
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

    /// Set the mode of the inode an `O_PATH` descriptor pins, which `fchmod`
    /// refuses.
    ///
    /// First `fchmodat2(fd, "", mode, AT_EMPTY_PATH)`. Where it does not
    /// exist (ENOSYS), does not take `AT_EMPTY_PATH` (EINVAL), or is refused
    /// by a seccomp filter that does not know it (EPERM), `fchmodat` on
    /// `self/fd/N` relative to a `/proc` verified to be procfs, which names
    /// the same inode; a genuine EPERM comes back from that call too. With
    /// neither, the mode is not set and the failure is reported: a by-name
    /// fallback could act on whatever the name holds by then.
    pub(super) fn chmod_pinned(fd: BorrowedFd<'_>, mode: libc::mode_t) -> io::Result<()> {
        if let Some(sys_fchmodat2) = SYS_FCHMODAT2 {
            let r = unsafe {
                libc::syscall(
                    sys_fchmodat2,
                    fd.as_raw_fd(),
                    c"".as_ptr(),
                    libc::c_uint::from(mode),
                    libc::AT_EMPTY_PATH,
                )
            };
            if r == 0 {
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
        let pinned = proc_fd_name(fd.as_raw_fd());
        cvt(unsafe { libc::fchmodat(proc_dir.as_raw_fd(), pinned.as_ptr(), mode, 0) })
    }

    /// `/proc`, opened and verified to be procfs (`PROC_SUPER_MAGIC`), so
    /// that `self/fd/N` names exactly the inode open on descriptor N rather
    /// than whatever else is mounted or planted there.
    pub(crate) fn procfs_dir() -> io::Result<File> {
        const PROC_SUPER_MAGIC: u32 = 0x9fa0;
        let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC;
        let fd = unsafe { libc::open(c"/proc".as_ptr(), flags) };
        if fd < 0 {
            return Err(io::Error::last_os_error());
        }
        let dir = unsafe { File::from_raw_fd(fd) };
        let mut st = std::mem::MaybeUninit::<libc::statfs>::uninit();
        cvt(unsafe { libc::fstatfs(dir.as_raw_fd(), st.as_mut_ptr()) })?;
        // `f_type` is a signed word whose width varies by architecture; the
        // magic is its low bits.
        if unsafe { st.assume_init() }.f_type as u32 != PROC_SUPER_MAGIC {
            return Err(io::Error::other("/proc is not a procfs mount"));
        }
        Ok(dir)
    }

    /// `self/fd/N`, relative to `procfs_dir`.
    pub(crate) fn proc_fd_name(fd: libc::c_int) -> CString {
        CString::new(format!("self/fd/{fd}")).expect("a formatted number has no NUL")
    }
}

/// Elsewhere there is no `O_PATH`, and opening a device to pin it can act on
/// the device. The node is checked by `lstat` when it is made and changed by
/// name with `AT_SYMLINK_NOFOLLOW`, each change only after `lstat` shows the
/// name still holds the same node. The residual is a replacement between that
/// check and the call it guards.
#[cfg(not(target_os = "linux"))]
mod other {
    use super::*;

    /// A FIFO, device or symbolic link pax has just made, held by name and
    /// identity.
    pub(crate) struct MadeNode<'a> {
        dirfd: BorrowedFd<'a>,
        name: &'a CStr,
        made_type: libc::mode_t,
        id: (u64, u64),
        trust: MadeTrust,
    }

    impl<'a> MadeNode<'a> {
        /// Check that `name` below `dirfd` is the node of type `made_type`
        /// just made there, and remember which node that is.
        pub(crate) fn pin(
            dirfd: BorrowedFd<'a>,
            name: &'a CStr,
            made_type: libc::mode_t,
        ) -> io::Result<Self> {
            let st = lstat_at(dirfd, name)?;
            let trust = check_node(&st, made_type, dirfd, fs_owners(dirfd))?;
            Ok(MadeNode {
                dirfd,
                name,
                made_type,
                id: crate::modes::anchored::file_id(&st),
                trust,
            })
        }

        pub(crate) fn trust(&self) -> MadeTrust {
            self.trust
        }

        /// Whether the name still holds the node that was checked.
        fn still_made(&self) -> io::Result<()> {
            let st = lstat_at(self.dirfd, self.name)?;
            let same = crate::modes::anchored::file_id(&st) == self.id
                && st.st_mode & libc::S_IFMT == self.made_type;
            if !same {
                return Err(replaced());
            }
            Ok(())
        }

        pub(crate) fn chown(&self, uid: libc::uid_t, gid: libc::gid_t) -> io::Result<()> {
            self.still_made()?;
            let flags = libc::AT_SYMLINK_NOFOLLOW;
            let (dirfd, name) = (self.dirfd.as_raw_fd(), self.name.as_ptr());
            cvt(unsafe { libc::fchownat(dirfd, name, uid, gid, flags) })
        }

        pub(crate) fn chmod(&self, mode: libc::mode_t) -> io::Result<()> {
            self.still_made()?;
            let flags = libc::AT_SYMLINK_NOFOLLOW;
            let (dirfd, name) = (self.dirfd.as_raw_fd(), self.name.as_ptr());
            cvt(unsafe { libc::fchmodat(dirfd, name, mode, flags) })
        }

        pub(crate) fn utimens(&self, times: &[libc::timespec; 2]) -> io::Result<()> {
            self.still_made()?;
            let flags = libc::AT_SYMLINK_NOFOLLOW;
            let (dirfd, name) = (self.dirfd.as_raw_fd(), self.name.as_ptr());
            cvt(unsafe { libc::utimensat(dirfd, name, times.as_ptr(), flags) })
        }
    }

    /// `fstatat` with `AT_SYMLINK_NOFOLLOW`.
    fn lstat_at(dirfd: BorrowedFd<'_>, name: &CStr) -> io::Result<libc::stat> {
        let mut st: libc::stat = unsafe { std::mem::zeroed() };
        let flags = libc::AT_SYMLINK_NOFOLLOW;
        cvt(unsafe { libc::fstatat(dirfd.as_raw_fd(), name.as_ptr(), &mut st, flags) })?;
        Ok(st)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const PAX: u32 = 1000;
    const OTHER: u32 = 2000;

    fn made(uid: u32, nlink: u64, is_dir: bool, owners: FsOwners) -> MadeObject {
        MadeObject {
            uid,
            nlink,
            is_dir,
            owners,
        }
    }

    #[test]
    fn trusts_what_pax_made() {
        let full = Some(MadeTrust::Full);
        assert_eq!(
            made_by_us(made(PAX, 1, false, FsOwners::Stored), PAX, PAX),
            full
        );
        assert_eq!(
            made_by_us(made(PAX, 2, true, FsOwners::Stored), OTHER, PAX),
            full
        );
        assert_eq!(
            made_by_us(made(PAX, 1, true, FsOwners::Stored), PAX, PAX),
            full
        );
    }

    /// A hard link to a file -- anyone's -- has two links.
    #[test]
    fn refuses_a_hard_link() {
        assert_eq!(
            made_by_us(made(PAX, 2, false, FsOwners::Stored), PAX, PAX),
            None
        );
        assert_eq!(
            made_by_us(made(OTHER, 2, false, FsOwners::None), OTHER, PAX),
            None
        );
    }

    /// A directory with a subdirectory in it is not one just made.
    #[test]
    fn refuses_a_populated_directory() {
        assert_eq!(
            made_by_us(made(PAX, 3, true, FsOwners::Stored), PAX, PAX),
            None
        );
    }

    #[test]
    fn refuses_another_users_object() {
        assert_eq!(
            made_by_us(made(OTHER, 1, false, FsOwners::Stored), PAX, PAX),
            None
        );
        // Owned like a parent pax does own: someone else's, wherever.
        let mapped = FsOwners::MayBeMapped;
        assert_eq!(made_by_us(made(OTHER, 1, false, mapped), PAX, PAX), None);
        // Owned like the parent, where owners are stored for real.
        assert_eq!(
            made_by_us(made(OTHER, 1, false, FsOwners::Stored), OTHER, PAX),
            None
        );
    }

    #[test]
    fn trusts_the_parents_owner_only_where_owners_may_not_be_real() {
        let mapped = made(OTHER, 1, false, FsOwners::MayBeMapped);
        assert_eq!(
            made_by_us(mapped, OTHER, PAX),
            Some(MadeTrust::ParentOwnerOnly)
        );
        let ownerless = made(OTHER, 1, false, FsOwners::None);
        assert_eq!(made_by_us(ownerless, OTHER, PAX), Some(MadeTrust::Full));
    }
}
