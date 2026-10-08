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
//! The trust rules and the pinned-inode primitives are cp's, shared through
//! `plib::madefs`.

pub(crate) use plib::madefs::MadeTrust;
use plib::madefs::{fs_owners, made_by_us, FsOwners, MadeObject};
#[cfg(target_os = "linux")]
pub(crate) use plib::madefs::{proc_fd_name, procfs_dir};
use std::ffi::CStr;
use std::io;
use std::os::fd::{AsRawFd, BorrowedFd};

/// What `st` says about an object pax has just made, on a filesystem keeping
/// owners as `owners`.
fn made_object(st: &libc::stat, owners: FsOwners) -> MadeObject {
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

/// How the filesystem holding `fd` keeps owners.
fn owners_of(fd: BorrowedFd<'_>) -> FsOwners {
    fs_owners(fd.as_raw_fd())
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
    let type_ok = st.st_mode & libc::S_IFMT == made_type;
    let parent = fstat(dirfd)?;
    let euid = unsafe { libc::geteuid() };
    node_trust(made_object(st, owners), type_ok, &parent, euid).ok_or_else(replaced)
}

/// How far `made`, found of the type made (`type_ok`) where pax running as
/// `euid` has just made a node in `parent`, is trusted to be that node.
///
/// Where nobody but pax's user can rename entries in the parent, nobody else
/// can have put anything at the name: what is there is pax's, whoever the
/// filesystem says owns it (`nobody`, on an export that squashes root).
/// Otherwise `made_by_us` decides.
pub(crate) fn node_trust(
    made: MadeObject,
    type_ok: bool,
    parent: &libc::stat,
    euid: u32,
) -> Option<MadeTrust> {
    if !type_ok {
        return None;
    }
    if !others_can_rename(parent, euid) {
        return Some(MadeTrust::Full);
    }
    made_by_us(made, Some(parent.st_uid), euid)
}

/// Whether anyone but `euid` can rename entries in the directory `parent`:
/// its owner, when that is someone else, and anyone with group or other write
/// permission on it when it is not sticky. (Group or other write permission
/// granted by an ACL shows in the group bits.)
pub(crate) fn others_can_rename(parent: &libc::stat, euid: u32) -> bool {
    // Cast needed: `mode_t` is u16 on macOS and u32 on Linux. S_ISVTX is
    // 0o1000, S_IWGRP|S_IWOTH 0o022 (fixed by POSIX).
    #[allow(clippy::unnecessary_cast)]
    let mode = parent.st_mode as u32;
    parent.st_uid != euid || (mode & 0o022 != 0 && mode & 0o1000 == 0)
}

/// Whether a directory found already existing in the directory `parent` --
/// not one this run made and verified -- may be given a member's owner, mode
/// and times by pax running as `euid`.
///
/// Only where nobody but `euid` can create entries in `parent`: it is owned
/// by `euid` and grants no group or other write permission (an ACL granting
/// it shows in the group bits). Anyone who can create entries there can
/// create the member's name before pax does -- renaming to it a directory of
/// their choosing, the user's own private one included -- and the sticky bit
/// does not stop that. Who owns the directory found proves nothing. Where
/// others can, it is extracted into (POSIX: an existing directory is not an
/// error) but keeps its own attributes.
pub(crate) fn may_take_attrs(parent: &libc::stat, euid: u32) -> bool {
    // Cast needed: `mode_t` is u16 on macOS and u32 on Linux. S_IWGRP|S_IWOTH
    // is 0o022 (fixed by POSIX).
    #[allow(clippy::unnecessary_cast)]
    let mode = parent.st_mode as u32;
    parent.st_uid == euid && mode & 0o022 == 0
}

/// `may_take_attrs` for a directory found in `parent`, as its descriptor
/// shows it.
pub(crate) fn found_dir_may_take_attrs(parent: BorrowedFd<'_>) -> io::Result<bool> {
    let euid = unsafe { libc::geteuid() };
    Ok(may_take_attrs(&fstat(parent)?, euid))
}

/// Check a directory pax has just made with `mkdirat` in `parent` and then
/// opened as `dir` (`O_DIRECTORY | O_NOFOLLOW`): between the two, anyone else
/// who can rename entries in the parent could have renamed a directory of
/// their choosing over it, and pax would extract into it and give it the
/// member's owner and mode.
///
/// Only the parent's owner, and anyone with group or other write permission
/// on it when it is not sticky, can do that; when that is nobody but pax's own
/// user there is nothing to check. Otherwise the directory must be what a
/// fresh `mkdirat` yields: empty, with the owner and link count `made_by_us`
/// accepts. `None` when it is not.
///
/// Every fact comes from the two descriptors, never from a name: the caller
/// goes on to use `dir` itself, or identifies the directory by `dir`'s fstat.
pub(crate) fn verify_made_dir(
    parent: BorrowedFd<'_>,
    dir: BorrowedFd<'_>,
) -> io::Result<Option<MadeTrust>> {
    #[cfg(test)]
    if let Some(trust) = crate::modes::race_hook::forced_dir_trust() {
        return Ok(Some(trust));
    }
    let euid = unsafe { libc::geteuid() };
    let parent_st = fstat(parent)?;
    if !others_can_rename(&parent_st, euid) {
        return Ok(Some(MadeTrust::Full));
    }
    let st = fstat(dir)?;
    let made = made_object(&st, owners_of(dir));
    let Some(trust) = made_by_us(made, Some(parent_st.st_uid), euid) else {
        return Ok(None);
    };
    if !made.is_dir || !is_empty_made_dir(dir, &st, euid)? {
        return Ok(None);
    }
    Ok(Some(trust))
}

/// Whether the directory open on `dir`, with `st`, lists nothing but `.` and
/// `..`.
///
/// Reading it takes the owner's read and search permission, which a umask
/// (0400, say) may have withheld from a directory pax has just made. When pax
/// owns it they are lent for the check, through the descriptor, and the mode
/// is put back.
fn is_empty_made_dir(dir: BorrowedFd<'_>, st: &libc::stat, euid: u32) -> io::Result<bool> {
    let mode = st.st_mode & 0o7777;
    let lend = st.st_uid == euid && mode & 0o500 != 0o500;
    if lend {
        chmod_fd(dir, mode | 0o700)?;
    }
    let empty = ftw::is_empty_dir_fd(dir.as_raw_fd());
    if lend {
        chmod_fd(dir, mode)?;
    }
    empty
}

/// Set the mode of the file open on `fd`, which may be held for search only
/// (`O_PATH` on Linux, where `fchmod` refuses it).
#[cfg(target_os = "linux")]
fn chmod_fd(fd: BorrowedFd<'_>, mode: libc::mode_t) -> io::Result<()> {
    plib::madefs::chmod_pinned(fd.as_raw_fd(), mode)
}

/// Elsewhere a search-only descriptor (`O_SEARCH`) takes `fchmod`.
#[cfg(not(target_os = "linux"))]
fn chmod_fd(fd: BorrowedFd<'_>, mode: libc::mode_t) -> io::Result<()> {
    cvt(unsafe { libc::fchmod(fd.as_raw_fd(), mode) })
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
    use crate::modes::anchored::file_id;
    use plib::madefs::{chmod_pinned, utimens_link_if_still};
    use std::os::fd::{AsFd, FromRawFd, OwnedFd};

    /// A FIFO, device or symbolic link pax has just made, held by descriptor.
    pub(crate) struct MadeNode<'a> {
        fd: OwnedFd,
        symlink: bool,
        trust: MadeTrust,
        /// Where it was made, for the one by-name fallback (`utimens`).
        dirfd: BorrowedFd<'a>,
        name: &'a CStr,
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
            let trust = check_node(&st, made_type, dirfd, owners_of(fd.as_fd()))?;
            Ok(MadeNode {
                fd,
                symlink: made_type == libc::S_IFLNK,
                trust,
                dirfd,
                name,
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
            chmod_pinned(self.fd.as_raw_fd(), mode)
        }

        /// `utimensat` with `AT_EMPTY_PATH` (Linux 5.8 and later). Before
        /// that -- on EINVAL from that call, and only then -- a FIFO or device
        /// through `/proc/self/fd`, which names exactly the pinned inode. A
        /// symbolic link has no such route -- that path is followed to the
        /// link and then through it -- so its times go by name, only once a
        /// fresh `lstat` shows the name still holds the pinned link
        /// (`utimens_link_if_still`). The residual window is between that
        /// `lstat` and the `utimensat`: a hard link to another file swapped in
        /// then takes the link's times -- a wrong mtime, at worst.
        pub(crate) fn utimens(&self, times: &[libc::timespec; 2]) -> io::Result<()> {
            let fd = self.fd.as_raw_fd();
            let r =
                unsafe { libc::utimensat(fd, c"".as_ptr(), times.as_ptr(), libc::AT_EMPTY_PATH) };
            let err = match cvt(r) {
                Ok(()) => return Ok(()),
                Err(e) => e,
            };
            if err.raw_os_error() != Some(libc::EINVAL) {
                return Err(err);
            }
            if self.symlink {
                let pinned = file_id(&fstat(self.fd.as_fd())?);
                let dirfd = self.dirfd.as_raw_fd();
                return match utimens_link_if_still(dirfd, self.name, pinned, times)? {
                    true => Ok(()),
                    false => Err(replaced()),
                };
            }
            let proc_dir = procfs_dir()?;
            let pinned = proc_fd_name(fd);
            cvt(unsafe {
                libc::utimensat(proc_dir.as_raw_fd(), pinned.as_ptr(), times.as_ptr(), 0)
            })
        }
    }
}

/// Elsewhere there is no `O_PATH`, so a node is pinned by an ordinary
/// descriptor where opening it has no effect beyond the open: a FIFO opened
/// for reading without blocking, and on macOS a symbolic link opened as itself
/// (`O_SYMLINK`). The `lstat` that comes first only says what to open; the
/// checks read the descriptor, which must be the node that `lstat` saw, and
/// every change goes through it.
///
/// A device -- whose open can act on the device -- a symbolic link on the
/// other BSDs, and a FIFO whose own mode denies its owner reading, cannot be
/// held that way. Those are checked by `lstat` and changed by name with
/// `AT_SYMLINK_NOFOLLOW`, each change only after `lstat` shows the name still
/// holds the same node. That residual is a replacement in the moment between
/// such a check and the call it guards; its worst case is the one this module
/// exists to prevent -- a hard link swapped in at that moment takes the
/// owner, mode or times then being applied -- and it needs a writer of the
/// directory to win that window on the call it aims at.
#[cfg(not(target_os = "linux"))]
mod other {
    use super::*;
    use crate::modes::anchored::file_id;
    use std::os::fd::{AsFd, FromRawFd, OwnedFd};

    /// How a made node is held.
    enum Held<'a> {
        /// By a descriptor for the node itself.
        Fd(OwnedFd),
        /// By name and identity, re-checked before each change.
        Name {
            dirfd: BorrowedFd<'a>,
            name: &'a CStr,
            made_type: libc::mode_t,
            id: (u64, u64),
        },
    }

    /// A FIFO, device or symbolic link pax has just made.
    pub(crate) struct MadeNode<'a> {
        held: Held<'a>,
        trust: MadeTrust,
    }

    impl<'a> MadeNode<'a> {
        /// Check that `name` below `dirfd` is the node of type `made_type`
        /// just made there, and hold on to it.
        pub(crate) fn pin(
            dirfd: BorrowedFd<'a>,
            name: &'a CStr,
            made_type: libc::mode_t,
        ) -> io::Result<Self> {
            let st = lstat_at(dirfd, name)?;
            if st.st_mode & libc::S_IFMT != made_type {
                return Err(replaced());
            }
            if let Some(fd) = open_node(dirfd, name, made_type)? {
                let held = fstat(fd.as_fd())?;
                if file_id(&held) != file_id(&st) {
                    return Err(replaced());
                }
                let trust = check_node(&held, made_type, dirfd, owners_of(fd.as_fd()))?;
                let held = Held::Fd(fd);
                return Ok(MadeNode { held, trust });
            }
            let trust = check_node(&st, made_type, dirfd, owners_of(dirfd))?;
            let id = file_id(&st);
            let held = Held::Name {
                dirfd,
                name,
                made_type,
                id,
            };
            Ok(MadeNode { held, trust })
        }

        pub(crate) fn trust(&self) -> MadeTrust {
            self.trust
        }

        pub(crate) fn chown(&self, uid: libc::uid_t, gid: libc::gid_t) -> io::Result<()> {
            let flags = libc::AT_SYMLINK_NOFOLLOW;
            match self.held {
                Held::Fd(ref fd) => cvt(unsafe { libc::fchown(fd.as_raw_fd(), uid, gid) }),
                Held::Name { dirfd, name, .. } => {
                    self.still_made()?;
                    let (dirfd, name) = (dirfd.as_raw_fd(), name.as_ptr());
                    cvt(unsafe { libc::fchownat(dirfd, name, uid, gid, flags) })
                }
            }
        }

        pub(crate) fn chmod(&self, mode: libc::mode_t) -> io::Result<()> {
            let flags = libc::AT_SYMLINK_NOFOLLOW;
            match self.held {
                Held::Fd(ref fd) => cvt(unsafe { libc::fchmod(fd.as_raw_fd(), mode) }),
                Held::Name { dirfd, name, .. } => {
                    self.still_made()?;
                    let (dirfd, name) = (dirfd.as_raw_fd(), name.as_ptr());
                    cvt(unsafe { libc::fchmodat(dirfd, name, mode, flags) })
                }
            }
        }

        pub(crate) fn utimens(&self, times: &[libc::timespec; 2]) -> io::Result<()> {
            let flags = libc::AT_SYMLINK_NOFOLLOW;
            match self.held {
                Held::Fd(ref fd) => cvt(unsafe { libc::futimens(fd.as_raw_fd(), times.as_ptr()) }),
                Held::Name { dirfd, name, .. } => {
                    self.still_made()?;
                    let (dirfd, name) = (dirfd.as_raw_fd(), name.as_ptr());
                    cvt(unsafe { libc::utimensat(dirfd, name, times.as_ptr(), flags) })
                }
            }
        }

        /// For a node held by name: whether the name still holds it.
        fn still_made(&self) -> io::Result<()> {
            let Held::Name {
                dirfd,
                name,
                made_type,
                id,
            } = self.held
            else {
                return Ok(());
            };
            let st = lstat_at(dirfd, name)?;
            if file_id(&st) != id || st.st_mode & libc::S_IFMT != made_type {
                return Err(replaced());
            }
            Ok(())
        }
    }

    /// A descriptor for the node of type `made_type` at `name`, where one can
    /// be had without acting on anything; `None` where it cannot.
    ///
    /// `O_NONBLOCK` and `O_NOCTTY` whatever is asked for: what is opened may
    /// by now be something else, and the caller's identity check refuses it,
    /// but the open itself must not wait on a FIFO or adopt a terminal.
    fn open_node(
        dirfd: BorrowedFd<'_>,
        name: &CStr,
        made_type: libc::mode_t,
    ) -> io::Result<Option<OwnedFd>> {
        let base = libc::O_RDONLY | libc::O_NONBLOCK | libc::O_NOCTTY | libc::O_CLOEXEC;
        let flags = match made_type {
            libc::S_IFIFO => base | libc::O_NOFOLLOW,
            #[cfg(target_os = "macos")]
            libc::S_IFLNK => base | libc::O_SYMLINK,
            _ => return Ok(None),
        };
        let fd = unsafe { libc::openat(dirfd.as_raw_fd(), name.as_ptr(), flags) };
        if fd >= 0 {
            return Ok(Some(unsafe { OwnedFd::from_raw_fd(fd) }));
        }
        let err = io::Error::last_os_error();
        if err.raw_os_error().is_some_and(held_by_name_after) {
            return Ok(None);
        }
        Err(err)
    }

    /// Whether `open_node` failing with `errno` means the node is to be held
    /// by name instead -- re-checked by `lstat` before each change, as a
    /// device is. EACCES: a mode denying the owner reading. ENOTSUP and
    /// EOPNOTSUPP: a filesystem, network ones especially, that does not
    /// support opening a symbolic link as itself.
    fn held_by_name_after(errno: libc::c_int) -> bool {
        errno == libc::EACCES || errno == libc::ENOTSUP || errno == libc::EOPNOTSUPP
    }

    /// `fstatat` with `AT_SYMLINK_NOFOLLOW`.
    fn lstat_at(dirfd: BorrowedFd<'_>, name: &CStr) -> io::Result<libc::stat> {
        let mut st: libc::stat = unsafe { std::mem::zeroed() };
        let flags = libc::AT_SYMLINK_NOFOLLOW;
        cvt(unsafe { libc::fstatat(dirfd.as_raw_fd(), name.as_ptr(), &mut st, flags) })?;
        Ok(st)
    }

    #[cfg(test)]
    mod tests {
        use super::held_by_name_after;

        /// A node that cannot be opened to be held -- a FIFO whose mode
        /// denies its owner reading, a symbolic link on a network filesystem
        /// that does not support `O_SYMLINK` -- is held by name, re-checked
        /// before each change; any other failure is one.
        #[test]
        fn falls_back_to_name_where_the_node_cannot_be_opened() {
            assert!(held_by_name_after(libc::EACCES));
            assert!(held_by_name_after(libc::ENOTSUP));
            assert!(held_by_name_after(libc::EOPNOTSUPP));
            assert!(!held_by_name_after(libc::ENOENT));
            assert!(!held_by_name_after(libc::ELOOP));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // made_by_us itself, and the by-name link times, are tested in
    // plib::madefs.
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

    /// A parent directory of `uid` with permission bits `mode`.
    fn parent(uid: u32, mode: libc::mode_t) -> libc::stat {
        let mut st: libc::stat = unsafe { std::mem::zeroed() };
        st.st_uid = uid;
        st.st_mode = libc::S_IFDIR | mode;
        st
    }

    /// Only the parent's owner, or anyone allowed to write a parent that is
    /// not sticky, can rename entries in it.
    #[test]
    fn who_can_rename_in_a_parent() {
        assert!(!others_can_rename(&parent(PAX, 0o755), PAX));
        assert!(!others_can_rename(&parent(PAX, 0o1777), PAX));
        assert!(others_can_rename(&parent(PAX, 0o775), PAX));
        assert!(others_can_rename(&parent(PAX, 0o777), PAX));
        assert!(others_can_rename(&parent(OTHER, 0o755), PAX));
    }

    /// An existing directory owned by someone other than pax's user and the
    /// parent's owner, in a parent others can rename entries in, may have
    /// been renamed there by them: it takes no member's attributes.
    #[test]
    fn a_found_directory_takes_attributes_only_where_nobody_else_can_create() {
        // Nobody but pax's user can create entries beside it.
        assert!(may_take_attrs(&parent(PAX, 0o755), PAX));
        assert!(may_take_attrs(&parent(0, 0o755), 0));
        // A sticky directory others may write: anyone can create the
        // member's name there first (/tmp, and root extracting into it).
        assert!(!may_take_attrs(&parent(PAX, 0o1777), PAX));
        assert!(!may_take_attrs(&parent(0, 0o1777), 0));
        // Group or other write permission.
        assert!(!may_take_attrs(&parent(PAX, 0o775), PAX));
        assert!(!may_take_attrs(&parent(PAX, 0o757), PAX));
        // Someone else's directory: its owner can.
        assert!(!may_take_attrs(&parent(OTHER, 0o755), PAX));
    }

    /// Where nobody but pax's user can rename entries in the parent, what is
    /// at the name is what pax made, whoever the filesystem says owns it --
    /// `nobody`, on an NFS export that squashes root.
    #[test]
    fn trusts_any_owner_where_nobody_else_can_rename() {
        const NOBODY: u32 = 65534;
        let squashed = made(NOBODY, 1, false, FsOwners::MayBeMapped);
        let full = Some(MadeTrust::Full);
        assert_eq!(node_trust(squashed, true, &parent(PAX, 0o755), PAX), full);
        assert_eq!(node_trust(squashed, true, &parent(PAX, 0o777), PAX), None);
        assert_eq!(node_trust(squashed, false, &parent(PAX, 0o755), PAX), None);
        let ours = made(PAX, 1, false, FsOwners::Stored);
        assert_eq!(node_trust(ours, true, &parent(PAX, 0o777), PAX), full);
        // A hard link to anyone's file, where others can rename: refused.
        let linked = made(PAX, 2, false, FsOwners::Stored);
        assert_eq!(node_trust(linked, true, &parent(PAX, 0o777), PAX), None);
    }
}
