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
use plib::madefs::{fs_owners, made_by_us, others_can_rename, FsOwners, MadeObject};
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

/// Check a directory pax has just made with `mkdirat` in `parent` and then
/// opened as `dir` (`plib::madefs::verify_made_dir`, which cp uses too):
/// `None` when it is not the directory made. Under test the trust can be
/// forced (`race_hook::with_dir_trust`), since no filesystem whose owners
/// may be mapped can be had there.
pub(crate) fn verify_made_dir(
    parent: BorrowedFd<'_>,
    dir: BorrowedFd<'_>,
) -> io::Result<Option<MadeTrust>> {
    #[cfg(test)]
    if let Some(trust) = crate::modes::race_hook::forced_dir_trust() {
        return Ok(Some(trust));
    }
    plib::madefs::verify_made_dir(parent.as_raw_fd(), dir.as_raw_fd())
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
    // plib::madefs too.
    const PAX: u32 = 1000;

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

    // others_can_rename, verify_made_dir, the read lend and what a directory
    // found existing may be given (ChainTrust::found_dir) are tested in
    // plib::madefs.

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
