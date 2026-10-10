//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Access control lists: reading a file's ACLs, writing them to a copy, and the forms they
//! travel in.
//!
//! POSIX.2024 has no ACL interface (POSIX.1e was withdrawn); XBD 4.7 leaves room for
//! "additional or alternate" access mechanisms, and this is the one each system has:
//! - Linux: POSIX.1e-style ACLs in the `system.posix_acl_access` and `system.posix_acl_default`
//!   extended attributes, read here without libacl (`posix`); NFSv4 and CIFS ACLs
//!   (`system.nfs4_acl`, `system.cifs_acl`) are kept as the bytes the filesystem hands out
//!   (`Native`).
//! - macOS: NFSv4-style extended ACLs, reached through the libSystem `acl(3)` calls and kept
//!   in their external form (`acl_copy_ext`).
//! - Anything else: no ACL is read or written (EOPNOTSUPP).
//!
//! An NFSv4-style ACL of either system travels in an archive as text (`nfs4`), and is written
//! back only where the same kind is held.

mod nfs4;
mod posix;

#[cfg(target_os = "macos")]
mod darwin;
#[cfg(target_os = "linux")]
mod linux;

#[cfg(target_os = "macos")]
use darwin as sys;
#[cfg(target_os = "linux")]
use linux as sys;

use nfs4::nfs4_is_trivial;
pub use nfs4::{Ace, AceKind, Named, Nfs4Acl, Who};
pub use posix::{Entry, PosixAcl, Tag};

use gettextrs::gettext;
use std::ffi::CStr;
#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::ffi::CString;
use std::io;
#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::RawFd;
use std::path::Path;

/// Which kind of ACL a `Native` one is: each can be written back only where the same kind is.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NativeKind {
    /// Linux `system.nfs4_acl`: the NFSv4 XDR the NFS client hands out.
    Nfs4,
    /// Linux `system.cifs_acl`: a Windows security descriptor, which every file on a CIFS
    /// mount has.
    Cifs,
    /// macOS extended ACL, in `acl_copy_ext` form.
    Darwin,
}

/// An ACL kept as the bytes its system hands out, read only to carry an NFSv4-style one as text
/// (`Acl::nfs4_text`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Native {
    pub kind: NativeKind,
    pub bytes: Vec<u8>,
}

/// The ACLs of one file.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Acl {
    pub access: Option<PosixAcl>,
    /// What new entries of a directory inherit; only a directory has one.
    pub default: Option<PosixAcl>,
    pub native: Option<Native>,
}

impl Acl {
    /// Whether the ACLs say nothing the mode does not (gnulib's `file_has_aclinfo`, the `+`
    /// of `ls -l`): no access ACL beyond owner, group and others, no default ACL, and no
    /// native ACL but a trivial NFSv4 one or a CIFS security descriptor -- which every file
    /// there has, so its being there tells nothing.
    pub fn is_trivial(&self) -> bool {
        self.access.as_ref().is_none_or(PosixAcl::is_trivial)
            && self.default.is_none()
            && self.native_is_trivial()
    }

    /// Whether the ACLs say exactly what the mode `mode` does, so that a file given that mode
    /// alone loses nothing of them: `is_trivial`, but an NFSv4 ACL only where it grants just
    /// what the mode does (`Nfs4Acl::says_only_mode`) -- an `everyone@` deny of what the mode
    /// grants others is not, though gnulib, and `ls`, count it trivial.
    pub fn is_trivial_for(&self, mode: u32) -> bool {
        self.access.as_ref().is_none_or(PosixAcl::is_trivial)
            && self.default.is_none()
            && self.native_trivial_for(mode)
    }

    /// Whether its native ACL, if any, says exactly what the mode `mode` does
    /// (`is_trivial_for`).
    fn native_trivial_for(&self, mode: u32) -> bool {
        match &self.native {
            None => true,
            Some(Native { kind, bytes }) => match kind {
                NativeKind::Nfs4 => {
                    Nfs4Acl::parse_xdr(bytes).is_ok_and(|acl| acl.says_only_mode(mode))
                }
                NativeKind::Cifs => true,
                NativeKind::Darwin => false,
            },
        }
    }

    /// Whether its native ACL, if any, may let anyone but the owner change the file
    /// (`Nfs4Acl::lets_others_change`); one not read for certain may. A CIFS security
    /// descriptor is never written, and lets nobody anything here.
    fn native_lets_others_change(&self) -> bool {
        let read = match &self.native {
            None => return false,
            Some(Native { kind, bytes }) => match kind {
                NativeKind::Nfs4 => Nfs4Acl::parse_xdr(bytes),
                NativeKind::Darwin => darwin_to_nfs4(bytes),
                NativeKind::Cifs => return false,
            },
        };
        read.map_or(true, |acl| acl.lets_others_change())
    }

    /// Whether its native ACL, if any, says nothing the mode does not (`is_trivial`).
    fn native_is_trivial(&self) -> bool {
        match &self.native {
            None => true,
            Some(Native { kind, bytes }) => match kind {
                NativeKind::Nfs4 => nfs4_is_trivial(bytes),
                NativeKind::Cifs => true,
                NativeKind::Darwin => false,
            },
        }
    }

    /// Its native ACL as the NFSv4 text a pax archive carries (`SCHILY.acl.ace`,
    /// `Nfs4Acl::to_ace_text`), where it says more than the mode `mode` does: a Linux NFSv4
    /// one as read, a macOS one followed by the entries libarchive adds to say what the mode
    /// does (`Nfs4Acl::with_mode_aces`). `None` for none, or for a CIFS security descriptor,
    /// which no text is made from.
    pub fn nfs4_text(&self, mode: u32) -> io::Result<Option<String>> {
        if self.native_trivial_for(mode) {
            return Ok(None);
        }
        let acl = match &self.native {
            Some(Native {
                kind: NativeKind::Nfs4,
                bytes,
            }) => Nfs4Acl::parse_xdr(bytes)?,
            Some(Native {
                kind: NativeKind::Darwin,
                bytes,
            }) => darwin_to_nfs4(bytes)?.with_mode_aces(mode),
            _ => return Ok(None),
        };
        acl.to_ace_text().map(Some)
    }
}

/// The NFSv4 ACL a pax archive carries as `text` (`Nfs4Acl::from_ace_text`), as the native ACL
/// this system holds it in: `system.nfs4_acl` XDR on Linux (`Nfs4Acl::to_xdr`), an extended
/// ACL on macOS -- whose `owner@`, `group@` and `everyone@` entries, which macOS has no place
/// for, are left out, as libarchive leaves them, where the mode `mode` says all they do
/// (`Nfs4Acl::specials_said_by_mode`). `None` for one that says exactly what `mode` does
/// (`Nfs4Acl::says_only_mode`). Elsewhere, or for one this system cannot hold, the failure
/// (EOPNOTSUPP on a system with no NFSv4-style ACL, or for an entry macOS cannot hold).
pub fn native_from_ace_text(text: &str, mode: u32) -> io::Result<Option<Native>> {
    let acl = Nfs4Acl::from_ace_text(text)?;
    if acl.says_only_mode(mode) {
        return Ok(None);
    }
    native_of(&acl, mode).map(Some)
}

/// `acl` as the native ACL this system holds it in, for a file of mode `mode`
/// (`native_from_ace_text`).
fn native_of(acl: &Nfs4Acl, mode: u32) -> io::Result<Native> {
    #[cfg(target_os = "linux")]
    {
        let _ = mode;
        Ok(Native {
            kind: NativeKind::Nfs4,
            bytes: acl.to_xdr(),
        })
    }
    #[cfg(target_os = "macos")]
    {
        Ok(Native {
            kind: NativeKind::Darwin,
            bytes: darwin::from_nfs4(acl, mode)?,
        })
    }
    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    {
        let _ = (acl, mode);
        Err(io::Error::from_raw_os_error(libc::EOPNOTSUPP))
    }
}

/// The macOS external ACL `bytes` as an NFSv4 ACL; only macOS reads one.
fn darwin_to_nfs4(bytes: &[u8]) -> io::Result<Nfs4Acl> {
    #[cfg(target_os = "macos")]
    {
        darwin::to_nfs4(bytes)
    }
    #[cfg(not(target_os = "macos"))]
    {
        let _ = bytes;
        Err(io::Error::from_raw_os_error(libc::EOPNOTSUPP))
    }
}

fn invalid(what: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, gettext(what))
}

/// Whether reading an attribute failed because there is none, or none can be: ENODATA, or
/// EOPNOTSUPP where the filesystem has no extended attributes.
fn absent(e: &io::Error) -> bool {
    e.raw_os_error()
        .is_some_and(|code| [libc::ENODATA, libc::EOPNOTSUPP, libc::ENOTSUP].contains(&code))
}

/// Whether an ACL of the directory open on `fd` may let others write it, `group_writable`
/// being whether its mode grants the group write permission:
/// - an NFSv4 or CIFS ACL (`system.nfs4_acl`, `system.nfs4_acl_xdr`, `system.cifs_acl`): its
///   attribute being there at all, its entries unread -- one may grant anyone write whatever
///   the mode shows;
/// - where the directory is group-writable, a POSIX access ACL (`system.posix_acl_access`)
///   naming a user or group, or not read for certain: such an entry shows in the mode as
///   group write permission (the ACL mask) and nowhere else.
///
/// Any other failure to read one counts as one that does. Residual: an ACL a server applies
/// that the client does not show as an attribute at all, which nothing here can see. Only
/// Linux ACLs are read (`read_xattr`); elsewhere this is always false.
pub fn lets_others_write(fd: RawFd, group_writable: bool) -> bool {
    lets_others_write_by(|name| read_xattr(fd, name), group_writable)
}

/// `lets_others_write`, `read` reading one extended attribute.
fn lets_others_write_by(read: impl Fn(&CStr) -> io::Result<Vec<u8>>, group_writable: bool) -> bool {
    const NFS4_OR_CIFS: [&CStr; 3] = [
        c"system.nfs4_acl",
        c"system.nfs4_acl_xdr",
        c"system.cifs_acl",
    ];
    for name in NFS4_OR_CIFS {
        match read(name) {
            Err(e) if absent(&e) => {}
            _ => return true,
        }
    }
    if !group_writable {
        return false;
    }
    match read(c"system.posix_acl_access") {
        Ok(xattr) => names_others(&xattr),
        Err(e) => !absent(&e),
    }
}

/// Whether the access ACL attribute `xattr` names a user or group besides the owner and the
/// owning group -- or is not one this reads for certain.
fn names_others(xattr: &[u8]) -> bool {
    PosixAcl::parse_xattr(xattr).map_or(true, |acl| acl.names_others())
}

/// The ACLs of the file open on `fd`.
///
/// An `O_PATH` descriptor (Linux) takes no `f*xattr` call (EBADF); its attributes are then
/// read through `/proc/self/fd/N`, once `/proc` is verified to be procfs
/// (`madefs::procfs_dir`), which names the same inode and needs no permission on it to read a
/// `system.` attribute. (The path is resolved again after the check: only root can mount
/// something else over `/proc`.) Without a procfs to read through, the read fails.
pub fn read_fd(fd: RawFd) -> io::Result<Acl> {
    sys::read(&sys::Target::Fd(fd))
}

/// The ACLs of the file `path` names; of a symbolic link itself unless `follow`.
pub fn read_path(path: &Path, follow: bool) -> io::Result<Acl> {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    {
        let path = CString::new(path.as_os_str().as_bytes())
            .map_err(|_| io::Error::from_raw_os_error(libc::EINVAL))?;
        sys::read(&sys::Target::Path(&path, follow))
    }
    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    {
        let _ = (path, follow);
        Err(io::Error::from_raw_os_error(libc::EOPNOTSUPP))
    }
}

/// The ACLs of the source open on `fd`, the descriptor its data or metadata came from, to copy
/// or archive them (`read_fd`). Where none can be read at all (EOPNOTSUPP: a system no ACL is
/// read on), it has none to lose.
pub fn read_source_fd(fd: RawFd) -> io::Result<Acl> {
    match read_fd(fd) {
        Err(e) if e.raw_os_error() == Some(libc::EOPNOTSUPP) => Ok(Acl::default()),
        read => read,
    }
}

/// The ACLs of the directory or special file a tree walk recorded at `entry`
/// (`read_source_fd`), read through a descriptor of its own, required to be the very file
/// recorded (`xattr::open_entry`); `None` when it is not.
pub fn read_entry(entry: &ftw::Entry) -> io::Result<Option<Acl>> {
    match crate::xattr::open_entry(entry)? {
        Some(fd) => read_opened(&fd).map(Some),
        None => Ok(None),
    }
}

/// The ACLs of the file `xattr::open_entry` opened (`read_source_fd`). A directory's are read
/// with plain `f*xattr` calls where it could be opened for reading; a special file's, and those
/// of a directory that could not (EACCES), only through procfs (Linux): without one a
/// directory's are not read, an error, and a special file's are taken to be none
/// (`xattr::no_procfs_route`), as it rarely has any.
pub fn read_opened(fd: &crate::xattr::EntryFd) -> io::Result<Acl> {
    use std::os::unix::io::AsRawFd;
    match read_source_fd(fd.as_raw_fd()) {
        Err(e) if fd.is_special() && crate::xattr::no_procfs_route(&e) => Ok(Acl::default()),
        read => read,
    }
}

/// The ACLs of a source and its other extended attributes, read together
/// (`read_source_attrs`).
#[derive(Debug, Default)]
pub struct SourceAttrs {
    pub acl: Acl,
    /// Each extended attribute `xattr::is_copied` admits: none of the ACL ones.
    pub xattrs: crate::xattr::Values,
}

/// `read_source_fd`, and where `xattrs` the source's other extended attributes with its ACLs,
/// from the one listing of its attributes that reading its ACLs takes on Linux: a file with
/// neither, the usual case, costs one call there.
pub fn read_source_attrs(fd: RawFd, xattrs: bool) -> io::Result<SourceAttrs> {
    match read_attrs(fd, xattrs) {
        Err(e) if e.raw_os_error() == Some(libc::EOPNOTSUPP) => Ok(SourceAttrs::default()),
        read => read,
    }
}

#[cfg(target_os = "linux")]
fn read_attrs(fd: RawFd, xattrs: bool) -> io::Result<SourceAttrs> {
    sys::read_attrs(fd, xattrs)
}

#[cfg(not(target_os = "linux"))]
fn read_attrs(fd: RawFd, xattrs: bool) -> io::Result<SourceAttrs> {
    Ok(SourceAttrs {
        acl: read_fd(fd)?,
        xattrs: match xattrs {
            true => crate::xattr::read_fd(fd)?,
            false => crate::xattr::Values::default(),
        },
    })
}

/// `read_source_attrs` of the file `xattr::open_entry` opened, as `read_opened` reads its
/// ACLs: a special file's, where there is no procfs to read them through, are none.
pub fn read_opened_attrs(fd: &crate::xattr::EntryFd, xattrs: bool) -> io::Result<SourceAttrs> {
    use std::os::unix::io::AsRawFd;
    match read_source_attrs(fd.as_raw_fd(), xattrs) {
        Err(e) if fd.is_special() && crate::xattr::no_procfs_route(&e) => {
            Ok(SourceAttrs::default())
        }
        read => read,
    }
}

/// `read_opened_attrs` of the file a tree walk recorded at `entry`, as `read_entry` reads its
/// ACLs; `None` when the file opened is not the one recorded.
pub fn read_entry_attrs(entry: &ftw::Entry, xattrs: bool) -> io::Result<Option<SourceAttrs>> {
    match crate::xattr::open_entry(entry)? {
        Some(fd) => read_opened_attrs(&fd, xattrs).map(Some),
        None => Ok(None),
    }
}

/// The extended attribute `name` of the file open on `fd`, `O_PATH` or not (`read_fd`).
/// Elsewhere than Linux none is read: EOPNOTSUPP.
pub fn read_xattr(fd: RawFd, name: &CStr) -> io::Result<Vec<u8>> {
    #[cfg(target_os = "linux")]
    {
        crate::xattr::get_fd(fd, name)
    }
    #[cfg(not(target_os = "linux"))]
    {
        let _ = (fd, name);
        Err(io::Error::from_raw_os_error(libc::EOPNOTSUPP))
    }
}

/// Give the file open on `fd` the ACLs `acl`, in place of the ones it has: an ACL `acl` lacks
/// -- or has only trivially, saying nothing the mode does not -- is removed. Its default ACL is
/// a directory's only. Each kind of ACL is written only where it is that kind: POSIX ACLs
/// (Linux xattrs), NFSv4 ones (`system.nfs4_acl`, written back as read), macOS ones (by
/// `acl_set_fd_np`); anywhere else, or on a filesystem that holds none, the write fails with
/// EOPNOTSUPP. Not written: a CIFS security descriptor, which every file there has anyway.
///
/// `fd` may be an `O_PATH` descriptor, written through `/proc/self/fd/N` as `read_fd` reads one.
/// Writing an access ACL sets the group bits of the mode to its mask: the caller sets the mode
/// first, then the ACL, as gnulib's `qcopy_acl` does. A write that fails leaves no POSIX ACL
/// on the file, access or default, so that what it keeps is its mode alone. Residual: an NFSv4
/// ACL the destination already has is not removed when `acl` has none; the mode set before it
/// is what the server makes of it.
pub fn write_fd(fd: RawFd, acl: &Acl) -> io::Result<()> {
    sys::write(fd, acl)
}

/// The mode a file holds while ACLs are written to it, whatever its own mode `mode`: the
/// owner's permission bits alone. No group or other bit, as an ACL not yet the source's may
/// name others under the mask they make; and no set-user-ID or set-group-ID bit, as a native
/// ACL -- a macOS allow entry, an NFSv4 one the server folds into the mode -- may let someone
/// write the file at once, before its final mode is given.
pub fn interim_mode(mode: u32) -> u32 {
    mode & 0o700
}

/// Copy the ACLs `acl`, a source's, to the file open on `fd`, its copy, which the caller holds
/// at `interim_mode(mode)` while they are written, `mode` being the source's. Returned: the
/// mode the caller is then to give the copy, and the failure to report, if any -- the one
/// rule cp -p and pax -p p follow.
///
/// - Written: `mode`, unless a native ACL lets anyone but the owner change the file
///   (`Nfs4Acl::lets_others_change`): then the set-user-ID and set-group-ID bits are withheld
///   -- someone else's writing must not become a set-ID program -- and that is the failure.
/// - Not written (`copy_to_fd`): the mode `mode_without` keeps, and the failure.
pub fn copy_with_mode(acl: &Acl, fd: RawFd, mode: u32) -> (u32, Option<io::Error>) {
    match copy_to_fd(acl, fd, mode) {
        Err(e) => (mode_without(Some(acl), mode), Some(e)),
        Ok(()) if setid_withheld(acl, mode) => (
            mode & !0o6000,
            Some(io::Error::other(gettext(
                "set-user-ID and set-group-ID bits not kept: the ACL lets others write the file",
            ))),
        ),
        Ok(()) => (mode, None),
    }
}

/// Whether a copy whose ACLs `acl` were written keeps no set-user-ID or set-group-ID bit of
/// its mode `mode`: it has one, and a native ACL, not one saying only what the mode does, that
/// lets anyone but the owner change it.
fn setid_withheld(acl: &Acl, mode: u32) -> bool {
    mode & 0o6000 != 0 && !acl.native_trivial_for(mode) && acl.native_lets_others_change()
}

/// Copy the ACLs `acl` to the file open on `fd`, which holds `interim_mode(mode)` while they
/// are written (`write_fd`). A filesystem that holds no ACL fails it with EOPNOTSUPP, which is
/// no failure when the ACLs said exactly what `mode` does (`loses_nothing`).
///
/// Writing an access ACL sets the mode's bits from its owner, mask (or owning group) and other
/// entries, so the access ACL is written with those set to the interim mode's
/// (`PosixAcl::with_mode`): one wider than `mode` -- an archive's, say -- leaves the file no
/// more open than that until the caller gives it its own mode, which sets those entries once
/// more.
///
/// ACLs of both kinds -- POSIX ones saying more than the mode, and a native one, as an archive
/// member may carry -- are written as the kind the file takes (`write_either`).
fn copy_to_fd(acl: &Acl, fd: RawFd, mode: u32) -> io::Result<()> {
    let held = Acl {
        access: acl
            .access
            .as_ref()
            .map(|access| access.with_mode(interim_mode(mode))),
        ..acl.clone()
    };
    match write_either(fd, &held, mode) {
        Err(e) if loses_nothing(acl, &e, mode) => Ok(()),
        written => written,
    }
}

/// `write_fd`, for ACLs that may hold both kinds. Where the POSIX ones say nothing the mode
/// does not, the native one alone is written, and where the native one says exactly what the
/// mode `mode` does, the POSIX ones alone. Where both say more, the POSIX ones are written, or
/// where the file holds none of those (EOPNOTSUPP) the native one; but no file holds both, so
/// the other kind is lost all the same, which fails it (EOPNOTSUPP).
fn write_either(fd: RawFd, acl: &Acl, mode: u32) -> io::Result<()> {
    let Some(native) = &acl.native else {
        return write_fd(fd, acl);
    };
    let native_only = Acl {
        native: Some(native.clone()),
        ..Acl::default()
    };
    let posix = Acl {
        native: None,
        ..acl.clone()
    };
    if posix.is_trivial() {
        return write_fd(fd, &native_only);
    }
    if native_only.native_trivial_for(mode) {
        return write_fd(fd, &posix);
    }
    match write_fd(fd, &posix) {
        Err(e) if unsupported(&e) => write_fd(fd, &native_only)?,
        written => written?,
    }
    Err(io::Error::from_raw_os_error(libc::EOPNOTSUPP))
}

/// Whether `e` says the file holds no ACL of the kind written.
fn unsupported(e: &io::Error) -> bool {
    e.raw_os_error()
        .is_some_and(|code| code == libc::EOPNOTSUPP || code == libc::ENOTSUP)
}

/// Whether the failure `e` to copy the ACLs `acl` loses nothing of them: the destination holds
/// no ACL at all (EOPNOTSUPP), and `acl` says exactly what the mode `mode` does
/// (`Acl::is_trivial_for`) -- the mode already carried everything.
pub fn loses_nothing(acl: &Acl, e: &io::Error, mode: u32) -> bool {
    unsupported(e) && acl.is_trivial_for(mode)
}

/// The mode a copy keeps where its source's ACLs `acl` -- `None` where they could not be read
/// -- could not be set on it, `mode` being the source's: one granting no more than the source
/// did.
///
/// An access ACL's mask shows as the mode's group bits, though the owning group itself may
/// have had less: its own entry, masked. Without the ACL those bits are the owning group's, so
/// they become that entry, masked. ACLs not read may be anything: the group and other bits
/// are then cleared. So they are for a native ACL saying more than the mode, which may deny
/// what the mode grants -- and the set-user-ID and set-group-ID bits too, as it may have been
/// written all the same (one kind written where both were asked, or a step after it failing)
/// and may let others write the file (`copy_with_mode`). A default ACL lost costs the copy
/// itself nothing.
pub fn mode_without(acl: Option<&Acl>, mode: u32) -> u32 {
    let Some(acl) = acl else {
        return mode & !0o077;
    };
    if !acl.native_trivial_for(mode) {
        return mode & !0o6077;
    }
    let Some(access) = acl.access.as_ref().filter(|a| !a.is_trivial()) else {
        return mode;
    };
    let perm = |tag| {
        access
            .entries
            .iter()
            .find(|e| e.tag == tag)
            .map(|e| u32::from(e.perm))
    };
    let group = perm(Tag::GroupObj).unwrap_or(0) & perm(Tag::Mask).unwrap_or(7);
    (mode & !0o070) | (group << 3)
}

/// Give the file open on `fd`, which this process made in a directory whose default ACL is
/// `default` (`None`: one with none) and has since held at a mode of its own, the permissions
/// creating it there with `mode` would have given it. Under a default ACL that is the access
/// ACL inherited from it (`PosixAcl::inherited`), the umask playing no part; without one,
/// `mode` less `umask`. The bits of `mode` above the nine permission bits are given as they
/// are.
///
/// `chmod` sets the mode of the file `fd` is open on. The mode is set first, then the ACL
/// (`write_fd`; a directory's default ACL is written as `default`, the one it inherited).
pub fn set_created_mode(
    fd: RawFd,
    default: Option<PosixAcl>,
    mode: u32,
    umask: u32,
    chmod: impl Fn(u32) -> io::Result<()>,
) -> io::Result<()> {
    let special = mode & 0o7000;
    let Some(default) = default else {
        return chmod(special | (mode & 0o777 & !umask));
    };
    let (access, bits) = default.inherited(mode);
    chmod(special | bits)?;
    let acl = Acl {
        access: Some(access),
        default: Some(default),
        native: None,
    };
    write_fd(fd, &acl)
}

/// `set_created_mode` for a directory this process made: the default ACL it was made under is
/// its own, which the kernel copied from its parent's, read through `fd`. Where no ACL can be
/// read (EOPNOTSUPP) it has none.
pub fn set_made_dir_mode(
    fd: RawFd,
    mode: u32,
    umask: u32,
    chmod: impl Fn(u32) -> io::Result<()>,
) -> io::Result<()> {
    let default = match read_fd(fd) {
        Ok(acl) => acl.default,
        Err(e) if e.raw_os_error() == Some(libc::EOPNOTSUPP) => None,
        Err(e) => return Err(e),
    };
    set_created_mode(fd, default, mode, umask, chmod)
}

/// The size of a macOS `acl_copy_ext` form's header -- a `kauth_filesec`: magic, owner and
/// group GUIDs, entry count, flags -- and of each entry after it.
#[cfg(any(target_os = "macos", test))]
const DARWIN_HEADER: usize = 44;
#[cfg(any(target_os = "macos", test))]
const DARWIN_ENTRY: usize = 24;

/// Fail unless `bytes` hold a whole macOS external ACL, as `acl_copy_int` reads it with no
/// length: the `kauth_filesec` magic (in either byte order), and every entry its count
/// (`KAUTH_FILESEC_NOACL` for none) says follows.
#[cfg(any(target_os = "macos", test))]
fn check_darwin_external(bytes: &[u8]) -> io::Result<()> {
    const MAGIC: u32 = 0x012c_c16d;
    const NO_ACL: u32 = u32::MAX;
    let bad = || io::Error::from_raw_os_error(libc::EINVAL);
    let word = |at: usize| -> [u8; 4] { bytes[at..at + 4].try_into().expect("four bytes") };
    if bytes.len() < DARWIN_HEADER {
        return Err(bad());
    }
    let read: fn([u8; 4]) -> u32 = if u32::from_be_bytes(word(0)) == MAGIC {
        u32::from_be_bytes
    } else if u32::from_le_bytes(word(0)) == MAGIC {
        u32::from_le_bytes
    } else {
        return Err(bad());
    };
    let count = read(word(36));
    let entries = if count == NO_ACL {
        0
    } else {
        usize::try_from(count).map_err(|_| bad())?
    };
    let need = entries
        .checked_mul(DARWIN_ENTRY)
        .and_then(|n| n.checked_add(DARWIN_HEADER))
        .ok_or_else(bad)?;
    if bytes.len() < need {
        return Err(bad());
    }
    Ok(())
}

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
mod sys {
    use super::Acl;
    use std::io;
    use std::os::unix::io::RawFd;

    pub enum Target {
        Fd(RawFd),
    }

    pub fn read(_target: &Target) -> io::Result<Acl> {
        Err(io::Error::from_raw_os_error(libc::EOPNOTSUPP))
    }

    pub fn write(_fd: RawFd, _acl: &Acl) -> io::Result<()> {
        Err(io::Error::from_raw_os_error(libc::EOPNOTSUPP))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use posix::tests::full;

    /// An NFSv4 or CIFS ACL may let anyone write, whatever the mode shows: its attribute being
    /// there at all, or not readable for certain, makes a directory others may write -- with
    /// group write permission or without. A POSIX ACL widens only group write permission, and
    /// counts only then.
    #[test]
    fn which_acl_attributes_let_others_write() {
        use std::ffi::CStr;
        let absent = || Err(std::io::Error::from_raw_os_error(libc::ENODATA));
        let unsupported = || Err(std::io::Error::from_raw_os_error(libc::EOPNOTSUPP));
        let named_user = {
            let mut xattr = 2u32.to_le_bytes().to_vec();
            for tag in [0x01u16, 0x02, 0x04, 0x10, 0x20] {
                xattr.extend(tag.to_le_bytes());
                xattr.extend(7u16.to_le_bytes());
                xattr.extend(0u32.to_le_bytes());
            }
            xattr
        };
        // Each case: the one attribute present (or failing), and whether that lets others
        // write without group write permission and with it.
        let cases: [(&CStr, std::io::Result<Vec<u8>>, bool, bool); 6] = [
            (c"system.nfs4_acl", Ok(vec![0; 8]), true, true),
            (c"system.nfs4_acl_xdr", Ok(Vec::new()), true, true),
            (c"system.cifs_acl", Ok(vec![1]), true, true),
            (
                c"system.cifs_acl",
                Err(std::io::Error::from_raw_os_error(libc::EIO)),
                true,
                true,
            ),
            (c"system.posix_acl_access", Ok(named_user), false, true),
            (c"system.posix_acl_access", unsupported(), false, false),
        ];
        for (name, value, without_group, with_group) in cases {
            let read = |asked: &CStr| -> std::io::Result<Vec<u8>> {
                if asked != name {
                    return absent();
                }
                match &value {
                    Ok(value) => Ok(value.clone()),
                    Err(e) => Err(std::io::Error::from_raw_os_error(e.raw_os_error().unwrap())),
                }
            };
            assert_eq!(lets_others_write_by(read, false), without_group, "{name:?}");
            assert_eq!(lets_others_write_by(read, true), with_group, "{name:?}");
        }
        // No attribute at all, or none supported: the mode tells.
        assert!(!lets_others_write_by(|_| absent(), true));
        assert!(!lets_others_write_by(|_| unsupported(), true));
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
        assert!(!names_others(&acl(&[0x01, 0x04, 0x20])));
        assert!(!names_others(&acl(&[0x01, 0x04, 0x10, 0x20])));
        // A named user, a named group, a tag not known.
        assert!(names_others(&acl(&[0x01, 0x02, 0x04, 0x10, 0x20])));
        assert!(names_others(&acl(&[0x01, 0x04, 0x08, 0x10, 0x20])));
        assert!(names_others(&acl(&[0x01, 0x04, 0x40, 0x20])));
        // Not the format read here.
        let mut other_version = acl(&[0x01, 0x04, 0x20]);
        other_version[0] = 3;
        assert!(names_others(&other_version));
        let mut torn = acl(&[0x01, 0x04, 0x20]);
        torn.pop();
        assert!(names_others(&torn));
        assert!(names_others(&[2, 0]));
    }

    /// The `system.nfs4_acl` XDR of the aces `(type, flag, who)`.
    fn nfs4(aces: &[(u32, u32, &str)]) -> Vec<u8> {
        let mut out = (aces.len() as u32).to_be_bytes().to_vec();
        for &(kind, flag, who) in aces {
            for word in [kind, flag, 0x1f, who.len() as u32] {
                out.extend(word.to_be_bytes());
            }
            out.extend(who.as_bytes());
            out.resize(out.len().div_ceil(4) * 4, 0);
        }
        out
    }

    /// An NFSv4 ACL is trivial as gnulib's `acl_nfs4_nontrivial` has it.
    #[test]
    fn which_nfs4_acls_are_trivial() {
        let trivial = |aces: &[(u32, u32, &str)]| {
            Acl {
                native: Some(Native {
                    kind: NativeKind::Nfs4,
                    bytes: nfs4(aces),
                }),
                ..Acl::default()
            }
            .is_trivial()
        };
        assert!(trivial(&[]));
        assert!(trivial(&[
            (0, 0, "OWNER@"),
            (0, 0x40, "GROUP@"),
            (0, 0, "EVERYONE@")
        ]));
        assert!(trivial(&[
            (1, 0, "OWNER@"),
            (0, 0, "OWNER@"),
            (1, 0x40, "GROUP@"),
            (0, 0x40, "GROUP@"),
            (1, 0, "EVERYONE@"),
            (0, 0, "EVERYONE@"),
        ]));
        // Someone named; an audit ace; an inheritance flag; the same ace twice; seven aces.
        assert!(!trivial(&[(0, 0, "alice@example.org")]));
        assert!(!trivial(&[(2, 0, "OWNER@")]));
        assert!(!trivial(&[(0, 0x1, "OWNER@")]));
        assert!(!trivial(&[(0, 0, "OWNER@"), (0, 0, "OWNER@")]));
        assert!(!trivial(&[(0, 0, "OWNER@"); 7]));
        // Torn.
        let mut torn = nfs4(&[(0, 0, "EVERYONE@")]);
        torn.truncate(torn.len() - 4);
        let acl = Acl {
            native: Some(Native {
                kind: NativeKind::Nfs4,
                bytes: torn,
            }),
            ..Acl::default()
        };
        assert!(!acl.is_trivial());
        // A CIFS security descriptor says nothing; a macOS ACL is never trivial.
        let native = |kind| Acl {
            native: Some(Native {
                kind,
                bytes: vec![1],
            }),
            ..Acl::default()
        };
        assert!(native(NativeKind::Cifs).is_trivial());
        assert!(!native(NativeKind::Darwin).is_trivial());
        assert!(Acl::default().is_trivial());
    }

    /// A failed copy loses nothing only where the destination holds no ACL and the source's
    /// said nothing the mode does not.
    #[test]
    fn which_failed_copies_lose_nothing() {
        let unsupported = std::io::Error::from_raw_os_error(libc::EOPNOTSUPP);
        let notsup = std::io::Error::from_raw_os_error(libc::ENOTSUP);
        let denied = std::io::Error::from_raw_os_error(libc::EPERM);
        let trivial = Acl::default();
        let named = Acl {
            access: Some(full()),
            ..Acl::default()
        };
        let default_only = Acl {
            default: Some(full()),
            ..Acl::default()
        };
        assert!(loses_nothing(&trivial, &unsupported, 0o644));
        assert!(loses_nothing(&trivial, &notsup, 0o644));
        assert!(!loses_nothing(&trivial, &denied, 0o644));
        assert!(!loses_nothing(&named, &unsupported, 0o644));
        assert!(!loses_nothing(&default_only, &unsupported, 0o644));
        let darwin = Acl {
            native: Some(Native {
                kind: NativeKind::Darwin,
                bytes: vec![1],
            }),
            ..Acl::default()
        };
        assert!(!loses_nothing(&darwin, &unsupported, 0o644));
    }

    /// A copy that lost its ACLs grants no more than the source did: the owning group gets its
    /// own entry, masked, not the mask; a native ACL lost, or ACLs never read, leave the owner
    /// alone.
    #[test]
    fn the_mode_kept_without_the_acls() {
        let e = |tag, perm| Entry { tag, perm };
        let named = |group, mask| Acl {
            access: Some(PosixAcl {
                entries: vec![
                    e(Tag::UserObj, 6),
                    e(Tag::User(65534), 6),
                    e(Tag::GroupObj, group),
                    e(Tag::Mask, mask),
                    e(Tag::Other, 4),
                ],
            }),
            ..Acl::default()
        };
        assert_eq!(mode_without(Some(&named(0, 6)), 0o4664), 0o4604);
        assert_eq!(mode_without(Some(&named(7, 5)), 0o654), 0o654);
        assert_eq!(mode_without(Some(&named(4, 6)), 0o664), 0o644);
        assert_eq!(mode_without(Some(&Acl::default()), 0o664), 0o664);
        let default_only = Acl {
            default: named(0, 6).access,
            ..Acl::default()
        };
        assert_eq!(mode_without(Some(&default_only), 0o775), 0o775);
        assert_eq!(mode_without(None, 0o1777), 0o1700);
        let darwin = Acl {
            native: Some(Native {
                kind: NativeKind::Darwin,
                bytes: vec![1],
            }),
            ..Acl::default()
        };
        assert_eq!(mode_without(Some(&darwin), 0o755), 0o700);
    }

    /// A macOS external ACL is taken only whole: its magic, and every entry its count says.
    #[test]
    fn darwin_external_form_is_checked_whole() {
        let form = |magic: u32, count: u32, entries: usize, be: bool| {
            let word = |w: u32| if be { w.to_be_bytes() } else { w.to_le_bytes() };
            let mut out = word(magic).to_vec();
            out.extend([0u8; 32]);
            out.extend(word(count));
            out.extend(word(0));
            out.extend(vec![0u8; entries * DARWIN_ENTRY]);
            out
        };
        const MAGIC: u32 = 0x012c_c16d;
        for be in [true, false] {
            assert!(check_darwin_external(&form(MAGIC, 2, 2, be)).is_ok());
            assert!(check_darwin_external(&form(MAGIC, u32::MAX, 0, be)).is_ok());
            assert!(check_darwin_external(&form(MAGIC, 3, 2, be)).is_err());
            assert!(check_darwin_external(&form(MAGIC + 1, 0, 0, be)).is_err());
        }
        assert!(check_darwin_external(&[]).is_err());
        assert!(check_darwin_external(&form(MAGIC, 0, 0, true)[..43]).is_err());
    }

    /// Writing an access ACL sets the mode's bits from it: `copy_to_fd` writes one wider than
    /// the mode the file holds no wider, so the file is never more open than that mode while
    /// the caller has yet to give it its own. Skipped where the filesystem takes no ACLs.
    #[cfg(target_os = "linux")]
    #[test]
    fn copy_to_fd_keeps_the_mode_the_file_holds() {
        use std::os::fd::AsRawFd;
        use std::os::unix::fs::PermissionsExt;
        let dir = crate::tmp::tempdir().unwrap();
        let path = dir.path().join("f");
        let file = std::fs::File::create(&path).unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o700)).unwrap();
        let wide = PosixAcl::from_text("user::rwx,user:65534:rwx,group::rwx,mask::rwx,other::rwx");
        let acl = Acl {
            access: Some(wide.unwrap()),
            ..Acl::default()
        };
        match copy_to_fd(&acl, file.as_raw_fd(), 0o700) {
            Err(e) if e.raw_os_error() == Some(libc::EOPNOTSUPP) => return,
            written => written.unwrap(),
        }
        let mode = std::fs::metadata(&path).unwrap().permissions().mode();
        assert_eq!(mode & 0o7777, 0o700);
        let named = read_path(&path, true).unwrap().access.unwrap();
        assert!(named.entries.contains(&Entry {
            tag: Tag::User(65534),
            perm: 7
        }));
    }

    /// What `write_fd` writes reads back, through a descriptor and through an `O_PATH` one (the
    /// procfs route); it replaces what was there, so an ACL the source lacks is removed, and a
    /// directory's default ACL with it. Skipped where `setfacl` is missing or the filesystem
    /// takes no ACLs.
    #[cfg(target_os = "linux")]
    #[test]
    fn writes_replace_what_was_there() {
        use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
        use std::os::unix::fs::PermissionsExt;
        let dir = crate::tmp::tempdir().unwrap();
        let src = dir.path().join("src");
        std::fs::write(&src, "x").unwrap();
        if !crate::testing::grant_named_acl(&src) {
            return;
        }
        let acl = read_path(&src, true).unwrap();
        assert!(!acl.is_trivial());

        let dst = dir.path().join("dst");
        let file = std::fs::File::create(&dst).unwrap();
        let mode = std::os::unix::fs::MetadataExt::mode(&std::fs::metadata(&src).unwrap());
        let (kept, failed) = copy_with_mode(&acl, file.as_raw_fd(), mode);
        assert!(failed.is_none(), "{failed:?}");
        assert_eq!(kept, mode);
        std::fs::set_permissions(&dst, std::fs::Permissions::from_mode(kept)).unwrap();
        assert_eq!(read_path(&dst, true).unwrap(), acl);
        write_fd(file.as_raw_fd(), &Acl::default()).unwrap();
        assert_eq!(read_path(&dst, true).unwrap(), Acl::default());

        let sub = dir.path().join("sub");
        std::fs::create_dir(&sub).unwrap();
        let both = std::process::Command::new("setfacl")
            .args(["-m", "u:65534:rx,d:u:65534:rwx"])
            .arg(&sub)
            .status()
            .unwrap();
        assert!(both.success());
        let dir_acl = read_path(&sub, true).unwrap();
        assert!(dir_acl.access.is_some() && dir_acl.default.is_some());
        let made = dir.path().join("made");
        std::fs::create_dir(&made).unwrap();
        let path = std::ffi::CString::new(made.as_os_str().as_encoded_bytes()).unwrap();
        let fd = unsafe { libc::open(path.as_ptr(), libc::O_PATH | libc::O_CLOEXEC) };
        assert!(fd >= 0);
        let fd = unsafe { OwnedFd::from_raw_fd(fd) };
        write_fd(fd.as_raw_fd(), &dir_acl).unwrap();
        assert_eq!(read_path(&made, true).unwrap(), dir_acl);
        write_fd(fd.as_raw_fd(), &acl).unwrap();
        let replaced = read_path(&made, true).unwrap();
        assert_eq!(replaced.access, acl.access);
        assert!(replaced.default.is_none());
    }

    /// The ACLs `setfacl` gives a directory read back, by path and through an `O_PATH`
    /// descriptor (the procfs route); a default ACL alone is not trivial. Skipped where
    /// `setfacl` is missing or the filesystem takes no ACLs.
    #[cfg(target_os = "linux")]
    #[test]
    fn reads_what_setfacl_set() {
        use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
        let dir = crate::tmp::tempdir().unwrap();
        let sub = dir.path().join("sub");
        std::fs::create_dir(&sub).unwrap();
        assert_eq!(read_path(&sub, true).unwrap(), Acl::default());
        let set = std::process::Command::new("setfacl")
            .args(["-d", "-m", "u:65534:rx"])
            .arg(&sub)
            .stderr(std::process::Stdio::null())
            .status()
            .is_ok_and(|s| s.success());
        if !set {
            eprintln!("note: setfacl is missing or this filesystem takes no ACLs; skipped");
            return;
        }
        let acl = read_path(&sub, true).unwrap();
        assert!(acl.access.is_none());
        let default = acl.default.as_ref().unwrap();
        assert!(default.entries.contains(&Entry {
            tag: Tag::User(65534),
            perm: 5
        }));
        assert!(!acl.is_trivial());

        assert!(crate::testing::grant_named_acl(&sub));
        let path = std::ffi::CString::new(sub.as_os_str().as_encoded_bytes()).unwrap();
        let fd = unsafe { libc::open(path.as_ptr(), libc::O_PATH | libc::O_CLOEXEC) };
        assert!(fd >= 0);
        let fd = unsafe { OwnedFd::from_raw_fd(fd) };
        let by_fd = read_fd(fd.as_raw_fd()).unwrap();
        assert_eq!(by_fd, read_path(&sub, true).unwrap());
        let access = by_fd.access.unwrap();
        assert!(access.entries.contains(&Entry {
            tag: Tag::User(65534),
            perm: 7
        }));
        assert!(lets_others_write(fd.as_raw_fd(), true));
        assert!(!lets_others_write(fd.as_raw_fd(), false));

        // A symbolic link itself has none.
        let link = dir.path().join("link");
        std::os::unix::fs::symlink("sub", &link).unwrap();
        assert_eq!(read_path(&link, false).unwrap(), Acl::default());
        assert_eq!(
            read_path(&link, true).unwrap(),
            read_path(&sub, true).unwrap()
        );
    }

    /// A Linux NFSv4 ACL goes into an archive as libarchive's text, and the text comes back as
    /// the XDR it was; a trivial one, or none, makes no text, and trivial text makes no ACL.
    #[test]
    fn nfs4_acls_travel_as_text() {
        let nfs4_acl = |aces: &[(u32, u32, &str)]| Acl {
            native: Some(Native {
                kind: NativeKind::Nfs4,
                bytes: nfs4(aces),
            }),
            ..Acl::default()
        };
        let named = nfs4_acl(&[(0, 0, "OWNER@"), (1, 0x40, "3999999002")]);
        let text = named.nfs4_text(0o644).unwrap().unwrap();
        assert_eq!(
            text,
            "owner@:rwpRW::allow,group:3999999002:rwpRW::deny:3999999002"
        );
        #[cfg(target_os = "linux")]
        assert_eq!(native_from_ace_text(&text, 0o644).unwrap(), named.native);
        let trivial = nfs4_acl(&[(0, 0, "OWNER@")]);
        // The fixture grants the owner read and write, and nobody else anything.
        assert_eq!(trivial.nfs4_text(0o600).unwrap(), None);
        assert!(trivial.nfs4_text(0o644).unwrap().is_some());
        assert_eq!(Acl::default().nfs4_text(0o644).unwrap(), None);
        assert_eq!(
            native_from_ace_text("owner@:rw::allow", 0o600).unwrap(),
            None
        );
        assert!(native_from_ace_text("owner@:rw::nope", 0o600).is_err());
    }

    /// ACLs of both kinds are written as the kind the file takes -- on a local Linux
    /// filesystem the POSIX ones; an NFSv4 one alone cannot be held there, and is a loss.
    /// Skipped where the filesystem takes no ACLs.
    #[cfg(target_os = "linux")]
    #[test]
    fn both_kinds_write_the_kind_the_file_takes() {
        use std::os::fd::AsRawFd;
        let dir = crate::tmp::tempdir().unwrap();
        let path = dir.path().join("f");
        let file = std::fs::File::create(&path).unwrap();
        let fd = file.as_raw_fd();
        let posix = Acl {
            access: Some(full()),
            ..Acl::default()
        };
        if write_fd(fd, &posix).is_err() {
            return;
        }
        let native = native_from_ace_text("user:3999999001:r::allow:3999999001", 0o640).unwrap();
        let both = Acl {
            native: native.clone(),
            ..posix
        };
        let (kept, failed) = copy_with_mode(&both, fd, 0o640);
        assert!(failed.is_some_and(|e| unsupported(&e)));
        assert_eq!(kept, 0o600);
        let read = read_path(&path, true).unwrap();
        assert_eq!(read.access, Some(full().with_mode(interim_mode(0o640))));
        assert_eq!(read.native, None);

        // A native one the mode says all of is no loss beside a POSIX one.
        let said = native_from_ace_text("owner@:rw::allow,group@:r::allow", 0o640).unwrap();
        assert_eq!(said, None);

        let alone = Acl {
            native,
            ..Acl::default()
        };
        let (kept, failed) = copy_with_mode(&alone, fd, 0o754);
        let e = failed.unwrap();
        assert!(unsupported(&e), "{e}");
        assert!(!loses_nothing(&alone, &e, 0o754));
        assert_eq!(kept, 0o700);
    }

    /// The set-ID bits are withheld from a copy whose native ACL lets anyone but the owner
    /// change it -- decided from the archived text, as pax decides it -- and kept otherwise. A
    /// native ACL not read for certain may let anyone.
    #[test]
    fn setid_bits_withheld_where_others_may_change_the_file() {
        let from_text = |text: &str, mode: u32| {
            let native = native_from_ace_text(text, mode).unwrap_or_else(|_| {
                // Elsewhere than Linux the text is made into this system's kind, or fails;
                // the decision is the same on the XDR.
                Some(Native {
                    kind: NativeKind::Nfs4,
                    bytes: Nfs4Acl::from_ace_text(text).unwrap().to_xdr(),
                })
            });
            Acl {
                native,
                ..Acl::default()
            }
        };
        let named_write = "user:3999999001:w::allow:3999999001";
        assert!(setid_withheld(&from_text(named_write, 0o4755), 0o4755));
        assert!(setid_withheld(&from_text(named_write, 0o2755), 0o2755));
        assert!(!setid_withheld(&from_text(named_write, 0o755), 0o755));
        for text in ["group@:p::allow,owner@:rwx::allow", "everyone@:C::allow"] {
            assert!(setid_withheld(&from_text(text, 0o4755), 0o4755), "{text}");
        }
        for text in [
            "user:3999999001:w::deny:3999999001",
            "user:3999999001:rx::allow:3999999001",
            "owner@:rwxpCo::allow,user:3999999001:r::allow:3999999001",
        ] {
            assert!(!setid_withheld(&from_text(text, 0o4755), 0o4755), "{text}");
        }
        let unread = Acl {
            native: Some(Native {
                kind: NativeKind::Nfs4,
                bytes: vec![0, 0, 0, 9],
            }),
            ..Acl::default()
        };
        assert!(setid_withheld(&unread, 0o4755));
        assert!(!setid_withheld(&Acl::default(), 0o4755));
    }

    /// An `everyone@` deny of what the mode grants others says more than the mode -- gnulib
    /// and `ls` count it trivial -- so it is restored, or its loss reported, not dropped.
    #[test]
    fn a_deny_the_mode_does_not_say_is_kept() {
        let deny = "everyone@:r::deny";
        #[cfg(target_os = "linux")]
        assert!(native_from_ace_text(deny, 0o644).unwrap().is_some());
        // It denies the owner and the owning group too: only a mode granting nobody says it.
        assert_eq!(native_from_ace_text(deny, 0o000).unwrap(), None);
        let acl = Acl {
            native: Some(Native {
                kind: NativeKind::Nfs4,
                bytes: Nfs4Acl::from_ace_text(deny).unwrap().to_xdr(),
            }),
            ..Acl::default()
        };
        assert!(acl.is_trivial());
        assert!(!acl.is_trivial_for(0o644));
        let unsupported = std::io::Error::from_raw_os_error(libc::EOPNOTSUPP);
        assert!(!loses_nothing(&acl, &unsupported, 0o644));
        assert!(loses_nothing(&acl, &unsupported, 0o000));
        assert_eq!(mode_without(Some(&acl), 0o644), 0o600);
    }

    /// Where a native ACL saying more than the mode is not set in full -- one kind of two
    /// written, or none -- the copy keeps no set-ID bit either, as it may hold the native one
    /// all the same. ACLs not read, or POSIX ones alone, leave those bits.
    #[test]
    fn a_failed_native_copy_keeps_no_setid_bit() {
        let native = Some(Native {
            kind: NativeKind::Nfs4,
            bytes: Nfs4Acl::from_ace_text("user:3999999001:w::allow:3999999001")
                .unwrap()
                .to_xdr(),
        });
        let alone = Acl {
            native: native.clone(),
            ..Acl::default()
        };
        let both = Acl {
            access: Some(full()),
            native,
            ..Acl::default()
        };
        for acl in [&alone, &both] {
            assert_eq!(mode_without(Some(acl), 0o6755), 0o700);
        }
        assert_eq!(mode_without(None, 0o6755), 0o6700);
        let posix = Acl {
            access: Some(full()),
            ..Acl::default()
        };
        assert_eq!(mode_without(Some(&posix), 0o6775), 0o6745);
        // Through the copy: on a file that holds no NFSv4 ACL, the native write fails.
        #[cfg(target_os = "linux")]
        {
            use std::os::fd::AsRawFd;
            let dir = crate::tmp::tempdir().unwrap();
            let file = std::fs::File::create(dir.path().join("f")).unwrap();
            let (kept, failed) = copy_with_mode(&alone, file.as_raw_fd(), 0o6755);
            assert!(failed.is_some());
            assert_eq!(kept, 0o700);
        }
    }
}
