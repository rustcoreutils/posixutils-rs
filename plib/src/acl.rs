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
//!   extended attributes, read here without libacl; NFSv4 and CIFS ACLs (`system.nfs4_acl`,
//!   `system.cifs_acl`) are kept as the bytes the filesystem hands out (`Native`).
//! - macOS: NFSv4-style extended ACLs, reached through the libSystem `acl(3)` calls and kept
//!   in their external form (`acl_copy_ext`).
//! - Anything else: no ACL is read or written (EOPNOTSUPP).

use gettextrs::gettext;
use std::ffi::CStr;
#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::ffi::CString;
use std::io;
#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::os::unix::ffi::OsStrExt;
use std::os::unix::io::RawFd;
use std::path::Path;

/// Who a POSIX ACL entry is about. The named forms carry the uid or gid.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Tag {
    UserObj,
    User(u32),
    GroupObj,
    Group(u32),
    Mask,
    Other,
}

/// One entry of a POSIX ACL: who, and the permissions (`r` 4, `w` 2, `x` 1, as in a mode).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Entry {
    pub tag: Tag,
    pub perm: u8,
}

/// A POSIX.1e ACL, an access or a default one: its entries in the order the kernel keeps
/// them -- owner, named users by uid, owning group, named groups by gid, mask, others.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PosixAcl {
    pub entries: Vec<Entry>,
}

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

/// An ACL kept as the bytes its system hands out, not read here.
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
}

/// `ACL_UNDEFINED_ID`: the id field of an entry that names nobody.
const UNDEFINED_ID: u32 = u32::MAX;
/// The only version of the Linux xattr format.
const XATTR_VERSION: u32 = 2;
/// The xattr tag values (`ACL_USER_OBJ` ...).
const USER_OBJ: u16 = 0x01;
const USER: u16 = 0x02;
const GROUP_OBJ: u16 = 0x04;
const GROUP: u16 = 0x08;
const MASK: u16 = 0x10;
const OTHER: u16 = 0x20;

fn invalid(what: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, gettext(what))
}

impl PosixAcl {
    /// Whether it has only the entries the mode shows: owner, owning group, others.
    pub fn is_trivial(&self) -> bool {
        self.entries
            .iter()
            .all(|e| matches!(e.tag, Tag::UserObj | Tag::GroupObj | Tag::Other))
    }

    /// What a file made with the permission bits `mode` in a directory whose default ACL this
    /// is takes, by Linux's `posix_acl_create`: this ACL with the owner's, the mask's (the owning
    /// group's, where there is no mask) and others' entries masked by the matching bits of
    /// `mode`, and the permission bits that then show it. The umask plays no part.
    pub fn inherited(&self, mode: u32) -> (PosixAcl, u32) {
        let has_mask = self.entries.iter().any(|e| e.tag == Tag::Mask);
        let mut acl = self.clone();
        let mut bits = 0;
        for e in &mut acl.entries {
            let shift = match e.tag {
                Tag::UserObj => 6,
                Tag::Mask => 3,
                Tag::GroupObj if !has_mask => 3,
                Tag::Other => 0,
                _ => continue,
            };
            // At most `rwx`: three bits.
            e.perm &= ((mode >> shift) & 7) as u8;
            bits |= u32::from(e.perm) << shift;
        }
        (acl, bits)
    }

    /// Whether it names a user or group besides the owner and owning group.
    fn names_others(&self) -> bool {
        self.entries
            .iter()
            .any(|e| matches!(e.tag, Tag::User(_) | Tag::Group(_)))
    }

    /// Parse the Linux xattr form: a little-endian 32-bit version (2), then 8-byte entries of
    /// a 16-bit tag, a 16-bit permission set and a 32-bit id. No entries at all is the empty
    /// ACL, which stands for none; any other ACL must be one the kernel accepts (`check`).
    pub fn parse_xattr(bytes: &[u8]) -> io::Result<PosixAcl> {
        let Some((version, rest)) = bytes.split_first_chunk::<4>() else {
            return Err(invalid("truncated ACL"));
        };
        if u32::from_le_bytes(*version) != XATTR_VERSION {
            return Err(invalid("unknown ACL version"));
        }
        let (chunks, torn) = rest.as_chunks::<8>();
        if !torn.is_empty() {
            return Err(invalid("truncated ACL"));
        }
        let mut entries = Vec::with_capacity(chunks.len());
        for chunk in chunks {
            let tag = u16::from_le_bytes([chunk[0], chunk[1]]);
            let perm = u16::from_le_bytes([chunk[2], chunk[3]]);
            let id = u32::from_le_bytes([chunk[4], chunk[5], chunk[6], chunk[7]]);
            let tag = match tag {
                USER_OBJ => Tag::UserObj,
                USER => Tag::User(id),
                GROUP_OBJ => Tag::GroupObj,
                GROUP => Tag::Group(id),
                MASK => Tag::Mask,
                OTHER => Tag::Other,
                _ => return Err(invalid("unknown ACL entry tag")),
            };
            let perm = u8::try_from(perm)
                .ok()
                .filter(|p| p & !7 == 0)
                .ok_or_else(|| invalid("invalid ACL permissions"))?;
            entries.push(Entry { tag, perm });
        }
        let acl = PosixAcl { entries };
        if !acl.entries.is_empty() {
            acl.check()?;
        }
        Ok(acl)
    }

    /// The Linux xattr form (`parse_xattr`).
    pub fn to_xattr(&self) -> Vec<u8> {
        let mut out = Vec::with_capacity(4 + 8 * self.entries.len());
        out.extend(XATTR_VERSION.to_le_bytes());
        for e in &self.entries {
            let (tag, id) = match e.tag {
                Tag::UserObj => (USER_OBJ, UNDEFINED_ID),
                Tag::User(uid) => (USER, uid),
                Tag::GroupObj => (GROUP_OBJ, UNDEFINED_ID),
                Tag::Group(gid) => (GROUP, gid),
                Tag::Mask => (MASK, UNDEFINED_ID),
                Tag::Other => (OTHER, UNDEFINED_ID),
            };
            out.extend(tag.to_le_bytes());
            out.extend(u16::from(e.perm).to_le_bytes());
            out.extend(id.to_le_bytes());
        }
        out
    }

    /// Fail unless the kernel would take it (`posix_acl_valid`): owner, named users in
    /// ascending uid order, owning group, named groups in ascending gid order, then a mask --
    /// required once anyone is named -- and others; each once, no permission beyond `rwx`.
    pub fn check(&self) -> io::Result<()> {
        // The entries in order are exactly what `Tag`'s ordering sorts, each once.
        let sorted = self.entries.windows(2).all(|w| w[0].tag < w[1].tag);
        let has = |tag| self.entries.iter().any(|e| e.tag == tag);
        let complete = has(Tag::UserObj) && has(Tag::GroupObj) && has(Tag::Other);
        if !sorted || !complete || self.entries.iter().any(|e| e.perm & !7 != 0) {
            return Err(invalid("invalid ACL"));
        }
        if self.names_others() && !has(Tag::Mask) {
            return Err(invalid("ACL names a user or group without a mask"));
        }
        Ok(())
    }

    /// The text form star and libarchive put in a pax archive (`SCHILY.acl.access`): entries
    /// separated by commas, a named one with its id as a fourth field --
    /// `user::rw-,user:alice:r--:1000,group::r--,mask::r--,other::r--`. A user or group with
    /// no name, or one the form cannot hold, is named by its number.
    pub fn to_text(&self) -> String {
        let fields: Vec<String> = self.entries.iter().map(entry_text).collect();
        fields.join(",")
    }

    /// Parse the text form (`to_text`), or the same without the id fields, entries separated
    /// by commas or newlines, the tags spelled out or by their first letter. A name is looked
    /// up first; one that is not known is taken from the id field, else read as a number.
    /// The entries are put in the kernel's order, and must make an ACL it takes (`check`).
    pub fn from_text(text: &str) -> io::Result<PosixAcl> {
        let mut entries = Vec::new();
        for field in text.split([',', '\n']).map(str::trim) {
            if !field.is_empty() {
                entries.push(parse_entry(field)?);
            }
        }
        entries.sort_by_key(|e: &Entry| e.tag);
        let acl = PosixAcl { entries };
        acl.check()?;
        Ok(acl)
    }
}

fn perm_text(perm: u8) -> String {
    [(4, 'r'), (2, 'w'), (1, 'x')]
        .iter()
        .map(|&(bit, c)| if perm & bit != 0 { c } else { '-' })
        .collect()
}

fn entry_text(e: &Entry) -> String {
    let perm = perm_text(e.perm);
    // A name the text form can carry: UTF-8, not empty, holding no separator.
    let usable = |name: std::ffi::OsString| {
        name.into_string()
            .ok()
            .filter(|n| !n.is_empty() && !n.contains([',', ':', '\n']))
    };
    let named = |kind: &str, id: u32, name: Option<String>| {
        let name = name.unwrap_or_else(|| id.to_string());
        format!("{kind}:{name}:{perm}:{id}")
    };
    match e.tag {
        Tag::UserObj => format!("user::{perm}"),
        Tag::User(uid) => {
            let name = crate::user::lookup_by_uid(uid).ok().flatten();
            named("user", uid, name.and_then(|u| usable(u.name)))
        }
        Tag::GroupObj => format!("group::{perm}"),
        Tag::Group(gid) => {
            let name = crate::group::lookup_by_gid(gid).ok().flatten();
            named("group", gid, name.and_then(|g| usable(g.name)))
        }
        Tag::Mask => format!("mask::{perm}"),
        Tag::Other => format!("other::{perm}"),
    }
}

fn parse_entry(field: &str) -> io::Result<Entry> {
    let bad = || invalid("invalid ACL text");
    let parts: Vec<&str> = field.split(':').collect();
    let (kind, qualifier, perm, id) = match parts[..] {
        [kind, qualifier, perm] => (kind, qualifier, perm, None),
        [kind, qualifier, perm, id] => (kind, qualifier, perm, Some(id)),
        _ => return Err(bad()),
    };
    let mut bits = 0u8;
    for c in perm.chars() {
        bits |= match c {
            'r' => 4,
            'w' => 2,
            'x' => 1,
            '-' => 0,
            _ => return Err(bad()),
        };
    }
    let id = id
        .map(|id| id.parse::<u32>().map_err(|_| bad()))
        .transpose()?;
    let resolve = |lookup: &dyn Fn(&str) -> Option<u32>| {
        lookup(qualifier)
            .or(id)
            .or_else(|| qualifier.parse::<u32>().ok())
            .filter(|&id| id != UNDEFINED_ID)
            .ok_or_else(bad)
    };
    let tag = match (kind, qualifier.is_empty()) {
        ("user" | "u", true) => Tag::UserObj,
        ("user" | "u", false) => Tag::User(resolve(&|name| {
            crate::user::lookup_by_name(name)
                .ok()
                .flatten()
                .map(|u| u.uid)
        })?),
        ("group" | "g", true) => Tag::GroupObj,
        ("group" | "g", false) => Tag::Group(resolve(&|name| {
            crate::group::lookup_by_name(name)
                .ok()
                .flatten()
                .map(|g| g.gid)
        })?),
        ("mask" | "m", true) => Tag::Mask,
        ("other" | "o", true) => Tag::Other,
        _ => return Err(bad()),
    };
    Ok(Entry { tag, perm: bits })
}

/// Whether the `system.nfs4_acl` XDR `xdr` says only what a mode can, by gnulib's
/// `acl_nfs4_nontrivial`: at most six entries, each an ALLOW or DENY for `OWNER@`, `GROUP@` or
/// `EVERYONE@` -- at most one of each type for each -- with no flag but IDENTIFIER_GROUP.
/// Anything not read for certain is not trivial.
fn nfs4_is_trivial(xdr: &[u8]) -> bool {
    nfs4_trivial(xdr).unwrap_or(false)
}

/// `nfs4_is_trivial`, `None` for XDR that ends early.
fn nfs4_trivial(mut xdr: &[u8]) -> Option<bool> {
    const ALLOW: u32 = 0;
    const DENY: u32 = 1;
    const IDENTIFIER_GROUP: u32 = 0x40;
    fn word(xdr: &mut &[u8]) -> Option<u32> {
        let (w, rest) = xdr.split_first_chunk::<4>()?;
        *xdr = rest;
        Some(u32::from_be_bytes(*w))
    }
    let count = word(&mut xdr)?;
    if count > 6 {
        return Some(false);
    }
    let mut seen = 0u32;
    for _ in 0..count {
        let (kind, flag, _mask) = (word(&mut xdr)?, word(&mut xdr)?, word(&mut xdr)?);
        let len = usize::try_from(word(&mut xdr)?).ok()?;
        if !matches!(kind, ALLOW | DENY) || flag & !IDENTIFIER_GROUP != 0 {
            return Some(false);
        }
        let who = xdr.get(..len)?;
        xdr = xdr.get(len.div_ceil(4) * 4..)?;
        let who_slot = match who {
            b"OWNER@" => 0,
            b"GROUP@" => 2,
            b"EVERYONE@" => 4,
            _ => return Some(false),
        };
        let bit = 1 << (who_slot | kind);
        if seen & bit != 0 {
            return Some(false);
        }
        seen |= bit;
    }
    Some(true)
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

/// The extended attribute `name` of the file open on `fd`, `O_PATH` or not (`read_fd`).
/// Elsewhere than Linux none is read: EOPNOTSUPP.
pub fn read_xattr(fd: RawFd, name: &CStr) -> io::Result<Vec<u8>> {
    #[cfg(target_os = "linux")]
    {
        sys::on_fd(fd, |target| sys::get(target, name))
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
/// first, then the ACL, as gnulib's `qcopy_acl` does. Residual: an NFSv4 ACL the destination
/// already has is not removed when `acl` has none; the mode set before it is what the server
/// makes of it.
pub fn write_fd(fd: RawFd, acl: &Acl) -> io::Result<()> {
    sys::write(fd, acl)
}

/// Copy the ACLs `acl`, a source's, to the file open on `fd`, its copy, already given the
/// source's mode (`write_fd`). A filesystem that holds no ACL fails it with EOPNOTSUPP, which
/// is no failure when the ACLs said nothing the mode does not (`loses_nothing`).
pub fn copy_to_fd(acl: &Acl, fd: RawFd) -> io::Result<()> {
    match write_fd(fd, acl) {
        Err(e) if loses_nothing(acl, &e) => Ok(()),
        written => written,
    }
}

/// Whether the failure `e` to copy the ACLs `acl` loses nothing of them: the destination holds
/// no ACL at all (EOPNOTSUPP), and `acl` is trivial -- the mode already carried everything.
pub fn loses_nothing(acl: &Acl, e: &io::Error) -> bool {
    let unsupported = e
        .raw_os_error()
        .is_some_and(|code| code == libc::EOPNOTSUPP || code == libc::ENOTSUP);
    unsupported && acl.is_trivial()
}

/// The mode a copy keeps where its source's ACLs `acl` -- `None` where they could not be read
/// -- could not be set on it, `mode` being the source's: one granting no more than the source
/// did.
///
/// An access ACL's mask shows as the mode's group bits, though the owning group itself may
/// have had less: its own entry, masked. Without the ACL those bits are the owning group's, so
/// they become that entry, masked. A native ACL may deny what the mode grants, and ACLs not
/// read may be anything: the group and other bits are then cleared. A default ACL lost costs
/// the copy itself nothing.
pub fn mode_without(acl: Option<&Acl>, mode: u32) -> u32 {
    let Some(acl) = acl.filter(|acl| acl.native_is_trivial()) else {
        return mode & !0o077;
    };
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

#[cfg(target_os = "linux")]
mod sys {
    use super::{absent, nfs4_is_trivial, Acl, Native, NativeKind, PosixAcl};
    use gettextrs::gettext;
    use std::ffi::{CStr, CString};
    use std::io;
    use std::os::unix::io::RawFd;

    /// What to read the attributes of: a descriptor, or a path followed or not.
    pub enum Target<'a> {
        Fd(RawFd),
        Path(&'a CStr, bool),
    }

    /// Run `call` on `fd`, or, where `fd` is an `O_PATH` descriptor that refuses it (EBADF),
    /// on its `/proc/self/fd/N` (`read_fd`). The first attribute call `call` makes is the one
    /// refused, so nothing is done twice.
    pub fn on_fd<T>(fd: RawFd, call: impl Fn(&Target) -> io::Result<T>) -> io::Result<T> {
        match call(&Target::Fd(fd)) {
            Err(e) if e.raw_os_error() == Some(libc::EBADF) => {
                crate::madefs::procfs_dir()?;
                let name = crate::madefs::proc_fd_name(fd);
                let path = CString::new(format!("/proc/{}", name.to_string_lossy()))
                    .expect("a formatted number has no NUL");
                call(&Target::Path(&path, true))
            }
            other => other,
        }
    }

    /// Call `call` with a buffer until it fits what is read, asking the size again when the
    /// attribute grew between the calls (ERANGE).
    fn sized(call: impl Fn(*mut u8, usize) -> isize) -> io::Result<Vec<u8>> {
        let mut buf = vec![0u8; 4096];
        for _ in 0..8 {
            let n = call(buf.as_mut_ptr(), buf.len());
            if let Ok(len) = usize::try_from(n) {
                buf.truncate(len);
                return Ok(buf);
            }
            let e = io::Error::last_os_error();
            if e.raw_os_error() != Some(libc::ERANGE) {
                return Err(e);
            }
            let need = usize::try_from(call(std::ptr::null_mut(), 0))
                .map_err(|_| io::Error::last_os_error())?;
            buf.resize(need.max(buf.len() * 2), 0);
        }
        Err(io::Error::from_raw_os_error(libc::ERANGE))
    }

    /// The attribute `name` of `target`.
    pub fn get(target: &Target, name: &CStr) -> io::Result<Vec<u8>> {
        let name = name.as_ptr();
        sized(|buf, len| unsafe {
            match *target {
                Target::Fd(fd) => libc::fgetxattr(fd, name, buf.cast(), len),
                Target::Path(p, true) => libc::getxattr(p.as_ptr(), name, buf.cast(), len),
                Target::Path(p, false) => libc::lgetxattr(p.as_ptr(), name, buf.cast(), len),
            }
        })
    }

    /// The names of the attributes of `target`, each ending in a NUL.
    fn list(target: &Target) -> io::Result<Vec<u8>> {
        sized(|buf, len| unsafe {
            match *target {
                Target::Fd(fd) => libc::flistxattr(fd, buf.cast(), len),
                Target::Path(p, true) => libc::listxattr(p.as_ptr(), buf.cast(), len),
                Target::Path(p, false) => libc::llistxattr(p.as_ptr(), buf.cast(), len),
            }
        })
    }

    /// The ACLs of `target`: the attributes listed are read, so that a file with none -- the
    /// usual case -- costs one call.
    pub fn read(target: &Target) -> io::Result<Acl> {
        match target {
            Target::Fd(fd) => on_fd(*fd, read_listed),
            path => read_listed(path),
        }
    }

    fn read_listed(target: &Target) -> io::Result<Acl> {
        let names = match list(target) {
            Err(e) if absent(&e) => return Ok(Acl::default()),
            names => names?,
        };
        let listed = |name: &CStr| -> io::Result<Option<Vec<u8>>> {
            if !names.split(|&b| b == 0).any(|n| n == name.to_bytes()) {
                return Ok(None);
            }
            match get(target, name) {
                Err(e) if absent(&e) => Ok(None),
                value => value.map(Some),
            }
        };
        let posix = |name| -> io::Result<Option<PosixAcl>> {
            let acl = listed(name)?
                .map(|b| PosixAcl::parse_xattr(&b))
                .transpose()?;
            Ok(acl.filter(|acl| !acl.entries.is_empty()))
        };
        let native = |kind, name| -> io::Result<Option<Native>> {
            Ok(listed(name)?.map(|bytes| Native { kind, bytes }))
        };
        Ok(Acl {
            access: posix(c"system.posix_acl_access")?,
            default: posix(c"system.posix_acl_default")?,
            native: match native(NativeKind::Nfs4, c"system.nfs4_acl")? {
                Some(nfs4) => Some(nfs4),
                None => native(NativeKind::Cifs, c"system.cifs_acl")?,
            },
        })
    }

    /// Set the attribute `name` of `target` to `value`.
    fn set(target: &Target, name: &CStr, value: &[u8]) -> io::Result<()> {
        let (name, ptr, len) = (name.as_ptr(), value.as_ptr().cast(), value.len());
        let ret = unsafe {
            match *target {
                Target::Fd(fd) => libc::fsetxattr(fd, name, ptr, len, 0),
                Target::Path(p, true) => libc::setxattr(p.as_ptr(), name, ptr, len, 0),
                Target::Path(p, false) => libc::lsetxattr(p.as_ptr(), name, ptr, len, 0),
            }
        };
        if ret != 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(())
    }

    /// Remove the attribute `name` of `target`; one it does not have, or cannot have, is
    /// removed already.
    fn remove(target: &Target, name: &CStr) -> io::Result<()> {
        let ret = unsafe {
            match *target {
                Target::Fd(fd) => libc::fremovexattr(fd, name.as_ptr()),
                Target::Path(p, true) => libc::removexattr(p.as_ptr(), name.as_ptr()),
                Target::Path(p, false) => libc::lremovexattr(p.as_ptr(), name.as_ptr()),
            }
        };
        if ret != 0 {
            let e = io::Error::last_os_error();
            if !absent(&e) {
                return Err(e);
            }
        }
        Ok(())
    }

    /// `super::write_fd`.
    pub fn write(fd: RawFd, acl: &Acl) -> io::Result<()> {
        let mut st: libc::stat = unsafe { std::mem::zeroed() };
        if unsafe { libc::fstat(fd, &mut st) } != 0 {
            return Err(io::Error::last_os_error());
        }
        let is_dir = st.st_mode & libc::S_IFMT == libc::S_IFDIR;
        on_fd(fd, |target| {
            let written = write_to(target, acl, is_dir);
            // A directory whose ACLs are not the source's must not keep a default ACL that
            // is not either -- its own, or one inherited -- for what is made in it later.
            if written.is_err() && is_dir {
                let _ = remove(target, c"system.posix_acl_default");
            }
            written
        })
    }

    /// The POSIX ACLs first -- set, or removed where `acl` has none beyond the mode -- then a
    /// native one, so that one the destination cannot hold fails only once the stale POSIX ones
    /// are gone. An NFSv4 ACL is written as read, trivial or not, in place of the
    /// destination's; where the source has none, a destination's own that says more than its
    /// mode cannot be removed (the NFS client takes no removal) and fails the write, so the
    /// copy does not pass for the source's.
    fn write_to(target: &Target, acl: &Acl, is_dir: bool) -> io::Result<()> {
        let posix = |name: &CStr, acl: Option<&PosixAcl>| match acl {
            Some(acl) => set(target, name, &acl.to_xattr()),
            None => remove(target, name),
        };
        let access = acl.access.as_ref().filter(|a| !a.is_trivial());
        posix(c"system.posix_acl_access", access)?;
        if is_dir {
            let default = acl.default.as_ref().filter(|a| !a.entries.is_empty());
            posix(c"system.posix_acl_default", default)?;
        }
        match &acl.native {
            Some(Native {
                kind: NativeKind::Nfs4,
                bytes,
            }) => set(target, c"system.nfs4_acl", bytes),
            Some(Native {
                kind: NativeKind::Darwin,
                ..
            }) => Err(io::Error::from_raw_os_error(libc::EOPNOTSUPP)),
            _ => match get(target, c"system.nfs4_acl") {
                Ok(own) if !nfs4_is_trivial(&own) => Err(io::Error::other(gettext(
                    "the destination's NFSv4 ACL cannot be removed",
                ))),
                Err(e) if !absent(&e) => Err(e),
                _ => Ok(()),
            },
        }
    }
}

#[cfg(target_os = "macos")]
mod sys {
    use super::{Acl, Native, NativeKind};
    use std::ffi::CStr;
    use std::io;
    use std::os::unix::io::RawFd;

    // libSystem's acl(3); the `libc` crate does not declare it. `acl_t` and `acl_entry_t` are
    // opaque pointers.
    extern "C" {
        fn acl_get_fd_np(fd: libc::c_int, acl_type: libc::c_uint) -> *mut libc::c_void;
        fn acl_get_file(path: *const libc::c_char, acl_type: libc::c_uint) -> *mut libc::c_void;
        fn acl_get_link_np(path: *const libc::c_char, acl_type: libc::c_uint) -> *mut libc::c_void;
        fn acl_get_entry(
            acl: *mut libc::c_void,
            entry_id: libc::c_int,
            entry_p: *mut *mut libc::c_void,
        ) -> libc::c_int;
        fn acl_size(acl: *mut libc::c_void) -> libc::ssize_t;
        fn acl_copy_ext(
            buf: *mut libc::c_void,
            acl: *mut libc::c_void,
            size: libc::ssize_t,
        ) -> libc::ssize_t;
        fn acl_free(obj_p: *mut libc::c_void) -> libc::c_int;
        fn acl_copy_int(buf: *const libc::c_void) -> *mut libc::c_void;
        fn acl_init(count: libc::c_int) -> *mut libc::c_void;
        fn acl_set_fd_np(
            fd: libc::c_int,
            acl: *mut libc::c_void,
            acl_type: libc::c_uint,
        ) -> libc::c_int;
    }
    const ACL_TYPE_EXTENDED: libc::c_uint = 0x0000_0100;
    const ACL_FIRST_ENTRY: libc::c_int = 0;

    pub enum Target<'a> {
        Fd(RawFd),
        Path(&'a CStr, bool),
    }

    /// The extended ACL of `target`, in `acl_copy_ext` form; none when it has no entry
    /// (ENOENT is how macOS says a file has no ACL).
    pub fn read(target: &Target) -> io::Result<Acl> {
        let acl = unsafe {
            match *target {
                Target::Fd(fd) => acl_get_fd_np(fd, ACL_TYPE_EXTENDED),
                Target::Path(p, true) => acl_get_file(p.as_ptr(), ACL_TYPE_EXTENDED),
                Target::Path(p, false) => acl_get_link_np(p.as_ptr(), ACL_TYPE_EXTENDED),
            }
        };
        if acl.is_null() {
            let e = io::Error::last_os_error();
            return match e.raw_os_error() {
                Some(libc::ENOENT) | Some(libc::ENOTSUP) => Ok(Acl::default()),
                _ => Err(e),
            };
        }
        let bytes = external(acl);
        unsafe { acl_free(acl) };
        Ok(Acl {
            native: bytes?.map(|bytes| Native {
                kind: NativeKind::Darwin,
                bytes,
            }),
            ..Acl::default()
        })
    }

    /// `super::write_fd`: the extended ACL `acl` has, or an empty one, which removes the one
    /// the file has (as `chmod -N` does). macOS has no POSIX ACL to write.
    pub fn write(fd: RawFd, acl: &Acl) -> io::Result<()> {
        if acl.access.is_some() || acl.default.is_some() {
            return Err(io::Error::from_raw_os_error(libc::EOPNOTSUPP));
        }
        let made = match &acl.native {
            None => unsafe { acl_init(0) },
            Some(Native {
                kind: NativeKind::Darwin,
                bytes,
            }) => {
                super::check_darwin_external(bytes)?;
                unsafe { acl_copy_int(bytes.as_ptr().cast()) }
            }
            Some(_) => return Err(io::Error::from_raw_os_error(libc::EOPNOTSUPP)),
        };
        if made.is_null() {
            return Err(io::Error::last_os_error());
        }
        let ret = unsafe { acl_set_fd_np(fd, made, ACL_TYPE_EXTENDED) };
        let e = io::Error::last_os_error();
        unsafe { acl_free(made) };
        if ret != 0 {
            return Err(e);
        }
        Ok(())
    }

    /// `acl` in external form, or `None` for one with no entry.
    fn external(acl: *mut libc::c_void) -> io::Result<Option<Vec<u8>>> {
        let mut entry = std::ptr::null_mut();
        if unsafe { acl_get_entry(acl, ACL_FIRST_ENTRY, &mut entry) } != 0 {
            return Ok(None);
        }
        let size = unsafe { acl_size(acl) };
        let mut buf = vec![0u8; usize::try_from(size).map_err(|_| io::Error::last_os_error())?];
        let n = unsafe { acl_copy_ext(buf.as_mut_ptr().cast(), acl, size) };
        let n = usize::try_from(n).map_err(|_| io::Error::last_os_error())?;
        buf.truncate(n);
        Ok(Some(buf))
    }
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

    /// The xattr bytes of the entries `(tag, perm, id)`.
    fn xattr(entries: &[(u16, u16, u32)]) -> Vec<u8> {
        let mut out = 2u32.to_le_bytes().to_vec();
        for &(tag, perm, id) in entries {
            out.extend(tag.to_le_bytes());
            out.extend(perm.to_le_bytes());
            out.extend(id.to_le_bytes());
        }
        out
    }

    const NONE: u32 = u32::MAX;

    /// An ACL with a named user and group, and a mask.
    fn full() -> PosixAcl {
        let e = |tag, perm| Entry { tag, perm };
        PosixAcl {
            entries: vec![
                e(Tag::UserObj, 6),
                e(Tag::User(1000), 4),
                e(Tag::User(1001), 7),
                e(Tag::GroupObj, 4),
                e(Tag::Group(50), 5),
                e(Tag::Mask, 7),
                e(Tag::Other, 0),
            ],
        }
    }

    /// The xattr form is the kernel's, byte for byte, and reads back as written.
    #[test]
    fn xattr_round_trip() {
        let acl = full();
        let bytes = acl.to_xattr();
        assert_eq!(
            bytes,
            xattr(&[
                (0x01, 6, NONE),
                (0x02, 4, 1000),
                (0x02, 7, 1001),
                (0x04, 4, NONE),
                (0x08, 5, 50),
                (0x10, 7, NONE),
                (0x20, 0, NONE),
            ])
        );
        assert_eq!(PosixAcl::parse_xattr(&bytes).unwrap(), acl);
        assert!(!acl.is_trivial());
        // The id of an entry that names nobody is not kept.
        let minimal = xattr(&[(0x01, 7, 0), (0x04, 5, 0), (0x20, 5, 0)]);
        let parsed = PosixAcl::parse_xattr(&minimal).unwrap();
        assert!(parsed.is_trivial());
        assert_eq!(
            parsed.to_xattr(),
            xattr(&[(1, 7, NONE), (4, 5, NONE), (0x20, 5, NONE)])
        );
        // No entries: the empty ACL, which stands for none.
        assert!(PosixAcl::parse_xattr(&xattr(&[]))
            .unwrap()
            .entries
            .is_empty());
    }

    /// What the kernel would refuse is refused.
    #[test]
    fn xattr_validation_rejects() {
        let base = [(0x01, 6, NONE), (0x04, 4, NONE), (0x20, 4, NONE)];
        assert!(PosixAcl::parse_xattr(&xattr(&base)).is_ok());
        let mut other_version = xattr(&base);
        other_version[0] = 3;
        let mut torn = xattr(&base);
        torn.pop();
        let bad: [Vec<u8>; 12] = [
            other_version,
            torn,
            vec![2, 0],
            // Unsorted: group before user, named users out of order.
            xattr(&[(0x04, 4, NONE), (0x01, 6, NONE), (0x20, 4, NONE)]),
            xattr(&[
                (0x01, 6, NONE),
                (0x02, 4, 9),
                (0x02, 4, 8),
                (0x04, 4, NONE),
                (0x10, 4, NONE),
                (0x20, 4, NONE),
            ]),
            // A named user twice; the owner twice.
            xattr(&[
                (0x01, 6, NONE),
                (0x02, 4, 9),
                (0x02, 4, 9),
                (0x04, 4, NONE),
                (0x10, 4, NONE),
                (0x20, 4, NONE),
            ]),
            xattr(&[
                (0x01, 6, NONE),
                (0x01, 6, NONE),
                (0x04, 4, NONE),
                (0x20, 4, NONE),
            ]),
            // A named group with no mask.
            xattr(&[
                (0x01, 6, NONE),
                (0x04, 4, NONE),
                (0x08, 4, 5),
                (0x20, 4, NONE),
            ]),
            // No others; an unknown tag; a permission beyond rwx.
            xattr(&[(0x01, 6, NONE), (0x04, 4, NONE)]),
            xattr(&[
                (0x01, 6, NONE),
                (0x04, 4, NONE),
                (0x40, 4, NONE),
                (0x20, 4, NONE),
            ]),
            xattr(&[(0x01, 8, NONE), (0x04, 4, NONE), (0x20, 4, NONE)]),
            // A mask after others.
            xattr(&[
                (0x01, 6, NONE),
                (0x04, 4, NONE),
                (0x20, 4, NONE),
                (0x10, 4, NONE),
            ]),
        ];
        for bytes in bad {
            assert!(PosixAcl::parse_xattr(&bytes).is_err(), "{bytes:?}");
        }
        // A mask with nobody named is allowed.
        let masked = [
            (0x01, 6, NONE),
            (0x04, 4, NONE),
            (0x10, 4, NONE),
            (0x20, 4, NONE),
        ];
        assert!(PosixAcl::parse_xattr(&xattr(&masked)).is_ok());
    }

    /// The text form names the users and groups it can, gives the id as a fourth field, and
    /// reads back to the same ACL.
    #[test]
    fn text_round_trip() {
        // Ids nobody has: named by number.
        let e = |tag, perm| Entry { tag, perm };
        let unknown = PosixAcl {
            entries: vec![
                e(Tag::UserObj, 6),
                e(Tag::User(3_999_999_001), 4),
                e(Tag::GroupObj, 4),
                e(Tag::Group(3_999_999_002), 3),
                e(Tag::Mask, 7),
                e(Tag::Other, 1),
            ],
        };
        let text = unknown.to_text();
        assert_eq!(
            text,
            "user::rw-,user:3999999001:r--:3999999001,group::r--,\
             group:3999999002:-wx:3999999002,mask::rwx,other::--x"
        );
        assert_eq!(PosixAcl::from_text(&text).unwrap(), unknown);

        // A user with a name: the name, then the id.
        let root = crate::user::lookup_by_uid(0).unwrap().map(|u| u.name);
        if let Some(name) = root.and_then(|n| n.into_string().ok()) {
            let acl = PosixAcl {
                entries: vec![
                    e(Tag::UserObj, 7),
                    e(Tag::User(0), 5),
                    e(Tag::GroupObj, 5),
                    e(Tag::Mask, 5),
                    e(Tag::Other, 0),
                ],
            };
            let text = acl.to_text();
            assert!(text.contains(&format!("user:{name}:r-x:0,")), "{text}");
            assert_eq!(PosixAcl::from_text(&text).unwrap(), acl);
        }

        // Without id fields, by first letters, out of order, a line each.
        let acl = PosixAcl::from_text("o::r--\nm::rw-\nu:3999999001:rw-\ng::r--\nu::rwx").unwrap();
        assert_eq!(
            acl.to_text(),
            "user::rwx,user:3999999001:rw-:3999999001,group::r--,mask::rw-,other::r--"
        );
        // A name nobody has, with its id: the id.
        let acl = PosixAcl::from_text(
            "user::rw-,user:no-such-user-x:r--:4321,group::r--,mask::r--,other::---",
        )
        .unwrap();
        assert_eq!(acl.entries[1].tag, Tag::User(4321));
    }

    /// `posix_acl_create`: the owner, mask and others entries are masked by the mode, a named
    /// entry is not, and the owning group's is masked only where there is no mask.
    #[test]
    fn inherited_masks_by_the_creating_mode() {
        let default = PosixAcl::from_text("u::rwx,u:3999999001:rwx,g::r-x,m::rwx,o::---").unwrap();
        let (acl, bits) = default.inherited(0o754);
        assert_eq!(bits, 0o750);
        let want = PosixAcl::from_text("u::rwx,u:3999999001:rwx,g::r-x,m::r-x,o::---").unwrap();
        assert_eq!(acl, want);

        let (acl, bits) = default.inherited(0o600);
        assert_eq!(bits, 0o600);
        let want = PosixAcl::from_text("u::rw-,u:3999999001:rwx,g::r-x,m::---,o::---").unwrap();
        assert_eq!(acl, want);

        let plain = PosixAcl::from_text("u::rwx,g::rwx,o::r-x").unwrap();
        let (acl, bits) = plain.inherited(0o751);
        assert_eq!(bits, 0o751);
        assert_eq!(acl, PosixAcl::from_text("u::rwx,g::r-x,o::--x").unwrap());
    }

    #[test]
    fn text_rejects() {
        for text in [
            "",
            "user::rw-,group::r--",
            "user::rw-,group::r--,other::r--,other::r--",
            "user::rwz,group::r--,other::r--",
            "user::rw-,group::r--,other:x:r--",
            "user::rw-,group::r--,other::r--:5:6",
            "user::rw-,user:no-such-user-x:r--,group::r--,mask::r--,other::---",
            "user::rw-,user:5:r--:x,group::r--,mask::r--,other::---",
            "user::rw-,user:5:r--,group::r--,other::---",
            "everyone::rw-,group::r--,other::r--",
        ] {
            assert!(PosixAcl::from_text(text).is_err(), "{text:?}");
        }
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
        assert!(loses_nothing(&trivial, &unsupported));
        assert!(loses_nothing(&trivial, &notsup));
        assert!(!loses_nothing(&trivial, &denied));
        assert!(!loses_nothing(&named, &unsupported));
        assert!(!loses_nothing(&default_only, &unsupported));
        let darwin = Acl {
            native: Some(Native {
                kind: NativeKind::Darwin,
                bytes: vec![1],
            }),
            ..Acl::default()
        };
        assert!(!loses_nothing(&darwin, &unsupported));
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

    /// What `write_fd` writes reads back, through a descriptor and through an `O_PATH` one (the
    /// procfs route); it replaces what was there, so an ACL the source lacks is removed, and a
    /// directory's default ACL with it. Skipped where `setfacl` is missing or the filesystem
    /// takes no ACLs.
    #[cfg(target_os = "linux")]
    #[test]
    fn writes_replace_what_was_there() {
        use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
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
        copy_to_fd(&acl, file.as_raw_fd()).unwrap();
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
}
