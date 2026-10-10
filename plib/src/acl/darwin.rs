//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! macOS: extended (NFSv4-style) ACLs through the libSystem `acl(3)` calls, kept in their
//! external form (`acl_copy_ext`), and read as or made from an `Nfs4Acl` for an archive.

use super::nfs4::{self, Ace, AceKind, Named, Nfs4Acl, Who};
use super::{Acl, Native, NativeKind};
use std::ffi::CStr;
use std::io;
use std::os::unix::io::RawFd;

// libSystem's acl(3) and membership(3); the `libc` crate declares neither. `acl_t`,
// `acl_entry_t`, `acl_permset_t` and `acl_flagset_t` are opaque pointers; `acl_tag_t`,
// `acl_perm_t` and `acl_flag_t` are enums; a `uuid_t` is 16 bytes.
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
    fn acl_create_entry(
        acl_p: *mut *mut libc::c_void,
        entry_p: *mut *mut libc::c_void,
    ) -> libc::c_int;
    fn acl_get_tag_type(entry: *mut libc::c_void, tag_p: *mut libc::c_int) -> libc::c_int;
    fn acl_set_tag_type(entry: *mut libc::c_void, tag: libc::c_int) -> libc::c_int;
    fn acl_get_qualifier(entry: *mut libc::c_void) -> *mut libc::c_void;
    fn acl_set_qualifier(entry: *mut libc::c_void, qualifier: *const libc::c_void) -> libc::c_int;
    fn acl_get_permset(entry: *mut libc::c_void, permset_p: *mut *mut libc::c_void) -> libc::c_int;
    fn acl_set_permset(entry: *mut libc::c_void, permset: *mut libc::c_void) -> libc::c_int;
    fn acl_get_perm_np(permset: *mut libc::c_void, perm: libc::c_int) -> libc::c_int;
    fn acl_clear_perms(permset: *mut libc::c_void) -> libc::c_int;
    fn acl_add_perm(permset: *mut libc::c_void, perm: libc::c_int) -> libc::c_int;
    fn acl_get_flagset_np(obj: *mut libc::c_void, flagset_p: *mut *mut libc::c_void)
        -> libc::c_int;
    fn acl_set_flagset_np(obj: *mut libc::c_void, flagset: *mut libc::c_void) -> libc::c_int;
    fn acl_get_flag_np(flagset: *mut libc::c_void, flag: libc::c_int) -> libc::c_int;
    fn acl_clear_flags_np(flagset: *mut libc::c_void) -> libc::c_int;
    fn acl_add_flag_np(flagset: *mut libc::c_void, flag: libc::c_int) -> libc::c_int;
    fn mbr_uuid_to_id(uu: *const u8, id: *mut libc::id_t, id_type: *mut libc::c_int)
        -> libc::c_int;
    fn mbr_uid_to_uuid(uid: libc::uid_t, uu: *mut u8) -> libc::c_int;
    fn mbr_gid_to_uuid(gid: libc::gid_t, uu: *mut u8) -> libc::c_int;
}
const ACL_TYPE_EXTENDED: libc::c_uint = 0x0000_0100;
const ACL_FIRST_ENTRY: libc::c_int = 0;
const ACL_NEXT_ENTRY: libc::c_int = -1;
const ACL_EXTENDED_ALLOW: libc::c_int = 1;
const ACL_EXTENDED_DENY: libc::c_int = 2;
const ID_TYPE_UID: libc::c_int = 0;
const ID_TYPE_GID: libc::c_int = 1;

/// Each NFSv4 access mask bit, and the `acl_perm_t` macOS has for it (`ACL_READ_DATA` ...), as
/// libarchive maps them.
const PERMS: [(u32, libc::c_int); 14] = [
    (nfs4::READ_DATA, 1 << 1),
    (nfs4::WRITE_DATA, 1 << 2),
    (nfs4::EXECUTE, 1 << 3),
    (nfs4::DELETE, 1 << 4),
    (nfs4::APPEND_DATA, 1 << 5),
    (nfs4::DELETE_CHILD, 1 << 6),
    (nfs4::READ_ATTRIBUTES, 1 << 7),
    (nfs4::WRITE_ATTRIBUTES, 1 << 8),
    (nfs4::READ_NAMED_ATTRS, 1 << 9),
    (nfs4::WRITE_NAMED_ATTRS, 1 << 10),
    (nfs4::READ_ACL, 1 << 11),
    (nfs4::WRITE_ACL, 1 << 12),
    (nfs4::WRITE_OWNER, 1 << 13),
    (nfs4::SYNCHRONIZE, 1 << 20),
];

/// Each NFSv4 flag bit macOS has an `acl_flag_t` for (`ACL_ENTRY_INHERITED` ...). It has none
/// for SUCCESSFUL_ACCESS or FAILED_ACCESS, which only audit and alarm entries use.
const FLAGS: [(u32, libc::c_int); 5] = [
    (nfs4::INHERITED, 1 << 4),
    (nfs4::FILE_INHERIT, 1 << 5),
    (nfs4::DIRECTORY_INHERIT, 1 << 6),
    (nfs4::NO_PROPAGATE_INHERIT, 1 << 7),
    (nfs4::INHERIT_ONLY, 1 << 8),
];

/// Something `acl(3)` allocated -- an `acl_t`, a qualifier -- freed by `acl_free` when dropped,
/// whatever path is taken.
struct Owned(*mut libc::c_void);

impl Owned {
    /// `ptr`, or the failure that made it null.
    fn new(ptr: *mut libc::c_void) -> io::Result<Owned> {
        if ptr.is_null() {
            return Err(io::Error::last_os_error());
        }
        Ok(Owned(ptr))
    }
}

impl Drop for Owned {
    fn drop(&mut self) {
        unsafe { acl_free(self.0) };
    }
}

/// Fail with the last error unless the `acl(3)` call that returned `ret` succeeded.
fn check(ret: libc::c_int) -> io::Result<()> {
    if ret != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

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
    let acl = match Owned::new(acl) {
        Err(e) if matches!(e.raw_os_error(), Some(libc::ENOENT | libc::ENOTSUP)) => {
            return Ok(Acl::default())
        }
        acl => acl?,
    };
    let mut entry = std::ptr::null_mut();
    if unsafe { acl_get_entry(acl.0, ACL_FIRST_ENTRY, &mut entry) } != 0 {
        return Ok(Acl::default());
    }
    Ok(Acl {
        native: Some(Native {
            kind: NativeKind::Darwin,
            bytes: external(&acl)?,
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
        None => Owned::new(unsafe { acl_init(0) })?,
        Some(Native {
            kind: NativeKind::Darwin,
            bytes,
        }) => internal(bytes)?,
        Some(_) => return Err(io::Error::from_raw_os_error(libc::EOPNOTSUPP)),
    };
    check(unsafe { acl_set_fd_np(fd, made.0, ACL_TYPE_EXTENDED) })
}

/// The ACL the external form `bytes` holds, checked whole first (`check_darwin_external`):
/// `acl_copy_int` reads it with no length.
fn internal(bytes: &[u8]) -> io::Result<Owned> {
    super::check_darwin_external(bytes)?;
    Owned::new(unsafe { acl_copy_int(bytes.as_ptr().cast()) })
}

/// `acl` in external form.
fn external(acl: &Owned) -> io::Result<Vec<u8>> {
    let size = unsafe { acl_size(acl.0) };
    let mut buf = vec![0u8; usize::try_from(size).map_err(|_| io::Error::last_os_error())?];
    let n = unsafe { acl_copy_ext(buf.as_mut_ptr().cast(), acl.0, size) };
    let n = usize::try_from(n).map_err(|_| io::Error::last_os_error())?;
    buf.truncate(n);
    Ok(buf)
}

/// The external form `bytes` as an NFSv4 ACL, as libarchive reads one: each allow or deny
/// entry, its user or group found from its UUID, and its permissions and flags mapped. An
/// entry of another kind, or a UUID that is no user's or group's, fails it: leaving it out
/// could drop a deny.
pub fn to_nfs4(bytes: &[u8]) -> io::Result<Nfs4Acl> {
    let acl = internal(bytes)?;
    let mut aces = Vec::new();
    let mut entry = std::ptr::null_mut();
    let mut which = ACL_FIRST_ENTRY;
    while unsafe { acl_get_entry(acl.0, which, &mut entry) } == 0 {
        aces.push(entry_ace(entry)?);
        which = ACL_NEXT_ENTRY;
    }
    Ok(Nfs4Acl { aces })
}

/// The ACE the ACL entry `entry` is.
fn entry_ace(entry: *mut libc::c_void) -> io::Result<Ace> {
    let unsupported = || io::Error::from_raw_os_error(libc::EOPNOTSUPP);
    let mut tag = 0;
    check(unsafe { acl_get_tag_type(entry, &mut tag) })?;
    let kind = match tag {
        ACL_EXTENDED_ALLOW => AceKind::Allow,
        ACL_EXTENDED_DENY => AceKind::Deny,
        _ => return Err(unsupported()),
    };
    let qualifier = Owned::new(unsafe { acl_get_qualifier(entry) })?;
    let (mut id, mut id_type) = (0, 0);
    let found = unsafe { mbr_uuid_to_id(qualifier.0.cast(), &mut id, &mut id_type) };
    if found != 0 {
        return Err(io::Error::from_raw_os_error(found));
    }
    let who = match id_type {
        ID_TYPE_UID => Who::User(Named::Id(id)),
        ID_TYPE_GID => Who::Group(Named::Id(id)),
        _ => return Err(unsupported()),
    };
    let mut permset = std::ptr::null_mut();
    check(unsafe { acl_get_permset(entry, &mut permset) })?;
    let mut flagset = std::ptr::null_mut();
    check(unsafe { acl_get_flagset_np(entry, &mut flagset) })?;
    Ok(Ace {
        who,
        perms: bits(&PERMS, |p| unsafe { acl_get_perm_np(permset, p) })?,
        flags: bits(&FLAGS, |f| unsafe { acl_get_flag_np(flagset, f) })?,
        kind,
    })
}

/// The NFSv4 bits of `map` whose macOS value `has` finds set (1), not (0), or fails on (-1).
fn bits(map: &[(u32, libc::c_int)], has: impl Fn(libc::c_int) -> libc::c_int) -> io::Result<u32> {
    let mut bits = 0;
    for &(bit, value) in map {
        match has(value) {
            1 => bits |= bit,
            0 => {}
            _ => return Err(io::Error::last_os_error()),
        }
    }
    Ok(bits)
}

/// The external form of `acl`, for a file of mode `mode`, as libarchive writes one on macOS:
/// an entry per allow or deny ACE naming a user or group, its UUID found from its id. The
/// `owner@`, `group@` and `everyone@` entries are left out, as macOS has no place for them --
/// only where the mode says all they do (`Nfs4Acl::specials_said_by_mode`); any other fails it
/// (EOPNOTSUPP), as do an audit or alarm ACE, a flag macOS has none for, and a user or group
/// known only by a name.
pub fn from_nfs4(acl: &Nfs4Acl, mode: u32) -> io::Result<Vec<u8>> {
    if !acl.specials_said_by_mode(mode) {
        return Err(io::Error::from_raw_os_error(libc::EOPNOTSUPP));
    }
    let named: Vec<&Ace> = acl
        .aces
        .iter()
        .filter(|ace| matches!(ace.who, Who::User(_) | Who::Group(_)))
        .collect();
    let count = libc::c_int::try_from(named.len())
        .map_err(|_| io::Error::from_raw_os_error(libc::EINVAL))?;
    let mut made = Owned::new(unsafe { acl_init(count) })?;
    for ace in named {
        add_entry(&mut made, ace)?;
    }
    external(&made)
}

/// Add to `acl` the entry `ace` is.
fn add_entry(acl: &mut Owned, ace: &Ace) -> io::Result<()> {
    let unsupported = || io::Error::from_raw_os_error(libc::EOPNOTSUPP);
    let tag = match ace.kind {
        AceKind::Allow => ACL_EXTENDED_ALLOW,
        AceKind::Deny => ACL_EXTENDED_DENY,
        AceKind::Audit | AceKind::Alarm => return Err(unsupported()),
    };
    let flags_known = FLAGS.iter().fold(0, |all, &(bit, _)| all | bit);
    if ace.flags & !flags_known != 0 {
        return Err(unsupported());
    }
    let mut uuid = [0u8; 16];
    let made = match &ace.who {
        Who::User(Named::Id(uid)) => unsafe { mbr_uid_to_uuid(*uid, uuid.as_mut_ptr()) },
        Who::Group(Named::Id(gid)) => unsafe { mbr_gid_to_uuid(*gid, uuid.as_mut_ptr()) },
        _ => libc::EINVAL,
    };
    if made != 0 {
        return Err(io::Error::from_raw_os_error(made));
    }
    // `acl_create_entry` may move the ACL: it takes it by reference.
    let mut entry = std::ptr::null_mut();
    check(unsafe { acl_create_entry(&mut acl.0, &mut entry) })?;
    check(unsafe { acl_set_tag_type(entry, tag) })?;
    check(unsafe { acl_set_qualifier(entry, uuid.as_ptr().cast()) })?;
    let mut permset = std::ptr::null_mut();
    check(unsafe { acl_get_permset(entry, &mut permset) })?;
    check(unsafe { acl_clear_perms(permset) })?;
    for &(bit, perm) in &PERMS {
        if ace.perms & bit != 0 {
            check(unsafe { acl_add_perm(permset, perm) })?;
        }
    }
    check(unsafe { acl_set_permset(entry, permset) })?;
    let mut flagset = std::ptr::null_mut();
    check(unsafe { acl_get_flagset_np(entry, &mut flagset) })?;
    check(unsafe { acl_clear_flags_np(flagset) })?;
    for &(bit, flag) in &FLAGS {
        if ace.flags & bit != 0 {
            check(unsafe { acl_add_flag_np(flagset, flag) })?;
        }
    }
    check(unsafe { acl_set_flagset_np(entry, flagset) })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// An NFSv4 ACL naming this process's user and group, with every permission and flag macOS
    /// has, made into an extended ACL and read back, is the same ACL; `owner@`, `group@` and
    /// `everyone@` entries are left out; an audit entry and a name with no local user fail.
    #[test]
    fn nfs4_round_trips_through_an_extended_acl() {
        let (uid, gid) = unsafe { (libc::getuid(), libc::getgid()) };
        let all_perms = PERMS.iter().fold(0, |all, &(bit, _)| all | bit);
        let named = vec![
            Ace {
                who: Who::User(Named::Id(uid)),
                perms: all_perms,
                flags: nfs4::FILE_INHERIT | nfs4::DIRECTORY_INHERIT,
                kind: AceKind::Allow,
            },
            Ace {
                who: Who::Group(Named::Id(gid)),
                perms: nfs4::WRITE_DATA | nfs4::DELETE,
                flags: nfs4::INHERITED | nfs4::INHERIT_ONLY | nfs4::NO_PROPAGATE_INHERIT,
                kind: AceKind::Deny,
            },
        ];
        let acl = Nfs4Acl { aces: named };
        let bytes = from_nfs4(&acl, 0o640).unwrap();
        assert_eq!(to_nfs4(&bytes).unwrap(), acl);

        let with_mode = acl.with_mode_aces(0o754);
        assert_eq!(
            to_nfs4(&from_nfs4(&with_mode, 0o754).unwrap()).unwrap(),
            acl
        );
        // A deny the mode does not repeat cannot be left out.
        let deny = Nfs4Acl::from_ace_text("group@:w::deny").unwrap();
        let mut denied = acl.clone();
        denied.aces.insert(0, deny.aces[0].clone());
        assert!(from_nfs4(&denied, 0o664).is_err());

        let audit = Nfs4Acl {
            aces: vec![Ace {
                kind: AceKind::Audit,
                ..acl.aces[0].clone()
            }],
        };
        assert!(from_nfs4(&audit, 0o640).is_err());
        let by_name = Nfs4Acl {
            aces: vec![Ace {
                who: Who::User(Named::Name("no-such-user-x".to_string())),
                ..acl.aces[0].clone()
            }],
        };
        assert!(from_nfs4(&by_name, 0o640).is_err());
    }
}
