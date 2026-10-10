//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Linux: POSIX ACLs in the `system.posix_acl_*` extended attributes, read and written without
//! libacl; NFSv4 and CIFS ACLs as the attributes the filesystem hands out.

use super::{absent, nfs4_is_trivial, Acl, Native, NativeKind, PosixAcl};
pub(crate) use crate::xattr::Target;
use crate::xattr::{self, get, list, on_fd, set};
use gettextrs::gettext;
use std::ffi::CStr;
use std::io;
use std::os::unix::io::RawFd;

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

/// Remove the attribute `name` of `target`; one it does not have, or cannot have, is
/// removed already.
fn remove(target: &Target, name: &CStr) -> io::Result<()> {
    match xattr::remove(target, name) {
        Err(e) if absent(&e) => Ok(()),
        removed => removed,
    }
}

/// `super::write_fd`.
pub fn write(fd: RawFd, acl: &Acl) -> io::Result<()> {
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::fstat(fd, &mut st) } != 0 {
        return Err(io::Error::last_os_error());
    }
    let is_dir = st.st_mode & libc::S_IFMT == libc::S_IFDIR;
    let written = on_fd(fd, |target| {
        let written = write_to(target, acl, is_dir);
        // A file whose ACLs are not the source's keeps none: not an access ACL written
        // before what failed, nor a default ACL -- its own, or one inherited -- for what is
        // made in a directory later.
        if written.is_err() {
            let _ = remove(target, c"system.posix_acl_access");
            if is_dir {
                let _ = remove(target, c"system.posix_acl_default");
            }
        }
        written
    });
    match written {
        // A special file is written only through its pin's `self/fd/N`: with no procfs to
        // write through, none can be written, as on a filesystem that holds none.
        Err(e) if is_special(st.st_mode) && xattr::no_procfs_route(&e) => {
            Err(io::Error::from_raw_os_error(libc::EOPNOTSUPP))
        }
        written => written,
    }
}

/// Whether the mode `mode` is a FIFO's, a device's or a socket's.
fn is_special(mode: libc::mode_t) -> bool {
    matches!(
        mode & libc::S_IFMT,
        libc::S_IFIFO | libc::S_IFCHR | libc::S_IFBLK | libc::S_IFSOCK
    )
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
