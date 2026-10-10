//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Extended attributes of an open file: listing, reading, writing and removing them, and
//! copying them from one file to another, as cp -a and mv do.
//!
//! POSIX.2024 has none. Each system's calls are used through a descriptor:
//! - Linux: `flistxattr`, `fgetxattr`, `fsetxattr`, `fremovexattr`. An `O_PATH` descriptor
//!   refuses them (EBADF); the file it pins is then reached through its `/proc/self/fd/N`
//!   (`on_fd`) -- a symbolic link pinned `O_PATH | O_NOFOLLOW` too: that path ends at the
//!   link itself, not at what it names.
//! - macOS: the same calls, which take a position and options besides.
//! - Anything else: none is read or written (EOPNOTSUPP).

use std::ffi::{CStr, CString};
use std::io;
use std::os::unix::io::{AsRawFd, FromRawFd, OwnedFd, RawFd};

/// The names `copy_fd` never copies.
///
/// What the ACL code copies with the mode, or what no other file can take:
/// `system.posix_acl_access`, `system.posix_acl_default`, the NFSv4 and CIFS ACLs, and the
/// macOS ACL (`com.apple.system.Security`, hidden from listing anyway). Then what libattr's
/// `/etc/xattr.conf`, which GNU cp follows, has it skip: XFS's own attributes, Beagle's index
/// data, the EVM signature (which only the kernel may write) and AFS metadata.
const SKIPPED: [&[u8]; 12] = [
    b"system.posix_acl_access",
    b"system.posix_acl_default",
    b"system.nfs4_acl",
    b"system.nfs4_acl_xdr",
    b"system.nfs4acl",
    b"system.cifs_acl",
    b"com.apple.system.Security",
    b"trusted.SGI_ACL_DEFAULT",
    b"trusted.SGI_ACL_FILE",
    b"trusted.SGI_CAP_FILE",
    b"trusted.SGI_MAC_FILE",
    b"security.evm",
];

/// The prefixes of the names `copy_fd` never copies (`SKIPPED`).
const SKIPPED_PREFIXES: [&[u8]; 4] = [b"trusted.SGI_DMI_", b"xfsroot.", b"user.Beagle.", b"afs."];

/// Whether `copy_fd` copies the attribute `name`: all but the ACLs, which go with the mode, and
/// the names libattr skips (`SKIPPED`). `security.selinux` is copied, as GNU cp -a copies it;
/// so is `security.capability`, which only a process with CAP_SETFCAP can set.
pub fn is_copied(name: &[u8]) -> bool {
    !SKIPPED.contains(&name) && !SKIPPED_PREFIXES.iter().any(|p| name.starts_with(p))
}

/// Whether `copy_fd` copies the attribute `name` to a copy whose owner is the source's
/// (`owner_kept`) or not: `is_copied`, but a file capability (`security.capability`) only to
/// a copy owned as the source is -- one whose owner could not be given, as its set-user-ID
/// and set-group-ID bits are withheld, gets none.
pub fn is_copied_to(name: &[u8], owner_kept: bool) -> bool {
    is_copied(name) && (owner_kept || name != b"security.capability")
}

/// Whether the attribute `name` an archive records is restored from it: a `user.` one only
/// (that `is_copied` admits), as GNU tar --xattrs restores by default, even for root. The
/// others -- `security.capability`, `security.selinux`, `trusted.`, `system.` -- say what
/// privilege or which policy a file has, which is not an archive's to grant; a copy of a file
/// on disk (`copy_fd`) takes them by `is_copied_to` instead.
pub fn is_restored_from_archive(name: &[u8]) -> bool {
    name.starts_with(b"user.") && is_copied(name)
}

/// Whether `e` says the file can hold no extended attribute, or none of that name.
pub fn unsupported(e: &io::Error) -> bool {
    e.raw_os_error()
        .is_some_and(|code| code == libc::EOPNOTSUPP || code == libc::ENOTSUP)
}

/// Whether `e` says the file has no attribute of the name asked.
pub fn no_such_attr(e: &io::Error) -> bool {
    #[cfg(target_os = "linux")]
    let code = Some(libc::ENODATA);
    #[cfg(target_os = "macos")]
    let code = Some(libc::ENOATTR);
    // None is read anywhere else.
    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    let code = None;
    code.is_some_and(|code| e.raw_os_error() == Some(code))
}

/// The names of the extended attributes of the file open on `fd`, each as the system lists it.
/// A file with none costs one call and no allocation.
pub fn list_fd(fd: RawFd) -> io::Result<Vec<CString>> {
    let names = sys::list_fd(fd)?;
    Ok(names
        .split(|&b| b == 0)
        .filter(|name| !name.is_empty())
        .map(|name| CString::new(name).expect("split at every NUL"))
        .collect())
}

/// The value of the extended attribute `name` of the file open on `fd`.
pub fn get_fd(fd: RawFd, name: &CStr) -> io::Result<Vec<u8>> {
    sys::get_fd(fd, name)
}

/// Give the file open on `fd` the extended attribute `name` with the value `value`, created or
/// replaced.
pub fn set_fd(fd: RawFd, name: &CStr, value: &[u8]) -> io::Result<()> {
    sys::set_fd(fd, name, value)
}

/// Remove the extended attribute `name` of the file open on `fd`.
pub fn remove_fd(fd: RawFd, name: &CStr) -> io::Result<()> {
    sys::remove_fd(fd, name)
}

/// The extended attributes of a file, each that `is_copied` admits, read to be copied later
/// or archived (`read_fd`).
#[derive(Debug, Default)]
pub struct Values {
    /// Each one read, with its value, in the order the system listed them.
    pub read: Vec<(CString, Vec<u8>)>,
    /// Each one listed that could not be read, and why.
    pub unread: Vec<(CString, io::Error)>,
}

/// The extended attributes of the file open on `fd` (`Values`), `O_PATH` or not (Linux,
/// through procfs as `copy_fd` reads one). A file with none costs one call; where it can
/// hold none at all (`unsupported`), it has none.
pub fn read_fd(fd: RawFd) -> io::Result<Values> {
    match sys::list_fd(fd) {
        Err(e) if unsupported(&e) => Ok(Values::default()),
        Err(e) => Err(e),
        Ok(names) => Ok(read_listed(&names, |name| get_fd(fd, name))),
    }
}

/// The values of the attributes `names` lists, each ending in a NUL as the system lists
/// them, that `is_copied` admits, each read by `get`. One gone since it was listed, or
/// refused as unsupported, is left out.
pub(crate) fn read_listed(names: &[u8], get: impl Fn(&CStr) -> io::Result<Vec<u8>>) -> Values {
    let mut values = Values::default();
    for name in names
        .split(|&b| b == 0)
        .filter(|name| !name.is_empty() && is_copied(name))
    {
        let name = CString::new(name).expect("split at every NUL");
        match get(&name) {
            Ok(value) => values.read.push((name, value)),
            Err(e) if no_such_attr(&e) || unsupported(&e) => {}
            Err(e) => values.unread.push((name, e)),
        }
    }
    values
}

/// What `copy_fd` could not copy.
#[derive(Debug)]
pub enum CopyFailure {
    /// The source's attributes could not be listed: none was copied.
    List(io::Error),
    /// The attribute named could not be read from the source.
    Get(CString, io::Error),
    /// The attribute named could not be written to the destination.
    Set(CString, io::Error),
}

/// Copy the extended attributes of the file open on `src` to the file open on `dst`, each that
/// `is_copied`, as GNU cp -a and mv do (libattr's `attr_copy_fd`): each is created or replaced,
/// and the destination's others are left as they are. `owner_kept` is whether `dst` was given
/// the source's owner: where it was not, a file capability is not copied either
/// (`is_copied_to`). Returned: each one that could not be, but not where the destination
/// holds none at all, or none of that name (`unsupported`) -- lost, as GNU loses it, without
/// a word -- nor one gone from the source since it was listed.
///
/// Either descriptor may be an `O_PATH` one (Linux), reached through `/proc/self/fd/N`. A
/// macOS resource fork, which may be far larger than any other attribute, is copied in
/// bounded pieces (`copy_chunked`).
pub fn copy_fd(src: RawFd, dst: RawFd, owner_kept: bool) -> Vec<CopyFailure> {
    let names = match list_fd(src) {
        Err(e) if unsupported(&e) => return Vec::new(),
        Err(e) => return vec![CopyFailure::List(e)],
        Ok(names) => names,
    };
    let mut failed = Vec::new();
    for name in names
        .into_iter()
        .filter(|n| is_copied_to(n.to_bytes(), owner_kept))
    {
        #[cfg(target_os = "macos")]
        if name.to_bytes() == sys::RESOURCE_FORK {
            if let Err(failure) = sys::copy_fork(src, dst, &name) {
                failed.extend(failure);
            }
            continue;
        }
        let value = match get_fd(src, &name) {
            Ok(value) => value,
            Err(e) if no_such_attr(&e) || unsupported(&e) => continue,
            Err(e) => {
                failed.push(CopyFailure::Get(name, e));
                continue;
            }
        };
        match set_fd(dst, &name, &value) {
            Err(e) if !unsupported(&e) => failed.push(CopyFailure::Set(name, e)),
            _ => {}
        }
    }
    failed
}

/// A descriptor `open_entry` opened for the file a tree walk recorded.
pub struct EntryFd {
    fd: OwnedFd,
    special: bool,
}

impl EntryFd {
    /// Whether the file is neither a directory nor a regular file: a symbolic link, a FIFO, a
    /// device or a socket, which Linux pins `O_PATH`.
    pub fn is_special(&self) -> bool {
        self.special
    }
}

impl AsRawFd for EntryFd {
    fn as_raw_fd(&self) -> RawFd {
        self.fd.as_raw_fd()
    }
}

/// A descriptor of the file a tree walk recorded at `entry`, to read its ACLs and extended
/// attributes through, required to be the very file recorded -- its `(st_dev, st_ino)` and
/// type; `None` when it is not.
///
/// A directory is opened for reading, and its attributes read with plain `f*xattr` calls,
/// which need no procfs. One that cannot be opened so (EACCES) is, on Linux, pinned `O_PATH`
/// instead, as anything else always is there -- a symbolic link itself, unless the walk
/// followed it: that needs no permission on the file and opens no device or FIFO, and its
/// attributes are read through its `self/fd/N` under a verified procfs (`on_fd`). Elsewhere
/// only a directory is opened: anything else cannot be opened without acting on it, and that
/// fails.
pub fn open_entry(entry: &ftw::Entry) -> io::Result<Option<EntryFd>> {
    use std::os::unix::fs::MetadataExt;
    let Some(recorded) = entry.metadata() else {
        return Ok(None);
    };
    // The walk recorded a followed link's referent; open the same thing.
    let follow = entry.is_symlink() == Some(true) && !recorded.is_symlink();
    let nofollow = if follow { 0 } else { libc::O_NOFOLLOW };
    let open = |flags: libc::c_int| {
        let fd = unsafe {
            libc::openat(
                entry.dir_fd(),
                entry.file_name().as_ptr(),
                flags | nofollow | libc::O_CLOEXEC,
            )
        };
        if fd < 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(unsafe { OwnedFd::from_raw_fd(fd) })
    };
    let dir_flags = libc::O_RDONLY | libc::O_DIRECTORY;
    #[cfg(target_os = "linux")]
    let fd = {
        let pin = || open(libc::O_PATH);
        if !recorded.is_dir() {
            pin()
        } else {
            match open(dir_flags) {
                Err(e) if e.raw_os_error() == Some(libc::EACCES) => pin(),
                opened => opened,
            }
        }
    }?;
    #[cfg(not(target_os = "linux"))]
    let fd = open(dir_flags)?;
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::fstat(fd.as_raw_fd(), &mut st) } != 0 {
        return Err(io::Error::last_os_error());
    }
    // Casts needed: `dev_t` is i32 on macOS, `mode_t` u16 there.
    #[allow(clippy::unnecessary_cast)]
    let same = (st.st_dev as u64, st.st_ino as u64) == (recorded.dev(), recorded.ino())
        && (st.st_mode & libc::S_IFMT) as u32 == recorded.mode() & libc::S_IFMT as u32;
    if !same {
        return Ok(None);
    }
    let special = !recorded.is_dir() && !recorded.is_file();
    Ok(Some(EntryFd { fd, special }))
}

/// `copy_fd` from the file `open_entry` opened. A special file pinned `O_PATH` is read only
/// through procfs: where there is none to read it through (`no_procfs_route`), it is taken to
/// have no attribute, as on a filesystem that holds none.
pub fn copy_from_entry(src: &EntryFd, dst: RawFd, owner_kept: bool) -> Vec<CopyFailure> {
    let failed = copy_fd(src.as_raw_fd(), dst, owner_kept);
    match failed.as_slice() {
        [CopyFailure::List(e)] if src.special && no_procfs_route(e) => Vec::new(),
        _ => failed,
    }
}

/// Whether the failure `e` to reach the attributes of a file pinned `O_PATH` is that there is
/// no procfs to reach them through (`on_fd`): no `/proc/self/fd/N` (ENOENT), or no `/proc`
/// verified to be procfs.
#[cfg(target_os = "linux")]
pub(crate) fn no_procfs_route(e: &io::Error) -> bool {
    e.raw_os_error() == Some(libc::ENOENT) || crate::madefs::procfs_dir().is_err()
}

#[cfg(not(target_os = "linux"))]
pub(crate) fn no_procfs_route(_e: &io::Error) -> bool {
    false
}

/// The most of a resource fork held in memory at once (`copy_chunked`).
#[cfg(any(target_os = "macos", test))]
const CHUNK: usize = 1 << 20;

/// Copy a value `size` bytes long, as its length was first read, in pieces of at most
/// `CHUNK` bytes: `read(position, buf)` reads what is at `position` into `buf`, returning how
/// much it read, and `write(position, bytes)` writes `bytes` there. A value that shrank since
/// its size was read ends where a read returns nothing; one that grew is copied up to `size`.
/// A value of no bytes is written once, empty, so the destination has it too.
#[cfg(any(target_os = "macos", test))]
fn copy_chunked<E>(
    size: usize,
    mut read: impl FnMut(usize, &mut [u8]) -> Result<usize, E>,
    mut write: impl FnMut(usize, &[u8]) -> Result<(), E>,
) -> Result<(), E> {
    if size == 0 {
        return write(0, &[]);
    }
    let mut buf = vec![0u8; size.min(CHUNK)];
    let mut position = 0;
    while position < size {
        let want = (size - position).min(buf.len());
        let n = read(position, &mut buf[..want])?.min(want);
        if n == 0 {
            break;
        }
        write(position, &buf[..n])?;
        position += n;
    }
    Ok(())
}

/// Call `call` with a buffer until it fits what is read, asking the size again when the
/// attribute grew between the calls (ERANGE). The first buffer is on the stack: a file with
/// no attributes, the usual case, then costs no allocation.
#[cfg(any(target_os = "linux", target_os = "macos"))]
fn sized(call: impl Fn(*mut u8, usize) -> isize) -> io::Result<Vec<u8>> {
    let mut small = [0u8; 256];
    if let Ok(len) = usize::try_from(call(small.as_mut_ptr(), small.len())) {
        return Ok(small[..len].to_vec());
    }
    let e = io::Error::last_os_error();
    if e.raw_os_error() != Some(libc::ERANGE) {
        return Err(e);
    }
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

/// Fail with the last error unless the call that returned `ret` succeeded.
#[cfg(any(target_os = "linux", target_os = "macos"))]
fn check(ret: libc::c_int) -> io::Result<()> {
    if ret != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

#[cfg(target_os = "linux")]
pub(crate) use linux::{get, list, on_fd, remove, set, Target};

#[cfg(target_os = "linux")]
mod linux {
    use super::{check, sized};
    use std::ffi::{CStr, CString};
    use std::io;
    use std::os::unix::io::RawFd;

    /// What to reach the attributes of: a descriptor, or a path followed or not.
    pub enum Target<'a> {
        Fd(RawFd),
        Path(&'a CStr, bool),
    }

    /// Run `call` on `fd`, or, where `fd` is an `O_PATH` descriptor that refuses it (EBADF),
    /// on its `/proc/self/fd/N`, once `/proc` is verified to be procfs
    /// (`madefs::procfs_dir`): that names the same inode -- a symbolic link pinned itself --
    /// and needs no permission on it to read a `system.` attribute. (The path is resolved
    /// again after the check: only root can mount something else over `/proc`.) The first
    /// attribute call `call` makes is the one refused, so nothing is done twice.
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
    pub fn list(target: &Target) -> io::Result<Vec<u8>> {
        sized(|buf, len| unsafe {
            match *target {
                Target::Fd(fd) => libc::flistxattr(fd, buf.cast(), len),
                Target::Path(p, true) => libc::listxattr(p.as_ptr(), buf.cast(), len),
                Target::Path(p, false) => libc::llistxattr(p.as_ptr(), buf.cast(), len),
            }
        })
    }

    /// Set the attribute `name` of `target` to `value`.
    pub fn set(target: &Target, name: &CStr, value: &[u8]) -> io::Result<()> {
        let (name, ptr, len) = (name.as_ptr(), value.as_ptr().cast(), value.len());
        check(unsafe {
            match *target {
                Target::Fd(fd) => libc::fsetxattr(fd, name, ptr, len, 0),
                Target::Path(p, true) => libc::setxattr(p.as_ptr(), name, ptr, len, 0),
                Target::Path(p, false) => libc::lsetxattr(p.as_ptr(), name, ptr, len, 0),
            }
        })
    }

    /// Remove the attribute `name` of `target`.
    pub fn remove(target: &Target, name: &CStr) -> io::Result<()> {
        check(unsafe {
            match *target {
                Target::Fd(fd) => libc::fremovexattr(fd, name.as_ptr()),
                Target::Path(p, true) => libc::removexattr(p.as_ptr(), name.as_ptr()),
                Target::Path(p, false) => libc::lremovexattr(p.as_ptr(), name.as_ptr()),
            }
        })
    }

    pub fn list_fd(fd: RawFd) -> io::Result<Vec<u8>> {
        on_fd(fd, list)
    }

    pub fn get_fd(fd: RawFd, name: &CStr) -> io::Result<Vec<u8>> {
        on_fd(fd, |target| get(target, name))
    }

    pub fn set_fd(fd: RawFd, name: &CStr, value: &[u8]) -> io::Result<()> {
        on_fd(fd, |target| set(target, name, value))
    }

    pub fn remove_fd(fd: RawFd, name: &CStr) -> io::Result<()> {
        on_fd(fd, |target| remove(target, name))
    }
}

#[cfg(target_os = "macos")]
mod sys {
    use super::{check, copy_chunked, no_such_attr, sized, unsupported, CopyFailure};
    use std::ffi::CStr;
    use std::io;
    use std::os::unix::io::RawFd;

    // A descriptor follows no link, so no XATTR_NOFOLLOW; position 0 is the whole value, a
    // resource fork's included.
    pub fn list_fd(fd: RawFd) -> io::Result<Vec<u8>> {
        sized(|buf, len| unsafe { libc::flistxattr(fd, buf.cast(), len, 0) })
    }

    pub fn get_fd(fd: RawFd, name: &CStr) -> io::Result<Vec<u8>> {
        sized(|buf, len| unsafe { libc::fgetxattr(fd, name.as_ptr(), buf.cast(), len, 0, 0) })
    }

    pub fn set_fd(fd: RawFd, name: &CStr, value: &[u8]) -> io::Result<()> {
        let (ptr, len) = (value.as_ptr().cast(), value.len());
        check(unsafe { libc::fsetxattr(fd, name.as_ptr(), ptr, len, 0, 0) })
    }

    pub fn remove_fd(fd: RawFd, name: &CStr) -> io::Result<()> {
        check(unsafe { libc::fremovexattr(fd, name.as_ptr(), 0) })
    }

    /// The resource fork's name: the one attribute read and written at a position.
    pub const RESOURCE_FORK: &[u8] = b"com.apple.ResourceFork";

    /// Copy the resource fork `name` of `src` to `dst` in pieces (`copy_chunked`), in place of
    /// the one `dst` has: a write at a position does not shorten what is there. `None` where
    /// it is no failure (`copy_fd`): gone from the source, or not held by the destination.
    pub fn copy_fork(src: RawFd, dst: RawFd, name: &CStr) -> Result<(), Option<CopyFailure>> {
        let get = |e: io::Error| {
            (!no_such_attr(&e) && !unsupported(&e)).then(|| CopyFailure::Get(name.into(), e))
        };
        let set = |e: io::Error| (!unsupported(&e)).then(|| CopyFailure::Set(name.into(), e));
        let size = unsafe { libc::fgetxattr(src, name.as_ptr(), std::ptr::null_mut(), 0, 0, 0) };
        let size = usize::try_from(size).map_err(|_| get(io::Error::last_os_error()))?;
        // The position is a u32: no larger fork can be read or written at all.
        if u32::try_from(size).is_err() {
            return Err(get(io::Error::from_raw_os_error(libc::EFBIG)));
        }
        match remove_fd(dst, name) {
            Err(e) if !no_such_attr(&e) => return Err(set(e)),
            _ => {}
        }
        // Casts needed: the position is a u32, and `size` fits one.
        let read = |position: usize, buf: &mut [u8]| {
            let n = unsafe {
                libc::fgetxattr(
                    src,
                    name.as_ptr(),
                    buf.as_mut_ptr().cast(),
                    buf.len(),
                    position as u32,
                    0,
                )
            };
            usize::try_from(n).map_err(|_| get(io::Error::last_os_error()))
        };
        let write = |position: usize, bytes: &[u8]| {
            let ret = unsafe {
                libc::fsetxattr(
                    dst,
                    name.as_ptr(),
                    bytes.as_ptr().cast(),
                    bytes.len(),
                    position as u32,
                    0,
                )
            };
            check(ret).map_err(set)
        };
        copy_chunked(size, read, write)
    }
}

#[cfg(target_os = "linux")]
use linux as sys;

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
mod sys {
    use std::ffi::CStr;
    use std::io;
    use std::os::unix::io::RawFd;

    fn none<T>() -> io::Result<T> {
        Err(io::Error::from_raw_os_error(libc::EOPNOTSUPP))
    }

    pub fn list_fd(_fd: RawFd) -> io::Result<Vec<u8>> {
        none()
    }

    pub fn get_fd(_fd: RawFd, _name: &CStr) -> io::Result<Vec<u8>> {
        none()
    }

    pub fn set_fd(_fd: RawFd, _name: &CStr, _value: &[u8]) -> io::Result<()> {
        none()
    }

    pub fn remove_fd(_fd: RawFd, _name: &CStr) -> io::Result<()> {
        none()
    }
}

#[cfg(test)]
mod tests {
    use super::is_copied;

    #[test]
    fn the_acls_and_what_libattr_skips_are_not_copied() {
        for name in [
            &b"system.posix_acl_access"[..],
            b"system.posix_acl_default",
            b"system.nfs4_acl",
            b"system.cifs_acl",
            b"security.evm",
            b"trusted.SGI_ACL_FILE",
            b"trusted.SGI_DMI_x",
            b"xfsroot.x",
            b"user.Beagle.x",
            b"afs.x",
        ] {
            assert!(!is_copied(name), "{}", String::from_utf8_lossy(name));
        }
        for name in [
            &b"user.foo"[..],
            b"security.selinux",
            b"security.capability",
            b"security.ima",
            b"trusted.foo",
            b"trusted.SGI_other",
            b"user.Beagle",
            b"com.apple.quarantine",
        ] {
            assert!(is_copied(name), "{}", String::from_utf8_lossy(name));
        }
    }

    /// A file capability goes only to a copy given the source's owner.
    #[test]
    fn a_capability_is_copied_only_with_the_owner() {
        assert!(super::is_copied_to(b"security.capability", true));
        assert!(!super::is_copied_to(b"security.capability", false));
        assert!(super::is_copied_to(b"user.foo", false));
        assert!(!super::is_copied_to(b"system.posix_acl_access", true));
    }

    /// Only a `user.` attribute is restored from an archive.
    #[test]
    fn only_user_attributes_are_restored_from_an_archive() {
        use super::is_restored_from_archive;
        for name in [&b"user.foo"[..], b"user.", b"user.\xff"] {
            assert!(is_restored_from_archive(name));
        }
        for name in [
            &b"security.capability"[..],
            b"security.selinux",
            b"trusted.foo",
            b"system.posix_acl_access",
            b"com.apple.quarantine",
            b"user",
            b"User.foo",
            b"user.Beagle.x",
            b"",
        ] {
            assert!(!is_restored_from_archive(name));
        }
    }

    /// A value is copied whole, a piece at a time, none larger than `CHUNK`; one that shrank
    /// meanwhile ends where the read finds nothing; an empty one is still written.
    #[test]
    fn copy_chunked_copies_in_bounded_pieces() {
        use super::{copy_chunked, CHUNK};
        let copy = |value: &[u8], size: usize| {
            let mut out = Vec::new();
            let mut writes = 0;
            let read = |position: usize, buf: &mut [u8]| -> Result<usize, ()> {
                assert!(buf.len() <= CHUNK);
                let from = value.get(position..).unwrap_or(&[]);
                let n = from.len().min(buf.len());
                buf[..n].copy_from_slice(&from[..n]);
                Ok(n)
            };
            let write = |position: usize, bytes: &[u8]| -> Result<(), ()> {
                assert_eq!(position, out.len());
                out.extend_from_slice(bytes);
                writes += 1;
                Ok(())
            };
            copy_chunked(size, read, write).unwrap();
            (out, writes)
        };
        let big: Vec<u8> = (0..CHUNK * 5 / 2).map(|i| (i % 251) as u8).collect();
        assert_eq!(copy(&big, big.len()), (big.clone(), 3));
        assert_eq!(copy(&big[..10], big.len()), (big[..10].to_vec(), 1));
        assert_eq!(copy(&big, 7), (big[..7].to_vec(), 1));
        assert_eq!(copy(&[], 0), (Vec::new(), 1));
        // A failure stops the copy and is returned as it is.
        let failed = copy_chunked(big.len(), |_, _| Err::<usize, i32>(5), |_, _| Ok(()));
        assert_eq!(failed, Err(5));
    }

    /// What does not fit the stack buffer is read whole into one that does, and what
    /// does fit comes back as it is.
    #[cfg(target_os = "linux")]
    #[test]
    fn sized_reads_past_the_stack_buffer() {
        for size in [0usize, 9, 256, 257, 5000, 65536] {
            let value: Vec<u8> = (0..size).map(|i| i as u8).collect();
            let read = super::sized(|buf, len| {
                if buf.is_null() {
                    return size as isize;
                }
                if len < size {
                    unsafe { *libc::__errno_location() = libc::ERANGE };
                    return -1;
                }
                unsafe { std::ptr::copy_nonoverlapping(value.as_ptr(), buf, size) };
                size as isize
            });
            assert_eq!(read.unwrap(), value, "{size} bytes");
        }
    }
}
