//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The directory-relative system calls behind `safe_fs`, for Unix.

use std::ffi::{CString, OsStr};
use std::fs::{File, Metadata, OpenOptions};
use std::io;
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd, RawFd};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
use std::path::Path;
use std::sync::atomic::{AtomicU32, Ordering};

/// An open directory, the base of the names below it.
pub struct Dir(File);

impl Dir {
    fn fd(&self) -> RawFd {
        self.0.as_raw_fd()
    }
}

/// What lstat says stands at a name.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kind {
    Regular,
    Symlink,
    Other,
}

/// Wrap a directory descriptor.
fn dir(fd: OwnedFd) -> Dir {
    Dir(File::from(fd))
}

fn c_name(name: &OsStr) -> io::Result<CString> {
    CString::new(name.as_bytes()).map_err(|_| io::Error::from(io::ErrorKind::InvalidInput))
}

/// openat(2), close-on-exec; `mode` is the variadic creation mode.
fn open_at(
    dir: RawFd,
    name: &OsStr,
    flags: libc::c_int,
    mode: libc::c_uint,
) -> io::Result<OwnedFd> {
    let name = c_name(name)?;
    // SAFETY: `name` is NUL-terminated; the descriptor returned is new and
    // owned by nobody else.
    let fd = unsafe { libc::openat(dir, name.as_ptr(), flags | libc::O_CLOEXEC, mode) };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    // SAFETY: `fd` was just opened and is valid.
    Ok(unsafe { OwnedFd::from_raw_fd(fd) })
}

/// Check the return of a call that reports failure as -1.
fn check(rc: libc::c_int) -> io::Result<()> {
    if rc < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}

const DIR_FLAGS: libc::c_int = libc::O_RDONLY | libc::O_DIRECTORY;

/// The working directory.
pub fn open_cwd() -> io::Result<Dir> {
    open_at(libc::AT_FDCWD, OsStr::new("."), DIR_FLAGS, 0).map(dir)
}

/// A directory named by the user, reached the ordinary way.
pub fn open_dir(path: &Path) -> io::Result<Dir> {
    open_at(libc::AT_FDCWD, path.as_os_str(), DIR_FLAGS, 0).map(dir)
}

/// The directory `name` in `parent`, which must not be a link.
pub fn open_subdir(parent: &Dir, name: &OsStr) -> io::Result<Dir> {
    open_at(parent.fd(), name, DIR_FLAGS | libc::O_NOFOLLOW, 0).map(dir)
}

/// Make the directory `name` in `dir` and open it. What is opened must be a
/// directory this process owns: the one just made, not one swapped in for it.
/// If another process made it first, it is opened like any existing one.
pub fn make_subdir(parent: &Dir, name: &OsStr) -> io::Result<Dir> {
    let c = c_name(name)?;
    // SAFETY: `c` is NUL-terminated and the descriptor is open.
    let made = check(unsafe { libc::mkdirat(parent.fd(), c.as_ptr(), 0o777) });
    match made {
        Ok(()) => {}
        Err(e) if e.kind() == io::ErrorKind::AlreadyExists => return open_subdir(parent, name),
        Err(e) => return Err(e),
    }
    let sub = open_subdir(parent, name)?;
    let meta = sub.0.metadata()?;
    // SAFETY: geteuid cannot fail.
    let euid = unsafe { libc::geteuid() };
    if !meta.is_dir() || meta.uid() != euid {
        return Err(io::Error::other("directory replaced while being made"));
    }
    Ok(sub)
}

/// lstat of `name` in `dir`; None if nothing is there.
fn lstat_raw(dir: &Dir, name: &OsStr) -> io::Result<Option<libc::stat>> {
    let c = c_name(name)?;
    // SAFETY: an all-zero `stat` is a valid value to be overwritten.
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    // SAFETY: `c` is NUL-terminated, `st` is writable, the descriptor is open.
    let rc = unsafe { libc::fstatat(dir.fd(), c.as_ptr(), &mut st, libc::AT_SYMLINK_NOFOLLOW) };
    match check(rc) {
        Ok(()) => Ok(Some(st)),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(e),
    }
}

/// What stands at `name` in `dir`, without following a link.
pub fn lstat(dir: &Dir, name: &OsStr) -> io::Result<Option<Kind>> {
    Ok(
        lstat_raw(dir, name)?.map(|st| match st.st_mode & libc::S_IFMT {
            libc::S_IFREG => Kind::Regular,
            libc::S_IFLNK => Kind::Symlink,
            _ => Kind::Other,
        }),
    )
}

/// The device and inode numbers of `st`, widened as std's Metadata widens
/// them.
#[allow(clippy::unnecessary_cast)] // dev_t is i32 on macOS, u64 on Linux
fn file_id(st: &libc::stat) -> (u64, u64) {
    (st.st_dev as u64, st.st_ino as u64)
}

/// Whether `name` in `dir` is still the file `meta` describes.
pub fn is_same_file(dir: &Dir, name: &OsStr, meta: &Metadata) -> io::Result<bool> {
    Ok(lstat_raw(dir, name)?.is_some_and(|st| file_id(&st) == (meta.dev(), meta.ino())))
}

/// Open `name` in `dir` for reading: not through a link, and without waiting
/// on a FIFO or taking a terminal as the controlling one.
pub fn open_read(dir: &Dir, name: &OsStr) -> io::Result<File> {
    let flags = libc::O_RDONLY | libc::O_NOFOLLOW | libc::O_NONBLOCK | libc::O_NOCTTY;
    open_at(dir.fd(), name, flags, 0).map(File::from)
}

/// Open `name` in `dir` for appending: not through a link, and failing
/// rather than waiting on a FIFO with no reader.
pub fn open_append(dir: &Dir, name: &OsStr) -> io::Result<File> {
    let flags =
        libc::O_WRONLY | libc::O_APPEND | libc::O_NOFOLLOW | libc::O_NONBLOCK | libc::O_NOCTTY;
    open_at(dir.fd(), name, flags, 0).map(File::from)
}

/// Create a new file under a fresh temporary name in `dir`.
/// A file that will be given another file's mode is made private (0600)
/// until then, so a file only its owner may read is never readable by others
/// on the way; otherwise it gets the usual 0666 less the umask.
pub fn create_temp(dir: &Dir, private: bool) -> io::Result<(File, std::ffi::OsString)> {
    static COUNTER: AtomicU32 = AtomicU32::new(0);
    let flags = libc::O_WRONLY | libc::O_CREAT | libc::O_EXCL | libc::O_NOFOLLOW;
    let mode = if private { 0o600 } else { 0o666 };
    loop {
        let n = COUNTER.fetch_add(1, Ordering::Relaxed);
        let name = std::ffi::OsString::from(format!(".patch.{}.{}", std::process::id(), n));
        match open_at(dir.fd(), &name, flags, mode) {
            Ok(fd) => return Ok((File::from(fd), name)),
            Err(e) if e.kind() == io::ErrorKind::AlreadyExists && n < u32::MAX => continue,
            Err(e) => return Err(e),
        }
    }
}

/// Give `file` the owner (when running as root) or else the group (when the
/// caller belongs to it) of `meta`, and then its mode; the owner first,
/// because changing it clears the set-ID bits. A set-user-ID or
/// set-group-ID bit is kept only where the new file really has the
/// original's owner or group, so patching another user's set-ID file never
/// yields a set-ID file of the caller's.
pub fn set_owner_and_mode(file: &File, meta: &Metadata) -> io::Result<()> {
    let fd = file.as_raw_fd();
    // SAFETY: geteuid cannot fail.
    if unsafe { libc::geteuid() } == 0 {
        // SAFETY: the descriptor is open.
        check(unsafe { libc::fchown(fd, meta.uid(), meta.gid()) })?;
    } else {
        // Best effort, as in GNU patch: it succeeds only for a group the
        // caller belongs to. An owner of -1 leaves the owner as it is.
        // SAFETY: the descriptor is open.
        let _ = unsafe { libc::fchown(fd, libc::uid_t::MAX, meta.gid()) };
    }
    let now = file.metadata()?;
    let mut bits = meta.mode() & 0o7777;
    if now.uid() != meta.uid() {
        bits &= !0o4000;
    }
    if now.gid() != meta.gid() {
        bits &= !0o2000;
    }
    // mode_t is u16 on macOS, u32 on Linux; the permission bits fit both.
    #[allow(clippy::unnecessary_cast)]
    let mode = bits as libc::mode_t;
    // SAFETY: the descriptor is open.
    check(unsafe { libc::fchmod(fd, mode) })
}

/// Rename `from` to `to`, both in `dir`.
pub fn rename(dir: &Dir, from: &OsStr, to: &OsStr) -> io::Result<()> {
    let (from, to) = (c_name(from)?, c_name(to)?);
    let fd = dir.fd();
    // SAFETY: both names are NUL-terminated and the descriptor is open.
    check(unsafe { libc::renameat(fd, from.as_ptr(), fd, to.as_ptr()) })
}

fn unlink_at(dir: &Dir, name: &OsStr, flags: libc::c_int) -> io::Result<()> {
    let c = c_name(name)?;
    // SAFETY: `c` is NUL-terminated and the descriptor is open.
    check(unsafe { libc::unlinkat(dir.fd(), c.as_ptr(), flags) })
}

/// Remove the file `name` from `dir`.
pub fn unlink(dir: &Dir, name: &OsStr) -> io::Result<()> {
    unlink_at(dir, name, 0)
}

/// Remove the empty directory `name` from `dir`.
pub fn rmdir(dir: &Dir, name: &OsStr) -> io::Result<()> {
    unlink_at(dir, name, libc::AT_REMOVEDIR)
}

/// Open a file the user named for output (-o, -r), as `tee` would: links are
/// followed, but a FIFO with no reader is an error rather than a hang, and a
/// terminal does not become the controlling one. A regular file is truncated
/// unless `append` is set; nothing else is.
pub fn open_user_output(path: &Path, append: bool) -> io::Result<File> {
    let file = OpenOptions::new()
        .write(true)
        .create(true)
        .append(append)
        .mode(0o666)
        .custom_flags(libc::O_NOCTTY | libc::O_NONBLOCK)
        .open(path)?;
    clear_nonblock(&file)?;
    if !append && file.metadata()?.is_file() {
        file.set_len(0)?;
    }
    Ok(file)
}

/// Make writes to `file` block again.
fn clear_nonblock(file: &File) -> io::Result<()> {
    let fd = file.as_raw_fd();
    // SAFETY: the descriptor is open.
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFL) };
    check(flags)?;
    // SAFETY: the descriptor is open.
    check(unsafe { libc::fcntl(fd, libc::F_SETFL, flags & !libc::O_NONBLOCK) })
}
