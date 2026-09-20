//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Resolving a member pathname without ever following a symbolic link.
//!
//! Both read mode and copy mode write a tree of member names below a directory
//! the user named, and both have to assume the names come from somewhere
//! untrusted -- an archive in one case, a source tree someone else can modify in
//! the other. Resolving such a name through the ordinary filesystem namespace
//! means every component is re-resolved on every call and any of them can be a
//! symbolic link pointing anywhere.
//!
//! So a member is walked one component at a time from a descriptor for the
//! directory it is anchored to, each `openat` refusing links, and the leaf is
//! always created fresh rather than written through.

use crate::error::{PaxError, PaxResult};
use std::ffi::{CStr, CString, OsString};
use std::fs::File;
use std::os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd, OwnedFd};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};

/// A member pathname reduced to the directory components that must be walked
/// and the final component to create.
pub(crate) struct MemberPath {
    pub(crate) dirs: Vec<CString>,
    pub(crate) leaf: CString,
    /// The same path as text, for diagnostics and hard-link bookkeeping.
    pub(crate) display: PathBuf,
}

impl MemberPath {
    /// `Ok(None)` for a member that names nothing to create -- `.`, or a name
    /// made up entirely of `.`, `..` and root components.
    ///
    /// `..` still pops and a leading `/` is still dropped, but this lexical
    /// pass is no longer the security boundary it used to be: it cannot see
    /// that `a/b` escapes when `a` is a symlink. Resolution opens each
    /// component with O_NOFOLLOW instead, which does not care how the name is
    /// spelled. What remains here is naming policy, plus the one check that
    /// must happen before any syscall: an embedded NUL.
    pub(crate) fn parse(path: &Path) -> PaxResult<Option<Self>> {
        use std::path::Component;

        let mut parts: Vec<OsString> = Vec::new();
        for comp in path.components() {
            match comp {
                Component::Normal(c) => parts.push(c.to_os_string()),
                Component::ParentDir => {
                    parts.pop();
                }
                Component::CurDir | Component::RootDir | Component::Prefix(_) => {}
            }
        }

        let Some(leaf_os) = parts.pop() else {
            return Ok(None);
        };

        let to_c = |s: &OsString| {
            CString::new(s.as_bytes())
                .map_err(|_| PaxError::InvalidHeader("path contains null".to_string()))
        };

        let dirs = parts.iter().map(to_c).collect::<PaxResult<Vec<_>>>()?;
        let mut display = PathBuf::new();
        for p in &parts {
            display.push(p);
        }
        display.push(&leaf_os);

        Ok(Some(MemberPath {
            dirs,
            leaf: to_c(&leaf_os)?,
            display,
        }))
    }

    /// Whether this name refers to the extraction directory itself rather
    /// than naming nothing at all.
    ///
    /// `pax -w .` records a `.` member, so every archive built that way
    /// carries one and there is nothing wrong with it -- it just has no file
    /// to create below the anchor. An empty name, or one made only of `..`
    /// and root components, is a different thing and worth saying out loud.
    pub(crate) fn names_current_directory(path: &Path) -> bool {
        use std::path::Component;
        let mut saw_something = false;
        for comp in path.components() {
            match comp {
                Component::CurDir => saw_something = true,
                _ => return false,
            }
        }
        saw_something
    }

    /// How deep the member sits, for ordering the deferred directory pass.
    pub(crate) fn depth(&self) -> usize {
        self.dirs.len()
    }
}

/// Extraction anchored at an open descriptor for the working directory.
///
/// Member paths are walked one component at a time with
/// `O_RDONLY|O_DIRECTORY|O_NOFOLLOW`, so a symlink planted anywhere along the
/// path fails the descent rather than redirecting the write outside the
/// extraction directory. The previous code resolved whole paths through the
/// ordinary filesystem namespace, where `create_dir_all` on `sub/file` was
/// happy to follow `sub -> /elsewhere`.
pub(crate) struct DirTree {
    root: OwnedFd,
}

impl DirTree {
    pub(crate) fn open_cwd() -> PaxResult<Self> {
        let dot = CString::new(".").expect("no NUL in \".\"");
        let fd = unsafe {
            libc::openat(
                libc::AT_FDCWD,
                dot.as_ptr(),
                libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC,
            )
        };
        if fd < 0 {
            return Err(std::io::Error::last_os_error().into());
        }
        Ok(DirTree {
            root: unsafe { OwnedFd::from_raw_fd(fd) },
        })
    }

    /// Anchor at a directory named by the caller, for copy mode's destination.
    pub(crate) fn open_path(path: &Path) -> PaxResult<Self> {
        let c = CString::new(path.as_os_str().as_bytes())
            .map_err(|_| PaxError::InvalidHeader("path contains null".to_string()))?;
        let fd = unsafe {
            libc::openat(
                libc::AT_FDCWD,
                c.as_ptr(),
                libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC,
            )
        };
        if fd < 0 {
            return Err(std::io::Error::last_os_error().into());
        }
        Ok(DirTree {
            root: unsafe { OwnedFd::from_raw_fd(fd) },
        })
    }

    /// The descriptor for the anchor itself.
    pub(crate) fn root(&self) -> BorrowedFd<'_> {
        self.root.as_fd()
    }

    /// Open the directory that will hold `member`, creating any missing
    /// intermediate components.
    pub(crate) fn parent_of(
        &self,
        member: &MemberPath,
        create_missing: bool,
    ) -> PaxResult<OwnedFd> {
        let mut cur = self.root.try_clone()?;
        for comp in &member.dirs {
            cur = open_dir_at(cur.as_fd(), comp, create_missing)?;
        }
        Ok(cur)
    }
}

/// Open one directory component below `dirfd` without following a symlink.
pub(crate) fn open_dir_at(
    dirfd: BorrowedFd<'_>,
    name: &CString,
    create_missing: bool,
) -> PaxResult<OwnedFd> {
    let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC;

    let fd = unsafe { libc::openat(dirfd.as_raw_fd(), name.as_ptr(), flags) };
    if fd >= 0 {
        return Ok(unsafe { OwnedFd::from_raw_fd(fd) });
    }

    let err = std::io::Error::last_os_error();
    if err.raw_os_error() != Some(libc::ENOENT) || !create_missing {
        return Err(err.into());
    }

    // Intermediate directories are created with the normal file-creation
    // action, per POSIX read/copy mode: mode 0777 modified by the umask.
    let r = unsafe { libc::mkdirat(dirfd.as_raw_fd(), name.as_ptr(), 0o777) };
    if r != 0 {
        let e = std::io::Error::last_os_error();
        if e.raw_os_error() != Some(libc::EEXIST) {
            return Err(e.into());
        }
    }

    let fd = unsafe { libc::openat(dirfd.as_raw_fd(), name.as_ptr(), flags) };
    if fd < 0 {
        return Err(std::io::Error::last_os_error().into());
    }
    Ok(unsafe { OwnedFd::from_raw_fd(fd) })
}

/// Remove whatever currently occupies `name`, so an exclusive create can win.
pub(crate) fn unlink_at(dirfd: BorrowedFd<'_>, name: &CStr) -> PaxResult<()> {
    let r = unsafe { libc::unlinkat(dirfd.as_raw_fd(), name.as_ptr(), 0) };
    if r != 0 {
        let err = std::io::Error::last_os_error();
        match err.raw_os_error() {
            Some(libc::ENOENT) => return Ok(()),
            // A directory in the way needs the directory flag instead.
            Some(libc::EISDIR) | Some(libc::EPERM) => {
                let r =
                    unsafe { libc::unlinkat(dirfd.as_raw_fd(), name.as_ptr(), libc::AT_REMOVEDIR) };
                if r == 0 {
                    return Ok(());
                }
                return Err(std::io::Error::last_os_error().into());
            }
            _ => return Err(err.into()),
        }
    }
    Ok(())
}

/// Create a member, retrying once after clearing whatever is in the way.
///
/// `create` reports `EEXIST` by returning `Err`; that is the whole point. With
/// -k an existing name means skip, atomically and with no window. Otherwise the
/// old entry is unlinked and the create retried, so the member is always a
/// freshly created object -- never a write *through* a symlink or a hard link
/// an attacker left behind, which `O_TRUNC` on an existing name would allow.
pub(crate) fn create_replacing<F>(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    no_clobber: bool,
    mut create: F,
) -> PaxResult<bool>
where
    F: FnMut() -> std::io::Result<()>,
{
    match create() {
        Ok(()) => return Ok(true),
        Err(e) if e.raw_os_error() == Some(libc::EEXIST) => {}
        Err(e) => return Err(e.into()),
    }

    if no_clobber {
        return Ok(false);
    }

    unlink_at(dirfd, name)?;
    match create() {
        Ok(()) => Ok(true),
        Err(e) => Err(e.into()),
    }
}

/// Open a source file from the descriptor of the directory it was found in.
///
/// One component, resolved once, rather than a whole pathname re-resolved by
/// the kernel on every call -- which is what let a source tree another process
/// could modify redirect a read outside the tree pax was asked to archive.
///
/// `O_NONBLOCK` is not optional here. `O_NOFOLLOW` refuses a symbolic link, but
/// it does not stop a regular file being replaced by a FIFO between the walk's
/// `fstatat` and this `openat`, and a blocking `open` of a FIFO with no writer
/// never returns. Opening non-blocking and then checking what was actually
/// opened turns that from a hang into a diagnostic.
///
/// The `fstat` on the descriptor doubles as the check that the name still
/// refers to the file the walk saw: a mismatch fails closed.
pub(crate) fn open_source_file(
    dir_fd: libc::c_int,
    name: &CStr,
    follow: bool,
    expected: (u64, u64),
) -> std::io::Result<File> {
    let mut flags = libc::O_RDONLY | libc::O_CLOEXEC | libc::O_NONBLOCK;
    if !follow {
        flags |= libc::O_NOFOLLOW;
    }

    let fd = unsafe { libc::openat(dir_fd, name.as_ptr(), flags) };
    if fd < 0 {
        return Err(std::io::Error::last_os_error());
    }
    let file = unsafe { File::from_raw_fd(fd) };

    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::fstat(file.as_raw_fd(), &mut st) } != 0 {
        return Err(std::io::Error::last_os_error());
    }

    if st.st_mode & libc::S_IFMT != libc::S_IFREG {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "source file changed type before it could be read",
        ));
    }
    if (st.st_dev as u64, st.st_ino) != expected {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "source file was replaced before it could be read",
        ));
    }

    // Nothing below reads this descriptor expecting non-blocking semantics.
    let cur = unsafe { libc::fcntl(file.as_raw_fd(), libc::F_GETFL) };
    if cur >= 0 {
        unsafe { libc::fcntl(file.as_raw_fd(), libc::F_SETFL, cur & !libc::O_NONBLOCK) };
    }

    Ok(file)
}

/// The attributes a copied or extracted file takes from its source, whether that
/// source is an archive member or a file on disk.
pub(crate) struct Attrs {
    pub mode: u32,
    pub uid: u32,
    pub gid: u32,
    pub mtime: i64,
    pub mtime_nsec: i64,
    /// `None` when the source recorded no access time, in which case the
    /// modification time stands in for it.
    pub atime: Option<i64>,
    pub atime_nsec: i64,
}

/// Which of those attributes the user asked to keep (`-p`).
pub(crate) struct AttrPolicy {
    pub preserve_owner: bool,
    pub preserve_perms: bool,
    pub preserve_mtime: bool,
    pub preserve_atime: bool,
    pub umask: u32,
}

impl AttrPolicy {
    /// The permission bits to apply.
    ///
    /// Without `-p o`/`-p e` the set-user-ID and set-group-ID bits are dropped,
    /// since the file is about to belong to whoever ran pax rather than to the
    /// user recorded in the source. Without `-p p`/`-p e` the file is created by
    /// the normal file-creation action, so the mode is modified by the umask
    /// exactly as `open()` or `mkdir()` would do.
    pub fn mode(&self, attrs: &Attrs) -> u32 {
        let mut mode = attrs.mode;
        if !self.preserve_owner {
            #[allow(clippy::unnecessary_cast)] // u16 on macOS, u32 on Linux
            let setid = !((libc::S_ISUID | libc::S_ISGID) as u32);
            mode &= setid;
        }
        if !self.preserve_perms {
            mode &= !self.umask;
        }
        mode
    }

    /// The permission bits to *create* with, as opposed to the ones the
    /// finished file ends up with.
    ///
    /// Never set-user-ID or set-group-ID, whatever `-p` asked for. A member is
    /// created before its contents are written, so passing the archived mode
    /// straight to `open()` leaves a set-id file -- owned by whoever is running
    /// pax -- executable for the whole duration of the copy. Extracting an
    /// untrusted archive as root that way hands out a root shell to anyone who
    /// wins the race to `exec` it.
    ///
    /// `set_attrs_fd` applies [`AttrPolicy::mode`] through the descriptor once
    /// the data is complete, so any set-id bit the archive legitimately carries
    /// arrives then, on a file whose contents are already final.
    pub fn creation_mode(&self, attrs: &Attrs) -> u32 {
        self.mode(attrs) & 0o777
    }

    /// The times to apply, or `None` when neither was asked for.
    ///
    /// `UTIME_OMIT` leaves the one that was not asked for exactly as it is,
    /// which is both simpler and more accurate than reading it back first.
    pub fn times(&self, attrs: &Attrs) -> Option<[libc::timespec; 2]> {
        if !self.preserve_mtime && !self.preserve_atime {
            return None;
        }
        let omit = libc::timespec {
            tv_sec: 0,
            tv_nsec: libc::UTIME_OMIT,
        };
        let atime = if self.preserve_atime {
            match attrs.atime {
                Some(sec) => libc::timespec {
                    tv_sec: sec as libc::time_t,
                    tv_nsec: attrs.atime_nsec as _,
                },
                None => libc::timespec {
                    tv_sec: attrs.mtime as libc::time_t,
                    tv_nsec: attrs.mtime_nsec as _,
                },
            }
        } else {
            omit
        };
        let mtime = if self.preserve_mtime {
            libc::timespec {
                tv_sec: attrs.mtime as libc::time_t,
                tv_nsec: attrs.mtime_nsec as _,
            }
        } else {
            omit
        };
        Some([atime, mtime])
    }
}

/// Apply owner, mode and times to an already-open file.
///
/// Nothing here names a path, so none of it can be redirected by a name that
/// changed after the file was created.
pub(crate) fn set_attrs_fd(
    fd: BorrowedFd<'_>,
    attrs: &Attrs,
    policy: &AttrPolicy,
) -> PaxResult<()> {
    if policy.preserve_owner {
        let r = unsafe { libc::fchown(fd.as_raw_fd(), attrs.uid, attrs.gid) };
        if r != 0 {
            let err = std::io::Error::last_os_error();
            // EPERM usually means we're not root - warn but continue
            if err.raw_os_error() == Some(libc::EPERM) {
                eprintln!("pax: cannot change owner: Operation not permitted");
                crate::error::note_error();
            } else {
                return Err(err.into());
            }
        }
    }

    let r = unsafe { libc::fchmod(fd.as_raw_fd(), policy.mode(attrs) as libc::mode_t) };
    if r != 0 {
        return Err(std::io::Error::last_os_error().into());
    }

    if let Some(times) = policy.times(attrs) {
        let r = unsafe { libc::futimens(fd.as_raw_fd(), times.as_ptr()) };
        if r != 0 {
            eprintln!(
                "pax: warning: cannot set times: {}",
                std::io::Error::last_os_error()
            );
            crate::error::note_error();
        }
    }

    Ok(())
}

/// Apply owner and times to a name that cannot be opened for the purpose -- a
/// symbolic link, whose own mode bits carry no meaning and which must never be
/// followed to reach them.
pub(crate) fn set_link_attrs_at(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    attrs: &Attrs,
    policy: &AttrPolicy,
) -> PaxResult<()> {
    if policy.preserve_owner {
        let r = unsafe {
            libc::fchownat(
                dirfd.as_raw_fd(),
                name.as_ptr(),
                attrs.uid,
                attrs.gid,
                libc::AT_SYMLINK_NOFOLLOW,
            )
        };
        if r != 0 {
            let err = std::io::Error::last_os_error();
            if err.raw_os_error() == Some(libc::EPERM) {
                eprintln!("pax: cannot change owner: Operation not permitted");
                crate::error::note_error();
            } else {
                return Err(err.into());
            }
        }
    }

    if let Some(times) = policy.times(attrs) {
        let r = unsafe {
            libc::utimensat(
                dirfd.as_raw_fd(),
                name.as_ptr(),
                times.as_ptr(),
                libc::AT_SYMLINK_NOFOLLOW,
            )
        };
        if r != 0 {
            eprintln!(
                "pax: warning: cannot set times: {}",
                std::io::Error::last_os_error()
            );
            crate::error::note_error();
        }
    }

    Ok(())
}

/// `fstatat` with `AT_SYMLINK_NOFOLLOW`, for asking what a name *is* without
/// following it anywhere.
pub(crate) fn stat_at(dirfd: BorrowedFd<'_>, name: &CStr) -> Option<libc::stat> {
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    let r = unsafe {
        libc::fstatat(
            dirfd.as_raw_fd(),
            name.as_ptr(),
            &mut st,
            libc::AT_SYMLINK_NOFOLLOW,
        )
    };
    (r == 0).then_some(st)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn attrs(mode: u32) -> Attrs {
        Attrs {
            mode,
            uid: 0,
            gid: 0,
            mtime: 0,
            mtime_nsec: 0,
            atime: None,
            atime_nsec: 0,
        }
    }

    fn policy(preserve_owner: bool, preserve_perms: bool) -> AttrPolicy {
        AttrPolicy {
            preserve_owner,
            preserve_perms,
            preserve_mtime: false,
            preserve_atime: false,
            umask: 0o022,
        }
    }

    /// The whole point of the method: a member is created before its contents
    /// are written, so a set-id bit present at creation time is executable by
    /// anyone for the duration of the copy. No `-p` combination may produce
    /// one -- including `-p e`, which is the combination that asks for set-id
    /// to be preserved and so is the one that used to leave the window open.
    #[test]
    fn test_creation_mode_never_carries_set_id() {
        for mode in [0o4755, 0o2755, 0o6755, 0o4777] {
            for owner in [false, true] {
                for perms in [false, true] {
                    let created = policy(owner, perms).creation_mode(&attrs(mode));
                    assert_eq!(
                        created & 0o7000,
                        0,
                        "mode {mode:o} created as {created:o} with \
                         preserve_owner={owner} preserve_perms={perms}"
                    );
                }
            }
        }
    }

    /// It must also never be *wider* than the mode the file ends up with,
    /// or the window trades one exposure for another.
    #[test]
    fn test_creation_mode_is_never_wider_than_the_final_mode() {
        for mode in [0o4755, 0o755, 0o600, 0o777, 0o000, 0o2750] {
            for owner in [false, true] {
                for perms in [false, true] {
                    let p = policy(owner, perms);
                    let created = p.creation_mode(&attrs(mode));
                    let final_mode = p.mode(&attrs(mode));
                    assert_eq!(
                        created & !final_mode,
                        0,
                        "mode {mode:o}: created {created:o} grants what final {final_mode:o} does not"
                    );
                }
            }
        }
    }

    /// And it must still carry the ordinary permission bits, or extraction
    /// would produce unreadable files and the fix would be a regression.
    #[test]
    fn test_creation_mode_keeps_the_permission_bits() {
        // -p p preserves the mode exactly; the umask does not apply.
        assert_eq!(policy(true, true).creation_mode(&attrs(0o4755)), 0o755);
        // Without -p p the normal file-creation action applies the umask.
        assert_eq!(policy(false, false).creation_mode(&attrs(0o4777)), 0o755);
    }
}
