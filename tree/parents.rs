//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `cp --parents`: the destination of `dir/sub/file` is `target/dir/sub/file`, and the
//! directories on the way are made from the source's. Forced by debhelper (dh_install,
//! dh_installdocs, dh_installexamples), which copies a filtered tree one path at a time.
//!
//! The directories are walked with `openat`/`mkdirat` from descriptors, so each step acts on
//! the directory the previous step opened, and `-p` applies attributes through those same
//! descriptors.

use crate::common::{copy_file_at, error_string, CopyConfig, InodeMap};
use gettextrs::gettext;
use std::collections::HashSet;
use std::ffi::CString;
use std::fs::{File, FileTimes};
use std::io;
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::{fchown, MetadataExt, PermissionsExt};
use std::path::{Component, Path, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// A destination directory `--parents` created, with the source directory it stands for.
struct MadeDir {
    dest: File,
    source: std::fs::Metadata,
}

/// Open directory `name` in `dirfd`. `extra_flags` is OR'ed in (`O_NOFOLLOW`).
fn open_dir_at(dirfd: libc::c_int, name: &CString, extra_flags: libc::c_int) -> io::Result<File> {
    let fd = unsafe {
        libc::openat(
            dirfd,
            name.as_ptr(),
            libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC | extra_flags,
        )
    };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(File::from(unsafe { OwnedFd::from_raw_fd(fd) }))
}

fn cstring(bytes: &[u8]) -> io::Result<CString> {
    CString::new(bytes).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))
}

/// Make every directory in `source`'s parent path under `target`, returning the ones made and
/// a descriptor for the last one, where the copy itself goes.
///
/// A directory that already exists is used as it is, a symbolic link to one included, as GNU cp
/// does. A directory this call made is opened with `O_NOFOLLOW`: one swapped for a symbolic link
/// between the `mkdirat` and the `openat` is refused, so neither the copy nor `-p`'s owner, mode
/// and times can be redirected through it.
fn make_parents(source: &Path, target: &Path) -> io::Result<(Vec<MadeDir>, File)> {
    let mut made = Vec::new();
    let mut dest_dir = open_dir_at(libc::AT_FDCWD, &cstring(target.as_os_str().as_bytes())?, 0)?;
    let Some(parent) = source.parent() else {
        return Ok((made, dest_dir));
    };
    let start = if source.is_absolute() { "/" } else { "." };
    let mut src_dir = open_dir_at(libc::AT_FDCWD, &cstring(start.as_bytes())?, 0)?;
    let mut dest_path = target.to_path_buf();

    for comp in parent.components() {
        // `..` is followed as written, as GNU cp does.
        let name = match comp {
            Component::Normal(_) | Component::ParentDir => cstring(comp.as_os_str().as_bytes())?,
            Component::RootDir | Component::CurDir | Component::Prefix(_) => continue,
        };
        dest_path.push(comp);

        let next_src = open_dir_at(src_dir.as_raw_fd(), &name, 0).map_err(|e| {
            io::Error::other(gettext!(
                "cannot stat '{}': {}",
                source.display(),
                error_string(&e)
            ))
        })?;
        let src_md = next_src.metadata()?;

        // Owner search and write are needed to fill the directory; -p restores the exact
        // mode afterwards, and without -p the umask applies as it does to cp -R.
        let mode = (src_md.mode() & 0o7777) as libc::mode_t | libc::S_IRWXU;
        let created = unsafe { libc::mkdirat(dest_dir.as_raw_fd(), name.as_ptr(), mode) } == 0;
        if !created {
            let e = io::Error::last_os_error();
            if e.raw_os_error() != Some(libc::EEXIST) {
                return Err(io::Error::other(gettext!(
                    "cannot make directory '{}': {}",
                    dest_path.display(),
                    error_string(&e)
                )));
            }
        }
        let nofollow = if created { libc::O_NOFOLLOW } else { 0 };
        let next_dest = open_dir_at(dest_dir.as_raw_fd(), &name, nofollow).map_err(|e| {
            io::Error::other(gettext!(
                "'{}' exists but is not a directory: {}",
                dest_path.display(),
                error_string(&e)
            ))
        })?;
        if created {
            made.push(MadeDir {
                dest: next_dest.try_clone()?,
                source: src_md,
            });
        }
        src_dir = next_src;
        dest_dir = next_dest;
    }
    Ok((made, dest_dir))
}

fn time_of(secs: i64, nsec: i64) -> SystemTime {
    let d = Duration::new(secs.unsigned_abs(), nsec as u32);
    if secs >= 0 {
        UNIX_EPOCH + d
    } else {
        UNIX_EPOCH - d
    }
}

/// `-p` for a directory `--parents` made: owner, mode and times of its source directory.
/// As with files, set-user-ID and set-group-ID are dropped when the owner cannot be copied.
fn preserve_dir(dir: &MadeDir) -> io::Result<()> {
    let src = &dir.source;
    let chown_ok = fchown(&dir.dest, Some(src.uid()), Some(src.gid())).is_ok();
    let mut mode = src.mode() & 0o7777;
    if !chown_ok {
        // Cast needed where `mode_t` is not u32 (macOS).
        #[allow(clippy::unnecessary_cast)]
        let id_bits = (libc::S_ISUID | libc::S_ISGID) as u32;
        mode &= !id_bits;
    }
    dir.dest
        .set_permissions(std::fs::Permissions::from_mode(mode))?;
    let times = FileTimes::new()
        .set_accessed(time_of(src.atime(), src.atime_nsec()))
        .set_modified(time_of(src.mtime(), src.mtime_nsec()));
    dir.dest.set_times(times)
}

/// Copy each source to `target` joined with the source's own path. Returns false if anything
/// failed; every failure has been reported.
pub fn copy_with_parents<F>(
    cfg: &CopyConfig,
    sources: &[PathBuf],
    target: &Path,
    mut inode_map: Option<&mut InodeMap>,
    prompt_fn: F,
) -> bool
where
    F: Copy + Fn(&str) -> bool,
{
    let mut ok = true;
    let mut created_files = HashSet::new();
    for source in sources {
        let (made, dest_dir) = match make_parents(source, target) {
            Ok(pair) => pair,
            Err(e) => {
                eprintln!("cp: {}", error_string(&e));
                ok = false;
                continue;
            }
        };
        let relative: PathBuf = source
            .components()
            .filter(|c| !matches!(c, Component::RootDir | Component::Prefix(_)))
            .collect();
        let dest = target.join(relative);
        // The copy is made in the directory `make_parents` holds open, never by re-resolving
        // `dest`, whose components could have been swapped since.
        if let Err(e) = copy_file_at(
            cfg,
            source,
            &dest,
            Some(OwnedFd::from(dest_dir).into()),
            &mut created_files,
            inode_map.as_deref_mut(),
            prompt_fn,
        ) {
            let s = error_string(&e);
            if !s.is_empty() {
                eprintln!("cp: {s}");
            }
            ok = false;
        }
        if cfg.preserve {
            for dir in &made {
                if let Err(e) = preserve_dir(dir) {
                    eprintln!("cp: {}", error_string(&e));
                    ok = false;
                }
            }
        }
    }
    ok
}
