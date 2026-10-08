//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The operations behind `safe_fs` where there are no directory-relative
//! system calls (Windows). The same checks are made by path: a link is still
//! refused and never written through, but the checks and the use that follows
//! them are separate steps, so a tree changed concurrently can race them.

use std::ffi::{OsStr, OsString};
use std::fs::{self, File, Metadata, OpenOptions};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU32, Ordering};

/// A directory, by path.
pub struct Dir(PathBuf);

/// What stands at a name, without following a link.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kind {
    Regular,
    Symlink,
    Other,
}

pub fn open_cwd() -> io::Result<Dir> {
    Ok(Dir(PathBuf::from(".")))
}

pub fn open_dir(path: &Path) -> io::Result<Dir> {
    if fs::metadata(path)?.is_dir() {
        Ok(Dir(path.to_path_buf()))
    } else {
        Err(io::Error::from(io::ErrorKind::NotADirectory))
    }
}

/// The directory `name` in `parent`, which must not be a link.
pub fn open_subdir(parent: &Dir, name: &OsStr) -> io::Result<Dir> {
    let path = parent.0.join(name);
    let meta = fs::symlink_metadata(&path)?;
    if meta.is_dir() && !meta.file_type().is_symlink() {
        Ok(Dir(path))
    } else {
        Err(io::Error::from(io::ErrorKind::NotADirectory))
    }
}

pub fn make_subdir(parent: &Dir, name: &OsStr) -> io::Result<Dir> {
    match fs::create_dir(parent.0.join(name)) {
        Err(e) if e.kind() != io::ErrorKind::AlreadyExists => Err(e),
        _ => open_subdir(parent, name),
    }
}

pub fn lstat(dir: &Dir, name: &OsStr) -> io::Result<Option<Kind>> {
    match fs::symlink_metadata(dir.0.join(name)) {
        Ok(meta) if meta.file_type().is_symlink() => Ok(Some(Kind::Symlink)),
        Ok(meta) if meta.is_file() => Ok(Some(Kind::Regular)),
        Ok(_) => Ok(Some(Kind::Other)),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(e),
    }
}

/// Without inode numbers, a regular file still at the name is taken to be
/// the same one.
pub fn is_same_file(dir: &Dir, name: &OsStr, _meta: &Metadata) -> io::Result<bool> {
    Ok(lstat(dir, name)? == Some(Kind::Regular))
}

pub fn open_read(dir: &Dir, name: &OsStr) -> io::Result<File> {
    File::open(dir.0.join(name))
}

pub fn open_append(dir: &Dir, name: &OsStr) -> io::Result<File> {
    OpenOptions::new().append(true).open(dir.0.join(name))
}

/// `_private` has no meaning without Unix modes.
pub fn create_temp(dir: &Dir, _private: bool) -> io::Result<(File, OsString)> {
    static COUNTER: AtomicU32 = AtomicU32::new(0);
    loop {
        let n = COUNTER.fetch_add(1, Ordering::Relaxed);
        let name = OsString::from(format!(".patch.{}.{}", std::process::id(), n));
        match OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(dir.0.join(&name))
        {
            Ok(file) => return Ok((file, name)),
            Err(e) if e.kind() == io::ErrorKind::AlreadyExists && n < u32::MAX => continue,
            Err(e) => return Err(e),
        }
    }
}

/// Only the read-only attribute carries over.
pub fn set_owner_and_mode(file: &File, meta: &Metadata) -> io::Result<()> {
    file.set_permissions(meta.permissions())
}

pub fn rename(dir: &Dir, from: &OsStr, to: &OsStr) -> io::Result<()> {
    fs::rename(dir.0.join(from), dir.0.join(to))
}

pub fn unlink(dir: &Dir, name: &OsStr) -> io::Result<()> {
    fs::remove_file(dir.0.join(name))
}

pub fn rmdir(dir: &Dir, name: &OsStr) -> io::Result<()> {
    fs::remove_dir(dir.0.join(name))
}

/// Open a file the user named for output (-o, -r).
pub fn open_user_output(path: &Path, append: bool) -> io::Result<File> {
    OpenOptions::new()
        .write(true)
        .create(true)
        .append(append)
        .truncate(!append)
        .open(path)
}
