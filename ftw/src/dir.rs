//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::{open_long_filename, Error, ErrorKind, FileDescriptor};
use std::{
    cell::{RefCell, RefMut},
    collections::HashSet,
    ffi::{CStr, CString},
    io,
    marker::PhantomData,
    os::unix::ffi::OsStrExt as _,
    path::PathBuf,
    rc::Rc,
};

// Not to be used publically. The public interface for a directory entry is `Entry`.
pub struct EntryInternal<'a> {
    dirent: *mut libc::dirent,
    phantom: PhantomData<&'a libc::dirent>,
}

impl EntryInternal<'_> {
    pub fn name_cstr(&self) -> &CStr {
        // Avoid dereferencing `dirent` when getting its fields. See note at:
        // https://github.com/rust-lang/rust/blob/1.80.1/library/std/src/sys/pal/unix/fs.rs#L725-L742
        const OFFSET: isize = std::mem::offset_of!(libc::dirent, d_name) as isize;
        unsafe { CStr::from_ptr(self.dirent.byte_offset(OFFSET).cast()) }
    }

    pub fn is_dot_or_double_dot(&self) -> bool {
        const DOT: u8 = b'.';

        let slice = self.name_cstr().to_bytes_with_nul();
        slice.get(..2) == Some(&[DOT, 0]) || slice.get(..3) == Some(&[DOT, DOT, 0])
    }
}

/// RAII wrapper for a `*mut libc::DIR`.
///
/// The state of the directory entry listing is preserved so this is more efficient than
/// `DeferredDir`.
#[derive(Debug)]
pub struct OwnedDir {
    dirp: *mut libc::DIR,
    file_descriptor: std::mem::ManuallyDrop<FileDescriptor>,
}

impl Drop for OwnedDir {
    fn drop(&mut self) {
        unsafe {
            // Also closes `self.dir_file_descriptor`
            libc::closedir(self.dirp);
        }
    }
}

impl OwnedDir {
    pub fn new(file_descriptor: FileDescriptor) -> io::Result<Self> {
        unsafe {
            let dirp = libc::fdopendir(file_descriptor.fd);
            if dirp.is_null() {
                return Err(io::Error::last_os_error());
            }

            Ok(Self {
                dirp,
                file_descriptor: std::mem::ManuallyDrop::new(file_descriptor),
            })
        }
    }

    /// Open a directory entry for traversal.
    ///
    /// `extra_flags` is OR'ed into the `openat` flags. Callers descending into a subdirectory pass
    /// `O_DIRECTORY` (and `O_NOFOLLOW` when symlinks must not be traversed) so that a directory
    /// entry that is concurrently replaced with a symlink or non-directory cannot redirect the
    /// walk (a filesystem-race / TOCTOU hardening).
    pub fn open_at(
        dir_file_descriptor: &FileDescriptor,
        filename: *const libc::c_char,
        extra_flags: libc::c_int,
    ) -> Result<Self, Error> {
        let file_descriptor = FileDescriptor::open_at(
            dir_file_descriptor,
            unsafe { CStr::from_ptr(filename) },
            libc::O_RDONLY | extra_flags,
        )
        .map_err(|e| Error::new(e, ErrorKind::Open))?;
        let dir = OwnedDir::new(file_descriptor).map_err(|e| Error::new(e, ErrorKind::OpenDir))?;
        Ok(dir)
    }

    pub fn iter(&self) -> OwnedDirIterator<'_> {
        OwnedDirIterator {
            dirp: self.dirp,
            phantom: PhantomData,
        }
    }

    pub fn file_descriptor(&self) -> &FileDescriptor {
        &self.file_descriptor
    }
}

pub struct OwnedDirIterator<'a> {
    dirp: *mut libc::DIR,
    phantom: PhantomData<&'a OwnedDir>,
}

impl<'a> Iterator for OwnedDirIterator<'a> {
    type Item = io::Result<EntryInternal<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            errno::set_errno(errno::Errno(0));

            let dirent = libc::readdir(self.dirp);
            if dirent.is_null() {
                let last_err = io::Error::last_os_error();
                let errno = last_err.raw_os_error().unwrap();
                if errno == 0 {
                    None
                } else {
                    Some(Err(last_err))
                }
            } else {
                Some(Ok(EntryInternal {
                    dirent,
                    phantom: PhantomData,
                }))
            }
        }
    }
}

/// Used when conserving file descriptors.
///
/// Its `iter` method returns `DeferredDirIterator` which has to recreate the directory state with
/// every instantiation.
#[derive(Debug)]
pub struct DeferredDir {
    parent: Rc<(FileDescriptor, PathBuf)>,
    path: PathBuf,
    /// Names already yielded, so a reopened directory resumes where it left off. Keyed on the
    /// entry name and not `d_ino`: an inode is not unique within a directory (two hard links to
    /// one file) nor across one (every mount point's root is inode 2), and a collision here
    /// silently drops a file from the walk.
    visited: RefCell<HashSet<Box<[u8]>>>,
    /// Flags OR'ed into the leaf `openat` when (re)opening this directory. Carries the same
    /// `O_DIRECTORY`/`O_NOFOLLOW` hardening as the non-deferred descent path.
    descent_flags: libc::c_int,
}

impl DeferredDir {
    pub fn new(
        parent: Rc<(FileDescriptor, PathBuf)>,
        path: PathBuf,
        descent_flags: libc::c_int,
    ) -> Self {
        Self {
            parent,
            path,
            visited: RefCell::new(HashSet::new()),
            descent_flags,
        }
    }

    pub fn iter(&self) -> io::Result<DeferredDirIterator<'_>> {
        let file_descriptor = self.open_file_descriptor()?;
        let dir = OwnedDir::new(file_descriptor)?;
        let dirp = dir.dirp;

        // Passing ownership of `dirp` to `SlowDirIterator`
        std::mem::forget(dir);

        Ok(DeferredDirIterator {
            dirp,
            visited: self.visited.borrow_mut(),
        })
    }

    /// Reopen this directory by path from the nearest ancestor descriptor still held.
    ///
    /// Fallible: the reopen is where the fail-closed `O_NOFOLLOW` hardening below actually
    /// refuses a swapped directory, and it is also where `EMFILE` shows up -- the condition that
    /// put the walk into descriptor-conserving mode in the first place. Both used to abort the
    /// process.
    pub fn open_file_descriptor(&self) -> io::Result<FileDescriptor> {
        // e.g.:
        // self.parent.1 - foo
        // self.path - foo/bar/baz
        // remainder - bar/baz
        let remainder = self.path.strip_prefix(&self.parent.1).unwrap();

        // `remainder` is not guaranteed to be shorter than `libc::PATH_MAX`
        let (starting_dir, components) =
            open_long_filename(self.parent.0.try_clone()?, remainder, None, &mut |_, _| {})?;

        let filename_cstr = CString::new(components.as_path().as_os_str().as_bytes())
            .map_err(|_| io::Error::from_raw_os_error(libc::EINVAL))?;

        // Same descent hardening as the non-deferred path. `O_NOFOLLOW` here makes a leaf that was
        // concurrently swapped for a symlink fail the reopen (fail-closed) rather than redirecting
        // the walk. Note: the deferred path has no captured dev/ino baseline, so a swap to a
        // different real directory is not detected here (documented residual).
        FileDescriptor::open_at(
            &starting_dir,
            &filename_cstr,
            libc::O_RDONLY | self.descent_flags,
        )
    }

    pub fn parent(&self) -> &Rc<(FileDescriptor, PathBuf)> {
        &self.parent
    }
}

pub struct DeferredDirIterator<'a> {
    dirp: *mut libc::DIR,
    visited: RefMut<'a, HashSet<Box<[u8]>>>,
}

impl Drop for DeferredDirIterator<'_> {
    fn drop(&mut self) {
        unsafe {
            libc::closedir(self.dirp);
        }
    }
}

impl<'a> Iterator for DeferredDirIterator<'a> {
    type Item = io::Result<EntryInternal<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            unsafe {
                errno::set_errno(errno::Errno(0));

                let dirent = libc::readdir(self.dirp);

                if dirent.is_null() {
                    let last_err = io::Error::last_os_error();
                    let errno = last_err.raw_os_error().unwrap();
                    if errno == 0 {
                        break None;
                    } else {
                        break Some(Err(last_err));
                    }
                } else {
                    let entry = EntryInternal {
                        dirent,
                        phantom: PhantomData,
                    };
                    // The name borrows the `dirent` buffer, which the next `readdir` reuses.
                    let name: Box<[u8]> = entry.name_cstr().to_bytes().into();
                    if !self.visited.insert(name) {
                        continue;
                    }

                    break Some(Ok(entry));
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum HybridDir {
    Owned(OwnedDir),
    Deferred(DeferredDir),
}

impl HybridDir {
    pub fn iter<'a>(
        &'a self,
    ) -> io::Result<Box<dyn Iterator<Item = io::Result<EntryInternal<'a>>> + 'a>> {
        match self {
            HybridDir::Owned(d) => Ok(Box::new(d.iter())),
            HybridDir::Deferred(d) => Ok(Box::new(d.iter()?)),
        }
    }
}
