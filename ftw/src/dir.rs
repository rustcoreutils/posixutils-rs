//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::{fd_matches, open_long_filename, Error, ErrorKind, FileDescriptor};
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
    /// `(st_dev, st_ino)` the walk recorded when it stat'ed this directory. Every reopen is
    /// checked against it, as a first descent is.
    identity: (libc::dev_t, libc::ino_t),
    /// The recorded identities of the directories between the anchor (`parent`) and this one,
    /// nearest last; `None` when this directory is a child of the anchor. A reopen that has to
    /// walk the path one component at a time checks each component against these.
    ancestors: Option<Rc<Lineage>>,
}

/// One link of a deferred directory's chain of ancestor identities. Shared rather than copied,
/// so a deep conserving walk holds one link per level, not one chain per level.
#[derive(Debug)]
pub struct Lineage {
    identity: (libc::dev_t, libc::ino_t),
    up: Option<Rc<Lineage>>,
}

impl DeferredDir {
    pub fn new(
        parent: Rc<(FileDescriptor, PathBuf)>,
        path: PathBuf,
        descent_flags: libc::c_int,
        identity: (libc::dev_t, libc::ino_t),
        ancestors: Option<Rc<Lineage>>,
    ) -> Self {
        Self {
            parent,
            path,
            visited: RefCell::new(HashSet::new()),
            descent_flags,
            identity,
            ancestors,
        }
    }

    /// The ancestor chain for a deferred child of this directory.
    pub fn lineage(&self) -> Rc<Lineage> {
        Rc::new(Lineage {
            identity: self.identity,
            up: self.ancestors.clone(),
        })
    }

    /// The identities of every directory from the anchor down to and including this one, in
    /// path order: one per component of this directory's path from the anchor.
    fn component_identities(&self) -> Vec<(libc::dev_t, libc::ino_t)> {
        let mut ids = vec![self.identity];
        let mut link = self.ancestors.as_deref();
        while let Some(l) = link {
            ids.push(l.identity);
            link = l.up.as_deref();
        }
        ids.reverse();
        ids
    }

    /// Reopen this directory for one visit.
    ///
    /// The caller keeps the result alive and takes both the descriptor and the entry stream from
    /// it, so a conserving walk needs one descriptor per visit rather than two.
    pub fn open(&self) -> io::Result<OwnedDir> {
        OwnedDir::new(self.open_file_descriptor()?)
    }

    /// Enumerate this directory through a descriptor already opened for it by `open`.
    ///
    /// Entries already yielded on an earlier visit are filtered out, which is what lets a
    /// directory that is reopened from scratch each time resume where it left off.
    pub fn iter_in<'a>(&'a self, dir: &'a OwnedDir) -> DeferredDirIterator<'a> {
        DeferredDirIterator {
            dirp: dir.dirp,
            visited: self.visited.borrow_mut(),
            phantom: PhantomData,
        }
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

        // `remainder` is not guaranteed to be shorter than `libc::PATH_MAX`. When it is not, the
        // prefix is opened one component at a time, each with this walk's descent flags and
        // checked against the identity the walk recorded for it.
        let identities = self.component_identities();
        let (starting_dir, components) = open_long_filename(
            self.parent.0.try_clone()?,
            remainder,
            None,
            self.descent_flags,
            Some(&identities),
            &mut |_, _| {},
        )?;

        let filename_cstr = CString::new(components.as_path().as_os_str().as_bytes())
            .map_err(|_| io::Error::from_raw_os_error(libc::EINVAL))?;

        // Same descent hardening as the non-deferred path. `O_NOFOLLOW` here makes a leaf that was
        // concurrently swapped for a symlink fail the reopen (fail-closed) rather than redirecting
        // the walk. The prefix is resolved by the kernel and may cross a symbolic link swapped in
        // for an intermediate directory; the identity check below catches that too, since
        // whatever the path reaches must be the very directory the walk stat'ed.
        let fd = FileDescriptor::open_at(
            &starting_dir,
            &filename_cstr,
            libc::O_RDONLY | self.descent_flags,
        )?;
        if !fd_matches(&fd, self.identity.0, self.identity.1) {
            return Err(io::Error::from_raw_os_error(libc::ENOTDIR));
        }
        Ok(fd)
    }

    pub fn parent(&self) -> &Rc<(FileDescriptor, PathBuf)> {
        &self.parent
    }
}

pub struct DeferredDirIterator<'a> {
    dirp: *mut libc::DIR,
    visited: RefMut<'a, HashSet<Box<[u8]>>>,
    /// The stream belongs to the `OwnedDir` this was created from, which closes it.
    phantom: PhantomData<&'a OwnedDir>,
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
