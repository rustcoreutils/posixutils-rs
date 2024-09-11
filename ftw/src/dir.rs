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

impl<'a> EntryInternal<'a> {
    pub fn name_cstr(&self) -> &CStr {
        // Avoid dereferencing `dirent` when getting its fields. See note at:
        // https://github.com/rust-lang/rust/blob/1.80.1/library/std/src/sys/pal/unix/fs.rs#L725-L742
        const OFFSET: isize = std::mem::offset_of!(libc::dirent, d_name) as isize;
        unsafe { CStr::from_ptr(self.dirent.byte_offset(OFFSET).cast()) }
    }

    pub fn ino(&self) -> libc::ino_t {
        const OFFSET: isize = std::mem::offset_of!(libc::dirent, d_ino) as isize;
        unsafe {
            self.dirent
                .byte_offset(OFFSET)
                .cast::<libc::ino_t>()
                .read_unaligned()
        }
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

    pub fn open_at(
        dir_file_descriptor: &FileDescriptor,
        filename: *const libc::c_char,
    ) -> Result<Self, Error> {
        let file_descriptor =
            FileDescriptor::open_at(dir_file_descriptor, filename, libc::O_RDONLY)
                .map_err(|e| Error::new(e, ErrorKind::Open))?;
        let dir = OwnedDir::new(file_descriptor).map_err(|e| Error::new(e, ErrorKind::OpenDir))?;
        Ok(dir)
    }

    pub fn iter<'a>(&'a self) -> OwnedDirIterator<'a> {
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
    visited: RefCell<HashSet<libc::ino_t>>,
}

impl DeferredDir {
    pub fn new(parent: Rc<(FileDescriptor, PathBuf)>, path: PathBuf) -> Self {
        Self {
            parent,
            path,
            visited: RefCell::new(HashSet::new()),
        }
    }

    pub fn iter<'a>(&'a self) -> DeferredDirIterator<'a> {
        let file_descriptor = self.open_file_descriptor();
        let dir = OwnedDir::new(file_descriptor).unwrap();
        let dirp = dir.dirp;

        // Passing ownership of `dirp` to `SlowDirIterator`
        std::mem::forget(dir);

        DeferredDirIterator {
            dirp,
            visited: self.visited.borrow_mut(),
        }
    }

    pub fn open_file_descriptor(&self) -> FileDescriptor {
        // e.g.:
        // self.parent.1 - foo
        // self.path - foo/bar/baz
        // remainder - bar/baz
        let remainder = self.path.strip_prefix(&self.parent.1).unwrap();

        // `remainder` is not guaranteed to be shorter than `libc::PATH_MAX`
        let (starting_dir, components) =
            open_long_filename(self.parent.0.clone(), remainder, None, &mut |_, _| {}).unwrap();

        let filename_cstr = CString::new(components.as_path().as_os_str().as_bytes()).unwrap();

        FileDescriptor::open_at(&starting_dir, filename_cstr.as_ptr(), libc::O_RDONLY).unwrap()
    }

    pub fn parent(&self) -> &Rc<(FileDescriptor, PathBuf)> {
        &self.parent
    }
}

pub struct DeferredDirIterator<'a> {
    dirp: *mut libc::DIR,
    visited: RefMut<'a, HashSet<libc::ino_t>>,
}

impl<'a> Drop for DeferredDirIterator<'a> {
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
                    let ino = entry.ino();
                    if self.visited.contains(&ino) {
                        continue;
                    } else {
                        self.visited.insert(ino);
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
    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = io::Result<EntryInternal<'a>>> + 'a> {
        match self {
            HybridDir::Owned(d) => Box::new(d.iter()),
            HybridDir::Deferred(d) => Box::new(d.iter()),
        }
    }
}
