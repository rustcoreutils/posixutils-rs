//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod dir;

use dir::{DeferredDir, HybridDir, OwnedDir};
use std::{
    ffi::{CStr, CString, OsStr},
    fmt, io,
    mem::MaybeUninit,
    ops::Deref,
    os::{
        fd::{AsRawFd, RawFd},
        unix::{self, ffi::OsStrExt},
    },
    path::{Path, PathBuf},
    rc::Rc,
};

// `faccessat` and `AT_EACCESS` are not exported by `libc` 0.2.189 for linux-gnu or musl, though
// they are for apple/bsd and were for 0.2.171. Declare the function here so the check does not
// depend on which `libc` 0.2.x the lockfile resolves to, and take the flag from `libc` wherever
// it does define it.
extern "C" {
    fn faccessat(
        dirfd: libc::c_int,
        pathname: *const libc::c_char,
        mode: libc::c_int,
        flags: libc::c_int,
    ) -> libc::c_int;
}

#[cfg(any(target_os = "linux", target_os = "android"))]
const AT_EACCESS: libc::c_int = 0x200;
#[cfg(not(any(target_os = "linux", target_os = "android")))]
use libc::AT_EACCESS;

/// Whether the calling process can write to `file_name`, resolved relative to `dirfd`.
///
/// This asks the kernel, via `faccessat(2)`. Comparing `st_mode` against `geteuid`/`getegid` is
/// not equivalent: it ignores supplementary groups, ACLs, read-only mounts and the superuser
/// bypass, so it reports files as unwritable that can in fact be written. Symbolic links are
/// followed, as in `access(2)`, and a path that cannot be resolved at all is not writable.
pub fn is_writable_at(dirfd: libc::c_int, file_name: &CStr) -> bool {
    unsafe { faccessat(dirfd, file_name.as_ptr(), libc::W_OK, AT_EACCESS) == 0 }
}

/// Type of error to be handled by the `err_reporter` of `traverse_directory`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    Open,
    OpenDir,
    ReadDir,
    Stat,
    ReadLink,
}

/// Why `traverse_directory` is leaving a directory.
///
/// `postprocess_dir` is called for every directory whose `file_handler` returned `Ok(true)`,
/// including those the traversal turned out to be unable to descend, so that a caller can unwind
/// per-directory state it established when it returned `Ok(true)`. This says which happened.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DirExit {
    /// The directory was opened and its entries were enumerated.
    Descended,
    /// `file_handler` returned `Ok(true)`, but the directory could not be descended into. The
    /// reason was already passed to `err_reporter`.
    NotDescended,
}

/// Wrapper for `std::io::Error` with additional context.
#[derive(Debug)]
pub struct Error {
    inner: io::Error,
    kind: ErrorKind,
}

impl Error {
    fn new(e: io::Error, kind: ErrorKind) -> Self {
        Self { inner: e, kind }
    }

    /// Determines where in the algorithm the error occurred.
    pub fn kind(&self) -> ErrorKind {
        self.kind
    }

    /// Deconstruct to the contained `std::io::Error`.
    pub fn inner(self) -> io::Error {
        self.inner
    }
}

/// RAII wrapper for a raw file descriptor.
#[derive(Debug)]
pub struct FileDescriptor {
    fd: libc::c_int,
}

impl Drop for FileDescriptor {
    fn drop(&mut self) {
        unsafe {
            // FDs are non-negative so the negative AT_FDCWD is safe to use as a guard
            if self.fd != libc::AT_FDCWD {
                libc::close(self.fd);
            }
        }
    }
}

impl FileDescriptor {
    /// Duplicate this descriptor with `dup(2)`.
    ///
    /// Fallible on purpose: a `Clone` impl has nowhere to report `EMFILE`, and the one this
    /// replaces stored the resulting `-1` instead, so the failure resurfaced later as a
    /// confusing `EBADF` from whatever used the copy.
    pub fn try_clone(&self) -> io::Result<Self> {
        // The negative `AT_FDCWD` is a sentinel, not a descriptor, so it must not be dup'ed.
        if self.fd == libc::AT_FDCWD {
            return Ok(Self { fd: libc::AT_FDCWD });
        }
        let fd = unsafe { libc::dup(self.fd) };
        if fd == -1 {
            return Err(io::Error::last_os_error());
        }
        Ok(Self { fd })
    }
}

impl FileDescriptor {
    /// Create a `FileDescriptor` with arguments similar to `libc::openat`.
    pub fn open_at(
        dir_file_descriptor: &FileDescriptor,
        file_name: &CStr,
        flags: i32,
    ) -> io::Result<Self> {
        unsafe {
            let fd = libc::openat(dir_file_descriptor.fd, file_name.as_ptr(), flags);
            if fd == -1 {
                Err(io::Error::last_os_error())
            } else {
                Ok(Self { fd })
            }
        }
    }

    /// Create a `FileDescriptor` that denotes the current working directory.
    pub fn cwd() -> Self {
        Self { fd: libc::AT_FDCWD }
    }
}

// Borrowing a file descriptor
impl AsRawFd for FileDescriptor {
    fn as_raw_fd(&self) -> RawFd {
        self.fd
    }
}

/// Metadata of an entry. This is analogous to `std::fs::Metadata`.
#[derive(Clone)]
pub struct Metadata(libc::stat);

impl fmt::Debug for Metadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Metadata")
    }
}

impl Metadata {
    /// Create a new `Metadata`.
    ///
    /// `dirfd` could be the special value `libc::AT_FDCWD` to query the metadata of a file at the
    /// process' current working directory.
    pub fn new(
        dirfd: libc::c_int,
        file_name: &CStr,
        follow_symlinks: bool,
    ) -> io::Result<Metadata> {
        let mut statbuf = MaybeUninit::uninit();
        let flags = if follow_symlinks {
            0
        } else {
            libc::AT_SYMLINK_NOFOLLOW
        };
        let ret = unsafe { libc::fstatat(dirfd, file_name.as_ptr(), statbuf.as_mut_ptr(), flags) };
        if ret != 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(Metadata(unsafe { statbuf.assume_init() }))
    }

    /// Query the file type.
    pub fn file_type(&self) -> FileType {
        match self.0.st_mode & libc::S_IFMT {
            libc::S_IFSOCK => FileType::Socket,
            libc::S_IFLNK => FileType::SymbolicLink,
            libc::S_IFREG => FileType::RegularFile,
            libc::S_IFBLK => FileType::BlockDevice,
            libc::S_IFDIR => FileType::Directory,
            libc::S_IFCHR => FileType::CharacterDevice,
            libc::S_IFIFO => FileType::Fifo,
            _ => FileType::Unknown,
        }
    }

    /// Returns `true` if this metadata is for a directory.
    pub fn is_dir(&self) -> bool {
        self.file_type().is_dir()
    }

    /// Returns `true` if this metadata is for a regular file.
    pub fn is_file(&self) -> bool {
        self.file_type().is_file()
    }

    /// Returns `true` if this metadata is for a symbolic link.
    pub fn is_symlink(&self) -> bool {
        self.file_type().is_symlink()
    }
}

impl unix::fs::MetadataExt for Metadata {
    fn dev(&self) -> u64 {
        self.0.st_dev as _
    }

    fn ino(&self) -> u64 {
        self.0.st_ino
    }

    fn mode(&self) -> u32 {
        self.0.st_mode as _
    }

    fn nlink(&self) -> u64 {
        self.0.st_nlink as _
    }

    fn uid(&self) -> u32 {
        self.0.st_uid
    }

    fn gid(&self) -> u32 {
        self.0.st_gid
    }

    fn rdev(&self) -> u64 {
        self.0.st_rdev as _
    }

    fn size(&self) -> u64 {
        self.0.st_size as _
    }

    fn atime(&self) -> i64 {
        self.0.st_atime
    }

    fn atime_nsec(&self) -> i64 {
        self.0.st_atime_nsec
    }

    fn mtime(&self) -> i64 {
        self.0.st_mtime
    }

    fn mtime_nsec(&self) -> i64 {
        self.0.st_mtime_nsec
    }

    fn ctime(&self) -> i64 {
        self.0.st_ctime
    }

    fn ctime_nsec(&self) -> i64 {
        self.0.st_ctime_nsec
    }

    fn blksize(&self) -> u64 {
        self.0.st_blksize as _
    }

    fn blocks(&self) -> u64 {
        self.0.st_blocks as _
    }
}

/// File type of an entry. Returned by `Metadata::file_type`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileType {
    Socket,
    SymbolicLink,
    RegularFile,
    BlockDevice,
    Directory,
    CharacterDevice,
    Fifo,
    /// A file whose `st_mode & S_IFMT` is not one of the types defined by the System Interfaces
    /// volume of POSIX.1-2024. Reported rather than panicking so an unexpected on-disk type cannot
    /// abort a traversal.
    Unknown,
}

impl FileType {
    /// Tests whether this file type represents a directory.
    pub fn is_dir(&self) -> bool {
        *self == FileType::Directory
    }

    /// Tests whether this file type represents a symbolic link.
    pub fn is_symlink(&self) -> bool {
        *self == FileType::SymbolicLink
    }

    /// Tests whether this file type represents a regular file.
    pub fn is_file(&self) -> bool {
        *self == FileType::RegularFile
    }
}

impl unix::fs::FileTypeExt for FileType {
    fn is_block_device(&self) -> bool {
        *self == FileType::BlockDevice
    }

    fn is_char_device(&self) -> bool {
        *self == FileType::CharacterDevice
    }

    fn is_fifo(&self) -> bool {
        *self == FileType::Fifo
    }

    fn is_socket(&self) -> bool {
        *self == FileType::Socket
    }
}

#[derive(Debug)]
struct TreeNode {
    dir: HybridDir,
    filename: Rc<[libc::c_char]>,
    metadata: Metadata,
    path_depth: usize,
}

/// An entry in the directory tree.
#[derive(Debug, Clone)]
pub struct Entry<'a> {
    dir_file_descriptor: &'a FileDescriptor,
    path_stack: &'a [Rc<[libc::c_char]>],
    filename: Rc<[libc::c_char]>,
    metadata: Option<Metadata>,
    is_symlink: Option<bool>,
    read_link: Option<Rc<[libc::c_char]>>,
}

impl<'a> Entry<'a> {
    fn new(
        dir_file_descriptor: &'a FileDescriptor,
        path_stack: &'a [Rc<[libc::c_char]>],
        filename: Rc<[libc::c_char]>,
        metadata: Option<Metadata>,
    ) -> Self {
        Self {
            dir_file_descriptor,
            path_stack,
            filename,
            metadata,
            is_symlink: None,
            read_link: None,
        }
    }

    /// Returns the file descriptor of the containing directory.
    pub fn dir_fd(&self) -> libc::c_int {
        self.dir_file_descriptor.fd
    }

    /// Returns the file name.
    ///
    /// Cast to `*const libc::c_char` for usage in libc functions.
    pub fn file_name(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.filename.as_ptr()) }
    }

    /// Returns the metadata of this entry.
    ///
    /// This is either the metadata of the file itself or the metadata of the file it points to.
    pub fn metadata(&self) -> Option<&Metadata> {
        self.metadata.as_ref()
    }

    /// Check if this entry is a symlink.
    pub fn is_symlink(&self) -> Option<bool> {
        self.is_symlink
    }

    /// Reads the symbolic link.
    pub fn read_link(&self) -> Option<&CStr> {
        self.read_link
            .as_ref()
            .map(|s| unsafe { CStr::from_ptr(s.as_ptr()) })
    }

    /// Returns the path.
    ///
    /// This is either relative to the current working directory or an absolute path.
    pub fn path(&self) -> DisplayablePath {
        DisplayablePath(build_path(self.path_stack, &self.filename))
    }

    /// Whether the calling process can write to the file this entry refers to.
    pub fn is_writable(&self) -> bool {
        is_writable_at(self.dir_fd(), self.file_name())
    }

    /// Check if this `Entry` is an empty directory.
    pub fn is_empty_dir(&self) -> io::Result<bool> {
        let file_descriptor =
            FileDescriptor::open_at(self.dir_file_descriptor, self.file_name(), libc::O_RDONLY)?;
        match OwnedDir::new(file_descriptor) {
            Ok(dir) => {
                let mut num_entries = 0;

                // Manually count the number of entries.
                for entry_or_err in dir.iter() {
                    let entry = match entry_or_err {
                        Ok(entry) => entry,
                        Err(e) => {
                            return Err(e);
                        }
                    };

                    if entry.is_dot_or_double_dot() {
                        continue;
                    }

                    num_entries += 1;
                }

                Ok(num_entries == 0)
            }
            Err(e) => Err(e),
        }
    }

    /// Returns whether this entry is a `..` or a `..`.
    pub fn is_dot_or_double_dot(&self) -> bool {
        const DOT: u8 = b'.';

        let slice = self.file_name().to_bytes_with_nul();
        slice.get(..2) == Some(&[DOT, 0]) || slice.get(..3) == Some(&[DOT, DOT, 0])
    }
}

/// Wrapper around a `PathBuf` that prevents directly using the path for `std::fs` functions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DisplayablePath(PathBuf);

impl fmt::Display for DisplayablePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt::Display::fmt(&self.0.display(), f)
    }
}

impl DisplayablePath {
    /// Simplifies trailing slashes.
    pub fn clean_trailing_slashes(&self) -> String {
        let mut s = format!("{}", self.0.display());
        while s.ends_with("//") {
            s.pop();
        }
        s
    }

    /// Get the internal representation of `self`.
    pub fn as_inner(&self) -> &Path {
        &self.0
    }
}

impl Deref for DisplayablePath {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// Ignore clippy warnings, this is only used in `process_file` below
#[allow(clippy::large_enum_variant)]
enum ProcessFileResult<'a> {
    ProcessedDirectory(Entry<'a>),
    ProcessedFile,
    NotProcessed,
    Skipped,
}

fn process_file<'a, F, H>(
    path_stack: &'a [Rc<[libc::c_char]>],
    dir_fd: &'a FileDescriptor,
    entry_filename: Rc<[libc::c_char]>,
    follow_symlinks: bool,
    is_dot_or_double_dot: bool,
    file_handler: &mut F,
    err_reporter: &mut H,
) -> ProcessFileResult<'a>
where
    F: FnMut(Entry<'_>) -> Result<bool, ()>,
    H: FnMut(Entry<'_>, Error),
{
    // Get the metadata for the file without following symlinks.
    let entry_symlink_metadata = match Metadata::new(
        dir_fd.fd,
        unsafe { CStr::from_ptr(entry_filename.as_ptr()) },
        false,
    ) {
        Ok(md) => md,
        Err(e) => {
            err_reporter(
                Entry::new(dir_fd, path_stack, entry_filename, None),
                Error::new(e, ErrorKind::Stat),
            );
            return ProcessFileResult::NotProcessed;
        }
    };
    let is_symlink = entry_symlink_metadata.file_type() == FileType::SymbolicLink;

    // Read the link target for every symbolic link, whether or not this walk follows links:
    // consumers need it either way -- `cp -P` and `mv` recreate the link from it, `ls -l` prints
    // it. Leaving it unset on a non-following walk handed callers a `None` they did not expect,
    // on the one path nobody exercised.
    let (entry_readlink, entry_metadata) = if is_symlink {
        let read_link = match read_link_at(dir_fd.fd, entry_filename.as_ptr()) {
            Ok(p) => Some(p),
            Err(e) => {
                err_reporter(
                    Entry::new(dir_fd, path_stack, entry_filename.clone(), None),
                    Error::new(e, ErrorKind::ReadLink),
                );
                if follow_symlinks {
                    // The target's metadata is about to be needed and cannot be obtained.
                    return ProcessFileResult::NotProcessed;
                }
                // Nothing else here depends on the target: a walk that does not follow links can
                // still stat, list and unlink the link itself.
                None
            }
        };

        if !follow_symlinks {
            (read_link, entry_symlink_metadata)
        } else {
            match Metadata::new(
                dir_fd.fd,
                unsafe { CStr::from_ptr(entry_filename.as_ptr()) },
                true,
            ) {
                Ok(md) => (read_link, md),
                Err(e) => {
                    if e.kind() == io::ErrorKind::NotFound {
                        // Don't treat dangling links as an error, use the metadata of the original
                        (read_link, entry_symlink_metadata)
                    } else {
                        err_reporter(
                            Entry::new(dir_fd, path_stack, entry_filename, None),
                            Error::new(e, ErrorKind::Stat),
                        );
                        return ProcessFileResult::NotProcessed;
                    }
                }
            }
        }
    } else {
        (None, entry_symlink_metadata)
    };

    let mut entry = Entry::new(dir_fd, path_stack, entry_filename, Some(entry_metadata));
    entry.is_symlink = Some(is_symlink);
    entry.read_link = entry_readlink;

    let file_handler_result = file_handler(entry.clone());

    // Always skip . and .. to avoid infinite loops
    if is_dot_or_double_dot {
        return ProcessFileResult::Skipped;
    }

    match file_handler_result {
        Ok(true) => {
            // No permission probe here: `openat` below is the authority on whether the directory
            // can be enumerated, and it needs read permission, not search permission. A mode-bit
            // comparison also ignored supplementary groups, ACLs and the superuser bypass, so it
            // refused directories the kernel would have opened. Let the open decide and report
            // the errno it actually returns.
            if entry.metadata.as_ref().unwrap().is_dir() {
                ProcessFileResult::ProcessedDirectory(entry)
            } else {
                ProcessFileResult::ProcessedFile
            }
        }
        Ok(false) => {
            // `false` means skip the directory
            ProcessFileResult::Skipped
        }
        Err(_) => ProcessFileResult::NotProcessed,
    }
}

fn open_long_filename<'a, H>(
    mut starting_dir: FileDescriptor,
    path: &'a Path,
    mut path_stack: Option<&mut Vec<Rc<[libc::c_char]>>>,
    err_reporter: &mut H,
) -> io::Result<(FileDescriptor, std::path::Components<'a>)>
where
    H: FnMut(Entry<'_>, Error),
{
    let mut path_components = path.components();

    // If `path` is too long, start at a prefix of `path`
    loop {
        let remaining_cstr =
            CString::new(path_components.as_path().as_os_str().as_bytes()).unwrap();

        // Test if openable without "filename too long" or "symlink loop" errors
        {
            let mut statbuf = MaybeUninit::uninit();
            let ret = unsafe {
                libc::fstatat(
                    starting_dir.as_raw_fd(),
                    remaining_cstr.as_ptr(),
                    statbuf.as_mut_ptr(),
                    0, // Not AT_SYMLINK_NOFOLLOW
                )
            };
            if ret == 0 {
                break; // Can open
            } else {
                let last = io::Error::last_os_error();
                let errno = last.raw_os_error();
                match errno {
                    Some(libc::ENAMETOOLONG) | Some(libc::ELOOP) => (), // Fall through below
                    _ => break, // Can't open but let the caller handle the error
                }
            }
        }

        let Some(component) = path_components.next() else {
            break;
        };

        let filename_cstr = CString::new(component.as_os_str().as_bytes()).unwrap();
        let filename = cstring_to_rc(&filename_cstr);

        starting_dir = match FileDescriptor::open_at(
            &starting_dir,
            unsafe { CStr::from_ptr(filename.as_ptr()) },
            libc::O_RDONLY,
        ) {
            Ok(fd) => fd,
            Err(e) => {
                let errno = e.raw_os_error().unwrap_or(libc::EIO);
                if let Some(path_stack) = &path_stack {
                    err_reporter(
                        Entry::new(&starting_dir, path_stack, filename, None),
                        Error::new(e, ErrorKind::Open),
                    );
                }
                // The caller still needs the reason: the deferred reopen path passes no reporter.
                return Err(io::Error::from_raw_os_error(errno));
            }
        };

        if let Some(path_stack) = &mut path_stack {
            path_stack.push(filename);
        }
    }

    Ok((starting_dir, path_components))
}

/// Options for `traverse_directory`. These are disabled by default.
#[derive(Debug, Clone, Default)] // Defaults to all `false`
pub struct TraverseDirectoryOpts {
    /// Whether to dereference `path` if it's a symlink.
    pub follow_symlinks_on_args: bool,
    /// Dereference symlinks encountered (also including `path`).
    pub follow_symlinks: bool,
    /// Do not ignore `.` and `..`
    pub include_dot_and_double_dot: bool,

    /// Number of file descriptors the *caller* holds open for each directory level of the walk.
    ///
    /// Folded into the traversal's own per-level usage when deciding to switch to the
    /// descriptor-conserving strategy. `cp`/`mv` keep one target-directory descriptor per level
    /// of the source tree, so they pass 1; without it the budget only counts ftw's own
    /// descriptors and the conserving path never engages before the process hits `EMFILE`.
    pub caller_fds_per_level: usize,
    /// List the contents of the current directory before descending into a subdirectory.
    pub list_contents_first: bool,
}

/// Walk through a directory tree.
///
/// The `file_handler` handles the processing of each entry encountered, starting with `path`
/// itself. There is no definite order of processing of entries as this function delegates to
/// `libc::readdir`.
///
/// # Arguments
/// * `path` - Pathname of the directory. Passing a file to this argument will cause the function to
///   return `false` but will otherwise allow processing the file inside `file_handler` like a
///   normal entry.
///
/// * `file_handler` - Called for each entry in the tree. If the current entry is a directory, its
///   contents will be skipped if `file_handler` returns `false`. The return value of
///   `file_handler` is ignored when the entry is a file.
///
/// * `postprocess_dir` - Called when `traverse_directory` is exiting a directory: exactly once
///   for every entry whose `file_handler` returned `Ok(true)` and whose metadata reported a
///   directory, whether or not the traversal was able to descend into it. The [`DirExit`]
///   argument says which of the two happened, so that a caller can unwind per-directory state it
///   established on that `Ok(true)` without repeating work that only makes sense for a directory
///   that was actually read. When descent was refused, `err_reporter` is called with the reason
///   first.
///
/// * `err_reporter` - Callback for reporting the errors encountered during the directory traversal.
///
/// * `opts` - Additional options for this function.
///
/// # Return
///
/// This function can return multiple errors via the `err_reporter` argument so its return value is
/// a bool indicating no errors occurred (`true`) or there is at least one error (`false`).
pub fn traverse_directory<P, F, G, H>(
    path: P,
    mut file_handler: F,
    mut postprocess_dir: G,
    mut err_reporter: H,
    opts: TraverseDirectoryOpts,
) -> bool
where
    P: AsRef<Path>,
    F: FnMut(Entry<'_>) -> Result<bool, ()>,
    G: FnMut(Entry<'_>, DirExit) -> Result<(), ()>,
    H: FnMut(Entry<'_>, Error),
{
    let TraverseDirectoryOpts {
        follow_symlinks_on_args,
        follow_symlinks,
        include_dot_and_double_dot,
        list_contents_first,
        caller_fds_per_level,
    } = opts;

    // Stack of the directories to process
    let mut stack: Vec<TreeNode> = Vec::new();
    // Stack of the filename (relative to CWD). Updated in sync with `stack` above
    let mut path_stack: Vec<Rc<[libc::c_char]>> = Vec::new();

    // Used in `ls`
    let mut subdirs: Vec<TreeNode> = Vec::new();

    let (starting_dir, path_components) = match open_long_filename(
        FileDescriptor::cwd(),
        path.as_ref(),
        Some(&mut path_stack),
        &mut err_reporter,
    ) {
        Ok(pair) => pair,
        // Already reported through `err_reporter`.
        Err(_) => return false,
    };

    {
        let dir_filename_cstr =
            CString::new(path_components.as_path().as_os_str().as_bytes()).unwrap();
        let dir_filename = cstring_to_rc(&dir_filename_cstr);

        match process_file(
            &path_stack,
            &starting_dir,
            dir_filename.clone(),
            follow_symlinks_on_args || follow_symlinks,
            false,
            &mut file_handler,
            &mut err_reporter,
        ) {
            ProcessFileResult::ProcessedDirectory(entry) => {
                // `O_DIRECTORY` rejects a non-directory; `O_NOFOLLOW` is intentionally NOT used for
                // the root operand so that a symlinked directory argument is still honored per
                // `follow_symlinks_on_args`.
                match OwnedDir::open_at(&starting_dir, dir_filename.as_ptr(), libc::O_DIRECTORY) {
                    Ok(new_dir) => {
                        let node = TreeNode {
                            dir: HybridDir::Owned(new_dir),
                            filename: dir_filename,
                            metadata: entry.metadata.unwrap(),
                            path_depth: path_stack.len(),
                        };
                        stack.push(node);
                    }
                    Err(error) => {
                        err_reporter(entry.clone(), error);
                        let _ = postprocess_dir(entry, DirExit::NotDescended);
                        return false;
                    }
                }
            }
            ProcessFileResult::ProcessedFile => {
                // `path` was not a directory
                return false;
            }
            ProcessFileResult::NotProcessed => {
                // Signal an error
                return false;
            }
            ProcessFileResult::Skipped => (), // Do nothing
        }
    }

    let mut success = true;

    // Max allowable open file descriptors. If `getrlimit` fails for any reason, fall back to a
    // conservative default rather than aborting the traversal.
    const FALLBACK_FD_LIMIT: libc::rlim_t = 1024;
    let fd_rlim_cur = unsafe {
        let mut rlim = MaybeUninit::uninit();
        let ret = libc::getrlimit(libc::RLIMIT_NOFILE, rlim.as_mut_ptr());
        if ret != 0 {
            FALLBACK_FD_LIMIT
        } else {
            rlim.assume_init().rlim_cur
        }
    };

    // Descriptors that are in use but not accounted for per level: the three standard streams,
    // the two a caller holds open while copying one file, the one this walk reopens a deferred
    // parent with on the way out, and slack. Conservation has to start before these no longer
    // fit, or the walk fails at its deepest point having done all the work.
    const FD_RESERVE: usize = 16;
    // The upper clamp is for `RLIM_INFINITY`, which would otherwise disable conservation
    // entirely and leave the walk to fail at the kernel's own per-process cap instead.
    const MAX_FD_THRESHOLD: usize = 4096;
    let fd_threshold: usize = (fd_rlim_cur as usize)
        .saturating_sub(FD_RESERVE)
        .clamp(1, MAX_FD_THRESHOLD);

    // Flags OR'ed into every descent `openat`. `O_DIRECTORY` rejects a directory entry that was
    // concurrently replaced with a non-directory (e.g. a FIFO, which would otherwise block the
    // open). When symlinks are not being followed, `O_NOFOLLOW` additionally rejects a leaf that
    // was swapped for a symlink, so the walk cannot be redirected out of the tree.
    let descent_flags: libc::c_int = if follow_symlinks {
        libc::O_DIRECTORY
    } else {
        libc::O_DIRECTORY | libc::O_NOFOLLOW
    };

    // Refuse to descend into a directory whose `file_handler` already returned `Ok(true)`:
    // report why, then still run `postprocess_dir` so the caller can unwind whatever state it
    // established on that `Ok(true)`. Without the second call a caller that pushes per-directory
    // state (`cp`'s target-directory descriptor stack, `du`'s running totals) is left one level
    // too deep for the remainder of the walk.
    macro_rules! refuse_descent {
        ($entry:expr, $error:expr) => {{
            let entry = $entry;
            err_reporter(entry.clone(), $error);
            success = false;
            if postprocess_dir(entry, DirExit::NotDescended).is_err() {
                success = false;
            }
            continue;
        }};
    }

    // Depth first traversal main loop
    'outer: while let Some(current) = stack.last() {
        let dir = &current.dir;

        // Keeps a deferred directory's reopened handle alive for as long as it is enumerated.
        // Dropped before the exit block below, which needs a descriptor of its own.
        let mut reopened = None;

        // Resize `path_stack` to the appropriate depth.
        debug_assert!(path_stack.len() >= current.path_depth);
        path_stack.truncate(current.path_depth);

        // Push the directory's filename. The contents' filename will be concatenated to the
        // directory's filename.
        path_stack.push(current.filename.clone());

        let path_depth = path_stack.len();

        // Acquiring the descriptor and the directory stream can both fail: a deferred directory
        // is reopened by path here, which is where a concurrently swapped entry is refused and
        // where EMFILE lands. The node is already on the stack and its `file_handler` returned
        // `Ok(true)`, so report and fall through to the exit block below -- a `continue` would
        // spin on the same node forever.
        let mut dir_exit = DirExit::Descended;
        // The failure is recorded rather than handled in place: anything still holding the
        // directory stream also holds a borrow of `stack`, which the body below pushes to.
        let mut enumeration_error: Option<io::Error> = None;

        // A labeled block, not nested matches: each `match` temporary then dies at the end of its
        // own `let`, leaving only `dir_iter` holding the borrow of `dir` -- which the body
        // already drops explicitly before pushing to `stack`.
        'enumerate: {
            // One descriptor per visit, not two: a deferred directory is reopened once here and
            // both the descriptor and the entry stream come from that same handle. Opening them
            // separately cost two descriptors per level in exactly the mode that exists to
            // conserve them.
            let (dir_fd, mut dir_iter): (&FileDescriptor, Box<dyn Iterator<Item = _>>) = match dir {
                HybridDir::Owned(dir) => (dir.file_descriptor(), Box::new(dir.iter())),
                HybridDir::Deferred(deferred) => match deferred.open() {
                    Ok(owned) => {
                        let owned = reopened.insert(owned);
                        (owned.file_descriptor(), Box::new(deferred.iter_in(owned)))
                    }
                    Err(e) => {
                        enumeration_error = Some(e);
                        break 'enumerate;
                    }
                },
            };
            {
                // Read the current directory
                while let Some(entry_or_err) = dir_iter.next() {
                    let entry = match entry_or_err {
                        Ok(entry) => entry,

                        // Errors in reading the entry usually occurs due to lack of permissions
                        Err(e) => {
                            // `path_stack` is the ancestors plus `current.filename`, and the report
                            // names `current` itself, so its parent is everything before the last
                            // component. This is the same slice the `postprocess_dir` call below
                            // passes, which pops first and then hands over the whole stack.
                            debug_assert_eq!(path_stack.len(), path_depth);
                            let parent_path_stack = &path_stack[..path_depth - 1];
                            // Only `entry.path()` is read from this report, and that comes from the
                            // path stack, so a deferred parent that cannot be reopened right now can
                            // fall back to the starting directory.
                            let reopened_parent;
                            let prev_dir = match stack.len().checked_sub(2) {
                                Some(index) => match &stack.get(index).unwrap().dir {
                                    HybridDir::Owned(dir) => dir.file_descriptor(),
                                    HybridDir::Deferred(dir) => match dir.open_file_descriptor() {
                                        Ok(fd) => {
                                            reopened_parent = fd;
                                            &reopened_parent
                                        }
                                        Err(_) => &starting_dir,
                                    },
                                },
                                None => &starting_dir,
                            };
                            err_reporter(
                                Entry::new(
                                    prev_dir,
                                    parent_path_stack,
                                    current.filename.clone(),
                                    Some(current.metadata.clone()),
                                ),
                                Error::new(e, ErrorKind::ReadDir),
                            );

                            success = false;

                            // A failing `readdir` keeps returning NULL with the same errno, so
                            // continuing here would spin forever re-reporting it. POSIX leaves the
                            // stream position unspecified after an error; give up on this directory
                            // and let it take the normal exit path.
                            break;
                        }
                    };

                    let is_dot_or_double_dot = entry.is_dot_or_double_dot();

                    // Skip . and ..
                    if is_dot_or_double_dot && !include_dot_and_double_dot {
                        continue;
                    }

                    let entry_filename = cstring_to_rc(entry.name_cstr());

                    let conserve_fds = match dir {
                        HybridDir::Owned(_) => {
                            let used_fds = stack
                                .len()
                                .saturating_add(subdirs.len())
                                .saturating_mul(1usize.saturating_add(caller_fds_per_level));
                            used_fds >= fd_threshold
                        }
                        HybridDir::Deferred(_) => {
                            // If parent is conserving file descriptors, so should its subdirectories
                            true
                        }
                    };

                    match process_file(
                        &path_stack,
                        dir_fd,
                        entry_filename.clone(),
                        follow_symlinks,
                        is_dot_or_double_dot,
                        &mut file_handler,
                        &mut err_reporter,
                    ) {
                        ProcessFileResult::ProcessedDirectory(entry) => {
                            let (want_dev, want_ino) = {
                                let md = entry.metadata.as_ref().unwrap();
                                (md.0.st_dev, md.0.st_ino)
                            };

                            // Symbolic-link loop detection. Only possible when following symlinks
                            // (a real directory tree is acyclic). `stack` is exactly the chain of
                            // ancestors of the entry about to be descended, so re-encountering an
                            // ancestor's (dev, ino) means a cycle.
                            if follow_symlinks
                                && stack.iter().any(|n| {
                                    n.metadata.0.st_dev == want_dev
                                        && n.metadata.0.st_ino == want_ino
                                })
                            {
                                refuse_descent!(
                                    entry,
                                    Error::new(
                                        io::Error::from_raw_os_error(libc::ELOOP),
                                        ErrorKind::Stat,
                                    )
                                );
                            }

                            let node = if conserve_fds {
                                match dir {
                                    HybridDir::Owned(current_dir) => {
                                        let path = build_path(&path_stack, &entry_filename);
                                        let anchor = match current_dir.file_descriptor().try_clone()
                                        {
                                            Ok(fd) => fd,
                                            // Running out of descriptors is exactly the condition
                                            // that put the walk in conserving mode; there is nothing
                                            // to fall back to.
                                            Err(e) => {
                                                refuse_descent!(
                                                    entry,
                                                    Error::new(e, ErrorKind::Open)
                                                )
                                            }
                                        };
                                        let slow_dir = DeferredDir::new(
                                            Rc::new((anchor, path.parent().unwrap().to_path_buf())),
                                            path,
                                            descent_flags,
                                        );
                                        TreeNode {
                                            dir: HybridDir::Deferred(slow_dir),
                                            filename: entry_filename,
                                            metadata: entry.metadata.unwrap(),
                                            path_depth,
                                        }
                                    }
                                    HybridDir::Deferred(current_dir) => {
                                        let slow_dir = DeferredDir::new(
                                            current_dir.parent().clone(),
                                            build_path(&path_stack, &entry_filename),
                                            descent_flags,
                                        );
                                        TreeNode {
                                            dir: HybridDir::Deferred(slow_dir),
                                            filename: entry_filename,
                                            metadata: entry.metadata.unwrap(),
                                            path_depth,
                                        }
                                    }
                                }
                            } else {
                                match OwnedDir::open_at(
                                    dir_fd,
                                    entry_filename.as_ptr(),
                                    descent_flags,
                                ) {
                                    Ok(new_dir) => {
                                        // TOCTOU re-verification: confirm the directory we opened is the
                                        // very file we stat'd. A concurrent swap to a different
                                        // directory passes `O_NOFOLLOW`/`O_DIRECTORY` but changes
                                        // (dev, ino), so the walk would otherwise be redirected.
                                        let verified = {
                                            let mut sb = MaybeUninit::<libc::stat>::uninit();
                                            let r = unsafe {
                                                libc::fstat(
                                                    new_dir.file_descriptor().as_raw_fd(),
                                                    sb.as_mut_ptr(),
                                                )
                                            };
                                            r == 0 && {
                                                let sb = unsafe { sb.assume_init() };
                                                sb.st_dev == want_dev && sb.st_ino == want_ino
                                            }
                                        };
                                        if !verified {
                                            refuse_descent!(
                                                entry,
                                                Error::new(
                                                    io::Error::from_raw_os_error(libc::ENOTDIR),
                                                    ErrorKind::OpenDir,
                                                )
                                            );
                                        }
                                        TreeNode {
                                            dir: HybridDir::Owned(new_dir),
                                            filename: entry_filename,
                                            metadata: entry.metadata.unwrap(),
                                            path_depth,
                                        }
                                    }
                                    Err(error) => {
                                        refuse_descent!(entry, error);
                                    }
                                }
                            };

                            if list_contents_first {
                                subdirs.push(node);
                            } else {
                                // `dir_iter` has a dependency on `stack` so run it's `Drop` method first
                                std::mem::drop(dir_iter);

                                stack.push(node);
                                continue 'outer;
                            }
                        }
                        ProcessFileResult::NotProcessed => {
                            success = false;
                        }
                        ProcessFileResult::ProcessedFile | ProcessFileResult::Skipped => (),
                    }
                }
            }
        }

        // The directory has been read; give its descriptor back before reopening the parent.
        drop(reopened);

        if let Some(e) = enumeration_error {
            err_reporter(
                Entry::new(
                    &starting_dir,
                    &path_stack[..path_depth - 1],
                    current.filename.clone(),
                    Some(current.metadata.clone()),
                ),
                Error::new(e, ErrorKind::OpenDir),
            );
            success = false;
            dir_exit = DirExit::NotDescended;
        }

        if list_contents_first && !subdirs.is_empty() {
            // Lexicographically sort for ls
            subdirs.sort_by(|a, b| {
                let filename_a = unsafe { CStr::from_ptr(a.filename.as_ptr()) };
                let filename_b = unsafe { CStr::from_ptr(b.filename.as_ptr()) };
                filename_a.cmp(filename_b)
            });

            // Add in reverse order because `stack` is a LIFO
            while let Some(node) = subdirs.pop() {
                stack.push(node);
            }
            continue 'outer;
        }

        // Undoes the `path_stack.push` above
        path_stack.pop();

        // The exit callback acts relative to the *parent's* descriptor -- `rm` unlinks through
        // it -- so unlike the reporter above there is no safe fallback if a deferred parent
        // cannot be reopened: a wrong descriptor here would remove the wrong directory. Report
        // and skip the callback instead. This is the one documented gap in the invariant, and it
        // is reachable only in descriptor-conserving mode.
        let reopened_parent;
        let prev_dir = match stack.len().checked_sub(2) {
            Some(index) => match &stack.get(index).unwrap().dir {
                HybridDir::Owned(dir) => Ok(dir.file_descriptor()),
                HybridDir::Deferred(dir) => match dir.open_file_descriptor() {
                    Ok(fd) => {
                        reopened_parent = fd;
                        Ok(&reopened_parent)
                    }
                    Err(e) => Err(e),
                },
            },
            None => Ok(&starting_dir),
        };
        match prev_dir {
            Ok(prev_dir) => {
                if postprocess_dir(
                    Entry::new(
                        prev_dir,
                        &path_stack,
                        current.filename.clone(),
                        Some(current.metadata.clone()),
                    ),
                    dir_exit,
                )
                .is_err()
                {
                    success = false;
                    // Don't `continue` here, falldown below
                }
            }
            Err(e) => {
                err_reporter(
                    Entry::new(
                        &starting_dir,
                        &path_stack,
                        current.filename.clone(),
                        Some(current.metadata.clone()),
                    ),
                    Error::new(e, ErrorKind::Open),
                );
                success = false;
            }
        }

        // Process the next node
        stack.pop().unwrap();
    }

    success
}

fn cstring_to_rc(filename: &CStr) -> Rc<[libc::c_char]> {
    let bytes_with_nul = filename.to_bytes_with_nul();

    // Transmute `&[u8]` to `&[libc::c_char]`
    let char_slice_with_nul =
        unsafe { std::slice::from_raw_parts(bytes_with_nul.as_ptr().cast(), bytes_with_nul.len()) };

    filename_slice_to_rc(char_slice_with_nul)
}

fn filename_slice_to_rc(filename: &[libc::c_char]) -> Rc<[libc::c_char]> {
    Rc::from(filename.to_vec().into_boxed_slice())
}

// Helper function for `libc::readlinkat`
fn read_link_at(
    dirfd: libc::c_int,
    filename: *const libc::c_char,
) -> io::Result<Rc<[libc::c_char]>> {
    read_link_at_with_capacity(dirfd, filename, libc::PATH_MAX as usize)
}

/// Largest symbolic link target this will read before giving up with `ENAMETOOLONG`.
const READ_LINK_MAX: usize = 1 << 16;

/// `read_link_at` with an explicit starting buffer size, so the grow-and-retry path can be
/// exercised by a test without needing a filesystem that allows a `PATH_MAX`-sized target.
fn read_link_at_with_capacity(
    dirfd: libc::c_int,
    filename: *const libc::c_char,
    initial_capacity: usize,
) -> io::Result<Rc<[libc::c_char]>> {
    let mut capacity = initial_capacity.max(1);

    loop {
        let mut buf = vec![0; capacity];

        let ret = unsafe { libc::readlinkat(dirfd, filename, buf.as_mut_ptr(), buf.len()) };
        if ret < 0 {
            return Err(io::Error::last_os_error());
        }
        let num_bytes = ret as usize;

        // `readlinkat` truncates silently and does not NUL-terminate, so a completely full
        // buffer is indistinguishable from a target that is exactly that long. Grow and retry.
        if num_bytes == buf.len() {
            capacity = match capacity.checked_mul(2) {
                Some(c) if c <= READ_LINK_MAX => c,
                _ => return Err(io::Error::from_raw_os_error(libc::ENAMETOOLONG)),
            };
            continue;
        }

        // `Vec::shrink_to` would only lower the capacity; the length has to be cut explicitly or
        // the `CStr` built from this is terminated only by luck of the zero-fill.
        buf.truncate(num_bytes);
        buf.push(0);
        return Ok(Rc::from(buf.into_boxed_slice()));
    }
}

// Build the full path of an entry
fn build_path(path_stack: &[Rc<[libc::c_char]>], filename: &Rc<[libc::c_char]>) -> PathBuf {
    let mut pathbuf = PathBuf::new();

    let mut append = |p: *const libc::c_char| {
        let cstr = unsafe { CStr::from_ptr(p) };
        let os_str = OsStr::from_bytes(cstr.to_bytes());
        pathbuf.push(os_str);
    };

    for p in path_stack {
        append(p.as_ptr());
    }
    append(filename.as_ptr());

    pathbuf
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    /// `readlinkat` neither NUL-terminates nor reports truncation, so a buffer it fills exactly
    /// has to be grown and retried. Driving the loop from a 1-byte buffer covers the retry path
    /// without needing a target near `PATH_MAX`.
    #[test]
    fn read_link_at_grows_until_the_target_fits() {
        let tmp_dir = plib::tmp::tempdir().unwrap();

        let target = "t".repeat(200);
        let link = tmp_dir.path().join("link");
        std::os::unix::fs::symlink(&target, &link).unwrap();

        let link_cstr = CString::new(link.as_os_str().as_bytes()).unwrap();
        let read = read_link_at_with_capacity(libc::AT_FDCWD, link_cstr.as_ptr(), 1).unwrap();

        let as_cstr = unsafe { CStr::from_ptr(read.as_ptr()) };
        assert_eq!(as_cstr.to_bytes(), target.as_bytes());
    }
}
