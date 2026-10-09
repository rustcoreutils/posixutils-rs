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
    path::{Component, Path, PathBuf},
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

/// Whether `file_name`, resolved relative to `dirfd`, is executable (searchable, for a
/// directory) as `access(2)` with `X_OK` answers it: for the real user and group IDs, following
/// a final symbolic link. Only the last component is looked up, through `dirfd`.
pub fn is_executable_at(dirfd: libc::c_int, file_name: &CStr) -> bool {
    unsafe { faccessat(dirfd, file_name.as_ptr(), libc::X_OK, 0) == 0 }
}

/// Type of error to be handled by the `err_reporter` of `traverse_directory`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    Open,
    OpenDir,
    ReadDir,
    Stat,
    ReadLink,
    /// Following a symbolic link would re-enter a directory that is an ancestor of the entry (same
    /// `(st_dev, st_ino)`), so the traversal refused to descend. The error is `ELOOP`.
    Cycle,
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
    /// Duplicate this descriptor, close-on-exec (`fcntl(F_DUPFD_CLOEXEC)`).
    ///
    /// Fallible on purpose: a `Clone` impl has nowhere to report `EMFILE`, and the one this
    /// replaces stored the resulting `-1` instead, so the failure resurfaced later as a
    /// confusing `EBADF` from whatever used the copy.
    pub fn try_clone(&self) -> io::Result<Self> {
        // The negative `AT_FDCWD` is a sentinel, not a descriptor, so it must not be dup'ed.
        if self.fd == libc::AT_FDCWD {
            return Ok(Self { fd: libc::AT_FDCWD });
        }
        let fd = unsafe { libc::fcntl(self.fd, libc::F_DUPFD_CLOEXEC, 0) };
        if fd == -1 {
            return Err(io::Error::last_os_error());
        }
        Ok(Self { fd })
    }
}

impl FileDescriptor {
    /// Create a `FileDescriptor` with arguments similar to `libc::openat`.
    ///
    /// The descriptor is always close-on-exec (`O_CLOEXEC` is added to `flags`): a command a
    /// caller runs during a walk (`find -exec`) must not inherit the directories it holds.
    pub fn open_at(
        dir_file_descriptor: &FileDescriptor,
        file_name: &CStr,
        flags: i32,
    ) -> io::Result<Self> {
        unsafe {
            let fd = libc::openat(
                dir_file_descriptor.fd,
                file_name.as_ptr(),
                flags | libc::O_CLOEXEC,
            );
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

/// Take ownership of a descriptor opened elsewhere, e.g. a directory a caller reached by `openat`.
impl From<std::os::fd::OwnedFd> for FileDescriptor {
    fn from(fd: std::os::fd::OwnedFd) -> Self {
        Self {
            fd: std::os::fd::IntoRawFd::into_raw_fd(fd),
        }
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
    /// The name shown for this directory when it is not `filename`; see `Entry::shown_name`.
    shown_name: Option<Rc<[libc::c_char]>>,
    metadata: Metadata,
    /// Whether the directory entry is itself a symbolic link (one the walk followed).
    is_symlink: Option<bool>,
    path_depth: usize,
}

impl TreeNode {
    /// The entry for this directory itself, relative to `dir_fd`, its parent's descriptor.
    fn entry<'a>(
        &self,
        dir_fd: &'a FileDescriptor,
        path_stack: &'a [Rc<[libc::c_char]>],
    ) -> Entry<'a> {
        let mut entry = Entry::new(
            dir_fd,
            path_stack,
            self.filename.clone(),
            Some(self.metadata.clone()),
        )
        .with_shown_name(self.shown_name.clone());
        entry.is_symlink = self.is_symlink;
        entry
    }

    /// The name this directory contributes to the paths shown for its contents.
    fn shown_name(&self) -> Rc<[libc::c_char]> {
        self.shown_name
            .clone()
            .unwrap_or_else(|| self.filename.clone())
    }
}

/// An entry in the directory tree.
#[derive(Debug, Clone)]
pub struct Entry<'a> {
    dir_file_descriptor: &'a FileDescriptor,
    path_stack: &'a [Rc<[libc::c_char]>],
    filename: Rc<[libc::c_char]>,
    /// The name `path()` shows in place of `filename`. Only a starting point named with a trailing
    /// slash (or `/.`) has one: the operand as written, while `filename` is the name the walk
    /// acts on (the operand without the slash, or `.` in a symbolic link's target directory).
    shown_name: Option<Rc<[libc::c_char]>>,
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
            shown_name: None,
            metadata,
            is_symlink: None,
            read_link: None,
        }
    }

    fn with_shown_name(mut self, shown_name: Option<Rc<[libc::c_char]>>) -> Self {
        self.shown_name = shown_name;
        self
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
        let shown = self.shown_name.as_ref().unwrap_or(&self.filename);
        DisplayablePath(build_path(self.path_stack, shown))
    }

    /// Remove this entry by its name in the directory the walk holds open: `unlinkat(dir_fd(),
    /// file_name(), flags)`.
    ///
    /// A starting point named as a symbolic link with a trailing slash (`link/`) is reached as `.`
    /// in the directory the link resolved to, and no directory is removed by that name: that
    /// fails with `ENOTDIR`, as Linux's `rmdir("link/")` does, without the `EINVAL` that removing
    /// `.` would give.
    pub fn unlink(&self, flags: libc::c_int) -> io::Result<()> {
        if self.reached_through_symlink() {
            return Err(io::Error::from_raw_os_error(libc::ENOTDIR));
        }
        let ret = unsafe { libc::unlinkat(self.dir_fd(), self.file_name().as_ptr(), flags) };
        if ret == 0 {
            Ok(())
        } else {
            Err(io::Error::last_os_error())
        }
    }

    /// Whether this is a starting point named as a symbolic link with a trailing slash (`link/`),
    /// which the walk followed to the directory it names, whatever its options. Such an entry is
    /// `.` in that directory; a caller that removes what it walks refuses it rather than act on
    /// the link's target.
    pub fn reached_through_symlink(&self) -> bool {
        self.shown_name.is_some() && self.file_name() == c"."
    }

    /// Whether the calling process can write to the file this entry refers to.
    pub fn is_writable(&self) -> bool {
        is_writable_at(self.dir_fd(), self.file_name())
    }

    /// Check if this `Entry` is an empty directory.
    ///
    /// The directory is opened through the containing directory's descriptor with the same
    /// hardening as a descent: `O_DIRECTORY`, `O_NOFOLLOW` unless this entry is a symbolic link the
    /// walk followed, and a check that the opened directory is the one the walk stat'ed. An entry
    /// swapped for a symbolic link or for another directory since then is an error rather than
    /// an answer about some other directory.
    pub fn is_empty_dir(&self) -> io::Result<bool> {
        let followed = self.is_symlink == Some(true)
            && self.metadata.as_ref().is_some_and(|md| !md.is_symlink());
        let nofollow = if followed { 0 } else { libc::O_NOFOLLOW };
        let file_descriptor = FileDescriptor::open_at(
            self.dir_file_descriptor,
            self.file_name(),
            libc::O_RDONLY | libc::O_DIRECTORY | nofollow,
        )?;
        if let Some(md) = &self.metadata {
            if !fd_matches(&file_descriptor, md.0.st_dev, md.0.st_ino) {
                return Err(io::Error::from_raw_os_error(libc::ENOTDIR));
            }
        }
        lists_nothing(OwnedDir::new(file_descriptor)?)
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

/// Stat one entry, call `file_handler` on it and say whether to descend.
///
/// `shown_name` is set only for a starting point named with a trailing slash (see
/// `Entry::shown_name`). Such a name must resolve to a directory, so anything else is reported as
/// `ENOTDIR` before `file_handler` sees it.
#[allow(clippy::too_many_arguments)]
fn process_file<'a, F, H>(
    path_stack: &'a [Rc<[libc::c_char]>],
    dir_fd: &'a FileDescriptor,
    entry_filename: Rc<[libc::c_char]>,
    shown_name: Option<&Rc<[libc::c_char]>>,
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
                Entry::new(dir_fd, path_stack, entry_filename, None)
                    .with_shown_name(shown_name.cloned()),
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
                    Entry::new(dir_fd, path_stack, entry_filename.clone(), None)
                        .with_shown_name(shown_name.cloned()),
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
                            Entry::new(dir_fd, path_stack, entry_filename, None)
                                .with_shown_name(shown_name.cloned()),
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

    let must_be_dir = shown_name.is_some() && !entry_metadata.is_dir();
    let mut entry = Entry::new(dir_fd, path_stack, entry_filename, Some(entry_metadata))
        .with_shown_name(shown_name.cloned());
    entry.is_symlink = Some(is_symlink);
    entry.read_link = entry_readlink;

    if must_be_dir {
        entry.metadata = None;
        err_reporter(
            entry,
            Error::new(io::Error::from_raw_os_error(libc::ENOTDIR), ErrorKind::Stat),
        );
        return ProcessFileResult::NotProcessed;
    }

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

/// Report a `path` that holds a NUL byte, which no file can be named by.
///
/// Passed to the kernel, the C string would end at the NUL and name some other file -- the prefix
/// before it -- so such a path is refused outright, as an `Open` error with `InvalidInput`. Each
/// NUL is shown as `\0` in the reported name, so the diagnostic names the path that was given
/// rather than that prefix.
fn report_nul_in_path<H>(path: &Path, err_reporter: &mut H)
where
    H: FnMut(Entry<'_>, Error),
{
    let mut shown = Vec::new();
    for &b in path.as_os_str().as_bytes() {
        match b {
            0 => shown.extend_from_slice(b"\\0"),
            _ => shown.push(b),
        }
    }
    let shown = CString::new(shown).expect("every NUL was escaped");
    let cwd = FileDescriptor::cwd();
    err_reporter(
        Entry::new(&cwd, &[], cstring_to_rc(&shown), None),
        Error::new(
            io::Error::new(io::ErrorKind::InvalidInput, "path contains a NUL byte"),
            ErrorKind::Open,
        ),
    );
}

/// Open as much of `path`'s prefix as is needed for the rest to fit in `PATH_MAX`, returning the
/// last directory opened and the components left.
///
/// Every prefix component is opened `O_RDONLY | O_DIRECTORY | O_CLOEXEC` plus `open_flags` (a
/// walk's descent flags, so `O_NOFOLLOW` when it does not follow links). `O_DIRECTORY` refuses a
/// FIFO or device swapped in for a component before the open can block on it or open the
/// device. With `identities` (giving one recorded `(dev, ino)` per component of `path`, and
/// called only once a component is to be opened), each opened component must also be the very
/// directory the walk recorded there.
fn open_long_filename<'a, H>(
    mut starting_dir: FileDescriptor,
    path: &'a Path,
    mut path_stack: Option<&mut Vec<Rc<[libc::c_char]>>>,
    open_flags: libc::c_int,
    identities: Option<&dyn Fn() -> Vec<(libc::dev_t, libc::ino_t)>>,
    err_reporter: &mut H,
) -> io::Result<(FileDescriptor, std::path::Components<'a>)>
where
    H: FnMut(Entry<'_>, Error),
{
    let mut path_components = path.components();
    let mut opened = 0usize;
    let mut recorded: Option<Vec<(libc::dev_t, libc::ino_t)>> = None;

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

        // Only a prefix is opened here. The last component is the entry itself, which the
        // caller stats and opens with its own follow semantics: a symbolic link loop there
        // (ELOOP above) is an entry to report, not a directory to step into.
        if path_components.clone().nth(1).is_none() {
            break;
        }
        let Some(component) = path_components.next() else {
            break;
        };

        let filename_cstr = CString::new(component.as_os_str().as_bytes()).unwrap();
        let filename = cstring_to_rc(&filename_cstr);

        let opened_component = FileDescriptor::open_at(
            &starting_dir,
            unsafe { CStr::from_ptr(filename.as_ptr()) },
            libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC | open_flags,
        )
        .and_then(|fd| match identities {
            // Fail closed: a component with no recorded identity, or the wrong one, is refused.
            Some(gather) => match recorded.get_or_insert_with(gather).get(opened) {
                Some(&(dev, ino)) if fd_matches(&fd, dev, ino) => Ok(fd),
                _ => Err(io::Error::from_raw_os_error(libc::ENOTDIR)),
            },
            None => Ok(fd),
        });
        opened += 1;
        starting_dir = match opened_component {
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
    file_handler: F,
    postprocess_dir: G,
    mut err_reporter: H,
    opts: TraverseDirectoryOpts,
) -> bool
where
    P: AsRef<Path>,
    F: FnMut(Entry<'_>) -> Result<bool, ()>,
    G: FnMut(Entry<'_>, DirExit) -> Result<(), ()>,
    H: FnMut(Entry<'_>, Error),
{
    // Stack of the filename (relative to CWD).
    let mut path_stack: Vec<Rc<[libc::c_char]>> = Vec::new();

    if path.as_ref().as_os_str().as_bytes().contains(&0) {
        report_nul_in_path(path.as_ref(), &mut err_reporter);
        return false;
    }

    // The operand's own components are resolved as the user wrote them, symbolic links included.
    let (starting_dir, path_components) = match open_long_filename(
        FileDescriptor::cwd(),
        path.as_ref(),
        Some(&mut path_stack),
        0,
        None,
        &mut err_reporter,
    ) {
        Ok(pair) => pair,
        // Already reported through `err_reporter`.
        Err(_) => return false,
    };

    // `Components` drops trailing slashes and `.`s, so this is the operand's remainder without
    // them.
    let dir_filename_cstr = CString::new(path_components.as_path().as_os_str().as_bytes()).unwrap();
    let dir_filename = cstring_to_rc(&dir_filename_cstr);

    // A trailing slash, alone or as `/.`, after a final component that may be a symbolic link
    // changes what the operand names (POSIX pathname resolution): the link is followed, whatever
    // the walk's options, and the result must be a directory. `/`, `.` and `..` already name
    // directories.
    let operand = path.as_ref().as_os_str().as_bytes();
    let suffix = &operand[operand.len() - directory_suffix_len(operand)..];
    if suffix.is_empty()
        || !matches!(
            path.as_ref().components().next_back(),
            Some(Component::Normal(_))
        )
    {
        return walk_from(
            starting_dir,
            path_stack,
            dir_filename,
            None,
            file_handler,
            postprocess_dir,
            err_reporter,
            opts,
        );
    }
    walk_slash_operand(
        starting_dir,
        path_stack,
        dir_filename,
        suffix,
        file_handler,
        postprocess_dir,
        err_reporter,
        opts,
    )
}

/// The length of the run of `/` and `/.` at the end of `path`: what follows its last component and
/// makes it name a directory.
fn directory_suffix_len(path: &[u8]) -> usize {
    let mut rest = path;
    while let Some(shorter) = rest.strip_suffix(b"/").or_else(|| rest.strip_suffix(b"/.")) {
        rest = shorter;
    }
    path.len() - rest.len()
}

/// `traverse_directory` for an operand whose last component is followed by `suffix`, a run of `/`
/// and `/.`: `dir_filename` (the operand without it) in `starting_dir` must be a directory, and a
/// symbolic link there is followed.
///
/// The starting point is shown as the operand was written. A name that is not a symbolic link is
/// walked as usual, with `process_file` refusing anything but a directory. A symbolic link is
/// resolved once, by opening it with `O_DIRECTORY` (the kernel follows it and refuses a
/// non-directory), and the walk starts at `.` in the directory that open pinned. Every call a
/// caller makes on the starting entry is then relative to that descriptor and acts on that
/// directory: a caller's `AT_SYMLINK_NOFOLLOW` call cannot reach the link instead, as it would on
/// a system that ignores a trailing slash under `AT_SYMLINK_NOFOLLOW` (macOS), and a link replaced
/// after the open cannot redirect it.
#[allow(clippy::too_many_arguments)]
fn walk_slash_operand<F, G, H>(
    starting_dir: FileDescriptor,
    path_stack: Vec<Rc<[libc::c_char]>>,
    dir_filename: Rc<[libc::c_char]>,
    suffix: &[u8],
    file_handler: F,
    postprocess_dir: G,
    mut err_reporter: H,
    opts: TraverseDirectoryOpts,
) -> bool
where
    F: FnMut(Entry<'_>) -> Result<bool, ()>,
    G: FnMut(Entry<'_>, DirExit) -> Result<(), ()>,
    H: FnMut(Entry<'_>, Error),
{
    let name = unsafe { CStr::from_ptr(dir_filename.as_ptr()) };
    let mut shown = name.to_bytes().to_vec();
    shown.extend_from_slice(suffix);
    let shown_name = cstring_to_rc(&CString::new(shown).expect("taken from a C string"));

    // Only chooses how the name is reached; it decides nothing about which object is acted on.
    // Either way the object comes from a single later lookup that is checked to be a directory.
    let is_symlink = Metadata::new(starting_dir.fd, name, false).is_ok_and(|md| md.is_symlink());
    if !is_symlink {
        return walk_from(
            starting_dir,
            path_stack,
            dir_filename,
            Some(shown_name),
            file_handler,
            postprocess_dir,
            err_reporter,
            opts,
        );
    }

    let target = FileDescriptor::open_at(
        &starting_dir,
        name,
        libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC,
    );
    match target {
        Ok(target) => walk_from(
            target,
            path_stack,
            cstring_to_rc(c"."),
            Some(shown_name),
            file_handler,
            postprocess_dir,
            err_reporter,
            opts,
        ),
        Err(e) => {
            err_reporter(
                Entry::new(&starting_dir, &path_stack, dir_filename.clone(), None)
                    .with_shown_name(Some(shown_name)),
                Error::new(e, ErrorKind::Stat),
            );
            false
        }
    }
}

/// Walk through the directory tree rooted at `name` in the directory open on `dir`.
///
/// This is `traverse_directory` for a starting point already pinned by a descriptor: no part of
/// the path to it is resolved again, so renaming or replacing one of its ancestors after `dir`
/// was opened cannot redirect the walk. `name` is a single component (it may carry trailing
/// slashes), looked up in `dir` exactly as `traverse_directory` looks up an operand's last
/// component: a symbolic link named with a trailing slash is walked as `.` in the directory it
/// names (`Entry::reached_through_symlink`). `postprocess_dir` for the starting point itself
/// receives `dir` as the containing directory (or, for such a link, the directory it names).
///
/// `display_parent` is only shown: each entry's `path()` is `display_parent` joined with the
/// entry's path from `name`. It is never resolved.
///
/// `dir` is duplicated for the walk and stays open, and owned, by the caller.
pub fn traverse_directory_at<F, G, H>(
    dir: &FileDescriptor,
    name: &CStr,
    display_parent: &Path,
    file_handler: F,
    postprocess_dir: G,
    mut err_reporter: H,
    opts: TraverseDirectoryOpts,
) -> bool
where
    F: FnMut(Entry<'_>) -> Result<bool, ()>,
    G: FnMut(Entry<'_>, DirExit) -> Result<(), ()>,
    H: FnMut(Entry<'_>, Error),
{
    let display_bytes = display_parent.as_os_str().as_bytes();
    let path_stack: Vec<Rc<[libc::c_char]>> = match CString::new(display_bytes) {
        Ok(_) if display_bytes.is_empty() => Vec::new(),
        Ok(shown) => vec![cstring_to_rc(&shown)],
        Err(_) => {
            report_nul_in_path(display_parent, &mut err_reporter);
            return false;
        }
    };
    let starting_dir = match dir.try_clone() {
        Ok(fd) => fd,
        Err(e) => {
            err_reporter(
                Entry::new(dir, &path_stack, cstring_to_rc(name), None),
                Error::new(e, ErrorKind::Open),
            );
            return false;
        }
    };

    // A trailing slash after a name that may be a symbolic link: as in `traverse_directory`.
    let name_bytes = name.to_bytes();
    let (bare, suffix) = name_bytes.split_at(name_bytes.len() - directory_suffix_len(name_bytes));
    if !suffix.is_empty() && !matches!(bare, b"" | b"." | b"..") {
        let bare = CString::new(bare).expect("taken from a C string");
        return walk_slash_operand(
            starting_dir,
            path_stack,
            cstring_to_rc(&bare),
            suffix,
            file_handler,
            postprocess_dir,
            err_reporter,
            opts,
        );
    }
    walk_from(
        starting_dir,
        path_stack,
        cstring_to_rc(name),
        None,
        file_handler,
        postprocess_dir,
        err_reporter,
        opts,
    )
}

/// The walk shared by `traverse_directory` and `traverse_directory_at`: `dir_filename` in
/// `starting_dir`, whose displayed path is `path_stack`. With `shown_name` (a starting point named
/// with a trailing slash), the starting point is shown by that name and must be a directory.
#[allow(clippy::too_many_arguments)]
fn walk_from<F, G, H>(
    starting_dir: FileDescriptor,
    mut path_stack: Vec<Rc<[libc::c_char]>>,
    dir_filename: Rc<[libc::c_char]>,
    shown_name: Option<Rc<[libc::c_char]>>,
    mut file_handler: F,
    mut postprocess_dir: G,
    mut err_reporter: H,
    opts: TraverseDirectoryOpts,
) -> bool
where
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

    // Stack of the directories to process. `path_stack` is updated in sync with it.
    let mut stack: Vec<TreeNode> = Vec::new();

    // Used in `ls`
    let mut subdirs: Vec<TreeNode> = Vec::new();

    {
        match process_file(
            &path_stack,
            &starting_dir,
            dir_filename.clone(),
            shown_name.as_ref(),
            follow_symlinks_on_args || follow_symlinks,
            false,
            &mut file_handler,
            &mut err_reporter,
        ) {
            ProcessFileResult::ProcessedDirectory(entry) => {
                // `O_DIRECTORY` rejects a non-directory. `O_NOFOLLOW` is added unless the walk
                // follows a symlinked starting point (-H/-L), so a directory swapped for a
                // symlink after the stat above is not followed; the (dev, ino) check then refuses
                // a swap for a different directory, as on every descent. (An operand that named
                // a symlink with a trailing slash is here as `.` in the directory it resolved to.)
                let root_flags = if follow_symlinks_on_args || follow_symlinks {
                    libc::O_DIRECTORY
                } else {
                    libc::O_DIRECTORY | libc::O_NOFOLLOW
                };
                let (want_dev, want_ino) = {
                    let md = entry.metadata.as_ref().unwrap();
                    (md.0.st_dev, md.0.st_ino)
                };
                let opened = OwnedDir::open_at(&starting_dir, dir_filename.as_ptr(), root_flags)
                    .and_then(|dir| {
                        if fd_matches(dir.file_descriptor(), want_dev, want_ino) {
                            Ok(dir)
                        } else {
                            Err(Error::new(
                                io::Error::from_raw_os_error(libc::ENOTDIR),
                                ErrorKind::OpenDir,
                            ))
                        }
                    });
                match opened {
                    Ok(new_dir) => {
                        let node = TreeNode {
                            dir: HybridDir::Owned(new_dir),
                            filename: dir_filename,
                            shown_name,
                            is_symlink: entry.is_symlink,
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
        path_stack.push(current.shown_name());

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
                                current.entry(prev_dir, parent_path_stack),
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
                        None,
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
                                        ErrorKind::Cycle,
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
                                            (want_dev, want_ino),
                                            None,
                                        );
                                        TreeNode {
                                            dir: HybridDir::Deferred(slow_dir),
                                            filename: entry_filename,
                                            shown_name: None,
                                            is_symlink: entry.is_symlink,
                                            metadata: entry.metadata.unwrap(),
                                            path_depth,
                                        }
                                    }
                                    HybridDir::Deferred(current_dir) => {
                                        let slow_dir = DeferredDir::new(
                                            current_dir.parent().clone(),
                                            build_path(&path_stack, &entry_filename),
                                            descent_flags,
                                            (want_dev, want_ino),
                                            Some(current_dir.lineage()),
                                        );
                                        TreeNode {
                                            dir: HybridDir::Deferred(slow_dir),
                                            filename: entry_filename,
                                            shown_name: None,
                                            is_symlink: entry.is_symlink,
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
                                        if !fd_matches(
                                            new_dir.file_descriptor(),
                                            want_dev,
                                            want_ino,
                                        ) {
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
                                            shown_name: None,
                                            is_symlink: entry.is_symlink,
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
                current.entry(&starting_dir, &path_stack[..path_depth - 1]),
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
                if postprocess_dir(current.entry(prev_dir, &path_stack), dir_exit).is_err() {
                    success = false;
                    // Don't `continue` here, falldown below
                }
            }
            Err(e) => {
                err_reporter(
                    current.entry(&starting_dir, &path_stack),
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

/// Whether `dir` lists nothing but `.` and `..`.
fn lists_nothing(dir: OwnedDir) -> io::Result<bool> {
    for entry in dir.iter() {
        if !entry?.is_dot_or_double_dot() {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Whether the directory open on `dir_fd` is empty.
///
/// It is read through a new open of `.` relative to `dir_fd` -- the same directory, which no
/// rename can swap -- so `dir_fd`'s own read position is left alone.
pub fn is_empty_dir_fd(dir_fd: RawFd) -> io::Result<bool> {
    let fd = unsafe {
        libc::openat(
            dir_fd,
            c".".as_ptr(),
            libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC,
        )
    };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    lists_nothing(OwnedDir::new(FileDescriptor { fd })?)
}

/// Whether the open descriptor `fd` refers to the file identified by `(dev, ino)`, the identity a
/// walk recorded when it stat'ed the entry. This is the post-open re-verification that turns a
/// concurrent swap of the entry for a different file into a refusal instead of a redirection.
fn fd_matches(fd: &FileDescriptor, dev: libc::dev_t, ino: libc::ino_t) -> bool {
    let mut sb = MaybeUninit::<libc::stat>::uninit();
    if unsafe { libc::fstat(fd.as_raw_fd(), sb.as_mut_ptr()) } != 0 {
        return false;
    }
    let sb = unsafe { sb.assume_init() };
    sb.st_dev == dev && sb.st_ino == ino
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
