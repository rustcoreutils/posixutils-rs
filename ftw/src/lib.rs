// #![feature(coverage_attribute)]

mod dir;

mod small_c_string;

use crate::dir::{DeferredDir, HybridDir, OwnedDir};
use crate::small_c_string::run_path_with_cstr;

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

/// Type of error to be handled by the `err_reporter` of `traverse_directory`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    Open,
    OpenDir,
    ReadDir,
    Stat,
    ReadLink,
    DirNotSearchable,
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

impl Clone for FileDescriptor {
    fn clone(&self) -> Self {
        if self.fd != libc::AT_FDCWD {
            Self {
                fd: unsafe { libc::dup(self.fd) },
            }
        } else {
            Self { fd: libc::AT_FDCWD }
        }
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

pub struct FilesystemStatistics(libc::statfs);

impl fmt::Debug for FilesystemStatistics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("FilesystemStatistics")
    }
}

impl FilesystemStatistics {
    /// Get filesystem statistics
    pub fn new<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        run_path_with_cstr(path.as_ref(), &|p| Self::new_cstr(p))
    }

    /// Get filesystem statistics
    pub fn new_cstr(path: &CStr) -> io::Result<Self> {
        let mut buf = unsafe { std::mem::zeroed() };
        let ret = unsafe { libc::statfs(path.as_ptr(), &mut buf) };
        if ret != 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(Self(buf))
    }

    #[must_use]
    pub fn bsize(&self) -> u64 {
        self.0.f_bsize as u64
    }

    #[must_use]
    pub fn blocks(&self) -> u64 {
        self.0.f_blocks
    }

    #[must_use]
    pub fn bavail(&self) -> u64 {
        self.0.f_bavail
    }

    #[must_use]
    pub fn bfree(&self) -> u64 {
        self.0.f_bfree
    }
}

impl From<&libc::statfs> for FilesystemStatistics {
    fn from(value: &libc::statfs) -> Self {
        Self(*value)
    }
}

/// Given a path, queries the file system to get information about a file,
/// directory, etc.
///
/// This function will traverse symbolic links to query information about the
/// destination file.
///
/// This is analogous to [`std::fs::metadata`].
pub fn metadata<P: AsRef<Path>>(path: P) -> io::Result<Metadata> {
    run_path_with_cstr(path.as_ref(), &|p| Metadata::new(libc::AT_FDCWD, p, true))
}

/// Given a path, queries the file system to get information about a file,
/// directory, etc.
///
/// This function will traverse symbolic links to query information about the
/// destination file.
///
/// This is analogous to [`std::fs::metadata`].
pub fn metadata_cstr(path: &CStr) -> io::Result<Metadata> {
    Metadata::new(libc::AT_FDCWD, path, true)
}

/// Queries the metadata about a file without following symlinks.
///
/// This is analogous to [`std::fs::symlink_metadata`].
pub fn symlink_metadata<P: AsRef<Path>>(path: P) -> io::Result<Metadata> {
    run_path_with_cstr(path.as_ref(), &|p| Metadata::new(libc::AT_FDCWD, p, false))
}

/// Queries the metadata about a file without following symlinks.
///
/// This is analogous to [`std::fs::symlink_metadata`].
pub fn symlink_metadata_cstr(path: &CStr) -> io::Result<Metadata> {
    Metadata::new(libc::AT_FDCWD, path, false)
}

/// Metadata information about a file.
///
/// This is analogous to [`std::fs::Metadata`].
#[derive(Clone)]
pub struct Metadata(libc::stat);

impl fmt::Debug for Metadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Metadata")
    }
}

impl Metadata {
    /// Create a new [`Metadata`].
    ///
    /// `dirfd` could be the special value [`libc::AT_FDCWD`] to query the metadata of a file at the
    /// process' current working directory.
    pub fn new(dirfd: libc::c_int, pathname: &CStr, follow_symlinks: bool) -> io::Result<Metadata> {
        let mut buf = MaybeUninit::uninit();
        let mut flags = 0;
        if !follow_symlinks {
            flags |= libc::AT_SYMLINK_NOFOLLOW;
        }
        let ret = unsafe { libc::fstatat(dirfd, pathname.as_ptr(), buf.as_mut_ptr(), flags) };
        if ret != 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(Metadata(unsafe { buf.assume_init() }))
    }

    /// Returns the file type for this metadata.
    ///
    /// This is analogous to [`std::fs::Metadata::file_type`].
    pub fn file_type(&self) -> FileType {
        FileType::new(self.0.st_mode)
    }

    /// Returns `true` if this metadata is for a directory. The
    /// result is mutually exclusive to the result of
    /// [`Metadata::is_file`], and will be false for symlink metadata
    /// obtained from [`symlink_metadata`].
    ///
    /// This is analogous to [`std::fs::Metadata::is_dir`].
    #[must_use]
    pub fn is_dir(&self) -> bool {
        self.file_type().is_dir()
    }

    /// Returns `true` if this metadata is for a regular file. The
    /// result is mutually exclusive to the result of
    /// [`Metadata::is_dir`], and will be false for symlink metadata
    /// obtained from [`symlink_metadata`].
    ///
    /// This is analogous to [`std::fs::Metadata::is_file`].
    #[must_use]
    pub fn is_file(&self) -> bool {
        self.file_type().is_file()
    }

    /// Returns `true` if this metadata is for a symbolic link.
    ///
    /// This is analogous to [`std::fs::Metadata::is_symlink`].
    #[must_use]
    pub fn is_symlink(&self) -> bool {
        self.file_type().is_symlink()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the size of the file, in bytes, this metadata is for.
    ///
    /// This is analogous to [`std::fs::Metadata::len`].
    #[must_use]
    pub fn len(&self) -> u64 {
        self.0.st_size as _
    }

    /// Returns the permissions of the file this metadata is for.
    ///
    /// This is analogous to [`std::fs::Metadata::permissions`].
    pub fn permissions(&self) -> Permissions {
        Permissions(self.0.st_mode)
    }

    // These are "effective" IDs and not "real" to allow for things like sudo
    #[must_use]
    fn get_uid_and_gid(&self) -> (libc::uid_t, libc::gid_t) {
        let uid = unsafe { libc::geteuid() };
        let gid = unsafe { libc::getegid() };
        (uid, gid)
    }

    /// Check if the current process has write permission for the file that this `Metadata` refers
    /// to.
    #[must_use]
    pub fn is_writable(&self) -> bool {
        let (uid, gid) = self.get_uid_and_gid();

        let same_user = self.0.st_uid == uid;
        let same_group = self.0.st_gid == gid;

        if same_user {
            self.0.st_mode & libc::S_IWUSR != 0
        } else if same_group {
            self.0.st_mode & libc::S_IWGRP != 0
        } else {
            self.0.st_mode & libc::S_IWOTH != 0
        }
    }

    /// Check if the current process has execute or search permission for the file that this
    /// `Metadata` refers to.
    #[must_use]
    pub fn is_executable(&self) -> bool {
        let (uid, gid) = self.get_uid_and_gid();

        let same_user = self.0.st_uid == uid;
        let same_group = self.0.st_gid == gid;

        if same_user {
            self.0.st_mode & libc::S_IXUSR != 0
        } else if same_group {
            self.0.st_mode & libc::S_IXGRP != 0
        } else {
            self.0.st_mode & libc::S_IXOTH != 0
        }
    }
}

impl unix::fs::MetadataExt for Metadata {
    /// Returns the ID of the device containing the file.
    #[must_use]
    fn dev(&self) -> u64 {
        self.0.st_dev as _
    }

    /// Returns the inode number.
    #[must_use]
    fn ino(&self) -> u64 {
        self.0.st_ino
    }

    /// Returns the rights applied to this file.
    #[must_use]
    fn mode(&self) -> u32 {
        self.0.st_mode as _
    }

    /// Returns the number of hard links pointing to this file.
    #[must_use]
    fn nlink(&self) -> u64 {
        self.0.st_nlink as _
    }

    /// Returns the user ID of the owner of this file.
    #[must_use]
    fn uid(&self) -> u32 {
        self.0.st_uid
    }

    /// Returns the group ID of the owner of this file.
    #[must_use]
    fn gid(&self) -> u32 {
        self.0.st_gid
    }

    /// Returns the device ID of this file (if it is a special one).
    #[must_use]
    fn rdev(&self) -> u64 {
        self.0.st_rdev as _
    }

    /// Returns the total size of this file in bytes.
    #[must_use]
    fn size(&self) -> u64 {
        self.0.st_size as _
    }

    /// Returns the last access time of the file, in seconds since Unix Epoch.
    #[must_use]
    fn atime(&self) -> i64 {
        self.0.st_atime
    }

    /// Returns the last access time of the file, in nanoseconds since [`atime`].
    #[must_use]
    fn atime_nsec(&self) -> i64 {
        self.0.st_atime_nsec
    }

    /// Returns the last modification time of the file, in seconds since Unix Epoch.
    #[must_use]
    fn mtime(&self) -> i64 {
        self.0.st_mtime
    }

    /// Returns the last modification time of the file, in nanoseconds since [`mtime`].
    #[must_use]
    fn mtime_nsec(&self) -> i64 {
        self.0.st_mtime_nsec
    }

    /// Returns the last status change time of the file, in seconds since Unix Epoch.
    #[must_use]
    fn ctime(&self) -> i64 {
        self.0.st_ctime
    }

    /// Returns the last status change time of the file, in nanoseconds since [`ctime`].
    #[must_use]
    fn ctime_nsec(&self) -> i64 {
        self.0.st_ctime_nsec
    }

    /// Returns the block size for filesystem I/O.
    #[must_use]
    fn blksize(&self) -> u64 {
        self.0.st_blksize as _
    }

    /// Returns the number of blocks allocated to the file, in 512-byte units.
    ///
    /// Please note that this may be smaller than `st_size / 512` when the file has holes.
    #[must_use]
    fn blocks(&self) -> u64 {
        self.0.st_blocks as _
    }
}

/// File type of an entry. Returned by [`Metadata::file_type`].
///
/// This is analogous to [`std::fs::FileType`].
#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileType {
    Socket,
    SymbolicLink,
    RegularFile,
    BlockDevice,
    Directory,
    CharacterDevice,
    Fifo,
}

impl FileType {
    fn new(mode: libc::mode_t) -> Self {
        match mode & libc::S_IFMT {
            libc::S_IFSOCK => FileType::Socket,
            libc::S_IFLNK => FileType::SymbolicLink,
            libc::S_IFREG => FileType::RegularFile,
            libc::S_IFBLK => FileType::BlockDevice,
            libc::S_IFDIR => FileType::Directory,
            libc::S_IFCHR => FileType::CharacterDevice,
            libc::S_IFIFO => FileType::Fifo,
            _ => unreachable!(),
        }
    }

    /// Tests whether this file type represents a directory. The
    /// result is mutually exclusive to the results of
    /// [`is_file`] and [`is_symlink`]; only zero or one of these
    /// tests may pass.
    ///
    /// This is analogous to [`std::fs::FileType::is_dir`].
    #[must_use]
    pub fn is_dir(&self) -> bool {
        *self == FileType::Directory
    }

    /// Tests whether this file type represents a regular file.
    /// The result is mutually exclusive to the results of
    /// [`is_dir`] and [`is_symlink`]; only zero or one of these
    /// tests may pass.
    ///   
    /// This is analogous to [`std::fs::FileType::is_file`].
    #[must_use]
    pub fn is_file(&self) -> bool {
        *self == FileType::RegularFile
    }

    /// Tests whether this file type represents a symbolic link.
    /// The result is mutually exclusive to the results of
    /// [`is_dir`] and [`is_file`]; only zero or one of these
    /// tests may pass.
    ///
    /// This is analogous to [`std::fs::FileType::is_symlink`].
    #[must_use]
    pub fn is_symlink(&self) -> bool {
        *self == FileType::SymbolicLink
    }
}

impl unix::fs::FileTypeExt for FileType {
    /// Returns `true` if this file type is a block device.
    ///   
    /// This is analogous to [`std::fs::FileType::is_block_device`].
    #[must_use]
    fn is_block_device(&self) -> bool {
        *self == FileType::BlockDevice
    }

    /// Returns `true` if this file type is a char device.
    ///  
    /// This is analogous to [`std::fs::FileType::is_char_device`].
    #[must_use]
    fn is_char_device(&self) -> bool {
        *self == FileType::CharacterDevice
    }

    /// Returns `true` if this file type is a fifo.
    ///
    /// This is analogous to [`std::fs::FileType::is_fifo`].
    #[must_use]
    fn is_fifo(&self) -> bool {
        *self == FileType::Fifo
    }

    /// Returns `true` if this file type is a socket.
    ///
    /// This is analogous to [`std::fs::FileType::is_socket`].
    #[must_use]
    fn is_socket(&self) -> bool {
        *self == FileType::Socket
    }
}

/// Representation of the various permissions on a file.
///
/// Returned by [`Metadata::permissions`].
#[must_use]
pub struct Permissions(libc::mode_t);

impl Permissions {
    /// Read permission bit for the owner of the file.
    #[must_use]
    pub fn is_read_owner(&self) -> bool {
        self.0 & libc::S_IRUSR != 0
    }

    /// Write permission bit for the owner of the file.
    #[must_use]
    pub fn is_write_owner(&self) -> bool {
        self.0 & libc::S_IWUSR != 0
    }

    /// Execute (for ordinary files) or search (for directories)
    /// permission bit for the owner of the file.
    #[must_use]
    pub fn is_executable_owner(&self) -> bool {
        self.0 & libc::S_IXUSR != 0
    }

    /// Read permission bit for the group owner of the file.
    #[must_use]
    pub fn is_read_group(&self) -> bool {
        self.0 & libc::S_IRGRP != 0
    }

    /// Write permission bit for the group owner of the file.
    #[must_use]
    pub fn is_write_group(&self) -> bool {
        self.0 & libc::S_IWGRP != 0
    }

    /// Execute or search permission bit for the group owner of the file.
    #[must_use]
    pub fn is_executable_group(&self) -> bool {
        self.0 & libc::S_IXGRP != 0
    }

    /// Read permission bit for other users.
    #[must_use]
    pub fn is_read_other(&self) -> bool {
        self.0 & libc::S_IROTH != 0
    }

    /// Write permission bit for other users.
    #[must_use]
    pub fn is_write_other(&self) -> bool {
        self.0 & libc::S_IWOTH != 0
    }

    /// Execute or search permission bit for other users.
    #[must_use]
    pub fn is_executable_other(&self) -> bool {
        self.0 & libc::S_IXOTH != 0
    }

    /// This is the set-user-ID on execute bit.
    #[must_use]
    pub fn is_set_user_id(&self) -> bool {
        self.0 & libc::S_ISUID != 0
    }

    /// This is the set-group-ID on execute bit.
    #[must_use]
    pub fn is_set_group_id(&self) -> bool {
        self.0 & libc::S_ISGID != 0
    }

    /// This is the sticky bit.
    #[must_use]
    pub fn is_sticky(&self) -> bool {
        self.0 & libc::S_ISVTX != 0
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

    // If `follow_symlinks` is enabled, read the location of the symlink and use the metadata of the
    // pointed file.
    let (entry_readlink, entry_metadata) = if is_symlink && follow_symlinks {
        let read_link = match read_link_at(dir_fd.fd, entry_filename.as_ptr()) {
            Ok(p) => p,
            Err(e) => {
                err_reporter(
                    Entry::new(dir_fd, path_stack, entry_filename, None),
                    Error::new(e, ErrorKind::ReadLink),
                );
                return ProcessFileResult::NotProcessed;
            }
        };

        match Metadata::new(
            dir_fd.fd,
            unsafe { CStr::from_ptr(entry_filename.as_ptr()) },
            true,
        ) {
            Ok(md) => (Some(read_link), md),
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound {
                    // Don't treat dangling links as an error, use the metadata of the original
                    (Some(read_link), entry_symlink_metadata)
                } else {
                    err_reporter(
                        Entry::new(dir_fd, path_stack, entry_filename, None),
                        Error::new(e, ErrorKind::Stat),
                    );
                    return ProcessFileResult::NotProcessed;
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
            let entry_metadata = entry.metadata.as_ref().unwrap();
            if entry_metadata.is_dir() {
                // Is the directory searchable?
                if entry_metadata.is_executable() {
                    ProcessFileResult::ProcessedDirectory(entry)
                } else {
                    // "Permission denied" error. `io::ErrorKind::PermissionDenied` uses
                    // lowercase for "permission" in the error message so don't use that here.
                    let e = io::Error::from_raw_os_error(libc::EACCES);
                    err_reporter(entry, Error::new(e, ErrorKind::DirNotSearchable));
                    ProcessFileResult::NotProcessed
                }
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
) -> Option<(FileDescriptor, std::path::Components<'a>)>
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
                if let Some(path_stack) = &path_stack {
                    err_reporter(
                        Entry::new(&starting_dir, path_stack, filename, None),
                        Error::new(e, ErrorKind::Open),
                    );
                }
                return None;
            }
        };

        if let Some(path_stack) = &mut path_stack {
            path_stack.push(filename);
        }
    }

    Some((starting_dir, path_components))
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
///     return `false` but will otherwise allow processing the file inside `file_handler` like a
///     normal entry.
///
/// * `file_handler` - Called for each entry in the tree. If the current entry is a directory, its
///     contents will be skipped if `file_handler` returns `false`. The return value of
///     `file_handler` is ignored when the entry is a file.
///
/// * `postprocess_dir` - Called when `traverse_directory` is exiting a directory.
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
    G: FnMut(Entry<'_>) -> Result<(), ()>,
    H: FnMut(Entry<'_>, Error),
{
    let TraverseDirectoryOpts {
        follow_symlinks_on_args,
        follow_symlinks,
        include_dot_and_double_dot,
        list_contents_first,
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
        Some(pair) => pair,
        None => return false,
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
                match OwnedDir::open_at(&starting_dir, dir_filename.as_ptr()) {
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
                        err_reporter(entry, error);
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

    // Max allowable open file descriptors
    let fd_rlim_cur = unsafe {
        let mut rlim = MaybeUninit::uninit();
        let ret = libc::getrlimit(libc::RLIMIT_NOFILE, rlim.as_mut_ptr());
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }
        let rlim = rlim.assume_init();

        rlim.rlim_cur
    };

    // Subtract a few to allow for some FD bookkeeping
    let fd_threshold: usize = fd_rlim_cur as usize - 7;

    // Depth first traversal main loop
    'outer: while let Some(current) = stack.last() {
        let dir = &current.dir;
        let dir_fd = match dir {
            HybridDir::Owned(dir) => dir.file_descriptor(),
            HybridDir::Deferred(dir) => &dir.open_file_descriptor(),
        };

        // Resize `path_stack` to the appropriate depth.
        debug_assert!(path_stack.len() >= current.path_depth);
        path_stack.truncate(current.path_depth);

        // Push the directory's filename. The contents' filename will be concatenated to the
        // directory's filename.
        path_stack.push(current.filename.clone());

        let path_depth = path_stack.len();

        {
            let mut dir_iter = dir.iter();

            // Read the current directory
            while let Some(entry_or_err) = dir_iter.next() {
                let entry = match entry_or_err {
                    Ok(entry) => entry,

                    // Errors in reading the entry usually occurs due to lack of permissions
                    Err(e) => {
                        let second_last_index = stack.len().checked_sub(2);
                        let prev_dir = match second_last_index {
                            Some(index) => match &stack.get(index).unwrap().dir {
                                HybridDir::Owned(dir) => dir.file_descriptor(),
                                HybridDir::Deferred(dir) => &dir.open_file_descriptor(),
                            },
                            None => &starting_dir,
                        };
                        err_reporter(
                            Entry::new(
                                prev_dir,
                                // Need to report the filename of the directory itself so exclude
                                // the last one
                                &path_stack[..(path_stack.len() - 2)],
                                current.filename.clone(),
                                Some(current.metadata.clone()),
                            ),
                            Error::new(e, ErrorKind::ReadDir),
                        );

                        success = false;
                        continue;
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
                        let used_fds = stack.len() + subdirs.len();
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
                        let node = if conserve_fds {
                            match dir {
                                HybridDir::Owned(current_dir) => {
                                    let path = build_path(&path_stack, &entry_filename);
                                    let slow_dir = DeferredDir::new(
                                        Rc::new((
                                            current_dir.file_descriptor().clone(),
                                            path.parent().unwrap().to_path_buf(),
                                        )),
                                        path,
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
                            match OwnedDir::open_at(dir_fd, entry_filename.as_ptr()) {
                                Ok(new_dir) => TreeNode {
                                    dir: HybridDir::Owned(new_dir),
                                    filename: entry_filename,
                                    metadata: entry.metadata.unwrap(),
                                    path_depth,
                                },
                                Err(error) => {
                                    err_reporter(entry, error);
                                    success = false;
                                    continue;
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

        let second_last_index = stack.len().checked_sub(2);
        let prev_dir = match second_last_index {
            Some(index) => match &stack.get(index).unwrap().dir {
                HybridDir::Owned(dir) => dir.file_descriptor(),
                HybridDir::Deferred(dir) => &dir.open_file_descriptor(),
            },
            None => &starting_dir,
        };
        if postprocess_dir(Entry::new(
            prev_dir,
            &path_stack,
            current.filename.clone(),
            Some(current.metadata.clone()),
        ))
        .is_err()
        {
            success = false;
            // Don't `continue` here, falldown below
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
    let mut buf = vec![0; libc::PATH_MAX as usize];

    let ret = unsafe { libc::readlinkat(dirfd, filename, buf.as_mut_ptr(), buf.len()) };
    if ret < 0 {
        return Err(io::Error::last_os_error());
    }

    let num_bytes = ret as usize;
    buf.shrink_to(num_bytes);
    Ok(Rc::from(buf.into_boxed_slice()))
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

    #[test]
    fn test_symlink_metadata_not_exist() {
        let file = "tests/not_exist";
        let meta = symlink_metadata(file);
        let meta = meta.unwrap_err();
        assert_eq!(meta.kind(), std::io::ErrorKind::NotFound);
    }

    #[test]
    fn test_symlink_metadata_is_dir() {
        let file = "tests";
        let meta = symlink_metadata(file).unwrap();
        assert!(meta.is_dir());
        assert_eq!(meta.file_type(), FileType::Directory);
    }

    #[test]
    fn test_symlink_metadata_cstr_is_dir() {
        let file = c"tests";
        let meta = symlink_metadata_cstr(file).unwrap();
        assert!(meta.is_dir());
        assert_eq!(meta.file_type(), FileType::Directory);
    }

    #[test]
    fn test_symlink_metadata_is_file() {
        let file = "tests/empty_file.txt";
        let meta = symlink_metadata(file).unwrap();
        assert!(meta.is_file());
        assert_eq!(meta.file_type(), FileType::RegularFile);
    }

    #[test]
    fn test_symlink_metadata_is_empty() {
        let file = "tests/empty_file.txt";
        let meta = symlink_metadata(file).unwrap();
        assert!(meta.is_empty());
    }

    #[test]
    fn test_symlink_metadata_is_char_device() {
        use unix::fs::FileTypeExt;

        let file = "/dev/null";
        let file_type = symlink_metadata(file).unwrap().file_type();
        assert!(file_type.is_char_device());
        assert_eq!(file_type, FileType::CharacterDevice);
    }
}
