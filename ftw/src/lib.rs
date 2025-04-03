mod dir;

use dir::{DeferredDir, HybridDir, OwnedDir};
use std::ffi::{CStr, CString, OsStr};
use std::mem::MaybeUninit;
use std::ops::Deref;
use std::os::fd::{AsRawFd, RawFd};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::{self};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::{fmt, io};

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
            _ => unreachable!(),
        }
    }

    // These are "effective" IDs and not "real" to allow for things like sudo
    fn get_uid_and_gid(&self) -> (libc::uid_t, libc::gid_t) {
        let uid = unsafe { libc::geteuid() };
        let gid = unsafe { libc::getegid() };
        (uid, gid)
    }

    /// Check if the current process has write permission for the file that this `Metadata` refers
    /// to.
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
