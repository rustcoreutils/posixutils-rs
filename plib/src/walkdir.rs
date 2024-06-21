use std::{
    ffi::{CStr, CString, OsStr},
    fmt::Debug,
    io,
    marker::PhantomData,
    mem::MaybeUninit,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
    ptr::NonNull,
    rc::Rc,
};

pub enum ErrorKind {
    OpenDir,
    ReadDir,
    Stat,
    ReadLink,
}

pub struct Error {
    pub inner: io::Error,
    pub kind: ErrorKind,
}

impl Error {
    fn new(e: io::Error, kind: ErrorKind) -> Self {
        Self { inner: e, kind }
    }
}

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
        Self {
            fd: unsafe { libc::dup(self.fd) },
        }
    }
}

impl FileDescriptor {
    fn open_at(dir_file_descriptor: &FileDescriptor, filename: *const i8, flags: i32) -> Self {
        unsafe {
            let fd = libc::openat(dir_file_descriptor.fd, filename, flags);
            Self { fd }
        }
    }

    fn cwd() -> Self {
        Self { fd: libc::AT_FDCWD }
    }
}

struct Dir {
    dirfd: libc::c_int,
    dirp: *mut libc::DIR,
}

impl Drop for Dir {
    fn drop(&mut self) {
        unsafe {
            // Also closes `self.fd`
            libc::closedir(self.dirp);
        }
    }
}

impl Dir {
    fn new(file_descriptor: FileDescriptor) -> io::Result<Self> {
        unsafe {
            let dirp = libc::fdopendir(file_descriptor.fd);
            if dirp.is_null() {
                return Err(io::Error::last_os_error());
            }

            let fd = file_descriptor.fd;

            // Prevent `FileDescriptor` from calling `libc::close` on `fd`
            std::mem::forget(file_descriptor);

            Ok(Self { dirfd: fd, dirp })
        }
    }

    fn read(&self) -> Option<io::Result<DirEntry>> {
        unsafe {
            set_errno(0);

            match NonNull::new(libc::readdir(self.dirp)) {
                Some(dirent) => Some(Ok(DirEntry {
                    dirent,
                    dirfd: self.dirfd,
                    phantom: PhantomData,
                })),
                None => {
                    let last_err = io::Error::last_os_error();
                    let errno = last_err.raw_os_error().unwrap();
                    if errno == 0 {
                        None
                    } else {
                        Some(Err(last_err))
                    }
                }
            }
        }
    }

    fn file_descriptor(&self) -> FileDescriptor {
        FileDescriptor {
            fd: unsafe { libc::dup(self.dirfd) },
        }
    }

    fn seek(&self, loc: libc::c_long) {
        unsafe {
            libc::seekdir(self.dirp, loc);
        }
    }

    fn tell(&self) -> libc::c_long {
        // `telldir` + `seekdir` is more efficient than manually seeking a `*mut libc::DIR` but it
        // may not be correct if the result of `telldir` is invalidated by `closedir`
        unsafe { libc::telldir(self.dirp) }
    }
}

struct DirEntry<'a> {
    dirent: NonNull<libc::dirent>,
    dirfd: libc::c_int,
    phantom: PhantomData<&'a libc::dirent>,
}

impl<'a> DirEntry<'a> {
    fn filename(&self) -> &[libc::c_char; 256] {
        unsafe { &self.dirent.as_ref().d_name }
    }

    fn stat(&self, follow_symlinks: bool) -> io::Result<Metadata> {
        stat_at(
            self.dirfd,
            unsafe { self.dirent.as_ref().d_name.as_ptr() },
            follow_symlinks,
        )
    }
}

pub struct Metadata(pub libc::stat);

impl Debug for Metadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Metadata")
    }
}

impl Metadata {
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
}

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

#[derive(Debug)]
enum Origin {
    Parent,
    FileDescriptor(FileDescriptor),
    Cwd,
}

#[derive(Debug)]
struct TreeNode {
    filename: Rc<[libc::c_char]>,
    origin: Option<Origin>,
    dir_last_position: Option<libc::c_long>,
    metadata: Metadata,
}

#[derive(Debug)]
pub struct Entry<'a> {
    dir_file_descriptor: &'a FileDescriptor,
    path_stack: &'a [Rc<[libc::c_char]>],
    filename: &'a Rc<[libc::c_char]>,
    metadata: Option<&'a Metadata>,
}

impl<'a> Entry<'a> {
    fn new(
        dir_file_descriptor: &'a FileDescriptor,
        path_stack: &'a [Rc<[libc::c_char]>],
        filename: &'a Rc<[libc::c_char]>,
        metadata: Option<&'a Metadata>,
    ) -> Self {
        Self {
            dir_file_descriptor,
            path_stack,
            filename,
            metadata,
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
    pub fn metadata(&self) -> Option<&Metadata> {
        self.metadata
    }

    /// Returns the path.
    ///
    /// This is either relative to the current working directory or an absolute path.
    pub fn path(&self) -> PathBuf {
        let mut pathbuf = PathBuf::new();

        let mut append = |p: *const libc::c_char| {
            let cstr = unsafe { CStr::from_ptr(p) };
            let os_str = OsStr::from_bytes(cstr.to_bytes());
            pathbuf.push(os_str);
        };

        for p in self.path_stack {
            append(p.as_ptr());
        }
        append(self.filename.as_ptr());

        pathbuf
    }
}

pub fn traverse_directory<P, F, G, H>(
    dir: P,
    mut file_handler: F,
    mut postprocess_dir: G,
    mut err_reporter: H,
    follow_symlinks_on_args: bool,
    follow_symlinks: bool,
) -> bool
where
    P: AsRef<Path>,
    F: FnMut(Entry<'_>) -> bool,
    G: FnMut(Entry<'_>),
    H: FnMut(Entry<'_>, Error),
{
    let mut success = true;
    let mut stack: Vec<TreeNode> = Vec::new();
    let mut cwd = FileDescriptor::cwd();
    let mut path_stack: Vec<Rc<[libc::c_char]>> = Vec::new();

    let dir_filename = CString::new(dir.as_ref().as_os_str().as_encoded_bytes()).unwrap();
    let dir_metadata = match stat_at(cwd.fd, dir_filename.as_ptr(), follow_symlinks_on_args) {
        Ok(s) => s,
        Err(e) => {
            err_reporter(
                Entry::new(&cwd, &path_stack, &cstring_to_rc(&dir_filename), None),
                Error::new(e, ErrorKind::Stat),
            );
            return false;
        }
    };
    let dir_file_type = dir_metadata.file_type();
    let dir_is_symlink = dir_file_type == FileType::SymbolicLink;

    let dir_filename = if dir_is_symlink {
        match read_link_at(cwd.fd, dir_filename.as_ptr(), &dir_metadata) {
            Ok(p) => p,
            Err(e) => {
                err_reporter(
                    Entry::new(
                        &cwd,
                        &path_stack,
                        &cstring_to_rc(&dir_filename),
                        Some(&dir_metadata),
                    ),
                    Error::new(e, ErrorKind::ReadLink),
                );
                return false;
            }
        }
    } else {
        cstring_to_rc(&dir_filename)
    };
    if file_handler(Entry::new(
        &cwd,
        &path_stack,
        &dir_filename,
        Some(&dir_metadata),
    )) {
        let next_origin = Some(if dir_is_symlink {
            Origin::FileDescriptor(cwd.clone())
        } else {
            Origin::Parent
        });

        stack.push(TreeNode {
            filename: dir_filename,
            origin: next_origin,
            dir_last_position: None,
            metadata: dir_metadata,
        });
    }

    'outer: loop {
        let last = match stack.last_mut() {
            Some(last) => last,
            None => break, // Done, stack is empty
        };

        let dir = match Dir::new(FileDescriptor::open_at(
            &cwd,
            last.filename.as_ptr(),
            libc::O_RDONLY,
        )) {
            Ok(dir) => dir,
            Err(e) => {
                err_reporter(
                    Entry::new(&cwd, &path_stack, &last.filename, Some(&last.metadata)),
                    Error::new(e, ErrorKind::OpenDir),
                );
                success = false;
                continue;
            }
        };

        if let Some(last_pos) = last.dir_last_position {
            dir.seek(last_pos);
        }

        // Check if the CWD hasn't been changed yet. This requires special handling because the CWD
        // may not be reachable by moving up the directory chain with "..".
        let at_cwd = cwd.fd == libc::AT_FDCWD;

        // Enter the directory. The value of `cwd` set here is used in the while loop.
        cwd = FileDescriptor::open_at(&cwd, last.filename.as_ptr(), libc::O_RDONLY);
        path_stack.push(last.filename.clone());

        while let Some(entry_or_err) = dir.read() {
            let entry = match entry_or_err {
                Ok(entry) => entry,
                Err(e) => {
                    // Temporarily go back to the previous directory to get the correct file
                    // descriptor for `Entry`.
                    cwd = FileDescriptor::open_at(&cwd, c"..".as_ptr(), libc::O_RDONLY);

                    err_reporter(
                        Entry::new(&cwd, &path_stack, &last.filename, Some(&last.metadata)),
                        Error::new(e, ErrorKind::ReadDir),
                    );

                    // Enter the directory again.
                    cwd = FileDescriptor::open_at(&cwd, last.filename.as_ptr(), libc::O_RDONLY);

                    success = false;
                    continue;
                }
            };
            let entry_filename = entry.filename();

            const DOT: i8 = b'.' as i8;

            // Skip . and ..
            if entry_filename.get(..2) == Some(&[DOT, 0])
                || entry_filename.get(..3) == Some(&[DOT, DOT, 0])
            {
                continue;
            }

            let mut entry_metadata = match entry.stat(false) {
                Ok(s) => s,
                Err(e) => {
                    err_reporter(
                        Entry::new(
                            &cwd,
                            &path_stack,
                            &filename_slice_to_rc(&entry_filename),
                            None,
                        ),
                        Error::new(e, ErrorKind::Stat),
                    );
                    success = false;
                    continue;
                }
            };
            let mut file_type = entry_metadata.file_type();
            let is_symlink = file_type == FileType::SymbolicLink;

            // If `follow_symlinks` is enabled, use the metadata and the file type of the file
            // pointed by the symbolic link.
            if is_symlink && follow_symlinks {
                entry_metadata = match entry.stat(true) {
                    Ok(s) => s,
                    Err(e) => {
                        err_reporter(
                            Entry::new(
                                &cwd,
                                &path_stack,
                                &filename_slice_to_rc(&entry_filename),
                                Some(&entry_metadata),
                            ),
                            Error::new(e, ErrorKind::Stat),
                        );
                        success = false;
                        continue;
                    }
                };
                file_type = entry_metadata.file_type();
            }

            // If `follow_symlinks` is enabled, use the filename of the file refereed by the symlink
            // else use the filename of the symlink.
            let next_filename = if is_symlink {
                match read_link_at(cwd.fd, entry_filename.as_ptr(), &entry_metadata) {
                    Ok(p) => p,
                    Err(e) => {
                        err_reporter(
                            Entry::new(
                                &cwd,
                                &path_stack,
                                &filename_slice_to_rc(&entry_filename),
                                Some(&entry_metadata),
                            ),
                            Error::new(e, ErrorKind::ReadLink),
                        );
                        success = false;
                        continue;
                    }
                }
            } else {
                Rc::from(entry_filename.to_vec().into_boxed_slice())
            };

            if file_handler(Entry::new(
                &cwd,
                &path_stack,
                &next_filename,
                Some(&entry_metadata),
            )) {
                if file_type == FileType::Directory {
                    last.dir_last_position = Some(dir.tell());

                    stack.push(TreeNode {
                        filename: next_filename,
                        origin: Some(if at_cwd {
                            Origin::Cwd
                        } else if is_symlink {
                            Origin::FileDescriptor(dir.file_descriptor())
                        } else {
                            Origin::Parent
                        }),
                        dir_last_position: None,
                        metadata: entry_metadata,
                    });

                    continue 'outer;
                }
            }
        }

        // Exit the directory, resetting `cwd` to its value before entering the directory.
        cwd = FileDescriptor::open_at(&cwd, c"..".as_ptr(), libc::O_RDONLY);

        path_stack.pop();
        postprocess_dir(Entry::new(
            &cwd,
            &path_stack,
            &last.filename,
            Some(&last.metadata),
        ));

        let last = stack.pop().unwrap();

        if let Some(origin) = last.origin {
            match origin {
                Origin::Cwd => cwd = FileDescriptor::cwd(),
                Origin::FileDescriptor(fd) => cwd = fd,
                Origin::Parent => {
                    cwd = FileDescriptor::open_at(&cwd, c"..".as_ptr(), libc::O_RDONLY);
                }
            }
        }
        path_stack.pop();
    }

    success
}

fn cstring_to_rc(filename: &CString) -> Rc<[libc::c_char]> {
    let bytes_with_nul = filename.as_bytes_with_nul();
    unsafe {
        Rc::from(
            std::slice::from_raw_parts(bytes_with_nul.as_ptr().cast(), bytes_with_nul.len())
                .to_vec()
                .into_boxed_slice(),
        )
    }
}

fn filename_slice_to_rc(filename: &[libc::c_char; 256]) -> Rc<[libc::c_char]> {
    Rc::from(filename.to_vec().into_boxed_slice())
}

fn read_link_at(
    dirfd: libc::c_int,
    filename: *const libc::c_char,
    stat: &Metadata,
) -> io::Result<Rc<[libc::c_char]>> {
    let max_len = if stat.0.st_size == 0 {
        libc::PATH_MAX as usize
    } else {
        (stat.0.st_size + 1) as usize
    };
    let mut buf = vec![0; max_len];
    unsafe {
        let ret = libc::readlinkat(dirfd, filename, buf.as_mut_ptr(), buf.len());
        if ret < 0 {
            return Err(io::Error::last_os_error());
        }

        let num_bytes = ret as usize;
        buf.shrink_to(num_bytes);
        return Ok(Rc::from(buf.into_boxed_slice()));
    }
}

fn stat_at(
    dirfd: libc::c_int,
    filename: *const libc::c_char,
    follow_symlinks: bool,
) -> io::Result<Metadata> {
    unsafe {
        let mut statbuf = MaybeUninit::uninit();
        let flags = if follow_symlinks {
            0
        } else {
            libc::AT_SYMLINK_NOFOLLOW
        };
        let ret = libc::fstatat(dirfd, filename, statbuf.as_mut_ptr(), flags);
        if ret != 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(Metadata(statbuf.assume_init()))
    }
}

// TODO: Use errno crate?
fn set_errno(errno: libc::c_int) {
    #[cfg(target_os = "linux")]
    unsafe {
        *libc::__errno_location() = errno;
    }

    #[cfg(target_os = "macos")]
    unsafe {
        *libc::__error() = errno;
    }
}
