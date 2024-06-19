use std::{
    ffi::{CStr, CString, OsStr},
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
    fn open_at(dir_file_descriptor: &FileDescriptor, pathname: *const i8, flags: i32) -> Self {
        unsafe {
            let fd = libc::openat(dir_file_descriptor.fd, pathname, flags);
            Self { fd }
        }
    }

    fn cwd() -> Self {
        Self { fd: libc::AT_FDCWD }
    }
}

struct Dir {
    fd: libc::c_int,
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

            Ok(Self { fd, dirp })
        }
    }

    fn read(&self) -> Option<io::Result<DirEntry>> {
        unsafe {
            set_errno(0);

            match NonNull::new(libc::readdir(self.dirp)) {
                Some(dirent) => Some(Ok(DirEntry {
                    dirent,
                    dirfd: self.fd,
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
            fd: unsafe { libc::dup(self.fd) },
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
    fn pathname(&self) -> &[libc::c_char; 256] {
        unsafe { &self.dirent.as_ref().d_name }
    }

    fn stat(&self, follow_symlinks: bool) -> io::Result<Stat> {
        stat_at(
            self.dirfd,
            unsafe { self.dirent.as_ref().d_name.as_ptr() },
            follow_symlinks,
        )
    }
}

struct Stat(libc::stat);

impl Stat {
    fn file_type(&self) -> FileType {
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
    pathname: Rc<[libc::c_char]>,
    origin: Option<Origin>,
    dir_last_position: Option<libc::c_long>,
}

#[derive(Debug)]
pub struct DisplayPathBuilder<'a> {
    dir: &'a [Rc<[libc::c_char]>],
    filename: Option<&'a Rc<[libc::c_char]>>,
}

impl<'a> DisplayPathBuilder<'a> {
    fn new(dir: &'a [Rc<[libc::c_char]>], filename: Option<&'a Rc<[libc::c_char]>>) -> Self {
        Self { dir, filename }
    }

    pub fn path(&self) -> PathBuf {
        let mut pathbuf = PathBuf::new();
        for p in self.dir {
            let cstr = unsafe { CStr::from_ptr(p.as_ptr()) };
            let os_str = OsStr::from_bytes(cstr.to_bytes());
            pathbuf.push(os_str);
        }

        if let Some(p) = &self.filename {
            let cstr = unsafe { CStr::from_ptr(p.as_ptr()) };
            let os_str = OsStr::from_bytes(cstr.to_bytes());
            pathbuf.push(os_str);
        }

        pathbuf
    }
}

pub fn traverse_directory<F, G, H>(
    dir: &Path,
    mut file_handler: F,
    mut postprocess_dir: G,
    mut err_reporter: H,
    follow_symlinks_on_args: bool,
    follow_symlinks: bool,
) -> bool
where
    F: FnMut(&FileDescriptor, *const i8, FileType, DisplayPathBuilder<'_>) -> bool,
    G: FnMut(&FileDescriptor, *const i8, DisplayPathBuilder<'_>),
    H: FnMut(Error),
{
    let mut success = true;
    let mut stack: Vec<TreeNode> = Vec::new();
    let mut cwd = FileDescriptor::cwd();
    let mut display_buf: Vec<Rc<[libc::c_char]>> = Vec::new();

    let dir_path = CString::new(dir.as_os_str().as_encoded_bytes()).unwrap();
    let dir_stat = match stat_at(cwd.fd, dir_path.as_ptr(), follow_symlinks_on_args) {
        Ok(s) => s,
        Err(e) => {
            err_reporter(Error::new(e, ErrorKind::Stat));
            return false;
        }
    };
    let dir_file_type = dir_stat.file_type();
    let dir_is_symlink = dir_file_type == FileType::SymbolicLink;

    let dir_pathname = if dir_is_symlink {
        match read_link_at(cwd.fd, dir_path.as_ptr(), &dir_stat) {
            Ok(p) => p,
            Err(e) => {
                err_reporter(Error::new(e, ErrorKind::ReadLink));
                return false;
            }
        }
    } else {
        let bytes_with_nul = dir_path.as_bytes_with_nul();
        // Convert `CString` to `Vec<i8>`
        unsafe {
            Rc::from(
                std::slice::from_raw_parts(bytes_with_nul.as_ptr().cast(), bytes_with_nul.len())
                    .to_vec()
                    .into_boxed_slice(),
            )
        }
    };
    if file_handler(
        &cwd,
        dir_path.as_ptr(),
        dir_file_type,
        DisplayPathBuilder::new(&display_buf, Some(&dir_pathname)),
    ) {
        let next_origin = Some(if dir_is_symlink {
            Origin::FileDescriptor(cwd.clone())
        } else {
            Origin::Parent
        });

        stack.push(TreeNode {
            pathname: dir_pathname,
            origin: next_origin,
            dir_last_position: None,
        });
    }

    'outer: loop {
        let last = match stack.last_mut() {
            Some(last) => last,
            None => break, // Stack is empty
        };

        let dir = match Dir::new(FileDescriptor::open_at(
            &cwd,
            last.pathname.as_ptr(),
            libc::O_RDONLY,
        )) {
            Ok(dir) => dir,
            Err(e) => {
                err_reporter(Error::new(e, ErrorKind::OpenDir));
                success = false;
                continue;
            }
        };

        if let Some(last_pos) = last.dir_last_position {
            dir.seek(last_pos);
        }

        let at_cwd = cwd.fd == libc::AT_FDCWD;

        // Enter the directory. The value of `cwd` set here is used in the while loop.
        cwd = FileDescriptor::open_at(&cwd, last.pathname.as_ptr(), libc::O_RDONLY);
        display_buf.push(last.pathname.clone());

        while let Some(entry_or_err) = dir.read() {
            let entry = match entry_or_err {
                Ok(entry) => entry,
                Err(e) => {
                    err_reporter(Error::new(e, ErrorKind::ReadDir));
                    success = false;
                    continue;
                }
            };
            let entry_pathname = entry.pathname();

            const DOT: i8 = b'.' as i8;

            // Skip . and ..
            if entry_pathname.get(..2) == Some(&[DOT, 0])
                || entry_pathname.get(..3) == Some(&[DOT, DOT, 0])
            {
                continue;
            }

            let mut entry_stat = match entry.stat(false) {
                Ok(s) => s,
                Err(e) => {
                    err_reporter(Error::new(e, ErrorKind::Stat));
                    success = false;
                    continue;
                }
            };
            let mut file_type = entry_stat.file_type();
            let is_symlink = file_type == FileType::SymbolicLink;

            if is_symlink && follow_symlinks {
                entry_stat = match entry.stat(true) {
                    Ok(s) => s,
                    Err(e) => {
                        err_reporter(Error::new(e, ErrorKind::Stat));
                        success = false;
                        continue;
                    }
                };
                file_type = entry_stat.file_type();
            }

            let next_pathname = if is_symlink {
                match read_link_at(cwd.fd, entry_pathname.as_ptr(), &entry_stat) {
                    Ok(p) => p,
                    Err(e) => {
                        err_reporter(Error::new(e, ErrorKind::ReadLink));
                        success = false;
                        continue;
                    }
                }
            } else {
                Rc::from(entry_pathname.to_vec().into_boxed_slice())
            };

            if file_handler(
                &cwd,
                entry_pathname.as_ptr(),
                file_type,
                DisplayPathBuilder::new(&display_buf, Some(&next_pathname)),
            ) {
                if file_type == FileType::Directory {
                    last.dir_last_position = Some(dir.tell());

                    stack.push(TreeNode {
                        pathname: next_pathname,
                        origin: Some(if at_cwd {
                            Origin::Cwd
                        } else if is_symlink {
                            Origin::FileDescriptor(dir.file_descriptor())
                        } else {
                            Origin::Parent
                        }),
                        dir_last_position: None,
                    });

                    continue 'outer;
                }
            }
        }

        // Exit the directory, resetting `cwd` to its value before entering the directory.
        cwd = FileDescriptor::open_at(&cwd, c"..".as_ptr(), libc::O_RDONLY);

        postprocess_dir(
            &cwd,
            last.pathname.as_ptr(),
            DisplayPathBuilder::new(&display_buf, None),
        );
        display_buf.pop();

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
        display_buf.pop();
    }

    success
}

fn read_link_at(
    dirfd: libc::c_int,
    pathname: *const libc::c_char,
    stat: &Stat,
) -> io::Result<Rc<[libc::c_char]>> {
    let max_len = if stat.0.st_size == 0 {
        libc::PATH_MAX as usize
    } else {
        (stat.0.st_size + 1) as usize
    };
    let mut buf = vec![0; max_len];
    unsafe {
        let ret = libc::readlinkat(dirfd, pathname, buf.as_mut_ptr(), buf.len());
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
    pathname: *const libc::c_char,
    follow_symlinks: bool,
) -> io::Result<Stat> {
    unsafe {
        let mut statbuf = MaybeUninit::uninit();
        let flags = if follow_symlinks {
            0
        } else {
            libc::AT_SYMLINK_NOFOLLOW
        };
        let ret = libc::fstatat(dirfd, pathname, statbuf.as_mut_ptr(), flags);
        if ret != 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(Stat(statbuf.assume_init()))
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

#[test]
fn walkdir_test() {
    let test_dir = "target/tmp/walkdir_test";
    let a_1 = format!("{test_dir}/a/1");
    let a_2 = format!("{test_dir}/a/2");
    let b_1 = format!("{test_dir}/b/1");
    let b_2 = format!("{test_dir}/b/2");

    for dir in [&a_1, &a_2, &b_1, &b_2] {
        std::fs::create_dir_all(dir).unwrap();
    }

    let filenames = [
        format!("{test_dir}"),
        format!("{test_dir}/b"),
        format!("{test_dir}/b/1"),
        format!("{test_dir}/b/2"),
        format!("{test_dir}/a"),
        format!("{test_dir}/a/1"),
        format!("{test_dir}/a/2"),
    ];
    let mut filename_iter = filenames.iter();

    traverse_directory(
        Path::new(test_dir),
        |_, _, _, buf| {
            let s = format!("{}", buf.path().display());
            println!("{s}");
            assert_eq!(filename_iter.next(), Some(&s));
            true
        },
        |_, _, _| {},
        |_| {},
        false,
        false,
    );

    std::fs::remove_dir_all(test_dir).unwrap();
}
