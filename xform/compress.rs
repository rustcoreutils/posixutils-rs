//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use flate2::read::GzDecoder;
use flate2::write::GzEncoder;
use flate2::Compression;
use gettextrs::gettext;
use plib::diag;
use plib::io::input_stream;
use plib::lzw::{UnixLZWReader, UnixLZWWriter};
use std::ffi::{OsStr, OsString};
use std::fs::{self, File};
use std::io::{self, IsTerminal, Read, Write};
use std::path::{Path, PathBuf};

use fsat::{Dir, Entry, FileId, Kind};

/// Conventional fallback when {NAME_MAX} cannot be queried.
const NAME_MAX_FALLBACK: usize = 255;

/// Query `{NAME_MAX}` for the directory that will hold the output file, via
/// `pathconf(_PC_NAME_MAX)`; fall back to a conventional 255 when unavailable.
#[cfg(unix)]
fn name_max(dir: &Path) -> usize {
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt;

    let dir = if dir.as_os_str().is_empty() {
        Path::new(".")
    } else {
        dir
    };
    if let Ok(c) = CString::new(dir.as_os_str().as_bytes()) {
        // SAFETY: c is a valid NUL-terminated C string for the lifetime of the call.
        let v = unsafe { libc::pathconf(c.as_ptr(), libc::_PC_NAME_MAX) };
        if v > 0 {
            return v as usize;
        }
    }
    NAME_MAX_FALLBACK
}

/// `{NAME_MAX}` on Windows, which has no `pathconf`: 255, NTFS's limit on a
/// path component.
#[cfg(windows)]
fn name_max(_dir: &Path) -> usize {
    NAME_MAX_FALLBACK
}

/// Compression algorithm
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Algorithm {
    Lzw,
    Deflate,
}

impl Algorithm {
    fn suffix(&self) -> &'static str {
        match self {
            Algorithm::Lzw => ".Z",
            Algorithm::Deflate => ".gz",
        }
    }

    fn from_magic(data: &[u8]) -> Option<Self> {
        if data.len() < 2 {
            return None;
        }
        match (data[0], data[1]) {
            (0x1F, 0x9D) => Some(Algorithm::Lzw),
            (0x1F, 0x8B) => Some(Algorithm::Deflate),
            _ => None,
        }
    }
}

/// Program invocation mode based on argv[0]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ProgramMode {
    Compress,
    Uncompress,
    Zcat,
}

impl ProgramMode {
    fn detect() -> Self {
        let prog = std::env::args().next().unwrap_or_default();
        if prog.ends_with("zcat") {
            ProgramMode::Zcat
        } else if prog.ends_with("uncompress") {
            ProgramMode::Uncompress
        } else {
            ProgramMode::Compress
        }
    }
}

/// compress - compress and decompress data
#[derive(Parser)]
#[command(version, about = gettext("compress - compress and decompress data"))]
struct Args {
    #[arg(short = 'b', allow_hyphen_values = true, help = gettext("For LZW: max bits (9-16). For DEFLATE: compression level (1-9)"))]
    bits: Option<u32>,

    #[arg(short = 'c', long, help = gettext("Write to standard output; no files are changed"))]
    stdout: bool,

    #[arg(short = 'd', long, help = gettext("Decompress files"))]
    decompress: bool,

    #[arg(short = 'f', long, help = gettext("Force compression/decompression; do not prompt"))]
    force: bool,

    #[arg(short = 'g', help = gettext("Equivalent to -m gzip"))]
    gzip: bool,

    #[arg(short = 'm', allow_hyphen_values = true, help = gettext("Use algorithm: lzw, deflate, or gzip"))]
    algo: Option<String>,

    #[arg(short = 'v', long, help = gettext("Write messages to standard error"))]
    verbose: bool,

    #[arg(help = gettext("Files to process. Use \"-\" or no args for stdin"))]
    files: Vec<PathBuf>,
}

/// Check if pathname represents stdin ("-")
fn is_stdin(pathname: &Path) -> bool {
    pathname.as_os_str() == "-"
}

fn prompt_user(prompt: &str) -> bool {
    eprint!("compress: {} ", prompt);
    let mut response = String::new();
    if io::stdin().read_line(&mut response).is_err() {
        return false;
    }
    plib::locale::is_affirmative(response.trim_end_matches(['\r', '\n']))
}

/// Combine per-file exit codes into a single, order-independent status.
///
/// Severity order: 1 (error) outranks 2 (file not compressed because it would
/// grow) outranks 0 (success). Both 1 and 2 are non-zero per spec 90511-90516;
/// this just makes the final code deterministic regardless of file order.
fn merge_exit(current: i32, new: i32) -> i32 {
    match (current, new) {
        (1, _) | (_, 1) => 1,
        (2, _) | (_, 2) => 2,
        _ => 0,
    }
}

/// Decide whether an output file that already exists may be replaced.
///
/// Returns `true` to proceed with the write, `false` to skip it (the caller
/// then returns a non-zero exit code). Per POSIX (90427-90432), the overwrite
/// prompt is issued **only** when standard input is a terminal; when stdin is
/// not a terminal and `-f` was not given, a diagnostic is written and the file
/// is not overwritten, with no prompt (so a pipeline's input stream is never
/// consumed by `read_line`).
fn may_overwrite(output_path: &Path, force: bool) -> bool {
    if force {
        return true;
    }
    if io::stdin().is_terminal() {
        let yes = prompt_user(&gettext!(
            "Do you want to overwrite {} (y)es or (n)o?",
            output_path.display()
        ));
        if !yes {
            diag::warning(&format!(
                "{}: {}",
                output_path.display(),
                gettext("not overwritten")
            ));
        }
        yes
    } else {
        diag::error(&format!(
            "{}: {}",
            output_path.display(),
            gettext("already exists; not overwritten (use -f to force)")
        ));
        false
    }
}

/// Directory-relative file operations.
///
/// Every name compress reads, creates or removes for an operand is resolved
/// in one directory opened once for that operand, so the input, the output
/// and the removal all act in the same directory even if a component of its
/// path is renamed meanwhile. Identities are compared by device and inode,
/// so a name that comes to hold a different file is noticed instead of
/// acted on.
#[cfg(unix)]
mod fsat {
    use gettextrs::gettext;
    use std::ffi::{CString, OsStr};
    use std::fs::File;
    use std::io;
    use std::mem::MaybeUninit;
    use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
    use std::os::unix::ffi::OsStrExt;
    use std::path::Path;

    /// The identity of a file: its device and inode numbers, kept in the
    /// C library's own types so `fstat` and `fstatat` results compare
    /// without a conversion.
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct FileId {
        dev: libc::dev_t,
        ino: libc::ino_t,
    }

    impl FileId {
        /// The identity of an open file, from its `fstat`.
        pub fn of(file: &File) -> io::Result<Self> {
            let mut st = MaybeUninit::<libc::stat>::uninit();
            // SAFETY: a valid descriptor and a buffer the call fills when
            // it succeeds.
            if unsafe { libc::fstat(file.as_raw_fd(), st.as_mut_ptr()) } != 0 {
                return Err(io::Error::last_os_error());
            }
            // SAFETY: fstat succeeded, so it filled `st`.
            Ok(Self::from_stat(unsafe { st.assume_init_ref() }))
        }

        fn from_stat(st: &libc::stat) -> Self {
            FileId {
                dev: st.st_dev,
                ino: st.st_ino,
            }
        }
    }

    /// The type of a directory entry itself, not of what a link names.
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub enum Kind {
        Regular,
        Symlink,
        Directory,
        Other,
    }

    /// What `lstat` reports about a directory entry.
    pub struct Entry {
        pub id: FileId,
        pub kind: Kind,
        pub links: libc::nlink_t,
    }

    impl Entry {
        fn from_stat(st: &libc::stat) -> Self {
            let kind = match st.st_mode & libc::S_IFMT {
                libc::S_IFREG => Kind::Regular,
                libc::S_IFLNK => Kind::Symlink,
                libc::S_IFDIR => Kind::Directory,
                _ => Kind::Other,
            };
            Entry {
                id: FileId::from_stat(st),
                kind,
                links: st.st_nlink,
            }
        }
    }

    /// An open directory that names are resolved in.
    pub struct Dir {
        fd: OwnedFd,
    }

    impl Dir {
        /// Open `path`, the directory holding an operand; "" is ".".
        pub fn open(path: &Path) -> io::Result<Dir> {
            let path = if path.as_os_str().is_empty() {
                Path::new(".")
            } else {
                path
            };
            let c = CString::new(path.as_os_str().as_bytes())?;
            let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC;
            // SAFETY: `c` is a valid NUL-terminated path for the call.
            let fd = unsafe { libc::open(c.as_ptr(), flags) };
            Ok(Dir { fd: owned(fd)? })
        }

        /// `lstat` of `name`: a symbolic link is reported as itself.
        pub fn lstat(&self, name: &OsStr) -> io::Result<Entry> {
            let c = CString::new(name.as_bytes())?;
            let mut st = MaybeUninit::<libc::stat>::uninit();
            // SAFETY: a valid directory descriptor, a valid NUL-terminated
            // name, and a buffer the call fills when it succeeds.
            let rc = unsafe {
                libc::fstatat(
                    self.fd.as_raw_fd(),
                    c.as_ptr(),
                    st.as_mut_ptr(),
                    libc::AT_SYMLINK_NOFOLLOW,
                )
            };
            if rc != 0 {
                return Err(io::Error::last_os_error());
            }
            // SAFETY: fstatat succeeded, so it filled `st`.
            Ok(Entry::from_stat(unsafe { st.assume_init_ref() }))
        }

        /// Open `name` for reading. `O_NONBLOCK` keeps the open of a FIFO
        /// from waiting for a writer and `O_NOCTTY` keeps a terminal from
        /// becoming the controlling one; the caller refuses anything but a
        /// regular file before reading. Unless `follow`, a symbolic link is
        /// refused (`ELOOP`) instead of followed.
        pub fn open_read(&self, name: &OsStr, follow: bool) -> io::Result<File> {
            let mut flags = libc::O_RDONLY | libc::O_NONBLOCK | libc::O_NOCTTY | libc::O_CLOEXEC;
            if !follow {
                flags |= libc::O_NOFOLLOW;
            }
            self.openat(name, flags)
        }

        /// Create `name` for writing. `O_EXCL` with `O_NOFOLLOW` fails if
        /// anything holds the name, a symbolic link (dangling or not)
        /// included, so nothing is ever written through one. The mode is
        /// 0600 until the caller sets the final one, so nobody else can
        /// open the file while it is being filled.
        pub fn create(&self, name: &OsStr) -> io::Result<File> {
            let flags = libc::O_WRONLY
                | libc::O_CREAT
                | libc::O_EXCL
                | libc::O_NOFOLLOW
                | libc::O_NOCTTY
                | libc::O_CLOEXEC;
            self.openat(name, flags)
        }

        fn openat(&self, name: &OsStr, flags: libc::c_int) -> io::Result<File> {
            let c = CString::new(name.as_bytes())?;
            // The mode is a variadic argument, so it is passed as an
            // unsigned int, the type `mode_t` promotes to everywhere
            // (`mode_t` is 16 bits on macOS).
            let mode: libc::c_uint = 0o600;
            // SAFETY: a valid directory descriptor and NUL-terminated name;
            // the mode is read only under O_CREAT.
            let fd = unsafe { libc::openat(self.fd.as_raw_fd(), c.as_ptr(), flags, mode) };
            Ok(File::from(owned(fd)?))
        }

        /// Remove `name`, which is not a directory.
        pub fn unlink(&self, name: &OsStr) -> io::Result<()> {
            let c = CString::new(name.as_bytes())?;
            // SAFETY: a valid directory descriptor and NUL-terminated name.
            if unsafe { libc::unlinkat(self.fd.as_raw_fd(), c.as_ptr(), 0) } != 0 {
                return Err(io::Error::last_os_error());
            }
            Ok(())
        }

        /// Remove `name` only if it still holds the file `id`. POSIX has no
        /// unlink by descriptor, so a swap between this check and the
        /// unlink is the one window left; it needs write access to this
        /// directory and removes only the swapped-in entry.
        pub fn unlink_if(&self, name: &OsStr, id: FileId) -> io::Result<()> {
            if self.lstat(name)?.id != id {
                return Err(io::Error::other(gettext(
                    "replaced by another file while being processed",
                )));
            }
            self.unlink(name)
        }
    }

    /// Take ownership of a descriptor an open call returned, or of its error.
    fn owned(fd: libc::c_int) -> io::Result<OwnedFd> {
        if fd < 0 {
            return Err(io::Error::last_os_error());
        }
        // SAFETY: `fd` was just returned by a successful open, and nothing
        // else owns it.
        Ok(unsafe { OwnedFd::from_raw_fd(fd) })
    }
}

/// Path-based stand-ins for the directory-relative operations on Windows,
/// which has no `openat`. Stable Rust exposes no file identity there, so
/// every identity compares equal and a removal rests on the name alone.
#[cfg(windows)]
mod fsat {
    use std::ffi::OsStr;
    use std::fs::{self, File};
    use std::io;
    use std::path::{Path, PathBuf};

    /// A file identity that Windows cannot supply: all compare equal.
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct FileId;

    impl FileId {
        pub fn of(_file: &File) -> io::Result<Self> {
            Ok(FileId)
        }
    }

    /// The type of a directory entry itself, not of what a link names.
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub enum Kind {
        Regular,
        Symlink,
        Directory,
        Other,
    }

    /// What `symlink_metadata` reports about a directory entry. Stable Rust
    /// exposes no link count on Windows, so a file counts as its only link.
    pub struct Entry {
        pub id: FileId,
        pub kind: Kind,
        pub links: u64,
    }

    /// The directory that names are resolved in, by path.
    pub struct Dir {
        path: PathBuf,
    }

    impl Dir {
        pub fn open(path: &Path) -> io::Result<Dir> {
            Ok(Dir {
                path: path.to_path_buf(),
            })
        }

        pub fn lstat(&self, name: &OsStr) -> io::Result<Entry> {
            let file_type = fs::symlink_metadata(self.path.join(name))?.file_type();
            let kind = if file_type.is_symlink() {
                Kind::Symlink
            } else if file_type.is_dir() {
                Kind::Directory
            } else if file_type.is_file() {
                Kind::Regular
            } else {
                Kind::Other
            };
            Ok(Entry {
                id: FileId,
                kind,
                links: 1,
            })
        }

        pub fn open_read(&self, name: &OsStr, _follow: bool) -> io::Result<File> {
            File::open(self.path.join(name))
        }

        /// Create `name`; `create_new` (CREATE_NEW) fails if anything,
        /// a symbolic link included, already holds the name.
        pub fn create(&self, name: &OsStr) -> io::Result<File> {
            File::options()
                .write(true)
                .create_new(true)
                .open(self.path.join(name))
        }

        /// Remove `name`, clearing its read-only attribute first. Windows
        /// will not delete a read-only file wherever
        /// FILE_DISPOSITION_IGNORE_READONLY_ATTRIBUTE is not honoured (Wine,
        /// older Windows), and compress copies that attribute onto its
        /// output, so both the input removal and the back-out of the output
        /// depend on this. A file that still cannot be removed gets its
        /// attribute back, so a failure leaves it as it was.
        pub fn unlink(&self, name: &OsStr) -> io::Result<()> {
            let path = self.path.join(name);
            let mut perms = fs::symlink_metadata(&path)?.permissions();
            if !perms.readonly() {
                return fs::remove_file(&path);
            }
            let original = perms.clone();
            #[expect(
                clippy::permissions_set_readonly_false,
                reason = "Windows only: clears the read-only attribute, no Unix mode bits"
            )]
            perms.set_readonly(false);
            fs::set_permissions(&path, perms)?;
            fs::remove_file(&path).inspect_err(|_| {
                let _ = fs::set_permissions(&path, original);
            })
        }

        pub fn unlink_if(&self, name: &OsStr, _id: FileId) -> io::Result<()> {
            self.unlink(name)
        }
    }
}

/// Saved file metadata for preservation
struct FileMetadata {
    /// The mode on Unix; the read-only attribute on Windows.
    permissions: fs::Permissions,
    #[cfg(unix)]
    owner: (u32, u32),
    times: fs::FileTimes,
}

impl FileMetadata {
    /// The attributes of the input, from the `fstat` of its descriptor.
    fn of(meta: &fs::Metadata) -> io::Result<Self> {
        Ok(Self {
            permissions: meta.permissions(),
            #[cfg(unix)]
            owner: {
                use std::os::unix::fs::MetadataExt;
                (meta.uid(), meta.gid())
            },
            times: fs::FileTimes::new()
                .set_accessed(meta.accessed()?)
                .set_modified(meta.modified()?),
        })
    }

    /// Give the output, through its still-open descriptor, the saved owner,
    /// then mode, then times. Each is best effort: the spec (90389-90392)
    /// asks for them only when the process has sufficient privilege, and
    /// the output is already complete and correct, so a failure must not
    /// turn into a non-zero exit. A step that fails leaves the output no
    /// more open than the 0600 it was created with.
    ///
    /// The times go through the descriptor too, so neither the umask (which
    /// may have created the file without owner write) nor the mode set just
    /// before can keep them from being set; set_times keeps nanoseconds
    /// (futimens on Unix), so a preserved time compares equal to the one it
    /// came from.
    fn apply_to(&self, out: &File) {
        let permissions = self.restore_owner(out);
        let _ = out.set_permissions(permissions);
        let _ = out.set_times(self.times);
    }

    /// Give `out` the saved owner and return the mode to set afterwards.
    /// The owner goes first because chown clears the set-user-ID and
    /// set-group-ID bits; and each of those bits is kept only if the output
    /// really ended up with the input's user or group, so a chown refused
    /// for lack of privilege never yields a set-ID file owned by the user
    /// who ran compress.
    #[cfg(unix)]
    fn restore_owner(&self, out: &File) -> fs::Permissions {
        use std::os::unix::fs::{fchown, MetadataExt, PermissionsExt};

        const SET_UID: u32 = 0o4000;
        const SET_GID: u32 = 0o2000;

        let (uid, gid) = self.owner;
        let _ = fchown(out, Some(uid), Some(gid));
        let mut mode = self.permissions.mode() & 0o7777;
        match out.metadata() {
            Ok(now) => {
                if now.uid() != uid {
                    mode &= !SET_UID;
                }
                if now.gid() != gid {
                    mode &= !SET_GID;
                }
            }
            Err(_) => mode &= !(SET_UID | SET_GID),
        }
        fs::Permissions::from_mode(mode)
    }

    /// Windows has no ownership to restore; the read-only attribute is all
    /// the "mode" there is.
    #[cfg(windows)]
    fn restore_owner(&self, _out: &File) -> fs::Permissions {
        self.permissions.clone()
    }
}

/// The last component of `path`, or an error for a path without one.
fn file_name(path: &Path) -> io::Result<&OsStr> {
    path.file_name().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            gettext("input path has no filename"),
        )
    })
}

/// An operand that is replaced by its compressed or decompressed form. It is
/// opened once, read and given its attributes through that descriptor, and
/// removed only while its name still holds what was opened.
struct Input {
    path: PathBuf,
    dir: Dir,
    name: OsString,
    entry: Entry,
    file: File,
    metadata: FileMetadata,
}

impl Input {
    /// Open `path`, which must be a regular file or a symbolic link to one.
    /// A link is followed (the operand names the file the user means) and
    /// it is the link that is removed afterwards. Any other type is refused
    /// before it is read, so a FIFO cannot hang the run.
    fn open(path: &Path) -> io::Result<Input> {
        let name = file_name(path)?.to_os_string();
        let dir = Dir::open(path.parent().unwrap_or(Path::new("")))?;
        let entry = dir.lstat(&name)?;
        let file = match entry.kind {
            // Not following here means a link swapped in after the lstat
            // is refused rather than read.
            Kind::Regular => dir.open_read(&name, false)?,
            Kind::Symlink => dir.open_read(&name, true)?,
            Kind::Directory | Kind::Other => return Err(not_regular()),
        };
        let meta = file.metadata()?;
        if !meta.is_file() {
            return Err(not_regular());
        }
        if entry.kind == Kind::Regular && FileId::of(&file)? != entry.id {
            return Err(io::Error::other(gettext(
                "replaced by another file while being opened",
            )));
        }
        Ok(Input {
            path: path.to_path_buf(),
            dir,
            name,
            entry,
            metadata: FileMetadata::of(&meta)?,
            file,
        })
    }

    fn read_all(&mut self) -> io::Result<Vec<u8>> {
        let mut data = Vec::new();
        self.file.read_to_end(&mut data)?;
        Ok(data)
    }

    /// Write `data` to the new file `output` beside the input, give it the
    /// input's attributes, and remove the input. Returns the exit status
    /// for the operand; a refusal has already been reported.
    fn replace_with(&self, output: &Path, data: &[u8], force: bool) -> io::Result<i32> {
        let out_name = file_name(output)?;
        let Some(mut out) = create_output(&self.dir, out_name, output, force)? else {
            return Ok(1);
        };
        let out_id = FileId::of(&out)?;
        if let Err(e) = out.write_all(data) {
            let _ = self.dir.unlink_if(out_name, out_id);
            return Err(e);
        }
        self.metadata.apply_to(&out);
        drop(out);

        // If the input cannot be removed, back out the output so we do not
        // leave both files behind, and report a non-zero status
        // (spec 90393-90400).
        if let Err(e) = self.dir.unlink_if(&self.name, self.entry.id) {
            let _ = self.dir.unlink_if(out_name, out_id);
            diag::error(&format!(
                "{}: {}: {}",
                self.path.display(),
                gettext("cannot remove input"),
                diag::io_error_text(&e)
            ));
            return Ok(1);
        }
        Ok(0)
    }
}

fn not_regular() -> io::Error {
    io::Error::new(io::ErrorKind::InvalidInput, gettext("not a regular file"))
}

/// Create the output file `name` in `dir`, shown to the user as `path`.
///
/// Whatever already holds the name is judged by `lstat`, so a symbolic
/// link, dangling or not, counts as an existing file: it is replaced only
/// with `-f` or a yes at the prompt, and then by unlinking it and creating
/// the output afresh, never by writing through it. A directory is never
/// replaced. If another entry appears between the unlink and the create,
/// the exclusive create fails and that is reported, not retried.
fn create_output(dir: &Dir, name: &OsStr, path: &Path, force: bool) -> io::Result<Option<File>> {
    match dir.lstat(name) {
        Err(e) if e.kind() == io::ErrorKind::NotFound => {}
        Err(e) => return Err(e),
        Ok(entry) if entry.kind == Kind::Directory => {
            diag::error(&format!(
                "{}: {}",
                path.display(),
                gettext("is a directory; not overwritten")
            ));
            return Ok(None);
        }
        Ok(_) => {
            if !may_overwrite(path, force) {
                return Ok(None);
            }
            match dir.unlink(name) {
                Err(e) if e.kind() != io::ErrorKind::NotFound => return Err(e),
                _ => {}
            }
        }
    }
    dir.create(name).map(Some)
}

/// Warn about, but do not refuse, a multiply-linked input that is to be
/// removed (spec 90403-90406), unless `-f` was given. Returns whether to
/// go ahead.
/// `links` is the platform's own link-count type (`nlink_t` is 16 to 64
/// bits wide depending on the target).
fn check_hard_links(path: &Path, links: impl Into<u64>, force: bool) -> bool {
    let links: u64 = links.into();
    if links > 1 {
        diag::warning(&format!(
            "{}: {}",
            path.display(),
            gettext!("has {} hard links", links)
        ));
        if !force {
            return false;
        }
    }
    true
}

/// Check if output path would exceed PATH_MAX
fn check_path_max(path: &Path) -> io::Result<()> {
    let path_len = path.as_os_str().len();
    #[cfg(unix)]
    {
        if path_len > libc::PATH_MAX as usize {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                gettext("pathname too long"),
            ));
        }
    }
    let _ = path_len; // silence unused warning on non-unix
    Ok(())
}

/// Compress data using LZW algorithm
fn compress_lzw(data: &[u8], bits: Option<u32>) -> io::Result<Vec<u8>> {
    let mut encoder = UnixLZWWriter::new(bits);
    let mut out = encoder.write(data)?;
    out.extend_from_slice(&encoder.close()?);
    Ok(out)
}

/// Compress data using DEFLATE/gzip algorithm
fn compress_gzip(data: &[u8], level: Option<u32>) -> io::Result<Vec<u8>> {
    let level = level.unwrap_or(6);
    let mut encoder = GzEncoder::new(Vec::new(), Compression::new(level));
    encoder.write_all(data)?;
    encoder.finish()
}

/// Decompress data using LZW algorithm
fn decompress_lzw(data: &[u8]) -> io::Result<Vec<u8>> {
    let cursor = io::Cursor::new(data.to_vec());
    let mut decoder = UnixLZWReader::new(Box::new(cursor));
    let mut output = Vec::new();
    loop {
        let buf = decoder.read()?;
        if buf.is_empty() {
            break;
        }
        output.extend_from_slice(&buf);
    }
    Ok(output)
}

/// Decompress data using DEFLATE/gzip algorithm
fn decompress_gzip(data: &[u8]) -> io::Result<Vec<u8>> {
    let mut decoder = GzDecoder::new(data);
    let mut output = Vec::new();
    decoder.read_to_end(&mut output)?;
    Ok(output)
}

/// Auto-detect algorithm from data and decompress
fn decompress_auto(data: &[u8]) -> io::Result<Vec<u8>> {
    match Algorithm::from_magic(data) {
        Some(Algorithm::Lzw) => decompress_lzw(data),
        Some(Algorithm::Deflate) => decompress_gzip(data),
        None => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            gettext("unknown compression format"),
        )),
    }
}

/// Parse algorithm from string
fn parse_algorithm(s: &str) -> io::Result<Algorithm> {
    match s.to_lowercase().as_str() {
        "lzw" => Ok(Algorithm::Lzw),
        "deflate" | "gzip" => Ok(Algorithm::Deflate),
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            gettext!("unknown algorithm: {}", s),
        )),
    }
}

/// Validate -b value for the given algorithm
fn validate_bits(algo: Algorithm, bits: u32) -> io::Result<()> {
    match algo {
        Algorithm::Lzw => {
            // The LZW writer's default (no -b) emits 16-bit codes, and the
            // RATIONALE (spec 90542-90544) encourages 15/16, so accept the full
            // historical 9-16 range rather than the normative-DESCRIPTION 14.
            if !(9..=16).contains(&bits) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    gettext("LZW bits must be 9-16"),
                ));
            }
        }
        Algorithm::Deflate => {
            if !(1..=9).contains(&bits) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    gettext("DEFLATE level must be 1-9"),
                ));
            }
        }
    }
    Ok(())
}

/// Determine algorithm for compression
fn get_compress_algorithm(args: &Args) -> io::Result<Algorithm> {
    if args.gzip {
        return Ok(Algorithm::Deflate);
    }
    if let Some(ref algo_str) = args.algo {
        return parse_algorithm(algo_str);
    }
    Ok(Algorithm::Lzw) // default
}

/// Build output path for compression
fn compress_output_path(input: &Path, algo: Algorithm) -> io::Result<PathBuf> {
    let file_name = file_name(input)?;
    let fname = format!("{}{}", file_name.to_string_lossy(), algo.suffix());

    let parent = input.parent();
    let dir = parent.unwrap_or_else(|| Path::new(""));
    if fname.len() > name_max(dir) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            gettext("filename too long"),
        ));
    }

    let output = match parent {
        Some(parent) if !parent.as_os_str().is_empty() => parent.join(&fname),
        _ => PathBuf::from(&fname),
    };

    check_path_max(&output)?;
    Ok(output)
}

/// Build output path for decompression (remove suffix)
fn decompress_output_path(input: &Path) -> PathBuf {
    let mut output = input.to_path_buf();
    // Remove .Z or .gz extension
    if let Some(ext) = input.extension() {
        if ext == "Z" || ext == "gz" {
            output.set_extension("");
        }
    }
    output
}

/// Find input file for decompression (try adding .Z if needed)
fn find_decompress_input(pathname: &Path) -> PathBuf {
    // If file exists as-is, use it
    if pathname.exists() {
        return pathname.to_path_buf();
    }

    // Check if it already has a known suffix
    if let Some(ext) = pathname.extension() {
        if ext == "Z" || ext == "gz" {
            return pathname.to_path_buf();
        }
    }

    // Try .Z suffix (per POSIX: default)
    let mut with_z = pathname.to_path_buf();
    let mut new_name = pathname.file_name().unwrap_or_default().to_os_string();
    new_name.push(".Z");
    with_z.set_file_name(new_name);
    if with_z.exists() {
        return with_z;
    }

    // Try .gz suffix
    let mut with_gz = pathname.to_path_buf();
    let mut new_name = pathname.file_name().unwrap_or_default().to_os_string();
    new_name.push(".gz");
    with_gz.set_file_name(new_name);
    if with_gz.exists() {
        return with_gz;
    }

    // Fall back to original with .Z appended (will error on open)
    with_z
}

/// Validate `-b` and compress `data` with `algo`.
fn compress_data(args: &Args, algo: Algorithm, data: &[u8]) -> io::Result<Vec<u8>> {
    if let Some(bits) = args.bits {
        validate_bits(algo, bits)?;
    }
    match algo {
        Algorithm::Lzw => compress_lzw(data, args.bits),
        Algorithm::Deflate => compress_gzip(data, args.bits),
    }
}

/// The space saved, as a percentage of the input size.
fn compression_ratio(inp_size: usize, out_size: usize) -> f64 {
    if inp_size > 0 {
        100.0 - (out_size as f64 / inp_size as f64) * 100.0
    } else {
        0.0
    }
}

/// Process a single file for compression
fn compress_file(args: &Args, pathname: &Path, algo: Algorithm) -> io::Result<i32> {
    let reading_stdin = is_stdin(pathname);

    // Warn if input already has compression suffix
    if !reading_stdin {
        if let Some(ext) = pathname.extension() {
            if ext == "Z" || ext == "gz" {
                diag::warning(&format!(
                    "{}: {}",
                    pathname.display(),
                    gettext!("already has {} suffix", ext.to_str().unwrap())
                ));
            }
        }
    }

    if args.stdout || reading_stdin {
        compress_to_stdout(args, pathname, algo)
    } else {
        compress_in_place(args, pathname, algo)
    }
}

/// Write `data` to standard output and flush it, reporting a failure as a
/// write error rather than one of the input file's. Without the flush, data
/// with no <newline> stays in stdout's line buffer until exit, where its
/// write error is lost. Returns whether the write succeeded.
fn write_stdout(data: &[u8]) -> bool {
    let mut out = io::stdout().lock();
    match out.write_all(data).and_then(|()| out.flush()) {
        Ok(()) => true,
        Err(e) => {
            diag::error(&format!(
                "{}: {}",
                gettext("write error"),
                diag::io_error_text(&e)
            ));
            false
        }
    }
}

/// Compress `pathname` (or standard input) to standard output. No file is
/// changed, so the operand may be of any type that can be read.
fn compress_to_stdout(args: &Args, pathname: &Path, algo: Algorithm) -> io::Result<i32> {
    let mut inp_buf = Vec::new();
    input_stream(pathname, true)?.read_to_end(&mut inp_buf)?;
    let out_buf = compress_data(args, algo, &inp_buf)?;
    if !write_stdout(&out_buf) {
        return Ok(1);
    }
    if args.verbose && !is_stdin(pathname) {
        let ratio = compression_ratio(inp_buf.len(), out_buf.len());
        eprintln!(
            "{}",
            gettext!("{}: Compression: {:.1}%", pathname.display(), ratio)
        );
    }
    Ok(0)
}

/// Replace the file `pathname` with its compressed form.
fn compress_in_place(args: &Args, pathname: &Path, algo: Algorithm) -> io::Result<i32> {
    let mut input = Input::open(pathname)?;
    if !check_hard_links(pathname, input.entry.links, args.force) {
        return Ok(1);
    }
    let inp_buf = input.read_all()?;
    let out_buf = compress_data(args, algo, &inp_buf)?;
    if out_buf.len() >= inp_buf.len() && !args.force {
        return Ok(2);
    }

    let output_path = compress_output_path(pathname, algo)?;
    let status = input.replace_with(&output_path, &out_buf, args.force)?;
    if status == 0 && args.verbose {
        eprintln!(
            "{}",
            gettext!(
                "{}: -- replaced with {} Compression: {:.1}%",
                pathname.display(),
                output_path.display(),
                compression_ratio(inp_buf.len(), out_buf.len())
            )
        );
    }
    Ok(status)
}

/// Process a single file for decompression
fn decompress_file(args: &Args, pathname: &Path) -> io::Result<i32> {
    if args.stdout || is_stdin(pathname) {
        decompress_to_stdout(args, pathname)
    } else {
        decompress_in_place(args, pathname)
    }
}

/// Decompress `pathname` (or standard input) to standard output. No file
/// is changed, so the operand may be of any type that can be read.
fn decompress_to_stdout(args: &Args, pathname: &Path) -> io::Result<i32> {
    let reading_stdin = is_stdin(pathname);
    let input_path = if reading_stdin {
        pathname.to_path_buf()
    } else {
        find_decompress_input(pathname)
    };

    let mut compressed_data = Vec::new();
    input_stream(&input_path, true)?.read_to_end(&mut compressed_data)?;
    let decompressed = decompress_auto(&compressed_data)?;
    if !write_stdout(&decompressed) {
        return Ok(1);
    }
    if args.verbose && !reading_stdin {
        eprintln!("{}", gettext!("{}: -- decompressed", input_path.display()));
    }
    Ok(0)
}

/// Replace the compressed file named by `pathname` with its decompressed
/// form.
fn decompress_in_place(args: &Args, pathname: &Path) -> io::Result<i32> {
    let input_path = find_decompress_input(pathname);
    let mut input = Input::open(&input_path)?;
    if !check_hard_links(&input_path, input.entry.links, args.force) {
        return Ok(1);
    }
    let decompressed = decompress_auto(&input.read_all()?)?;

    // Refuse when the input has no known suffix to strip, so the output path
    // equals the input path: writing the decompressed bytes and then removing
    // the input would destroy the result (#C3, data loss).
    let output_path = decompress_output_path(&input_path);
    if output_path == input_path {
        diag::error(&format!(
            "{}: {}",
            input_path.display(),
            gettext("unknown suffix -- ignored")
        ));
        return Ok(1);
    }

    let status = input.replace_with(&output_path, &decompressed, args.force)?;
    if status == 0 && args.verbose {
        eprintln!(
            "{}",
            gettext!(
                "{}: -- replaced with {} ({} bytes)",
                input_path.display(),
                output_path.display(),
                decompressed.len()
            )
        );
    }
    Ok(status)
}

fn main() {
    diag::init_locale("compress");

    let program_mode = ProgramMode::detect();
    let mut args = Args::parse();

    // Apply program mode defaults
    match program_mode {
        ProgramMode::Zcat => {
            args.stdout = true;
            args.decompress = true;
        }
        ProgramMode::Uncompress => {
            args.decompress = true;
        }
        ProgramMode::Compress => {}
    }

    // If no files specified, read from stdin
    if args.files.is_empty() {
        args.files.push(PathBuf::from("-"));
    }

    // Determine algorithm for compression
    let algo = if !args.decompress {
        match get_compress_algorithm(&args) {
            Ok(a) => a,
            Err(e) => {
                diag::error(&e.to_string());
                std::process::exit(1);
            }
        }
    } else {
        Algorithm::Lzw // not used for decompression (auto-detect)
    };

    let mut exit_code = 0;

    for filename in &args.files {
        let result = if args.decompress {
            decompress_file(&args, filename)
        } else {
            compress_file(&args, filename, algo)
        };

        match result {
            Ok(code) => exit_code = merge_exit(exit_code, code),
            Err(e) => {
                exit_code = merge_exit(exit_code, 1);
                let display_name = if is_stdin(filename) {
                    gettext("standard input")
                } else {
                    filename.display().to_string()
                };
                diag::error(&format!("{}: {}", display_name, diag::io_error_text(&e)));
            }
        }
    }

    std::process::exit(exit_code)
}
