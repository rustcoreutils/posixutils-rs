//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Temporary files and directories built on POSIX `mkstemp`/`mkdtemp`.
//!
//! # Cleanup guarantees
//!
//! [`tempfile`] hands back a file with no directory entry, so the kernel
//! reclaims it when the last descriptor closes. That holds through
//! `process::exit`, a signal, or a crash, because nothing in this process has
//! to run for it to happen.
//!
//! [`TempDir`] and [`NamedTempFile`] need a name to hand to a caller, so they
//! can only unlink from `Drop`. A process that dies without unwinding leaves
//! them behind. Prefer [`tempfile`] whenever the path is not needed.
//!
//! # Security
//!
//! Names come from `mkstemp`/`mkdtemp`, which create with `O_EXCL` (mode 0600)
//! and `0700` respectively, so neither call can be made to open or reuse an
//! attacker-planted path.

use std::ffi::{c_char, CString, OsStr, OsString};
use std::fs::{self, File};
use std::io::{self, Write};
use std::os::unix::ffi::OsStringExt;
use std::os::unix::io::FromRawFd;
use std::path::{Path, PathBuf};

/// The default name prefix, matching the historical `tmp` convention.
const DEFAULT_PREFIX: &str = ".tmp";

/// The run of `X`s `mkstemp`/`mkdtemp` replace with random characters.
const RANDOM: &str = "XXXXXX";

/// Where a set-id process puts temporaries, ignoring the environment.
const SECURE_TEMP_DIR: &str = "/tmp";

/// Whether the process runs with privileges its real user does not have.
fn is_set_id() -> bool {
    // SAFETY: these four calls take no arguments and cannot fail.
    unsafe { libc::geteuid() != libc::getuid() || libc::getegid() != libc::getgid() }
}

/// The directory for temporaries created without an explicit parent.
///
/// `env::temp_dir` honors `$TMPDIR`, which a set-id process must not trust:
/// letting the caller choose the directory hands them one they own, and with
/// it the ability to swap the file out from under the privileged program.
/// glibc suppresses `TMPDIR` under `__libc_enable_secure` for this reason;
/// Rust's `env::temp_dir` does not, so do it here. `crontab` is the case in
/// this workspace -- it takes its identity from the real uid and writes into
/// the cron spool, i.e. it is meant to be installed set-id.
///
/// Split from [`is_set_id`] so the privileged branch can be asserted without
/// the test having to be set-id itself.
fn temp_dir_for(set_id: bool) -> PathBuf {
    if set_id {
        PathBuf::from(SECURE_TEMP_DIR)
    } else {
        std::env::temp_dir()
    }
}

/// [`temp_dir_for`], for this process.
fn default_temp_dir() -> PathBuf {
    temp_dir_for(is_set_id())
}

/// Reject a name fragment that would move the temporary somewhere else.
///
/// `Path::join` lets an absolute fragment replace the whole path, so a prefix
/// of `/tmp/x-` would silently ignore the requested directory; a relative one
/// containing `/` would drop the file into a subdirectory. Either breaks the
/// placement guarantee callers rely on -- `plib::io::write_atomic_mode` needs
/// the temporary beside its target so the `rename(2)` is atomic.
fn reject_separator(what: &str, fragment: &OsStr) -> io::Result<()> {
    if fragment.as_encoded_bytes().contains(&b'/') {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("temporary file {what} must not contain a path separator"),
        ));
    }
    Ok(())
}

/// Build a `<dir>/<prefix>XXXXXX<suffix>` template as a mutable C string.
///
/// Returned as a byte vector rather than a `CString` because `mkstemp` and
/// `mkdtemp` rewrite the template in place.
fn template(dir: &Path, prefix: &OsStr, suffix: &OsStr) -> io::Result<Vec<u8>> {
    reject_separator("prefix", prefix)?;
    reject_separator("suffix", suffix)?;

    let mut name = OsString::with_capacity(prefix.len() + RANDOM.len() + suffix.len());
    name.push(prefix);
    name.push(RANDOM);
    name.push(suffix);

    let path = dir.join(name).into_os_string().into_vec();
    // An interior NUL would silently truncate the path handed to libc.
    let path = CString::new(path)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "path contains a NUL byte"))?;
    Ok(path.into_bytes_with_nul())
}

/// Recover the path `mkstemp`/`mkdtemp` wrote back into `template`.
fn filled_path(template: Vec<u8>) -> PathBuf {
    let mut bytes = template;
    // Drop the trailing NUL; libc rewrote only the X run, so the length holds.
    bytes.pop();
    PathBuf::from(OsString::from_vec(bytes))
}

/// Create a uniquely named file, returning it open along with its path.
fn make_file(dir: &Path, prefix: &OsStr, suffix: &OsStr) -> io::Result<(File, PathBuf)> {
    let mut template = template(dir, prefix, suffix)?;
    let suffix_len = suffix.len() as libc::c_int;
    let ptr = template.as_mut_ptr().cast::<c_char>();

    // The descriptor must be close-on-exec: these utilities spawn children
    // constantly (cc shells out to lex/yacc, crontab launches $EDITOR), and a
    // temporary leaking into one is exactly what O_CLOEXEC prevents. Rust's
    // own File always sets it; mkstemps does not.
    //
    // SAFETY (both calls): the template is a NUL-terminated buffer ending in
    // the required X run plus `suffix_len` trailing bytes, which is what
    // mkstemps rewrites.
    #[cfg(target_os = "linux")]
    let fd = unsafe { libc::mkostemps(ptr, suffix_len, libc::O_CLOEXEC) };
    #[cfg(not(target_os = "linux"))]
    let fd = unsafe { libc::mkstemps(ptr, suffix_len) };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }

    let path = filled_path(template);

    // Where mkostemps is unavailable, set the flag afterwards. This leaves a
    // window in which a concurrent fork+exec could inherit the descriptor,
    // which is why the flag is passed at creation time above where it can be.
    #[cfg(not(target_os = "linux"))]
    {
        // SAFETY: `fd` is open and owned here; on failure it is closed before
        // returning, so it is never leaked.
        let ok = unsafe {
            let flags = libc::fcntl(fd, libc::F_GETFD);
            flags >= 0 && libc::fcntl(fd, libc::F_SETFD, flags | libc::FD_CLOEXEC) >= 0
        };
        if !ok {
            let err = io::Error::last_os_error();
            unsafe { libc::close(fd) };
            // mkstemps already created the name; failing out without removing
            // it would strand a file in the temporary directory for good.
            let _ = fs::remove_file(&path);
            return Err(err);
        }
    }

    // SAFETY: mkstemps returned a fresh descriptor that nothing else owns.
    let file = unsafe { File::from_raw_fd(fd) };
    Ok((file, path))
}

/// Create a uniquely named directory, returning its path.
fn make_dir(dir: &Path, prefix: &OsStr, suffix: &OsStr) -> io::Result<PathBuf> {
    // mkdtemp requires the template to *end* with the X run, so a suffix has to
    // be rejected rather than silently dropped.
    if !suffix.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "a temporary directory cannot have a name suffix",
        ));
    }

    let mut template = template(dir, prefix, suffix)?;
    // SAFETY: the template is a NUL-terminated buffer ending in the X run.
    let ret = unsafe { libc::mkdtemp(template.as_mut_ptr().cast::<c_char>()) };
    if ret.is_null() {
        return Err(io::Error::last_os_error());
    }

    Ok(filled_path(template))
}

/// Create an unnamed temporary file in the system temporary directory.
///
/// The file has no directory entry, so the kernel reclaims it once the last
/// descriptor closes — no destructor has to run. See the module docs.
pub fn tempfile() -> io::Result<File> {
    tempfile_in(default_temp_dir())
}

/// [`tempfile`], in a caller-chosen directory.
pub fn tempfile_in(dir: impl AsRef<Path>) -> io::Result<File> {
    let dir = dir.as_ref();

    #[cfg(target_os = "linux")]
    {
        // O_TMPFILE allocates an inode that never had a name, so there is no
        // window at all in which the file is reachable by path.
        match open_tmpfile(dir) {
            Ok(file) => return Ok(file),
            // Not every filesystem implements O_TMPFILE; fall through to the
            // portable create-then-unlink path when it does not.
            Err(e)
                if matches!(
                    e.raw_os_error(),
                    Some(libc::EOPNOTSUPP) | Some(libc::EISDIR) | Some(libc::ENOENT)
                ) => {}
            Err(e) => return Err(e),
        }
    }

    let (file, path) = make_file(dir, OsStr::new(DEFAULT_PREFIX), OsStr::new(""))?;
    // Unlink immediately: from here on the file is reachable only through the
    // descriptor, which is what makes the cleanup survive a crash. Someone
    // else having removed the entry first reaches the same end state, so it is
    // not an error.
    match fs::remove_file(&path) {
        Ok(()) => Ok(file),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(file),
        Err(e) => Err(e),
    }
}

#[cfg(target_os = "linux")]
fn open_tmpfile(dir: &Path) -> io::Result<File> {
    use std::os::unix::ffi::OsStrExt;

    let dir = CString::new(dir.as_os_str().as_bytes())
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "path contains a NUL byte"))?;

    // SAFETY: `dir` is a valid NUL-terminated path for the duration of the call.
    let fd = unsafe {
        libc::open(
            dir.as_ptr(),
            libc::O_TMPFILE | libc::O_RDWR | libc::O_CLOEXEC,
            0o600 as libc::c_int,
        )
    };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }

    // SAFETY: open returned a fresh descriptor that nothing else owns.
    Ok(unsafe { File::from_raw_fd(fd) })
}

/// Create a temporary directory in the system temporary directory.
pub fn tempdir() -> io::Result<TempDir> {
    Builder::new().tempdir()
}

/// A temporary directory, removed with its contents when dropped.
///
/// Cleanup runs from `Drop`, so it does not survive a process that dies
/// without unwinding. See the module docs.
pub struct TempDir {
    path: PathBuf,
}

impl TempDir {
    /// Create a temporary directory in the system temporary directory.
    pub fn new() -> io::Result<TempDir> {
        tempdir()
    }

    /// The directory's path.
    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl std::fmt::Debug for TempDir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TempDir").field("path", &self.path).finish()
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        // Nothing useful to report from a destructor, and a directory that is
        // already gone is the outcome we wanted anyway.
        let _ = fs::remove_dir_all(&self.path);
    }
}

/// A named temporary file, removed when dropped.
///
/// Cleanup runs from `Drop`, so it does not survive a process that dies
/// without unwinding. Use [`tempfile`] unless the path is needed.
pub struct NamedTempFile {
    path: PathBuf,
    file: Option<File>,
    /// Set once the file has been renamed into place, so `Drop` leaves it.
    persisted: bool,
}

impl NamedTempFile {
    /// Create a named temporary file in the system temporary directory.
    pub fn new() -> io::Result<NamedTempFile> {
        Builder::new().tempfile()
    }

    /// Create a named temporary file in `dir`.
    ///
    /// Use this when the file will be renamed onto a final path, so that the
    /// rename stays within one filesystem and is therefore atomic.
    pub fn new_in(dir: impl AsRef<Path>) -> io::Result<NamedTempFile> {
        Builder::new().tempfile_in(dir)
    }

    /// The file's path.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// The open file.
    pub fn as_file(&self) -> &File {
        self.file.as_ref().expect("file is taken only by persist")
    }

    /// The open file, mutably.
    pub fn as_file_mut(&mut self) -> &mut File {
        self.file.as_mut().expect("file is taken only by persist")
    }

    /// Remove the file now, reporting any failure.
    ///
    /// `Drop` does the same thing but has nowhere to report to, so use this
    /// where the removal failing is worth noticing.
    pub fn close(mut self) -> io::Result<()> {
        // Close the descriptor first, then unlink. The "handled" flag is set
        // only once the removal succeeded, so a caller that ignores the error
        // still gets the destructor's attempt.
        drop(self.file.take());
        fs::remove_file(&self.path)?;
        self.persisted = true;
        Ok(())
    }

    /// Rename the file to `path`, giving up ownership of the cleanup.
    ///
    /// `path` must be on the same filesystem, since the rename has to be
    /// atomic. **Anything already at `path` is replaced** -- that is the point
    /// for `plib::io::write_atomic_mode`, but it means this is not a way to
    /// create a file only if one is absent. On failure the temporary is handed
    /// back inside the error so the caller can retry or let it be cleaned up.
    pub fn persist(mut self, path: impl AsRef<Path>) -> Result<File, PersistError> {
        match fs::rename(&self.path, path.as_ref()) {
            Ok(()) => {
                self.persisted = true;
                Ok(self.file.take().expect("file is taken only by persist"))
            }
            Err(error) => Err(PersistError { error, file: self }),
        }
    }
}

impl std::fmt::Debug for NamedTempFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NamedTempFile")
            .field("path", &self.path)
            .finish()
    }
}

impl Write for NamedTempFile {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.as_file_mut().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.as_file_mut().flush()
    }
}

impl Drop for NamedTempFile {
    fn drop(&mut self) {
        if !self.persisted {
            let _ = fs::remove_file(&self.path);
        }
    }
}

/// The error from [`NamedTempFile::persist`], carrying the file back.
pub struct PersistError {
    /// Why the rename failed.
    pub error: io::Error,
    /// The temporary, still intact.
    pub file: NamedTempFile,
}

impl std::fmt::Debug for PersistError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PersistError")
            .field("error", &self.error)
            .field("file", &self.file)
            .finish()
    }
}

impl std::fmt::Display for PersistError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "could not persist temporary file: {}", self.error)
    }
}

impl std::error::Error for PersistError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.error)
    }
}

impl From<PersistError> for io::Error {
    fn from(e: PersistError) -> io::Error {
        e.error
    }
}

/// Names the parts of a temporary's filename before creating it.
pub struct Builder {
    prefix: OsString,
    suffix: OsString,
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            prefix: OsString::from(DEFAULT_PREFIX),
            suffix: OsString::new(),
        }
    }

    /// Set the text before the random component.
    pub fn prefix<S: AsRef<OsStr> + ?Sized>(&mut self, prefix: &S) -> &mut Builder {
        self.prefix = prefix.as_ref().to_os_string();
        self
    }

    /// Set the text after the random component, typically a file extension.
    ///
    /// Only meaningful for files; [`Builder::tempdir`] rejects a suffix,
    /// because `mkdtemp` requires the random run to end the template.
    pub fn suffix<S: AsRef<OsStr> + ?Sized>(&mut self, suffix: &S) -> &mut Builder {
        self.suffix = suffix.as_ref().to_os_string();
        self
    }

    /// Create a temporary directory in the system temporary directory.
    pub fn tempdir(&self) -> io::Result<TempDir> {
        self.tempdir_in(default_temp_dir())
    }

    /// Create a temporary directory in `dir`.
    pub fn tempdir_in(&self, dir: impl AsRef<Path>) -> io::Result<TempDir> {
        let path = make_dir(dir.as_ref(), &self.prefix, &self.suffix)?;
        Ok(TempDir { path })
    }

    /// Create a named temporary file in the system temporary directory.
    pub fn tempfile(&self) -> io::Result<NamedTempFile> {
        self.tempfile_in(default_temp_dir())
    }

    /// Create a named temporary file in `dir`.
    pub fn tempfile_in(&self, dir: impl AsRef<Path>) -> io::Result<NamedTempFile> {
        let (file, path) = make_file(dir.as_ref(), &self.prefix, &self.suffix)?;
        Ok(NamedTempFile {
            path,
            file: Some(file),
            persisted: false,
        })
    }
}

impl Default for Builder {
    fn default() -> Builder {
        Builder::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{Read, Seek, SeekFrom};
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::{MetadataExt, PermissionsExt};
    use std::os::unix::io::AsRawFd;

    #[test]
    fn tempdir_exists_then_is_removed_with_contents() {
        let path = {
            let dir = tempdir().unwrap();
            assert!(dir.path().is_dir());
            fs::write(dir.path().join("nested"), b"x").unwrap();
            fs::create_dir(dir.path().join("sub")).unwrap();
            fs::write(dir.path().join("sub/deep"), b"y").unwrap();
            dir.path().to_path_buf()
        };
        assert!(!path.exists(), "TempDir::drop must remove the tree");
    }

    #[test]
    fn tempdir_is_private() {
        let dir = tempdir().unwrap();
        let mode = fs::metadata(dir.path()).unwrap().permissions().mode();
        assert_eq!(mode & 0o777, 0o700, "mkdtemp must create the dir 0700");
    }

    #[test]
    fn tempdir_names_use_prefix_and_are_distinct() {
        let a = Builder::new().prefix("pfx-").tempdir().unwrap();
        let b = Builder::new().prefix("pfx-").tempdir().unwrap();
        for dir in [&a, &b] {
            let name = dir.path().file_name().unwrap().to_str().unwrap();
            assert!(name.starts_with("pfx-"), "{name}");
            assert_eq!(name.len(), "pfx-".len() + RANDOM.len(), "{name}");
        }
        assert_ne!(a.path(), b.path());
    }

    #[test]
    fn tempdir_in_places_the_dir_under_the_given_parent() {
        let parent = tempdir().unwrap();
        let child = Builder::new().tempdir_in(parent.path()).unwrap();
        assert_eq!(child.path().parent().unwrap(), parent.path());
    }

    #[test]
    fn tempdir_rejects_a_suffix() {
        // mkdtemp needs the X run at the end, so a suffix cannot be honored and
        // must not be silently dropped.
        let err = Builder::new().suffix(".d").tempdir().unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
    }

    #[test]
    fn named_tempfile_round_trips_and_is_removed() {
        let path = {
            let mut tmp = NamedTempFile::new().unwrap();
            tmp.write_all(b"hello").unwrap();
            tmp.flush().unwrap();

            let mode = tmp.as_file().metadata().unwrap().permissions().mode();
            assert_eq!(mode & 0o777, 0o600, "mkstemp must create the file 0600");

            tmp.as_file_mut().seek(SeekFrom::Start(0)).unwrap();
            let mut got = String::new();
            tmp.as_file_mut().read_to_string(&mut got).unwrap();
            assert_eq!(got, "hello");

            tmp.path().to_path_buf()
        };
        assert!(!path.exists(), "NamedTempFile::drop must remove the file");
    }

    #[test]
    fn named_tempfile_honors_prefix_and_suffix() {
        let tmp = Builder::new()
            .prefix("src-")
            .suffix(".c")
            .tempfile()
            .unwrap();
        let name = tmp.path().file_name().unwrap().to_str().unwrap();
        assert!(name.starts_with("src-"), "{name}");
        assert!(name.ends_with(".c"), "{name}");
        assert_eq!(
            name.len(),
            "src-".len() + RANDOM.len() + ".c".len(),
            "{name}"
        );
    }

    #[test]
    fn persist_renames_and_disarms_cleanup() {
        let dir = tempdir().unwrap();
        let target = dir.path().join("final");

        let mut tmp = NamedTempFile::new_in(dir.path()).unwrap();
        let tmp_path = tmp.path().to_path_buf();
        tmp.write_all(b"payload").unwrap();
        tmp.persist(&target).unwrap();

        assert!(!tmp_path.exists(), "the temporary name must be gone");
        assert_eq!(fs::read(&target).unwrap(), b"payload");
    }

    #[test]
    fn failed_persist_returns_the_file_intact() {
        let tmp = NamedTempFile::new().unwrap();
        let tmp_path = tmp.path().to_path_buf();

        // Renaming into a directory that does not exist cannot succeed.
        let err = tmp
            .persist(Path::new("/nonexistent-dir-for-persist/final"))
            .unwrap_err();
        assert!(err.file.path().exists(), "the temporary must survive");
        assert_eq!(err.file.path(), tmp_path);
    }

    #[test]
    fn temporaries_are_close_on_exec() {
        // A temporary must not survive into a spawned child; every one of
        // these utilities forks and execs.
        let named = NamedTempFile::new().unwrap();
        let anon = tempfile().unwrap();
        for fd in [named.as_file().as_raw_fd(), anon.as_raw_fd()] {
            let flags = unsafe { libc::fcntl(fd, libc::F_GETFD) };
            assert!(flags >= 0, "F_GETFD failed");
            assert_eq!(flags & libc::FD_CLOEXEC, libc::FD_CLOEXEC, "fd {fd}");
        }
    }

    #[test]
    fn a_failed_close_leaves_cleanup_armed() {
        let dir = tempdir().unwrap();
        let tmp = NamedTempFile::new_in(dir.path()).unwrap();
        let path = tmp.path().to_path_buf();

        // Remove it behind close()'s back so the unlink cannot succeed.
        fs::remove_file(&path).unwrap();
        assert!(tmp.close().is_err(), "close must report the failed unlink");
        assert!(!path.exists());
    }

    #[test]
    fn tempfile_has_no_directory_entry() {
        let mut file = tempfile().unwrap();
        file.write_all(b"anonymous").unwrap();
        file.seek(SeekFrom::Start(0)).unwrap();
        let mut got = String::new();
        file.read_to_string(&mut got).unwrap();
        assert_eq!(got, "anonymous");

        // This is the property that makes the cleanup survive a crash: the
        // inode is already unreferenced by any directory.
        assert_eq!(
            file.metadata().unwrap().nlink(),
            0,
            "an anonymous temporary must have no links"
        );
    }

    #[test]
    fn tempfile_in_uses_the_given_directory() {
        let dir = tempdir().unwrap();
        let file = tempfile_in(dir.path()).unwrap();
        assert_eq!(file.metadata().unwrap().nlink(), 0);
        // Nothing was left behind under the chosen parent.
        assert_eq!(fs::read_dir(dir.path()).unwrap().count(), 0);
    }

    #[test]
    fn a_prefix_cannot_escape_the_chosen_directory() {
        let dir = tempdir().unwrap();

        // Absolute: `Path::join` would otherwise discard `dir` entirely.
        let err = Builder::new()
            .prefix("/tmp/ESCAPED-")
            .tempfile_in(dir.path())
            .unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);

        // Relative but still a separator: would land in a subdirectory.
        let err = Builder::new()
            .prefix("sub/NESTED-")
            .tempfile_in(dir.path())
            .unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);

        let err = Builder::new()
            .suffix("/etc/passwd")
            .tempfile_in(dir.path())
            .unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);

        let err = Builder::new()
            .prefix("/tmp/ESCAPED-")
            .tempdir_in(dir.path())
            .unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);

        // Nothing was created under either candidate location.
        assert_eq!(fs::read_dir(dir.path()).unwrap().count(), 0);
    }

    #[test]
    fn a_set_id_process_ignores_tmpdir() {
        // The security property: under set-id the directory is a fixed path,
        // so nothing the caller puts in the environment can steer a privileged
        // temporary into a directory they control. Asserted through the
        // parameter rather than the process, which a test cannot make set-id.
        assert_eq!(temp_dir_for(true), Path::new(SECURE_TEMP_DIR));
    }

    #[test]
    fn an_ordinary_process_honors_tmpdir() {
        assert_eq!(temp_dir_for(false), std::env::temp_dir());
    }

    #[test]
    fn this_test_process_is_not_set_id() {
        // Guards the assumption the rest of these tests rest on: they create
        // temporaries under $TMPDIR via the non-set-id branch.
        assert!(!is_set_id());
        assert_eq!(default_temp_dir(), std::env::temp_dir());
    }

    #[test]
    fn a_nul_in_the_path_is_rejected() {
        let err = Builder::new()
            .prefix(OsStr::from_bytes(b"bad\0name"))
            .tempfile()
            .unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
    }
}
