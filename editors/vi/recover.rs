//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Crash/hangup recovery for vi/ex.
//!
//! When the editor receives a hangup (SIGHUP), termination (SIGTERM), or an
//! unexpected end-of-input while the buffer has unsaved changes — or when the
//! user runs `:preserve` — a copy of the edit buffer is written to a recovery
//! file under a per-user recovery directory, and (best effort) the user is
//! mailed a notification. `vi -r` (or `:recover`) reads the saved buffer back.
//!
//! Recovery files live in `<base>/vi.recover.<uid>` where `<base>` is the
//! `directory` option (default `$TMPDIR` or `/tmp`). The directory is created
//! mode 0700 and is verified before use, through one descriptor, to be a real
//! directory (not a symlink) owned by the current user; one of ours with looser
//! permissions is tightened to 0700, and refused if that does not take. Every
//! file in it is then created, read, listed and removed relative to that
//! descriptor, so a directory renamed or swapped above it after the check is
//! never used. Recovery files are created `O_EXCL|O_NOFOLLOW`, so a link
//! planted at their name is never followed.
//!
//! Recovery file format (text):
//! ```text
//! VI-RECOVER-1
//! path: /abs/path/to/original        (empty if the buffer was unnamed)
//! time: <unix epoch seconds>
//! user: <login name>
//! ---
//! <buffer contents>
//! ```

use std::ffi::{CStr, CString};
use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, Read, Write};
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

const MAGIC: &str = "VI-RECOVER-1";

/// Metadata describing one recoverable edit buffer.
pub struct RecoverInfo {
    /// Path of the recovery file itself, for messages. Reading and removing
    /// go through the verified directory instead: see [`RecoverDir`].
    pub recovery_path: PathBuf,
    /// The directory it was listed in, kept so reading and removing act on
    /// the directory that was checked.
    dir: Rc<RecoverDir>,
    /// The file's name in the recovery directory.
    name: CString,
    /// Original file path the buffer was editing, if any.
    pub orig_path: Option<String>,
    /// Modification time (unix epoch seconds).
    pub modified: u64,
}

/// The default base directory for recovery files (`$TMPDIR` or `/tmp`).
pub fn default_base() -> String {
    std::env::var("TMPDIR")
        .ok()
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "/tmp".to_string())
}

/// The per-user recovery directory, verified once and then used only through
/// its descriptor.
///
/// Every file under it is created, opened, listed and removed relative to
/// that descriptor (`openat`, `unlinkat`, `fdopendir`), so what is checked is
/// what is used: if any directory above it can be renamed by someone else,
/// the name may come to mean another directory, but the descriptor cannot.
pub struct RecoverDir {
    path: PathBuf,
    fd: OwnedFd,
}

/// The name of the per-user recovery directory under `base`. Named for the
/// effective uid, which is also the owner it must have: what this process
/// creates there is owned by that uid.
fn dir_path(base: &str) -> PathBuf {
    let euid = unsafe { libc::geteuid() };
    Path::new(base).join(format!("vi.recover.{}", euid))
}

/// The permission bits of `st`.
fn perm_bits(st: &libc::stat) -> u32 {
    // Cast needed: `mode_t` is u16 on macOS and u32 on Linux.
    #[allow(clippy::unnecessary_cast)]
    let mode = st.st_mode as u32;
    mode & 0o7777
}

/// Whether `st` is a regular file.
fn is_regular(st: &libc::stat) -> bool {
    st.st_mode & libc::S_IFMT == libc::S_IFREG
}

fn refuse<T>(msg: &str) -> io::Result<T> {
    Err(io::Error::new(io::ErrorKind::PermissionDenied, msg))
}

fn c_name(name: &str) -> io::Result<CString> {
    CString::new(name).map_err(|_| io::Error::from_raw_os_error(libc::EINVAL))
}

fn open_raw(path: &CStr, flags: libc::c_int) -> io::Result<OwnedFd> {
    let fd = unsafe { libc::open(path.as_ptr(), flags) };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(unsafe { OwnedFd::from_raw_fd(fd) })
}

/// Resolve (and create, mode 0700) the per-user recovery directory under
/// `base`, verifying through one descriptor that it is a real directory (not
/// a symlink) owned by the effective user, and tightening it to 0700 if it is
/// ours and looser. Returns an error if any of that does not hold.
pub fn recover_dir(base: &str) -> io::Result<Rc<RecoverDir>> {
    use std::os::unix::fs::DirBuilderExt;

    let path = dir_path(base);
    let made = match fs::DirBuilder::new().mode(0o700).create(&path) {
        Err(e) if e.kind() == io::ErrorKind::NotFound => {
            // The base itself is missing: it gets ordinary permissions, and
            // only the leaf is private.
            fs::create_dir_all(base)?;
            fs::DirBuilder::new().mode(0o700).create(&path)
        }
        other => other,
    };
    match made {
        Ok(()) => open_dir(path),
        Err(e) if e.kind() == io::ErrorKind::AlreadyExists => open_dir(path),
        // `mkdir` may report EROFS or EACCES for a name that already exists;
        // the directory may well be there and usable, so try it first.
        Err(e) => open_dir(path).map_err(|_| e),
    }
}

/// Open and verify an existing recovery directory. `O_NOFOLLOW` refuses a
/// symlink outright, so a planted link is never followed (a path-based chmod
/// before the check used to tighten whatever directory a link named).
fn open_dir(path: PathBuf) -> io::Result<Rc<RecoverDir>> {
    let cpath = CString::new(path.as_os_str().as_bytes())
        .map_err(|_| io::Error::from_raw_os_error(libc::EINVAL))?;
    let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC;
    let euid = unsafe { libc::geteuid() };

    let fd = match open_raw(&cpath, flags) {
        Ok(fd) => fd,
        Err(e) if e.raw_os_error() == Some(libc::EACCES) => {
            // Ours but unreadable (0300, 0000): repair it through a
            // search-only descriptor, then open it for real and make sure it
            // is still the directory that was repaired.
            let search = open_raw(
                &cpath,
                plib::madefs::SEARCH_ONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC,
            )
            .map_err(|_| e)?;
            let st = plib::madefs::fstat(search.as_raw_fd())?;
            if st.st_uid != euid {
                return refuse("recovery directory not owned by the current user");
            }
            plib::madefs::chmod_fd(search.as_raw_fd(), 0o700)?;
            let fd = open_raw(&cpath, flags)?;
            let again = plib::madefs::fstat(fd.as_raw_fd())?;
            if (again.st_dev, again.st_ino) != (st.st_dev, st.st_ino) {
                return refuse("recovery directory changed while it was repaired");
            }
            fd
        }
        Err(e) => {
            // Which errno a symlink gives under `O_NOFOLLOW` varies (ELOOP,
            // EMLINK, EFTYPE); ask the name what it is instead.
            return match fs::symlink_metadata(&path) {
                Ok(md) if !md.is_dir() => refuse("recovery path is not a directory"),
                _ => Err(e),
            };
        }
    };

    let st = plib::madefs::fstat(fd.as_raw_fd())?;
    if st.st_uid != euid {
        return refuse("recovery directory not owned by the current user");
    }
    if perm_bits(&st) != 0o700 {
        // Best effort: what matters is that nobody else has access. A
        // filesystem may refuse the chmod, or accept it and ignore it.
        let _ = plib::madefs::chmod_fd(fd.as_raw_fd(), 0o700);
        if perm_bits(&plib::madefs::fstat(fd.as_raw_fd())?) & 0o077 != 0 {
            return refuse("recovery directory cannot be made private");
        }
    }
    Ok(Rc::new(RecoverDir { path, fd }))
}

impl RecoverDir {
    /// Create `name` new, `0600`: `O_EXCL|O_NOFOLLOW` refuses a link planted
    /// at the name instead of writing through it.
    fn create(&self, name: &CStr) -> io::Result<File> {
        let flags =
            libc::O_WRONLY | libc::O_CREAT | libc::O_EXCL | libc::O_NOFOLLOW | libc::O_CLOEXEC;
        let mode: libc::c_uint = 0o600;
        let fd = unsafe { libc::openat(self.fd.as_raw_fd(), name.as_ptr(), flags, mode) };
        if fd < 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(unsafe { File::from_raw_fd(fd) })
    }

    /// Open `name` for reading: never through a symlink, and only a regular
    /// file of ours. `O_NONBLOCK` keeps a FIFO left in a directory that was
    /// once loose from hanging every editor start (each one lists).
    fn open(&self, name: &CStr) -> io::Result<File> {
        let flags = libc::O_RDONLY | libc::O_NOFOLLOW | libc::O_NONBLOCK | libc::O_CLOEXEC;
        let fd = unsafe { libc::openat(self.fd.as_raw_fd(), name.as_ptr(), flags) };
        if fd < 0 {
            return Err(io::Error::last_os_error());
        }
        let file = unsafe { File::from_raw_fd(fd) };
        let st = plib::madefs::fstat(file.as_raw_fd())?;
        if !is_regular(&st) || st.st_uid != unsafe { libc::geteuid() } {
            return refuse("not a recovery file of ours");
        }
        Ok(file)
    }

    fn unlink(&self, name: &CStr) {
        unsafe { libc::unlinkat(self.fd.as_raw_fd(), name.as_ptr(), 0) };
    }

    /// The names of the entries, `.` and `..` left out.
    fn names(&self) -> io::Result<Vec<CString>> {
        // `fdopendir` takes ownership of its descriptor, so give it a copy.
        let dup = unsafe { libc::fcntl(self.fd.as_raw_fd(), libc::F_DUPFD_CLOEXEC, 0) };
        if dup < 0 {
            return Err(io::Error::last_os_error());
        }
        let dirp = unsafe { libc::fdopendir(dup) };
        if dirp.is_null() {
            let err = io::Error::last_os_error();
            unsafe { libc::close(dup) };
            return Err(err);
        }
        // The copy shares the original's offset; start from the top
        // whatever an earlier listing left it at.
        unsafe { libc::rewinddir(dirp) };
        let mut out = Vec::new();
        let result = loop {
            errno::set_errno(errno::Errno(0));
            let ent = unsafe { libc::readdir(dirp) };
            if ent.is_null() {
                // NULL is both the end and an error; errno tells them apart.
                let err = io::Error::last_os_error();
                break match err.raw_os_error() {
                    Some(0) | None => Ok(out),
                    Some(_) => Err(err),
                };
            }
            // Read `d_name` through its offset, never by dereferencing the
            // `dirent`, whose size can be shorter than the type's.
            const OFFSET: isize = std::mem::offset_of!(libc::dirent, d_name) as isize;
            let name = unsafe { CStr::from_ptr(ent.byte_offset(OFFSET).cast()) };
            if name.to_bytes() != b"." && name.to_bytes() != b".." {
                out.push(name.to_owned());
            }
        };
        unsafe { libc::closedir(dirp) };
        result
    }

    fn path_of(&self, name: &CStr) -> PathBuf {
        self.path.join(std::ffi::OsStr::from_bytes(name.to_bytes()))
    }
}

fn now_secs() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// Write `text` to a new recovery file under `base` for the (optional) original
/// path. Returns the path of the recovery file.
pub fn preserve(orig: Option<&Path>, text: &str, base: &str) -> io::Result<PathBuf> {
    let dir = recover_dir(base)?;
    let pid = std::process::id();
    let secs = now_secs();
    let name = orig
        .and_then(|p| p.file_name())
        .and_then(|s| s.to_str())
        .unwrap_or("buffer");
    let safe: String = name
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || matches!(c, '.' | '-' | '_') {
                c
            } else {
                '_'
            }
        })
        .collect();
    // A name already taken -- a second preserve within the same second, or
    // something planted -- gets a numeric suffix rather than being reused.
    let mut n = 0u32;
    let (name, mut f) = loop {
        let name = match n {
            0 => c_name(&format!("recover.{}.{}.{}", safe, pid, secs))?,
            _ => c_name(&format!("recover.{}.{}.{}.{}", safe, pid, secs, n))?,
        };
        match dir.create(&name) {
            Ok(f) => break (name, f),
            Err(e) if e.kind() == io::ErrorKind::AlreadyExists && n < 100 => n += 1,
            Err(e) => return Err(e),
        }
    };
    let written = (|| -> io::Result<()> {
        writeln!(f, "{}", MAGIC)?;
        match orig {
            Some(p) => writeln!(f, "path: {}", p.display())?,
            None => writeln!(f, "path:")?,
        }
        writeln!(f, "time: {}", secs)?;
        writeln!(f, "user: {}", std::env::var("USER").unwrap_or_default())?;
        writeln!(f, "---")?;
        f.write_all(text.as_bytes())?;
        if !text.is_empty() && !text.ends_with('\n') {
            writeln!(f)?;
        }
        // On disk before it is reported saved: this runs on a hangup.
        f.sync_all()
    })();
    if let Err(e) = written {
        // A short file would be offered by `vi -r` as a whole buffer.
        dir.unlink(&name);
        return Err(e);
    }
    Ok(dir.path_of(&name))
}

/// Parse the header of a recovery file into a [`RecoverInfo`].
fn read_header(dir: &Rc<RecoverDir>, name: &CString) -> io::Result<RecoverInfo> {
    // The header is a few short lines; cap what is read so a huge file with
    // no newline is not pulled into memory just to be rejected.
    let mut r = BufReader::new(dir.open(name)?.take(64 * 1024));
    let mut line = String::new();
    r.read_line(&mut line)?;
    if line.trim_end() != MAGIC {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "not a recovery file",
        ));
    }
    let mut orig_path = None;
    let mut modified = 0u64;
    loop {
        line.clear();
        if r.read_line(&mut line)? == 0 {
            break;
        }
        let l = line.trim_end();
        if l == "---" {
            break;
        }
        if let Some(v) = l.strip_prefix("path:") {
            let v = v.trim();
            if !v.is_empty() {
                orig_path = Some(v.to_string());
            }
        } else if let Some(v) = l.strip_prefix("time:") {
            modified = v.trim().parse().unwrap_or(0);
        }
    }
    Ok(RecoverInfo {
        recovery_path: dir.path_of(name),
        dir: Rc::clone(dir),
        name: name.clone(),
        orig_path,
        modified,
    })
}

/// List recoverable buffers under `base`, newest first.
///
/// Listing never creates the recovery directory: every editor start lists
/// (to prune stale files), and one that preserves nothing must leave nothing
/// behind under `base`.
pub fn list(base: &str) -> Vec<RecoverInfo> {
    let mut out = Vec::new();
    let Ok(dir) = open_dir(dir_path(base)) else {
        return out;
    };
    for name in dir.names().unwrap_or_default() {
        if name.to_bytes().starts_with(b"recover.") {
            if let Ok(info) = read_header(&dir, &name) {
                out.push(info);
            }
        }
    }
    // Newest first.
    out.sort_by_key(|i| std::cmp::Reverse(i.modified));
    out
}

/// Read the buffer text (the body) from a recovery file.
pub fn load_body(info: &RecoverInfo) -> io::Result<String> {
    let mut r = BufReader::new(info.dir.open(&info.name)?);
    let mut line = String::new();
    loop {
        line.clear();
        if r.read_line(&mut line)? == 0 {
            break;
        }
        if line.trim_end() == "---" {
            break;
        }
    }
    let mut body = String::new();
    r.read_to_string(&mut body)?;
    Ok(body)
}

/// Find the newest recovery file under `base` for `orig` (or the newest of any).
pub fn find_for(base: &str, orig: Option<&str>) -> Option<RecoverInfo> {
    let all = list(base);
    match orig {
        Some(want) => all
            .into_iter()
            .find(|i| i.orig_path.as_deref() == Some(want)),
        None => all.into_iter().next(),
    }
}

/// Remove a recovery file (e.g. after a successful recovery).
pub fn remove(info: &RecoverInfo) {
    info.dir.unlink(&info.name);
}

/// Remove recovery files under `base` older than `max_age_secs`.
pub fn cleanup_stale(base: &str, max_age_secs: u64) {
    let now = now_secs();
    for info in list(base) {
        if now.saturating_sub(info.modified) > max_age_secs {
            remove(&info);
        }
    }
}

/// Best-effort email notification that a buffer was preserved.
pub fn notify(orig: Option<&Path>) {
    use std::process::{Command, Stdio};
    let user = match std::env::var("USER") {
        Ok(u) if !u.is_empty() => u,
        _ => return,
    };
    // Defend against RFC-822 header injection via $USER / the file path when
    // piping to `sendmail -t`.
    if user.contains(['\n', '\r']) {
        return;
    }
    let fname = orig
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| "an unnamed buffer".to_string())
        .replace(['\n', '\r'], " ");
    let body = format!(
        "From: vi\nTo: {user}\nSubject: editor recovery\n\n\
         A copy of an edit session for {fname} was saved.\n\
         Run \"vi -r\" to recover it.\n"
    );
    for prog in ["/usr/sbin/sendmail", "/usr/lib/sendmail", "sendmail"] {
        if let Ok(mut child) = Command::new(prog)
            .arg("-t")
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
        {
            if let Some(mut sin) = child.stdin.take() {
                let _ = sin.write_all(body.as_bytes());
            }
            let _ = child.wait();
            return;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::fs::{MetadataExt, PermissionsExt};

    #[test]
    fn test_preserve_and_load_roundtrip() {
        let td = plib::tmp::tempdir().unwrap();
        let base = td.path().to_str().unwrap();

        let orig = PathBuf::from("/home/user/notes.txt");
        let text = "line one\nline two\n";
        let rec = preserve(Some(&orig), text, base).unwrap();
        assert!(rec.exists());

        let info = find_for(base, Some("/home/user/notes.txt")).expect("recoverable found");
        assert_eq!(info.orig_path.as_deref(), Some("/home/user/notes.txt"));

        assert_eq!(info.recovery_path, rec);

        let body = load_body(&info).unwrap();
        assert_eq!(body, text);

        remove(&info);
        assert!(!rec.exists());
    }

    #[test]
    fn test_list_does_not_create_the_recovery_directory() {
        let td = plib::tmp::tempdir().unwrap();
        let base = td.path().to_str().unwrap();

        assert!(list(base).is_empty());
        cleanup_stale(base, 0);
        assert_eq!(
            fs::read_dir(td.path()).unwrap().count(),
            0,
            "listing must leave nothing behind under the base directory"
        );
    }

    #[test]
    fn test_recover_dir_rejects_symlink() {
        let td = plib::tmp::tempdir().unwrap();
        let base = td.path();
        // Pre-create the recovery dir name as a symlink to elsewhere.
        let target = td.path().join("elsewhere");
        fs::create_dir(&target).unwrap();
        let link = dir_path(base.to_str().unwrap());
        std::os::unix::fs::symlink(&target, &link).unwrap();
        fs::set_permissions(&target, fs::Permissions::from_mode(0o755)).unwrap();

        assert!(recover_dir(base.to_str().unwrap()).is_err());
        // Refusing is not enough: the directory the link names must not have
        // been touched on the way.
        let mode = fs::metadata(&target).unwrap().mode() & 0o777;
        assert_eq!(mode, 0o755, "recover_dir chmodded the symlink's target");
    }

    #[test]
    fn test_recover_dir_repairs_missing_owner_bits() {
        let td = plib::tmp::tempdir().unwrap();
        let dir = dir_path(td.path().to_str().unwrap());
        fs::create_dir(&dir).unwrap();
        for mode in [0o500, 0o300, 0o000] {
            fs::set_permissions(&dir, fs::Permissions::from_mode(mode)).unwrap();
            assert_eq!(recover_dir(td.path().to_str().unwrap()).unwrap().path, dir);
            assert_eq!(fs::metadata(&dir).unwrap().mode() & 0o777, 0o700);
        }
    }

    #[test]
    fn test_list_skips_fifo_and_lists_twice() {
        let td = plib::tmp::tempdir().unwrap();
        let base = td.path().to_str().unwrap();
        preserve(None, "text\n", base).unwrap();
        let dir = recover_dir(base).unwrap();
        let fifo = c_name(&dir.path.join("recover.fifo").to_string_lossy()).unwrap();
        assert_eq!(unsafe { libc::mkfifo(fifo.as_ptr(), 0o600) }, 0);

        // A FIFO would block a plain open forever; it must just be skipped.
        assert_eq!(list(base).len(), 1);
        assert_eq!(dir.names().unwrap().len(), 2);
        assert_eq!(
            dir.names().unwrap().len(),
            2,
            "a second listing sees the same"
        );
    }

    #[test]
    fn test_preserve_does_not_follow_planted_symlink() {
        let td = plib::tmp::tempdir().unwrap();
        let base = td.path().to_str().unwrap();
        let dir = recover_dir(base).unwrap().path.clone();
        let victim = td.path().join("victim");
        fs::write(&victim, "keep\n").unwrap();
        let pid = std::process::id();
        // Plant links at the names this second (and the next) would use.
        let secs = now_secs();
        for t in secs..secs + 3 {
            let name = dir.join(format!("recover.notes.txt.{}.{}", pid, t));
            std::os::unix::fs::symlink(&victim, name).unwrap();
        }
        let rec = preserve(Some(Path::new("/x/notes.txt")), "secret\n", base).unwrap();
        assert_eq!(fs::read_to_string(&victim).unwrap(), "keep\n");
        assert!(!fs::symlink_metadata(&rec).unwrap().file_type().is_symlink());
        assert_eq!(fs::metadata(&rec).unwrap().mode() & 0o777, 0o600);
    }

    #[test]
    fn test_recover_dir_tightens_own_directory() {
        let td = plib::tmp::tempdir().unwrap();
        let base = td.path();
        let dir = dir_path(base.to_str().unwrap());
        fs::create_dir(&dir).unwrap();
        fs::set_permissions(&dir, fs::Permissions::from_mode(0o755)).unwrap();

        assert_eq!(recover_dir(base.to_str().unwrap()).unwrap().path, dir);
        assert_eq!(fs::metadata(&dir).unwrap().mode() & 0o777, 0o700);
    }
}
