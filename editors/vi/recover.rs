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
            // Ours but unreadable (0300, 0000): repair it, then open it for
            // real and make sure it is still the directory that was
            // repaired.
            let search = open_raw(
                &cpath,
                plib::madefs::SEARCH_ONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC,
            );
            let repaired = match search {
                Ok(search) => {
                    let st = plib::madefs::fstat(search.as_raw_fd())?;
                    if st.st_uid != euid {
                        return refuse("recovery directory not owned by the current user");
                    }
                    plib::madefs::chmod_fd(search.as_raw_fd(), 0o700)?;
                    (st.st_dev as u64, st.st_ino as u64)
                }
                // macOS's O_SEARCH needs search permission, which 0000 does
                // not give (Linux's O_PATH needs none): repair it by name.
                Err(_) => repair_by_name(&path, &cpath, euid).map_err(|_| e)?,
            };
            let fd = open_raw(&cpath, flags)?;
            let again = plib::madefs::fstat(fd.as_raw_fd())?;
            if (again.st_dev as u64, again.st_ino as u64) != repaired {
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

/// Make the directory at `path` (`cpath`) 0700 by name, for when no
/// descriptor can be had for it: only if it is a directory, not a link, and
/// owned by `euid`, and through `fchmodat(AT_SYMLINK_NOFOLLOW)`, so a link
/// put in its place since is not followed.  Returns its device and inode,
/// for the caller to check it opens the same directory after.
fn repair_by_name(path: &Path, cpath: &CStr, euid: libc::uid_t) -> io::Result<(u64, u64)> {
    use std::os::unix::fs::MetadataExt;
    let md = fs::symlink_metadata(path)?;
    if !md.is_dir() {
        return refuse("recovery path is not a directory");
    }
    if md.uid() != euid {
        return refuse("recovery directory not owned by the current user");
    }
    // SAFETY: a NUL-terminated path, relative to the working directory.
    let rc = unsafe {
        libc::fchmodat(
            libc::AT_FDCWD,
            cpath.as_ptr(),
            0o700,
            libc::AT_SYMLINK_NOFOLLOW,
        )
    };
    if rc != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok((md.dev(), md.ino()))
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
///
/// The original path is recorded as its [`path_key`], so `vi -r` finds it
/// from any directory, through `..` or a symbolic link to its directory.
pub fn preserve(orig: Option<&Path>, text: &str, base: &str) -> io::Result<PathBuf> {
    let key = orig.map(path_key);
    let orig = key.as_deref();
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
        // The pathname as its bytes, so a name that is not UTF-8 recovers
        // to the same file -- with `\` and newline escaped, so a name holding
        // a newline cannot end the field or forge the `---` that starts the
        // body.
        f.write_all(b"path: ")?;
        if let Some(p) = orig {
            f.write_all(&escape_field(p.as_os_str().as_bytes()))?;
        }
        f.write_all(b"\n")?;
        writeln!(f, "time: {}", secs)?;
        writeln!(f, "user: {}", std::env::var("USER").unwrap_or_default())?;
        writeln!(f, "---")?;
        // The buffer as the bytes it stands for (see `crate::bytes`).
        f.write_all(&crate::bytes::encode(text))?;
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

/// A header field's bytes with `\` and newline escaped as `\\` and `\n`.
fn escape_field(raw: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(raw.len());
    for &b in raw {
        match b {
            b'\\' => out.extend_from_slice(b"\\\\"),
            b'\n' => out.extend_from_slice(b"\\n"),
            b => out.push(b),
        }
    }
    out
}

/// Undo [`escape_field`].
fn unescape_field(field: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(field.len());
    let mut bytes = field.iter();
    while let Some(&b) = bytes.next() {
        if b == b'\\' {
            match bytes.next() {
                Some(b'n') => out.push(b'\n'),
                Some(&other) => out.push(other),
                None => out.push(b'\\'),
            }
        } else {
            out.push(b);
        }
    }
    out
}

/// Parse the header of a recovery file into a [`RecoverInfo`].
fn read_header(dir: &Rc<RecoverDir>, name: &CString) -> io::Result<RecoverInfo> {
    // The header is a few short lines; cap what is read so a huge file with
    // no newline is not pulled into memory just to be rejected.
    let mut r = BufReader::new(dir.open(name)?.take(64 * 1024));
    let mut raw = Vec::new();
    let mut next_line = |line: &mut String| -> io::Result<usize> {
        raw.clear();
        let n = r.read_until(b'\n', &mut raw)?;
        *line = crate::bytes::decode(&raw);
        Ok(n)
    };
    let mut line = String::new();
    next_line(&mut line)?;
    if line.trim_end() != MAGIC {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "not a recovery file",
        ));
    }
    let mut orig_path = None;
    let mut modified = 0u64;
    loop {
        if next_line(&mut line)? == 0 {
            break;
        }
        let l = line.trim_end();
        if l == "---" {
            break;
        }
        if let Some(v) = line
            .strip_suffix('\n')
            .unwrap_or(&line)
            .strip_prefix("path:")
        {
            // Exactly the one blank after the colon: the name's own leading
            // or trailing blanks are part of it.
            let v = v.strip_prefix(' ').unwrap_or(v);
            if !v.is_empty() {
                orig_path = Some(crate::bytes::decode(&unescape_field(
                    &crate::bytes::encode(v),
                )));
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
/// Listing never creates the recovery directory: `vi -r` lists, and an
/// editor that preserves nothing must leave nothing behind under `base`.
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
    // Skip the header, which may name a path that is not UTF-8.
    let mut line = Vec::new();
    loop {
        line.clear();
        if r.read_until(b'\n', &mut line)? == 0 {
            break;
        }
        if line.trim_ascii_end() == b"---" {
            break;
        }
    }
    let mut body = Vec::new();
    r.read_to_end(&mut body)?;
    Ok(crate::bytes::decode(&body))
}

/// The name a recovery file records for `path`: absolute, with its
/// directory resolved (`..`, symbolic links), so every spelling of one file
/// gives the same name.  The last component stays as written: the file
/// itself may not exist, or may be a link that is edited as such.
pub fn path_key(path: &Path) -> PathBuf {
    let abs = std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf());
    match (abs.parent(), abs.file_name()) {
        (Some(dir), Some(name)) => std::fs::canonicalize(dir)
            .map(|d| d.join(name))
            .unwrap_or(abs),
        _ => abs,
    }
}

/// Every recovery file under `base` for `orig`, newest first.  A file
/// matches by its [`path_key`].
pub fn find_all_for(base: &str, orig: &Path) -> Vec<RecoverInfo> {
    let want = path_key(orig);
    list(base)
        .into_iter()
        .filter(|i| {
            i.orig_path
                .as_deref()
                .is_some_and(|p| path_key(&crate::bytes::path(p)) == want)
        })
        .collect()
}

/// Remove a recovery file (e.g. after a successful recovery).
pub fn remove(info: &RecoverInfo) {
    info.dir.unlink(&info.name);
}

/// The mail `notify` sends (95222-95225): the file's name, when it was
/// preserved, and an ex command that recovers it.
fn notice(user: &str, orig: Option<&Path>, secs: u64) -> String {
    let fname = orig
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| "an unnamed buffer".to_string())
        .replace(['\n', '\r'], " ");
    let when = local_time(secs);
    let command = match orig {
        // Quoted for the shell: in single quotes, a quote as '\''.
        Some(p) => format!(
            "ex -r '{}'",
            crate::bytes::from_path(&path_key(p))
                .replace(['\n', '\r'], " ")
                .replace('\'', "'\\''")
        ),
        None => "ex -r".to_string(),
    };
    format!(
        "From: vi\nTo: {user}\nSubject: editor recovery\n\n\
         A copy of an edit session for {fname} was saved at {when}.\n\
         To recover it, run:\n\n    {command}\n"
    )
}

/// `secs` since the epoch as a local date and time, as date(1) writes it.
fn local_time(secs: u64) -> String {
    i64::try_from(secs)
        .ok()
        .and_then(|t| plib::locale::strftime("%a %b %e %H:%M:%S %Z %Y", t).ok())
        .unwrap_or_else(|| format!("{secs} seconds after the epoch"))
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
    let body = notice(&user, orig, now_secs());
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

        let info = find_newest(base, "/home/user/notes.txt").expect("recoverable found");
        assert_eq!(info.orig_path.as_deref(), Some("/home/user/notes.txt"));

        assert_eq!(info.recovery_path, rec);

        let body = load_body(&info).unwrap();
        assert_eq!(body, text);

        remove(&info);
        assert!(!rec.exists());
    }

    #[test]
    fn test_preserve_and_load_non_utf8() {
        use std::os::unix::ffi::OsStrExt;
        let td = plib::tmp::tempdir().unwrap();
        let base = td.path().to_str().unwrap();
        let orig = PathBuf::from(std::ffi::OsStr::from_bytes(b"/x/n\xffm"));
        let text = crate::bytes::decode(b"a\xffb\n\xfe\n");
        let rec = preserve(Some(&orig), &text, base).unwrap();
        // On disk, the bytes themselves.
        let raw = fs::read(&rec).unwrap();
        assert!(raw.ends_with(b"---\na\xffb\n\xfe\n"));
        let want = crate::bytes::from_os(orig.as_os_str());
        let info = find_newest(base, &want).expect("found by its own name");
        assert_eq!(load_body(&info).unwrap(), text);
        assert_eq!(crate::bytes::path(info.orig_path.as_deref().unwrap()), orig);
    }

    #[test]
    fn test_pathname_with_newline_and_blanks_recovers() {
        use std::os::unix::ffi::OsStrExt;
        let td = plib::tmp::tempdir().unwrap();
        let base = td.path().to_str().unwrap();
        for name in [
            &b"/x/a\n---\nX"[..],
            b"/x/notes ",
            b" /x/lead",
            b"/x/back\\slash",
        ] {
            let orig = PathBuf::from(std::ffi::OsStr::from_bytes(name));
            preserve(Some(&orig), "body\n", base).unwrap();
            let want = crate::bytes::from_path(&orig);
            let info = find_newest(base, &want).expect("found by its own name");
            assert_eq!(load_body(&info).unwrap(), "body\n", "{name:?}");
            remove(&info);
        }
    }

    #[test]
    fn test_notice_names_file_time_and_command() {
        let body = notice("u", Some(Path::new("/x/it's")), 0);
        assert!(body.contains("/x/it's was saved at "), "{body}");
        assert!(body.contains("1970") || body.contains("1969"), "{body}");
        assert!(body.contains("ex -r '/x/it'\\''s'"), "{body}");
    }

    #[test]
    fn test_list_does_not_create_the_recovery_directory() {
        let td = plib::tmp::tempdir().unwrap();
        let base = td.path().to_str().unwrap();

        assert!(list(base).is_empty());
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
    fn test_repair_by_name_makes_an_unsearchable_directory_private() {
        // The path macOS takes for mode 0000; run here too.
        let td = plib::tmp::tempdir().unwrap();
        let dir = dir_path(td.path().to_str().unwrap());
        fs::create_dir(&dir).unwrap();
        fs::set_permissions(&dir, fs::Permissions::from_mode(0o000)).unwrap();
        let c = CString::new(dir.as_os_str().as_bytes()).unwrap();
        let euid = unsafe { libc::geteuid() };
        let (dev, ino) = repair_by_name(&dir, &c, euid).unwrap();
        let md = fs::metadata(&dir).unwrap();
        assert_eq!(md.mode() & 0o777, 0o700);
        assert_eq!((md.dev(), md.ino()), (dev, ino));
        // A link in its place is refused, not followed.
        let link = td.path().join("link");
        std::os::unix::fs::symlink(&dir, &link).unwrap();
        let c = CString::new(link.as_os_str().as_bytes()).unwrap();
        assert!(repair_by_name(&link, &c, euid).is_err());
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

#[cfg(test)]
fn find_newest(base: &str, orig: &str) -> Option<RecoverInfo> {
    find_all_for(base, &crate::bytes::path(orig))
        .into_iter()
        .next()
}

#[cfg(test)]
mod path_tests {
    use super::*;

    #[test]
    fn test_dot_dot_spelling_is_the_same_file() {
        let td = plib::tmp::tempdir().unwrap();
        let base = td.path().to_str().unwrap();
        let sub = td.path().join("sub");
        std::fs::create_dir(&sub).unwrap();
        let via = td.path().join("sub/../f");
        preserve(Some(&via), "x\n", base).unwrap();
        let direct = crate::bytes::from_path(&td.path().join("f"));
        assert!(find_newest(base, &direct).is_some());
    }

    #[test]
    fn test_relative_name_is_found_by_its_absolute_form() {
        let td = plib::tmp::tempdir().unwrap();
        let base = td.path().to_str().unwrap();
        preserve(Some(Path::new("rel-notes.txt")), "x\n", base).unwrap();
        let cwd = std::env::current_dir().unwrap();
        let abs = crate::bytes::from_path(&cwd.join("rel-notes.txt"));
        assert!(find_newest(base, &abs).is_some());
        assert!(find_newest(base, "./rel-notes.txt").is_some());
        assert!(find_newest(base, "other.txt").is_none());
    }
}
