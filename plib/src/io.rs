//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

#[cfg(unix)]
use std::ffi::CString;
use std::fs;
use std::io::{self, Read, Write};
#[cfg(unix)]
use std::os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd, OwnedFd};
#[cfg(unix)]
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};

/// open file, or stdin
pub fn input_stream(pathname: &Path, dashed_stdin: bool) -> io::Result<Box<dyn Read>> {
    let path_str = pathname.as_os_str();
    let file: Box<dyn Read> =
        if (dashed_stdin && path_str == "-") || (!dashed_stdin && path_str.is_empty()) {
            Box::new(io::stdin().lock())
        } else {
            Box::new(fs::File::open(pathname)?)
        };

    Ok(file)
}

pub fn input_stream_opt(pathname: &Option<PathBuf>) -> io::Result<Box<dyn Read>> {
    match pathname {
        Some(path) => input_stream(path, false),
        None => input_stream(&PathBuf::new(), false),
    }
}

/// Open `pathname` for reading, treating both an empty path and the literal `-`
/// as standard input.
///
/// POSIX utilities accept `-` as a stdin operand at *any* position in the file
/// list (XBD §12.2 Guideline 13), not only when it is the sole operand. Use
/// this at each per-operand open site so a `-` interleaved with real files
/// (e.g. `util a - b`) reads stdin at that position, while keeping an empty
/// path (the conventional "no file operands" sentinel) routed to stdin too.
///
/// Unlike [`input_stream`], the stdin case returns the unlocked [`io::Stdin`]
/// handle (which acquires the stdin lock per read) rather than a persistent
/// [`io::StdinLock`]. This lets a utility hold several stdin sources open at
/// once — e.g. `cut - -` or `sort - -` build a vector of readers — without
/// deadlocking on a second `StdinLock` acquisition. The first source drains
/// stdin; later stdin sources see EOF.
pub fn input_stream_dashed(pathname: &Path) -> io::Result<Box<dyn Read>> {
    let s = pathname.as_os_str();
    if s.is_empty() || s == "-" {
        Ok(Box::new(io::stdin()))
    } else {
        Ok(Box::new(fs::File::open(pathname)?))
    }
}

pub fn input_reader(
    pathname: &Path,
    dashed_stdin: bool,
) -> io::Result<io::BufReader<Box<dyn Read>>> {
    let file = input_stream(pathname, dashed_stdin)?;
    Ok(io::BufReader::new(file))
}

/// Atomically replace `path` with `bytes`.
///
/// Writes to a temp file in the same directory as `path`, syncs it, then
/// `rename(2)`s over the original — so a reader that has the old `path` open
/// keeps seeing the old bytes, and a crash mid-write leaves either the old
/// content intact or, on success, the new content fully visible.
///
/// If `path` already exists, the new file inherits its mode (`st_mode &
/// 0o7777`). If it does not, it gets what XCU 1.1.1.4 requires of a utility
/// that creates a file: what creating it with `0o666` would give it, which is
/// `0o666 & ~umask` -- or, on Linux in a directory with a default ACL, the
/// access ACL inherited from that default masked by `0o666`, the umask playing
/// no part. On Windows the mode is the read-only attribute (see
/// [`crate::perm`]).
///
/// The mode has to be set explicitly because the temporary this writes through
/// is created `O_EXCL|0600` — deliberately, since it is world-visible in the
/// target's directory before the rename. Inheriting *that* is how a fresh
/// `tags` file and a fresh `ar` archive came out `-rw-------`.
///
/// Used by utilities like `ar` that rewrite a binary in place
/// where a partial write would corrupt the artifact on disk.
pub fn write_atomic(path: &Path, bytes: &[u8]) -> io::Result<()> {
    match fs::metadata(path) {
        Ok(meta) => write_atomic_mode(path, bytes, crate::perm::mode_of(&meta.permissions())),
        // Only "there is no file here" means a file is being created. Every
        // other stat failure is reported rather than read as absence: an
        // `Err(_)` arm would take, say, EACCES on a path component or ENOTDIR
        // on a parent as "missing" and go on to pick a mode for a file it
        // could not have looked at.
        Err(e) if e.kind() == io::ErrorKind::NotFound => {
            write_atomic_with(path, bytes, set_created_mode)
        }
        Err(e) => Err(e),
    }
}

/// `write_atomic`, with the resulting file's mode named outright.
///
/// For the callers whose spec, or whose security posture, fixes the mode
/// rather than deriving it — `crontab` writes the spool copy `0600` whether or
/// not one was already there.
pub fn write_atomic_mode(path: &Path, bytes: &[u8], mode: u32) -> io::Result<()> {
    write_atomic_with(path, bytes, |file, _| {
        let mut perm = file.metadata()?.permissions();
        crate::perm::set_mode(&mut perm, mode);
        file.set_permissions(perm)
    })
}

/// Give `file`, the temporary just made in the directory `parent`, what
/// creating it there with `0o666` would have (`write_atomic`). Only Linux has
/// default ACLs to inherit; they are read through `parent`, the descriptor the
/// temporary was made and is renamed through.
#[cfg(unix)]
fn set_created_mode(file: &fs::File, parent: BorrowedFd<'_>) -> io::Result<()> {
    #[cfg(target_os = "linux")]
    let default = match crate::acl::read_fd(parent.as_raw_fd()) {
        Ok(acl) => acl.default,
        Err(e) if e.raw_os_error() == Some(libc::EOPNOTSUPP) => None,
        Err(e) => return Err(e),
    };
    #[cfg(not(target_os = "linux"))]
    let default = {
        let _ = parent;
        None
    };
    let fd = file.as_raw_fd();
    crate::acl::set_created_mode(fd, default, 0o666, crate::modestr::umask(), |mode| {
        // Cast needed: `mode_t` is u16 on macOS and u32 on Linux.
        crate::madefs::cvt(unsafe { libc::fchmod(fd, mode as libc::mode_t) })
    })
}

/// Windows has no umask and no default ACL to inherit from: a new file is an
/// ordinary writable one (`perm::new_file_mode`).
#[cfg(windows)]
fn set_created_mode(file: &fs::File, _parent: &Path) -> io::Result<()> {
    let mut perm = file.metadata()?.permissions();
    crate::perm::set_mode(&mut perm, crate::perm::new_file_mode());
    file.set_permissions(perm)
}

/// The directory `path` is in: `.` for a bare name.
fn parent_of(path: &Path) -> &Path {
    path.parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."))
}

/// `write_atomic`, giving the temporary its mode with `set_mode`, which takes
/// it and the directory it was made in.
///
/// The directory is opened once, and the temporary is made in it, given its
/// mode and renamed over `path` all through that descriptor: every step acts
/// on the one directory, whatever happens to its path meanwhile.
#[cfg(unix)]
fn write_atomic_with(
    path: &Path,
    bytes: &[u8],
    set_mode: impl FnOnce(&fs::File, BorrowedFd<'_>) -> io::Result<()>,
) -> io::Result<()> {
    let c_string =
        |bytes: &[u8]| CString::new(bytes).map_err(|_| io::Error::from_raw_os_error(libc::EINVAL));
    let name = path
        .file_name()
        .ok_or_else(|| io::Error::from_raw_os_error(libc::EINVAL))?;
    let name = c_string(name.as_bytes())?;
    let parent = c_string(parent_of(path).as_os_str().as_bytes())?;
    let flags = crate::madefs::SEARCH_ONLY | libc::O_DIRECTORY | libc::O_CLOEXEC;
    let dirfd = unsafe { libc::open(parent.as_ptr(), flags) };
    if dirfd < 0 {
        return Err(io::Error::last_os_error());
    }
    let dir = unsafe { OwnedFd::from_raw_fd(dirfd) };

    // Made in the same directory as `path` so the final rename stays within
    // one filesystem and is atomic.
    let mut tmp = TempAt::new(dir.as_fd())?;
    tmp.file.write_all(bytes)?;
    tmp.file.sync_all()?;

    // Before the rename, so the file is never visible at `path` under the
    // temporary's 0600.
    set_mode(&tmp.file, dir.as_fd())?;

    let (dirfd, from, to) = (dir.as_raw_fd(), tmp.name.as_ptr(), name.as_ptr());
    if unsafe { libc::renameat(dirfd, from, dirfd, to) } != 0 {
        return Err(io::Error::last_os_error());
    }
    tmp.renamed = true;
    Ok(())
}

/// A temporary file made 0600 and `O_EXCL` under a fresh name in the directory
/// `dir`, removed from it again when dropped unless it was renamed.
#[cfg(unix)]
struct TempAt<'a> {
    dir: BorrowedFd<'a>,
    name: CString,
    file: fs::File,
    renamed: bool,
}

#[cfg(unix)]
impl<'a> TempAt<'a> {
    fn new(dir: BorrowedFd<'a>) -> io::Result<Self> {
        use std::time::{SystemTime, UNIX_EPOCH};
        let flags = libc::O_WRONLY
            | libc::O_CREAT
            | libc::O_EXCL
            | libc::O_NOFOLLOW
            | libc::O_NOCTTY
            | libc::O_CLOEXEC;
        let seed = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_or(0, |d| d.subsec_nanos());
        for n in 0..100u32 {
            let name = format!(".tmp{}.{seed:x}.{n}", std::process::id());
            let name = CString::new(name).expect("no NUL in a formatted name");
            let mode: libc::c_uint = 0o600;
            let fd = unsafe { libc::openat(dir.as_raw_fd(), name.as_ptr(), flags, mode) };
            if fd >= 0 {
                let file = unsafe { fs::File::from_raw_fd(fd) };
                let renamed = false;
                return Ok(TempAt {
                    dir,
                    name,
                    file,
                    renamed,
                });
            }
            let e = io::Error::last_os_error();
            if e.raw_os_error() != Some(libc::EEXIST) {
                return Err(e);
            }
        }
        Err(io::Error::from_raw_os_error(libc::EEXIST))
    }
}

#[cfg(unix)]
impl Drop for TempAt<'_> {
    fn drop(&mut self) {
        if !self.renamed {
            unsafe { libc::unlinkat(self.dir.as_raw_fd(), self.name.as_ptr(), 0) };
        }
    }
}

/// `write_atomic`, giving the temporary its mode with `set_mode`, which takes
/// it and the directory it was made in.
#[cfg(windows)]
fn write_atomic_with(
    path: &Path,
    bytes: &[u8],
    set_mode: impl FnOnce(&fs::File, &Path) -> io::Result<()>,
) -> io::Result<()> {
    let parent = parent_of(path);
    // Tempfile is created in the same directory as `path` so the final
    // `rename(2)` stays within one filesystem and is atomic.
    let mut tmp = crate::tmp::NamedTempFile::new_in(parent)?;
    tmp.as_file_mut().write_all(bytes)?;
    tmp.as_file_mut().sync_all()?;

    // Before the rename, so the file is never visible at `path` under the
    // temporary's 0600.
    set_mode(tmp.as_file(), parent)?;

    replace_with(tmp, path)
}

/// Rename `tmp` over `path`, replacing whatever is there.
///
/// Windows will not replace a read-only file, so a read-only target has its
/// attribute cleared first and given back if the rename still fails. The new
/// file carries its own attribute, set before the rename. On a failure the
/// temporary is made writable again, so its cleanup can delete it wherever a
/// read-only file cannot be deleted (Wine, older Windows).
#[cfg(windows)]
fn replace_with(tmp: crate::tmp::NamedTempFile, path: &Path) -> io::Result<()> {
    let target = match fs::symlink_metadata(path) {
        Ok(meta) if meta.permissions().readonly() => Some(meta.permissions()),
        Ok(_) => None,
        Err(e) if e.kind() == io::ErrorKind::NotFound => None,
        Err(e) => return Err(e),
    };
    if let Some(original) = &target {
        fs::set_permissions(path, writable(original.clone()))?;
    }
    tmp.persist(path).map(drop).map_err(|e| {
        if let Some(original) = target {
            let _ = fs::set_permissions(path, original);
        }
        if let Ok(meta) = e.file.as_file().metadata() {
            let _ = e
                .file
                .as_file()
                .set_permissions(writable(meta.permissions()));
        }
        e.error
    })
}

/// `perm` without the read-only attribute.
#[cfg(windows)]
fn writable(mut perm: fs::Permissions) -> fs::Permissions {
    #[expect(
        clippy::permissions_set_readonly_false,
        reason = "Windows only: clears the read-only attribute, no Unix mode bits"
    )]
    perm.set_readonly(false);
    perm
}

/// Restore the default disposition for `SIGPIPE`, unless the process was
/// started with it ignored.
///
/// The Rust runtime sets `SIGPIPE` to `SIG_IGN` before `main`, so a write to a
/// closed pipe returns `EPIPE` instead of killing the process. For a filter
/// that is routinely piped into `head` or `less` that is the wrong shape: the
/// error surfaces as a panic ("failed printing to stdout: Broken pipe") and
/// exit 101, where the historical utilities die by the signal and the shell
/// reports 141.
///
/// An *inherited* `SIG_IGN` is a different matter. POSIX keeps an ignored
/// signal ignored across `exec`, and whoever started the utility that way
/// (`trap '' PIPE` in a shell) asked to see `EPIPE` as a write error instead
/// of dying. That disposition is left alone; a write to a closed pipe is then
/// reported like any other write error. The runtime has already overwritten
/// the disposition by the time `main` runs, so the inherited one is recorded
/// by a constructor that runs before it (see [`sigpipe_inherited_ignored`]).
///
/// [`crate::diag::init_locale`] calls this, so a utility gets it by starting up
/// the usual way; call it directly only before that, or instead of it.
///
/// It also settles what a child inherits, which is worth stating because the
/// rule is the opposite of the one for caught signals: POSIX keeps a `SIG_IGN`
/// disposition *across* `exec`, so leaving the runtime's `SIG_IGN` in place
/// would hand it to everything the process runs. Nothing in this tree does:
/// `std::process::Command` restores the default in the child, and `sh` -- the
/// one raw `libc::execve` -- resets every disposition itself in
/// `TrapManager::reset`. A default disposition is inherited unchanged, so with
/// this called there is nothing left for either of them to undo.
///
/// Windows has no `SIGPIPE`: a write to a closed pipe fails with an error,
/// and there is nothing to restore.
pub fn restore_sigpipe() {
    #[cfg(unix)]
    if !sigpipe_inherited_ignored() {
        // SAFETY: `signal` with SIG_DFL is async-signal-safe and this runs
        // before any other thread exists.
        unsafe {
            libc::signal(libc::SIGPIPE, libc::SIG_DFL);
        }
    }
}

/// Whether the process was started with `SIGPIPE` ignored.
///
/// The answer is recorded before `main`, before the Rust runtime replaces the
/// inherited disposition with its own `SIG_IGN`.
#[cfg(unix)]
pub fn sigpipe_inherited_ignored() -> bool {
    // Name the constructor's slot, so the object file holding it is linked
    // into every binary that asks; an unreferenced archive member, and the
    // constructor with it, would be left out and the answer always false.
    std::hint::black_box(&RECORD_INHERITED_SIGPIPE);
    SIGPIPE_INHERITED_IGNORED.load(std::sync::atomic::Ordering::Relaxed)
}

#[cfg(unix)]
static SIGPIPE_INHERITED_IGNORED: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Record the `SIGPIPE` disposition the process was started with. Runs from
/// the platform's constructor list, before the C `main` that starts the Rust
/// runtime.
#[cfg(unix)]
extern "C" fn record_inherited_sigpipe() {
    // SAFETY: a null new action makes `sigaction` only read the current one
    // into `old`, which is a plain C struct for which all-zero is valid.
    unsafe {
        let mut old: libc::sigaction = std::mem::zeroed();
        if libc::sigaction(libc::SIGPIPE, std::ptr::null(), &mut old) == 0 {
            let ignored = old.sa_sigaction == libc::SIG_IGN;
            SIGPIPE_INHERITED_IGNORED.store(ignored, std::sync::atomic::Ordering::Relaxed);
        }
    }
}

#[cfg(unix)]
#[used]
#[cfg_attr(target_vendor = "apple", link_section = "__DATA,__mod_init_func")]
#[cfg_attr(not(target_vendor = "apple"), link_section = ".init_array")]
static RECORD_INHERITED_SIGPIPE: extern "C" fn() = record_inherited_sigpipe;

/// Report a failed write to standard output as `UTILITY: write error: ...`
/// and exit 1, instead of the panic and exit 101 that `print!`/`println!`
/// turn it into.
///
/// With `SIGPIPE` ignored (see [`restore_sigpipe`]) a closed pipe is the
/// common case, but a full disk or an I/O error on standard output takes the
/// same path. Any other panic goes to the hook that was installed before.
pub fn report_stdout_write_errors(utility: &str) {
    let utility = utility.to_string();
    let previous = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let payload = info.payload();
        let message = payload
            .downcast_ref::<String>()
            .map(String::as_str)
            .or_else(|| payload.downcast_ref::<&str>().copied());
        let failure = message.and_then(|m| m.strip_prefix("failed printing to stdout: "));
        let Some(failure) = failure else {
            return previous(info);
        };
        // libstd appends " (os error N)" to the system's message.
        let reason = failure.split(" (os error ").next().unwrap_or(failure);
        let _ = writeln!(io::stderr(), "{utility}: write error: {reason}");
        std::process::exit(1);
    }));
}

/// Ignores `SIGPIPE` for as long as the guard is held, then restores whatever
/// disposition was in force before.
///
/// A utility's own standard output wants the default disposition: a closed pipe
/// there means the reader is gone and the right answer is to die, which is what
/// [`restore_sigpipe`] arranges. But a utility that spawns a pager or a filter
/// and writes into *that* pipe owns its far end, and a close there is an event
/// to observe — the user pressed `q`, the filter read enough — not a reason to
/// abandon the operation. `w !head -1` in `ed` must report `?` and keep the
/// buffer, not take the signal and lose it.
///
/// One process-wide disposition cannot tell those two pipes apart, so hold this
/// across the write to the child and no longer:
///
/// ```ignore
/// let _sigpipe = SigPipeIgnored::new();
/// child_stdin.write_all(&data)?;   // EPIPE here, not death
/// ```
///
/// Bind it to a named local. `let _ = SigPipeIgnored::new();` drops the guard
/// at once and restores the default before the write, which is the one mistake
/// that silently does nothing.
///
/// When a thread does the writing, the guard has to outlive the join, not just
/// the spawn.
#[cfg(unix)]
#[must_use = "SIGPIPE is only ignored while the guard is alive"]
pub struct SigPipeIgnored(libc::sighandler_t);

#[cfg(unix)]
impl SigPipeIgnored {
    /// Ignore `SIGPIPE`, remembering the disposition being replaced.
    pub fn new() -> Self {
        // SAFETY: `signal` is async-signal-safe and returns the previous
        // handler, which is what Drop puts back.
        let previous = unsafe { libc::signal(libc::SIGPIPE, libc::SIG_IGN) };
        Self(previous)
    }
}

#[cfg(unix)]
impl Default for SigPipeIgnored {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(unix)]
impl Drop for SigPipeIgnored {
    fn drop(&mut self) {
        // SAFETY: as `new`; `self.0` came from `signal` and is a valid handler.
        unsafe {
            libc::signal(libc::SIGPIPE, self.0);
        }
    }
}

/// The terminal a prompt reads its answer from: the controlling terminal on
/// Unix, the console's input buffer on Windows.
#[cfg(unix)]
const TERMINAL_INPUT: &str = "/dev/tty";
#[cfg(windows)]
const TERMINAL_INPUT: &str = "CONIN$";

/// The terminal a prompt is written to: the controlling terminal on Unix, the
/// console's screen buffer on Windows.
#[cfg(unix)]
const TERMINAL_OUTPUT: &str = "/dev/tty";
#[cfg(windows)]
const TERMINAL_OUTPUT: &str = "CONOUT$";

/// Open the terminal for reading a prompt's answer, which POSIX takes from
/// `/dev/tty` rather than standard input (`pr -p`, `patch`'s questions).
///
/// Windows has no `/dev/tty`; the same thing there is the console, opened as
/// `CONIN$`. Either open fails when the process has no terminal, and a caller
/// then skips the prompt.
pub fn open_terminal_input() -> io::Result<fs::File> {
    fs::File::open(TERMINAL_INPUT)
}

/// Open the terminal for writing a prompt: `/dev/tty`, or the console's
/// `CONOUT$` on Windows. See [`open_terminal_input`].
pub fn open_terminal_output() -> io::Result<fs::File> {
    fs::OpenOptions::new().write(true).open(TERMINAL_OUTPUT)
}

/// Make sure standard input, output and error are open before anything else
/// runs.
///
/// A process can be started with one of them closed. The first file anything
/// opens then lands on that descriptor, and what the utility believes it is
/// writing to standard output goes silently into that file instead -- a
/// message catalog opened while installing the locale is enough to trigger it.
/// Taking the free slots with `/dev/null` first, as coreutils does, keeps
/// output out of an unrelated file.
///
/// Call this as the first statement of `main`, before opening anything.
///
/// Windows hands out handles rather than reusing the lowest free descriptor
/// number, so a closed standard handle is never filled by an unrelated open,
/// and there is nothing to do.
pub fn ensure_std_fds_open() {
    #[cfg(unix)]
    open_free_std_fds();
}

#[cfg(unix)]
fn open_free_std_fds() {
    use std::os::fd::{AsRawFd, IntoRawFd};
    while let Ok(file) = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/null")
    {
        if file.as_raw_fd() > 2 {
            // 0, 1 and 2 were all taken already; this one closes on drop.
            break;
        }
        // Leak it deliberately: it is holding a standard descriptor open.
        let _ = file.into_raw_fd();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input_stream_dashed_opens_file() {
        let dir = crate::tmp::tempdir().unwrap();
        let path = dir.path().join("data.txt");
        fs::write(&path, b"hello\n").unwrap();

        let mut s = input_stream_dashed(&path).unwrap();
        let mut buf = String::new();
        s.read_to_string(&mut buf).unwrap();
        assert_eq!(buf, "hello\n");
    }

    #[test]
    fn input_stream_dashed_missing_file_errors() {
        let dir = crate::tmp::tempdir().unwrap();
        let path = dir.path().join("nope.txt");
        // A real (non-"-") path that does not exist must surface the open error,
        // not be silently treated as stdin.
        assert!(input_stream_dashed(&path).is_err());
    }

    #[test]
    fn write_atomic_replaces_content() {
        let dir = crate::tmp::tempdir().unwrap();
        let path = dir.path().join("target.bin");
        fs::write(&path, b"original").unwrap();

        write_atomic(&path, b"replaced").unwrap();
        assert_eq!(fs::read(&path).unwrap(), b"replaced");
    }

    /// Content only. The created file's *mode* is a function of the umask, so
    /// it is asserted in `plib/tests/write_atomic_umask.rs`, which gets its own
    /// process — this test binary also runs `modestr::mutate`, which reads the
    /// umask by setting it to 0 and back.
    #[test]
    fn write_atomic_creates_when_missing() {
        let dir = crate::tmp::tempdir().unwrap();
        let path = dir.path().join("new.bin");
        assert!(!path.exists());

        write_atomic(&path, b"hello").unwrap();
        assert_eq!(fs::read(&path).unwrap(), b"hello");
    }

    // Modes are the subject; see the read-only test below for Windows.
    #[cfg(unix)]
    #[test]
    fn write_atomic_preserves_mode() {
        use std::os::unix::fs::PermissionsExt;
        let dir = crate::tmp::tempdir().unwrap();
        let path = dir.path().join("executable.bin");
        fs::write(&path, b"#!/bin/sh\necho hi\n").unwrap();
        fs::set_permissions(&path, fs::Permissions::from_mode(0o755)).unwrap();

        write_atomic(&path, b"replaced").unwrap();
        let mode = fs::metadata(&path).unwrap().permissions().mode() & 0o7777;
        assert_eq!(mode, 0o755);
    }

    #[test]
    fn write_atomic_no_leftover_temp() {
        let dir = crate::tmp::tempdir().unwrap();
        let path = dir.path().join("file.bin");
        write_atomic(&path, b"data").unwrap();

        // Only the target file should exist in the directory.
        let entries: Vec<_> = fs::read_dir(dir.path())
            .unwrap()
            .map(|e| e.unwrap().file_name())
            .collect();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0], "file.bin");
    }

    /// A read-only target is replaced and stays read-only: the attribute is
    /// Windows's whole mode, so this is `write_atomic_preserves_mode` there.
    #[test]
    fn write_atomic_replaces_a_read_only_file() {
        let dir = crate::tmp::tempdir().unwrap();
        let path = dir.path().join("ro.bin");
        fs::write(&path, b"original").unwrap();
        let mut perm = fs::metadata(&path).unwrap().permissions();
        crate::perm::set_mode(&mut perm, 0o444);
        fs::set_permissions(&path, perm).unwrap();

        write_atomic(&path, b"replaced").unwrap();
        assert_eq!(fs::read(&path).unwrap(), b"replaced");
        let mut perm = fs::metadata(&path).unwrap().permissions();
        assert!(perm.readonly(), "the replacement must keep the mode");

        // Wine will not delete a read-only file; clear it for the TempDir.
        crate::perm::set_mode(&mut perm, 0o644);
        fs::set_permissions(&path, perm).unwrap();
    }

    /// A file created in a directory with a default ACL takes what `creat(2)` would give it
    /// there: the default masked by 0666, the umask playing no part -- not 0666 less the umask
    /// set over the ACL its temporary inherited, which let others read it.
    #[cfg(unix)]
    #[test]
    fn write_atomic_creates_a_file_under_the_default_acl() {
        let dir = crate::tmp::tempdir().unwrap();
        if !crate::testing::set_default_acl(dir.path()) {
            return;
        }
        let path = dir.path().join("new.bin");
        write_atomic(&path, b"new").unwrap();
        assert_eq!(
            crate::testing::mode_and_acl(&path),
            "660 user::rw- user:65534:rwx group::r-x mask::rw- other::---"
        );
    }
}
