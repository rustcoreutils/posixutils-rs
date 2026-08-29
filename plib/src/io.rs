//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;
use std::io::{self, Read, Write};
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
/// 0o7777`). If it does not, the file is created `0o666 & ~umask`, which is
/// what XCU 1.1.1.4 requires of a utility that creates a file.
///
/// The mode has to be set explicitly because the temporary this writes through
/// is created `O_EXCL|0600` — deliberately, since it is world-visible in the
/// target's directory before the rename. Inheriting *that* is how a fresh
/// `tags` file and a fresh `ar` archive came out `-rw-------`.
///
/// Used by utilities like `ar` and `strip` that rewrite a binary in place
/// where a partial write would corrupt the artifact on disk.
pub fn write_atomic(path: &Path, bytes: &[u8]) -> io::Result<()> {
    let mode = match fs::metadata(path) {
        Ok(meta) => {
            use std::os::unix::fs::PermissionsExt;
            meta.permissions().mode()
        }
        // Only "there is no file here" means a file is being created. Every
        // other stat failure is reported rather than read as absence: an
        // `Err(_)` arm would take, say, EACCES on a path component or ENOTDIR
        // on a parent as "missing" and go on to pick a mode for a file it
        // could not have looked at.
        Err(e) if e.kind() == io::ErrorKind::NotFound => crate::modestr::default_create_mode(),
        Err(e) => return Err(e),
    };
    write_atomic_mode(path, bytes, mode)
}

/// `write_atomic`, with the resulting file's mode named outright.
///
/// For the callers whose spec, or whose security posture, fixes the mode
/// rather than deriving it — `crontab` writes the spool copy `0600` whether or
/// not one was already there.
pub fn write_atomic_mode(path: &Path, bytes: &[u8], mode: u32) -> io::Result<()> {
    use std::os::unix::fs::PermissionsExt;

    let parent = path
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    // Tempfile is created in the same directory as `path` so the final
    // `rename(2)` stays within one filesystem and is atomic.
    let mut tmp = crate::tmp::NamedTempFile::new_in(parent)?;
    tmp.as_file_mut().write_all(bytes)?;
    tmp.as_file_mut().sync_all()?;

    // Before the rename, so the file is never visible at `path` under the
    // temporary's 0600.
    tmp.as_file()
        .set_permissions(std::fs::Permissions::from_mode(mode))?;

    tmp.persist(path).map_err(|e| e.error)?;
    Ok(())
}

/// Restore the default disposition for `SIGPIPE`.
///
/// The Rust runtime sets `SIGPIPE` to `SIG_IGN` before `main`, so a write to a
/// closed pipe returns `EPIPE` instead of killing the process. For a filter
/// that is routinely piped into `head` or `less` that is the wrong shape: the
/// error surfaces as a panic ("failed printing to stdout: Broken pipe") and
/// exit 101, where the historical utilities die by the signal and the shell
/// reports 141.
///
/// Call this once at the top of `main`, before any output. It affects only
/// this process; `exec`ing a child resets ignored signals anyway, and a
/// default disposition is inherited unchanged.
pub fn restore_sigpipe() {
    // SAFETY: `signal` with SIG_DFL is async-signal-safe and this runs before
    // any other thread exists.
    unsafe {
        libc::signal(libc::SIGPIPE, libc::SIG_DFL);
    }
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
pub fn ensure_std_fds_open() {
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
    use std::os::unix::fs::PermissionsExt;

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

    #[test]
    fn write_atomic_preserves_mode() {
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
}
