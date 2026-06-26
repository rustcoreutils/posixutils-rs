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
/// 0o7777`); otherwise the umask determines it.
///
/// Used by utilities like `ar` and `strip` that rewrite a binary in place
/// where a partial write would corrupt the artifact on disk.
pub fn write_atomic(path: &Path, bytes: &[u8]) -> io::Result<()> {
    let parent = path
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    // Tempfile is created in the same directory as `path` so the final
    // `rename(2)` stays within one filesystem and is atomic.
    let mut tmp = tempfile::NamedTempFile::new_in(parent)?;
    tmp.as_file_mut().write_all(bytes)?;
    tmp.as_file_mut().sync_all()?;

    // Preserve the existing file's mode if it exists. New files get their
    // mode from the umask via NamedTempFile's default.
    if let Ok(meta) = fs::metadata(path) {
        use std::os::unix::fs::PermissionsExt;
        let mode = meta.permissions().mode();
        let perms = std::fs::Permissions::from_mode(mode);
        tmp.as_file().set_permissions(perms)?;
    }

    tmp.persist(path).map_err(|e| e.error)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::fs::PermissionsExt;

    #[test]
    fn input_stream_dashed_opens_file() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("data.txt");
        fs::write(&path, b"hello\n").unwrap();

        let mut s = input_stream_dashed(&path).unwrap();
        let mut buf = String::new();
        s.read_to_string(&mut buf).unwrap();
        assert_eq!(buf, "hello\n");
    }

    #[test]
    fn input_stream_dashed_missing_file_errors() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("nope.txt");
        // A real (non-"-") path that does not exist must surface the open error,
        // not be silently treated as stdin.
        assert!(input_stream_dashed(&path).is_err());
    }

    #[test]
    fn write_atomic_replaces_content() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("target.bin");
        fs::write(&path, b"original").unwrap();

        write_atomic(&path, b"replaced").unwrap();
        assert_eq!(fs::read(&path).unwrap(), b"replaced");
    }

    #[test]
    fn write_atomic_creates_when_missing() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("new.bin");
        assert!(!path.exists());

        write_atomic(&path, b"hello").unwrap();
        assert_eq!(fs::read(&path).unwrap(), b"hello");
    }

    #[test]
    fn write_atomic_preserves_mode() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("executable.bin");
        fs::write(&path, b"#!/bin/sh\necho hi\n").unwrap();
        fs::set_permissions(&path, fs::Permissions::from_mode(0o755)).unwrap();

        write_atomic(&path, b"replaced").unwrap();
        let mode = fs::metadata(&path).unwrap().permissions().mode() & 0o7777;
        assert_eq!(mode, 0o755);
    }

    #[test]
    fn write_atomic_no_leftover_temp() {
        let dir = tempfile::tempdir().unwrap();
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
