//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Rewriting a mailbox in place without putting its mail at risk.
//!
//! A system mailbox is rewritten in place, never replaced: the delivery
//! agent holds it open and locked, and its directory is often writable only
//! by the mail system. Truncating it leaves the mail it held nowhere but in
//! mailx, and traditional mailx loses that mail if it is killed or the
//! machine stops before the rewrite ends.
//!
//! So the new contents are first written in full to a recovery file in
//! `HOME` and synced; then the mailbox is truncated and filled from that
//! file, and synced; only then is the recovery file removed. If the rewrite
//! fails the error names the file, and if mailx dies the file outlives it:
//! every later start reports it, so the mail can be recovered by hand.

use std::fs::{self, File};
use std::io::{self, Seek, Write};
use std::path::{Path, PathBuf};

/// The start of every recovery file's name.
const PREFIX: &str = "mailx-recover.";

/// Where recovery files are written: `HOME`, which survives a reboot, or
/// the temporary directory when `HOME` cannot take one.
fn directories() -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    if let Ok(home) = std::env::var("HOME") {
        if !home.is_empty() {
            dirs.push(PathBuf::from(home));
        }
    }
    dirs.push(std::env::temp_dir());
    dirs
}

/// Replace the contents of `file`, the mailbox at `path`, opened and locked
/// for writing, with what `write` produces, through a recovery file in the
/// first of `dirs` that can take one.
///
/// `write` is handed `file` as well, still holding its old contents, so the
/// parts of them that are kept can be read straight from it.
pub(crate) fn rewrite_in_place(
    path: &str,
    file: &mut File,
    dirs: &[PathBuf],
    write: impl FnOnce(&File, &mut dyn Write) -> io::Result<()>,
) -> io::Result<()> {
    let mut recovery = create(path, dirs)?;
    // Held while the file is in use, so a report run meanwhile passes over
    // it; released when it is closed, so one left behind can be taken.
    lock(recovery.as_file(), libc::LOCK_EX)?;
    {
        let mut w = io::BufWriter::new(recovery.as_file_mut());
        write(file, &mut w)?;
        w.flush()?;
    }
    recovery.as_file().sync_all()?;

    match fill(file, recovery.as_file_mut()) {
        Ok(()) => recovery.close(),
        Err(e) => {
            let kept = recovery.keep();
            Err(io::Error::new(
                e.kind(),
                format!(
                    "{}; its new contents are in {}",
                    plib::diag::io_error_text(&e),
                    kept.display()
                ),
            ))
        }
    }
}

/// [`rewrite_in_place`], with the recovery file in `HOME` or the temporary
/// directory.
pub(crate) fn rewrite(
    path: &str,
    file: &mut File,
    write: impl FnOnce(&File, &mut dyn Write) -> io::Result<()>,
) -> io::Result<()> {
    rewrite_in_place(path, file, &directories(), write)
}

/// A new recovery file for the mailbox at `path`, named after it.
fn create(path: &str, dirs: &[PathBuf]) -> io::Result<plib::tmp::NamedTempFile> {
    let name = Path::new(path)
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    let prefix = format!("{}{}.", PREFIX, name);
    let mut last = io::Error::other("no directory for a recovery file");
    for dir in dirs {
        match plib::tmp::Builder::new().prefix(&prefix).tempfile_in(dir) {
            Ok(file) => return Ok(file),
            Err(e) => last = e,
        }
    }
    Err(last)
}

/// Truncate `file` and copy `contents` into it, then sync it.
fn fill(file: &mut File, contents: &mut File) -> io::Result<()> {
    contents.rewind()?;
    file.rewind()?;
    file.set_len(0)?;
    {
        let mut w = io::BufWriter::new(&mut *file);
        io::copy(contents, &mut w)?;
        w.flush()?;
    }
    file.sync_all()
}

/// `flock` `file` with `op`.
fn lock(file: &File, op: libc::c_int) -> io::Result<()> {
    use std::os::fd::AsRawFd;
    // SAFETY: the fd is valid for the lifetime of `file`.
    if unsafe { libc::flock(file.as_raw_fd(), op) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

/// Whether `path` is a recovery file of this user's that a rewrite left
/// behind: owned by the user, and not locked by a rewrite still running.
fn left_behind(path: &Path) -> bool {
    use std::os::unix::fs::MetadataExt;
    let Ok(file) = File::open(path) else {
        return false;
    };
    // SAFETY: getuid cannot fail.
    let mine = file
        .metadata()
        .is_ok_and(|m| m.uid() == unsafe { libc::getuid() });
    mine && lock(&file, libc::LOCK_EX | libc::LOCK_NB).is_ok()
}

/// Report on standard error any recovery file a rewrite left behind.
pub(crate) fn report_leftovers() {
    for dir in directories() {
        let Ok(entries) = fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            if entry.file_name().to_string_lossy().starts_with(PREFIX) && left_behind(&entry.path())
            {
                eprintln!(
                    "mailx: {}: holds the new contents of a mailbox whose rewrite was \
                     interrupted; recover them by hand",
                    entry.path().display()
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mailbox(dir: &Path, contents: &str) -> String {
        let path = dir.join("box");
        fs::write(&path, contents).unwrap();
        path.to_str().unwrap().to_string()
    }

    fn entries(dir: &Path) -> Vec<String> {
        let mut names: Vec<String> = fs::read_dir(dir)
            .unwrap()
            .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
            .collect();
        names.sort();
        names
    }

    #[test]
    fn a_rewrite_replaces_the_contents_and_leaves_no_recovery_file() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let recover = plib::tmp::TempDir::new().unwrap();
        let path = mailbox(dir.path(), "old contents\n");
        let mut file = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open(&path)
            .unwrap();
        rewrite_in_place(&path, &mut file, &[recover.path().into()], |_, w| {
            w.write_all(b"new\n")
        })
        .unwrap();
        assert_eq!(fs::read_to_string(&path).unwrap(), "new\n");
        assert!(entries(recover.path()).is_empty());
    }

    #[test]
    fn a_failed_rewrite_keeps_the_new_contents_and_names_them() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let recover = plib::tmp::TempDir::new().unwrap();
        let path = mailbox(dir.path(), "old contents\n");
        // Open for reading only, so truncating the mailbox fails.
        let mut file = File::open(&path).unwrap();
        let err = rewrite_in_place(&path, &mut file, &[recover.path().into()], |_, w| {
            w.write_all(b"new\n")
        })
        .unwrap_err();

        assert_eq!(fs::read_to_string(&path).unwrap(), "old contents\n");
        let kept = entries(recover.path());
        assert_eq!(kept.len(), 1, "{:?}", kept);
        assert!(kept[0].starts_with("mailx-recover.box."), "{:?}", kept);
        assert!(err.to_string().contains(&kept[0]), "{}", err);
        assert_eq!(
            fs::read_to_string(recover.path().join(&kept[0])).unwrap(),
            "new\n"
        );
    }

    #[test]
    fn the_next_directory_is_used_when_one_cannot_take_the_file() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let recover = plib::tmp::TempDir::new().unwrap();
        let path = mailbox(dir.path(), "old\n");
        let mut file = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open(&path)
            .unwrap();
        let missing = dir.path().join("no-such-dir");
        rewrite_in_place(
            &path,
            &mut file,
            &[missing, recover.path().into()],
            |_, w| w.write_all(b"new\n"),
        )
        .unwrap();
        assert_eq!(fs::read_to_string(&path).unwrap(), "new\n");
    }
}
