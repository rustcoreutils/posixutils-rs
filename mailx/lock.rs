//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Locking a mailbox against the local delivery agent.

use std::fs::{self, File};
use std::io::{self, Write};
use std::os::unix::io::AsRawFd;

/// Take a blocking lock on `file`, exclusive or shared.
///
/// Mailbox files are shared with the local delivery agent, so every write path
/// that is not a plain atomic append has to serialize against it.
///
/// Delivery agents do not agree on one kind of lock. On Linux `flock` and
/// `fcntl` locks are independent of each other, and the common agents --
/// Postfix `local`, Exim, procmail, mutt -- take the `fcntl` kind, so holding
/// only `flock` would let a rewrite race a delivery and lose its mail. Both
/// kinds are taken there. Elsewhere the two are one mechanism, and taking
/// both would conflict with ourselves.
pub(crate) fn lock(file: &File, exclusive: bool) -> io::Result<()> {
    let op = if exclusive {
        libc::LOCK_EX
    } else {
        libc::LOCK_SH
    };
    // SAFETY: the fd is valid for the lifetime of `file`, flock takes an fd and
    // an integer operation, and the return value is checked.
    if unsafe { libc::flock(file.as_raw_fd(), op) } != 0 {
        return Err(io::Error::last_os_error());
    }
    #[cfg(target_os = "linux")]
    fcntl_lock(file, exclusive)?;
    Ok(())
}

/// Take a blocking POSIX record lock over the whole of `file`.
///
/// The lock belongs to the process and is released when any descriptor for
/// the file is closed, so a locked mailbox must not be opened a second time
/// while the lock is wanted.
#[cfg(target_os = "linux")]
fn fcntl_lock(file: &File, exclusive: bool) -> io::Result<()> {
    // SAFETY: an all-zero flock is a valid value; the fields that matter are
    // set below (start 0 and length 0 mean the whole file).
    let mut fl: libc::flock = unsafe { std::mem::zeroed() };
    fl.l_type = if exclusive {
        libc::F_WRLCK
    } else {
        libc::F_RDLCK
    } as libc::c_short;
    fl.l_whence = libc::SEEK_SET as libc::c_short;
    // SAFETY: the fd is valid for the lifetime of `file`, `fl` is a valid
    // flock structure that outlives the call, and the return value is checked.
    if unsafe { libc::fcntl(file.as_raw_fd(), libc::F_SETLKW, &fl) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

/// A `<mailbox>.lock` dotlock, removed when dropped.
///
/// Some delivery agents lock the spool only this way, by creating a lock file
/// beside it, so traditional mailx takes the dotlock too, before its kernel
/// locks. Creating it needs write access to the mailbox's directory; where
/// that is lacking, as in a spool directory writable only by group mail, the
/// dotlock is skipped and the kernel locks stand alone.
pub(crate) struct DotLock {
    path: String,
}

impl Drop for DotLock {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}

/// How old a dotlock must be before it is taken as left behind by a holder
/// that died, and removed. Five minutes is the traditional figure.
const STALE_DOTLOCK: std::time::Duration = std::time::Duration::from_secs(300);

/// Take the dotlock for `path`, waiting while another process holds it.
///
/// Returns `None` when the lock file cannot be created at all.
fn dotlock(path: &str) -> Option<DotLock> {
    let lock_path = format!("{}.lock", path);
    loop {
        match crate::util::private_open()
            .write(true)
            .create_new(true)
            .open(&lock_path)
        {
            Ok(_) => return Some(DotLock { path: lock_path }),
            Err(e) if e.kind() == io::ErrorKind::AlreadyExists => {
                let stale = fs::metadata(&lock_path)
                    .and_then(|m| m.modified())
                    .ok()
                    .and_then(|t| t.elapsed().ok())
                    .is_some_and(|age| age > STALE_DOTLOCK);
                if stale {
                    let _ = fs::remove_file(&lock_path);
                } else {
                    std::thread::sleep(std::time::Duration::from_secs(1));
                }
            }
            Err(_) => return None,
        }
    }
}

/// Open `path` to read under its dotlock and a shared lock.
///
/// Both are held while the file and the dotlock returned are, so a
/// delivery in progress is waited for rather than half read.
pub(crate) fn open_read_locked(path: &str) -> io::Result<(File, Option<DotLock>)> {
    let file = File::open(path)?;
    let dotlock = dotlock(path);
    lock(&file, false)?;
    Ok((file, dotlock))
}

/// Open `path` for a locked read-modify-write, creating it if absent.
///
/// The dotlock returned with the file must be held until the file is
/// committed.
pub(crate) fn open_locked(path: &str) -> io::Result<(File, Option<DotLock>)> {
    let dotlock = dotlock(path);
    let file = crate::util::private_open()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(path)?;
    lock(&file, true)?;
    Ok((file, dotlock))
}

/// Flush and commit `file`, reporting any deferred write error.
///
/// Dropping a `File` discards a write error that only surfaced at close, which
/// is exactly the case where a mailbox has been truncated but not rewritten.
pub(crate) fn commit(mut file: File) -> io::Result<()> {
    file.flush()?;
    file.sync_all()
}
