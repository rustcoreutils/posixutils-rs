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

use crate::signals::{SignalsHeld, LOCK_SIGNALS};

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
        return Err(interrupted_or(io::Error::last_os_error()));
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
        return Err(interrupted_or(io::Error::last_os_error()));
    }
    Ok(())
}

/// `e`, having ended a lock wait. An interrupt that ended it has been
/// answered by giving up the wait, so the flag it set is cleared: left set,
/// it would also abort the next command read.
fn interrupted_or(e: io::Error) -> io::Error {
    if e.kind() == io::ErrorKind::Interrupted {
        crate::signals::take_sigint();
    }
    e
}

/// A `<mailbox>.lock` dotlock, removed when dropped.
///
/// Some delivery agents lock the spool only this way, by creating a lock file
/// beside it, so traditional mailx takes the dotlock too, before its kernel
/// locks. Creating it needs write access to the mailbox's directory; where
/// that is lacking, as in a spool directory writable only by group mail, the
/// dotlock is skipped and the kernel locks stand alone.
///
/// The signals whose default action would end mailx with the lock file left
/// behind are held for as long as it exists, and act once it is removed.
pub(crate) struct DotLock {
    path: String,
    _held: SignalsHeld,
}

impl Drop for DotLock {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}

/// How old a dotlock must be before it is taken as left behind by a holder
/// that died, and removed. Five minutes is the traditional figure.
const STALE_DOTLOCK: std::time::Duration = std::time::Duration::from_secs(300);

/// How long to sleep between attempts on a dotlock another process holds.
const DOTLOCK_RETRY: std::time::Duration = std::time::Duration::from_millis(100);

/// Take the dotlock for `path`, waiting while another process holds it.
///
/// Returns `None` when the lock file cannot be created at all, or when a
/// stale one cannot be removed: either way the kernel locks stand alone.
/// An interrupt gives up the wait with `ErrorKind::Interrupted`; while
/// signals are held, as they are through `quit`, none arrives.
fn dotlock(path: &str) -> io::Result<Option<DotLock>> {
    let lock_path = format!("{}.lock", path);
    loop {
        // Held from before the file exists, so it is never left behind.
        let held = SignalsHeld::new(&LOCK_SIGNALS);
        match crate::util::private_open()
            .write(true)
            .create_new(true)
            .open(&lock_path)
        {
            Ok(_) => {
                return Ok(Some(DotLock {
                    path: lock_path,
                    _held: held,
                }))
            }
            Err(e) if e.kind() == io::ErrorKind::AlreadyExists => drop(held),
            Err(_) => return Ok(None),
        }
        let stale = fs::metadata(&lock_path)
            .and_then(|m| m.modified())
            .ok()
            .and_then(|t| t.elapsed().ok())
            .is_some_and(|age| age > STALE_DOTLOCK);
        if stale {
            if fs::remove_file(&lock_path).is_err() {
                return Ok(None);
            }
        } else if crate::signals::take_sigint() {
            return Err(io::ErrorKind::Interrupted.into());
        } else {
            std::thread::sleep(DOTLOCK_RETRY);
        }
    }
}

/// Open `path` to read under its dotlock and a shared lock.
///
/// Both are held while the file and the dotlock returned are, so a
/// delivery in progress is waited for rather than half read.
pub(crate) fn open_read_locked(path: &str) -> io::Result<(File, Option<DotLock>)> {
    let file = File::open(path)?;
    let dotlock = dotlock(path)?;
    lock(&file, false)?;
    Ok((file, dotlock))
}

/// Open `path` for a locked read-modify-write, creating it if absent.
///
/// The dotlock returned with the file must be held until the file is
/// committed.
///
/// A file renamed over `path` while the lock was awaited -- another mailx
/// prepending to the same mbox -- would leave the lock on a file no longer
/// there, so the lock is taken again until it is on the file `path` names.
pub(crate) fn open_locked(path: &str) -> io::Result<(File, Option<DotLock>)> {
    use std::os::unix::fs::MetadataExt;

    let dotlock = dotlock(path)?;
    loop {
        let file = crate::util::private_open()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(path)?;
        lock(&file, true)?;
        let (held, named) = (file.metadata()?, fs::metadata(path)?);
        if (held.dev(), held.ino()) == (named.dev(), named.ino()) {
            return Ok((file, dotlock));
        }
    }
}

/// Flush and commit `file`, reporting any deferred write error.
///
/// Dropping a `File` discards a write error that only surfaced at close, which
/// is exactly the case where a mailbox has been truncated but not rewritten.
pub(crate) fn commit(mut file: File) -> io::Result<()> {
    file.flush()?;
    file.sync_all()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::fs::MetadataExt;

    /// A file renamed over the mailbox while a writer waits for its lock is
    /// the one the writer ends up holding, not the inode it first opened.
    #[test]
    fn a_lock_taken_after_a_rename_is_on_the_new_file() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let path = dir.path().join("mbox");
        fs::write(&path, "old\n").unwrap();
        let holder = File::open(&path).unwrap();
        // SAFETY: the fd is valid for the lifetime of `holder`.
        assert_eq!(unsafe { libc::flock(holder.as_raw_fd(), libc::LOCK_EX) }, 0);

        let waiting = path.to_str().unwrap().to_string();
        let waiter = std::thread::spawn(move || open_locked(&waiting).map(|(f, _)| f));
        std::thread::sleep(std::time::Duration::from_millis(200));
        let replacement = dir.path().join("new");
        fs::write(&replacement, "new\n").unwrap();
        fs::rename(&replacement, &path).unwrap();
        drop(holder);

        let locked = waiter.join().unwrap().unwrap();
        assert_eq!(
            locked.metadata().unwrap().ino(),
            fs::metadata(&path).unwrap().ino()
        );
    }

    /// What `dotlock` returns for `path`, or `None` if it is still waiting
    /// after a few seconds.
    fn dotlock_within_seconds(path: &std::path::Path) -> Option<io::Result<bool>> {
        let path = path.to_str().unwrap().to_string();
        let (tx, rx) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            let _ = tx.send(dotlock(&path).map(|lock| lock.is_some()));
        });
        rx.recv_timeout(std::time::Duration::from_secs(5)).ok()
    }

    /// A stale dotlock that cannot be removed, as in a spool directory the
    /// user cannot write to, is passed over rather than retried forever.
    #[test]
    fn an_unremovable_stale_dotlock_is_passed_over() {
        use std::os::unix::fs::PermissionsExt;

        // SAFETY: geteuid has no preconditions.
        if unsafe { libc::geteuid() } == 0 {
            return; // root can remove the lock whatever the mode
        }
        let dir = plib::tmp::TempDir::new().unwrap();
        let path = dir.path().join("spool");
        fs::write(&path, "").unwrap();
        let lock = File::create(dir.path().join("spool.lock")).unwrap();
        let old = std::time::SystemTime::now() - 2 * STALE_DOTLOCK;
        lock.set_modified(old).unwrap();
        fs::set_permissions(dir.path(), fs::Permissions::from_mode(0o555)).unwrap();

        let result = dotlock_within_seconds(&path);
        fs::set_permissions(dir.path(), fs::Permissions::from_mode(0o755)).unwrap();
        assert!(
            matches!(result, Some(Ok(false))),
            "stale lock not passed over: {:?}",
            result
        );
    }

    /// An interrupt ends a wait on a live dotlock, and is answered there.
    #[test]
    fn an_interrupt_ends_a_dotlock_wait() {
        let _serial = crate::signals::SIGINT_TESTS
            .lock()
            .unwrap_or_else(|e| e.into_inner());
        crate::signals::setup();
        let dir = plib::tmp::TempDir::new().unwrap();
        let path = dir.path().join("spool");
        fs::write(&path, "").unwrap();
        File::create(dir.path().join("spool.lock")).unwrap();

        // SAFETY: the handler `setup` installed only sets a flag.
        unsafe {
            libc::raise(libc::SIGINT);
        }
        let result = dotlock_within_seconds(&path);
        assert!(
            matches!(&result, Some(Err(e)) if e.kind() == io::ErrorKind::Interrupted),
            "wait not interrupted: {:?}",
            result
        );
        assert!(!crate::signals::take_sigint(), "interrupt left pending");
    }

    /// An interrupt that ends a kernel lock wait is answered there, so it
    /// does not abort the command read next.
    #[test]
    fn an_interrupted_lock_wait_clears_the_interrupt() {
        use std::os::unix::thread::JoinHandleExt;

        let _serial = crate::signals::SIGINT_TESTS
            .lock()
            .unwrap_or_else(|e| e.into_inner());
        crate::signals::setup();
        let _ = crate::signals::take_sigint();
        let dir = plib::tmp::TempDir::new().unwrap();
        let path = dir.path().join("spool");
        fs::write(&path, "").unwrap();
        let holder = File::open(&path).unwrap();
        // SAFETY: the fd is valid for the lifetime of `holder`.
        assert_eq!(unsafe { libc::flock(holder.as_raw_fd(), libc::LOCK_EX) }, 0);

        let waiting = path.clone();
        let waiter = std::thread::spawn(move || lock(&File::open(&waiting).unwrap(), false));
        std::thread::sleep(std::time::Duration::from_millis(200));
        // SAFETY: the thread is still running, blocked in flock.
        unsafe {
            libc::pthread_kill(waiter.as_pthread_t(), libc::SIGINT);
        }
        let result = waiter.join().unwrap();
        assert_eq!(
            result.map_err(|e| e.kind()),
            Err(io::ErrorKind::Interrupted)
        );
        assert!(!crate::signals::take_sigint(), "interrupt left pending");
    }

    /// A signal whose default action would end mailx is held while it owns
    /// a dotlock, and acts only once the lock file is gone.
    #[test]
    fn signals_wait_for_a_dotlock_to_go() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let path = dir.path().join("spool");
        let watch =
            crate::signals::PathWatch::new(libc::SIGTSTP, &format!("{}.lock", path.display()));
        let lock = dotlock(path.to_str().unwrap()).unwrap().expect("dotlock");
        // SAFETY: raise sends the signal to this thread; the handler only
        // records what it finds.
        unsafe {
            libc::raise(libc::SIGTSTP);
        }
        assert_eq!(
            watch.seen(),
            None,
            "the signal acted while the lock was held"
        );
        drop(lock);
        assert_eq!(
            watch.seen(),
            Some(false),
            "the signal did not wait for the lock to go"
        );
    }
}
