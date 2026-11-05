//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Asynchronous-event (signal) handling for mailx.
//!
//! POSIX mailx must not be terminated by `SIGINT`: in command mode an interrupt
//! aborts the command in progress and returns to the prompt; in input mode it
//! either (with `ignore`) prints `@` and discards the current line or, without
//! `ignore`, requires a second interrupt to abort the message (writing the
//! partial letter to the dead-letter file when `save` is set).
//!
//! A hangup (`SIGHUP`) ends mailx as in BSD and Heirloom mailx: in command
//! mode at once, with no writeback; while composing, after saving the
//! message to the dead-letter file.
//!
//! The handlers merely record that a signal arrived; the main and composition
//! loops poll the flags. So that a blocking read returns promptly when the user
//! interrupts, the handler is installed with `sigaction` and `SA_RESTART`
//! cleared, and interactive input is read one byte at a time through
//! [`read_line_interruptible`] (whose `BufReader`-backed reads surface `EINTR`
//! as `ErrorKind::Interrupted`, unlike `BufRead::read_line`, which retries).

use std::io::{self, Read};
use std::sync::atomic::{AtomicBool, Ordering};

/// Set by the `SIGINT` handler; polled (and cleared) by the input loops.
static SIGINT_RECEIVED: AtomicBool = AtomicBool::new(false);
/// Set by the `SIGHUP` handler; polled by the input loops.
static SIGHUP_RECEIVED: AtomicBool = AtomicBool::new(false);

extern "C" fn sigint_handler(_signum: libc::c_int) {
    SIGINT_RECEIVED.store(true, Ordering::SeqCst);
}

extern "C" fn sighup_handler(_signum: libc::c_int) {
    SIGHUP_RECEIVED.store(true, Ordering::SeqCst);
}

/// Install the `SIGINT` and `SIGHUP` handlers.  `SA_RESTART` is deliberately
/// left unset so a blocking `read(2)` returns `EINTR` instead of being
/// restarted.
pub fn setup() {
    install(libc::SIGINT, sigint_handler);
    install(libc::SIGHUP, sighup_handler);
}

/// Catch `signal` with `handler`, unless it was ignored when mailx started:
/// `nohup mailx` keeps its hangups ignored, as in BSD mailx.
fn install(signal: libc::c_int, handler: extern "C" fn(libc::c_int)) {
    // SAFETY: the handler only stores to an atomic, which is
    // async-signal-safe, and both sigaction structs are fully initialized.
    unsafe {
        let mut old: libc::sigaction = std::mem::zeroed();
        libc::sigaction(signal, std::ptr::null(), &mut old);
        if old.sa_sigaction == libc::SIG_IGN {
            return;
        }
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = handler as *const () as libc::sighandler_t;
        libc::sigemptyset(&mut sa.sa_mask);
        sa.sa_flags = 0; // no SA_RESTART => interrupted reads return EINTR
        libc::sigaction(signal, &sa, std::ptr::null_mut());
    }
}

/// Whether a `SIGHUP` has been delivered. It is not cleared: a hangup ends
/// mailx.
pub fn hangup() -> bool {
    SIGHUP_RECEIVED.load(Ordering::SeqCst)
}

/// Return whether a `SIGINT` has been delivered since the last check, clearing
/// the flag.
pub fn take_sigint() -> bool {
    SIGINT_RECEIVED.swap(false, Ordering::SeqCst)
}

/// Read a line from standard input, returning `ErrorKind::Interrupted` promptly
/// when a `SIGINT` interrupts the read.
///
/// Bytes are read individually through the locked, buffered standard-input
/// handle so that the shared input buffer is not bypassed (avoiding byte loss
/// between calls) while still propagating `EINTR` — `BufReader`'s `read`/
/// `fill_buf` do not swallow `Interrupted` the way `read_line` does.
pub fn read_line_interruptible(buf: &mut String) -> io::Result<usize> {
    let mut bytes = Vec::new();
    let n = read_bytes_interruptible(&mut bytes)?;
    buf.push_str(&String::from_utf8_lossy(&bytes));
    Ok(n)
}

/// As [`read_line_interruptible`], keeping the line as the bytes read.
///
/// A message body need not be UTF-8; this is the reader input mode uses.
pub fn read_bytes_interruptible(buf: &mut Vec<u8>) -> io::Result<usize> {
    let start = buf.len();
    let mut stdin = io::stdin().lock();
    let mut byte = [0u8; 1];
    loop {
        match stdin.read(&mut byte) {
            Ok(0) => break,
            Ok(_) => {
                buf.push(byte[0]);
                if byte[0] == b'\n' {
                    break;
                }
            }
            // An interrupted line is discarded, not half kept.
            Err(e) => {
                buf.truncate(start);
                return Err(e);
            }
        }
    }
    Ok(buf.len() - start)
}

/// Signals held off while the guard lives; any that arrive act once it is
/// dropped.
pub struct SignalsHeld {
    previous: libc::sigset_t,
}

/// What traditional mailx ignores for its quit, so that a hangup or an
/// interrupt does not cut a writeback short, nor throw the session's work
/// away while it waits for a lock.
pub const QUIT_SIGNALS: [libc::c_int; 3] = [libc::SIGHUP, libc::SIGINT, libc::SIGQUIT];

/// The signals whose default action would stop or end mailx while it holds
/// a lock, leaving others to wait on it -- and, for a dotlock, to find it
/// still there after mailx is gone.
pub const LOCK_SIGNALS: [libc::c_int; 3] = [libc::SIGQUIT, libc::SIGTERM, libc::SIGTSTP];

impl SignalsHeld {
    pub fn new(signals: &[libc::c_int]) -> SignalsHeld {
        // SAFETY: both sets are initialized by sigemptyset or by the call
        // that fills them, and the return values are checked.
        unsafe {
            let mut set: libc::sigset_t = std::mem::zeroed();
            libc::sigemptyset(&mut set);
            for &sig in signals {
                libc::sigaddset(&mut set, sig);
            }
            let mut previous: libc::sigset_t = std::mem::zeroed();
            libc::pthread_sigmask(libc::SIG_BLOCK, &set, &mut previous);
            SignalsHeld { previous }
        }
    }
}

impl Drop for SignalsHeld {
    fn drop(&mut self) {
        // SAFETY: `previous` is the mask `new` saved.
        unsafe {
            libc::pthread_sigmask(libc::SIG_SETMASK, &self.previous, std::ptr::null_mut());
        }
    }
}

/// The SIGINT flag is process-wide; tests that raise SIGINT, or that read
/// the flag, take this so that one cannot take the other's signal.
#[cfg(test)]
pub(crate) static SIGINT_TESTS: std::sync::Mutex<()> = std::sync::Mutex::new(());

/// For tests: a handler for one signal that records whether a path exists
/// when the signal is delivered, so a test can tell what a held signal
/// would have found had it acted.
#[cfg(test)]
pub(crate) struct PathWatch {
    signal: libc::c_int,
    _serial: std::sync::MutexGuard<'static, ()>,
}

#[cfg(test)]
mod path_watch {
    use std::sync::atomic::{AtomicI32, AtomicPtr, Ordering};

    /// The path looked for, a C string leaked for the life of the process.
    pub(super) static WATCHED: AtomicPtr<libc::c_char> = AtomicPtr::new(std::ptr::null_mut());
    /// -1 before delivery; then 1 if the path existed, 0 if not.
    pub(super) static SEEN: AtomicI32 = AtomicI32::new(-1);
    pub(super) static SERIAL: std::sync::Mutex<()> = std::sync::Mutex::new(());

    pub(super) extern "C" fn record(_: libc::c_int) {
        // SAFETY: access is async-signal-safe and WATCHED is a C string.
        let exists = unsafe { libc::access(WATCHED.load(Ordering::SeqCst), libc::F_OK) } == 0;
        SEEN.store(exists as i32, Ordering::SeqCst);
    }
}

#[cfg(test)]
impl PathWatch {
    pub(crate) fn new(signal: libc::c_int, path: &str) -> PathWatch {
        use std::sync::atomic::Ordering;
        let serial = path_watch::SERIAL.lock().unwrap_or_else(|e| e.into_inner());
        let path = std::ffi::CString::new(path).unwrap();
        path_watch::WATCHED.store(path.into_raw(), Ordering::SeqCst);
        path_watch::SEEN.store(-1, Ordering::SeqCst);
        // SAFETY: the handler only calls access and stores to an atomic,
        // and the sigaction struct is fully initialized.
        unsafe {
            let mut sa: libc::sigaction = std::mem::zeroed();
            sa.sa_sigaction = path_watch::record as *const () as libc::sighandler_t;
            libc::sigemptyset(&mut sa.sa_mask);
            libc::sigaction(signal, &sa, std::ptr::null_mut());
        }
        PathWatch {
            signal,
            _serial: serial,
        }
    }

    /// Whether the path existed when the signal was delivered; `None` if it
    /// has not been.
    pub(crate) fn seen(&self) -> Option<bool> {
        match path_watch::SEEN.load(std::sync::atomic::Ordering::SeqCst) {
            -1 => None,
            n => Some(n == 1),
        }
    }
}

#[cfg(test)]
impl Drop for PathWatch {
    fn drop(&mut self) {
        // SAFETY: restoring the default action for a signal is always valid.
        unsafe {
            libc::signal(self.signal, libc::SIG_DFL);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The installed handler records SIGINT in the flag instead of terminating,
    /// and `take_sigint` reports-and-clears it.
    #[test]
    fn sigint_sets_and_clears_flag() {
        let _serial = SIGINT_TESTS.lock().unwrap_or_else(|e| e.into_inner());
        setup();
        // Clear any stale state, then deliver a SIGINT to this thread.
        let _ = take_sigint();
        unsafe {
            libc::raise(libc::SIGINT);
        }
        assert!(take_sigint(), "SIGINT should set the flag");
        assert!(!take_sigint(), "flag should be cleared after being taken");
    }

    /// A signal raised while held is delivered only when the guard drops.
    #[test]
    fn held_signal_waits_for_the_guard() {
        let _serial = SIGINT_TESTS.lock().unwrap_or_else(|e| e.into_inner());
        setup();
        let _ = take_sigint();
        {
            let _held = SignalsHeld::new(&QUIT_SIGNALS);
            unsafe {
                libc::raise(libc::SIGINT);
            }
            assert!(!take_sigint(), "SIGINT was delivered while held");
        }
        assert!(take_sigint(), "SIGINT was not delivered after the guard");
    }
}
