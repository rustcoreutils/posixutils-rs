//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Asynchronous signal handling for vi/ex.
//!
//! POSIX requires an interactive editor to react to terminal resizes
//! (SIGWINCH) and job-control resume (SIGCONT), and to treat an interrupt
//! (SIGINT) as a command-cancel rather than terminating. Handlers here only
//! set an `AtomicBool` (an async-signal-safe operation) and write a byte to
//! a self-pipe; the editor's input waits on standard input and that pipe
//! together (see [`wait_for_input`]), so a signal ends the wait even when
//! it lands just before the wait begins -- which `EINTR` alone misses.

use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};

/// The self-pipe: its read end, and the write end the handlers write to
/// (-1 until the first handler is installed).
static WAKE_READ: AtomicI32 = AtomicI32::new(-1);
static WAKE_WRITE: AtomicI32 = AtomicI32::new(-1);

/// Note in the self-pipe that a signal came.  Async-signal-safe: `write(2)`
/// on a non-blocking pipe, and a full pipe already says so.
fn wake() {
    let fd = WAKE_WRITE.load(Ordering::SeqCst);
    if fd >= 0 {
        // SAFETY: one byte from a static, to a descriptor this module owns.
        unsafe { libc::write(fd, b"!".as_ptr().cast(), 1) };
    }
}

/// Make the self-pipe, once.
fn make_wake_pipe() {
    if WAKE_READ.load(Ordering::SeqCst) >= 0 {
        return;
    }
    let mut fds = [-1; 2];
    // SAFETY: `fds` has room for the two descriptors pipe(2) returns.
    if unsafe { libc::pipe(fds.as_mut_ptr()) } != 0 {
        return;
    }
    for fd in fds {
        // SAFETY: fcntl on descriptors just made.
        unsafe {
            let flags = libc::fcntl(fd, libc::F_GETFL);
            libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
            libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC);
        }
    }
    WAKE_READ.store(fds[0], Ordering::SeqCst);
    WAKE_WRITE.store(fds[1], Ordering::SeqCst);
}

/// Wait until standard input can be read (`Ok`) or a signal has come since
/// the last wait (`Err` of kind `Interrupted`).
pub fn wait_for_input() -> std::io::Result<()> {
    let wake = WAKE_READ.load(Ordering::SeqCst);
    let mut fds = [
        libc::pollfd {
            fd: libc::STDIN_FILENO,
            events: libc::POLLIN,
            revents: 0,
        },
        libc::pollfd {
            fd: wake,
            events: libc::POLLIN,
            revents: 0,
        },
    ];
    let count = if wake >= 0 { 2 } else { 1 };
    // SAFETY: `fds` holds `count` initialised entries.
    let n = unsafe { libc::poll(fds.as_mut_ptr(), count, -1) };
    let signalled = n < 0 || (count == 2 && fds[1].revents != 0);
    if n < 0 && std::io::Error::last_os_error().kind() != std::io::ErrorKind::Interrupted {
        return Err(std::io::Error::last_os_error());
    }
    if signalled {
        // Drained whether poll saw the byte or a signal cut it short, so a
        // byte left behind cannot report the same signal twice.
        if wake >= 0 {
            let mut drain = [0u8; 64];
            // SAFETY: reading into a local buffer from the non-blocking pipe.
            while unsafe { libc::read(wake, drain.as_mut_ptr().cast(), drain.len()) } > 0 {}
        }
        return Err(std::io::ErrorKind::Interrupted.into());
    }
    Ok(())
}

/// Set when the terminal window size changed (SIGWINCH).
pub static SIGWINCH_RECEIVED: AtomicBool = AtomicBool::new(false);
/// Set when the process resumed from job-control stop (SIGCONT).
pub static SIGCONT_RECEIVED: AtomicBool = AtomicBool::new(false);
/// Set when an interrupt was requested (SIGINT).
pub static SIGINT_RECEIVED: AtomicBool = AtomicBool::new(false);
/// Set on hangup or termination (SIGHUP/SIGTERM): preserve the buffer and exit.
pub static HANGUP_RECEIVED: AtomicBool = AtomicBool::new(false);

extern "C" fn handle_sigwinch(_: libc::c_int) {
    SIGWINCH_RECEIVED.store(true, Ordering::SeqCst);
    wake();
}

extern "C" fn handle_sigcont(_: libc::c_int) {
    SIGCONT_RECEIVED.store(true, Ordering::SeqCst);
    wake();
}

extern "C" fn handle_sigint(_: libc::c_int) {
    SIGINT_RECEIVED.store(true, Ordering::SeqCst);
    wake();
}

extern "C" fn handle_hangup(_: libc::c_int) {
    HANGUP_RECEIVED.store(true, Ordering::SeqCst);
    wake();
}

/// Install `handler` for `signum` *without* `SA_RESTART`.
///
/// This is the whole point of not using `libc::signal`: on the platforms this
/// targets it has BSD semantics, which include `SA_RESTART`, so the kernel
/// restarts an interrupted `read` instead of returning `EINTR`.  The editor's
/// input loop services signals only when a read comes back interrupted, so
/// with `SA_RESTART` the flags set below were never observed until the user
/// happened to press a key -- and SIGHUP/SIGTERM, which have no key coming,
/// took the unsaved buffer with them.
fn install(signum: libc::c_int, handler: extern "C" fn(libc::c_int)) {
    make_wake_pipe();
    unsafe {
        let mut action: libc::sigaction = std::mem::zeroed();
        action.sa_sigaction = handler as usize;
        libc::sigemptyset(&mut action.sa_mask);
        action.sa_flags = 0; // deliberately not SA_RESTART
        libc::sigaction(signum, &action, std::ptr::null_mut());
    }
}

/// Install handlers for SIGHUP and SIGTERM (buffer preservation). Idempotent;
/// installed for both visual and ex modes.
pub fn install_hangup_handlers() {
    install(libc::SIGHUP, handle_hangup);
    install(libc::SIGTERM, handle_hangup);
}

/// Install the ex-mode handler for SIGINT, which ex acts on itself
/// (94292-94306). Idempotent.
pub fn install_ex_handlers() {
    install(libc::SIGINT, handle_sigint);
}

/// Install the visual-mode signal handlers. Idempotent.
pub fn install_visual_handlers() {
    install(libc::SIGWINCH, handle_sigwinch);
    install(libc::SIGCONT, handle_sigcont);
    install(libc::SIGINT, handle_sigint);
}

/// Atomically read and clear a signal flag.
pub fn take(flag: &AtomicBool) -> bool {
    flag.swap(false, Ordering::SeqCst)
}
