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
//! set an `AtomicBool` (an async-signal-safe operation); the editor's input
//! loop polls the flags after each (possibly `EINTR`-interrupted) read.

use std::sync::atomic::{AtomicBool, Ordering};

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
}

extern "C" fn handle_sigcont(_: libc::c_int) {
    SIGCONT_RECEIVED.store(true, Ordering::SeqCst);
}

extern "C" fn handle_sigint(_: libc::c_int) {
    SIGINT_RECEIVED.store(true, Ordering::SeqCst);
}

extern "C" fn handle_hangup(_: libc::c_int) {
    HANGUP_RECEIVED.store(true, Ordering::SeqCst);
}

/// Install handlers for SIGHUP and SIGTERM (buffer preservation). Idempotent;
/// installed for both visual and ex modes.
pub fn install_hangup_handlers() {
    unsafe {
        libc::signal(
            libc::SIGHUP,
            handle_hangup as *const () as libc::sighandler_t,
        );
        libc::signal(
            libc::SIGTERM,
            handle_hangup as *const () as libc::sighandler_t,
        );
    }
}

/// Install the visual-mode signal handlers. Idempotent.
pub fn install_visual_handlers() {
    unsafe {
        libc::signal(
            libc::SIGWINCH,
            handle_sigwinch as *const () as libc::sighandler_t,
        );
        libc::signal(
            libc::SIGCONT,
            handle_sigcont as *const () as libc::sighandler_t,
        );
        libc::signal(
            libc::SIGINT,
            handle_sigint as *const () as libc::sighandler_t,
        );
    }
}

/// Atomically read and clear a signal flag.
pub fn take(flag: &AtomicBool) -> bool {
    flag.swap(false, Ordering::SeqCst)
}
