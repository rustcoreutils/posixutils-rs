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

extern "C" fn handle_sigwinch(_: libc::c_int) {
    SIGWINCH_RECEIVED.store(true, Ordering::SeqCst);
}

extern "C" fn handle_sigcont(_: libc::c_int) {
    SIGCONT_RECEIVED.store(true, Ordering::SeqCst);
}

extern "C" fn handle_sigint(_: libc::c_int) {
    SIGINT_RECEIVED.store(true, Ordering::SeqCst);
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
