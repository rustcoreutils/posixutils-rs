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
//! The handler merely records that a `SIGINT` arrived; the main and composition
//! loops poll the flag. So that a blocking read returns promptly when the user
//! interrupts, the handler is installed with `sigaction` and `SA_RESTART`
//! cleared, and interactive input is read one byte at a time through
//! [`read_line_interruptible`] (whose `BufReader`-backed reads surface `EINTR`
//! as `ErrorKind::Interrupted`, unlike `BufRead::read_line`, which retries).

use std::io::{self, Read};
use std::sync::atomic::{AtomicBool, Ordering};

/// Set by the `SIGINT` handler; polled (and cleared) by the input loops.
static SIGINT_RECEIVED: AtomicBool = AtomicBool::new(false);

extern "C" fn sigint_handler(_signum: libc::c_int) {
    SIGINT_RECEIVED.store(true, Ordering::SeqCst);
}

/// Install the `SIGINT` handler.  `SA_RESTART` is deliberately left unset so a
/// blocking `read(2)` returns `EINTR` instead of being restarted.
pub fn setup() {
    unsafe {
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = sigint_handler as *const () as libc::sighandler_t;
        libc::sigemptyset(&mut sa.sa_mask);
        sa.sa_flags = 0; // no SA_RESTART => interrupted reads return EINTR
        libc::sigaction(libc::SIGINT, &sa, std::ptr::null_mut());
    }
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
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    let mut bytes: Vec<u8> = Vec::new();
    let mut byte = [0u8; 1];

    loop {
        match handle.read(&mut byte) {
            Ok(0) => break, // EOF
            Ok(_) => {
                bytes.push(byte[0]);
                if byte[0] == b'\n' {
                    break;
                }
            }
            Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {
                return Err(io::Error::from(io::ErrorKind::Interrupted));
            }
            Err(e) => return Err(e),
        }
    }

    if bytes.is_empty() {
        return Ok(0);
    }

    buf.push_str(&String::from_utf8_lossy(&bytes));
    Ok(bytes.len())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The installed handler records SIGINT in the flag instead of terminating,
    /// and `take_sigint` reports-and-clears it.
    #[test]
    fn sigint_sets_and_clears_flag() {
        setup();
        // Clear any stale state, then deliver a SIGINT to this thread.
        let _ = take_sigint();
        unsafe {
            libc::raise(libc::SIGINT);
        }
        assert!(take_sigint(), "SIGINT should set the flag");
        assert!(!take_sigint(), "flag should be cleared after being taken");
    }
}
