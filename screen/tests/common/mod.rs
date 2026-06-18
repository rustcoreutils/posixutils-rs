//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shared pseudo-terminal harness for the screen-crate integration tests.
//!
//! `stty` requires a controlling terminal on its standard input
//! (`Termios::from_fd(STDIN_FILENO)` + `tcsetattr`), and `tabs` writes hardware
//! tab-stop escape sequences to its standard output. Both therefore need a real
//! PTY to exercise their primary code paths.
//!
//! IMPORTANT (portability): `portable_pty`'s `spawn_command` invalidates the
//! slave on macOS after the first spawn, so each PTY is used for exactly **one**
//! child. To observe state set by one `stty` invocation in a following one
//! (e.g. `stty -echo` then `stty -a`), run both in a single child via
//! `run_sh` (`/bin/sh -c "..."`), which shares one controlling terminal.
//!
//! Each helper returns `None` when the platform/CI cannot allocate a PTY;
//! callers print a "Skipping PTY test" notice and return early.

use portable_pty::{native_pty_system, CommandBuilder, PtySize};
use std::io::Read;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

/// Spawn `cmd` in a fresh PTY (TERM=vt100), drain its terminal output, and
/// return the exit code plus the captured bytes (carriage returns stripped, as
/// the PTY line discipline maps `\n` to `\r\n`). `None` if no PTY is available.
fn spawn_and_capture(mut cmd: CommandBuilder) -> Option<(i32, String)> {
    let pty_system = native_pty_system();
    let pair = pty_system
        .openpty(PtySize {
            rows: 24,
            cols: 80,
            pixel_width: 0,
            pixel_height: 0,
        })
        .ok()?;

    cmd.env("TERM", "vt100");
    let mut child = pair.slave.spawn_command(cmd).ok()?;
    // Drop our handle to the slave so the master sees EOF once the child exits.
    drop(pair.slave);

    let mut reader = pair.master.try_clone_reader().ok()?;
    let buf = Arc::new(Mutex::new(Vec::new()));
    let buf_thread = Arc::clone(&buf);
    thread::spawn(move || {
        let mut tmp = [0u8; 4096];
        loop {
            match reader.read(&mut tmp) {
                Ok(0) => break, // EOF
                Ok(n) => buf_thread.lock().unwrap().extend_from_slice(&tmp[..n]),
                Err(_) => break,
            }
        }
    });

    let deadline = Instant::now() + Duration::from_secs(5);
    let status = loop {
        if let Ok(Some(s)) = child.try_wait() {
            break Some(s);
        }
        if Instant::now() > deadline {
            let _ = child.kill();
            break None;
        }
        thread::sleep(Duration::from_millis(20));
    };

    // Let the final output drain through the line discipline.
    thread::sleep(Duration::from_millis(150));

    let out = {
        let b = buf.lock().unwrap();
        String::from_utf8_lossy(&b).replace('\r', "")
    };
    drop(pair.master);

    let code = match status {
        Some(s) if s.success() => 0,
        Some(s) => s.exit_code() as i32,
        None => -1,
    };
    Some((code, out))
}

/// Run `bin` with `args` directly in a fresh PTY.
pub fn run(bin: &str, args: &[&str]) -> Option<(i32, String)> {
    let mut cmd = CommandBuilder::new(bin);
    for a in args {
        cmd.arg(*a);
    }
    spawn_and_capture(cmd)
}

/// Run a `/bin/sh -c` script in a fresh PTY. Use this to chain several commands
/// against a single controlling terminal (one child → one PTY).
pub fn run_sh(script: &str) -> Option<(i32, String)> {
    let mut cmd = CommandBuilder::new("/bin/sh");
    cmd.arg("-c");
    cmd.arg(script);
    spawn_and_capture(cmd)
}
