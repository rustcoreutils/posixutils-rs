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
//! PTY to exercise their primary code paths. This harness opens a PTY pair and
//! keeps the slave open so multiple commands can be run against the *same*
//! terminal — the termios state set by one `stty` invocation persists and can be
//! observed by a following `stty -a`.
//!
//! Each test that uses the harness is skip-gated: `PtyHarness::new` returns
//! `None` when the platform/CI cannot allocate a PTY, and callers print a
//! "Skipping PTY test" notice and return early (matching `users/tests/{tty,write}`).

#![allow(dead_code)] // each test module uses a subset of these helpers

use portable_pty::{native_pty_system, CommandBuilder, PtyPair, PtySize};
use std::io::Read;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

/// A PTY pair plus a background thread that continuously drains the master into
/// a shared buffer. The slave stays open for the harness's lifetime so termios
/// state persists across successive `run` calls.
pub struct PtyHarness {
    pair: PtyPair,
    buf: Arc<Mutex<Vec<u8>>>,
}

impl PtyHarness {
    /// Open a PTY of the given size, or `None` if the platform cannot.
    pub fn new(rows: u16, cols: u16) -> Option<Self> {
        let pty_system = native_pty_system();
        let pair = pty_system
            .openpty(PtySize {
                rows,
                cols,
                pixel_width: 0,
                pixel_height: 0,
            })
            .ok()?;

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

        Some(Self { pair, buf })
    }

    /// Run `bin` with `args` in the PTY (TERM=vt100). Returns the exit code and
    /// the bytes the command wrote to the terminal during this run, with
    /// carriage returns stripped (the PTY line discipline maps `\n` to `\r\n`).
    pub fn run(&self, bin: &str, args: &[&str]) -> (i32, String) {
        let start_len = self.buf.lock().unwrap().len();

        let mut cmd = CommandBuilder::new(bin);
        for a in args {
            cmd.arg(*a);
        }
        cmd.env("TERM", "vt100");

        let mut child = self.pair.slave.spawn_command(cmd).expect("spawn in PTY");

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
        thread::sleep(Duration::from_millis(100));

        let out = {
            let b = self.buf.lock().unwrap();
            String::from_utf8_lossy(&b[start_len..]).replace('\r', "")
        };

        let code = match status {
            Some(s) if s.success() => 0,
            Some(s) => s.exit_code() as i32,
            None => -1,
        };
        (code, out)
    }
}
