//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shared pseudo-terminal harness for the display-crate integration tests.
//!
//! `more` only takes its interactive render path when standard output is a
//! terminal (otherwise it copies its input through verbatim), and it reads
//! commands from stderr or `/dev/tty` rather than stdin.  Exercising that path
//! therefore needs a real PTY that can both capture output *and* send
//! keystrokes.  Neither existing harness in the tree does both:
//! `screen/tests/common/mod.rs` captures but never takes the master's writer,
//! and `editors/tests/pty/mod.rs` writes keystrokes but discards all output
//! (and `unwrap()`s, so it panics rather than skipping where no PTY exists).
//!
//! The part unique to this harness is [`MoreSession::grid`].
//! `more`'s `Terminal::display` emits a `Goto(col,row)` before every single
//! cell, so replaying the captured byte stream through a small CSI interpreter
//! recovers the exact screen contents — which is what makes assertions on
//! rendered output possible at all.
//!
//! Every fallible setup step returns `None` so a platform or CI runner without
//! PTY support skips instead of failing; callers print a notice and return.

use portable_pty::{native_pty_system, Child, CommandBuilder, PtySize};
use std::io::{Read, Write};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

/// How long to let `more` draw before sampling the screen.
const SETTLE: Duration = Duration::from_millis(300);

/// An interactive `more` running in a pseudo-terminal.
pub struct MoreSession {
    child: Box<dyn Child + Send + Sync>,
    writer: Box<dyn Write + Send>,
    output: Arc<Mutex<Vec<u8>>>,
    rows: usize,
    cols: usize,
}

impl MoreSession {
    /// Spawn `more` with `args` in a fresh `rows` x `cols` PTY.
    ///
    /// `LC_ALL` is forced to `C` unless the caller supplies a locale variable
    /// of its own, matching `plib::testing`'s policy: the render path is
    /// locale-sensitive, so a runner whose default locale is not C would
    /// otherwise change what is drawn.
    ///
    /// Returns `None` when no PTY can be allocated.
    pub fn spawn(args: &[&str], env: &[(&str, &str)], rows: u16, cols: u16) -> Option<Self> {
        Self::spawn_in(None, args, env, rows, cols)
    }

    /// Like [`spawn`], but runs `more` with a given working directory.
    ///
    /// Lets a test refer to its fixture by a bare filename. Anything that
    /// echoes the filename back — the `=` message, the prompt — is then
    /// independent of how long the system temporary directory happens to be,
    /// which differs by an order of magnitude between `/tmp` on Linux and
    /// `/var/folders/…/T` on macOS.
    pub fn spawn_in(
        cwd: Option<&std::path::Path>,
        args: &[&str],
        env: &[(&str, &str)],
        rows: u16,
        cols: u16,
    ) -> Option<Self> {
        let pair = native_pty_system()
            .openpty(PtySize {
                rows,
                cols,
                pixel_width: 0,
                pixel_height: 0,
            })
            .ok()?;

        let mut cmd = CommandBuilder::new(env!("CARGO_BIN_EXE_more"));
        for arg in args {
            cmd.arg(arg);
        }
        if let Some(dir) = cwd {
            cmd.cwd(dir);
        }
        cmd.env("TERM", "vt100");

        let test_controls_locale = env
            .iter()
            .any(|(key, _)| key.starts_with("LC_") || *key == "LANG" || *key == "LANGUAGE");
        if !test_controls_locale {
            cmd.env("LC_ALL", "C");
        }
        for (key, value) in env {
            cmd.env(key, value);
        }

        let child = pair.slave.spawn_command(cmd).ok()?;
        // Drop our handle to the slave so the master sees EOF once more exits.
        drop(pair.slave);

        let mut reader = pair.master.try_clone_reader().ok()?;
        let writer = pair.master.take_writer().ok()?;

        let output = Arc::new(Mutex::new(Vec::new()));
        let sink = Arc::clone(&output);
        thread::spawn(move || {
            let mut buf = [0u8; 4096];
            loop {
                match reader.read(&mut buf) {
                    Ok(0) | Err(_) => break,
                    Ok(n) => sink.lock().unwrap().extend_from_slice(&buf[..n]),
                }
            }
        });

        let session = Self {
            child,
            writer,
            output,
            rows: rows as usize,
            cols: cols as usize,
        };
        session.settle();
        Some(session)
    }

    /// Send a key sequence to `more`.
    pub fn keys(&mut self, s: &str) {
        let _ = self.writer.write_all(s.as_bytes());
        let _ = self.writer.flush();
        self.settle();
    }

    /// Give `more` time to redraw before the screen is sampled.
    pub fn settle(&self) {
        thread::sleep(SETTLE);
    }

    /// The screen as `more` has drawn it, one `String` per row, trailing
    /// blanks trimmed.
    pub fn grid(&self) -> Vec<String> {
        let bytes = self.output.lock().unwrap().clone();
        render_grid(&bytes, self.rows, self.cols)
    }

    /// A single screen row (0-based), or the empty string if out of range.
    pub fn row(&self, index: usize) -> String {
        self.grid().get(index).cloned().unwrap_or_default()
    }

    /// Send `q`, wait for exit, and report the exit code.
    pub fn quit(mut self) -> Option<i32> {
        self.keys("q");

        let deadline = Instant::now() + Duration::from_secs(5);
        while Instant::now() < deadline {
            if let Ok(Some(status)) = self.child.try_wait() {
                return Some(if status.success() {
                    0
                } else {
                    status.exit_code() as i32
                });
            }
            thread::sleep(Duration::from_millis(20));
        }

        let _ = self.child.kill();
        None
    }
}

/// Replay a terminal byte stream into a screen grid.
///
/// Deliberately minimal: `more` only ever emits absolute cursor positioning
/// (CUP), erase-line/erase-display, and SGR styling, so the interpreter
/// handles those and skips every other escape sequence rather than pretending
/// to be a full terminal emulator.
fn render_grid(bytes: &[u8], rows: usize, cols: usize) -> Vec<String> {
    let mut grid = vec![vec![' '; cols]; rows];
    let (mut row, mut col) = (0usize, 0usize);

    let mut i = 0;
    let mut text = Vec::new();

    while i < bytes.len() {
        if bytes[i] != 0x1b {
            text.push(bytes[i]);
            i += 1;
            continue;
        }

        place(&mut grid, &mut row, &mut col, &mut text, rows, cols);

        // ESC [ ... <final>, where <final> is 0x40..=0x7e
        if bytes.get(i + 1) == Some(&b'[') {
            let start = i + 2;
            let mut end = start;
            while end < bytes.len() && !(0x40..=0x7e).contains(&bytes[end]) {
                end += 1;
            }
            if end >= bytes.len() {
                break;
            }

            let params = String::from_utf8_lossy(&bytes[start..end]).into_owned();
            apply_csi(
                &mut grid, &mut row, &mut col, rows, cols, &params, bytes[end],
            );
            i = end + 1;
        } else if bytes.get(i + 1) == Some(&b']') {
            // OSC: skip to BEL or ST.
            let mut end = i + 2;
            while end < bytes.len() && bytes[end] != 0x07 {
                end += 1;
            }
            i = (end + 1).min(bytes.len());
        } else {
            // Two-byte escape such as a charset selector.
            i = (i + 2).min(bytes.len());
        }
    }

    place(&mut grid, &mut row, &mut col, &mut text, rows, cols);

    grid.into_iter()
        .map(|line| line.into_iter().collect::<String>().trim_end().to_string())
        .collect()
}

/// Write the pending run of content bytes into the grid at the cursor.
fn place(
    grid: &mut [Vec<char>],
    row: &mut usize,
    col: &mut usize,
    text: &mut Vec<u8>,
    rows: usize,
    cols: usize,
) {
    if text.is_empty() {
        return;
    }

    for ch in String::from_utf8_lossy(text).chars() {
        match ch {
            '\r' => *col = 0,
            '\n' => {
                *row += 1;
                *col = 0;
            }
            _ => {
                if *row < rows && *col < cols {
                    grid[*row][*col] = ch;
                }
                *col += 1;
            }
        }
    }

    text.clear();
}

fn apply_csi(
    grid: &mut [Vec<char>],
    row: &mut usize,
    col: &mut usize,
    rows: usize,
    cols: usize,
    params: &str,
    final_byte: u8,
) {
    let numbers: Vec<usize> = params
        .split(';')
        .map(|p| p.trim_start_matches('?').parse().unwrap_or(0))
        .collect();
    let nth = |i: usize| numbers.get(i).copied().unwrap_or(0);

    match final_byte {
        // CUP: ESC[row;colH — both 1-based, absent means 1.
        b'H' | b'f' => {
            *row = nth(0).max(1) - 1;
            *col = nth(1).max(1) - 1;
        }
        // ED: 2 clears the whole display.
        b'J' => {
            if nth(0) == 2 {
                for line in grid.iter_mut().take(rows) {
                    line.iter_mut().for_each(|c| *c = ' ');
                }
            }
        }
        // EL: 0/absent clears to end of line, 2 clears the whole line.
        b'K' if *row < rows => {
            let from = if nth(0) == 2 { 0 } else { *col };
            for c in grid[*row].iter_mut().take(cols).skip(from) {
                *c = ' ';
            }
        }
        _ => {}
    }
}
