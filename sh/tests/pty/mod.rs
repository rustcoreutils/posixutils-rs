//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! PTY-based integration tests.
//!
//! The shell only enables job control, the vi-mode line editor and the other
//! interactive machinery when its standard input and standard error are
//! terminals, so none of that is reachable through the stdin-pipe harness in
//! `integration.rs`.
//!
//! Tests drive the real `sh` binary through a pseudo-terminal and assert on
//! files the shell was asked to write, rather than on the terminal stream: the
//! line editor redraws the line after every keystroke, so scraping the screen
//! would be both noisy and brittle.

use portable_pty::{native_pty_system, CommandBuilder, PtySize};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::thread;
use std::time::{Duration, Instant};
use tempfile::{tempdir, TempDir};

/// An interactive `sh` running in a pseudo-terminal.
struct ShPtySession {
    child: Box<dyn portable_pty::Child + Send + Sync>,
    writer: Box<dyn Write + Send>,
    dir: TempDir,
}

impl ShPtySession {
    fn new() -> Self {
        let dir = tempdir().unwrap();
        let pty_system = native_pty_system();
        let pair = pty_system
            .openpty(PtySize {
                rows: 24,
                cols: 80,
                pixel_width: 0,
                pixel_height: 0,
            })
            .unwrap();

        let mut cmd = CommandBuilder::new(plib::testing::get_binary_path("sh"));
        cmd.env("TERM", "vt100");
        // keep the tests independent of the user's history file
        cmd.env("HISTFILE", dir.path().join("history"));
        cmd.env("ENV", "");
        cmd.cwd(dir.path());

        let child = pair.slave.spawn_command(cmd).unwrap();
        drop(pair.slave);

        // The slave blocks once the terminal's output buffer fills, so the
        // master has to be drained continuously.
        let mut reader = pair.master.try_clone_reader().unwrap();
        thread::spawn(move || {
            let mut buf = [0u8; 4096];
            while matches!(reader.read(&mut buf), Ok(n) if n > 0) {}
        });
        let writer = pair.master.take_writer().unwrap();

        Self { child, writer, dir }
    }

    /// The directory the shell was started in; tests write their results here.
    fn path(&self, name: &str) -> PathBuf {
        self.dir.path().join(name)
    }

    /// Types `keys` as if at the keyboard, without a trailing newline.
    fn keys(&mut self, keys: &str) {
        self.writer.write_all(keys.as_bytes()).unwrap();
        self.writer.flush().unwrap();
        // the editor processes one keystroke at a time
        thread::sleep(Duration::from_millis(40));
    }

    /// Types a command line and submits it.
    fn line(&mut self, command: &str) {
        self.keys(command);
        self.keys("\r");
        // give the shell time to run it before the next line is typed
        thread::sleep(Duration::from_millis(120));
    }

    /// Submits `exit` and waits for the shell to terminate.
    fn exit(mut self) -> TempDir {
        self.line("exit");
        let deadline = Instant::now() + Duration::from_secs(5);
        while Instant::now() < deadline {
            if matches!(self.child.try_wait(), Ok(Some(_))) {
                break;
            }
            thread::sleep(Duration::from_millis(50));
        }
        let _ = self.child.kill();
        self.dir
    }
}

/// Reads a file the shell wrote, waiting briefly for it to appear.
fn read_result(path: &Path) -> String {
    let deadline = Instant::now() + Duration::from_secs(2);
    loop {
        if let Ok(contents) = std::fs::read_to_string(path) {
            return contents;
        }
        if Instant::now() >= deadline {
            panic!("{} was never written", path.display());
        }
        thread::sleep(Duration::from_millis(20));
    }
}

#[test]
fn dollar_dash_reports_an_interactive_shell() {
    let mut sh = ShPtySession::new();
    let out = sh.path("flags");
    sh.line(&format!("echo $- > '{}'", out.display()));
    let dir = sh.exit();
    let flags = read_result(&dir.path().join("flags"));
    assert!(
        flags.contains('i'),
        "expected $- to contain 'i', got {flags:?}"
    );
    // job control is on by default for an interactive shell
    assert!(
        flags.contains('m'),
        "expected $- to contain 'm', got {flags:?}"
    );
}

#[test]
fn jobs_lists_a_background_job_in_the_posix_format() {
    let mut sh = ShPtySession::new();
    let out = sh.path("jobs");
    sh.line("sleep 5 &");
    sh.line(&format!("jobs > '{}'", out.display()));
    sh.line("kill %1");
    let dir = sh.exit();
    let listing = read_result(&dir.path().join("jobs"));
    // "[%d] %c %s %s\n"
    assert!(
        listing.starts_with("[1] + Running sleep 5"),
        "got {listing:?}"
    );
}

#[test]
fn jobs_reports_a_signal_terminated_job_distinctly() {
    let mut sh = ShPtySession::new();
    let out = sh.path("jobs");
    sh.line("sleep 5 &");
    sh.line("kill -TERM %1");
    thread::sleep(Duration::from_millis(200));
    sh.line(&format!("jobs > '{}'", out.display()));
    let dir = sh.exit();
    let listing = read_result(&dir.path().join("jobs"));
    assert!(listing.contains("Terminated (SIGTERM)"), "got {listing:?}");
}

#[test]
fn fg_returns_the_jobs_exit_status() {
    let mut sh = ShPtySession::new();
    let out = sh.path("status");
    sh.line("sh -c 'sleep 0.2; exit 7' &");
    sh.line("fg");
    sh.line(&format!("echo $? > '{}'", out.display()));
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("status")).trim(), "7");
}

#[test]
fn bg_resumes_a_stopped_job() {
    let mut sh = ShPtySession::new();
    let out = sh.path("state");
    let marker = sh.path("resumed");
    sh.line(&format!(
        "{{ kill -STOP $$; echo resumed > '{}'; }} &",
        marker.display()
    ));
    thread::sleep(Duration::from_millis(200));
    sh.line("bg %1");
    thread::sleep(Duration::from_millis(300));
    sh.line(&format!("echo done > '{}'", out.display()));
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("resumed")).trim(), "resumed");
}

#[test]
fn vi_mode_recalls_the_previous_command_from_history() {
    // `k` in command mode steps back through the history; running the recalled
    // line again appends a second copy to the file.
    let mut sh = ShPtySession::new();
    let out = sh.path("history_test");
    sh.line("set -o vi");
    sh.line(&format!("echo ONE >> '{}'", out.display()));
    sh.keys("\x1b"); // ESC: leave insert mode
    sh.keys("k"); // recall the previous command
    sh.keys("\r");
    thread::sleep(Duration::from_millis(200));
    let dir = sh.exit();
    assert_eq!(
        read_result(&dir.path().join("history_test")),
        "ONE\nONE\n",
        "the recalled command should have run a second time"
    );
}

#[test]
fn vi_mode_edits_the_line_before_running_it() {
    // Type a line with a trailing typo, then delete it with `x` in command
    // mode before submitting.
    let mut sh = ShPtySession::new();
    let out = sh.path("edited");
    sh.line("set -o vi");
    sh.keys(&format!("echo TWO >> '{}'X", out.display()));
    sh.keys("\x1b"); // ESC: leaving insert mode steps back onto the typo
    sh.keys("x"); // delete it
    sh.keys("\r");
    thread::sleep(Duration::from_millis(200));
    let dir = sh.exit();
    assert_eq!(read_result(&dir.path().join("edited")), "TWO\n");
}
