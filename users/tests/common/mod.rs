//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shared pseudo-terminal harness for the users-crate integration tests.
//!
//! Several utilities here (`mesg`, `tty`, `write`, `talk`, `newgrp`) only
//! exercise their primary code paths when standard input/output is a real
//! terminal. This harness spawns the built binary in a fresh PTY and captures
//! its output and exit status.
//!
//! IMPORTANT (portability): `portable_pty`'s `spawn_command` invalidates the
//! slave on macOS after the first spawn, so each PTY is used for exactly **one**
//! child. To observe state set by one invocation in a following one (e.g.
//! `mesg n` then `mesg`), run both in a single child via `run_sh`
//! (`/bin/sh -c "..."`), which shares one controlling terminal.
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

/// Run a `/bin/sh -c` script in a fresh PTY. Use this to chain several commands
/// against a single controlling terminal (one child → one PTY).
pub fn run_sh(script: &str) -> Option<(i32, String)> {
    let mut cmd = CommandBuilder::new("/bin/sh");
    cmd.arg("-c");
    cmd.arg(script);
    spawn_and_capture(cmd)
}

// ============================================================================
// talkd fixtures
//
// Shared by the `talk` and `talkd` test modules. They must share LOCK_FILE:
// both start a daemon, and two daemons racing on one socket path would
// interfere.
// ============================================================================

use std::fs::{File, OpenOptions};
use std::io;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};

/// File-based lock path serializing every test that starts a talkd.
const LOCK_FILE: &str = "/tmp/talk_test.lock";

/// RAII guard for file-based locking across test processes.
pub struct FileLockGuard {
    _file: File,
}

impl FileLockGuard {
    fn acquire() -> io::Result<Self> {
        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(LOCK_FILE)?;

        unsafe {
            if libc::flock(std::os::unix::io::AsRawFd::as_raw_fd(&file), libc::LOCK_EX) != 0 {
                return Err(io::Error::last_os_error());
            }
        }
        Ok(FileLockGuard { _file: file })
    }
}

/// Acquire the shared talkd test lock. The lock is released when the returned
/// guard is dropped (i.e. when the file is closed).
pub fn acquire_lock() -> FileLockGuard {
    FileLockGuard::acquire().expect("Failed to acquire test lock")
}

/// Find a built binary in target/release or target/debug.
pub fn find_binary(name: &str) -> Option<PathBuf> {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let workspace_root = Path::new(manifest_dir).parent()?;

    let release = workspace_root.join(format!("target/release/{}", name));
    if release.exists() {
        return Some(release);
    }

    let debug = workspace_root.join(format!("target/debug/{}", name));
    if debug.exists() {
        return Some(debug);
    }

    None
}

/// A unique socket path for one test.
pub fn test_socket_path() -> PathBuf {
    let pid = std::process::id();
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    PathBuf::from(format!("/tmp/talkd_test_{}_{}.sock", pid, timestamp))
}

/// Start a talkd with extra arguments, returning the child once its socket
/// exists. `None` when the binary is absent.
pub fn start_talkd_with(socket_path: &Path, extra: &[&str]) -> Option<Child> {
    let talkd_path = find_binary("talkd")?;

    let child = Command::new(&talkd_path)
        .arg("--socket")
        .arg(socket_path)
        .arg("--foreground")
        .args(extra)
        // The daemon's own log lines are gettext-wrapped; pin C so tests can
        // match on them.
        .env("LC_ALL", "C")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .ok()?;

    // Poll for the socket to appear, up to 2 seconds.
    for _ in 0..20 {
        thread::sleep(Duration::from_millis(100));
        if socket_path.exists() {
            return Some(child);
        }
    }

    // Socket didn't appear - return the child anyway so the caller can report.
    Some(child)
}

/// Start a talkd with default arguments.
pub fn start_talkd(socket_path: &Path) -> Option<Child> {
    start_talkd_with(socket_path, &[])
}

/// Stop a talkd and remove its socket.
pub fn stop_talkd(mut child: Child, socket_path: &Path) {
    let _ = child.kill();
    let _ = child.wait();
    let _ = std::fs::remove_file(socket_path);
}

/// Drain a child's stderr on a background thread into a shared buffer.
///
/// `start_talkd` pipes stderr but nothing reads it; a chatty daemon would
/// eventually block on a full pipe buffer. Tests that need to observe log
/// output must call this, and it is good hygiene for the rest.
pub fn drain_stderr(child: &mut Child) -> Arc<Mutex<String>> {
    let buf = Arc::new(Mutex::new(String::new()));
    if let Some(mut err) = child.stderr.take() {
        let sink = Arc::clone(&buf);
        thread::spawn(move || {
            let mut tmp = [0u8; 1024];
            loop {
                match err.read(&mut tmp) {
                    Ok(0) => break,
                    Ok(n) => sink
                        .lock()
                        .unwrap()
                        .push_str(&String::from_utf8_lossy(&tmp[..n])),
                    Err(_) => break,
                }
            }
        });
    }
    buf
}

/// Wait up to `timeout` for `needle` to appear in a drained stderr buffer.
pub fn wait_for_line(buf: &Arc<Mutex<String>>, needle: &str, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    while Instant::now() < deadline {
        if buf.lock().unwrap().contains(needle) {
            return true;
        }
        thread::sleep(Duration::from_millis(50));
    }
    false
}
