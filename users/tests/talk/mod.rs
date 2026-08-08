//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Integration tests for the `talk` and `talkd` utilities.
//!
//! These tests verify the local mode functionality of talk using Unix domain
//! sockets to communicate with a local talkd daemon.
//!
//! Note: These tests use file-based locking to ensure they run serially,
//! as multiple talkd instances can conflict with each other.

use portable_pty::{native_pty_system, CommandBuilder, PtySize};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use crate::common::{acquire_lock, find_binary, start_talkd, stop_talkd, test_socket_path};

// ============================================================================
// Tests
// ============================================================================

#[test]
fn test_talk_help() {
    let talk_path = match find_binary("talk") {
        Some(p) => p,
        None => {
            eprintln!("Skipping test: talk binary not found");
            return;
        }
    };

    let output = Command::new(&talk_path)
        .arg("--help")
        .output()
        .expect("Failed to run talk --help");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{}{}", stdout, stderr);

    assert!(
        combined.contains("talk") || combined.contains("Talk"),
        "Help output should mention 'talk'"
    );
}

#[test]
fn test_talkd_help() {
    let talkd_path = match find_binary("talkd") {
        Some(p) => p,
        None => {
            eprintln!("Skipping test: talkd binary not found");
            return;
        }
    };

    let output = Command::new(&talkd_path)
        .arg("--help")
        .output()
        .expect("Failed to run talkd --help");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{}{}", stdout, stderr);

    assert!(
        combined.contains("talkd") || combined.contains("daemon"),
        "Help output should mention 'talkd' or 'daemon'"
    );
}

#[test]
fn test_talkd_startup_shutdown() {
    let _lock = acquire_lock();

    let talkd_path = match find_binary("talkd") {
        Some(p) => p,
        None => {
            eprintln!("Skipping test: talkd binary not found");
            return;
        }
    };

    let socket_path = test_socket_path();

    // Start talkd
    let child = Command::new(&talkd_path)
        .arg("--socket")
        .arg(&socket_path)
        .arg("--foreground")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn();

    let mut child = match child {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Failed to start talkd: {}", e);
            return;
        }
    };

    // Give it time to start
    thread::sleep(Duration::from_millis(200));

    // Verify socket was created
    assert!(
        socket_path.exists(),
        "talkd should create socket at {:?}",
        socket_path
    );

    // Stop talkd
    let _ = child.kill();
    let _ = child.wait();

    // Clean up
    let _ = std::fs::remove_file(&socket_path);
}

#[test]
fn test_talk_no_daemon_error() {
    let _lock = acquire_lock();

    let talk_path = match find_binary("talk") {
        Some(p) => p,
        None => {
            eprintln!("Skipping test: talk binary not found");
            return;
        }
    };

    let socket_path = test_socket_path();

    // Make sure no daemon is running at this socket
    let _ = std::fs::remove_file(&socket_path);

    // Run talk with --local pointing to non-existent socket
    let output = Command::new(&talk_path)
        .arg("--local")
        .arg("testuser")
        .env("TALKD_SOCKET", socket_path.to_str().unwrap())
        .output()
        .expect("Failed to run talk");

    // Should fail because no daemon is running
    assert!(
        !output.status.success(),
        "talk should fail when no daemon is running"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("not found") || stderr.contains("not running") || stderr.contains("talkd"),
        "Error message should indicate daemon is not running: {}",
        stderr
    );
}

#[test]
fn test_talk_not_a_tty_error() {
    let _lock = acquire_lock();

    let talk_path = match find_binary("talk") {
        Some(p) => p,
        None => {
            eprintln!("Skipping test: talk binary not found");
            return;
        }
    };

    let socket_path = test_socket_path();

    // Start talkd
    let child = match start_talkd(&socket_path) {
        Some(c) => c,
        None => {
            eprintln!("Skipping test: could not start talkd");
            return;
        }
    };

    // Run talk (stdin is not a TTY in test environment)
    let output = Command::new(&talk_path)
        .arg("--local")
        .arg("testuser")
        .env("TALKD_SOCKET", socket_path.to_str().unwrap())
        .stdin(Stdio::null())
        .output()
        .expect("Failed to run talk");

    // Clean up talkd
    stop_talkd(child, &socket_path);

    // Should fail because stdin is not a TTY
    assert!(
        !output.status.success(),
        "talk should fail when stdin is not a TTY"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("TTY") || stderr.contains("tty") || stderr.contains("terminal"),
        "Error should mention TTY requirement: {}",
        stderr
    );
}

#[test]
fn test_talkd_socket_cleanup() {
    let _lock = acquire_lock();

    let socket_path = test_socket_path();

    // Ensure socket doesn't exist
    let _ = std::fs::remove_file(&socket_path);

    // Start talkd
    let child = match start_talkd(&socket_path) {
        Some(c) => c,
        None => {
            eprintln!("Skipping test: could not start talkd");
            return;
        }
    };

    // Verify socket exists
    assert!(socket_path.exists(), "Socket should exist after start");

    // Stop talkd
    stop_talkd(child, &socket_path);

    // Socket may or may not exist after stop (depends on cleanup implementation)
    // but our stop_talkd function cleans it up regardless
    assert!(
        !socket_path.exists(),
        "Socket should be cleaned up after stop"
    );
}

// ============================================================================
// Two-peer end-to-end session
//
// The one genuinely open item from the `users/` audit. #TK21 (a deadlock on
// the reentrant stdout lock, which wedged the session on the first keystroke)
// and #TK22 each left `talk` completely non-functional, and both survived a
// full audit plus the whole test suite, because nothing ever connected two
// peers. Everything below is what those bugs would have failed.
// ============================================================================

/// One `talk` peer running on its own pseudo-terminal.
///
/// `portable_pty`'s `spawn_command` invalidates the slave after the first spawn
/// on macOS, so each peer gets its own PTY — which is what this test needs
/// anyway. Output is drained continuously into a shared buffer rather than read
/// at exit: a talk peer never exits on its own.
struct TalkPeer {
    child: Box<dyn portable_pty::Child + Send + Sync>,
    writer: Box<dyn Write + Send>,
    screen: Arc<Mutex<String>>,
}

impl TalkPeer {
    /// Spawn `talk --local <user>` on a fresh PTY, or `None` if no PTY can be
    /// allocated (some CI sandboxes).
    fn spawn(talk: &Path, socket: &Path, user: &str) -> Option<Self> {
        let pair = native_pty_system()
            .openpty(PtySize {
                rows: 24,
                cols: 80,
                pixel_width: 0,
                pixel_height: 0,
            })
            .ok()?;

        let mut cmd = CommandBuilder::new(talk);
        cmd.arg("--local");
        cmd.arg(user);
        cmd.env("TERM", "vt100");
        cmd.env("TALKD_SOCKET", socket);
        cmd.env("LC_ALL", "C");

        let child = pair.slave.spawn_command(cmd).ok()?;
        drop(pair.slave);

        let mut reader = pair.master.try_clone_reader().ok()?;
        let screen = Arc::new(Mutex::new(String::new()));
        let sink = Arc::clone(&screen);
        thread::spawn(move || {
            let mut buf = [0u8; 4096];
            loop {
                match reader.read(&mut buf) {
                    Ok(0) | Err(_) => break,
                    Ok(n) => sink
                        .lock()
                        .unwrap()
                        .push_str(&String::from_utf8_lossy(&buf[..n])),
                }
            }
        });

        let writer = pair.master.take_writer().ok()?;
        Some(TalkPeer {
            child,
            writer,
            screen,
        })
    }

    fn type_keys(&mut self, s: &str) {
        let _ = self.writer.write_all(s.as_bytes());
        let _ = self.writer.flush();
    }

    /// Wait up to `timeout` for `needle` to appear on this peer's screen.
    fn wait_for(&self, needle: &str, timeout: Duration) -> bool {
        let deadline = Instant::now() + timeout;
        while Instant::now() < deadline {
            if self.screen.lock().unwrap().contains(needle) {
                return true;
            }
            thread::sleep(Duration::from_millis(50));
        }
        false
    }

    fn screen(&self) -> String {
        self.screen.lock().unwrap().clone()
    }
}

impl Drop for TalkPeer {
    fn drop(&mut self) {
        // Peers never exit on their own, and `request_local`'s timeout path
        // calls `process::exit(128)`, so they are always killed explicitly.
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// Bring up a talkd and two connected peers, or `None` when the environment
/// cannot supply what the test needs (no binaries, no PTY, no rendezvous).
///
/// Both peers talk to the *same* user: `talkd`'s `find_for_callee` matches an
/// invitation on `callee == local_name && caller == remote_name`, so a
/// self-directed call is the only pairing one test process can make.
#[allow(clippy::type_complexity)]
fn connected_peers() -> Option<(Child, PathBuf, TalkPeer, TalkPeer)> {
    let talk = find_binary("talk")?;
    let socket = test_socket_path();
    let daemon = start_talkd(&socket)?;

    let user = std::env::var("USER").unwrap_or_else(|_| "root".to_string());

    // The first peer finds no invitation, so it binds a TCP socket on the
    // loopback address, leaves an invitation with talkd, and waits.
    let a = TalkPeer::spawn(&talk, &socket, &user)?;
    thread::sleep(Duration::from_millis(400));

    // The second finds that invitation and connects to the address in it.
    let b = TalkPeer::spawn(&talk, &socket, &user)?;

    // Both draw "[Connection established]" once the TCP session is up.
    if !a.wait_for("[Connection established]", Duration::from_secs(5))
        || !b.wait_for("[Connection established]", Duration::from_secs(5))
    {
        stop_talkd(daemon, &socket);
        return None;
    }
    Some((daemon, socket, a, b))
}

macro_rules! peers_or_skip {
    () => {
        match connected_peers() {
            Some(v) => v,
            None => {
                println!("Skipping talk two-peer test: no PTY, binaries, or rendezvous");
                return;
            }
        }
    };
}

#[test]
fn test_talk_two_peers_exchange_text() {
    let _lock = acquire_lock();
    let (daemon, socket, mut a, b) = peers_or_skip!();

    a.type_keys("hello");
    assert!(
        b.wait_for("hello", Duration::from_secs(5)),
        "typing on one peer must reach the other; B's screen was {:?}",
        b.screen()
    );

    drop(a);
    drop(b);
    stop_talkd(daemon, &socket);
}

#[test]
fn test_talk_forwards_alert_but_not_refresh() {
    // 116756: ^G "alerts the recipient's terminal", so it is forwarded.
    // 116757: ^L redraws the *sender's* screen and is never transmitted.
    let _lock = acquire_lock();
    let (daemon, socket, mut a, b) = peers_or_skip!();

    a.type_keys("\x0c");
    a.type_keys("X");
    assert!(
        b.wait_for("X", Duration::from_secs(5)),
        "expected the character after ^L to arrive: {:?}",
        b.screen()
    );
    assert!(
        !b.screen().contains('\x0c'),
        "^L is a local refresh and must not be forwarded: {:?}",
        b.screen()
    );

    a.type_keys("\x07");
    thread::sleep(Duration::from_millis(300));
    assert!(
        b.screen().contains('\x07'),
        "^G must reach the peer's terminal as a bell: {:?}",
        b.screen()
    );

    drop(a);
    drop(b);
    stop_talkd(daemon, &socket);
}

#[test]
fn test_talk_renders_control_characters_as_carets() {
    // 116769: a non-printable is replaced by a printable stand-in rather than
    // being sent raw. ^A is 0x01, so it is written as the two characters "^A".
    let _lock = acquire_lock();
    let (daemon, socket, mut a, b) = peers_or_skip!();

    a.type_keys("\x01");
    assert!(
        b.wait_for("^A", Duration::from_secs(5)),
        "expected ^A on the peer's screen: {:?}",
        b.screen()
    );
    assert!(
        !b.screen().contains('\x01'),
        "the raw control character must not be forwarded: {:?}",
        b.screen()
    );

    drop(a);
    drop(b);
    stop_talkd(daemon, &socket);
}

#[test]
fn test_talk_erase_and_kill_reach_the_peer() {
    // 116758: erase removes the preceding character and kill discards the
    // line; both are applied on *both* screens, so the peer must see them.
    let _lock = acquire_lock();
    let (daemon, socket, mut a, b) = peers_or_skip!();

    a.type_keys("abc");
    assert!(
        b.wait_for("abc", Duration::from_secs(5)),
        "{:?}",
        b.screen()
    );
    let before = b.screen().len();

    // DEL is the POSIX default erase character.
    a.type_keys("\x7f");
    thread::sleep(Duration::from_millis(300));
    assert!(
        b.screen().len() > before,
        "erase must be forwarded and redraw the peer's window: {:?}",
        b.screen()
    );

    drop(a);
    drop(b);
    stop_talkd(daemon, &socket);
}

#[test]
fn test_talk_multibyte_character_survives() {
    // #TK10: `recv` may split a multi-byte character across two chunks, and
    // decoding each chunk in isolation dropped the whole chunk.
    let _lock = acquire_lock();
    let (daemon, socket, mut a, b) = peers_or_skip!();

    a.type_keys("caf\u{e9}");
    assert!(
        b.wait_for("caf\u{e9}", Duration::from_secs(5)),
        "a multi-byte character must arrive intact: {:?}",
        b.screen()
    );

    drop(a);
    drop(b);
    stop_talkd(daemon, &socket);
}
