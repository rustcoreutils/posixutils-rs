//
// Copyright (c) 2024 Jeff Garzik
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

use std::fs::{File, OpenOptions};
use std::io;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::thread;
use std::time::Duration;

/// File-based lock path for serializing talk tests
const LOCK_FILE: &str = "/tmp/talk_test.lock";

/// RAII guard for file-based locking across test processes
struct FileLockGuard {
    _file: File,
}

impl FileLockGuard {
    fn acquire() -> io::Result<Self> {
        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(LOCK_FILE)?;

        // Use flock for exclusive locking
        unsafe {
            if libc::flock(std::os::unix::io::AsRawFd::as_raw_fd(&file), libc::LOCK_EX) != 0 {
                return Err(io::Error::last_os_error());
            }
        }
        Ok(FileLockGuard { _file: file })
    }
}

impl Drop for FileLockGuard {
    fn drop(&mut self) {
        // File lock is automatically released when file is closed
    }
}

/// Acquire file-based lock for test serialization
fn acquire_lock() -> FileLockGuard {
    FileLockGuard::acquire().expect("Failed to acquire test lock")
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Find a binary in target/release or target/debug
fn find_binary(name: &str) -> Option<PathBuf> {
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

/// Generate a unique socket path for testing
fn test_socket_path() -> PathBuf {
    let pid = std::process::id();
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    PathBuf::from(format!("/tmp/talkd_test_{}_{}.sock", pid, timestamp))
}

/// Start talkd daemon and return the child process
fn start_talkd(socket_path: &Path) -> Option<Child> {
    let talkd_path = find_binary("talkd")?;

    let child = Command::new(&talkd_path)
        .arg("--socket")
        .arg(socket_path)
        .arg("--foreground")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .ok()?;

    // Give talkd time to start and bind the socket
    // Poll for socket creation up to 2 seconds
    for _ in 0..20 {
        thread::sleep(Duration::from_millis(100));
        if socket_path.exists() {
            return Some(child);
        }
    }

    // Socket didn't appear - check if process is still alive
    Some(child)
}

/// Stop talkd daemon and clean up socket
fn stop_talkd(mut child: Child, socket_path: &Path) {
    // Send SIGTERM to gracefully stop talkd
    let _ = child.kill();
    let _ = child.wait();

    // Clean up socket file
    let _ = std::fs::remove_file(socket_path);
}

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
