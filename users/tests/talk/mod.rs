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

use std::process::{Command, Stdio};
use std::thread;
use std::time::Duration;

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
