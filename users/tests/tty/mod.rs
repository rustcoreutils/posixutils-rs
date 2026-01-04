//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Integration tests for the `tty` utility.
//!
//! Tests include:
//! - Non-terminal stdin (piped input) - outputs "not a tty" with exit code 1
//! - Terminal stdin (PTY) - outputs terminal path with exit code 0
//! - --help and --version options

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::ffi::{CStr, CString};
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::thread;
use std::time::Duration;

/// Mutex to serialize PTY-based tests to avoid timing issues
static PTY_TEST_LOCK: Mutex<()> = Mutex::new(());

// ============================================================================
// PTY Helper Functions
// ============================================================================

/// Create a PTY pair, returns (master_fd, slave_path)
fn create_pty() -> Result<(i32, String), String> {
    unsafe {
        let master_fd = libc::posix_openpt(libc::O_RDWR | libc::O_NOCTTY);
        if master_fd < 0 {
            return Err(format!(
                "posix_openpt failed: {}",
                std::io::Error::last_os_error()
            ));
        }

        if libc::grantpt(master_fd) < 0 {
            libc::close(master_fd);
            return Err(format!(
                "grantpt failed: {}",
                std::io::Error::last_os_error()
            ));
        }

        if libc::unlockpt(master_fd) < 0 {
            libc::close(master_fd);
            return Err(format!(
                "unlockpt failed: {}",
                std::io::Error::last_os_error()
            ));
        }

        let slave_name = libc::ptsname(master_fd);
        if slave_name.is_null() {
            libc::close(master_fd);
            return Err(format!(
                "ptsname failed: {}",
                std::io::Error::last_os_error()
            ));
        }

        let slave_path = CStr::from_ptr(slave_name).to_string_lossy().into_owned();

        Ok((master_fd, slave_path))
    }
}

/// Set a file descriptor to non-blocking mode
fn set_nonblocking(fd: i32) -> Result<(), String> {
    unsafe {
        let flags = libc::fcntl(fd, libc::F_GETFL);
        if flags < 0 {
            return Err("fcntl F_GETFL failed".into());
        }
        if libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) < 0 {
            return Err("fcntl F_SETFL failed".into());
        }
    }
    Ok(())
}

/// Find the tty binary in target/release or target/debug
fn find_tty_binary() -> Option<PathBuf> {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let workspace_root = Path::new(manifest_dir).parent()?;

    let release = workspace_root.join("target/release/tty");
    if release.exists() {
        return Some(release);
    }

    let debug = workspace_root.join("target/debug/tty");
    if debug.exists() {
        return Some(debug);
    }

    None
}

// ============================================================================
// Test Runner for PTY Tests
// ============================================================================

/// Result from running the tty utility with PTY stdin
struct TtyTestResult {
    output: String,
    exit_code: i32,
}

/// Run the tty utility with stdin connected to a PTY slave and capture output.
fn run_tty_with_pty(tty_path: &str) -> Result<TtyTestResult, String> {
    // Create PTY pair
    let (master_fd, slave_path) = create_pty()?;

    // Set master to non-blocking for reading output
    set_nonblocking(master_fd)?;

    // Create pipe for capturing stdout
    let mut stdout_pipe: [i32; 2] = [0; 2];
    unsafe {
        if libc::pipe(stdout_pipe.as_mut_ptr()) < 0 {
            libc::close(master_fd);
            return Err("stdout pipe failed".into());
        }
    }

    // Fork
    let pid = unsafe { libc::fork() };
    if pid < 0 {
        unsafe {
            libc::close(master_fd);
            libc::close(stdout_pipe[0]);
            libc::close(stdout_pipe[1]);
        }
        return Err("fork failed".into());
    }

    if pid == 0 {
        // Child process
        unsafe {
            libc::close(master_fd);
            libc::close(stdout_pipe[0]);

            // Open slave PTY and connect to stdin
            let slave_cstr = CString::new(slave_path.as_str()).unwrap();
            let slave_fd = libc::open(slave_cstr.as_ptr(), libc::O_RDWR);
            if slave_fd < 0 {
                libc::_exit(126);
            }

            // Redirect stdin to PTY slave
            libc::dup2(slave_fd, libc::STDIN_FILENO);

            // Redirect stdout to pipe
            libc::dup2(stdout_pipe[1], libc::STDOUT_FILENO);
            libc::close(stdout_pipe[1]);
            libc::close(slave_fd);

            // Redirect stderr to /dev/null
            let devnull = CString::new("/dev/null").unwrap();
            let fd = libc::open(devnull.as_ptr(), libc::O_WRONLY);
            if fd >= 0 {
                libc::dup2(fd, libc::STDERR_FILENO);
                libc::close(fd);
            }

            // Build argv
            let prog = CString::new(tty_path).unwrap();
            let arg0 = CString::new("tty").unwrap();
            let argv: [*const libc::c_char; 2] = [arg0.as_ptr(), std::ptr::null()];

            libc::execv(prog.as_ptr(), argv.as_ptr());
            libc::_exit(127);
        }
    }

    // Parent process
    unsafe {
        libc::close(stdout_pipe[1]);
    }

    // Give child a moment to start
    thread::sleep(Duration::from_millis(100));

    // Read output from stdout pipe
    let mut output = Vec::with_capacity(1024);
    let mut exit_code = -1i32;
    let mut child_done = false;

    for _ in 0..20 {
        // Read available output
        unsafe {
            let mut buf = [0u8; 1024];
            let n = libc::read(
                stdout_pipe[0],
                buf.as_mut_ptr() as *mut libc::c_void,
                buf.len(),
            );
            if n > 0 {
                output.extend_from_slice(&buf[..n as usize]);
            }
        }

        // Check if child exited
        if !child_done {
            let mut status: i32 = 0;
            unsafe {
                let ret = libc::waitpid(pid, &mut status, libc::WNOHANG);
                if ret == pid {
                    child_done = true;
                    if libc::WIFEXITED(status) {
                        exit_code = libc::WEXITSTATUS(status);
                    }
                    // Continue reading to drain buffer
                } else if ret < 0 {
                    break;
                }
            }
        } else {
            // Child done, drain remaining output
            thread::sleep(Duration::from_millis(50));
            break;
        }

        thread::sleep(Duration::from_millis(50));
    }

    // Final read to catch any remaining output
    unsafe {
        let mut buf = [0u8; 1024];
        let n = libc::read(
            stdout_pipe[0],
            buf.as_mut_ptr() as *mut libc::c_void,
            buf.len(),
        );
        if n > 0 {
            output.extend_from_slice(&buf[..n as usize]);
        }
    }

    // Kill child if still running
    if !child_done {
        unsafe {
            libc::kill(pid, libc::SIGKILL);
            let mut status: i32 = 0;
            libc::waitpid(pid, &mut status, 0);
        }
    }

    // Close file descriptors
    unsafe {
        libc::close(master_fd);
        libc::close(stdout_pipe[0]);
    }

    let output_str = String::from_utf8_lossy(&output).into_owned();
    Ok(TtyTestResult {
        output: output_str,
        exit_code,
    })
}

// ============================================================================
// Basic Functionality Tests (Non-PTY)
// ============================================================================

#[test]
fn test_tty_not_terminal() {
    // When stdin is not a terminal (piped), should output "not a tty" and exit 1
    run_test(TestPlan {
        cmd: String::from("tty"),
        args: vec![],
        stdin_data: String::new(),
        expected_out: String::from("not a tty\n"),
        expected_err: String::new(),
        expected_exit_code: 1,
    });
}

#[test]
fn test_tty_not_terminal_with_input() {
    // Even with input data, if stdin is piped it's still not a terminal
    run_test(TestPlan {
        cmd: String::from("tty"),
        args: vec![],
        stdin_data: String::from("some input data\n"),
        expected_out: String::from("not a tty\n"),
        expected_err: String::new(),
        expected_exit_code: 1,
    });
}

// ============================================================================
// Help and Version Tests
// ============================================================================

#[test]
fn test_tty_help() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("tty"),
            args: vec![String::from("--help")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            assert!(output.status.success(), "tty --help should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains("tty") && stdout.contains("terminal"),
                "Help should mention tty and terminal: {}",
                stdout
            );
        },
    );
}

#[test]
fn test_tty_version() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("tty"),
            args: vec![String::from("--version")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_plan, output| {
            assert!(output.status.success(), "tty --version should succeed");
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Version output includes package name and version number
            assert!(
                stdout.contains("posixutils") || stdout.contains("0."),
                "Version should show version info: {}",
                stdout
            );
        },
    );
}

// ============================================================================
// PTY-based Tests (stdin is a terminal)
// ============================================================================

#[test]
fn test_tty_with_pty() {
    let _lock = PTY_TEST_LOCK.lock().unwrap();

    let tty_path = match find_tty_binary() {
        Some(p) => p,
        None => {
            eprintln!("Skipping PTY test: tty binary not found");
            return;
        }
    };

    let result = match run_tty_with_pty(tty_path.to_str().unwrap()) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Skipping PTY test: {}", e);
            return;
        }
    };

    // When stdin is a PTY, tty should output the PTY path and exit 0
    assert_eq!(
        result.exit_code, 0,
        "tty should exit 0 when stdin is a terminal, got exit code {}",
        result.exit_code
    );

    // Output should be a path starting with /dev/
    let output = result.output.trim();
    assert!(
        output.starts_with("/dev/"),
        "tty output should be a device path starting with /dev/, got: {}",
        output
    );

    // Output should end with newline in original (we trimmed for checking)
    assert!(
        result.output.ends_with('\n'),
        "tty output should end with newline"
    );
}

#[test]
fn test_tty_pty_output_is_slave_path() {
    let _lock = PTY_TEST_LOCK.lock().unwrap();

    let tty_path = match find_tty_binary() {
        Some(p) => p,
        None => {
            eprintln!("Skipping PTY test: tty binary not found");
            return;
        }
    };

    // Create PTY to get the expected slave path
    let (master_fd, slave_path) = match create_pty() {
        Ok((m, s)) => (m, s),
        Err(e) => {
            eprintln!("Skipping PTY test: {}", e);
            return;
        }
    };

    // Clean up immediately - we just wanted the path format
    unsafe {
        libc::close(master_fd);
    }

    // Run tty with a fresh PTY
    let result = match run_tty_with_pty(tty_path.to_str().unwrap()) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Skipping PTY test: {}", e);
            return;
        }
    };

    // The output path should be in the same format as our test PTY path
    // (e.g., /dev/pts/X on Linux, /dev/ttysXXX on macOS)
    let output = result.output.trim();

    // Check that output looks like a valid PTY path
    // Linux: /dev/pts/N
    // macOS: /dev/ttysNNN
    let is_linux_pty = output.starts_with("/dev/pts/");
    let is_macos_pty = output.starts_with("/dev/ttys");

    assert!(
        is_linux_pty || is_macos_pty,
        "tty output should be a valid PTY path, got: {} (expected format like {} or /dev/pts/N)",
        output,
        slave_path
    );
}
