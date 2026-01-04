//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! PTY-based integration tests for the `write` utility.
//!
//! These tests create pseudo-terminal pairs to fully test the write utility
//! without requiring actual logged-in users or real terminals.
//!
//! Note: PTY tests are serialized via a mutex to avoid timing issues when
//! running in parallel. The tests are sensitive to timing due to the
//! fork/exec/read pattern used to capture output from the write utility.

use plib::testing::{run_test_with_checker, TestPlan};
use std::ffi::{CStr, CString};
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::thread;
use std::time::Duration;

/// Mutex to serialize PTY-based tests to avoid timing issues
/// We use unwrap_or_else to recover from poisoning (if a previous test panicked)
static PTY_TEST_LOCK: Mutex<()> = Mutex::new(());

/// Lock the PTY test mutex, recovering from poisoning if necessary
fn lock_pty_tests() -> std::sync::MutexGuard<'static, ()> {
    PTY_TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner())
}

// ============================================================================
// PTY Helper Functions
// ============================================================================

/// Create a PTY pair, returns (master_fd, slave_fd, slave_path)
/// The slave_fd is kept open to ensure the PTY connection is established.
/// On some systems (especially macOS CI), the slave must be opened for
/// data written to it to be readable from the master.
fn create_pty() -> Result<(i32, i32, String), String> {
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

        // Open the slave to establish the PTY connection.
        // This is required on some systems (especially macOS) for data
        // written to the slave to be readable from the master.
        let slave_cstr = CString::new(slave_path.clone()).unwrap();
        let slave_fd = libc::open(slave_cstr.as_ptr(), libc::O_RDWR | libc::O_NOCTTY);
        if slave_fd < 0 {
            libc::close(master_fd);
            return Err(format!(
                "open slave failed: {}",
                std::io::Error::last_os_error()
            ));
        }

        Ok((master_fd, slave_fd, slave_path))
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

/// Find the write binary in target/release or target/debug
fn find_write_binary() -> Option<PathBuf> {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let workspace_root = Path::new(manifest_dir).parent()?;

    let release = workspace_root.join("target/release/write");
    if release.exists() {
        return Some(release);
    }

    let debug = workspace_root.join("target/debug/write");
    if debug.exists() {
        return Some(debug);
    }

    None
}

// ============================================================================
// Test Runner
// ============================================================================

/// Result from running the write utility
struct WriteTestResult {
    output: String,
    exit_code: i32,
}

/// Run the write utility with the given input and capture output.
///
/// # Arguments
/// * `write_path` - Path to the write binary
/// * `input` - Input to send to write's stdin
/// * `send_sigint` - If true, send SIGINT after sending input
/// * `target_user` - Optional user to target (defaults to current user)
/// * `target_terminal` - Optional terminal to target (defaults to PTY)
fn run_write_test(
    write_path: &str,
    input: &str,
    send_sigint: bool,
    target_user: Option<&str>,
    target_terminal: Option<&str>,
) -> Result<WriteTestResult, String> {
    // Create PTY pair if we need to capture output
    // slave_fd is kept open to establish the PTY connection (required on macOS CI)
    let (master_fd, slave_fd, slave_path) = if target_terminal.is_none() {
        create_pty()?
    } else {
        (-1, -1, String::new())
    };

    // Set master to non-blocking if we have one
    if master_fd >= 0 {
        set_nonblocking(master_fd)?;
    }

    // Create pipe for stdin
    let mut stdin_pipe: [i32; 2] = [0; 2];
    unsafe {
        if libc::pipe(stdin_pipe.as_mut_ptr()) < 0 {
            if master_fd >= 0 {
                libc::close(master_fd);
            }
            if slave_fd >= 0 {
                libc::close(slave_fd);
            }
            return Err("pipe failed".into());
        }
    }

    // Fork
    let pid = unsafe { libc::fork() };
    if pid < 0 {
        unsafe {
            if master_fd >= 0 {
                libc::close(master_fd);
            }
            if slave_fd >= 0 {
                libc::close(slave_fd);
            }
            libc::close(stdin_pipe[0]);
            libc::close(stdin_pipe[1]);
        }
        return Err("fork failed".into());
    }

    if pid == 0 {
        // Child process
        unsafe {
            if master_fd >= 0 {
                libc::close(master_fd);
            }
            // Close slave_fd in child - the write utility will open it fresh
            if slave_fd >= 0 {
                libc::close(slave_fd);
            }
            libc::close(stdin_pipe[1]);

            // Redirect stdin
            libc::dup2(stdin_pipe[0], libc::STDIN_FILENO);
            libc::close(stdin_pipe[0]);

            // Redirect stderr to /dev/null
            let devnull = CString::new("/dev/null").unwrap();
            let fd = libc::open(devnull.as_ptr(), libc::O_WRONLY);
            if fd >= 0 {
                libc::dup2(fd, libc::STDERR_FILENO);
                libc::close(fd);
            }

            // Get username
            let user = target_user
                .map(|s| s.to_string())
                .or_else(|| std::env::var("USER").ok())
                .unwrap_or_else(|| "nobody".to_string());

            // Determine terminal
            let terminal = target_terminal.map(|s| s.to_string()).unwrap_or(slave_path);

            // Build argv
            let prog = CString::new(write_path).unwrap();
            let arg0 = CString::new("write").unwrap();
            let arg1 = CString::new(user).unwrap();
            let arg2 = CString::new(terminal).unwrap();

            let argv: [*const libc::c_char; 4] = [
                arg0.as_ptr(),
                arg1.as_ptr(),
                arg2.as_ptr(),
                std::ptr::null(),
            ];

            libc::execv(prog.as_ptr(), argv.as_ptr());
            libc::_exit(127);
        }
    }

    // Parent process
    unsafe {
        libc::close(stdin_pipe[0]);
    }

    // Give write a moment to start
    thread::sleep(Duration::from_millis(200));

    // Send input line by line
    if !input.is_empty() {
        let mut remaining = input;
        while !remaining.is_empty() {
            let (line, rest) = match remaining.find('\n') {
                Some(pos) => (&remaining[..=pos], &remaining[pos + 1..]),
                None => (remaining, ""),
            };

            unsafe {
                libc::write(
                    stdin_pipe[1],
                    line.as_ptr() as *const libc::c_void,
                    line.len(),
                );
            }
            thread::sleep(Duration::from_millis(50));
            remaining = rest;
        }
    }

    // Optionally send SIGINT
    if send_sigint {
        // Wait longer to ensure header is written before interrupt
        thread::sleep(Duration::from_millis(300));
        unsafe {
            libc::kill(pid, libc::SIGINT);
        }
        // Wait for signal handler to complete
        thread::sleep(Duration::from_millis(200));
    }

    // Close stdin to trigger EOF
    unsafe {
        libc::close(stdin_pipe[1]);
    }

    // Give write utility time to process EOF and write final EOT
    thread::sleep(Duration::from_millis(200));

    // Read output from master PTY while waiting for child
    // Match the C test timing: 50 cycles of 100ms = 5 seconds max
    let mut output = Vec::with_capacity(4096);
    let mut child_done = false;
    let mut exit_code = -1i32;
    let max_wait = 50;
    let mut drain_count = 0; // Continue reading after child exits to drain buffer

    for _ in 0..max_wait {
        // Try to read output (non-blocking)
        if master_fd >= 0 {
            let mut buf = [0u8; 1024];
            unsafe {
                let n = libc::read(master_fd, buf.as_mut_ptr() as *mut libc::c_void, buf.len());
                if n > 0 {
                    output.extend_from_slice(&buf[..n as usize]);
                }
            }
        }

        // Check if child exited (only if not already done)
        if !child_done {
            let mut status: i32 = 0;
            unsafe {
                let ret = libc::waitpid(pid, &mut status, libc::WNOHANG);
                if ret == pid {
                    child_done = true;
                    if libc::WIFEXITED(status) {
                        exit_code = libc::WEXITSTATUS(status);
                    } else if libc::WIFSIGNALED(status) {
                        // Child was killed by signal
                        let sig = libc::WTERMSIG(status);
                        if sig == libc::SIGINT {
                            exit_code = 0; // POSIX says exit 0 on interrupt
                        } else {
                            exit_code = 128 + sig;
                        }
                    }
                    // Don't break - continue to drain output buffer
                } else if ret < 0 {
                    break;
                }
            }
        } else {
            // Child is done, drain buffer for a few more iterations
            drain_count += 1;
            if drain_count >= 5 {
                break;
            }
        }

        thread::sleep(Duration::from_millis(100));
    }

    // Final reads to catch any remaining output
    for _ in 0..3 {
        thread::sleep(Duration::from_millis(50));
        if master_fd >= 0 {
            let mut buf = [0u8; 1024];
            unsafe {
                let n = libc::read(master_fd, buf.as_mut_ptr() as *mut libc::c_void, buf.len());
                if n > 0 {
                    output.extend_from_slice(&buf[..n as usize]);
                }
            }
        }
    }

    // Kill child if still running
    if !child_done {
        unsafe {
            libc::kill(pid, libc::SIGKILL);
            let mut status: i32 = 0;
            libc::waitpid(pid, &mut status, 0);
        }
        exit_code = -1;
    }

    // Close master and slave
    if master_fd >= 0 {
        unsafe {
            libc::close(master_fd);
        }
    }
    if slave_fd >= 0 {
        unsafe {
            libc::close(slave_fd);
        }
    }

    let output_str = String::from_utf8_lossy(&output).into_owned();
    Ok(WriteTestResult {
        output: output_str,
        exit_code,
    })
}

// ============================================================================
// Error handling tests (non-PTY)
// ============================================================================

#[test]
fn test_write_no_args() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("write"),
            args: vec![],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 2,
        },
        |_plan, output| {
            assert_eq!(output.status.code(), Some(2));
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("USERNAME") || stderr.contains("required"),
                "Expected usage error mentioning USERNAME, got: {}",
                stderr
            );
        },
    );
}

#[test]
fn test_write_invalid_user() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("write"),
            args: vec![String::from("nonexistent_user_xyz_99999")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_plan, output| {
            assert_eq!(output.status.code(), Some(1));
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("No terminals found")
                    || stderr.contains("not logged")
                    || stderr.contains("nonexistent_user"),
                "Expected error about user not found, got: {}",
                stderr
            );
        },
    );
}

#[test]
fn test_write_invalid_terminal() {
    run_test_with_checker(
        TestPlan {
            cmd: String::from("write"),
            args: vec![String::from("root"), String::from("nonexistent_tty_xyz")],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_plan, output| {
            assert_eq!(output.status.code(), Some(1));
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains("Permission denied")
                    || stderr.contains("No such file")
                    || stderr.contains("Error"),
                "Expected error about terminal, got: {}",
                stderr
            );
        },
    );
}

// ============================================================================
// Caret notation verification
// ============================================================================

#[test]
fn test_caret_notation_math() {
    // Verify expected caret notation mappings:
    // ASCII 0 (NUL) -> ^@
    // ASCII 1 (SOH) -> ^A
    // ASCII 3 (ETX/Ctrl-C) -> ^C
    // ASCII 7 (BEL) -> ^G
    // ASCII 127 (DEL) -> ^?
    assert_eq!(0u8 + 64, b'@'); // ^@
    assert_eq!(1u8 + 64, b'A'); // ^A
    assert_eq!(3u8 + 64, b'C'); // ^C
    assert_eq!(7u8 + 64, b'G'); // ^G
    assert_eq!(26u8 + 64, b'Z'); // ^Z
}

// ============================================================================
// PTY-based integration tests
// ============================================================================

#[test]
fn test_pty_header_format() {
    let _lock = lock_pty_tests();
    let write_path = match find_write_binary() {
        Some(p) => p,
        None => {
            eprintln!("Skipping PTY test: write binary not found");
            return;
        }
    };

    let result = run_write_test(write_path.to_str().unwrap(), "\n", false, None, None)
        .expect("Failed to run test");

    // Check header format: "Message from USER (TTY) [DATE]..."
    assert!(
        result.output.contains("Message from"),
        "Missing 'Message from' in output: {}",
        result.output
    );
    assert!(
        result.output.contains("(") && result.output.contains(")"),
        "Missing TTY in parens in output: {}",
        result.output
    );
    assert!(
        result.output.contains("[") && result.output.contains("]"),
        "Missing date in brackets in output: {}",
        result.output
    );
    assert!(
        result.output.contains("..."),
        "Missing '...' in output: {}",
        result.output
    );
}

#[test]
fn test_pty_basic_message() {
    let _lock = lock_pty_tests();
    let write_path = match find_write_binary() {
        Some(p) => p,
        None => {
            eprintln!("Skipping PTY test: write binary not found");
            return;
        }
    };

    let result = run_write_test(
        write_path.to_str().unwrap(),
        "Hello, World!\n",
        false,
        None,
        None,
    )
    .expect("Failed to run test");

    assert!(
        result.output.contains("Message from"),
        "No header in output: {}",
        result.output
    );
    assert!(
        result.output.contains("Hello, World!"),
        "Message not found in output: {}",
        result.output
    );
    assert!(
        result.output.contains("EOT"),
        "No EOT in output: {}",
        result.output
    );
    assert_eq!(
        result.exit_code, 0,
        "Expected exit code 0, got {}",
        result.exit_code
    );
}

#[test]
fn test_pty_multiple_lines() {
    let _lock = lock_pty_tests();
    let write_path = match find_write_binary() {
        Some(p) => p,
        None => {
            eprintln!("Skipping PTY test: write binary not found");
            return;
        }
    };

    let result = run_write_test(
        write_path.to_str().unwrap(),
        "Line 1\nLine 2\nLine 3\n",
        false,
        None,
        None,
    )
    .expect("Failed to run test");

    assert!(
        result.output.contains("Line 1"),
        "Line 1 not found: {}",
        result.output
    );
    assert!(
        result.output.contains("Line 2"),
        "Line 2 not found: {}",
        result.output
    );
    assert!(
        result.output.contains("Line 3"),
        "Line 3 not found: {}",
        result.output
    );
}

#[test]
fn test_pty_empty_input() {
    let _lock = lock_pty_tests();
    let write_path = match find_write_binary() {
        Some(p) => p,
        None => {
            eprintln!("Skipping PTY test: write binary not found");
            return;
        }
    };

    let result = run_write_test(write_path.to_str().unwrap(), "", false, None, None)
        .expect("Failed to run test");

    assert!(
        result.output.contains("EOT"),
        "No EOT in output: {}",
        result.output
    );
}

#[test]
fn test_pty_sigint_handling() {
    let _lock = lock_pty_tests();
    let write_path = match find_write_binary() {
        Some(p) => p,
        None => {
            eprintln!("Skipping PTY test: write binary not found");
            return;
        }
    };

    let result = run_write_test(
        write_path.to_str().unwrap(),
        "Before interrupt\n",
        true, // send SIGINT
        None,
        None,
    )
    .expect("Failed to run test");

    // POSIX: interrupt signal should exit with status 0
    assert_eq!(
        result.exit_code, 0,
        "Expected exit code 0 on SIGINT, got {}",
        result.exit_code
    );

    // Should have written EOT
    assert!(
        result.output.contains("EOT"),
        "No EOT on interrupt: {}",
        result.output
    );
}

#[test]
fn test_pty_caret_notation() {
    let _lock = lock_pty_tests();
    let write_path = match find_write_binary() {
        Some(p) => p,
        None => {
            eprintln!("Skipping PTY test: write binary not found");
            return;
        }
    };

    // Send line with Ctrl-A (0x01) which should become ^A
    let input = "test\x01text\n";
    let result = run_write_test(write_path.to_str().unwrap(), input, false, None, None)
        .expect("Failed to run test");

    assert!(
        result.output.contains("^A"),
        "^A not found in output: {}",
        result.output
    );
}

#[test]
fn test_pty_alert_passthrough() {
    let _lock = lock_pty_tests();
    let write_path = match find_write_binary() {
        Some(p) => p,
        None => {
            eprintln!("Skipping PTY test: write binary not found");
            return;
        }
    };

    // Send line with BEL character - should pass through, not become ^G
    let input = "\x07\n";
    let result = run_write_test(write_path.to_str().unwrap(), input, false, None, None)
        .expect("Failed to run test");

    // BEL should NOT become ^G
    assert!(
        !result.output.contains("^G"),
        "BEL became ^G, should pass through: {}",
        result.output
    );
}

#[test]
fn test_pty_special_characters() {
    let _lock = lock_pty_tests();
    let write_path = match find_write_binary() {
        Some(p) => p,
        None => {
            eprintln!("Skipping PTY test: write binary not found");
            return;
        }
    };

    let input = "!@#$%^&*()_+-=[]{}|;':\",./<>?\n";
    let result = run_write_test(write_path.to_str().unwrap(), input, false, None, None)
        .expect("Failed to run test");

    assert!(
        result.output.contains("!@#$%"),
        "Special chars not found: {}",
        result.output
    );
    assert!(
        result.output.contains("<>?"),
        "Special chars not found: {}",
        result.output
    );
}

#[test]
fn test_pty_unicode() {
    let _lock = lock_pty_tests();
    let write_path = match find_write_binary() {
        Some(p) => p,
        None => {
            eprintln!("Skipping PTY test: write binary not found");
            return;
        }
    };

    // UTF-8: éàü
    let input = "Hello éàü world\n";
    let result = run_write_test(write_path.to_str().unwrap(), input, false, None, None)
        .expect("Failed to run test");

    assert!(
        result.output.contains("é") || result.output.contains("\u{e9}"),
        "Unicode not found: {}",
        result.output
    );
    assert!(
        result.output.contains("world"),
        "Message not found: {}",
        result.output
    );
}

#[test]
fn test_pty_invalid_user_exit() {
    let _lock = lock_pty_tests();
    let write_path = match find_write_binary() {
        Some(p) => p,
        None => {
            eprintln!("Skipping PTY test: write binary not found");
            return;
        }
    };

    // Test with nonexistent user - must NOT provide terminal so write tries utmpx lookup
    // We need a custom fork/exec here that doesn't pass terminal arg
    unsafe {
        let mut stdin_pipe: [i32; 2] = [0; 2];
        if libc::pipe(stdin_pipe.as_mut_ptr()) < 0 {
            panic!("pipe failed");
        }

        let pid = libc::fork();
        if pid < 0 {
            libc::close(stdin_pipe[0]);
            libc::close(stdin_pipe[1]);
            panic!("fork failed");
        }

        if pid == 0 {
            // Child
            libc::close(stdin_pipe[1]);
            libc::dup2(stdin_pipe[0], libc::STDIN_FILENO);
            libc::close(stdin_pipe[0]);

            // Redirect stderr to /dev/null
            let devnull = CString::new("/dev/null").unwrap();
            let fd = libc::open(devnull.as_ptr(), libc::O_WRONLY);
            if fd >= 0 {
                libc::dup2(fd, libc::STDERR_FILENO);
                libc::close(fd);
            }

            // Run write with only invalid username - no terminal arg
            let prog = CString::new(write_path.to_str().unwrap()).unwrap();
            let arg0 = CString::new("write").unwrap();
            let arg1 = CString::new("nonexistent_user_xyz_99999").unwrap();
            let argv: [*const libc::c_char; 3] = [arg0.as_ptr(), arg1.as_ptr(), std::ptr::null()];

            libc::execv(prog.as_ptr(), argv.as_ptr());
            libc::_exit(127);
        }

        // Parent
        libc::close(stdin_pipe[0]);
        libc::close(stdin_pipe[1]);

        let mut status: i32 = 0;
        libc::waitpid(pid, &mut status, 0);

        let exit_code = if libc::WIFEXITED(status) {
            libc::WEXITSTATUS(status)
        } else {
            -1
        };

        assert_ne!(exit_code, 0, "Expected non-zero exit for invalid user");
    }
}

#[test]
fn test_pty_invalid_terminal_exit() {
    let _lock = lock_pty_tests();
    let write_path = match find_write_binary() {
        Some(p) => p,
        None => {
            eprintln!("Skipping PTY test: write binary not found");
            return;
        }
    };

    let result = run_write_test(
        write_path.to_str().unwrap(),
        "",
        false,
        None, // use current user
        Some("/dev/nonexistent_tty_xyz"),
    );

    match result {
        Ok(r) => {
            assert_ne!(
                r.exit_code, 0,
                "Expected non-zero exit for invalid terminal"
            );
        }
        Err(_) => {
            // Error during setup is also acceptable
        }
    }
}
