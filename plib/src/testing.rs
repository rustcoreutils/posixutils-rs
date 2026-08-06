//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::OsString;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Output, Stdio};
use std::thread;
use std::time::Duration;

/// Get the target directory for built binaries.
///
/// Handles cargo-llvm-cov which uses a custom target directory.
pub fn get_target_dir() -> String {
    std::env::var("CARGO_TARGET_DIR")
        .or_else(|_| std::env::var("CARGO_LLVM_COV_TARGET_DIR"))
        .unwrap_or_else(|_| {
            if cfg!(coverage) {
                String::from("target/llvm-cov-target")
            } else {
                String::from("target")
            }
        })
}

/// Get the current build profile ("debug" or "release").
pub fn get_profile() -> &'static str {
    if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    }
}

/// Get the full path to a built binary.
///
/// This assumes the current directory is within a workspace member crate
/// and navigates up to the workspace root to find the binary.
pub fn get_binary_path(cmd: &str) -> PathBuf {
    let target_dir = get_target_dir();
    let profile = get_profile();
    let relpath = format!("{}/{}/{}", target_dir, profile, cmd);

    std::env::current_dir()
        .unwrap()
        .parent()
        .unwrap() // Move up to the workspace root from the current package directory
        .join(relpath)
}

pub struct TestPlan {
    pub cmd: String,
    pub args: Vec<String>,
    pub stdin_data: String,
    pub expected_out: String,
    pub expected_err: String,
    pub expected_exit_code: i32,
}

pub struct TestPlanU8 {
    pub cmd: String,
    pub args: Vec<String>,
    pub stdin_data: Vec<u8>,
    pub expected_out: Vec<u8>,
    pub expected_err: Vec<u8>,
    pub expected_exit_code: i32,
}

/// A plan whose arguments and expectations are raw bytes rather than `String`.
///
/// `TestPlan::args` is `Vec<String>`, so it cannot express an operand that is
/// not valid UTF-8 — and POSIX pathname operands are byte strings, not
/// character strings. Utilities that are byte-clean internally therefore had no
/// way to prove it, and the one test in the tree that needed such an argument
/// (`display/tests/echo`) dropped to a raw `Command`. Expectations are `Vec<u8>`
/// for the same reason: a lossy-UTF-8 comparison would mask exactly the
/// corruption these tests exist to catch.
pub struct TestPlanOs {
    pub cmd: String,
    pub args: Vec<OsString>,
    pub stdin_data: Vec<u8>,
    pub expected_out: Vec<u8>,
    pub expected_err: Vec<u8>,
    pub expected_exit_code: i32,
}

/// Build an `OsString` from raw bytes, including sequences that are not valid
/// UTF-8.
///
/// Unix-only, which is where the byte-oriented pathname tests run.
#[cfg(unix)]
pub fn os_bytes(bytes: &[u8]) -> OsString {
    use std::os::unix::ffi::OsStrExt;
    std::ffi::OsStr::from_bytes(bytes).to_os_string()
}

/// Spawn a child process, retrying transient OS-level failures.
///
/// The test suite runs many tests in parallel, each forking child processes
/// (some, like the PTY-based `talk`/`write`/`tty` tests, fork several). Under
/// that load `fork`/`exec` can transiently fail with `EAGAIN` (RLIMIT_NPROC or
/// memory pressure) or `ETXTBSY` (the binary briefly held open for write by a
/// concurrent build). These are not test failures, so retry with backoff
/// instead of swallowing the error and panicking the first time it happens.
fn spawn_with_retry(command: &mut Command, cmd: &str) -> std::process::Child {
    use std::io::ErrorKind;

    const MAX_ATTEMPTS: u32 = 5;
    let mut last_err = None;
    for attempt in 0..MAX_ATTEMPTS {
        match command.spawn() {
            Ok(child) => return child,
            Err(e) => {
                let transient = matches!(
                    e.kind(),
                    ErrorKind::WouldBlock | ErrorKind::ResourceBusy | ErrorKind::Interrupted
                ) || e.raw_os_error() == Some(libc::EAGAIN)
                    || e.raw_os_error() == Some(libc::ETXTBSY);
                if !transient {
                    panic!("failed to spawn command {cmd}: {e}");
                }
                // Exponential backoff: 20ms, 40ms, 80ms, 160ms — but only when
                // another attempt will follow (no sleep before the final panic).
                if attempt + 1 < MAX_ATTEMPTS {
                    thread::sleep(Duration::from_millis(20u64 << attempt));
                }
                last_err = Some(e);
            }
        }
    }
    panic!(
        "failed to spawn command {cmd} after {MAX_ATTEMPTS} attempts: {}",
        last_err.expect("retry loop ran without recording an error")
    );
}

/// Run a test command with environment variables
///
/// This is the core test runner that supports setting environment variables.
/// Use this when tests need specific environment configuration.
pub fn run_test_base_with_env(
    cmd: &str,
    args: &[String],
    stdin_data: &[u8],
    env_vars: &[(&str, &str)],
) -> Output {
    let os_args: Vec<OsString> = args.iter().map(OsString::from).collect();
    run_test_base_os(cmd, &os_args, stdin_data, env_vars)
}

/// Core runner: like [`run_test_base_with_env`] but takes `OsString` arguments,
/// so operands need not be valid UTF-8.
pub fn run_test_base_os(
    cmd: &str,
    args: &[OsString],
    stdin_data: &[u8],
    env_vars: &[(&str, &str)],
) -> Output {
    let test_bin_path = get_binary_path(cmd);

    let mut command = Command::new(test_bin_path);
    command
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    // Default the spawned utility to the C locale for deterministic, host-locale-
    // independent output. The utilities honor LC_* at runtime (LC_COLLATE-sensitive
    // fnmatch ranges and strcoll, LC_CTYPE iswprint, LC_TIME strftime, ...), so a
    // runner whose default locale is not C (e.g. the GitHub macOS runner's
    // en_US.UTF-8) would otherwise change results. A test that needs a specific
    // locale provides its own LC_*/LANG variable, in which case the locale is left
    // entirely under its control (LC_ALL is not forced over it).
    let test_controls_locale = env_vars
        .iter()
        .any(|(key, _)| key.starts_with("LC_") || *key == "LANG" || *key == "LANGUAGE");
    if !test_controls_locale {
        command.env("LC_ALL", "C");
    }

    // Set environment variables
    for (key, value) in env_vars {
        command.env(key, value);
    }

    let mut child = spawn_with_retry(&mut command, cmd);

    // Separate the mutable borrow of stdin from the child process
    if let Some(mut stdin) = child.stdin.take() {
        let chunk_size = 1024; // Arbitrary chunk size, adjust if needed
        for chunk in stdin_data.chunks(chunk_size) {
            // Write each chunk
            if let Err(e) = stdin.write_all(chunk) {
                eprintln!("Error writing to stdin: {}", e);
                break;
            }
            // Flush after writing each chunk
            if let Err(e) = stdin.flush() {
                eprintln!("Error flushing stdin: {}", e);
                break;
            }

            // Sleep briefly to avoid CPU spinning
            thread::sleep(Duration::from_millis(10));
        }
        // Explicitly drop stdin to close the pipe
        drop(stdin);
    }

    // Ensure we wait for the process to complete after writing to stdin
    child.wait_with_output().expect("failed to wait for child")
}

/// Run a test command (without custom environment variables)
pub fn run_test_base(cmd: &str, args: &[String], stdin_data: &[u8]) -> Output {
    run_test_base_with_env(cmd, args, stdin_data, &[])
}

pub fn run_test(plan: TestPlan) {
    let output = run_test_base(&plan.cmd, &plan.args, plan.stdin_data.as_bytes());

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout, plan.expected_out);

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(stderr, plan.expected_err);

    assert_eq!(output.status.code(), Some(plan.expected_exit_code));
    if plan.expected_exit_code == 0 {
        assert!(output.status.success());
    }
}

/// Run a [`TestPlanOs`], asserting stdout, stderr and exit code byte-exactly.
pub fn run_test_os(plan: TestPlanOs) {
    let output = run_test_base_os(&plan.cmd, &plan.args, &plan.stdin_data, &[]);

    assert_eq!(
        output.stdout,
        plan.expected_out,
        "stdout mismatch: got {:?}, expected {:?}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&plan.expected_out)
    );
    assert_eq!(
        output.stderr,
        plan.expected_err,
        "stderr mismatch: got {:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(output.status.code(), Some(plan.expected_exit_code));
}

pub fn run_test_u8(plan: TestPlanU8) {
    let output = run_test_base(&plan.cmd, &plan.args, &plan.stdin_data);

    assert_eq!(output.stdout, plan.expected_out);

    assert_eq!(output.stderr, plan.expected_err);

    assert_eq!(output.status.code(), Some(plan.expected_exit_code));
    if plan.expected_exit_code == 0 {
        assert!(output.status.success());
    }
}

pub fn run_test_with_checker<F: FnMut(&TestPlan, &Output)>(plan: TestPlan, mut checker: F) {
    let output = run_test_base(&plan.cmd, &plan.args, plan.stdin_data.as_bytes());
    checker(&plan, &output);
}

/// Run a test with custom environment variables
///
/// Like `run_test` but allows setting environment variables for the subprocess.
pub fn run_test_with_env(plan: TestPlan, env_vars: &[(&str, &str)]) {
    let output =
        run_test_base_with_env(&plan.cmd, &plan.args, plan.stdin_data.as_bytes(), env_vars);

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout, plan.expected_out);

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(stderr, plan.expected_err);

    assert_eq!(output.status.code(), Some(plan.expected_exit_code));
    if plan.expected_exit_code == 0 {
        assert!(output.status.success());
    }
}

/// Run a test with custom environment variables and a checker function
///
/// Like `run_test_with_checker` but allows setting environment variables.
pub fn run_test_with_checker_and_env<F: FnMut(&TestPlan, &Output)>(
    plan: TestPlan,
    env_vars: &[(&str, &str)],
    mut checker: F,
) {
    let output =
        run_test_base_with_env(&plan.cmd, &plan.args, plan.stdin_data.as_bytes(), env_vars);
    checker(&plan, &output);
}

/// Name of an installed UTF-8 locale, or `None` if the host has none.
///
/// Locale-dependent behavior — character boundaries, case mapping, collation,
/// character classes — only differs from the byte-oriented C locale when a
/// multi-byte locale is actually available, and a stripped-down container may
/// have none. Tests that need one should return early rather than fail:
///
/// ```no_run
/// let Some(locale) = plib::testing::utf8_locale() else {
///     return;
/// };
/// ```
///
/// The search is case-insensitive but the returned name keeps its canonical
/// spelling: glibc locale names are case-sensitive, so `LC_ALL=c.utf8` is not
/// recognized and silently falls back to C.
pub fn utf8_locale() -> Option<String> {
    let avail = std::process::Command::new("locale")
        .arg("-a")
        .output()
        .ok()?;
    let list = String::from_utf8_lossy(&avail.stdout).to_lowercase();
    for name in ["C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8"] {
        if list.contains(&name.to_lowercase()) {
            return Some(name.to_string());
        }
    }
    None
}

/// Name of an installed locale matching one of `candidates`, or `None`.
///
/// For tests that need a *specific* locale rather than any UTF-8 one — Turkish
/// for dotless-i case mapping, say — which most hosts do not have installed.
pub fn locale_matching(candidates: &[&str]) -> Option<String> {
    let avail = std::process::Command::new("locale")
        .arg("-a")
        .output()
        .ok()?;
    let list = String::from_utf8_lossy(&avail.stdout).to_lowercase();
    candidates
        .iter()
        .find(|name| list.contains(&name.to_lowercase()))
        .map(|name| (*name).to_string())
}
