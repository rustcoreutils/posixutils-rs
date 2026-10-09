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
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::thread;
use std::time::Duration;

/// Get the full path to a built binary.
///
/// Cargo puts an integration-test executable in `<bin dir>/deps/` and the
/// package's binaries in `<bin dir>`, whatever the target directory, profile,
/// `--target` triple or coverage wrapper, so the binaries are found from the
/// running test executable rather than reconstructed from guesses at those.
/// The platform's executable suffix (`.exe` on Windows) is appended.
pub fn get_binary_path(cmd: &str) -> PathBuf {
    binary_dir().join(format!("{cmd}{}", std::env::consts::EXE_SUFFIX))
}

/// The directory holding the workspace's built binaries: the running test
/// executable's own directory, or its parent when that is `deps`.
fn binary_dir() -> PathBuf {
    let exe = std::env::current_exe().expect("locate the running test executable");
    let dir = exe
        .parent()
        .expect("test executable has a parent directory");
    if dir.file_name().is_some_and(|name| name == "deps") {
        dir.parent()
            .expect("deps has a parent directory")
            .to_path_buf()
    } else {
        dir.to_path_buf()
    }
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
///
/// On Windows this is always `C.UTF-8`, without asking `locale -a` (Windows
/// has no such command, and a `locale.exe` found on `PATH` -- Git Bash's --
/// lists a different system's locales): plib supports UTF-8 on every Windows
/// system, and resolves `C.UTF-8` to Unicode characters in UTF-8 with the C
/// locale's byte-order collation, as glibc does (see
/// [`crate::diag::init_locale`]).
pub fn utf8_locale() -> Option<String> {
    #[cfg(windows)]
    {
        Some("C.UTF-8".to_string())
    }
    #[cfg(not(windows))]
    {
        installed_locale(&["C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8"])
    }
}

/// The text a utility reports when it cannot open `path`, which must not
/// exist: the system's own words for the failed open, as
/// [`crate::diag::io_error_text`] gives them. They differ between systems
/// (Windows has "The system cannot find the file specified." where Unix has
/// "No such file or directory", and another message again when a directory
/// in the path is missing), so a test asks rather than spelling them out.
///
/// A relative `path` is taken from the test's own working directory.
pub fn open_error_text(path: impl AsRef<Path>) -> String {
    let path = path.as_ref();
    let err = std::fs::File::open(path).expect_err(&format!("{} must not exist", path.display()));
    crate::diag::io_error_text(&err)
}

/// Name of an installed locale matching one of `candidates`, or `None`.
///
/// For tests that need a *specific* locale rather than any UTF-8 one — Turkish
/// for dotless-i case mapping, say — which most hosts do not have installed.
///
/// On Windows this is always `None`: plib gives a POSIX locale name meaning
/// only when it names the C locale (see [`crate::diag::init_locale`]), and
/// any other name selects the user's own regional locale, not the one named,
/// so no candidate can be the locale a test asks for.
pub fn locale_matching(candidates: &[&str]) -> Option<String> {
    #[cfg(windows)]
    {
        let _ = candidates;
        None
    }
    #[cfg(not(windows))]
    {
        installed_locale(candidates)
    }
}

/// The first of `candidates` that `locale -a` lists, compared without regard
/// to case, in its own spelling.
#[cfg(not(windows))]
fn installed_locale(candidates: &[&str]) -> Option<String> {
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

/// Fill `buf`, looping until it is full or the stream ends.
///
/// Returns how many bytes arrived, so a caller can tell "the stream really is
/// this short" -- the answer `read_exact` throws away -- from "one `read`
/// happened to come back early", which is not an answer about the stream at
/// all.
#[cfg(any(unix, test))]
fn read_until_full(r: &mut impl std::io::Read, buf: &mut [u8]) -> usize {
    let mut n = 0;
    while n < buf.len() {
        match r.read(&mut buf[n..]) {
            Ok(0) => break,
            Ok(k) => n += k,
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
            Err(_) => break,
        }
    }
    n
}

/// Run `cmd` with `args` and assert that the argument parser took every word
/// after an option that requires a value as that value, even where the word
/// begins with '-' (XBD 12.2, Guideline 7). The utility may still refuse the
/// value itself, as a number out of range or a file that is not there; only
/// the parser reading the word as an option, or calling the value missing,
/// fails the assertion. Returns the output for further checks.
///
/// A probe that would otherwise act (queue a job, write a log record) ends
/// with `--help`, which is reached only once the words before it parsed.
pub fn assert_hyphen_option_argument(cmd: &str, args: &[&str]) -> Output {
    let args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    let output = run_test_base(cmd, &args, b"");
    let stderr = String::from_utf8_lossy(&output.stderr);
    for refusal in ["unexpected argument", "a value is required", "tip: to pass"] {
        assert!(
            !stderr.contains(refusal),
            "{cmd} {args:?}: an option-argument beginning with '-' was refused: {stderr}"
        );
    }
    output
}

/// Run `cmd ARGS... x\xff` and assert that the last argument, which is not
/// valid UTF-8, is reported by [`crate::optarg::args_utf8`] with status 1
/// instead of making `cmd` panic.
#[cfg(unix)]
pub fn assert_non_utf8_argument_rejected(cmd: &str, args: &[&str]) {
    let mut argv: Vec<OsString> = args.iter().map(OsString::from).collect();
    argv.push(os_bytes(b"x\xff"));
    let output = run_test_base_os(cmd, &argv, b"", &[]);
    let expected = format!("{cmd}: x\u{FFFD}: argument is not valid UTF-8\n");
    assert_eq!(
        (
            output.status.code(),
            String::from_utf8_lossy(&output.stderr)
        ),
        (Some(1), expected.into()),
        "{cmd} {argv:?}"
    );
}

/// Run `cmd` with an option-argument beginning with '=' attached to the
/// short option `opt` (`-d=`, `-d=x`) and again as the next word (`-d =`,
/// `-d =x`), with `rest` after it and `stdin` as input, and assert that the
/// two runs agree in status, standard output and standard error.
///
/// An attached option-argument is everything after the option letter (XBD
/// 12.1), so `-d=` is the argument "="; clap alone reads it as `-d` with the
/// empty argument.
pub fn assert_equals_option_argument(cmd: &str, opt: &str, rest: &[&str], stdin: &[u8]) {
    for value in ["=", "=x"] {
        let rest = rest.iter().map(|s| s.to_string());
        let attached: Vec<String> = std::iter::once(format!("{opt}{value}"))
            .chain(rest.clone())
            .collect();
        let separate: Vec<String> = [opt.to_string(), value.to_string()]
            .into_iter()
            .chain(rest)
            .collect();
        let a = run_test_base(cmd, &attached, stdin);
        let s = run_test_base(cmd, &separate, stdin);
        assert_eq!(
            (
                a.status.code(),
                String::from_utf8_lossy(&a.stdout),
                String::from_utf8_lossy(&a.stderr)
            ),
            (
                s.status.code(),
                String::from_utf8_lossy(&s.stdout),
                String::from_utf8_lossy(&s.stderr)
            ),
            "{cmd} {attached:?} differs from {cmd} {separate:?}"
        );
    }
}

/// Assert that a utility dies by `SIGPIPE` when the reader of its standard
/// output goes away, writing nothing to standard error.
///
/// The Rust runtime sets `SIGPIPE` to `SIG_IGN` before `main`, so without
/// [`crate::io::restore_sigpipe`] — which [`crate::diag::init_locale`] now
/// calls — the write fails with `EPIPE`, libstd panics with "failed printing
/// to stdout: Broken pipe", and the process exits 101. A shell reports the
/// correct outcome as 141.
///
/// `cmd` is the binary name as [`get_binary_path`] resolves it. The utility
/// must produce enough output that it is still writing when the pipe closes;
/// `args` should name something large.
#[cfg(unix)]
pub fn assert_dies_by_sigpipe(cmd: &str, args: &[&str]) {
    use std::os::unix::process::ExitStatusExt as _;

    let mut child = Command::new(get_binary_path(cmd))
        .args(args)
        .env("LC_ALL", "C")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| panic!("spawn {}: {}", cmd, e));

    // Read a little, then drop the read end while the utility has more to say.
    //
    // How much came back matters: a utility whose entire output fits the pipe
    // buffer finishes before the reader leaves and exits 0, and the assertions
    // below would then blame SIGPIPE for an operand that was simply too small.
    // Reading a full buffer says the output is larger than this, which is the
    // precondition the caller has to meet.
    //
    // Which is why this loops rather than calling `read` once. A single `read`
    // returns whatever is in the pipe at that instant, and is explicitly
    // allowed to come back short with more on the way -- so its count measures
    // when the reader was scheduled, not how much the utility has to say. Every
    // caller here names an operand far larger than this buffer, and `ls` still
    // failed the assertion with exactly 30 bytes: one `println!`, which is one
    // line, which is one write. Looping asks the question the assertion means.
    let mut stdout = child.stdout.take().expect("child stdout");
    let mut buf = [0u8; 64];
    let got = read_until_full(&mut stdout, &mut buf);
    drop(stdout);

    let out = child
        .wait_with_output()
        .unwrap_or_else(|e| panic!("wait for {}: {}", cmd, e));

    // Reaped *before* this is judged, so a short answer arrives with the
    // reason attached. The utility reaching end of output early and the
    // utility dying early look identical in a byte count, and the first
    // version asserted here with the child still running -- so the one
    // question a failure raises was the one thing the message could not
    // answer.
    assert_eq!(
        got,
        buf.len(),
        "{}: only {} bytes of output before the pipe closed -- too small to \
         race a reader, so this says nothing about SIGPIPE. The utility \
         exited {:?} (signal {:?}) with stderr {:?}. Either the operand needs \
         to be larger, or it stopped early for the reason shown.",
        cmd,
        got,
        out.status.code(),
        out.status.signal(),
        String::from_utf8_lossy(&out.stderr)
    );

    assert_eq!(
        out.status.signal(),
        Some(libc::SIGPIPE),
        "{}: expected death by SIGPIPE, got {:?} with stderr {:?}",
        cmd,
        out.status,
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        out.stderr.is_empty(),
        "{}: a closed pipe is not an error to report: {:?}",
        cmd,
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Assert that a utility started with `SIGPIPE` ignored, as `trap '' PIPE`
/// in a shell leaves it, reports a write to a closed pipe as a write error:
/// it exits with `status`, is not killed by a signal, does not panic, and
/// says "Broken pipe" on standard error.
///
/// POSIX keeps an ignored signal ignored across `exec`, and a process may
/// rely on that to see `EPIPE` instead of dying. Resetting the disposition
/// to the default at startup -- what [`crate::io::restore_sigpipe`] did
/// unconditionally -- killed the utility anyway, and the shell saw 141.
///
/// The reader end of standard output is closed before the utility starts,
/// so its first write fails; any output at all reaches the error.
#[cfg(unix)]
pub fn assert_epipe_when_sigpipe_ignored(cmd: &str, args: &[&str], status: i32) {
    use std::os::unix::process::ExitStatusExt as _;

    let (reader, writer) = std::io::pipe().expect("pipe");
    drop(reader);
    let out = Command::new("/bin/sh")
        .arg("-c")
        .arg("trap '' PIPE; exec \"$0\" \"$@\"")
        .arg(get_binary_path(cmd))
        .args(args)
        .env("LC_ALL", "C")
        .stdin(Stdio::null())
        .stdout(writer)
        .stderr(Stdio::piped())
        .output()
        .unwrap_or_else(|e| panic!("spawn {}: {}", cmd, e));
    let stderr = String::from_utf8_lossy(&out.stderr);

    assert_eq!(
        out.status.signal(),
        None,
        "{cmd}: killed by a signal although SIGPIPE was ignored; stderr {stderr:?}"
    );
    assert_eq!(
        out.status.code(),
        Some(status),
        "{cmd}: wrong exit status for a write error; stderr {stderr:?}"
    );
    assert!(
        stderr.contains("Broken pipe") && !stderr.contains("panicked"),
        "{cmd}: expected a write-error diagnostic, got {stderr:?}"
    );
}

/// Assert that a utility whose standard output is `/dev/full` reports the
/// failed write: it exits with `status`, does not panic, and gives the
/// system's text for `ENOSPC` on standard error.
///
/// Feed it input whose output does not end in a <newline>: standard output is
/// line-buffered, and a final partial line reaches the device only when the
/// buffer is flushed at exit, where the runtime discards the error. Hosts
/// without `/dev/full` (macOS) skip the check.
pub fn assert_write_error_on_full_device(cmd: &str, args: &[&str], stdin: &[u8], status: i32) {
    let Ok(full) = std::fs::OpenOptions::new().write(true).open("/dev/full") else {
        return;
    };
    let enospc = {
        let mut probe = full.try_clone().expect("dup /dev/full");
        crate::diag::io_error_text(&probe.write_all(b"x").unwrap_err())
    };
    let mut child = Command::new(get_binary_path(cmd))
        .args(args)
        .env("LC_ALL", "C")
        .stdin(Stdio::piped())
        .stdout(full)
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| panic!("spawn {}: {}", cmd, e));
    let mut input = child.stdin.take().expect("stdin");
    input.write_all(stdin).expect("write stdin");
    drop(input);
    let out = child.wait_with_output().expect("wait");
    let stderr = String::from_utf8_lossy(&out.stderr);

    assert_eq!(
        out.status.code(),
        Some(status),
        "{cmd} {args:?} >/dev/full: wrong exit status; stderr {stderr:?}"
    );
    assert!(
        stderr.contains(&enospc) && !stderr.contains("panicked"),
        "{cmd} {args:?} >/dev/full: expected a write-error diagnostic, got {stderr:?}"
    );
}

/// A file a test wrote, alone in a temporary directory of its own.
///
/// Dropping it removes the directory and the file, and a panic drops it, so a
/// failing assertion leaves nothing behind in the temporary directory -- the
/// leak that a `temp_dir().join(..)` path removed by hand after the last
/// assertion has. It dereferences to the file's [`Path`], so it stands where
/// that path did.
pub struct TempFile {
    path: PathBuf,
    _dir: crate::tmp::TempDir,
}

impl TempFile {
    /// Write `contents` to a file called `name` in a fresh temporary
    /// directory. `name` is a single path component.
    pub fn new(name: &str, contents: impl AsRef<[u8]>) -> TempFile {
        let dir = crate::tmp::tempdir().expect("create temporary directory");
        let path = dir.path().join(name);
        std::fs::write(&path, contents).expect("write temporary file");
        TempFile { path, _dir: dir }
    }

    /// The file's path.
    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl std::ops::Deref for TempFile {
    type Target = Path;

    fn deref(&self) -> &Path {
        &self.path
    }
}

impl AsRef<Path> for TempFile {
    fn as_ref(&self) -> &Path {
        &self.path
    }
}

impl AsRef<std::ffi::OsStr> for TempFile {
    fn as_ref(&self) -> &std::ffi::OsStr {
        self.path.as_os_str()
    }
}

#[cfg(test)]
mod tests {
    use super::{read_until_full, TempFile};
    use std::io::Read;

    /// The file holds what was written, under its own name, and dropping it
    /// takes the directory it was made in too.
    #[test]
    fn temp_file_is_written_and_removed_with_its_directory() {
        let (path, dir) = {
            let f = TempFile::new("name.txt", b"contents");
            assert_eq!(std::fs::read(&f).unwrap(), b"contents");
            assert_eq!(f.file_name().unwrap(), "name.txt");
            (f.path().to_path_buf(), f.parent().unwrap().to_path_buf())
        };
        assert!(!path.exists(), "the file outlived its TempFile");
        assert!(!dir.exists(), "the directory outlived its TempFile");
    }

    /// A stream that hands back exactly the chunks it was given, one per
    /// `read`, however much room the caller offers.
    ///
    /// Which is what a pipe does: a `read` returns what is in it now. Writing
    /// the chunks down makes the question "does the helper loop?" answerable
    /// without a second process, a scheduler or a platform.
    struct Chunks(Vec<Vec<u8>>);

    impl Read for Chunks {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            if self.0.is_empty() {
                return Ok(0);
            }
            let chunk = self.0.remove(0);
            let n = chunk.len().min(buf.len());
            buf[..n].copy_from_slice(&chunk[..n]);
            Ok(n)
        }
    }

    fn line(n: usize) -> Vec<u8> {
        vec![b'x'; n]
    }

    /// The regression: a stream with plenty to say, delivered in pieces
    /// smaller than the buffer. One `read` answers 30 and the caller concludes
    /// the operand was too small; the loop answers 64, which is the truth.
    #[test]
    fn read_until_full_keeps_reading_past_a_short_chunk() {
        // 30 bytes is one `entry-with-a-long-name-NNNNNN\n`, which is what
        // `ls` writes per `println!` and what macOS CI reported.
        let mut src = Chunks(vec![line(30), line(30), line(30), line(30)]);
        let mut buf = [0u8; 64];
        assert_eq!(read_until_full(&mut src, &mut buf), 64);
    }

    /// A single chunk that already fills the buffer is unchanged.
    #[test]
    fn read_until_full_stops_once_the_buffer_is_full() {
        let mut src = Chunks(vec![line(200)]);
        let mut buf = [0u8; 64];
        assert_eq!(read_until_full(&mut src, &mut buf), 64);
        // Nothing was consumed beyond the one chunk the buffer could hold.
        assert!(src.0.is_empty());
    }

    /// The case the assertion exists to catch still reports short: a stream
    /// that really does end before the buffer fills. Without this the fix
    /// would have removed the check rather than corrected it.
    #[test]
    fn read_until_full_reports_a_stream_that_truly_ends_early() {
        let mut src = Chunks(vec![line(30)]);
        let mut buf = [0u8; 64];
        assert_eq!(read_until_full(&mut src, &mut buf), 30);

        let mut empty = Chunks(vec![]);
        assert_eq!(read_until_full(&mut empty, &mut buf), 0);
    }
}
