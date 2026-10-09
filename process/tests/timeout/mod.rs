//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{
    io::{BufRead, BufReader, Read, Write},
    process::{Command, Output, Stdio},
    thread,
    time::{Duration, Instant},
};

use plib::testing::get_binary_path;

pub struct TestPlan {
    pub cmd: String,
    pub args: Vec<String>,
    pub stdin_data: String,
    pub expected_out: String,
    pub expected_err: String,
    pub expected_exit_code: Option<i32>,
    pub has_subprocesses: bool,
}

fn run_test_base(cmd: &str, args: &Vec<String>, stdin_data: &[u8]) -> (Output, u32) {
    let test_bin_path = get_binary_path(cmd);

    let mut command = Command::new(test_bin_path);
    let mut child = command
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|_| panic!("failed to spawn command {}", cmd));

    let pid = child.id();

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
    let output = child.wait_with_output().expect("failed to wait for child");
    (output, pid)
}

/// Whether any process, zombies included, is still a member of group `pgid`.
fn process_group_exists(pgid: u32) -> bool {
    let rc = unsafe { libc::kill(-(pgid as libc::pid_t), 0) };
    rc == 0 || std::io::Error::last_os_error().raw_os_error() != Some(libc::ESRCH)
}

/// Without `-f`, timeout makes itself a process-group leader, so its group is
/// the one whose PGID is its PID and holds only what timeout started. Once
/// timeout has exited, a member it killed may still be a zombie awaiting its
/// new parent's reap, so poll for the group to empty rather than look once.
/// With `-f` timeout stays in the test runner's group, which is shared with
/// every concurrently running test, so there is nothing to check.
fn assert_no_leftover_subprocesses(args: &[String], timeout_pid: u32) {
    if args.iter().any(|a| a == "-f" || a == "--foreground") {
        return;
    }
    let deadline = Instant::now() + Duration::from_secs(5);
    while process_group_exists(timeout_pid) {
        assert!(
            Instant::now() < deadline,
            "timeout's process group {timeout_pid} still has members after it exited"
        );
        thread::sleep(Duration::from_millis(10));
    }
}

pub fn run_test(plan: TestPlan) {
    let (output, pid) = run_test_base(&plan.cmd, &plan.args, plan.stdin_data.as_bytes());

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout, plan.expected_out);

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(stderr, plan.expected_err);

    assert_eq!(output.status.code(), plan.expected_exit_code);
    if let Some(0) = plan.expected_exit_code {
        assert!(output.status.success());
    }

    if !plan.has_subprocesses {
        assert_no_leftover_subprocesses(&plan.args, pid);
    }
}

fn timeout_test(args: &[&str], expected_err: &str, expected_exit_code: i32) {
    run_test(TestPlan {
        cmd: String::from("timeout"),
        args: args.iter().map(|s| String::from(*s)).collect(),
        stdin_data: String::from(""),
        expected_out: String::from(""),
        expected_err: String::from(expected_err),
        expected_exit_code: Some(expected_exit_code),
        has_subprocesses: false,
    });
}

fn timeout_test_extended(
    args: &[&str],
    expected_err: &str,
    expected_exit_code: Option<i32>,
    has_subprocesses: bool,
) {
    run_test(TestPlan {
        cmd: String::from("timeout"),
        args: args.iter().map(|s| String::from(*s)).collect(),
        stdin_data: String::from(""),
        expected_out: String::from(""),
        expected_err: String::from(expected_err),
        expected_exit_code,
        has_subprocesses,
    });
}

const TRUE: &str = "true";
const SLEEP: &str = "sleep";
const NON_EXECUTABLE: &str = "tests/timeout/non_executable.sh";
const WITH_ARGUMENT: &str = "tests/timeout/with_argument.sh";
const SPAWN_CHILD: &str = "tests/timeout/spawn_child.sh";

#[test]
fn test_absent_duration() {
    timeout_test(&[TRUE], "timeout: invalid duration format 'true'\n", 125);
}

#[test]
fn test_absent_utility() {
    timeout_test(
        &["5"],
        "timeout: one or more required arguments were not provided\n",
        125,
    );
}

#[test]
fn test_signal_parsing_invalid() {
    timeout_test(
        &["-s", "MY_SIGNAL", "1", TRUE],
        "timeout: invalid signal name 'MY_SIGNAL'\n",
        125,
    );
}

#[test]
fn test_signal_parsing_uppercase() {
    timeout_test(&["-s", "TERM", "1", TRUE], "", 0);
    timeout_test(&["-s", "KILL", "1", TRUE], "", 0);
    timeout_test(&["-s", "CONT", "1", TRUE], "", 0);
    timeout_test(&["-s", "STOP", "1", TRUE], "", 0);
}

#[test]
fn test_signal_parsing_lowercase() {
    timeout_test(&["-s", "term", "1", TRUE], "", 0);
    timeout_test(&["-s", "kill", "1", TRUE], "", 0);
    timeout_test(&["-s", "cont", "1", TRUE], "", 0);
    timeout_test(&["-s", "stop", "1", TRUE], "", 0);
}

#[test]
fn test_signal_parsing_uppercase_with_prefix() {
    timeout_test(&["-s", "SIGTERM", "1", TRUE], "", 0);
    timeout_test(&["-s", "SIGKILL", "1", TRUE], "", 0);
    timeout_test(&["-s", "SIGCONT", "1", TRUE], "", 0);
    timeout_test(&["-s", "SIGSTOP", "1", TRUE], "", 0);
}

#[test]
fn test_signal_parsing_lowercase_with_prefix() {
    timeout_test(&["-s", "sigterm", "1", TRUE], "", 0);
    timeout_test(&["-s", "sigkill", "1", TRUE], "", 0);
    timeout_test(&["-s", "sigcont", "1", TRUE], "", 0);
    timeout_test(&["-s", "sigstop", "1", TRUE], "", 0);
}

#[test]
fn test_multiple_signals() {
    timeout_test(
        &["-s", "TERM", "-s", "KILL", "1", TRUE],
        "timeout: an argument cannot be used with one or more of the other specified arguments\n",
        125,
    );
}

#[test]
fn test_invalid_duration_negative() {
    // "-1" is considered as argument, not a value
    timeout_test(&["-1", TRUE], "timeout: unexpected argument found\n", 125);
}

#[test]
fn test_invalid_duration_empty_float() {
    timeout_test(&[".", TRUE], "timeout: invalid duration format '.'\n", 125);
}

#[test]
fn test_invalid_duration_format_invalid_suffix() {
    timeout_test(
        &["1a", TRUE],
        "timeout: invalid duration format '1a'\n",
        125,
    );
}

#[test]
fn test_invalid_duration_only_suffixes() {
    timeout_test(&["s", TRUE], "timeout: invalid duration format 's'\n", 125);
    timeout_test(&["m", TRUE], "timeout: invalid duration format 'm'\n", 125);
    timeout_test(&["h", TRUE], "timeout: invalid duration format 'h'\n", 125);
    timeout_test(&["d", TRUE], "timeout: invalid duration format 'd'\n", 125);
}

#[test]
fn test_valid_duration_parsing_with_suffixes() {
    timeout_test(&["1.1s", TRUE], "", 0);
    timeout_test(&["1.1m", TRUE], "", 0);
    timeout_test(&["1.1h", TRUE], "", 0);
    timeout_test(&["1.1d", TRUE], "", 0);
}

#[test]
fn test_utility_cound_not_execute() {
    timeout_test(
        &["1", NON_EXECUTABLE],
        "timeout: unable to run the utility 'tests/timeout/non_executable.sh'\n",
        126,
    );
}

#[test]
fn test_utility_not_found() {
    timeout_test(
        &["1", "inexistent_utility"],
        "timeout: utility 'inexistent_utility' not found\n",
        127,
    );
}

#[test]
fn test_utility_error() {
    timeout_test(&["1", WITH_ARGUMENT], "error: enter some argument\n", 1);
}

#[test]
fn test_basic() {
    timeout_test(&["2", SLEEP, "1"], "", 0);
}

#[test]
fn test_send_kill() {
    timeout_test_extended(&["-s", "KILL", "1", SLEEP, "2"], "", None, false);
}

#[test]
fn test_zero_duration() {
    timeout_test(&["0", SLEEP, "2"], "", 0);
}

#[test]
fn test_timeout_reached() {
    timeout_test(&["1", SLEEP, "2"], "", 124);
}

// #T1: a sub-second (fractional) duration must time out. Before the fix,
// alarm() truncated 0.3s to 0 seconds, disabling the timeout entirely.
#[test]
fn test_fractional_duration_times_out() {
    timeout_test(&["0.3", SLEEP, "2"], "", 124);
}

// #T1: a sub-second duration with the 's' suffix likewise times out.
#[test]
fn test_fractional_duration_suffix_times_out() {
    timeout_test(&["0.3s", SLEEP, "2"], "", 124);
}

// #T1: a fractional -k kill-after grace period also uses sub-second precision.
// CONT does not terminate, so after the 0.3s timeout the fractional 0.3s
// kill-after must still fire SIGKILL and the child must be reaped (the run
// completes quickly rather than hanging for the full 3s sleep). The child is
// killed by a signal, so timeout reports no exit code.
#[test]
fn test_fractional_kill_after() {
    timeout_test_extended(
        &["-p", "-s", "CONT", "-k", "0.3", "0.3", SLEEP, "3"],
        "",
        None,
        false,
    );
}

#[test]
fn test_preserve_status_wait() {
    timeout_test(&["-p", "2", SLEEP, "1"], "", 0);
}

#[test]
fn test_preserve_status_with_sigterm() {
    // 143 = 128 + 15 (SIGTERM after first timeout)
    timeout_test(&["-p", "1", SLEEP, "2"], "", 143);
}

#[test]
fn test_preserve_status_sigcont_with_sigkill() {
    // 137 = 128 + 9 (SIGKILL after second timeout)
    timeout_test_extended(
        &["-p", "-s", "CONT", "-k", "1", "1", SLEEP, "3"],
        "",
        None,
        false,
    );
}

#[test]
fn test_preserve_status_cont() {
    // First duration is 0, so sending SIGCONT and second timeout won't happen
    timeout_test(&["-p", "-s", "CONT", "-k", "1", "0", SLEEP, "3"], "", 0);
}

#[test]
fn test_not_foreground_timeout() {
    timeout_test_extended(&["1", SPAWN_CHILD], "", Some(124), true);
}

#[test]
fn test_foreground_timeout() {
    timeout_test_extended(&["-f", "1", SPAWN_CHILD], "", Some(124), false);
}

#[test]
fn test_not_foreground_ok() {
    timeout_test_extended(&["5", SPAWN_CHILD], "", Some(0), true);
}

#[test]
fn test_foreground_ok() {
    timeout_test_extended(&["-f", "5", SPAWN_CHILD], "", Some(0), false);
}

// #T2: "the child's signal dispositions shall be the same as the disposition
// that timeout inherited" (spec 117593-117598), except for the -s signal.
// timeout installs SIG_IGN for SIGTTIN/SIGTTOU on itself while managing the
// child, and used to reset both to SIG_DFL in the child unconditionally — so a
// child launched under an already-ignoring parent came out with the wrong
// disposition.
//
// The probe reads the child's own /proc status: SigIgn is a bitmask of ignored
// signals, so bit (signo-1) tells us what the child actually inherited.
#[test]
#[cfg(target_os = "linux")]
fn test_child_inherits_ignored_sigttin_sigttou() {
    // Launch: sh (ignoring SIGTTIN/SIGTTOU) -> timeout -> sh (reports SigIgn).
    // `trap '' TTIN TTOU` makes the outer shell ignore both, and that
    // disposition is what timeout inherits and must pass on.
    let timeout_bin = get_binary_path("timeout");
    let script = format!(
        "trap '' TTIN TTOU; exec {} 10 sh -c 'grep ^SigIgn: /proc/self/status'",
        timeout_bin.display()
    );

    let out = Command::new("sh")
        .args(["-c", &script])
        .output()
        .expect("failed to run the SIGTTIN/SIGTTOU probe");

    let stdout = String::from_utf8_lossy(&out.stdout);
    let mask_hex = stdout
        .split_whitespace()
        .nth(1)
        .unwrap_or_else(|| panic!("unexpected SigIgn line: {stdout:?}"));
    let mask = u64::from_str_radix(mask_hex, 16).expect("SigIgn should be hex");

    let ignored = |signo: i32| mask & (1u64 << (signo - 1)) != 0;
    assert!(
        ignored(libc::SIGTTIN),
        "child must inherit SIGTTIN ignored, SigIgn={mask_hex}"
    );
    assert!(
        ignored(libc::SIGTTOU),
        "child must inherit SIGTTOU ignored, SigIgn={mask_hex}"
    );
}

// The counterpart: when the parent does *not* ignore them, the child must not
// come out ignoring them either. Without this, the test above would also pass
// an implementation that ignored the two signals unconditionally.
#[test]
#[cfg(target_os = "linux")]
fn test_child_does_not_inherit_ignored_sigttin_when_parent_does_not() {
    let timeout_bin = get_binary_path("timeout");
    let script = format!(
        "exec {} 10 sh -c 'grep ^SigIgn: /proc/self/status'",
        timeout_bin.display()
    );

    let out = Command::new("sh")
        .args(["-c", &script])
        .output()
        .expect("failed to run the SIGTTIN probe");

    let stdout = String::from_utf8_lossy(&out.stdout);
    let mask_hex = stdout.split_whitespace().nth(1).unwrap();
    let mask = u64::from_str_radix(mask_hex, 16).unwrap();

    assert_eq!(
        mask & (1u64 << (libc::SIGTTIN - 1)),
        0,
        "child must not ignore SIGTTIN when the parent did not, SigIgn={mask_hex}"
    );
}

// #T3: timeout forwards "the signal it received" to the child for any signal
// whose default action is to terminate (spec 117587-117591), not just the five
// it originally handled. A SIGUSR1 to timeout used to kill it silently and
// orphan the child.
//
// Send SIGUSR1 to timeout and require that the child sees it: the child traps
// USR1 and prints a marker, so the marker proves forwarding rather than the
// child merely dying.
#[test]
fn test_forwards_sigusr1_to_the_child() {
    assert_signal_forwarded("USR1", libc::SIGUSR1);
}

// Same for SIGPIPE, the case the audit called out specifically: its default
// action terminates, so it must be forwarded rather than silently killing
// timeout and orphaning the child.
#[test]
fn test_forwards_sigpipe_to_the_child() {
    assert_signal_forwarded("PIPE", libc::SIGPIPE);
}

/// Run `timeout 20 sh` with a child that traps `name`, send `signal` to
/// timeout, and require that the child's trap ran.
///
/// The signal is sent only once the child has written READY, which it does
/// after installing its trap. timeout installs its own handlers before it
/// spawns the child, so READY also proves timeout is ready to forward. A fixed
/// delay instead lets a loaded machine deliver the signal before either
/// handler exists.
fn assert_signal_forwarded(name: &str, signal: libc::c_int) {
    let timeout_bin = get_binary_path("timeout");
    let script =
        format!("trap 'echo GOT-{name}; exit 0' {name}; echo READY; while :; do sleep 0.1; done");

    let mut child = Command::new(&timeout_bin)
        .args(["20", "sh", "-c", &script])
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn timeout");

    let mut stdout = BufReader::new(child.stdout.take().unwrap());
    let mut ready = String::new();
    stdout.read_line(&mut ready).expect("failed to read child");
    assert_eq!(
        ready, "READY\n",
        "the child must start and install its trap"
    );

    unsafe { libc::kill(child.id() as i32, signal) };

    let mut rest = String::new();
    stdout
        .read_to_string(&mut rest)
        .expect("failed to read child");
    let out = child
        .wait_with_output()
        .expect("failed to wait for timeout");
    assert!(
        rest.contains(&format!("GOT-{name}")),
        "SIG{name} must be forwarded to the child, got stdout={:?} stderr={:?}",
        rest,
        String::from_utf8_lossy(&out.stderr)
    );
}

// A utility invoked under timeout may take its own options. Until 2026-08-06,
// `trailing_var_arg` was set but `allow_hyphen_values` was not, so clap tried
// to parse the utility's first hyphenated argument as one of timeout's own
// options and rejected it: `timeout 5 ls -l`, `timeout 5 grep -c ...` and
// `timeout 5 sh -c '...'` all failed with "unexpected argument found" (#T5).
#[test]
fn test_utility_arguments_may_start_with_a_hyphen() {
    let timeout_bin = get_binary_path("timeout");

    let out = Command::new(&timeout_bin)
        .args(["10", "echo", "-n", "hyphenated"])
        .output()
        .expect("failed to run timeout");

    assert!(
        out.status.success(),
        "timeout rejected the utility's own option: {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout), "hyphenated");
}

// The same for an option that happens to collide with one of timeout's own
// short options: after DURATION and UTILITY, `-s` belongs to the utility.
#[test]
fn test_utility_option_colliding_with_timeouts_own_is_passed_through() {
    let timeout_bin = get_binary_path("timeout");

    // `sh -c` collides with nothing, but `-s` is timeout's signal option.
    // Here it must reach `sh`, which treats `-s` as "read commands from stdin".
    let out = Command::new(&timeout_bin)
        .args(["10", "sh", "-c", "echo passed-through"])
        .output()
        .expect("failed to run timeout");

    assert!(
        out.status.success(),
        "timeout rejected `sh -c`: {:?}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim_end(),
        "passed-through"
    );
}

// timeout's *own* options must still parse when they precede the operands, so
// the fix above cannot have been "stop parsing options entirely".
#[test]
fn test_timeouts_own_options_still_parse_before_operands() {
    let timeout_bin = get_binary_path("timeout");

    for args in [
        vec!["-s", "KILL", "10", "echo", "ok"],
        vec!["--signal-name", "KILL", "10", "echo", "ok"],
        vec!["-p", "10", "echo", "ok"],
    ] {
        let out = Command::new(&timeout_bin)
            .args(&args)
            .output()
            .expect("failed to run timeout");
        assert!(
            out.status.success(),
            "timeout {args:?} failed: {:?}",
            String::from_utf8_lossy(&out.stderr)
        );
        assert_eq!(String::from_utf8_lossy(&out.stdout).trim_end(), "ok");
    }
}

// XBD 12.2 Guideline 9: once DURATION and UTILITY are read, every later word
// is the utility's, including one spelled like timeout's own -s, -k, -f or
// -p. `timeout 10 echo -s KILL x` took `-s KILL` as timeout's signal and ran
// `echo x`.
#[test]
fn test_options_after_utility_belong_to_the_utility() {
    let out = Command::new(get_binary_path("timeout"))
        .args(["10", "echo", "-s", "KILL", "-k", "1", "-f", "-p", "x"])
        .output()
        .expect("failed to run timeout");

    assert_eq!(out.status.code(), Some(0));
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "-s KILL -k 1 -f -p x\n"
    );
}

// The utility's arguments are passed through byte for byte, valid UTF-8 or
// not, and a PATH directory whose name is not valid UTF-8 is searched; such a
// PATH was treated as unset.
#[test]
fn timeout_non_utf8_arguments_and_path() {
    use plib::testing::{create_non_utf8, get_binary_path, os_bytes};
    use std::os::unix::fs::PermissionsExt;

    let dir = plib::tmp::tempdir().unwrap();
    let Some(bin) = create_non_utf8(dir.path(), b"bin\xff", |p| std::fs::create_dir(p)) else {
        return;
    };
    let script = bin.join("posixutils-timeout-probe");
    std::fs::write(&script, "#!/bin/sh\nprintf '%s' \"$1\"\n").unwrap();
    std::fs::set_permissions(&script, std::fs::Permissions::from_mode(0o755)).unwrap();

    let output = std::process::Command::new(get_binary_path("timeout"))
        .env("PATH", &bin)
        .args(["10", "posixutils-timeout-probe"])
        .arg(os_bytes(b"arg\xfe"))
        .output()
        .unwrap();
    assert!(output.status.success(), "{output:?}");
    assert_eq!(output.stdout, b"arg\xfe");
}

/// Run `timeout 10 utility` in `cwd` with `path` as PATH (unset if None).
fn timeout_in(cwd: &std::path::Path, path: Option<&std::ffi::OsStr>, utility: &str) -> Output {
    let mut command = Command::new(get_binary_path("timeout"));
    command.current_dir(cwd).args(["10", utility]);
    match path {
        Some(path) => command.env("PATH", path),
        None => command.env_remove("PATH"),
    };
    command.output().unwrap()
}

/// Write an executable script at `path` that prints `word`.
fn write_script(path: &std::path::Path, word: &str) {
    use std::os::unix::fs::PermissionsExt;
    std::fs::write(path, format!("#!/bin/sh\necho {word}\n")).unwrap();
    std::fs::set_permissions(path, std::fs::Permissions::from_mode(0o755)).unwrap();
}

// A utility name without a slash is looked up through PATH, as execvp does;
// a file of that name in the current directory does not shadow it.
#[test]
fn timeout_bare_name_found_through_path_not_cwd() {
    let dir = plib::tmp::tempdir().unwrap();
    let cwd = dir.path().join("cwd");
    let bin = dir.path().join("bin");
    std::fs::create_dir(&cwd).unwrap();
    std::fs::create_dir(&bin).unwrap();
    write_script(&cwd.join("posixutils-probe"), "WRONG");
    write_script(&bin.join("posixutils-probe"), "RIGHT");
    write_script(&cwd.join("posixutils-cwd-only"), "WRONG");

    let output = timeout_in(&cwd, Some(bin.as_os_str()), "posixutils-probe");
    assert!(output.status.success(), "{output:?}");
    assert_eq!(output.stdout, b"RIGHT\n");

    // Present only in the current directory, which PATH does not name.
    let output = timeout_in(&cwd, Some(bin.as_os_str()), "posixutils-cwd-only");
    assert_eq!(output.status.code(), Some(127), "{output:?}");
    assert!(output.stdout.is_empty(), "{output:?}");
}

// A name with a slash is a pathname and is never searched for in PATH.
// timeout looked `./posixutils-probe` up under each PATH directory when no
// such file was in the current directory, and ran bin/./posixutils-probe.
#[test]
fn timeout_name_with_slash_is_not_searched_in_path() {
    let dir = plib::tmp::tempdir().unwrap();
    let cwd = dir.path().join("cwd");
    let bin = dir.path().join("bin");
    std::fs::create_dir(&cwd).unwrap();
    std::fs::create_dir(&bin).unwrap();
    write_script(&bin.join("posixutils-probe"), "WRONG");

    let output = timeout_in(&cwd, Some(bin.as_os_str()), "./posixutils-probe");
    assert_eq!(output.status.code(), Some(127), "{output:?}");
    assert!(output.stdout.is_empty(), "{output:?}");
}

// With PATH unset the utility is searched for in the system's default path,
// as execvp does; timeout reported every utility as not found.
#[test]
fn timeout_unset_path_uses_default_search_path() {
    let dir = plib::tmp::tempdir().unwrap();
    let output = timeout_in(dir.path(), None, "true");
    assert_eq!(output.status.code(), Some(0), "{output:?}");
}

// An empty PATH element names the current directory.
#[test]
fn timeout_empty_path_element_is_cwd() {
    let dir = plib::tmp::tempdir().unwrap();
    write_script(&dir.path().join("posixutils-probe"), "HERE");
    let output = timeout_in(
        dir.path(),
        Some(std::ffi::OsStr::new(":/nonexistent")),
        "posixutils-probe",
    );
    assert!(output.status.success(), "{output:?}");
    assert_eq!(output.stdout, b"HERE\n");
}
