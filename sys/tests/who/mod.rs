//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};
use std::process::Output;

fn run_who_test(args: Vec<&str>, expected_exit_code: i32, check_fn: fn(&TestPlan, &Output)) {
    let plan = TestPlan {
        cmd: "who".to_string(),
        args: args.iter().map(|&s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    };
    run_test_with_checker(plan, check_fn);
}

// Checker functions
fn check_exit_success(_: &TestPlan, output: &Output) {
    assert!(output.status.success(), "Expected successful exit");
}

fn check_has_column_headings(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.lines().collect();

    // Should have at least a header line
    assert!(!lines.is_empty(), "Expected output with headings");

    // First line should contain column headers
    let first_line = lines[0].to_uppercase();
    assert!(
        first_line.contains("NAME") || first_line.contains("LINE") || first_line.contains("TIME"),
        "Expected header line to contain NAME, LINE, or TIME"
    );
}

fn check_summary_format(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.lines().collect();

    // Summary format should have at least the "# users=" line
    assert!(
        lines.iter().any(|line| line.contains("# users=")),
        "Expected '# users=' line in summary output"
    );
}

fn check_output_has_terminal_state(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);

    // With -T, output should include terminal state characters (+, -, ?)
    // Skip if no output (no logged in users)
    if !stdout.trim().is_empty() {
        let lines: Vec<&str> = stdout.lines().filter(|l| !l.trim().is_empty()).collect();
        if !lines.is_empty() {
            // At least one line should potentially have terminal state indicators
            // The format should be: NAME STATE LINE TIME
            // We can't guarantee specific output, but we can check structure
            for line in lines {
                // Each line should have multiple fields
                let fields: Vec<&str> = line.split_whitespace().collect();
                assert!(fields.len() >= 2, "Expected multiple fields in output");
            }
        }
    }
}

#[test]
fn who_no_args() {
    // Default behavior: show logged in users
    run_who_test(vec![], 0, check_exit_success);
}

#[test]
fn who_short_format() {
    // -s is the default, explicit test
    run_who_test(vec!["-s"], 0, check_exit_success);
}

#[test]
fn who_heading() {
    // -H should print column headings
    run_who_test(vec!["-H"], 0, check_has_column_headings);
}

#[test]
fn who_summary() {
    // -q should show summary format
    run_who_test(vec!["-q"], 0, check_summary_format);
}

#[test]
fn who_boot() {
    // -b should show boot time
    run_who_test(vec!["-b"], 0, check_exit_success);
}

#[test]
fn who_dead() {
    // -d should show dead processes
    run_who_test(vec!["-d"], 0, check_exit_success);
}

#[test]
fn who_login() {
    // -l should show login processes
    run_who_test(vec!["-l"], 0, check_exit_success);
}

#[test]
fn who_process() {
    // -p should show active processes spawned by init
    run_who_test(vec!["-p"], 0, check_exit_success);
}

#[test]
fn who_runlevel() {
    // -r should show current runlevel
    run_who_test(vec!["-r"], 0, check_exit_success);
}

#[test]
fn who_time() {
    // -t should show last system clock change
    run_who_test(vec!["-t"], 0, check_exit_success);
}

#[test]
fn who_terminals() {
    // -T should show terminal state
    run_who_test(vec!["-T"], 0, check_output_has_terminal_state);
}

#[test]
fn who_users() {
    // -u should show idle time for users
    run_who_test(vec!["-u"], 0, check_exit_success);
}

#[test]
fn who_all() {
    // -a should enable all options
    run_who_test(vec!["-a"], 0, check_exit_success);
}

#[test]
fn who_combined_options() {
    // Test combining options
    run_who_test(vec!["-H", "-b"], 0, check_has_column_headings);
}

#[test]
fn who_current_terminal() {
    // -m should show only current terminal
    run_who_test(vec!["-m"], 0, check_exit_success);
}

#[test]
fn who_userproc() {
    // --userproc should show normal user processes
    run_who_test(vec!["--userproc"], 0, check_exit_success);
}

#[test]
fn who_am_i() {
    // "who am i" should be equivalent to -m option
    run_who_test(vec!["am", "i"], 0, check_exit_success);
}

#[test]
fn who_am_capital_i() {
    // "who am I" should also work
    run_who_test(vec!["am", "I"], 0, check_exit_success);
}

#[test]
fn who_file_operand() {
    // Test reading from an alternate utmpx file
    // On macOS, the default file is /var/run/utmpx
    // On Linux, it's typically /var/run/utmp
    #[cfg(target_os = "macos")]
    let utmpx_file = "/var/run/utmpx";
    #[cfg(target_os = "linux")]
    let utmpx_file = "/var/run/utmp";

    // Only run if the file exists
    if std::path::Path::new(utmpx_file).exists() {
        run_who_test(vec![utmpx_file], 0, check_exit_success);
    }
}

// ============================================
// -T terminal state and -b system boot (#P-audit)
//
// These need a utmpx database whose records this test controls: on a live
// host only `+` is reachable, because every allocated pty is group-writable.
// `who` takes a `file` operand substituting for the database (POSIX OPERANDS),
// and `utmpxname(3)` reads it, so the fixture is a real utmpx file.
// ============================================

/// Whether this platform can be pointed at a substituted utmpx database with
/// `utmpxname(3)`.
///
/// Linux can. macOS CI reports *nothing* from a database written here -- not
/// even the `USER_PROCESS` record -- so `who` never sees the records these
/// tests rely on. Why Darwin refuses the file is not visible from a Linux
/// host, and two CI rounds spent guessing at it bought nothing, so the tests
/// that need the mechanism skip there rather than assert what this host cannot
/// check.
///
/// Only the *fixture* is platform-specific. What it exercises is not: `who`
/// selects on `typ == platform::BOOT_TIME` and reads the mode of `/dev/<line>`,
/// both platform-independent, and Linux covers them. Recorded as a gap rather
/// than papered over -- macOS asserts nothing about `-b` or `-T` content.
const UTMPX_FIXTURE_WORKS: bool = cfg!(target_os = "linux");

/// Write `records` as a utmpx database and return its path.
///
/// The records are built as `libc::utmpx` and written as raw bytes rather than
/// hand-packed, so the layout is whatever the platform's header says it is.
fn utmpx_fixture(dir: &std::path::Path, records: &[libc::utmpx]) -> std::path::PathBuf {
    use std::io::Write;

    let path = dir.join("utmpx");
    let mut file = std::fs::File::create(&path).unwrap();
    for record in records {
        // SAFETY: utmpx is a POD struct; this reads its own bytes.
        let bytes = unsafe {
            std::slice::from_raw_parts(
                (record as *const libc::utmpx) as *const u8,
                std::mem::size_of::<libc::utmpx>(),
            )
        };
        file.write_all(bytes).unwrap();
    }
    path
}

fn utmpx_record(ut_type: libc::c_short, user: &str, line: &str) -> libc::utmpx {
    utmpx_record_with_id(ut_type, user, line, "")
}

fn utmpx_record_with_id(ut_type: libc::c_short, user: &str, line: &str, id: &str) -> libc::utmpx {
    // SAFETY: utmpx is a POD struct and every field is overwritten or left as
    // the zero that means "unset".
    let mut record: libc::utmpx = unsafe { std::mem::zeroed() };
    record.ut_type = ut_type;
    record.ut_pid = 1234;
    for (slot, byte) in record.ut_id.iter_mut().zip(id.bytes()) {
        *slot = byte as _;
    }
    for (slot, byte) in record.ut_user.iter_mut().zip(user.bytes()) {
        *slot = byte as _;
    }
    for (slot, byte) in record.ut_line.iter_mut().zip(line.bytes()) {
        *slot = byte as _;
    }
    // 2020-09-13 UTC, so the rendered time is stable.
    record.ut_tv.tv_sec = 1_600_000_000 as _;
    record
}

/// The name under `/dev` of a character device whose group-write bit matches
/// `want`, or `None`.
///
/// Chosen by reading the mode rather than hard-coded, so the test asserts the
/// mapping from permission to state character instead of asserting that some
/// particular device has some particular mode.
fn dev_with_group_write(want: bool) -> Option<String> {
    use std::os::unix::fs::PermissionsExt;

    [
        "null", "zero", "full", "random", "tty", "console", "mem", "kmsg",
    ]
    .into_iter()
    .find(|name| {
        std::fs::metadata(format!("/dev/{name}"))
            .map(|m| (m.permissions().mode() & 0o020 != 0) == want)
            .unwrap_or(false)
    })
    .map(String::from)
}

fn run_who_on(fixture: &std::path::Path, args: &[&str]) -> String {
    let mut argv: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    argv.push(fixture.to_str().unwrap().to_string());
    let output = std::process::Command::new(plib::testing::get_binary_path("who"))
        .args(&argv)
        .env("TZ", "UTC")
        .env("LC_ALL", "C")
        .output()
        .expect("run who");
    assert!(
        output.status.success(),
        "who failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stdout).to_string()
}

// POSIX (XSI) 122974-122977: the -T state is '+' when the terminal allows
// write access to other users, '-' when it denies it, and '?' when it cannot
// be determined. Only '+' occurs on a live host, so all three were emitted by
// code nothing had ever run.
#[test]
fn who_dash_t_writes_the_three_state_characters() {
    if !UTMPX_FIXTURE_WORKS {
        eprintln!("skipping: this platform cannot read a substituted utmpx database");
        return;
    }
    let (Some(writable), Some(unwritable)) =
        (dev_with_group_write(true), dev_with_group_write(false))
    else {
        eprintln!("skipping: /dev lacks a group-writable and a non-group-writable device");
        return;
    };

    let dir = plib::tmp::TempDir::new().unwrap();
    let fixture = utmpx_fixture(
        dir.path(),
        &[
            utmpx_record(libc::USER_PROCESS, "alice", &writable),
            utmpx_record(libc::USER_PROCESS, "bob", &unwritable),
            utmpx_record(libc::USER_PROCESS, "carol", "no_such_tty_zz"),
        ],
    );

    let out = run_who_on(&fixture, &["-T"]);
    let state_of = |user: &str| -> char {
        let line = out
            .lines()
            .find(|l| l.starts_with(user))
            .unwrap_or_else(|| panic!("no line for {user}: {out}"));
        line.split_whitespace()
            .nth(1)
            .and_then(|f| f.chars().next())
            .unwrap_or_else(|| panic!("no state field for {user}: {line:?}"))
    };

    assert_eq!(state_of("alice"), '+', "group-writable terminal: {out}");
    assert_eq!(state_of("bob"), '-', "not group-writable: {out}");
    assert_eq!(state_of("carol"), '?', "terminal does not exist: {out}");
}

// POSIX (XSI) 122970: "For the -b option, <line> shall be 'system boot'."
// The <name> is explicitly unspecified, so only the line is pinned.
#[test]
fn who_dash_b_writes_a_system_boot_line() {
    if !UTMPX_FIXTURE_WORKS {
        eprintln!("skipping: this platform cannot read a substituted utmpx database");
        return;
    }
    let dir = plib::tmp::TempDir::new().unwrap();
    let fixture = utmpx_fixture(
        dir.path(),
        &[
            // `~` / `~~` are the conventional line and id of a boot record.
            utmpx_record_with_id(libc::BOOT_TIME, "reboot", "~", "~~"),
            utmpx_record(libc::USER_PROCESS, "alice", "null"),
        ],
    );

    // The precondition, asserted rather than assumed: the substituted database
    // is being read at all. Without this the skip below could quietly swallow a
    // real break in `plib::utmpx::load_from_file`.
    let all = run_who_on(&fixture, &[]);
    assert!(
        all.contains("alice"),
        "the substituted utmpx database must be read: {all}"
    );

    let out = run_who_on(&fixture, &["-b"]);
    assert!(
        out.contains("system boot"),
        "-b must write the line 'system boot': {out}"
    );
    assert_eq!(
        out.lines().count(),
        1,
        "-b reports the boot record only, not the logged-in users: {out}"
    );
    // The time comes from the record, not from now.
    assert!(
        out.contains("Sep 13"),
        "-b must render the boot record's own timestamp: {out}"
    );
}
