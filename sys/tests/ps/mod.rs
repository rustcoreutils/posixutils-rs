//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod bsd;

use plib::testing::{run_test_with_checker, TestPlan};
use std::process::Output;

fn run_ps_test(args: Vec<&str>, expected_exit_code: i32, check_fn: fn(&TestPlan, &Output)) {
    let plan = TestPlan {
        cmd: "ps".to_string(),
        args: args.iter().map(|&s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    };
    run_test_with_checker(plan, check_fn);
}

// ============================================
// Basic functionality tests
// ============================================

fn check_exit_success(_: &TestPlan, output: &Output) {
    assert!(output.status.success(), "Expected successful exit");
}

fn check_output_nonempty(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(!stdout.is_empty(), "Expected non-empty output");
}

fn check_default_header(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let first_line = stdout.lines().next().unwrap_or("");
    // POSIX default format: PID TTY TIME CMD
    assert!(first_line.contains("PID"), "Expected PID in header");
    assert!(first_line.contains("TTY"), "Expected TTY in header");
    assert!(first_line.contains("TIME"), "Expected TIME in header");
    assert!(
        first_line.contains("CMD") || first_line.contains("COMMAND"),
        "Expected CMD/COMMAND in header"
    );
}

fn check_full_format_header(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let first_line = stdout.lines().next().unwrap_or("");
    // Full format (-f): UID PID PPID C STIME TTY TIME CMD
    assert!(first_line.contains("UID"), "Expected UID in -f header");
    assert!(first_line.contains("PID"), "Expected PID in -f header");
    assert!(first_line.contains("PPID"), "Expected PPID in -f header");
    assert!(first_line.contains("TTY"), "Expected TTY in -f header");
    assert!(first_line.contains("TIME"), "Expected TIME in -f header");
}

fn check_long_format_header(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let first_line = stdout.lines().next().unwrap_or("");
    // Long format (-l): F S UID PID PPID C PRI NI ADDR SZ WCHAN TTY TIME CMD
    assert!(first_line.contains("F"), "Expected F in -l header");
    assert!(first_line.contains("S"), "Expected S in -l header");
    assert!(first_line.contains("PID"), "Expected PID in -l header");
    assert!(first_line.contains("PPID"), "Expected PPID in -l header");
    assert!(first_line.contains("PRI"), "Expected PRI in -l header");
    assert!(first_line.contains("NI"), "Expected NI in -l header");
}

fn check_custom_format_header(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let first_line = stdout.lines().next().unwrap_or("");
    // Custom format with -o pid,comm
    assert!(first_line.contains("PID"), "Expected PID in custom header");
    assert!(
        first_line.contains("COMMAND"),
        "Expected COMMAND in custom header"
    );
}

fn check_has_multiple_processes(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let line_count = stdout.lines().count();
    // At least header + 1 process
    assert!(line_count >= 2, "Expected at least 2 lines (header + data)");
}

fn check_time_format(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Skip header, check data lines
    for line in stdout.lines().skip(1) {
        // Time format should contain colons (HH:MM:SS or similar)
        if line.contains(':') {
            return; // Found time format
        }
    }
    // If no data lines, that's okay for filtered output
}

fn check_exit_failure(_: &TestPlan, output: &Output) {
    assert!(!output.status.success(), "Expected non-zero exit");
}

// ============================================
// Basic execution tests
// ============================================

#[test]
fn ps_no_args() {
    run_ps_test(vec![], 0, check_exit_success);
}

#[test]
fn ps_no_args_has_header() {
    run_ps_test(vec![], 0, check_default_header);
}

#[test]
fn ps_no_args_has_output() {
    run_ps_test(vec![], 0, check_output_nonempty);
}

// ============================================
// Selection option tests
// ============================================

#[test]
fn ps_all_processes() {
    run_ps_test(vec!["-A"], 0, check_has_multiple_processes);
}

#[test]
fn ps_all_processes_alias() {
    run_ps_test(vec!["-e"], 0, check_has_multiple_processes);
}

#[test]
fn ps_terminal_processes() {
    run_ps_test(vec!["-a"], 0, check_exit_success);
}

#[test]
fn ps_exclude_session_leaders() {
    run_ps_test(vec!["-d"], 0, check_exit_success);
}

#[test]
fn ps_combined_a_d() {
    // -a and -d together
    run_ps_test(vec!["-a", "-d"], 0, check_exit_success);
}

// ============================================
// Output format tests
// ============================================

#[test]
fn ps_full_format() {
    run_ps_test(vec!["-A", "-f"], 0, check_full_format_header);
}

#[test]
fn ps_long_format() {
    run_ps_test(vec!["-A", "-l"], 0, check_long_format_header);
}

#[test]
fn ps_custom_format_pid_comm() {
    run_ps_test(vec!["-A", "-o", "pid,comm"], 0, check_custom_format_header);
}

#[test]
fn ps_custom_format_with_custom_header() {
    run_ps_test(vec!["-A", "-o", "pid=PROCESS,comm=NAME"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first_line = stdout.lines().next().unwrap_or("");
        assert!(
            first_line.contains("PROCESS"),
            "Expected custom header PROCESS"
        );
        assert!(first_line.contains("NAME"), "Expected custom header NAME");
    });
}

#[test]
fn ps_multiple_o_options() {
    run_ps_test(vec!["-A", "-o", "pid", "-o", "comm"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first_line = stdout.lines().next().unwrap_or("");
        assert!(first_line.contains("PID"), "Expected PID header");
        assert!(first_line.contains("COMMAND"), "Expected COMMAND header");
    });
}

#[test]
fn ps_empty_header() {
    // Using = with nothing after should still work
    run_ps_test(vec!["-A", "-o", "pid=,comm="], 0, check_exit_success);
}

/// The first line of `stdout`, or "" when there is none.
fn first_line(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout)
        .lines()
        .next()
        .unwrap_or("")
        .to_string()
}

// POSIX -o: "If all the header strings are null, the header line shall not be
// written." Only the exit status was checked here, so nothing pinned whether
// the header was actually absent -- or, in the mixed case below, present.
#[test]
fn ps_all_null_headers_suppress_the_header_line() {
    run_ps_test(vec!["-A", "-o", "pid=,comm="], 0, |_, output| {
        let line = first_line(output);
        assert!(
            !line.contains("PID") && !line.contains("COMMAND"),
            "every header string is null, so no header line may be written: {line:?}"
        );
        // The data is still there -- suppressing the header is not suppressing
        // the report.
        assert!(
            line.split_whitespace()
                .next()
                .is_some_and(|f| f.chars().all(|c| c.is_ascii_digit())),
            "the first line must be a process row starting with a pid: {line:?}"
        );
    });
}

// The other side of the same clause: with one header string non-null the
// header line *is* written, and the null column's heading is blank. This is
// where an implementation that keys off "any null" rather than "all null"
// goes wrong, and nothing exercised it.
#[test]
fn ps_one_non_null_header_keeps_the_header_line() {
    run_ps_test(vec!["-A", "-o", "pid=,comm"], 0, |_, output| {
        let line = first_line(output);
        assert!(
            line.contains("COMMAND"),
            "a non-null header string means the header line is written: {line:?}"
        );
        assert!(
            !line.contains("PID"),
            "the null column's heading must be blank: {line:?}"
        );
    });
}

// The control: with no `=` at all both headings appear.
#[test]
fn ps_no_null_headers_names_every_column() {
    run_ps_test(vec!["-A", "-o", "pid,comm"], 0, |_, output| {
        let line = first_line(output);
        assert!(
            line.contains("PID") && line.contains("COMMAND"),
            "both headings must appear: {line:?}"
        );
    });
}

// A renamed heading is written instead of the default, which is the third
// thing `=` does and was equally unasserted.
#[test]
fn ps_renamed_header_replaces_the_default() {
    run_ps_test(vec!["-A", "-o", "pid=PROCNUM"], 0, |_, output| {
        let line = first_line(output);
        assert!(
            line.contains("PROCNUM"),
            "the supplied heading must be used: {line:?}"
        );
        assert!(
            !line.contains("PID"),
            "the default heading must not also appear: {line:?}"
        );
    });
}

// ============================================
// Filter option tests
// ============================================

#[test]
fn ps_filter_by_pid() {
    // Filter by current process's parent PID (should exist)
    run_ps_test(vec!["-p", "1"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Should have header and at least the filtered process
        let line_count = stdout.lines().count();
        assert!(
            line_count >= 1,
            "Expected at least header for -p filter, got {} lines",
            line_count
        );
    });
}

#[test]
fn ps_filter_by_user_numeric() {
    // Filter by current user's UID
    let uid = unsafe { libc::getuid() };
    run_ps_test(
        vec!["-u", &uid.to_string()],
        0,
        check_has_multiple_processes,
    );
}

#[test]
fn ps_filter_by_real_user() {
    let uid = unsafe { libc::getuid() };
    run_ps_test(
        vec!["-U", &uid.to_string()],
        0,
        check_has_multiple_processes,
    );
}

#[test]
fn ps_filter_by_group() {
    let gid = unsafe { libc::getgid() };
    run_ps_test(vec!["-G", &gid.to_string()], 0, check_exit_success);
}

#[test]
fn ps_filter_by_terminal() {
    // This may or may not find processes depending on environment
    run_ps_test(vec!["-t", "pts/0"], 0, check_exit_success);
}

// ============================================
// Error handling tests
// ============================================

#[test]
fn ps_invalid_format_specifier() {
    run_ps_test(vec!["-o", "invalid_field"], 1, check_exit_failure);
}

// ============================================
// POSIX format specifier tests
// ============================================

#[test]
fn ps_format_user() {
    run_ps_test(vec!["-A", "-o", "user"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first_line = stdout.lines().next().unwrap_or("");
        assert!(first_line.contains("USER"), "Expected USER header");
    });
}

#[test]
fn ps_format_ruser() {
    run_ps_test(vec!["-A", "-o", "ruser"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first_line = stdout.lines().next().unwrap_or("");
        assert!(first_line.contains("RUSER"), "Expected RUSER header");
    });
}

#[test]
fn ps_format_group() {
    run_ps_test(vec!["-A", "-o", "group"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first_line = stdout.lines().next().unwrap_or("");
        assert!(first_line.contains("GROUP"), "Expected GROUP header");
    });
}

#[test]
fn ps_format_ppid() {
    run_ps_test(vec!["-A", "-o", "ppid"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first_line = stdout.lines().next().unwrap_or("");
        assert!(first_line.contains("PPID"), "Expected PPID header");
    });
}

#[test]
fn ps_format_pgid() {
    run_ps_test(vec!["-A", "-o", "pgid"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first_line = stdout.lines().next().unwrap_or("");
        assert!(first_line.contains("PGID"), "Expected PGID header");
    });
}

#[test]
fn ps_format_nice() {
    run_ps_test(vec!["-A", "-o", "nice"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first_line = stdout.lines().next().unwrap_or("");
        assert!(first_line.contains("NI"), "Expected NI header");
    });
}

#[test]
fn ps_format_vsz() {
    run_ps_test(vec!["-A", "-o", "vsz"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first_line = stdout.lines().next().unwrap_or("");
        assert!(first_line.contains("VSZ"), "Expected VSZ header");
    });
}

#[test]
fn ps_format_time() {
    run_ps_test(vec!["-A", "-o", "time"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first_line = stdout.lines().next().unwrap_or("");
        assert!(first_line.contains("TIME"), "Expected TIME header");
    });
}

#[test]
fn ps_format_etime() {
    run_ps_test(vec!["-A", "-o", "etime"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first_line = stdout.lines().next().unwrap_or("");
        assert!(first_line.contains("ELAPSED"), "Expected ELAPSED header");
    });
}

#[test]
fn ps_format_tty() {
    run_ps_test(vec!["-A", "-o", "tty"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first_line = stdout.lines().next().unwrap_or("");
        assert!(first_line.contains("TT"), "Expected TT header");
    });
}

#[test]
fn ps_format_args() {
    run_ps_test(vec!["-A", "-o", "args"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first_line = stdout.lines().next().unwrap_or("");
        assert!(first_line.contains("COMMAND"), "Expected COMMAND header");
    });
}

// ============================================
// Combined option tests
// ============================================

#[test]
fn ps_all_with_full() {
    run_ps_test(vec!["-A", "-f"], 0, check_has_multiple_processes);
}

#[test]
fn ps_all_with_long() {
    run_ps_test(vec!["-A", "-l"], 0, check_has_multiple_processes);
}

#[test]
fn ps_full_and_long() {
    // Both -f and -l together
    run_ps_test(vec!["-A", "-f", "-l"], 0, check_exit_success);
}

// ============================================
// Output format validation
// ============================================

#[test]
fn ps_output_time_format() {
    run_ps_test(vec!["-A"], 0, check_time_format);
}

#[test]
fn ps_output_ends_with_newline() {
    run_ps_test(vec!["-A"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.ends_with('\n'), "Output should end with newline");
    });
}

// -w (wide) and -ww (no limit) are accepted (#P4).
#[test]
fn ps_wide_options_accepted() {
    run_ps_test(vec!["-A", "-w"], 0, check_exit_success);
    run_ps_test(vec!["-A", "-w", "-w"], 0, check_exit_success);
}

// -n namelist is accepted for XSI conformance and ignored (#P5).
#[test]
fn ps_namelist_accepted() {
    run_ps_test(vec!["-n", "/dev/null", "-A"], 0, check_exit_success);
}

// XBD 12.2, Guideline 7: an option-argument may begin with '-'. Each option
// below used to have the word after it refused as an unknown option.
#[test]
fn option_argument_may_begin_with_hyphen() {
    for opt in ["-g", "-G", "-p", "-t", "-u", "-U", "-o", "-n"] {
        plib::testing::assert_hyphen_option_argument("ps", &[opt, "-zq", "--help"]);
    }
}

// ps discarded every write error and exited 0.
#[test]
fn ps_reports_write_error() {
    plib::testing::assert_write_error_on_full_device("ps", &["-A"], b"", 1);
}

// A reader that stops after the first line must not kill ps with SIGPIPE once
// ps has produced its whole listing.  perl's dist/threads/t/join.t reads
// `ps -f |` up to its own line and dies if closing the pipe reports a failed
// ps.  procps fully buffers a pipe, so its listing is in the pipe before the
// reader sees the first byte; ps wrote line by line and was still writing when
// the reader closed.  The listing must fit in the pipe, as it must for procps:
// `pid,comm` keeps it small.
#[test]
fn ps_survives_reader_closing_after_first_line() {
    use std::io::Read;
    use std::process::{Command, Stdio};

    for _ in 0..20 {
        let mut child = Command::new(plib::testing::get_binary_path("ps"))
            .args(["-A", "-o", "pid,comm"])
            .stdout(Stdio::piped())
            .spawn()
            .expect("failed to spawn ps");
        let mut stdout = child.stdout.take().unwrap();
        let mut byte = [0u8; 1];
        loop {
            match stdout.read(&mut byte) {
                Ok(1) if byte[0] != b'\n' => continue,
                _ => break,
            }
        }
        // Give a kernel that copies a large write without holding the pipe
        // locked (macOS) time to finish it.
        #[cfg(not(target_os = "linux"))]
        std::thread::sleep(std::time::Duration::from_millis(100));
        drop(stdout);
        let status = child.wait().unwrap();
        assert!(
            status.success(),
            "ps failed after an early close: {status:?}"
        );
    }
}

/// `ps ARGS` over this test process alone: its header and its one line.
fn ps_self(args: &[&str]) -> Vec<String> {
    let mut argv: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    argv.extend(["-p".to_string(), std::process::id().to_string()]);
    let output = plib::testing::run_test_base("ps", &argv, b"");
    assert!(output.status.success(), "ps {argv:?} failed: {output:?}");
    let stdout = String::from_utf8(output.stdout).unwrap();
    let lines: Vec<String> = stdout.lines().map(String::from).collect();
    assert_eq!(lines.len(), 2, "expected a header and one line: {stdout:?}");
    lines
}

// The C column of -f is the processor utilization, an integer as procps
// prints it (CPU time over elapsed time, as a percentage capped at 99),
// right-aligned under its header; it was always "-".
#[test]
fn ps_full_format_c_is_an_integer() {
    let lines = ps_self(&["-f"]);
    let c = lines[1].split_whitespace().nth(3).unwrap();
    let value: u32 = c
        .parse()
        .unwrap_or_else(|_| panic!("C is not an integer: {:?}", lines[1]));
    assert!(value <= 99, "C is capped at 99: {value}");

    let lines = ps_self(&["-o", "c,pid"]);
    assert_eq!(lines[0].find('C'), Some(1), "header: {:?}", lines[0]);
    assert!(
        lines[1].as_bytes()[1].is_ascii_digit(),
        "C is right-aligned: {:?}",
        lines[1]
    );
}

// The last column is not padded: no trailing blanks after CMD, and no blanks
// before its header beyond the one separating it, as procps prints it.
#[test]
fn ps_last_column_is_not_padded() {
    for args in [&["-f"][..], &["-l"], &[], &["-o", "pid,comm"]] {
        let lines = ps_self(args);
        for line in &lines {
            assert!(
                !line.ends_with(' '),
                "ps {args:?}: trailing blanks in {line:?}"
            );
        }
        let last = lines[0].rfind(' ').unwrap();
        assert_ne!(
            lines[0].as_bytes()[last - 1],
            b' ',
            "ps {args:?}: last header is padded: {:?}",
            lines[0]
        );
    }
}
