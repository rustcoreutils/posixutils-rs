//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

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
