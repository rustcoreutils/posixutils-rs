//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};
use std::process::Output;

fn run_getconf_test(args: Vec<&str>, expected_exit_code: i32, check_fn: fn(&TestPlan, &Output)) {
    let plan = TestPlan {
        cmd: "getconf".to_string(),
        args: args.iter().map(|&s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    };

    run_test_with_checker(plan, check_fn);
}

fn check_output_is_positive_integer(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let value: i64 = stdout.trim().parse().expect("Output is not a valid number");
    assert!(
        value > 0,
        "Expected a positive integer, but got '{}'",
        value
    );
}

fn check_output_is_nonnegative_integer(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let value: i64 = stdout.trim().parse().expect("Output is not a valid number");
    assert!(
        value >= 0,
        "Expected a non-negative integer, but got '{}'",
        value
    );
}

fn check_output_is_integer_or_undefined(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let trimmed = stdout.trim();
    if trimmed == "undefined" {
        return; // Valid output
    }
    let _value: i64 = trimmed
        .parse()
        .expect("Output is neither a valid number nor 'undefined'");
}

fn check_has_stderr(_: &TestPlan, output: &Output) {
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.is_empty(),
        "Expected error message on stderr, but got empty"
    );
}

fn check_output_is_nonempty_string(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.trim().is_empty(),
        "Expected non-empty string output"
    );
}

// ============================================
// Sysconf variable tests
// ============================================

#[test]
fn sysconf_arg_max() {
    run_getconf_test(vec!["ARG_MAX"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_ngroups_max() {
    run_getconf_test(vec!["NGROUPS_MAX"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_open_max() {
    run_getconf_test(vec!["OPEN_MAX"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_page_size() {
    run_getconf_test(vec!["PAGE_SIZE"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_pagesize() {
    run_getconf_test(vec!["PAGESIZE"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_posix_version() {
    run_getconf_test(vec!["_POSIX_VERSION"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_child_max() {
    run_getconf_test(vec!["CHILD_MAX"], 0, check_output_is_integer_or_undefined);
}

#[test]
fn sysconf_host_name_max() {
    run_getconf_test(vec!["HOST_NAME_MAX"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_login_name_max() {
    run_getconf_test(vec!["LOGIN_NAME_MAX"], 0, check_output_is_positive_integer);
}

#[test]
fn sysconf_line_max() {
    run_getconf_test(vec!["LINE_MAX"], 0, check_output_is_positive_integer);
}

// Test POSIX2_* compatibility aliases (required by POSIX for backwards compat)
#[test]
fn sysconf_posix2_version() {
    run_getconf_test(
        vec!["POSIX2_VERSION"],
        0,
        check_output_is_integer_or_undefined,
    );
}

#[test]
fn sysconf_posix2_c_bind() {
    run_getconf_test(
        vec!["POSIX2_C_BIND"],
        0,
        check_output_is_integer_or_undefined,
    );
}

#[test]
fn sysconf_posix2_line_max() {
    run_getconf_test(vec!["POSIX2_LINE_MAX"], 0, check_output_is_positive_integer);
}

// Test _SC_ prefixed names
#[test]
fn sysconf_sc_arg_max() {
    run_getconf_test(vec!["_SC_ARG_MAX"], 0, check_output_is_positive_integer);
}

// ============================================
// Pathconf variable tests
// ============================================

#[test]
fn pathconf_link_max() {
    run_getconf_test(vec!["LINK_MAX", "/"], 0, check_output_is_positive_integer);
}

#[test]
fn pathconf_name_max() {
    run_getconf_test(vec!["NAME_MAX", "/"], 0, check_output_is_positive_integer);
}

#[test]
fn pathconf_path_max() {
    run_getconf_test(vec!["PATH_MAX", "/"], 0, check_output_is_positive_integer);
}

#[test]
fn pathconf_pipe_buf() {
    run_getconf_test(vec!["PIPE_BUF", "/"], 0, check_output_is_positive_integer);
}

#[test]
fn pathconf_chown_restricted() {
    run_getconf_test(
        vec!["CHOWN_RESTRICTED", "/"],
        0,
        check_output_is_nonnegative_integer,
    );
}

#[test]
fn pathconf_no_trunc() {
    run_getconf_test(
        vec!["NO_TRUNC", "/"],
        0,
        check_output_is_nonnegative_integer,
    );
}

#[test]
fn pathconf_filesizebits() {
    run_getconf_test(
        vec!["FILESIZEBITS", "/"],
        0,
        check_output_is_integer_or_undefined,
    );
}

#[test]
fn pathconf_tmp_dir() {
    run_getconf_test(
        vec!["NAME_MAX", "/tmp"],
        0,
        check_output_is_positive_integer,
    );
}

// ============================================
// Confstr variable tests
// ============================================

#[test]
fn confstr_path() {
    run_getconf_test(vec!["PATH"], 0, check_output_is_nonempty_string);
}

#[cfg(target_os = "macos")]
#[test]
fn confstr_darwin_user_dir() {
    run_getconf_test(vec!["DARWIN_USER_DIR"], 0, check_output_is_nonempty_string);
}

#[cfg(target_os = "linux")]
#[test]
fn confstr_gnu_libc_version() {
    run_getconf_test(vec!["GNU_LIBC_VERSION"], 0, check_output_is_nonempty_string);
}

// ============================================
// Error handling tests
// ============================================

#[test]
fn error_invalid_variable() {
    run_getconf_test(
        vec!["INVALID_NONEXISTENT_VARIABLE_XYZ"],
        1,
        check_has_stderr,
    );
}

#[test]
fn error_invalid_pathname() {
    run_getconf_test(
        vec!["NAME_MAX", "/nonexistent/path/that/does/not/exist"],
        1,
        check_has_stderr,
    );
}

// ============================================
// -v specification tests
// ============================================

#[test]
fn v_specification_valid_posix_v7() {
    // Valid specification should be accepted (as no-op)
    run_getconf_test(
        vec!["-v", "POSIX_V7_LP64_OFF64", "ARG_MAX"],
        0,
        check_output_is_positive_integer,
    );
}

#[test]
fn v_specification_valid_posix_v6() {
    run_getconf_test(
        vec!["-v", "POSIX_V6_LP64_OFF64", "ARG_MAX"],
        0,
        check_output_is_positive_integer,
    );
}

#[test]
fn v_specification_invalid() {
    run_getconf_test(vec!["-v", "INVALID_SPEC", "ARG_MAX"], 1, check_has_stderr);
}

#[test]
fn v_specification_with_pathconf() {
    run_getconf_test(
        vec!["-v", "POSIX_V7_LP64_OFF64", "NAME_MAX", "/"],
        0,
        check_output_is_positive_integer,
    );
}

// ============================================
// Output format tests
// ============================================

#[test]
fn output_ends_with_newline() {
    let plan = TestPlan {
        cmd: "getconf".to_string(),
        args: vec!["ARG_MAX".to_string()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };

    run_test_with_checker(plan, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.ends_with('\n'),
            "Output should end with newline, got: {:?}",
            stdout
        );
    });
}

/// The programming environments the `c17` page mandates (88106-88176).
///
/// POSIX.2024 is Issue 8, so V8 is the required spelling; Austin Group Defect
/// 1330 renamed the V7 names without changing the environments behind them.
/// `getconf -v POSIX_V8_*` was already accepted, but every V8 *variable* was
/// rejected, so an application following the spec's own procedure -- read the
/// flags for the environment it wants, then compile with them -- could not get
/// past the first step.
///
/// Checked as V8-agrees-with-V7 rather than against fixed strings: the values
/// are the host's, and differ between a 32- and 64-bit machine.
#[test]
fn posix_v8_programming_environments_match_v7() {
    // The flag lists: compiler and linker flags are the same strings whichever
    // issue asked for them.
    let flag_names = [
        "ILP32_OFF32_CFLAGS",
        "ILP32_OFF32_LDFLAGS",
        "ILP32_OFF32_LIBS",
        "ILP32_OFFBIG_CFLAGS",
        "ILP32_OFFBIG_LDFLAGS",
        "ILP32_OFFBIG_LIBS",
        "LP64_OFF64_CFLAGS",
        "LP64_OFF64_LDFLAGS",
        "LP64_OFF64_LIBS",
        "LPBIG_OFFBIG_CFLAGS",
        "LPBIG_OFFBIG_LDFLAGS",
        "LPBIG_OFFBIG_LIBS",
    ];

    for suffix in flag_names {
        let v8 = getconf_output(&format!("POSIX_V8_{suffix}"));
        let v7 = getconf_output(&format!("POSIX_V7_{suffix}"));
        assert_eq!(
            v8, v7,
            "POSIX_V8_{suffix} must resolve, and to the same environment as V7"
        );
    }
}

/// `WIDTH_RESTRICTED_ENVS` is the one that answers with environment *names*
/// rather than flags, and 88129-88132 wants them "suitable for use with the
/// getconf -v option".
///
/// No host has a V8 confstr: glibc answers a V8 query from its V7 constant and
/// macOS from its V6 one, so the reply comes back in an earlier issue's
/// spelling. Austin Group Defect 1330 renamed the names without changing the
/// environments behind them, so the answer is restated in the issue that was
/// asked about.
///
/// What is *not* asserted is the content of the host's list. POSIX does not
/// promise that any environment meets the width restriction, and `getconf`
/// prints `undefined` for a variable that is valid but has no value — which is
/// how this first failed on macOS, where the round-trip below tried to feed
/// `undefined` back to `getconf -v`.
#[test]
fn posix_width_restricted_envs_answer_in_the_issue_asked_about() {
    let v8 = getconf_output("POSIX_V8_WIDTH_RESTRICTED_ENVS");
    let v7 = getconf_output("POSIX_V7_WIDTH_RESTRICTED_ENVS");

    // The rule: a V8 query never answers in an earlier issue's spelling.
    for stale in ["POSIX_V5_", "POSIX_V6_", "POSIX_V7_"] {
        assert!(
            !v8.contains(stale),
            "a V8 query answered with a {stale} name: {v8:?}"
        );
    }

    // The two queries name the same environments, differing only in the issue
    // they are stated in. Compared with the prefix stripped from both, so this
    // does not just restate the implementation's own rewrite.
    let strip = |s: &str| {
        s.split_whitespace()
            .map(|n| n.rsplit_once('_').map_or(n, |(_, rest)| rest).to_string())
            .collect::<Vec<_>>()
    };
    assert_eq!(
        strip(&v8),
        strip(&v7),
        "the V8 and V7 answers name different environments: {v8:?} vs {v7:?}"
    );

    // Each name the host does offer must round-trip: being usable with
    // `getconf -v` is the whole point of the variable. A reply that names no
    // environment — `undefined`, or an empty list — is a conforming answer and
    // has nothing to round-trip.
    for name in v8.split_whitespace().filter(|n| n.starts_with("POSIX_V")) {
        run_getconf_test(
            vec!["-v", name, "POSIX_V8_LP64_OFF64_CFLAGS"],
            0,
            |_, output| {
                assert!(
                    output.status.success(),
                    "getconf -v rejected a name it had just printed"
                );
            },
        );
    }
}

/// The `_POSIX_V8_*` support variables say whether an environment is offered.
///
/// A "0" or "undefined" answer is as valid as "1" -- a 64-bit host need not
/// offer ILP32 -- so this asserts they are answered at all, and consistently
/// with the V7 spelling.
#[test]
fn posix_v8_environment_support_variables_are_answered() {
    for suffix in ["ILP32_OFF32", "ILP32_OFFBIG", "LP64_OFF64", "LPBIG_OFFBIG"] {
        let v8 = getconf_output(&format!("_POSIX_V8_{suffix}"));
        let v7 = getconf_output(&format!("_POSIX_V7_{suffix}"));
        assert!(
            !v8.contains("unrecognized"),
            "_POSIX_V8_{suffix} should be a known variable, got {v8:?}"
        );
        assert_eq!(v8, v7, "_POSIX_V8_{suffix} should agree with V7");
    }
}

/// The multi-threaded programming environment's flags (c17, Table 3-6).
///
/// Orthogonal to the four type-size environments, and mandated alongside them
/// since Issue 8. Unlike those, there is no host query behind these: glibc
/// declares no `_CS_POSIX_V*_THREADS_*`, its own `getconf` calls both names
/// unrecognized, and Darwin's `confstr` stops at Issue 6. So the value is a
/// fixed string of ours, and the test asserts the string rather than agreement
/// with a V7 spelling that would only be comparing our table against itself.
#[test]
fn posix_v8_threads_environment_flags_are_answered() {
    for var in [
        "POSIX_V8_THREADS_CFLAGS",
        "POSIX_V8_THREADS_LDFLAGS",
        "POSIX_V7_THREADS_CFLAGS",
        "POSIX_V7_THREADS_LDFLAGS",
    ] {
        let got = getconf_output(var);
        assert_eq!(
            got.trim_end_matches('\n'),
            "-pthread",
            "{var} should report the threading flags, got {got:?}"
        );
    }
}

/// Run `getconf VAR` and return its stdout, or the error text if it failed.
fn getconf_output(var: &str) -> String {
    let plan = TestPlan {
        cmd: "getconf".to_string(),
        args: vec![var.to_string()],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };
    let mut captured = String::new();
    run_test_with_checker(plan, |_, output| {
        captured = if output.status.success() {
            String::from_utf8_lossy(&output.stdout).into_owned()
        } else {
            String::from_utf8_lossy(&output.stderr).into_owned()
        };
    });
    captured
}
