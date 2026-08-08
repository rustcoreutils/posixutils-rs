//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::process::Command;

fn run_tput(args: &[&str]) -> (i32, String, String) {
    let output = Command::new(env!("CARGO_BIN_EXE_tput"))
        .args(args)
        .output()
        .expect("Failed to execute tput");

    let exit_code = output.status.code().unwrap_or(-1);
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();

    (exit_code, stdout, stderr)
}

#[test]
fn test_tput_invalid_operand() {
    // POSIX exit code 4 for invalid operand
    let (exit_code, _stdout, stderr) = run_tput(&["invalidoperand12345"]);
    assert_eq!(exit_code, 4, "Expected exit code 4 for invalid operand");
    assert!(
        stderr.contains("Invalid operand"),
        "Expected error message about invalid operand"
    );
}

#[test]
fn test_tput_invalid_terminal_type() {
    // POSIX exit code 3 for no terminal info
    let (exit_code, _stdout, stderr) = run_tput(&["-T", "nonexistent_terminal_type_xyz", "clear"]);
    assert_eq!(
        exit_code, 3,
        "Expected exit code 3 for unknown terminal type"
    );
    assert!(
        stderr.contains("terminal type") || stderr.contains("Unknown"),
        "Expected error message about terminal type"
    );
}

#[test]
fn test_tput_no_operand() {
    // POSIX exit code 2 for usage error (missing required operand)
    let (exit_code, _stdout, _stderr) = run_tput(&[]);
    assert_eq!(exit_code, 2, "Expected exit code 2 for missing operand");
}

// tput writes to stdout even when it is not a terminal (it is commonly used in
// command substitution, e.g. `clear=$(tput clear)`), so these run with a piped
// stdout against a known terminal type.

#[test]
fn test_tput_clear_xterm() {
    let (exit_code, stdout, _stderr) = run_tput(&["-T", "xterm", "clear"]);
    assert_eq!(exit_code, 0, "tput -T xterm clear should exit 0");
    assert!(
        !stdout.is_empty(),
        "tput clear should emit the xterm clear sequence"
    );
}

// Audit #T3: a valid operand runs even when a later operand is invalid; the
// invalid operand still yields exit 4.
#[test]
fn test_tput_valid_then_invalid_operand() {
    let (exit_code, stdout, stderr) = run_tput(&["-T", "xterm", "clear", "bogus"]);
    assert_eq!(
        exit_code, 4,
        "an invalid operand should yield exit 4 even after a valid one"
    );
    assert!(
        !stdout.is_empty(),
        "the leading 'clear' operand should have produced output before the invalid one"
    );
    assert!(stderr.contains("Invalid operand"));
}

#[test]
fn test_tput_multiple_valid_operands() {
    let (exit_code, _stdout, _stderr) = run_tput(&["-T", "xterm", "clear", "init"]);
    assert_eq!(exit_code, 0, "clear + init should both succeed on xterm");
}

// ============================================================================
// init / reset file capabilities -- audit #T1
//
// `if`, `rf` and `iprog` name a *file* whose contents are the sequence, and a
// *program* to run. Very few real terminfo entries carry them, which is why
// this had only ever been exercised by hand. A synthetic compiled entry written
// straight into a tempdir and selected with TERMINFO is a portable fixture: it
// needs neither `tic` nor a terminal that happens to define these caps.
// ============================================================================

/// String capability indices, from the terminfo standard ordering.
mod cap {
    pub const CLEAR_SCREEN: usize = 5;
    pub const INIT_1STRING: usize = 48;
    pub const INIT_2STRING: usize = 49;
    pub const INIT_3STRING: usize = 50;
    pub const INIT_FILE: usize = 51;
    pub const RESET_1STRING: usize = 122;
    pub const RESET_2STRING: usize = 123;
    pub const RESET_3STRING: usize = 124;
    pub const RESET_FILE: usize = 125;
    pub const INIT_PROG: usize = 138;
    /// One past the highest index used here.
    pub const COUNT: usize = 139;
}

/// Write a compiled terminfo entry in the legacy binary format.
///
/// Header: magic 0o432, then the sizes of the names, boolean, number and
/// string-offset sections and of the string table, each a little-endian i16.
/// The number section must start on an even boundary, so a pad byte follows the
/// booleans when the names and booleans together are an odd length. An offset
/// of -1 marks an absent capability.
fn write_terminfo(
    dir: &std::path::Path,
    name: &str,
    strings: &[(usize, &str)],
) -> std::path::PathBuf {
    let names = format!("{name}|synthetic test terminal\0");
    let bools: Vec<u8> = Vec::new();
    let numbers: Vec<i16> = Vec::new();

    let mut offsets = vec![-1i16; cap::COUNT];
    let mut table: Vec<u8> = Vec::new();
    for (index, value) in strings {
        offsets[*index] = table.len() as i16;
        table.extend_from_slice(value.as_bytes());
        table.push(0);
    }

    let mut out: Vec<u8> = Vec::new();
    let push = |v: i16, out: &mut Vec<u8>| out.extend_from_slice(&v.to_le_bytes());
    push(0o432, &mut out);
    push(names.len() as i16, &mut out);
    push(bools.len() as i16, &mut out);
    push(numbers.len() as i16, &mut out);
    push(offsets.len() as i16, &mut out);
    push(table.len() as i16, &mut out);
    out.extend_from_slice(names.as_bytes());
    out.extend_from_slice(&bools);
    if (names.len() + bools.len()) % 2 != 0 {
        out.push(0);
    }
    for n in &numbers {
        push(*n, &mut out);
    }
    for o in &offsets {
        push(*o, &mut out);
    }
    out.extend_from_slice(&table);

    // Entries live at <dir>/<first letter of name>/<name>.
    let sub = dir.join(&name[..1]);
    std::fs::create_dir_all(&sub).unwrap();
    let path = sub.join(name);
    std::fs::write(&path, out).unwrap();
    path
}

/// Run tput against a synthetic terminfo database.
fn run_tput_with_terminfo(terminfo_dir: &std::path::Path, args: &[&str]) -> (i32, Vec<u8>, String) {
    let output = Command::new(env!("CARGO_BIN_EXE_tput"))
        .args(args)
        .env("TERMINFO", terminfo_dir)
        .env_remove("TERMINFO_DIRS")
        .output()
        .expect("Failed to execute tput");
    (
        output.status.code().unwrap_or(-1),
        output.stdout,
        String::from_utf8_lossy(&output.stderr).to_string(),
    )
}

#[test]
fn test_tput_init_emits_file_capability_and_runs_init_prog() {
    let dir = tempfile::tempdir().unwrap();
    let ti = dir.path().join("terminfo");

    // `if` names a file whose *contents* are the sequence.
    let init_file = dir.path().join("init.seq");
    std::fs::write(&init_file, b"FILE-INIT-BYTES").unwrap();

    // `iprog` names a program to run; have it leave a marker behind.
    let marker = dir.path().join("iprog.ran");
    let prog = dir.path().join("initprog.sh");
    std::fs::write(
        &prog,
        format!("#!/bin/sh\ntouch {}\n", marker.to_string_lossy()),
    )
    .unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&prog, std::fs::Permissions::from_mode(0o755)).unwrap();
    }

    write_terminfo(
        &ti,
        "synthinit",
        &[
            (cap::INIT_1STRING, "IS1"),
            (cap::INIT_2STRING, "IS2"),
            (cap::INIT_FILE, init_file.to_str().unwrap()),
            (cap::INIT_PROG, prog.to_str().unwrap()),
            (cap::INIT_3STRING, "IS3"),
        ],
    );

    let (code, stdout, stderr) = run_tput_with_terminfo(&ti, &["-T", "synthinit", "init"]);
    assert_eq!(code, 0, "tput init failed: {stderr}");

    let out = String::from_utf8_lossy(&stdout);
    assert!(
        out.contains("FILE-INIT-BYTES"),
        "`if` must emit the file's contents, not its name: {out:?}"
    );
    // POSIX order: is1, is2, if, iprog, is3.
    let pos = |needle: &str| {
        out.find(needle)
            .unwrap_or_else(|| panic!("missing {needle} in {out:?}"))
    };
    assert!(
        pos("IS1") < pos("IS2")
            && pos("IS2") < pos("FILE-INIT-BYTES")
            && pos("FILE-INIT-BYTES") < pos("IS3"),
        "init sequences must be emitted in order is1, is2, if, is3: {out:?}"
    );
    assert!(
        marker.exists(),
        "`iprog` names a program to run; it was not executed"
    );
}

#[test]
fn test_tput_reset_emits_reset_file_capability() {
    let dir = tempfile::tempdir().unwrap();
    let ti = dir.path().join("terminfo");

    let reset_file = dir.path().join("reset.seq");
    std::fs::write(&reset_file, b"FILE-RESET-BYTES").unwrap();

    write_terminfo(
        &ti,
        "synthreset",
        &[
            (cap::RESET_1STRING, "RS1"),
            (cap::RESET_2STRING, "RS2"),
            (cap::RESET_FILE, reset_file.to_str().unwrap()),
            (cap::RESET_3STRING, "RS3"),
        ],
    );

    let (code, stdout, stderr) = run_tput_with_terminfo(&ti, &["-T", "synthreset", "reset"]);
    assert_eq!(code, 0, "tput reset failed: {stderr}");

    let out = String::from_utf8_lossy(&stdout);
    assert!(
        out.contains("FILE-RESET-BYTES"),
        "`rf` must emit the file's contents: {out:?}"
    );
    let pos = |needle: &str| {
        out.find(needle)
            .unwrap_or_else(|| panic!("missing {needle} in {out:?}"))
    };
    assert!(
        pos("RS1") < pos("RS2")
            && pos("RS2") < pos("FILE-RESET-BYTES")
            && pos("FILE-RESET-BYTES") < pos("RS3"),
        "reset sequences must be emitted in order rs1, rs2, rf, rs3: {out:?}"
    );
}

#[test]
fn test_tput_reset_falls_back_to_init_when_no_reset_strings() {
    // Historical parity: a terminal with no reset capabilities at all is reset
    // with its initialization sequence.
    let dir = tempfile::tempdir().unwrap();
    let ti = dir.path().join("terminfo");
    write_terminfo(
        &ti,
        "synthfallback",
        &[(cap::INIT_2STRING, "ONLY-INIT"), (cap::CLEAR_SCREEN, "CLR")],
    );

    let (code, stdout, stderr) = run_tput_with_terminfo(&ti, &["-T", "synthfallback", "reset"]);
    assert_eq!(code, 0, "tput reset failed: {stderr}");
    assert!(
        String::from_utf8_lossy(&stdout).contains("ONLY-INIT"),
        "with no reset strings, reset must fall back to the init sequence"
    );
}

#[test]
fn test_tput_unreadable_reset_file_still_falls_back() {
    // An unreadable `rf` does not count as "reset emitted", so the init
    // sequence must still be used.
    let dir = tempfile::tempdir().unwrap();
    let ti = dir.path().join("terminfo");
    write_terminfo(
        &ti,
        "synthmissing",
        &[
            (cap::RESET_FILE, "/nonexistent/reset.seq"),
            (cap::INIT_2STRING, "INIT-FALLBACK"),
        ],
    );

    let (code, stdout, stderr) = run_tput_with_terminfo(&ti, &["-T", "synthmissing", "reset"]);
    assert_eq!(code, 0, "tput reset failed: {stderr}");
    assert!(
        String::from_utf8_lossy(&stdout).contains("INIT-FALLBACK"),
        "an unreadable rf must not count as a reset, so init must follow"
    );
}
