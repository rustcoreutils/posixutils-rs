//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Integration tests for `stty`.
//!
//! `stty` operates on the termios state of its controlling terminal, so these
//! tests run it inside a pseudo-terminal via the shared `common::PtyHarness`.
//! Tests are skip-gated when the platform cannot allocate a PTY.

use crate::common::PtyHarness;

const STTY: &str = env!("CARGO_BIN_EXE_stty");

macro_rules! pty_or_skip {
    () => {
        match PtyHarness::new(24, 80) {
            Some(h) => h,
            None => {
                eprintln!("Skipping PTY test: cannot allocate a pseudo-terminal");
                return;
            }
        }
    };
}

// Audit #1: `stty <single mode operand>` previously panicked at
// `assert!(args.operands.len() > 1)`. It must now succeed.
#[test]
fn test_stty_single_operand_no_panic() {
    let pty = pty_or_skip!();
    let (code, _out) = pty.run(STTY, &["sane"]);
    assert_eq!(code, 0, "stty sane should exit 0, got {code}");

    let (code, _out) = pty.run(STTY, &["cs8"]);
    assert_eq!(
        code, 0,
        "stty cs8 (single operand) should exit 0, got {code}"
    );

    let (code, _out) = pty.run(STTY, &["raw"]);
    assert_eq!(code, 0, "stty raw should exit 0, got {code}");
}

// Audit #2: negation operands (`-echo`, ...) were rejected by clap. They must
// now reach the set logic, and the change must be observable via `stty -a`.
#[test]
fn test_stty_negation_operand_applied() {
    let pty = pty_or_skip!();

    // Exact whitespace-delimited token match: "-echo" is a prefix of "-echonl"
    // and "-echoprt", so a substring test would be ambiguous.
    let has_token = |out: &str, tok: &str| out.split_whitespace().any(|t| t == tok);

    let (code, _) = pty.run(STTY, &["-echo"]);
    assert_eq!(code, 0, "stty -echo should exit 0, got {code}");

    let (code, out) = pty.run(STTY, &["-a"]);
    assert_eq!(code, 0, "stty -a should exit 0");
    assert!(
        has_token(&out, "-echo"),
        "stty -a should report -echo after disabling echo; got:\n{out}"
    );

    // Re-enable and confirm the flag flips back.
    let (code, _) = pty.run(STTY, &["echo"]);
    assert_eq!(code, 0, "stty echo should exit 0, got {code}");
    let (_code, out) = pty.run(STTY, &["-a"]);
    assert!(
        has_token(&out, "echo") && !has_token(&out, "-echo"),
        "stty -a should report echo (not -echo) after re-enabling; got:\n{out}"
    );
}

// `stty -a` produces the speed line and flag groups.
#[test]
fn test_stty_all_output() {
    let pty = pty_or_skip!();
    let (code, out) = pty.run(STTY, &["-a"]);
    assert_eq!(code, 0, "stty -a should exit 0");
    assert!(
        out.contains("baud"),
        "stty -a should print a speed line; got:\n{out}"
    );
}

// `stty -g` emits a single reusable settings blob that restores without error.
#[test]
fn test_stty_save_restore_roundtrip() {
    let pty = pty_or_skip!();
    let (code, blob) = pty.run(STTY, &["-g"]);
    assert_eq!(code, 0, "stty -g should exit 0");
    let blob = blob.trim().to_string();
    assert!(
        blob.starts_with("pfmt1"),
        "stty -g blob should start with pfmt1; got: {blob:?}"
    );

    let (code, _) = pty.run(STTY, &[blob.as_str()]);
    assert_eq!(
        code, 0,
        "restoring a saved -g blob should exit 0, got {code}"
    );
}

// Audit #3: single-character control-char assignment was rejected ("Invalid
// cchar specification"). It must now set the control character. Use a literal
// backspace (0x08) so `stty -a` renders it as "^H".
#[test]
fn test_stty_single_char_cchar_assignment() {
    let pty = pty_or_skip!();
    let (code, _) = pty.run(STTY, &["erase", "\u{8}"]);
    assert_eq!(code, 0, "stty erase <BS> should exit 0, got {code}");

    let (_code, out) = pty.run(STTY, &["-a"]);
    assert!(
        out.to_lowercase().contains("erase = ^h"),
        "stty -a should show erase = ^H after assigning a literal backspace; got:\n{out}"
    );
}

// Audit #4: rows/cols operands and the size informational query were missing
// ("Unknown operand"). rows/cols set the window size; size reports it.
#[test]
fn test_stty_rows_cols_size() {
    let pty = pty_or_skip!();
    let (code, _) = pty.run(STTY, &["rows", "40", "cols", "100"]);
    assert_eq!(code, 0, "stty rows 40 cols 100 should exit 0, got {code}");

    let (code, out) = pty.run(STTY, &["size"]);
    assert_eq!(code, 0, "stty size should exit 0, got {code}");
    assert_eq!(
        out.trim(),
        "40 100",
        "stty size should report the rows/cols just set; got: {out:?}"
    );
}

// Audit #6: the speed table mis-keyed B50 as "54", so `stty 50` failed with
// "invalid speed". It must now resolve to B50 and apply without error. (The
// pts line discipline does not honor baud changes, so the value is not read
// back here; the table fix itself is covered by the `test_speed_table_50_not_54`
// unit test in stty.rs.)
#[test]
fn test_stty_speed_50() {
    let pty = pty_or_skip!();
    let (code, _) = pty.run(STTY, &["50"]);
    assert_eq!(
        code, 0,
        "stty 50 should exit 0 (was: 'invalid speed'), got {code}"
    );
}

// Audit #7: a printable control-char value rendered as a decimal. It must now
// render as the character.
#[test]
fn test_stty_printable_cchar_rendering() {
    let pty = pty_or_skip!();
    let (code, _) = pty.run(STTY, &["intr", "q"]);
    assert_eq!(code, 0, "stty intr q should exit 0, got {code}");

    let (_code, out) = pty.run(STTY, &["-a"]);
    assert!(
        out.contains("intr = q"),
        "stty -a should render the printable intr char as 'q', not a decimal; got:\n{out}"
    );
}

// Audit #10: operands alongside -a/-g are a usage error.
#[test]
fn test_stty_operands_with_all_rejected() {
    let pty = pty_or_skip!();
    let (code, _) = pty.run(STTY, &["-a", "sane"]);
    assert_ne!(
        code, 0,
        "stty -a sane should be a usage error (non-zero exit)"
    );
}
