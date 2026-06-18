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
//! tests run it inside a pseudo-terminal. Because a PTY can host only one child
//! (see `common`), state changes that must be observed by a later `stty -a` are
//! chained inside a single `/bin/sh -c` via `run_sh`. Tests are skip-gated when
//! the platform cannot allocate a PTY.

use crate::common::{run, run_sh};

const STTY: &str = env!("CARGO_BIN_EXE_stty");

// Unwrap a PTY result, or skip the test when no PTY is available.
macro_rules! pty {
    ($e:expr) => {
        match $e {
            Some(v) => v,
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
    for op in ["sane", "cs8", "raw"] {
        let (code, _out) = pty!(run(STTY, &[op]));
        assert_eq!(code, 0, "stty {op} should exit 0, got {code}");
    }
}

// Audit #2: negation operands (`-echo`, ...) were rejected by clap. They must
// now reach the set logic, observable via a chained `stty -a`.
#[test]
fn test_stty_negation_operand_applied() {
    // Exact whitespace-delimited token match: "-echo" is a prefix of "-echonl"
    // and "-echoprt", so a substring test would be ambiguous.
    let has_token = |out: &str, tok: &str| out.split_whitespace().any(|t| t == tok);

    let (code, out) = pty!(run_sh(&format!("'{STTY}' -echo && '{STTY}' -a")));
    assert_eq!(code, 0, "stty -echo && stty -a should exit 0, got {code}");
    assert!(
        has_token(&out, "-echo"),
        "stty -a should report -echo after disabling echo; got:\n{out}"
    );

    let (code, out) = pty!(run_sh(&format!("'{STTY}' echo && '{STTY}' -a")));
    assert_eq!(code, 0, "stty echo && stty -a should exit 0, got {code}");
    assert!(
        has_token(&out, "echo") && !has_token(&out, "-echo"),
        "stty -a should report echo (not -echo) after re-enabling; got:\n{out}"
    );
}

// `stty -a` produces the speed line and flag groups.
#[test]
fn test_stty_all_output() {
    let (code, out) = pty!(run(STTY, &["-a"]));
    assert_eq!(code, 0, "stty -a should exit 0");
    assert!(
        out.contains("baud"),
        "stty -a should print a speed line; got:\n{out}"
    );
}

// `stty -g` emits a single reusable settings blob that restores without error.
#[test]
fn test_stty_save_restore_roundtrip() {
    let (code, blob) = pty!(run(STTY, &["-g"]));
    assert_eq!(code, 0, "stty -g should exit 0");
    assert!(
        blob.trim().starts_with("pfmt1"),
        "stty -g blob should start with pfmt1; got: {:?}",
        blob.trim()
    );

    // Save and restore within one controlling terminal.
    let (code, _) = pty!(run_sh(&format!("S=$('{STTY}' -g) && '{STTY}' \"$S\"")));
    assert_eq!(
        code, 0,
        "restoring a saved -g blob should exit 0, got {code}"
    );
}

// Audit #3: single-character control-char assignment was rejected ("Invalid
// cchar specification"). It must now set the control character (here a printable
// 'Z'), observable via a chained `stty -a`.
#[test]
fn test_stty_single_char_cchar_assignment() {
    let (code, out) = pty!(run_sh(&format!("'{STTY}' erase Z && '{STTY}' -a")));
    assert_eq!(code, 0, "stty erase Z && stty -a should exit 0, got {code}");
    assert!(
        out.contains("erase = Z"),
        "stty -a should show erase = Z after a single-char assignment; got:\n{out}"
    );
}

// Audit #4: rows/cols operands and the size informational query were missing
// ("Unknown operand"). rows/cols set the window size; size reports it.
#[test]
fn test_stty_rows_cols_size() {
    let (code, out) = pty!(run_sh(&format!(
        "'{STTY}' rows 40 cols 100 && '{STTY}' size"
    )));
    assert_eq!(
        code, 0,
        "stty rows/cols && stty size should exit 0, got {code}"
    );
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
    let (code, _) = pty!(run(STTY, &["50"]));
    assert_eq!(
        code, 0,
        "stty 50 should exit 0 (was: 'invalid speed'), got {code}"
    );
}

// Audit #7: a printable control-char value rendered as a decimal. It must now
// render as the character.
#[test]
fn test_stty_printable_cchar_rendering() {
    let (code, out) = pty!(run_sh(&format!("'{STTY}' intr q && '{STTY}' -a")));
    assert_eq!(code, 0, "stty intr q && stty -a should exit 0, got {code}");
    assert!(
        out.contains("intr = q"),
        "stty -a should render the printable intr char as 'q', not a decimal; got:\n{out}"
    );
}

// Audit #10: operands alongside -a/-g are a usage error.
#[test]
fn test_stty_operands_with_all_rejected() {
    let (code, _) = pty!(run(STTY, &["-a", "sane"]));
    assert_ne!(
        code, 0,
        "stty -a sane should be a usage error (non-zero exit)"
    );
}
