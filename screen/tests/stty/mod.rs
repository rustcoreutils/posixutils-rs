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
