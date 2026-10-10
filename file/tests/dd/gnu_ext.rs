//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU extensions: `conv=fsync` and `status=LEVEL`.  Expected results were
//! taken from GNU coreutils dd, less its transfer-rate line.

use super::run_dd;

/// util-linux tests/ts/fadvise/drop: `conv=fsync` is accepted and the output
/// is the same as without it.
#[test]
fn test_conv_fsync_regular_file() {
    let dir = plib::tmp::tempdir().unwrap();
    let out = dir.path().join("ddtest");
    let of = format!("of={}", out.to_str().unwrap());
    let args = ["if=/dev/zero", &of, "bs=65536", "count=8", "conv=fsync"];
    let (stdout, err, code) = run_dd(&args, b"");
    assert_eq!(code, Some(0), "{err}");
    assert_eq!(err, "8+0 records in\n8+0 records out\n");
    assert!(stdout.is_empty());
    let data = std::fs::read(&out).unwrap();
    assert_eq!(data.len(), 524288);
    assert!(data.iter().all(|&b| b == 0));
}

/// `fsync` combines with the other values in the `conv=` list.
#[test]
fn test_conv_fsync_combines() {
    let dir = plib::tmp::tempdir().unwrap();
    let out = dir.path().join("out");
    let of = format!("of={}", out.to_str().unwrap());

    std::fs::write(&out, b"hello world").unwrap();
    let args = ["if=/dev/zero", &of, "bs=3", "count=1", "conv=notrunc,fsync"];
    let (_, err, code) = run_dd(&args, b"");
    assert_eq!(code, Some(0), "{err}");
    assert_eq!(err, "1+0 records in\n1+0 records out\n");
    assert_eq!(std::fs::read(&out).unwrap(), b"\0\0\0lo world");

    // The sync-padded final block is written before the fsync.
    let (_, err, code) = run_dd(&[&of, "bs=8", "conv=sync,fsync"], b"abc");
    assert_eq!(code, Some(0), "{err}");
    assert_eq!(err, "0+1 records in\n1+0 records out\n");
    assert_eq!(std::fs::read(&out).unwrap(), b"abc\0\0\0\0\0");

    // A repeated fsync is harmless; an unknown value beside it is not.
    let (_, err, code) = run_dd(&[&of, "conv=fsync,fsync"], b"x");
    assert_eq!(code, Some(0), "{err}");
    let (_, err, code) = run_dd(&[&of, "conv=bogus,fsync"], b"x");
    assert_eq!(code, Some(1), "{err}");
    assert!(err.contains("bogus"), "{err}");
}

/// GNU fsyncs standard output too, and an output that cannot be synced (a pipe
/// on Linux) is an error reported before the statistics, with status 1.  The
/// data has still been written.
#[cfg(target_os = "linux")]
#[test]
fn test_conv_fsync_pipe_fails() {
    let (stdout, err, code) = run_dd(&["bs=1", "count=1", "conv=fsync"], b"z");
    assert_eq!(code, Some(1), "{err}");
    assert_eq!(stdout, b"z");
    assert!(
        err.starts_with("dd: fsync failed for 'standard output': "),
        "{err}"
    );
    assert!(
        err.ends_with("\n1+0 records in\n1+0 records out\n"),
        "{err}"
    );

    let (_, err, code) = run_dd(&["of=/dev/null", "conv=fsync"], b"z");
    assert_eq!(code, Some(1), "{err}");
    assert!(
        err.starts_with("dd: fsync failed for '/dev/null': "),
        "{err}"
    );
    assert!(
        err.ends_with("\n0+1 records in\n0+1 records out\n"),
        "{err}"
    );
}

/// util-linux tests/ts/lsfd/error-eperm: `status=none` writes no statistics.
#[test]
fn test_status_none_silences_statistics() {
    let dir = plib::tmp::tempdir().unwrap();
    let out = dir.path().join("out");
    let of = format!("of={}", out.to_str().unwrap());
    let args = ["if=/dev/zero", &of, "bs=4096", "count=1", "status=none"];
    let (stdout, err, code) = run_dd(&args, b"");
    assert_eq!(code, Some(0), "{err}");
    assert_eq!(err, "");
    assert!(stdout.is_empty());
    assert_eq!(std::fs::read(&out).unwrap(), vec![0u8; 4096]);

    // Output to standard output is untouched.
    let (stdout, err, code) = run_dd(&["status=none"], b"xy");
    assert_eq!(code, Some(0), "{err}");
    assert_eq!(err, "");
    assert_eq!(stdout, b"xy");
}

/// `status=none` does not silence errors.
#[test]
fn test_status_none_still_reports_errors() {
    let (_, err, code) = run_dd(&["if=/nonexistent/x", "status=none"], b"");
    assert_eq!(code, Some(1), "{err}");
    assert!(!err.is_empty());
}

/// An unknown level is rejected before anything is copied.
#[test]
fn test_status_invalid_level() {
    for level in ["bogus", "", "progress", "noxfer", "none,none"] {
        let arg = format!("status={level}");
        let (stdout, err, code) = run_dd(&[&arg], b"xy");
        assert_eq!(code, Some(1), "{arg}: {err}");
        assert!(stdout.is_empty(), "{arg}");
        assert!(
            err.starts_with(&format!("dd: invalid status level: '{level}'\n")),
            "{arg}: {err}"
        );
    }
}
