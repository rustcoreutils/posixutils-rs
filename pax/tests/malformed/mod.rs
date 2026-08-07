//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Deliberately corrupt archives. A malformed archive is untrusted input: pax
//! must diagnose it and exit non-zero, never panic. Nothing else in the suite
//! feeds pax bad bytes.

use crate::common::*;

const BLOCK: usize = 512;

/// Build a 512-byte ustar header block with a correct checksum.
fn ustar_header(name: &str, size: usize, typeflag: u8) -> [u8; BLOCK] {
    let mut h = [0u8; BLOCK];
    h[..name.len()].copy_from_slice(name.as_bytes());
    h[100..108].copy_from_slice(b"0000644\0"); // mode
    h[108..116].copy_from_slice(b"0000000\0"); // uid
    h[116..124].copy_from_slice(b"0000000\0"); // gid
    h[124..136].copy_from_slice(format!("{:011o}\0", size).as_bytes());
    h[136..148].copy_from_slice(b"00000000000\0"); // mtime
    h[148..156].copy_from_slice(b"        "); // checksum field is spaces while summing
    h[156] = typeflag;
    h[257..263].copy_from_slice(b"ustar\0");
    h[263..265].copy_from_slice(b"00");

    let sum: u32 = h.iter().map(|&b| b as u32).sum();
    h[148..156].copy_from_slice(format!("{:06o}\0 ", sum).as_bytes());
    h
}

fn pad_to_block(data: &mut Vec<u8>) {
    let rem = data.len() % BLOCK;
    if rem != 0 {
        data.resize(data.len() + (BLOCK - rem), 0);
    }
}

/// A pax archive whose single member is preceded by an `x` extended header
/// carrying exactly `records` as its data.
fn archive_with_ext_records(records: &[u8]) -> Vec<u8> {
    let mut a = Vec::new();
    a.extend_from_slice(&ustar_header("PaxHeaders/f", records.len(), b'x'));
    let mut data = records.to_vec();
    pad_to_block(&mut data);
    a.extend_from_slice(&data);
    a.extend_from_slice(&ustar_header("f", 0, b'0'));
    a.extend_from_slice(&[0u8; 2 * BLOCK]); // end-of-archive
    a
}

/// A record length field of `0` makes `pos + record_len - 1` underflow. In a
/// debug build that is an overflow panic; in release it wraps to `usize::MAX`,
/// the `record_end <= record_start` guard passes, and the subsequent slice
/// index aborts the process.
#[test]
fn test_malformed_zero_length_extended_record() {
    let archive = archive_with_ext_records(b"0 path=x\n");
    let output = run_pax_with_stdin_bytes(&[], &archive);

    assert!(
        !stderr_str(&output).contains("panicked"),
        "a zero-length record must be diagnosed, not panic: {}",
        stderr_str(&output)
    );
    assert_exit_code(&output, 1, "list a zero-length extended-header record");
}

/// A record length that runs past the end of the header data must also be
/// rejected rather than indexing out of bounds.
#[test]
fn test_malformed_overlong_extended_record() {
    let archive = archive_with_ext_records(b"9999 path=x\n");
    let output = run_pax_with_stdin_bytes(&[], &archive);

    assert!(
        !stderr_str(&output).contains("panicked"),
        "an overlong record must be diagnosed, not panic: {}",
        stderr_str(&output)
    );
    assert_exit_code(&output, 1, "list an overlong extended-header record");
}

/// A record length shorter than its own length field is the boundary case just
/// above zero; it is already handled, and must stay handled.
#[test]
fn test_malformed_undersized_extended_record() {
    let archive = archive_with_ext_records(b"1 path=x\n");
    let output = run_pax_with_stdin_bytes(&[], &archive);

    assert!(
        !stderr_str(&output).contains("panicked"),
        "an undersized record must not panic: {}",
        stderr_str(&output)
    );
}

/// A header that claims more member data than the archive contains.
#[test]
fn test_malformed_truncated_member_data() {
    let mut archive = Vec::new();
    archive.extend_from_slice(&ustar_header("f", 4096, b'0'));
    archive.extend_from_slice(&[b'x'; BLOCK]); // far short of 4096

    let output = run_pax_with_stdin_bytes(&["-v"], &archive);
    assert!(
        !stderr_str(&output).contains("panicked"),
        "a truncated member must not panic: {}",
        stderr_str(&output)
    );
}

/// Garbage where a header block belongs: not any known format's magic.
#[test]
fn test_malformed_unrecognized_leading_block() {
    let output = run_pax_with_stdin_bytes(&[], &[0xffu8; BLOCK]);
    assert!(
        !stderr_str(&output).contains("panicked"),
        "unrecognized input must not panic: {}",
        stderr_str(&output)
    );
    assert_failure(&output, "list an unrecognized archive");
}
