//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The s-file checksum.

/// The stored bytes the checksum covers: everything after the `^Ah` checksum
/// line. That is the first line once the blank lines the parser skips ahead
/// of it (`SccsParser::skip_whitespace`) are passed over; counting them as
/// the first line had the checksum cover the `^Ah` line itself.
fn checksummed_bytes(data: &[u8]) -> &[u8] {
    let start = data
        .iter()
        .position(|&b| b != b'\n' && b != b'\r')
        .unwrap_or(data.len());
    let data = &data[start..];
    let newline_pos = data.iter().position(|&b| b == b'\n').unwrap_or(data.len());
    data.get(newline_pos + 1..).unwrap_or(&[])
}

/// The checksum over an s-file's stored bytes, as [`compute_checksum`] sums
/// it: the value a rewrite stores, and the one to report as "computed".
///
/// `admin -h` and `val` each open-coded this, and guarded it differently:
/// `admin` with `content_start < data.len()`, `val` with `<=`. No s-file that
/// parses can tell the two apart — even a body-less file has a delta table
/// after the checksum line — but two spellings of one rule is how the next
/// difference becomes reachable.
pub fn stored_body_checksum(data: &[u8]) -> u16 {
    compute_checksum(checksummed_bytes(data))
}

/// Whether `stored`, the checksum an s-file's first line records, is right
/// for the file `data`.
///
/// Either sum is right: the signed one [`compute_checksum`] writes, which is
/// what historical SCCS and CSSC write and accept, and the unsigned one
/// earlier versions of these utilities wrote. The two differ only for a file
/// holding bytes above 0x7f.
pub fn stored_checksum_matches(data: &[u8], stored: u16) -> bool {
    let bytes = checksummed_bytes(data);
    let unsigned = bytes
        .iter()
        .fold(0u16, |s, &b| s.wrapping_add(u16::from(b)));
    compute_checksum(bytes) == stored || unsigned == stored
}

/// The s-file `data` with its first line replaced by the `^Ah` line for the
/// checksum of the rest, which is left byte for byte as it was: what
/// `admin -z` writes (POSIX 84111).
pub fn with_recomputed_checksum(data: &[u8]) -> Vec<u8> {
    let rest = checksummed_bytes(data);
    let mut out = format!("\x01h{:05}\n", compute_checksum(rest)).into_bytes();
    out.extend_from_slice(rest);
    out
}

/// Compute the SCCS checksum: the sum, modulo 65536, of every byte taken as
/// a signed char.
///
/// Historical SCCS summed `char`s, which are signed where it ran, and CSSC
/// keeps that, so a byte above 0x7f counts below zero. Summing them unsigned
/// gave a file CSSC rejected as corrupt as soon as it held one such byte --
/// and rejected CSSC's own files the same way.
pub fn compute_checksum(data: &[u8]) -> u16 {
    data.iter().fold(0u16, |sum, &byte| {
        sum.wrapping_add(byte as i8 as i16 as u16)
    })
}
