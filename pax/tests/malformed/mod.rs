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
    let mut archive = Ustar {
        name: b"f",
        size: Some(4096),
        ..Default::default()
    }
    .header()
    .to_vec();
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

/// A header length field is attacker-controlled, so it must never size an
/// allocation. A single 512-byte block declaring a 4 GiB extended header used to
/// abort the process on the allocation alone, before reading any of it.
#[test]
fn test_malformed_huge_extended_header_size_is_rejected() {
    let archive = Ustar {
        name: b"PaxHeaders/1",
        typeflag: b'x',
        size: Some(0o37777777777), // 4294967295
        ..Default::default()
    }
    .header()
    .to_vec();

    let output = run_pax_with_stdin_bytes(&["-t"], &archive);

    assert!(
        !output.status.success(),
        "an unsatisfiable extended-header size should be rejected"
    );
    assert!(
        stderr_str(&output).contains("exceeds"),
        "expected a size-limit diagnostic, got: {}",
        stderr_str(&output)
    );
}

/// The same for cpio, whose name and symbolic-link-target lengths come straight
/// from the header: a 128-byte archive declaring a 2 GiB link target.
#[test]
fn test_malformed_huge_cpio_symlink_size_is_rejected() {
    // A symbolic link whose target is the member data: c_filesize declares
    // 2 GiB of it and the archive supplies none.
    let archive = CpioNewc {
        name: b"link",
        mode: 0o120777,
        filesize: Some(0x7FFF_FFF0),
        ..Default::default()
    }
    .member();

    let output = run_pax_with_stdin_bytes(&["-t", "-x", "cpio"], &archive);

    assert!(
        !output.status.success(),
        "an unsatisfiable symbolic-link target size should be rejected"
    );
    assert!(
        stderr_str(&output).contains("exceeds"),
        "expected a size-limit diagnostic, got: {}",
        stderr_str(&output)
    );
}

/// A record length near `usize::MAX` makes `pos + record_len` wrap. The wrapped
/// sum compares below `data.len()`, so the bounds check passes and the slice
/// built from it has a start beyond its end.
///
/// The wrap needs `pos > 0`, which is why every test above -- all of which put
/// their one malformed record first -- missed it. A valid record has to come
/// first for the offset to be non-zero at all.
#[test]
fn test_malformed_record_length_overflow_after_a_valid_record() {
    let mut records = pax_record("comment", b"first record is well-formed");
    records.extend_from_slice(b"18446744073709551615 path=x\n");
    let archive = archive_with_ext_records(&records);

    let output = run_pax_with_stdin_bytes(&[], &archive);
    assert!(
        !stderr_str(&output).contains("panicked"),
        "an overflowing record length must be diagnosed, not panic: {}",
        stderr_str(&output)
    );
    assert_exit_code(&output, 1, "list an overflowing extended-header record");
}

/// The same archive through the extraction path, which reaches the parser by a
/// different route and must not panic either.
#[test]
fn test_malformed_record_length_overflow_on_extract() {
    let temp = plib::tmp::TempDir::new().unwrap();
    let mut records = pax_record("comment", b"first record is well-formed");
    records.extend_from_slice(b"18446744073709551615 path=x\n");
    let archive = archive_with_ext_records(&records);

    let output = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, temp.path());
    assert!(
        !stderr_str(&output).contains("panicked"),
        "an overflowing record length must not panic on extract: {}",
        stderr_str(&output)
    );
    assert_exit_code(&output, 1, "extract an overflowing extended-header record");
}

/// And at the third record, so the fix cannot be "special-case record two".
#[test]
fn test_malformed_record_length_overflow_at_third_record() {
    let mut records = pax_record("comment", b"one");
    records.extend_from_slice(&pax_record("comment", b"two"));
    records.extend_from_slice(b"18446744073709551615 path=x\n");
    let archive = archive_with_ext_records(&records);

    let output = run_pax_with_stdin_bytes(&[], &archive);
    assert!(
        !stderr_str(&output).contains("panicked"),
        "record position must not matter: {}",
        stderr_str(&output)
    );
    assert_exit_code(&output, 1, "list a third-record overflow");
}

/// A `size=` keyword of 2^64-1 rounds up to zero blocks, after which the skip
/// length underflows and the reader treats the rest of the archive as member
/// data. It must be refused, and it must not panic in a checked build.
#[test]
fn test_malformed_pax_size_keyword_at_u64_max() {
    let records = pax_record("size", b"18446744073709551615");
    let archive = archive_with_ext_records(&records);

    let output = run_pax_with_stdin_bytes(&["-v"], &archive);
    assert!(
        !stderr_str(&output).contains("panicked"),
        "a u64::MAX size must not panic: {}",
        stderr_str(&output)
    );
    assert!(
        !output.status.success(),
        "a member declaring 2^64-1 bytes cannot be satisfied and must fail"
    );
}
