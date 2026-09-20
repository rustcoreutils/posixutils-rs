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

// ---------------------------------------------------------------------------
// Input that used to be accepted in silence. A missing feature that fails
// loudly is a missing feature; one that fails quietly is a defect, because the
// archive looks like it came back whole.
// ---------------------------------------------------------------------------

/// POSIX: "If conversion to a regular file occurs, the pax utility shall
/// produce an error indicating that the conversion took place." Every
/// unrecognized typeflag became a regular file with no diagnostic and a zero
/// exit status.
#[test]
fn test_unknown_typeflag_is_diagnosed() {
    let archive = Ustar {
        name: b"weird",
        typeflag: b'Q',
        body: b"data\n",
        ..Default::default()
    }
    .archive();

    let output = run_pax_with_stdin_bytes(&[], &archive);
    assert!(
        stderr_str(&output).contains("unrecognized type 'Q'"),
        "an unknown typeflag must be named: {}",
        stderr_str(&output)
    );
    assert_failure(&output, "list a member of unknown type");
}

/// A typeflag that is not a printable character must not reach the terminal
/// raw -- writing it out is how an archive gets to send escape sequences to
/// whoever listed it.
#[test]
fn test_unprintable_typeflag_is_escaped_in_the_diagnostic() {
    let archive = Ustar {
        name: b"esc",
        typeflag: 0x1b,
        body: b"data\n",
        ..Default::default()
    }
    .archive();

    let output = run_pax_with_stdin_bytes(&[], &archive);
    assert!(
        stderr_str(&output).contains(r"'\033'"),
        "an unprintable typeflag must be shown as an escape: {}",
        stderr_str(&output)
    );
    assert!(
        !output.stderr.contains(&0x1b),
        "the raw escape byte must not be written to the terminal"
    );
}

/// Typeflag 7 is the exception: POSIX defines it as a regular file for any
/// implementation without the high-performance extension, so nothing was lost
/// and nothing is diagnosed.
#[test]
fn test_contiguous_typeflag_is_not_diagnosed() {
    let archive = Ustar {
        name: b"contig",
        typeflag: b'7',
        body: b"data\n",
        ..Default::default()
    }
    .archive();

    let output = run_pax_with_stdin_bytes(&[], &archive);
    assert_success(&output, "list a typeflag 7 member");
    assert_eq!(stderr_str(&output), "");
}

/// A GNU sparse member extracted as a regular file gets its sparse map as
/// contents, and a volume label becomes a file named after the label. Both
/// look like success, so both are named specifically rather than being folded
/// into the generic unknown-type message.
#[test]
fn test_gnu_extensions_are_named_in_the_diagnostic() {
    for (typeflag, expected) in [(b'S', "GNU sparse file"), (b'V', "GNU volume label")] {
        let archive = Ustar {
            name: b"m",
            typeflag,
            body: b"data\n",
            ..Default::default()
        }
        .archive();

        let output = run_pax_with_stdin_bytes(&[], &archive);
        assert!(
            stderr_str(&output).contains(expected),
            "typeflag {} should be named as {expected}: {}",
            typeflag as char,
            stderr_str(&output)
        );
    }
}

/// A GNU `L` record carries the real name of the member that follows, whose
/// own header holds a name truncated to 100 bytes. Treating `L` as an unknown
/// type created a file called `@LongLink` *and* extracted the member under its
/// truncated name -- two wrong files, silently. Both are now skipped, and the
/// diagnostic reports the name the archive actually meant.
#[test]
fn test_gnu_long_name_record_skips_the_member_it_describes() {
    let long = [b"d/".as_slice(), &[b'x'; 200], b"/deep.txt"].concat();
    let mut body = long.clone();
    body.push(0);

    let mut archive = Ustar {
        name: b"././@LongLink",
        typeflag: b'L',
        body: &body,
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &Ustar {
            name: &long[..100],
            body: b"payload\n",
            ..Default::default()
        }
        .member(),
    );
    archive.extend_from_slice(&ustar_trailer());

    let temp = plib::tmp::TempDir::new().unwrap();
    let output = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, temp.path());

    assert!(
        stderr_str(&output).contains("GNU long name record"),
        "the unsupported extension must be named: {}",
        stderr_str(&output)
    );
    assert!(
        stderr_str(&output).contains("deep.txt"),
        "the diagnostic should report the name the archive meant: {}",
        stderr_str(&output)
    );
    assert!(
        !temp.path().join("@LongLink").exists(),
        "the long-name record must not be extracted as a file of its own"
    );
    assert!(
        std::fs::read_dir(temp.path()).unwrap().next().is_none(),
        "nothing at all should have been created"
    );
    assert_failure(&output, "extract an archive using GNU long names");
}

/// `c_namesize` counts the NUL terminator, so zero is malformed. GNU cpio
/// says "file name of zero length"; pax listed a blank line and exited zero.
#[test]
fn test_cpio_zero_length_name_is_diagnosed() {
    let archive = CpioNewc {
        namesize: Some(0),
        ..Default::default()
    }
    .member();

    let output = run_pax_with_stdin_bytes(&["-x", "cpio"], &archive);
    assert!(
        stderr_str(&output).contains("zero length"),
        "a zero-length cpio name must be diagnosed: {}",
        stderr_str(&output)
    );
    assert_failure(&output, "list a cpio member with no name");
}

/// A name field whose declared size leaves no room for the terminator is
/// equally malformed, and GNU cpio equally diagnoses it.
#[test]
fn test_cpio_unterminated_name_is_diagnosed() {
    let archive = CpioNewc {
        name: b"abcd",
        namesize: Some(4),
        ..Default::default()
    }
    .member();

    let output = run_pax_with_stdin_bytes(&["-x", "cpio"], &archive);
    assert!(
        stderr_str(&output).contains("NUL-terminated"),
        "an unterminated cpio name must be diagnosed: {}",
        stderr_str(&output)
    );
    assert_failure(&output, "list a cpio member with an unterminated name");
}

/// A member whose name resolves to nothing below the extraction directory is
/// dropped -- correctly -- but dropping it in silence made an archive that
/// extracted nothing look like one that extracted everything.
#[test]
fn test_member_naming_no_file_is_diagnosed() {
    let temp = plib::tmp::TempDir::new().unwrap();
    let archive = Ustar {
        name: b"..",
        typeflag: b'5',
        ..Default::default()
    }
    .archive();

    let output = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, temp.path());
    assert!(
        stderr_str(&output).contains("names no file to create"),
        "a member that creates nothing must say so: {}",
        stderr_str(&output)
    );
    assert_failure(&output, "extract a member named `..`");
}

/// But `.` is ordinary: every archive built with `pax -w .` carries one, and
/// it must not be diagnosed or the common case fails.
#[test]
fn test_current_directory_member_is_not_diagnosed() {
    let temp = plib::tmp::TempDir::new().unwrap();
    let mut archive = Ustar {
        name: b".",
        typeflag: b'5',
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &Ustar {
            name: b"./a.txt",
            body: b"x\n",
            ..Default::default()
        }
        .member(),
    );
    archive.extend_from_slice(&ustar_trailer());

    let output = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, temp.path());
    assert_success(&output, "extract an archive containing a `.` member");
    assert_eq!(
        std::fs::read_to_string(temp.path().join("a.txt")).unwrap(),
        "x\n"
    );
}

/// A member can be preceded by *more than one* long-name record: GNU tar
/// writes `K` (long link target) and then `L` (long name) when a member has
/// both. Consuming only one mistakes the second record's header for the member
/// and then reads the real member header as an ordinary one -- restoring it
/// under exactly the truncated 100-byte name the skip exists to avoid, while
/// the diagnostic claims the member was skipped.
#[test]
fn test_paired_gnu_long_name_records_skip_the_whole_group() {
    let long_name = [b"n".as_slice(), &[b'd'; 121]].concat();
    let long_target = [b"t".as_slice(), &[b't'; 137]].concat();

    let with_nul = |v: &[u8]| {
        let mut b = v.to_vec();
        b.push(0);
        b
    };
    let name_body = with_nul(&long_name);
    let target_body = with_nul(&long_target);

    // K, then L, then the member -- the order GNU tar emits.
    let mut archive = Ustar {
        name: b"././@LongLink",
        typeflag: b'K',
        body: &target_body,
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &Ustar {
            name: b"././@LongLink",
            typeflag: b'L',
            body: &name_body,
            ..Default::default()
        }
        .member(),
    );
    archive.extend_from_slice(
        &Ustar {
            name: &long_name[..100],
            typeflag: b'2',
            linkname: &long_target[..100],
            ..Default::default()
        }
        .member(),
    );
    // A following member proves the reader resynchronised on the right block.
    archive.extend_from_slice(
        &Ustar {
            name: b"after.txt",
            body: b"still here\n",
            ..Default::default()
        }
        .member(),
    );
    archive.extend_from_slice(&ustar_trailer());

    let temp = plib::tmp::TempDir::new().unwrap();
    let output = run_pax_with_stdin_bytes_in_dir(&["-r", "-v"], &archive, temp.path());

    let created: Vec<_> = std::fs::read_dir(temp.path())
        .unwrap()
        .map(|e| e.unwrap().file_name())
        .collect();
    assert_eq!(
        created.len(),
        1,
        "only the member after the group should be created, got {created:?}"
    );
    assert_eq!(created[0], std::ffi::OsStr::new("after.txt"));

    let err = stderr_str(&output);
    assert!(
        err.contains("long link target") && err.contains("long name"),
        "both records should be named: {err}"
    );
    // The diagnostic must name the member, which is what the `L` record holds
    // -- not the link target, which is what a lone `K` would leave in hand.
    assert!(
        err.contains(std::str::from_utf8(&long_name).unwrap()),
        "the diagnostic should report the member's real name: {err}"
    );
}
