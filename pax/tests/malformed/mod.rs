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

/// POSIX ustar: for typeflags 3, 4 and 6 "no data logical records shall be
/// stored on the medium. Additionally, for type 6, the size field shall be
/// ignored when reading." A FIFO header whose size field says 1024 must not
/// swallow the member after it as its data.
#[test]
fn test_fifo_size_field_is_ignored() {
    let mut a = Ustar {
        name: b"fifo",
        typeflag: b'6',
        size: Some(1024),
        ..Default::default()
    }
    .header()
    .to_vec();
    a.extend_from_slice(
        &Ustar {
            name: b"hidden.txt",
            body: b"HIDDEN\n",
            ..Default::default()
        }
        .member(),
    );
    a.extend_from_slice(
        &Ustar {
            name: b"visible.txt",
            body: b"VISIBLE\n",
            ..Default::default()
        }
        .archive(),
    );

    let output = run_pax_with_stdin_bytes(&[], &a);
    assert_success(&output, "list");
    assert_eq!(stdout_str(&output), "fifo\nhidden.txt\nvisible.txt\n");
}

/// The same for character and block special files, whose headers carry no
/// data either.
#[test]
fn test_device_size_field_carries_no_data() {
    for typeflag in *b"34" {
        let mut a = Ustar {
            name: b"dev",
            typeflag,
            size: Some(512),
            ..Default::default()
        }
        .header()
        .to_vec();
        a.extend_from_slice(
            &Ustar {
                name: b"next.txt",
                body: b"NEXT\n",
                ..Default::default()
            }
            .archive(),
        );

        let output = run_pax_with_stdin_bytes(&[], &a);
        assert_success(&output, "list");
        assert_eq!(
            stdout_str(&output),
            "dev\nnext.txt\n",
            "typeflag {}",
            typeflag as char
        );
    }
}

/// An archive cut off inside a header block -- an interrupted download -- is
/// not a clean end of archive. The members before the cut are still listed,
/// but pax must say the archive is truncated and exit non-zero, as bsdtar and
/// BSD pax do, rather than silently report a shorter archive.
#[test]
fn test_ustar_truncated_inside_header_is_an_error() {
    let mut a = Ustar {
        name: b"one",
        body: b"ONE\n",
        ..Default::default()
    }
    .member();
    let cut = a.len() + 300;
    a.extend_from_slice(
        &Ustar {
            name: b"two",
            body: b"TWO\n",
            ..Default::default()
        }
        .archive(),
    );
    a.truncate(cut);

    let output = run_pax_with_stdin_bytes(&[], &a);
    assert_exit_code(&output, 1, "list of an archive cut inside a header");
    assert_eq!(stdout_str(&output), "one\n");
}

/// The cpio form: a stray partial header after the last whole member.
#[test]
fn test_cpio_truncated_inside_header_is_an_error() {
    let mut a = CpioNewc {
        name: b"one",
        body: b"ONE\n",
        ..Default::default()
    }
    .member();
    a.push(b'0');

    let output = run_pax_with_stdin_bytes(&[], &a);
    assert_exit_code(&output, 1, "list of a cpio archive cut inside a header");
    assert_eq!(stdout_str(&output), "one\n");
}

/// A fatal error part-way through still leaves the directories already
/// extracted with their archived attributes: the deferred pass is what gives
/// a directory its mode and times, and skipping it on the error path left
/// them with the creation mode and the time of extraction.
#[test]
fn test_fatal_read_error_still_applies_directory_attributes() {
    let temp = plib::tmp::TempDir::new().unwrap();
    let mut archive = Ustar {
        name: b"d/",
        typeflag: b'5',
        mode: 0o750,
        mtime: 1_000_000_000,
        ..Default::default()
    }
    .member();
    // A member whose header promises far more data than follows: reading it
    // runs out of archive, which ends the extraction.
    archive.extend_from_slice(
        &Ustar {
            name: b"d/f",
            body: b"x",
            size: Some(100_000),
            ..Default::default()
        }
        .member(),
    );

    let out = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, temp.path());
    assert!(!out.status.success(), "a truncated archive must fail");
    let meta = std::fs::metadata(temp.path().join("d")).unwrap();
    use std::os::unix::fs::MetadataExt;
    assert_eq!(meta.mtime(), 1_000_000_000, "{}", stderr_str(&out));
}

/// An extended header member: an `x` or `g` header whose data is `records`.
fn ext_header(typeflag: u8, records: &[u8]) -> Vec<u8> {
    Ustar {
        name: b"PaxHeaders/h",
        typeflag,
        body: records,
        ..Default::default()
    }
    .member()
}

/// A GNU `L` long-name record carrying `name`.
fn gnu_long_name(name: &[u8]) -> Vec<u8> {
    let body = [name, b"\0"].concat();
    Ustar {
        name: b"././@LongLink",
        typeflag: b'L',
        body: &body,
        ..Default::default()
    }
    .member()
}

/// Each `g` header is capped, but every one is layered over the global values
/// already in force, so a run of them naming distinct keywords piled up
/// without limit: sixteen 16 MiB headers held 256 MiB. The merged global set
/// is capped as a whole.
#[test]
fn test_global_headers_are_capped_in_total() {
    let temp = plib::tmp::TempDir::new().unwrap();
    let value = vec![b'v'; 33 * 1024 * 1024];
    let mut archive = ext_header(b'g', &pax_record("one", &value));
    archive.extend_from_slice(&ext_header(b'g', &pax_record("two", &value)));
    archive.extend_from_slice(
        &Ustar {
            name: b"f",
            ..Default::default()
        }
        .archive(),
    );
    let path = temp.path().join("g.tar");
    std::fs::write(&path, &archive).unwrap();

    let output = run_pax(&["-f", path.to_str().unwrap()]);
    assert_exit_code(
        &output,
        1,
        "list an archive whose global records exceed the cap",
    );
    assert!(
        stderr_str(&output).contains("global extended header"),
        "the cap must be named: {}",
        stderr_str(&output)
    );
}

/// POSIX: a record is "%d %s=%s\n", and its length is a decimal number. A
/// record whose last byte is not the <newline>, or whose length has a sign,
/// is malformed -- reading it anyway dropped the value's last byte.
#[test]
fn test_malformed_record_framing_is_rejected() {
    for records in [b"11 path=abc".as_slice(), b"+13 path=abc\n"] {
        let output = run_pax_with_stdin_bytes(&[], &archive_with_ext_records(records));
        assert_exit_code(&output, 1, "list a malformed extended header record");
        assert!(
            !stdout_str(&output).contains("ab"),
            "{:?} must not name the member: {}",
            String::from_utf8_lossy(records),
            stdout_str(&output)
        );
    }
}

/// A numeric record value is decimal digits (a time may be negative), and
/// Rust's `parse` also takes a leading '+'. bsdtar does not, so `size=+5`
/// framed the member one way for pax and another for bsdtar -- a member one
/// tool sees and the other does not.
#[test]
fn test_numeric_record_with_a_plus_sign_is_rejected() {
    for (keyword, value) in [
        ("size", b"+5".as_slice()),
        ("uid", b"+1"),
        ("gid", b"+1"),
        ("mtime", b"+1"),
        ("atime", b"+1.5"),
    ] {
        let mut archive = ext_header(b'x', &pax_record(keyword, value));
        archive.extend_from_slice(
            &Ustar {
                name: b"f",
                body: b"hello",
                ..Default::default()
            }
            .archive(),
        );
        let output = run_pax_with_stdin_bytes(&[], &archive);
        assert_exit_code(&output, 1, &format!("list {keyword}=+"));
        let value = std::str::from_utf8(value).unwrap();
        assert!(
            stderr_str(&output).contains(&format!("invalid {keyword}: {value}"))
                || stderr_str(&output).contains(&format!("invalid pax time: {value}")),
            "the diagnostic must name {keyword}={value}: {}",
            stderr_str(&output)
        );
    }
}

/// The same for a newc header's hexadecimal fields: `from_str_radix` takes a
/// leading '+', GNU and BSD cpio do not.
#[test]
fn test_newc_hex_field_with_a_plus_sign_is_rejected() {
    let mut archive = CpioNewc {
        name: b"f",
        body: b"hello",
        ..Default::default()
    }
    .archive();
    // c_filesize: the seventh eight-digit field after the six-byte magic.
    let filesize = 6 + 6 * 8;
    archive[filesize..filesize + 8].copy_from_slice(b"+0000005");

    let output = run_pax_with_stdin_bytes(&["-x", "cpio"], &archive);
    assert_exit_code(&output, 1, "list a newc header with a signed field");
    assert!(
        !stdout_str(&output).contains('f'),
        "the member must not be listed: {}",
        stdout_str(&output)
    );
}

/// An `x` header whose `size=` record describes a member that a GNU `L`
/// record also describes: the member is skipped, and it has to be skipped by
/// the size the `x` header gave it, or its data is read as headers.
#[test]
fn test_extended_size_applies_across_a_gnu_long_name_group() {
    let long = [b'L'; 150];
    let mut archive = ext_header(b'x', &pax_record("size", b"1024"));
    archive.extend_from_slice(&gnu_long_name(&long));
    archive.extend_from_slice(
        &Ustar {
            name: &long[..100],
            size: Some(0),
            ..Default::default()
        }
        .header(),
    );
    archive.extend_from_slice(&[b'B'; 1024]);
    archive.extend_from_slice(
        &Ustar {
            name: b"after",
            body: b"after\n",
            ..Default::default()
        }
        .archive(),
    );

    let output = run_pax_with_stdin_bytes(&[], &archive);
    assert_eq!(stdout_str(&output), "after\n", "{}", stderr_str(&output));
    assert!(
        !stderr_str(&output).contains("checksum"),
        "the member's data must be skipped, not read as a header: {}",
        stderr_str(&output)
    );
}

/// An `x` header between a GNU `L` record and the member they both describe
/// is not the member. Taking it for one skipped the `x` header's data and
/// then read the real member under its truncated 100-byte name -- exactly
/// the name the skip is there to avoid. An `x` header that names the member
/// itself makes the `L` record moot, and the member is read under that name.
#[test]
fn test_extended_header_after_a_gnu_long_name_record() {
    let long = [b'L'; 150];
    let build = |records: &[u8]| {
        let mut archive = gnu_long_name(&long);
        archive.extend_from_slice(&ext_header(b'x', records));
        archive.extend_from_slice(
            &Ustar {
                name: &long[..100],
                body: b"data\n",
                ..Default::default()
            }
            .member(),
        );
        archive.extend_from_slice(
            &Ustar {
                name: b"after",
                body: b"after\n",
                ..Default::default()
            }
            .archive(),
        );
        archive
    };

    let output = run_pax_with_stdin_bytes(&[], &build(&pax_record("uname", b"u")));
    assert_eq!(stdout_str(&output), "after\n", "{}", stderr_str(&output));
    assert!(stderr_str(&output).contains("GNU long name record"));

    let output = run_pax_with_stdin_bytes(&[], &build(&pax_record("path", b"named")));
    assert_success(&output, "list a member an x header names over an L record");
    assert_eq!(stdout_str(&output), "named\nafter\n");
}

/// An old GNU header (magic "ustar  \0") has no prefix field: those bytes
/// hold the access and change times. Joining them onto the name made
/// `dir/file` into `14524770401/dir/file`.
#[test]
fn test_old_gnu_header_has_no_prefix_field() {
    let mut archive = Ustar {
        name: b"dir/file",
        body: b"hello",
        ..Default::default()
    }
    .archive();
    archive[257..265].copy_from_slice(b"ustar  \0");
    archive[345..357].copy_from_slice(b"14524770401\0");
    archive[357..369].copy_from_slice(b"14524770402\0");
    reseal_header(&mut archive);

    let output = run_pax_with_stdin_bytes(&[], &archive);
    assert_success(&output, "list an old GNU archive");
    assert_eq!(stdout_str(&output), "dir/file\n");
}

/// A base-256 id or mode wider than 32 bits has no value pax can give the
/// file. Truncating it made a uid of 2^32 into 0 -- root, on a setuid file.
#[test]
fn test_base256_field_beyond_32_bits_is_refused() {
    for offset in [100, 108, 116] {
        let mut archive = Ustar {
            name: b"wide",
            body: b"x",
            mode: 0o4755,
            ..Default::default()
        }
        .archive();
        archive[offset] = 0x80;
        archive[offset + 1..offset + 8].copy_from_slice(&(1u64 << 32).to_be_bytes()[1..]);
        reseal_header(&mut archive);

        let output = run_pax_with_stdin_bytes(&["-v"], &archive);
        assert_exit_code(&output, 1, "list a header with a 33-bit field");
        assert!(
            !stdout_str(&output).contains("wide"),
            "field at {offset} must not be truncated: {}",
            stdout_str(&output)
        );
    }
}

/// Historical writers pad numeric fields with leading spaces, and sum the
/// checksum over signed bytes. Format detection already accepted both; the
/// header parser then refused the archive it had just detected.
#[test]
fn test_historical_numeric_fields_are_accepted() {
    let mut spaced = Ustar {
        name: b"lead",
        body: b"hello",
        ..Default::default()
    }
    .archive();
    spaced[100..108].copy_from_slice(b"   644 \0");
    spaced[124..136].copy_from_slice(b"          5 ");
    // The checksum field itself in the "%6o" form, leading spaces included.
    let sum: u32 = {
        let mut h = spaced[..BLOCK].to_vec();
        h[148..156].copy_from_slice(b"        ");
        h.iter().map(|&b| b as u32).sum()
    };
    spaced[148..156].copy_from_slice(format!("{sum:6o}\0 ").as_bytes());

    let output = run_pax_with_stdin_bytes(&["-v"], &spaced);
    assert_success(&output, "list a header with space-padded numbers");
    assert!(stdout_str(&output).contains("-rw-r--r--"));
    assert!(stdout_str(&output).contains("lead"));

    let mut signed = Ustar {
        name: b"caf\xe9",
        body: b"x",
        ..Default::default()
    }
    .archive();
    signed[148..156].copy_from_slice(b"        ");
    let sum: i32 = signed[..BLOCK].iter().map(|&b| b as i8 as i32).sum();
    signed[148..156].copy_from_slice(format!("{sum:06o}\0 ").as_bytes());

    let output = run_pax_with_stdin_bytes(&[], &signed);
    assert_success(&output, "list a header with a signed checksum");
    assert_eq!(output.stdout, b"caf\xe9\n");
}

/// Before typeflag 5, a directory was a regular-file header whose name ends
/// in a slash, and GNU and BSD tar still read it so. Extracting it as a file
/// named `olddir` left nowhere to put `olddir/f`.
#[test]
fn test_regular_typeflag_with_trailing_slash_is_a_directory() {
    let temp = plib::tmp::TempDir::new().unwrap();
    let mut archive = Ustar {
        name: b"olddir/",
        mode: 0o755,
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &Ustar {
            name: b"olddir/f",
            body: b"hi",
            ..Default::default()
        }
        .archive(),
    );

    let output = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, temp.path());
    assert_success(&output, "extract an old-style directory");
    assert!(temp.path().join("olddir").is_dir());
    assert_eq!(
        std::fs::read_to_string(temp.path().join("olddir/f")).unwrap(),
        "hi"
    );
}

/// The old-style directory rule is about the member, not its header block: a
/// `size` record replaces the size field before the rule looks at it. A header
/// named `x/` with an empty size field but `size=1024` is a 1024-byte file
/// named by its `path` record, and those bytes are its data. Deciding on the
/// raw fields made it an empty directory and read the data as further members
/// -- a member bsdtar extracts as file contents, smuggled in as a file of its
/// own.
#[test]
fn test_size_record_overrides_old_style_directory() {
    let mut records = pax_record("path", b"file.bin");
    records.extend_from_slice(&pax_record("size", b"1024"));
    let mut archive = ext_header(b'x', &records);
    archive.extend_from_slice(
        &Ustar {
            name: b"x/",
            size: Some(0),
            ..Default::default()
        }
        .header(),
    );
    let mut data = Ustar {
        name: b"smuggled",
        body: b"payload",
        ..Default::default()
    }
    .member();
    data.resize(1024, 0);
    archive.extend_from_slice(&data);
    archive.extend_from_slice(&ustar_trailer());

    let output = run_pax_with_stdin_bytes(&["-o", "listopt=%(size)d %F"], &archive);
    assert_success(&output, "list a member sized by its size record");
    assert_eq!(stdout_str(&output), "1024 file.bin\n");
}

/// The rule looks at the member's final name too. Python's tarfile and pax
/// itself put the first 100 bytes of a long name in the name field and the
/// whole name in a `path` record, and those 100 bytes can end in a slash. An
/// empty file at such a path was extracted as a directory; bsdtar extracts
/// the file.
#[test]
fn test_path_record_overrides_old_style_directory() {
    let temp = plib::tmp::TempDir::new().unwrap();
    let mut archive = archive_with_ext_records(&pax_record("path", b"pkg/__init__.py"));
    // `archive_with_ext_records` names the member `f`; give it the truncated
    // spelling instead, which ends in a slash.
    let member = 2 * BLOCK;
    archive[member..member + 100].fill(0);
    archive[member..member + 4].copy_from_slice(b"pkg/");
    reseal_header(&mut archive[member..]);

    let output = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, temp.path());
    assert_success(&output, "extract a member named by its path record");
    let extracted = temp.path().join("pkg/__init__.py");
    assert!(
        extracted.is_file(),
        "pkg/__init__.py was not a regular file"
    );

    // A `path` record that itself ends in a slash is still a directory.
    let archive = archive_with_ext_records(&pax_record("path", b"olddir/"));
    let output = run_pax_with_stdin_bytes_in_dir(&["-r"], &archive, temp.path());
    assert_success(&output, "extract an old-style directory named by a record");
    assert!(temp.path().join("olddir").is_dir());
}

/// pax's own writer: an empty file whose name needs a `path` record, and
/// whose name field is cut where the 100th byte is a slash, must read back
/// as the file it was.
#[test]
fn test_long_name_cut_at_a_slash_round_trips() {
    let temp = plib::tmp::TempDir::new().unwrap();
    let dir = "a".repeat(99);
    let file = format!("{dir}/{}", "b".repeat(200));
    std::fs::create_dir(temp.path().join(&dir)).unwrap();
    std::fs::write(temp.path().join(&file), "").unwrap();

    let output = run_pax_in_dir(&["-w", "-x", "pax", &file], temp.path());
    assert_success(&output, "archive a long name");
    // The name field is a fallback for a reader without extended headers;
    // it must not spell a directory for a file.
    let header = &output.stdout[2 * BLOCK..3 * BLOCK];
    assert_ne!(header[99], b'/', "name field of a file ends in a slash");

    let out = temp.path().join("out");
    std::fs::create_dir(&out).unwrap();
    let read = run_pax_with_stdin_bytes_in_dir(&["-r"], &output.stdout, &out);
    assert_success(&read, "extract the long name");
    assert!(out.join(&file).is_file(), "{file} was not a regular file");
}

/// An archive cut off inside the second block of its end-of-archive indicator
/// lost nothing: the first zero block already ended it. bsdtar reads it.
#[test]
fn test_archive_cut_inside_the_end_indicator_is_complete() {
    let mut archive = Ustar {
        name: b"f",
        body: b"x\n",
        ..Default::default()
    }
    .archive();
    archive.truncate(archive.len() - BLOCK + 100);

    let output = run_pax_with_stdin_bytes(&[], &archive);
    assert_success(&output, "list an archive cut inside its end indicator");
    assert_eq!(stdout_str(&output), "f\n");
}

/// A plain ustar archive's hard link carries no data, whatever its size field
/// says -- the pre-POSIX convention repeated the linked file's size there.
/// Only a pax archive may give typeflag 1 data, so reading every archive with
/// the ustar magic by the pax rule swallowed the members after such a link.
#[test]
fn test_ustar_hard_link_size_does_not_swallow_members() {
    let mut archive = Ustar {
        name: b"a",
        body: b"hello",
        ..Default::default()
    }
    .member();
    archive.extend_from_slice(
        &Ustar {
            name: b"b",
            typeflag: b'1',
            linkname: b"a",
            size: Some(5),
            ..Default::default()
        }
        .header(),
    );
    archive.extend_from_slice(
        &Ustar {
            name: b"c",
            body: b"see",
            ..Default::default()
        }
        .archive(),
    );

    let output = run_pax_with_stdin_bytes(&[], &archive);
    assert_eq!(stdout_str(&output), "a\nb\nc\n", "{}", stderr_str(&output));
}
