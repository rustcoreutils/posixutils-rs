//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::*;
use std::collections::HashSet;
use std::path::PathBuf;
use std::str::FromStr;

#[test]
fn test_sid_parse() {
    assert_eq!("1.1".parse::<Sid>().unwrap(), Sid::trunk(1, 1));
    assert_eq!("1.2.3.4".parse::<Sid>().unwrap(), Sid::new(1, 2, 3, 4));
    assert_eq!("2".parse::<Sid>().unwrap(), Sid::new(2, 0, 0, 0));
}

#[test]
fn test_uuencode_decode_roundtrip() {
    // Exact CSSC encoding for this input (space-for-zero uuencode).
    let data = b"\x00\x01hello";
    let enc = uuencode_sccs(data);
    assert_eq!(enc[0], b"'  %H96QL;P  ");
    assert_eq!(enc[1], b" "); // count-0 terminator
    assert_eq!(uudecode_sccs(&enc), data);

    // Round-trip a range of binary payloads of varying lengths.
    for len in [0usize, 1, 2, 3, 44, 45, 46, 90, 137] {
        let payload: Vec<u8> = (0..len).map(|i| (i * 31 + 7) as u8).collect();
        let lines = uuencode_sccs(&payload);
        assert_eq!(uudecode_sccs(&lines), payload, "len={}", len);
    }
}

#[test]
fn test_prs_fl_line_canonical() {
    assert_eq!(
        SccsFlag::BranchEnabled.prs_fl_line().as_deref(),
        Some(&b"branch"[..])
    );
    assert_eq!(
        SccsFlag::ModuleName("mod".into()).prs_fl_line().as_deref(),
        Some(&b"module\tmod"[..])
    );
    assert_eq!(
        SccsFlag::ModuleType("ty".into()).prs_fl_line().as_deref(),
        Some(&b"type\tty"[..])
    );
    assert_eq!(
        SccsFlag::QText("Q".into()).prs_fl_line().as_deref(),
        Some(&b"csect name\tQ"[..])
    );
    assert_eq!(
        SccsFlag::MrValidation(None).prs_fl_line().as_deref(),
        Some(&b"validate MRs\t"[..])
    );
    // The encoded flag is not part of the :FL: listing.
    assert_eq!(SccsFlag::Encoded(0).prs_fl_line(), None);
}

/// Removing a delta takes its changes out of the body -- the lines it
/// inserted go, the lines it deleted are text again -- and keeps its entry,
/// marked removed. Written back, the file is the one before that delta but
/// for the entry.
#[test]
fn remove_delta_undoes_the_deltas_changes() {
    // 1.1 inserts a, b, c; 1.2 (serial 2) deletes b and inserts X.
    let table = b"\x01s 00001/00001/00002\n\x01d D 1.2 26/01/02 00:00:00 u 2 1\n\x01e\n\
\x01s 00003/00000/00000\n\x01d D 1.1 26/01/01 00:00:00 u 1 0\n\x01e\n\
\x01u\n\x01U\n\x01t\n\x01T\n";
    let with_body = |body: &[u8]| with_checksum(&[&table[..], body].concat());
    let mut f = SccsFile::from_bytes(&with_body(
        b"\x01I 1\na\n\x01D 2\nb\n\x01E 2\n\x01I 2\nX\n\x01E 2\nc\n\x01E 1\n",
    ))
    .unwrap();

    f.remove_delta(2).unwrap();

    let removed = String::from_utf8(with_body(b"\x01I 1\na\nb\nc\n\x01E 1\n"))
        .unwrap()
        .replacen("\x01d D 1.2", "\x01d R 1.2", 1);
    let written = f.to_bytes();
    assert_eq!(
        checksummed_part(&written),
        checksummed_part(removed.as_bytes())
    );
    assert_eq!(
        f.header().deltas[0].delta_type,
        DeltaType::Removed,
        "the entry is kept, as removed"
    );
    assert!(f.remove_delta(2).is_err(), "it cannot be removed twice");
}

/// An s-file image without its checksum line.
fn checksummed_part(data: &[u8]) -> &[u8] {
    let at = data.iter().position(|&b| b == b'\n').unwrap();
    &data[at + 1..]
}

#[test]
fn test_real_login_name_nonempty() {
    // Must resolve a name from the passwd database (real uid) regardless of
    // a spoofed/empty USER/LOGNAME environment; never the empty string.
    let name = real_login_name();
    assert!(!name.is_empty());
}

#[test]
fn test_now_local_time_fields_in_range() {
    // now() must produce valid wall-clock fields (TZ-aware via localtime_r).
    let dt = SccsDateTime::now();
    assert!(dt.month >= 1 && dt.month <= 12);
    assert!(dt.day >= 1 && dt.day <= 31);
    assert!(dt.hour <= 23);
    assert!(dt.minute <= 59);
    assert!(dt.second <= 60);
}

#[test]
fn test_sid_display() {
    assert_eq!(Sid::trunk(1, 1).to_string(), "1.1");
    assert_eq!(Sid::new(1, 2, 3, 4).to_string(), "1.2.3.4");
}

#[test]
fn test_delta_stats() {
    let stats = DeltaStats::from_str("00003/00001/00010").unwrap();
    assert_eq!(stats.inserted, 3);
    assert_eq!(stats.deleted, 1);
    assert_eq!(stats.unchanged, 10);
    assert_eq!(stats.to_string(), "00003/00001/00010");
}

#[test]
fn test_checksum() {
    let data = b"hello world";
    let sum = compute_checksum(data);
    assert!(sum > 0);
}

#[test]
fn test_path_utilities() {
    use std::path::Path;

    assert!(paths::is_sfile(Path::new("s.foo")));
    assert!(paths::is_sfile(Path::new("SCCS/s.foo.c")));
    assert!(!paths::is_sfile(Path::new("foo.c")));

    assert_eq!(
        paths::gfile_from_sfile(Path::new("s.foo.c")),
        Some(PathBuf::from("foo.c"))
    );

    assert_eq!(
        paths::pfile_from_sfile(Path::new("SCCS/s.foo")),
        PathBuf::from("SCCS/p.foo")
    );

    assert_eq!(
        paths::module_name(Path::new("s.foo.c")),
        Some("foo.c".to_string())
    );
}

#[test]
fn test_sccs_flag_parse() {
    let flag = SccsFlag::parse('b', b"").unwrap();
    assert!(matches!(flag, SccsFlag::BranchEnabled));

    let flag = SccsFlag::parse('c', b"5").unwrap();
    assert!(matches!(flag, SccsFlag::Ceiling(5)));

    let flag = SccsFlag::parse('m', b"mymodule").unwrap();
    assert!(matches!(flag, SccsFlag::ModuleName(ref s) if s == b"mymodule"));
}

// Fixture tests - verify parser can read cssc-generated SCCS files

fn fixtures_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("sccs/tests/fixtures")
}

#[test]
fn test_parse_simple_fixture() {
    let path = fixtures_path().join("s.simple");
    if !path.exists() {
        eprintln!("Skipping test: fixture not found at {:?}", path);
        return;
    }

    let sccs = SccsFile::from_path(&path).expect("Failed to parse s.simple");

    // Should have exactly one delta
    assert_eq!(
        sccs.header().deltas.len(),
        1,
        "Expected 1 delta in s.simple"
    );

    // Delta should be 1.1
    let delta = &sccs.header().deltas[0];
    assert_eq!(delta.sid.to_string(), "1.1");
    assert_eq!(delta.serial, 1);
    assert_eq!(delta.pred_serial, 0);

    // Body should have content
    assert!(!sccs.body().is_empty(), "Body should not be empty");

    // Evaluate body for delta 1.1
    let applied = sccs
        .compute_applied_set(1)
        .expect("Failed to compute applied set");
    let content = sccs.evaluate_body(&applied);

    // Should have 3 lines: line1, line2, line3
    assert_eq!(content.len(), 3, "Expected 3 lines of content");
    assert_eq!(content[0], b"line1");
    assert_eq!(content[1], b"line2");
    assert_eq!(content[2], b"line3");
}

#[test]
fn test_parse_multi_fixture() {
    let path = fixtures_path().join("s.multi");
    if !path.exists() {
        eprintln!("Skipping test: fixture not found at {:?}", path);
        return;
    }

    let sccs = SccsFile::from_path(&path).expect("Failed to parse s.multi");

    // Should have multiple deltas
    assert!(
        sccs.header().deltas.len() >= 2,
        "Expected multiple deltas in s.multi"
    );

    // Find trunk head
    let head = sccs.get_trunk_head();
    assert!(head.is_some(), "Should have a trunk head");

    // Verify we can evaluate body for the latest version
    let head_serial = head.unwrap().serial;
    let applied = sccs
        .compute_applied_set(head_serial)
        .expect("Failed to compute applied set");
    let content = sccs.evaluate_body(&applied);
    assert!(
        !content.is_empty(),
        "Content should not be empty for latest version"
    );
}

#[test]
fn test_parse_keywords_fixture() {
    let path = fixtures_path().join("s.keywords");
    if !path.exists() {
        eprintln!("Skipping test: fixture not found at {:?}", path);
        return;
    }

    let sccs = SccsFile::from_path(&path).expect("Failed to parse s.keywords");

    // Should have at least one delta
    assert!(
        !sccs.header().deltas.is_empty(),
        "Should have at least one delta"
    );

    // Evaluate body
    let applied = sccs
        .compute_applied_set(1)
        .expect("Failed to compute applied set");
    let content = sccs.evaluate_body(&applied);

    // Content should contain keyword markers
    let content_str = String::from_utf8(content.join(&b"\n"[..])).unwrap();
    assert!(
        content_str.contains("%"),
        "Content should contain SCCS keyword markers"
    );
}

#[test]
fn test_parse_branched_fixture() {
    let path = fixtures_path().join("s.branched");
    if !path.exists() {
        eprintln!("Skipping test: fixture not found at {:?}", path);
        return;
    }

    let sccs = SccsFile::from_path(&path).expect("Failed to parse s.branched");

    // Should have multiple deltas including branch
    assert!(sccs.header().deltas.len() >= 2, "Expected multiple deltas");

    // Should have 'b' flag enabled for branches
    assert!(sccs.branch_enabled(), "Branch flag should be enabled");

    // Look for a branch delta (SID with 4 components)
    let has_branch = sccs.header().deltas.iter().any(|d| !d.sid.is_trunk());
    assert!(has_branch, "Should have at least one branch delta");

    // Verify we can get different versions
    for delta in &sccs.header().deltas {
        let applied = sccs.compute_applied_set(delta.serial).unwrap_or_else(|_| {
            panic!("Failed to compute applied set for serial {}", delta.serial)
        });
        let content = sccs.evaluate_body(&applied);
        // Each version should produce some content
        assert!(
            !content.is_empty(),
            "Version {} should have content",
            delta.sid
        );
    }
}

/// Reading an s-file and writing it back changes nothing: not one byte, for
/// every fixture. (They were written by CSSC, in the canonical layout.) This
/// is the invariant every rewrite of an s-file rests on.
#[test]
fn an_unchanged_file_is_written_back_byte_for_byte() {
    for name in ["s.simple", "s.multi", "s.keywords", "s.branched", "s.odd"] {
        let data = std::fs::read(fixtures_path().join(name)).expect(name);
        let sccs = SccsFile::from_bytes(&data).expect(name);
        assert_eq!(sccs.to_bytes(), data, "{name} changed on a round trip");
    }

    // Not only the canonical layout: blank lines ahead of the checksum line,
    // a wrong checksum, and a last line with no newline all come back as
    // they were.
    for data in [
        &b"\n\r\n\x01h00000\n\x01s 0/0/0\n\x01d D 1.1 26/01/01 00:00:00 u 1 0\n\x01e\n\
\x01u\n\x01U\n\x01t\n\x01T\n\x01I 1\nlast line"[..],
        b"\x01h1\n\x01u\n\x01U\n\x01t\n\x01T",
    ] {
        let sccs = SccsFile::from_bytes(data).unwrap();
        assert_eq!(sccs.to_bytes(), data);
    }
}

/// `s.odd`: a valid s-file in anything but the canonical layout -- unpadded
/// statistics, doubled blanks, comment and MR lines interleaved, text after
/// the section markers, a padded login, stray lines, markers with extra
/// blanks and zeros.
fn odd_file() -> (Vec<u8>, SccsFile) {
    let data = std::fs::read(fixtures_path().join("s.odd")).unwrap();
    let sccs = SccsFile::from_bytes(&data).unwrap();
    (data, sccs)
}

/// Assert that an edit changed only what it was to change: the lines of
/// `after`, less the checksum line and the lines `added`, are the lines of
/// `before`, less the checksum line and the lines `removed`, in order.
#[track_caller]
fn assert_only_changed(before: &[u8], after: &[u8], removed: &[&[u8]], added: &[&[u8]]) {
    let rest = |data: &'_ [u8], without: &[&[u8]]| -> Vec<Vec<u8>> {
        let mut without = without.to_vec();
        split_lines(data)
            .into_iter()
            .skip(1)
            .filter(|line| match without.iter().position(|w| w == line) {
                Some(at) => {
                    without.remove(at);
                    false
                }
                None => true,
            })
            .map(<[u8]>::to_vec)
            .collect()
    };
    let show = |lines: &[Vec<u8>]| {
        lines
            .iter()
            .map(|l| String::from_utf8_lossy(l).into_owned())
            .collect::<Vec<_>>()
    };
    assert_eq!(
        show(&rest(after, added)),
        show(&rest(before, removed)),
        "the edit touched lines it was not asked to"
    );
    for line in added {
        assert!(
            split_lines(after).contains(line),
            "{:?} was not written",
            String::from_utf8_lossy(line)
        );
    }
    assert!(stored_checksum_matches(after, stored_body_checksum(after)));
    assert!(after.starts_with(format!("\x01h{:05}\n", stored_body_checksum(after)).as_bytes()));
}

/// Each change to the header rewrites the lines it names and no others: the
/// odd spacing, the stray lines and the record order everywhere else in the
/// file survive it.
#[test]
fn a_header_edit_changes_only_the_lines_it_names() {
    let (before, original) = odd_file();
    let edit = |f: &dyn Fn(&mut SccsFile)| -> Vec<u8> {
        let mut sccs = original.clone();
        f(&mut sccs);
        sccs.to_bytes()
    };

    // A new flag goes after the last flag line.
    let after = edit(&|s| s.set_flag(SccsFlag::JointEdit).unwrap());
    assert_only_changed(&before, &after, &[], &[b"\x01f j"]);
    let lines = split_lines(&after);
    let at = lines.iter().position(|l| l == b"\x01f j").unwrap();
    assert_eq!(lines[at - 1], b"\x01f b");

    // A flag set again is replaced where it stands.
    let after = edit(&|s| s.set_flag(SccsFlag::QText(b"y".to_vec())).unwrap());
    assert_only_changed(&before, &after, &[b"\x01f  q  x"], &[b"\x01f q y"]);
    let lines = split_lines(&after);
    let at = lines.iter().position(|l| l == b"\x01f q y").unwrap();
    assert_eq!(lines[at + 1], b"\x01f v");

    let after = edit(&|s| assert!(s.remove_flag('v').unwrap()));
    assert_only_changed(&before, &after, &[b"\x01f v"], &[]);

    let after = edit(&|s| assert!(s.add_user(b"fred").unwrap()));
    assert_only_changed(&before, &after, &[], &[b"fred"]);
    let lines = split_lines(&after);
    let at = lines.iter().position(|l| l == b"fred").unwrap();
    assert_eq!(
        (lines[at - 1], lines[at + 1]),
        (&b"  bob  "[..], &b"\x01U"[..])
    );

    let after = edit(&|s| assert!(s.remove_user(b"bob").unwrap()));
    assert_only_changed(&before, &after, &[b"  bob  "], &[]);

    let after = edit(&|s| {
        s.set_descriptive_text(&[b"new".to_vec(), b"text".to_vec()])
            .unwrap()
    });
    assert_only_changed(&before, &after, &[b"described "], &[b"new", b"text"]);
    let after = edit(&|s| s.set_descriptive_text(&[]).unwrap());
    assert_only_changed(&before, &after, &[b"described "], &[]);
}

/// A change that changes nothing leaves the file as it was, to the byte:
/// setting a flag to what it is, adding a login already listed, removing
/// what is not there, replacing the descriptive text with itself.
#[test]
fn an_edit_that_changes_nothing_leaves_the_file_alone() {
    let (before, mut sccs) = odd_file();
    sccs.set_flag(SccsFlag::BranchEnabled).unwrap();
    assert!(!sccs.add_user(b"bob").unwrap());
    assert!(!sccs.remove_user(b"nobody").unwrap());
    assert!(!sccs.remove_flag('j').unwrap());
    sccs.set_descriptive_text(&[b"described ".to_vec()])
        .unwrap();
    assert_eq!(sccs.to_bytes(), before);
}

/// Removing a delta changes one byte of its entry, its type, and takes its
/// markers and inserted lines out of the body; every other line is as it
/// was. Adding a delta puts its entry first and its blocks in the body, and
/// copies the rest.
#[test]
fn rmdel_and_delta_change_only_their_own_lines() {
    use crate::linediff::LineOp::{Delete, Insert, Keep};
    let (before, original) = odd_file();

    let mut sccs = original.clone();
    sccs.remove_delta(2).unwrap();
    assert_only_changed(
        &before,
        &sccs.to_bytes(),
        &[
            b"\x01d D 1.2  26/01/02 00:00:00 u 2 1",
            b"\x01D 02",
            b"\x01E 02",
            b"\x01I 2",
            b"B",
            b"\x01E 2",
        ],
        &[b"\x01d R 1.2  26/01/02 00:00:00 u 2 1"],
    );
    assert_eq!(
        sccs.evaluate_body(&[1].into()),
        [b"a".to_vec(), b"b".to_vec()]
    );

    // 1.2 is "a B"; make 1.3 "a C".
    let mut sccs = original.clone();
    let entry = DeltaEntry {
        sid: Sid::trunk(1, 3),
        datetime: SccsDateTime::parse("26/01/03", "00:00:00").unwrap(),
        user: b"u".to_vec(),
        serial: 3,
        pred_serial: 2,
        stats: DeltaStats::new(1, 1, 1),
        comments: vec![b"third".to_vec()],
        ..Default::default()
    };
    let new_lines = [b"a".to_vec(), b"C".to_vec()];
    sccs.add_delta(
        entry,
        &[1, 2].into(),
        &[Keep, Delete, Insert(1)],
        &new_lines,
    )
    .unwrap();
    let after = sccs.to_bytes();
    assert_only_changed(
        &before,
        &after,
        &[],
        &[
            b"\x01s 00001/00001/00001",
            b"\x01d D 1.3 26/01/03 00:00:00 u 3 2",
            b"\x01c third",
            b"\x01e",
            b"\x01D 3",
            b"\x01E 3",
            b"\x01I 3",
            b"C",
            b"\x01E 3",
        ],
    );
    assert_eq!(split_lines(&after)[1], b"\x01s 00001/00001/00001");
    assert_eq!(sccs.evaluate_body(&[1, 2, 3].into()), new_lines);
    assert_eq!(
        sccs.evaluate_body(&[1, 2].into()),
        [b"a".to_vec(), b"B".to_vec()]
    );
}

/// An s-file image: `content` (everything after the `^Ah` line) with
/// its correct checksum line prepended.
fn with_checksum(content: &[u8]) -> Vec<u8> {
    let mut out = format!("\x01h{:05}\n", compute_checksum(content)).into_bytes();
    out.extend_from_slice(content);
    out
}

const ONE_DELTA: &[u8] = b"\x01s 00001/00000/00000\n\
\x01d D 1.1 26/01/01 00:00:00 u 1 0\n\
\x01c c\n\
\x01e\n\
\x01u\n\x01U\n";

/// A CR before a newline is part of the line: a CRLF text file put
/// under SCCS must come back with its CRs, and a rewrite of the s-file
/// must not drop them.
#[test]
fn carriage_returns_survive_a_round_trip() {
    let mut content = ONE_DELTA.to_vec();
    content.extend_from_slice(b"\x01t\ndesc\r\n\x01T\n\x01I 1\na\r\nb\r\n\x01E 1\n");
    let data = with_checksum(&content);
    let sccs = SccsFile::from_bytes(&data).unwrap();
    let applied = sccs.compute_applied_set(1).unwrap();
    let body = sccs.evaluate_body(&applied);
    assert_eq!(body, [b"a\r", b"b\r"]);
    assert_eq!(sccs.to_bytes(), data, "a rewrite must be byte-identical");
}

/// Control lines too short to carry an argument, and a flag letter that
/// is not ASCII, are malformed input, not a reason to panic.
#[test]
fn short_control_lines_and_odd_flags_do_not_panic() {
    let content = b"\x01s 00000/00000/00000\n\
\x01d D 1.1 26/01/01 00:00:00 u 1 0\n\
\x01c\n\x01m\n\x01i\n\x01x\n\x01g\n\
\x01e\n\
\x01u\n\x01U\n\
\x01f \xc3\xa9x\n\
\x01t\n\x01T\n\
\x01I\n\x01E\n";
    let sccs = SccsFile::from_bytes(&with_checksum(content)).unwrap();
    let nothing = HashSet::new();
    let records: Vec<Rec> = Walker::new(sccs.body(), &nothing)
        .map(|(_, rec)| rec)
        .collect();
    assert_eq!(records, [Rec::Insert(0), Rec::End(0)]);
    assert_eq!(
        sccs.header().flags,
        [SccsFlag::Unknown('\u{e9}', b"x".to_vec())]
    );
}

#[test]
fn split_lines_keeps_carriage_returns() {
    assert!(split_lines(b"").is_empty());
    assert_eq!(split_lines(b"a\r\nb\n"), [&b"a\r"[..], b"b"]);
    assert_eq!(split_lines(b"a\n\nb"), [&b"a"[..], b"", b"b"]);
    assert_eq!(split_lines(b"\n"), [&b""[..]]);
}

/// The checksum sums bytes as signed chars, as historical SCCS and CSSC
/// do, so a byte above 0x7f counts below zero; verification also accepts
/// the unsigned sum earlier versions of these utilities wrote.
#[test]
fn checksum_sums_signed_bytes_and_accepts_either_sum() {
    assert_eq!(compute_checksum(b"ab"), 0x61 + 0x62);
    assert_eq!(compute_checksum(b"a\xe9"), (0x61u16).wrapping_sub(0x17));

    let file = |sum: u16| {
        let mut v = format!("\x01h{:05}\n", sum).into_bytes();
        v.extend_from_slice(b"\xe9\n");
        v
    };
    let signed = (0xe9u16 + 0x0a).wrapping_sub(0x100);
    let unsigned = 0xe9 + 0x0a;
    assert!(stored_checksum_matches(&file(signed), signed));
    assert!(stored_checksum_matches(&file(unsigned), unsigned));
    assert!(!stored_checksum_matches(&file(1), 1));
}

/// A count above 99 999 is recorded as 99 999 (POSIX admin 84132-84133):
/// the fields of the `^As` line are five digits wide.
#[test]
fn delta_stats_are_recorded_capped_at_99999() {
    let stats = DeltaStats::new(100_005, 99_999, 7);
    assert_eq!(stats.to_string(), "99999/99999/00007");
}

#[test]
fn not_storable_as_text_names_the_reason() {
    assert_eq!(not_storable_as_text(b""), None);
    assert_eq!(not_storable_as_text(b"a\r\ncaf\xe9\nmid\x01soh\n"), None);
    assert_eq!(not_storable_as_text(b"a\nb"), Some(NotText::NoFinalNewline));
    assert_eq!(not_storable_as_text(b"a\n\x00\n"), Some(NotText::Nul));
    assert_eq!(
        not_storable_as_text(b"a\n\x01b\n"),
        Some(NotText::LeadingSoh(2))
    );
}

/// The walker reports every record in order, and for each text line of an
/// overlapping weave the delta that inserted it, or that it is not part of
/// the version.
#[test]
fn walker_reports_each_line_with_its_inserter() {
    let mut content = b"\x01s 00000/00000/00000\n\x01d D 1.1 26/01/01 00:00:00 u 1 0\n\x01e\n\
\x01u\n\x01U\n\x01t\n\x01T\n"
        .to_vec();
    content
        .extend_from_slice(b"\x01I 1\na\n\x01D 3\nb\n\x01E 1\n\x01I 2\nc\n\x01E 3\nd\n\x01E 2\n");
    let sccs = SccsFile::from_bytes(&with_checksum(&content)).unwrap();
    let lines = |applied: &[u16]| -> Vec<(Vec<u8>, Option<u16>)> {
        let applied: HashSet<u16> = applied.iter().copied().collect();
        Walker::new(sccs.body(), &applied)
            .filter_map(|(_, rec)| match rec {
                Rec::Text { text, kept_by } => Some((text.to_vec(), kept_by)),
                _ => None,
            })
            .collect()
    };
    let l = |t: &str, by: Option<u16>| (t.as_bytes().to_vec(), by);
    assert_eq!(
        lines(&[1]),
        [l("a", Some(1)), l("b", Some(1)), l("c", None), l("d", None)]
    );
    assert_eq!(
        lines(&[1, 2]),
        [
            l("a", Some(1)),
            l("b", Some(1)),
            l("c", Some(2)),
            l("d", Some(2))
        ]
    );
    assert_eq!(
        lines(&[1, 2, 3]),
        [l("a", Some(1)), l("b", None), l("c", None), l("d", Some(2))]
    );
    let indices: Vec<usize> = Walker::new(sccs.body(), &HashSet::new())
        .map(|(i, _)| i)
        .collect();
    assert_eq!(indices, (0..sccs.body().len()).collect::<Vec<_>>());
}

/// Adding a delta records its changes and leaves every earlier version as it
/// was; an edit script that does not account for exactly the lines of the
/// base version is refused, and the file is left untouched.
#[test]
fn add_delta_records_the_edit_or_refuses_it() {
    use crate::linediff::LineOp::{Delete, Insert, Keep};
    let content = b"\x01s 00003/00000/00000\n\x01d D 1.1 26/01/01 00:00:00 u 1 0\n\x01e\n\
\x01u\n\x01U\n\x01t\n\x01T\n\x01I 1\na\nb\nc\n\x01E 1\n";
    let original = SccsFile::from_bytes(&with_checksum(content)).unwrap();
    let v1: HashSet<u16> = [1].into();
    let entry = || DeltaEntry {
        sid: Sid::trunk(1, 2),
        user: b"u".to_vec(),
        serial: 2,
        pred_serial: 1,
        ..Default::default()
    };

    // a b c  ->  x a c y
    let new_lines = [b"x".to_vec(), b"a".to_vec(), b"c".to_vec(), b"y".to_vec()];
    let ops = [Insert(0), Keep, Delete, Keep, Insert(3)];
    let mut f = original.clone();
    f.add_delta(entry(), &v1, &ops, &new_lines).unwrap();
    assert_eq!(f.evaluate_body(&[1, 2].into()), new_lines);
    assert_eq!(
        f.evaluate_body(&v1),
        [b"a".to_vec(), b"b".to_vec(), b"c".to_vec()]
    );
    assert_eq!(f.header().deltas[0].serial, 2, "the new entry comes first");

    for bad in [
        &[Keep, Keep][..],              // a line of the base unaccounted for
        &[Keep, Keep, Keep, Keep],      // one line too many
        &[Keep, Keep, Keep, Insert(9)], // an insert of a line that is not there
    ] {
        let mut f = original.clone();
        assert!(
            f.add_delta(entry(), &v1, bad, &new_lines).is_err(),
            "{bad:?}"
        );
        assert_eq!(
            f.to_bytes(),
            original.to_bytes(),
            "{bad:?} changed the file"
        );
    }
}

/// A small deterministic generator (xorshift64*), so the property tests need
/// no dependency and fail the same way every time.
struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 >> 12;
        self.0 ^= self.0 << 25;
        self.0 ^= self.0 >> 27;
        self.0.wrapping_mul(0x2545_F491_4F6C_DD1D)
    }

    /// A number below `n` (which is not zero).
    fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }
}

/// Random histories through the edit methods: deltas made from any earlier
/// version, deltas removed, flags and users changed. After every step the
/// file holds together:
/// - every version still in it reads back as the text it was given;
/// - what is written reads back, strictly, as the same file, with the right
///   checksum;
/// - the parsed view is the one a fresh parse of the bytes gives.
#[test]
fn random_histories_keep_every_version_and_stay_well_formed() {
    use std::collections::HashMap;
    for seed in 1..=40u64 {
        let mut rng = Rng(seed.wrapping_mul(0x9E37_79B9_7F4A_7C15));
        let entry = |serial: u16, pred: u16| DeltaEntry {
            sid: Sid::trunk(1, serial),
            datetime: SccsDateTime::parse("26/01/01", "00:00:00").unwrap(),
            user: b"u".to_vec(),
            serial,
            pred_serial: pred,
            comments: vec![format!("step {serial}").into_bytes()],
            ..Default::default()
        };
        let first: Vec<Vec<u8>> = (0..rng.below(6))
            .map(|i| format!("l{i}").into_bytes())
            .collect();
        let mut sccs = SccsFile::create(NewFile {
            delta: entry(1, 0),
            users: &[],
            flags: &[],
            descriptive_text: &[],
            lines: &first,
        })
        .unwrap();
        // The text each live version was given.
        let mut model: HashMap<u16, Vec<Vec<u8>>> = HashMap::from([(1, first)]);

        for step in 0..25 {
            match rng.below(10) {
                0 => {
                    let flag = [SccsFlag::BranchEnabled, SccsFlag::JointEdit][rng.below(2)].clone();
                    if rng.below(2) == 0 {
                        sccs.set_flag(flag).unwrap();
                    } else {
                        sccs.remove_flag(flag.flag_char()).unwrap();
                    }
                }
                1 => {
                    let login = format!("user{}", rng.below(3)).into_bytes();
                    if rng.below(2) == 0 {
                        sccs.add_user(&login).unwrap();
                    } else {
                        sccs.remove_user(&login).unwrap();
                    }
                }
                2 => {
                    // Remove a delta nothing was made from, never the last.
                    let leaves: Vec<u16> = model
                        .keys()
                        .copied()
                        .filter(|&s| !sccs.live_deltas().any(|d| d.pred_serial == s))
                        .collect();
                    if model.len() > 1 && !leaves.is_empty() {
                        let victim = leaves[rng.below(leaves.len())];
                        sccs.remove_delta(victim).unwrap();
                        model.remove(&victim);
                    }
                }
                _ => {
                    let mut live: Vec<u16> = model.keys().copied().collect();
                    live.sort_unstable();
                    let base = live[rng.below(live.len())];
                    let mut text = model[&base].clone();
                    for _ in 0..1 + rng.below(3) {
                        let at = rng.below(text.len() + 1);
                        if rng.below(2) == 0 && at < text.len() {
                            let end = (at + 1 + rng.below(3)).min(text.len());
                            text.drain(at..end);
                        } else {
                            let n = 1 + rng.below(3);
                            text.splice(
                                at..at,
                                (0..n).map(|k| format!("s{step}.{k}").into_bytes()),
                            );
                        }
                    }
                    let serial = sccs.max_serial() + 1;
                    let applied = sccs.compute_applied_set(base).unwrap();
                    let ops = crate::linediff::diff(&model[&base], &text);
                    sccs.add_delta(entry(serial, base), &applied, &ops, &text)
                        .unwrap();
                    model.insert(serial, text);
                }
            }

            let bytes = sccs.to_bytes();
            let reread = SccsFile::from_bytes_strict(&bytes)
                .unwrap_or_else(|e| panic!("seed {seed} step {step}: {e}"));
            assert_eq!(reread.to_bytes(), bytes, "seed {seed} step {step}");
            assert_eq!(
                format!("{:?}", reread.header()),
                format!("{:?}", sccs.header()),
                "seed {seed} step {step}: the view is not what the bytes say"
            );
            assert!(
                bytes.starts_with(format!("\x01h{:05}\n", stored_body_checksum(&bytes)).as_bytes()),
                "seed {seed} step {step}: checksum"
            );
            for (serial, text) in &model {
                let applied = sccs.compute_applied_set(*serial).unwrap();
                assert_eq!(
                    &sccs.evaluate_body(&applied),
                    text,
                    "seed {seed} step {step}: version {serial}"
                );
            }
        }
    }
}
