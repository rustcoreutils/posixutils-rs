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

#[test]
fn test_remove_delta_reweaves_body() {
    // Body for: 1.1 inserts a,b,c; 1.2 (serial 2) deletes b and inserts X.
    let mut f = SccsFile {
        body: vec![
            BodyRecord::Insert(1),
            BodyRecord::Text("a".into()),
            BodyRecord::Delete(2),
            BodyRecord::Text("b".into()),
            BodyRecord::End(2),
            BodyRecord::Insert(2),
            BodyRecord::Text("X".into()),
            BodyRecord::End(2),
            BodyRecord::Text("c".into()),
            BodyRecord::End(1),
        ],
        ..Default::default()
    };
    f.header.deltas.push(DeltaEntry {
        serial: 2,
        ..Default::default()
    });

    f.remove_delta(2);

    // Removing serial 2 reverts to 1.1: insert block dropped, deletion undone.
    assert_eq!(
        f.body,
        vec![
            BodyRecord::Insert(1),
            BodyRecord::Text("a".into()),
            BodyRecord::Text("b".into()),
            BodyRecord::Text("c".into()),
            BodyRecord::End(1),
        ]
    );
    // Delta-table entry retained as Removed.
    assert_eq!(
        f.find_delta_by_serial(2).unwrap().delta_type,
        DeltaType::Removed
    );
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
    assert_eq!(sccs.header.deltas.len(), 1, "Expected 1 delta in s.simple");

    // Delta should be 1.1
    let delta = &sccs.header.deltas[0];
    assert_eq!(delta.sid.to_string(), "1.1");
    assert_eq!(delta.serial, 1);
    assert_eq!(delta.pred_serial, 0);

    // Body should have content
    assert!(!sccs.body.is_empty(), "Body should not be empty");

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
        sccs.header.deltas.len() >= 2,
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
        !sccs.header.deltas.is_empty(),
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
    assert!(sccs.header.deltas.len() >= 2, "Expected multiple deltas");

    // Should have 'b' flag enabled for branches
    assert!(sccs.branch_enabled(), "Branch flag should be enabled");

    // Look for a branch delta (SID with 4 components)
    let has_branch = sccs.header.deltas.iter().any(|d| !d.sid.is_trunk());
    assert!(has_branch, "Should have at least one branch delta");

    // Verify we can get different versions
    for delta in &sccs.header.deltas {
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
    for name in ["s.simple", "s.multi", "s.keywords", "s.branched"] {
        let data = std::fs::read(fixtures_path().join(name)).expect(name);
        let sccs = SccsFile::from_bytes(&data).expect(name);
        assert_eq!(sccs.to_bytes(), data, "{name} changed on a round trip");
    }
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
    assert_eq!(sccs.body, [BodyRecord::Insert(0), BodyRecord::End(0)]);
    assert_eq!(
        sccs.header.flags,
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
        Walker::new(&sccs.body, &applied)
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
    let indices: Vec<usize> = Walker::new(&sccs.body, &HashSet::new())
        .map(|(i, _)| i)
        .collect();
    assert_eq!(indices, (0..sccs.body.len()).collect::<Vec<_>>());
}

/// Weaving a delta in records its changes and leaves every earlier version
/// as it was; an edit script that does not account for exactly the lines of
/// the base version is refused.
#[test]
fn weave_delta_records_the_edit_or_refuses_it() {
    use crate::linediff::LineOp::{Delete, Insert, Keep};
    let body = [
        BodyRecord::Insert(1),
        BodyRecord::Text(b"a".to_vec()),
        BodyRecord::Text(b"b".to_vec()),
        BodyRecord::Text(b"c".to_vec()),
        BodyRecord::End(1),
    ];
    let v1: HashSet<u16> = [1].into();
    let text = |body: &[BodyRecord], applied: &[u16]| -> Vec<Vec<u8>> {
        let applied: HashSet<u16> = applied.iter().copied().collect();
        Walker::new(body, &applied)
            .filter_map(|(_, rec)| match rec {
                Rec::Text {
                    text,
                    kept_by: Some(_),
                } => Some(text.to_vec()),
                _ => None,
            })
            .collect()
    };

    // a b c  ->  x a c y
    let new_lines = [b"x".to_vec(), b"a".to_vec(), b"c".to_vec(), b"y".to_vec()];
    let ops = [Insert(0), Keep, Delete, Keep, Insert(3)];
    let woven = weave_delta(&body, &v1, 2, &ops, &new_lines).unwrap();
    assert_eq!(text(&woven, &[1, 2]), new_lines);
    assert_eq!(
        text(&woven, &[1]),
        [b"a".to_vec(), b"b".to_vec(), b"c".to_vec()]
    );

    for bad in [
        &[Keep, Keep][..],              // a line of the base unaccounted for
        &[Keep, Keep, Keep, Keep],      // one line too many
        &[Keep, Keep, Keep, Insert(9)], // an insert of a line that is not there
    ] {
        assert!(
            weave_delta(&body, &v1, 2, bad, &new_lines).is_err(),
            "{bad:?}"
        );
    }
}
