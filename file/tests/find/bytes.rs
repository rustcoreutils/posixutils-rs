//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! -name, -iname, -path and -ipath match file names as bytes, with
//! fnmatch(3) in the current locale.  They matched a lossy copy of the name,
//! with each byte that is not valid UTF-8 replaced by U+FFFD, and refused a
//! pattern operand that is not valid UTF-8.

use std::ffi::OsStr;
use std::os::unix::ffi::OsStrExt;
use std::process::{Command, Output, Stdio};

use plib::testing::get_binary_path;
use plib::tmp::{tempdir, TempDir};

/// Run find on `dir` with `args` (byte strings) in locale `locale`.
fn find_in(dir: &TempDir, locale: &str, args: &[&[u8]]) -> Output {
    Command::new(get_binary_path("find"))
        .arg(dir.path())
        .args(args.iter().map(|a| OsStr::from_bytes(a)))
        .env("LC_ALL", locale)
        .stdin(Stdio::null())
        .output()
        .expect("run find")
}

/// The names (last path component) find printed, one per line, sorted.
fn printed_names(output: &Output) -> Vec<Vec<u8>> {
    assert!(output.status.success(), "{output:?}");
    assert!(output.stderr.is_empty(), "{output:?}");
    let mut names: Vec<Vec<u8>> = output
        .stdout
        .split(|&b| b == b'\n')
        .filter(|line| !line.is_empty())
        .map(|line| {
            let start = line.iter().rposition(|&b| b == b'/').map_or(0, |i| i + 1);
            line[start..].to_vec()
        })
        .collect();
    names.sort();
    names
}

/// A scratch directory holding empty files with the given names.
fn dir_with(names: &[&[u8]]) -> TempDir {
    let dir = tempdir().expect("create scratch dir");
    for name in names {
        std::fs::File::create(dir.path().join(OsStr::from_bytes(name))).unwrap();
    }
    dir
}

/// Assert that find in `locale` with `args` prints exactly `expected` names.
fn assert_finds(dir: &TempDir, locale: &str, args: &[&[u8]], expected: &[&[u8]]) {
    let output = find_in(dir, locale, args);
    let expected: Vec<Vec<u8>> = expected.iter().map(|e| e.to_vec()).collect();
    assert_eq!(
        printed_names(&output),
        expected,
        "find {:?} in {locale}",
        args.iter()
            .map(|a| String::from_utf8_lossy(a))
            .collect::<Vec<_>>()
    );
}

// A pattern operand that is not valid UTF-8 is a byte string like any other
// pattern; it was refused as "expression operand is not valid UTF-8".
#[test]
fn non_utf8_pattern_operand_is_accepted() {
    let dir = dir_with(&[b"plain"]);
    for primary in [&b"-name"[..], b"-iname", b"-path", b"-ipath"] {
        assert_finds(&dir, "C", &[primary, b"x\xffy"], &[]);
    }
}

// Linux only from here on: APFS refuses a name that is not valid UTF-8.

#[cfg(target_os = "linux")]
#[test]
fn name_matches_non_utf8_file_name_as_bytes() {
    let dir = dir_with(&[b"a\xffb", b"plain"]);
    assert_finds(&dir, "C", &[b"-name", b"a?b"], &[b"a\xffb"]);
    assert_finds(&dir, "C", &[b"-name", b"a*"], &[b"a\xffb"]);
    assert_finds(&dir, "C", &[b"-name", b"a\xffb"], &[b"a\xffb"]);
    assert_finds(&dir, "C", &[b"-name", b"a[\xff]b"], &[b"a\xffb"]);
    // U+FFFD's UTF-8 encoding is not the byte \xff.
    assert_finds(&dir, "C", &[b"-name", "a\u{FFFD}b".as_bytes()], &[]);
    assert_finds(&dir, "C", &[b"-iname", b"A?B"], &[b"a\xffb"]);
}

#[cfg(target_os = "linux")]
#[test]
fn path_matches_non_utf8_path_as_bytes() {
    let dir = dir_with(&[b"a\xffb", b"plain"]);
    assert_finds(&dir, "C", &[b"-path", b"*/a?b"], &[b"a\xffb"]);
    assert_finds(&dir, "C", &[b"-path", b"*/a\xffb"], &[b"a\xffb"]);
    assert_finds(&dir, "C", &[b"-ipath", b"*/A\xffB"], &[b"a\xffb"]);
}

// A multibyte character is one character to `?` in a UTF-8 locale and
// several bytes in the C locale, as GNU find has it.
#[test]
fn multibyte_name_matched_per_locale() {
    let dir = dir_with(&["x\u{e9}y".as_bytes()]);
    let e_acute = "x\u{e9}y".as_bytes();
    assert_finds(&dir, "C", &[b"-name", b"x?y"], &[]);
    assert_finds(&dir, "C", &[b"-name", b"x??y"], &[e_acute]);
    if let Some(utf8) = plib::testing::utf8_locale() {
        assert_finds(&dir, &utf8, &[b"-name", b"x?y"], &[e_acute]);
    }
}
