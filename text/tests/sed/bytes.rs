//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Input is text in the current locale, not UTF-8: in the C locale every byte
//! is a character. Expected outputs were taken from GNU sed 4.9 under
//! `LC_ALL=C` (the test runner's default locale).

use plib::testing::{run_test_base_with_env, run_test_u8, utf8_locale, TestPlanU8};

/// Latin-1 text, bytes that begin no UTF-8 sequence, and UTF-8 text.
const MIXED: &[u8] = b"caf\xe9 na\xefve\n\xff\xfe\x80 bin\n\xc3\xa9t\xc3\xa9\n";

fn sed_bytes(args: &[&str], input: &[u8], expected: &[u8]) {
    run_test_u8(TestPlanU8 {
        cmd: String::from("sed"),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: input.to_vec(),
        expected_out: expected.to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_sed_bytes_pass_through() {
    sed_bytes(&[""], MIXED, MIXED);
    sed_bytes(&["-n", "p"], MIXED, MIXED);
}

#[test]
fn test_sed_bytes_dot_matches_every_byte() {
    sed_bytes(&["s/./X/g"], MIXED, b"XXXXXXXXXX\nXXXXXXX\nXXXXX\n");
}

#[test]
fn test_sed_bytes_l_escapes_octal() {
    sed_bytes(
        &["-n", "l"],
        MIXED,
        b"caf\\351 na\\357ve$\n\\377\\376\\200 bin$\n\\303\\251t\\303\\251$\n",
    );
    sed_bytes(
        &["-n", "l 5"],
        b"\xff\xfe\x80 bin\n",
        b"\\377\\\n\\376\\\n\\200\\\n bin$\n",
    );
}

#[test]
fn test_sed_bytes_y_leaves_high_bytes() {
    sed_bytes(
        &["y/ae/AE/"],
        MIXED,
        b"cAf\xe9 nA\xefvE\n\xff\xfe\x80 bin\n\xc3\xa9t\xc3\xa9\n",
    );
}

#[test]
fn test_sed_bytes_y_multibyte_operand_is_bytes_in_c_locale() {
    // In the C locale `é` is two characters, so the operands differ in length.
    run_test_u8(TestPlanU8 {
        cmd: String::from("sed"),
        args: vec![String::from("y/\u{e9}/E/")],
        stdin_data: MIXED.to_vec(),
        expected_out: Vec::new(),
        expected_err:
            b"sed: number of characters in the two arrays does not match (line: 0, col: 7)\n"
                .to_vec(),
        expected_exit_code: 1,
    });
    // Two bytes for two bytes is a valid byte transliteration.
    sed_bytes(&["y/\u{e9}/EF/"], b"\xc3\xa9t\xc3\n", b"EFtE\n");
}

#[test]
fn test_sed_bytes_address_re_selects_lines() {
    sed_bytes(&["-n", "/bin/p"], MIXED, b"\xff\xfe\x80 bin\n");
    sed_bytes(&["/na/d"], MIXED, b"\xff\xfe\x80 bin\n\xc3\xa9t\xc3\xa9\n");
    sed_bytes(&["-n", "/^.\\{5\\}$/p"], MIXED, b"\xc3\xa9t\xc3\xa9\n");
}

#[test]
fn test_sed_bytes_r_and_w_files() {
    let rfile = format!("{}/sed_bytes_r.txt", env!("CARGO_TARGET_TMPDIR"));
    let wfile = format!("{}/sed_bytes_w.txt", env!("CARGO_TARGET_TMPDIR"));
    std::fs::write(&rfile, b"r\xe9ad\n").unwrap();
    sed_bytes(&[&format!("r {rfile}")], b"a\n", b"a\nr\xe9ad\n");
    sed_bytes(&["-n", &format!("w {wfile}")], b"\xe9\n\xff\n", b"");
    assert_eq!(std::fs::read(&wfile).unwrap(), b"\xe9\n\xff\n");
    sed_bytes(&["-n", &format!("s/a/b/w {wfile}")], b"\xe9a\n", b"");
    assert_eq!(std::fs::read(&wfile).unwrap(), b"\xe9b\n");
    let _ = std::fs::remove_file(&rfile);
    let _ = std::fs::remove_file(&wfile);
}

#[test]
fn test_sed_bytes_substitute() {
    sed_bytes(&["s/[^a-z]/?/g"], MIXED, b"caf??na?ve\n????bin\n??t??\n");
    sed_bytes(
        &["s/\\(.\\)\\(.\\)/\\2\\1/"],
        MIXED,
        b"acf\xe9 na\xefve\n\xfe\xff\x80 bin\n\xa9\xc3t\xc3\xa9\n",
    );
    sed_bytes(&["s/ .*/&&/"], b"\xe9 \xff\n", b"\xe9 \xff \xff\n");
}

#[test]
fn test_sed_bytes_empty_match_steps_one_byte() {
    sed_bytes(&["s/x*/-/g"], b"\xe9a\xc3\xa9\n", b"-\xe9-a-\xc3-\xa9-\n");
}

#[test]
fn test_sed_bytes_hold_and_append_keep_bytes() {
    sed_bytes(
        &["-n", "H;${x;s/\\n/,/g;p}"],
        b"\xe9\n\xff\n",
        b",\xe9,\xff\n",
    );
    sed_bytes(&["$!N;P;D"], b"\xe9\n\xff\n\x80\n", b"\xe9\n\xff\n\x80\n");
    sed_bytes(&["$!d"], b"\xe9\n\xff", b"\xff");
    // `P` ends the line it writes even when the input's last line has no
    // <newline>: the embedded one terminates it.
    sed_bytes(&["$!N;P;D"], b"a\r\nb\xa0c\r\n\xe9", b"a\r\nb\xa0c\r\n\xe9");
}

// A <carriage-return> is an ordinary character: CRLF text keeps its CRs.
#[test]
fn test_sed_bytes_carriage_returns_kept() {
    sed_bytes(&[""], b"a\r\nb\r\r\n\r\n", b"a\r\nb\r\r\n\r\n");
    sed_bytes(&["s/a/\u{e9}/"], b"a\r\n", b"\xc3\xa9\r\n");
}

// In a UTF-8 locale `y` maps characters, and a byte that is no character
// there is left alone.
#[test]
fn test_sed_bytes_y_utf8_locale() {
    let Some(locale) = utf8_locale() else {
        return;
    };
    let out = run_test_base_with_env(
        "sed",
        &[String::from("y/\u{e9}a/E\u{e0}/")],
        b"\xc3\xa9a\xe9\n",
        &[("LC_ALL", &locale)],
    );
    assert_eq!(out.stdout, b"E\xc3\xa0\xe9\n");
    assert!(
        out.stderr.is_empty(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
}
