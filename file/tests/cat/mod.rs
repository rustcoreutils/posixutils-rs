//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;

use plib::testing::{run_test, run_test_u8, run_test_with_checker, TestPlan, TestPlanU8};
use plib::tmp::tempdir;

fn cat_test(args: &[&str], stdin: &str, expected_out: &str) {
    let str_args: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    run_test(TestPlan {
        cmd: String::from("cat"),
        args: str_args,
        stdin_data: String::from(stdin),
        expected_out: String::from(expected_out),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn cat_no_args_reads_stdin() {
    cat_test(&[], "hello\nworld\n", "hello\nworld\n");
}

#[test]
fn cat_dash_is_stdin() {
    cat_test(&["-"], "abc\n", "abc\n");
}

#[test]
fn cat_u_flag_is_noop() {
    cat_test(&["-u"], "data\n", "data\n");
}

#[test]
fn cat_multiple_dash_reads_stdin_once() {
    // The first '-' consumes all of stdin; the second sees EOF. stdin is not
    // closed/reopened, so the result is the full input followed by nothing.
    cat_test(&["-", "-"], "line\n", "line\n");
}

#[test]
fn cat_file_operand() {
    let dir = tempdir().unwrap();
    let f = dir.path().join("file");
    fs::write(&f, "file contents\n").unwrap();
    cat_test(&[f.to_str().unwrap()], "", "file contents\n");
}

#[test]
fn cat_dash_among_files() {
    let dir = tempdir().unwrap();
    let f = dir.path().join("among");
    fs::write(&f, "A\n").unwrap();
    // file, then stdin (-), in order.
    cat_test(&[f.to_str().unwrap(), "-"], "B\n", "A\nB\n");
}

#[test]
fn cat_missing_file_sets_exit_and_continues() {
    let dir = tempdir().unwrap();
    let good = dir.path().join("good");
    fs::write(&good, "ok\n").unwrap();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("cat"),
            args: vec![
                String::from("/no/such/file/xyz"),
                good.to_str().unwrap().to_string(),
            ],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 1,
        },
        |_, output| {
            // The good file is still written, and the exit status is non-zero.
            assert_eq!(String::from_utf8_lossy(&output.stdout), "ok\n");
            assert_eq!(output.status.code(), Some(1));
            assert!(String::from_utf8_lossy(&output.stderr).contains("/no/such/file/xyz"));
        },
    );
}

/// `cat` of a large file into a closed pipe must die by SIGPIPE.
/// See `plib::testing::assert_dies_by_sigpipe`.
#[test]
fn test_cat_dies_by_sigpipe_on_a_closed_pipe() {
    let dir = tempdir().unwrap();
    let big = dir.path().join("big");
    let body: String = (0..200_000).map(|n| format!("line {n}\n")).collect();
    std::fs::write(&big, body).unwrap();

    plib::testing::assert_dies_by_sigpipe("cat", &[big.to_str().unwrap()]);
}

/// Started with SIGPIPE ignored, `cat` into a closed pipe reports the write
/// error and exits 1 instead of dying by the signal it was told to ignore.
/// See `plib::testing::assert_epipe_when_sigpipe_ignored`.
#[test]
fn test_cat_reports_epipe_when_sigpipe_is_ignored() {
    let dir = tempdir().unwrap();
    let f = dir.path().join("f");
    std::fs::write(&f, "hello\n").unwrap();

    plib::testing::assert_epipe_when_sigpipe_ignored("cat", &[f.to_str().unwrap()], 1);
}

/// Run `cat` with `args` on `stdin`, asserting stdout byte for byte.
fn cat_test_bytes(args: &[&str], stdin: &[u8], expected_out: &[u8]) {
    run_test_u8(TestPlanU8 {
        cmd: String::from("cat"),
        args: args.iter().map(|s| s.to_string()).collect(),
        stdin_data: stdin.to_vec(),
        expected_out: expected_out.to_vec(),
        expected_err: Vec::new(),
        expected_exit_code: 0,
    });
}

/// Every byte value, 0 to 255, in order.
fn all_bytes() -> Vec<u8> {
    (0..=255).collect()
}

/// GNU cat 9.4's `-v` rendering of `all_bytes()`: control characters as
/// `^X`, DEL as `^?`, and bytes above 127 as `M-` and the rendering of the
/// byte 128 below; tab and newline as they are (but `M-^I` and `M-^J`).
const ALL_BYTES_V: &str = "^@^A^B^C^D^E^F^G^H\t\n^K^L^M^N^O^P^Q^R^S^T^U^V^W^X^Y^Z^[^\\^]^^^_ !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~^?M-^@M-^AM-^BM-^CM-^DM-^EM-^FM-^GM-^HM-^IM-^JM-^KM-^LM-^MM-^NM-^OM-^PM-^QM-^RM-^SM-^TM-^UM-^VM-^WM-^XM-^YM-^ZM-^[M-^\\M-^]M-^^M-^_M- M-!M-\"M-#M-$M-%M-&M-'M-(M-)M-*M-+M-,M--M-.M-/M-0M-1M-2M-3M-4M-5M-6M-7M-8M-9M-:M-;M-<M-=M->M-?M-@M-AM-BM-CM-DM-EM-FM-GM-HM-IM-JM-KM-LM-MM-NM-OM-PM-QM-RM-SM-TM-UM-VM-WM-XM-YM-ZM-[M-\\M-]M-^M-_M-`M-aM-bM-cM-dM-eM-fM-gM-hM-iM-jM-kM-lM-mM-nM-oM-pM-qM-rM-sM-tM-uM-vM-wM-xM-yM-zM-{M-|M-}M-~M-^?";

#[test]
fn cat_v_shows_nonprinting_bytes() {
    cat_test_bytes(&["-v"], &all_bytes(), ALL_BYTES_V.as_bytes());
    cat_test_bytes(&["--show-nonprinting"], b"a\x1b[0m\r\n", b"a^[[0m^M\n");
    cat_test_bytes(&["-v"], b"\xc3\xa9\n", b"M-CM-)\n");
}

/// `-e` is `-v` with a `$` at the end of each line; a final line with no
/// newline gets none.
#[test]
fn cat_e_marks_line_ends() {
    let expected = ALL_BYTES_V.replacen("\t\n", "\t$\n", 1);
    cat_test_bytes(&["-e"], &all_bytes(), expected.as_bytes());
    cat_test_bytes(&["-e"], b"a\tb\n\nc", b"a\tb$\n$\nc");
    // GNU patch's tests run `cat -ve a.rej`.
    cat_test_bytes(&["-ve"], b"x\r\n", b"x^M$\n");
    cat_test_bytes(&["-u", "-e"], b"x\n", b"x$\n");
}

/// The rendering does not depend on where a read happens to end.
#[test]
fn cat_v_across_file_operands() {
    let dir = tempdir().unwrap();
    let (a, b) = (dir.path().join("a"), dir.path().join("b"));
    fs::write(&a, b"one\x7f").unwrap();
    fs::write(&b, b"\ntwo\xff\n").unwrap();
    cat_test_bytes(
        &["-e", a.to_str().unwrap(), b.to_str().unwrap()],
        b"",
        b"one^?$\ntwoM-^?$\n",
    );
}
