//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `-o` / `--only-matching` (GNU): each non-empty matched part of a selected line on a line of
//! its own.  util-linux's test suite (`tests/ts/libmount/debug`) runs `grep -o '0x.*'`.  Every
//! expected output here is what GNU grep 3.11 writes.

use super::grep_test;
use plib::testing::run_test_base_with_env;

/// `a` on lines 1, 4 and 9 of ten.
const SPARSE: &str = "1 a\n2\n3\n4 a\n5\n6\n7\n8\n9 a\n10\n";

/// Matches are leftmost, then longest, and do not overlap; the search goes on after each one,
/// and `^` matches only at the start of the line.
#[test]
fn test_grep_only_matching() {
    grep_test(&["-o", "a[0-9]"], "a1 a2\nb\na3\n", "a1\na2\na3\n", "", 0);
    grep_test(
        &["--only-matching", "a[0-9]"],
        "a1 a2\nb\n",
        "a1\na2\n",
        "",
        0,
    );
    grep_test(&["-o", "^a"], "aaa\n", "a\n", "", 0);
    grep_test(&["-o", "b$"], "abab\n", "b\n", "", 0);
    grep_test(&["-oE", "a|aa"], "xaay\n", "aa\n", "", 0);
    grep_test(&["-o", "0x.*"], "id 0x1f: x\nnone\n", "0x1f: x\n", "", 0);
    grep_test(&["-o", "z"], "abc\n", "", "", 1);
}

/// Across several patterns the earliest match wins, and of those starting there the longest.
#[test]
fn test_grep_only_matching_several_patterns() {
    grep_test(
        &["-o", "-e", "ab", "-e", "abc", "-e", "bcd"],
        "abcd abd\n",
        "abc\nab\n",
        "",
        0,
    );
    grep_test(&["-o", "-e", "b", "-e", "abc"], "abcd\n", "abc\n", "", 0);
    grep_test(
        &["-o", "-e", "a", "-e", "ab", "-e", "b"],
        "ab\n",
        "ab\n",
        "",
        0,
    );
}

/// An empty match is not written, but its line is still selected.
#[test]
fn test_grep_only_matching_empty_match() {
    grep_test(&["-o", "x*"], "abc\nxx\n", "xx\n", "", 0);
    grep_test(&["-o", "x*"], "abc\n", "", "", 0);
    grep_test(&["-o", "b*"], "abc\n", "b\n", "", 0);
}

/// The file name and line number go before each match; the line number repeats.
#[test]
fn test_grep_only_matching_prefixes() {
    grep_test(
        &["-onH", "a[0-9]"],
        "a1 a2\nb\na3\n",
        "(standard input):1:a1\n(standard input):1:a2\n(standard input):3:a3\n",
        "",
        0,
    );
}

#[test]
fn test_grep_only_matching_fixed_strings() {
    grep_test(&["-oF", "aa"], "aaaa\n", "aa\naa\n", "", 0);
    grep_test(
        &["-oF", "-e", "ab", "-e", "abc", "-e", ""],
        "abcab\n",
        "abc\nab\n",
        "",
        0,
    );
}

/// Under -w only whole-word matches are written, under -x only whole lines, and under -i the
/// text written is the line's own.
#[test]
fn test_grep_only_matching_word_line_case() {
    grep_test(
        &["-ow", "foo"],
        "foo foobar xfoo foo\n",
        "foo\nfoo\n",
        "",
        0,
    );
    grep_test(&["-ow", "ab*"], "abbx a ab\n", "a\nab\n", "", 0);
    grep_test(&["-ow", "b*"], "a b\n", "b\n", "", 0);
    grep_test(&["-owF", "foo"], "foo xfoo foo_ foo\n", "foo\nfoo\n", "", 0);
    grep_test(
        &["-ox", "foo.*"],
        "foo\nfoo bar\nxfoo\n",
        "foo\nfoo bar\n",
        "",
        0,
    );
    grep_test(&["-oxw", "foo"], "foo\n", "foo\n", "", 0);
    grep_test(&["-oxiF", "foo"], "Foo\n", "Foo\n", "", 0);
    grep_test(&["-oi", "foo"], "Foo FOO fo\n", "Foo\nFOO\n", "", 0);
    grep_test(&["-oiF", "foo"], "Foo FOO fo\n", "Foo\nFOO\n", "", 0);
}

/// -c, -l and -q are unchanged by -o.
#[test]
fn test_grep_only_matching_count_list_quiet() {
    grep_test(&["-oc", "a"], "aa\nb\na\n", "2\n", "", 0);
    grep_test(&["-ol", "a"], "aa\n", "(standard input)\n", "", 0);
    grep_test(&["-oq", "a"], "aa\n", "", "", 0);
}

/// A selected line under -v has no match to write; a context line, which under -v is one that
/// matches, has its matches written with `-`, as GNU does.
#[test]
fn test_grep_only_matching_invert() {
    grep_test(&["-ov", "a"], SPARSE, "", "", 0);
    grep_test(&["-ov", "."], "a\n", "", "", 1);
    grep_test(&["-ovn", "-C1", "a"], SPARSE, "1-a\n4-a\n9-a\n", "", 0);
    grep_test(&["-ov", "-A0", "a"], "b\na\nb\nb\nb\na\n", "--\n", "", 0);
}

/// Context lines write nothing, but groups of lines are separated by `--` as without -o.
#[test]
fn test_grep_only_matching_context() {
    grep_test(&["-o", "-A1", "a"], SPARSE, "a\n--\na\n--\na\n", "", 0);
    grep_test(&["-o", "-B1", "a"], SPARSE, "a\n--\na\n--\na\n", "", 0);
    grep_test(&["-o", "-C1", "a"], SPARSE, "a\na\n--\na\n", "", 0);
    grep_test(&["-on", "-C1", "a"], SPARSE, "1:a\n4:a\n--\n9:a\n", "", 0);
    grep_test(&["-o", "-C3", "a"], SPARSE, "a\na\na\n", "", 0);
    grep_test(
        &["-o", "-A0", "-e", "a", "-e", "^$"],
        "a\n\nx\na\n",
        "a\n--\na\n",
        "",
        0,
    );
}

/// Lines need not be text: in the C locale every byte is a character.
#[test]
fn test_grep_only_matching_bytes() {
    let run = |locale: &str, pattern: &str| {
        let args = vec![String::from("-o"), String::from(pattern)];
        run_test_base_with_env("grep", &args, b"ab\xffab\x80a\n", &[("LC_ALL", locale)])
    };
    let out = run("C", "b.");
    assert_eq!(out.stdout, b"b\xff\nb\x80\n");
    assert_eq!(out.status.code(), Some(0));
    if let Some(utf8) = plib::testing::utf8_locale() {
        let out = run(&utf8, "a");
        assert_eq!(out.stdout, b"a\na\na\n");
        assert_eq!(out.status.code(), Some(0));
        let out = run(&utf8, "b.");
        assert_eq!(out.stdout, b"");
        assert_eq!(out.status.code(), Some(1));
    }
}
