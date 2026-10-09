//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `-w` / `--word-regexp` (BSD and GNU), and `--silent`, GNU's long spelling of -q.  binutils
//! runs `grep --word-regexp --silent`.

use super::grep_test;
use plib::testing::{run_test_with_env, TestPlan};

const WORDS: &str = "foo bar\nfoobar\nbar_foo\nfoo-x\n(foo)\nfo\nabbx a\nab cd_e\n\nxfoo foo\n";
const FOO_WORDS: &str = "foo bar\nfoo-x\n(foo)\nxfoo foo\n";

/// A match counts only with no word character (letter, digit, underscore) just before or just
/// after it; each later match on the line is tried in turn.
#[test]
fn test_grep_word_regexp() {
    for args in [
        &["-w", "foo"][..],
        &["--word-regexp", "foo"],
        &["-wF", "foo"],
        &["-wi", "FOO"],
        &["-wE", "foo|bar"],
        &["-w", "-e", "foo", "-e", "oba"],
    ] {
        grep_test(args, WORDS, FOO_WORDS, "", 0);
    }
    grep_test(
        &["-wv", "foo"],
        WORDS,
        "foobar\nbar_foo\nfo\nabbx a\nab cd_e\n\n",
        "",
        0,
    );
    grep_test(&["-wc", "bar"], WORDS, "1\n", "", 0);
    // -x wins over -w.
    grep_test(&["-wx", "foo"], WORDS, "", "", 1);
}

/// A match that fails the test at its end is tried shorter from the same start, as GNU does:
/// `[a-z ]*` matches `ab ` in `ab cd_e`, and an empty match counts where no word character
/// touches it.
#[test]
fn test_grep_word_regexp_shorter_match() {
    grep_test(&["-w", "ab*"], WORDS, "abbx a\nab cd_e\n", "", 0);
    grep_test(
        &["-w", "[a-z ]*"],
        WORDS,
        "foo bar\nfoobar\nfoo-x\n(foo)\nfo\nabbx a\nab cd_e\n\nxfoo foo\n",
        "",
        0,
    );
    grep_test(&["-wc", ""], WORDS, "2\n", "", 0);
    grep_test(&["-w", "^foo$"], "foo\nfoo foo\n", "foo\n", "", 0);
    grep_test(&["-w", "^b"], "a b\nb\n", "b\n", "", 0);
}

/// Letters are the locale's: in a UTF-8 locale `é` is a word character, in the C locale its
/// bytes are not.
#[test]
fn test_grep_word_regexp_locale() {
    let input = "café x\nécafé\n";
    let test = |locale: &str, pattern: &str, out: &str, code: i32| {
        run_test_with_env(
            TestPlan {
                cmd: String::from("grep"),
                args: vec!["-w".into(), pattern.into()],
                stdin_data: String::from(input),
                expected_out: String::from(out),
                expected_err: String::new(),
                expected_exit_code: code,
            },
            &[("LC_ALL", locale)],
        );
    };
    test("C", "caf", input, 0);
    if let Some(utf8) = plib::testing::utf8_locale() {
        test(&utf8, "caf", "", 1);
        test(&utf8, "café", "café x\n", 0);
    }
}

#[test]
fn test_grep_silent() {
    grep_test(&["--word-regexp", "--silent", "foo"], WORDS, "", "", 0);
    grep_test(&["--silent", "nomatch"], WORDS, "", "", 1);
}
