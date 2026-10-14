//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `-z` / `--null-data` (GNU): input and output records end in NUL instead of newline.
//! libxcrypt's debian/rules builds libxcrypt-source with
//! `find * -print0 | egrep --null-data --invert-match '...' | sort -z | tar --null -T -`.
//! Every expected output here is what GNU grep 3.11 writes.

use super::grep_test;

/// libxcrypt's own form: `$` ends a record, `-v` keeps the others.
#[test]
fn test_grep_null_data_invert() {
    grep_test(
        &[
            "-E",
            "--null-data",
            "--invert-match",
            "^(INSTALL$|debian/files$)",
        ],
        "INSTALL\0README\0debian/files\0INSTALL.md\0",
        "README\0INSTALL.md\0",
        "",
        0,
    );
    grep_test(&["-z", "-v", "c"], "ab\0cd\0ef\n", "ab\0ef\n\0", "", 0);
}

/// A newline inside a record is an ordinary character, and a last record without its NUL
/// gets one on output.
#[test]
fn test_grep_null_data_records() {
    grep_test(&["-z", "a.b"], "a\nb\0c\0", "a\nb\0", "", 0);
    grep_test(&["-z", "."], "x\0y", "x\0y\0", "", 0);
    grep_test(&["-z", "-c", "."], "ab\0cd\0", "2\n", "", 0);
}

/// `^` and `$` anchor at the ends of a record only, never at a newline inside it.
#[test]
fn test_grep_null_data_anchors() {
    grep_test(&["-z", "^b"], "a\nb\0c\0", "", "", 1);
    grep_test(&["-z", "a$"], "a\nb\0c\0", "", "", 1);
    grep_test(&["-z", "-E", "c$|^d"], "ab\0c\nd\0ef\0", "", "", 1);
    grep_test(&["-z", "-x", "c.d"], "ab\0c\nd\0ef\0", "c\nd\0", "", 0);
    grep_test(&["-z", "-F", "-x", "c"], "ab\0c\nd\0ef\0", "", "", 1);
}

/// A newline separates patterns still; `-w`, `-F` and `-i` work within a record, a newline
/// being no word character.
#[test]
fn test_grep_null_data_patterns() {
    grep_test(&["-z", "-e", "q\nef"], "ab\0C\nd\0ef\0", "ef\0", "", 0);
    grep_test(&["-z", "-w", "c"], "ab\0c\nd\0ef\0", "c\nd\0", "", 0);
    grep_test(&["-z", "-F", "c\nd"], "ab\0c\nd\0ef\0", "c\nd\0", "", 0);
    grep_test(&["-z", "-i", "-F", "c"], "ab\0C\nd\0ef\0", "C\nd\0", "", 0);
    grep_test(&["-z", "-i", "c.D"], "ab\0C\nd\0ef\0", "C\nd\0", "", 0);
}

/// `-o` and `-n` output ends each part or line in NUL; `-c` and `-l` still end in newline.
#[test]
fn test_grep_null_data_output_modes() {
    grep_test(&["-z", "-o", "c.d"], "ab\0c\nd\0ef\0", "c\nd\0", "", 0);
    grep_test(&["-z", "-o", "[a-c]"], "ab\0cd\0", "a\0b\0c\0", "", 0);
    grep_test(
        &["-z", "-n", "-H", "d"],
        "ab\0c\nd\0",
        "(standard input):2:c\nd\0",
        "",
        0,
    );
    grep_test(&["-z", "-l", "."], "ab\0cd\0", "(standard input)\n", "", 0);
    grep_test(&["-z", "-c", "-v", "ef"], "ab\0c\nd\0ef\0", "2\n", "", 0);
    grep_test(&["-z", "-c", ""], "abc\0\n", "2\n", "", 0);
    grep_test(&["-z", "-q", "."], "ab\0", "", "", 0);
}

/// Context lines end in NUL, but the `--` between groups ends in a newline, as in GNU grep.
#[test]
fn test_grep_null_data_context() {
    grep_test(
        &["-z", "-A1", "-e", "a", "-e", "e"],
        "a\0b\0c\0d\0e\0f\0",
        "a\0b\0--\ne\0f\0",
        "",
        0,
    );
    grep_test(
        &["-z", "-B1", "-n", "e"],
        "ab\0cd\0ef\0gh\0",
        "2-cd\x003:ef\0",
        "",
        0,
    );
}
