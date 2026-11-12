//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `:g`/`:v` and `:s` details: the last RE, the edit options, errors, and
//! the replacement escapes; and `:l`.

use super::addresses::{ex, ex_env, ok};

const ABCD: &str = "a\nb\nc\nd\n";

#[test]
fn test_global_empty_pattern_is_the_last_re() {
    // POSIX 94995: `//` is the last RE, not one that matches every line.
    assert_eq!(ok(ABCD, "/c/\ng//d\n%p\nq!\n"), "c\na\nb\nd\n");
}

#[test]
fn test_global_pattern_becomes_the_last_re() {
    assert_eq!(ok(ABCD, "g/b/s//X/\n%p\nq!\n"), "a\nX\nc\nd\n");
}

#[test]
fn test_global_honours_ignorecase_and_nomagic() {
    assert_eq!(ok(ABCD, "set ic\ng/B/d\n%p\nq!\n"), "a\nc\nd\n");
    assert_eq!(ok("a.c\nabc\n", "set nomagic\ng/a.c/d\n%p\nq!\n"), "abc\n");
}

#[test]
fn test_global_stops_at_the_first_error() {
    // POSIX 95005-95008: an error ends the global, and is an error.
    let (out, _, code, _) = ex(ABCD, "g/./1,9d\n%p\nq!\n");
    assert_ne!(code, 0);
    assert_eq!(out, "");
}

#[test]
fn test_global_stops_when_the_buffer_is_replaced() {
    let (_, _, code, _) = ex(ABCD, "g/./e! f\nq!\n");
    assert_ne!(code, 0, "a global that re-edits must stop with an error");
}

#[test]
fn test_substitute_honours_nomagic() {
    assert_eq!(
        ok("a.c\nabc\n", "set nomagic\n%s/a.c/X/\n%p\nq!\n"),
        "X\nabc\n"
    );
}

#[test]
fn test_backslash_n_in_replacement_is_n() {
    // POSIX 95724: `\n` is a literal `n`; only `\<newline>` splits the line.
    assert_eq!(ok("ab\n", "s/a/a\\n/\n%p\nq!\n"), "anb\n");
    assert_eq!(ok("ab\n", "s/a/a\\\n/\n%p\nq!\n"), "a\nb\n");
}

#[test]
fn test_list_folds_long_lines() {
    let line = "x".repeat(100);
    let out = ok(&format!("{line}\n"), "l\nq!\n");
    let rows: Vec<&str> = out.lines().collect();
    assert!(rows.len() > 1, "not folded: {out:?}");
    assert!(rows[..rows.len() - 1].iter().all(|r| r.ends_with('\\')));
    assert!(rows.last().unwrap().ends_with('$'));
    let joined: String = rows
        .iter()
        .map(|r| r.trim_end_matches('\\').trim_end_matches('$'))
        .collect();
    assert_eq!(joined, line);
}

#[test]
fn test_list_shows_printable_multibyte_in_a_utf8_locale() {
    let Some(utf8) = plib::testing::utf8_locale() else {
        return; // no UTF-8 locale installed here
    };
    let out = |locale: &str| ex_env("caf\u{e9}\n", "l\nq!\n", &[("LC_ALL", locale)]).0;
    assert_eq!(out(&utf8), "caf\u{e9}$\n");
    assert_eq!(out("C"), "caf\\303\\251$\n");
}

#[test]
fn test_nomagic_replacement_ampersand_and_tilde() {
    // POSIX 95712-95714: with nomagic, `&` and `~` are literal and the
    // escaped forms are special.
    assert_eq!(ok("a\n", "set nomagic\ns/a/&&/\np\nq!\n"), "&&\n");
    assert_eq!(ok("a\n", "set nomagic\ns/a/\\&\\&/\np\nq!\n"), "aa\n");
}

#[test]
fn test_global_stops_mid_list_when_the_buffer_is_replaced() {
    let (_, _, code, file) = ex(ABCD, "g/./e! f|1d\nw\nq!\n");
    assert_ne!(code, 0);
    assert_eq!(file, ABCD, "the 1d after e! must not run");
}

#[test]
fn test_numbered_list_folds_within_the_width() {
    let line = "x".repeat(200);
    let out = ok(&format!("{line}\n"), "set nu\nl\nq!\n");
    assert!(out.lines().all(|r| r.chars().count() <= 80), "{out:?}");
}
