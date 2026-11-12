//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! ex commands typed in vi: `:` lines against vi's own `/` prompt, and
//! failures that stay on the status line.

use vi_rs::Editor;

fn editor(text: &str) -> Editor {
    let mut e = Editor::new_headless();
    e.set_buffer_text(text);
    e
}

#[test]
fn test_colon_line_number_past_either_end_clamps() {
    let mut e = editor("a\nb\nc\n");
    e.execute_keys(":999\r").unwrap();
    assert_eq!(e.get_cursor().line, 3);
    e.execute_keys(":0\r").unwrap();
    assert_eq!(e.get_cursor().line, 1);
}

#[test]
fn test_colon_search_address_runs_its_command() {
    let mut e = editor("a\nb\nc\n");
    e.execute_keys(":/b/d\r").unwrap();
    assert_eq!(e.get_buffer_text(), "a\nc\n");
}

#[test]
fn test_slash_prompt_is_a_search() {
    let mut e = editor("a\nb/d\nc\n");
    // At the `/` prompt `b/d` is the pattern `b` and an offset: still a
    // search, never an ex command.
    e.execute_keys("/b\\/d\r").unwrap();
    assert_eq!(e.get_cursor().line, 2);
    assert_eq!(e.get_buffer_text(), "a\nb/d\nc\n");
}

#[test]
fn test_failed_command_does_not_change_the_search_pattern() {
    let mut e = editor("b\nc\nb\nc\n");
    e.execute_keys("/b\r").unwrap();
    // A line that fails to parse runs nothing, its address included.
    e.execute_keys(":/c/zzz\r").unwrap();
    e.execute_keys("1Gn").unwrap();
    assert_eq!(e.get_cursor().line, 3, "n searched for something else");
}

#[test]
fn test_failed_ampersand_stays_on_the_status_line() {
    let mut e = editor("foo\nbar\n");
    e.execute_keys(":s/foo/x/\r").unwrap();
    // No `foo` on line 2: an error message, not the end of the session.
    assert!(e.execute_keys("j&").is_ok());
    assert_eq!(e.get_buffer_text(), "x\nbar\n");
}

#[test]
fn test_global_keeps_the_search_direction() {
    let mut e = editor("x\nfoo\nx\nfoo\nx\n");
    e.execute_keys("G?foo\r").unwrap();
    // The global's RE becomes the last RE; the direction stays backward.
    e.execute_keys(":g/x/p\r").unwrap();
    assert_eq!(e.get_cursor().line, 5);
    e.execute_keys("n").unwrap();
    // Backward from line 5 is line 3; forward would have wrapped to 1.
    assert_eq!(e.get_cursor().line, 3);
}
