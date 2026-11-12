//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Counts on the text input commands: `[count] a i A I o O R C` repeat
//! the input (vi 121193-121204, 121352-121361, 121423-121436, 121507).

use vi_rs::Editor;

fn run(text: &str, keys: &str) -> (String, usize, usize) {
    let mut e = Editor::new_headless();
    e.set_buffer_text(text);
    e.execute_keys(keys).unwrap();
    let c = e.get_cursor();
    (e.get_buffer_text(), c.line, c.column)
}

fn text(text: &str, keys: &str) -> String {
    run(text, keys).0
}

#[test]
fn test_count_insert_repeats_text() {
    assert_eq!(text("Z\n", "3ix\x1b"), "xxxZ\n");
    assert_eq!(text("Z\n", "2iab\x1b"), "ababZ\n");
}

#[test]
fn test_count_append_repeats_text() {
    assert_eq!(text("Z\n", "2aab\x1b"), "Zabab\n");
    assert_eq!(text("ab\nc\n", "3A-\x1b"), "ab---\nc\n");
    assert_eq!(text("  ab\n", "2I-\x1b"), "  --ab\n");
}

#[test]
fn test_count_insert_cursor_on_last_inserted_char() {
    let (_, line, col) = run("Z\n", "2iab\x1b");
    assert_eq!((line, col), (1, 3));
}

#[test]
fn test_count_open_repeats_lines() {
    assert_eq!(text("a\nb\n", "3ox\x1b"), "a\nx\nx\nx\nb\n");
    assert_eq!(text("a\nb\n", "j2Ox\x1b"), "a\nx\nx\nb\n");
}

#[test]
fn test_count_replace_appends_copies() {
    // R overwrites, then the copies are appended.
    assert_eq!(text("abcdef\n", "2Rxy\x1b"), "xyxyef\n");
}

#[test]
fn test_count_change_to_eol_spans_lines() {
    // `2C` is `2c$`: through the end of the next line.
    assert_eq!(text("ab\ncd\nef\n", "l2CX\x1b"), "aX\nef\n");
}

#[test]
fn test_change_to_eol_fills_registers() {
    // `"aC` is `"ac$`.
    assert_eq!(text("abc\n", "l\"aCX\x1b$\"ap"), "aXbc\n");
}

#[test]
fn test_count_insert_is_one_undo() {
    assert_eq!(text("Z\n", "3ix\x1bu"), "Z\n");
    assert_eq!(text("a\nb\n", "3ox\x1bu"), "a\nb\n");
}

#[test]
fn test_multi_line_insert_undoes() {
    assert_eq!(text("Z\n", "ia\rb\x1bu"), "Z\n");
    assert_eq!(text("Z\n", "2ia\rb\x1bu"), "Z\n");
}

#[test]
fn test_dot_repeats_counted_insert() {
    assert_eq!(text("Z\n", "2ix\x1b."), "xxxxZ\n");
    assert_eq!(text("Z\n", "2ix\x1b3."), "xxxxxZ\n");
}

#[test]
fn test_replace_overwrites_and_erase_restores() {
    assert_eq!(text("abcd\n", "Rxy\x1b"), "xycd\n");
    // An erased overwrite puts the original back.
    assert_eq!(text("abcd\n", "Rxy\x08\x1b"), "xbcd\n");
    // Past the end, R appends.
    assert_eq!(text("ab\n", "lRxyz\x1b"), "axyz\n");
}

#[test]
fn test_replace_is_one_undo() {
    assert_eq!(text("abcd\n", "2Rxy\x1bu"), "abcd\n");
}

#[test]
fn test_dot_repeats_counted_open() {
    assert_eq!(text("a\n", "2ox\x1b."), "a\nx\nx\nx\nx\n");
}

#[test]
fn test_erase_does_not_join_above_the_insert_line() {
    // ^H at the start of the line the session began on stays there.
    assert_eq!(text("a\nb\n", "ji\x08x\x1b"), "a\nxb\n");
}

#[test]
fn test_dot_repeats_change_to_eol() {
    assert_eq!(text("ab\ncd\n", "lCX\x1bj."), "aX\ncX\n");
    assert_eq!(text("ab cd\nef\n", "cwX\x1bw."), "X X\nef\n");
}

#[test]
fn test_change_to_eol_on_empty_line_enters_input() {
    assert_eq!(text("\nb\n", "CX\x1b"), "X\nb\n");
}

#[test]
fn test_each_change_is_its_own_undo() {
    assert_eq!(text("ab\ncd\n", "lCX\x1bjlCX\x1bu"), "aX\ncd\n");
    assert_eq!(text("ab cd\n", "cwX\x1bwcwY\x1bu"), "X cd\n");
}

#[test]
fn test_failed_dot_keeps_the_change() {
    // `cj` cannot run on the last line; the next `.` still has its text.
    assert_eq!(text("a\nb\nc\nd\n", "cjX\x1bG.k."), "X\nX\n");
}

#[test]
fn test_replace_word_and_line_erase_restore() {
    assert_eq!(text("abcdef\n", "Rxyz\x17\x1b"), "abcdef\n");
    assert_eq!(text("abcdef\n", "Rxyz\x15\x1b"), "abcdef\n");
}

#[test]
fn test_erase_stops_where_input_began() {
    // 121802: erasing before the start point is an error; the text stays.
    assert_eq!(text("abc\n", "lRx\x08\x08\x1b"), "abc\n");
    assert_eq!(text("abc\n", "A\x08\x08\x1b"), "abc\n");
}

#[test]
fn test_dot_repeats_literal_control_characters() {
    // A ^V-quoted ^H is text, and `.` types it as text again (before the
    // `y` ESC left the cursor on).
    assert_eq!(text("Z\n", "ix\x16\x08y\x1b."), "x\x08x\x08yyZ\n");
}

#[test]
fn test_count_open_autoindents_every_copy() {
    assert_eq!(text("  a\n", ":set ai\r2ox\x1b"), "  a\n  x\n  x\n");
}
