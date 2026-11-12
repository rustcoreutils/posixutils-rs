//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! vi motions: the commands that had none (`W B E + - <CR> <space> ^N ^P
//! ^H ^J`), the motions operators lacked (`/ ? n N [[ ]] ; ,`), and the
//! boundaries `{ } [[ ]]` land on (vi 120909-120992).

use vi_rs::Editor;

fn run(text: &str, keys: &str) -> (String, usize, usize) {
    let mut e = Editor::new_headless();
    e.set_buffer_text(text);
    e.execute_keys(keys).unwrap();
    let c = e.get_cursor();
    (e.get_buffer_text(), c.line, c.column)
}

fn at(text: &str, keys: &str) -> (usize, usize) {
    let (_, l, c) = run(text, keys);
    (l, c)
}

fn text(text: &str, keys: &str) -> String {
    run(text, keys).0
}

#[test]
fn test_bigword_commands() {
    assert_eq!(at("a.b c.d e\n", "W"), (1, 4));
    assert_eq!(at("a.b c.d e\n", "$B"), (1, 4));
    assert_eq!(at("a.b c.d e\n", "$BB"), (1, 0));
    assert_eq!(at("a.b c.d e\n", "E"), (1, 2));
}

#[test]
fn test_line_motions() {
    let t = "a\n  b\n  c\n";
    assert_eq!(at(t, "+"), (2, 2));
    assert_eq!(at(t, "\r"), (2, 2));
    assert_eq!(at(t, "2+"), (3, 2));
    assert_eq!(at(t, "G-"), (2, 2));
    assert_eq!(at(t, "\x0e"), (2, 0), "^N is j");
    assert_eq!(at(t, "G\x10"), (2, 2), "^P is k");
    // (`\n` here is <CR>, as a map's right-hand side reads; a typed ^J is
    // the parser's, tested there.)
}

#[test]
fn test_line_motion_past_the_end_is_an_error() {
    let mut e = Editor::new_headless();
    e.set_buffer_text("a\nb\n");
    e.execute_keys("5+").unwrap();
    assert_eq!(e.get_cursor().line, 1);
    assert!(e.is_error_message());
}

#[test]
fn test_space_and_ctrl_h() {
    assert_eq!(at("abc\n", "  "), (1, 2));
    assert_eq!(at("abc\n", "$\x08"), (1, 1));
}

#[test]
fn test_operator_with_search_motions() {
    assert_eq!(text("ab cd ef\n", "d/ef\r"), "ef\n");
    assert_eq!(text("ab cd ef\n", "$d?cd\r"), "ab f\n");
    assert_eq!(text("x a x b\n", "/x\r1Gdn"), "x b\n");
    assert_eq!(text("a x b x c\n", "/x\r0dn"), "x b x c\n");
    assert_eq!(text("a x b x c\n", "/x\r$dN"), "a x b c\n");
    // With a line offset the region is whole lines.
    assert_eq!(text("a\nb\nc\nd\n", "d/c/-\r"), "c\nd\n");
    // `.` repeats the search motion.
    assert_eq!(text("a x b x c x\n", "d/x\r.."), "x\n");
}

#[test]
fn test_operator_with_find_repeat() {
    // f-type motions are inclusive.
    assert_eq!(text("a.b.c.d\n", "f.d;"), "ac.d\n");
    assert_eq!(text("a.b.c.d\n", "$F.d,"), "a.b.c.d\n");
    assert_eq!(text("a.b.c.d\n", "$F.0d,"), "b.c.d\n");
}

#[test]
fn test_operator_with_sections() {
    // 120914-120917: line-wise, through the boundary line.
    assert_eq!(text("a\nb\n{\nc\n", "d]]"), "c\n");
    assert_eq!(text("x\n{\nb\nc\n", "Gd[["), "x\n");
}

#[test]
fn test_paragraph_lands_on_the_blank_line() {
    let t = "a\nb\n\nc\n\n\nd\n";
    assert_eq!(at(t, "}"), (3, 0));
    assert_eq!(
        at(t, "}}"),
        (5, 0),
        "a run of blank lines starts the boundary"
    );
    assert_eq!(at(t, "G{"), (5, 0));
    assert_eq!(at(t, "G{{"), (3, 0));
    // Past the last boundary: the last character of the buffer.
    assert_eq!(at("a\nbc\n", "}"), (2, 1));
}

#[test]
fn test_delete_paragraph_is_line_wise_through_the_boundary() {
    // 120914-120917: from a line start, d} takes whole lines up to and
    // including the boundary line.
    assert_eq!(text("a\nb\n\nc\n", "d}"), "c\n");
    // From mid-line, up to but not including the boundary's first
    // character -- so through the newline before it.
    assert_eq!(text("ab\nb\n\nc\n", "ld}"), "a\nc\n");
}

#[test]
fn test_paragraph_and_section_macros_follow_the_options() {
    let t = "a\n.PP\nb\n.XY\nc\n";
    assert_eq!(at(t, "}"), (2, 0));
    assert_eq!(at(t, ":set paragraphs=XY\r}"), (4, 0));
    let t = "a\n.SH\nb\n.QQ\nc\n";
    assert_eq!(at(t, "]]"), (2, 0));
    assert_eq!(at(t, ":set sections=QQ\r]]"), (4, 0));
    // A one-letter macro matches a pair ending in a space.
    assert_eq!(at("a\n.H\nb\n", "]]"), (2, 0));
}

#[test]
fn test_count_on_g_motion() {
    assert_eq!(text("a\nb\nc\nd\n", "d2G"), "c\nd\n");
}

#[test]
fn test_huge_count_does_not_panic() {
    let mut e = Editor::new_headless();
    e.set_buffer_text("a\n");
    e.execute_keys("99999999999999999999999j").unwrap();
    assert_eq!(e.get_cursor().line, 1);
}

#[test]
fn test_replace_with_newline() {
    // 121498-121506: count characters become count new lines.
    assert_eq!(run("abcd\n", "lr\r"), ("a\ncd\n".to_string(), 2, 0));
    assert_eq!(run("abcd\n", "l2r\r"), ("a\n\nd\n".to_string(), 3, 0));
    assert_eq!(text("  abcd\n", ":set ai\r3lr\r"), "  a\n  cd\n");
    assert_eq!(text("abcd\n", "lr\ru"), "abcd\n");
}

#[test]
fn test_control_motions_as_operator_motions() {
    assert_eq!(text("a\nb\nc\n", "d\x0e"), "c\n", "d^N is dj");
    assert_eq!(text("a\nb\nc\n", "Gd\x10"), "a\n", "d^P is dk");
    assert_eq!(text("a\nb\nc\n", "d\r"), "c\n", "d<CR> is d+");
}

#[test]
fn test_count_before_scroll_is_not_left_for_the_next_command() {
    let t: String = (1..=100).map(|n| format!("{n}\n")).collect();
    // ^F takes the 2; the `x` after it deletes one character, not two.
    let mut e = Editor::new_headless();
    e.set_buffer_text("abc\n");
    e.execute_keys("2\x06x").unwrap();
    assert_eq!(e.get_buffer_text(), "bc\n");
    // Two screens are further than one.
    let one = at(&t, "\x06").0;
    let two = at(&t, "2\x06").0;
    assert!(two > one, "2^F {two} vs ^F {one}");
}

#[test]
fn test_count_on_backward_boundary_from_mid_line() {
    // Each pass moves on: the cursor's own line counts only the first time.
    assert_eq!(at("a\n{\nb\n{ x\n", "G$2[["), (2, 0));
}

#[test]
fn test_empty_pattern_search_motion_goes_the_prompt_way() {
    // `?` with the last pattern searches backward, whatever `/` set.
    assert_eq!(text("foo a foo b foo\n", "/foo\r/\rhd?\r"), "foo a  foo\n");
}

#[test]
fn test_dot_repeats_an_offset_search_motion() {
    assert_eq!(text("a\nb\nx\nc\nd\nx\n", "d/x/-\r."), "x\n");
}

#[test]
fn test_count_on_dot_reaches_g() {
    assert_eq!(text("1\n2\n3\n4\n5\n", "jdGu1G3."), "4\n5\n");
}

#[test]
fn test_huge_counts_terminate() {
    let mut e = Editor::new_headless();
    e.set_buffer_text("a x\nb x\n");
    e.execute_keys("/x\r").unwrap();
    // `n` from off a match: the cycle starts at the first match.
    e.execute_keys("1G0").unwrap();
    for keys in [
        "99999999999999999999\x04",
        "99999999999999999999\x06",
        "99999999999999999999\x05",
        "99999999999n",
        "99999999999999999999H",
        "99999999999999999999l",
        "99999999999999999999$",
        "99999999999999999999~",
        "99999999999999999999x",
    ] {
        e.execute_keys(keys).unwrap();
    }
}

#[test]
fn test_erasing_the_search_prompt_leaves_it() {
    let mut e = Editor::new_headless();
    e.set_buffer_text("a\n");
    // Backspace over `/`, then `x`: a command, not text for the prompt.
    e.execute_keys("ab\x1b/\x7fx").unwrap();
    assert_eq!(e.get_buffer_text(), "a\n");
}

#[test]
fn test_operator_find_sets_last_find() {
    assert_eq!(at("ab,cd,ef\n", "dt,;"), (1, 2));
    assert_eq!(text("ab,cd,ef\n", "dt,ld;"), ",,ef\n");
}

#[test]
fn test_replace_with_del_is_del() {
    assert_eq!(text("ab\n", "r\x7f"), "\x7fb\n");
}

#[test]
fn test_count_on_n_wraps_around() {
    // Matches at (1,2) and (2,2): from the start, 3n is the first again.
    assert_eq!(at("a x\nb x\n", "/x\r1G03n"), (1, 2));
}
