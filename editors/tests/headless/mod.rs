//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Comprehensive integration tests using the headless Editor.
//!
//! These tests exercise the full editor functionality without requiring
//! a real terminal, allowing us to test complete command sequences.

use vi_rs::buffer::Position;
use vi_rs::mode::Mode;
use vi_rs::Editor;

// ============================================================================
// Basic Insert Mode Tests
// ============================================================================

#[test]
fn test_insert_hello_world() {
    let mut editor = Editor::new_headless();

    // Type: iHello World<ESC>
    editor.execute_keys("iHello World\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim(), "Hello World");
    assert!(matches!(editor.get_mode(), Mode::Command));
}

#[test]
fn test_insert_multiple_lines() {
    let mut editor = Editor::new_headless();

    // Type: iLine 1<Enter>Line 2<Enter>Line 3<ESC>
    editor.execute_keys("iLine 1\nLine 2\nLine 3\x1b").unwrap();

    let text = editor.get_buffer_text();
    assert!(text.contains("Line 1"));
    assert!(text.contains("Line 2"));
    assert!(text.contains("Line 3"));
}

#[test]
fn test_append_mode() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello");

    // a appends after cursor, add " World"
    editor.execute_keys("$a World\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim(), "Hello World");
}

#[test]
fn test_append_end_of_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello");

    // A appends at end of line
    editor.execute_keys("A World\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim(), "Hello World");
}

#[test]
fn test_insert_at_beginning() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("World");

    // I inserts at first non-blank
    editor.execute_keys("IHello \x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim(), "Hello World");
}

#[test]
fn test_open_line_below() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Line 1\nLine 3");

    // o opens line below
    editor.execute_keys("oLine 2\x1b").unwrap();

    let text = editor.get_buffer_text();
    assert!(text.contains("Line 1"));
    assert!(text.contains("Line 2"));
    assert!(text.contains("Line 3"));
}

#[test]
fn test_open_line_above() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Line 2\nLine 3");

    // O opens line above
    editor.execute_keys("OLine 1\x1b").unwrap();

    let text = editor.get_buffer_text();
    let lines: Vec<&str> = text.lines().collect();
    assert_eq!(lines[0].trim(), "Line 1");
}

// ============================================================================
// Motion Tests
// ============================================================================

#[test]
fn test_hjkl_motions() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello\nWorld\nTest");
    editor.set_cursor(Position::new(1, 0));

    // Move right
    editor.execute_keys("ll").unwrap();
    assert_eq!(editor.get_cursor().column, 2);

    // Move down
    editor.execute_keys("j").unwrap();
    assert_eq!(editor.get_cursor().line, 2);

    // Move left
    editor.execute_keys("h").unwrap();
    assert_eq!(editor.get_cursor().column, 1);

    // Move up
    editor.execute_keys("k").unwrap();
    assert_eq!(editor.get_cursor().line, 1);
}

#[test]
fn test_word_motions() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two three four");
    editor.set_cursor(Position::new(1, 0));

    // w moves to next word
    editor.execute_keys("w").unwrap();
    assert_eq!(editor.get_cursor().column, 4); // "two"

    // Move again
    editor.execute_keys("w").unwrap();
    assert_eq!(editor.get_cursor().column, 8); // "three"

    // b moves back
    editor.execute_keys("b").unwrap();
    assert_eq!(editor.get_cursor().column, 4); // "two"
}

#[test]
fn test_line_motions() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("  Hello World");
    editor.set_cursor(Position::new(1, 5));

    // 0 goes to start of line
    editor.execute_keys("0").unwrap();
    assert_eq!(editor.get_cursor().column, 0);

    // ^ goes to first non-blank
    editor.execute_keys("^").unwrap();
    assert_eq!(editor.get_cursor().column, 2);

    // $ goes to end of line
    editor.execute_keys("$").unwrap();
    assert!(editor.get_cursor().column > 5);
}

#[test]
fn test_goto_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Line 1\nLine 2\nLine 3\nLine 4\nLine 5");

    // G goes to last line
    editor.execute_keys("G").unwrap();
    assert_eq!(editor.get_cursor().line, 5);

    // 1G goes to first line
    editor.execute_keys("1G").unwrap();
    assert_eq!(editor.get_cursor().line, 1);

    // 3G goes to line 3
    editor.execute_keys("3G").unwrap();
    assert_eq!(editor.get_cursor().line, 3);
}

#[test]
fn test_find_char() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello World");
    editor.set_cursor(Position::new(1, 0));

    // fo finds 'o'
    editor.execute_keys("fo").unwrap();
    assert_eq!(editor.get_cursor().column, 4); // first 'o' in "Hello"

    // ; repeats find
    editor.execute_keys(";").unwrap();
    assert_eq!(editor.get_cursor().column, 7); // 'o' in "World"
}

// ============================================================================
// Delete Tests
// ============================================================================

#[test]
fn test_delete_char() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello");
    editor.set_cursor(Position::new(1, 0));

    // x deletes char under cursor
    editor.execute_keys("x").unwrap();
    assert_eq!(editor.get_buffer_text().trim(), "ello");
}

#[test]
fn test_delete_char_before() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello");
    editor.set_cursor(Position::new(1, 4)); // on 'o'

    // X deletes char before cursor
    editor.execute_keys("X").unwrap();
    assert_eq!(editor.get_buffer_text().trim(), "Helo");
}

#[test]
fn test_delete_word() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello World");
    editor.set_cursor(Position::new(1, 0));

    // dw deletes word
    editor.execute_keys("dw").unwrap();
    assert_eq!(editor.get_buffer_text().trim(), "World");
}

#[test]
fn test_delete_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Line 1\nLine 2\nLine 3");
    editor.set_cursor(Position::new(2, 0));

    // dd deletes current line
    editor.execute_keys("dd").unwrap();

    let text = editor.get_buffer_text();
    assert!(text.contains("Line 1"));
    assert!(!text.contains("Line 2"));
    assert!(text.contains("Line 3"));
}

#[test]
fn test_delete_multiple_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Line 1\nLine 2\nLine 3\nLine 4");
    editor.set_cursor(Position::new(2, 0));

    // 2dd deletes 2 lines
    editor.execute_keys("2dd").unwrap();

    let text = editor.get_buffer_text();
    assert!(text.contains("Line 1"));
    assert!(!text.contains("Line 2"));
    assert!(!text.contains("Line 3"));
    assert!(text.contains("Line 4"));
}

#[test]
fn test_delete_to_end_of_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello World");
    editor.set_cursor(Position::new(1, 5)); // on space

    // D deletes to end of line
    editor.execute_keys("D").unwrap();
    assert_eq!(editor.get_buffer_text().trim(), "Hello");
}

// ============================================================================
// Change Tests
// ============================================================================

#[test]
fn test_change_word() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello World");
    editor.set_cursor(Position::new(1, 0));

    // cw changes word
    editor.execute_keys("cwGoodbye\x1b").unwrap();
    assert_eq!(editor.get_buffer_text().trim(), "Goodbye World");
}

#[test]
fn test_change_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Old Line\nKeep this");
    editor.set_cursor(Position::new(1, 0));

    // cc changes entire line
    editor.execute_keys("ccNew Line\x1b").unwrap();

    let text = editor.get_buffer_text();
    assert!(text.contains("New Line"));
    assert!(text.contains("Keep this"));
    assert!(!text.contains("Old Line"));
}

#[test]
fn test_substitute_char() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello");
    editor.set_cursor(Position::new(1, 0));

    // s substitutes char
    editor.execute_keys("sJ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text().trim(), "Jello");
}

// ============================================================================
// Yank and Put Tests
// ============================================================================

#[test]
fn test_yank_and_put() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello");
    editor.set_cursor(Position::new(1, 0));

    // yy yanks line
    editor.execute_keys("yy").unwrap();

    // Verify register has content
    let reg = editor.get_unnamed_register();
    assert!(reg.is_some());

    // p puts after
    editor.execute_keys("p").unwrap();

    let text = editor.get_buffer_text();
    // Should have "Hello" twice now
    assert!(text.matches("Hello").count() >= 2);
}

#[test]
fn test_yank_word_and_put() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello World");
    editor.set_cursor(Position::new(1, 0));

    // yw yanks word
    editor.execute_keys("yw").unwrap();

    // Move to end and put
    editor.execute_keys("$p").unwrap();

    let text = editor.get_buffer_text();
    assert!(text.contains("Hello"));
}

#[test]
fn test_named_register() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello");
    editor.set_cursor(Position::new(1, 0));

    // "ayy yanks to register a
    editor.execute_keys("\"ayy").unwrap();

    // Verify register a has content
    let reg = editor.get_register('a');
    assert!(reg.is_some());
    assert!(reg.unwrap().text.contains("Hello"));
}

#[test]
fn test_yank_to_end_of_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello World");
    editor.set_cursor(Position::new(1, 0));

    // y$ yanks from cursor to end of line (inclusive)
    editor.execute_keys("y$").unwrap();

    // Verify unnamed register has content
    let reg = editor.get_unnamed_register();
    assert!(reg.is_some(), "y$ should populate unnamed register");
    let text = &reg.unwrap().text;
    assert!(
        text.contains("Hello"),
        "y$ should yank 'Hello World' but got {:?}",
        text
    );
}

#[test]
fn test_yank_char_motion() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello");
    editor.set_cursor(Position::new(1, 0));

    // yl yanks 1 character
    editor.execute_keys("yl").unwrap();

    // Verify unnamed register has 'H'
    let reg = editor.get_unnamed_register();
    assert!(reg.is_some(), "yl should populate unnamed register");
    let text = &reg.unwrap().text;
    assert_eq!(text, "H", "yl should yank 'H' but got {:?}", text);
}

// ============================================================================
// Undo/Redo Tests
// ============================================================================

#[test]
fn test_undo_insert() {
    let mut editor = Editor::new_headless();

    // Insert text
    editor.execute_keys("iHello\x1b").unwrap();
    assert!(editor.get_buffer_text().contains("Hello"));

    // Undo
    editor.execute_keys("u").unwrap();

    // Buffer should be empty or back to original
    let text = editor.get_buffer_text();
    assert!(!text.contains("Hello") || text.trim().is_empty());
}

#[test]
fn test_undo_delete() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello World");

    // Delete first character
    editor.execute_keys("x").unwrap();
    assert!(editor.get_buffer_text().starts_with("ello"));

    // Undo single char delete
    editor.execute_keys("u").unwrap();
    assert!(editor.get_buffer_text().starts_with("Hello"));
}

// ============================================================================
// Dot Repeat Tests
// ============================================================================

#[test]
fn test_dot_repeat_delete() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Line 1\nLine 2\nLine 3");

    // dd deletes line
    editor.execute_keys("dd").unwrap();
    assert!(!editor.get_buffer_text().contains("Line 1"));

    // . repeats
    editor.execute_keys(".").unwrap();
    assert!(!editor.get_buffer_text().contains("Line 2"));
}

#[test]
fn test_dot_repeat_insert() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a\nb\nc");
    editor.set_cursor(Position::new(1, 0));

    // Insert "X" at beginning
    editor.execute_keys("iX\x1b").unwrap();
    assert!(editor.get_buffer_text().starts_with("X"));

    // Move down and repeat
    editor.execute_keys("j0.").unwrap();

    let text = editor.get_buffer_text();
    let lines: Vec<&str> = text.lines().collect();
    assert!(lines.len() >= 2);
    assert!(lines[0].starts_with("X"));
    assert!(lines[1].starts_with("X"));
}

#[test]
fn test_dot_with_count() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aaaaa");
    editor.set_cursor(Position::new(1, 0));

    // x deletes one char
    editor.execute_keys("x").unwrap();
    assert_eq!(editor.get_buffer_text().trim(), "aaaa");

    // 3. repeats x three times
    editor.execute_keys("3.").unwrap();
    assert_eq!(editor.get_buffer_text().trim(), "a");
}

// ============================================================================
// Macro/Register Execution Tests (@)
// ============================================================================

// Test macro execution with line-based yank (yy)
#[test]
fn test_execute_register_simple() {
    let mut editor = Editor::new_headless();

    // Yank line "x" into register a using yy
    editor.set_buffer_text("x");
    editor.execute_keys("\"ayy").unwrap();

    let reg_a = editor.get_register('a');
    assert!(reg_a.is_some(), "Register a should have content");

    // Now try executing the full content via macro
    // Content is "x\n" - x deletes char, \n does nothing in command mode
    editor.set_buffer_text("Hello");
    editor.set_cursor(Position::new(1, 0));

    editor.execute_keys("@a").unwrap();

    assert!(
        editor.get_buffer_text().starts_with("ello"),
        "Expected 'x' to delete first char"
    );
}

#[test]
fn test_execute_register_repeat() {
    let mut editor = Editor::new_headless();

    editor.set_buffer_text("x");
    editor.execute_keys("\"ayy").unwrap();

    editor.set_buffer_text("Hello");
    editor.set_cursor(Position::new(1, 0));
    editor.execute_keys("2@a").unwrap();

    assert!(
        !editor.get_buffer_text().starts_with("He"),
        "Expected 2@a to execute 'x' twice"
    );
}

// ============================================================================
// Search Tests
// ============================================================================

#[test]
fn test_search_forward() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello World Hello");
    editor.set_cursor(Position::new(1, 0));

    // /World<Enter> searches forward
    editor.execute_keys("/World\n").unwrap();

    // Cursor should be on "World"
    assert_eq!(editor.get_cursor().column, 6);
}

#[test]
fn test_search_next() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two one three one");
    editor.set_cursor(Position::new(1, 0));

    // Search for "one"
    editor.execute_keys("/one\n").unwrap();

    // First "one" is at 0, search finds next
    let first_pos = editor.get_cursor().column;

    // n finds next
    editor.execute_keys("n").unwrap();
    let second_pos = editor.get_cursor().column;

    assert!(second_pos > first_pos);
}

// ============================================================================
// Ex Command Tests
// ============================================================================

#[test]
fn test_ex_substitute() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello World");

    // :s/World/Universe/<Enter>
    editor.execute_keys(":s/World/Universe/\n").unwrap();

    assert!(editor.get_buffer_text().contains("Universe"));
    assert!(!editor.get_buffer_text().contains("World"));
}

#[test]
fn test_ex_substitute_global() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one one one");

    // :s/one/two/g<Enter>
    editor.execute_keys(":s/one/two/g\n").unwrap();

    let text = editor.get_buffer_text();
    assert!(!text.contains("one"));
    assert_eq!(text.matches("two").count(), 3);
}

#[test]
fn test_ex_delete() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Line 1\nLine 2\nLine 3");

    // :d<Enter> deletes current line (line 1)
    editor.execute_keys(":d\n").unwrap();

    let text = editor.get_buffer_text();
    // Current line (Line 1) should be deleted
    assert!(!text.contains("Line 1"));
    assert!(text.contains("Line 2"));
    assert!(text.contains("Line 3"));
}

#[test]
fn test_ex_goto() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Line 1\nLine 2\nLine 3");
    editor.set_cursor(Position::new(1, 0));

    // :3<Enter> goes to line 3
    editor.execute_keys(":3\n").unwrap();

    assert_eq!(editor.get_cursor().line, 3);
}

// ============================================================================
// Replace Mode Tests
// ============================================================================

#[test]
fn test_replace_char() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello");
    editor.set_cursor(Position::new(1, 0));

    // rJ replaces H with J
    editor.execute_keys("rJ").unwrap();

    assert_eq!(editor.get_buffer_text().trim(), "Jello");
}

#[test]
fn test_replace_multiple() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aaaa");
    editor.set_cursor(Position::new(1, 0));

    // rx replaces one character with x. A count is honoured too --
    // `test_replace_char_with_a_count_replaces_that_many` and
    // `test_replace_char_past_end_of_line_changes_nothing` cover that; what is
    // under test here is that repeated *single* replacements accumulate.
    editor.execute_keys("rx").unwrap();
    assert_eq!(editor.get_buffer_text().trim(), "xaaa");

    // Multiple single replacements work
    editor.execute_keys("lrx").unwrap();
    assert_eq!(editor.get_buffer_text().trim(), "xxaa");
}

// ============================================================================
// Join Lines Tests
// ============================================================================

#[test]
fn test_join_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello\nWorld");
    editor.set_cursor(Position::new(1, 0));

    // J joins lines
    editor.execute_keys("J").unwrap();

    let text = editor.get_buffer_text();
    assert!(text.contains("Hello World") || text.contains("Hello  World"));
}

// ============================================================================
// Marks Tests
// ============================================================================

#[test]
fn test_set_and_jump_to_mark() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Line 1\nLine 2\nLine 3");
    editor.set_cursor(Position::new(2, 3));

    // ma sets mark a
    editor.execute_keys("ma").unwrap();

    // Go somewhere else
    editor.execute_keys("1G").unwrap();
    assert_eq!(editor.get_cursor().line, 1);

    // 'a jumps to mark a (line)
    editor.execute_keys("'a").unwrap();
    assert_eq!(editor.get_cursor().line, 2);
}

// ============================================================================
// Complex Sequences Tests
// ============================================================================

#[test]
fn test_complex_edit_sequence() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello World");

    // Complex edit: delete "World", insert "Universe"
    editor.execute_keys("wcwUniverse\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim(), "Hello Universe");
}

#[test]
fn test_multiple_operations() {
    let mut editor = Editor::new_headless();

    // Create a file with multiple lines, edit various parts
    editor
        .execute_keys("iFirst line\nSecond line\nThird line\x1b")
        .unwrap();

    // Go to line 2, delete it
    editor.execute_keys("2Gdd").unwrap();

    // Go to line 1, append " - edited"
    editor.execute_keys("1GA - edited\x1b").unwrap();

    let text = editor.get_buffer_text();
    assert!(text.contains("First line - edited"));
    assert!(!text.contains("Second line"));
    assert!(text.contains("Third line"));
}

#[test]
fn test_yank_delete_put_workflow() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("AAA\nBBB\nCCC");

    // Yank first line
    editor.execute_keys("yy").unwrap();

    // Go to last line
    editor.execute_keys("G").unwrap();

    // Put after
    editor.execute_keys("p").unwrap();

    let text = editor.get_buffer_text();
    let lines: Vec<&str> = text.lines().collect();

    // Should have AAA at the end now
    assert!(lines.last().unwrap().contains("AAA"));
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn test_empty_buffer_operations() {
    let mut editor = Editor::new_headless();

    // Operations on empty buffer shouldn't crash
    editor.execute_keys("dd").unwrap();
    editor.execute_keys("x").unwrap();
    editor.execute_keys("yy").unwrap();
}

#[test]
fn test_escape_cancels_command() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hello");

    // Start delete command, then escape
    editor.execute_keys("d\x1b").unwrap();

    // Buffer should be unchanged
    assert_eq!(editor.get_buffer_text().trim(), "Hello");
}

#[test]
fn test_cursor_bounds() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Hi");
    editor.set_cursor(Position::new(1, 0));

    // Try to move past end of line
    editor.execute_keys("llllllllll").unwrap();

    // Cursor should be clamped
    assert!(editor.get_cursor().column <= 2);
}

#[test]
fn test_s_and_upper_s_behave_as_change_operators() {
    // #V17: `s`/`S` were handled in the pre-parser fast path, so they saved
    // nothing to a register and recorded no undo. They are now `c` over an
    // implied range, so they must be indistinguishable from it -- including
    // undo behavior.
    let mut via_c = Editor::new_headless();
    via_c.execute_keys("iabcdef\x1b0").unwrap();
    via_c.execute_keys("clX\x1b").unwrap();
    let c_after = via_c.get_buffer_text();
    via_c.execute_keys("u").unwrap();
    let c_undone = via_c.get_buffer_text();

    let mut via_s = Editor::new_headless();
    via_s.execute_keys("iabcdef\x1b0").unwrap();
    via_s.execute_keys("sX\x1b").unwrap();
    let s_after = via_s.get_buffer_text();
    via_s.execute_keys("u").unwrap();
    let s_undone = via_s.get_buffer_text();

    assert_eq!(s_after, c_after, "`s` must match `cl`");
    assert_eq!(
        s_undone, c_undone,
        "`s` must be undoable exactly as `cl` is -- it recorded no undo at all before"
    );
    assert_ne!(s_after, s_undone, "undo must actually change the buffer");
}

#[test]
fn test_s_accepts_a_count() {
    // The fast path ignored counts entirely, even though 's' was already in
    // the command parser's table.
    let mut editor = Editor::new_headless();
    editor.execute_keys("iabcdef\x1b0").unwrap();
    editor.execute_keys("3sX\x1b").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "Xdef");
}

#[test]
fn test_upper_s_substitutes_whole_lines() {
    let mut editor = Editor::new_headless();
    editor.execute_keys("iL1\nL2\nL3\x1b").unwrap();
    editor.execute_keys("1G").unwrap();
    editor.execute_keys("SX\x1b").unwrap();
    let text = editor.get_buffer_text();
    assert!(text.contains('X'), "S must replace the line: {:?}", text);
    assert!(
        !text.contains("L1"),
        "the original line must be gone: {:?}",
        text
    );
}

#[test]
fn test_insert_honors_configured_erase_and_kill_chars() {
    // #V12: insert mode hardcoded ^H/^U. POSIX vi honors the terminal's
    // `stty erase` and `stty kill` characters, which are now read from the
    // termios captured before raw mode and carried into the insert session.
    //
    // Driven at the InsertState level because a headless editor has no
    // terminal to read termios from -- the PTY suite covers the wiring.
    use vi_rs::{process_insert_key, Buffer, InsertKind, InsertState, Key, Options, Position};

    let mut buffer = Buffer::from_text("");
    let mut state = InsertState::new(InsertKind::Insert, Position::new(1, 0), 1);
    state.erase_char = Some('#');
    state.kill_char = Some('@');

    for c in "abc".chars() {
        process_insert_key(&mut buffer, Key::Char(c), &mut state, &Options::default()).unwrap();
    }
    // '#' is this terminal's erase character, so it deletes rather than
    // inserting a literal '#'.
    process_insert_key(&mut buffer, Key::Char('#'), &mut state, &Options::default()).unwrap();
    assert_eq!(buffer.line(1).unwrap().content(), "ab");

    // '@' is the kill character: discard back to the start of the line.
    process_insert_key(&mut buffer, Key::Char('@'), &mut state, &Options::default()).unwrap();
    assert_eq!(buffer.line(1).unwrap().content(), "");

    // With no erase/kill configured the same bytes are ordinary text.
    let mut buffer = Buffer::from_text("");
    let mut plain = InsertState::new(InsertKind::Insert, Position::new(1, 0), 1);
    for c in "a#b".chars() {
        process_insert_key(&mut buffer, Key::Char(c), &mut plain, &Options::default()).unwrap();
    }
    assert_eq!(buffer.line(1).unwrap().content(), "a#b");
}

// ============================================================================
// Undo of operators (#V19 and the defects found with it)
// ============================================================================

/// #V19. A change operator recorded no undo for the text it removed, so `u`
/// popped an unrelated older change and `apply_inverse` deleted characters that
/// were never inserted -- emptying the line.
///
/// POSIX (ex `undo`, 95443) treats a change plus its insert session as a single
/// command, so *one* `u` restores the original. The audit entry said "a second
/// `u`"; that expectation was itself wrong.
#[test]
fn test_undo_after_change_operator_restores_the_original_text() {
    let mut editor = Editor::new_headless();
    editor.execute_keys("iabcdef\x1b0").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "abcdef");

    editor.execute_keys("clX\x1b").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "Xbcdef");

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text().trim_end(),
        "abcdef",
        "one `u` must reverse the whole change command"
    );
}

/// POSIX: `u` reverses "the last command that modified the contents of the edit
/// buffer, **including undo**" (95442), so it is its own inverse.
#[test]
fn test_undo_is_its_own_inverse() {
    let mut editor = Editor::new_headless();
    editor.execute_keys("iabcdef\x1b0").unwrap();
    editor.execute_keys("clX\x1b").unwrap();

    editor.execute_keys("u").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "abcdef");
    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text().trim_end(),
        "Xbcdef",
        "a second `u` must reverse the first"
    );
    editor.execute_keys("u").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "abcdef");
}

/// `d`/`dd` recorded no undo either, though the audit ticked them CONFORM.
#[test]
fn test_undo_after_delete_restores_the_text() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\nbeta\ngamma");

    editor.execute_keys("dd").unwrap();
    assert!(!editor.get_buffer_text().contains("alpha"));
    editor.execute_keys("u").unwrap();
    assert!(
        editor.get_buffer_text().contains("alpha"),
        "dd must be undoable, got {:?}",
        editor.get_buffer_text()
    );

    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world");
    editor.execute_keys("dw").unwrap();
    let after = editor.get_buffer_text();
    assert_ne!(after.trim_end(), "hello world");
    editor.execute_keys("u").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "hello world");
}

/// `C` (change to end of line) took the pre-parser fast path and recorded
/// nothing at all.
#[test]
fn test_undo_after_change_to_end_of_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("keep this tail");
    editor.execute_keys("0").unwrap();
    editor.execute_keys("wCgone\x1b").unwrap();
    assert!(editor.get_buffer_text().contains("gone"));

    editor.execute_keys("u").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "keep this tail");
}

/// `J` recorded nothing, and `ChangeKind::Join` is inert in both `apply_change`
/// and `apply_inverse`, so a join could not have been undone even if recorded
/// that way.
#[test]
fn test_undo_after_join_restores_the_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("first\nsecond");

    editor.execute_keys("J").unwrap();
    let joined = editor.get_buffer_text();
    assert_eq!(joined.lines().count(), 1, "J should join, got {joined:?}");

    editor.execute_keys("u").unwrap();
    let restored = editor.get_buffer_text();
    assert_eq!(restored.lines().count(), 2, "got {restored:?}");
    assert!(restored.contains("first") && restored.contains("second"));
}

/// An insert session that types nothing must not leave an undo entry for `u` to
/// consume silently.
#[test]
fn test_empty_insert_session_leaves_no_undo_entry() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("unchanged");
    editor.execute_keys("i\x1b").unwrap();
    // Nothing was typed, so there is nothing to undo; `u` must not damage the
    // buffer whether it reports an error or does nothing.
    let _ = editor.execute_keys("u");
    assert_eq!(editor.get_buffer_text().trim_end(), "unchanged");
}

// ============================================================================
// #V21 / #V22 / #V23 / #V24 — shift width, TAB, ^V literal, ^T
// ============================================================================

/// `>>` shifts by one shiftwidth, not by `shiftwidth` tabs.
///
/// `shiftwidth` was being passed into `shift_right`'s repeat-**count**
/// parameter, so the default `shiftwidth = 8` produced `"\t".repeat(8)` — eight
/// tab characters, 64 columns — on every `>>`.
#[test]
fn test_shift_right_inserts_one_shiftwidth_not_eight_tabs() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello");

    editor.execute_keys(">>").unwrap();

    let line = editor.get_buffer_text().lines().next().unwrap().to_string();
    assert_eq!(
        line, "\thello",
        "default shiftwidth 8 at tabstop 8 is one tab, got {line:?}"
    );
    assert_eq!(
        line.chars().filter(|c| *c == '\t').count(),
        1,
        "got {line:?}"
    );
}

/// `<<` removes one shiftwidth, not 64 columns.
#[test]
fn test_shift_left_removes_one_shiftwidth() {
    let mut editor = Editor::new_headless();
    // Two tabs = 16 columns; one `<<` at shiftwidth 8 must leave 8.
    editor.set_buffer_text("\t\thello");

    editor.execute_keys("<<").unwrap();

    let line = editor.get_buffer_text().lines().next().unwrap().to_string();
    assert_eq!(line, "\thello", "got {line:?}");
}

/// `:set sw=4` must actually change the shift amount.
#[test]
fn test_shift_right_honors_set_shiftwidth() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello");

    editor.execute_keys(":set sw=4\n").unwrap();
    editor.execute_keys(">>").unwrap();

    let line = editor.get_buffer_text().lines().next().unwrap().to_string();
    assert_eq!(
        line, "    hello",
        "4 columns is below tabstop 8, so four spaces, got {line:?}"
    );
}

/// Shifting left past column 0 clamps instead of eating the text.
#[test]
fn test_shift_left_clamps_at_column_zero() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("  hello");

    editor.execute_keys("<<").unwrap();

    assert_eq!(editor.get_buffer_text().lines().next().unwrap(), "hello");
}

/// #V22: a typed TAB must reach the buffer. `Key::Tab` was unreachable —
/// `from_byte` mapped byte 9 to `Ctrl('i')`, which had no arm — so TAB fell
/// through the ignore-everything-else arm and vanished.
#[test]
fn test_tab_inserts_a_tab_in_insert_mode() {
    let mut editor = Editor::new_headless();

    editor.execute_keys("ia\tb\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "a\tb");
}

/// #V23: `^V` takes the next character literally, ESC included, rather than
/// letting it end the insert session.
#[test]
fn test_ctrl_v_inserts_literal_escape() {
    let mut editor = Editor::new_headless();

    // i ^V ESC ESC  -> one literal 0x1b in the buffer, then leave insert mode.
    editor.execute_keys("i\x16\x1b\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "\x1b");
    assert!(matches!(editor.get_mode(), Mode::Command));
}

/// A literalized control character must not trigger its normal insert-mode
/// action. `^D` would otherwise dedent.
#[test]
fn test_ctrl_v_inserts_literal_control_char() {
    let mut editor = Editor::new_headless();

    editor.execute_keys("iX\x16\x04Y\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "X\x04Y");
}

/// POSIX 121875-121877: a `^V` before a <newline> is *discarded* and the
/// newline behaves normally, so this must split the line rather than insert a
/// literal carriage return.
#[test]
fn test_ctrl_v_before_newline_is_discarded() {
    let mut editor = Editor::new_headless();

    editor.execute_keys("iA\x16\nB\x1b").unwrap();

    let text = editor.get_buffer_text();
    let lines: Vec<&str> = text.lines().collect();
    assert_eq!(lines, vec!["A", "B"], "got {text:?}");
}

/// The pending-literal flag is consumed by exactly one key.
#[test]
fn test_ctrl_v_consumes_only_one_key() {
    let mut editor = Editor::new_headless();

    editor.execute_keys("i\x16aa\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "aa");
}

/// `^Q` is the spec's synonym for `^V` (121874).
#[test]
fn test_ctrl_q_is_a_synonym_for_ctrl_v() {
    let mut editor = Editor::new_headless();

    editor.execute_keys("i\x11\x1b\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "\x1b");
}

/// A literal inserted via `^V` is part of the insert session, so `.` repeats it.
/// The old empty `^V` arm never touched `inserted_text`.
#[test]
fn test_ctrl_v_literal_is_recorded_for_dot_repeat() {
    let mut editor = Editor::new_headless();

    editor.execute_keys("i\x16\x02\x1b").unwrap();
    editor.execute_keys(".").unwrap();

    let text = editor.get_buffer_text();
    assert_eq!(
        text.trim_end().matches('\x02').count(),
        2,
        "`.` must repeat the literal, got {text:?}"
    );
}

/// #V24: `^T` inserts blanks at the *cursor* up to the next shiftwidth
/// boundary, not a single tab at column 0.
#[test]
fn test_ctrl_t_indents_at_cursor_to_shiftwidth_boundary() {
    let mut editor = Editor::new_headless();
    editor.execute_keys(":set sw=4\n").unwrap();

    // Type "ab", then ^T: the cursor is at column 2, so the next 4-column
    // boundary is 4, i.e. two blanks. Then type "c".
    editor.execute_keys("iab\x14c\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "ab  c");
}

/// `^T` goes through the insert-session record, so `u` removes it along with
/// the rest of the session and `.` can repeat it.
#[test]
fn test_ctrl_t_is_recorded_in_the_insert_session() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("base");
    editor.execute_keys(":set sw=4\n").unwrap();

    editor.execute_keys("A\x14x\x1b").unwrap();
    let after = editor.get_buffer_text().trim_end().to_string();
    assert_ne!(after, "base", "^T + x must have changed the line");

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text().trim_end(),
        "base",
        "undo must remove the ^T blanks too"
    );
}

/// #V27: `u` after an insert session must remove exactly what was typed.
///
/// `InsertState::start_pos` was captured before `enter_insert_mode`
/// repositioned the cursor, so it was only right for `i`. For `A` it named the
/// pre-command column, and the undo record then deleted that many characters
/// from the wrong place: `Axy<ESC>u` on "base" left "sexy" — two characters
/// removed from column 0 instead of the two that were appended.
#[test]
fn test_undo_after_append_removes_only_the_appended_text() {
    for (keys, typed) in [
        ("Axy\x1b", "basexy"), // A - append at end of line
        ("axy\x1b", "bxyase"), // a - append after cursor
        ("Ixy\x1b", "xybase"), // I - insert at first non-blank
        ("ixy\x1b", "xybase"), // i - insert before cursor (was already right)
    ] {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text("base");

        editor.execute_keys(keys).unwrap();
        assert_eq!(
            editor.get_buffer_text().trim_end(),
            typed,
            "typing {keys:?} produced the wrong text"
        );

        editor.execute_keys("u").unwrap();
        assert_eq!(
            editor.get_buffer_text().trim_end(),
            "base",
            "undo after {keys:?} must restore the original line exactly"
        );
    }
}

/// `^U` in insert mode deletes back to the start of the *insert*, not past it
/// into pre-existing text. This shares the `start_pos` anchor with #V27.
#[test]
fn test_ctrl_u_stops_at_the_start_of_the_insert() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("keep");

    // Append "junk", then ^U: only "junk" may go.
    editor.execute_keys("Ajunk\x15\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "keep");
}

// ============================================================================
// #V20 — U (undo current line)
// ============================================================================

/// `U` restores the line to its state when the cursor arrived on it.
///
/// It could never restore anything before: `line_original` was only written by
/// `save_line_original`, which had no production callers, so `U` always
/// reported "Nothing to undo".
#[test]
fn test_upper_u_restores_line_after_deletions() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world\nsecond line");

    editor.execute_keys("xxx").unwrap();
    assert_eq!(
        editor.get_buffer_text().lines().next().unwrap(),
        "lo world",
        "three x's should have removed three characters"
    );

    editor.execute_keys("U").unwrap();
    assert_eq!(
        editor.get_buffer_text().lines().next().unwrap(),
        "hello world"
    );
    // POSIX 121571-121572: first column, not first non-blank.
    assert_eq!(editor.get_cursor().column, 0);
}

/// A whole insert session is undone by `U`, since it all happened after the
/// cursor arrived on the line.
#[test]
fn test_upper_u_restores_line_after_insert_session() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("original");

    editor.execute_keys("iABC\x1b").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "ABCoriginal");

    editor.execute_keys("U").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "original");
}

/// `U` is its own inverse — pressing it twice returns to the modified text.
#[test]
fn test_upper_u_is_its_own_inverse() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello");

    editor.execute_keys("x").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "ello");

    editor.execute_keys("U").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "hello");

    editor.execute_keys("U").unwrap();
    assert_eq!(
        editor.get_buffer_text().trim_end(),
        "ello",
        "a second U must put the change back"
    );
}

/// `U` is a buffer-modifying command, so `u` reverses it (POSIX 95442).
#[test]
fn test_lowercase_u_reverses_an_upper_u() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello");

    editor.execute_keys("xx").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "llo");

    editor.execute_keys("U").unwrap();
    assert_eq!(editor.get_buffer_text().trim_end(), "hello");

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text().trim_end(),
        "llo",
        "u must reverse the U"
    );
}

/// Leaving a line and coming back re-snapshots it, so `U` no longer reaches
/// back past the return.
#[test]
fn test_upper_u_resnapshots_when_the_cursor_returns_to_a_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("first line\nsecond line");

    editor.execute_keys("xx").unwrap();
    assert_eq!(editor.get_buffer_text().lines().next().unwrap(), "rst line");

    // Leave the line and come back: the snapshot is retaken from "rst line".
    editor.execute_keys("jk").unwrap();
    editor.execute_keys("U").unwrap();

    assert_eq!(
        editor.get_buffer_text().lines().next().unwrap(),
        "rst line",
        "U must not reach back past the cursor's return to the line"
    );
}

/// `U` on an untouched line changes nothing and leaves the undo stack alone.
#[test]
fn test_upper_u_on_unchanged_line_is_a_noop() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("untouched\nsecond");

    let _ = editor.execute_keys("U");
    assert_eq!(
        editor.get_buffer_text().lines().next().unwrap(),
        "untouched"
    );
}

/// After a command that changes the line count the snapshot is abandoned, so
/// `U` reports nothing to undo rather than overwriting an unrelated line.
#[test]
fn test_upper_u_refuses_after_the_line_count_changes() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree");

    // Join lines 1 and 2, then ask U to restore.
    editor.execute_keys("J").unwrap();
    let joined = editor.get_buffer_text();
    let _ = editor.execute_keys("U");

    assert_eq!(
        editor.get_buffer_text(),
        joined,
        "U must not touch the buffer once the line numbering has shifted"
    );
}

// ============================================================================
// #V25 / #V26 — autoindent and ^D
// ============================================================================

/// `o`/`O` indent the new line to match the line the command was issued on
/// (POSIX 121501-121502). `options.autoindent` was parsed but never read, so
/// none of this happened before.
#[test]
fn test_autoindent_o_derives_indent_from_originating_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\tindented");
    editor.execute_keys(":set ai\n").unwrap();

    editor.execute_keys("oNEW\x1b").unwrap();

    assert_eq!(editor.get_buffer_text(), "\tindented\n\tNEW\n");
}

/// With autoindent unset the new line gets no indent at all.
#[test]
fn test_autoindent_unset_leaves_new_line_flush() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\tindented");

    editor.execute_keys("oNEW\x1b").unwrap();

    assert_eq!(editor.get_buffer_text(), "\tindented\nNEW\n");
}

/// A <newline> in input mode carries the indent onto the next line
/// (121826-121827).
#[test]
fn test_autoindent_is_carried_across_newline() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\tbase");
    editor.execute_keys(":set ai\n").unwrap();

    editor.execute_keys("oONE\nTWO\x1b").unwrap();

    assert_eq!(editor.get_buffer_text(), "\tbase\n\tONE\n\tTWO\n");
}

/// "Any autoindent characters entered on newly created lines that have no
/// other non-<newline> characters shall be deleted" on ESC (121912-121913) —
/// so `o<ESC>` leaves an empty line, not a line full of whitespace.
#[test]
fn test_autoindent_only_line_is_emptied_on_escape() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\tbase");
    editor.execute_keys(":set ai\n").unwrap();

    editor.execute_keys("o\x1b").unwrap();

    assert_eq!(editor.get_buffer_text(), "\tbase\n\n");
}

/// `^D` moves back to the column after the previous shiftwidth boundary
/// (121781-121782), in display columns. With two tabs (16 columns at
/// tabstop 8) and shiftwidth 4, one `^D` leaves 12 columns.
#[test]
fn test_ctrl_d_backs_up_one_shiftwidth() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\t\tbase");
    editor.execute_keys(":set ai\n").unwrap();
    editor.execute_keys(":set sw=4\n").unwrap();

    editor.execute_keys("o\x04X\x1b").unwrap();

    // 12 columns at tabstop 8 renders as one tab plus four spaces.
    assert_eq!(editor.get_buffer_text(), "\t\tbase\n\t    X\n");
}

/// `0^D` discards the whole autoindent and the `0` itself (121777).
#[test]
fn test_zero_ctrl_d_clears_the_whole_indent() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\t\tbase");
    editor.execute_keys(":set ai\n").unwrap();

    editor.execute_keys("o0\x04X\x1b").unwrap();

    assert_eq!(editor.get_buffer_text(), "\t\tbase\nX\n");
}

/// The one behavior that distinguishes `^^D` from `0^D`: both clear the
/// current line's indent, but after `^^D` "the autoindent level for the next
/// input line shall be derived from the same line from which the autoindent
/// level for the current input line was derived" (121779-121780).
#[test]
fn test_caret_ctrl_d_restores_the_indent_on_the_next_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\t\tbase");
    editor.execute_keys(":set ai\n").unwrap();

    editor.execute_keys("o^\x04A\nB\x1b").unwrap();

    assert_eq!(
        editor.get_buffer_text(),
        "\t\tbase\nA\n\t\tB\n",
        "^^D must un-indent only the current line"
    );
}

/// The contrasting case: after `0^D` the next line does *not* get the indent
/// back. Paired with the test above, this is what proves the two are handled
/// differently rather than both mapped onto "clear the indent".
#[test]
fn test_zero_ctrl_d_does_not_restore_the_indent_on_the_next_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\t\tbase");
    editor.execute_keys(":set ai\n").unwrap();

    editor.execute_keys("o0\x04A\nB\x1b").unwrap();

    assert_eq!(editor.get_buffer_text(), "\t\tbase\nA\nB\n");
}

/// When the cursor does not follow autoindent characters, `^D` "shall have no
/// special meaning" (121776) — it is appended like any other input character.
#[test]
fn test_ctrl_d_after_typed_text_has_no_special_meaning() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("");

    editor.execute_keys("iabc\x04\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "abc\u{4}");
}

/// In column 1 with nothing to erase, `^D` "shall be discarded and no further
/// action taken" (121774-121775) — it must not insert a literal.
#[test]
fn test_ctrl_d_in_column_one_is_discarded() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("");

    editor.execute_keys("i\x04X\x1b").unwrap();

    assert_eq!(editor.get_buffer_text().trim_end(), "X");
}

// ============================================================================
// `:file` informational message (ex.md §94981-94987)
// ============================================================================

/// The message must carry the current pathname (or say there is none), the
/// current line and the line count (or say the buffer is empty), and the fact
/// that the buffer has been modified. It used to report only the name, a `[+]`
/// marker and a percentage, with no current line number at all.
#[test]
fn test_file_info_reports_position_and_state() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys(":2\n:f\n").unwrap();

    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(
        msg.contains("line 2 of 3"),
        "expected the current line and line count in {msg:?}"
    );
    assert!(
        !msg.contains("[Modified]"),
        "a buffer that was only navigated is not modified: {msg:?}"
    );
    assert!(
        msg.contains("[No file]"),
        "with no pathname the message must say so: {msg:?}"
    );
}

#[test]
fn test_file_info_reports_modification() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\n");
    editor.execute_keys("ix\x1b:f\n").unwrap();

    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(msg.contains("[Modified]"), "expected [Modified] in {msg:?}");
}

// ============================================================================
// `:vi[sual]` as `edit` (ex.md §95473-95474)
// ============================================================================

/// "If ex is currently in open or visual mode, the Synopsis and behavior of the
/// visual command shall be the same as the edit command", which includes its
/// `[+command][file]` arguments. `:vi` used to parse to a bare `Visual` with the
/// arguments thrown away.
#[test]
fn test_visual_command_in_visual_mode_edits_a_file() {
    let dir = plib::tmp::tempdir().unwrap();
    let path = dir.path().join("v.txt");
    std::fs::write(&path, "one\ntwo\nthree\n").unwrap();

    let mut editor = Editor::new_headless();
    editor
        .execute_keys(&format!(":vi +2 {}\n", path.display()))
        .unwrap();

    assert!(
        editor.get_buffer_text().contains("three"),
        "expected the file to have been loaded: {:?}",
        editor.get_buffer_text()
    );
    assert_eq!(
        editor.get_cursor().line,
        2,
        "expected the +command to have moved to line 2"
    );
}

// ============================================================================
// Phase 1 — crash & hang stoppers
// ============================================================================

/// `:s` with a `\n` in the replacement splits one line into several. The loop
/// that walks the range must step *past* the lines it just inserted; stepping
/// back onto the first part re-substitutes it forever, growing the buffer
/// without bound until the process is killed.
///
/// Run on a worker thread so a regression fails the suite instead of hanging it.
#[test]
fn test_ex_substitute_newline_terminates() {
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text("a\n");
        let r = editor.execute_keys(":1s/^/\\n/\n");
        let _ = tx.send(r.map(|()| editor.get_buffer_text()));
    });

    match rx.recv_timeout(std::time::Duration::from_secs(10)) {
        Ok(Ok(text)) => assert_eq!(
            text, "\na\n",
            "the empty match at `^` should have split line 1 in two"
        ),
        Ok(Err(e)) => panic!("substitute failed: {}", e),
        Err(_) => panic!("`:1s/^/\\n/` did not terminate within 10s"),
    }
}

/// An ex command that fails must report on the status line and leave the editor
/// running. Propagating the error out of `handle_key` unwinds to `run_editor`,
/// which prints and exits — so visual-mode `:q` on a modified buffer used to
/// quit vi and discard the unsaved work it was supposed to be protecting.
#[test]
fn test_visual_quit_on_modified_buffer_warns_without_quitting() {
    let mut editor = Editor::new_headless();
    editor.execute_keys("ihello\x1b").unwrap();

    editor
        .execute_keys(":q\n")
        .expect("`:q` on a modified buffer must not propagate an error");

    assert!(
        !editor.should_quit(),
        "vi must stay running so the unsaved buffer is not lost"
    );
    assert!(
        editor.is_error_message(),
        "expected an error on the status line, got {:?}",
        editor.get_message()
    );
    assert_eq!(editor.get_buffer_text().trim(), "hello");
}

/// Every failing ex command takes the same path out of `handle_ex_key`, so one
/// escaping error means all of them escape.
#[test]
fn test_failing_ex_commands_do_not_quit_the_editor() {
    for cmd in [":e /nonexistent/nope\n", ":n\n", ":zzzz\n"] {
        let mut editor = Editor::new_headless();
        editor.execute_keys("ihello\x1b").unwrap();

        editor
            .execute_keys(cmd)
            .unwrap_or_else(|e| panic!("{:?} propagated an error: {}", cmd, e));
        assert!(!editor.should_quit(), "{:?} quit the editor", cmd);
        assert_eq!(
            editor.get_buffer_text().trim(),
            "hello",
            "{:?} lost the buffer",
            cmd
        );
    }
}

/// `:d`/`:y` accept an explicit count. A count of zero used to reach
/// `start + count - 1` unguarded.
#[test]
fn test_ex_delete_and_yank_reject_zero_count() {
    for cmd in [":1d 0\n", ":1y 0\n"] {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text("one\ntwo\nthree\n");

        editor
            .execute_keys(cmd)
            .unwrap_or_else(|e| panic!("{:?} propagated an error: {}", cmd, e));
        assert!(
            editor.is_error_message(),
            "{:?} should be rejected, got message {:?}",
            cmd,
            editor.get_message()
        );
        assert_eq!(
            editor.get_buffer_text(),
            "one\ntwo\nthree\n",
            "{:?} must leave the buffer alone",
            cmd
        );
    }
}

// ============================================================================
// Phase 8 — the cursor and every range endpoint stay on a character boundary
// ============================================================================
//
// `Position::column` is a 0-indexed *byte* offset (buffer/position.rs), which
// is what the renderer assumes too. But `clamp_column` normalised with a bare
// `min()`, which lowers an out-of-range column and otherwise leaves it alone --
// so a mid-character byte offset was representable, and the next slice panicked.

#[test]
fn test_multibyte_toggle_case_does_not_panic() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("héllo\n");
    // Three toggles, each advancing one *character*: h, é, l.
    editor.execute_keys("~~~").unwrap();
    assert_eq!(editor.get_buffer_text(), "HÉLlo\n");
}

#[test]
fn test_multibyte_substitute_char_does_not_panic() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("éx\n");
    editor.execute_keys("sZ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "Zx\n");
}

#[test]
fn test_multibyte_change_word_does_not_panic() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("café x\n");
    editor.execute_keys("cwZ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "Z x\n");
}

/// `p` used to back the cursor up one *byte* to land on the last character it
/// pasted, leaving it inside a multi-byte character; the next `x` then panicked
/// in `String::remove`.
#[test]
fn test_multibyte_put_then_delete_does_not_panic() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aébc\n");
    editor.execute_keys("lxp").unwrap();
    editor.execute_keys("x").unwrap();
    assert!(
        editor.get_buffer_text().is_char_boundary(0),
        "buffer must remain valid UTF-8"
    );
}

/// Whatever the keys, the cursor must never be left inside a character.
#[test]
fn test_cursor_stays_on_a_character_boundary() {
    // `p`/`P` need something in the unnamed register, so they yank first.
    for keys in [
        "$", "0", "w", "b", "e", "x", "X", "~", "A!\x1b", "iZ\x1b", "dw", "de", "D", "ywp", "ywP",
        "yyp", "dwp",
    ] {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text("héllo wörld — naïve\n");
        editor.execute_keys("ll").unwrap();
        editor
            .execute_keys(keys)
            .unwrap_or_else(|e| panic!("{:?} errored: {}", keys, e));

        let text = editor.get_buffer_text();
        let cursor = editor.get_cursor();
        let line = text.lines().nth(cursor.line - 1).unwrap_or("");
        assert!(
            line.is_char_boundary(cursor.column.min(line.len())),
            "{:?} left the cursor at byte {} of {:?}, mid-character",
            keys,
            cursor.column,
            line
        );
    }
}

/// `r` and `~` reached into the buffer through `line_mut`, which bypasses the
/// dirty flag as well as undo -- so `rX` then `:q` exited with no warning and
/// the edit was lost silently.
#[test]
fn test_replace_char_marks_buffer_modified() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello\n");
    editor.execute_keys("rX").unwrap();
    editor.execute_keys(":q\n").unwrap();
    assert!(
        !editor.should_quit(),
        "`r` must mark the buffer modified so `:q` warns"
    );
}

#[test]
fn test_toggle_case_marks_buffer_modified() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello\n");
    editor.execute_keys("~").unwrap();
    editor.execute_keys(":q\n").unwrap();
    assert!(
        !editor.should_quit(),
        "`~` must mark the buffer modified so `:q` warns"
    );
}

/// The vi counterpart of ed's malformed-command corpus: drive the editor with
/// generated key sequences over multi-byte text and assert it neither panics
/// nor corrupts the buffer.
///
/// Deterministic (fixed-seed LCG, no `rand` dependency), so any failure is
/// reproducible from the reported case.  The committed case count is sized to
/// keep the suite fast; raising it to 200_000 with sequences of up to 40 keys
/// is what turned up the `L`, `Y` and text-object defects fixed alongside it,
/// and is clean at that depth.
#[test]
fn test_generated_keys_over_multibyte_text_never_panic() {
    // Excludes anything that can reach the filesystem or the shell: ':' (ex
    // commands), 'Z' (ZZ writes the file), and '!' -- the filter operator,
    // which with '>' and Enter in the alphabet runs shell redirections and
    // really does litter the working directory.
    const KEYS: &[char] = &[
        'h', 'j', 'k', 'l', 'w', 'W', 'b', 'B', 'e', 'E', '0', '^', '$', 'G', 'H', 'L', 'M', '%',
        '|', '(', ')', '{', '}', 'x', 'X', 'D', 'J', 'p', 'P', 'u', '.', '~', 'd', 'y', 'c', 's',
        'r', 'a', 'i', 'A', 'I', 'o', 'O', 'R', 'f', 't', 'F', 'T', 'n', 'N', '1', '2', '3', 'é',
        'ö', '\x1b', '\x1b', '"', 'q', 'm', '\'', '`', 'Y', 'C', 'S', '_', '+', '-', '\n', '\t',
        '\x7f', '&', 'g', '[', ']', '<', '>',
    ];
    const SEEDS: &[&str] = &[
        "héllo wörld — naïve\nsecond ligne aé\nthird\n",
        "αβγ δεζ\nηθι\n",
        "\ta\tb\n\né\n",
        "one\ntwo\nthree\n",
        "",
        "é",
        "\n\n\n",
        "   \n\ta\n",
    ];

    let mut state: u64 = 0x9E37_79B9_7F4A_7C15;
    let mut next = || {
        state = state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        (state >> 33) as usize
    };

    for case in 0..5000 {
        let seed = SEEDS[next() % SEEDS.len()];
        let len = 1 + next() % 40;
        let keys: String = (0..len).map(|_| KEYS[next() % KEYS.len()]).collect();

        let mut editor = Editor::new_headless();
        editor.set_buffer_text(seed);
        // Errors are fine (many sequences are invalid); panics are not.
        let _ = editor.execute_keys(&keys);
        // Always leave insert mode, so the next assertion sees a settled state.
        let _ = editor.execute_keys("\x1b");

        let text = editor.get_buffer_text();
        let cursor = editor.get_cursor();
        let line = text
            .lines()
            .nth(cursor.line.saturating_sub(1))
            .unwrap_or("");
        assert!(
            line.is_char_boundary(cursor.column.min(line.len())),
            "case {} keys {:?} on {:?}: cursor at byte {} is mid-character in {:?}",
            case,
            keys,
            seed,
            cursor.column,
            line
        );
    }
}

// ============================================================================
// Phase 9 — search reports byte offsets like everything else
// ============================================================================
//
// `Position::column` is a byte offset everywhere in the editor, but
// `search_forward`/`search_backward` both read `from.column` as a character
// index and returned a character index in the same field. On any line with
// multi-byte text before the match the cursor therefore landed short, and the
// snapping added with the cursor invariant hid it as a silent off-by-N.

#[test]
fn test_search_forward_reports_a_byte_column() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aé bcd\n");
    editor.execute_keys("/bcd\n").unwrap();
    // a=0, é=1..2, space=3, b=4.
    assert_eq!(editor.get_cursor().column, 4);
}

#[test]
fn test_search_backward_reports_a_byte_column() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aé bcd é xy\n");
    editor.execute_keys("$?bcd\n").unwrap();
    assert_eq!(editor.get_cursor().column, 4);
}

/// The cursor lands on the match, so deleting a word there deletes the match.
#[test]
fn test_search_then_operate_hits_the_match() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aé bcd\n");
    editor.execute_keys("/bcd\n").unwrap();
    // The cursor is on the match, so `D` truncates exactly at it.
    editor.execute_keys("D").unwrap();
    assert_eq!(editor.get_buffer_text(), "aé \n");
}

/// An empty line has nothing after the search start, and the "is there room
/// to search" guard used a strict `<` -- so `/^$/` could never match one.
#[test]
fn test_search_finds_an_empty_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\n\nthree\n");
    editor.execute_keys("/^$\n").unwrap();
    assert_eq!(editor.get_cursor().line, 2);
}

#[test]
fn test_search_next_and_previous_on_multibyte_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aé x\nbö x\ncü x\n");
    // Each line is "<ascii><2-byte> x", so the x sits at byte 4, not char 3.
    editor.execute_keys("/x\n").unwrap();
    assert_eq!(
        (editor.get_cursor().line, editor.get_cursor().column),
        (1, 4)
    );
    editor.execute_keys("n").unwrap();
    assert_eq!(
        (editor.get_cursor().line, editor.get_cursor().column),
        (2, 4)
    );
    editor.execute_keys("n").unwrap();
    assert_eq!(
        (editor.get_cursor().line, editor.get_cursor().column),
        (3, 4)
    );
    editor.execute_keys("N").unwrap();
    assert_eq!(
        (editor.get_cursor().line, editor.get_cursor().column),
        (2, 4)
    );
}

/// `^` anchors to the start of the *line*, so a global substitute must not
/// re-anchor it at each restart: `:s/^/> /g` inserts one prefix, not one per
/// character.
#[test]
fn test_global_substitute_does_not_reanchor_caret() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abc\n");
    editor.execute_keys(":s/^/> /g\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "> abc\n");
}

/// Likewise `$`.
#[test]
fn test_global_substitute_does_not_reanchor_dollar() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abc\n");
    editor.execute_keys(":s/$/!/g\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "abc!\n");
}

// ============================================================================
// Phase 9b — counts that mean characters, not bytes
// ============================================================================

/// `3s` substitutes three *characters*. The end column added a character
/// count to a byte offset and then clamped it with a character count, so on
/// multi-byte text it consumed the wrong span.
#[test]
fn test_substitute_count_counts_characters() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("héllo\n");
    editor.execute_keys("3sZ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "Zlo\n");
}

#[test]
fn test_substitute_count_past_end_of_line_is_clamped() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("éx\n");
    editor.execute_keys("9sZ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "Z\n");
}

/// `cw` extends the range past the last character of the word. Adding one
/// *byte* landed inside that character when it was multi-byte.
#[test]
fn test_change_word_includes_a_multibyte_final_character() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("café x\n");
    editor.execute_keys("cwZ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "Z x\n");
}

/// `^W` deletes the word before the cursor, `^U` the whole insert. Both
/// counted a byte distance and then deleted that many *characters*, so they
/// over-deleted on multi-byte text -- `^U` ate the preceding newline.
#[test]
fn test_insert_delete_word_before_cursor_on_multibyte() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("");
    editor.execute_keys("ihéllo wörld\x17").unwrap();
    editor.execute_keys("\x1b").unwrap();
    // trim only the newline -- the trailing blank is exactly what `^W` keeps.
    assert_eq!(
        editor.get_buffer_text().trim_end_matches('\n'),
        "héllo ",
        "^W deletes the word, not the blank before it"
    );
}

#[test]
fn test_insert_delete_line_does_not_eat_the_previous_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("keep\n");
    editor.execute_keys("A\nhéllo wörld\x15").unwrap();
    editor.execute_keys("\x1b").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "keep\n\n",
        "^U must clear only what was inserted on this line"
    );
}

/// POSIX (vi, "Move to Specified Column"): `|` moves to a *column*, so a tab
/// counts for as many columns as it displays. The implementation used the
/// count as a character index, which disagrees with the screen on any line
/// containing a tab.
#[test]
fn test_pipe_moves_to_a_display_column() {
    let mut editor = Editor::new_headless();
    // With tabstop 8: 'a' is column 1, the tab spans columns 2-8, 'b' is
    // column 9 and 'c' column 10. As a character index, column 9 would run
    // past the four characters and clamp to 'c'.
    editor.set_buffer_text("a\tbc\n");
    editor.execute_keys("9|").unwrap();
    assert_eq!(editor.get_cursor().column, 2, "column 9 is 'b' at byte 2");
}

#[test]
fn test_pipe_display_column_with_consecutive_tabs() {
    let mut editor = Editor::new_headless();
    // Two tabs span columns 1-8 and 9-16; 'x' is column 17.
    editor.set_buffer_text("\t\tx\n");
    editor.execute_keys("9|").unwrap();
    assert_eq!(
        editor.get_cursor().column,
        1,
        "column 9 is the second tab, at byte 1"
    );
}

/// Without tabs a column is a character, which on multi-byte text is still
/// not a byte.
#[test]
fn test_pipe_column_is_not_a_byte_offset_on_multibyte() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("héllo\n");
    // h=1, é=2, l=3 -- and 'l' sits at byte 3 because é is two bytes.
    editor.execute_keys("3|").unwrap();
    assert_eq!(editor.get_cursor().column, 3);
}

// ============================================================================
// Phase 10 — `:g` must survive the renumbering its own commands cause
// ============================================================================
//
// POSIX marks the lines matching the pattern first, then runs the command list
// against each in order. Every command that inserts, removes or relocates
// lines renumbers the marks not yet visited. The code compensated only when
// `command.trim().starts_with('d')` -- a test that misses `.d`, `.,.+1d`, `j`,
// `m` and everything else -- so the loop then addressed unrelated lines and
// destroyed them. Marks are now followed through the buffer's edit journal.

#[test]
fn test_ex_global_delete_with_explicit_current_address() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a\nX\nb\nX\nc\n");
    editor.execute_keys(":g/X/.d\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "a\nb\nc\n");
}

#[test]
fn test_ex_global_bare_delete_still_works() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a\nX\nb\nX\nc\n");
    editor.execute_keys(":g/X/d\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "a\nb\nc\n");
}

#[test]
fn test_ex_global_delete_two_lines_per_match() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("X\ngone\nkeep\nX\ngone\nkeep\n");
    editor.execute_keys(":g/X/.,.+1d\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "keep\nkeep\n");
}

#[test]
fn test_ex_global_join_does_not_lose_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("X\ntail\nkeep\nX\ntail\n");
    editor.execute_keys(":g/X/j\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "X tail\nkeep\nX tail\n");
}

#[test]
fn test_ex_global_move_to_top() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a\nX1\nb\nX2\nc\n");
    editor.execute_keys(":g/X/m0\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "X2\nX1\na\nb\nc\n");
}

#[test]
fn test_ex_global_substitute_that_adds_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aXb\ncXd\n");
    editor.execute_keys(":g/X/s/X/-/\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "a-b\nc-d\n");
}

/// A relocating command leaves the line count alone while still renumbering
/// the marks not yet visited -- the case a count delta cannot see, and the
/// reason marks are followed through the edit journal. Matches /usr/bin/ex.
#[test]
fn test_ex_global_move_to_last_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("X1\na\nX2\nb\n");
    editor.execute_keys(":g/X/m$\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "a\nb\nX1\nX2\n");
}

#[test]
fn test_ex_global_copy_to_last_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("X1\na\nX2\nb\n");
    editor.execute_keys(":g/X/t$\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "X1\na\nX2\nb\nX1\nX2\n");
}

// ============================================================================
// `:m` and `:t` take an address, not a bare line number
// ============================================================================
//
// POSIX (ex, `copy`/`move`) gives the destination as an address, so `$`, `.`,
// `.+2`, a mark and a search are all valid. It was parsed with a plain integer
// parse, so everything but a decimal literal failed outright -- `:1m$`
// answered "invalid line number". Cross-checked against /usr/bin/ex.

#[test]
fn test_ex_move_to_last_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("X1\na\nX2\nb\n");
    editor.execute_keys(":1m$\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "a\nX2\nb\nX1\n");
}

#[test]
fn test_ex_copy_to_last_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("X1\na\n");
    editor.execute_keys(":1t$\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "X1\na\nX1\n");
}

#[test]
fn test_ex_move_to_zero_still_works() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a\nb\nc\n");
    editor.execute_keys(":3m0\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "c\na\nb\n");
}

#[test]
fn test_ex_move_to_relative_address() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a\nb\nc\nd\n");
    editor.execute_keys(":1\n").unwrap();
    editor.execute_keys(":1m.+2\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "b\nc\na\nd\n");
}

#[test]
fn test_ex_copy_to_search_address() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys(":1t/three/\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "one\ntwo\nthree\none\n");
}

/// The other half of the same rule: vi begins on the first line.
#[test]
fn test_vi_starts_on_the_first_line() {
    let dir = plib::tmp::tempdir().unwrap();
    let path = dir.path().join("f.txt");
    std::fs::write(&path, "one\ntwo\nthree\n").unwrap();

    let mut editor = Editor::new_headless();
    editor.open(path.to_str().unwrap()).unwrap();
    assert_eq!(editor.get_cursor().line, 1);
}

// ============================================================================
// Counts and registers reach the operator
// ============================================================================
//
// The parser computes `cmd.count` as count1*count2, which POSIX requires
// ("2d3w" deletes six words), but set `motion.count` to count2 alone -- and
// the operator+motion path reads only the motion's count. The doubled forms
// (`dd`, `yy`, `cc`) read `cmd.count` and were always right, which is why the
// discrepancy went unnoticed.

#[test]
fn test_count_before_operator_applies_to_the_motion() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two three four\n");
    editor.execute_keys("2dw").unwrap();
    assert_eq!(editor.get_buffer_text(), "three four\n");
}

#[test]
fn test_counts_before_and_after_operator_multiply() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a b c d e f g\n");
    editor.execute_keys("2d3w").unwrap();
    assert_eq!(editor.get_buffer_text(), "g\n");
}

#[test]
fn test_count_before_operator_applies_to_yank() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two three\n");
    editor.execute_keys("2yw").unwrap();
    assert_eq!(
        editor.get_unnamed_register().map(|r| r.text.as_str()),
        Some("one two ")
    );
}

/// `p` and `P` dropped `cmd.register` on the floor, so `"ap` always pasted
/// the unnamed register. `put_after` already accepted a register.
#[test]
fn test_put_uses_the_named_register() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\nbeta\n");
    editor.execute_keys("\"ayy").unwrap();
    editor.execute_keys("j").unwrap();
    editor.execute_keys("\"add").unwrap();
    editor.execute_keys("\"ap").unwrap();
    assert_eq!(editor.get_buffer_text(), "alpha\nbeta\n");
}

#[test]
fn test_put_before_uses_the_named_register() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\n");
    editor.execute_keys("\"byy").unwrap();
    editor.execute_keys("j\"bP").unwrap();
    assert_eq!(editor.get_buffer_text(), "one\none\ntwo\n");
}

/// A named register survives an intervening unnamed delete.
#[test]
fn test_named_register_is_not_clobbered_by_an_unnamed_delete() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("keep\ntrash\ntail\n");
    editor.execute_keys("\"kyy").unwrap();
    editor.execute_keys("jdd").unwrap();
    editor.execute_keys("\"kp").unwrap();
    assert!(
        editor.get_buffer_text().matches("keep").count() == 2,
        "expected the k register to still hold \"keep\": {:?}",
        editor.get_buffer_text()
    );
}

/// `x`, `X`, `D` and `Y` wrote straight to the small-delete or unnamed
/// register, bypassing `Registers::delete`/`yank` and so ignoring `"x`.
/// `yy` honoured it, which made `"ayy` and `"aY` disagree.
#[test]
fn test_delete_char_uses_the_named_register() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abc\n");
    editor.execute_keys("\"qx").unwrap();
    assert_eq!(editor.get_register('q').map(|r| r.text.as_str()), Some("a"));
}

#[test]
fn test_delete_char_before_uses_the_named_register() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abc\n");
    editor.execute_keys("$\"qX").unwrap();
    assert_eq!(editor.get_register('q').map(|r| r.text.as_str()), Some("b"));
}

#[test]
fn test_delete_to_end_of_line_uses_the_named_register() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world\n");
    editor.execute_keys("ll\"qD").unwrap();
    assert_eq!(
        editor.get_register('q').map(|r| r.text.as_str()),
        Some("llo world")
    );
}

#[test]
fn test_yank_lines_shorthand_agrees_with_yy() {
    let mut a = Editor::new_headless();
    a.set_buffer_text("one\ntwo\n");
    a.execute_keys("\"qyy").unwrap();

    let mut b = Editor::new_headless();
    b.set_buffer_text("one\ntwo\n");
    b.execute_keys("\"qY").unwrap();

    assert_eq!(
        a.get_register('q').map(|r| r.text.as_str()),
        b.get_register('q').map(|r| r.text.as_str()),
        "`Y` must be `yy`"
    );
}

// ============================================================================
// Undo: every command that changes the buffer must be undoable
// ============================================================================
//
// `apply_inverse` rebuilds by position and length without checking what is
// actually there, so a mutation that records nothing does not merely fail to
// undo -- the next `u` pops an unrelated older change and destroys whatever
// now sits at its position (the warning on `record_removal`, audit #V19).
//
// `check_undoable` is therefore a check on the *absence* of that hazard: if a
// command changed the buffer, one `u` must put it back exactly.

/// Returns `Err(reason)` if `keys` changed the buffer and `u` did not restore
/// it, or if `keys` changed nothing at all (so the case proves nothing).
fn check_undoable(setup: &str, keys: &str) -> Result<(), String> {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text(setup);
    let before = editor.get_buffer_text();

    editor
        .execute_keys(keys)
        .map_err(|e| format!("{:?} errored: {}", keys, e))?;
    // Leave insert mode, so the change is complete.
    let _ = editor.execute_keys("\x1b");
    let after = editor.get_buffer_text();
    if after == before {
        return Err(format!("{:?} did not change the buffer", keys));
    }

    editor
        .execute_keys("u")
        .map_err(|e| format!("{:?} then u errored: {}", keys, e))?;
    let undone = editor.get_buffer_text();
    if undone != before {
        return Err(format!(
            "{:?}: u gave {:?}, expected {:?} (after the edit it was {:?})",
            keys, undone, before, after
        ));
    }
    Ok(())
}

const UNDO_SETUP: &str = "alpha beta\ngamma delta\nepsilon zeta\n";

/// Run every case and report all the failures at once, so one broken command
/// does not hide the rest.
fn assert_all_undoable(cases: &[&str]) {
    let failures: Vec<String> = cases
        .iter()
        .filter_map(|keys| check_undoable(UNDO_SETUP, keys).err())
        .collect();
    assert!(
        failures.is_empty(),
        "{} of {} commands are not undoable:\n  {}",
        failures.len(),
        cases.len(),
        failures.join("\n  ")
    );
}

#[test]
fn test_undo_restores_character_edits() {
    assert_all_undoable(&[
        "x", "3x", "llX", "rZ", "~", "sZ", "cwZ", "CZ", "iZ", "AZ", "IZ", "aZ",
    ]);
}

#[test]
fn test_undo_restores_line_edits() {
    assert_all_undoable(&["dd", "2dd", "dw", "de", "cc", "S", "J", "oZ", "OZ", "D"]);
}

#[test]
fn test_undo_restores_put_and_shift() {
    // Only single commands here: `ddp` is two, and one `u` correctly undoes
    // just the put -- see test_undo_of_a_sequence_unwinds_one_command_at_a_time.
    assert_all_undoable(&["yyp", "yyP", "yy2p", ">>", "2>>"]);
}

/// `<` needs something to remove, so it gets its own indented setup.
#[test]
fn test_undo_restores_shift_left() {
    let setup = "\talpha\n\tbeta\n";
    for keys in ["<<", "2<<"] {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text(setup);
        editor.execute_keys(keys).unwrap();
        assert_ne!(
            editor.get_buffer_text(),
            setup,
            "{:?} changed nothing",
            keys
        );
        editor.execute_keys("u").unwrap();
        assert_eq!(editor.get_buffer_text(), setup, "{:?} did not undo", keys);
    }
}

/// POSIX `u` is its own inverse: the first undoes the last command, the
/// second puts it back. So after `dd` `p`, one `u` reverses the put and a
/// second replays it -- it does not keep walking back through the history.
#[test]
fn test_undo_reverses_only_the_last_command_and_is_its_own_inverse() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text(UNDO_SETUP);
    editor.execute_keys("ddp").unwrap();
    let after_put = editor.get_buffer_text();
    assert_eq!(after_put, "gamma delta\nalpha beta\nepsilon zeta\n");

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "gamma delta\nepsilon zeta\n",
        "the first u must undo the put, not the whole sequence"
    );

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        after_put,
        "the second u must put it back"
    );
}

#[test]
fn test_undo_restores_ex_edits() {
    assert_all_undoable(&[
        ":2d\n",
        ":1,2d\n",
        ":s/alpha/ZZ/\n",
        ":1,2j\n",
        ":1t2\n",
        ":1m2\n",
        ":1,2>\n",
        ":g/a/s/a/Z/\n",
    ]);
}

/// `:pu` needs a register to put from, and `:<` something to unindent.
#[test]
fn test_undo_restores_ex_put_and_shift_left() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text(UNDO_SETUP);
    editor.execute_keys("yy").unwrap();
    let before = editor.get_buffer_text();
    editor.execute_keys(":2pu\n").unwrap();
    assert_ne!(editor.get_buffer_text(), before);
    editor.execute_keys("u").unwrap();
    assert_eq!(editor.get_buffer_text(), before);

    let indented = "\talpha\n\tbeta\n";
    let mut editor = Editor::new_headless();
    editor.set_buffer_text(indented);
    editor.execute_keys(":1,2<\n").unwrap();
    assert_ne!(editor.get_buffer_text(), indented);
    editor.execute_keys("u").unwrap();
    assert_eq!(editor.get_buffer_text(), indented);
}

#[test]
fn test_undo_restores_operator_and_motion_edits() {
    assert_all_undoable(&[
        "dw", "2dw", "d$", "de", "dj", "cwZ", "c$Z", "ceZ", "yyp", "3x",
        // These need the cursor off the very start of the buffer to have
        // anything to act on.
        "jdk", "ll2X", "lld0",
    ]);
}

#[test]
fn test_undo_restores_repeated_and_counted_edits() {
    assert_all_undoable(&["3rZ", "2~", "2dd", "3>>", "2J", "2sZ"]);
}

/// `.` after an insert used to re-enter insert mode without ever closing the
/// group it opened, so every later change joined that stale group and one `u`
/// reversed the rest of the session.
#[test]
fn test_undo_after_dot_repeat_reverses_only_the_last_command() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text(UNDO_SETUP);
    editor.execute_keys("iX\x1b").unwrap();
    editor.execute_keys(".").unwrap();
    let after_repeat = editor.get_buffer_text();

    editor.execute_keys("dd").unwrap();
    assert_ne!(editor.get_buffer_text(), after_repeat);

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        after_repeat,
        "u after `.` must reverse the dd alone, not the whole session"
    );
}

/// The same hazard through an ex command.
#[test]
fn test_undo_after_ex_command_reverses_only_that_command() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text(UNDO_SETUP);
    editor.execute_keys("iX\x1b").unwrap();
    let after_insert = editor.get_buffer_text();

    editor.execute_keys(":2d\n").unwrap();
    editor.execute_keys("u").unwrap();
    assert_eq!(editor.get_buffer_text(), after_insert);
}

/// A global is one command: one `u` reverses every line it touched.
#[test]
fn test_undo_after_global_reverses_the_whole_command() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("Xa\nXb\nXc\nd\n");
    editor.execute_keys(":g/X/d\n").unwrap();
    assert_eq!(editor.get_buffer_text(), "d\n");
    editor.execute_keys("u").unwrap();
    assert_eq!(editor.get_buffer_text(), "Xa\nXb\nXc\nd\n");
}

/// Generated sweep over complete single commands: whatever a command does to
/// the buffer, one `u` must put back exactly.
///
/// This is the standing check that a new mutation path cannot be added without
/// recording undo -- the failure mode being not a missing undo but a
/// *destructive* one, since `apply_inverse` rebuilds by position and length
/// without looking at what is there.
#[test]
fn test_generated_single_commands_are_undoable() {
    // Each entry is one complete command, so `u` reverses exactly it.
    const COMMANDS: &[&str] = &[
        "x",
        "2x",
        "X",
        "3X",
        "rZ",
        "2rZ",
        "~",
        "3~",
        "D",
        "J",
        "2J",
        "dd",
        "2dd",
        "3dd",
        "dw",
        "2dw",
        "de",
        "d$",
        "dj",
        "dk",
        "d0",
        "cwZ",
        "ceZ",
        "c$Z",
        "cc",
        "2cc",
        "sZ",
        "2sZ",
        "S",
        "iZ",
        "aZ",
        "IZ",
        "AZ",
        "oZ",
        "OZ",
        "CZ",
        ">>",
        "2>>",
        "<<",
        "p",
        "P",
        "2p",
        ":d\n",
        ":1,2d\n",
        ":j\n",
        ":s/a/Z/\n",
        ":s/a/Z/g\n",
        ":1t2\n",
        ":1m3\n",
        ":>\n",
        ":<\n",
        ":pu\n",
        ":g/a/s/a/Z/\n",
        ":v/a/s/e/Z/\n",
        ":1,3>\n",
    ];
    // Indented and varied, so `<<`, `p` and the shifts all have something to do.
    const SETUPS: &[&str] = &[
        "\talpha beta\n\tgamma delta\n\tepsilon zeta\neta theta\n",
        "one\ntwo\nthree\nfour\nfive\n",
        "  a\n\tb\n c\nd\n",
    ];

    let mut state: u64 = 0xDEAD_BEEF_CAFE_F00D;
    let mut next = || {
        state = state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        (state >> 33) as usize
    };

    let mut failures = Vec::new();
    for _ in 0..3000 {
        let setup = SETUPS[next() % SETUPS.len()];
        let keys = COMMANDS[next() % COMMANDS.len()];
        // A cursor somewhere other than the very start, so the backward and
        // upward commands have something to act on.
        let lead = ["", "j", "jl", "jjll", "l", "G", "jj"][next() % 7];

        let mut editor = Editor::new_headless();
        editor.set_buffer_text(setup);
        // Fill the unnamed register so `p`/`P`/`:pu` are not no-ops.
        let _ = editor.execute_keys("yy");
        let _ = editor.execute_keys(lead);
        let before = editor.get_buffer_text();

        if editor.execute_keys(keys).is_err() {
            continue; // an invalid command here is not an undo failure
        }
        let _ = editor.execute_keys("\x1b");
        if editor.get_buffer_text() == before {
            continue; // nothing to undo
        }

        if editor.execute_keys("u").is_err() {
            failures.push(format!("{:?} after {:?}: u errored", keys, lead));
            continue;
        }
        if editor.get_buffer_text() != before {
            failures.push(format!(
                "{:?} after {:?} on {:?}: u gave {:?}, expected {:?}",
                keys,
                lead,
                setup,
                editor.get_buffer_text(),
                before
            ));
        }
    }
    failures.dedup();
    assert!(
        failures.is_empty(),
        "{} commands did not undo cleanly:\n  {}",
        failures.len(),
        failures
            .iter()
            .take(12)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n  ")
    );
}

// ============================================================================
// Motion classification: inclusive, exclusive, linewise
// ============================================================================
//
// An operator's region depends on how the motion is classified. The motions
// computed `linewise` correctly and `execute_motion_get_pos` threw it away by
// returning only the position, and every operator+motion path then hardcoded
// a character-mode range -- so `dj` deleted from the cursor column on one line
// to the cursor column on the next instead of both whole lines. Nothing
// tracked inclusivity at all outside a special case for `cw`, so every
// inclusive motion came up one character short.

#[test]
fn test_delete_to_end_of_word_is_inclusive() {
    // POSIX (vi, "Move to End-of-Word"): the region includes the last
    // character of the word.
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two\n");
    editor.execute_keys("de").unwrap();
    assert_eq!(editor.get_buffer_text(), " two\n");
}

#[test]
fn test_delete_to_end_of_line_is_inclusive() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world\n");
    editor.execute_keys("d$").unwrap();
    assert_eq!(editor.get_buffer_text(), "\n");
}

#[test]
fn test_yank_to_end_of_line_is_inclusive() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello\n");
    editor.execute_keys("y$").unwrap();
    assert_eq!(
        editor.get_unnamed_register().map(|r| r.text.as_str()),
        Some("hello")
    );
}

#[test]
fn test_find_char_forward_is_inclusive() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world\n");
    editor.execute_keys("dfo").unwrap();
    assert_eq!(editor.get_buffer_text(), " world\n");
}

#[test]
fn test_till_char_forward_is_inclusive_of_the_char_before() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world\n");
    editor.execute_keys("dto").unwrap();
    assert_eq!(editor.get_buffer_text(), "o world\n");
}

/// `F` and `T` search backwards, so the character under the cursor survives.
#[test]
fn test_find_char_backward_is_exclusive() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world\n");
    editor.execute_keys("$dFo").unwrap();
    assert_eq!(editor.get_buffer_text(), "hello wd\n");
}

#[test]
fn test_match_bracket_is_inclusive() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("(abc)x\n");
    editor.execute_keys("d%").unwrap();
    assert_eq!(editor.get_buffer_text(), "x\n");
}

#[test]
fn test_delete_down_is_linewise() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys("ldj").unwrap();
    assert_eq!(editor.get_buffer_text(), "three\n");
}

#[test]
fn test_delete_up_is_linewise() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys("jjldk").unwrap();
    assert_eq!(editor.get_buffer_text(), "one\n");
}

#[test]
fn test_delete_to_line_is_linewise() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys("jldG").unwrap();
    assert_eq!(editor.get_buffer_text(), "one\n");
}

#[test]
fn test_yank_down_is_linewise() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys("lyj").unwrap();
    assert_eq!(
        editor.get_unnamed_register().map(|r| r.linewise),
        Some(true),
        "a linewise yank must be stored linewise, so `p` puts whole lines"
    );
}

/// POSIX (vi, "Change"): `cw` on a non-blank behaves as `ce`, leaving the
/// blanks after the word -- but only on a non-blank. On a blank it is an
/// ordinary `w`.
#[test]
fn test_change_word_on_a_blank_is_not_change_to_end_of_word() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a   bcd efg\n");
    // On a blank, `cw` is an ordinary `w`: the region runs to the start of the
    // next word, so all three blanks go. The `ce` substitution would instead
    // have consumed "bcd".
    editor.execute_keys("lcwZ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "aZbcd efg\n");
}

#[test]
fn test_change_word_on_a_non_blank_keeps_the_following_blanks() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one   two\n");
    editor.execute_keys("cwZ\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "Z   two\n");
}

/// Every motion the parser accepts after an operator must actually be
/// dispatched. `execute_motion` fell through to `None` for a dozen of them,
/// so `dH`, `d+`, `d'a` and friends silently did nothing at all -- the worst
/// outcome, since the user sees no diagnostic.
#[test]
fn test_every_parseable_motion_works_as_an_operator_target() {
    let setup = "one\ntwo\nthree\nfour\nfive\nsix\n";
    let cases: &[(&str, &str)] = &[
        ("dH", "from the top of the screen"),
        ("dL", "to the bottom of the screen"),
        ("dM", "to the middle of the screen"),
        ("d+", "to the next line"),
        ("d-", "to the previous line"),
        ("d_", "the current line"),
        ("d'a", "to a marked line"),
        ("d`a", "to a marked position"),
        ("d|", "to a column"),
    ];

    let mut silent = Vec::new();
    for (keys, what) in cases {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text(setup);
        // Sit in the middle and set mark `a` on the first line, so backward
        // and mark motions have somewhere to go.
        editor.execute_keys("ma").unwrap();
        editor.execute_keys("jjl").unwrap();

        let before = editor.get_buffer_text();
        let outcome = editor.execute_keys(keys);
        if outcome.is_ok() && editor.get_buffer_text() == before {
            silent.push(format!("{:?} ({}) did nothing", keys, what));
        }
    }
    assert!(
        silent.is_empty(),
        "{} motions are silent no-ops after an operator:\n  {}",
        silent.len(),
        silent.join("\n  ")
    );
}

/// The newly dispatched motions must produce the *right* region, not merely
/// a non-empty one.
#[test]
fn test_line_relative_motions_are_linewise_as_operator_targets() {
    let setup = "one\ntwo\nthree\nfour\n";

    let mut editor = Editor::new_headless();
    editor.set_buffer_text(setup);
    editor.execute_keys("jl").unwrap();
    editor.execute_keys("d+").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "one\nfour\n",
        "`d+` deletes the current line and the next, whole"
    );

    let mut editor = Editor::new_headless();
    editor.set_buffer_text(setup);
    editor.execute_keys("jjl").unwrap();
    editor.execute_keys("d-").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "one\nfour\n",
        "`d-` deletes the current line and the previous, whole"
    );

    let mut editor = Editor::new_headless();
    editor.set_buffer_text(setup);
    editor.execute_keys("jl").unwrap();
    editor.execute_keys("d_").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "one\nthree\nfour\n",
        "`d_` deletes just the current line"
    );
}

#[test]
fn test_mark_motions_as_operator_targets() {
    let setup = "one\ntwo\nthree\nfour\n";

    // `'a` addresses the line, so the region is whole lines.
    let mut editor = Editor::new_headless();
    editor.set_buffer_text(setup);
    editor.execute_keys("ma").unwrap();
    editor.execute_keys("jj").unwrap();
    editor.execute_keys("d'a").unwrap();
    assert_eq!(editor.get_buffer_text(), "four\n");

    // A backtick mark addresses the character, so the region is exclusive.
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abcdef\n");
    editor.execute_keys("ll").unwrap();
    editor.execute_keys("ma").unwrap();
    editor.execute_keys("$").unwrap();
    editor.execute_keys("d`a").unwrap();
    assert_eq!(editor.get_buffer_text(), "abf\n");
}

/// An unset mark is an error, not a silent no-op on the whole buffer.
#[test]
fn test_operator_with_an_unset_mark_changes_nothing() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\n");
    let _ = editor.execute_keys("d'z");
    assert_eq!(editor.get_buffer_text(), "one\ntwo\n");
}

/// Filtering a buffer through a command writes all of the input before
/// reading any of the output. Once the child's output fills the pipe buffer
/// (~64 KB) it blocks writing, while the editor is still blocked writing its
/// input -- both wait for the other and the editor hangs.
///
/// Run on a worker thread so a regression fails the suite instead of hanging
/// it.
#[test]
fn test_shell_filter_does_not_deadlock_on_a_large_buffer() {
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        // Comfortably past a pipe buffer in both directions.
        let text: String = (0..20_000)
            .map(|i| format!("line {} of filler text\n", i))
            .collect();
        let expected_lines = text.lines().count();

        let mut editor = Editor::new_headless();
        editor.set_buffer_text(&text);
        let r = editor
            .execute_keys(":%!cat\n")
            .map(|()| (editor.get_buffer_text().lines().count(), expected_lines));
        let _ = tx.send(r);
    });

    match rx.recv_timeout(std::time::Duration::from_secs(20)) {
        Ok(Ok((got, want))) => assert_eq!(got, want, "cat must round-trip every line"),
        Ok(Err(e)) => panic!("filter failed: {}", e),
        Err(_) => panic!("`:%!cat` deadlocked on a buffer larger than the pipe buffer"),
    }
}

/// A filter that exits without reading all its input gives the writer EPIPE.
/// That is the command's choice, not an editor error.
#[test]
fn test_shell_filter_tolerates_a_command_that_stops_reading() {
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let text: String = (0..20_000).map(|i| format!("line {}\n", i)).collect();
        let mut editor = Editor::new_headless();
        editor.set_buffer_text(&text);
        let r = editor
            .execute_keys(":%!head -1\n")
            .map(|()| editor.get_buffer_text());
        let _ = tx.send(r);
    });

    match rx.recv_timeout(std::time::Duration::from_secs(20)) {
        Ok(Ok(text)) => assert_eq!(text.trim_end(), "line 0"),
        Ok(Err(e)) => panic!("a filter that stops reading must not be an error: {}", e),
        Err(_) => panic!("`:%!head -1` deadlocked"),
    }
}

// ============================================================================
// Remaining conformance items
// ============================================================================

/// POSIX (ex, `set`): "set [option[=[value]] ...]" -- the arguments are
/// <blank>-separated, which is how an ordinary .exrc line is written.
#[test]
fn test_set_accepts_several_options_on_one_line() {
    let mut editor = Editor::new_headless();
    editor.execute_keys(":set ai number\n").unwrap();
    assert!(editor.options().autoindent, "ai should be on");
    assert!(editor.options().number, "number should be on");
    assert!(
        !editor.is_error_message(),
        "unexpected error: {:?}",
        editor.get_message()
    );
}

#[test]
fn test_set_accepts_several_options_with_values() {
    let mut editor = Editor::new_headless();
    editor.execute_keys(":set sw=4 ts=2 ai\n").unwrap();
    assert_eq!(editor.options().shiftwidth, 4);
    assert_eq!(editor.options().tabstop, 2);
    assert!(editor.options().autoindent);
}

/// `r` with a count replaces that many characters, each with the same one.
/// It called the single-character replace `count` times without moving, so
/// `3rx` rewrote the same character three times.
#[test]
fn test_replace_char_with_a_count_replaces_that_many() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abcdef\n");
    editor.execute_keys("3rZ").unwrap();
    assert_eq!(editor.get_buffer_text(), "ZZZdef\n");
}

/// POSIX: `r` with a count larger than the characters left on the line is an
/// error, and the line is unchanged.
#[test]
fn test_replace_char_past_end_of_line_changes_nothing() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("ab\n");
    let _ = editor.execute_keys("5rZ");
    assert_eq!(editor.get_buffer_text(), "ab\n");
}

/// `^` moves to the first non-blank; on an all-blank line there is none, so
/// the cursor goes to the last character rather than to column 0.
#[test]
fn test_first_non_blank_on_an_all_blank_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("    \n");
    editor.execute_keys("$").unwrap();
    editor.execute_keys("^").unwrap();
    assert_eq!(
        editor.get_cursor().column,
        3,
        "an all-blank line has no non-blank, so `^` stays on the last character"
    );
}

/// `#` in a shell command expands to the alternate file. `open` captured the
/// "previous" current file *after* `set_current_file` had already replaced
/// it, so `#` expanded to the file just opened -- the same as `%`.
#[test]
fn test_alternate_file_expands_to_the_previous_file() {
    let dir = plib::tmp::tempdir().unwrap();
    let first = dir.path().join("first.txt");
    let second = dir.path().join("second.txt");
    std::fs::write(&first, "one\n").unwrap();
    std::fs::write(&second, "two\n").unwrap();

    let mut editor = Editor::new_headless();
    editor.open(first.to_str().unwrap()).unwrap();
    editor.open(second.to_str().unwrap()).unwrap();

    editor.execute_keys(":r !echo #\n").unwrap();
    let text = editor.get_buffer_text();
    assert!(
        text.contains("first.txt"),
        "`#` must be the alternate (previous) file, got {:?}",
        text
    );
    assert!(
        !text.contains("second.txt"),
        "`#` must not be the current file, got {:?}",
        text
    );
}

/// Ctrl-^ edits the alternate file, which discards the buffer. POSIX requires
/// the same warning as `:e`; it opened unconditionally and the unsaved work
/// went with it.
#[test]
fn test_ctrl_caret_warns_before_discarding_a_modified_buffer() {
    let dir = plib::tmp::tempdir().unwrap();
    let first = dir.path().join("first.txt");
    let second = dir.path().join("second.txt");
    std::fs::write(&first, "one\n").unwrap();
    std::fs::write(&second, "two\n").unwrap();

    let mut editor = Editor::new_headless();
    editor.open(first.to_str().unwrap()).unwrap();
    editor.open(second.to_str().unwrap()).unwrap();
    editor.execute_keys("iEDITED\x1b").unwrap();

    editor.execute_keys("\x1e").unwrap(); // Ctrl-^
    assert!(
        editor.get_buffer_text().contains("EDITED"),
        "the modified buffer must survive; got {:?}",
        editor.get_buffer_text()
    );
    assert!(
        editor.is_error_message(),
        "expected a warning, got {:?}",
        editor.get_message()
    );
}

/// POSIX (vi, "Delete"): "If the motion command is `w` or `W`, and the last
/// word on the line is being deleted, the region shall end at the last
/// character of the line."
///
/// `w` on the last word has nowhere to advance to within the line, so the
/// region came out empty and `dw` deleted nothing at all.
#[test]
fn test_delete_word_on_the_last_word_of_a_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two\n");
    editor.execute_keys("wdw").unwrap();
    assert_eq!(editor.get_buffer_text(), "one \n");
}

/// The same rule keeps `dw` from joining lines.
#[test]
fn test_delete_word_does_not_join_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two\nthree four\n");
    editor.execute_keys("wdw").unwrap();
    assert_eq!(editor.get_buffer_text(), "one \nthree four\n");
}

#[test]
fn test_yank_word_on_the_last_word_of_a_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two\nthree\n");
    editor.execute_keys("wyw").unwrap();
    assert_eq!(
        editor.get_unnamed_register().map(|r| r.text.as_str()),
        Some("two")
    );
}

// ============================================================================
// Code-review findings
// ============================================================================

/// POSIX (vi, "Delete"): the `w`/`W` end-of-line rule applies when *the last
/// word moved over* ends a line -- not whenever the motion happens to cross
/// one. With a count the motion may legitimately span lines.
#[test]
fn test_delete_word_with_a_count_may_cross_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two\nthree four\n");
    editor.execute_keys("d3w").unwrap();
    assert_eq!(editor.get_buffer_text(), "four\n");
}

#[test]
fn test_yank_word_with_a_count_may_cross_lines() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two\nthree four\n");
    editor.execute_keys("y3w").unwrap();
    assert_eq!(
        editor.get_unnamed_register().map(|r| r.text.as_str()),
        Some("one two\nthree ")
    );
}

/// ...but a count whose final step still ends a line keeps the rule.
#[test]
fn test_delete_word_with_a_count_still_stops_at_end_of_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one two\nthree four\n");
    editor.execute_keys("d2w").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "\nthree four\n",
        "the second word ends line 1, so the region stops there"
    );
}

/// `c` with a line-wise motion has the same hazard as `cc`: `change_lines`
/// empties the first line rather than removing it, so a linewise delete
/// record makes `u` restore the lines *and* leave the emptied one behind.
#[test]
fn test_undo_after_change_with_a_linewise_motion() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("aaa\nbbb\nccc\n");
    editor.execute_keys("cjX\x1b").unwrap();
    editor.execute_keys("u").unwrap();
    assert_eq!(editor.get_buffer_text(), "aaa\nbbb\nccc\n");
}

/// The shift operator's undo snapshot must cover the lines the *motion*
/// selects, not `count` lines from the cursor -- otherwise `u` reverts only
/// part of the change and leaves a state that is neither before nor after.
#[test]
fn test_undo_after_shift_with_a_motion() {
    for keys in [">j", ">G", "j>k"] {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text("aaa\nbbb\nccc\n");
        editor.execute_keys(keys).unwrap();
        assert_ne!(editor.get_buffer_text(), "aaa\nbbb\nccc\n");
        editor.execute_keys("u").unwrap();
        assert_eq!(
            editor.get_buffer_text(),
            "aaa\nbbb\nccc\n",
            "{:?} did not fully undo",
            keys
        );
    }
}

/// An inclusive motion that runs *backwards* has to include the character
/// under the cursor: character ranges are end-exclusive, so extending the end
/// only helps when the motion went forward.
#[test]
fn test_match_bracket_is_inclusive_backwards_too() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("(abc)\n");
    editor.execute_keys("$d%").unwrap();
    assert_eq!(editor.get_buffer_text(), "\n");
}

#[test]
fn test_find_char_backward_inclusive_forms_keep_the_cursor_char() {
    // `F`/`T` are exclusive, so the cursor's character survives -- pinned so
    // the backward-inclusive fix does not change them.
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("hello world\n");
    editor.execute_keys("$dFo").unwrap();
    assert_eq!(editor.get_buffer_text(), "hello wd\n");
}

/// `^` anchors to the start of the line, so a search that resumes mid-line
/// must not let it match there.
#[test]
fn test_search_does_not_reanchor_caret_mid_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abc\nzzz\n");
    let r = editor.execute_keys("/^b\n");
    assert!(
        r.is_err() || editor.is_error_message(),
        "`^b` must not match mid-line; cursor went to line {} col {}",
        editor.get_cursor().line,
        editor.get_cursor().column
    );
}

#[test]
fn test_backward_search_does_not_reanchor_caret_mid_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abc\nzzz\n");
    editor.execute_keys("G$").unwrap();
    let r = editor.execute_keys("?^b\n");
    assert!(
        r.is_err() || editor.is_error_message(),
        "`^b` must not match mid-line; cursor went to line {} col {}",
        editor.get_cursor().line,
        editor.get_cursor().column
    );
}

/// A display column that falls *inside* a tab belongs to the tab.
#[test]
fn test_pipe_column_inside_a_tab_lands_on_the_tab() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("a\tbc\n");
    // With tabstop 8 the tab spans display columns 2-8, so 5 is inside it.
    editor.execute_keys("5|").unwrap();
    assert_eq!(
        editor.get_cursor().column,
        1,
        "column 5 is the tab, at byte 1"
    );
}

// ============================================================================
// :map / :ab definitions -- storage and listing
// ============================================================================

/// `:map` and `:ab` define; `:unmap` and `:una` remove; the no-argument forms
/// list (95080-95083, 94864). Expansion is not wired up yet, so what is
/// observable here is that a definition is stored and comes back.
#[test]
fn test_map_and_ab_define_and_list() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");

    editor.execute_keys(":map q dd\n").unwrap();
    assert!(!editor.is_error_message(), "{:?}", editor.get_message());

    editor.execute_keys(":map\n").unwrap();
    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(msg.contains('q') && msg.contains("dd"), "got {msg:?}");

    editor.execute_keys(":ab teh the\n").unwrap();
    editor.execute_keys(":ab\n").unwrap();
    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(msg.contains("teh") && msg.contains("the"), "got {msg:?}");
}

/// 95089-95092: the `!` form is a separate list, so one lhs can hold two
/// definitions at once, and `:map` must not show the `:map!` one.
#[test]
fn test_map_bang_is_a_separate_list() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");
    editor.execute_keys(":map q dd\n").unwrap();
    editor.execute_keys(":map! q xyz\n").unwrap();

    editor.execute_keys(":map\n").unwrap();
    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(msg.contains("dd") && !msg.contains("xyz"), "got {msg:?}");

    editor.execute_keys(":map!\n").unwrap();
    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(msg.contains("xyz") && !msg.contains("dd"), "got {msg:?}");

    // Removing from one list leaves the other alone.
    editor.execute_keys(":unmap q\n").unwrap();
    assert!(!editor.is_error_message(), "{:?}", editor.get_message());
    editor.execute_keys(":map!\n").unwrap();
    assert!(editor.get_message().unwrap_or_default().contains("xyz"));
}

/// Removing something that is not there is an error for both commands
/// (95457-95462 for `unmap`, 95436-95437 for `una`), and `:unmap!` addresses
/// only the text input list.
#[test]
fn test_unmap_and_una_report_a_missing_entry() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");

    editor.execute_keys(":unmap nosuch\n").unwrap();
    assert!(editor.is_error_message(), "{:?}", editor.get_message());
    editor.execute_keys(":una nosuch\n").unwrap();
    assert!(editor.is_error_message(), "{:?}", editor.get_message());

    // Defined in the command list only, so the `!` form must still fail.
    editor.execute_keys(":map q dd\n").unwrap();
    editor.execute_keys(":unmap! q\n").unwrap();
    assert!(
        editor.is_error_message(),
        "unmap! addresses the text input list; got {:?}",
        editor.get_message()
    );
    editor.execute_keys(":unmap q\n").unwrap();
    assert!(!editor.is_error_message(), "{:?}", editor.get_message());
}

/// `is_error` describes `message`, so clearing one has to clear the other.
/// Four sites assigned `message = None` on its own, so the flag outlived the
/// message it described and the next command that succeeded still read as
/// having failed. Surfaced by the `:unmap` test above, which does exactly this
/// sequence.
#[test]
fn test_the_error_flag_does_not_outlive_its_message() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");

    editor.execute_keys(":unmap nosuch\n").unwrap();
    assert!(editor.is_error_message(), "the failure is reported");

    // Any subsequent keystroke discards the message; the flag must go with it.
    editor.execute_keys("j").unwrap();
    assert!(editor.get_message().is_none(), "the message is gone");
    assert!(
        !editor.is_error_message(),
        "and so is the flag that described it"
    );
}

/// The `^V` quoting proved at the ex-command-line level in
/// `test_ctrl_v_on_the_ex_line_quotes_the_next_key` now has somewhere to land:
/// `:map Q :wq^V^M` must store a carriage return, which the listing shows as
/// `^M` rather than ending the line.
#[test]
fn test_ctrl_v_quoted_cr_survives_into_the_stored_map() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");
    // The quoted Enter is a CR in the replacement; a second Enter submits.
    editor.execute_keys(":map Q :wq\x16\n").unwrap();
    editor.execute_keys("\n").unwrap();
    assert!(!editor.is_error_message(), "{:?}", editor.get_message());

    editor.execute_keys(":map\n").unwrap();
    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(
        msg.contains(":wq^M"),
        "the CR must be stored and shown in caret notation; got {msg:?}"
    );
}

// ============================================================================
// Tag stack (:pop, :tags) -- extensions, see NONPOSIX.md
// ============================================================================

/// Build a tags file plus two sources and return (dir, a.c path, b.c path).
///
/// Paths written into the tags file are absolute, and the editor is opened with
/// the same strings. `goto_tag` decides "is this file already open?" by
/// comparing the two as written, so a relative name here and an absolute one
/// there would silently take the reopen path and reset the cursor.
fn tag_fixture() -> (plib::tmp::TempDir, String, String) {
    let dir = plib::tmp::tempdir().unwrap();
    let a = dir.path().join("a.c");
    let b = dir.path().join("b.c");
    std::fs::write(&a, "one\ntwo\nint local() { }\nfour\nfive\n").unwrap();
    std::fs::write(&b, "alpha\nbravo\nint helper() { }\n").unwrap();
    std::fs::write(
        dir.path().join("tags"),
        format!(
            "local\t{}\t3\nhelper\t{}\t3\n",
            a.to_str().unwrap(),
            b.to_str().unwrap()
        ),
    )
    .unwrap();
    let (as_, bs) = (
        a.to_str().unwrap().to_string(),
        b.to_str().unwrap().to_string(),
    );
    (dir, as_, bs)
}

fn set_tags_option(editor: &mut Editor, dir: &std::path::Path) {
    let tags = dir.join("tags");
    editor
        .execute_initial_command(&format!("set tags={}", tags.to_str().unwrap()))
        .unwrap();
}

/// `:tag` within one file, then `:pop` back. A jump that does not change the
/// file still has to push: `^]` then a way back inside one source is the common
/// case, and the whole point of the stack.
#[test]
fn test_pop_returns_within_the_same_file() {
    let (dir, a, _b) = tag_fixture();
    let mut editor = Editor::new_headless();
    set_tags_option(&mut editor, dir.path());
    editor.open(&a).unwrap();
    editor.execute_keys("3G").unwrap();
    editor.execute_keys("1G").unwrap();
    assert_eq!(editor.get_cursor().line, 1);

    editor.execute_keys(":tag local\n").unwrap();
    assert_eq!(editor.get_cursor().line, 3, "jumped to the definition");

    editor.execute_keys(":pop\n").unwrap();
    assert_eq!(editor.get_cursor().line, 1, "and back to where we started");
}

/// Across files: `:pop` reopens the origin and restores the position in it.
#[test]
fn test_pop_returns_across_files() {
    let (dir, a, _b) = tag_fixture();
    let mut editor = Editor::new_headless();
    set_tags_option(&mut editor, dir.path());
    editor.open(&a).unwrap();
    editor.execute_keys("4G").unwrap();

    editor.execute_keys(":tag helper\n").unwrap();
    assert!(
        editor.get_buffer_text().contains("bravo"),
        "should be in b.c now, got {:?}",
        editor.get_buffer_text()
    );

    editor.execute_keys(":pop\n").unwrap();
    assert!(
        editor.get_buffer_text().contains("four"),
        "should be back in a.c, got {:?}",
        editor.get_buffer_text()
    );
    assert_eq!(editor.get_cursor().line, 4, "at the line we left from");
}

/// `^]` pushes exactly as `:tag` does -- the push lives in `goto_tag`, so both
/// entry points get it without knowing about the stack.
#[test]
fn test_ctrl_bracket_pushes_the_tag_stack() {
    let (dir, a, _b) = tag_fixture();
    let mut editor = Editor::new_headless();
    set_tags_option(&mut editor, dir.path());
    editor.open(&a).unwrap();
    // Put the cursor on the word `local` on line 3... start from line 1 and
    // write the word there so `^]` has something to read.
    editor.execute_keys("1Gcwlocal\x1b").unwrap();
    editor.execute_keys("0").unwrap();

    editor.execute_keys("\x1d").unwrap(); // ^]
    assert_eq!(editor.get_cursor().line, 3, "^] jumped to the definition");

    editor.execute_keys(":pop\n").unwrap();
    assert_eq!(editor.get_cursor().line, 1, ":pop undid the ^] jump");
}

/// A tag that is not in the tags file moves nothing, so it must not push --
/// otherwise a later `:pop` would "return" to a place we never left.
#[test]
fn test_a_failed_tag_lookup_does_not_push() {
    let (dir, a, _b) = tag_fixture();
    let mut editor = Editor::new_headless();
    set_tags_option(&mut editor, dir.path());
    editor.open(&a).unwrap();

    editor.execute_keys(":tag nosuchtag\n").unwrap();
    assert!(editor.is_error_message(), "a missing tag reports an error");

    editor.execute_keys(":pop\n").unwrap();
    assert!(
        editor.is_error_message(),
        "the stack must still be empty; got {:?}",
        editor.get_message()
    );
    assert!(editor
        .get_message()
        .unwrap_or_default()
        .contains("tag stack empty"));
}

/// `:pop` on an empty stack is an error, and says which.
#[test]
fn test_pop_on_an_empty_stack_reports_itself() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");
    editor.execute_keys(":pop\n").unwrap();
    assert!(editor.is_error_message());
    assert!(
        editor
            .get_message()
            .unwrap_or_default()
            .contains("tag stack empty"),
        "got {:?}",
        editor.get_message()
    );
}

/// A modified buffer refuses the cross-file return -- and keeps the entry, so
/// the user can write and pop again. Discarding it would make the refusal
/// unrecoverable, which is the opposite of what a guard is for.
#[test]
fn test_pop_refuses_a_modified_buffer_without_losing_the_entry() {
    let (dir, a, _b) = tag_fixture();
    let mut editor = Editor::new_headless();
    set_tags_option(&mut editor, dir.path());
    editor.open(&a).unwrap();
    editor.execute_keys("2G").unwrap();
    editor.execute_keys(":tag helper\n").unwrap();

    editor.execute_keys("iEDITED\x1b").unwrap();
    editor.execute_keys(":pop\n").unwrap();
    assert!(editor.is_error_message(), "a modified buffer refuses");
    assert!(
        editor.get_buffer_text().contains("EDITED"),
        "and the edit survives"
    );

    // The entry is still there: undo the change and the pop now works.
    editor.execute_keys(":w\n").unwrap();
    editor.execute_keys(":pop\n").unwrap();
    assert!(
        editor.get_buffer_text().contains("one"),
        "the retry must return to a.c, so the entry was kept; got {:?}",
        editor.get_buffer_text()
    );
    assert_eq!(editor.get_cursor().line, 2);
}

/// `:tags` names the tag and where it will return to.
#[test]
fn test_tags_lists_the_stack() {
    let (dir, a, _b) = tag_fixture();
    let mut editor = Editor::new_headless();
    set_tags_option(&mut editor, dir.path());
    editor.open(&a).unwrap();
    editor.execute_keys("2G").unwrap();
    editor.execute_keys(":tag helper\n").unwrap();

    editor.execute_keys(":tags\n").unwrap();
    let msg = editor.get_message().unwrap_or_default().to_string();
    assert!(!editor.is_error_message(), "listing is not a failure");
    assert!(
        msg.contains("helper") && msg.contains("a.c") && msg.contains("line 2"),
        "must name the tag and the position :pop returns to; got {msg:?}"
    );
}

/// An empty stack lists a line rather than erroring, so `:tags` stays usable
/// from a script running under `-s`.
#[test]
fn test_tags_on_an_empty_stack_is_not_an_error() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");
    editor.execute_keys(":tags\n").unwrap();
    assert!(!editor.is_error_message());
    assert!(editor
        .get_message()
        .unwrap_or_default()
        .contains("tag stack empty"));
}

/// A `^V` typed on the colon line quotes the next key, which is the only way to
/// get a CR into a map's replacement -- `:map Q :wq^V^M` is the commonest
/// mapping there is. `Key::Ctrl('v')` used to fall into `handle_ex_key`'s
/// catch-all and vanish, so the sequence could not be entered at all.
///
/// The observable half of that is here: a quoted Enter is a character in the
/// command line, not the end of it. That the quoting survives all the way into
/// the stored replacement is asserted once `:map` is implemented.
#[test]
fn test_ctrl_v_on_the_ex_line_quotes_the_next_key() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");

    // :map Q :wq^V<Enter> -- the ^V makes the Enter a quoted CR.
    editor.execute_keys(":map Q :wq\x16\n").unwrap();
    assert_eq!(
        editor.get_mode(),
        Mode::Ex,
        "a quoted Enter must not submit the command line"
    );
    assert!(
        editor.get_message().is_none(),
        "nothing should have run yet; got {:?}",
        editor.get_message()
    );

    // An unquoted Enter does submit it, and the definition takes.
    editor.execute_keys("\n").unwrap();
    assert_eq!(editor.get_mode(), Mode::Command);
    assert!(
        !editor.is_error_message(),
        "the line should have parsed as a map definition; got {:?}",
        editor.get_message()
    );
}

/// Erasing a quoted character removes the whole pair: two chars in the buffer,
/// but one keystroke to the user.
#[test]
fn test_backspace_erases_a_quoted_pair_on_the_ex_line() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");
    // Type `:ab x ` then a quoted space, erase it, then finish with a real
    // replacement. If the erase left the stray ^V behind, the lhs would not be
    // `x` and the diagnostic would name something else.
    editor.execute_keys(":ab x \x16 \x7f").unwrap();
    editor.execute_keys("y\n").unwrap();
    assert!(
        !editor.is_error_message(),
        "should have parsed as an abbreviation; got {:?}",
        editor.get_message()
    );
    // And the lhs really is `x`: removing exactly `x` succeeds, which it could
    // not if the erase had left the stray ^V attached to it.
    editor.execute_keys(":una x\n").unwrap();
    assert!(
        !editor.is_error_message(),
        "the lhs should be exactly `x`; got {:?}",
        editor.get_message()
    );
}

// ============================================================================
// :map expansion
// ============================================================================

/// The basic case, and the count case beside it. A count is part of the
/// command, not an argument to it, and 96607-96608 requires that a digit lhs
/// work at all -- so expansion is not suppressed while a count is accumulating.
#[test]
fn test_command_map_expands() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\nfour\n");
    editor.execute_keys(":map q dd\n").unwrap();

    editor.execute_keys("q").unwrap();
    assert_eq!(editor.get_buffer_text(), "two\nthree\nfour\n");

    editor.execute_keys("2q").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "four\n",
        "a count reaches the map"
    );
}

/// 96598-96600, the case POSIX pins by name: "if the character 'x' was mapped
/// to 'y', the command fx searched for the 'x' character, not the 'y'
/// character. POSIX.1-2024 requires this behavior." The same holds everywhere
/// the vi parser is waiting for an argument rather than a command.
#[test]
fn test_no_expansion_in_argument_position() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("axbycz\n");
    editor.execute_keys(":map x y\n").unwrap();

    // f takes a character argument: it must find the literal `x` at column 1.
    editor.execute_keys("0fx").unwrap();
    assert_eq!(editor.get_cursor().column, 1, "fx must find x, not y");

    // r takes one too: `rx` must write an `x`.
    editor.execute_keys("0rx").unwrap();
    assert_eq!(&editor.get_buffer_text()[..1], "x");

    // And a register name after `"` is an argument, not a command.
    editor.set_buffer_text("alpha\nbravo\n");
    editor.execute_keys("\"xyy").unwrap();
    assert_eq!(
        editor.get_register('x').map(|r| r.text.clone()),
        Some("alpha\n".to_string()),
        "the register name must be the literal x"
    );
}

/// 95097-95098: a `^V`-escaped character "shall not be part of a match to an
/// lhs". 96600-96606 makes that required from the second character on and
/// permitted for the first; it is implemented uniformly.
#[test]
fn test_ctrl_v_suppresses_a_command_map() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\n");
    editor.execute_keys(":map q dd\n").unwrap();

    editor.execute_keys("\x16q").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "one\ntwo\n",
        "a quoted q must not fire the map"
    );
}

/// A multi-key lhs holds its keys until the match resolves. POSIX leaves the
/// wait unspecified (95116-95118); under shortest-match a held prefix can only
/// be a strict prefix, so waiting for the next keystroke always resolves it.
#[test]
fn test_multi_key_map_waits_for_the_rest() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys(":map ab dd\n").unwrap();

    // `a` alone is a prefix: nothing happens yet, and in particular the `a`
    // has not entered insert mode.
    editor.execute_keys("a").unwrap();
    assert_eq!(editor.get_buffer_text(), "one\ntwo\nthree\n");
    assert_eq!(editor.get_mode(), Mode::Command, "held, not dispatched");

    editor.execute_keys("b").unwrap();
    assert_eq!(editor.get_buffer_text(), "two\nthree\n");
}

/// When a held prefix turns out to match nothing, the keys after the first go
/// back to the front of the queue rather than being dispatched where they lie
/// -- otherwise a left-hand side starting inside them could never fire.
#[test]
fn test_a_failed_prefix_releases_its_keys_for_a_later_match() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    // `ax` is a prefix of nothing, but `x` on its own is mapped.
    editor.execute_keys(":map ax dd\n").unwrap();
    editor.execute_keys(":map x dd\n").unwrap();

    // `a` holds as a prefix of `ax`; `y` fails the match. The `a` is then
    // dispatched (append, entering insert mode) and the `y` goes back to the
    // front of the queue rather than being dropped or acted on out of order,
    // so it is inserted as text after the first character.
    editor.execute_keys("ay\x1b").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "oyne\ntwo\nthree\n",
        "the released key must still be acted on, in order"
    );
}

/// vi.md 120624: "If the vi command resulted from a map expansion, all
/// characters from that map expansion shall be discarded."
#[test]
fn test_an_error_discards_the_rest_of_the_expansion() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("ab\ntwo\n");
    // `5rZ` on a two-character line is an error -- POSIX makes a count larger
    // than the characters left on the line fail with the line unchanged -- so
    // the `dd` behind it must never run.
    editor.execute_keys(":map q 5rZdd\n").unwrap();

    editor.execute_keys("q").unwrap();
    assert!(
        editor.is_error_message(),
        "the 5rZ must have failed; got {:?}",
        editor.get_message()
    );
    assert_eq!(
        editor.get_buffer_text(),
        "ab\ntwo\n",
        "the dd after the failing command must be discarded"
    );

    // The same keys typed by hand are not an expansion, so the `dd` does run:
    // it is the discarding that is conditional, not the failure.
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("ab\ntwo\n");
    editor.execute_keys("5rZ").unwrap();
    assert!(editor.is_error_message());
    editor.execute_keys("dd").unwrap();
    assert_eq!(editor.get_buffer_text(), "two\n");
}

// ============================================================================
// :map! expansion, and @ through the same queue
// ============================================================================

/// 95107-95109: in text input mode the lhs is matched "as any part of text
/// entered", and the replacement acts as if it had been entered instead. The
/// classic use is leaving insert mode without reaching for <escape>.
#[test]
fn test_text_input_map_expands() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");
    // jk leaves insert mode.
    editor.execute_keys(":map! jk \x16\x1b\n").unwrap();
    editor.execute_keys("\n").unwrap();

    editor.execute_keys("ifoojk").unwrap();
    assert_eq!(editor.get_mode(), Mode::Command, "jk must have left insert");
    assert_eq!(editor.get_buffer_text(), "fooalpha\n");
}

/// 95089-95092: the same lhs can mean one thing in command mode and another in
/// text input mode, and each table is consulted only in its own mode.
#[test]
fn test_the_two_map_tables_apply_in_their_own_modes() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys(":map q dd\n").unwrap();
    editor.execute_keys(":map! q XY\n").unwrap();

    // Command mode takes the `:map` definition.
    editor.execute_keys("q").unwrap();
    assert_eq!(editor.get_buffer_text(), "two\nthree\n");

    // Text input mode takes the `:map!` one.
    editor.execute_keys("iq\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "XYtwo\nthree\n");
}

/// 95110-95111: "If any character in the input text is escaped using a
/// <control>-V character, that character shall not be part of a match to an
/// lhs."
#[test]
fn test_ctrl_v_suppresses_a_text_input_map() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\n");
    editor.execute_keys(":map! q XY\n").unwrap();

    editor.execute_keys("i\x16q\x1b").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "qalpha\n",
        "the quoted q must be inserted literally"
    );
}

/// vi.md 121171: an `@` buffer behaves "as if the contents of the named buffer
/// were entered as standard input" -- which is what makes its characters
/// subject to maps. They used to run through a separate path that no map could
/// see.
#[test]
fn test_buffer_execution_is_subject_to_maps() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\n");
    editor.execute_keys(":map q dd\n").unwrap();
    // Put a `q` into register a by yanking a line that holds one.
    editor.set_buffer_text("q\none\ntwo\nthree\n");
    editor.execute_keys("\"ayy").unwrap();
    editor.execute_keys("dd").unwrap();

    // @a enters `q`, which the map turns into `dd`.
    editor.execute_keys("@a").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "two\nthree\n",
        "the buffer's q must have been mapped to dd"
    );
}

/// vi.md 121176-121177: "If a count is specified, behave as if that count were
/// entered as user input *before* the characters from the @ buffer were
/// entered." Running the buffer `count` times is a different thing: with `dw`
/// in the register, `3@a` is `3dw` -- one command over three words -- not three
/// separate `dw`s, and the two differ as soon as a count interacts.
#[test]
fn test_buffer_execution_count_is_entered_before_the_buffer() {
    let mut editor = Editor::new_headless();
    // Register a holds `x`, which deletes one character.
    editor.set_buffer_text("x\nabcdef\n");
    editor.execute_keys("\"ayy").unwrap();
    editor.execute_keys("dd").unwrap();

    // `3@a` must mean `3x`, deleting three characters with one command, so a
    // single `u` puts all three back.
    editor.execute_keys("3@a").unwrap();
    assert_eq!(editor.get_buffer_text(), "def\n");
    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "abcdef\n",
        "3x is one command, so one undo restores all three characters"
    );
}

/// vi.md 121022-121024: "Commands (other than commands that enter text input
/// mode) executed as a result of map expansions, shall not change the value of
/// the last repeatable command." So `.` repeats what the user last did by
/// hand, not what a map did on their behalf.
#[test]
fn test_a_map_expansion_does_not_become_the_dot_command() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("abcd\nefgh\nijkl\nmnop\n");
    editor.execute_keys(":map q dd\n").unwrap();

    // Do something repeatable by hand.
    editor.execute_keys("x").unwrap();
    assert_eq!(editor.get_buffer_text(), "bcd\nefgh\nijkl\nmnop\n");

    // Now run the map, whose `dd` must not become the `.` command.
    editor.execute_keys("q").unwrap();
    assert_eq!(editor.get_buffer_text(), "efgh\nijkl\nmnop\n");

    // `.` repeats the hand-typed `x`, not the map's `dd`.
    editor.execute_keys(".").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "fgh\nijkl\nmnop\n",
        ". must repeat the x, not the mapped dd"
    );
}

// ============================================================================
// Abbreviations
// ============================================================================

/// 94870-94873: a non-word character entered after a word character triggers a
/// check against the text input so far, and a match is replaced.
#[test]
fn test_abbreviation_expands() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\n");
    editor.execute_keys(":ab teh the\n").unwrap();

    editor.execute_keys("iteh \x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "the \n");
}

/// <ESC> triggers the check too, and the replacement is entered before the
/// mode change takes effect.
#[test]
fn test_abbreviation_triggered_by_escape() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\n");
    editor.execute_keys(":ab teh the\n").unwrap();

    editor.execute_keys("iteh\x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "the\n");
    assert_eq!(editor.get_mode(), Mode::Command);
}

/// 96489-96495 works these through by name. `(p`, `p` and `((p` can fire
/// because the rules can produce those sets; `(` and `(pp` never can.
#[test]
fn test_the_abbreviations_posix_says_do_and_do_not_work() {
    for (lhs, typed, expect) in [
        ("(p", "i(p \x1b", "REPL \n"),
        ("p", "ip \x1b", "REPL \n"),
        ("((p", "i((p \x1b", "REPL \n"),
        // Cannot be produced by the rules, so must never fire.
        ("(", "i( \x1b", "( \n"),
        ("(pp", "i(pp \x1b", "(pp \n"),
    ] {
        let mut editor = Editor::new_headless();
        editor.set_buffer_text("\n");
        editor.execute_keys(&format!(":ab {lhs} REPL\n")).unwrap();
        editor.execute_keys(typed).unwrap();
        assert_eq!(
            editor.get_buffer_text(),
            expect,
            "with `:ab {lhs} REPL`, typing {typed:?}"
        );
    }
}

/// 94870-94871: a `^V`-escaped trigger is text, not a trigger.
#[test]
fn test_ctrl_v_suppresses_an_abbreviation() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\n");
    editor.execute_keys(":ab teh the\n").unwrap();

    editor.execute_keys("iteh\x16 \x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "teh \n");
}

/// 96471-96476: the replacement is "logically pushed onto the terminal input
/// queue", so it is itself subject to map expansion -- not a plain text
/// substitution.
#[test]
fn test_an_abbreviation_replacement_is_itself_expanded() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\n");
    editor.execute_keys(":ab aa b\n").unwrap();
    editor.execute_keys(":map! b ZZ\n").unwrap();

    editor.execute_keys("iaa \x1b").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "ZZ \n",
        "the replacement `b` must then be mapped to ZZ"
    );
}

/// An abbreviation whose replacement begins with its own left-hand side must
/// not re-trigger on itself. POSIX does not say this; it is the map prefix rule
/// (95120-95121) applied by analogy, and without it `:ab foo foo` hangs.
#[test]
fn test_a_self_referential_abbreviation_does_not_loop() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\n");
    editor.execute_keys(":ab foo foo\n").unwrap();

    editor.execute_keys("ifoo \x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "foo \n");
}

/// 96496-96509. Historical vi expanded abbreviations on the colon line, with
/// two results POSIX calls out as "not permitted ... because they clearly
/// violate the expectations of the user": `:ab foo bar` then `:ab foo baz`
/// registering baz for *bar*, and `:una foo2` deleting foo1.
#[test]
fn test_the_colon_line_behaviours_posix_forbids() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\n");

    // Redefining `foo` must redefine `foo`, not define an entry for `bar`.
    editor.execute_keys(":ab foo bar\n").unwrap();
    editor.execute_keys(":ab foo baz\n").unwrap();
    editor.execute_keys("ifoo \x1b").unwrap();
    assert_eq!(editor.get_buffer_text(), "baz \n");

    // `:una foo2` must delete foo2, leaving foo1 alone.
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("\n");
    editor.execute_keys(":ab foo1 bar\n").unwrap();
    editor.execute_keys(":ab foo2 bar\n").unwrap();
    editor.execute_keys(":una foo2\n").unwrap();
    assert!(!editor.is_error_message(), "{:?}", editor.get_message());
    editor.execute_keys(":una foo1\n").unwrap();
    assert!(
        !editor.is_error_message(),
        "foo1 must still exist; got {:?}",
        editor.get_message()
    );
}

/// 95443-95445: "commands resulting from buffer executions and mapped character
/// expansions, are considered single commands" for undo. So one `u` reverses
/// everything a map did, however many commands its replacement contained.
#[test]
fn test_a_map_expansion_undoes_as_one_command() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("one\ntwo\nthree\nfour\n");
    editor.execute_keys(":map q dddd\n").unwrap();

    editor.execute_keys("q").unwrap();
    assert_eq!(editor.get_buffer_text(), "three\nfour\n");

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "one\ntwo\nthree\nfour\n",
        "one u must reverse both deletes the map performed"
    );
}

/// The nesting case the depth counter exists for: a mapped command that opens
/// its own undo group. A bare flag let the inner `end_group` close the outer
/// one, leaving half the expansion outside the group.
#[test]
fn test_a_mapped_change_command_undoes_as_one_command() {
    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha bravo\n");
    // `cw` opens a group for the change and the insert session it starts; the
    // trailing `x` is a second command inside the same expansion.
    editor.execute_keys(":map q cwZZ\x16\x1bx\n").unwrap();
    editor.execute_keys("\n").unwrap();

    editor.execute_keys("q").unwrap();
    assert_ne!(editor.get_buffer_text(), "alpha bravo\n", "the map ran");

    editor.execute_keys("u").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "alpha bravo\n",
        "one u must reverse the whole expansion, not just its last command"
    );
}

/// `^T` returns to where the last tag jump started, the companion to `^]`.
///
/// Not POSIX -- the only non-POSIX command-mode key in the editor. vi.md
/// 121838-121840 gives `^T` a meaning in *text input* mode only, where it
/// shifts the autoindent; that is untouched, and
/// `test_ctrl_t_indents_at_cursor_to_shiftwidth_boundary` and
/// `test_ctrl_t_is_recorded_in_the_insert_session` fail if it stops being.
#[test]
fn test_ctrl_t_pops_the_tag_stack() {
    let (dir, a, _b) = tag_fixture();
    let mut editor = Editor::new_headless();
    set_tags_option(&mut editor, dir.path());
    editor.open(&a).unwrap();
    editor.execute_keys("2G").unwrap();

    editor.execute_keys(":tag local\n").unwrap();
    assert_eq!(editor.get_cursor().line, 3);

    editor.execute_keys("\x14").unwrap(); // ^T
    assert_eq!(editor.get_cursor().line, 2, "^T returned to the origin");

    // On an empty stack it reports rather than doing nothing silently.
    editor.execute_keys("\x14").unwrap();
    assert!(editor.is_error_message());
    assert!(editor
        .get_message()
        .unwrap_or_default()
        .contains("tag stack empty"));
}

/// ex.md 96616-96617: a map defined in terms of itself loops, and POSIX
/// "requires conformance to historical practice, and that such loops be
/// interruptible". The escape is therefore a signal, not a depth cap -- a cap
/// would refuse a mapping the spec says must work.
///
/// What this pins is that the drain polls SIGINT at all and abandons the
/// expansion when it is set. It cannot deliver the signal mid-loop from one
/// thread, so it arms the flag first; an interactive interrupt sets the same
/// flag asynchronously and reaches the same poll. Removing the poll makes this
/// test hang rather than fail, which is the honest shape of the bug.
#[test]
fn test_sigint_abandons_a_map_expansion() {
    use vi_rs::signals::SIGINT_RECEIVED;

    let mut editor = Editor::new_headless();
    editor.set_buffer_text("alpha\nbravo\n");
    editor.execute_keys(":map q dd\n").unwrap();

    SIGINT_RECEIVED.store(true, std::sync::atomic::Ordering::SeqCst);
    editor.execute_keys("q").unwrap();
    assert_eq!(
        editor.get_buffer_text(),
        "alpha\nbravo\n",
        "an armed interrupt must abandon the expansion before it runs"
    );

    // The flag is consumed, so the editor works normally again.
    assert!(!SIGINT_RECEIVED.load(std::sync::atomic::Ordering::SeqCst));
    editor.execute_keys("q").unwrap();
    assert_eq!(editor.get_buffer_text(), "bravo\n");
}
