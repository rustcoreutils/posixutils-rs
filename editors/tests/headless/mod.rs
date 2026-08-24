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

    // rx replaces 1 char with x (count with r not yet implemented)
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
    let dir = tempfile::tempdir().unwrap();
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
    let dir = tempfile::tempdir().unwrap();
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
