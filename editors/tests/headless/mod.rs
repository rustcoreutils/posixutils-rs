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
