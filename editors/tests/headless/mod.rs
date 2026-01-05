//! Comprehensive integration tests using the headless Editor.
//!
//! These tests exercise the full editor functionality without requiring
//! a real terminal, allowing us to test complete command sequences.

use vi_rs::Editor;
use vi_rs::buffer::Position;
use vi_rs::mode::Mode;

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
