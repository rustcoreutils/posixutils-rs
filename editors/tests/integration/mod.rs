//! Integration tests for vi-rs.
//!
//! These tests verify the editor components work together correctly,
//! covering all POSIX vi commands.

use vi_rs::*;

// ============================================================================
// Buffer Operations Tests
// ============================================================================

#[test]
fn test_buffer_operations() {
    let mut buffer = Buffer::from_text("hello world\ntest line\nthird line");

    assert_eq!(buffer.line_count(), 3);
    assert_eq!(buffer.line(1).unwrap().content(), "hello world");

    buffer.set_line(2);
    assert_eq!(buffer.cursor().line, 2);

    buffer.set_cursor(Position::new(1, 0));
    buffer.insert_char('X');
    assert_eq!(buffer.line(1).unwrap().content(), "Xhello world");
}

#[test]
fn test_buffer_empty() {
    let buffer = Buffer::new();
    assert_eq!(buffer.line_count(), 0);
    assert!(buffer.is_empty());
}

#[test]
fn test_buffer_modified_flag() {
    let mut buffer = Buffer::from_text("test");
    assert!(!buffer.is_modified());

    buffer.insert_char('x');
    assert!(buffer.is_modified());

    buffer.mark_saved();
    assert!(!buffer.is_modified());
}

// ============================================================================
// Motion Command Tests (h, j, k, l)
// ============================================================================

#[test]
fn test_motion_h_move_left() {
    let mut buffer = Buffer::from_text("hello");
    buffer.set_cursor(Position::new(1, 3));

    let result = command::motion::move_left(&buffer, 1).unwrap();
    assert_eq!(result.position.column, 2);

    let result = command::motion::move_left(&buffer, 2).unwrap();
    assert_eq!(result.position.column, 1);
}

#[test]
fn test_motion_l_move_right() {
    let mut buffer = Buffer::from_text("hello");
    buffer.set_cursor(Position::new(1, 0));

    let result = command::motion::move_right(&buffer, 1).unwrap();
    assert_eq!(result.position.column, 1);

    let result = command::motion::move_right(&buffer, 3).unwrap();
    assert_eq!(result.position.column, 3);
}

#[test]
fn test_motion_j_move_down() {
    let buffer = Buffer::from_text("line one\nline two\nline three");

    let result = command::motion::move_down(&buffer, 1).unwrap();
    assert_eq!(result.position.line, 2);

    let result = command::motion::move_down(&buffer, 2).unwrap();
    assert_eq!(result.position.line, 3);
}

#[test]
fn test_motion_k_move_up() {
    let mut buffer = Buffer::from_text("line one\nline two\nline three");
    buffer.set_cursor(Position::new(3, 0));

    let result = command::motion::move_up(&buffer, 1).unwrap();
    assert_eq!(result.position.line, 2);

    let result = command::motion::move_up(&buffer, 2).unwrap();
    assert_eq!(result.position.line, 1);
}

// ============================================================================
// Word Motion Tests (w, b, e, W, B, E)
// ============================================================================

#[test]
fn test_motion_w_word_forward() {
    let buffer = Buffer::from_text("hello world test");

    let result = command::motion::move_word_forward(&buffer, 1).unwrap();
    assert_eq!(result.position.column, 6); // Start of "world"

    let result = command::motion::move_word_forward(&buffer, 2).unwrap();
    assert_eq!(result.position.column, 12); // Start of "test"
}

#[test]
fn test_motion_b_word_backward() {
    let mut buffer = Buffer::from_text("hello world test");
    buffer.set_cursor(Position::new(1, 12)); // At "test"

    let result = command::motion::move_word_backward(&buffer, 1).unwrap();
    assert_eq!(result.position.column, 6); // Start of "world"
}

#[test]
fn test_motion_e_word_end() {
    let buffer = Buffer::from_text("hello world");

    let result = command::motion::move_word_end(&buffer, 1).unwrap();
    assert_eq!(result.position.column, 4); // End of "hello"
}

#[test]
fn test_motion_bigword_forward() {
    let buffer = Buffer::from_text("hello-world test");

    let result = command::motion::move_bigword_forward(&buffer, 1).unwrap();
    assert_eq!(result.position.column, 12); // Start of "test" (skips hyphenated word)
}

#[test]
fn test_motion_bigword_backward() {
    let mut buffer = Buffer::from_text("hello test-word");
    buffer.set_cursor(Position::new(1, 14));

    let result = command::motion::move_bigword_backward(&buffer, 1).unwrap();
    assert_eq!(result.position.column, 6); // Start of "test-word"
}

#[test]
fn test_motion_bigword_end() {
    let buffer = Buffer::from_text("hello-world test");

    let result = command::motion::move_bigword_end(&buffer, 1).unwrap();
    assert_eq!(result.position.column, 10); // End of "hello-world"
}

// ============================================================================
// Line Position Tests (0, ^, $, G)
// ============================================================================

#[test]
fn test_motion_0_line_start() {
    let mut buffer = Buffer::from_text("  hello world");
    buffer.set_cursor(Position::new(1, 8));

    let result = command::motion::move_to_line_start(&buffer).unwrap();
    assert_eq!(result.position.column, 0);
}

#[test]
fn test_motion_caret_first_non_blank() {
    let mut buffer = Buffer::from_text("  hello world");
    buffer.set_cursor(Position::new(1, 8));

    let result = command::motion::move_to_first_non_blank(&buffer).unwrap();
    assert_eq!(result.position.column, 2); // First non-blank
}

#[test]
fn test_motion_dollar_line_end() {
    let buffer = Buffer::from_text("hello world");

    let result = command::motion::move_to_line_end(&buffer, 1).unwrap();
    assert_eq!(result.position.column, 10); // Last character
}

#[test]
fn test_motion_goto_line() {
    let buffer = Buffer::from_text("line one\nline two\nline three");

    // G without count goes to last line
    let result = command::motion::move_to_line(&buffer, None).unwrap();
    assert_eq!(result.position.line, 3);

    // 2G goes to line 2
    let result = command::motion::move_to_line(&buffer, Some(2)).unwrap();
    assert_eq!(result.position.line, 2);
}

// ============================================================================
// Character Find Tests (f, F, t, T)
// ============================================================================

#[test]
fn test_motion_f_find_char_forward() {
    let buffer = Buffer::from_text("hello world");

    let result = command::motion::find_char_forward(&buffer, 'o', 1).unwrap();
    assert_eq!(result.position.column, 4); // First 'o'

    let result = command::motion::find_char_forward(&buffer, 'o', 2).unwrap();
    assert_eq!(result.position.column, 7); // Second 'o'
}

#[test]
fn test_motion_find_char_backward() {
    let mut buffer = Buffer::from_text("hello world");
    buffer.set_cursor(Position::new(1, 10));

    let result = command::motion::find_char_backward(&buffer, 'o', 1).unwrap();
    assert_eq!(result.position.column, 7); // Last 'o' before cursor
}

#[test]
fn test_motion_t_till_char_forward() {
    let buffer = Buffer::from_text("hello world");

    let result = command::motion::till_char_forward(&buffer, 'o', 1).unwrap();
    assert_eq!(result.position.column, 3); // One before 'o'
}

#[test]
fn test_motion_till_char_backward() {
    let mut buffer = Buffer::from_text("hello world");
    buffer.set_cursor(Position::new(1, 10));

    let result = command::motion::till_char_backward(&buffer, 'o', 1).unwrap();
    assert_eq!(result.position.column, 8); // One after 'o'
}

// ============================================================================
// Bracket Matching Test (%)
// ============================================================================

#[test]
fn test_motion_percent_matching_bracket() {
    let mut buffer = Buffer::from_text("(hello)");
    buffer.set_cursor(Position::new(1, 0));

    let result = command::motion::find_matching_bracket(&buffer).unwrap();
    assert_eq!(result.position.column, 6); // Closing paren
}

#[test]
fn test_motion_percent_nested_brackets() {
    let mut buffer = Buffer::from_text("((inner))");
    buffer.set_cursor(Position::new(1, 0));

    let result = command::motion::find_matching_bracket(&buffer).unwrap();
    assert_eq!(result.position.column, 8); // Outermost closing paren
}

// ============================================================================
// Column Motion Test (|)
// ============================================================================

#[test]
fn test_motion_pipe_column() {
    let buffer = Buffer::from_text("hello world");

    let result = command::motion::move_to_column(&buffer, 5).unwrap();
    assert_eq!(result.position.column, 4); // Column 5 is index 4
}

// ============================================================================
// Paragraph Motion Tests ({, })
// ============================================================================

#[test]
fn test_motion_paragraph_forward() {
    let buffer = Buffer::from_text("para one\n\npara two\n\npara three");
    // Buffer: line 1 = "para one", line 2 = "", line 3 = "para two", line 4 = "", line 5 = "para three"

    let result = command::motion::move_paragraph_forward(&buffer, 1).unwrap();
    // Move from line 1, skips non-blank, finds blank (line 2), then finds next non-blank (line 3)
    assert_eq!(result.position.line, 3); // Start of "para two"
}

#[test]
fn test_motion_paragraph_backward() {
    let mut buffer = Buffer::from_text("para one\n\npara two\n\npara three");
    buffer.set_cursor(Position::new(5, 0));
    // Buffer: line 1 = "para one", line 2 = "", line 3 = "para two", line 4 = "", line 5 = "para three"

    let result = command::motion::move_paragraph_backward(&buffer, 1).unwrap();
    // Move from line 5 backward, skips blanks, finds start of previous paragraph
    assert_eq!(result.position.line, 3); // Start of "para two"
}

// ============================================================================
// Operator Tests (d, c, y)
// ============================================================================

#[test]
fn test_operator_delete_lines() {
    let mut buffer = Buffer::from_text("line one\nline two\nline three");
    let mut registers = Registers::new();

    let range = Range::lines(Position::new(1, 0), Position::new(2, 0));
    let _result = command::delete(&mut buffer, range, &mut registers, None).unwrap();

    assert_eq!(buffer.line_count(), 1);
    assert_eq!(buffer.line(1).unwrap().content(), "line three");

    let content = registers.get('"').unwrap();
    assert!(content.linewise);
}

#[test]
fn test_operator_delete_chars() {
    let mut buffer = Buffer::from_text("hello world");
    let mut registers = Registers::new();

    let range = Range::new(
        Position::new(1, 0),
        Position::new(1, 5),
        BufferMode::Character,
    );
    let _result = command::delete(&mut buffer, range, &mut registers, None).unwrap();

    assert_eq!(buffer.line(1).unwrap().content(), " world");
}

#[test]
fn test_operator_yank() {
    let buffer = Buffer::from_text("line one\nline two");
    let mut registers = Registers::new();

    let range = Range::lines(Position::new(1, 0), Position::new(1, 0));
    command::yank(&buffer, range, &mut registers, None).unwrap();

    let content = registers.get('"').unwrap();
    assert!(content.text.contains("line one"));
    assert!(content.linewise);
}

#[test]
fn test_operator_change() {
    let mut buffer = Buffer::from_text("hello world");
    let mut registers = Registers::new();

    let range = Range::new(
        Position::new(1, 0),
        Position::new(1, 5),
        BufferMode::Character,
    );
    let result = command::change(&mut buffer, range, &mut registers, None).unwrap();

    assert!(result.enter_insert);
    assert_eq!(buffer.line(1).unwrap().content(), " world");
}

#[test]
fn test_operator_put_after() {
    let mut buffer = Buffer::from_text("line one\nline two");
    let mut registers = Registers::new();

    let range = Range::lines(Position::new(1, 0), Position::new(1, 0));
    command::yank(&buffer, range, &mut registers, None).unwrap();

    buffer.set_cursor(Position::new(2, 0));
    let _result = command::put_after(&mut buffer, &registers, None, 1).unwrap();

    assert_eq!(buffer.line_count(), 3);
    assert_eq!(buffer.line(3).unwrap().content(), "line one");
}

#[test]
fn test_operator_put_before() {
    let mut buffer = Buffer::from_text("line one\nline two");
    let mut registers = Registers::new();

    let range = Range::lines(Position::new(2, 0), Position::new(2, 0));
    command::yank(&buffer, range, &mut registers, None).unwrap();

    buffer.set_cursor(Position::new(1, 0));
    let _result = command::put_before(&mut buffer, &registers, None, 1).unwrap();

    assert_eq!(buffer.line_count(), 3);
    assert_eq!(buffer.line(1).unwrap().content(), "line two");
}

#[test]
fn test_operator_shift_right() {
    let mut buffer = Buffer::from_text("hello\nworld");

    let range = Range::lines(Position::new(1, 0), Position::new(1, 0));
    let _result = command::shift_right(&mut buffer, range, 1).unwrap();

    // shift_right adds tabs, not spaces
    assert!(buffer.line(1).unwrap().content().starts_with("\t"));
}

#[test]
fn test_operator_shift_left() {
    let mut buffer = Buffer::from_text("    hello\n    world");

    let range = Range::lines(Position::new(1, 0), Position::new(1, 0));
    let _result = command::shift_left(&mut buffer, range, 4).unwrap();

    assert_eq!(buffer.line(1).unwrap().content(), "hello");
}

// ============================================================================
// Register Tests
// ============================================================================

#[test]
fn test_register_named() {
    let mut registers = Registers::new();

    registers.set('a', RegisterContent::new("test text".to_string(), false));
    let content = registers.get('a').unwrap();
    assert_eq!(content.text, "test text");
    assert!(!content.linewise);
}

#[test]
fn test_register_numbered_shift() {
    let mut registers = Registers::new();

    registers.delete(
        None,
        RegisterContent::new("deleted1".to_string(), true),
        false,
    );
    registers.delete(
        None,
        RegisterContent::new("deleted2".to_string(), true),
        false,
    );
    registers.delete(
        None,
        RegisterContent::new("deleted3".to_string(), true),
        false,
    );

    assert_eq!(registers.get('1').unwrap().text, "deleted3");
    assert_eq!(registers.get('2').unwrap().text, "deleted2");
    assert_eq!(registers.get('3').unwrap().text, "deleted1");
}

#[test]
fn test_register_uppercase_appends() {
    let mut registers = Registers::new();

    registers.set('a', RegisterContent::new("first".to_string(), false));
    registers.set('A', RegisterContent::new("second".to_string(), false));

    let content = registers.get('a').unwrap();
    assert!(content.text.contains("first"));
    assert!(content.text.contains("second"));
}

#[test]
fn test_register_unnamed() {
    let mut registers = Registers::new();

    registers.set('"', RegisterContent::new("unnamed".to_string(), false));
    let content = registers.get('"').unwrap();
    assert_eq!(content.text, "unnamed");
}

// ============================================================================
// Undo/Redo Tests
// ============================================================================

#[test]
fn test_undo_insert() {
    let mut buffer = Buffer::from_text("original line");
    let mut undo = UndoManager::new();

    let pos = buffer.cursor();
    buffer.insert_char('X');
    undo.record_insert(pos, "X", false);

    assert_eq!(buffer.line(1).unwrap().content(), "Xoriginal line");

    let _ = undo.undo(&mut buffer).unwrap();
    assert_eq!(buffer.line(1).unwrap().content(), "original line");
}

#[test]
fn test_undo_redo() {
    let mut buffer = Buffer::from_text("original");
    let mut undo = UndoManager::new();

    let pos = buffer.cursor();
    buffer.insert_char('X');
    undo.record_insert(pos, "X", false);

    let _ = undo.undo(&mut buffer).unwrap();
    assert_eq!(buffer.line(1).unwrap().content(), "original");

    let _ = undo.redo(&mut buffer).unwrap();
    assert_eq!(buffer.line(1).unwrap().content(), "Xoriginal");
}

#[test]
fn test_undo_nothing_to_undo() {
    let mut buffer = Buffer::from_text("test");
    let mut undo = UndoManager::new();

    let result = undo.undo(&mut buffer);
    assert!(result.is_err());
}

// ============================================================================
// Search Tests
// ============================================================================

#[test]
fn test_search_forward() {
    let buffer = Buffer::from_text("line one\nline two\nline three");
    let mut search = SearchState::new();

    search.set_pattern("two", SearchDirection::Forward).unwrap();
    let result = search.search_forward(&buffer, Position::new(1, 0)).unwrap();

    assert_eq!(result.line, 2);
}

#[test]
fn test_search_backward() {
    let buffer = Buffer::from_text("line one\nline two\nline three");
    let mut search = SearchState::new();

    search
        .set_pattern("one", SearchDirection::Backward)
        .unwrap();
    let result = search
        .search_backward(&buffer, Position::new(3, 0))
        .unwrap();

    assert_eq!(result.line, 1);
}

#[test]
fn test_search_wrap() {
    let buffer = Buffer::from_text("find me\nother line");
    let mut search = SearchState::new();
    // wrapscan is true by default

    search
        .set_pattern("find", SearchDirection::Forward)
        .unwrap();
    let result = search.search_forward(&buffer, Position::new(2, 0)).unwrap();

    assert_eq!(result.line, 1); // Wrapped to beginning
}

#[test]
fn test_search_no_wrap() {
    let buffer = Buffer::from_text("find me\nother line");
    let mut search = SearchState::new();
    let options = Options {
        wrapscan: false,
        ..Default::default()
    };
    search.update_options(&options);

    search
        .set_pattern("find", SearchDirection::Forward)
        .unwrap();
    let result = search.search_forward(&buffer, Position::new(2, 0));

    assert!(result.is_err()); // Should not wrap
}

#[test]
fn test_search_ignorecase() {
    let buffer = Buffer::from_text("other HELLO world");
    let mut search = SearchState::new();
    let options = Options {
        ignorecase: true,
        ..Default::default()
    };
    search.update_options(&options);

    search
        .set_pattern("hello", SearchDirection::Forward)
        .unwrap();
    // Search starts AFTER position (1,0), so find "HELLO" at column 6
    let result = search.search_forward(&buffer, Position::new(1, 0)).unwrap();

    assert_eq!(result.column, 6); // "HELLO" starts at column 6
}

// ============================================================================
// Substitution Tests
// ============================================================================

#[test]
fn test_substitute_simple() {
    let sub = Substitutor::new("foo", "bar", false, false, false, false, false).unwrap();
    let (result, count) = sub.substitute_line("foo baz foo");

    assert_eq!(result, "bar baz foo");
    assert_eq!(count, 1);
}

#[test]
fn test_substitute_global() {
    let sub = Substitutor::new("foo", "bar", true, false, false, false, false).unwrap();
    let (result, count) = sub.substitute_line("foo baz foo");

    assert_eq!(result, "bar baz bar");
    assert_eq!(count, 2);
}

#[test]
fn test_substitute_ignorecase() {
    // Substitutor::new(pattern, replacement, global, confirm, print, count_only, ignorecase)
    let sub = Substitutor::new("FOO", "bar", false, false, false, false, true).unwrap();
    let (result, count) = sub.substitute_line("foo baz FOO");

    // Case insensitive: "foo" matches first
    assert_eq!(result, "bar baz FOO");
    assert_eq!(count, 1);
}

#[test]
fn test_substitute_ampersand() {
    let sub = Substitutor::new("foo", "[&]", false, false, false, false, false).unwrap();
    let (result, _) = sub.substitute_line("foo");

    assert_eq!(result, "[foo]");
}

// ============================================================================
// Options Tests
// ============================================================================

#[test]
fn test_options_defaults() {
    let options = Options::default();

    assert_eq!(options.tabstop, 8);
    assert_eq!(options.shiftwidth, 8);
    assert!(!options.ignorecase);
    assert!(options.wrapscan);
    assert!(!options.autoindent);
}

#[test]
fn test_options_set_numeric() {
    let mut options = Options::default();

    options.set("tabstop=4").unwrap();
    assert_eq!(options.tabstop, 4);

    options.set("shiftwidth=2").unwrap();
    assert_eq!(options.shiftwidth, 2);
}

#[test]
fn test_options_set_boolean() {
    let mut options = Options::default();

    options.set("ignorecase").unwrap();
    assert!(options.ignorecase);

    options.set("noignorecase").unwrap();
    assert!(!options.ignorecase);
}

#[test]
fn test_options_set_abbreviations() {
    let mut options = Options::default();

    options.set("ic").unwrap();
    assert!(options.ignorecase);

    options.set("ts=4").unwrap();
    assert_eq!(options.tabstop, 4);
}

// ============================================================================
// Ex Command Parsing Tests
// ============================================================================

#[test]
fn test_ex_quit() {
    let cmd = ex::parse_ex_command("q").unwrap();
    assert!(matches!(cmd, ExCommand::Quit { force: false }));

    let cmd = ex::parse_ex_command("q!").unwrap();
    assert!(matches!(cmd, ExCommand::Quit { force: true }));

    let cmd = ex::parse_ex_command("quit").unwrap();
    assert!(matches!(cmd, ExCommand::Quit { force: false }));
}

#[test]
fn test_ex_write() {
    let cmd = ex::parse_ex_command("w").unwrap();
    assert!(matches!(cmd, ExCommand::Write { .. }));

    let cmd = ex::parse_ex_command("w test.txt").unwrap();
    if let ExCommand::Write { file, .. } = cmd {
        assert_eq!(file, Some("test.txt".to_string()));
    }
}

#[test]
fn test_ex_write_quit() {
    let cmd = ex::parse_ex_command("wq").unwrap();
    assert!(matches!(cmd, ExCommand::WriteQuit { .. }));

    let cmd = ex::parse_ex_command("x").unwrap();
    assert!(matches!(cmd, ExCommand::WriteQuit { .. }));
}

#[test]
fn test_ex_edit() {
    let cmd = ex::parse_ex_command("e test.txt").unwrap();
    if let ExCommand::Edit { file, force } = cmd {
        assert_eq!(file, Some("test.txt".to_string()));
        assert!(!force);
    }

    let cmd = ex::parse_ex_command("e!").unwrap();
    if let ExCommand::Edit { force, .. } = cmd {
        assert!(force);
    }
}

#[test]
fn test_ex_set() {
    let cmd = ex::parse_ex_command("set tabstop=4").unwrap();
    assert!(matches!(cmd, ExCommand::Set { .. }));

    let cmd = ex::parse_ex_command("set all").unwrap();
    assert!(matches!(cmd, ExCommand::Set { .. }));
}

#[test]
fn test_ex_substitute() {
    let cmd = ex::parse_ex_command("s/foo/bar/").unwrap();
    if let ExCommand::Substitute {
        pattern,
        replacement,
        ..
    } = cmd
    {
        assert_eq!(pattern, "foo");
        assert_eq!(replacement, "bar");
    } else {
        panic!("Expected Substitute");
    }

    let cmd = ex::parse_ex_command("s/foo/bar/g").unwrap();
    if let ExCommand::Substitute { flags, .. } = cmd {
        assert!(flags.global);
    } else {
        panic!("Expected Substitute");
    }
}

#[test]
fn test_ex_global() {
    let cmd = ex::parse_ex_command("g/pattern/d").unwrap();
    assert!(matches!(cmd, ExCommand::Global { .. }));
}

#[test]
fn test_ex_line_number() {
    let cmd = ex::parse_ex_command("10").unwrap();
    assert!(matches!(cmd, ExCommand::Goto { line: 10 }));
}

// ============================================================================
// File I/O Tests
// ============================================================================

#[test]
fn test_file_read() {
    use std::io::Write;
    use tempfile::NamedTempFile;

    let mut tmp = NamedTempFile::new().unwrap();
    writeln!(tmp, "test content").unwrap();
    writeln!(tmp, "line two").unwrap();

    let buffer = file::read_file(tmp.path()).unwrap();
    assert_eq!(buffer.line_count(), 2);
    assert_eq!(buffer.line(1).unwrap().content(), "test content");
}

#[test]
fn test_file_read_nonexistent() {
    use std::path::Path;

    let buffer = file::read_file(Path::new("/nonexistent/file")).unwrap();
    assert_eq!(buffer.line_count(), 0); // New file case
}

#[test]
fn test_file_write() {
    use std::fs;
    use tempfile::NamedTempFile;

    let buffer = Buffer::from_text("hello\nworld");
    let output = NamedTempFile::new().unwrap();

    let stats = file::write_file(&buffer, output.path(), false).unwrap();
    assert_eq!(stats.lines, 2);

    let content = fs::read_to_string(output.path()).unwrap();
    assert!(content.contains("hello"));
    assert!(content.contains("world"));
}

#[test]
fn test_file_write_range() {
    use std::fs;
    use tempfile::NamedTempFile;

    let buffer = Buffer::from_text("one\ntwo\nthree\nfour");
    let output = NamedTempFile::new().unwrap();

    let stats = file::write_range(&buffer, output.path(), 2, 3, false).unwrap();
    assert_eq!(stats.lines, 2);

    let content = fs::read_to_string(output.path()).unwrap();
    assert!(content.contains("two"));
    assert!(content.contains("three"));
    assert!(!content.contains("one"));
}

// ============================================================================
// Command Parser Tests
// ============================================================================

#[test]
fn test_parser_simple_motion() {
    let mut parser = CommandParser::new();

    parser.process_key(Key::Char('j'));
    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.command, 'j');
    assert_eq!(cmd.count, 1);
}

#[test]
fn test_parser_count_motion() {
    let mut parser = CommandParser::new();

    parser.process_key(Key::Char('5'));
    parser.process_key(Key::Char('j'));
    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.command, 'j');
    assert_eq!(cmd.count, 5);
}

#[test]
fn test_parser_operator_motion() {
    let mut parser = CommandParser::new();

    parser.process_key(Key::Char('d'));
    parser.process_key(Key::Char('w'));
    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.command, 'd');
    assert!(cmd.motion.is_some());
    assert_eq!(cmd.motion.unwrap().motion, 'w');
}

#[test]
fn test_parser_operator_doubled() {
    let mut parser = CommandParser::new();

    parser.process_key(Key::Char('d'));
    parser.process_key(Key::Char('d'));
    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.command, 'd');
    assert!(cmd.motion.is_some());
    assert_eq!(cmd.motion.unwrap().motion, 'd');
}

#[test]
fn test_parser_register() {
    let mut parser = CommandParser::new();

    parser.process_key(Key::Char('"'));
    parser.process_key(Key::Char('a'));
    parser.process_key(Key::Char('y'));
    parser.process_key(Key::Char('y'));
    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.register, Some('a'));
}

#[test]
fn test_parser_char_arg() {
    let mut parser = CommandParser::new();

    parser.process_key(Key::Char('f'));
    parser.process_key(Key::Char('x'));
    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.command, 'f');
    assert_eq!(cmd.char_arg, Some('x'));
}

#[test]
fn test_parser_escape_resets() {
    let mut parser = CommandParser::new();

    parser.process_key(Key::Char('d'));
    parser.process_key(Key::Escape);
    parser.process_key(Key::Char('j'));
    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.command, 'j');
}

// ============================================================================
// Insert Mode Tests
// ============================================================================

#[test]
fn test_insert_mode_basic() {
    let mut buffer = Buffer::from_text("hello");
    buffer.set_cursor(Position::new(1, 0));

    let state = enter_insert_mode(&mut buffer, InsertKind::Insert).unwrap();
    assert_eq!(state.kind, InsertKind::Insert);

    let mut state = state;
    let exit = process_insert_key(&mut buffer, Key::Char('X'), &mut state).unwrap();
    assert!(!exit);
    assert_eq!(buffer.line(1).unwrap().content(), "Xhello");
}

#[test]
fn test_insert_mode_append() {
    let mut buffer = Buffer::from_text("hello");
    buffer.set_cursor(Position::new(1, 2));

    let _state = enter_insert_mode(&mut buffer, InsertKind::Append).unwrap();
    assert_eq!(buffer.cursor().column, 3); // Moved one right
}

#[test]
fn test_insert_mode_append_eol() {
    let mut buffer = Buffer::from_text("hello");
    buffer.set_cursor(Position::new(1, 0));

    let _state = enter_insert_mode(&mut buffer, InsertKind::AppendEol).unwrap();
    // 'A' should put cursor AFTER the last character (position 5 for "hello")
    // so that inserted text appears after existing text
    assert_eq!(buffer.cursor().column, 5);
}

#[test]
fn test_insert_mode_open_below() {
    let mut buffer = Buffer::from_text("hello");

    let _state = enter_insert_mode(&mut buffer, InsertKind::OpenBelow).unwrap();
    assert_eq!(buffer.line_count(), 2);
    assert_eq!(buffer.cursor().line, 2);
}

#[test]
fn test_insert_mode_open_above() {
    let mut buffer = Buffer::from_text("hello");

    let _state = enter_insert_mode(&mut buffer, InsertKind::OpenAbove).unwrap();
    assert_eq!(buffer.line_count(), 2);
    assert_eq!(buffer.cursor().line, 1);
}

#[test]
fn test_insert_mode_escape() {
    let mut buffer = Buffer::from_text("hello");
    let mut state = enter_insert_mode(&mut buffer, InsertKind::Insert).unwrap();

    let exit = process_insert_key(&mut buffer, Key::Escape, &mut state).unwrap();
    assert!(exit);
}

#[test]
fn test_insert_mode_backspace() {
    let mut buffer = Buffer::from_text("hello");
    buffer.set_cursor(Position::new(1, 2));
    let mut state = enter_insert_mode(&mut buffer, InsertKind::Insert).unwrap();

    let _ = process_insert_key(&mut buffer, Key::Char('X'), &mut state).unwrap();
    let _ = process_insert_key(&mut buffer, Key::Backspace, &mut state).unwrap();
    assert_eq!(buffer.line(1).unwrap().content(), "hello");
}

// ============================================================================
// Address Parsing Tests
// ============================================================================

#[test]
fn test_address_current() {
    let buffer = Buffer::from_text("one\ntwo\nthree");
    let range = ex::AddressRange::empty();

    let (start, end) = range.resolve(&buffer, 2).unwrap();
    assert_eq!(start, 2);
    assert_eq!(end, 2);
}

#[test]
fn test_address_line_number() {
    use ex::Address;

    let buffer = Buffer::from_text("one\ntwo\nthree");
    let range = ex::AddressRange::range(Address::Line(1), Address::Line(3));

    let (start, end) = range.resolve(&buffer, 1).unwrap();
    assert_eq!(start, 1);
    assert_eq!(end, 3);
}

#[test]
fn test_address_last() {
    use ex::Address;

    let buffer = Buffer::from_text("one\ntwo\nthree");
    let range = ex::AddressRange::range(Address::Last, Address::Last);

    let (start, end) = range.resolve(&buffer, 1).unwrap();
    assert_eq!(start, 3);
    assert_eq!(end, 3);
}

// ============================================================================
// Text Object Helper Tests
// ============================================================================

#[test]
fn test_text_object_word_char() {
    assert!(command::text_object::is_word_char('a'));
    assert!(command::text_object::is_word_char('Z'));
    assert!(command::text_object::is_word_char('0'));
    assert!(command::text_object::is_word_char('_'));
    assert!(!command::text_object::is_word_char(' '));
    assert!(!command::text_object::is_word_char('.'));
}

#[test]
fn test_text_object_is_blank() {
    assert!(command::text_object::is_blank(' '));
    assert!(command::text_object::is_blank('\t'));
    assert!(!command::text_object::is_blank('a'));
}

#[test]
fn test_text_object_paragraph_boundary() {
    let buffer = Buffer::from_text("text\n\nmore text");

    assert!(!command::text_object::is_paragraph_boundary(&buffer, 1));
    assert!(command::text_object::is_paragraph_boundary(&buffer, 2));
    assert!(!command::text_object::is_paragraph_boundary(&buffer, 3));
}

#[test]
fn test_text_object_section_boundary() {
    let buffer = Buffer::from_text("text\n{\nmore");

    assert!(!command::text_object::is_section_boundary(&buffer, 1));
    assert!(command::text_object::is_section_boundary(&buffer, 2));
}

// ============================================================================
// Display Utility Tests
// ============================================================================

#[test]
fn test_display_char_width() {
    assert_eq!(char_width('\t', 0, 8), 8);
    assert_eq!(char_width('\t', 4, 8), 4);
    assert_eq!(char_width('a', 0, 8), 1);
}

#[test]
fn test_display_string_width() {
    assert_eq!(string_width("hello", 8), 5);
    assert_eq!(string_width("\t", 8), 8);
    assert_eq!(string_width("a\tb", 8), 9);
}

// ============================================================================
// FileManager Tests
// ============================================================================

#[test]
fn test_file_manager_args() {
    use std::path::PathBuf;

    let mut fm = FileManager::new();
    fm.set_args(vec![
        PathBuf::from("file1.txt"),
        PathBuf::from("file2.txt"),
        PathBuf::from("file3.txt"),
    ]);

    assert_eq!(fm.arg_index(), 0);
    assert_eq!(fm.files_remaining(), 2);

    let next = fm.next_file().unwrap();
    assert_eq!(next, PathBuf::from("file2.txt"));
}

#[test]
fn test_file_manager_alternate() {
    use std::path::{Path, PathBuf};

    let mut fm = FileManager::new();
    fm.set_current_file(Some(PathBuf::from("file1.txt")));
    fm.set_current_file(Some(PathBuf::from("file2.txt")));

    assert_eq!(fm.current_file(), Some(Path::new("file2.txt")));
    assert_eq!(fm.alternate_file(), Some(Path::new("file1.txt")));

    let alt = fm.switch_to_alternate().unwrap();
    assert_eq!(alt, PathBuf::from("file1.txt"));
}

// ============================================================================
// Screen Tests
// ============================================================================

#[test]
fn test_screen_scroll() {
    let mut screen = Screen::new(TerminalSize { rows: 24, cols: 80 });

    screen.scroll_to_line(10, 100);
    assert!(screen.top_line() <= 10);
}

#[test]
fn test_screen_expand_tabs() {
    let screen = Screen::new(TerminalSize { rows: 24, cols: 80 });
    let expanded = screen.expand_line("a\tb", 80);
    assert_eq!(expanded.len(), 9); // 'a' + 7 spaces + 'b'
}

// ============================================================================
// Error Type Tests
// ============================================================================

#[test]
fn test_error_display() {
    let err = ViError::MarkNotSet('a');
    assert_eq!(format!("{}", err), "Mark not set: 'a'");

    let err = ViError::PatternNotFound("test".to_string());
    assert_eq!(format!("{}", err), "Pattern not found: test");
}

#[test]
fn test_error_io_conversion() {
    use std::io;

    let io_err = io::Error::new(io::ErrorKind::NotFound, "file not found");
    let vi_err: ViError = io_err.into();
    assert!(matches!(vi_err, ViError::Io(_)));
}

// ============================================================================
// Additional Buffer Operation Tests (x, X, J)
// ============================================================================

#[test]
fn test_buffer_delete_char_at_cursor() {
    let mut buffer = Buffer::from_text("hello");
    buffer.set_cursor(Position::new(1, 0));

    let deleted = buffer.delete_char();
    assert_eq!(deleted, Some('h'));
    assert_eq!(buffer.line(1).unwrap().content(), "ello");
}

#[test]
fn test_buffer_delete_char_before_cursor() {
    let mut buffer = Buffer::from_text("hello");
    buffer.set_cursor(Position::new(1, 3));

    let deleted = buffer.delete_char_before();
    assert_eq!(deleted, Some('l'));
    assert_eq!(buffer.line(1).unwrap().content(), "helo");
    assert_eq!(buffer.cursor().column, 2);
}

#[test]
fn test_buffer_join_lines() {
    let mut buffer = Buffer::from_text("hello\nworld");
    buffer.set_cursor(Position::new(1, 0));

    buffer.join_lines(1, true).unwrap();
    assert_eq!(buffer.line_count(), 1);
    assert_eq!(buffer.line(1).unwrap().content(), "hello world");
}

#[test]
fn test_buffer_join_lines_no_space() {
    let mut buffer = Buffer::from_text("hello\nworld");
    buffer.set_cursor(Position::new(1, 0));

    buffer.join_lines(1, false).unwrap();
    assert_eq!(buffer.line_count(), 1);
    assert_eq!(buffer.line(1).unwrap().content(), "helloworld");
}

// ============================================================================
// Ex Command Variants Tests
// ============================================================================

#[test]
fn test_ex_delete() {
    let cmd = ex::parse_ex_command("d").unwrap();
    assert!(matches!(cmd, ExCommand::Delete { .. }));
}

#[test]
fn test_ex_yank() {
    let cmd = ex::parse_ex_command("y").unwrap();
    assert!(matches!(cmd, ExCommand::Yank { .. }));
}

#[test]
fn test_ex_put() {
    let cmd = ex::parse_ex_command("put").unwrap();
    assert!(matches!(cmd, ExCommand::Put { .. }));
}

#[test]
fn test_ex_copy() {
    let cmd = ex::parse_ex_command("co 5").unwrap();
    if let ExCommand::Copy { dest, .. } = cmd {
        assert_eq!(dest, 5);
    } else {
        panic!("Expected Copy");
    }
}

#[test]
fn test_ex_move() {
    let cmd = ex::parse_ex_command("m 10").unwrap();
    if let ExCommand::Move { dest, .. } = cmd {
        assert_eq!(dest, 10);
    } else {
        panic!("Expected Move");
    }
}

#[test]
fn test_ex_read_file() {
    let cmd = ex::parse_ex_command("r myfile.txt").unwrap();
    if let ExCommand::Read { file, .. } = cmd {
        assert_eq!(file, Some("myfile.txt".to_string()));
    } else {
        panic!("Expected Read");
    }
}

#[test]
fn test_ex_next_prev() {
    let cmd = ex::parse_ex_command("n").unwrap();
    assert!(matches!(cmd, ExCommand::Next { force: false }));

    let cmd = ex::parse_ex_command("prev").unwrap();
    assert!(matches!(cmd, ExCommand::Previous { force: false }));
}

#[test]
fn test_ex_args() {
    let cmd = ex::parse_ex_command("args").unwrap();
    assert!(matches!(cmd, ExCommand::Args));
}

#[test]
fn test_ex_undo_redo() {
    let cmd = ex::parse_ex_command("u").unwrap();
    assert!(matches!(cmd, ExCommand::Undo));

    let cmd = ex::parse_ex_command("redo").unwrap();
    assert!(matches!(cmd, ExCommand::Redo));
}

#[test]
fn test_ex_print() {
    let cmd = ex::parse_ex_command("p").unwrap();
    assert!(matches!(cmd, ExCommand::Print { .. }));
}

#[test]
fn test_ex_number() {
    let cmd = ex::parse_ex_command("nu").unwrap();
    assert!(matches!(cmd, ExCommand::Number { .. }));
}

#[test]
fn test_ex_version() {
    let cmd = ex::parse_ex_command("version").unwrap();
    assert!(matches!(cmd, ExCommand::Version));
}

// ============================================================================
// Line Object Tests
// ============================================================================

#[test]
fn test_line_is_blank() {
    use vi_rs::Line;

    let blank = Line::new();
    assert!(blank.is_blank_line());

    let spaces = Line::from("   ");
    assert!(spaces.is_blank_line());

    let content = Line::from("hello");
    assert!(!content.is_blank_line());
}

#[test]
fn test_line_first_non_blank() {
    use vi_rs::Line;

    let line = Line::from("   hello");
    assert_eq!(line.first_non_blank(), 3);

    // For blank line, returns 0 (unwrap_or(0))
    let blank = Line::from("     ");
    assert_eq!(blank.first_non_blank(), 0);
}

// ============================================================================
// Shell Command Tests
// ============================================================================

#[test]
fn test_shell_executor_expand_percent() {
    let mut exec = ShellExecutor::new("/bin/sh");
    exec.set_current_file(Some("test.txt".to_string()));
    let result = exec.expand_command("cat %").unwrap();
    assert_eq!(result, "cat test.txt");
}

#[test]
fn test_shell_executor_expand_hash() {
    let mut exec = ShellExecutor::new("/bin/sh");
    exec.set_current_file(Some("current.txt".to_string()));
    exec.set_alternate_file(Some("alternate.txt".to_string()));
    let result = exec.expand_command("diff % #").unwrap();
    assert_eq!(result, "diff current.txt alternate.txt");
}

#[test]
fn test_shell_executor_expand_escaped() {
    let mut exec = ShellExecutor::new("/bin/sh");
    exec.set_current_file(Some("test.txt".to_string()));
    let result = exec.expand_command("echo \\%").unwrap();
    assert_eq!(result, "echo %");
}

#[test]
fn test_shell_executor_execute_capture() {
    let mut exec = ShellExecutor::new("/bin/sh");
    let output = exec.execute_capture("echo hello").unwrap();
    assert!(output.success);
    assert_eq!(output.stdout_string().trim(), "hello");
}

#[test]
fn test_shell_executor_filter() {
    let mut exec = ShellExecutor::new("/bin/sh");
    let output = exec.filter("sort", "banana\napple\ncherry\n").unwrap();
    assert!(output.success);
    assert_eq!(output.stdout_string(), "apple\nbanana\ncherry\n");
}

#[test]
fn test_shell_executor_filter_uppercase() {
    let mut exec = ShellExecutor::new("/bin/sh");
    let output = exec.filter("tr a-z A-Z", "hello\n").unwrap();
    assert!(output.success);
    assert_eq!(output.stdout_string(), "HELLO\n");
}

#[test]
fn test_shell_executor_last_command() {
    let mut exec = ShellExecutor::new("/bin/sh");
    assert!(exec.last_command().is_none());

    exec.execute_capture("echo test").unwrap();
    assert_eq!(exec.last_command(), Some("echo test"));

    // ! should expand to last command
    let result = exec.expand_command("!").unwrap();
    assert_eq!(result, "echo test");
}

#[test]
fn test_shell_executor_no_current_file() {
    let exec = ShellExecutor::new("/bin/sh");
    let result = exec.expand_command("cat %");
    assert!(result.is_err());
}

#[test]
fn test_shell_executor_no_previous_command() {
    let exec = ShellExecutor::new("/bin/sh");
    let result = exec.expand_command("!");
    assert!(result.is_err());
}

#[test]
fn test_ex_shell_filter_parse() {
    // Test that range + ! is parsed as ShellFilter
    let cmd = parse_ex_command("1,5!sort").unwrap();
    match cmd {
        ExCommand::ShellFilter { range, command } => {
            assert!(range.explicit);
            assert_eq!(command, "sort");
        }
        _ => panic!("Expected ShellFilter command"),
    }
}

#[test]
fn test_ex_shell_escape_parse() {
    // Test that plain ! is parsed as Shell
    let cmd = parse_ex_command("!ls -la").unwrap();
    match cmd {
        ExCommand::Shell { command } => {
            assert_eq!(command, "ls -la");
        }
        _ => panic!("Expected Shell command"),
    }
}

#[test]
fn test_ex_shell_read_parse() {
    // Test :r !command
    let cmd = parse_ex_command("r !date").unwrap();
    match cmd {
        ExCommand::ShellRead { command, .. } => {
            assert_eq!(command, "date");
        }
        _ => panic!("Expected ShellRead command"),
    }
}

#[test]
fn test_ex_shell_write_parse() {
    // Test :w !command
    let cmd = parse_ex_command("w !cat").unwrap();
    match cmd {
        ExCommand::ShellWrite { command, .. } => {
            assert_eq!(command, "cat");
        }
        _ => panic!("Expected ShellWrite command"),
    }
}

// ============================================================================
// New POSIX Command Tests
// ============================================================================

#[test]
fn test_motion_section_forward() {
    use vi_rs::command::motion::move_section_forward;

    // Test section motion with function-like structure
    let buffer = Buffer::from_text("line 1\n{\nline 3\n}\nline 5\n{\nline 7");

    // Move forward to first section boundary (line starting with '{')
    let result = move_section_forward(&buffer, 1).unwrap();
    assert_eq!(result.position.line, 2); // Line with '{'
}

#[test]
fn test_motion_section_backward() {
    use vi_rs::command::motion::move_section_backward;

    let mut buffer = Buffer::from_text("{\nline 2\n}\nline 4\n{\nline 6");
    buffer.set_line(6); // Start at last line

    let result = move_section_backward(&buffer, 1).unwrap();
    assert_eq!(result.position.line, 5); // Line with '{'
}

#[test]
fn test_motion_column() {
    use vi_rs::command::motion::move_to_column;

    let buffer = Buffer::from_text("hello world");

    let result = move_to_column(&buffer, 6).unwrap();
    assert_eq!(result.position.column, 5); // column 6 is 0-indexed as 5
}

#[test]
fn test_parser_double_bracket() {
    // Test [[ and ]] parsing
    let mut parser = CommandParser::new();

    parser.process_key(Key::Char('['));
    parser.process_key(Key::Char('['));
    assert!(parser.is_complete());
    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.command, '[');
    assert_eq!(cmd.char_arg, Some('['));

    parser.reset();

    parser.process_key(Key::Char(']'));
    parser.process_key(Key::Char(']'));
    assert!(parser.is_complete());
    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.command, ']');
    assert_eq!(cmd.char_arg, Some(']'));
}

#[test]
fn test_parser_find_commands() {
    // Test f, F, t, T parsing
    let mut parser = CommandParser::new();

    // Test 'f' command
    parser.process_key(Key::Char('f'));
    parser.process_key(Key::Char('x'));
    assert!(parser.is_complete());
    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.command, 'f');
    assert_eq!(cmd.char_arg, Some('x'));

    parser.reset();

    // Test 't' command
    parser.process_key(Key::Char('t'));
    parser.process_key(Key::Char('y'));
    assert!(parser.is_complete());
    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.command, 't');
    assert_eq!(cmd.char_arg, Some('y'));
}

#[test]
fn test_yank_lines_operation() {
    use vi_rs::command::operator::yank;

    let buffer = Buffer::from_text("line 1\nline 2\nline 3");
    let mut registers = Registers::new();

    let range = Range::lines(Position::new(1, 0), Position::new(2, 0));
    let _result = yank(&buffer, range, &mut registers, None).unwrap();

    // Check that text was yanked - use get_unnamed() and access fields directly
    let content = registers.get_unnamed().unwrap();
    assert!(content.linewise);
    assert!(content.text.contains("line 1"));
    assert!(content.text.contains("line 2"));
}

#[test]
fn test_paragraph_motion_in_buffer() {
    use vi_rs::command::motion::{move_paragraph_backward, move_paragraph_forward};

    let buffer = Buffer::from_text("para 1\n\npara 2\n\npara 3");

    // Forward
    let result = move_paragraph_forward(&buffer, 1).unwrap();
    assert!(result.position.line > 1);

    // Backward from end
    let mut buffer2 = Buffer::from_text("para 1\n\npara 2\n\npara 3");
    buffer2.set_line(5);
    let result = move_paragraph_backward(&buffer2, 1).unwrap();
    assert!(result.position.line < 5);
}

#[test]
fn test_find_command_struct() {
    use vi_rs::FindCommand;

    let find = FindCommand::new('f', 'x');
    assert_eq!(find.command, 'f');
    assert_eq!(find.target, 'x');
    assert_eq!(find.reverse(), 'F');

    let find2 = FindCommand::new('T', 'y');
    assert_eq!(find2.reverse(), 't');
}

// ============================================================================
// Tests for new POSIX compliance features: ., @, :source, :a/:i/:c
// ============================================================================

#[test]
fn test_last_command_enum() {
    use vi_rs::mode::InsertKind;
    use vi_rs::LastCommand;

    // Test Insert variant
    let insert_cmd = LastCommand::Insert {
        kind: InsertKind::Insert,
        text: "hello".to_string(),
        count: 1,
    };
    assert!(matches!(insert_cmd, LastCommand::Insert { .. }));

    // Test Simple variant
    let simple_cmd = LastCommand::Simple {
        command: 'x',
        count: 3,
        char_arg: None,
    };
    assert!(matches!(
        simple_cmd,
        LastCommand::Simple { command: 'x', .. }
    ));
}

#[test]
fn test_ex_insert_mode_enum() {
    use vi_rs::ExInsertMode;

    let append = ExInsertMode::Append(5);
    assert!(matches!(append, ExInsertMode::Append(5)));

    let insert = ExInsertMode::Insert(3);
    assert!(matches!(insert, ExInsertMode::Insert(3)));

    let change = ExInsertMode::Change { start: 1, end: 3 };
    assert!(matches!(change, ExInsertMode::Change { start: 1, end: 3 }));
}

#[test]
fn test_ex_source_parsing() {
    use vi_rs::ex::parse_ex_command;
    use vi_rs::ex::ExCommand;

    // Test :source command parsing
    let result = parse_ex_command("source test.vim");
    assert!(result.is_ok());
    if let Ok(ExCommand::Source { file }) = result {
        assert_eq!(file, "test.vim");
    } else {
        panic!("Expected Source command");
    }

    // Test :so abbreviation
    let result = parse_ex_command("so ~/.exrc");
    assert!(result.is_ok());
    if let Ok(ExCommand::Source { file }) = result {
        assert_eq!(file, "~/.exrc");
    } else {
        panic!("Expected Source command");
    }

    // Test source without filename fails
    let result = parse_ex_command("source");
    assert!(result.is_err());
}

#[test]
fn test_ex_append_parsing() {
    use vi_rs::ex::parse_ex_command;
    use vi_rs::ex::ExCommand;

    // Test :a command parsing
    let result = parse_ex_command("a");
    assert!(result.is_ok());
    assert!(matches!(result.unwrap(), ExCommand::Append { .. }));

    // Test :append
    let result = parse_ex_command("append");
    assert!(result.is_ok());
    assert!(matches!(result.unwrap(), ExCommand::Append { .. }));

    // Test with line number
    let result = parse_ex_command("5a");
    assert!(result.is_ok());
    if let Ok(ExCommand::Append { line }) = result {
        assert_eq!(line, 5);
    } else {
        panic!("Expected Append command with line 5");
    }
}

#[test]
fn test_ex_insert_parsing() {
    use vi_rs::ex::parse_ex_command;
    use vi_rs::ex::ExCommand;

    // Test :i command parsing
    let result = parse_ex_command("i");
    assert!(result.is_ok());
    assert!(matches!(result.unwrap(), ExCommand::Insert { .. }));

    // Test :insert
    let result = parse_ex_command("insert");
    assert!(result.is_ok());
    assert!(matches!(result.unwrap(), ExCommand::Insert { .. }));
}

#[test]
fn test_ex_change_parsing() {
    use vi_rs::ex::parse_ex_command;
    use vi_rs::ex::ExCommand;

    // Test :c command parsing
    let result = parse_ex_command("c");
    assert!(result.is_ok());
    assert!(matches!(result.unwrap(), ExCommand::Change { .. }));

    // Test :change
    let result = parse_ex_command("change");
    assert!(result.is_ok());
    assert!(matches!(result.unwrap(), ExCommand::Change { .. }));
}

#[test]
fn test_parsed_command_clone() {
    use vi_rs::command::MotionCommand;
    use vi_rs::command::ParsedCommand;

    // Test that ParsedCommand can be cloned (needed for . repeat)
    let cmd = ParsedCommand::new('d')
        .with_count(3, true)
        .with_motion(MotionCommand::new('w').with_count(2));

    let cloned = cmd.clone();
    assert_eq!(cloned.command, 'd');
    assert_eq!(cloned.count, 3);
    assert!(cloned.motion.is_some());
    assert_eq!(cloned.motion.unwrap().motion, 'w');
}

#[test]
fn test_at_command_parsing() {
    use vi_rs::command::CommandParser;
    use vi_rs::input::Key;

    // Test @ command parsing
    let mut parser = CommandParser::new();
    parser.process_key(Key::Char('@'));
    parser.process_key(Key::Char('a'));
    assert!(parser.is_complete());

    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.command, '@');
    assert_eq!(cmd.char_arg, Some('a'));
}

#[test]
fn test_at_at_command_parsing() {
    use vi_rs::command::CommandParser;
    use vi_rs::input::Key;

    // Test @@ command parsing (repeat last macro)
    let mut parser = CommandParser::new();
    parser.process_key(Key::Char('@'));
    parser.process_key(Key::Char('@'));
    assert!(parser.is_complete());

    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.command, '@');
    assert_eq!(cmd.char_arg, Some('@'));
}

#[test]
fn test_dot_command_parsing() {
    use vi_rs::command::CommandParser;
    use vi_rs::input::Key;

    // Test . command parsing
    let mut parser = CommandParser::new();
    parser.process_key(Key::Char('.'));
    assert!(parser.is_complete());

    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.command, '.');
}

#[test]
fn test_dot_with_count_parsing() {
    use vi_rs::command::CommandParser;
    use vi_rs::input::Key;

    // Test 3. command parsing (repeat with count)
    let mut parser = CommandParser::new();
    parser.process_key(Key::Char('3'));
    parser.process_key(Key::Char('.'));
    assert!(parser.is_complete());

    let cmd = parser.get_command().unwrap();
    assert_eq!(cmd.command, '.');
    assert_eq!(cmd.count, 3);
}

#[test]
fn test_macro_register_storage() {
    // Test that registers can store and retrieve macro-like content
    use vi_rs::register::{RegisterContent, Registers};

    let mut registers = Registers::new();

    // Store a "macro" in register a - a sequence that would delete a word
    let macro_content = "dw";
    registers.set('a', RegisterContent::new(macro_content.to_string(), false));

    // Verify we can retrieve it
    let content = registers.get('a').unwrap();
    assert_eq!(content.text, "dw");
    assert!(!content.linewise);
}

#[test]
fn test_macro_with_insert_mode() {
    // Test that registers can store sequences including insert mode
    use vi_rs::register::{RegisterContent, Registers};

    let mut registers = Registers::new();

    // Store a macro that enters insert mode, types text, and exits
    // In vi, ESC is \x1b
    let macro_content = "ihello\x1b";
    registers.set('b', RegisterContent::new(macro_content.to_string(), false));

    let content = registers.get('b').unwrap();
    assert_eq!(content.text, "ihello\x1b");
    assert!(content.text.contains('\x1b')); // Contains escape
}

#[test]
fn test_source_file_creation_and_content() {
    // Test that we can create a source file and it has valid ex commands
    use std::io::Write;
    use tempfile::NamedTempFile;

    // Create a temporary file with ex commands
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file, "\" This is a comment").unwrap();
    writeln!(file, "set number").unwrap();
    writeln!(file, "set tabstop=4").unwrap();
    file.flush().unwrap();

    // Read it back and verify content
    use std::fs::File;
    use std::io::{BufRead, BufReader};

    let f = File::open(file.path()).unwrap();
    let reader = BufReader::new(f);
    let lines: Vec<String> = reader.lines().map(|l| l.unwrap()).collect();

    assert_eq!(lines.len(), 3);
    assert!(lines[0].starts_with('"')); // Comment
    assert_eq!(lines[1], "set number");
    assert_eq!(lines[2], "set tabstop=4");
}

#[test]
fn test_source_commands_are_valid_ex() {
    // Verify that typical .exrc commands parse correctly
    use vi_rs::ex::parse_ex_command;

    // Common commands that would appear in a source file
    let commands = vec![
        ("set number", true),
        ("set tabstop=8", true),
        ("set autoindent", true),
        ("map Q gq", true),
        ("ab teh the", true),
    ];

    for (cmd, should_parse) in commands {
        let result = parse_ex_command(cmd);
        assert_eq!(
            result.is_ok(),
            should_parse,
            "Command '{}' parse result unexpected",
            cmd
        );
    }
}

#[test]
fn test_execute_keys_sequence() {
    // Test that a sequence of keys can be converted properly
    use vi_rs::input::Key;

    // Simulate what execute_keys_from_string does
    let keys_str = "3dw";
    let mut keys: Vec<Key> = Vec::new();

    for c in keys_str.chars() {
        let key = if c == '\x1b' {
            Key::Escape
        } else if c == '\n' || c == '\r' {
            Key::Enter
        } else if c.is_ascii_control() {
            let ctrl_char = (c as u8 + b'@') as char;
            Key::Ctrl(ctrl_char.to_ascii_lowercase())
        } else {
            Key::Char(c)
        };
        keys.push(key);
    }

    assert_eq!(keys.len(), 3);
    assert_eq!(keys[0], Key::Char('3'));
    assert_eq!(keys[1], Key::Char('d'));
    assert_eq!(keys[2], Key::Char('w'));
}

#[test]
fn test_execute_keys_with_escape() {
    use vi_rs::input::Key;

    let keys_str = "ihello\x1b"; // Insert "hello" then escape
    let mut keys: Vec<Key> = Vec::new();

    for c in keys_str.chars() {
        let key = if c == '\x1b' {
            Key::Escape
        } else if c == '\n' || c == '\r' {
            Key::Enter
        } else if c.is_ascii_control() {
            let ctrl_char = (c as u8 + b'@') as char;
            Key::Ctrl(ctrl_char.to_ascii_lowercase())
        } else {
            Key::Char(c)
        };
        keys.push(key);
    }

    assert_eq!(keys.len(), 7); // i, h, e, l, l, o, ESC
    assert_eq!(keys[0], Key::Char('i'));
    assert_eq!(keys[6], Key::Escape);
}

#[test]
fn test_execute_keys_with_control_chars() {
    use vi_rs::input::Key;

    // Ctrl-D is \x04
    let keys_str = "\x04";
    let mut keys: Vec<Key> = Vec::new();

    for c in keys_str.chars() {
        let key = if c == '\x1b' {
            Key::Escape
        } else if c == '\n' || c == '\r' {
            Key::Enter
        } else if c.is_ascii_control() {
            let ctrl_char = (c as u8 + b'@') as char;
            Key::Ctrl(ctrl_char.to_ascii_lowercase())
        } else {
            Key::Char(c)
        };
        keys.push(key);
    }

    assert_eq!(keys.len(), 1);
    assert_eq!(keys[0], Key::Ctrl('d'));
}
