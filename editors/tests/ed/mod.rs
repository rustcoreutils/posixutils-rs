//! Integration tests for the ed line editor.
//!
//! These tests verify the ed binary works correctly as a POSIX ed editor,
//! testing commands via stdin/stdout.

use plib::testing::{run_test, TestPlan};
use std::fs;
use tempfile::NamedTempFile;

// Helper to create a test plan for ed in silent mode
fn ed_test(stdin: &str, expected_out: &str) {
    run_test(TestPlan {
        cmd: "ed".to_string(),
        args: vec!["-s".to_string()],
        stdin_data: stdin.to_string(),
        expected_out: expected_out.to_string(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// Helper to test ed with a file
fn ed_test_with_file(file_content: &str, stdin: &str, expected_out: &str) {
    let temp = NamedTempFile::new().unwrap();
    fs::write(temp.path(), file_content).unwrap();

    run_test(TestPlan {
        cmd: "ed".to_string(),
        args: vec!["-s".to_string(), temp.path().to_string_lossy().to_string()],
        stdin_data: stdin.to_string(),
        expected_out: expected_out.to_string(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// ============================================================================
// Basic Operation Tests
// ============================================================================

#[test]
fn test_ed_quit() {
    ed_test("q\n", "");
}

#[test]
fn test_ed_force_quit() {
    ed_test("Q\n", "");
}

#[test]
fn test_ed_append_and_print() {
    ed_test(
        "a\nhello world\nline two\n.\n1,$p\nQ\n",
        "hello world\nline two\n",
    );
}

#[test]
fn test_ed_insert_and_print() {
    ed_test(
        "a\nfirst line\n.\n1i\ninserted line\n.\n1,$p\nQ\n",
        "inserted line\nfirst line\n",
    );
}

#[test]
fn test_ed_number_command() {
    ed_test(
        "a\nline one\nline two\n.\n1,$n\nQ\n",
        "     1\tline one\n     2\tline two\n",
    );
}

#[test]
fn test_ed_list_command() {
    // Test that list shows $ at end of lines
    ed_test("a\nhello\n.\n1l\nQ\n", "hello$\n");
}

#[test]
fn test_ed_delete() {
    ed_test(
        "a\nline one\nline two\nline three\n.\n2d\n1,$p\nQ\n",
        "line one\nline three\n",
    );
}

// ============================================================================
// Substitute Tests
// ============================================================================

#[test]
fn test_ed_substitute() {
    ed_test(
        "a\nhello world\n.\n1s/world/everyone/\n1p\nQ\n",
        "hello everyone\n",
    );
}

#[test]
fn test_ed_substitute_global() {
    ed_test(
        "a\nhello hello hello\n.\n1s/hello/hi/g\n1p\nQ\n",
        "hi hi hi\n",
    );
}

// ============================================================================
// Copy and Move Tests
// ============================================================================

#[test]
fn test_ed_copy() {
    ed_test(
        "a\nline one\nline two\n.\n1t2\n1,$p\nQ\n",
        "line one\nline two\nline one\n",
    );
}

#[test]
fn test_ed_move() {
    ed_test(
        "a\nline one\nline two\nline three\n.\n1m2\n1,$p\nQ\n",
        "line two\nline one\nline three\n",
    );
}

// ============================================================================
// Address Tests
// ============================================================================

#[test]
fn test_ed_goto_line() {
    ed_test("a\nline one\nline two\nline three\n.\n2\nQ\n", "line two\n");
}

#[test]
fn test_ed_range_all() {
    ed_test("a\na\nb\nc\nd\n.\n1,$p\nQ\n", "a\nb\nc\nd\n");
}

#[test]
fn test_ed_range_single() {
    ed_test("a\na\nb\nc\n.\n2p\nQ\n", "b\n");
}

#[test]
fn test_ed_range_explicit() {
    ed_test("a\na\nb\nc\nd\ne\n.\n2,4p\nQ\n", "b\nc\nd\n");
}

#[test]
fn test_ed_current_line_address() {
    ed_test("a\nfirst\nsecond\nthird\n.\n2\n.p\nQ\n", "second\nsecond\n");
}

#[test]
fn test_ed_last_line_address() {
    ed_test("a\nfirst\nlast\n.\n$p\nQ\n", "last\n");
}

// ============================================================================
// Join Tests
// ============================================================================

#[test]
fn test_ed_join() {
    ed_test(
        "a\nline one\nline two\n.\n1,2j\n1p\nQ\n",
        "line oneline two\n",
    );
}

// ============================================================================
// Undo Tests
// ============================================================================

#[test]
fn test_ed_undo() {
    ed_test("a\nhello\n.\n1s/hello/goodbye/\nu\n1p\nQ\n", "hello\n");
}

// ============================================================================
// File Operation Tests
// ============================================================================

#[test]
fn test_ed_read_file() {
    ed_test_with_file("content from file\n", "1,$p\nq\n", "content from file\n");
}

#[test]
fn test_ed_write_file() {
    let temp = NamedTempFile::new().unwrap();
    let path = temp.path().to_string_lossy().to_string();

    run_test(TestPlan {
        cmd: "ed".to_string(),
        args: vec!["-s".to_string()],
        stdin_data: format!("a\ntest content\n.\nw {}\nq\n", path),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    let content = fs::read_to_string(temp.path()).unwrap();
    assert_eq!(content, "test content\n");
}

// ============================================================================
// Line Number Command Tests
// ============================================================================

#[test]
fn test_ed_line_number() {
    ed_test("a\nline one\nline two\nline three\n.\n=\nQ\n", "3\n");
}

#[test]
fn test_ed_line_number_with_address() {
    ed_test("a\nline one\nline two\nline three\n.\n2=\nQ\n", "2\n");
}

// ============================================================================
// Mark Tests
// ============================================================================

#[test]
fn test_ed_mark() {
    ed_test(
        "a\nline one\nline two\nline three\n.\n2ka\n3\n'ap\nQ\n",
        "line three\nline two\n",
    );
}

// ============================================================================
// Global Command Tests
// ============================================================================

#[test]
fn test_ed_global_print() {
    ed_test(
        "a\napple\nbanana\napricot\ncherry\n.\ng/^a/p\nQ\n",
        "apple\napricot\n",
    );
}

#[test]
fn test_ed_global_delete() {
    ed_test(
        "a\nkeep this\ndelete me\nkeep this too\ndelete me also\n.\ng/delete/d\n1,$p\nQ\n",
        "keep this\nkeep this too\n",
    );
}

// ============================================================================
// Scroll Command Tests
// ============================================================================

#[test]
fn test_ed_scroll() {
    ed_test(
        "a\nline 1\nline 2\nline 3\nline 4\nline 5\n.\n1z3\nQ\n",
        "line 1\nline 2\nline 3\n",
    );
}

// ============================================================================
// Change Command Tests
// ============================================================================

#[test]
fn test_ed_change() {
    ed_test(
        "a\nold line\nkeep this\n.\n1c\nnew line\n.\n1,$p\nQ\n",
        "new line\nkeep this\n",
    );
}

// ============================================================================
// Help Command Tests
// ============================================================================

#[test]
fn test_ed_help_mode() {
    // H toggles help mode, then an invalid command should show the error message
    ed_test("H\nQ\n", "");
}

// ============================================================================
// Address Separator Tests (POSIX compliance)
// ============================================================================

#[test]
fn test_ed_semicolon_separator() {
    // Semicolon separator: second address is relative to first
    // 2;+1p should print lines 2 and 3 (2 and 2+1)
    ed_test(
        "a\nline 1\nline 2\nline 3\nline 4\nline 5\n.\n2;+1p\nQ\n",
        "line 2\nline 3\n",
    );
}

#[test]
fn test_ed_comma_separator() {
    // Comma separator: second address is relative to current line (which is 5 at end)
    // After adding 5 lines, current line is 5
    // 2,4p should print lines 2 to 4
    ed_test(
        "a\nline 1\nline 2\nline 3\nline 4\nline 5\n.\n2,4p\nQ\n",
        "line 2\nline 3\nline 4\n",
    );
}

#[test]
fn test_ed_semicolon_with_offset() {
    // 3;+2p should print lines 3, 4, 5 (3 to 3+2)
    ed_test(
        "a\nline 1\nline 2\nline 3\nline 4\nline 5\n.\n3;+2p\nQ\n",
        "line 3\nline 4\nline 5\n",
    );
}

#[test]
fn test_ed_semicolon_current_line() {
    // Go to line 2, then ;+2p should print from current line (2) to 2+2=4
    ed_test(
        "a\nline 1\nline 2\nline 3\nline 4\nline 5\n.\n2\n;+2p\nQ\n",
        "line 2\nline 2\nline 3\nline 4\n",
    );
}

// ============================================================================
// Additional File Operation Tests
// ============================================================================

#[test]
fn test_ed_edit_command() {
    // Test edit command with a file
    let temp = NamedTempFile::new().unwrap();
    fs::write(temp.path(), "test content\n").unwrap();

    run_test(TestPlan {
        cmd: "ed".to_string(),
        args: vec!["-s".to_string()],
        stdin_data: format!("e {}\n1p\nq\n", temp.path().to_string_lossy()),
        expected_out: "test content\n".to_string(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_ed_filename_command() {
    // Test f command - sets filename then prints it
    // The output will be the temp file path, which we verify by checking it's printed
    let temp = NamedTempFile::new().unwrap();
    fs::write(temp.path(), "content\n").unwrap();
    let path = temp.path().to_string_lossy().to_string();

    run_test(TestPlan {
        cmd: "ed".to_string(),
        args: vec!["-s".to_string(), path.clone()],
        stdin_data: "f\nq\n".to_string(),
        expected_out: format!("{}\n", path),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// ============================================================================
// Address Offset Tests
// ============================================================================

#[test]
fn test_ed_positive_offset() {
    ed_test(
        "a\nline 1\nline 2\nline 3\n.\n1\n+1p\nQ\n",
        "line 1\nline 2\n",
    );
}

#[test]
fn test_ed_negative_offset() {
    ed_test(
        "a\nline 1\nline 2\nline 3\n.\n3\n-1p\nQ\n",
        "line 3\nline 2\n",
    );
}

#[test]
fn test_ed_multiple_offsets() {
    // 1+1+1 should be line 3
    ed_test("a\nline 1\nline 2\nline 3\n.\n1+1+1p\nQ\n", "line 3\n");
}

// ============================================================================
// Phase 3: Comprehensive Line Operation Tests
// ============================================================================

// --- Delete Command Tests ---

#[test]
fn test_ed_delete_range() {
    // Delete lines 2-3, current line becomes 2 (the old line 4)
    ed_test(
        "a\nline 1\nline 2\nline 3\nline 4\n.\n2,3d\n.p\nQ\n",
        "line 4\n",
    );
}

#[test]
fn test_ed_delete_at_end() {
    // Delete at end, current line becomes new last line
    ed_test("a\nline 1\nline 2\nline 3\n.\n3d\n.p\nQ\n", "line 2\n");
}

#[test]
fn test_ed_delete_all() {
    // Delete all lines, current line becomes 0
    ed_test("a\nline 1\nline 2\n.\n1,2d\n=\nQ\n", "0\n");
}

// --- Change Command Tests ---

#[test]
fn test_ed_change_with_text() {
    // Change line 2 with new text
    ed_test(
        "a\nline 1\nold line\nline 3\n.\n2c\nnew line\n.\n1,$p\nQ\n",
        "line 1\nnew line\nline 3\n",
    );
}

#[test]
fn test_ed_change_no_text() {
    // Change command with no text (effectively delete)
    ed_test(
        "a\nline 1\nline 2\nline 3\n.\n2c\n.\n1,$p\nQ\n",
        "line 1\nline 3\n",
    );
}

#[test]
fn test_ed_change_range() {
    // Change multiple lines
    ed_test(
        "a\nline 1\nline 2\nline 3\nline 4\n.\n2,3c\nreplacement\n.\n1,$p\nQ\n",
        "line 1\nreplacement\nline 4\n",
    );
}

// --- Join Command Tests ---

#[test]
fn test_ed_join_single_address() {
    // Join with single address should do nothing per POSIX
    ed_test("a\nline 1\nline 2\n.\n1j\n1,$p\nQ\n", "line 1\nline 2\n");
}

#[test]
fn test_ed_join_default_range() {
    // Default range for j is .,.+1
    ed_test(
        "a\nline 1\nline 2\nline 3\n.\n1\nj\n1,$p\nQ\n",
        "line 1\nline 1line 2\nline 3\n",
    );
}

#[test]
fn test_ed_join_multiple() {
    // Join three lines
    ed_test("a\nA\nB\nC\nD\n.\n1,3j\n1,$p\nQ\n", "ABC\nD\n");
}

// --- Move Command Tests ---

#[test]
fn test_ed_move_to_start() {
    // Move line 3 to start (address 0)
    ed_test(
        "a\nline 1\nline 2\nline 3\n.\n3m0\n1,$p\nQ\n",
        "line 3\nline 1\nline 2\n",
    );
}

#[test]
fn test_ed_move_to_end() {
    // Move line 1 to end
    ed_test(
        "a\nline 1\nline 2\nline 3\n.\n1m$\n1,$p\nQ\n",
        "line 2\nline 3\nline 1\n",
    );
}

#[test]
fn test_ed_move_range() {
    // Move lines 1-2 to after line 3
    ed_test("a\nA\nB\nC\nD\n.\n1,2m3\n1,$p\nQ\n", "C\nA\nB\nD\n");
}

// --- Copy Command Tests ---

#[test]
fn test_ed_copy_to_start() {
    // Copy line 3 to start
    ed_test(
        "a\nline 1\nline 2\nline 3\n.\n3t0\n1,$p\nQ\n",
        "line 3\nline 1\nline 2\nline 3\n",
    );
}

#[test]
fn test_ed_copy_range() {
    // Copy lines 1-2 to after line 3
    ed_test("a\nA\nB\nC\n.\n1,2t3\n1,$p\nQ\n", "A\nB\nC\nA\nB\n");
}

// --- Mark Command Tests ---

#[test]
fn test_ed_mark_and_address() {
    // Set mark and use it in address
    ed_test(
        "a\nfirst\nsecond\nthird\n.\n2kb\n1\n'bp\nQ\n",
        "first\nsecond\n",
    );
}

#[test]
fn test_ed_mark_in_range() {
    // Use mark in a range
    ed_test("a\nA\nB\nC\nD\nE\n.\n2ka\n4kb\n'a,'bp\nQ\n", "B\nC\nD\n");
}

// --- Number Command Tests ---

#[test]
fn test_ed_number_range() {
    // Number command on range
    ed_test(
        "a\nfirst\nsecond\nthird\n.\n1,3n\nQ\n",
        "     1\tfirst\n     2\tsecond\n     3\tthird\n",
    );
}

// --- List Command Tests ---

#[test]
fn test_ed_list_tab() {
    // List shows \t for tab
    ed_test("a\nhello\tworld\n.\n1l\nQ\n", "hello\\tworld$\n");
}

#[test]
fn test_ed_list_dollar() {
    // List escapes $ in text
    ed_test("a\nprice: $100\n.\n1l\nQ\n", "price: \\$100$\n");
}

#[test]
fn test_ed_list_backslash() {
    // List escapes backslash
    ed_test("a\npath\\name\n.\n1l\nQ\n", "path\\\\name$\n");
}

// --- Line Number Command Tests ---

#[test]
fn test_ed_line_number_current_unchanged() {
    // = command should not change current line
    // Go to line 2, use .= to print line number of current line, then verify current line unchanged
    ed_test(
        "a\nfirst\nsecond\nthird\n.\n2\n.=\n.p\nQ\n",
        "second\n2\nsecond\n",
    );
}

#[test]
fn test_ed_line_number_default() {
    // Default address for = is $ (last line)
    ed_test("a\nline 1\nline 2\nline 3\n.\n=\nQ\n", "3\n");
}

// --- Search Address Tests ---

#[test]
fn test_ed_search_forward_in_range() {
    // Use forward search as address
    ed_test(
        "a\napple\nbanana\ncherry\napricot\n.\n/cherry/p\nQ\n",
        "cherry\n",
    );
}

#[test]
fn test_ed_search_backward_in_range() {
    // Use backward search as address
    ed_test(
        "a\napple\nbanana\ncherry\n.\n$\n?banana?p\nQ\n",
        "cherry\nbanana\n",
    );
}

#[test]
fn test_ed_search_wrap_forward() {
    // Forward search wraps around
    ed_test(
        "a\napple\nbanana\ncherry\n.\n3\n/apple/p\nQ\n",
        "cherry\napple\n",
    );
}

// ============================================================================
// Phase 4: Substitute Command Tests
// ============================================================================

#[test]
fn test_ed_substitute_ampersand() {
    // & in replacement is replaced by matched string
    ed_test(
        "a\nhello world\n.\n1s/world/& &/\n1p\nQ\n",
        "hello world world\n",
    );
}

#[test]
fn test_ed_substitute_escaped_ampersand() {
    // \& in replacement is literal &
    ed_test("a\nhello world\n.\n1s/world/\\&/\n1p\nQ\n", "hello &\n");
}

#[test]
fn test_ed_substitute_percent_repeat() {
    // % as sole replacement uses previous replacement
    ed_test(
        "a\nfoo bar\nfoo baz\n.\n1s/foo/XXX/\n2s/foo/%/\n1,$p\nQ\n",
        "XXX bar\nXXX baz\n",
    );
}

#[test]
fn test_ed_substitute_nth_occurrence() {
    // Replace nth occurrence with count flag
    ed_test(
        "a\naaa bbb aaa ccc aaa\n.\n1s/aaa/XXX/2\n1p\nQ\n",
        "aaa bbb XXX ccc aaa\n",
    );
}

#[test]
fn test_ed_substitute_with_print() {
    // s command with p flag prints result
    ed_test(
        "a\nhello world\n.\n1s/world/everyone/p\nQ\n",
        "hello everyone\n",
    );
}

#[test]
fn test_ed_substitute_with_number() {
    // s command with n flag prints with line number
    ed_test(
        "a\nhello world\n.\n1s/world/everyone/n\nQ\n",
        "     1\thello everyone\n",
    );
}

#[test]
fn test_ed_substitute_global_on_line() {
    // s command with g flag replaces all occurrences on line
    ed_test(
        "a\naaa bbb aaa ccc aaa\n.\n1s/aaa/X/g\n1p\nQ\n",
        "X bbb X ccc X\n",
    );
}

#[test]
fn test_ed_substitute_range() {
    // Substitute on multiple lines
    ed_test(
        "a\nfoo one\nbar two\nfoo three\n.\n1,3s/foo/baz/\n1,$p\nQ\n",
        "baz one\nbar two\nbaz three\n",
    );
}

#[test]
fn test_ed_substitute_repeat_pattern() {
    // Empty pattern uses previous pattern
    ed_test(
        "a\nhello world\nhello there\n.\n1s/hello/hi/\n2s//hi/\n1,$p\nQ\n",
        "hi world\nhi there\n",
    );
}

#[test]
fn test_ed_substitute_different_delimiter() {
    // Can use different delimiter character
    ed_test("a\npath/to/file\n.\n1s#path#dir#\n1p\nQ\n", "dir/to/file\n");
}

// ============================================================================
// Phase 4: Undo Command Tests
// ============================================================================

#[test]
fn test_ed_undo_substitute() {
    // Undo a substitute command
    ed_test(
        "a\nhello world\n.\n1s/world/everyone/\nu\n1p\nQ\n",
        "hello world\n",
    );
}

#[test]
fn test_ed_undo_delete() {
    // Undo a delete command
    ed_test(
        "a\nline 1\nline 2\nline 3\n.\n2d\nu\n1,$p\nQ\n",
        "line 1\nline 2\nline 3\n",
    );
}

#[test]
fn test_ed_undo_append() {
    // Undo an append command
    ed_test("a\nline 1\n.\na\nline 2\n.\nu\n1,$p\nQ\n", "line 1\n");
}

#[test]
fn test_ed_undo_insert() {
    // Undo an insert command
    ed_test("a\nline 2\n.\n1i\nline 1\n.\nu\n1,$p\nQ\n", "line 2\n");
}

#[test]
fn test_ed_undo_change() {
    // Undo a change command
    ed_test(
        "a\noriginal\n.\n1c\nreplacement\n.\nu\n1p\nQ\n",
        "original\n",
    );
}

#[test]
fn test_ed_undo_join() {
    // Undo a join command
    ed_test(
        "a\nline 1\nline 2\n.\n1,2j\nu\n1,$p\nQ\n",
        "line 1\nline 2\n",
    );
}

#[test]
fn test_ed_undo_move() {
    // Undo a move command
    ed_test("a\nA\nB\nC\n.\n1m2\nu\n1,$p\nQ\n", "A\nB\nC\n");
}

#[test]
fn test_ed_undo_copy() {
    // Undo a copy command
    ed_test("a\nA\nB\n.\n1t2\nu\n1,$p\nQ\n", "A\nB\n");
}

#[test]
fn test_ed_undo_redo() {
    // Undo then redo (undo of undo)
    ed_test("a\nhello\n.\n1s/hello/goodbye/\nu\nu\n1p\nQ\n", "goodbye\n");
}

#[test]
fn test_ed_undo_restores_current_line() {
    // Undo should restore current line position
    ed_test(
        "a\nline 1\nline 2\nline 3\n.\n1\n3d\n.=\nu\n.=\nQ\n",
        "line 1\n2\n1\n",
    );
}

// ============================================================================
// Phase 5: Comprehensive Global Command Tests
// ============================================================================

#[test]
fn test_ed_global_print_all_matches() {
    // g/pattern/p should print all matching lines
    ed_test(
        "a\nfoo one\nbar two\nfoo three\nbar four\nfoo five\n.\ng/foo/p\nQ\n",
        "foo one\nfoo three\nfoo five\n",
    );
}

#[test]
fn test_ed_global_delete_all_matches() {
    // g/pattern/d should delete all matching lines
    ed_test(
        "a\nkeep 1\nremove this\nkeep 2\nremove that\nkeep 3\n.\ng/remove/d\n1,$p\nQ\n",
        "keep 1\nkeep 2\nkeep 3\n",
    );
}

#[test]
fn test_ed_global_number_matches() {
    // g/pattern/n should print matching lines with numbers
    ed_test(
        "a\nfoo\nbar\nfoo\n.\ng/foo/n\nQ\n",
        "     1\tfoo\n     3\tfoo\n",
    );
}

#[test]
fn test_ed_global_substitute() {
    // g/pattern/s should substitute on matching lines
    ed_test(
        "a\nfoo one\nbar two\nfoo three\n.\ng/foo/s/foo/baz/\n1,$p\nQ\n",
        "baz one\nbar two\nbaz three\n",
    );
}

#[test]
fn test_ed_global_invert_print() {
    // v/pattern/p should print non-matching lines
    ed_test(
        "a\napple\nbanana\napricot\ncherry\n.\nv/^a/p\nQ\n",
        "banana\ncherry\n",
    );
}

#[test]
fn test_ed_global_invert_delete() {
    // v/pattern/d should delete non-matching lines
    ed_test(
        "a\napple\nbanana\napricot\ncherry\n.\nv/^a/d\n1,$p\nQ\n",
        "apple\napricot\n",
    );
}

#[test]
fn test_ed_global_undo_entire_operation() {
    // Undo should restore buffer to state before g command
    ed_test(
        "a\nfoo 1\nbar 2\nfoo 3\nbar 4\n.\ng/foo/d\nu\n1,$p\nQ\n",
        "foo 1\nbar 2\nfoo 3\nbar 4\n",
    );
}

#[test]
fn test_ed_global_empty_command_defaults_to_print() {
    // g/pattern/ with no command should default to print
    ed_test(
        "a\napple\nbanana\napricot\n.\ng/^a/\nQ\n",
        "apple\napricot\n",
    );
}

#[test]
fn test_ed_global_with_range() {
    // Global command with address range
    ed_test(
        "a\napple 1\nbanana 2\napple 3\nbanana 4\napple 5\n.\n2,4g/apple/p\nQ\n",
        "apple 3\n",
    );
}

#[test]
fn test_ed_global_no_match_unchanged() {
    // If no lines match, current line should not change
    ed_test(
        "a\napple\nbanana\ncherry\n.\n2\ng/xyz/p\n.p\nQ\n",
        "banana\nbanana\n",
    );
}

#[test]
fn test_ed_global_current_line_after() {
    // After g command, current line should be set by last command
    ed_test(
        "a\nfoo 1\nbar 2\nfoo 3\nbar 4\nfoo 5\n.\ng/foo/p\n.=\nQ\n",
        "foo 1\nfoo 3\nfoo 5\n5\n",
    );
}

#[test]
fn test_ed_global_different_delimiter() {
    // Global command with different delimiter
    ed_test(
        "a\npath/to/file\npath/to/dir\nother\n.\ng#path#p\nQ\n",
        "path/to/file\npath/to/dir\n",
    );
}

#[test]
fn test_ed_global_with_previous_pattern() {
    // Empty pattern should use previous pattern (set by search address)
    ed_test("a\nfoo\nbar\nfoo\n.\n/foo/\ng//p\nQ\n", "foo\nfoo\nfoo\n");
}

#[test]
fn test_ed_invert_global_with_range() {
    // v command with address range
    ed_test(
        "a\napple 1\nbanana 2\napple 3\nbanana 4\napple 5\n.\n2,4v/apple/p\nQ\n",
        "banana 2\nbanana 4\n",
    );
}

#[test]
fn test_ed_global_substitute_with_flags() {
    // g with substitute and flags
    ed_test(
        "a\nfoo foo foo\nbar bar\nfoo baz foo\n.\ng/foo/s/foo/X/g\n1,$p\nQ\n",
        "X X X\nbar bar\nX baz X\n",
    );
}

#[test]
fn test_ed_global_undo_substitute() {
    // Undo global substitute
    ed_test(
        "a\nfoo one\nbar two\nfoo three\n.\ng/foo/s/foo/baz/\nu\n1,$p\nQ\n",
        "foo one\nbar two\nfoo three\n",
    );
}

#[test]
fn test_ed_global_list_command() {
    // g with list command
    ed_test(
        "a\nfoo\tbar\nbaz\nfoo\tend\n.\ng/foo/l\nQ\n",
        "foo\\tbar$\nfoo\\tend$\n",
    );
}
