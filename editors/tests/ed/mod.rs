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
