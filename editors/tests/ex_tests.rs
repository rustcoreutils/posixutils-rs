//! Integration tests for the ex editor.
//!
//! These tests verify the ex binary works correctly in line-oriented mode,
//! testing POSIX ex commands via stdin/stdout.

use plib::testing::{run_test, TestPlan};
use std::fs;
use tempfile::NamedTempFile;

// Helper to create a test plan for ex in silent mode
fn ex_test(stdin: &str, expected_out: &str) {
    run_test(TestPlan {
        cmd: "ex".to_string(),
        args: vec!["-s".to_string()],
        stdin_data: stdin.to_string(),
        expected_out: expected_out.to_string(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

// Helper to test ex with a file
fn ex_test_with_file(file_content: &str, stdin: &str, expected_out: &str) {
    let temp = NamedTempFile::new().unwrap();
    fs::write(temp.path(), file_content).unwrap();

    run_test(TestPlan {
        cmd: "ex".to_string(),
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
fn test_ex_quit() {
    ex_test("q\n", "");
}

#[test]
fn test_ex_append_and_print() {
    ex_test(
        "a\nhello world\nline two\n.\n1,$p\nq!\n",
        "hello world\nline two\n",
    );
}

#[test]
fn test_ex_insert_and_print() {
    ex_test(
        "a\nfirst line\n.\n1i\ninserted line\n.\n1,$p\nq!\n",
        "inserted line\nfirst line\n",
    );
}

#[test]
fn test_ex_number_command() {
    ex_test(
        "a\nline one\nline two\n.\n1,$nu\nq!\n",
        "     1\tline one\n     2\tline two\n",
    );
}

#[test]
fn test_ex_list_command() {
    // Test that list shows $ at end of lines
    ex_test("a\nhello\n.\n1l\nq!\n", "hello$\n");
}

#[test]
fn test_ex_delete() {
    ex_test(
        "a\nline one\nline two\nline three\n.\n2d\n1,$p\nq!\n",
        "line one\nline three\n",
    );
}

#[test]
fn test_ex_substitute() {
    ex_test(
        "a\nhello world\n.\n1s/world/everyone/\n1p\nq!\n",
        "hello everyone\n",
    );
}

#[test]
fn test_ex_substitute_global() {
    ex_test(
        "a\nhello hello hello\n.\n1s/hello/hi/g\n1p\nq!\n",
        "hi hi hi\n",
    );
}

#[test]
fn test_ex_yank_and_put() {
    ex_test(
        "a\nline one\nline two\n.\n1y\n2pu\n1,$p\nq!\n",
        "line one\nline two\nline one\n",
    );
}

#[test]
fn test_ex_copy() {
    ex_test(
        "a\nline one\nline two\n.\n1co2\n1,$p\nq!\n",
        "line one\nline two\nline one\n",
    );
}

#[test]
fn test_ex_move() {
    ex_test(
        "a\nline one\nline two\nline three\n.\n1m2\n1,$p\nq!\n",
        "line two\nline one\nline three\n",
    );
}

#[test]
fn test_ex_goto_line() {
    ex_test(
        "a\nline one\nline two\nline three\n.\n2\np\nq!\n",
        "line two\n",
    );
}

#[test]
fn test_ex_join() {
    ex_test(
        "a\nline one\nline two\n.\n1,2j\n1p\nq!\n",
        "line one line two\n",
    );
}

#[test]
fn test_ex_undo() {
    ex_test("a\nhello\n.\n1s/hello/goodbye/\nu\n1p\nq!\n", "hello\n");
}

// ============================================================================
// File Operation Tests
// ============================================================================

#[test]
fn test_ex_read_file() {
    ex_test_with_file("content from file\n", "1,$p\nq\n", "content from file\n");
}

#[test]
fn test_ex_write_file() {
    let temp = NamedTempFile::new().unwrap();
    let path = temp.path().to_string_lossy().to_string();

    run_test(TestPlan {
        cmd: "ex".to_string(),
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
// Address Range Tests
// ============================================================================

#[test]
fn test_ex_range_all() {
    ex_test("a\na\nb\nc\nd\n.\n1,$p\nq!\n", "a\nb\nc\nd\n");
}

#[test]
fn test_ex_range_single() {
    ex_test("a\na\nb\nc\n.\n2p\nq!\n", "b\n");
}

#[test]
fn test_ex_range_explicit() {
    ex_test("a\na\nb\nc\nd\ne\n.\n2,4p\nq!\n", "b\nc\nd\n");
}

#[test]
fn test_ex_current_line_address() {
    ex_test("a\nfirst\nsecond\nthird\n.\n2\n.p\nq!\n", "second\n");
}

#[test]
fn test_ex_last_line_address() {
    ex_test("a\nfirst\nlast\n.\n$p\nq!\n", "last\n");
}

// ============================================================================
// Global Command Tests
// ============================================================================

#[test]
fn test_ex_global_delete() {
    ex_test(
        "a\nkeep this\ndelete me\nkeep this too\ndelete me also\n.\ng/delete/d\n1,$p\nq!\n",
        "keep this\nkeep this too\n",
    );
}

#[test]
fn test_ex_global_print() {
    ex_test(
        "a\napple\nbanana\napricot\ncherry\n.\ng/^a/p\nq!\n",
        "apple\napricot\n",
    );
}

// ============================================================================
// Set Option Tests
// ============================================================================

#[test]
fn test_ex_set_option() {
    // Just verify set command is accepted without error
    ex_test("set number\nq!\n", "");
}

// ============================================================================
// Error Handling Tests
// ============================================================================

#[test]
fn test_ex_invalid_command_silent() {
    // In silent mode, errors exit with code 1
    // Note: Error message goes to stderr which TestPlan captures separately
    run_test(TestPlan {
        cmd: "ex".to_string(),
        args: vec!["-s".to_string()],
        stdin_data: "invalidcmd\n".to_string(),
        expected_out: String::new(),
        expected_err: "Invalid command: invalidcmd\n".to_string(),
        expected_exit_code: 1,
    });
}

// ============================================================================
// Version and Help Tests
// ============================================================================

#[test]
fn test_ex_version_command() {
    // Version command should return package info - but we don't check exact output
    // Just verify it runs without error
    ex_test("version\nq!\n", "");
}
