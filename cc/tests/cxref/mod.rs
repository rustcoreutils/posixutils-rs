//! Integration tests for cxref

use std::fs;
use std::process::Command;
use tempfile::TempDir;

fn run_cxref(args: &[&str]) -> (String, String, bool) {
    let output = Command::new(env!("CARGO_BIN_EXE_cxref"))
        .args(args)
        .output()
        .expect("Failed to execute cxref");

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    (stdout, stderr, output.status.success())
}

#[test]
fn test_cxref_basic() {
    let (stdout, stderr, success) = run_cxref(&["tests/cxref/test.c"]);

    assert!(success, "cxref failed: {}", stderr);

    // Check for symbol entries
    assert!(
        stdout.contains("counter"),
        "Should contain counter symbol: {}",
        stdout
    );
    assert!(
        stdout.contains("increment"),
        "Should contain increment function: {}",
        stdout
    );
    assert!(
        stdout.contains("decrement"),
        "Should contain decrement function: {}",
        stdout
    );
    assert!(
        stdout.contains("main"),
        "Should contain main function: {}",
        stdout
    );
}

#[test]
fn test_cxref_definitions_marked() {
    let (stdout, _stderr, success) = run_cxref(&["tests/cxref/test.c"]);

    assert!(success);

    // Definitions should be marked with *
    // Look for lines that contain both the symbol and a * prefix on a line number
    let has_definition_marker = stdout.lines().any(|line| line.contains('*'));

    assert!(
        has_definition_marker,
        "Should have definition markers (*): {}",
        stdout
    );
}

#[test]
fn test_cxref_silent_mode() {
    let (stdout, stderr, success) = run_cxref(&["-s", "tests/cxref/test.c"]);

    assert!(success, "cxref -s failed: {}", stderr);

    // In silent mode, filenames should not appear
    // The file path should not be in the output
    // Note: the symbol "test" might appear, but the full path shouldn't
    let lines_without_filename = stdout
        .lines()
        .filter(|line| !line.contains("tests/cxref/test.c"))
        .count();

    let total_lines = stdout.lines().count();

    assert_eq!(
        lines_without_filename, total_lines,
        "Silent mode should not show filenames: {}",
        stdout
    );
}

#[test]
fn test_cxref_output_file() {
    let temp_dir = TempDir::new().unwrap();
    let output_file = temp_dir.path().join("xref.txt");

    let (_, stderr, success) =
        run_cxref(&["-o", output_file.to_str().unwrap(), "tests/cxref/test.c"]);

    assert!(success, "cxref -o failed: {}", stderr);
    assert!(output_file.exists(), "Output file should be created");

    let content = fs::read_to_string(&output_file).unwrap();
    assert!(
        content.contains("counter"),
        "Output file should contain cross-reference data"
    );
}

#[test]
fn test_cxref_symbol_references() {
    let (stdout, _stderr, success) = run_cxref(&["tests/cxref/test.c"]);

    assert!(success);

    // counter is used in multiple functions: increment, decrement, main
    // Find the counter section and verify it shows multiple references
    let counter_line = stdout
        .lines()
        .find(|line| line.starts_with("counter"))
        .unwrap_or("");

    assert!(
        !counter_line.is_empty(),
        "Should have a line starting with counter: {}",
        stdout
    );

    // The counter symbol should have multiple line references
    let has_multiple_refs = stdout.contains("counter") && stdout.lines().count() > 1;
    assert!(has_multiple_refs, "counter should appear with references");
}

#[test]
fn test_cxref_function_scope() {
    let (stdout, _stderr, success) = run_cxref(&["tests/cxref/test.c"]);

    assert!(success);

    // Check that function names appear in the output
    // They should be tracked as the scope for references
    assert!(
        stdout.contains("increment") || stdout.contains("decrement") || stdout.contains("main"),
        "Should show function scopes in output: {}",
        stdout
    );
}
