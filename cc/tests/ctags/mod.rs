//! Integration tests for ctags

use std::fs;
use std::process::Command;
use tempfile::TempDir;

fn run_ctags(args: &[&str]) -> (String, String, bool) {
    let output = Command::new(env!("CARGO_BIN_EXE_ctags"))
        .args(args)
        .output()
        .expect("Failed to execute ctags");

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    (stdout, stderr, output.status.success())
}

#[test]
fn test_ctags_basic() {
    let temp_dir = TempDir::new().unwrap();
    let tags_file = temp_dir.path().join("tags");

    let (_, stderr, success) =
        run_ctags(&["-f", tags_file.to_str().unwrap(), "tests/ctags/test.c"]);

    assert!(success, "ctags failed: {}", stderr);
    assert!(tags_file.exists(), "tags file should be created");

    let tags_content = fs::read_to_string(&tags_file).unwrap();

    // Check for function tags
    assert!(
        tags_content.contains("foo\ttests/ctags/test.c"),
        "Should contain foo function tag"
    );
    assert!(
        tags_content.contains("bar\ttests/ctags/test.c"),
        "Should contain bar function tag"
    );

    // Check for macro tag
    assert!(
        tags_content.contains("MAX\ttests/ctags/test.c"),
        "Should contain MAX macro tag"
    );

    // Check for typedef tag
    assert!(
        tags_content.contains("myint\ttests/ctags/test.c"),
        "Should contain myint typedef tag"
    );

    // main should be prefixed with Mtest
    assert!(
        tags_content.contains("Mtest\ttests/ctags/test.c"),
        "Should contain Mtest (main) tag"
    );
}

#[test]
fn test_ctags_index_mode() {
    let (stdout, stderr, success) = run_ctags(&["-x", "tests/ctags/test.c"]);

    assert!(success, "ctags -x failed: {}", stderr);

    // Check for function entries in index format
    assert!(stdout.contains("foo"), "Should contain foo in index output");
    assert!(stdout.contains("bar"), "Should contain bar in index output");
    assert!(stdout.contains("MAX"), "Should contain MAX in index output");
}

#[test]
fn test_ctags_append_mode() {
    let temp_dir = TempDir::new().unwrap();
    let tags_file = temp_dir.path().join("tags");

    // Create initial tags file with some content
    fs::write(&tags_file, "existing\tfile.c\t/^existing$/\n").unwrap();

    let (_, stderr, success) = run_ctags(&[
        "-a",
        "-f",
        tags_file.to_str().unwrap(),
        "tests/ctags/test.c",
    ]);

    assert!(success, "ctags -a failed: {}", stderr);

    let tags_content = fs::read_to_string(&tags_file).unwrap();

    // Should keep existing content
    assert!(
        tags_content.contains("existing\tfile.c"),
        "Should preserve existing tags"
    );

    // Should also have new tags
    assert!(
        tags_content.contains("foo\ttests/ctags/test.c"),
        "Should contain new foo tag"
    );
}

#[test]
fn test_ctags_sorted_output() {
    let temp_dir = TempDir::new().unwrap();
    let tags_file = temp_dir.path().join("tags");

    let (_, _, success) = run_ctags(&["-f", tags_file.to_str().unwrap(), "tests/ctags/test.c"]);

    assert!(success);

    let tags_content = fs::read_to_string(&tags_file).unwrap();
    let lines: Vec<&str> = tags_content.lines().collect();

    // Check that tags are sorted alphabetically
    let tag_names: Vec<&str> = lines
        .iter()
        .filter_map(|line| line.split('\t').next())
        .collect();

    let mut sorted_names = tag_names.clone();
    sorted_names.sort();

    assert_eq!(
        tag_names, sorted_names,
        "Tags should be sorted alphabetically"
    );
}
