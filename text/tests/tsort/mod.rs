//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};
use std::collections::HashMap;

/// Run tsort with given input and return stdout
fn run_tsort(input: &str) -> String {
    let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_tsort"))
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("failed to spawn tsort");

    use std::io::Write;
    {
        let stdin = child.stdin.as_mut().unwrap();
        stdin.write_all(input.as_bytes()).unwrap();
    }

    let output = child.wait_with_output().expect("failed to wait on tsort");
    String::from_utf8_lossy(&output.stdout).to_string()
}

/// Verify that output satisfies all dependencies (a before b for each "a b" pair)
fn verify_topological_order(output: &str, input: &str) -> bool {
    // Parse output into a list of nodes
    let output_nodes: Vec<&str> = output.lines().collect();

    // Build position map
    let mut positions: HashMap<&str, usize> = HashMap::new();
    for (i, node) in output_nodes.iter().enumerate() {
        positions.insert(*node, i);
    }

    // Parse input pairs and verify each dependency
    let tokens: Vec<&str> = input.split_whitespace().collect();
    for pair in tokens.chunks(2) {
        if pair.len() == 2 && pair[0] != pair[1] {
            // This is an ordering constraint: pair[0] before pair[1]
            let from = pair[0];
            let to = pair[1];

            // Both must be in output
            let from_pos = match positions.get(from) {
                Some(&p) => p,
                None => return false, // Missing node
            };
            let to_pos = match positions.get(to) {
                Some(&p) => p,
                None => return false, // Missing node
            };

            // from must come before to
            if from_pos >= to_pos {
                return false;
            }
        }
    }

    true
}

/// Run tsort and expect success with valid topological order
fn tsort_test(input: &str) {
    let output = run_tsort(input);
    assert!(
        verify_topological_order(&output, input),
        "Output is not a valid topological order.\nInput:\n{}\nOutput:\n{}",
        input,
        output
    );
}

/// Run tsort and verify specific nodes appear in output
fn tsort_test_contains_all(input: &str, expected_nodes: &[&str]) {
    let output = run_tsort(input);
    let output_nodes: Vec<&str> = output.lines().collect();

    for node in expected_nodes {
        assert!(
            output_nodes.contains(node),
            "Expected node '{}' not found in output.\nOutput: {:?}",
            node,
            output_nodes
        );
    }
}

// === Basic functionality tests ===

#[test]
fn test_tsort_posix_example() {
    // POSIX example from specification
    let input = "a b c c d e\ng g\nf g e f\nh h\n";
    tsort_test(input);
    tsort_test_contains_all(input, &["a", "b", "c", "d", "e", "f", "g", "h"]);
}

#[test]
fn test_tsort_simple_chain() {
    // a -> b -> c -> d
    let input = "a b\nb c\nc d\n";
    tsort_test(input);
    tsort_test_contains_all(input, &["a", "b", "c", "d"]);
}

#[test]
fn test_tsort_self_pairs() {
    // Self-pairs indicate presence only, no ordering
    let input = "a a\nb b\nc c\n";
    let output = run_tsort(input);
    let nodes: Vec<&str> = output.lines().collect();
    assert_eq!(nodes.len(), 3);
    assert!(nodes.contains(&"a"));
    assert!(nodes.contains(&"b"));
    assert!(nodes.contains(&"c"));
}

#[test]
fn test_tsort_empty_input() {
    run_test(TestPlan {
        cmd: String::from("tsort"),
        args: vec![],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

#[test]
fn test_tsort_single_pair() {
    let input = "a b\n";
    tsort_test(input);
    tsort_test_contains_all(input, &["a", "b"]);
}

#[test]
fn test_tsort_duplicate_pairs() {
    // Duplicate edges should be handled correctly
    let input = "a b\na b\na b\n";
    tsort_test(input);
    tsort_test_contains_all(input, &["a", "b"]);
}

#[test]
fn test_tsort_multiple_roots() {
    // Multiple nodes with no predecessors
    let input = "a c\nb c\n";
    tsort_test(input);
    tsort_test_contains_all(input, &["a", "b", "c"]);
}

#[test]
fn test_tsort_diamond() {
    // Diamond dependency: a -> b, a -> c, b -> d, c -> d
    let input = "a b\na c\nb d\nc d\n";
    tsort_test(input);
    tsort_test_contains_all(input, &["a", "b", "c", "d"]);
}

#[test]
fn test_tsort_whitespace_handling() {
    // Multiple spaces between tokens
    let input = "a    b\nc   d\n";
    tsort_test(input);
    tsort_test_contains_all(input, &["a", "b", "c", "d"]);
}

#[test]
fn test_tsort_tabs() {
    // Tabs as separators
    let input = "a\tb\nc\td\n";
    tsort_test(input);
    tsort_test_contains_all(input, &["a", "b", "c", "d"]);
}

#[test]
fn test_tsort_mixed_whitespace() {
    // Mixed spaces and tabs
    let input = "a \t b\n c\t \td\n";
    tsort_test(input);
    tsort_test_contains_all(input, &["a", "b", "c", "d"]);
}

#[test]
fn test_tsort_no_newline_at_end() {
    // Input without trailing newline
    let input = "a b";
    tsort_test(input);
    tsort_test_contains_all(input, &["a", "b"]);
}

#[test]
fn test_tsort_multiple_lines_single_pair() {
    // Pair split across lines (tokens on separate lines)
    let input = "a\nb\n";
    tsort_test(input);
    tsort_test_contains_all(input, &["a", "b"]);
}

// === Cycle detection tests ===

#[test]
fn test_tsort_cycle_two_nodes() {
    // Simple cycle: a -> b -> a
    let input = "a b\nb a\n";
    let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_tsort"))
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("failed to spawn tsort");

    use std::io::Write;
    {
        let stdin = child.stdin.as_mut().unwrap();
        stdin.write_all(input.as_bytes()).unwrap();
    }

    let output = child.wait_with_output().expect("failed to wait on tsort");
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Exit code should be 0 (matching macOS behavior)
    assert_eq!(output.status.code(), Some(0));

    // Should report cycle to stderr
    assert!(stderr.contains("cycle"), "Expected cycle message in stderr");

    // Both nodes should appear in output (stdout)
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("a") && stdout.contains("b"));
}

#[test]
fn test_tsort_cycle_three_nodes() {
    // Cycle: a -> b -> c -> a
    let input = "a b\nb c\nc a\n";
    let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_tsort"))
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("failed to spawn tsort");

    use std::io::Write;
    {
        let stdin = child.stdin.as_mut().unwrap();
        stdin.write_all(input.as_bytes()).unwrap();
    }

    let output = child.wait_with_output().expect("failed to wait on tsort");
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert_eq!(output.status.code(), Some(0));
    assert!(stderr.contains("cycle"));
    assert!(stdout.contains("a") && stdout.contains("b") && stdout.contains("c"));
}

#[test]
fn test_tsort_partial_cycle() {
    // Some nodes in cycle, some not: x -> y (no cycle), a -> b -> a (cycle)
    let input = "x y\na b\nb a\n";
    let mut child = std::process::Command::new(env!("CARGO_BIN_EXE_tsort"))
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("failed to spawn tsort");

    use std::io::Write;
    {
        let stdin = child.stdin.as_mut().unwrap();
        stdin.write_all(input.as_bytes()).unwrap();
    }

    let output = child.wait_with_output().expect("failed to wait on tsort");
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert_eq!(output.status.code(), Some(0));
    assert!(stderr.contains("cycle"));

    // All nodes should appear in output
    assert!(stdout.contains("x"));
    assert!(stdout.contains("y"));
    assert!(stdout.contains("a"));
    assert!(stdout.contains("b"));

    // x should come before y (non-cycle dependency)
    let lines: Vec<&str> = stdout.lines().collect();
    let x_pos = lines.iter().position(|&n| n == "x").unwrap();
    let y_pos = lines.iter().position(|&n| n == "y").unwrap();
    assert!(x_pos < y_pos, "x should come before y");
}

// === Error handling tests ===

#[test]
fn test_tsort_nonexistent_file() {
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_tsort"))
        .arg("/nonexistent/path/to/file")
        .output()
        .expect("failed to run tsort");

    assert_ne!(output.status.code(), Some(0));
}
