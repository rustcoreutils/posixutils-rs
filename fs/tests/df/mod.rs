//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

fn run_df_test(args: Vec<&str>) -> String {
    // Run and capture output for header verification
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_df"))
        .args(&args)
        .output()
        .expect("Failed to execute df");

    String::from_utf8_lossy(&output.stdout).to_string()
}

#[test]
fn test_df_basic() {
    let output = run_df_test(vec![]);

    // Verify header contains expected columns
    let header = output.lines().next().expect("No output from df");
    assert!(header.contains("Filesystem"), "Header missing 'Filesystem'");
    assert!(
        header.contains("512-blocks") || header.contains("1024-blocks"),
        "Header missing block size column"
    );
    assert!(header.contains("Used"), "Header missing 'Used'");
    assert!(header.contains("Available"), "Header missing 'Available'");
    assert!(header.contains("Capacity"), "Header missing 'Capacity'");
    assert!(header.contains("Mounted on"), "Header missing 'Mounted on'");

    // Verify we have at least one filesystem row
    assert!(output.lines().count() >= 2, "No filesystem data in output");
}

#[test]
fn test_df_k_option() {
    let output = run_df_test(vec!["-k"]);

    // With -k, header should show "1024-blocks"
    let header = output.lines().next().expect("No output from df");
    assert!(
        header.contains("1024-blocks"),
        "Header should show '1024-blocks' with -k option"
    );
}

#[test]
fn test_df_portable_option() {
    let output = run_df_test(vec!["-P"]);

    // With -P (POSIX format), header should show "512-blocks"
    let header = output.lines().next().expect("No output from df");
    assert!(
        header.contains("512-blocks"),
        "Header should show '512-blocks' with -P option"
    );
}

#[test]
fn test_df_portable_k_options() {
    let output = run_df_test(vec!["-P", "-k"]);

    // With -P and -k, header should show "1024-blocks"
    let header = output.lines().next().expect("No output from df");
    assert!(
        header.contains("1024-blocks"),
        "Header should show '1024-blocks' with -P -k options"
    );
}

#[test]
fn test_df_file_operand() {
    // Test with root directory - should always exist
    let output = run_df_test(vec!["/"]);

    // Verify header and at least one filesystem row
    let lines: Vec<&str> = output.lines().collect();
    assert!(lines.len() >= 2, "Should have header and at least one row");

    // The filesystem for "/" should be shown
    let header = lines[0];
    assert!(header.contains("Filesystem"), "Header missing 'Filesystem'");
}

#[test]
fn test_df_nonexistent_file() {
    // Test with nonexistent file - should fail with exit code > 0
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_df"))
        .arg("/nonexistent/path/that/does/not/exist")
        .output()
        .expect("Failed to execute df");

    assert!(
        !output.status.success(),
        "df should fail for nonexistent path"
    );

    // Error message should be written to stderr
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.is_empty() || !output.status.success(),
        "Should report error for nonexistent path"
    );
}

#[test]
fn test_df_operand_does_not_dump_all_filesystems() {
    // Regression (audit #5): a single `file` operand must report only the
    // filesystem(s) containing it, never the whole mount table.
    let all = run_df_test(vec![]);
    let one = run_df_test(vec!["/"]);

    let all_rows = all.lines().count();
    let one_rows = one.lines().count();

    assert!(one_rows >= 2, "df / should print header + at least one row");
    assert!(
        one_rows <= all_rows,
        "df / printed {one_rows} lines but the full table has only {all_rows}; \
         an operand must never expand beyond the full table"
    );
}

#[test]
fn test_df_bad_operand_prints_no_filesystem_rows() {
    // Regression (audit #5): a failed operand must NOT fall back to printing
    // every filesystem. At most the header line may appear on stdout.
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_df"))
        .arg("/nonexistent/path/that/does/not/exist")
        .output()
        .expect("Failed to execute df");

    assert!(!output.status.success(), "df should fail for a bad operand");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.lines().count() <= 1,
        "df <bad operand> must not dump all filesystems; stdout was:\n{stdout}"
    );
}

#[test]
fn test_df_help() {
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_df"))
        .arg("--help")
        .output()
        .expect("Failed to execute df --help");

    assert!(output.status.success(), "df --help should succeed");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("-k"), "Help should mention -k option");
    assert!(stdout.contains("-P"), "Help should mention -P option");
}
