//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Common test utilities for pcc integration tests
//

use plib::testing::run_test_base;
use std::io::Write;
use std::process::Command;
use tempfile::NamedTempFile;

// ============================================================================
// Compile Matrix Configuration
// ============================================================================

/// Full compile matrix (enabled with --features test_matrix).
/// Tests all 4 configurations: default, debug-only, optimized-only, and both.
#[cfg(feature = "test_matrix")]
pub const COMPILE_MATRIX: &[(&str, &[&str])] = &[
    ("default", &[]),
    ("debug", &["-g"]),
    ("optimized", &["-O"]),
    ("debug_opt", &["-g", "-O"]),
];

/// Default compile config: -O -g only (fastest while still catching optimization bugs).
#[cfg(not(feature = "test_matrix"))]
pub const COMPILE_MATRIX: &[(&str, &[&str])] = &[("debug_opt", &["-g", "-O"])];

// ============================================================================
// File Path Utilities
// ============================================================================

/// Create a temporary C file with the given content
/// Returns NamedTempFile which auto-deletes on drop
pub fn create_c_file(name: &str, content: &str) -> NamedTempFile {
    let mut file = tempfile::Builder::new()
        .prefix(&format!("pcc_test_{}_", name))
        .suffix(".c")
        .tempfile()
        .expect("failed to create temp file");
    file.write_all(content.as_bytes())
        .expect("failed to write test file");
    file
}

// ============================================================================
// Matrix-aware Compile and Run (main API)
// ============================================================================

/// Internal: Compile and run with a single specific configuration (no matrix loop)
fn compile_and_run_single(
    name: &str,
    content: &str,
    extra_opts: &[String],
    config_name: &str,
) -> i32 {
    let c_file = create_c_file(name, content);
    let c_path = c_file.path().to_path_buf();

    // Use thread ID and config name to make exe path unique for parallel test execution
    let thread_id = format!("{:?}", std::thread::current().id());
    let exe_path = std::env::temp_dir().join(format!(
        "pcc_exe_{}_{}_{}",
        name,
        config_name,
        thread_id.replace(|c: char| !c.is_alphanumeric(), "_")
    ));

    let mut args = vec!["-o".to_string(), exe_path.to_string_lossy().to_string()];
    args.extend(extra_opts.iter().cloned());
    args.push(c_path.to_string_lossy().to_string());

    let output = run_test_base("pcc", &args, &[]);

    if !output.status.success() {
        eprintln!(
            "pcc compilation failed for {} [config: {}]:\n{}",
            name,
            config_name,
            String::from_utf8_lossy(&output.stderr)
        );
        return -1;
    }

    let run_output = Command::new(&exe_path)
        .output()
        .expect("failed to run executable");

    let exit_code = run_output.status.code().unwrap_or(-1);

    // Cleanup exe (c_file auto-cleaned by NamedTempFile drop)
    let _ = std::fs::remove_file(&exe_path);

    exit_code
}

/// Compile inline C code and run with all matrix configurations.
/// Returns 0 if all configurations pass, or the first non-zero exit code on failure.
pub fn compile_and_run(name: &str, content: &str, extra_opts: &[String]) -> i32 {
    for (config_name, matrix_flags) in COMPILE_MATRIX {
        // Combine matrix flags with caller's extra options
        let mut combined: Vec<String> = matrix_flags.iter().map(|s| s.to_string()).collect();
        combined.extend(extra_opts.iter().cloned());

        let result = compile_and_run_single(name, content, &combined, config_name);
        if result != 0 {
            eprintln!(
                "Test '{}' FAILED with config '{}': exit code {}",
                name, config_name, result
            );
            return result;
        }
    }
    0
}

/// Compile inline C code with optimization and run (single config, skips matrix).
/// This is used by tests that specifically test optimization behavior.
pub fn compile_and_run_optimized(name: &str, content: &str) -> i32 {
    compile_and_run_single(name, content, &["-O1".to_string()], "optimized_only")
}
