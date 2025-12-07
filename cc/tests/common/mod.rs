//
// Copyright (c) 2024 Jeff Garzik
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
use std::path::PathBuf;
use std::process::Command;
use tempfile::NamedTempFile;

/// Get path to a test input file in a specific test directory
pub fn get_test_file_path(test_dir: &str, filename: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push(test_dir);
    path.push(filename);
    path
}

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

/// Compile a C file using pcc and return the path to the executable
/// The executable is placed in temp dir and must be cleaned up by caller
pub fn compile(c_file: &PathBuf) -> Option<PathBuf> {
    let exe_path = std::env::temp_dir().join(format!(
        "pcc_exe_{}",
        c_file.file_stem().unwrap().to_string_lossy()
    ));

    let output = run_test_base(
        "pcc",
        &vec![
            "-o".to_string(),
            exe_path.to_string_lossy().to_string(),
            c_file.to_string_lossy().to_string(),
        ],
        &[],
    );

    if output.status.success() {
        Some(exe_path)
    } else {
        eprintln!("pcc failed: {}", String::from_utf8_lossy(&output.stderr));
        None
    }
}

/// Compile a C file from a specific test directory
pub fn compile_test_file(test_dir: &str, filename: &str) -> Option<PathBuf> {
    let c_file = get_test_file_path(test_dir, filename);
    compile(&c_file)
}

/// Run an executable and return its exit code
pub fn run(exe: &PathBuf) -> i32 {
    let output = Command::new(exe)
        .output()
        .expect("failed to run executable");
    output.status.code().unwrap_or(-1)
}

/// Run an executable and return (exit_code, stdout)
pub fn run_with_output(exe: &PathBuf) -> (i32, String) {
    let output = Command::new(exe)
        .output()
        .expect("failed to run executable");
    (
        output.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&output.stdout).to_string(),
    )
}

/// Clean up an executable file
pub fn cleanup_exe(exe_file: &Option<PathBuf>) {
    if let Some(exe) = exe_file {
        let _ = std::fs::remove_file(exe);
    }
}

/// Compile inline C code and run, returning exit code
/// Temp files are cleaned up automatically via tempfile
pub fn compile_and_run(name: &str, content: &str) -> i32 {
    compile_and_run_with_opts(name, content, &[])
}

/// Compile inline C code with optimization and run, returning exit code
pub fn compile_and_run_optimized(name: &str, content: &str) -> i32 {
    compile_and_run_with_opts(name, content, &["-O1".to_string()])
}

/// Compile inline C code with extra options and run, returning exit code
/// Temp files are cleaned up automatically via tempfile
pub fn compile_and_run_with_opts(name: &str, content: &str, extra_opts: &[String]) -> i32 {
    let c_file = create_c_file(name, content);
    let c_path = c_file.path().to_path_buf();

    let exe_path = std::env::temp_dir().join(format!("pcc_exe_{}", name));

    let mut args = vec!["-o".to_string(), exe_path.to_string_lossy().to_string()];
    args.extend(extra_opts.iter().cloned());
    args.push(c_path.to_string_lossy().to_string());

    let output = run_test_base("pcc", &args, &[]);

    if !output.status.success() {
        eprintln!(
            "pcc compilation failed for {}:\n{}",
            name,
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
