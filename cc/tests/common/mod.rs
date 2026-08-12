//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Common test utilities for c17 integration tests
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
        .prefix(&format!("c17_test_{}_", name))
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
        "c17_exe_{}_{}_{}",
        name,
        config_name,
        thread_id.replace(|c: char| !c.is_alphanumeric(), "_")
    ));

    // The source operand goes before `extra_opts`, because those may contain
    // `-l` and a library is searched where its name is encountered — naming it
    // ahead of the object that references it resolves nothing. This is the
    // ordinary `c17 prog.c -lm` shape.
    let mut args = vec!["-o".to_string(), exe_path.to_string_lossy().to_string()];
    args.push(c_path.to_string_lossy().to_string());
    args.extend(extra_opts.iter().cloned());

    let output = run_test_base("c17", &args, &[]);

    if !output.status.success() {
        eprintln!(
            "c17 compilation failed for {} [config: {}]:\n{}",
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

    // On failure, dump generated assembly for diagnosis
    if exit_code != 0 {
        let asm_path = std::env::temp_dir().join(format!("c17_asm_{}_{}.s", name, config_name));
        let mut asm_args = vec![
            "-S".to_string(),
            "-o".to_string(),
            asm_path.to_string_lossy().to_string(),
        ];
        asm_args.push(c_path.to_string_lossy().to_string());
        asm_args.extend(extra_opts.iter().cloned());
        let asm_output = run_test_base("c17", &asm_args, &[]);
        if asm_output.status.success() {
            if let Ok(asm) = std::fs::read_to_string(&asm_path) {
                eprintln!(
                    "=== Generated assembly for '{}' [{}] ===",
                    name, config_name
                );
                eprintln!("{}", asm);
                eprintln!("=== End assembly ===");
            }
        }
        let _ = std::fs::remove_file(&asm_path);
    }

    // Cleanup exe (c_file auto-cleaned by NamedTempFile drop)
    let _ = std::fs::remove_file(&exe_path);

    exit_code
}

/// Compile two translation units together, link them, and run the result.
///
/// Every other helper here compiles a single file, which cannot see the class
/// of defect that only appears at link time: a definition emitted into two
/// objects that should have been emitted into neither. `inline` in a shared
/// header is the ordinary way to hit that.
///
/// Returns the program's exit status, or -1 if compiling or linking failed
/// (with the toolchain's own message on stderr, since a duplicate-symbol error
/// is the interesting outcome and is worth reading).
pub fn compile_and_run_two_units(
    name: &str,
    unit_a: &str,
    unit_b: &str,
    extra_opts: &[String],
) -> i32 {
    let a = create_c_file(&format!("{}_a", name), unit_a);
    let b = create_c_file(&format!("{}_b", name), unit_b);

    let thread_id = format!("{:?}", std::thread::current().id());
    let exe_path = std::env::temp_dir().join(format!(
        "c17_exe2_{}_{}",
        name,
        thread_id.replace(|c: char| !c.is_alphanumeric(), "_")
    ));

    let mut args = vec!["-o".to_string(), exe_path.to_string_lossy().to_string()];
    args.push(a.path().to_string_lossy().to_string());
    args.push(b.path().to_string_lossy().to_string());
    args.extend(extra_opts.iter().cloned());

    let output = run_test_base("c17", &args, &[]);
    if !output.status.success() {
        eprintln!(
            "c17 failed to build {} from two units:\n{}",
            name,
            String::from_utf8_lossy(&output.stderr)
        );
        return -1;
    }

    let run_output = Command::new(&exe_path)
        .output()
        .expect("failed to run executable");
    let exit_code = run_output.status.code().unwrap_or(-1);
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

// ============================================================================
// Arbitrary-argv driver invocation
// ============================================================================

/// The result of invoking `c17` with an arbitrary argument vector.
pub struct C17Run {
    pub stdout: String,
    pub stderr: String,
    pub success: bool,
}

/// Invoke `c17` with exactly `args`.
///
/// `compile_and_run` always builds one source with one `-o`, which cannot
/// express the multi-operand and option-ordering cases the POSIX spec's
/// EXAMPLE 1 and EXAMPLE 3 describe. Driver tests need the raw vector.
pub fn run_c17(args: &[&str]) -> C17Run {
    let owned: Vec<String> = args.iter().map(|s| s.to_string()).collect();
    let output = run_test_base("c17", &owned, &[]);
    C17Run {
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        success: output.status.success(),
    }
}

/// Compile `content` and require it to be **rejected** with a diagnostic
/// containing `expected`.
///
/// Nothing could express this before: `compile_and_run` collapses a compile
/// failure into the sentinel `-1`, indistinguishable from a program that exits
/// `-1`, and discards stderr to the test log. So the suites proved that
/// accepted programs run correctly, never that invalid programs are diagnosed
/// — which is how a dozen missing constraint checks went unnoticed.
///
/// Compiles with `-S` to a scratch file: that path runs the whole front end and
/// passes both error checkpoints (after parsing and after linearization), while
/// `--dump-ast` returns before linearization and would miss anything the
/// linearizer diagnoses. There is no `-fsyntax-only`.
pub fn compile_expect_error(name: &str, content: &str, expected: &str) {
    let c_file = create_c_file(name, content);
    let asm = tempfile::Builder::new()
        .prefix(&format!("c17_reject_{}_", name))
        .suffix(".s")
        .tempfile()
        .expect("failed to create temp file");

    let args = vec![
        "-S".to_string(),
        "-o".to_string(),
        asm.path().to_string_lossy().to_string(),
        c_file.path().to_string_lossy().to_string(),
    ];
    let output = run_test_base("c17", &args, &[]);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        !output.status.success(),
        "'{}' should have been rejected but compiled cleanly.\nSource:\n{}\nstderr:\n{}",
        name,
        content,
        stderr
    );
    assert!(
        stderr.contains(expected),
        "'{}' was rejected, but no diagnostic mentioned {:?}.\nstderr:\n{}",
        name,
        expected,
        stderr
    );
}

/// Compile `content` and require it to be **accepted**.
///
/// The companion to `compile_expect_error`: every new constraint check needs a
/// case proving it does not fire on legal code.
pub fn compile_expect_ok(name: &str, content: &str) {
    let c_file = create_c_file(name, content);
    let asm = tempfile::Builder::new()
        .prefix(&format!("c17_accept_{}_", name))
        .suffix(".s")
        .tempfile()
        .expect("failed to create temp file");

    let args = vec![
        "-S".to_string(),
        "-o".to_string(),
        asm.path().to_string_lossy().to_string(),
        c_file.path().to_string_lossy().to_string(),
    ];
    let output = run_test_base("c17", &args, &[]);
    assert!(
        output.status.success(),
        "'{}' should have compiled, but was rejected.\nSource:\n{}\nstderr:\n{}",
        name,
        content,
        String::from_utf8_lossy(&output.stderr)
    );
}

/// Preprocess `content` with `-E` and return the run.
///
/// Every other preprocessor test asserts on the exit code of a compiled
/// program, which cannot see spacing, stringification, or which branch of a
/// `#if` survived. This looks at the text.
pub fn preprocess_text(name: &str, content: &str, extra_opts: &[&str]) -> C17Run {
    let c_file = create_c_file(name, content);
    let mut args = vec!["-E".to_string()];
    args.extend(extra_opts.iter().map(|s| s.to_string()));
    args.push(c_file.path().to_string_lossy().to_string());

    let output = run_test_base("c17", &args, &[]);
    C17Run {
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        success: output.status.success(),
    }
}
