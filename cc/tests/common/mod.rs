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

    // `-1` is also what a failed build returns, so say which happened and, if
    // the program died, on what signal. Without this a segfault and a compiler
    // error are the same number, and on a target that cannot be run locally
    // that is the difference between a diagnosis and a guess.
    report_abnormal_exit(name, config_name, &run_output);

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
    report_abnormal_exit(name, "single", &run_output);
    let _ = std::fs::remove_file(&exe_path);
    exit_code
}

/// Build `lib_src` into a shared object with c17, then run `main_src` (also
/// built with c17) which is expected to `dlopen` it.
///
/// The library is written to a private directory and always named `lib.so`, so
/// `main_src` can hardcode `"./lib.so"` and the test can `chdir`-free by
/// running the executable with that directory as its working directory.
///
/// This exists for the class of defect that only appears at load time. A
/// thread-local model that is wrong for a dynamically loaded library links
/// perfectly and then fails in `dlopen`, and does so only once the library's
/// TLS block outgrows the loader's static-TLS surplus -- so neither compiling
/// nor linking nor a small test case reveals it.
///
/// Returns the program's exit status, or -1 if any build step failed. `dlopen`
/// diagnostics are on stdout/stderr and are echoed on failure, since the
/// loader's message is the interesting part.
pub fn compile_and_dlopen(name: &str, lib_src: &str, main_src: &str, extra_opts: &[String]) -> i32 {
    let dir = tempfile::Builder::new()
        .prefix(&format!("c17_dl_{}_", name))
        .tempdir()
        .expect("failed to create work dir");

    let lib_c = dir.path().join("lib.c");
    let main_c = dir.path().join("main.c");
    std::fs::write(&lib_c, lib_src).expect("failed to write library source");
    std::fs::write(&main_c, main_src).expect("failed to write driver source");
    let so = dir.path().join("lib.so");
    let exe = dir.path().join("driver");

    let mut lib_args = vec![
        "-fPIC".to_string(),
        "--shared".to_string(),
        "-o".to_string(),
        so.to_string_lossy().into_owned(),
        lib_c.to_string_lossy().into_owned(),
    ];
    lib_args.extend(extra_opts.iter().cloned());
    let r = run_test_base("c17", &lib_args, &[]);
    if !r.status.success() {
        eprintln!(
            "c17 failed to build the shared object for {}:\n{}",
            name,
            String::from_utf8_lossy(&r.stderr)
        );
        return -1;
    }

    let main_args = vec![
        "-o".to_string(),
        exe.to_string_lossy().into_owned(),
        main_c.to_string_lossy().into_owned(),
        "-ldl".to_string(),
    ];
    let r = run_test_base("c17", &main_args, &[]);
    if !r.status.success() {
        eprintln!(
            "c17 failed to build the dlopen driver for {}:\n{}",
            name,
            String::from_utf8_lossy(&r.stderr)
        );
        return -1;
    }

    let out = Command::new(&exe)
        .current_dir(dir.path())
        .output()
        .expect("failed to run dlopen driver");
    let code = out.status.code().unwrap_or(-1);
    if code != 0 {
        eprintln!(
            "dlopen driver for {} exited {}:\n{}{}",
            name,
            code,
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
    }
    code
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

/// Print why a test program produced no exit code.
///
/// A program killed by a signal has `code() == None`, which the callers turn
/// into `-1` -- the same value a failed build returns. Reporting the signal,
/// and whatever the program managed to write first, is what tells a segfault
/// apart from a compiler error when the target cannot be run locally.
fn report_abnormal_exit(name: &str, config_name: &str, out: &std::process::Output) {
    if out.status.code().is_some() {
        return;
    }
    #[cfg(unix)]
    let detail = {
        use std::os::unix::process::ExitStatusExt;
        match out.status.signal() {
            Some(sig) => format!("killed by signal {sig}"),
            None => "terminated without an exit code".to_string(),
        }
    };
    #[cfg(not(unix))]
    let detail = "terminated without an exit code".to_string();

    eprintln!(
        "c17 test {name} [config: {config_name}] {detail}\n\
         --- program stdout ---\n{}\n--- program stderr ---\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
}
