//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Codegen Misc Mega-Test
//
// Consolidates: optimization, debug tests
//

use crate::common::{compile_and_run_optimized, create_c_file};
use plib::testing::run_test_base;
use std::io::Write;
use std::process::Command;

// ============================================================================
// Mega-test: Optimization correctness
// ============================================================================

#[test]
fn codegen_optimization_mega() {
    let code = r#"
int counter = 0;

int side_effect(int x) {
    counter++;
    return x * 2;
}

int global_var = 0;

int main(void) {
    // ========== BASIC ARITHMETIC (returns 1-9) ==========
    {
        int a = 2 + 3;
        int b = 10 - 10;
        int c = 4 * 1;
        int d = 0 + 7;
        if (a != 5) return 1;
        if (b != 0) return 2;
        if (c != 4) return 3;
        if (d != 7) return 4;
    }

    // ========== ALGEBRAIC SIMPLIFICATIONS (returns 10-19) ==========
    {
        int x = 42;
        int y = x + 0;
        int z = x * 1;
        int w = x - 0;
        if (y != 42) return 10;
        if (z != 42) return 11;
        if (w != 42) return 12;
    }

    // ========== IDENTITY PATTERNS (returns 20-29) ==========
    {
        int x = 42;
        int r = x & x;
        int s = x | x;
        if (r != 42) return 20;
        if (s != 42) return 21;
    }

    // ========== BITWISE WITH ZERO (returns 30-39) ==========
    {
        int x = 42;
        int u = x | 0;
        int v = x ^ 0;
        if (u != 42) return 30;
        if (v != 42) return 31;
    }

    // ========== SHIFTS BY ZERO (returns 40-49) ==========
    {
        int x = 42;
        int sh1 = x << 0;
        int sh2 = x >> 0;
        if (sh1 != 42) return 40;
        if (sh2 != 42) return 41;
    }

    // ========== COMPARISONS (returns 50-59) ==========
    {
        int cmp = 5;
        int cmp2 = 10;
        if (cmp != cmp) return 50;
        if (cmp == cmp2) return 51;
        if (!(cmp < cmp2)) return 52;
        if (!(cmp2 > cmp)) return 53;
        if (cmp > cmp2) return 54;
        if (cmp2 < cmp) return 55;
    }

    // ========== LOOPS WITH DEAD CODE (returns 60-69) ==========
    {
        int sum = 0;
        for (int i = 0; i < 10; i++) {
            int live = i + 1;
            sum += live;
        }
        if (sum != 55) return 60;

        int count = 0;
        int j = 5;
        while (j > 0) {
            int unused = j + 0;
            count++;
            j--;
        }
        if (count != 5) return 61;
    }

    // ========== SIDE EFFECTS (returns 70-79) ==========
    {
        int unused = side_effect(5);
        int used = side_effect(10);
        if (used != 20) return 70;
        if (counter != 2) return 71;
    }

    // ========== GLOBAL STORES (returns 80-89) ==========
    {
        global_var = 42;
        int local = 10;
        local = local + 5;
        if (global_var != 42) return 80;
        if (local != 15) return 81;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run_optimized("opt_mega", code), 0);
}

// ============================================================================
// Mega-test: Debug info generation
// ============================================================================

#[test]
fn codegen_debug_sections() {
    let c_file = create_c_file(
        "debug_test",
        r#"
int foo(int x) {
    return x + 1;
}

int main() {
    return foo(41);
}
"#,
    );
    let c_path = c_file.path().to_path_buf();
    let obj_path = std::env::temp_dir().join("pcc_debug_mega_test.o");

    // Compile with -g -c to produce object file
    let output = run_test_base(
        "pcc",
        &vec![
            "-g".to_string(),
            "-c".to_string(),
            "-o".to_string(),
            obj_path.to_string_lossy().to_string(),
            c_path.to_string_lossy().to_string(),
        ],
        &[],
    );

    assert!(
        output.status.success(),
        "pcc -g -c failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Check for debug sections using platform-specific tools
    #[cfg(target_os = "macos")]
    {
        let otool_output = Command::new("otool")
            .args(["-l", obj_path.to_str().unwrap()])
            .output()
            .expect("failed to run otool");

        let otool_stdout = String::from_utf8_lossy(&otool_output.stdout);
        assert!(
            otool_stdout.contains("__debug_line") || otool_stdout.contains("__DWARF"),
            "No debug sections found in object file"
        );
    }

    #[cfg(target_os = "linux")]
    {
        let objdump_output = Command::new("objdump")
            .args(["-h", obj_path.to_str().unwrap()])
            .output()
            .expect("failed to run objdump");

        let objdump_stdout = String::from_utf8_lossy(&objdump_output.stdout);
        assert!(
            objdump_stdout.contains(".debug_line") || objdump_stdout.contains(".debug_info"),
            "No debug sections found in object file"
        );
    }

    let _ = std::fs::remove_file(&obj_path);
}

#[test]
fn codegen_cfi_directives() {
    let c_file = create_c_file(
        "cfi_test",
        r#"
int main() {
    return 42;
}
"#,
    );
    let c_path = c_file.path().to_path_buf();

    // Test default CFI directives
    let output = run_test_base(
        "pcc",
        &vec![
            "-S".to_string(),
            "-o".to_string(),
            "-".to_string(),
            c_path.to_string_lossy().to_string(),
        ],
        &[],
    );

    assert!(
        output.status.success(),
        "pcc -S -o - failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let asm = String::from_utf8_lossy(&output.stdout);
    assert!(
        asm.contains(".cfi_startproc"),
        "Missing .cfi_startproc in assembly output"
    );
    assert!(
        asm.contains(".cfi_endproc"),
        "Missing .cfi_endproc in assembly output"
    );
    // Detailed CFI should NOT be present without -g
    assert!(
        !asm.contains(".cfi_def_cfa"),
        "Unexpected .cfi_def_cfa in assembly output without -g"
    );
}

#[test]
fn codegen_cfi_disabled() {
    let c_file = create_c_file(
        "no_cfi_test",
        r#"
int main() {
    return 42;
}
"#,
    );
    let c_path = c_file.path().to_path_buf();

    let output = run_test_base(
        "pcc",
        &vec![
            "-S".to_string(),
            "-o".to_string(),
            "-".to_string(),
            "--fno-unwind-tables".to_string(),
            c_path.to_string_lossy().to_string(),
        ],
        &[],
    );

    assert!(
        output.status.success(),
        "pcc -S -o - --fno-unwind-tables failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let asm = String::from_utf8_lossy(&output.stdout);
    assert!(
        !asm.contains(".cfi_startproc"),
        "Unexpected .cfi_startproc with --fno-unwind-tables"
    );
    assert!(
        !asm.contains(".cfi_endproc"),
        "Unexpected .cfi_endproc with --fno-unwind-tables"
    );
}

#[test]
fn codegen_debug_file_loc() {
    let c_file = create_c_file(
        "file_loc_test",
        r#"
int main() {
    return 42;
}
"#,
    );
    let c_path = c_file.path().to_path_buf();

    // With -g
    let output = run_test_base(
        "pcc",
        &vec![
            "-g".to_string(),
            "-S".to_string(),
            "-o".to_string(),
            "-".to_string(),
            c_path.to_string_lossy().to_string(),
        ],
        &[],
    );

    assert!(
        output.status.success(),
        "pcc -g -S -o - failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let asm = String::from_utf8_lossy(&output.stdout);
    assert!(asm.contains(".file 1"), "Missing .file directive with -g");
    assert!(asm.contains(".loc 1"), "Missing .loc directive with -g");
    assert!(asm.contains(".cfi_def_cfa"), "Missing .cfi_def_cfa with -g");
}

#[test]
fn codegen_no_debug_no_loc() {
    let c_file = create_c_file(
        "no_debug_test",
        r#"
int main() {
    return 42;
}
"#,
    );
    let c_path = c_file.path().to_path_buf();

    let output = run_test_base(
        "pcc",
        &vec![
            "-S".to_string(),
            "-o".to_string(),
            "-".to_string(),
            c_path.to_string_lossy().to_string(),
        ],
        &[],
    );

    assert!(
        output.status.success(),
        "pcc -S -o - failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let asm = String::from_utf8_lossy(&output.stdout);
    // .file is emitted unconditionally
    assert!(
        asm.contains(".file 1"),
        "Missing .file directive (should be unconditional)"
    );
    // .loc should NOT be present without -g
    assert!(
        !asm.contains(".loc 1"),
        "Unexpected .loc directive without -g"
    );
}

// ============================================================================
// Assembly file compilation tests (.s and .S)
// ============================================================================

/// Create a temporary assembly file
fn create_asm_file(name: &str, content: &str, extension: &str) -> tempfile::NamedTempFile {
    let mut file = tempfile::Builder::new()
        .prefix(&format!("pcc_test_{}_", name))
        .suffix(extension)
        .tempfile()
        .expect("failed to create temp file");
    file.write_all(content.as_bytes())
        .expect("failed to write test file");
    file
}

#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_asm_file_support() {
    // Test .s file (pure assembly) and .S file (assembly with preprocessor)
    // Tests compile-only (-c) mode which fully works. Mixing C + asm in one
    // invocation requires passing asm objects to C file's link step (future work).

    // Assembly function that returns 42 (x86_64)
    // Note: macOS Mach-O uses underscore prefix and has no .type/.size directives
    #[cfg(target_os = "macos")]
    let asm_content = r#"
    .text
    .globl _get_value
_get_value:
    movl $42, %eax
    ret
"#;

    #[cfg(not(target_os = "macos"))]
    let asm_content = r#"
    .text
    .globl get_value
    .type get_value, @function
get_value:
    movl $42, %eax
    ret
    .size get_value, .-get_value
"#;

    // Assembly with preprocessor directives (.S) (x86_64)
    #[cfg(target_os = "macos")]
    let asm_s_content = r#"
#define RETURN_VALUE 99
    .text
    .globl _get_value_s
_get_value_s:
    movl $RETURN_VALUE, %eax
    ret
"#;

    #[cfg(not(target_os = "macos"))]
    let asm_s_content = r#"
#define RETURN_VALUE 99
    .text
    .globl get_value_s
    .type get_value_s, @function
get_value_s:
    movl $RETURN_VALUE, %eax
    ret
    .size get_value_s, .-get_value_s
"#;

    // C main that calls both functions
    let c_content = r#"
extern int get_value(void);
extern int get_value_s(void);

int main(void) {
    if (get_value() != 42) return 1;
    if (get_value_s() != 99) return 2;
    return 0;
}
"#;

    let asm_file = create_asm_file("asm_test", asm_content, ".s");
    let asm_s_file = create_asm_file("asm_s_test", asm_s_content, ".S");
    let c_file = create_c_file("asm_main", c_content);

    let obj_s = std::env::temp_dir().join(format!("pcc_asm_{}.o", std::process::id()));
    let obj_s_upper = std::env::temp_dir().join(format!("pcc_asm_s_{}.o", std::process::id()));
    let obj_c = std::env::temp_dir().join(format!("pcc_asm_c_{}.o", std::process::id()));
    let exe_path = std::env::temp_dir().join(format!("pcc_asm_test_{}", std::process::id()));

    // Step 1: Compile .s to .o
    let output = run_test_base(
        "pcc",
        &vec![
            "-c".to_string(),
            "-o".to_string(),
            obj_s.to_string_lossy().to_string(),
            asm_file.path().to_string_lossy().to_string(),
        ],
        &[],
    );
    assert!(
        output.status.success(),
        "pcc -c .s failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Step 2: Compile .S to .o (with preprocessing)
    let output = run_test_base(
        "pcc",
        &vec![
            "-c".to_string(),
            "-o".to_string(),
            obj_s_upper.to_string_lossy().to_string(),
            asm_s_file.path().to_string_lossy().to_string(),
        ],
        &[],
    );
    assert!(
        output.status.success(),
        "pcc -c .S failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Step 3: Compile C to .o
    let output = run_test_base(
        "pcc",
        &vec![
            "-c".to_string(),
            "-o".to_string(),
            obj_c.to_string_lossy().to_string(),
            c_file.path().to_string_lossy().to_string(),
        ],
        &[],
    );
    assert!(
        output.status.success(),
        "pcc -c .c failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Step 4: Link all .o files
    let output = run_test_base(
        "pcc",
        &vec![
            "-o".to_string(),
            exe_path.to_string_lossy().to_string(),
            obj_c.to_string_lossy().to_string(),
            obj_s.to_string_lossy().to_string(),
            obj_s_upper.to_string_lossy().to_string(),
        ],
        &[],
    );
    assert!(
        output.status.success(),
        "pcc link failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Run the executable
    let run_output = Command::new(&exe_path)
        .output()
        .expect("failed to run executable");

    let exit_code = run_output.status.code().unwrap_or(-1);

    // Cleanup
    let _ = std::fs::remove_file(&obj_s);
    let _ = std::fs::remove_file(&obj_s_upper);
    let _ = std::fs::remove_file(&obj_c);
    let _ = std::fs::remove_file(&exe_path);

    assert_eq!(
        exit_code, 0,
        "Assembly file test failed with exit code {}",
        exit_code
    );
}
