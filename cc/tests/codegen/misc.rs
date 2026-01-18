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
