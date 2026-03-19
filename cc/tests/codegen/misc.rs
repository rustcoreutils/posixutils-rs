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

use crate::common::{compile_and_run, compile_and_run_optimized, create_c_file};
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

/// Test that non-PIE code uses direct RIP-relative addressing for globals.
/// With -fno-pie, local globals should use foo(%rip) not GOT.
#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_global_accesses_use_rip_relative() {
    let c_file = create_c_file(
        "global_access_rip",
        r#"
int foo = 3;

int main(void) {
    return foo;
}
"#,
    );
    let c_path = c_file.path().to_path_buf();

    let output = run_test_base(
        "pcc",
        &vec![
            "-fno-pie".to_string(),
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
        asm.contains("foo(%rip)"),
        "expected RIP-relative access in asm output"
    );
    assert!(
        !asm.contains("@GOTPCREL"),
        "unexpected GOTPCREL usage for local globals"
    );
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

/// Test that __ASSEMBLER__ is defined when preprocessing .S files
#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_asm_assembler_macro() {
    // Test that __ASSEMBLER__ is defined in .S files and that
    // #ifdef __ASSEMBLER__ conditional compilation works

    // Assembly with __ASSEMBLER__ conditional
    #[cfg(target_os = "macos")]
    let asm_s_content = r#"
#ifdef __ASSEMBLER__
#define RETURN_VALUE 77
#else
#error "__ASSEMBLER__ should be defined"
#endif
    .text
    .globl _get_asm_value
_get_asm_value:
    movl $RETURN_VALUE, %eax
    ret
"#;

    #[cfg(not(target_os = "macos"))]
    let asm_s_content = r#"
#ifdef __ASSEMBLER__
#define RETURN_VALUE 77
#else
#error "__ASSEMBLER__ should be defined"
#endif
    .text
    .globl get_asm_value
    .type get_asm_value, @function
get_asm_value:
    movl $RETURN_VALUE, %eax
    ret
    .size get_asm_value, .-get_asm_value
"#;

    // C main that calls the asm function
    let c_content = r#"
extern int get_asm_value(void);

int main(void) {
    if (get_asm_value() != 77) return 1;
    return 0;
}
"#;

    let asm_s_file = create_asm_file("asm_assembler_test", asm_s_content, ".S");
    let c_file = create_c_file("asm_assembler_main", c_content);

    let obj_asm = std::env::temp_dir().join(format!("pcc_asm_macro_{}.o", std::process::id()));
    let obj_c = std::env::temp_dir().join(format!("pcc_asm_macro_c_{}.o", std::process::id()));
    let exe_path = std::env::temp_dir().join(format!("pcc_asm_macro_test_{}", std::process::id()));

    // Step 1: Compile .S to .o (with preprocessing, should have __ASSEMBLER__ defined)
    let output = run_test_base(
        "pcc",
        &vec![
            "-c".to_string(),
            "-o".to_string(),
            obj_asm.to_string_lossy().to_string(),
            asm_s_file.path().to_string_lossy().to_string(),
        ],
        &[],
    );
    assert!(
        output.status.success(),
        "pcc -c .S with __ASSEMBLER__ failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Step 2: Compile C to .o
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

    // Step 3: Link
    let output = run_test_base(
        "pcc",
        &vec![
            "-o".to_string(),
            exe_path.to_string_lossy().to_string(),
            obj_c.to_string_lossy().to_string(),
            obj_asm.to_string_lossy().to_string(),
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
    let _ = std::fs::remove_file(&obj_asm);
    let _ = std::fs::remove_file(&obj_c);
    let _ = std::fs::remove_file(&exe_path);

    assert_eq!(
        exit_code, 0,
        "__ASSEMBLER__ macro test failed with exit code {}",
        exit_code
    );
}

// ============================================================================
// Test: Large struct member copy from global variable
// ============================================================================
// Regression test for bug where accessing a large struct member (size > 64 bits)
// from a global variable would generate an extra dereference, treating the
// struct address as a pointer to dereference rather than the struct itself.
#[test]
fn codegen_large_struct_member_copy() {
    let code = r#"
typedef struct {
    void *ctx;
    void *malloc_fn;
    void *calloc_fn;
    void *realloc_fn;
    void *free_fn;
} Allocator;

typedef struct {
    Allocator raw;
    Allocator mem;
    Allocator obj;
} StandardAllocators;

typedef struct {
    int use_hugepages;
    StandardAllocators standard;
} AllAllocators;

typedef struct {
    char padding[928];
    AllAllocators allocators;
} RuntimeState;

RuntimeState _MyRuntime;

#define _MyMem_Raw (_MyRuntime.allocators.standard.raw)

void get_allocator(Allocator *result) {
    *result = _MyMem_Raw;
}

int main(void) {
    // Initialize the source struct
    _MyMem_Raw.ctx = (void*)0x1234;
    _MyMem_Raw.malloc_fn = (void*)0x5678;
    _MyMem_Raw.calloc_fn = (void*)0x9abc;
    _MyMem_Raw.realloc_fn = (void*)0xdef0;
    _MyMem_Raw.free_fn = (void*)0x1111;

    // Copy via function
    Allocator a;
    get_allocator(&a);

    // Verify all fields were copied correctly
    if (a.ctx != (void*)0x1234) return 1;
    if (a.malloc_fn != (void*)0x5678) return 2;
    if (a.calloc_fn != (void*)0x9abc) return 3;
    if (a.realloc_fn != (void*)0xdef0) return 4;
    if (a.free_fn != (void*)0x1111) return 5;

    // Test direct assignment in main (not via function)
    Allocator b = _MyMem_Raw;
    if (b.ctx != (void*)0x1234) return 6;
    if (b.malloc_fn != (void*)0x5678) return 7;

    // Test arrow access (p->member.field pattern)
    RuntimeState *rp = &_MyRuntime;
    Allocator c = rp->allocators.standard.raw;
    if (c.ctx != (void*)0x1234) return 8;
    if (c.malloc_fn != (void*)0x5678) return 9;

    return 0;
}
"#;

    let exit_code = compile_and_run_optimized("struct_member_copy", code);
    assert_eq!(
        exit_code, 0,
        "Large struct member copy test failed with exit code {}",
        exit_code
    );
}

// Test for atomic compare-and-swap register clobbering bug.
// The bug: when regalloc assigns CAS operands to registers R9/R10/R11/RAX,
// loading one operand can clobber another before it's used.
// This test uses inline functions to trigger the problematic register allocation.
#[test]
fn codegen_atomic_cas_register_clobbering() {
    let code = r#"
#include <stdint.h>
#include <stdatomic.h>

#define UNLOCKED 0
#define LOCKED 1

typedef struct { uintptr_t v; } RawMutex;

__attribute__((always_inline))
static inline int
atomic_cas_uintptr(uintptr_t *obj, uintptr_t *expected, uintptr_t desired) {
    return atomic_compare_exchange_strong((_Atomic(uintptr_t)*)obj, expected, desired);
}

__attribute__((always_inline))
static inline int lock_mutex(RawMutex *m) {
    uintptr_t unlocked = UNLOCKED;
    return atomic_cas_uintptr(&m->v, &unlocked, LOCKED);
}

__attribute__((always_inline))
static inline int unlock_mutex(RawMutex *m) {
    uintptr_t locked = LOCKED;
    return atomic_cas_uintptr(&m->v, &locked, UNLOCKED);
}

int main(void) {
    RawMutex m = {0};

    // Test 1: Lock should succeed (v: 0 -> 1)
    if (m.v != 0) return 1;
    if (!lock_mutex(&m)) return 2;  // Lock should succeed
    if (m.v != 1) return 3;         // Value should be 1 after lock

    // Test 2: Double-lock should fail (v is already 1)
    if (lock_mutex(&m)) return 4;   // Second lock should fail
    if (m.v != 1) return 5;         // Value should still be 1

    // Test 3: Unlock should succeed (v: 1 -> 0)
    if (!unlock_mutex(&m)) return 6;  // Unlock should succeed
    if (m.v != 0) return 7;           // Value should be 0 after unlock

    // Test 4: Double-unlock should fail (v is already 0)
    if (unlock_mutex(&m)) return 8;   // Second unlock should fail
    if (m.v != 0) return 9;           // Value should still be 0

    // Test 5: Can lock again after unlock
    if (!lock_mutex(&m)) return 10;
    if (m.v != 1) return 11;

    return 0;
}
"#;

    let exit_code = compile_and_run_optimized("atomic_cas_clobber", code);
    assert_eq!(
        exit_code, 0,
        "Atomic CAS register clobbering test failed with exit code {}",
        exit_code
    );
}

// Regression test for bug where copying a large struct (> 64 bits) from an
// array element would incorrectly dereference the first field as a pointer
// instead of doing a proper block copy.
// Bug: `struct pair item = array[0];` would crash when struct has 2+ pointers.
#[test]
fn codegen_large_struct_array_copy() {
    let code = r#"
#include <stdio.h>

struct pair {
    void *ptr;
    const char *str;
};

static struct pair pairs[] = {
    {(void*)0xDEADBEEF, "First"},
    {(void*)0xCAFEBABE, "Second"},
};

int main(void) {
    // Test: copy struct from array to local variable
    // This was crashing because the code tried to dereference
    // the first field (0xDEADBEEF) as a pointer to copy from
    struct pair item = pairs[0];
    
    // Verify the copy was correct
    if (item.ptr != (void*)0xDEADBEEF) {
        printf("FAIL: item.ptr = %p, expected 0xDEADBEEF\n", item.ptr);
        return 1;
    }
    if (item.str[0] != 'F' || item.str[1] != 'i') {
        printf("FAIL: item.str = %s, expected First\n", item.str);
        return 2;
    }
    
    // Test: copy second element
    struct pair item2 = pairs[1];
    if (item2.ptr != (void*)0xCAFEBABE) {
        printf("FAIL: item2.ptr = %p, expected 0xCAFEBABE\n", item2.ptr);
        return 3;
    }
    if (item2.str[0] != 'S') {
        printf("FAIL: item2.str = %s, expected Second\n", item2.str);
        return 4;
    }
    
    // Test: copy in a loop (dynamic index)
    for (int i = 0; i < 2; i++) {
        struct pair p = pairs[i];
        if (i == 0 && p.ptr != (void*)0xDEADBEEF) return 5;
        if (i == 1 && p.ptr != (void*)0xCAFEBABE) return 6;
    }
    
    printf("OK\n");
    return 0;
}
"#;

    let exit_code = compile_and_run_optimized("struct_array_copy", code);
    assert_eq!(
        exit_code, 0,
        "Large struct array copy test failed with exit code {}",
        exit_code
    );
}

// ============================================================================
// Compound literal zero-initialization test
// ============================================================================
// C99 6.7.8p21: Fields not explicitly initialized in a compound literal
// must be zero-initialized. Bug: *p = (struct S){.a = val} left .b and .c
// as garbage instead of zero.
#[test]
fn codegen_compound_literal_zero_init() {
    let code = r#"
typedef long int64_t;
void *malloc(unsigned long);
void free(void *);
int printf(const char *, ...);
#define NULL ((void*)0)

typedef int (*func_ptr)(void);

struct cached_m_dict {
    void *copied;
    int64_t extra;
};

typedef struct cached_m_dict *cached_m_dict_t;

typedef enum {
    ORIGIN_BUILTIN = 0,
    ORIGIN_CORE = 1,
    ORIGIN_DYNAMIC = 2
} origin_t;

struct extensions_cache_value {
    void *def;                      // offset 0: 8 bytes
    func_ptr m_init;                // offset 8: 8 bytes
    int64_t m_index;                // offset 16: 8 bytes
    cached_m_dict_t m_dict;         // offset 24: 8 bytes (pointer)
    struct cached_m_dict _m_dict;   // offset 32: 16 bytes (embedded struct)
    origin_t origin;                // offset 48: 4 bytes
};

int main(void) {
    struct extensions_cache_value *v = malloc(sizeof(*v));
    
    // Fill with known non-zero pattern to detect failure to zero-init
    v->def = (void*)0xAAAA;
    v->m_init = (func_ptr)0xBBBB;
    v->m_index = 0xCCCC;
    v->m_dict = (cached_m_dict_t)0xDDDD;
    v->_m_dict.copied = (void*)0xEEEE;
    v->_m_dict.extra = 0xFFFF;
    v->origin = ORIGIN_DYNAMIC;
    
    // Assign compound literal with partial initialization
    // Only .def, .m_init, .m_index, and .origin are explicitly set
    // .m_dict, ._m_dict.copied, ._m_dict.extra should become 0
    *v = (struct extensions_cache_value){
        .def = (void*)0x1234,
        .m_init = NULL,
        .m_index = 1,
        .origin = ORIGIN_CORE,
    };
    
    // Check explicitly initialized fields
    if (v->def != (void*)0x1234) {
        printf("FAIL: v->def = %p, expected 0x1234\n", v->def);
        return 1;
    }
    if (v->m_init != NULL) {
        printf("FAIL: v->m_init = %p, expected NULL\n", (void*)v->m_init);
        return 2;
    }
    if (v->m_index != 1) {
        printf("FAIL: v->m_index = %ld, expected 1\n", (long)v->m_index);
        return 3;
    }
    if (v->origin != ORIGIN_CORE) {
        printf("FAIL: v->origin = %d, expected 1\n", v->origin);
        return 4;
    }
    
    // Check implicitly zero-initialized fields (the bug was here!)
    if (v->m_dict != NULL) {
        printf("FAIL: v->m_dict = %p, expected NULL (should be zero-init)\n", 
               (void*)v->m_dict);
        return 5;
    }
    if (v->_m_dict.copied != NULL) {
        printf("FAIL: v->_m_dict.copied = %p, expected NULL (should be zero-init)\n", 
               v->_m_dict.copied);
        return 6;
    }
    if (v->_m_dict.extra != 0) {
        printf("FAIL: v->_m_dict.extra = %ld, expected 0 (should be zero-init)\n", 
               (long)v->_m_dict.extra);
        return 7;
    }
    
    free(v);
    printf("OK\n");
    return 0;
}
"#;

    let exit_code = compile_and_run_optimized("compound_literal_zero", code);
    assert_eq!(
        exit_code, 0,
        "Compound literal zero-init test failed with exit code {}",
        exit_code
    );
}

// ============================================================================
// Ternary conditional expressions with pointer dereference must use short-circuit
// evaluation. Bug: `value = ptr == NULL ? 0 : ptr->x` would evaluate `ptr->x`
// unconditionally, causing a crash when `ptr` is NULL because the compiler
// incorrectly used a select instruction (cmov) instead of proper branching.
#[test]
fn codegen_conditional_short_circuit() {
    let code = r#"
#include <stdio.h>
#include <stddef.h>

struct foo {
    int x;
};

// Test function that uses ternary with pointer dereference
int get_value(struct foo *entry) {
    // This MUST use short-circuit evaluation (branching)
    // If implemented incorrectly with select/cmov, it will crash when entry is NULL
    return entry == NULL ? 0 : entry->x;
}

int main(void) {
    struct foo f = { .x = 42 };
    
    // Test 1: non-NULL pointer should return the value
    int result1 = get_value(&f);
    if (result1 != 42) {
        printf("FAIL: get_value(&f) = %d, expected 42\n", result1);
        return 1;
    }
    
    // Test 2: NULL pointer should return 0 without crashing
    // This will CRASH if the compiler eagerly evaluates entry->x
    int result2 = get_value(NULL);
    if (result2 != 0) {
        printf("FAIL: get_value(NULL) = %d, expected 0\n", result2);
        return 2;
    }
    
    printf("OK\n");
    return 0;
}
"#;

    let exit_code = compile_and_run_optimized("conditional_short_circuit", code);
    assert_eq!(
        exit_code, 0,
        "Conditional short-circuit test failed with exit code {} (likely crashed on NULL dereference)",
        exit_code
    );
}

// ============================================================================
// Test: Inlined two-register struct returns
// ============================================================================

#[test]
fn codegen_inline_two_reg_struct_return() {
    let code = r#"
#include <stdio.h>

/* 16-byte struct: returned via RAX+RDX on x86-64 SysV ABI */
struct S16 { long a; long b; };

/* Same-TU function that will be inlined at -O2 */
static struct S16 make_s16(long x, long y) {
    struct S16 r;
    r.a = x;
    r.b = y;
    return r;
}

/* Mixed int+pointer struct, also 16 bytes */
struct S16b { int a; void *b; };

static struct S16b make_s16b(int x, void *y) {
    struct S16b r;
    r.a = x;
    r.b = y;
    return r;
}

int main(void) {
    /* Test 1: two longs */
    struct S16 s = make_s16(0x2222, 0x3333);
    if (s.a != 0x2222) {
        printf("FAIL: s.a = %lx, expected 2222\n", s.a);
        return 1;
    }
    if (s.b != 0x3333) {
        printf("FAIL: s.b = %lx, expected 3333\n", s.b);
        return 2;
    }

    /* Test 2: int + pointer */
    struct S16b sb = make_s16b(0x44, (void*)0x5555);
    if (sb.a != 0x44) {
        printf("FAIL: sb.a = %x, expected 44\n", sb.a);
        return 3;
    }
    if (sb.b != (void*)0x5555) {
        printf("FAIL: sb.b = %p, expected 0x5555\n", sb.b);
        return 4;
    }

    /* Test 3: multiple calls to verify no state leakage */
    struct S16 s1 = make_s16(100, 200);
    struct S16 s2 = make_s16(300, 400);
    if (s1.a != 100 || s1.b != 200) {
        printf("FAIL: s1 = (%ld, %ld)\n", s1.a, s1.b);
        return 5;
    }
    if (s2.a != 300 || s2.b != 400) {
        printf("FAIL: s2 = (%ld, %ld)\n", s2.a, s2.b);
        return 6;
    }

    printf("OK\n");
    return 0;
}
"#;

    let exit_code = compile_and_run_optimized("inline_two_reg_struct_return", code);
    assert_eq!(
        exit_code, 0,
        "Inline two-reg struct return test failed with exit code {}",
        exit_code
    );
}

// ============================================================================
// Test: Large struct parameter ABI (> 16 bytes passed by value on stack)
// ============================================================================

#[test]
fn codegen_large_struct_param_abi() {
    let code = r#"
#include <stdio.h>
#include <string.h>

/* 32-byte struct: must be passed by value on the stack per SysV AMD64 ABI */
struct Big {
    long a;
    long b;
    long c;
    long d;
};

/* 24-byte struct */
struct Medium {
    long x;
    long y;
    long z;
};

/* 40-byte struct */
struct Bigger {
    long v[5];
};

/* --- Section 1: Basic large struct parameter passing --- */

int check_big(struct Big s) {
    if (s.a != 10) return 1;
    if (s.b != 20) return 2;
    if (s.c != 30) return 3;
    if (s.d != 40) return 4;
    return 0;
}

int check_medium(struct Medium s) {
    if (s.x != 100) return 5;
    if (s.y != 200) return 6;
    if (s.z != 300) return 7;
    return 0;
}

int check_bigger(struct Bigger s) {
    if (s.v[0] != 1) return 8;
    if (s.v[1] != 2) return 9;
    if (s.v[2] != 3) return 10;
    if (s.v[3] != 4) return 11;
    if (s.v[4] != 5) return 12;
    return 0;
}

/* --- Section 2: Large struct with other args (register pressure) --- */

int check_big_with_int(int before, struct Big s, int after) {
    if (before != 99) return 13;
    if (s.a != 10) return 14;
    if (s.b != 20) return 15;
    if (s.c != 30) return 16;
    if (s.d != 40) return 17;
    if (after != 77) return 18;
    return 0;
}

/* --- Section 3: Multiple large struct params --- */

int check_two_bigs(struct Big s1, struct Big s2) {
    if (s1.a != 1) return 19;
    if (s1.d != 4) return 20;
    if (s2.a != 5) return 21;
    if (s2.d != 8) return 22;
    return 0;
}

/* --- Section 4: Large struct return + parameter (sret + stack param) --- */

struct Big make_and_check(struct Big input) {
    struct Big result;
    result.a = input.a + 1;
    result.b = input.b + 1;
    result.c = input.c + 1;
    result.d = input.d + 1;
    return result;
}

/* --- Section 5: Nested call with large struct --- */

int nested_check(struct Big s) {
    return check_big(s);
}

int main(void) {
    int rc;

    /* Section 1: Basic */
    struct Big b = {10, 20, 30, 40};
    rc = check_big(b);
    if (rc) return rc;

    struct Medium m = {100, 200, 300};
    rc = check_medium(m);
    if (rc) return rc;

    struct Bigger bg = {{1, 2, 3, 4, 5}};
    rc = check_bigger(bg);
    if (rc) return rc;

    /* Section 2: Mixed with int args */
    rc = check_big_with_int(99, b, 77);
    if (rc) return rc;

    /* Section 3: Multiple large structs */
    struct Big b1 = {1, 2, 3, 4};
    struct Big b2 = {5, 6, 7, 8};
    rc = check_two_bigs(b1, b2);
    if (rc) return rc;

    /* Section 4: Return + parameter */
    struct Big b3 = make_and_check(b);
    if (b3.a != 11) return 23;
    if (b3.b != 21) return 24;
    if (b3.c != 31) return 25;
    if (b3.d != 41) return 26;

    /* Section 5: Nested call */
    rc = nested_check(b);
    if (rc) return rc + 26;

    printf("OK\n");
    return 0;
}
"#;

    let exit_code = compile_and_run("large_struct_param_abi", code, &[]);
    assert_eq!(
        exit_code, 0,
        "Large struct param ABI test failed with exit code {}",
        exit_code
    );
}

// ============================================================================
// Regression test: SSA phi insertion in goto-dispatch pattern
// ============================================================================
// Tests that variables declared at function scope maintain correct values
// across a goto-dispatch loop with a switch statement. This pattern is
// used heavily by CPython's ceval.c bytecode interpreter.
//
// The bug: insert_phi_nodes() in ssa.rs had a filter that skipped phi node
// insertion at IDF blocks not dominated by the variable's declaration block.
// This caused function-scope variables to lose their values at the dispatch
// label when connected by goto back-edges from case handlers.
#[test]
fn codegen_ssa_phi_goto_dispatch() {
    let code = r#"
/* Regression test for SSA phi insertion bug in goto-dispatch patterns.
 *
 * This mimics CPython's ceval.c pattern: a bytecode dispatch loop where
 * a variable (accumulator) is modified by different opcode handlers and
 * its value must survive across goto back-edges to the dispatch label.
 */

/* Opcodes for our mini interpreter */
#define OP_LOAD_CONST  0
#define OP_ADD         1
#define OP_MUL         2
#define OP_NEGATE      3
#define OP_HALT        4

struct instruction {
    int opcode;
    int operand;
};

/* A mini bytecode interpreter using goto-dispatch */
int interpret(struct instruction *code, int n_insns) {
    int accumulator = 0;   /* function-scope variable: the bug target */
    int ip = 0;            /* instruction pointer */
    int temp;

    goto dispatch;

dispatch:
    if (ip >= n_insns)
        return -1;  /* ran off the end */

    switch (code[ip].opcode) {
    case OP_LOAD_CONST:
        goto do_load_const;
    case OP_ADD:
        goto do_add;
    case OP_MUL:
        goto do_mul;
    case OP_NEGATE:
        goto do_negate;
    case OP_HALT:
        goto do_halt;
    default:
        return -2;  /* unknown opcode */
    }

do_load_const:
    accumulator = code[ip].operand;
    ip++;
    goto dispatch;

do_add:
    accumulator = accumulator + code[ip].operand;
    ip++;
    goto dispatch;

do_mul:
    accumulator = accumulator * code[ip].operand;
    ip++;
    goto dispatch;

do_negate:
    accumulator = -accumulator;
    ip++;
    goto dispatch;

do_halt:
    return accumulator;
}

int main(void) {
    /* Test 1: LOAD_CONST 5, HALT -> expect 5 */
    {
        struct instruction prog[] = {
            {OP_LOAD_CONST, 5},
            {OP_HALT, 0}
        };
        int result = interpret(prog, 2);
        if (result != 5) return 1;
    }

    /* Test 2: LOAD_CONST 3, ADD 7, HALT -> expect 10 */
    {
        struct instruction prog[] = {
            {OP_LOAD_CONST, 3},
            {OP_ADD, 7},
            {OP_HALT, 0}
        };
        int result = interpret(prog, 3);
        if (result != 10) return 2;
    }

    /* Test 3: LOAD_CONST 4, MUL 5, HALT -> expect 20 */
    {
        struct instruction prog[] = {
            {OP_LOAD_CONST, 4},
            {OP_MUL, 5},
            {OP_HALT, 0}
        };
        int result = interpret(prog, 3);
        if (result != 20) return 3;
    }

    /* Test 4: LOAD_CONST 7, NEGATE, HALT -> expect -7 */
    {
        struct instruction prog[] = {
            {OP_LOAD_CONST, 7},
            {OP_NEGATE, 0},
            {OP_HALT, 0}
        };
        int result = interpret(prog, 3);
        if (result != -7) return 4;
    }

    /* Test 5: Multi-step computation:
     * LOAD_CONST 2, MUL 3, ADD 4, NEGATE, ADD 100, HALT
     * -> ((2 * 3) + 4) = 10, negate = -10, + 100 = 90 */
    {
        struct instruction prog[] = {
            {OP_LOAD_CONST, 2},
            {OP_MUL, 3},
            {OP_ADD, 4},
            {OP_NEGATE, 0},
            {OP_ADD, 100},
            {OP_HALT, 0}
        };
        int result = interpret(prog, 6);
        if (result != 90) return 5;
    }

    /* Test 6: Verify accumulator starts at 0 each call (no state leakage)
     * ADD 42, HALT -> 0 + 42 = 42 */
    {
        struct instruction prog[] = {
            {OP_ADD, 42},
            {OP_HALT, 0}
        };
        int result = interpret(prog, 2);
        if (result != 42) return 6;
    }

    return 0;
}
"#;

    assert_eq!(compile_and_run("ssa_phi_goto_dispatch", code, &[]), 0);
}

// ============================================================================
// PhiSource integration tests
// ============================================================================

#[test]
fn codegen_phisource_ternary() {
    let code = r#"
int main(void) {
    // Ternary produces phi with PhiSource in each branch
    int x = 10;
    int y = 20;
    int a = (x > 5) ? x : y;
    if (a != 10) return 1;

    int b = (x < 5) ? x : y;
    if (b != 20) return 2;

    // Nested ternary
    int c = (x > 5) ? ((y > 15) ? 100 : 200) : 300;
    if (c != 100) return 3;

    // Ternary with function calls and side effects
    int d = (x > 0) ? x + y : x - y;
    if (d != 30) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("phisource_ternary", code, &[]), 0);
}

#[test]
fn codegen_phisource_logical_and_or() {
    let code = r#"
int side = 0;
int inc(void) { side++; return side; }

int main(void) {
    // Logical AND produces phi via short-circuit
    int a = (1 && 1);
    if (a != 1) return 1;

    int b = (1 && 0);
    if (b != 0) return 2;

    int c = (0 && 1);
    if (c != 0) return 3;

    // Logical OR produces phi via short-circuit
    int d = (0 || 1);
    if (d != 1) return 4;

    int e = (0 || 0);
    if (e != 0) return 5;

    int f = (1 || 0);
    if (f != 1) return 6;

    // Chained: a && b && c
    int g = (1 && 1 && 1);
    if (g != 1) return 7;

    int h = (1 && 0 && 1);
    if (h != 0) return 8;

    // Chained: a || b || c
    int i = (0 || 0 || 1);
    if (i != 1) return 9;

    // Mixed: (a && b) || (c && d)
    int j = (1 && 0) || (1 && 1);
    if (j != 1) return 10;

    // Short-circuit: side effects should not execute
    side = 0;
    int k = (0 && inc());
    if (side != 0) return 11;  // inc() should NOT be called

    side = 0;
    int l = (1 || inc());
    if (side != 0) return 12;  // inc() should NOT be called

    return 0;
}
"#;
    assert_eq!(compile_and_run("phisource_logical", code, &[]), 0);
}

#[test]
fn codegen_phisource_loop_phi() {
    let code = r#"
int main(void) {
    // Simple loop with induction variable (SSA phi)
    int sum = 0;
    for (int i = 0; i < 10; i++) {
        sum += i;
    }
    if (sum != 45) return 1;

    // Nested loops
    int total = 0;
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 3; j++) {
            total++;
        }
    }
    if (total != 15) return 2;

    // While loop with phi
    int n = 100;
    int count = 0;
    while (n > 1) {
        if (n % 2 == 0) {
            n = n / 2;
        } else {
            n = 3 * n + 1;
        }
        count++;
    }
    // Collatz sequence for 100 has 25 steps
    if (count != 25) return 3;

    // Do-while with phi
    int x = 0;
    int iter = 0;
    do {
        x += iter * 2;
        iter++;
    } while (iter < 5);
    // x = 0+2+4+6+8 = 20
    if (x != 20) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("phisource_loop", code, &[]), 0);
}

#[test]
fn codegen_phisource_optimized() {
    let code = r#"
int main(void) {
    // Test PhiSource survives optimization passes (DCE, instcombine, inlining)
    int x = 10;

    // Ternary at -O2
    int a = (x > 5) ? x + 1 : x - 1;
    if (a != 11) return 1;

    // Logical ops at -O2
    int b = (x > 0 && x < 100);
    if (b != 1) return 2;

    int c = (x < 0 || x > 5);
    if (c != 1) return 3;

    // Loop at -O2
    int sum = 0;
    for (int i = 1; i <= 10; i++) {
        sum += i;
    }
    if (sum != 55) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run_optimized("phisource_optimized", code), 0);
}

// ============================================================================
// 32/64-bit type width audit tests
// ============================================================================

/// Test: enum is treated as integer for pointer arithmetic and is_integer checks
#[test]
fn codegen_enum_is_integer() {
    let code = r#"
enum Color { RED, GREEN, BLUE };

int arr[] = {10, 20, 30};

int get_via_enum_ptr(int *p, enum Color c) {
    return *(p + c);
}

int main(void) {
    // Section 1: enum used in pointer arithmetic
    if (get_via_enum_ptr(arr, RED) != 10) return 1;
    if (get_via_enum_ptr(arr, GREEN) != 20) return 2;
    if (get_via_enum_ptr(arr, BLUE) != 30) return 3;

    // Section 2: enum used as array index (also pointer arithmetic)
    enum Color idx = BLUE;
    if (arr[idx] != 30) return 4;

    // Section 3: enum in arithmetic with unsigned
    enum Color c = GREEN;
    unsigned int u = 5;
    unsigned int result = c + u;
    if (result != 6) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("enum_is_integer", code, &[]), 0);
}

/// Test: unary minus applies integer promotion (char/short -> int)
#[test]
fn codegen_unary_neg_promotion() {
    let code = r#"
int main(void) {
    // Section 1: negating a char should produce int-width result
    char c = 100;
    int neg_c = -c;
    if (neg_c != -100) return 1;

    // Section 2: negating a short should produce int-width result
    short s = 30000;
    int neg_s = -s;
    if (neg_s != -30000) return 2;

    // Section 3: negating a char and using in arithmetic
    char ch = 1;
    int x = -ch + 200;
    if (x != 199) return 3;

    // Section 4: negating unsigned char (should be int, not wrapping char)
    unsigned char uc = 200;
    int neg_uc = -uc;
    // -200 as int (not as unsigned char which would be 56)
    if (neg_uc != -200) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("unary_neg_promotion", code, &[]), 0);
}

/// Test: function returning pointer/long with no explicit return gets 64-bit zero
#[test]
fn codegen_default_return_64bit() {
    let code = r#"
// Function returning pointer without explicit return
void *get_null(int flag) {
    if (flag) {
        return (void*)0;
    }
    // implicit return should be 64-bit zero, not 32-bit
}

// Function returning long without explicit return
long get_long_default(int flag) {
    if (flag) {
        return 42L;
    }
    // implicit return should be 64-bit zero
}

int main(void) {
    // Section 1: pointer default return
    void *p = get_null(0);
    if (p != (void*)0) return 1;

    // Section 2: long default return
    long l = get_long_default(0);
    if (l != 0L) return 2;

    // Section 3: explicit returns still work
    void *p2 = get_null(1);
    if (p2 != (void*)0) return 3;
    long l2 = get_long_default(1);
    if (l2 != 42L) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("default_return_64bit", code, &[]), 0);
}

/// Test: pointer arithmetic with char/short index uses correct sign extension
#[test]
fn codegen_ptr_arith_narrow_index() {
    let code = r#"
int arr[10] = {0, 10, 20, 30, 40, 50, 60, 70, 80, 90};

int main(void) {
    int *p = arr;

    // Section 1: char index
    char ci = 3;
    if (*(p + ci) != 30) return 1;

    // Section 2: short index
    short si = 5;
    if (*(p + si) != 50) return 2;

    // Section 3: negative char index from middle of array
    int *mid = &arr[5];
    char neg = -2;
    if (*(mid + neg) != 30) return 3;

    // Section 4: negative short index
    short sneg = -3;
    if (*(mid + sneg) != 20) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ptr_arith_narrow_index", code, &[]), 0);
}

/// Test: float-to-int and float-to-float casts produce correct size
#[test]
fn codegen_float_cast_sizes() {
    let code = r#"
int main(void) {
    // Section 1: double to int
    double d = 42.7;
    int i = (int)d;
    if (i != 42) return 1;

    // Section 2: double to long
    double d2 = 1000000000000.0;
    long l = (long)d2;
    if (l != 1000000000000L) return 2;

    // Section 3: float to long
    float f = 123456.0f;
    long l2 = (long)f;
    if (l2 != 123456L) return 3;

    // Section 4: double to unsigned long
    double d3 = 4000000000.0;
    unsigned long ul = (unsigned long)d3;
    if (ul != 4000000000UL) return 4;

    // Section 5: float to double (FCvtF)
    float f2 = 3.14f;
    double d4 = (double)f2;
    // Check approximate equality (float precision)
    if (d4 < 3.13 || d4 > 3.15) return 5;

    // Section 6: double to float (FCvtF)
    double d5 = 2.718;
    float f3 = (float)d5;
    if (f3 < 2.71f || f3 > 2.72f) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("float_cast_sizes", code, &[]), 0);
}

/// Test: switch on long value compares all 64 bits
#[test]
fn codegen_switch_64bit() {
    let code = r#"
int classify(long val) {
    switch (val) {
        case 0L: return 0;
        case 1L: return 1;
        case 0x100000000L: return 2;  // distinguishable only in upper 32 bits
        case 0x200000000L: return 3;
        default: return -1;
    }
}

int main(void) {
    if (classify(0L) != 0) return 1;
    if (classify(1L) != 1) return 2;
    if (classify(0x100000000L) != 2) return 3;
    if (classify(0x200000000L) != 3) return 4;
    if (classify(99L) != -1) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("switch_64bit", code, &[]), 0);
}

/// Test: function returning pointer via call — return value is full 64 bits
#[test]
fn codegen_call_return_pointer() {
    let code = r#"
void *identity(void *p) {
    return p;
}

long return_long(long v) {
    return v;
}

int main(void) {
    // Section 1: pointer return value preserved through call
    long stack_var = 12345;
    void *p = &stack_var;
    void *q = identity(p);
    if (p != q) return 1;
    if (*(long*)q != 12345) return 2;

    // Section 2: long return value preserved through call
    long big = 0x123456789ABCDEF0L;
    long result = return_long(big);
    if (result != big) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("call_return_pointer", code, &[]), 0);
}

/// Test: signed bitfield in 64-bit storage unit
#[test]
fn codegen_bitfield_64bit_storage() {
    let code = r#"
struct Wide {
    long a : 40;
    long b : 20;
};

int main(void) {
    struct Wide w;

    // Section 1: store and read 40-bit signed field
    w.a = 0x7FFFFFFFFFL;  // max positive 40-bit
    if (w.a != 0x7FFFFFFFFFL) return 1;

    // Section 2: negative value in 40-bit field
    w.a = -1;
    if (w.a != -1) return 2;

    // Section 3: 20-bit field
    w.b = 500000;
    if (w.b != 500000) return 3;

    w.b = -1;
    if (w.b != -1) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_64bit_storage", code, &[]), 0);
}

/// Test: emit_ret with 64-bit integer return value
#[test]
fn codegen_ret_64bit() {
    let code = r#"
long return_big(void) {
    return 0x123456789ABCDEF0L;
}

void *return_ptr(void *p) {
    return p;
}

unsigned long return_unsigned(void) {
    return 0xFFFFFFFFFFFFFFFFUL;
}

int main(void) {
    // Section 1: long return
    long v = return_big();
    if (v != 0x123456789ABCDEF0L) return 1;

    // Section 2: pointer return
    int x = 42;
    void *p = return_ptr(&x);
    if (*(int*)p != 42) return 2;

    // Section 3: unsigned long return
    unsigned long u = return_unsigned();
    if (u != 0xFFFFFFFFFFFFFFFFUL) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ret_64bit", code, &[]), 0);
}

// ============================================================================
// Type-based size derivation: verify 64-bit width through binop, unary, mul,
// div, compare, and select when insn.typ carries the authoritative width.
// Values above 2^32 prove no 32-bit truncation occurs.
// ============================================================================

#[test]
fn codegen_type_based_sizing_64bit() {
    let code = r#"
long identity(long x) { return x; }

int main(void) {
    long base = 0x100000000L;  /* 4 GiB — above 32-bit range */

    /* ===== binop: add, sub, and, or, xor, shift (returns 1-9) ===== */
    long a = base + 1L;
    if (a != 0x100000001L) return 1;

    long b = a - 1L;
    if (b != base) return 2;

    long c = base | 0xFL;
    if (c != 0x10000000FL) return 3;

    long d = c & 0x1FFFFFFFFL;
    if (d != 0x10000000FL) return 4;

    long e = base ^ 0x100000000L;
    if (e != 0L) return 5;

    long f = 1L << 33;
    if (f != 0x200000000L) return 6;

    long g = 0x200000000L >> 1;
    if (g != base) return 7;

    /* ===== unary neg (returns 10-12) ===== */
    long h = -base;
    if (h != -0x100000000L) return 10;

    long i = -h;
    if (i != base) return 11;

    /* ===== mul (returns 20-22) ===== */
    long j = base * 2L;
    if (j != 0x200000000L) return 20;

    long k = 0x10000L * 0x10000L;
    if (k != base) return 21;

    /* ===== div (returns 30-34) ===== */
    long m = 0x200000000L / 2L;
    if (m != base) return 30;

    long n = 0x200000001L % base;
    if (n != 1L) return 31;

    /* unsigned div */
    unsigned long ud = 0x200000000UL / 2UL;
    if (ud != 0x100000000UL) return 32;

    unsigned long um = 0x200000003UL % 0x100000000UL;
    if (um != 3UL) return 33;

    /* ===== compare (returns 40-46) ===== */
    if (!(base > 0xFFFFFFFFL)) return 40;
    if (!(base == 0x100000000L)) return 41;
    if (base < 0L) return 42;
    if (!(0x200000000L > base)) return 43;

    /* pointer comparison */
    long arr[4];
    long *p1 = &arr[0];
    long *p2 = &arr[3];
    if (!(p2 > p1)) return 44;
    if (p1 == p2) return 45;

    /* ===== select / ternary (returns 50-53) ===== */
    int flag = 1;
    long s1 = flag ? base : 0L;
    if (s1 != base) return 50;

    long s2 = (!flag) ? 0L : 0x200000000L;
    if (s2 != 0x200000000L) return 51;

    /* select with pointer */
    long *ps = flag ? p2 : p1;
    if (ps != p2) return 52;

    return 0;
}
"#;
    assert_eq!(compile_and_run("type_based_sizing_64bit", code, &[]), 0);
}

// ============================================================================
// VLA sizeof with multi-dimensional arrays — exercises VLA Mul instructions
// that now carry .with_type(ulong_id) for correct 64-bit multiplication.
// ============================================================================

#[test]
fn codegen_vla_sizeof_mul_type() {
    let code = r#"
int main(void) {
    /* ===== 1D VLA sizeof with different element types (returns 1-4) ===== */
    int n = 10;

    char c_arr[n];
    if (sizeof(c_arr) != 10) return 1;   /* 10 * 1 */

    int i_arr[n];
    if (sizeof(i_arr) != 40) return 2;   /* 10 * 4 */

    long l_arr[n];
    if (sizeof(l_arr) != 80) return 3;   /* 10 * 8 */

    long *p_arr[n];
    if (sizeof(p_arr) != 80) return 4;   /* 10 * 8 (pointer) */

    /* ===== 2D VLA sizeof — exercises dimension mul (returns 10-13) ===== */
    int rows = 5, cols = 7;
    int mat[rows][cols];
    if (sizeof(mat) != 140) return 10;   /* 5 * 7 * 4 */

    long lmat[rows][cols];
    if (sizeof(lmat) != 280) return 11;  /* 5 * 7 * 8 */

    /* ===== 3D VLA sizeof — two chained muls (returns 20-21) ===== */
    int d1 = 3, d2 = 4, d3 = 5;
    int cube[d1][d2][d3];
    if (sizeof(cube) != 240) return 20;  /* 3 * 4 * 5 * 4 */

    long lcube[d1][d2][d3];
    if (sizeof(lcube) != 480) return 21; /* 3 * 4 * 5 * 8 */

    /* ===== VLA sizeof via function (returns 30-31) ===== */
    /* Ensure runtime sizeof flows correctly through return */
    int m = 12;
    int dyn[m];
    int sz = sizeof(dyn);
    if (sz != 48) return 30;             /* 12 * 4 */

    /* sizeof in expression context */
    int total = sizeof(dyn) + sizeof(mat);
    if (total != 188) return 31;         /* 48 + 140 */

    return 0;
}
"#;
    assert_eq!(compile_and_run("vla_sizeof_mul_type", code, &[]), 0);
}

// ============================================================================
// Variadic functions with 64-bit args — exercises VaArg .with_size() for
// long and pointer types. Values above 2^32 prove no 32-bit truncation.
// ============================================================================

#[test]
fn codegen_variadic_64bit_args() {
    let code = r#"
#include <stdarg.h>
#include <string.h>

/* va_arg with long — tests 64-bit integer extraction */
long sum_longs(int count, ...) {
    va_list ap;
    va_start(ap, count);
    long sum = 0;
    for (int i = 0; i < count; i++) {
        sum += va_arg(ap, long);
    }
    va_end(ap);
    return sum;
}

/* va_arg with pointer — tests 64-bit pointer extraction */
int sum_through_ptrs(int count, ...) {
    va_list ap;
    va_start(ap, count);
    int sum = 0;
    for (int i = 0; i < count; i++) {
        int *p = va_arg(ap, int *);
        sum += *p;
    }
    va_end(ap);
    return sum;
}

/* mixed int and long args — tests correct slot advancement */
long mixed_args(int count, ...) {
    va_list ap;
    va_start(ap, count);
    long result = 0;
    for (int i = 0; i < count; i++) {
        if (i % 2 == 0) {
            result += va_arg(ap, int);
        } else {
            result += va_arg(ap, long);
        }
    }
    va_end(ap);
    return result;
}

/* va_copy with 64-bit args */
long sum_longs_twice(int count, ...) {
    va_list ap, ap2;
    va_start(ap, count);
    va_copy(ap2, ap);

    long sum1 = 0;
    for (int i = 0; i < count; i++) {
        sum1 += va_arg(ap, long);
    }

    long sum2 = 0;
    for (int i = 0; i < count; i++) {
        sum2 += va_arg(ap2, long);
    }

    va_end(ap);
    va_end(ap2);
    return sum1 + sum2;
}

int main(void) {
    /* ===== long va_arg (returns 1-4) ===== */
    long big = 0x100000000L;

    long r1 = sum_longs(1, big);
    if (r1 != big) return 1;

    long r2 = sum_longs(2, big, big);
    if (r2 != 0x200000000L) return 2;

    long r3 = sum_longs(3, 1L, 2L, 0x100000000L);
    if (r3 != 0x100000003L) return 3;

    /* negative 64-bit */
    long r4 = sum_longs(2, -1L, big);
    if (r4 != 0xFFFFFFFFL) return 4;

    /* ===== pointer va_arg (returns 10-12) ===== */
    int a = 10, b = 20, c = 30;
    int s1 = sum_through_ptrs(1, &a);
    if (s1 != 10) return 10;

    int s2 = sum_through_ptrs(3, &a, &b, &c);
    if (s2 != 60) return 11;

    /* Array of values, pass pointers to several elements */
    int arr[5] = {100, 200, 300, 400, 500};
    int s3 = sum_through_ptrs(3, &arr[0], &arr[2], &arr[4]);
    if (s3 != 900) return 12;  /* 100 + 300 + 500 */

    /* ===== mixed int/long (returns 20-21) ===== */
    /* mixed_args reads: int, long, int, long */
    long m1 = mixed_args(4, 1, 0x100000000L, 2, 0x200000000L);
    if (m1 != 0x300000003L) return 20;

    long m2 = mixed_args(2, 42, 0x100000000L);
    if (m2 != 0x10000002AL) return 21;

    /* ===== va_copy with 64-bit (returns 30-31) ===== */
    long vc1 = sum_longs_twice(1, big);
    if (vc1 != 0x200000000L) return 30;  /* big + big */

    long vc2 = sum_longs_twice(2, big, 1L);
    if (vc2 != 0x200000002L) return 31;  /* (big+1) + (big+1) */

    return 0;
}
"#;
    assert_eq!(compile_and_run("variadic_64bit_args", code, &[]), 0);
}

// ============================================================================
// Regression: string literal bytes >= 0x80 must not be UTF-8 encoded
// ============================================================================

#[test]
fn codegen_string_literal_high_bytes() {
    let code = r#"
#include <stdio.h>

struct test {
    int x;
    char code[8];
    int y;
};

static struct test t = {
    .x = 42,
    .code = "\x97\x00\x64\x00\xAA\xBB\xCC\xDD",
    .y = 99,
};

int main(void) {
    /* Verify struct fields are correct (not shifted by UTF-8 expansion) */
    if (t.x != 42) return 1;
    if (t.y != 99) return 2;

    /* Verify each byte is stored as a raw byte, not UTF-8 encoded */
    unsigned char *p = (unsigned char *)t.code;
    if (p[0] != 0x97) return 10;
    if (p[1] != 0x00) return 11;
    if (p[2] != 0x64) return 12;
    if (p[3] != 0x00) return 13;
    if (p[4] != 0xAA) return 14;
    if (p[5] != 0xBB) return 15;
    if (p[6] != 0xCC) return 16;
    if (p[7] != 0xDD) return 17;

    /* Test string literals with various high bytes */
    const char *s = "\x80\xFF\xFE\xC0\xC2\x97";
    unsigned char *q = (unsigned char *)s;
    if (q[0] != 0x80) return 20;
    if (q[1] != 0xFF) return 21;
    if (q[2] != 0xFE) return 22;
    if (q[3] != 0xC0) return 23;
    if (q[4] != 0xC2) return 24;
    if (q[5] != 0x97) return 25;

    /* sizeof must count C bytes, not UTF-8 encoded bytes */
    if (sizeof("\x80") != 2) return 30;       /* 1 byte + null */
    if (sizeof("\xc2\x80") != 3) return 31;   /* 2 bytes + null */
    if (sizeof("\xff") != 2) return 32;        /* 1 byte + null */
    if (sizeof("hello") != 6) return 33;      /* 5 bytes + null */

    return 0;
}
"#;
    assert_eq!(compile_and_run("string_literal_high_bytes", code, &[]), 0);
}

// ============================================================================
// Regression: XMM registers must be spilled across function calls
// ============================================================================

#[test]
fn codegen_xmm_spill_across_calls() {
    let code = r#"
#include <math.h>
#include <stdio.h>

/* External function to force a real call (prevents inlining) */
extern int check_type(void *ptr);

struct obj {
    int type_tag;
    double value;
};

int check_type(void *ptr) {
    struct obj *o = (struct obj *)ptr;
    return o->type_tag == 1;
}

double get_value(struct obj *o) {
    return o->value;
}

int main(void) {
    struct obj a = { .type_tag = 1, .value = 1.5 };
    struct obj b = { .type_tag = 1, .value = 2.5 };

    /* Load a.value, then make a function call, then use a.value */
    double va = get_value(&a);
    int is_valid = check_type(&b);  /* call between load and use */
    double vb = get_value(&b);

    if (!is_valid) return 1;

    /* va should still be 1.5, not clobbered by the call */
    if (va != 1.5) return 10;
    if (vb != 2.5) return 11;
    if (va == vb) return 12;  /* they must not be equal */

    /* Test with math library calls */
    double x = exp(1.0);
    double y = sqrt(4.0);  /* call between loads */
    double z = exp(2.0);

    /* x should still be e^1, not clobbered */
    if (x < 2.71 || x > 2.72) return 20;
    if (y < 1.99 || y > 2.01) return 21;
    if (z < 7.38 || z > 7.40) return 22;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("xmm_spill_across_calls", code, &["-lm".to_string()]),
        0
    );
}

// ============================================================================
// Regression: double-to-bool conversion must use ucomisd, not ucomiss
// ============================================================================

#[test]
fn codegen_double_to_bool() {
    let code = r#"
int main(void) {
    /* if(double) must compare as 64-bit, not truncate to 32-bit */
    double m = 0.5;
    if (!m) return 1;  /* 0.5 is truthy */

    double z = 0.0;
    if (z) return 2;  /* 0.0 is falsy */

    /* while(double) */
    double w = 0.25;
    int count = 0;
    while (w) {
        count++;
        w = 0.0;  /* exit after one iteration */
    }
    if (count != 1) return 3;

    /* Values whose lower 32 bits are zero (only visible in 64-bit double) */
    double tiny = 1e-40;  /* nonzero but lower float32 bits are zero */
    if (!tiny) return 4;

    /* Negative values */
    double neg = -0.5;
    if (!neg) return 5;

    /* float should also work */
    float f = 0.5f;
    if (!f) return 6;

    float fz = 0.0f;
    if (fz) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("double_to_bool", code, &[]), 0);
}

// ============================================================================
// Regression: unsigned long to double must not use signed cvtsi2sd
// ============================================================================

#[test]
fn codegen_unsigned_long_to_double() {
    let code = r#"
#include <limits.h>
#include <stdio.h>

int main(void) {
    /* Values >= 2^63 require unsigned conversion path */
    unsigned long big = (unsigned long)LONG_MAX + 1;  /* 2^63 */
    double d = (double)big;
    /* Must be positive 9.22e18, not negative */
    if (d < 0.0) return 1;
    if (d < 9.2e18) return 2;

    /* Smaller unsigned values should still work */
    unsigned long small = 1000;
    double ds = (double)small;
    if (ds != 1000.0) return 3;

    /* ULONG_MAX */
    unsigned long umax = (unsigned long)-1;
    double du = (double)umax;
    if (du < 0.0) return 4;
    if (du < 1.8e19) return 5;

    /* Zero */
    unsigned long zero = 0;
    double dz = (double)zero;
    if (dz != 0.0) return 6;

    /* Value just below 2^63 (should use signed path) */
    unsigned long below = (unsigned long)LONG_MAX;
    double db = (double)below;
    if (db < 9.2e18) return 7;
    if (db < 0.0) return 8;

    /* float conversion too */
    unsigned long fbig = (unsigned long)LONG_MAX + 1;
    float f = (float)fbig;
    if (f < 0.0f) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("unsigned_long_to_double", code, &[]), 0);
}

// ============================================================================
// Regression: ternary result type must be common type of both branches
// ============================================================================

#[test]
fn codegen_ternary_common_type() {
    let code = r#"
struct rec {
    unsigned char flags;
    unsigned char decimal;
};

static struct rec table[] = {
    { 0x02, 5 },   /* digit */
    { 0x00, 0xFF }, /* non-digit */
};

int get_decimal(struct rec *r) {
    /* Result type must be int (common of unsigned char and int), not unsigned char */
    return (r->flags & 0x02) ? r->decimal : -1;
}

int main(void) {
    /* Digit case: should return 5 */
    if (get_decimal(&table[0]) != 5) return 1;
    /* Non-digit case: should return -1, not 255 */
    if (get_decimal(&table[1]) != -1) return 2;

    /* Also test with wider types */
    int x = 1;
    long result = x ? (short)42 : 1000000L;
    if (result != 42) return 3;

    long result2 = (!x) ? (short)42 : 1000000L;
    if (result2 != 1000000L) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ternary_common_type", code, &[]), 0);
}

/// Regression test: FP compare clobber when src2 is allocated to Xmm0.
/// The codegen for `d < -(double)MIN` would move src1 into Xmm0, clobbering
/// src2 if it was already in Xmm0, then compare Xmm0 with itself.
#[test]
fn codegen_fp_compare_xmm0_clobber() {
    let code = r#"
#include <stdint.h>
#include <limits.h>

typedef int64_t _PyTime_t;
#define _PyTime_MIN INT64_MIN

/* Separate function to prevent constant folding */
static double scale(double x, double y) { return x * y; }

static int check(double value, long unit_to_ns) {
    volatile double d;
    d = value;
    d = scale(d, (double)unit_to_ns);

    /* This pattern triggers the bug: && with two FP comparisons
       involving a negated cast of an int64_t min constant.
       The second comparison's src2 (-(double)_PyTime_MIN) could be
       allocated to Xmm0, which gets clobbered when src1 (d) is
       moved into Xmm0 for the ucomisd instruction. */
    if (!((double)_PyTime_MIN <= d && d < -(double)_PyTime_MIN)) {
        return -1;  /* overflow */
    }
    return 0;  /* ok */
}

int main(void) {
    /* Small values should NOT overflow */
    if (check(0.001, 1000000000L) != 0) return 1;
    if (check(1.0, 1000000000L) != 0) return 2;
    if (check(120.0, 1000000000L) != 0) return 3;
    if (check(0.0, 1000000000L) != 0) return 4;
    if (check(-0.001, 1000000000L) != 0) return 5;

    /* Huge values SHOULD overflow */
    if (check(1e19, 1000000000L) != -1) return 6;
    if (check(-1e19, 1000000000L) != -1) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("fp_compare_xmm0_clobber", code, &[]), 0);
}

/// Regression test: inline asm with "+r" constraint on 64-bit value used 32-bit register.
/// The xchg instruction in CPython's atomic store macro would truncate pointers.
#[test]
fn codegen_inline_asm_64bit_constraint() {
    let code = r#"
#include <stdint.h>

typedef struct { uintptr_t _value; } atomic_addr;

static inline void atomic_store(atomic_addr *addr, uintptr_t val) {
    uintptr_t new_val = val;
    __asm__ volatile("xchg %0, %1"
                     : "+r"(new_val)
                     : "m"(addr->_value)
                     : "memory");
}

int main(void) {
    atomic_addr slot = {0};
    /* Use a pointer value that exercises upper 32 bits */
    uintptr_t ptr = 0x7F5500123456UL;

    atomic_store(&slot, ptr);

    uintptr_t loaded = *(volatile uintptr_t *)&slot._value;
    if (loaded != ptr) return 1;  /* upper bits truncated */

    /* Also test 32-bit values still work */
    uintptr_t small = 42;
    atomic_store(&slot, small);
    loaded = *(volatile uintptr_t *)&slot._value;
    if (loaded != 42) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("inline_asm_64bit_constraint", code, &[]), 0);
}

/// Regression test: XMM-to-GPR move used movd (32-bit) instead of movq (64-bit)
/// for doubles. This truncated the upper 32 bits, causing double values like
/// 1.0 (0x3FF0000000000000) to become 0.0 (lower 32 bits are zero).
#[test]
fn codegen_double_xmm_to_gpr_movq() {
    let code = r#"
double negate_if(double val, int neg) {
    if (neg) return -val;
    return val;
}

int main(void) {
    /* Without the fix, movd truncates to 32 bits.
       1.0 = 0x3FF0000000000000 → lower 32 bits = 0 → result is 0.0 */
    if (negate_if(1.0, 0) != 1.0) return 1;
    if (negate_if(1.0, 1) != -1.0) return 2;
    if (negate_if(3.14, 0) != 3.14) return 3;
    if (negate_if(3.14, 1) != -3.14) return 4;

    /* Also test ternary with doubles */
    double x = 1.0;
    double y = 2.0;
    int cond = 1;
    double r = cond ? x : y;
    if (r != 1.0) return 5;
    r = (!cond) ? x : y;
    if (r != 2.0) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("double_xmm_to_gpr_movq", code, &[]), 0);
}

/// Regression test: ternary selecting function pointers lost return type.
/// `(cond ? func_a : func_b)(arg)` used pointer_to() which returned void*
/// when the pointer-to-function type wasn't in the lookup table. The call's
/// return type defaulted to int (32-bit), truncating pointer return values.
#[test]
fn codegen_ternary_fptr_return_type() {
    let code = r#"
#include <stdlib.h>

typedef struct { int x; } Obj;

Obj *func_a(int *p) { Obj *o = malloc(sizeof(*o)); o->x = *p + 100; return o; }
Obj *func_b(int *p) { Obj *o = malloc(sizeof(*o)); o->x = *p + 200; return o; }

int main(void) {
    int data = 42;
    int cond = 1;

    /* Ternary selecting function pointer, then calling result */
    Obj *result = (cond ? func_a : func_b)(&data);
    if (result->x != 142) return 1;

    cond = 0;
    Obj *result2 = (cond ? func_a : func_b)(&data);
    if (result2->x != 242) return 2;

    free(result);
    free(result2);
    return 0;
}
"#;
    assert_eq!(compile_and_run("ternary_fptr_return_type", code, &[]), 0);
}

/// Regression test: character literals >= 0x80 must be sign-extended like GCC.
/// '\x80' should produce -128 (signed char interpretation), not 128.
/// This broke CPython's serialization module where enum { PROTO = '\x80' }
/// didn't match switch cases on signed char values.
#[test]
fn codegen_char_literal_signedness() {
    let code = r#"
enum opcode { NONE = 'N', PROTO = '\x80', STOP = '.' };

int dispatch(const char *s) {
    switch ((enum opcode)s[0]) {
    case NONE: return 1;
    case PROTO: return 2;
    case STOP: return 3;
    default: return -1;
    }
}

int main(void) {
    char proto = '\x80';
    char none = 'N';
    char stop = '.';

    if (dispatch(&proto) != 2) return 1;  /* PROTO must match */
    if (dispatch(&none) != 1) return 2;
    if (dispatch(&stop) != 3) return 3;

    /* Verify char literal value */
    int val = '\x80';
    if (val != -128) return 4;  /* must be -128, not 128 */

    int val2 = '\xff';
    if (val2 != -1) return 5;  /* must be -1, not 255 */

    return 0;
}
"#;
    assert_eq!(compile_and_run("char_literal_signedness", code, &[]), 0);
}

/// Regression test: static local struct variables must be in static storage,
/// not on the stack. The linearizer checked type modifiers instead of
/// declarator.storage_class for the STATIC flag, which missed struct types.
#[test]
fn codegen_static_local_struct() {
    let code = r#"
#include <stddef.h>

struct Parser {
    int initialized;
    const char *fname;
    struct Parser *next;
};

struct Parser *get_parser(void) {
    static struct Parser p = { .fname = "test" };
    return &p;
}

int main(void) {
    struct Parser *p1 = get_parser();
    struct Parser *p2 = get_parser();

    /* Must return same address (static storage) */
    if (p1 != p2) return 1;

    /* Must NOT be near the stack */
    int stack_var = 0;
    ptrdiff_t diff = (char*)p1 - (char*)&stack_var;
    if (diff < 0) diff = -diff;
    if (diff < 1000000) return 2;  /* on stack = bug */

    /* Value must persist across calls */
    p1->initialized = 42;
    if (get_parser()->initialized != 42) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("static_local_struct", code, &[]), 0);
}

/// Regression test: NaN comparisons must follow IEEE 754.
/// ucomisd sets PF=1 for NaN. Ordered comparisons (==, <, <=) must
/// return false for NaN; != must return true. sete/setb/setbe/setne
/// alone don't check PF, so we need setnp AND / setp OR.
#[test]
fn codegen_nan_comparison() {
    let code = r#"
int main(void) {
    double nan = __builtin_nan("");
    double x = 21.0;

    /* IEEE 754: all ordered comparisons with NaN return false */
    if (nan == nan) return 1;
    if (nan == x) return 2;
    if (nan < x) return 3;
    if (nan <= x) return 4;
    if (nan > x) return 5;
    if (nan >= x) return 6;

    /* != with NaN must return true */
    if (!(nan != nan)) return 7;
    if (!(nan != x)) return 8;

    /* Normal comparisons must still work */
    if (!(1.0 == 1.0)) return 9;
    if (1.0 != 1.0) return 10;
    if (!(1.0 < 2.0)) return 11;
    if (!(1.0 <= 1.0)) return 12;
    if (!(2.0 > 1.0)) return 13;
    if (!(1.0 >= 1.0)) return 14;

    return 0;
}
"#;
    assert_eq!(compile_and_run("nan_comparison", code, &[]), 0);
}

/// Comprehensive NaN test: value comparisons, float (single), stored results,
/// and NaN propagation through variables.
#[test]
fn codegen_nan_comparison_comprehensive() {
    let code = r#"
int main(void) {
    /* Test NaN comparison results as stored integers (not just if-branches) */
    double nan = __builtin_nan("");
    double x = 42.0;
    int r;

    r = (nan == x);  if (r != 0) return 1;
    r = (nan != x);  if (r != 1) return 2;
    r = (nan < x);   if (r != 0) return 3;
    r = (nan <= x);  if (r != 0) return 4;
    r = (nan > x);   if (r != 0) return 5;
    r = (nan >= x);  if (r != 0) return 6;

    /* NaN compared with itself */
    r = (nan == nan); if (r != 0) return 7;
    r = (nan != nan); if (r != 1) return 8;
    r = (nan < nan);  if (r != 0) return 9;
    r = (nan > nan);  if (r != 0) return 10;

    /* Float (single-precision) NaN */
    float fnan = __builtin_nanf("");
    float fy = 42.0f;
    r = (fnan == fy);  if (r != 0) return 11;
    r = (fnan != fy);  if (r != 1) return 12;
    r = (fnan < fy);   if (r != 0) return 13;
    r = (fnan <= fy);  if (r != 0) return 14;

    /* NaN through function call (prevents constant folding) */
    volatile double vnan = nan;
    volatile double vx = x;
    r = (vnan == vx); if (r != 0) return 15;
    r = (vnan != vx); if (r != 1) return 16;

    /* Normal float comparisons must still work */
    float a = 1.0f, b = 2.0f;
    r = (a == a);  if (r != 1) return 17;
    r = (a < b);   if (r != 1) return 18;
    r = (a <= a);  if (r != 1) return 19;
    r = (b > a);   if (r != 1) return 20;
    r = (a >= a);  if (r != 1) return 21;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("nan_comparison_comprehensive", code, &[]),
        0
    );
}

/// Regression test: ++*s++ re-evaluated s++ when storing back, causing the
/// increment to write to the wrong address. The PreInc handler must compute
/// the deref address once before evaluating the operand value.
#[test]
fn codegen_preinc_deref_postinc() {
    let code = r#"
#include <string.h>

int main(void) {
    /* ++*s++: increment char at *s, then advance s */
    char buf[4] = "abc";
    char *s = buf;
    ++*s++;
    *s = 0;
    if (strcmp(buf, "b") != 0) return 1;

    /* --*s++ */
    char buf2[4] = "bcd";
    s = buf2;
    --*s++;
    *s = 0;
    if (strcmp(buf2, "a") != 0) return 2;

    /* Multiple in sequence */
    char buf3[6] = "abcde";
    s = buf3;
    ++*s++;
    ++*s++;
    *s = 0;
    if (strcmp(buf3, "bc") != 0) return 3;

    /* Pointer advancement check */
    char buf4[4] = "xyz";
    s = buf4;
    ++*s++;
    if (s - buf4 != 1) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("preinc_deref_postinc", code, &[]), 0);
}

/// Regression test: struct { double, double } must be passed in XMM registers
/// per SysV AMD64 ABI, and return values in XMM0+XMM1 must be passable
/// directly as arguments to another function taking the same struct type.
#[test]
fn codegen_two_sse_struct_abi() {
    let code = r#"
#include <stdio.h>

typedef struct { double real; double imag; } Complex;

static Complex c_1 = {1.0, 0.0};

Complex identity(Complex x) { return x; }

Complex divide(Complex a, Complex b) {
    double d = b.real*b.real + b.imag*b.imag;
    Complex r = {(a.real*b.real + a.imag*b.imag) / d,
                 (a.imag*b.real - a.real*b.imag) / d};
    return r;
}

/* Chain: divide(c_1, identity(x)) */
Complex reciprocal(Complex x) {
    return divide(c_1, identity(x));
}

int main(void) {
    Complex x = {2.0, 1.0};
    Complex r = reciprocal(x);
    /* 1/(2+i) = (0.4, -0.2) */
    if (r.real != 0.4) return 1;
    if (r.imag != -0.2) return 2;

    /* Direct chaining */
    Complex a = {3.0, 4.0};
    Complex b = divide(a, identity(a));
    if (b.real != 1.0) return 3;
    if (b.imag != 0.0) return 4;

    return 0;
}
"#;
    // Use extra_opts to force -O0 only (optimization needs further work for 2-SSE structs)
    assert_eq!(
        compile_and_run("two_sse_struct_abi", code, &["-O0".to_string()]),
        0
    );
}

// ============================================================================
// Large switch stack reduction test (stack slot reuse)
// ============================================================================

#[test]
fn codegen_large_switch_stack() {
    // 50 switch cases, each with 4 local variables.
    // Without stack slot reuse, this would allocate 200+ unique stack slots.
    // With reuse, case-local temporaries share slots.
    let mut code = String::from(
        r#"
int large_switch(int x) {
    switch (x) {
"#,
    );
    for i in 0..50 {
        let a = i * 4 + 1;
        let b = i * 4 + 2;
        let c = i * 4 + 3;
        let d = i * 4 + 4;
        code.push_str(&format!(
            "    case {i}: {{ int a={a},b={b},c={c},d={d}; return a+b+c+d; }}\n"
        ));
    }
    code.push_str(
        r#"    default: return -1;
    }
}

int main(void) {
"#,
    );
    for i in 0..50 {
        let expected = i * 4 + 1 + i * 4 + 2 + i * 4 + 3 + i * 4 + 4;
        code.push_str(&format!(
            "    if (large_switch({i}) != {expected}) return {ret};\n",
            ret = i + 1
        ));
    }
    code.push_str("    if (large_switch(999) != -1) return 51;\n");
    code.push_str("    return 0;\n}\n");
    assert_eq!(compile_and_run("codegen_large_switch_stack", &code, &[]), 0);
}

/// Regression: FP negation clobbered RAX (used for sign mask), corrupting
/// integer pseudos allocated to RAX. FP ternary select with CMov also
/// failed because CMov doesn't work on XMM registers.
#[test]
fn codegen_fp_ternary_select() {
    let code = r#"
/* FP ternary with negation — tests that fneg doesn't clobber condition */
double select_val(int negate) {
    return negate ? -1.0 : 1.0;
}

double negate_then_select(double v, int flag) {
    double neg = -v;  /* fneg uses RAX for sign mask */
    return flag ? neg : v;
}

int main(void) {
    /* Basic FP ternary */
    if (select_val(0) != 1.0) return 1;
    if (select_val(1) != -1.0) return 2;

    /* FP ternary after negation (fneg clobbers RAX) */
    if (negate_then_select(5.0, 0) != 5.0) return 3;
    if (negate_then_select(5.0, 1) != -5.0) return 4;
    if (negate_then_select(-3.0, 0) != -3.0) return 5;
    if (negate_then_select(-3.0, 1) != 3.0) return 6;

    /* Multiple ternaries in sequence */
    int a = 0, b = 1;
    double x = a ? -2.0 : 2.0;
    double y = b ? -3.0 : 3.0;
    if (x != 2.0) return 7;
    if (y != -3.0) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_fp_ternary_select", code, &[]), 0);
}

/// Regression: two-register integer struct return clobbered rax when
/// second source was allocated there (parallel-move problem).
#[test]
fn codegen_two_reg_int_struct_return() {
    let code = r#"
#include <stdint.h>

typedef struct { uint64_t low; uint64_t high; } u128_t;

static u128_t make_u128(uint64_t lo, uint64_t hi) {
    u128_t r;
    r.low = lo;
    r.high = hi;
    return r;
}

static u128_t add_u128(u128_t a, u128_t b) {
    u128_t r;
    r.low = a.low + b.low;
    uint64_t carry = (r.low < a.low) ? 1 : 0;
    r.high = a.high + b.high + carry;
    return r;
}

int main(void) {
    u128_t a = make_u128(100, 0);
    if (a.low != 100 || a.high != 0) return 1;

    u128_t b = make_u128(200, 0);
    if (b.low != 200 || b.high != 0) return 2;

    u128_t c = add_u128(a, b);
    if (c.low != 300 || c.high != 0) return 3;

    /* Test with carry */
    a = make_u128(0xFFFFFFFFFFFFFFFFULL, 0);
    b = make_u128(1, 0);
    c = add_u128(a, b);
    if (c.low != 0 || c.high != 1) return 4;

    /* Test chained calls — rvalue struct as call argument */
    u128_t total = make_u128(0, 0);
    for (int i = 0; i < 10; i++) {
        total = add_u128(total, make_u128(i, 0));
    }
    if (total.low != 45 || total.high != 0) return 5;

    /* Mixed values */
    c = add_u128(make_u128(7, 3), make_u128(5, 2));
    if (c.low != 12 || c.high != 5) return 6;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_two_reg_int_struct_return", code, &[]),
        0
    );
}

/// Regression test: emit_fp_move Loc::Imm used RAX as scratch, clobbering a live
/// integer value. Fixed by using R10 (reserved scratch) instead.
#[test]
fn codegen_fp_move_no_rax_clobber() {
    let code = r#"
int printf(const char *, ...);

/* Force RAX to hold a live integer value across an FP immediate load.
   The function call returns in RAX, and the subsequent FP operation
   must not clobber it. */
int compute(int x) { return x * 7; }

int main(void) {
    int a = compute(6);   /* a = 42, likely in RAX after call */
    double d = 3.14;      /* FP immediate load — must not clobber a */
    int b = a + 1;        /* uses a — would get wrong value if clobbered */

    if (b != 43) return 1;

    /* Also test with float (32-bit path) */
    int c = compute(10);  /* c = 70, in RAX */
    float f = 2.5f;       /* float immediate — 32-bit Loc::Imm path */
    int e = c + 2;
    if (e != 72) return 2;

    /* Multiple live ints across FP loads */
    int x = compute(3);   /* x = 21 */
    int y = compute(4);   /* y = 28 */
    double d2 = 1.5;
    float f2 = 0.5f;
    if (x + y != 49) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_fp_move_no_rax_clobber", code, &[]), 0);
}

/// Regression test: emit_struct_store used RAX as a data shuttle in the qword
/// copy loop (R10=src, R11=dst, RAX=shuttle). If a live pseudo was in RAX,
/// it got clobbered. Fixed by using XMM15 (reserved scratch) as shuttle.
#[test]
fn codegen_struct_store_no_rax_clobber() {
    let code = r#"
typedef struct { long a; long b; long c; } Triple;

Triple make_triple(long x, long y, long z) {
    Triple t;
    t.a = x; t.b = y; t.c = z;
    return t;
}

int compute(int x) { return x * 11; }

int main(void) {
    /* Get a value in RAX from a call, then do a struct copy */
    int val = compute(5);   /* val = 55, likely in RAX */
    Triple t = make_triple(1, 2, 3);  /* struct store — must not clobber val */
    int check = val + 1;
    if (check != 56) return 1;
    if (t.a != 1 || t.b != 2 || t.c != 3) return 2;

    /* Chain: struct copy while multiple ints are live */
    int a = compute(3);  /* 33 */
    int b = compute(4);  /* 44 */
    Triple t2 = make_triple(10, 20, 30);
    if (a + b != 77) return 3;
    if (t2.a != 10 || t2.b != 20 || t2.c != 30) return 4;

    /* Struct assignment (copy, not return) */
    Triple t3;
    t3 = t2;
    if (t3.a != 10 || t3.b != 20 || t3.c != 30) return 5;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_struct_store_no_rax_clobber", code, &[]),
        0
    );
}

/// Regression test: stack coloring with interference-graph approach.
/// Tests that variables with non-contiguous live ranges in complex control flow
/// (gotos creating non-linear block ordering) correctly share or don't share slots
/// based on actual block-level liveness.
#[test]
fn codegen_stack_coloring_interference() {
    let code = r#"
int printf(const char *, ...);

/* Volatile to prevent optimization */
volatile int sink;

int main(void) {
    /* Test 1: Non-overlapping locals can share stack space */
    {
        int a = 10;
        sink = a;
    }
    {
        int b = 20;
        sink = b;
    }
    if (sink != 20) return 1;

    /* Test 2: Overlapping locals must NOT share stack space.
       Use a loop with values that persist across iterations. */
    int total = 0;
    for (int i = 0; i < 10; i++) {
        int x = i * 3;
        int y = i * 7;
        total += x + y;
    }
    /* sum(i*10, i=0..9) = 10*45 = 450 */
    if (total != 450) return 2;

    /* Test 3: Goto creating non-linear control flow */
    int result = 0;
    int phase = 0;
    goto start;

mid:
    result += 100;
    phase = 2;
    goto done;

start:
    result = 42;
    phase = 1;
    goto mid;

done:
    if (result != 142) return 3;
    if (phase != 2) return 4;

    /* Test 4: Switch with fallthrough — complex CFG */
    int sw_result = 0;
    for (int i = 0; i < 4; i++) {
        int temp = i * 5;
        switch (i) {
            case 0: sw_result += temp + 1; break;
            case 1: sw_result += temp + 2; break;
            case 2: sw_result += temp + 3; break;
            default: sw_result += temp + 4; break;
        }
    }
    /* (0+1) + (5+2) + (10+3) + (15+4) = 1+7+13+19 = 40 */
    if (sw_result != 40) return 5;

    /* Test 5: Large number of locals to exercise slot reuse */
    int sum = 0;
    for (int i = 0; i < 20; i++) {
        int v0 = i;
        int v1 = i + 1;
        int v2 = i + 2;
        int v3 = i + 3;
        sum += v0 + v1 + v2 + v3;
    }
    /* sum = sum(4i+6, i=0..19) = 4*190+120 = 880 */
    if (sum != 880) return 6;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_stack_coloring_interference", code, &[]),
        0
    );
}

/// Regression test: two-SSE struct (e.g., Py_complex {double, double}) passed as
/// a call argument when the address pseudo is spilled to stack. The Loc::Stack
/// case used LEA (address of stack slot) instead of MOV (load pointer from stack
/// slot), producing garbage values.
#[test]
fn codegen_two_sse_struct_arg_spilled() {
    let code = r#"
typedef struct { double real; double imag; } Complex;

/* Prevent inlining so the struct goes through the ABI */
__attribute__((noinline))
Complex make_complex(double r, double i) {
    Complex c;
    c.real = r;
    c.imag = i;
    return c;
}

__attribute__((noinline))
Complex add_complex(Complex a, Complex b) {
    Complex c;
    c.real = a.real + b.real;
    c.imag = a.imag + b.imag;
    return c;
}

__attribute__((noinline))
int check_complex(Complex c, double expect_real, double expect_imag) {
    if (c.real != expect_real) return 1;
    if (c.imag != expect_imag) return 1;
    return 0;
}

int main(void) {
    /* Basic: make and check */
    Complex a = make_complex(1.0, 2.0);
    if (check_complex(a, 1.0, 2.0)) return 1;

    /* Chain: result of one call passed to another — triggers spill */
    Complex b = make_complex(3.0, 4.0);
    Complex c = add_complex(a, b);
    if (check_complex(c, 4.0, 6.0)) return 2;

    /* Multiple live complex values to force spills */
    Complex d = make_complex(10.0, 20.0);
    Complex e = make_complex(30.0, 40.0);
    Complex f = add_complex(d, e);
    if (check_complex(f, 40.0, 60.0)) return 3;

    /* Verify earlier values weren't corrupted */
    if (check_complex(a, 1.0, 2.0)) return 4;
    if (check_complex(b, 3.0, 4.0)) return 5;

    /* Triple chain */
    Complex g = add_complex(add_complex(a, b), c);
    if (check_complex(g, 8.0, 12.0)) return 6;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_two_sse_struct_arg_spilled", code, &[]),
        0
    );
}

/// Regression test: float arguments to variadic functions (e.g., printf) were
/// not promoted to double per C99 6.5.2.2p7 "default argument promotions".
/// The ABI requires xmm0 to hold a double, but pcc passed 32-bit float bits.
#[test]
fn codegen_variadic_float_promotion() {
    let code = r#"
#include <stdio.h>
#include <string.h>

int main(void) {
    char buf[64];
    float f = 3.14f;

    /* snprintf with %f — float must be promoted to double */
    snprintf(buf, sizeof(buf), "%.2f", f);
    if (strcmp(buf, "3.14") != 0) return 1;

    /* FLT_MAX equivalent (avoid float.h dependency) */
    float big = 3.40282346638528859811704183484516925440e+38f;
    snprintf(buf, sizeof(buf), "%.0e", big);
    /* Should be "3e+38" not "0e+00" */
    if (buf[0] != '3') return 2;

    /* Multiple float args in variadic call */
    float a = 1.5f, b = 2.5f;
    snprintf(buf, sizeof(buf), "%.1f,%.1f", a, b);
    if (strcmp(buf, "1.5,2.5") != 0) return 3;

    /* Mixed int and float in variadic */
    snprintf(buf, sizeof(buf), "%d,%.1f,%d", 42, a, 99);
    if (strcmp(buf, "42,1.5,99") != 0) return 4;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_variadic_float_promotion", code, &[]),
        0
    );
}

/// Regression test: small struct (<=64 bits) returned by value from a function
/// was stored as a raw register value. When assigned to a struct variable,
/// emit_assign's block_copy dereferenced it as a pointer → SIGSEGV.
/// Fixed by allocating local storage for small struct returns.
#[test]
fn codegen_small_struct_return() {
    let code = r#"
typedef struct { int a; int b; } Pair;

__attribute__((noinline))
Pair make_pair(int x, int y) {
    Pair p;
    p.a = x;
    p.b = y;
    return p;
}

__attribute__((noinline))
int sum_pair(Pair p) {
    return p.a + p.b;
}

int main(void) {
    /* Basic: return small struct and access fields */
    Pair p = make_pair(10, 20);
    if (p.a != 10) return 1;
    if (p.b != 20) return 2;

    /* Pass returned struct to another function */
    int s = sum_pair(make_pair(3, 7));
    if (s != 10) return 3;

    /* Assign return value to existing variable */
    Pair q;
    q = make_pair(100, 200);
    if (q.a != 100 || q.b != 200) return 4;

    /* Chain: use result in expression */
    Pair r = make_pair(make_pair(1, 2).a + 3, make_pair(4, 5).b + 6);
    if (r.a != 4 || r.b != 11) return 5;

    /* Single-field struct (common in error handling) */
    typedef struct { int err; } ErrCode;
    ErrCode e;
    e = (ErrCode){42};
    if (e.err != 42) return 6;

    /* Struct with two shorts (fits in single register) */
    typedef struct { short x; short y; } Point;
    Point pt;
    pt = (Point){100, 200};
    if (pt.x != 100 || pt.y != 200) return 7;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_small_struct_return", code, &[]),
        0
    );
}

/// Regression test: FP binary operations (FMul, FDiv, etc.) clobbered src2
/// when src2 was in the same XMM register as dst_xmm (Xmm0 for stack targets).
/// emit_fp_move(src1, Xmm0) overwrote src2 before the operation.
/// Manifested as `x *= scale` computing `x * x` instead of `x * scale`.
#[test]
fn codegen_fp_binop_src2_clobber() {
    let code = r#"
#include <math.h>

/* Force enough register pressure that scale ends up in Xmm0 */
__attribute__((noinline))
double vector_norm_mini(int n, double *vec, double max) {
    double x, scale, csum = 1.0, frac1 = 0.0;
    int max_e;

    frexp(max, &max_e);
    scale = ldexp(1.0, -max_e);

    for (int i = 0; i < n; i++) {
        x = vec[i];
        x *= scale;  /* Bug: became x *= x when scale was in Xmm0 */
        double sq = x * x;
        csum += sq;
        frac1 += sq * 0.001;
    }
    double h = sqrt(csum - 1.0 + frac1);
    return h / scale;
}

int main(void) {
    double vec[] = {3.0, 4.0};
    double r = vector_norm_mini(2, vec, 4.0);
    /* Expected: sqrt((3/8)^2 + (4/8)^2 + frac) / (1/8) ≈ 5.0 */
    if (r < 4.9 || r > 5.1) return 1;

    double vec2[] = {5.0, 12.0};
    r = vector_norm_mini(2, vec2, 12.0);
    if (r < 12.9 || r > 13.1) return 2;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_fp_binop_src2_clobber", code, &["-lm".to_string()]),
        0
    );
}

/// Regression test: va_start overflow_arg_area didn't skip past fixed params
/// that were passed on the stack (>6 int params). va_arg read the last fixed
/// param as the first variadic arg, producing garbage values.
#[test]
fn codegen_va_start_stack_overflow_params() {
    let code = r#"
#include <stdarg.h>
#include <stdio.h>

/* 7 fixed int params — 6 in registers, 1 on stack. The variadic arg is the 8th. */
__attribute__((noinline))
int fmt7(int a, int b, int c, int d, int e, int f, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int val = va_arg(ap, int);
    va_end(ap);
    return val;
}

/* 8 fixed int params — 6 in regs, 2 on stack */
__attribute__((noinline))
int fmt8(int a, int b, int c, int d, int e, int f, int g, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int val = va_arg(ap, int);
    va_end(ap);
    return val;
}

int main(void) {
    /* Test 7 fixed params + 1 variadic */
    int r = fmt7(1, 2, 3, 4, 5, 6, "%c", 42);
    if (r != 42) return 1;

    /* Test 8 fixed params + 1 variadic */
    r = fmt8(1, 2, 3, 4, 5, 6, 7, "%c", 99);
    if (r != 99) return 2;

    /* Test with snprintf-like pattern (7 params) */
    char buf[64];
    snprintf(buf, sizeof(buf), "%d", fmt7(10, 20, 30, 40, 50, 60, "test", 777));
    if (buf[0] != '7') return 3;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_va_start_stack_overflow_params", code, &[]),
        0
    );
}
