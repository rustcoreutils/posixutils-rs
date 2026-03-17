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
