//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Types and expressions: typeof, sizeof, qualifiers, conversions and
// the operators that read them.
//

use crate::common::{
    compile_and_run, compile_and_run_everywhere, compile_and_run_optimized, create_c_file,
};
use plib::testing::run_test_base;

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
        "c17",
        &[
            "-S".to_string(),
            "-o".to_string(),
            "-".to_string(),
            c_path.to_string_lossy().to_string(),
        ],
        &[],
    );

    assert!(
        output.status.success(),
        "c17 -S -o - failed: {}",
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
    // The rules are part of the unwind tables, not of -g: without them the
    // brackets describe a frame that does not exist.
    assert!(
        asm.contains(".cfi_def_cfa"),
        "Missing .cfi_def_cfa in assembly output without -g"
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
        "c17",
        &[
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
        "c17 -S -o - --fno-unwind-tables failed: {}",
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

/// Aggregate copies, atomics, compound literals and short-circuit
/// conditions under the optimizer, one program run at -O1
/// (`compile_and_run_optimized`).
///
/// Consolidates these tests, one section each (each original `main` is
/// a `noinline` section function; the program exits with the section's
/// base plus the original code):
/// - `codegen_large_struct_member_copy`: 1..=9
/// - `codegen_atomic_cas_register_clobbering`: 10..=20
/// - `codegen_large_struct_array_copy`: 21..=26
/// - `codegen_compound_literal_zero_init`: 27..=33
/// - `codegen_conditional_short_circuit`: 34..=35
#[test]
fn codegen_types_exprs_optimized_mega() {
    let code = r#"
/* ---- codegen_large_struct_member_copy: exits 1..9
 * Test: Large struct member copy from global variable
 * Regression test for bug where accessing a large struct member (size > 64 bits)
 * from a global variable would generate an extra dereference, treating the
 * struct address as a pointer to dereference rather than the struct itself.
 */
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

static __attribute__((noinline)) int t_large_struct_member_copy(void)
{
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
#undef _MyMem_Raw

/* ---- codegen_atomic_cas_register_clobbering: exits 10..20
 * Test for atomic compare-and-swap register clobbering bug.
 * The bug: when regalloc assigns CAS operands to registers R9/R10/R11/RAX,
 * loading one operand can clobber another before it's used.
 * This test uses inline functions to trigger the problematic register allocation.
 */
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

static __attribute__((noinline)) int t_atomic_cas_register_clobbering(void)
{
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
#undef UNLOCKED
#undef LOCKED

/* ---- codegen_large_struct_array_copy: exits 21..26
 * Regression test for bug where copying a large struct (> 64 bits) from an
 * array element would incorrectly dereference the first field as a pointer
 * instead of doing a proper block copy.
 * Bug: `struct pair item = array[0];` would crash when struct has 2+ pointers.
 */
#include <stdio.h>

struct pair {
    void *ptr;
    const char *str;
};

static struct pair pairs[] = {
    {(void*)0xDEADBEEF, "First"},
    {(void*)0xCAFEBABE, "Second"},
};

static __attribute__((noinline)) int t_large_struct_array_copy(void)
{
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

/* ---- codegen_compound_literal_zero_init: exits 27..33
 * Compound literal zero-initialization test
 * C99 6.7.8p21: Fields not explicitly initialized in a compound literal
 * must be zero-initialized. Bug: *p = (struct S){.a = val} left .b and .c
 * as garbage instead of zero.
 */
typedef long to4_int64_t;
void *malloc(unsigned long);
void free(void *);
int printf(const char *, ...);
#define to4_NULL ((void*)0)

typedef int (*func_ptr)(void);

struct cached_m_dict {
    void *copied;
    to4_int64_t extra;
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
    to4_int64_t m_index;                // offset 16: 8 bytes
    cached_m_dict_t m_dict;         // offset 24: 8 bytes (pointer)
    struct cached_m_dict _m_dict;   // offset 32: 16 bytes (embedded struct)
    origin_t origin;                // offset 48: 4 bytes
};

static __attribute__((noinline)) int t_compound_literal_zero_init(void)
{
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
        .m_init = to4_NULL,
        .m_index = 1,
        .origin = ORIGIN_CORE,
    };
    
    // Check explicitly initialized fields
    if (v->def != (void*)0x1234) {
        printf("FAIL: v->def = %p, expected 0x1234\n", v->def);
        return 1;
    }
    if (v->m_init != to4_NULL) {
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
    if (v->m_dict != to4_NULL) {
        printf("FAIL: v->m_dict = %p, expected NULL (should be zero-init)\n", 
               (void*)v->m_dict);
        return 5;
    }
    if (v->_m_dict.copied != to4_NULL) {
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
#undef to4_NULL

/* ---- codegen_conditional_short_circuit: exits 34..35
 * Ternary conditional expressions with pointer dereference must use short-circuit
 * evaluation. Bug: `value = ptr == NULL ? 0 : ptr->x` would evaluate `ptr->x`
 * unconditionally, causing a crash when `ptr` is NULL because the compiler
 * incorrectly used a select instruction (cmov) instead of proper branching.
 */
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

static __attribute__((noinline)) int t_conditional_short_circuit(void)
{
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

int main(void)
{
    int r;
    if ((r = t_large_struct_member_copy()) != 0) return r;
    if ((r = t_atomic_cas_register_clobbering()) != 0) return 9 + r;
    if ((r = t_large_struct_array_copy()) != 0) return 20 + r;
    if ((r = t_compound_literal_zero_init()) != 0) return 26 + r;
    if ((r = t_conditional_short_circuit()) != 0) return 33 + r;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run_optimized("types_exprs_optimized_mega", code),
        0
    );
}

// ============================================================================
// 32/64-bit type width audit tests
// ============================================================================

/// Integer types, promotions, sizing and expression forms, one program
/// run at the compile matrix levels.
///
/// Consolidates these tests, one section each (each original `main` is
/// a `noinline` section function; the program exits with the section's
/// base plus the original code):
/// - `codegen_enum_is_integer`: 1..=5
/// - `codegen_unary_neg_promotion`: 6..=9
/// - `codegen_bitfield_64bit_storage`: 10..=13
/// - `codegen_type_based_sizing_64bit`: 14..=65
/// - `codegen_vla_sizeof_mul_type`: 66..=96
/// - `codegen_string_literal_high_bytes`: 97..=129
/// - `codegen_ternary_common_type`: 130..=133
/// - `codegen_preinc_deref_postinc`: 134..=137
/// - `codegen_abi_dispatch_medium_struct`: 138..=144
#[test]
fn codegen_types_exprs_mega() {
    let code = r#"
/* ---- codegen_enum_is_integer: exits 1..5
 * Test: enum is treated as integer for pointer arithmetic and is_integer checks
 */
enum Color { RED, GREEN, BLUE };

int arr[] = {10, 20, 30};

int get_via_enum_ptr(int *p, enum Color c) {
    return *(p + c);
}

static __attribute__((noinline)) int t_enum_is_integer(void)
{
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

/* ---- codegen_unary_neg_promotion: exits 6..9
 * Test: unary minus applies integer promotion (char/short -> int)
 */
static __attribute__((noinline)) int t_unary_neg_promotion(void)
{
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

/* ---- codegen_bitfield_64bit_storage: exits 10..13
 * Test: signed bitfield in 64-bit storage unit
 */
struct Wide {
    long a : 40;
    long b : 20;
};

static __attribute__((noinline)) int t_bitfield_64bit_storage(void)
{
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

/* ---- codegen_type_based_sizing_64bit: exits 14..65
 */
long identity(long x) { return x; }

static __attribute__((noinline)) int t_type_based_sizing_64bit(void)
{
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

/* ---- codegen_vla_sizeof_mul_type: exits 66..96
 */
static __attribute__((noinline)) int t_vla_sizeof_mul_type(void)
{
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

/* ---- codegen_string_literal_high_bytes: exits 97..129
 */
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

static __attribute__((noinline)) int t_string_literal_high_bytes(void)
{
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

/* ---- codegen_ternary_common_type: exits 130..133
 */
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

static __attribute__((noinline)) int t_ternary_common_type(void)
{
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

/* ---- codegen_preinc_deref_postinc: exits 134..137
 * Regression test: ++*s++ re-evaluated s++ when storing back, causing the
 * increment to write to the wrong address. The PreInc handler must compute
 * the deref address once before evaluating the operand value.
 */
#include <string.h>

static __attribute__((noinline)) int t_preinc_deref_postinc(void)
{
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

/* ---- codegen_abi_dispatch_medium_struct: exits 138..144
 */
struct TwoLongs {
    long a;
    long b;
};

long sum_two_longs(struct TwoLongs s) {
    return s.a + s.b;
}

struct TwoLongs make_two_longs(long a, long b) {
    struct TwoLongs s;
    s.a = a;
    s.b = b;
    return s;
}

struct IntDouble {
    int i;
    double d;
};

double get_sum(struct IntDouble s) {
    return (double)s.i + s.d;
}

struct IntDouble make_id(int i, double d) {
    struct IntDouble s;
    s.i = i;
    s.d = d;
    return s;
}

static __attribute__((noinline)) int t_abi_dispatch_medium_struct(void)
{
    // Test passing a two-GP-register struct as parameter
    struct TwoLongs s = { 15, 25 };
    long r = sum_two_longs(s);
    if (r != 40) return 1;

    // Test returning a two-GP-register struct
    struct TwoLongs s2 = make_two_longs(100, 200);
    if (s2.a != 100) return 2;
    if (s2.b != 200) return 3;

    // Test struct from return passed directly to param
    long r2 = sum_two_longs(make_two_longs(1000, 2000));
    if (r2 != 3000) return 4;

    // Test mixed int+double struct
    struct IntDouble id = { 5, 7.5 };
    double dr = get_sum(id);
    if (dr != 12.5) return 5;

    // Test returning mixed struct
    struct IntDouble id2 = make_id(10, 20.5);
    if (id2.i != 10) return 6;
    if (id2.d != 20.5) return 7;

    return 0;
}

int main(void)
{
    int r;
    if ((r = t_enum_is_integer()) != 0) return r;
    if ((r = t_unary_neg_promotion()) != 0) return 5 + r;
    if ((r = t_bitfield_64bit_storage()) != 0) return 9 + r;
    if ((r = t_type_based_sizing_64bit()) != 0) return 13 + r;
    if ((r = t_vla_sizeof_mul_type()) != 0) return 65 + r;
    if ((r = t_string_literal_high_bytes()) != 0) return 96 + r;
    if ((r = t_ternary_common_type()) != 0) return 129 + r;
    if ((r = t_preinc_deref_postinc()) != 0) return 133 + r;
    if ((r = t_abi_dispatch_medium_struct()) != 0) return 137 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("types_exprs_mega", code, &[]), 0);
}

// ============================================================================
// Type-based size derivation: verify 64-bit width through binop, unary, mul,
// div, compare, and select when insn.typ carries the authoritative width.
// Values above 2^32 prove no 32-bit truncation occurs.
// ============================================================================

// ============================================================================
// VLA sizeof with multi-dimensional arrays — exercises VLA Mul instructions
// that now carry .with_type(ulong_id) for correct 64-bit multiplication.
// ============================================================================

// ============================================================================
// Regression: string literal bytes >= 0x80 must not be UTF-8 encoded
// ============================================================================

// ============================================================================
// Regression: ternary result type must be common type of both branches
// ============================================================================

// ============================================================================
// Test: ABI dispatch for medium struct params (Phase 0 correctness fix)
// ============================================================================

/// Logical/shift immediates and signed bit-fields, one program run at
/// the compile matrix levels and at -O1 (`compile_and_run_optimized`).
///
/// Consolidates these tests, one section each (each original `main` is
/// a `noinline` section function; the program exits with the section's
/// base plus the original code):
/// - `codegen_logical_and_shift_immediates_are_encodable`: 1..=14
/// - `codegen_signed_bitfield_extends_to_its_declared_type`: 15..=27
#[test]
fn codegen_immediates_and_bitfields_mega() {
    let code = r#"
/* ---- codegen_logical_and_shift_immediates_are_encodable: exits 1..14
 * AArch64 gives each instruction family its own immediate encoding, and c17
 * tested one 0..=4095 range for all of them. `add`/`sub` do take a 12-bit
 * unsigned value, but the logical operations take a *bitmask* immediate that
 * cannot represent 0 at all, and a shift takes an amount below the operand
 * width — so `x & 0` emitted `and w1, w1, #0`, `x | 1000` emitted
 * `orr w1, w1, #1000`, and `x << 32` emitted `lsl w1, w1, #32`, each of which
 * the assembler refuses. The build failed; nothing was miscompiled.
 *
 * Pre-existing, and invisible to this suite because `compile_and_run` targets
 * the host. Found by the aarch64 sweep after it was taught that a program
 * c17 cannot build, where gcc can, is a defect rather than something to skip.
 */
unsigned u32and(unsigned x, unsigned m) { return x & m; }

static __attribute__((noinline)) int t_logical_and_shift_immediates_are_encodable(void)
{
    unsigned x = 0xFFFFu;

    /* Zero has no bitmask encoding on aarch64. */
    if ((x & 0u) != 0u) return 1;
    if ((x | 0u) != 0xFFFFu) return 2;
    if ((x ^ 0u) != 0xFFFFu) return 3;

    /* Ordinary numbers mostly have none either. */
    if ((x & 1000u) != (0xFFFFu & 1000u)) return 4;
    if ((x | 3000u) != (0xFFFFu | 3000u)) return 5;
    if ((x ^ 1000u) != (0xFFFFu ^ 1000u)) return 6;

    /* The ones that do encode must still work. */
    if ((x & 255u) != 255u) return 7;
    if ((x & 4095u) != 4095u) return 8;
    if ((x | 0xF0F0F0F0u) != (0xFFFFu | 0xF0F0F0F0u)) return 9;

    /* Shift amounts at and beyond the width. 6.5.7p3 leaves the value
       undefined, but the compiler must still emit something assemblable. */
    unsigned s = 1u;
    if ((s << 31) != 0x80000000u) return 10;

    /* 64-bit logical immediates, encodable and not. */
    unsigned long q = ~0UL;
    if ((q & 0xFFFFFFFF00000000UL) != 0xFFFFFFFF00000000UL) return 11;
    if ((q & 0x0123456789ABCDEFUL) != 0x0123456789ABCDEFUL) return 12;
    if ((0UL | 0x5555555555555555UL) != 0x5555555555555555UL) return 13;

    /* And through a call, so the operand is a register on both sides. */
    if (u32and(x, 0u) != 0u) return 14;
    return 0;
}

/* ---- codegen_signed_bitfield_extends_to_its_declared_type: exits 15..27
 * A signed bit-field sign-extends to its declared type's width.
 *
 * The extension was measured against the *access unit* the field is carried
 * in. For a packed `long long a:4` that unit is one byte, so the value was
 * extended only as far as the unit reached and -1 came back 4294967295 with
 * `a < 0` false. Where the field exactly filled its unit the guard was false
 * outright and no extension was emitted at all.
 */
#pragma pack(1)
struct Packed {
    long long a : 4;
    long long b : 20;
    int c : 3;
};
#pragma pack()

struct Plain {
    int a : 4;
    long long b : 40;
    short c : 9;
    signed char d : 8;
    __int128 e : 27;
};

static __attribute__((noinline)) int t_signed_bitfield_extends_to_its_declared_type(void)
{
    struct Packed p;
    p.a = -1; p.b = -1; p.c = -1;
    if (p.a != -1 || !(p.a < 0)) return 1;
    if (p.b != -1 || !(p.b < 0)) return 2;
    if (p.c != -1 || !(p.c < 0)) return 3;
    p.a = -8; p.b = -524288;
    if (p.a != -8 || p.b != -524288) return 4;
    p.a = 7; p.b = 524287;
    if (p.a != 7 || p.b != 524287) return 5;

    struct Plain q;
    q.a = -1; q.b = -1; q.c = -1; q.d = -1; q.e = -1;
    if (q.a != -1 || !(q.a < 0)) return 6;
    if (q.b != -1 || !(q.b < 0)) return 7;
    if (q.c != -1 || !(q.c < 0)) return 8;
    if (q.d != -1 || !(q.d < 0)) return 9;
    if (q.e != -1 || !(q.e < 0)) return 10;

    /* A field that exactly fills its unit still sign-extends. */
    q.d = -128;
    if (q.d != -128) return 11;

    /* Unsigned fields are unaffected. */
    struct U { unsigned x : 4; unsigned long long y : 40; } u;
    u.x = 15; u.y = 0xFFFFFFFFFFULL;
    if (u.x != 15u) return 12;
    if (u.y != 0xFFFFFFFFFFULL) return 13;

    return 0;
}

int main(void)
{
    int r;
    if ((r = t_logical_and_shift_immediates_are_encodable()) != 0) return r;
    if ((r = t_signed_bitfield_extends_to_its_declared_type()) != 0) return 14 + r;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("immediates_and_bitfields_mega", code, &[]),
        0
    );
    assert_eq!(
        compile_and_run_optimized("immediates_and_bitfields_mega_opt", code),
        0
    );
}

/// Unary `-` and `~` perform the integer promotions on their operand
/// (C17 6.5.3.3p3, p4), and the *value* has to be converted, not just the
/// result type.
///
/// `promote_unary_operand` computed the promoted type and handed the operand
/// back untouched, so `-(signed char)200` reached the IR as `neg.32` over an
/// eight-bit value with no extension between them. The backend's move
/// widens without a sign, which is right for `unsigned char` and silently
/// wrong for a signed one: the answer was -200 where C says 56.
///
/// A variable operand hid it, because loading one already knows the type --
/// only a narrowing cast applied directly to the operand reaches the shape.
#[test]
fn codegen_unary_operators_promote_their_operand() {
    let code = r#"
extern void abort(void);

volatile int sink;

int main(void)
{
    /* The shape that was wrong: a narrowing cast straight under the
       operator, with nothing in between to carry the sign. */
    if (-(signed char) 200 != 56) abort();
    if (~(signed char) 200 != 55) abort();
    if (-(signed char) -56 != 56) abort();
    if (-(short) 40000 != 25536) abort();
    if (~(short) 40000 != 25535) abort();

    /* The unsigned forms, which were already right and must stay so. */
    if (-(unsigned char) 200 != -200) abort();
    if (~(unsigned char) 200 != -201) abort();
    if (-(unsigned short) 40000 != -40000) abort();

    /* Through a variable, and through a value the optimizer cannot know:
       all three spellings must agree. */
    {
        signed char v = (signed char) 200;
        sink = 200;
        signed char r = (signed char) sink;
        if (-v != 56) abort();
        if (-r != 56) abort();
        if (-v != -(signed char) 200) abort();
        if (~v != ~(signed char) 200) abort();
    }
    {
        short v = (short) 40000;
        sink = 40000;
        short r = (short) sink;
        if (-v != 25536) abort();
        if (-r != 25536) abort();
        if (-v != -(short) 40000) abort();
    }

    /* `_Bool` and plain `char` promote too. Plain `char`'s signedness is the
       target's business, so this asserts the agreement rather than a number. */
    if (-(_Bool) 1 != -1) abort();
    if (~(_Bool) 1 != -2) abort();
    {
        sink = 0xEF;
        char v = (char) sink;
        if (-(char) 0xEF != -v) abort();
        if (~(char) 0xEF != ~v) abort();
    }

    return 0;
}
"#;
    for opt in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run("c17_unary_promotion", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// Variably modified types, `typeof`, decays and static initializers, one
/// program run on the host at the matrix levels, -O0 and -O2, and on
/// aarch64 at -O0 and -O2.
///
/// Consolidates these tests, one section each (each original `main` is
/// a `noinline` section function; the program exits with the section's
/// base plus the original code):
/// - `codegen_typeof_of_variably_modified_object`: 1..=71
/// - `codegen_sizeof_parenthesized_operand_takes_postfix_operators`: 72..=76
/// - `codegen_typeof_vla_declaration_keeps_its_extent`: 77..=96
/// - `codegen_typedef_name_after_type_specifier_is_declared`: 97..=99
/// - `codegen_vla_extent_inside_a_grouped_declarator`: 100..=101
/// - `codegen_function_and_array_convert_from_their_address`: 102..=130
/// - `codegen_decaying_operand_converts_to_bool_from_its_address`: 131..=147
/// - `codegen_const_subobject_folds_in_a_static_initializer`: 148..=149
#[test]
fn codegen_types_exprs_everywhere_mega() {
    let code = r#"
/* ---- codegen_typeof_of_variably_modified_object: exits 1..71
 * `typeof` of an expression whose type is variably modified names that type
 * with the extents its object was declared with. The type alone is `int[]`
 * -- every VLA type interns to one `TypeId` -- so `typeof(v) w;` gave `w` an
 * incomplete type and no storage, and was then rejected as "array size
 * missing". The extents are the object's, fixed when it was declared; the
 * operand is evaluated (once per declarator, as gcc does) only when it is
 * variably modified, and so is `sizeof`'s (C17 6.5.3.4p2), which c17 skipped.
 */
static int g(int n, int (*a)[n], typeof(a) b, typeof(*a) *c)
{
    if ((char *)(b + 1) - (char *)b != n * (long)sizeof(int)) return 1;
    if ((char *)(c + 1) - (char *)c != n * (long)sizeof(int)) return 2;
    if (b[1][2] != a[1][2]) return 3;
    return 0;
}

static int f(int n)
{
    int v[n];
    typeof(v) w;                    /* int[n], fixed at v's declaration */
    n = 100;
    typeof(v) *p = &w;
    if (sizeof w != 5 * sizeof(int)) return 10;
    if (sizeof *p != 5 * sizeof(int)) return 11;
    if ((char *)(p + 1) - (char *)p != 5 * (long)sizeof(int)) return 12;
    typeof(w) w2;                   /* typeof of a typeof-declared VLA */
    if (sizeof w2 != 5 * sizeof(int)) return 13;
    typeof(v) a3[3];                /* int[3][5] */
    if (sizeof a3 != 15 * sizeof(int)) return 14;
    if ((char *)&a3[1] - (char *)&a3[0] != 5 * (long)sizeof(int)) return 15;

    int m[n / 50][n / 25][3];       /* 2 x 4 x 3 */
    typeof(m) m2;
    typeof(m[0]) row;
    typeof(m[1][2]) cell;           /* int[3]: constant */
    if (sizeof m2 != 24 * sizeof(int)) return 20;
    if (sizeof row != 12 * sizeof(int)) return 21;
    if (sizeof cell != 3 * sizeof(int)) return 22;
    for (int i = 0; i < 2; i++)
        for (int j = 0; j < 4; j++)
            for (int k = 0; k < 3; k++)
                m2[i][j][k] = i * 100 + j * 10 + k;
    if (m2[1][3][2] != 132 || m2[0][2][1] != 21) return 23;

    int (*pv)[n / 10] = 0;          /* pointee int[10] */
    typeof(*pv) x;
    typeof(pv) q = pv;
    typeof(v + 1) r = v;            /* int *: no extent */
    if (sizeof x != 10 * sizeof(int)) return 30;
    if ((char *)(q + 1) - (char *)q != 10 * (long)sizeof(int)) return 31;
    if ((char *)(r + 1) - (char *)r != (long)sizeof(int)) return 32;

    /* A variably modified operand is evaluated, as gcc does. */
    int i = 0, j = 0, k = 0, a = 0;
    typeof(pv[i++]) y;
    if (i != 1 || sizeof y != 10 * sizeof(int)) return 40;
    if (sizeof(typeof(pv[j++])) != 10 * sizeof(int) || j != 1) return 41;
    if (sizeof(pv[k++]) != 10 * sizeof(int) || k != 1) return 42;
    if (sizeof(m[a++]) != 12 * sizeof(int) || a != 1) return 43;
    /* ...and a constant one is not. */
    int c = 0;
    typeof(m[1][c++]) z;
    if (c != 0 || sizeof z != 3 * sizeof(int)) return 44;

    /* gcc evaluates the operand once per declarator, not once per
       declaration as it does `typeof(int[n++])`. */
    int e = 0;
    typeof(pv[e++]) d1, *d2;
    if (e != 2 || sizeof d1 != sizeof *d2) return 45;
    typedef typeof(pv[e++]) T;
    T t1, t2;
    if (e != 3 || sizeof t1 != sizeof t2 || sizeof t2 != 10 * sizeof(int)) return 46;

    typedef int (*P)[n / 20];       /* pointer to int[5] */
    P pp = 0;
    if ((char *)(pp + 1) - (char *)pp != 5 * (long)sizeof(int)) return 50;

    for (int t = 0; t < 5; t++) w[t] = t * 3;
    int sum = 0;
    for (int t = 0; t < 5; t++) sum += (*p)[t];
    if (sum != 30) return 60;

    int grid[3][n / 25];            /* 3 x 4 */
    grid[1][2] = 7;
    return g(n / 25, grid, grid, grid);
}

/* A static pointer to a VLA takes its pointee's extents each time its
   declaration is reached. */
static int h(int n)
{
    int rows[2][n];
    static int (*sp)[n];
    sp = rows;
    if (sizeof *sp != n * sizeof(int)) return 70;
    if ((char *)(sp + 1) - (char *)sp != n * (long)sizeof(int)) return 71;
    return 0;
}

static __attribute__((noinline)) int t_typeof_of_variably_modified_object(void)
{
    int r = h(3);
    if (r == 0) r = h(7);
    return r ? r : f(5);
}

/* ---- codegen_sizeof_parenthesized_operand_takes_postfix_operators: exits 72..76
 * `sizeof ( expression )` is `sizeof` of a unary expression, and the
 * parenthesized expression is only its primary: `sizeof (a)[0]` measures
 * `a[0]`. c17 stopped at the `)` and rejected the `[`.
 */
struct S { int x[3]; } s;
int a[10];
int *te2_f(void) { return a; }
static __attribute__((noinline)) int t_sizeof_parenthesized_operand_takes_postfix_operators(void)
{
    if (sizeof (a)[0] != sizeof(int)) return 1;
    if (sizeof (s).x != 3 * sizeof(int)) return 2;
    if (_Alignof (a)[0] != _Alignof(int)) return 3;
    if (sizeof (te2_f)() != sizeof(int *)) return 4;
    if (sizeof (a) != 10 * sizeof(int)) return 5;
    return 0;
}

/* ---- codegen_typeof_vla_declaration_keeps_its_extent: exits 77..96
 * `typeof(int[n])` in a declaration names a variable length array whose
 * extent is `n`, as it does in `sizeof`. The declaration specifiers parsed
 * the operand with the type-name parser that drops variably modified
 * extents, so `typeof(int[n]) a;` declared an `int[]` and `sizeof a` was
 * rejected as incomplete. The extent is evaluated once, at the specifier.
 */
static int calls;
static int next(int n) { calls++; return n; }

static int probe(int n)
{
    typeof(int[n]) a;
    __typeof__(char[n][3]) b;
    typeof(int[next(n)]) c;
    if (sizeof a != n * sizeof(int))
        return 1;
    if (sizeof b != (unsigned long)n * 3)
        return 2;
    if (sizeof c != n * sizeof(int))
        return 3;
    for (int i = 0; i < n; i++)
        a[i] = i * 7;
    for (int i = 0; i < n; i++)
        if (a[i] != i * 7)
            return 4;
    return 0;
}

static __attribute__((noinline)) int t_typeof_vla_declaration_keeps_its_extent(void)
{
    int r = probe(5);
    if (r)
        return r;
    r = probe(11);
    if (r)
        return 10 + r;
    if (calls != 2)
        return 20;
    return 0;
}

/* ---- codegen_typedef_name_after_type_specifier_is_declared: exits 97..99
 * A typedef name is a type specifier only when no other type specifier has
 * been given (C17 6.7.2p2), so `unsigned T = ..` declares a variable named
 * `T`. `unsigned` sets no base type of its own, and the typedef-name test
 * asked only for one, so `T` was taken as the type and the declaration
 * failed for want of a declarator.
 */
typedef char T;

int te4_f(void)
{
    /* `T` here is the name being declared, an unsigned int hiding the
       typedef -- not a second type specifier. */
    unsigned T = 3000000000u;
    if (T != 3000000000u)
        return 1;
    if (sizeof T != sizeof(unsigned int))
        return 2;
    return 0;
}

int te4_g(void)
{
    const T c = 'x';   /* a qualifier is not a type specifier */
    return sizeof c == 1 && c == 'x' ? 0 : 3;
}

static __attribute__((noinline)) int t_typedef_name_after_type_specifier_is_declared(void)
{
    int r = te4_f();
    if (r)
        return r;
    return te4_g();
}

/* ---- codegen_vla_extent_inside_a_grouped_declarator: exits 100..101
 * `int (*p[n]);` is an array of `n` pointers: the extent sits in the grouped
 * inner declarator, and `parse_declarator` dropped every inner extent, so the
 * array came out incomplete and `sizeof p` was refused.
 */
static int grouped(int n)
{
    int v = 5;
    int (*p[n]);
    if (sizeof p != (unsigned long)n * sizeof(int *))
        return 1;
    for (int i = 0; i < n; i++)
        p[i] = &v;
    return *p[n - 1] == 5 ? 0 : 2;
}

static __attribute__((noinline)) int t_vla_extent_inside_a_grouped_declarator(void)
{
    return grouped(3);
}

/* ---- codegen_function_and_array_convert_from_their_address: exits 102..130
 * A function designator or an array converts from the pointer it decays to
 * (C17 6.3.2.1p3-4), so every bit of the address survives a cast to an
 * integer, a conversion to `_Bool` sees all of it, and as a static
 * initializer the cast is a relocation (6.6p9).
 *
 * Typed as the function itself, the conversion read a value with no width:
 * on x86-64 `(long)h` kept the low 32 bits of the address, `(_Bool)h` was an
 * internal compiler error, and `long l = (long)h;` at file scope was rejected
 * as "the value of a variable". `full` is the address read back through a
 * `volatile` pointer, which no conversion touches.
 */
#include <stdint.h>
#include <stdarg.h>

__attribute__((aligned(256))) static long te6_h(void) { return 7; }
static long te6_g(void) { return 9; }
static int arr[4] __attribute__((aligned(256))) = {1, 2, 3, 4};

static long via_mem(long (*volatile *pp)(void)) { return (long)*pp; }
static long arr_via_mem(int *volatile *pp) { return (long)*pp; }

static long take_long(long x) { return x; }
static long take_var(int n, ...) {
    va_list ap;
    va_start(ap, n);
    long v = (long)va_arg(ap, long (*)(void));
    va_end(ap);
    return v;
}
static long take_old();
static _Bool ret_bool_h(void) { return te6_h; }
static _Bool ret_bool_arr(void) { return arr; }
static _Bool take_bool(_Bool b) { return b; }

static long file_scope_h = (long)te6_h;
static uintptr_t file_scope_u = (uintptr_t)te6_h;
static long file_scope_arr = (long)arr;
static uintptr_t file_scope_str = (uintptr_t)"str";
static _Bool file_scope_bool = (_Bool)te6_h;

static __attribute__((noinline)) int t_function_and_array_convert_from_their_address(void)
{
    long (*volatile fp)(void) = te6_h;
    int *volatile ap = arr;
    long full = via_mem(&fp);
    long afull = arr_via_mem(&ap);

    if ((long)te6_h != full) return 1;
    if ((unsigned long)te6_h != (unsigned long)full) return 2;
    if ((uintptr_t)te6_h != (uintptr_t)full) return 3;
    if ((int)te6_h != (int)full) return 4;
    if ((_Bool)te6_h != 1) return 5;
    if ((void *)te6_h != (void *)full) return 6;
    if ((char *)te6_h != (char *)full) return 7;
    if ((long)arr != afull) return 8;
    if ((uintptr_t)arr != (uintptr_t)afull) return 9;
    if (((long)te6_h >> 32) != (full >> 32)) return 10;
    if (take_long((long)te6_h) != full) return 11;
    if (take_var(1, te6_h) != full) return 12;
    if (take_old(te6_h) != full) return 13;
    if (file_scope_h != full) return 14;
    if (file_scope_u != (uintptr_t)full) return 15;
    if (file_scope_arr != afull) return 16;
    if (file_scope_str == 0 || !file_scope_bool) return 17;
    void *p = (void *)full;
    if (!(te6_h == (long (*)(void))p) || !((void *)te6_h == p)) return 18;
    volatile int c = 1;
    if ((long)(c ? te6_h : te6_g) != full) return 19;
    c = 0;
    if ((long)(c ? te6_h : te6_g) == full) return 20;
    if ((long)&*te6_h != full || (long)*te6_h != full) return 21;
    if ((short)te6_h != (short)full || (unsigned char)arr != (unsigned char)afull) return 22;
    if ((long long)te6_h != (long long)full) return 23;
    _Bool b = te6_h;
    if (!b) return 24;
    b = arr;
    if (!b || !(_Bool)arr) return 25;
    if (!ret_bool_h() || !ret_bool_arr()) return 26;
    if (!take_bool(te6_h) || !take_bool(arr)) return 27;
    if (sizeof(&te6_h) != sizeof(void *) || sizeof arr != 4 * sizeof(int)) return 28;
    if (te6_h() + te6_g() != 16) return 29;
    return 0;
}
static long take_old(x) long (*x)(void); { return (long)x; }

/* ---- codegen_decaying_operand_converts_to_bool_from_its_address: exits 131..147
 * A function designator or an array passed to a `_Bool` parameter, or
 * converted to `_Bool` at any other site, is converted from the address it
 * decays to: true, since no function or object is at the null address.
 *
 * The argument path decayed the operand and then passed the pointer with no
 * conversion to the parameter's type, so at -O0 the callee read the
 * address's low byte -- zero for anything aligned to 256, which is why the
 * `_Bool` checks above are aligned that way. A static `_Bool` initialized
 * with an array or a function stored the same low byte.
 */
__attribute__((aligned(256))) static long te7_h(void) { return 7; }
static int te7_arr[4] __attribute__((aligned(256))) = {1, 2, 3, 4};
static int grid[2][64] __attribute__((aligned(256)));
struct holder { int a[64]; } __attribute__((aligned(256))) hold;
__attribute__((noinline)) static _Bool te7_take_bool(_Bool b) { return b; }
__attribute__((noinline)) static int take_two(int n, _Bool b) { return n + b; }
static _Bool ret_h(void) { return te7_h; }
static _Bool ret_arr(void) { return te7_arr; }
struct flags { _Bool f; };
_Bool te7_file_scope_arr = (_Bool)te7_arr;

static __attribute__((noinline)) int t_decaying_operand_converts_to_bool_from_its_address(void)
{
    if (!te7_take_bool(te7_h)) return 1;
    if (!te7_take_bool(te7_arr)) return 2;
    if (!te7_take_bool("str")) return 3;
    if (!te7_take_bool(grid[1])) return 4;
    if (!te7_take_bool(hold.a)) return 5;
    if (!te7_take_bool((int[]){0})) return 6;
    if (take_two(1, te7_h) != 2 || take_two(2, te7_arr) != 3) return 7;
    if (!ret_h() || !ret_arr()) return 8;
    _Bool b = te7_h;
    if (!b) return 9;
    b = 0;
    b = te7_arr;
    if (!b) return 10;
    _Bool bs[3] = { te7_h, te7_arr, "x" };
    if (!bs[0] || !bs[1] || !bs[2]) return 11;
    struct flags s = { te7_arr };
    if (!s.f) return 12;
    _Bool cl = (_Bool){ te7_h };
    if (!cl) return 13;
    _Atomic _Bool ab = 0;
    ab = te7_arr;
    if (!ab) return 14;
    struct flags ss = { .f = grid[0] };
    if (!ss.f) return 15;
    _Bool (*fp)(_Bool) = te7_take_bool;
    if (!fp(te7_arr) || !fp(te7_h)) return 16;
    if (!te7_file_scope_arr) return 17;
    return 0;
}

/* ---- codegen_const_subobject_folds_in_a_static_initializer: exits 148..149
 * An element or member of a `const` object whose initializer folded is
 * folded in a static initializer, as the object named whole is -- gcc does
 * both. The subscript or member access was taken as an address, so
 * `int w = a[0];` was initialized with eight bytes of relocation to `a`.
 */
struct P { int x; double d; short v[3]; };
const int te8_a[3] = {1, 2, 3};
const struct P p = { 4, 2.5, { 7, 8 } };
const struct P ps[2] = { { 1, 0.5, {0} }, { 9, 1.25, { 5, 6, 7 } } };
const double da[2] = { 1.5, 3.5 };
int w0 = te8_a[0], w2 = te8_a[2] + 1;
long wx = p.x;
double wd = p.d * 2;
int wv = p.v[1];
int ww = ps[1].v[2] + ps[1].x;
double wf = da[1];
float wq = ps[1].d;
static __attribute__((noinline)) int t_const_subobject_folds_in_a_static_initializer(void)
{
    if (w0 != 1 || w2 != 4 || wx != 4 || wd != 5.0) return 1;
    if (wv != 8 || ww != 16 || wf != 3.5 || wq != 1.25f) return 2;
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_typeof_of_variably_modified_object()) != 0) return r;
    if ((r = t_sizeof_parenthesized_operand_takes_postfix_operators()) != 0) return 71 + r;
    if ((r = t_typeof_vla_declaration_keeps_its_extent()) != 0) return 76 + r;
    if ((r = t_typedef_name_after_type_specifier_is_declared()) != 0) return 96 + r;
    if ((r = t_vla_extent_inside_a_grouped_declarator()) != 0) return 99 + r;
    if ((r = t_function_and_array_convert_from_their_address()) != 0) return 101 + r;
    if ((r = t_decaying_operand_converts_to_bool_from_its_address()) != 0) return 130 + r;
    if ((r = t_const_subobject_folds_in_a_static_initializer()) != 0) return 147 + r;
    return 0;
}
"#;
    compile_and_run_everywhere("types_exprs_everywhere_mega", code);
}

/// Objects up to `PTRDIFF_MAX`: sizes, member offsets and pointer scaling.
///
/// gcc.c-torture `991014-1` declares a struct of 2^63 - 496 bytes, and c17
/// refused it because struct layout accumulated its bits in a `usize` and so
/// capped every object at `u64::MAX / 8` bytes. Every figure here is gcc's.
///
/// No object this large is ever defined -- a test program that asks its loader
/// for gigabytes fails on macOS. The member accesses go through a pointer
/// placed so that the member lands on a small real buffer, which is what puts
/// a displacement in [2^31, 2^32) and past 2^32 on each target's load and
/// store. The accesses are `volatile` so that neither compiler may reason
/// from the pointee's size that it cannot overlap the buffer.
#[test]
fn codegen_objects_up_to_ptrdiff_max() {
    let src = r#"
typedef unsigned long UL;
typedef __UINTPTR_TYPE__ UP;

struct Mid { char pad[3000000000L]; int x; long y; };
struct Big { char pad[5000000000L]; int x; long y; char tail[3]; };
struct Bits { char pad[5000000000L]; unsigned f : 3, g : 5; long z; };
struct Nest { char p[7]; struct Big b; };
struct Huge { short buf[(1L << 62) - 256]; int a, b, c, d; };
union HU { int a; char buf[(1L << 62) - 256]; };
struct Edge { char buf[9223372036854775807L - 7]; };
typedef struct Big BigArr[3];

_Static_assert(sizeof(struct Mid) == 3000000016UL, "mid size");
_Static_assert(__builtin_offsetof(struct Mid, y) == 3000000008UL, "mid y");
_Static_assert(sizeof(struct Big) == 5000000024UL, "big size");
_Static_assert(__builtin_offsetof(struct Big, tail[2]) == 5000000018UL, "big tail");
_Static_assert(sizeof(struct Bits) == 5000000016UL, "bits size");
_Static_assert(__builtin_offsetof(struct Bits, z) == 5000000008UL, "bits z");
_Static_assert(__builtin_offsetof(struct Nest, b.y) == 5000000016UL, "nest b.y");
_Static_assert(sizeof(struct Huge) == 9223372036854775312UL, "huge size");
_Static_assert(__builtin_offsetof(struct Huge, d) == 9223372036854775308UL, "huge d");
_Static_assert(sizeof(union HU) == 4611686018427387648UL, "union size");
_Static_assert(sizeof(struct Edge) == 9223372036854775800UL, "edge size");
_Static_assert(sizeof(BigArr) == 15000000072UL, "array of big");

static const UL off_huge_c = (UL) & ((struct Huge *)0)->c;
static const UL off_big_y = (UL) & ((struct Big *)0)->y;
static char chk[(UL) & ((struct Huge *)0)->b == 9223372036854775300UL ? 1 : -1];

UP volatile base_addr;
long volatile one = 1;

static UP off_huge_d(void) { return (UP) & ((struct Huge *)0)->d; }

int main(void)
{
    struct R { int x; int pad; long y; char t[8]; } store;
    volatile struct R *vb = &store;
    UP a = (UP)&store;

    if (off_huge_c != 9223372036854775304UL) return 1;
    if (off_big_y != 5000000008UL) return 2;
    if (off_huge_d() != 9223372036854775308UL) return 3;
    if (sizeof chk != 1) return 4;

    base_addr = a - 3000000000UL;
    volatile struct Mid *m = (volatile struct Mid *)base_addr;
    m->x = 41;
    m->y = 0x1122334455667788L;
    if (vb->x != 41) return 5;
    if (vb->y != 0x1122334455667788L) return 6;
    if (m->x + 1 != 42 || m->y != 0x1122334455667788L) return 7;

    base_addr = a - 5000000000UL;
    volatile struct Big *p = (volatile struct Big *)base_addr;
    p->x = 7;
    p->y = -3;
    p->tail[2] = 'z';
    if (vb->x != 7 || vb->y != -3 || vb->t[2] != 'z') return 8;
    if ((UP)&p->tail[1] != a + 17) return 9;
    volatile long *py = &p->y;
    if (*py != -3) return 10;

    base_addr = a - 5000000000UL;
    volatile struct Bits *bp = (volatile struct Bits *)base_addr;
    vb->x = 0;
    bp->f = 5;
    bp->g = 17;
    bp->z = 99;
    if (bp->f != 5 || bp->g != 17 || vb->y != 99) return 11;

    base_addr = a - 5000000008UL;
    volatile struct Nest *np = (volatile struct Nest *)base_addr;
    np->b.x = 1234;
    if (vb->x != 1234 || np->b.x != 1234) return 12;

    /* Pointer arithmetic scales by the element size; nothing is dereferenced. */
    base_addr = 0x10000;
    struct Big *q = (struct Big *)base_addr;
    if ((UP)(q + one) - (UP)q != 5000000024UL) return 13;
    if ((UP)&q[one].y != 0x10000 + 5000000024UL + 5000000008UL) return 14;
    struct Big *q3 = (struct Big *)(base_addr + 3 * 5000000024UL);
    if (q3 - q != 3) return 15;
    q3 -= one;
    if ((UP)q3 != 0x10000 + 2 * 5000000024UL) return 16;

    struct Huge *h = (struct Huge *)base_addr;
    if ((UP)(h + one) - (UP)h != 9223372036854775312UL) return 17;
    struct Huge *h1 = (struct Huge *)(base_addr + 9223372036854775312UL);
    if (h1 - h != 1) return 18;
    if ((UP)&h->c != 0x10000 + 9223372036854775304UL) return 19;
    if ((UP)&h->buf[(1L << 62) - 257] != 0x10000 + 9223372036854775294UL) return 20;

    BigArr *pa = (BigArr *)base_addr;
    if ((UP)(pa + one) - (UP)pa != 15000000072UL) return 21;
    if ((UP)&(*pa)[2].y != 0x10000 + 2 * 5000000024UL + 5000000008UL) return 22;

    return 0;
}
"#;
    compile_and_run_everywhere("objects_up_to_ptrdiff_max", src);
}

/// GNU C rejects a static `_Bool` initialized with a bare array or function,
/// as not computable at load time; c17 folds it as the cast form folds,
/// since an address constant is never null.
#[test]
fn codegen_static_bool_from_an_address_constant_is_true() {
    let src = r#"
static int arr[4] __attribute__((aligned(256)));
__attribute__((aligned(256))) static long h(void) { return 7; }
_Bool gb = arr, gs = "x", ga = &arr[1], gz = 0, gf = 0.5;
int main(void) {
    static _Bool sb = arr, sf = h;
    if (!gb || !gs || !ga || gz || !gf) return 1;
    if (!sb || !sf) return 2;
    return 0;
}
"#;
    compile_and_run_everywhere("static_bool_from_address", src);
}
