//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Inlining: what is inlined, what must not be, and the stack and
// recursion guards around it.
//

use crate::common::{
    compile_and_run, compile_and_run_aarch64, compile_and_run_everywhere,
    compile_and_run_optimized, compile_and_run_two_units,
};

// ============================================================================
// Test: Inlined two-register struct returns
// ============================================================================

/// Inlined bodies: returns, phis, loops, nested and noreturn callees, and
/// aggregate parameters and returns, one program run at -O1
/// (`compile_and_run_optimized`).
///
/// The inliner decides per call site from the callee (its size, and its
/// call count, which is by name) and the caller's own size, so each
/// original `main` keeps its decisions as a `noinline` section function
/// with every helper name kept distinct.
///
/// Consolidates these tests, one section each (each original `main` is
/// a `noinline` section function; the program exits with the section's
/// base plus the original code):
/// - `codegen_inline_two_reg_struct_return`: 1..=6
/// - `codegen_inline_multi_ret_phi_join`: 7..=13
/// - `codegen_inline_m0_a_void_return`: 14..=14
/// - `codegen_inline_m0_b_single_ret`: 15..=17
/// - `codegen_inline_m0_c_ret_in_loop`: 18..=21
/// - `codegen_inline_m0_d_internal_phi`: 22..=25
/// - `codegen_inline_m0_e_multiple_calls`: 26..=29
/// - `codegen_inline_m0_f_pointer_return`: 30..=35
/// - `codegen_inline_m0_g_long_return`: 36..=38
/// - `codegen_inline_m0_h_nested`: 39..=42
/// - `codegen_inline_m0_j_consecutive_rets_in_block`: 43..=46
/// - `codegen_inline_m0_i_mixed_noreturn`: 47..=49
/// - `codegen_inline_two_sse_struct_param`: 50..=53
/// - `codegen_inline_large_struct_param`: 54..=55
#[test]
fn codegen_inlining_mega() {
    let code = r#"
/* ---- codegen_inline_two_reg_struct_return: exits 1..6
 */
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

static __attribute__((noinline)) int t_inline_two_reg_struct_return(void)
{
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

/* ---- codegen_inline_multi_ret_phi_join: exits 7..13
 * Test: Inliner emits a φ-join (not multiple Copies) for multi-`ret` callees
 *
 * Regression for the SSA single-def invariant the inliner is required to
 * preserve. A callee like
 *
 *     static inline int choose(int c, int a, int b) {
 *         if (c) return a;
 *         return b;
 *     }
 *
 * has two `ret` paths. The historical lowering converted each `ret` to a
 * `Copy %ret_target = %x` in the cloned predecessor block — and because the
 * inliner uses one shared `%ret_target`, this produced *multiple definitions*
 * of the same SSA pseudo. That is not valid SSA, and any downstream pass
 * that merges pseudos (copyprop, CSE, GVN, SCCP) would mis-route the
 * continuation's reads to whichever Copy it visited first, silently dropping
 * the other return path.
 *
 * After M0, the inliner emits a `PhiSource` in each predecessor and a single
 * `Phi %ret_target = phi (pred1, ps1), (pred2, ps2)` at the head of the
 * continuation block. SSA single-def is restored.
 *
 * The test compiles at -O so the small callees are inlined and exercises
 * every routing path: c true with both arms, c false with both arms,
 * nested calls, calls used in expressions, and calls whose results are
 * stored to globals.
 */
static inline int choose(int c, int a, int b) {
    if (c) return a;
    return b;
}

static inline int max2(int x, int y) {
    if (x > y) return x;
    return y;
}

int g_sum;

static __attribute__((noinline)) int t_inline_multi_ret_phi_join(void)
{
    /* basic two-arm: cond true and false */
    if (choose(1, 7, 9) != 7) return 1;
    if (choose(0, 7, 9) != 9) return 2;

    /* both arms in arithmetic context */
    int sum = choose(1, 10, 20) + choose(0, 100, 200);
    if (sum != 210) return 3;

    /* nested inline */
    if (max2(choose(1, 3, 1), choose(0, 5, 8)) != 8) return 4;
    if (max2(choose(0, 3, 1), choose(1, 5, 8)) != 5) return 5;

    /* result stored to a writable global (exercises store after Phi) */
    g_sum = choose(1, -1, -2);
    if (g_sum != -1) return 6;

    /* loop with inlined call inside (exercises the Phi across a loop carry) */
    int acc = 0;
    for (int i = 0; i < 5; i++) {
        acc += choose(i & 1, i, -i);
    }
    /* i=0: choose(0, 0, -0) =  0
       i=1: choose(1, 1, -1) =  1
       i=2: choose(0, 2, -2) = -2
       i=3: choose(1, 3, -3) =  3
       i=4: choose(0, 4, -4) = -4
       sum = -2 */
    if (acc != -2) return 7;

    return 0;
}

/* ---- codegen_inline_m0_a_void_return: exits 14..14
 * (A) Inlined void-returning function. The M0 path skips Phi materialization
 * when the call has no result target.
 */
static int g_counter;

static inline void bump(int delta) {
    g_counter += delta;
}

static __attribute__((noinline)) int t_inline_m0_a_void_return(void)
{
    g_counter = 0;
    bump(3);
    bump(7);
    if (g_counter != 10) return 1;
    return 0;
}

/* ---- codegen_inline_m0_b_single_ret: exits 15..17
 * (B) Inlined function with a SINGLE `ret`. 1-arm Phi.
 */
static inline int identity(int x) {
    return x;
}

static __attribute__((noinline)) int t_inline_m0_b_single_ret(void)
{
    if (identity(0) != 0) return 1;
    if (identity(42) != 42) return 2;
    if (identity(-7) != -7) return 3;
    return 0;
}

/* ---- codegen_inline_m0_c_ret_in_loop: exits 18..21
 * (C) `ret` inside a loop in the callee. PhiSource lands inside the loop;
 * the Phi sits in the post-inline continuation outside the loop.
 */
static inline int find(int needle, int *haystack, int n) {
    for (int i = 0; i < n; i++) {
        if (haystack[i] == needle) return i;
    }
    return -1;
}

static __attribute__((noinline)) int t_inline_m0_c_ret_in_loop(void)
{
    int arr[16] = {10, 20, 30, 40, 50, 60, 70, 80,
                   90, 100, 110, 120, 130, 140, 150, 160};
    if (find(10, arr, 16) != 0) return 1;
    if (find(160, arr, 16) != 15) return 2;
    if (find(75, arr, 16) != -1) return 3;
    if (find(80, arr, 16) != 7) return 4;
    return 0;
}

/* ---- codegen_inline_m0_d_internal_phi: exits 22..25
 * (D) Inlined function whose result comes from a callee-internal Phi
 * (ternary). Two Phi nodes in series — callee's inner Phi feeds M0's
 * outer continuation Phi.
 */
static inline int picker(int c, int a, int b) {
    return c ? a : b;
}

static __attribute__((noinline)) int t_inline_m0_d_internal_phi(void)
{
    if (picker(1, 10, 20) != 10) return 1;
    if (picker(0, 10, 20) != 20) return 2;
    if (picker(picker(1, 1, 0), 100, 200) != 100) return 3;
    if (picker(picker(0, 1, 0), 100, 200) != 200) return 4;
    return 0;
}

/* ---- codegen_inline_m0_e_multiple_calls: exits 26..29
 * (E) Multiple inlined calls in the same caller. Each call materializes
 * its own Phi; pseudo and block IDs must stay disjoint.
 */
static inline int absx(int x) {
    if (x < 0) return -x;
    return x;
}

static inline int io7_max2(int a, int b) {
    if (a > b) return a;
    return b;
}

static __attribute__((noinline)) int t_inline_m0_e_multiple_calls(void)
{
    int t = absx(-5);
    int u = absx(3);
    int v = io7_max2(t, u);
    int w = io7_max2(absx(-9), -2);
    if (t != 5) return 1;
    if (u != 3) return 2;
    if (v != 5) return 3;
    if (w != 9) return 4;
    return 0;
}

/* ---- codegen_inline_m0_f_pointer_return: exits 30..35
 * (F) Inlined function returning a pointer (size 64). Phi must propagate
 * the correct size from `Ret`.
 */
static char buf_a[] = "alpha";
static char buf_b[] = "beta";

static inline char *choose_buf(int which) {
    if (which) return buf_a;
    return buf_b;
}

static int strlen3(const char *s) {
    int n = 0;
    while (*s++) n++;
    return n;
}

static __attribute__((noinline)) int t_inline_m0_f_pointer_return(void)
{
    char *p1 = choose_buf(1);
    char *p2 = choose_buf(0);
    if (strlen3(p1) != 5) return 1;
    if (strlen3(p2) != 4) return 2;
    if (p1[0] != 'a') return 3;
    if (p2[0] != 'b') return 4;
    if (choose_buf(1)[1] != 'l') return 5;
    if (choose_buf(0)[1] != 'e') return 6;
    return 0;
}

/* ---- codegen_inline_m0_g_long_return: exits 36..38
 * (G) Inlined function returning a long (size 64, int bank).
 */
static inline long bigchoice(int c, long a, long b) {
    if (c) return a;
    return b;
}

static __attribute__((noinline)) int t_inline_m0_g_long_return(void)
{
    long x = bigchoice(1, 0x1122334455667788L, 0x99AABBCCDDEEFF00L);
    long y = bigchoice(0, 0x1122334455667788L, 0x99AABBCCDDEEFF00L);
    if (x != 0x1122334455667788L) return 1;
    if (y != (long)0x99AABBCCDDEEFF00L) return 2;
    long sum = bigchoice(1, 7L, 999L) + bigchoice(0, 999L, 13L);
    if (sum != 20L) return 3;
    return 0;
}

/* ---- codegen_inline_m0_h_nested: exits 39..42
 * (H) Nested inlining: outer→middle→innermost. Each level emits its own
 * Phi using fresh IDs from next_inline_id.
 */
static inline int innermost(int c, int a, int b) {
    if (c) return a;
    return b;
}

static inline int middle(int c, int x) {
    int v = innermost(c, x, -x);
    if (v < 0) return v - 1;
    return v + 1;
}

static inline int outer(int c, int x) {
    int m = middle(c, x);
    if (m > 0) return m * 2;
    return m;
}

static __attribute__((noinline)) int t_inline_m0_h_nested(void)
{
    if (outer(1, 5) != 12) return 1;   /* 5  -> innermost=5 -> middle=6 -> outer=12 */
    if (outer(0, 5) != -6) return 2;   /* 5  -> innermost=-5 -> middle=-6 -> outer=-6 */
    if (outer(1, 0) != 2) return 3;    /* 0  -> innermost=0 -> middle=1 -> outer=2 */
    if (outer(0, 0) != 2) return 4;    /* 0  -> innermost=0 -> middle=1 -> outer=2 */
    return 0;
}

/* ---- codegen_inline_m0_j_consecutive_rets_in_block: exits 43..46
 * (J) Two `ret`s in the SAME callee block (e.g., a real return followed by
 * an `#ifdef`-fenced fallback `return 0;`). The pre-M0 inliner generated a
 * single block with two `Br` terminators, and `lower.rs` placed
 * phi-elimination copies before the *last* `Br` — unreached at runtime —
 * leaving the M0 Phi reading an undefined register. This was the actual
 * CPython asyncio fork-multiprocessing crash. The fix in
 * `clone_callee_blocks` truncates cloning at the first `Ret`.
 */
#define HAS_FAST_PATH 1

static int g_x;

/* Two `return`s with no intervening branch. The second is unreachable
   but the linearizer keeps both in the same IR block. M0 must clone only
   up to the first `Ret`. Mirrors `_PyIsPerfTrampolineActive`'s shape. */
static inline int is_x_one(void) {
#if HAS_FAST_PATH
    return g_x == 1;
#endif
    return 0;  /* unreachable when HAS_FAST_PATH defined */
}

static __attribute__((noinline)) int t_inline_m0_j_consecutive_rets_in_block(void)
{
    g_x = 1;
    if (is_x_one() != 1) return 1;

    g_x = 7;
    if (is_x_one() != 0) return 2;

    g_x = -3;
    if (is_x_one() != 0) return 3;

    g_x = 1;
    int r = is_x_one() + is_x_one();
    if (r != 2) return 4;

    return 0;
}
#undef HAS_FAST_PATH

/* ---- codegen_inline_m0_i_mixed_noreturn: exits 47..49
 * (I) Mixed Noreturn/abort paths with normal returns. Only the returning
 * paths contribute to M0's Phi; the noreturn path's Unreachable does not.
 */
#include <stdlib.h>

static inline int validated(int x) {
    if (x < 0) {
        exit(99);  /* noreturn — must not become a Phi arm */
    }
    if (x == 0) return 100;
    return x * 2;
}

static __attribute__((noinline)) int t_inline_m0_i_mixed_noreturn(void)
{
    if (validated(0) != 100) return 1;
    if (validated(5) != 10) return 2;
    if (validated(7) != 14) return 3;
    return 0;
}

/* ---- codegen_inline_two_sse_struct_param: exits 50..53
 * Regression: inlining a function with two-SSE struct parameters (e.g.
 * `{double, double}`) failed because the inliner did not generate the
 * struct copy that the backend prologue normally performs.  The local
 * for the parameter was left uninitialised (zero-filled), so the
 * inlined body read all-zeros.
 */
typedef struct { double real; double imag; } Complex;

Complex make_complex(double r, double i) {
    Complex c;
    c.real = r;
    c.imag = i;
    return c;
}

Complex add_complex(Complex a, Complex b) {
    Complex c;
    c.real = a.real + b.real;
    c.imag = a.imag + b.imag;
    return c;
}

Complex sub_complex(Complex a, Complex b) {
    Complex c;
    c.real = a.real - b.real;
    c.imag = a.imag - b.imag;
    return c;
}

static __attribute__((noinline)) int t_inline_two_sse_struct_param(void)
{
    Complex z1 = make_complex(3.0, 4.0);
    if (z1.real != 3.0 || z1.imag != 4.0) return 1;

    Complex z2 = make_complex(1.0, 2.0);
    Complex z3 = add_complex(z1, z2);
    if (z3.real != 4.0 || z3.imag != 6.0) return 2;

    Complex z4 = sub_complex(z1, z2);
    if (z4.real != 2.0 || z4.imag != 2.0) return 3;

    /* Chained: add(sub(z1, z2), z2) == z1 */
    Complex z5 = add_complex(sub_complex(z1, z2), z2);
    if (z5.real != 3.0 || z5.imag != 4.0) return 4;

    return 0;
}

/* ---- codegen_inline_large_struct_param: exits 54..55
 * Bug AL: Inlining a function with a MEMORY-class struct parameter (>32 bytes,
 * passed on stack) caused symaddr of the Arg pseudo to produce a pointer-to-pointer
 * instead of the struct address. The fix converts symaddr-on-Arg to copy during
 * inlining, since call_args already provides the struct address.
 */
typedef struct {
    long a[16]; // 128 bytes, MEMORY class on x86-64
} BigStruct;

int convert(void *obj, BigStruct *out) {
    out->a[0] = 42;
    out->a[1] = 99;
    return 1;
}

long use_big(int how, BigStruct s) {
    return s.a[0] + s.a[1] + how;
}

static __attribute__((noinline)) int t_inline_large_struct_param(void)
{
    BigStruct local;
    int ok = convert((void*)0x1234, &local);
    if (!ok) return 1;
    long result = use_big(10, local);
    // 42 + 99 + 10 = 151
    if (result != 151) return 2;
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_inline_two_reg_struct_return()) != 0) return r;
    if ((r = t_inline_multi_ret_phi_join()) != 0) return 6 + r;
    if ((r = t_inline_m0_a_void_return()) != 0) return 13 + r;
    if ((r = t_inline_m0_b_single_ret()) != 0) return 14 + r;
    if ((r = t_inline_m0_c_ret_in_loop()) != 0) return 17 + r;
    if ((r = t_inline_m0_d_internal_phi()) != 0) return 21 + r;
    if ((r = t_inline_m0_e_multiple_calls()) != 0) return 25 + r;
    if ((r = t_inline_m0_f_pointer_return()) != 0) return 29 + r;
    if ((r = t_inline_m0_g_long_return()) != 0) return 35 + r;
    if ((r = t_inline_m0_h_nested()) != 0) return 38 + r;
    if ((r = t_inline_m0_j_consecutive_rets_in_block()) != 0) return 42 + r;
    if ((r = t_inline_m0_i_mixed_noreturn()) != 0) return 46 + r;
    if ((r = t_inline_two_sse_struct_param()) != 0) return 49 + r;
    if ((r = t_inline_large_struct_param()) != 0) return 53 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run_optimized("inlining_mega", code), 0);
}

// ============================================================================
// M0 edge-case coverage (A–I)
//
// Each test isolates one shape of inlined `ret` lowering that M0 must handle.
// They were written *after* M0 broke CPython at -O2 (asyncio hot paths) to
// localize the regression to a fast, debuggable scope.
// ============================================================================

/// Regression test: inline asm with "+r" constraint on 64-bit value used 32-bit register.
/// The xchg instruction in CPython's atomic store macro would truncate pointers.
#[test]
#[cfg(target_arch = "x86_64")]
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

/// AArch64 version: inline asm with "r" constraint on 64-bit value.
/// Verifies str instruction preserves full 64-bit register width.
#[test]
#[cfg(target_arch = "aarch64")]
fn codegen_inline_asm_64bit_constraint() {
    let code = r#"
#include <stdint.h>

typedef struct { uintptr_t _value; } atomic_addr;

static inline void atomic_store(atomic_addr *addr, uintptr_t val) {
    uintptr_t new_val = val;
    __asm__ volatile("str %0, [%1]"
                     :
                     : "r"(new_val), "r"(&addr->_value)
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

/// Array initializers and C99 inline definitions, one program run at the
/// compile matrix levels.
///
/// Consolidates these tests, one section each (each original `main` is
/// a `noinline` section function; the program exits with the section's
/// base plus the original code):
/// - `codegen_2d_char_array_init`: 1..=9
/// - `codegen_inline_definition_constraint_only_binds_inline_definitions`: 10..=13
#[test]
fn codegen_inline_definitions_mega() {
    let code = r#"
/* ---- codegen_2d_char_array_init: exits 1..9
 * Regression test: 2D char array initializers (e.g., char names[7][4] = {"Sun", ...})
 * stored pointers to string constants instead of inline char data. The initializer
 * treated each string as a `char*` (SymAddr) instead of `char[4]` (String).
 */
#include <string.h>

static const char wday[7][4] = {"Sun","Mon","Tue","Wed","Thu","Fri","Sat"};
static const char mon[12][4] = {
    "Jan","Feb","Mar","Apr","May","Jun",
    "Jul","Aug","Sep","Oct","Nov","Dec"
};

static __attribute__((noinline)) int t_2d_char_array_init(void)
{
    if (strcmp(wday[0], "Sun") != 0) return 1;
    if (strcmp(wday[3], "Wed") != 0) return 2;
    if (strcmp(wday[6], "Sat") != 0) return 3;
    if (strcmp(mon[0], "Jan") != 0) return 4;
    if (strcmp(mon[2], "Mar") != 0) return 5;
    if (strcmp(mon[11], "Dec") != 0) return 6;

    /* Test local (stack) 2D char array */
    const char colors[3][6] = {"red", "green", "blue"};
    if (strcmp(colors[0], "red") != 0) return 7;
    if (strcmp(colors[1], "green") != 0) return 8;
    if (strcmp(colors[2], "blue") != 0) return 9;

    return 0;
}

/* ---- codegen_inline_definition_constraint_only_binds_inline_definitions: exits 10..13
 * C99 6.7.4p3 constrains an inline *definition*, not every non-static inline
 * function.
 *
 * The rule exists so that the several inline definitions of a function cannot
 * differ from each other or from the external one: an inline definition must
 * not name an identifier with internal linkage, nor define a modifiable
 * object with static storage duration. A definition that *is* the external
 * definition -- because some declaration of it says `extern`, which is the
 * standard idiom for providing one -- is an ordinary function, and gcc
 * accepts what this used to reject outright.
 */
#include <stdio.h>

static const int table[4] = {1, 2, 3, 4};
static int counter;

/* An `extern` declaration makes this the external definition. */
inline int get(int i) { return table[i]; }
extern int get(int);

/* `extern inline` says the same thing up front. */
extern inline int bump(void) { return ++counter; }

/* A static inline may always reach a file-scope static. */
static inline int twice(int i) { return table[i] * 2; }

static __attribute__((noinline)) int t_inline_definition_constraint_only_binds_inline_definitions(void)
{
    if (get(2) != 3) return 1;
    if (bump() != 1) return 2;
    if (bump() != 2) return 3;
    if (twice(3) != 8) return 4;
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_2d_char_array_init()) != 0) return r;
    if ((r = t_inline_definition_constraint_only_binds_inline_definitions()) != 0) return 9 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("inline_definitions_mega", code, &[]), 0);
}

#[test]
#[cfg(target_arch = "x86_64")]
fn codegen_gp_live_across_inline_asm_clobber() {
    // Forward-looking defensive test for C1 of the constraint-system
    // milestone.
    //
    // C1 wires inline-asm clobber lists into the allocator's
    // `get_constraint_info` callback. Today the bug this guards
    // against is not actually triggerable in C: every user-declared
    // local becomes a `Sym` pseudo which Phase 1 force-spills to
    // stack, so no GP pseudo holding a named local survives across
    // the asm in a register. The test passes both with and without
    // the C1 fix.
    //
    // The defensive value lights up the moment any later milestone
    // promotes Sym pseudos to registers (mem2reg-style). At that
    // point a value held in RAX across an asm declaring `: "rax"`
    // gets clobbered, and this test fails loudly.
    let code = r#"
int compute(int a) {
    int x = a + 1;
    __asm__ volatile("movq $0, %%rax" : : : "rax");
    return x;
}

int main(void) {
    return compute(41) != 42;
}
"#;
    assert_eq!(
        compile_and_run("gp_live_across_inline_asm_clobber", code, &[]),
        0,
        "GP value corrupted across inline asm that declared the host register in its clobber list"
    );
}

// ============================================================================
// C99 and GNU inline: which definitions produce an external symbol
// ============================================================================

/// An `inline` function in a shared header must not become a symbol.
///
/// C99 6.7.4p6: a definition with `inline` and no `extern` in any declaration
/// is an *inline definition* and "does not provide an external definition".
/// c17 emitted one anyway, so the most ordinary use of `inline` there is --
/// a small function in a header, included by two translation units -- failed
/// to link with `multiple definition of`. gcc links it.
///
/// Compiled at `-O` because with no external definition anywhere the calls
/// have to be inlined for the program to link at all. gcc is the same: at
/// `-O0` it reports `undefined reference`. The next test covers the spelling
/// that does not depend on the optimizer.
#[test]
fn codegen_inline_in_a_header_links_from_two_units() {
    let header = "inline int hdr(int x) { return x * 2; }\n";
    let unit_a = format!("{header}int a(int x) {{ return hdr(x); }}\n");
    let unit_b = format!("{header}int a(int);\nint main(void) {{ return a(21) == 42 ? 0 : 1; }}\n");

    assert_eq!(
        compile_and_run_two_units("inline_header", &unit_a, &unit_b, &["-O".to_string()]),
        0
    );
}

/// The C99 idiom: a header's inline definition, plus one translation unit
/// naming it `extern` to emit the single out-of-line copy.
///
/// This is the spelling that works without relying on the optimizer, and it
/// exercises the ordering that makes the question hard -- the `extern`
/// declaration comes *after* the definition, so whether the definition is an
/// inline definition is not decidable when it is parsed.
#[test]
fn codegen_inline_header_with_one_extern_declaration() {
    let header = "inline int hdr(int x) { return x * 2; }\n";
    let unit_a = format!("{header}extern int hdr(int x);\nint a(int x) {{ return hdr(x); }}\n");
    let unit_b = format!("{header}int a(int);\nint main(void) {{ return a(21) == 42 ? 0 : 1; }}\n");

    assert_eq!(
        compile_and_run_two_units("inline_header_extern", &unit_a, &unit_b, &[]),
        0
    );
}

/// An inline definition still has to *work* when it cannot be inlined.
///
/// Taking a function's address forces an out-of-line body. For a plain
/// `inline` definition the call must still reach the one external definition,
/// which here lives in the other translation unit.
#[test]
fn codegen_inline_definition_reached_through_a_pointer() {
    let unit_a = r#"
inline int shared(int x) { return x * 3; }
int extern_def(int x);
int via_ptr(int x) { int (*fp)(int) = shared; return fp(x); }
"#;
    let unit_b = r#"
extern int shared(int x);
int shared(int x) { return x * 3; }   /* the external definition */
int via_ptr(int);
int main(void) { return via_ptr(14) == 42 ? 0 : 1; }
"#;
    assert_eq!(
        compile_and_run_two_units("inline_ptr", unit_a, unit_b, &[]),
        0
    );
}

/// An `.init_array` entry must name a symbol that was emitted.
///
/// A C99 inline definition provides no external definition, so its body is
/// deliberately not emitted -- but the constructor/destructor emitter walked
/// every function regardless and pointed `.init_array` at the missing symbol,
/// which fails at link time with an undefined reference.
#[test]
fn codegen_constructor_on_an_inline_definition_links() {
    let src = r#"
#include <stdio.h>

static int booted;

inline __attribute__((constructor)) void boot(void) { booted = 1; }
inline __attribute__((destructor)) void shutdown(void) { booted = 0; }

/* A constructor on an ordinary function still runs. */
static int ran;
__attribute__((constructor)) static void real_boot(void) { ran = 1; }

int main(void)
{
    if (!ran) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("inline_constructor_links", src, &[]), 0);
}

/// An `always_inline` helper that consumes a `va_list` must actually be
/// inlined, and the caller's `ap` must come back advanced.
///
/// `analyze_all_functions` set one `uses_varargs` flag for `va_start`,
/// `va_arg`, `va_end` and `va_copy` alike, and `should_inline` refused such a
/// callee *above* the `always_inline` check. A C99 inline definition has no
/// out-of-line copy, so the call was left pointing at a symbol that was never
/// emitted -- `undefined reference to 'f1i'`.
///
/// The two are not the same thing. `va_start` reads the enclosing function's
/// register save area and named-parameter counts, neither of which survives a
/// splice; `va_arg` on a `va_list` that arrived as a parameter is a
/// read-modify-write through a pointer, and C99 requires the caller to see the
/// advance -- which is what the second and third calls below check.
#[test]
fn codegen_always_inline_over_a_va_list() {
    let code = r#"
#include <stdarg.h>

long x, y;

inline void __attribute__((always_inline)) f1i(va_list ap)
{
    x = va_arg(ap, double);
    x += va_arg(ap, long);
    x += va_arg(ap, double);
}

void f1(int i, ...)
{
    va_list ap;
    va_start(ap, i);
    f1i(ap);
    va_end(ap);
}

/* Two levels: f2i consumes three of its own and then hands the *advanced*
   `ap` to f1i. If the splice did not share the caller's va_list, f1i would
   re-read what f2i already took. */
inline void __attribute__((always_inline)) f2i(va_list ap)
{
    y = va_arg(ap, int);
    y += va_arg(ap, long);
    y += va_arg(ap, double);
    f1i(ap);
}

void f2(int i, ...)
{
    va_list ap;
    va_start(ap, i);
    f2i(ap);
    va_end(ap);
}

/* The enclosing function takes some arguments itself before delegating. */
void f4(int i, ...)
{
    va_list ap;
    va_start(ap, i);
    y = va_arg(ap, double);
    f1i(ap);
    va_end(ap);
}

int main(void)
{
    f1(3, 16.0, 128L, 32.0);
    if (x != 176L) return 1;

    f2(6, 5, 7L, 18.0, 19.0, 17L, 64.0);
    if (x != 100L || y != 30L) return 2;

    f4(4, 6.0, 9.0, 16L, 18.0);
    if (x != 43L || y != 6L) return 3;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_always_inline_va_list", code, &[]),
        0
    );
    assert_eq!(
        compile_and_run(
            "codegen_always_inline_va_list_o2",
            code,
            &["-O2".to_string()]
        ),
        0
    );
    for opt in ["-O0", "-O2"] {
        if let Some(status) =
            compile_and_run_aarch64("codegen_always_inline_va_list_a64", code, opt)
        {
            assert_eq!(status, 0, "aarch64 at {opt}");
        }
    }
}

/// Inlined aggregate parameters, switches, `alloca` and zeroing, one
/// program run at the compile matrix levels and at -O1
/// (`compile_and_run_optimized`).
///
/// Each original `main` is a `noinline` section function with its helper
/// names kept distinct, so each call site is inlined as it was alone.
///
/// Consolidates these tests, one section each (each original `main` is
/// a `noinline` section function; the program exits with the section's
/// base plus the original code):
/// - `codegen_inlined_register_sized_aggregate_param`: 1..=6
/// - `codegen_wide_switch_survives_inlining`: 7..=12
/// - `codegen_over_aligned_locals_survive_alloca`: 13..=17
/// - `codegen_aggregate_zero_across_the_inline_threshold`: 18..=24
#[test]
fn codegen_inlined_bodies_mega() {
    let code = r#"
/* ---- codegen_inlined_register_sized_aggregate_param: exits 1..6
 * An aggregate that fits in one register travels *as* its value: the argument
 * pseudo holds the data, not a pointer to it. The inliner's implicit
 * parameter copies — which stand in for the backend prologue when a callee is
 * inlined — always loaded *through* the argument, so a `struct { float a, b; }`
 * became a wild pointer spelled by two floats and the inlined body segfaulted
 * on its first read.
 *
 * This is #C38, and it took a sharper case than the one recorded against it:
 * the shape in the audit had a *double* HFA on the stack, which is 16 bytes
 * and does travel by address, so it worked. It needs a float one.
 *
 * aarch64 at `-O2` only — `-O0` and `noinline` were both fine, which is what
 * made it look like an ABI bug rather than an inlining one. Values checked
 * against `aarch64-linux-gnu-gcc`.
 */
#include <stdio.h>

typedef struct { float a, b; } F2;
typedef struct { float a, b, c; } F3;
typedef struct { float a, b, c, d; } F4;
typedef struct { double a, b; } D2;
typedef struct { double a, b, c, d; } D4;
typedef struct { long x, y, z; } S24;
typedef struct { float a; } F1;

/* Two four-element double HFAs fill V0-V7, so the F2 goes on the stack. It is
   eight bytes, so it arrives as a value. */
static double f5(D4 p, D4 q, F2 r) { return p.a + q.a + r.a + r.b; }
/* Five F2s overflow the same way, with a D2 behind them. */
static double f6(F2 a, F2 b, F2 c, F2 d, F2 e, D2 f)
{ return a.a + b.a + c.a + d.a + e.a + f.a + f.b; }
/* A single float is four bytes, and also travels as a value. */
static double f7(D4 p, D4 q, F1 r) { return p.a + q.a + r.a; }
/* Controls: these travel by address and were always right. */
static double f1(F4 p, F4 q, D2 r) { return p.a + p.d + q.a + q.d + r.a + r.b; }
static double f2(F4 p, F4 q, F3 r) { return p.a + q.a + r.a + r.c; }
static double f4(F4 p, F4 q, D2 r, S24 s) { return p.a + q.a + r.a + (double)s.z; }

static __attribute__((noinline)) int t_inlined_register_sized_aggregate_param(void)
{
    F4 p = {1,2,3,4}, q = {5,6,7,8};
    F3 t3 = {1,2,3};
    D2 r = {9,10};
    D4 d4 = {1,2,3,4};
    F2 f2v = {2,3};
    F1 f1v = {5};
    S24 s = {1,2,3};

    if (f5(d4, d4, f2v) != 7.0) return 1;
    if (f6(f2v, f2v, f2v, f2v, f2v, r) != 29.0) return 2;
    if (f7(d4, d4, f1v) != 7.0) return 3;

    if (f1(p, q, r) != 37.0) return 4;
    if (f2(p, q, t3) != 10.0) return 5;
    if (f4(p, q, r, s) != 18.0) return 6;

    return 0;
}

/* ---- codegen_wide_switch_survives_inlining: exits 7..12
 * A wide switch keeps its width through inlining.
 *
 * The inliner builds a fresh `Switch` instruction rather than cloning one, so
 * everything it needs has to be carried over by hand — and the operation
 * width was not. Once inlined, a 64-bit switch was compared in 32 bits, so
 * `case 4294967296ul:` matched 0.
 *
 * Pre-existing, and invisible for a long time because an equality compare on
 * the low half agrees with the full compare unless the constants collide
 * there. A `case lo ... hi` range made it plain: its subtraction exposes the
 * top half every time.
 */
static int hit(unsigned long x)
{
    switch (x) { case 4294967296ul: return 1; default: return 0; }
}

static int upper(unsigned long x)
{
    switch (x) {
    case 9223372036854775808ul ... 18446744073709551615ul: return 1;
    default: return 0;
    }
}

static int wide_label(long x)
{
    switch (x) { case -4294967296L: return 1; default: return 0; }
}

static __attribute__((noinline)) int t_wide_switch_survives_inlining(void)
{
    /* The low 32 bits of 2^32 are zero, so a 32-bit compare says these match. */
    if (!hit(4294967296ul)) return 1;
    if (hit(0)) return 2;

    if (!upper(9223372036854775808ul)) return 3;
    if (upper(0) || upper(9223372036854775807ul)) return 4;

    if (!wide_label(-4294967296L)) return 5;
    if (wide_label(0)) return 6;
    return 0;
}

/* ---- codegen_over_aligned_locals_survive_alloca: exits 13..17
 * An over-aligned local keeps its address when `%rsp` moves under it.
 *
 * A local wanting more than 16-byte alignment cannot be reached from `%rbp`,
 * which the ABI only guarantees to 16, so the prologue rounds a base up and
 * addresses every local from that. x86-64 used `%rsp` itself as that base --
 * correct at the instant the prologue's `andq` runs, and wrong from the first
 * thing that moves it. `alloca` moves it, and the locals shifted out from
 * under their own addresses: the store of one local landed on top of another.
 *
 * Both ways in are covered. The caller need not contain an `alloca` of its
 * own -- the inliner splices callees that do into callers that do not -- and
 * the direct form was broken at every optimization level, not just where
 * inlining runs.
 */
int printf(const char *, ...);

static int use(int n) {
    char *p = __builtin_alloca(n);
    for (int i = 0; i < n; i++) p[i] = (char)i;
    int s = 0;
    for (int i = 0; i < n; i++) s += p[i];
    return s;
}

int many(int a,int b,int c,int d,int e,int f,int g,int h,int i,int j) {
    return a + b*2 + c*3 + d*4 + e*5 + f*6 + g*7 + h*8 + i*9 + j*10;
}

/* The alloca arrives by inlining: this function's source has none. */
static int inlined_form(void) {
    _Alignas(32) int buf[8];
    for (int i = 0; i < 8; i++) buf[i] = 1000 + i;
    int t = use(16);
    if (t != 120) return -1;
    for (int i = 0; i < 8; i++) if (buf[i] != 1000 + i) return -2;
    return 0;
}

/* The alloca is written here, and sits beside the over-aligned local. */
static int direct_form(int n) {
    _Alignas(64) long a[8];
    _Alignas(32) int b[8];
    for (int i = 0; i < 8; i++) { a[i] = 1000 + i; b[i] = 2000 + i; }

    char *p = __builtin_alloca(n);
    for (int i = 0; i < n; i++) p[i] = (char)i;

    /* The alignment the source asked for actually held. */
    if (((unsigned long)a & 63) != 0) return -1;
    if (((unsigned long)b & 31) != 0) return -2;

    /* A call needing stack arguments, placed after the alloca: reserving the
       outgoing area moves %rsp again. */
    if (many(1,2,3,4,5,6,7,8,9,10) != 385) return -3;

    int s = 0;
    for (int i = 0; i < n; i++) s += p[i];
    if (s != n * (n - 1) / 2) return -4;

    for (int i = 0; i < 8; i++) if (a[i] != 1000 + i) return -5;
    for (int i = 0; i < 8; i++) if (b[i] != 2000 + i) return -6;
    return 0;
}

/* A variably-modified local moves %rsp the same way an alloca does. */
static int vla_form(int n) {
    _Alignas(32) double d[4];
    for (int i = 0; i < 4; i++) d[i] = i + 0.5;
    int vla[n];
    for (int i = 0; i < n; i++) vla[i] = 3000 + i;
    if (((unsigned long)d & 31) != 0) return -1;
    long sv = 0;
    for (int i = 0; i < n; i++) sv += vla[i];
    if (sv != (long)n * 3000 + (long)n * (n - 1) / 2) return -2;
    for (int i = 0; i < 4; i++) if (d[i] != i + 0.5) return -3;
    return 0;
}

static __attribute__((noinline)) int t_over_aligned_locals_survive_alloca(void)
{
    if (inlined_form() != 0) return 1;
    if (direct_form(16) != 0) return 2;
    if (direct_form(64) != 0) return 3;
    if (vla_form(5) != 0) return 4;
    if (vla_form(9) != 0) return 5;
    return 0;
}

/* ---- codegen_aggregate_zero_across_the_inline_threshold: exits 18..24
 * Zero-initializing an aggregate, across the size at which unrolling stops.
 *
 * The companion to `codegen_struct_copy_across_the_inline_threshold`, for the
 * other half of the same family. `emit_aggregate_zero` hand-rolled the same
 * 8/4/2/1 descent that `memexpand::block_chunks` already produces, but with
 * **no upper bound** — so `char buf[N] = {0}` emitted one store per chunk for
 * any N. Measured before the fix: 8 KB cost 2081 instructions in the function
 * body and 1 MB did not finish compiling in 25 minutes, while its sibling
 * `emit_block_copy_at_offset` had capped at `INLINE_LIMIT_BYTES` all along.
 *
 * The declaration is inside a loop on purpose. On entry the backend zeroes the
 * whole frame, which masks a missing zero-fill the first time through; only
 * re-execution shows it.
 *
 * Sizes straddle 128 and none is a multiple of 8, so a rounded-up or
 * short-by-a-tail fill shows as a wrong byte rather than passing by luck.
 */
void sink(char *p);

#define MKZ(N)                                                            \
    static int zero##N(void) {                                            \
        for (int pass = 0; pass < 2; pass++) {                            \
            unsigned char lo = 0xA5;                                      \
            char buf[N] = {0};                                            \
            unsigned char hi = 0x5A;                                      \
            for (int i = 0; i < N; i++)                                   \
                if (buf[i] != 0) return 1;                                \
            if (lo != 0xA5 || hi != 0x5A) return 2;                       \
            for (int i = 0; i < N; i++) buf[i] = (char)(i + 1);           \
            sink(buf);                                                    \
        }                                                                 \
        return 0;                                                         \
    }

MKZ(7)
MKZ(12)
MKZ(13)
MKZ(127)
MKZ(129)
MKZ(200)
MKZ(1000)

void sink(char *p) { (void)p; }

static __attribute__((noinline)) int t_aggregate_zero_across_the_inline_threshold(void)
{
    if (zero7()) return 1;
    if (zero12()) return 2;
    if (zero13()) return 3;
    if (zero127()) return 4;
    if (zero129()) return 5;
    if (zero200()) return 6;
    if (zero1000()) return 7;
    return 0;
}
#undef MKZ

int main(void)
{
    int r;
    if ((r = t_inlined_register_sized_aggregate_param()) != 0) return r;
    if ((r = t_wide_switch_survives_inlining()) != 0) return 6 + r;
    if ((r = t_over_aligned_locals_survive_alloca()) != 0) return 12 + r;
    if ((r = t_aggregate_zero_across_the_inline_threshold()) != 0) return 17 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("inlined_bodies_mega", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("inlined_bodies_mega_opt", code),
        0
    );
}

/// Inlining a body that `alloca`s must not extend the allocation's lifetime.
///
/// A real call releases the memory when it returns. Splicing the body in would
/// instead hold it until the *caller* returns, so a loop around the call takes
/// another bite of the stack every iteration -- half a million of them
/// overflows it. The splice brackets the body with a stack-pointer save and
/// restore, which is the lifetime the call had.
///
/// `alloca` used to disqualify the callee outright, which hid this; the
/// refusal was silent, and gcc inlines these.
#[test]
fn codegen_inlined_alloca_is_released_per_call() {
    let code = r#"
__attribute__((always_inline)) static inline long use(int n) {
    char *p = __builtin_alloca(n);
    for (int i = 0; i < n; i++) p[i] = (char)(i & 7);
    return p[n - 1];
}

/* Two inlined allocas in one expression, so the brackets have to nest
   correctly rather than merely balance overall. */
static long deep(int n) { return use(n) + use(n * 2); }

/* Leaves before the end of the body on half its calls. Both exits have to
   release the stack, or the loop below overflows. */
__attribute__((always_inline)) static inline int early(int i) {
    char *p = __builtin_alloca(512);
    p[0] = (char)(i & 1);
    if (p[0]) return 1;
    p[511] = 0;
    return 0;
}

int main(void) {
    long t = 0;
    for (int i = 0; i < 500000; i++) t += use(256);
    if (t != 500000L * 7) return 1;
    if (deep(8) != 7 + 7) return 2;

    /* An early return out of the inlined body still reaches the restore,
       since every path leaves through the continuation block. */
    for (int i = 0; i < 500000; i++) {
        if (early(i) != (i & 1)) return 3;
    }
    return 0;
}
"#;
    assert_eq!(compile_and_run("inlined_alloca_lifetime", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("inlined_alloca_lifetime_opt", code),
        0
    );
}

/// An aggregate returned in registers, read back after the returning function
/// is inlined.
///
/// A `Ret` can carry the *address* of the returned value rather than the value,
/// and `Function::ret_is_address` exists to keep the inliner from splicing
/// across that boundary. It knew about `_Complex` and about an x87 `long
/// double` aggregate, and missed a third shape: AAPCS64 returns a homogeneous
/// floating-point aggregate in registers at **any** size -- four `double`s is
/// thirty-two bytes and still comes back in `d0`-`d3` -- but the check was
/// gated behind the two-register return path, which stops at 128 bits.
///
/// So on aarch64 every HFA past 128 bits was inlined, and the continuation
/// phi-ed the returned *address* as though it were the aggregate. The caller
/// then read the pointer's own storage as the struct's bytes and got a
/// denormal. Only at -O2, because that is where the inliner's size threshold
/// admits these functions.
///
/// Sizes either side of the old 128-bit cap are covered, along with the
/// non-HFA aggregates of the same sizes -- those return through the hidden
/// pointer, where inlining is correct and must keep working.
#[test]
fn codegen_inlined_register_returned_aggregate() {
    let code = r#"
struct H2 { double v[2]; };     /* HFA, 16 bytes -- at the old cap    */
struct H3 { double v[3]; };     /* HFA, 24 bytes -- past it           */
struct H4 { double v[4]; };     /* HFA, 32 bytes -- past it           */
struct F4 { float  v[4]; };     /* HFA of floats, 16 bytes            */
struct L2 { long   v[2]; };     /* not an HFA, 16 bytes               */
struct L4 { long   v[4]; };     /* not an HFA, 32 bytes -- sret       */
struct M  { long a; double b; };/* mixed, 16 bytes                    */

static struct H2 mk2(double s){ struct H2 r; for(int i=0;i<2;i++) r.v[i]=s+i; return r; }
static struct H3 mk3(double s){ struct H3 r; for(int i=0;i<3;i++) r.v[i]=s+i; return r; }
static struct H4 mk4(double s){ struct H4 r; for(int i=0;i<4;i++) r.v[i]=s+i; return r; }
static struct F4 mkf(float  s){ struct F4 r; for(int i=0;i<4;i++) r.v[i]=s+i; return r; }
static struct L2 mkl2(long  s){ struct L2 r; for(int i=0;i<2;i++) r.v[i]=s+i; return r; }
static struct L4 mkl4(long  s){ struct L4 r; for(int i=0;i<4;i++) r.v[i]=s+i; return r; }
static struct M  mkm(long a, double b){ struct M r; r.a=a; r.b=b; return r; }

/* Each check is its own small function, and `noinline` keeps it that way.
   That is the point: the defect needs the *maker* inlined into its caller, and
   the inliner's growth limit declines to do that inside a caller that has
   already absorbed several. Folding these into main hides the bug entirely --
   the first version of this test did, and passed on aarch64 while it was live. */
__attribute__((noinline)) static int c2(double s){
    struct H2 b = mk2(s);
    for (int i = 0; i < 2; i++) if (b.v[i] != s + i) return 1;
    return 0; }
__attribute__((noinline)) static int c3(double s){
    struct H3 b = mk3(s);
    for (int i = 0; i < 3; i++) if (b.v[i] != s + i) return 2;
    return 0; }
__attribute__((noinline)) static int c4(double s){
    struct H4 b = mk4(s);
    for (int i = 0; i < 4; i++) if (b.v[i] != s + i) return 3;
    return 0; }
__attribute__((noinline)) static int cf(float s){
    struct F4 b = mkf(s);
    for (int i = 0; i < 4; i++) if (b.v[i] != s + i) return 4;
    return 0; }
__attribute__((noinline)) static int cl2(long s){
    struct L2 b = mkl2(s);
    for (int i = 0; i < 2; i++) if (b.v[i] != s + i) return 5;
    return 0; }
__attribute__((noinline)) static int cl4(long s){
    struct L4 b = mkl4(s);
    for (int i = 0; i < 4; i++) if (b.v[i] != s + i) return 6;
    return 0; }
__attribute__((noinline)) static int cm(void){
    struct M b = mkm(7, 2.5);
    if (b.a != 7 || b.b != 2.5) return 7;
    return 0; }

/* Each maker below is called from exactly one place. A maker with several
   call sites is judged differently by the inliner's growth heuristic and stops
   being inlined at all, which takes the defect with it. */
static struct H4 mk4b(double s){ struct H4 r; for(int i=0;i<4;i++) r.v[i]=s+i; return r; }
static struct H3 mk3b(double s){ struct H3 r; for(int i=0;i<3;i++) r.v[i]=s+i; return r; }
static struct H4 mk4c(double s){ struct H4 r; for(int i=0;i<4;i++) r.v[i]=s+i; return r; }
static struct H4 mk4d(double s){ struct H4 r; for(int i=0;i<4;i++) r.v[i]=s+i; return r; }

/* Consumed in place rather than stored to a named local. */
__attribute__((noinline)) static int cdirect(double s){
    if (mk4b(s).v[2] != s + 2) return 8;
    return 0; }
__attribute__((noinline)) static int cdirect3(double s){
    if (mk3b(s).v[1] != s + 1) return 9;
    return 0; }

/* Two live at once: each return site needs its own destination. */
__attribute__((noinline)) static int ctwo(void){
    struct H4 a = mk4c(10.5), b = mk4d(20.5);
    if (a.v[0] != 10.5 || b.v[0] != 20.5) return 10;
    if (a.v[3] != 13.5 || b.v[3] != 23.5) return 11;
    return 0; }

int main(void) {
    int r;
    if ((r = c2(1.5)))    return r;
    if ((r = c3(1.5)))    return r;
    if ((r = c4(1.5)))    return r;
    if ((r = cf(1.5f)))   return r;
    if ((r = cl2(10)))    return r;
    if ((r = cl4(10)))    return r;
    if ((r = cm()))       return r;
    if ((r = cdirect(1.5))) return r;
    if ((r = cdirect3(1.5))) return r;
    if ((r = ctwo()))     return r;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("inlined_reg_return_aggregate", code, &[]),
        0
    );
    // Explicitly -O2: `compile_and_run_optimized` builds at -O1, and the
    // inliner's size threshold only admits these makers at -O2, so the helper
    // alone never reaches the defect.
    assert_eq!(
        compile_and_run(
            "inlined_reg_return_aggregate_o2",
            code,
            &["-O2".to_string()]
        ),
        0
    );
}

/// Struct copies across the size at which the compiler stops unrolling.
///
/// `emit_block_copy` emitted one load, one store and a fresh pseudo per eight
/// bytes, with no upper bound. A 256 KB struct passed by value — which
/// gcc.c-torture's `pr28982b` does — cost 65,536 IR instructions and a
/// **65-second** compile, against gcc's 0.02. Above 128 bytes it now emits a
/// `memcpy` call instead; below, it still unrolls, because a call would cost
/// more than the moves it replaces.
///
/// Two bugs came out of that change, and both are pinned here:
///
/// - The parameter prologue and the sret return path each had their own copy
///   of the unrolled loop, written as `while offset < size` stepping 8 — which
///   rounds *up*. A 12-byte struct copied 16 bytes, four of them past the
///   object. Routing all three through the one helper fixed it.
/// - `memcpy` takes addresses, and a `Sym` pseudo names a local's storage
///   rather than a pointer to it. The inline path could store through it
///   directly; the call path could not, and every copy over the threshold
///   segfaulted until it went through `rvalue_addr`.
///
/// Sizes straddle 128 deliberately, and none is a multiple of 8, so a
/// rounded-up copy shows as corruption rather than passing by luck.
#[test]
fn codegen_struct_copy_across_the_inline_threshold() {
    let code = r#"
#define MK(N)                                                             \
    struct s##N { unsigned char c[N]; };                                  \
    static void take##N(struct s##N v) {                                  \
        for (int i = 0; i < N; i++)                                       \
            if (v.c[i] != (unsigned char)(i + 1)) __builtin_abort();      \
    }                                                                     \
    static struct s##N ret##N(struct s##N v) { return v; }                \
    static void run##N(void) {                                            \
        struct s##N a, b, d;                                              \
        unsigned char guard = 0xAB;                                       \
        for (int i = 0; i < N; i++) a.c[i] = (unsigned char)(i + 1);      \
        take##N(a);              /* by value into a parameter */          \
        b = ret##N(a);           /* returned, then assigned   */          \
        take##N(b);                                                       \
        d = a;                   /* plain struct assignment    */         \
        take##N(d);                                                       \
        if (guard != 0xAB) __builtin_abort();                             \
    }

MK(7)     /* under the threshold, not a multiple of 8 */
MK(12)    /* the over-copy case: 12 rounds up to 16   */
MK(13)
MK(127)   /* just under */
MK(129)   /* just over  */
MK(200)
MK(1000)  /* comfortably into call territory */

/* Larger than this is deliberately not here: it would be testing the
   aarch64 frame-offset legalization rather than the copy. That is covered
   on its own by `codegen_large_stack_frame_offsets_are_encodable`. */

int main(void) {
    run7(); run12(); run13(); run127(); run129(); run200(); run1000();
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(
                &format!("codegen_struct_copy_threshold{}", opt.replace('-', "_")),
                code,
                &[opt.to_string()]
            ),
            0,
            "struct copy across the inline threshold failed at {opt}"
        );
    }
}

/// A stack frame past the AArch64 immediate ranges.
///
/// AArch64 encodes an `add` immediate in twelve bits, optionally shifted left
/// by twelve, and a `str`/`ldr` offset in twelve bits scaled by the access
/// size. Two places emitted one without checking:
///
/// - taking a local's address produced `add x1, x29, #18032`, which the
///   assembler rejects outright (`Error: immediate out of range`);
/// - the prologue's frame zeroing produced `str xzr, [x29, #32768]`, one past
///   the 32760 its own comment called "all practical frames".
///
/// Neither is a wrong-answer bug: the assembler refuses the output, so the
/// build fails. It reached CI because the local aarch64 check only ran -O0,
/// and inlining at -O2 grows a frame that fit before. Three locals of 9001
/// bytes reproduces the first at -O0; a 40 KB array reaches the second.
///
/// x86-64 has no equivalent limit, so on this host the test guards a
/// regression rather than proving the fix — that was done by building for
/// aarch64 and assembling with the cross toolchain, at both opt levels.
#[test]
fn codegen_large_stack_frame_offsets_are_encodable() {
    let code = r#"
/* Past the add-immediate range: locals land beyond 4095. */
static int three_big_locals(void) {
    volatile unsigned char a[9001], b[9001], d[9001];
    a[0] = 1; b[9000] = 2; d[4500] = 3;
    return a[0] + b[9000] + d[4500];
}

/* Past the scaled store-offset range: the frame exceeds 32760. */
static int one_huge_local(void) {
    volatile unsigned char e[40000];
    e[0] = 4; e[39999] = 5;
    return e[0] + e[39999];
}

/* A frame the prologue zeroes with its loop rather than unrolled stores. */
static int very_huge_local(void) {
    volatile unsigned char f[100000];
    f[0] = 6; f[50000] = 7; f[99999] = 8;
    return f[0] + f[50000] + f[99999];
}

/* Addresses taken across the whole span, so the add path is exercised at
   several magnitudes rather than only the largest. */
static int addresses_across_the_frame(void) {
    volatile unsigned char g[20000];
    unsigned char *p0 = (unsigned char *)&g[0];
    unsigned char *p1 = (unsigned char *)&g[4000];
    unsigned char *p2 = (unsigned char *)&g[5000];
    unsigned char *p3 = (unsigned char *)&g[19999];
    *p0 = 1; *p1 = 2; *p2 = 3; *p3 = 4;
    return (p1 - p0 == 4000) && (p2 - p0 == 5000) && (p3 - p0 == 19999)
        && *p0 == 1 && *p1 == 2 && *p2 == 3 && *p3 == 4;
}

int main(void) {
    if (three_big_locals() != 6) return 1;
    if (one_huge_local() != 9) return 2;
    if (very_huge_local() != 21) return 3;
    if (!addresses_across_the_frame()) return 4;
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(
                &format!("codegen_large_frame{}", opt.replace('-', "_")),
                code,
                &[opt.to_string()]
            ),
            0,
            "large stack frame failed at {opt}"
        );
    }
}

/// `__typeof__` names a type, never a storage class. The declaration-specifier
/// parser carried the operand's `static`/`extern`/`_Thread_local` into the
/// new declaration, so `__typeof__(g) c = 0;` inside a function silently made
/// `c` a static local: it kept its value across calls and was shared between
/// recursive frames. gcc.c-torture's `split-path-5` is the same shape through
/// a subscript, where the static initializer check then rejected it.
#[test]
fn codegen_typeof_does_not_copy_storage_class() {
    let src = r#"
static int g;
static unsigned char pat[4] = {1, 2, 3, 4};
extern int e;
int e = 5;
_Thread_local int t;

int counter(void)
{
    __typeof__(g) c = 0;         /* must be automatic: a fresh 0 every call */
    __typeof__(e) d = 0;
    __typeof__(t) u = 0;
    static int sl;
    __typeof__(sl) w = 0;
    return ++c + ++d + ++u + ++w;
}

int depth(int n)
{
    static int s;
    __typeof__(s) mine = n;       /* each frame has its own */
    if (n > 0 && depth(n - 1) != n - 1) return -1;
    return mine;
}

int subscript(int i)
{
    __typeof__(pat[i]) x = pat[i];
    return x;
}

int main(void)
{
    if (counter() != 4) return 1;
    if (counter() != 4) return 2;
    if (depth(5) != 5) return 3;
    if (subscript(2) != 3) return 4;
    return 0;
}
"#;
    compile_and_run_everywhere("typeof_storage_class", src);
}

/// An inline definition emits no symbol, so the same name may also be an
/// alias: the body is there to inline, and the alias is what an out-of-line
/// call or the function's address reaches. gcc.c-torture `compile/20011119-1`
/// and `-2` are this shape, which c17 first rejected as a name defined twice.
// Mach-O has no symbol aliases, so c17 rejects `alias` on a Darwin host
// (`diagnostics_alias_attribute_unsupported_on_darwin` covers that side).
#[cfg(not(target_os = "macos"))]
#[test]
fn codegen_alias_beside_inline_definition() {
    let src = r#"
extern inline __attribute__((gnu_inline)) int foo(void) { return 23; }
int bar(void) { return foo(); }
extern int foo(void) __attribute__((weak, alias("xxx")));
int xxx(void) { return 24; }

int main(void)
{
    int (*p)(void) = foo;
    int b = bar();
    if (b != 23 && b != 24)
        return 1;
    if (p() != 24 || p != xxx)
        return 2;
    return 0;
}
"#;
    compile_and_run_everywhere("alias_beside_inline", src);
}

/// `__builtin_X` names the library function `X`, never an inline definition
/// of `X` in this unit. glibc's fortify wrappers are exactly that shape -- an
/// `always_inline` `gnu_inline` `extern inline` `strncpy` whose body calls
/// `__builtin_strncpy` -- and c17 bound the builtin to the wrapper itself, so
/// the wrapper looked recursive and was rejected as an `always_inline` that
/// could not be inlined (gcc.c-torture `compile/pr46360`).
#[test]
fn codegen_library_builtin_binds_past_inline_wrapper() {
    let src = r#"
typedef __SIZE_TYPE__ size_t;
extern char *strncpy(char *, const char *, size_t);
extern void *memcpy(void *, const void *, size_t);
extern size_t strlen(const char *);

int wrapped;

__attribute__((gnu_inline, always_inline)) extern inline char *
strncpy(char *dest, const char *src, size_t len)
{
    wrapped++;
    return __builtin_strncpy(dest, src, len);
}

__attribute__((gnu_inline, always_inline, artificial)) extern inline void *
memcpy(void *d, const void *s, size_t n)
{
    wrapped += 10;
    return __builtin___memcpy_chk(d, s, n, __builtin_object_size(d, 0));
}

int main(void)
{
    char buf[16];
    char out[16];
    if (strncpy(buf, "hello", sizeof buf) != buf)
        return 1;
    if (strlen(buf) != 5 || buf[4] != 'o' || buf[15] != 0)
        return 2;
    memcpy(out, buf, 6);
    if (out[0] != 'h' || out[5] != 0)
        return 3;
    if (wrapped != 11)
        return 4;
    return 0;
}
"#;
    compile_and_run_everywhere("library_builtin_wrapper", src);
}

/// A function that takes a label's address is inlined like any other, and
/// each copy has its own label: gcc.c-torture's 990208-1, where two callers
/// storing `&&here` from one `static inline` body must see different
/// addresses. c17 refused to inline any such function, so both stored the
/// out-of-line body's one address.
///
/// Inlining it renames the label symbol for every copy, including a copy of a
/// copy. What gcc refuses to copy stays out of line: a computed `goto`, which
/// may jump to an address saved by an earlier call (here, in another copy it
/// would name a block of a different function), and a label address in a
/// static table, which names the out-of-line body's blocks.
#[test]
fn codegen_inlined_label_address_is_per_copy() {
    let src = r#"
/* Label addresses in inlined functions. */
void exit(int);
#define FAIL() exit(__LINE__)

static void *ptr1, *ptr2;
static int one = 1;

/* No computed goto: gcc inlines this, and each copy has its own label. */
static inline void mark(void **pptr, int cond)
{
    if (cond) {
    here:
        *pptr = &&here;
    }
}
__attribute__((noinline)) static void f(int c) { mark(&ptr1, c); }
__attribute__((noinline)) static void g(int c) { mark(&ptr2, c); }

/* The address is taken and compared inside the same copy. */
static inline int self_equal(int c)
{
    void *p = &&top;
top:
    if (c-- > 0)
        return p == &&top;
    return 2;
}
__attribute__((noinline)) static int h1(int c) { return self_equal(c) + 1; }
__attribute__((noinline)) static int h2(int c) { return self_equal(c) * 3; }

/* Inlined twice over: the label is renamed at each step. */
static inline void *where(int c)
{
    if (c) {
    lab:
        return &&lab;
    }
    return (void *)0;
}
static inline void *twice(int c) { return where(c); }
__attribute__((noinline)) static void *w1(int c) { return twice(c); }
__attribute__((noinline)) static void *w2(int c) { return twice(c); }

/* always_inline is honoured at -O0 too, so the copies differ there. */
static inline __attribute__((always_inline)) void *ai(int c)
{
    if (c) {
    lab:
        return &&lab;
    }
    return (void *)0;
}
__attribute__((noinline)) static void *a1(int c) { return ai(c); }
__attribute__((noinline)) static void *a2(int c) { return ai(c); }

/* A computed goto: gcc never inlines this, because a label address saved
   on one call must stay valid on the next. */
static void *saved;
static inline int step(int x)
{
    if (!saved)
        saved = &&later;
    goto *saved;
later:
    return x + 1;
}
__attribute__((noinline)) static int s1(int x) { return step(x); }
__attribute__((noinline)) static int s2(int x) { return step(x) * 2; }

/* A small threaded interpreter called from two callers. */
static inline int run(const unsigned char *code, int acc)
{
    void *ops[] = { &&op_inc, &&op_dbl, &&op_halt };
    goto *ops[*code++];
op_inc:
    acc++;
    goto *ops[*code++];
op_dbl:
    acc *= 2;
    goto *ops[*code++];
op_halt:
    return acc;
}
static const unsigned char prog1[] = { 0, 1, 1, 0, 2 };
static const unsigned char prog2[] = { 1, 0, 0, 1, 2 };
__attribute__((noinline)) static int r1(int a) { return run(prog1, a); }
__attribute__((noinline)) static int r2(int a) { return run(prog2, a) + 100; }

/* A label address in a static table: gcc never copies this function, so
   both callers see the one table. */
static inline void *pick(int i)
{
    static void *const tbl[] = { &&a, &&b };
    if (i < 0) {
    a:
        return (void *)0;
    b:
        return (void *)1;
    }
    return tbl[i];
}
__attribute__((noinline)) static void *p1(int i) { return pick(i); }
__attribute__((noinline)) static void *p2(int i) { return pick(i); }

int main(void)
{
    f(one);
    g(one);
    if (!ptr1 || !ptr2)
        FAIL();
#ifdef __OPTIMIZE__
    if (ptr1 == ptr2)
        FAIL();
#endif
    if (h1(1) != 2 || h2(1) != 3 || h1(0) != 3 || h2(0) != 6)
        FAIL();
    if (!w1(one) || !w2(one) || w1(0))
        FAIL();
#ifdef __OPTIMIZE__
    if (w1(one) == w2(one))
        FAIL();
#endif
    if (!a1(one) || !a2(one) || a1(one) == a2(one) || a1(0))
        FAIL();
    if (s1(1) != 2 || s2(1) != 4 || s1(5) != 6 || s2(5) != 12)
        FAIL();
    if (r1(1) != 9 || r2(1) != 108)
        FAIL();
    if (p1(0) != p2(0) || p1(1) != p2(1) || !p1(0))
        FAIL();
    return 0;
}
"#;
    compile_and_run_everywhere("inlined_label_address", src);
}

/// The inliner moves an implicit parameter copy in whole eight-byte chunks,
/// which reads and writes past a object whose size is not a multiple of eight.
///
/// The third copy of the unrolled block move, after the two named in
/// `codegen_struct_copy_across_the_inline_threshold`. `while offset < size_bytes
/// { load 64; store 64; offset += 8 }` rounds *up*: a 12-byte `struct P` moved
/// 16 bytes, over-reading the argument and over-writing the callee's local.
/// `memexpand::block_chunks` descends 8/4/2/1 and is what the already-fixed
/// twin in the linearizer uses.
///
/// Stated on the IR because the overrun is layout-dependent: the four extra
/// bytes usually land in frame padding, so a program can be correct and still
/// be reading memory that does not belong to the object — and would fault if it
/// ended a page.
#[test]
fn codegen_an_inlined_parameter_copy_moves_no_more_than_the_object() {
    let src = r#"
struct P { float x, y, z; };
static float sum(struct P p) { return p.x + p.y + p.z; }
float probe(void) { struct P q = {1, 2, 3}; return sum(q); }
"#;
    let dir = plib::tmp::Builder::new()
        .prefix("inline_param_copy")
        .tempdir()
        .expect("tempdir");
    let c = dir.path().join("t.c");
    std::fs::write(&c, src).expect("write source");
    let r = crate::common::run_c17(&[
        "-O2",
        "--dump-ir",
        "post-opt",
        "--dump-ir-func",
        "probe",
        "-S",
        "-o",
        "/dev/null",
        c.to_str().unwrap(),
    ]);
    assert!(r.success, "compile failed: {}", r.stderr);
    let ir = format!("{}{}", r.stdout, r.stderr);

    // A 12-byte object has four bytes at offset 8, so a 64-bit access there is
    // four bytes past the end -- on the load side and again on the store side.
    // A correct copy reaches it with a 32-bit access.
    let overruns: Vec<&str> = ir
        .lines()
        .map(str::trim)
        .filter(|l| l.contains("+ 8") && (l.contains("load.64") || l.contains("store.64")))
        .collect();
    assert!(
        overruns.is_empty(),
        "a 12-byte object has 4 bytes at offset 8, so these access 4 past it:\n  {}\n\nfull IR:\n{ir}",
        overruns.join("\n  ")
    );

    // The control: the copy has to still be there. If the inliner stopped
    // inlining, or the parameter stopped being copied, the check above would
    // pass while testing nothing.
    assert!(
        ir.contains("+ 8"),
        "expected the inlined parameter copy to reach offset 8 at all:\n{ir}"
    );
}

/// A gnu_inline `extern inline` body followed by the unit's real definition
/// of the same name: the real one is the function, at every level and
/// through its address. Both bodies reached the module under one name, and
/// the inliner took the first -- the inline-only one -- from -O1 up.
#[test]
fn codegen_real_definition_wins_over_gnu_inline_body() {
    compile_and_run_everywhere(
        "gnu_inline_then_real",
        r#"
/* A gnu_inline `extern inline` body is only an inlining hint; the
   translation unit's real definition of the same name is the function. gcc
   calls (or inlines) the real one here at every level. */
extern inline __attribute__((gnu_inline)) int f(void) { return 1; }
int f(void) { return 0; }

/* The address names the real definition too. */
static int (*volatile pf)(void) = f;

int main(void)
{
    if (f() != 0) return 1;
    if (pf() != 0) return 3;
    return 0;
}
"#,
    );
}

/// Under GNU inline semantics only the definition's own `extern` makes it
/// inline-only. A plain `inline` gnu_inline definition is the real one even
/// with an `extern` declaration before it or after it, so its body is
/// emitted: gcc links all three of these calls against it.
#[test]
fn codegen_gnu_inline_definition_reads_only_its_own_extern() {
    compile_and_run_everywhere(
        "gnu_inline_own_extern",
        r#"
extern int before(void);
inline __attribute__((gnu_inline)) int before(void) { return 1; }

inline __attribute__((gnu_inline)) int after(void) { return 2; }
extern int after(void);

/* The inline-only body, then the real definition written `inline`. */
extern inline __attribute__((gnu_inline)) int both(void) { return 9; }
inline __attribute__((gnu_inline)) int both(void) { return 3; }

static int (*volatile pb)(void) = before;
static int (*volatile pa)(void) = after;
static int (*volatile pboth)(void) = both;

int main(void)
{
    if (before() != 1 || pb() != 1) return 1;
    if (after() != 2 || pa() != 2) return 2;
    if (both() != 3 || pboth() != 3) return 3;
    return 0;
}
"#,
    );
}
