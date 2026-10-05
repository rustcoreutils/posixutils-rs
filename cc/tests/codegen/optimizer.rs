//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Optimizer decisions seen from the generated code: folds, SSA and
// VRP shapes, constant conditions and the branches they decide.
//

use crate::common::{compile_and_run, compile_and_run_aarch64, compile_and_run_optimized};

// ============================================================================
// Mega-test: Optimization correctness
// ============================================================================

/// Optimizer regressions, one program run at -O1
/// (`compile_and_run_optimized`).
///
/// Consolidates these tests, one section each (each original `main` is
/// a `noinline` section function; the program exits with the section's
/// base plus the original code):
/// - `codegen_optimization_mega`: 1..=81
/// - `codegen_phisource_optimized`: 82..=85
#[test]
fn codegen_optimization_all_mega() {
    let code = r#"
/* ---- codegen_optimization_mega: exits 1..81
 */
int counter = 0;

int side_effect(int x) {
    counter++;
    return x * 2;
}

int global_var = 0;

static __attribute__((noinline)) int t_optimization_mega(void)
{
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

/* ---- codegen_phisource_optimized: exits 82..85
 */
static __attribute__((noinline)) int t_phisource_optimized(void)
{
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

int main(void)
{
    int r;
    if ((r = t_optimization_mega()) != 0) return r;
    if ((r = t_phisource_optimized()) != 0) return 81 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run_optimized("optimization_all_mega", code), 0);
}

// ============================================================================
// PhiSource integration tests
// ============================================================================

/// Phi sources, switches and `sizeof` forms, one program run at the
/// compile matrix levels.
///
/// Consolidates these tests, one section each (each original `main` is
/// a `noinline` section function; the program exits with the section's
/// base plus the original code):
/// - `codegen_phisource_ternary`: 1..=4
/// - `codegen_phisource_logical_and_or`: 5..=16
/// - `codegen_phisource_loop_phi`: 17..=20
/// - `codegen_switch_64bit`: 21..=25
/// - `codegen_ternary_div_by_zero`: 26..=30
/// - `codegen_sizeof_through_typeof`: 31..=42
/// - `codegen_sizeof_of_array_objects`: 43..=51
#[test]
fn codegen_optimizer_mega() {
    let code = r#"
/* ---- codegen_phisource_ternary: exits 1..4
 */
static __attribute__((noinline)) int t_phisource_ternary(void)
{
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

/* ---- codegen_phisource_logical_and_or: exits 5..16
 */
int side = 0;
int inc(void) { side++; return side; }

static __attribute__((noinline)) int t_phisource_logical_and_or(void)
{
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

/* ---- codegen_phisource_loop_phi: exits 17..20
 */
static __attribute__((noinline)) int t_phisource_loop_phi(void)
{
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

/* ---- codegen_switch_64bit: exits 21..25
 * Test: switch on long value compares all 64 bits
 */
int classify(long val) {
    switch (val) {
        case 0L: return 0;
        case 1L: return 1;
        case 0x100000000L: return 2;  // distinguishable only in upper 32 bits
        case 0x200000000L: return 3;
        default: return -1;
    }
}

static __attribute__((noinline)) int t_switch_64bit(void)
{
    if (classify(0L) != 0) return 1;
    if (classify(1L) != 1) return 2;
    if (classify(0x100000000L) != 2) return 3;
    if (classify(0x200000000L) != 3) return 4;
    if (classify(99L) != -1) return 5;

    return 0;
}

/* ---- codegen_ternary_div_by_zero: exits 26..30
 * Regression test: ternary operator with division evaluated both branches
 * unconditionally using cmov, causing SIGFPE when the divisor was zero.
 * `b == 0 ? 0 : (a / b) + 1` crashed because `a / b` was computed even
 * when `b == 0`.
 */
long safe_div(long a, long b) {
    return b == 0 ? 0 : (a / b) + 1;
}

int safe_mod(int a, int b) {
    return b == 0 ? -1 : a % b;
}

static __attribute__((noinline)) int t_ternary_div_by_zero(void)
{
    /* Division by zero should return 0, not crash */
    if (safe_div(10, 0) != 0) return 1;
    if (safe_div(10, 2) != 6) return 2;
    if (safe_div(100, 3) != 34) return 3;

    /* Modulo by zero should return -1, not crash */
    if (safe_mod(10, 0) != -1) return 4;
    if (safe_mod(10, 3) != 1) return 5;

    return 0;
}

/* ---- codegen_sizeof_through_typeof: exits 31..42
 * `sizeof` through `typeof` answers the real size (#C89).
 *
 * A `typeof` yielded a bare `TypeId`, and `int[]`, `int[n]` and `int[m]` all
 * intern to one type -- so a VLA's extent did not survive it and
 * `sizeof(typeof(a))` answered **0** where gcc answers the size. The two
 * operand forms need different mechanisms and both are exercised here:
 * `typeof(type-name)` carries the extent expressions out of the
 * specifier-qualifier list the way #C52 carries a type-name's own, while
 * `typeof(expr)` is rewritten to `sizeof(expr)`, whose answer lives in the
 * declaration of the object and which the linearizer already recorded.
 *
 * Both dimension orders are checked. Concatenating declarator and specifier
 * extents the wrong way round makes `int[3][n]` and `int[n][3]` the same size
 * -- right for one shape and wrong for another, the failure the 2026-08-17
 * series existed to remove.
 */
static __attribute__((noinline)) int t_sizeof_through_typeof(void)
{
    int n = 4;
    int a[n];
    int b[3][n];
    int fixed[4];

    /* typeof of an expression: the answer is the declaration's. */
    if (sizeof(typeof(a)) != 16) return 1;
    if (sizeof(typeof(b)) != 48) return 2;
    if (sizeof(typeof(fixed)) != 16) return 3;
    if (sizeof(typeof(n)) != sizeof(int)) return 4;

    /* typeof of a type-name: the extents ride out of the specifier list. */
    if (sizeof(typeof(int[n])) != 16) return 5;
    if (sizeof(typeof(int[n][2])) != 32) return 6;
    if (sizeof(typeof(int[3][n])) != 48) return 7;
    if (sizeof(typeof(int[n])[3]) != 48) return 8;
    if (sizeof(typeof(int)) != 4) return 9;
    if (sizeof(typeof(int[n]) *) != sizeof(void *)) return 10;

    /* A different extent gives a different answer, so nothing is being
       folded to a constant behind our back. */
    int m = 7;
    int c[m];
    if (sizeof(typeof(c)) != 28) return 11;
    if (sizeof(typeof(int[m])) != 28) return 12;

    return 0;
}

/* ---- codegen_sizeof_of_array_objects: exits 43..51
 * The sizes that #C112's check must not disturb, and the one it repaired.
 *
 * The two declarator paths disagreed about how an absent extent is spelled --
 * `parse_declarator` recorded `None`, the file-scope loop collapsed it to
 * `Some(0)` -- which made `int a[];` indistinguishable from the GNU
 * zero-length `int a[0];`. Both are exercised here, along with the composite
 * type 6.2.7p4 forms when a later declaration completes an earlier one.
 */
int inferred[] = {1, 2, 3};
int zero_length[0];
char from_string[] = "hi";
int two_d[2][3];
extern int completed[];
int completed[4];

static __attribute__((noinline)) int t_sizeof_of_array_objects(void)
{
    if (sizeof inferred != 3 * sizeof(int)) return 1;
    if (sizeof zero_length != 0) return 2;
    if (sizeof from_string != 3) return 3;
    if (sizeof two_d != 6 * sizeof(int)) return 4;
    if (sizeof completed != 4 * sizeof(int)) return 5;

    int n = 5;
    int vla[n];
    if (sizeof vla != 5 * sizeof(int)) return 6;
    int vla2[n][3];
    if (sizeof vla2 != 15 * sizeof(int)) return 7;

    /* A different extent gives a different answer, so nothing is folding to a
       constant behind the test's back. */
    n = 7;
    int vla3[n];
    if (sizeof vla3 != 7 * sizeof(int)) return 8;

    int fixed[4];
    if (sizeof fixed != 4 * sizeof(int)) return 9;
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_phisource_ternary()) != 0) return r;
    if ((r = t_phisource_logical_and_or()) != 0) return 4 + r;
    if ((r = t_phisource_loop_phi()) != 0) return 16 + r;
    if ((r = t_switch_64bit()) != 0) return 20 + r;
    if ((r = t_ternary_div_by_zero()) != 0) return 25 + r;
    if ((r = t_sizeof_through_typeof()) != 0) return 30 + r;
    if ((r = t_sizeof_of_array_objects()) != 0) return 42 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("optimizer_mega", code, &[]), 0);
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

// ============================================================================
// Test: switch-case block-scoped struct stack slot reuse
// ============================================================================

/// Folds and wide arithmetic the optimizer must keep exact, one program
/// run at the compile matrix levels and at -O1
/// (`compile_and_run_optimized`).
///
/// Consolidates these tests, one section each (each original `main` is
/// a `noinline` section function; the program exits with the section's
/// base plus the original code):
/// - `codegen_switch_case_slot_reuse`: 1..=4
/// - `codegen_checked_arith_128bit`: 5..=106
/// - `codegen_trunc_to_32_discards_the_upper_half`: 107..=117
/// - `codegen_vm_typedef_extents_index_the_variable_levels`: 118..=126
/// - `codegen_checked_sub_sees_a_negative_difference`: 127..=142
/// - `codegen_constant_shift_folds_at_the_operand_width`: 143..=162
#[test]
fn codegen_optimizer_folds_mega() {
    let code = r#"
/* ---- codegen_switch_case_slot_reuse: exits 1..4
 */
struct big { long a; long b; long c; long d; };

int test_switch(int sel) {
    int result = 0;
    switch (sel) {
    case 0: {
        struct big x = {1, 2, 3, 4};
        result = (int)(x.a + x.b + x.c + x.d);
        break;
    }
    case 1: {
        struct big y = {10, 20, 30, 40};
        result = (int)(y.a + y.b + y.c + y.d);
        break;
    }
    case 2: {
        struct big z = {100, 200, 300, 400};
        result = (int)(z.a + z.b + z.c + z.d);
        break;
    }
    case 3: {
        struct big w = {5, 6, 7, 8};
        result = (int)(w.a + w.b + w.c + w.d);
        break;
    }
    }
    return result;
}

static __attribute__((noinline)) int t_switch_case_slot_reuse(void)
{
    if (test_switch(0) != 10) return 1;
    if (test_switch(1) != 100) return 2;
    if (test_switch(2) != 1000) return 3;
    if (test_switch(3) != 26) return 4;
    return 0;
}

/* ---- codegen_checked_arith_128bit: exits 5..106
 * `__builtin_*_overflow` with a 128-bit destination. The ordinary lowering
 * computes in a type twice the destination's width and asks whether narrowing
 * lost anything; nothing is wider than 128 bits, so at that width it compared
 * a value to itself and always answered "no overflow". Every expectation here
 * was taken from `gcc -std=c17` on the same source.
 */
typedef __int128 i;
typedef unsigned __int128 u;
#define MAXI (((i)1 << 126) - 1 + ((i)1 << 126))
#define MINI (-MAXI - 1)
#define MAXU (~(u)0)

static int n = 0;
#define T(e, want) do { n++; if ((e) != (want)) return n; } while (0)

static __attribute__((noinline)) int t_checked_arith_128bit(void)
{
    i r;
    u ur;

    T(__builtin_add_overflow(MAXI, (i)1, &r), 1);
    T(__builtin_add_overflow(MAXI - 1, (i)1, &r), 0);
    T(__builtin_add_overflow(MINI, (i)-1, &r), 1);
    T(__builtin_add_overflow((i)5, (i)7, &r), 0);

    T(__builtin_sub_overflow(MINI, (i)1, &r), 1);
    T(__builtin_sub_overflow(MAXI, (i)-1, &r), 1);
    T(__builtin_sub_overflow((i)5, (i)7, &r), 0);

    /* The multiply check runs on magnitudes; MIN * -1 is the case a
       division-based check traps on, so it is here deliberately. */
    T(__builtin_mul_overflow(MAXI, (i)2, &r), 1);
    T(__builtin_mul_overflow(MINI, (i)-1, &r), 1);
    T(__builtin_mul_overflow(MINI, (i)1, &r), 0);
    T(__builtin_mul_overflow((i)1 << 126, (i)2, &r), 1);
    T(__builtin_mul_overflow((i)1 << 125, (i)2, &r), 0);
    T(__builtin_mul_overflow((i)0, MAXI, &r), 0);
    T(__builtin_mul_overflow(MAXI, (i)0, &r), 0);
    T(__builtin_mul_overflow((i)-3, (i)5, &r), 0);
    T(__builtin_mul_overflow(MINI / 2, (i)2, &r), 0);
    T(__builtin_mul_overflow(MINI / 2, (i)-2, &r), 1);

    T(__builtin_add_overflow(MAXU, (u)1, &ur), 1);
    T(__builtin_add_overflow(MAXU - 1, (u)1, &ur), 0);
    T(__builtin_sub_overflow((u)0, (u)1, &ur), 1);
    T(__builtin_sub_overflow((u)5, (u)3, &ur), 0);
    T(__builtin_mul_overflow(MAXU, (u)2, &ur), 1);
    T(__builtin_mul_overflow((u)1 << 127, (u)2, &ur), 1);
    T(__builtin_mul_overflow((u)1 << 126, (u)2, &ur), 0);
    T(__builtin_mul_overflow((u)0, MAXU, &ur), 0);

    /* Narrower operands widening into a 128-bit destination: the conversion is
       exact, so these must agree with infinite precision. */
    T(__builtin_mul_overflow(1000000, 1000000, &r), 0);
    if ((long long)r != 1000000000000LL) return 100;
    {
        unsigned long um = ~0UL;
        T(__builtin_mul_overflow(um, um, &r), 1);
    }
    T(__builtin_add_overflow(-5, 3, &r), 0);
    if ((long long)r != -2) return 101;

    /* And the narrower destinations still behave. */
    {
        int q;
        T(__builtin_add_overflow(2000000000, 2000000000, &q), 1);
        T(__builtin_mul_overflow(65536, 65536, &q), 1);
        T(__builtin_add_overflow(1, 2, &q), 0);
        if (q != 3) return 102;
    }
    return 0;
}
#undef MAXI
#undef MINI
#undef MAXU
#undef T

/* ---- codegen_trunc_to_32_discards_the_upper_half: exits 107..117
 * Truncating a 128-bit value to 32 bits left the whole low half in the
 * register. "Already the right width" was true only of the *store* that
 * usually follows a truncation; anything that read the pseudo directly saw
 * the bits that were supposed to have been dropped.
 *
 * `__builtin_add_overflow` is exactly that shape -- compute wide, narrow to
 * the destination, widen back, compare -- so it compared its result against
 * an untruncated copy of itself and could never report an overflow on
 * aarch64. Both aarch64 CI targets failed on it; x86-64 emits the same
 * self-move and was always right.
 */
static __attribute__((noinline)) int t_trunc_to_32_discards_the_upper_half(void)
{
    unsigned ur;
    unsigned x = 4294967295u, y = 1u;

    /* The builtin, which is how this was found. */
    if (!__builtin_add_overflow(x, y, &ur)) return 1;
    if (ur != 0) return 2;
    if (!__builtin_add_overflow(4294967295u, 1u, &ur)) return 3;
    if (__builtin_add_overflow(1u, 2u, &ur) || ur != 3) return 4;

    /* And the narrowing on its own, which is the actual defect. */
    {
        unsigned __int128 wide = ((unsigned __int128)1 << 40) | 0x5u;
        unsigned narrow = (unsigned)wide;
        unsigned __int128 back = (unsigned __int128)narrow;
        if (narrow != 5u) return 5;
        if (back != 5u) return 6;
        if ((unsigned long long)back != 5ull) return 7;
    }
    {
        __int128 wide = -((__int128)1 << 40) - 3;
        int narrow = (int)wide;
        long long back = (long long)narrow;
        if (narrow != -3) return 8;
        if (back != -3LL) return 9;
    }
    /* Signed 128 to unsigned 32 and back, where the discarded bits are set. */
    {
        __int128 w = ((__int128)7 << 32) | 0xdeadbeefu;
        unsigned n = (unsigned)w;
        unsigned __int128 b = (unsigned __int128)n;
        if (n != 0xdeadbeefu) return 10;
        if (b != 0xdeadbeefu) return 11;
    }
    return 0;
}

/* ---- codegen_vm_typedef_extents_index_the_variable_levels: exits 118..126
 * A variably modified typedef's extents are indexed by *variable* extent.
 *
 * The parser mints one `VmTypedefExtent(sym, k)` per size expression -- one
 * per variable level, since a constant level has no expression to evaluate --
 * but the linearizer recorded one entry per *array level*, constants
 * included. The two agreed only while every level was variable. Add one
 * constant extent and the index slid:
 *
 *   typedef int T[2][n]; T a;   read the constant 2 where `n` belonged, so
 *                               `sizeof a` was 16 against gcc's 80 and every
 *                               write past `a[0][3]` landed outside `a`.
 */
#include <string.h>

static __attribute__((noinline)) int t_vm_typedef_extents_index_the_variable_levels(void)
{
    int n = 10, m = 5;

    /* A leading constant extent: the case that slid. */
    {
        typedef int T[2][n];
        T a;
        if (sizeof a != 2 * 10 * sizeof(int)) return 1;
        if (sizeof a[0] != 10 * sizeof(int)) return 2;
        memset(a, 0, sizeof a);
        a[1][9] = 7;
        if (a[1][9] != 7) return 3;
    }

    /* A trailing constant extent. */
    {
        typedef int T[n][4];
        T a;
        if (sizeof a != 10 * 4 * sizeof(int)) return 4;
        memset(a, 0, sizeof a);
        a[9][3] = 7;
        if (a[9][3] != 7) return 5;
    }

    /* A constant between two variables. */
    {
        typedef int T[n][3][m];
        T a;
        if (sizeof a != 10 * 3 * 5 * sizeof(int)) return 6;
        memset(a, 0, sizeof a);
        a[9][2][4] = 7;
        if (a[9][2][4] != 7) return 7;
    }

    /* All variable: the shape that always worked, kept as the control. */
    {
        typedef int T[n][m];
        T a;
        if (sizeof a != 10 * 5 * sizeof(int)) return 8;
    }

    /* 6.7.7p3: the extents are the ones in effect at the typedef. */
    {
        typedef int T[2][n];
        n = 100;
        T a;
        if (sizeof a != 2 * 10 * sizeof(int)) return 9;
    }

    return 0;
}

/* ---- codegen_checked_sub_sees_a_negative_difference: exits 127..142
 * A checked subtraction computes signed, because a difference can be
 * negative even when every operand is unsigned.
 *
 * Negative is exactly the unrepresentable case for an unsigned destination,
 * and computing in an unsigned 128-bit type wraps it instead. Then nothing
 * downstream could see it: `exact < 0` is never true in unsigned arithmetic,
 * and the narrow fast path returned a hard "no overflow" whenever the wide
 * type equalled the destination.
 */
static __attribute__((noinline)) int t_checked_sub_sees_a_negative_difference(void)
{
    unsigned __int128 u;
    unsigned long long ull;
    unsigned un;

    /* The regression: a negative difference into an unsigned 128-bit
       destination. */
    if (!__builtin_sub_overflow(1u, 2u, &u)) return 1;
    if (!__builtin_sub_overflow(0u, 1u, &u)) return 2;
    if (!__builtin_sub_overflow(0ull, 1ull, &u)) return 3;

    /* And one that does not overflow. */
    if (__builtin_sub_overflow(5u, 2u, &u)) return 4;
    if (u != 3) return 5;

    /* Narrower unsigned destinations were already right; keep them so. */
    if (!__builtin_sub_overflow(1u, 2u, &un)) return 6;
    if (__builtin_sub_overflow(5u, 2u, &un)) return 7;
    if (un != 3) return 8;
    if (!__builtin_sub_overflow(0ull, 1ull, &ull)) return 9;

    /* Signed destinations, and the sibling operations. */
    __int128 s;
    if (__builtin_sub_overflow(1u, 2u, &s)) return 10;
    if (s != -1) return 11;
    if (__builtin_add_overflow(1u, 2u, &u)) return 12;
    if (u != 3) return 13;
    if (__builtin_mul_overflow(3u, 4u, &u)) return 14;
    if (u != 12) return 15;

    /* A product of two 64-bit values still needs the unsigned range. */
    unsigned long long big = 0xFFFFFFFFFFFFFFFFULL;
    if (__builtin_mul_overflow(big, big, &u)) return 16;

    return 0;
}

/* ---- codegen_constant_shift_folds_at_the_operand_width: exits 143..162
 * A constant right shift must fold at the operand's width, with the
 * signedness the opcode implies.
 *
 * `Function::const_val` hands back an `i128` holding whatever bit pattern the
 * constant was built from, and for a narrower operand that can be wider than
 * the operand: `(int)0xFFFFFFFFu` is stored as 4294967295, not -1. Every
 * consumer that only *emits* the value truncates and never noticed. Folding
 * arithmetic on it does notice — `Asr` shifted in zeros and answered
 * 2147483647 where the operand is -1 and the answer is -1.
 *
 * So this was a silent wrong answer in ordinary C, at `-O` and above only,
 * for any arithmetic shift of a constant whose value came through a cast from
 * an unsigned literal. `-1 >> 1` was always right, which is why nothing
 * caught it: the literal is already negative there, so the stored i128 agrees
 * with the operand.
 *
 * Found while lowering `__builtin_clrsb`, which folds a sign bit down and so
 * feeds the folder exactly this shape.
 */
static __attribute__((noinline)) int t_constant_shift_folds_at_the_operand_width(void)
{
    /* Arithmetic: the operand is negative at its own width. */
    if (((int)0x80000000) >> 31 != -1) return 1;
    if (((int)0x80000000) >> 1 != -1073741824) return 2;
    if (((int)0xFFFFFFFFu) >> 1 != -1) return 3;
    if (((int)0xC0000000u) >> 7 != -8388608) return 4;
    if (((int)2147483648u) >> 31 != -1) return 5;
    if (((signed char)0x80) >> 3 != -16) return 6;
    if (((short)0x8000) >> 5 != -1024) return 7;
    if (((long long)0x8000000000000000LL) >> 1 != -4611686018427387904LL) return 8;
    if (((long long)0xFFFFFFFFFFFFFFFFULL) >> 40 != -1LL) return 9;

    /* Logical shifts must stay logical. */
    if ((0xFFFFFFFFu >> 1) != 2147483647u) return 10;
    if ((0x80000000u >> 31) != 1u) return 11;
    if ((0xFFFFFFFFFFFFFFFFULL >> 40) != 16777215ULL) return 12;

    /* Left shifts wrap at the operand width rather than growing. */
    if ((int)(0x7FFFFFFF << 1) != -2) return 13;
    if ((int)(3 << 30) != -1073741824) return 14;
    if ((unsigned)(0xFFFFFFFFu << 4) != 0xFFFFFFF0u) return 15;

    /* The shapes that were always correct must stay so. */
    if ((-1 >> 1) != -1) return 16;
    if ((-2 >> 1) != -1) return 17;
    if ((-256 >> 4) != -16) return 18;
    if ((255 >> 4) != 15) return 19;
    if ((5 << 3) != 40) return 20;
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_switch_case_slot_reuse()) != 0) return r;
    if ((r = t_checked_arith_128bit()) != 0) return 4 + r;
    if ((r = t_trunc_to_32_discards_the_upper_half()) != 0) return 106 + r;
    if ((r = t_vm_typedef_extents_index_the_variable_levels()) != 0) return 117 + r;
    if ((r = t_checked_sub_sees_a_negative_difference()) != 0) return 126 + r;
    if ((r = t_constant_shift_folds_at_the_operand_width()) != 0) return 142 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("optimizer_folds_mega", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("optimizer_folds_mega_opt", code),
        0
    );
}

/// Sparse conditional constant propagation: constants follow the *reachable*
/// paths, so a branch proved not taken takes its whole arm with it.
///
/// The half that matters is the negative half. A pass that folds a branch it
/// has not proved dead deletes side effects, which is how a DCE change once
/// broke every `subprocess.run` in CPython -- so the loop counter, the
/// `volatile` read, the opaque guard on a `_exit`, and the computed goto are
/// the point of this test, not the folds above them.
#[test]
fn codegen_sccp_folds_only_what_it_proves() {
    let code = r#"
#include <unistd.h>
extern void link_error(void);

int seen;
__attribute__((noinline)) static void note(int v) { seen += v; }

int main(int argc, char **argv)
{
    /* A constant condition takes its dead arm with it, including the call
       to a function that is never defined -- so this fails to *link* rather
       than to run if the fold does not happen. */
    if (0) link_error();

    /* A value that is constant only once the branch above it has folded. */
    int x; if (1) x = 3; else x = 4;
    if (x != 3) link_error();

    /* A switch on a constant, with a GNU range arm and a default. */
    switch (2) {
    case 1: link_error(); break;
    case 3 ... 9: link_error(); break;
    case 2: break;
    default: link_error();
    }
    switch (40) {
    case 1: link_error(); break;
    case 30 ... 50: break;
    default: link_error();
    }
    switch (99) {
    case 1: link_error(); break;
    default: break;
    }

    /* `?:` on a constant condition. */
    if ((1 ? 11 : 22) != 11) return 1;
    if ((0 ? 11 : 22) != 22) return 2;

    /* --- and now the things that must NOT fold --- */

    /* A loop counter is not its initial value. */
    int s = 0; for (int i = 0; i < 5; i++) s += i;
    if (s != 10) return 3;

    /* A volatile read is not a constant however it was stored. */
    volatile int z = 0; z = 1;
    if (!z) return 4;

    /* An opaque guard on a noreturn call: the arm does real work before
       `_exit`, and folding past it is the shape that broke CPython. */
    if (argc > 99) { note(1); _exit(77); }

    /* A computed goto reaches a block with no CFG edge into it. */
    void *tbl[2] = { &&a, &&b };
    goto *tbl[argc > 1 ? 1 : 0];
a:  note(2); goto done;
b:  note(4); goto done;
done:
    if (seen != 2) return 5;

    /* A phi whose arms genuinely differ. */
    int q; if (argc > 1) q = 7; else q = 9;
    if (q != 9) return 6;

    /* Signed shift identities must keep their sign. */
    if ((-1 >> 1) != -1) return 7;
    /* Unsigned wraparound is not undefined and must fold to the wrapped
       value, not to the mathematical one. */
    { unsigned u = 0u - 1u; if (u != 4294967295u) return 8; }

    (void)argv;
    return 0;
}
"#;
    for opt in ["-O1", "-O2"] {
        assert_eq!(
            compile_and_run("codegen_sccp_folds", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// The dead arm goes even when it is the *only* thing that would have made
/// the program link, and the program still runs correctly.
///
/// Separate from the test above because it must also pass at `-O0`, where
/// SCCP does not run at all: `link_error` is defined here, so nothing depends
/// on the fold happening, only on the answer being right either way.
#[test]
fn codegen_sccp_agrees_with_the_unoptimized_answer() {
    let code = r#"
int calls;
static int side(int v) { calls++; return v; }

int main(void)
{
    int total = 0;

    /* Every arm here is decided at compile time, but the *answers* must be
       the same as running it. */
    if (2 + 2 == 4) total += 1; else total += 100;
    if (3 > 7) total += 100; else total += 2;
    total += (5 & 3) == 1 ? 4 : 100;
    total += (1 << 3) == 8 ? 8 : 100;
    total += (7 % 3) == 1 ? 16 : 100;
    total += (-8 / 2) == -4 ? 32 : 100;

    /* A call in a dead arm must not run. */
    if (0) side(1);
    if (calls != 0) return 1;

    /* A call in a live arm must. */
    if (1) side(1);
    if (calls != 1) return 2;

    return total == 63 ? 0 : 3;
}
"#;
    for opt in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run("codegen_sccp_answers", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// A bit builtin over a value only a branch pins down folds like any other
/// operation over a constant. `k` is an argument, so the constant exists only
/// on the edge `k == 8` (or `k == 256`) proves -- which is value-range
/// propagation's to see, and it used to give up on every bit operation even
/// over a one-value range. `link_error` is never defined, so a missed fold
/// fails the link.
#[test]
fn codegen_vrp_folds_a_bit_builtin_over_an_edge_constant() {
    let code = r#"
extern void link_error(void);

__attribute__((noinline)) int f(unsigned k, unsigned long l)
{
    int r = 0;
    if (k == 8) {
        if (__builtin_popcount(k) != 1) link_error();
        if (__builtin_ctz(k) != 3) link_error();
        if (__builtin_clz(k) != 28) link_error();
        r += 1;
    }
    if (l == 256) {
        if (__builtin_bswap64(l) != 0x0001000000000000ul) link_error();
        r += 2;
    }
    return r;
}

int main(void)
{
    return f(8, 256) == 3 && f(1, 1) == 0 ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run("codegen_vrp_bit_builtin", code, &["-O2".to_string()]),
        0
    );
}

/// Shift identities that hold for *every* shift count, and the comparisons
/// that prove them.
///
/// Two things were needed and neither was the shift. `x >> 0 != x` folds the
/// shift to a `Copy` already, but the comparison did not: promotion out of
/// memory gives each use of `x` its own copy, so the two sides arrive as
/// distinct pseudos naming one value, and the identity test compared raw ids.
/// And `-1 >> x` is only recognizable when the operand is read *signed* at its
/// own width -- read either way, as the folder must when nothing tells it
/// which, `-1` at 32 bits is ambiguous and was refused.
///
/// The negative half is the point: a *logical* shift of all-ones shifts in
/// zeros, so `0xFFFFFFFFu >> x` is all-ones only when `x` is zero.
#[test]
fn codegen_shift_identities_hold_for_any_count() {
    let code = r#"
extern void link_error(void);

__attribute__((noinline)) static void utest(unsigned int x)
{
    if (x >> 0 != x) link_error();
    if (x << 0 != x) link_error();
    if (0 << x != 0) link_error();
    if (0 >> x != 0) link_error();
    if (-1 >> x != -1) link_error();
    if (~0 >> x != ~0) link_error();
}

__attribute__((noinline)) static void stest(int x)
{
    if (x >> 0 != x) link_error();
    if (x << 0 != x) link_error();
    if (0 << x != 0) link_error();
    if (0 >> x != 0) link_error();
}

/* The identity must not be claimed where it does not hold. A logical shift
   of all-ones is all-ones only for a zero count, so these must still be
   evaluated rather than folded away. */
__attribute__((noinline)) static int lsr_is_not_asr(unsigned int x)
{
    unsigned int all = 0xFFFFFFFFu;
    return (all >> x) == all;
}

int main(void)
{
    utest(9); utest(0); stest(9); stest(0);
    if (!lsr_is_not_asr(0)) return 1;
    if (lsr_is_not_asr(1)) return 2;
    if (lsr_is_not_asr(31)) return 3;
    return 0;
}
"#;
    // `link_error` is deliberately never defined, so a surviving call is a
    // *link* failure -- which is the whole proof. `-O0` is therefore not in
    // the list: nothing folds there, exactly as the torture test's own
    // `#ifndef __OPTIMIZE__` fallback definition concedes.
    for opt in ["-O1", "-O2"] {
        assert_eq!(
            compile_and_run("c17_shift_identities", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// Two comparisons of the same operand pair answer each other.
///
/// `&&` and `||` lower to a diamond because C forbids evaluating the right
/// operand once the left has decided. When evaluating it anyway cannot be
/// observed, if-conversion collapses the diamond into a `select`, and the two
/// relationals end up side by side where a peephole can compare their
/// orderings: `&&` is never true when they are disjoint, `||` always true when
/// together they cover less, equal and greater.
///
/// Operands written the other way round count: `(x < y) && (y < x)` is the
/// same disjointness with the mask mirrored.
#[test]
fn codegen_relational_pairs_over_one_operand_pair_fold() {
    let code = r#"
extern void link_error0(void);
extern void link_error1(void);

__attribute__((noinline)) static void never(int x, int y)
{
    if ((x == y) && (x != y)) link_error0();
    if ((x < y) && (x > y)) link_error0();
    if ((x < y) && (y < x)) link_error0();
    if ((x <= y) && (y < x)) link_error0();
}

__attribute__((noinline)) static void always(int x, int y)
{
    if ((x == y) || (x != y)) { } else link_error1();
    if ((x >= y) || (x < y)) { } else link_error1();
    if ((x <= y) || (y < x)) { } else link_error1();
}

/* Signed and unsigned comparisons of one pair are not each other's
   complements. Read without regard to signedness these two orderings would
   be exhaustive -- less, together with greater-or-equal -- and the whole
   thing would fold to 1. It must not. */
__attribute__((noinline)) static int mixed(int x, int y)
{
    return ((x < y) || ((unsigned)x >= (unsigned)y)) ? 1 : 0;
}

int main(void)
{
    never(0, 0); never(1, 2); never(4, 3);
    always(0, 0); always(1, 2); always(4, 3);
    /* Signed says -1 < 1, so the first arm carries it. */
    if (!mixed(-1, 1)) return 1;
    /* The case that proves it did not fold: signed says 1 < -1 is false, and
       unsigned says 1 >= 0xFFFFFFFF is false too, so the answer is 0. A fold
       that ignored signedness would have answered 1. */
    if (mixed(1, -1)) return 2;
    return 0;
}
"#;
    // `link_error0`/`link_error1` are never defined, so a surviving call is a
    // link failure. `-O0` folds nothing, exactly as the torture test's own
    // `#ifndef __OPTIMIZE__` fallback concedes.
    for opt in ["-O1", "-O2"] {
        assert_eq!(
            compile_and_run("c17_relational_pairs", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// An `if` that assigns several variables leaves one phi per variable at
/// its merge, and if-conversion turns them all into selects at once -- each
/// over its own pair of values, whatever its type -- or leaves the branch
/// alone. Checked by value on both edges.
#[test]
fn codegen_ifconv_collapses_every_phi_at_a_merge() {
    let code = r#"
extern void abort(void);

__attribute__((noinline)) static long merged(int c, int x, double dv)
{
    int a = 7;
    long b = -5;
    short s = 300;
    double d = 0.5;
    if (c > x) {
        a = x + 1;
        b = (long)x * 3;
        s = (short)(x ^ 0x55);
        d = dv;
    }
    return a + b + s + (long)(d * 4.0);
}

int main(void)
{
    if (merged(0, 10, 2.25) != 7 - 5 + 300 + 2) abort();
    if (merged(20, 10, 2.25) != 11 + 30 + (10 ^ 0x55) + 9) abort();
    if (merged(-1, -3, -1.0) != -2 - 9 + (short)(-3 ^ 0x55) - 4) abort();
    return 0;
}
"#;
    for opt in ["-O1", "-O2"] {
        assert_eq!(
            compile_and_run("c17_ifconv_every_phi", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// A `const` global's initializer is its value for the whole run, so a load
/// of one becomes that constant.
///
/// No alias information and no escape analysis are needed for this, and the
/// escaping pointer below is the point: modifying an object defined with a
/// `const`-qualified type is undefined behaviour (C17 6.7.3p6), so a pointer
/// to one may go anywhere at all and the value still cannot change.
///
/// The refusals are the other half, and each names something that *can*
/// change underneath the fold: `volatile` says so outright, a weak
/// definition exists to be replaced at link time, a tentative definition may
/// be merged with a real one in another translation unit, an `extern`
/// declaration has its definition there already, and a type-punned read is
/// asking for different bits than the initializer describes.
#[test]
fn codegen_const_globals_propagate() {
    let code = r#"
extern void link_error(void);
extern void abort(void);

const double one = 1.0;
const int two = 2;
const float half = 0.5f;
const long double big = 1.5L;
const char letter = 'A';
static const int internal = 9;

volatile const int watched = 5;
const int replaceable __attribute__((weak)) = 7;
const int tentative;
extern const int elsewhere_defined;
int mutable_global = 3;

/* Reading a `const` object through a pointer that escaped this function
   still reads the initializer -- storing through it would be undefined. */
__attribute__((noinline)) static int through(const int *p) { return *p; }

int main(void)
{
    if ((int) one != 1) link_error();
    if (two != 2) link_error();
    if (half != 0.5f) link_error();
    if (letter != 'A') link_error();
    if (internal != 9) link_error();
    /* Arithmetic on folded constants folds too. */
    if (one * 2.0 != 2.0) link_error();
    if (two + internal != 11) link_error();

    /* The `long double` load folds on every target -- but comparing two of
       them is a libcall where the type is soft-float (binary128 on Linux
       aarch64), and a call is not something the optimizer folds. Checked by
       value, so the fold is still exercised without assuming the host's
       `long double`. */
    if (big != 1.5L) abort();

    if (through(&two) != 2) abort();

    /* These must still be read from memory; each is checked by value so the
       test fails loudly if one were folded to the wrong thing. */
    if (watched != 5) abort();
    if (replaceable != 7) abort();
    if (tentative != 0) abort();
    if (mutable_global != 3) abort();
    mutable_global = 4;
    if (mutable_global != 4) abort();

    /* A read of the same bytes as a different kind of value is not the
       initializer, and must not be answered with it. */
    if (*(const long *)&one != 0x3FF0000000000000L) abort();

    return 0;
}

/* Defined after use, so `elsewhere_defined` is a declaration at the point
   the optimizer would fold it. */
const int elsewhere_defined = 11;
"#;
    for opt in ["-O1", "-O2"] {
        assert_eq!(
            compile_and_run("c17_const_global_propagate", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// Integer width conversions fold over a constant, at the width the operand
/// was stored in rather than the one it is being widened to.
///
/// The source width is the whole of it: read at the destination instead, a
/// sign extension is the identity, so `(int)(signed char)200` answers 200
/// where C says -56. Every `char` and `short` read is widened before
/// anything is done with it, so a fold that stops here stops one instruction
/// after it started.
#[test]
fn codegen_integer_conversions_fold() {
    let code = r#"
extern void link_error(void);
extern void abort(void);

volatile int sink;

int main(void)
{
    /* Sign extension reads the operand signed at its own width. */
    if ((int)(signed char) 200 != -56) link_error();
    if ((int)(signed char) -56 != -56) link_error();
    if ((int)(short) 70000 != 4464) link_error();
    if ((long)(int) -1 != -1L) link_error();

    /* Zero extension reads it unsigned. */
    if ((int)(unsigned char) 200 != 200) link_error();
    if ((int)(unsigned short) 70000 != 4464) link_error();
    if ((unsigned long)(unsigned) -1 != 0xFFFFFFFFUL) link_error();

    /* Truncation keeps the low bits, and what it means then depends on the
       type it is read back as. These are checked by value rather than for
       folding: a truncated constant that reads two ways must NOT be folded,
       because nothing in the IR says how its consumer widens it. */
    if ((unsigned char) 200 != 200) abort();
    if ((signed char) 200 != -56) abort();
    if ((unsigned char) -1 != 255) abort();
    if ((signed char) -1 != -1) abort();
    /* Plain `char` is signed on x86-64 and unsigned on aarch64, so this
       asserts the agreement rather than a number: the folded conversion must
       give what the same conversion gives at run time. */
    sink = 0xEF;
    {
        int v = sink;
        if ((int)(char) 0x123456789ABCDEFLL != (int)(char) v) abort();
    }

    /* A narrow value promoted by a unary operator, with no extension in
       the IR between the two: the truncated constant must not be folded to
       one of its two readings. */
    if (-(unsigned char) 200 != -200) abort();
    if (-(unsigned short) 60000 != -60000) abort();
    if (~(unsigned char) 200 != ~200) abort();

    /* The same conversions over values the optimizer cannot know must still
       give the same answers. */
    sink = 200;
    { int v = sink; if ((int)(signed char) v != -56) abort(); }
    sink = 70000;
    { int v = sink; if ((int)(short) v != 4464) abort(); }
    sink = -1;
    { int v = sink; if ((int)(unsigned char) v != 255) abort(); }

    return 0;
}
"#;
    for opt in ["-O1", "-O2"] {
        assert_eq!(
            compile_and_run("c17_integer_conversion_fold", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// Value-range propagation: on the edge where a condition is false, its
/// operands are constrained, and that constraint carries through arithmetic
/// and widening to decide a later comparison.
///
/// `var <= 0` being false means `var >= 1`, so `var - 1` is non-negative, so
/// `(unsigned)(var - 1)` is not `UINT_MAX`, so the `||` is exhaustive. No
/// other pass here can reach that: `sccp` has no notion of a fact attached
/// to an edge, and `instcombine` can only relate two comparisons over the
/// same operand pair.
///
/// The negative half is most of the test, and `f(0)` is the boundary that
/// breaks a careless analysis: there `var - 1` is `-1`, `(unsigned)(var-1)`
/// *is* `UINT_MAX`, and the second disjunct is genuinely false.
#[test]
fn codegen_vrp_folds_a_guard_proved_by_a_range() {
    let code = r#"
#include <limits.h>
extern void link_error(void);
extern void abort(void);

volatile int opaque;

__attribute__((noinline)) static void proved(int var)
{
    /* The c-torture shape: always true, so `link_error` must go. */
    if (!(var <= 0 || ((long unsigned)(unsigned)(var - 1) < UINT_MAX)))
        link_error();

    /* A mask bounds a value however unknown it was. */
    if ((var & 0xff) > 255) link_error();
    if ((unsigned)(var & 7) >= 8u) link_error();

    /* Both halves of an exhaustive pair. */
    if (!(var < 5 || var >= 5)) link_error();
}

/* A compound condition makes a nested diamond, and the inner block has two
   predecessors, so the edge fact does not govern it. Checked by value: this
   records a limit of the analysis rather than asserting a fold. */
__attribute__((noinline)) static int bounded(int var)
{
    if (var > 0 && var < 100)
        return (unsigned)(var - 1) <= 98u;
    return 1;
}

/* Each of these is genuinely undecidable, and must survive. */
__attribute__((noinline)) static int boundary(int var)
{
    return (long unsigned)(unsigned)(var - 1) < UINT_MAX;
}
__attribute__((noinline)) static int unknown_divisor(int a, int b) { return a / b; }
__attribute__((noinline)) static int shifted(int x, int n) { return (x >> n) == 0; }

int main(void)
{
    proved(opaque);
    proved(0);
    proved(1);
    proved(-1);
    proved(INT_MAX);
    proved(INT_MIN);

    /* `var == 0` is exactly where the second disjunct is false. */
    if (boundary(0) != 0) abort();
    if (boundary(1) != 1) abort();
    if (boundary(INT_MAX) != 1) abort();
    if (boundary(INT_MIN) != 1) abort();

    if (!bounded(1) || !bounded(99) || !bounded(50)) abort();
    if (!bounded(0) || !bounded(-1) || !bounded(100)) abort();

    if (unknown_divisor(6, 3) != 2) abort();
    if (shifted(0, 3) != 1) abort();
    if (shifted(16, 3) != 0) abort();

    /* Unsigned wrap is not undefined, and must not be range-reasoned away. */
    { unsigned u = 0; if (u - 1u != UINT_MAX) abort(); }
    { unsigned char c = 0; if ((unsigned char)(c - 1) != 255) abort(); }

    /* Narrow types through the promotions. */
    { signed char s = -1; if ((int)s != -1) abort(); }
    { short h = -1; if ((unsigned)(unsigned short)h != 65535u) abort(); }

    return 0;
}
"#;
    for opt in ["-O1", "-O2"] {
        assert_eq!(
            compile_and_run("c17_vrp_range_guard", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// The ranges must not change what a program computes, only what the
/// optimizer can prove. Every arm here is checked by value at every level,
/// including the shapes where a range analysis is most tempted to overreach:
/// a loop counter, a volatile read, an opaque call, a computed goto and an
/// inline-asm output.
#[test]
fn codegen_vrp_agrees_with_the_unoptimized_answer() {
    let code = r#"
#include <limits.h>
extern void abort(void);

volatile int v;

__attribute__((noinline)) static int opaque(int x) { return x; }

int main(void)
{
    /* A loop counter: widening must not conclude a bound it cannot prove. */
    { int n = 0; for (int i = 0; i < 10; i++) n += i; if (n != 45) abort(); }
    { int i = 0; while (v == 0 && i < 3) i++; if (i > 3) abort(); }

    /* A nested loop with a guard inside. */
    {
        int c = 0;
        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j++)
                if (i + j >= 3) c++;
        if (c != 10) abort();
    }

    /* Signed overflow boundaries, computed rather than assumed. */
    if (opaque(INT_MAX) + 0 != INT_MAX) abort();
    if (opaque(INT_MIN) + 0 != INT_MIN) abort();
    /* `INT_MIN / -1` is left out deliberately: it overflows, and on x86-64
       it raises SIGFPE in hardware rather than producing a value. A test
       that ran it would be asserting something C does not define. */
    { int x = opaque(INT_MIN); if (x / 2 != INT_MIN / 2) abort(); }

    /* A volatile read is a different value each time. */
    v = 1;
    { int a = v; v = 2; int b = v; if (a == b) abort(); }

    /* A computed goto: the CFG has an edge the terminator does not name. */
    {
        static void *targets[] = {&&one, &&two};
        int k = (int)(v & 1);
        goto *targets[k];
    one:
        if (v != 2) abort();
        goto done;
    two:
        abort();
    done:;
    }

    /* An inline-asm output must not be folded past. */
    {
        int out = 7;
        __asm__ volatile("" : "+r"(out));
        if (out != 7) abort();
    }

    /* A switch on a value the optimizer cannot know. */
    switch (v) {
    case 2: break;
    default: abort();
    }

    return 0;
}
"#;
    for opt in ["-O0", "-O1", "-O2", "-Os"] {
        assert_eq!(
            compile_and_run("c17_vrp_conservative", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// A loop-carried counter compared against a bound the function cannot see.
///
/// The first time VRP reaches the inner comparison the counter is still
/// `{0}`, so `count + 1` is `{1}` and the true edge appears to prove
/// `maxcount == 1`. That fact is read off the *ranges* of the comparison's
/// operands, which keep widening as the loop is analyzed -- while the
/// comparison's own answer settles at "either" immediately and never moves
/// again. Re-deriving the fact only when the comparison moved therefore
/// froze the narrowest one, and `return maxcount` came back `1`.
///
/// This is CPython's `stringlib` `count_char`, which is why
/// `"AAA".replace("A", "", 3)` answered `"AA"`.
#[test]
fn codegen_vrp_does_not_freeze_an_edge_fact_from_a_loop_counter() {
    let code = r#"
extern void abort(void);

static long count_char(const unsigned char *s, long n, unsigned char p0, long maxcount)
{
    long i, count = 0;
    for (i = 0; i < n; i++) {
        if (s[i] == p0) {
            count++;
            if (count == maxcount) {
                return maxcount;
            }
        }
    }
    return count;
}

static long dispatch(const unsigned char *s, long n, const unsigned char *p, long m, long maxcount)
{
    if (n < m || maxcount == 0) return 0;
    if (m == 1) return count_char(s, n, p[0], maxcount);
    return -2;
}

int main(void) {
    const unsigned char s[] = "AAA";
    const unsigned char p[] = "A";

    /* Every cap from below the count to above it. */
    if (dispatch(s, 3, p, 1, 1) != 1) abort();
    if (dispatch(s, 3, p, 1, 2) != 2) abort();
    if (dispatch(s, 3, p, 1, 3) != 3) abort();
    if (dispatch(s, 3, p, 1, 4) != 3) abort();
    if (dispatch(s, 3, p, 1, 0) != 0) abort();

    /* The same loop with no match at all. */
    const unsigned char t[] = "BBB";
    if (dispatch(t, 3, p, 1, 3) != 0) abort();

    return 0;
}
"#;
    for opt in ["-O1", "-O2"] {
        assert_eq!(
            compile_and_run("c17_vrp_loop_counter_fact", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// Thousands of branches in one function -- the shape of the gcc torture test
/// `compile/20001226-1`, which spent minutes in register allocation on both
/// targets: pairing every constraint point with every live interval, an
/// ordering that rescanned every vertex per pick, and per-interval and
/// per-constant scans of the whole function. This pins that the function
/// still computes the right answer after those became indexed.
#[test]
fn codegen_many_branches_one_function() {
    let mut src = String::from("__attribute__((noinline)) int cmp(int x[64], int y[64]) {\n");
    for i in 0..1500 {
        let a = i % 64;
        src.push_str(&format!(
            "if (x[{a}] > y[{a}]) goto gt; if (x[{a}] < y[{a}]) goto lt;\n"
        ));
    }
    src.push_str(
        "return 0; gt: return 1; lt: return 2; }\n\
         int main(void) {\n\
         int x[64], y[64];\n\
         for (int i = 0; i < 64; i++) x[i] = y[i] = i;\n\
         if (cmp(x, y) != 0) return 10;\n\
         x[20] = 100;\n\
         if (cmp(x, y) != 1) return 11;\n\
         x[20] = 20; y[23] = 100;\n\
         if (cmp(x, y) != 2) return 12;\n\
         return 0; }\n",
    );
    assert_eq!(compile_and_run("many_branches", &src, &[]), 0);
    let opts = vec!["-O2".to_string()];
    assert_eq!(compile_and_run("many_branches_o2", &src, &opts), 0);
    for opt in ["-O0", "-O2"] {
        if let Some(code) = compile_and_run_aarch64("many_branches_a64", &src, opt) {
            assert_eq!(code, 0, "aarch64 at {opt}");
        }
    }
}

/// A large aarch64 frame is allocated and addressed whole.
///
/// The locals of `big_frame` span a megabyte, past every load and store
/// immediate, and `dirty` writes a frame `big_frame` has just left behind.
const AARCH64_BIG_FRAME: &str = r#"
#define NI __attribute__((noinline))
volatile long sink;
NI void touch(void *p) { sink += *(volatile char *)p; }

NI long big_frame(long seed)
{
    volatile char a[1000000];
    long x = seed;
    a[0] = 1; a[sizeof a - 1] = 2;
    touch((void *)a); touch(&x);
    return x + a[0] + a[sizeof a - 1];
}

NI long dirty(void)
{
    volatile char a[100000];
    for (int i = 0; i < 100000; i += 997) a[i] = 0x55;
    touch((void *)a);
    return a[997];
}

int main(void)
{
    if (big_frame(5) != 8) return 1;
    if (dirty() != 0x55) return 2;
    return 0;
}
"#;

#[test]
fn codegen_aarch64_big_frame_runs() {
    for opt in ["-O0", "-O2"] {
        if let Some(code) = compile_and_run_aarch64("a64_big_frame", AARCH64_BIG_FRAME, opt) {
            assert_eq!(code, 0, "at {opt}");
        }
    }
}

/// Members more than 2 GiB into a struct, read and written through a pointer.
///
/// Both backends narrowed a load's or store's IR offset with `as i32`, so
/// `p->y` at offset 3,000,000,000 became a displacement of -1,294,967,296 and
/// the access landed gigabytes away: a segfault on both targets at every
/// level, where gcc builds and runs this. `Linearizer::emit` now folds such an
/// offset into the address. The pointer is formed 3 GB below a real buffer, so
/// only the far members are ever touched and nothing large is allocated.
const FAR_MEMBERS: &str = r#"
/* Members past 2 GiB, reached through a pointer that is never dereferenced
   below its real storage: `base` points 3 GB before `storage`, so only the
   far members are ever touched. */
#define NI __attribute__((noinline))
typedef unsigned long size_t;
struct pair { long a, b; };

struct far {
    char pad[3000000000UL];
    int y;
    long z;
    double w;
    float f;
    short s;
    unsigned char c;
    struct pair pair;
    unsigned bits : 5;
    long double ld;
};

#define OFF(m) ((size_t)&((struct far *)0)->m)

static struct {
    int y; long z; double w; float f; short s; unsigned char c;
    struct pair pair; unsigned bits; long double ld; long pad[8];
} storage_shadow;
static char storage[256] __attribute__((aligned(16)));

NI struct far *base(void) { return (struct far *)(storage - OFF(y)); }

NI int get_y(struct far *p) { return p->y; }
NI long get_z(struct far *p) { return p->z; }
NI double get_w(struct far *p) { return p->w; }
NI float get_f(struct far *p) { return p->f; }
NI short get_s(struct far *p) { return p->s; }
NI unsigned char get_c(struct far *p) { return p->c; }
NI long double get_ld(struct far *p) { return p->ld; }
NI void set_all(struct far *p)
{
    p->y = 11; p->z = 22; p->w = 3.5; p->f = 4.5f; p->s = -6; p->c = 7;
    p->pair.a = 8; p->pair.b = 9; p->bits = 13; p->ld = 10.25L;
}
NI int *addr_y(struct far *p) { return &p->y; }
NI long pair_sum(struct far *p)
{
    struct pair q = p->pair;
    return q.a + q.b;
}
NI unsigned get_bits(struct far *p) { return p->bits; }

int main(void)
{
    struct far *p = base();
    (void)storage_shadow;
    set_all(p);
    if (get_y(p) != 11) return 1;
    if (get_z(p) != 22) return 2;
    if (get_w(p) != 3.5) return 3;
    if (get_f(p) != 4.5f) return 4;
    if (get_s(p) != -6) return 5;
    if (get_c(p) != 7) return 6;
    if (pair_sum(p) != 17) return 7;
    if (get_bits(p) != 13) return 8;
    if (get_ld(p) != 10.25L) return 9;
    if ((char *)addr_y(p) != storage) return 10;
    if (*(int *)storage != 11) return 11;
    return 0;
}
"#;

#[test]
fn codegen_member_past_two_gigabytes_through_a_pointer() {
    for opt in ["-O0", "-O2"] {
        let opts = vec![opt.to_string()];
        assert_eq!(
            compile_and_run("far_members", FAR_MEMBERS, &opts),
            0,
            "x86-64 {opt}"
        );
        if let Some(code) = compile_and_run_aarch64("far_members_a64", FAR_MEMBERS, opt) {
            assert_eq!(code, 0, "aarch64 {opt}");
        }
    }
}

/// A narrow constant is extended when it is compiled, not when it runs, and a
/// narrow value is extended by its type's signedness either way.
///
/// `char buf[16] = {0}` spent a `shll $24; sarl $24` pair on the zero byte it
/// stored, even at `-O2`: the copy of the constant to a `char` survived, and
/// the back end sign-extended whatever it held at run time. Copy propagation
/// now stores the constant directly, and a narrow copy of a constant that
/// does survive is materialized already extended. The program half pins the
/// values a narrowing conversion produces, on both targets.
///
/// The assembly half is `codegen_a_narrow_constant_is_extended_at_compile_time`
/// in `cc/test_asm/codegen_optimizer.rs`.
#[test]
fn codegen_a_narrow_constant_is_extended_at_compile_time() {
    let run = r#"
__attribute__((noinline)) int sc(int x) { signed char c = x; return c; }
__attribute__((noinline)) int uc(int x) { unsigned char c = x; return c; }
__attribute__((noinline)) int ss(int x) { short c = x; return c; }
__attribute__((noinline)) int us(int x) { unsigned short c = x; return c; }
__attribute__((noinline)) int k200(void) { signed char c = 200; return c; }
__attribute__((noinline)) int k200u(void) { unsigned char c = 200; return c; }
int main(void) {
    if (sc(300) != 44 || uc(300) != 44 || ss(70000) != 4464 || us(70000) != 4464)
        return 1;
    if (k200() != -56 || k200u() != 200) return 2;
    return 0;
}
"#;
    for level in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(&format!("narrow_values{level}"), run, &[level.to_string()]),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64("narrow_values_a64", run, level) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}

/// Every conversion of a constant folds to the value the conversion produces
/// at run time: the operand is read at the width the conversion records for
/// it, never at its result's width. An extension read at its destination
/// width is the identity, so a negative `char` would come back positive; a
/// `double` read at the 32 bits of the `float` or `int` it becomes is not
/// the `double` at all.
#[test]
fn codegen_a_conversion_of_a_constant_reads_its_source_width() {
    let src = r#"
#include <math.h>
int main(void) {
    signed char sc = -56;
    unsigned char uc = 200;
    long long wide = 0x123456789LL;
    if ((int)sc != -56 || (unsigned)sc != 4294967240u) return 1;
    if ((int)uc != 200 || (long long)(short)-2 != -2) return 2;
    if ((int)wide != 0x23456789 || (short)wide != 0x6789) return 3;
    double d = 0.1;
    float f = (float)d;
    if (f != 0.1f || (double)f == d) return 4;
    if ((int)-3.75 != -3 || (unsigned)3e9 != 3000000000u) return 5;
    if ((long long)-1e15 != -1000000000000000LL) return 6;
    if ((double)sc != -56.0 || (float)uc != 200.0f) return 7;
    if ((double)18446744073709551615ull != 18446744073709551616.0) return 8;
    long double ld = -0.0L;
    if (!signbit(ld) || !signbit(-2.5) || signbit(2.5f) || (long double)d != 0.1) return 9;
    return 0;
}
"#;
    for level in ["-O0", "-O1", "-O2"] {
        let opts = vec![level.to_string(), "-lm".to_string()];
        assert_eq!(
            compile_and_run(&format!("convert_const{level}"), src, &opts),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64("convert_const_a64", src, level) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}
