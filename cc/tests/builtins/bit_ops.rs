//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Bit Operations Builtins Mega-Test
//
// Consolidates: clz, ctz, popcount, bswap tests
//

use crate::common::{
    compile_and_run, compile_and_run_aarch64, compile_and_run_everywhere, compile_and_run_optimized,
};

// ============================================================================
// Mega-test: Bit operation builtins
// ============================================================================

/// The bit builtins, with every other program of this file built at the matrix
/// levels with no options, as one program; see the exit-code table at the top.
///
/// Consolidates: builtins_bit_ops_mega, the matrix-level run of
/// builtins_clrsb_counts_redundant_sign_bits, and builtins_ffsll_matches_its_siblings.
#[test]
fn builtins_bit_ops_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1- 98  builtins_bit_ops_mega
 *   101-119  builtins_clrsb_counts_redundant_sign_bits
 *   121-128  builtins_ffsll_matches_its_siblings
 */

/* ---- builtins_bit_ops_mega (exit codes 1-98) ----
 */
static int t_builtins_bit_ops_mega(void) {
    // ========== CLZ (count leading zeros) (returns 1-29) ==========
    {
        unsigned int val;
        int result;

        // 32-bit clz
        val = 0x80000000;  // High bit set
        result = __builtin_clz(val);
        if (result != 0) return 1;

        val = 0x40000000;  // Second highest bit
        result = __builtin_clz(val);
        if (result != 1) return 2;

        val = 1;  // Lowest bit
        result = __builtin_clz(val);
        if (result != 31) return 3;

        val = 256;  // 2^8
        result = __builtin_clz(val);
        if (result != 23) return 4;

        result = __builtin_clz(16);  // constant
        if (result != 27) return 5;

        val = 0xFFFFFFFF;
        result = __builtin_clz(val);
        if (result != 0) return 6;

        // 64-bit clzl
        unsigned long lval = 1UL << 63;
        result = __builtin_clzl(lval);
        if (result != 0) return 7;

        lval = 1UL << 62;
        result = __builtin_clzl(lval);
        if (result != 1) return 8;

        lval = 1UL;
        result = __builtin_clzl(lval);
        if (result != 63) return 9;

        // 64-bit clzll
        unsigned long long llval = 1ULL << 63;
        result = __builtin_clzll(llval);
        if (result != 0) return 10;

        llval = 1ULL;
        result = __builtin_clzll(llval);
        if (result != 63) return 11;
    }

    // ========== CTZ (count trailing zeros) (returns 30-59) ==========
    {
        unsigned int val;
        int result;

        // 32-bit ctz
        val = 1;
        result = __builtin_ctz(val);
        if (result != 0) return 30;

        val = 2;
        result = __builtin_ctz(val);
        if (result != 1) return 31;

        val = 0x80000000;
        result = __builtin_ctz(val);
        if (result != 31) return 32;

        val = 256;  // 2^8
        result = __builtin_ctz(val);
        if (result != 8) return 33;

        val = 0xFFFFFFFF;
        result = __builtin_ctz(val);
        if (result != 0) return 34;

        result = __builtin_ctz(16);  // constant
        if (result != 4) return 35;

        // 64-bit ctzl
        unsigned long lval = 1UL;
        result = __builtin_ctzl(lval);
        if (result != 0) return 36;

        lval = 1UL << 63;
        result = __builtin_ctzl(lval);
        if (result != 63) return 37;

        lval = 1UL << 40;
        result = __builtin_ctzl(lval);
        if (result != 40) return 38;

        // 64-bit ctzll
        unsigned long long llval = 1ULL;
        result = __builtin_ctzll(llval);
        if (result != 0) return 39;

        llval = 1ULL << 63;
        result = __builtin_ctzll(llval);
        if (result != 63) return 40;
    }

    // ========== POPCOUNT (population count) (returns 60-89) ==========
    {
        unsigned int val;
        int result;

        // 32-bit popcount
        val = 0;
        result = __builtin_popcount(val);
        if (result != 0) return 60;

        val = 1;
        result = __builtin_popcount(val);
        if (result != 1) return 61;

        val = 3;  // 0b11
        result = __builtin_popcount(val);
        if (result != 2) return 62;

        val = 0xFFFFFFFF;
        result = __builtin_popcount(val);
        if (result != 32) return 63;

        val = 0xAAAAAAAA;  // alternating bits
        result = __builtin_popcount(val);
        if (result != 16) return 64;

        val = 0x80000000;  // power of 2
        result = __builtin_popcount(val);
        if (result != 1) return 65;

        // 64-bit popcountl
        unsigned long lval = 0UL;
        result = __builtin_popcountl(lval);
        if (result != 0) return 66;

        lval = 0xFFFFFFFFFFFFFFFFUL;
        result = __builtin_popcountl(lval);
        if (result != 64) return 67;

        lval = 0xAAAAAAAAAAAAAAAAUL;
        result = __builtin_popcountl(lval);
        if (result != 32) return 68;

        // 64-bit popcountll
        unsigned long long llval = 0ULL;
        result = __builtin_popcountll(llval);
        if (result != 0) return 69;

        llval = 0xFFFFFFFFFFFFFFFFULL;
        result = __builtin_popcountll(llval);
        if (result != 64) return 70;

        llval = 0x5555555555555555ULL;
        result = __builtin_popcountll(llval);
        if (result != 32) return 71;
    }

    // ========== BSWAP (byte swap) (returns 90-109) ==========
    {
        // 32-bit bswap
        unsigned int val = 0x12345678;
        unsigned int swapped = __builtin_bswap32(val);
        if (swapped != 0x78563412) return 90;

        val = 0x01020304;
        swapped = __builtin_bswap32(val);
        if (swapped != 0x04030201) return 91;

        val = 0;
        swapped = __builtin_bswap32(val);
        if (swapped != 0) return 92;

        val = 0xFFFFFFFF;
        swapped = __builtin_bswap32(val);
        if (swapped != 0xFFFFFFFF) return 93;

        // 64-bit bswap
        unsigned long long llval = 0x0102030405060708ULL;
        unsigned long long llswapped = __builtin_bswap64(llval);
        if (llswapped != 0x0807060504030201ULL) return 94;

        llval = 0x123456789ABCDEF0ULL;
        llswapped = __builtin_bswap64(llval);
        if (llswapped != 0xF0DEBC9A78563412ULL) return 95;

        llval = 0;
        llswapped = __builtin_bswap64(llval);
        if (llswapped != 0) return 96;

        // 16-bit bswap
        unsigned short sval = 0x1234;
        unsigned short sswapped = __builtin_bswap16(sval);
        if (sswapped != 0x3412) return 97;

        sval = 0x0102;
        sswapped = __builtin_bswap16(sval);
        if (sswapped != 0x0201) return 98;
    }

    return 0;
}


/* ---- builtins_clrsb_counts_redundant_sign_bits (exit codes 101-119) ----
 *
 *  `__builtin_clrsb` and friends: the count of redundant sign bits.
 *
 *  Absent entirely until now, which is what stopped sparse's `builtin.c`
 *  compiling. Unlike the `clz` family these are defined for *every* input --
 *  0 and -1 both answer one less than the width -- so the lowering cannot use
 *  `clz` naively: `clz(0)` is undefined, and c17 and gcc happen to answer
 *  differently there, so relying on it would be relying on two undefined
 *  behaviours agreeing. The fold shifts up and sets a low bit instead, which
 *  both removes the zero case and absorbs the `- 1`.
 *
 *  Every bit position is checked, both signs, at each width.
 */
static int side_effect_count = 0;
static int bump(void) { side_effect_count++; return 255; }

static int t_builtins_clrsb_counts_redundant_sign_bits(void) {
    /* Documented anchors, taken from gcc. */
    if (__builtin_clrsb(0) != 31) return 1;
    if (__builtin_clrsb(-1) != 31) return 2;
    if (__builtin_clrsb(1) != 30) return 3;
    if (__builtin_clrsb(-2) != 30) return 4;
    if (__builtin_clrsb(2) != 29) return 5;
    if (__builtin_clrsb(255) != 23) return 6;
    if (__builtin_clrsb(-256) != 23) return 7;
    if (__builtin_clrsb(0x7fffffff) != 0) return 8;
    if (__builtin_clrsb((int)0x80000000) != 0) return 9;

    if (__builtin_clrsbll(0LL) != 63) return 10;
    if (__builtin_clrsbll(-1LL) != 63) return 11;
    if (__builtin_clrsbll(1LL) != 62) return 12;
    if (__builtin_clrsbl(0L) != 63) return 13;

    /* Every bit position, both signs, at each width. */
    for (int i = 0; i < 31; i++) {
        int v = 1 << i;
        if (__builtin_clrsb(v) != 30 - i) return 14;
        if (__builtin_clrsb(-v) != 31 - i) return 15;   /* clrsb(-1) is 31 */
    }
    for (int i = 0; i < 63; i++) {
        long long v = 1LL << i;
        if (__builtin_clrsbll(v) != 62 - i) return 16;
        if (__builtin_clrsbll(-v) != 63 - i) return 17;
    }

    /* The operand is evaluated exactly once. The fold reads its value twice,
       so a lowering that re-evaluated the *expression* would call bump twice. */
    if (__builtin_clrsb(bump()) != 23) return 18;
    if (side_effect_count != 1) return 19;
    return 0;
}


/* ---- builtins_ffsll_matches_its_siblings (exit codes 121-128) ----
 *
 *  `__builtin_ffsll` completes a family that had its first two members only.
 */
static int t_builtins_ffsll_matches_its_siblings(void) {
    if (__builtin_ffs(0) != 0) return 1;
    if (__builtin_ffs(8) != 4) return 2;
    if (__builtin_ffsl(8L) != 4) return 3;
    if (__builtin_ffsll(8LL) != 4) return 4;
    if (__builtin_ffsll(0LL) != 0) return 5;
    if (__builtin_ffsll(1LL) != 1) return 6;
    /* A bit only a 64-bit form can reach. */
    if (__builtin_ffsll(1LL << 40) != 41) return 7;
    if (__builtin_ffsll((long long)0x8000000000000000LL) != 64) return 8;
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_builtins_bit_ops_mega()) != 0)
        return 0 + r;
    if ((r = t_builtins_clrsb_counts_redundant_sign_bits()) != 0)
        return 100 + r;
    if ((r = t_builtins_ffsll_matches_its_siblings()) != 0)
        return 120 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_bit_ops_mega", code, &[]), 0);
}

/// `__builtin_clrsb` and friends: the count of redundant sign bits.
///
/// Absent entirely until now, which is what stopped sparse's `builtin.c`
/// compiling. Unlike the `clz` family these are defined for *every* input --
/// 0 and -1 both answer one less than the width -- so the lowering cannot use
/// `clz` naively: `clz(0)` is undefined, and c17 and gcc happen to answer
/// differently there, so relying on it would be relying on two undefined
/// behaviours agreeing. The fold shifts up and sets a low bit instead, which
/// both removes the zero case and absorbs the `- 1`.
///
/// Every bit position is checked, both signs, at each width.
#[test]
fn builtins_clrsb_counts_redundant_sign_bits() {
    let code = r#"
static int side_effect_count = 0;
static int bump(void) { side_effect_count++; return 255; }

int main(void) {
    /* Documented anchors, taken from gcc. */
    if (__builtin_clrsb(0) != 31) return 1;
    if (__builtin_clrsb(-1) != 31) return 2;
    if (__builtin_clrsb(1) != 30) return 3;
    if (__builtin_clrsb(-2) != 30) return 4;
    if (__builtin_clrsb(2) != 29) return 5;
    if (__builtin_clrsb(255) != 23) return 6;
    if (__builtin_clrsb(-256) != 23) return 7;
    if (__builtin_clrsb(0x7fffffff) != 0) return 8;
    if (__builtin_clrsb((int)0x80000000) != 0) return 9;

    if (__builtin_clrsbll(0LL) != 63) return 10;
    if (__builtin_clrsbll(-1LL) != 63) return 11;
    if (__builtin_clrsbll(1LL) != 62) return 12;
    if (__builtin_clrsbl(0L) != 63) return 13;

    /* Every bit position, both signs, at each width. */
    for (int i = 0; i < 31; i++) {
        int v = 1 << i;
        if (__builtin_clrsb(v) != 30 - i) return 14;
        if (__builtin_clrsb(-v) != 31 - i) return 15;   /* clrsb(-1) is 31 */
    }
    for (int i = 0; i < 63; i++) {
        long long v = 1LL << i;
        if (__builtin_clrsbll(v) != 62 - i) return 16;
        if (__builtin_clrsbll(-v) != 63 - i) return 17;
    }

    /* The operand is evaluated exactly once. The fold reads its value twice,
       so a lowering that re-evaluated the *expression* would call bump twice. */
    if (__builtin_clrsb(bump()) != 23) return 18;
    if (side_effect_count != 1) return 19;
    return 0;
}
"#;
    // The matrix-level run is a section of builtins_bit_ops_mega.
    assert_eq!(compile_and_run_optimized("builtins_clrsb_o2", code), 0);
}

// ============================================================================
// popcount and parity on the x86-64 baseline
// ============================================================================

// Self-contained for the header-less aarch64 run.
const POPCOUNT_PROGRAM: &str = r#"
int main(void) {
    volatile unsigned z = 0, ones = ~0u, alt = 0xaaaaaaaau, one = 1u, top = 0x80000000u;
    volatile unsigned long long lz = 0, lones = ~0ull, lalt = 0x5555555555555555ull,
        ltop = 1ull << 63, mixed = 0x0123456789abcdefull;
    if (__builtin_popcount(z) != 0 || __builtin_popcount(ones) != 32) return 1;
    if (__builtin_popcount(alt) != 16 || __builtin_popcount(one) != 1) return 2;
    if (__builtin_popcount(top) != 1) return 3;
    if (__builtin_popcountll(lz) != 0 || __builtin_popcountll(lones) != 64) return 4;
    if (__builtin_popcountll(lalt) != 32 || __builtin_popcountll(ltop) != 1) return 5;
    if (__builtin_popcountll(mixed) != 32 || __builtin_popcountl(mixed) != 32) return 6;
    if (__builtin_parity(alt) != 0 || __builtin_parity(one) != 1) return 7;
    if (__builtin_parityll(mixed) != 0 || __builtin_parityll(ltop) != 1) return 8;
    if (__builtin_parityl(lones) != 0) return 9;
    return 0;
}
"#;

/// `popcnt` is not in the x86-64 baseline -- it arrived with SSE4.2-era
/// processors -- and c17 targets the baseline, as gcc does without
/// `-mpopcnt`. It was emitted unconditionally, so a popcount or parity
/// raised SIGILL on a processor without it. gcc calls libgcc's
/// `__popcountdi2`; c17 counts inline instead. (The assembly half is a unit
/// test in cc/test_asm/builtins_bit_ops.rs.)
#[test]
fn builtins_popcount_uses_baseline_instructions() {
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(
                &format!("popcount{opt}"),
                POPCOUNT_PROGRAM,
                &[opt.to_string()]
            ),
            0,
            "host {opt}"
        );
        if let Some(rc) =
            compile_and_run_aarch64(&format!("popcount_a64{opt}"), POPCOUNT_PROGRAM, opt)
        {
            assert_eq!(rc, 0, "aarch64 {opt}");
        }
    }
}

/// The bit builtins as calls through a prototype and as constant expressions, on
/// every level and target; each section keeps its original test name and doc
/// comment.
///
/// Consolidates: builtins_bit_ops_convert_their_argument,
/// builtins_bit_ops_of_constants_are_constant_expressions and
/// builtins_ctz_clz_of_constant_zero_fold_to_the_width.
#[test]
fn builtins_bit_ops_everywhere_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1-  9  builtins_bit_ops_convert_their_argument
 *    11- 61  builtins_bit_ops_of_constants_are_constant_expressions
 *    71- 73  builtins_ctz_clz_of_constant_zero_fold_to_the_width
 */

/* ---- builtins_bit_ops_convert_their_argument (exit codes 1-9) ----
 *
 *  The bit builtins have prototypes -- `int __builtin_ctz(unsigned int)` and
 *  so on -- so an argument converts to the parameter type as in any call
 *  through a prototype (C17 6.5.2.2p7). c17 took the argument's bits as they
 *  were: `__builtin_ctz(8.0)` counted the zeros of the double's
 *  representation, 0 on x86-64 and 2 on aarch64, where gcc answers 3.
 */
/* Bit builtins convert their argument to the parameter type, as a call
   through a prototype would. */
static int t_builtins_bit_ops_convert_their_argument(void) {
    volatile double d = 8.0, e = 7.9;
    volatile float f = 12.0f;
    volatile long double ld = 2147483648.0L;
    if (__builtin_ctz(d) != 3) return 1;
    if (__builtin_popcount(e) != 3) return 2;
    if (__builtin_clz(f) != 28) return 3;
    if (__builtin_ctzll(ld) != 31) return 4;
    if (__builtin_popcountl(e) != 3) return 5;
    if (__builtin_parity(e) != 1) return 6;
    if (__builtin_bswap32(d) != 0x08000000u) return 7;
    if (__builtin_ffs(d) != 4) return 8;
    if (__builtin_clrsb(f) != 27) return 9;
    return 0;
}


/* ---- builtins_bit_ops_of_constants_are_constant_expressions (exit codes 11-61) ----
 *
 *  A bit builtin of a constant is an integer constant expression in gcc, so
 *  it may initialize a static object and label a `case`. c17 folded only
 *  `popcount` and `parity`, and none of them in a static initializer.
 */
/* Every bit builtin of a constant is an integer constant expression in gcc,
   so it works in a static initializer and a case label. */
static const int bc_t[] = {
    __builtin_ctz(8), __builtin_clz(1), __builtin_ctzll(1ULL << 40),
    __builtin_clzl(1), __builtin_popcount(7), __builtin_parity(7),
    __builtin_clrsb(0), __builtin_ffs(8), __builtin_bswap16(0x1234),
};
static const unsigned bc_b32 = __builtin_bswap32(0x12345678u);
static const unsigned long long bc_b64 = __builtin_bswap64(0x0102030405060708ULL);
static int t_builtins_bit_ops_of_constants_are_constant_expressions(void) {
    switch (8) { case __builtin_ctz(256): break; case __builtin_popcount(127): return 50; default: return 51; }
    if (bc_t[0] != 3 || bc_t[1] != 31 || bc_t[2] != 40 || bc_t[3] != 63 || bc_t[4] != 3 || bc_t[5] != 1
        || bc_t[6] != 31 || bc_t[7] != 4 || bc_t[8] != 0x3412) return 1;
    if (bc_b32 != 0x78563412u || bc_b64 != 0x0807060504030201ULL) return 2;
    return 0;
}


/* ---- builtins_ctz_clz_of_constant_zero_fold_to_the_width (exit codes 71-73) ----
 *
 *  `ctz` and `clz` of 0 are undefined at run time, but of a constant 0 gcc
 *  still folds them, to the operand width, on both targets: a static
 *  initializer, an enumerator, an array bound and `_Static_assert` accept
 *  them. The other bit builtins are constants in those places too.
 */
static int bz_z[] = { __builtin_ctz(0), __builtin_clz(0), __builtin_ctzll(0), __builtin_clzl(0) };
enum { BZ_E = __builtin_ctz(0), BZ_F = __builtin_ffs(0), BZ_G = __builtin_clrsb(-1) };
static char bz_bound[__builtin_bswap16(0x0100) + __builtin_ctz(-1)];
_Static_assert(__builtin_clzll(0) == 64 && __builtin_popcountll(-1) == 64, "folded");
_Static_assert(__builtin_bswap32(0x12345678u) == 0x78563412u, "folded");
static int t_builtins_ctz_clz_of_constant_zero_fold_to_the_width(void) {
    if (bz_z[0] != 32 || bz_z[1] != 32 || bz_z[2] != 64 || bz_z[3] != 64) return 1;
    if (BZ_E != 32 || BZ_F != 0 || BZ_G != 31) return 2;
    if (sizeof bz_bound != 1) return 3;
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_builtins_bit_ops_convert_their_argument()) != 0)
        return 0 + r;
    if ((r = t_builtins_bit_ops_of_constants_are_constant_expressions()) != 0)
        return 10 + r;
    if ((r = t_builtins_ctz_clz_of_constant_zero_fold_to_the_width()) != 0)
        return 70 + r;
    return 0;
}
"#;
    compile_and_run_everywhere("builtins_bit_ops_everywhere_mega", code);
}

/// A bit builtin whose operand is an argument the caller passed on the
/// stack, read in place: the x86-64 lowering of `clz`, `ctz` and `bswap`
/// knew a register, a spill slot and a constant, and emitted nothing at all
/// for an incoming stack argument, so the result was whatever the scratch
/// register held. perl's regex compiler (`single_1bit_pos32` inlined into
/// `S_optimize_regclass`, whose ninth parameter is the class mask) never
/// built a `POSIXL` node.
#[test]
fn builtins_bit_ops_of_a_stacked_argument() {
    let code = r#"
#define N __attribute__((noinline))
#define P int a, int b, int c, int d, int e, int f, int g, int h
N int clz32(P, unsigned x) { return __builtin_clz(x); }
N int clz64(P, unsigned long long x) { return __builtin_clzll(x); }
N int ctz32(P, unsigned x) { return __builtin_ctz(x); }
N int ctz64(P, unsigned long long x) { return __builtin_ctzll(x); }
N unsigned short bs16(P, unsigned short x) { return __builtin_bswap16(x); }
N unsigned bs32(P, unsigned x) { return __builtin_bswap32(x); }
N unsigned long long bs64(P, unsigned long long x) { return __builtin_bswap64(x); }
N int pop32(P, unsigned x) { return __builtin_popcount(x); }

int main(void)
{
    if (clz32(0, 0, 0, 0, 0, 0, 0, 0, 0x100) != 23) return 1;
    if (clz64(0, 0, 0, 0, 0, 0, 0, 0, 0x100000000ULL) != 31) return 2;
    if (ctz32(0, 0, 0, 0, 0, 0, 0, 0, 0x100) != 8) return 3;
    if (ctz64(0, 0, 0, 0, 0, 0, 0, 0, 0x100000000ULL) != 32) return 4;
    if (bs16(0, 0, 0, 0, 0, 0, 0, 0, 0x1234) != 0x3412) return 5;
    if (bs32(0, 0, 0, 0, 0, 0, 0, 0, 0x12345678) != 0x78563412) return 6;
    if (bs64(0, 0, 0, 0, 0, 0, 0, 0, 0x1122334455667788ULL) != 0x8877665544332211ULL) return 7;
    if (pop32(0, 0, 0, 0, 0, 0, 0, 0, 0xf0f) != 8) return 8;
    return 0;
}
"#;
    compile_and_run_everywhere("builtins_bit_ops_of_a_stacked_argument", code);
}
