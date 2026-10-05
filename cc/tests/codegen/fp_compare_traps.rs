//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Floating comparisons raise exactly the exceptions IEEE 754 assigns them.
//
// c17 defines `__STDC_IEC_559__`, so Annex F binds it: the relational
// operators `<`, `<=`, `>` and `>=` are IEEE 754's signalling predicates and
// raise "invalid" when an operand is a NaN (C17 F.9.3), while `==`, `!=` and
// the <math.h> comparison macros (`isgreater` .. `isunordered`, 7.12.14) are
// the quiet ones and raise nothing.
//
// gcc at -O2 on x86-64 rewrites `if (!(a < b))` into a quiet unordered
// compare and loses the exception (exit 16); this file is written against the
// standard, not against that.
//

use crate::common::{compile_and_run, compile_and_run_aarch64_with};

const FP_COMPARE_TRAPS: &str = r#"
#include <fenv.h>
#include <math.h>

static volatile int sink;

static int raised(void) { return fetestexcept(FE_INVALID) != 0; }

/* Each comparison in its own function, so the operands are unknown to the
   caller's optimizer: once as a value and once as a branch condition. */
#define DEF(T, S)                                                              \
    __attribute__((noinline)) static int lt_##S(T a, T b) { return a < b; }    \
    __attribute__((noinline)) static int le_##S(T a, T b) { return a <= b; }   \
    __attribute__((noinline)) static int gt_##S(T a, T b) { return a > b; }    \
    __attribute__((noinline)) static int ge_##S(T a, T b) { return a >= b; }   \
    __attribute__((noinline)) static int eq_##S(T a, T b) { return a == b; }   \
    __attribute__((noinline)) static int ne_##S(T a, T b) { return a != b; }   \
    __attribute__((noinline)) static int blt_##S(T a, T b) { if (a < b) return 1; return 2; }  \
    __attribute__((noinline)) static int ble_##S(T a, T b) { if (a <= b) return 1; return 2; } \
    __attribute__((noinline)) static int bgt_##S(T a, T b) { if (a > b) return 1; return 2; }  \
    __attribute__((noinline)) static int bge_##S(T a, T b) { if (a >= b) return 1; return 2; } \
    __attribute__((noinline)) static int bnlt_##S(T a, T b) { if (!(a < b)) return 1; return 2; } \
    __attribute__((noinline)) static int beq_##S(T a, T b) { if (a == b) return 1; return 2; } \
    __attribute__((noinline)) static int bne_##S(T a, T b) { if (a != b) return 1; return 2; } \
    __attribute__((noinline)) static int qgt_##S(T a, T b) { return isgreater(a, b); }         \
    __attribute__((noinline)) static int qge_##S(T a, T b) { return isgreaterequal(a, b); }    \
    __attribute__((noinline)) static int qlt_##S(T a, T b) { return isless(a, b); }            \
    __attribute__((noinline)) static int qle_##S(T a, T b) { return islessequal(a, b); }       \
    __attribute__((noinline)) static int qlg_##S(T a, T b) { return islessgreater(a, b); }     \
    __attribute__((noinline)) static int qun_##S(T a, T b) { return isunordered(a, b); }

DEF(float, f)
DEF(double, d)
DEF(long double, ld)

/* `want` is whether FE_INVALID must be raised; `code` is the exit status
   naming the failing check. */
#define CHECK(call, want, code)                                                \
    do {                                                                       \
        feclearexcept(FE_ALL_EXCEPT);                                          \
        sink = (call);                                                         \
        if (raised() != (want))                                                \
            return (code);                                                     \
    } while (0)

/* C17 F.9.3 / IEEE 754: the relational operators raise invalid when an
   operand is a NaN; ==, != and the <math.h> comparison macros are quiet.
   Ordered operands raise nothing at all. */
#define RUN(S, T, base)                                                        \
    do {                                                                       \
        volatile T nan = (T)__builtin_nan("");                                 \
        volatile T one = 1;                                                    \
        CHECK(lt_##S(nan, one), 1, base + 0);                                  \
        CHECK(le_##S(nan, one), 1, base + 1);                                  \
        CHECK(gt_##S(nan, one), 1, base + 2);                                  \
        CHECK(ge_##S(nan, one), 1, base + 3);                                  \
        CHECK(lt_##S(one, nan), 1, base + 4);                                  \
        CHECK(ge_##S(one, nan), 1, base + 5);                                  \
        CHECK(blt_##S(nan, one), 1, base + 6);                                 \
        CHECK(ble_##S(nan, one), 1, base + 7);                                 \
        CHECK(bgt_##S(nan, one), 1, base + 8);                                 \
        CHECK(bge_##S(nan, one), 1, base + 9);                                 \
        CHECK(bnlt_##S(nan, one), 1, base + 10);                               \
        CHECK(eq_##S(nan, one), 0, base + 11);                                 \
        CHECK(ne_##S(nan, one), 0, base + 12);                                 \
        CHECK(beq_##S(nan, one), 0, base + 13);                                \
        CHECK(bne_##S(nan, one), 0, base + 14);                                \
        CHECK(qgt_##S(nan, one), 0, base + 15);                                \
        CHECK(qge_##S(nan, one), 0, base + 16);                                \
        CHECK(qlt_##S(nan, one), 0, base + 17);                                \
        CHECK(qle_##S(nan, one), 0, base + 18);                                \
        CHECK(qlg_##S(nan, one), 0, base + 19);                                \
        CHECK(qun_##S(nan, one), 0, base + 20);                                \
        CHECK(lt_##S(one, one), 0, base + 21);                                 \
        CHECK(bge_##S(one, one), 0, base + 22);                                \
        /* The answers themselves: every relation with a NaN is false. */      \
        if (lt_##S(nan, one) || le_##S(nan, one) || gt_##S(nan, one) ||        \
            ge_##S(nan, one) || eq_##S(nan, one) || !ne_##S(nan, one))         \
            return base + 23;                                                  \
        if (blt_##S(nan, one) != 2 || bnlt_##S(nan, one) != 1 ||               \
            qun_##S(nan, one) != 1 || qlg_##S(nan, one) != 0)                  \
            return base + 24;                                                  \
        if (!lt_##S(one, (T)2) || !qlt_##S(one, (T)2) || ge_##S(one, (T)2))    \
            return base + 25;                                                  \
    } while (0)

int main(void)
{
    RUN(f, float, 10);
    RUN(d, double, 40);
    RUN(ld, long double, 70);
    return 0;
}
"#;

#[test]
fn codegen_relational_compares_signal_and_equality_compares_are_quiet() {
    for level in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run(
                "fp_compare_traps",
                FP_COMPARE_TRAPS,
                &[level.to_string(), "-lm".to_string()]
            ),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64_with(
            "fp_compare_traps_a64",
            FP_COMPARE_TRAPS,
            &[level],
            &["-lm"],
        ) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}

/// A relational guarded by a test that its operands are ordered is evaluated
/// only once they are, so it raises nothing -- unless the optimizer runs it
/// unconditionally. `ifconv` made `isnan(x) || x < y` branch-free and raised
/// invalid for every NaN `x`; the linearizer did the same to `c ? x < y : 0`.
/// Each guard below is one a program writes precisely to keep the flag
/// clear; the last pair is unguarded and must raise.
const GUARDED_COMPARES: &str = r#"
#include <fenv.h>
#include <math.h>

static volatile int sink;

#define DEF(T, S)                                                              \
    __attribute__((noinline)) static int a_##S(T x, T y)                       \
    { return __builtin_isnan(x) || x < y; }                                    \
    __attribute__((noinline)) static int b_##S(T x, T y)                       \
    { return isunordered(x, y) || x < y; }                                     \
    __attribute__((noinline)) static int c_##S(T x, T y)                       \
    { return !isunordered(x, y) && x <= y; }                                   \
    __attribute__((noinline)) static int d_##S(T x, T y)                       \
    { return !__builtin_isnan(x) ? x > y : 0; }                                \
    __attribute__((noinline)) static int e_##S(int k, T x, T y)                \
    { return k && x >= y; }                                                    \
    __attribute__((noinline)) static int f_##S(T x, T y)                       \
    { if (isunordered(x, y)) return 0; return x < y; }                         \
    __attribute__((noinline)) static int g_##S(T x, T y)                       \
    { return (x < y) && (x > y); }                                             \
    __attribute__((noinline)) static int h_##S(T x)                            \
    { return isfinite(x) + isnormal(x) + fpclassify(x); }

DEF(float, f)
DEF(double, d)
DEF(long double, l)

#define CHECK(call, want, code)                                                \
    do {                                                                       \
        feclearexcept(FE_ALL_EXCEPT);                                          \
        sink = (call);                                                         \
        if ((fetestexcept(FE_INVALID) != 0) != (want))                         \
            return (code);                                                     \
    } while (0)

#define RUN(S, T, base)                                                        \
    do {                                                                       \
        volatile T n = (T)__builtin_nan(""), one = 1;                          \
        CHECK(a_##S(one, n), 1, base + 0);                                     \
        CHECK(b_##S(n, one), 0, base + 1);                                     \
        CHECK(c_##S(n, one), 0, base + 2);                                     \
        CHECK(d_##S(n, one), 0, base + 3);                                     \
        CHECK(e_##S(0, n, one), 0, base + 4);                                  \
        CHECK(f_##S(n, one), 0, base + 5);                                     \
        CHECK(h_##S(n), 0, base + 6);                                          \
        CHECK(e_##S(1, n, one), 1, base + 7);                                  \
        CHECK(a_##S(n, one), 0, base + 8);                                     \
        if (!a_##S(n, one) || !b_##S(n, one) || c_##S(n, one) ||               \
            d_##S(n, one) || f_##S(n, one) || g_##S(n, one))                   \
            return base + 9;                                                   \
        if (a_##S(one, one) || !c_##S(one, one) || g_##S(one, (T)2) ||         \
            !d_##S((T)2, one) || !e_##S(1, one, one))                          \
            return base + 10;                                                  \
    } while (0)

int main(void)
{
    RUN(f, float, 10);
    RUN(d, double, 40);
    RUN(l, long double, 70);
    return 0;
}
"#;

#[test]
fn codegen_a_guarded_relational_compare_is_not_run_unguarded() {
    for level in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run(
                "fp_guarded_compares",
                GUARDED_COMPARES,
                &[level.to_string(), "-lm".to_string()]
            ),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64_with(
            "fp_guarded_compares_a64",
            GUARDED_COMPARES,
            &[level],
            &["-lm"],
        ) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}
