//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Floating point: float, double, long double, _Float16 and complex
// values, their conversions and comparisons.
//

use super::symbols::FCMP_FOLD_LINK;
use crate::codegen::asm_probe::{asm_for_with, AARCH64_LINUX, X86_64_LINUX};
use crate::common::{
    compile_and_dlopen, compile_and_run, compile_and_run_aarch64, compile_and_run_everywhere,
    compile_and_run_optimized,
};

/// Floating-point conversions, comparisons, ABI and register-allocation
/// regressions, one program run at the compile matrix levels.
///
/// Consolidates these tests, one section each (each original `main` is
/// a `noinline` section function; the program exits with the section's
/// base plus the original code):
/// - `codegen_float_cast_sizes`: 1..=6
/// - `codegen_double_to_bool`: 7..=13
/// - `codegen_unsigned_long_to_double`: 14..=23
/// - `codegen_fp_compare_xmm0_clobber`: 24..=30
/// - `codegen_double_xmm_to_gpr_movq`: 31..=36
/// - `codegen_ternary_fptr_return_type`: 37..=38
/// - `codegen_nan_comparison`: 39..=52
/// - `codegen_nan_comparison_comprehensive`: 53..=73
/// - `codegen_fp_ternary_select`: 74..=81
/// - `codegen_fp_move_no_rax_clobber`: 82..=84
/// - `codegen_stack_coloring_interference`: 85..=90
/// - `codegen_two_sse_struct_arg_spilled`: 91..=96
/// - `codegen_variadic_float_promotion`: 97..=100
/// - `codegen_float16_mega`: 101..=143
/// - `codegen_long_double_initializer_keeps_its_value`: 144..=153
/// - `codegen_variadic_floating_arguments`: 154..=161
/// - `codegen_long_double_literals_keep_their_precision`: 162..=183
/// - `codegen_long_double_nan_comparisons`: 184..=212
#[test]
fn codegen_floating_mega() {
    let code = r#"
/* ---- codegen_float_cast_sizes: exits 1..6
 * Test: float-to-int and float-to-float casts produce correct size
 */
static __attribute__((noinline)) int t_float_cast_sizes(void)
{
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

/* ---- codegen_double_to_bool: exits 7..13
 */
static __attribute__((noinline)) int t_double_to_bool(void)
{
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

/* ---- codegen_unsigned_long_to_double: exits 14..23
 */
#include <limits.h>
#include <stdio.h>

static __attribute__((noinline)) int t_unsigned_long_to_double(void)
{
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

/* ---- codegen_fp_compare_xmm0_clobber: exits 24..30
 * Regression test: FP compare clobber when src2 is allocated to Xmm0.
 * The codegen for `d < -(double)MIN` would move src1 into Xmm0, clobbering
 * src2 if it was already in Xmm0, then compare Xmm0 with itself.
 */
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

static __attribute__((noinline)) int t_fp_compare_xmm0_clobber(void)
{
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
#undef _PyTime_MIN

/* ---- codegen_double_xmm_to_gpr_movq: exits 31..36
 * Regression test: XMM-to-GPR move used movd (32-bit) instead of movq (64-bit)
 * for doubles. This truncated the upper 32 bits, causing double values like
 * 1.0 (0x3FF0000000000000) to become 0.0 (lower 32 bits are zero).
 */
double negate_if(double val, int neg) {
    if (neg) return -val;
    return val;
}

static __attribute__((noinline)) int t_double_xmm_to_gpr_movq(void)
{
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

/* ---- codegen_ternary_fptr_return_type: exits 37..38
 * Regression test: ternary selecting function pointers lost return type.
 * `(cond ? func_a : func_b)(arg)` used pointer_to() which returned void*
 * when the pointer-to-function type wasn't in the lookup table. The call's
 * return type defaulted to int (32-bit), truncating pointer return values.
 */
#include <stdlib.h>

typedef struct { int x; } Obj;

Obj *func_a(int *p) { Obj *o = malloc(sizeof(*o)); o->x = *p + 100; return o; }
Obj *func_b(int *p) { Obj *o = malloc(sizeof(*o)); o->x = *p + 200; return o; }

static __attribute__((noinline)) int t_ternary_fptr_return_type(void)
{
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

/* ---- codegen_nan_comparison: exits 39..52
 * Regression test: NaN comparisons must follow IEEE 754.
 * ucomisd sets PF=1 for NaN. Ordered comparisons (==, <, <=) must
 * return false for NaN; != must return true. sete/setb/setbe/setne
 * alone don't check PF, so we need setnp AND / setp OR.
 */
static __attribute__((noinline)) int t_nan_comparison(void)
{
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

/* ---- codegen_nan_comparison_comprehensive: exits 53..73
 * Comprehensive NaN test: value comparisons, float (single), stored results,
 * and NaN propagation through variables.
 */
static __attribute__((noinline)) int t_nan_comparison_comprehensive(void)
{
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

/* ---- codegen_fp_ternary_select: exits 74..81
 * Regression: FP negation clobbered RAX (used for sign mask), corrupting
 * integer pseudos allocated to RAX. FP ternary select with CMov also
 * failed because CMov doesn't work on XMM registers.
 */
/* FP ternary with negation — tests that fneg doesn't clobber condition */
double select_val(int negate) {
    return negate ? -1.0 : 1.0;
}

double negate_then_select(double v, int flag) {
    double neg = -v;  /* fneg uses RAX for sign mask */
    return flag ? neg : v;
}

static __attribute__((noinline)) int t_fp_ternary_select(void)
{
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

/* ---- codegen_fp_move_no_rax_clobber: exits 82..84
 * Regression test: emit_fp_move Loc::Imm used RAX as scratch, clobbering a live
 * integer value. Fixed by using R10 (reserved scratch) instead.
 */
int printf(const char *, ...);

/* Force RAX to hold a live integer value across an FP immediate load.
   The function call returns in RAX, and the subsequent FP operation
   must not clobber it. */
int compute(int x) { return x * 7; }

static __attribute__((noinline)) int t_fp_move_no_rax_clobber(void)
{
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

/* ---- codegen_stack_coloring_interference: exits 85..90
 * Regression test: stack coloring with interference-graph approach.
 * Tests that variables with non-contiguous live ranges in complex control flow
 * (gotos creating non-linear block ordering) correctly share or don't share slots
 * based on actual block-level liveness.
 */
int printf(const char *, ...);

/* Volatile to prevent optimization */
volatile int sink;

static __attribute__((noinline)) int t_stack_coloring_interference(void)
{
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

/* ---- codegen_two_sse_struct_arg_spilled: exits 91..96
 * Regression test: two-SSE struct (e.g., Py_complex {double, double}) passed as
 * a call argument when the address pseudo is spilled to stack. The Loc::Stack
 * case used LEA (address of stack slot) instead of MOV (load pointer from stack
 * slot), producing garbage values.
 */
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

static __attribute__((noinline)) int t_two_sse_struct_arg_spilled(void)
{
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

/* ---- codegen_variadic_float_promotion: exits 97..100
 * Regression test: float arguments to variadic functions (e.g., printf) were
 * not promoted to double per C99 6.5.2.2p7 "default argument promotions".
 * The ABI requires xmm0 to hold a double, but c17 passed 32-bit float bits.
 */
#include <stdio.h>
#include <string.h>

static __attribute__((noinline)) int t_variadic_float_promotion(void)
{
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

/* ---- codegen_float16_mega: exits 101..143
 */
static __attribute__((noinline)) int t_float16_mega(void)
{
    /* Arithmetic */
    _Float16 a = 3.5f16;
    _Float16 b = 2.0f16;

    _Float16 sum = a + b;
    if ((float)sum < 5.49f || (float)sum > 5.51f) return 1;

    _Float16 diff = a - b;
    if ((float)diff < 1.49f || (float)diff > 1.51f) return 2;

    _Float16 prod = a * b;
    if ((float)prod < 6.99f || (float)prod > 7.01f) return 3;

    _Float16 quot = a / b;
    if ((float)quot < 1.74f || (float)quot > 1.76f) return 4;

    /* Negation */
    _Float16 neg = -a;
    if ((float)neg > -3.49f || (float)neg < -3.51f) return 5;

    /* Comparisons */
    if (!(a == a)) return 10;
    if (a != a) return 11;
    if (!(a > b)) return 12;
    if (!(b < a)) return 13;
    if (!(a >= b)) return 14;
    if (!(b <= a)) return 15;
    if (a == b) return 16;
    if (!(a != b)) return 17;

    /* Float16 <-> float conversions */
    float f = (float)a;
    if (f < 3.49f || f > 3.51f) return 20;

    _Float16 from_float = (_Float16)f;
    if ((float)from_float < 3.49f || (float)from_float > 3.51f) return 21;

    /* Float16 <-> double conversions */
    double d = (double)a;
    if (d < 3.49 || d > 3.51) return 22;

    _Float16 from_double = (_Float16)d;
    if ((float)from_double < 3.49f || (float)from_double > 3.51f) return 23;

    /* Float16 <-> int, directly. These used to be written through a float
       intermediary by hand, to avoid __fixhfsi and the seven siblings libgcc
       does not define; the compiler goes through float itself now. */
    int i = (int)a;
    if (i != 3) return 30;

    _Float16 from_int = (_Float16)42;
    if ((float)from_int < 41.9f || (float)from_int > 42.1f) return 31;

    unsigned u = (unsigned)a;
    if (u != 3u) return 32;

    long l = (long)a;
    if (l != 3L) return 33;

    _Float16 neg_half = -3.5f16;
    if ((int)neg_half != -3) return 34;

    _Float16 from_long = (_Float16)1234L;
    if ((float)from_long < 1233.0f || (float)from_long > 1235.0f) return 35;

    /* A 128-bit operand must use the 128-bit helper. Choosing by
       `dst_size <= 32` handed these the 64-bit one and read half the value:
       (_Float16)(1<<100) came out 0 rather than infinite. */
    __int128 big = (__int128)1 << 100;
    _Float16 from_big = (_Float16)big;
    if ((float)from_big <= 65504.0f) return 36;      /* must overflow to inf */

    _Float16 exact = 2048.0f16;
    if ((long long)(__int128)exact != 2048) return 37;

    /* Compound assignment */
    _Float16 ca = 10.0f16;
    ca += 5.0f16;
    if ((float)ca < 14.9f || (float)ca > 15.1f) return 40;

    ca -= 3.0f16;
    if ((float)ca < 11.9f || (float)ca > 12.1f) return 41;

    ca *= 2.0f16;
    if ((float)ca < 23.9f || (float)ca > 24.1f) return 42;

    ca /= 4.0f16;
    if ((float)ca < 5.9f || (float)ca > 6.1f) return 43;

    return 0;
}

/* ---- codegen_long_double_initializer_keeps_its_value: exits 144..153
 * A wide `long double` initializer must reach memory in the target's own
 * format, not truncated to the `double` it was parsed into.
 *
 * Both the global and the local path used to write the raw `f64` encoding:
 * the global emitted a single 8-byte `.quad` under a `.size` of 16, and the
 * local went through the 128-bit *integer* copy helper, which moves the low
 * half through a general-purpose register and zero-fills the rest. On
 * aarch64, where `long double` is binary128, `3.14159...L` therefore landed
 * as a denormal near zero and compared less than `3.14L`.
 */
#include <float.h>

static long double g_pi = 3.14159265358979323846L;
/* Placed right after g_pi: an under-sized g_pi would run into it. */
static long double g_next = 7.5L;
static _Float16 g_half = 1.5f;
static short g_after_half = 0x1234;

static long double ld_ident(long double v) { return v; }

static __attribute__((noinline)) int t_long_double_initializer_keeps_its_value(void)
{
    if (g_pi < 3.14L || g_pi > 3.15L) return 1;
    if (g_next != 7.5L) return 2;
    if (g_half != 1.5f) return 3;
    if (g_after_half != 0x1234) return 4;

    /* Local initialization from a wide literal. */
    long double l = 3.14159265358979323846L;
    if (l < 3.14L || l > 3.15L) return 5;
    if (l != g_pi) return 6;

    /* Hex float, and a power of two that is exact in every format. */
    long double p = 0x1.0p3L;
    if (p != 8.0L) return 7;

    /* Copy through a call return value, which is where a 128-bit value moved
       via a general-purpose register loses its upper half. */
    if (ld_ident(l) != l) return 8;

    long double arr[3];
    arr[0] = l; arr[1] = p; arr[2] = -l;
    if (arr[0] != l || arr[1] != p || arr[2] != -l) return 9;

    /* More mantissa than a double can hold: if the slot ever narrows to 64
       bits on a target where it is wider, this comes back equal to 1. */
    long double eps = 1.0L + LDBL_EPSILON;
    if (sizeof(long double) > sizeof(double) && eps == 1.0L) return 10;

    return 0;
}

/* ---- codegen_variadic_floating_arguments: exits 154..161
 * Floating-point variadic arguments must survive the callee's `va_arg`.
 *
 * AAPCS64 hands unnamed floating arguments in v0-v7 and reads them back
 * through `__vr_top` / `__vr_offs`, but the aarch64 backend saved only x0-x7
 * and walked `ap` as a flat pointer over that GP area. Every
 * `va_arg(ap, double)` therefore read a general-purpose slot while the actual
 * values sat in v0-v7, unspilled. The cases past eight arguments also cover
 * the spill to the caller's stack, which is a different path again.
 */
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

static double sum_d(int n, ...) {
    va_list ap; va_start(ap, n);
    double t = 0.0;
    for (int i = 0; i < n; i++) t += va_arg(ap, double);
    va_end(ap);
    return t;
}

/* Alternating classes: the two register banks advance independently, so a
   shared cursor would interleave them wrongly. */
static double sum_mixed(int n, ...) {
    va_list ap; va_start(ap, n);
    double t = 0.0;
    for (int i = 0; i < n; i++) {
        t += (double)va_arg(ap, int);
        t += va_arg(ap, double);
    }
    va_end(ap);
    return t;
}

static long double sum_ld(int n, ...) {
    va_list ap; va_start(ap, n);
    long double t = 0.0L;
    for (int i = 0; i < n; i++) t += va_arg(ap, long double);
    va_end(ap);
    return t;
}

static double twice(int n, ...) {
    va_list ap, copy;
    va_start(ap, n);
    va_copy(copy, ap);
    double a = 0.0, b = 0.0;
    for (int i = 0; i < n; i++) a += va_arg(ap, double);
    for (int i = 0; i < n; i++) b += va_arg(copy, double);
    va_end(copy);
    va_end(ap);
    return a + b;
}

/* Hands the list to libc, which decodes it per the real platform ABI. */
static int fmt(char *buf, size_t n, const char *f, ...) {
    va_list ap; va_start(ap, f);
    int r = vsnprintf(buf, n, f, ap);
    va_end(ap);
    return r;
}

static int near(double a, double b) { double d = a - b; return d < 0.01 && d > -0.01; }

static __attribute__((noinline)) int t_variadic_floating_arguments(void)
{
    if (!near(sum_d(2, 1.5, 2.5), 4.0)) return 1;

    /* Nine doubles: the ninth has to come off the stack. */
    if (!near(sum_d(9, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0), 45.0)) return 2;

    /* Float arguments are promoted to double by the caller. */
    float fa = 1.5f, fb = 2.5f;
    if (!near(sum_d(2, fa, fb), 4.0)) return 3;

    if (!near(sum_mixed(3, 1, 1.5, 2, 2.5, 3, 3.5), 13.5)) return 4;

    long double lsum = sum_ld(3, 1.5L, 2.5L, 4.0L);
    if (lsum != 8.0L) return 5;

    if (!near(twice(3, 1.0, 2.0, 4.0), 14.0)) return 6;

    char buf[64];
    int r = fmt(buf, sizeof buf, "%d %.2f %d %.2f", 7, 1.25, 9, 2.5);
    if (r < 0) return 7;
    if (strcmp(buf, "7 1.25 9 2.50") != 0) return 8;

    return 0;
}

/* ---- codegen_long_double_literals_keep_their_precision: exits 162..183
 * A `long double` literal keeps the precision and range of its target type.
 *
 * Literals were carried as `f64` from the moment they were lexed, before the
 * type of the literal was even known, so `LDBL_MAX` had already become
 * infinity and `LDBL_MIN` zero. Hex literals now reach the target format
 * exactly, and c17's own `__LDBL_*__` macros are spelled in hex for the same
 * reason.
 *
 * Decimal literals beyond double's range or precision are still rounded --
 * that needs a big-integer decimal conversion and is tracked separately.
 *
 * Every expectation was checked against gcc on the same source, at -O0 and
 * -O2, where it returns 0 throughout.
 */
#include <float.h>

/* Which long double the target has decides what can be asserted about it:
   x87 gives 64 mantissa bits and a 15-bit exponent, AArch64 Linux gives
   binary128's 113 bits over the same exponent range, and Apple's arm64
   long double *is* double. The properties below hold wherever the format
   supports them, spelled from <float.h> so no one format is assumed. */
#define WIDER_RANGE   (LDBL_MAX_EXP > DBL_MAX_EXP)
#define WIDER_MANTISSA (LDBL_MANT_DIG > DBL_MANT_DIG)

/* Static initializers: the value must survive into the data section. */
static long double g_max = LDBL_MAX;
static long double g_min = LDBL_MIN;
static long double g_eps = LDBL_EPSILON;
#if WIDER_RANGE
static long double g_sub = 0x1p-16400L;
static long double g_neg = -0x1.8p+16000L;
#else
static long double g_sub = LDBL_TRUE_MIN;
static long double g_neg = -0x1.8p+1000L;
#endif

/* LDBL_MAX spelled in hex, needing every mantissa bit the format has. One
   ulp below it is a different value. */
#if LDBL_MANT_DIG == 64
static long double g_hex = 0x1.fffffffffffffffep+16383L;
#define ONE_ULP_BELOW_MAX 0x1.fffffffffffffffcp+16383L
#elif LDBL_MANT_DIG == 113
static long double g_hex = 0x1.ffffffffffffffffffffffffffffp+16383L;
#define ONE_ULP_BELOW_MAX 0x1.fffffffffffffffffffffffffffep+16383L
#else
static long double g_hex = 0x1.fffffffffffffp+1023L;
#define ONE_ULP_BELOW_MAX 0x1.ffffffffffffep+1023L
#endif

static __attribute__((noinline)) int t_long_double_literals_keep_their_precision(void)
{
    if (!(g_max > 0.0L)) return 1;
    if (!(g_min > 0.0L)) return 2;
    if (!(g_eps > 0.0L)) return 4;

#if WIDER_RANGE
    /* Out of double's range in both directions. */
    if (!(g_max > 1.0e308L)) return 21;
    if (!(g_min < 1.0e-308L)) return 3;
#endif

    /* The limits agree with freshly parsed literals. */
    long double l_max = LDBL_MAX;
    long double l_min = LDBL_MIN;
    if (l_max != g_max) return 5;
    if (l_min != g_min) return 6;

    /* A hex literal needing every mantissa bit is not rounded. */
    if (g_hex != LDBL_MAX) return 7;

    /* One ulp below LDBL_MAX must be a different value. */
    if (ONE_ULP_BELOW_MAX == LDBL_MAX) return 9;

    /* Subnormal long double survives rather than flushing to zero. */
    if (!(g_sub > 0.0L)) return 10;
    if (!(g_sub < g_min)) return 11;

    /* Sign is carried. */
    if (!(g_neg < 0.0L)) return 12;

    /* Arithmetic on the largest value stays finite. */
    if (g_max / 2.0L >= g_max) return 14;
    if (!(g_max / 2.0L > 0.0L)) return 22;
#if WIDER_RANGE
    if (!(g_max / 2.0L > 1.0e308L)) return 13;
#endif

    /* float and double are unaffected. */
    if (DBL_MAX <= 0.0 || DBL_MIN <= 0.0) return 15;
    if ((double)0x1.921fb54442d18p+1 != 3.141592653589793) return 16;

#if WIDER_MANTISSA
    /* Two long doubles differing only below the 53rd bit stay distinct --
       they used to collide in the constant pool, which is keyed on the value. */
    long double a = 0x1.0000000000000002p+0L;
    long double b = 0x1.0000000000000004p+0L;
    if (a == b) return 17;
    if (a == 1.0L) return 18;
#endif

    return 0;
}
#undef WIDER_RANGE
#undef WIDER_MANTISSA
#undef ONE_ULP_BELOW_MAX

/* ---- codegen_long_double_nan_comparisons: exits 184..212
 * `long double` comparisons follow IEEE unordered semantics.
 *
 * The x87 path mapped every comparison onto an *unsigned* condition code,
 * but x87 signals an unordered result by setting CF, ZF and PF together --
 * exactly the flags those codes read as "below" and "equal". Six of the ten
 * NaN comparisons came out inverted: `n == n` was true and `n != n` false,
 * and `<` / `<=` were true in both operand orders. Only `>` and `>=` were
 * right, because their codes are already false when CF is set.
 *
 * The compare also used `fcomip`, which raises invalid-operation on a quiet
 * NaN; C's relational operators other than the signalling ones want the
 * quiet `fucomip`.
 *
 * Every expectation is gcc's output on the same source.
 */
static volatile double z = 0.0;

static __attribute__((noinline)) int t_long_double_nan_comparisons(void)
{
    long double n = (long double)(z / z);   /* NaN */
    long double a = 1.0L, b = 2.0L;

    /* Every comparison against a NaN is false, except `!=` which is true. */
    if (n == n) return 1;
    if (!(n != n)) return 2;
    if (n <  a) return 3;
    if (n <= a) return 4;
    if (n >  a) return 5;
    if (n >= a) return 6;
    if (a <  n) return 7;
    if (a <= n) return 8;
    if (a >  n) return 9;
    if (a >= n) return 10;
    if (n == a) return 11;
    if (!(n != a)) return 12;

    /* Ordered comparisons must be unaffected. */
    if (!(a <  b)) return 20;
    if (b <  a)    return 21;
    if (!(a <= a)) return 22;
    if (!(a >= a)) return 23;
    if (!(a == a)) return 24;
    if (!(a != b)) return 25;
    if (!(b >  a)) return 26;
    if (a >= b)    return 27;
    if (!(b >= a)) return 28;
    if (!(a <= b)) return 29;

    return 0;
}

int main(void)
{
    int r;
    if ((r = t_float_cast_sizes()) != 0) return r;
    if ((r = t_double_to_bool()) != 0) return 6 + r;
    if ((r = t_unsigned_long_to_double()) != 0) return 13 + r;
    if ((r = t_fp_compare_xmm0_clobber()) != 0) return 23 + r;
    if ((r = t_double_xmm_to_gpr_movq()) != 0) return 30 + r;
    if ((r = t_ternary_fptr_return_type()) != 0) return 36 + r;
    if ((r = t_nan_comparison()) != 0) return 38 + r;
    if ((r = t_nan_comparison_comprehensive()) != 0) return 52 + r;
    if ((r = t_fp_ternary_select()) != 0) return 73 + r;
    if ((r = t_fp_move_no_rax_clobber()) != 0) return 81 + r;
    if ((r = t_stack_coloring_interference()) != 0) return 84 + r;
    if ((r = t_two_sse_struct_arg_spilled()) != 0) return 90 + r;
    if ((r = t_variadic_float_promotion()) != 0) return 96 + r;
    if ((r = t_float16_mega()) != 0) return 100 + r;
    if ((r = t_long_double_initializer_keeps_its_value()) != 0) return 143 + r;
    if ((r = t_variadic_floating_arguments()) != 0) return 153 + r;
    if ((r = t_long_double_literals_keep_their_precision()) != 0) return 161 + r;
    if ((r = t_long_double_nan_comparisons()) != 0) return 183 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("floating_mega", code, &[]), 0);
}

// ============================================================================
// Regression: double-to-bool conversion must use ucomisd, not ucomiss
// ============================================================================

// ============================================================================
// Regression: unsigned long to double must not use signed cvtsi2sd
// ============================================================================

// ============================================================================
// Regression: float to unsigned long long must not use signed cvttsd2si
// ============================================================================

/// The other direction of `codegen_unsigned_long_to_double`, and the same
/// trap: `cvttsd2si`/`cvttss2si`/`fisttp` all answer as though the destination
/// were signed, so every value at or above 2^63 came back as the "integer
/// indefinite" 0x8000000000000000 -- silently, at every optimization level.
///
/// The cases below straddle 2^63 in both directions and cover all three source
/// formats, because each has its own emitter: SSE for `float` and `double`,
/// x87 for `long double`. The `unsigned int` rows are here because the x87
/// path got its 32-bit answer from a signed 32-bit store, so any value at or
/// above 2^31 was wrong there too.
const FLOAT_TO_UNSIGNED: &str = r#"
int main(void) {
    volatile double d;
    volatile float f;
    volatile long double ld;

    /* Below 2^63: the signed conversion was always right, and must stay. */
    d = 9223372036854774784.0;              /* the double just below 2^63 */
    if ((unsigned long long)d != 9223372036854774784ULL) return 1;
    d = 1.0e10;
    if ((unsigned long long)d != 10000000000ULL) return 2;
    d = 0.5;
    if ((unsigned long long)d != 0ULL) return 3;

    /* At and above 2^63: this is what was broken. */
    d = 9223372036854775808.0;              /* exactly 2^63 */
    if ((unsigned long long)d != 9223372036854775808ULL) return 4;
    d = 9700000000000000000.0;
    if ((unsigned long long)d != 9700000000000000000ULL) return 5;
    d = 18446744073709549568.0;             /* the double just below 2^64 */
    if ((unsigned long long)d != 18446744073709549568ULL) return 6;

    /* float has its own emitter path. */
    f = 9223372036854775808.0f;
    if ((unsigned long long)f != 9223372036854775808ULL) return 7;
    f = 18446742974197923840.0f;            /* the float just below 2^64 */
    if ((unsigned long long)f != 18446742974197923840ULL) return 8;
    f = 100.5f;
    if ((unsigned long long)f != 100ULL) return 9;

    /* long double goes through x87 on x86-64. */
    ld = 9223372036854775808.0L;
    if ((unsigned long long)ld != 9223372036854775808ULL) return 10;
    ld = 9700000000000000000.0L;
    if ((unsigned long long)ld != 9700000000000000000ULL) return 11;
    ld = 1.0e10L;
    if ((unsigned long long)ld != 10000000000ULL) return 12;

    /* unsigned int at and above 2^31, from each source format. */
    d = 4294967295.0;
    if ((unsigned int)d != 4294967295U) return 13;
    f = 2147483648.0f;
    if ((unsigned int)f != 2147483648U) return 14;
    ld = 4294967295.0L;
    if ((unsigned int)ld != 4294967295U) return 15;

    /* Deliberately no out-of-range case: a value the destination cannot hold
       is undefined, and the two targets disagree -- x86 keeps the low bits,
       aarch64 saturates. Both are allowed, so asserting either would pin a
       platform rather than the rule. */

    /* The signed destinations must not have moved. */
    d = -1.5;
    if ((long long)d != -1LL) return 16;
    ld = -1.5L;
    if ((long long)ld != -1LL) return 17;
    return 0;
}
"#;

/// Floating-point ABI and conversion regressions, one program run at the
/// compile matrix levels and at -O1 (`compile_and_run_optimized`).
///
/// Consolidates these tests, one section each (each original `main` is
/// a `noinline` section function; the program exits with the section's
/// base plus the original code):
/// - `codegen_float_to_unsigned_long_long` and `codegen_float_to_unsigned_long_long_optimized`: 1..=17
/// - `codegen_two_sse_struct_abi`: 18..=21
/// - `codegen_float16_struct_returns_in_a_register`: 22..=29
/// - `codegen_long_double_aggregate_returns_in_st0`: 30..=42
/// - `codegen_medium_struct_passes_in_registers`: 43..=54
/// - `codegen_sse_aggregate_advances_fp_arg_count`: 55..=62
/// - `codegen_overaligned_fp_locals`: 63..=68
/// - `codegen_float16_constants_round_to_nearest_even`: 69..=80
#[test]
fn codegen_floating_abi_mega() {
    let code = r#"
/* ---- codegen_float_to_unsigned_long_long: exits 1..17
 * The other direction of `codegen_unsigned_long_to_double`, and the same
 * trap: `cvttsd2si`/`cvttss2si`/`fisttp` all answer as though the destination
 * were signed, so every value at or above 2^63 came back as the "integer
 * indefinite" 0x8000000000000000 -- silently, at every optimization level.
 *
 * The cases below straddle 2^63 in both directions and cover all three source
 * formats, because each has its own emitter: SSE for `float` and `double`,
 * x87 for `long double`. The `unsigned int` rows are here because the x87
 * path got its 32-bit answer from a signed 32-bit store, so any value at or
 * above 2^31 was wrong there too.
 *
 * (Also `codegen_float_to_unsigned_long_long_optimized`, which ran this program at another level.)
 */
static __attribute__((noinline)) int t_float_to_unsigned_long_long(void)
{
    volatile double d;
    volatile float f;
    volatile long double ld;

    /* Below 2^63: the signed conversion was always right, and must stay. */
    d = 9223372036854774784.0;              /* the double just below 2^63 */
    if ((unsigned long long)d != 9223372036854774784ULL) return 1;
    d = 1.0e10;
    if ((unsigned long long)d != 10000000000ULL) return 2;
    d = 0.5;
    if ((unsigned long long)d != 0ULL) return 3;

    /* At and above 2^63: this is what was broken. */
    d = 9223372036854775808.0;              /* exactly 2^63 */
    if ((unsigned long long)d != 9223372036854775808ULL) return 4;
    d = 9700000000000000000.0;
    if ((unsigned long long)d != 9700000000000000000ULL) return 5;
    d = 18446744073709549568.0;             /* the double just below 2^64 */
    if ((unsigned long long)d != 18446744073709549568ULL) return 6;

    /* float has its own emitter path. */
    f = 9223372036854775808.0f;
    if ((unsigned long long)f != 9223372036854775808ULL) return 7;
    f = 18446742974197923840.0f;            /* the float just below 2^64 */
    if ((unsigned long long)f != 18446742974197923840ULL) return 8;
    f = 100.5f;
    if ((unsigned long long)f != 100ULL) return 9;

    /* long double goes through x87 on x86-64. */
    ld = 9223372036854775808.0L;
    if ((unsigned long long)ld != 9223372036854775808ULL) return 10;
    ld = 9700000000000000000.0L;
    if ((unsigned long long)ld != 9700000000000000000ULL) return 11;
    ld = 1.0e10L;
    if ((unsigned long long)ld != 10000000000ULL) return 12;

    /* unsigned int at and above 2^31, from each source format. */
    d = 4294967295.0;
    if ((unsigned int)d != 4294967295U) return 13;
    f = 2147483648.0f;
    if ((unsigned int)f != 2147483648U) return 14;
    ld = 4294967295.0L;
    if ((unsigned int)ld != 4294967295U) return 15;

    /* Deliberately no out-of-range case: a value the destination cannot hold
       is undefined, and the two targets disagree -- x86 keeps the low bits,
       aarch64 saturates. Both are allowed, so asserting either would pin a
       platform rather than the rule. */

    /* The signed destinations must not have moved. */
    d = -1.5;
    if ((long long)d != -1LL) return 16;
    ld = -1.5L;
    if ((long long)ld != -1LL) return 17;
    return 0;
}

/* ---- codegen_two_sse_struct_abi: exits 18..21
 * Regression test: struct { double, double } must be passed in XMM registers
 * per SysV AMD64 ABI, and return values in XMM0+XMM1 must be passable
 * directly as arguments to another function taking the same struct type.
 */
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

static __attribute__((noinline)) int t_two_sse_struct_abi(void)
{
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

/* ---- codegen_float16_struct_returns_in_a_register: exits 22..29
 * A struct holding `_Float16` is returned in an SSE register, not through a
 * hidden pointer.
 *
 * `_Float16` was missing from the ABI classifier's notion of a floating type,
 * so an eightbyte holding one answered MEMORY. The caller then expected the
 * callee to have written the value through a hidden pointer, the callee
 * returned it in registers instead, and the result read back as zero -- with
 * no diagnostic. Checked against gcc, which returns it in xmm0.
 */
struct H  { _Float16 v; };
struct H2 { _Float16 a, v; };
struct F  { float v; };
struct D  { double a, v; };

__attribute__((noinline)) static struct H  mk(void)  { struct H r;  r.v = 2.5f16; return r; }
__attribute__((noinline)) static struct H2 mk2(void) { struct H2 r; r.a = 1.5f16; r.v = 2.5f16; return r; }
__attribute__((noinline)) static struct F  mkf(void) { struct F r;  r.v = 3.5f;   return r; }
__attribute__((noinline)) static struct D  mkd(void) { struct D r;  r.a = 1.0; r.v = 4.5; return r; }

__attribute__((noinline)) static _Float16 take(struct H a)  { return a.v; }
__attribute__((noinline)) static _Float16 take2(struct H2 a) { return a.v; }
__attribute__((noinline)) static _Float16 scalar(_Float16 a, _Float16 b) { return a + b; }

static __attribute__((noinline)) int t_float16_struct_returns_in_a_register(void)
{
    if ((float)mk().v != 2.5f) return 1;
    if ((float)mk2().v != 2.5f) return 2;
    if ((float)mk2().a != 1.5f) return 3;

    struct H h = mk();
    if ((float)take(h) != 2.5f) return 4;
    struct H2 h2 = mk2();
    if ((float)take2(h2) != 2.5f) return 5;

    /* A scalar `_Float16` is an SSE argument too; it used to be counted as an
       integer one, which only worked because the two sides kept separate
       indices. */
    if ((float)scalar(1.5f16, 2.5f16) != 4.0f) return 6;

    /* Controls: the float and two-double shapes were already right. */
    if (mkf().v != 3.5f) return 7;
    if (mkd().v != 4.5) return 8;

    return 0;
}

/* ---- codegen_long_double_aggregate_returns_in_st0: exits 30..42
 * An aggregate that is nothing but a `long double` is returned in st(0).
 *
 * System V classifies its two eightbytes X87 and X87UP, and X87UP is preceded
 * by X87, so the merge-to-MEMORY rule does not fire: gcc emits `fld1; ret` for
 * `struct R { long double v; } f(void)`. c17 decided sret by raw size instead
 * -- and 128 bits is not *greater* than 128 -- so it took the two-register
 * path, returned RAX:RDX, and the caller read a slot nothing had written.
 * The value came back as zero.
 *
 * The moment anything shares an eightbyte the merge rules do apply, so
 * `union { long double v; double d; }` is MEMORY and really is returned
 * through a hidden pointer. Both halves are checked here, against gcc.
 */
struct R  { long double v; };
struct I  { long double v; };
struct N  { struct I v; };
struct A  { long double v[1]; };
union  U  { long double v; };
union  M  { long double v; double d; };   /* X87 merged with SSE -> MEMORY */
struct W  { long double v; int tag; };    /* over two eightbytes -> MEMORY */

__attribute__((noinline)) static struct R fo4_mk(void)  { struct R r; r.v = 3.25L; return r; }
__attribute__((noinline)) static struct N mkn(void) { struct N r; r.v.v = 3.25L; return r; }
__attribute__((noinline)) static struct A mka(void) { struct A r; r.v[0] = 3.25L; return r; }
__attribute__((noinline)) static union  U mku(void) { union  U r; r.v = 3.25L; return r; }
__attribute__((noinline)) static union  M mkm(void) { union  M r; r.v = 3.25L; return r; }
__attribute__((noinline)) static struct W mkw(void) { struct W r; r.v = 3.25L; r.tag = 7; return r; }

/* Small enough to tempt the inliner: its `Ret` carries an address, which must
   not be spliced into a caller expecting a value. */
static struct R mk_inlinable(void) { struct R r; r.v = 6.5L; return r; }

static __attribute__((noinline)) int t_long_double_aggregate_returns_in_st0(void)
{
    if (fo4_mk().v  != 3.25L) return 1;
    if (mkn().v.v != 3.25L) return 2;
    /* Through a local: indexing an array member of a call-result rvalue is a
       separate, pre-existing defect that has nothing to do with the return
       class -- it fails for `struct { int v[2]; }` too. */
    struct A arr = mka();
    if (arr.v[0] != 3.25L) return 3;
    if (mku().v != 3.25L) return 4;
    if (mkm().v != 3.25L) return 5;
    if (mkw().v != 3.25L || mkw().tag != 7) return 6;

    /* Assigned through a local, and used twice in one expression. */
    struct R a = fo4_mk();
    if (a.v != 3.25L) return 7;
    if (fo4_mk().v + fo4_mk().v != 6.5L) return 8;

    /* The inlinable one, twice, so a spliced body would be caught. */
    if (mk_inlinable().v != 6.5L) return 9;
    struct R b = mk_inlinable();
    if (b.v != 6.5L) return 10;
    if (mk_inlinable().v + mk_inlinable().v != 13.0L) return 11;

    /* A long double local must survive all of it. */
    long double keep = 1.5L;
    if (fo4_mk().v != 3.25L) return 12;
    if (keep != 1.5L) return 13;

    return 0;
}

/* ---- codegen_medium_struct_passes_in_registers: exits 43..54
 * A nine-to-sixteen-byte integer or mixed struct travels in two registers.
 *
 * System V classifies each eightbyte independently: `struct { long a, b; }` is
 * two general registers, `struct { double a; int b; }` is an SSE register and
 * a general one. c17 passed a *pointer* in one general register instead. Both
 * sides of a c17 translation unit agreed, so running a program could never
 * catch it -- it only bites against a gcc-compiled peer, which is why this is
 * checked here for behaviour and by an assembly probe for the register file.
 *
 * Also covers the two accounting rules the register form needs: an argument
 * that does not fit goes to memory *whole* (System V 3.2.3 step 5), and a
 * struct before an ellipsis spends every register it occupies, or `va_start`
 * reads the save area at the wrong index.
 */
#include <stdarg.h>

struct LL { long a, b; };
struct DI { double a; int b; };
struct ID { int a; double b; };
struct III { int a, b, c; };
struct PAD { char pad[12]; int v; };
struct DD { double a, b; };          /* control: already correct */
struct I1 { int v; };                /* control: one eightbyte */

__attribute__((noinline)) static long  ll(struct LL s)  { return s.a + s.b; }
__attribute__((noinline)) static double di(struct DI s) { return s.a + s.b; }
__attribute__((noinline)) static double id(struct ID s) { return s.a + s.b; }
__attribute__((noinline)) static int   iii(struct III s){ return s.a + s.b + s.c; }
__attribute__((noinline)) static int   pad(struct PAD s){ return s.v; }
__attribute__((noinline)) static double dd(struct DD s) { return s.a + s.b; }
__attribute__((noinline)) static int    i1(struct I1 s) { return s.v; }

/* The struct runs out of registers and must go to memory whole. */
__attribute__((noinline)) static long over(long a, long b, long c, long d,
                                           long e, long f, struct LL s)
{ return a + b + c + d + e + f + s.a + s.b; }
__attribute__((noinline)) static double overf(double a, double b, double c, double d,
                                              double e, double f, double g, double h,
                                              struct DI s)
{ return a + b + c + d + e + f + g + h + s.a + s.b; }

/* A register-pair struct before the ellipsis. */
__attribute__((noinline)) static long va(struct LL s, ...)
{
    va_list ap; va_start(ap, s);
    long x = va_arg(ap, long), y = va_arg(ap, long);
    va_end(ap);
    return s.a + s.b + x + y;
}

/* The argument crosses a call, so it has to survive a spill. */
__attribute__((noinline)) static long ident(long v) { return v; }
__attribute__((noinline)) static long cross(struct LL s)
{ long t = ident(5); return s.a + s.b + t; }

static __attribute__((noinline)) int t_medium_struct_passes_in_registers(void)
{
    struct LL  l = { 3, 4 };
    struct DI  m = { 1.5, 2 };
    struct ID  n = { 2, 1.5 };
    struct III o = { 1, 2, 3 };
    struct PAD p = { { 0 }, 9 };
    struct DD  d = { 1.5, 2.5 };
    struct I1  i = { 7 };

    if (ll(l) != 7) return 1;
    if (di(m) != 3.5) return 2;
    if (id(n) != 3.5) return 3;
    if (iii(o) != 6) return 4;
    if (pad(p) != 9) return 5;

    /* Controls: an all-SSE pair and a single eightbyte must not move. */
    if (dd(d) != 4.0) return 6;
    if (i1(i) != 7) return 7;

    if (over(1, 2, 3, 4, 5, 6, l) != 28) return 8;
    if (overf(1, 2, 3, 4, 5, 6, 7, 8, m) != 39.5) return 9;
    if (va(l, 10L, 20L) != 37) return 10;
    if (cross(l) != 12) return 11;

    /* Passed straight through, so both directions run in one expression. */
    if (ll((struct LL){ 5, 6 }) != 11) return 12;

    return 0;
}

/* ---- codegen_sse_aggregate_advances_fp_arg_count: exits 55..62
 * An eight-byte all-float aggregate is one whole XMM register by its class,
 * but the prologue's *counting-only* path for a spilled parameter asked the
 * size instead -- `> 64 bits` -- and tallied it as a general register. Every
 * FP argument behind it then read a register one too low: in `g(F2, D2)` the
 * two-SSE `D2` was loaded from XMM0/XMM1, the second of which still held the
 * `F2`.
 *
 * The register-emitting walk in the same function asks `sse_struct_regs` and
 * was right all along, which is why this only shows when the parameter
 * spills. `-O0`/`-O1` on x86-64; at `-O2` the callee inlines and the bug
 * disappears. Values checked against `gcc -std=c17`.
 */
typedef struct { float a, b; } F2;
typedef struct { float a; } F1;
typedef struct { double a, b; } D2;
typedef struct { long x; double y; } MIX;

#define N __attribute__((noinline)) static
N double g1(D2 f) { return f.a + f.b; }
N double g2(F2 a, D2 f) { (void)a; return f.a + f.b; }
N double g3(F1 a, D2 f) { (void)a; return f.a + f.b; }
N double g4(F2 a, F2 b, D2 f) { (void)a; (void)b; return f.a + f.b; }
N double g5(D2 f, F2 a) { (void)a; return f.a + f.b; }
N double g6(F2 a, double x, D2 f) { (void)a; (void)x; return f.a + f.b; }
N double g7(MIX m, D2 f) { (void)m; return f.a + f.b; }
N double g8(F2 a, F2 b, F2 c, F2 d, F2 e, D2 f)
{ return a.a + b.a + c.a + d.a + e.a + f.a + f.b; }

static __attribute__((noinline)) int t_sse_aggregate_advances_fp_arg_count(void)
{
    F2 v = {2, 3};
    F1 w = {4};
    D2 r = {9, 10};
    MIX m = {1, 2};

    if (g1(r) != 19.0) return 1;
    if (g2(v, r) != 19.0) return 2;
    if (g3(w, r) != 19.0) return 3;
    if (g4(v, v, r) != 19.0) return 4;
    if (g5(r, v) != 19.0) return 5;
    if (g6(v, 1.0, r) != 19.0) return 6;
    if (g7(m, r) != 19.0) return 7;
    if (g8(v, v, v, v, v, r) != 29.0) return 8;
    return 0;
}
#undef N

/* ---- codegen_overaligned_fp_locals: exits 63..68
 * A frame whose locals are over-aligned addresses them through `%rsp`, since
 * `andq $-64, %rsp` breaks the fixed relationship to `%rbp`. `stack_mem`
 * exists to pick the right base, and the floating-point paths spelled the
 * `%rbp` case out longhand instead -- eleven sites, every one of them losing
 * the `%rsp` case. An over-aligned `double` array was zeroed at its real
 * address and initialized somewhere else entirely.
 *
 * `long` was always fine, and so was `aligned(16)`: it takes an alignment
 * past the natural one *and* a floating-point type to reach these paths.
 */
static __attribute__((noinline)) int t_overaligned_fp_locals(void)
{
    __attribute__((aligned(64))) double w[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    __attribute__((aligned(128))) float f[4] = {1.5f, 2.5f, 3.5f, 4.5f};
    __attribute__((aligned(64))) double scalar = 2.25;
    double s = 0;
    float t = 0;
    for (int i = 0; i < 10; i++) s += w[i];
    for (int i = 0; i < 4; i++) t += f[i];

    if (w[0] != 1.0 || w[9] != 10.0) return 1;
    if (s != 55.0) return 2;
    if (t != 12.0f) return 3;
    if (scalar != 2.25) return 4;
    /* Comparison and arithmetic read through the same paths. */
    if (!(w[3] < w[4]) || !(w[9] > scalar)) return 5;
    if (w[2] * w[3] != 12.0) return 6;
    return 0;
}

/* ---- codegen_float16_constants_round_to_nearest_even: exits 69..80
 * A `_Float16` constant rounds to nearest, ties to even.
 *
 * The conversion truncated the significand, which put every inexact
 * `_Float16` constant one ulp below the value the source named: `0.3f16`
 * came out 0.299805 where gcc gives 0.300049. So a program's constants
 * disagreed with the same values computed at run time, and with every other
 * compiler.
 *
 * The aarch64 backend carried a verbatim copy of the conversion, which is
 * what would have kept this fix on one target.
 */
static __attribute__((noinline)) int t_float16_constants_round_to_nearest_even(void)
{
    /* The headline case: 0.3 is 1.2 x 2^-2, and 0.2 x 1024 is 204.8, so the
       fraction rounds up to 205 -- truncation gave 204. */
    _Float16 a = 0.3f16;
    if ((float)a <= 0.30004f || (float)a >= 0.30006f) return 1;

    /* Rounding up and rounding down both have to happen. */
    _Float16 b = 1.0009765625f16;   /* exactly representable: 1 + 1/1024 */
    if ((float)b != 1.0009765625f) return 2;

    /* A tie rounds to even, not away from zero. Above 2048 the spacing is 2,
       so every odd value is exactly halfway between two representable ones,
       and the one with the even fraction wins -- which sends ties in both
       directions. Truncation would send all four down. */
    _Float16 t1 = 2049.0f16;   /* between 2048 and 2050; 2048 is even */
    _Float16 t2 = 2051.0f16;   /* between 2050 and 2052; 2052 is even */
    _Float16 t3 = 2053.0f16;   /* between 2052 and 2054; 2052 is even */
    _Float16 t4 = 2055.0f16;   /* between 2054 and 2056; 2056 is even */
    if ((float)t1 != 2048.0f) return 3;
    if ((float)t2 != 2052.0f) return 4;
    if ((float)t3 != 2052.0f) return 11;
    if ((float)t4 != 2056.0f) return 12;

    /* Exact values are unaffected. */
    if ((float)(_Float16)1.0f16 != 1.0f) return 5;
    if ((float)(_Float16)0.5f16 != 0.5f) return 6;
    if ((float)(_Float16)(-2.0f16) != -2.0f) return 7;
    if ((float)(_Float16)0.0f16 != 0.0f) return 8;

    /* Overflow and underflow still saturate. */
    _Float16 big = 1e30f16;
    if (!(big > 60000.0f16 || big != big)) return 9;

    /* A constant must agree with the same value converted at run time. */
    volatile float src = 0.3f;
    _Float16 converted = (_Float16)src;
    if ((float)converted != (float)a) return 10;

    return 0;
}

int main(void)
{
    int r;
    if ((r = t_float_to_unsigned_long_long()) != 0) return r;
    if ((r = t_two_sse_struct_abi()) != 0) return 17 + r;
    if ((r = t_float16_struct_returns_in_a_register()) != 0) return 21 + r;
    if ((r = t_long_double_aggregate_returns_in_st0()) != 0) return 29 + r;
    if ((r = t_medium_struct_passes_in_registers()) != 0) return 42 + r;
    if ((r = t_sse_aggregate_advances_fp_arg_count()) != 0) return 54 + r;
    if ((r = t_overaligned_fp_locals()) != 0) return 62 + r;
    if ((r = t_float16_constants_round_to_nearest_even()) != 0) return 68 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("floating_abi_mega", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("floating_abi_mega_opt", code), 0);
}

#[test]
fn codegen_float_to_unsigned_long_long_aarch64() {
    // aarch64 has `fcvtzu` and was never wrong here; the test pins that, and
    // catches a "fix" applied to the wrong target.
    if let Some(code) = compile_and_run_aarch64("float_to_unsigned_a64", FLOAT_TO_UNSIGNED, "-O2") {
        assert_eq!(code, 0);
    }
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

/// A `_Float16` store writes two bytes, not four.
///
/// x86-64 stored a half with `movss`, which writes four bytes: assigning one
/// member of a struct of `_Float16`s overwrote the next member, and storing
/// the imaginary half of a `_Float16 _Complex` wrote two bytes past the
/// object. SSE2 has no two-byte store from an XMM register, so the value
/// goes through a general register, as gcc does.
#[test]
fn codegen_float16_store_writes_two_bytes() {
    let code = r#"
typedef _Float16 _Complex hc;
struct S { _Float16 a, b, c, d; };
_Float16 g[4] = {1, 2, 3, 4};
__attribute__((noinline)) void member(struct S *p, _Float16 v) { p->b = v; }
__attribute__((noinline)) void global(_Float16 v) { g[1] = v; }
__attribute__((noinline)) void whole(hc *p, hc v) { *p = v; }
__attribute__((noinline)) void imag(hc *p, _Float16 v) { __imag__ *p = v; }
__attribute__((noinline)) void real(hc *p, _Float16 v) { __real__ *p = v; }
static hc mk(int r, int i) { return __builtin_complex((_Float16)r, (_Float16)i); }
int main(void) {
    struct S s = {1, 2, 3, 4};
    member(&s, 9);
    if (s.a != 1 || s.b != 9 || s.c != 3 || s.d != 4) return 1;
    global(9);
    if (g[0] != 1 || g[1] != 9 || g[2] != 3 || g[3] != 4) return 2;
    hc arr[3] = {mk(1, 2), mk(3, 4), mk(5, 6)};
    whole(&arr[1], mk(7, 8));
    if (__imag__ arr[0] != 2 || __real__ arr[1] != 7 || __imag__ arr[1] != 8
        || __real__ arr[2] != 5)
        return 3;
    imag(&arr[0], 10);
    if (__real__ arr[0] != 1 || __imag__ arr[0] != 10 || __real__ arr[1] != 7) return 4;
    real(&arr[1], 11);
    if (__real__ arr[1] != 11 || __imag__ arr[1] != 8 || __real__ arr[2] != 5) return 5;
    /* A local struct's member, addressed from the frame. */
    struct S t = {1, 2, 3, 4};
    volatile _Float16 v = 9;
    t.b = v;
    if (t.a != 1 || t.b != 9 || t.c != 3) return 6;
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(&format!("float16_store{opt}"), code, &[opt.to_string()]),
            0,
            "host {opt}"
        );
        if let Some(rc) = compile_and_run_aarch64(&format!("float16_store_a64{opt}"), code, opt) {
            assert_eq!(rc, 0, "aarch64 {opt}");
        }
    }
}

// ============================================================================
// Thread-local storage: which model each build mode selects
// ============================================================================

/// `-fPIC` must not leave thread-local access in the Local Exec model.
///
/// Local Exec bakes the offset from the thread pointer in at link time, which
/// only works for the main executable. c17 recognised `-fPIC` and folded it
/// into `pic_mode`, but the TLS decision reads `shared_mode`, so `-fPIC`
/// silently produced `%fs:tv@TPOFF` -- a code-generation flag with no effect
/// on code generation.
///
/// `pic_mode` is the wrong signal to have used, which is why the control test
/// below matters: it is also set by `-fPIE` and by the PIE default, and a PIE
/// executable *should* keep Local Exec, since it still resolves its own
/// thread-locals at link time. gcc draws the line in the same place.
///
/// gcc goes further and uses General Dynamic here; Initial Exec is the
/// strongest model c17 has, and is correct for a shared object loaded at
/// startup. See `cc/TODO.md` for what General Dynamic still needs.
#[test]
fn codegen_fpic_selects_a_position_independent_tls_model() {
    let src = r#"
_Thread_local int tv;
int read_tls(int x) { return x + tv; }
void write_tls(int x) { tv = x; }
int *addr_tls(void) { return &tv; }
"#;

    for flags in [&["-O", "-fPIC"][..], &["-O", "--shared"][..]] {
        let asm = asm_for_with("tls_pic", X86_64_LINUX, src, flags);
        assert!(
            asm.contains("@TLSDESC") && asm.contains("@TLSCALL"),
            "x86_64 {flags:?}: expected the dynamic model:\n{asm}"
        );
        assert!(
            !asm.contains("@TPOFF") && !asm.contains("@GOTTPOFF"),
            "x86_64 {flags:?}: a static model cannot serve a dlopened library:\n{asm}"
        );

        let asm = asm_for_with("tls_pic", AARCH64_LINUX, src, flags);
        assert!(
            asm.contains(":tlsdesc:") && asm.contains(".tlsdesccall"),
            "aarch64 {flags:?}: expected the dynamic model:\n{asm}"
        );
        assert!(
            !asm.contains("tprel") && !asm.contains("gottprel"),
            "aarch64 {flags:?}: a static model cannot serve a dlopened library:\n{asm}"
        );
    }
}

/// The dynamic model still computes the right addresses and values.
///
/// The companion to the assembly test above: typing the address as a pointer
/// and materializing it once must not change what the program reads or where
/// it writes.
#[test]
fn codegen_dynamic_tls_double_round_trips() {
    let lib = r#"
__thread double dv = 2.5;
__thread int iv = 7;
double get_double(void) { return dv * 2.0 + dv; }
int get_int(void) { return iv + iv; }
double *addr_double(void) { return &dv; }
void set_double(double v) { dv = v; }
"#;
    let main = r#"
#include <dlfcn.h>
int main(void)
{
    void *h = dlopen("./lib.so", RTLD_NOW);
    if (!h) return 1;
    double (*get_double)(void) = (double (*)(void))dlsym(h, "get_double");
    int (*get_int)(void) = (int (*)(void))dlsym(h, "get_int");
    double *(*addr_double)(void) = (double *(*)(void))dlsym(h, "addr_double");
    void (*set_double)(double) = (void (*)(double))dlsym(h, "set_double");
    if (!get_double || !get_int || !addr_double || !set_double) return 2;

    if (get_double() != 7.5) return 3;
    if (get_int() != 14) return 4;
    if (*addr_double() != 2.5) return 5;

    set_double(4.0);
    if (get_double() != 12.0) return 6;
    if (*addr_double() != 4.0) return 7;
    return 0;
}
"#;
    assert_eq!(compile_and_dlopen("tls_double", lib, main, &[]), 0);
}

/// An x87 conversion must not scribble on a live `long double` local.
///
/// `fild`/`fld` have no register form, so an immediate or a general register
/// has to be staged through memory on its way into the FPU. That staging
/// address was `-(callee_saved_offset + 8)(%rbp)`, which nothing had reserved:
/// slot offsets start at zero, so it landed on the first local. For a
/// `long double` first local it overwrote bytes 8 and 9 -- the sign and
/// exponent -- turning `1.0L` into `2^-16382`, which prints as `0.0`.
///
/// The audit filed this as two `long double`-returning calls in one argument
/// list outliving a temporary. It is neither: one conversion is enough, and no
/// call is needed at all. Every expectation checked against gcc.
#[test]
fn codegen_x87_scratch_does_not_clobber_a_live_local() {
    let code = r#"
static long double add2(long double a, long double b) { return a + b; }

int main(void)
{
    /* The audit's shape: a live long double, then calls whose arguments need
       an int -> long double conversion. */
    long double x = 1;
    if (add2(1, 2) != 3.0L) return 1;
    if (x != 1.0L) return 2;

    long double y = 2;
    if (add2(3, 4) != 7.0L) return 3;
    if (x != 1.0L) return 4;
    if (y != 2.0L) return 5;

    /* No call at all: one int -> long double conversion is enough. */
    long double z = 5;
    int n = 9;
    long double w = n;
    if (w != 9.0L) return 6;
    if (z != 5.0L) return 7;

    /* Through a register rather than an immediate. */
    long double u = 6;
    volatile int m = 11;
    long double v = m;
    if (v != 11.0L) return 8;
    if (u != 6.0L) return 9;

    /* long double -> int, which stages through the same address. */
    long double p = 7;
    if ((int)3.75L != 3) return 10;
    if (p != 7.0L) return 11;

    /* An XMM <-> x87 transfer, likewise. */
    long double r = 8;
    double d = 2.5;
    long double e = d;
    if (e != 2.5L) return 12;
    if (r != 8.0L) return 13;

    return 0;
}
"#;
    // Pinned to -O0. The staging path is only reached when the value being
    // converted is an immediate or a general register; at -O the folder turns
    // these into constants and the conversion disappears, so the default
    // `-g -O` config cannot see the defect at all. A trailing flag wins, so
    // this overrides the matrix.
    assert_eq!(
        compile_and_run("codegen_x87_scratch_no_clobber", code, &["-O0".to_string()]),
        0
    );
    assert_eq!(
        compile_and_run_optimized("codegen_x87_scratch_no_clobber_opt", code),
        0
    );
}

/// A GNU complex integer's argument and return ABI, including every overflow
/// case.
///
/// A complex type carries its *base's* kind, so `_Complex long` satisfied
/// `is_integer` and was handed a single register for a sixteen-byte value,
/// while `_Complex signed char` reached the sub-32-bit path and was
/// sign-extended -- which overwrites the imaginary half with a copy of the
/// real one's sign. The backends' `is_complex` tests all meant "arrives in
/// SSE/V registers", which a complex integer does not.
///
/// The overflow cases are separate bugs again: a stacked complex value's
/// address is sometimes spilled to a slot, and the outgoing copy took the
/// address *of the slot* rather than loading the pointer out of it, so the
/// callee received a pointer's bytes.
#[test]
fn codegen_complex_integer_abi() {
    let code = r#"
_Complex signed char cc_id(_Complex signed char x) { return x; }
_Complex short cs_add(_Complex short a, _Complex short b) { return a + b; }

/* Past the general-register file, one and two registers wide. */
_Complex int one_reg(long a, long b, long c, long d, long e, long f, long g,
                     long h, _Complex int p) { return p; }
_Complex long two_reg(long a, long b, long c, long d, long e, long f, long g,
                      long h, _Complex long p) { return p; }
/* Three stacked complex integers in a row: the second's width decides where
   the third lands, so a wrong slot size shows up only here. */
_Complex int three(long a, long b, long c, long d, long e, long f, long g,
                   long h, _Complex int p, _Complex long q, _Complex int r) {
    return p + (_Complex int)q + r;
}
/* Mixed with the floating file, so both register counters advance right. */
_Complex long mixed(double x, _Complex int a, double y, _Complex long b) {
    return b + (_Complex long)a + (long)x + (long)y;
}
/* A complex integer behind a complex double, which takes V/XMM registers. */
_Complex int after_cd(_Complex double z, _Complex int a) {
    return a + (int)__real__ z;
}

int main(void) {
    _Complex signed char a;
    __real__ a = 3; __imag__ a = -4;
    _Complex signed char b = cc_id(a);
    if (__real__ b != 3 || __imag__ b != -4) return 1;

    _Complex short p, q;
    __real__ p = 300; __imag__ p = 400;
    __real__ q = 1;   __imag__ q = 2;
    _Complex short s = cs_add(p, q);
    if (__real__ s != 301 || __imag__ s != 402) return 2;

    _Complex int ci, cr;
    _Complex long cl;
    __real__ ci = 1;   __imag__ ci = 2;
    __real__ cl = 10;  __imag__ cl = 20;
    __real__ cr = 100; __imag__ cr = 200;

    _Complex int o = one_reg(1, 2, 3, 4, 5, 6, 7, 8, ci);
    if (__real__ o != 1 || __imag__ o != 2) return 3;

    _Complex long t = two_reg(1, 2, 3, 4, 5, 6, 7, 8, cl);
    if (__real__ t != 10 || __imag__ t != 20) return 4;

    _Complex int th = three(1, 2, 3, 4, 5, 6, 7, 8, ci, cl, cr);
    if (__real__ th != 111 || __imag__ th != 222) return 5;

    _Complex long ml = mixed(2.0, ci, 3.0, cl);
    if (__real__ ml != 10 + 1 + 2 + 3) return 6;
    if (__imag__ ml != 20 + 2) return 7;

    _Complex double cd;
    __real__ cd = 7.0; __imag__ cd = 8.0;
    _Complex int ac = after_cd(cd, ci);
    if (__real__ ac != 8 || __imag__ ac != 2) return 8;
    return 0;
}
"#;
    assert_eq!(compile_and_run("cg_complex_int_abi", code, &[]), 0);
    assert_eq!(
        compile_and_run("cg_complex_int_abi_o2", code, &["-O2".to_string()]),
        0
    );
}

/// Every `long double` local took the same hand-spelled `%rbp` displacement,
/// from the x87 emitter's own copies of it. That one is not a wild pointer
/// -- the store and the load agree with each other -- so it runs, but it
/// names an address the aligned base also hands out, and which of the two
/// survives depends on `%rbp`'s dynamic alignment.
pub(super) const OVER_ALIGNED_LONG_DOUBLE: &str = r#"
typedef struct __attribute__((aligned(32))) { double a, b, c, d; } Over;

__attribute__((noinline)) static int take(long double x, long double y, int t)
{ return (x == 1.5L && y == 2.5L && t == 7) ? 0 : 1; }

__attribute__((noinline)) static long double sum(long double a, long double b)
{ return a + b; }

int main(void)
{
    Over o = { 1, 2, 3, 4 };    /* over-aligns main's frame */
    long double x = 1.5L, y = 2.5L;
    volatile long double z;
    if (o.a != 1 || o.d != 4) return 2;
    if (take(x, y, 7)) return 3;
    z = sum(x, y);
    if (z != 4.0L) return 4;
    return 0;
}
"#;

#[test]
fn codegen_over_aligned_frame_holds_a_long_double() {
    assert_eq!(
        compile_and_run("c17_overaligned_x87", OVER_ALIGNED_LONG_DOUBLE, &[]),
        0
    );
    assert_eq!(
        compile_and_run(
            "c17_overaligned_x87_o2",
            OVER_ALIGNED_LONG_DOUBLE,
            &["-O2".to_string()]
        ),
        0
    );
}

/// Float constants reach the optimizer: arithmetic over two of them folds,
/// so does a comparison and a conversion, and `fabs(x) < 0.0` folds without
/// knowing `x` at all.
///
/// Three things here answer differently from the integer rules underneath
/// them, and each has its own arm below. NaN makes a comparison *false*
/// rather than reversing it, so only `!=` is true of one -- which is why the
/// `fabs` rule is stated as "never less than zero" rather than "non-negative"
/// and covers only the strict form. A conversion whose value does not fit is
/// undefined, so it must still be computed at run time. And a comparison
/// carries its *operand* type, so a folded one had to be re-typed: left as a
/// `double`, the constant came back in an SSE register from a function
/// returning `int`.
#[test]
fn codegen_float_constants_fold() {
    let code = r#"
extern double fabs(double);
extern void link_error(void);
extern void abort(void);

volatile double opaque = 1.0;

__attribute__((noinline)) static void folds(double x)
{
    /* No absolute value is below zero, a NaN argument included: an
       unordered `<` is false as well. */
    if (fabs(x) < 0.0) link_error();
    if (0.0 > fabs(x)) link_error();

    /* Both constants known. A negative literal is a negation of a positive
       one, exactly as in the integer case, so `-0.0` folding at all is the
       `FNeg` rule. */
    if (1.0 > 2.0) link_error();
    if (2.0 != 2.0) link_error();
    if (-0.0 != 0.0) link_error();
    if ((int) 1.9 != 1) link_error();
    if ((int) -1.9 != -1) link_error();
    if ((unsigned) 3.5 != 3u) link_error();

    /* Arithmetic, rounded at the format the program computes in rather than
       at the width the literals are carried in: at 128 significand bits
       these three would come out equal. */
    if (1.5 * 2.0 - 0.5 != 2.5) link_error();
    if (0.1 + 0.2 == 0.3) link_error();
    if ((float) (0.1f + 0.2f) != 0.3f) link_error();
    if ((double) 1.5f != 1.5) link_error();
}

/* The neighbours of the fabs rule that do NOT hold. Each is true or false
   depending on the argument, so each must still be evaluated. */
__attribute__((noinline)) static int le_zero(double x) { return fabs(x) <= 0.0; }
__attribute__((noinline)) static int ge_zero(double x) { return fabs(x) >= 0.0; }

/* A folded comparison returns an `int`, in an integer register. */
__attribute__((noinline)) static int always_false(void) { return 1.0 > 2.0; }

/* Out of `int` range, so undefined and not foldable: this must still be the
   hardware's answer rather than one invented here. */
__attribute__((noinline)) static long big(void) { return (long) 3e9; }

/* Neither is foldable either, because each raises: one overflows to
   infinity on the way to `float`, the other divides by zero. */
__attribute__((noinline)) static float overflows(void) { return (float) 1e300; }
__attribute__((noinline)) static double by_zero(double z) { return 1.0 / z; }

/* A negative zero is a *constant* once the negation folds, and it has to
   survive being materialized: an XMM register zeroed with `xorpd` holds
   `+0.0`, which compares equal under `==` and is a different value under
   `signbit` and `copysign`. */
__attribute__((noinline)) static int neg_zero_keeps_its_sign(void)
{
    double d = -0.0;
    float  f = -0.0f;
    return __builtin_signbit(d) && __builtin_signbit(f)
        && !__builtin_signbit(0.0) && !__builtin_signbit(0.0f);
}

int main(void)
{
    double nan = __builtin_nan("");

    folds(opaque);
    folds(-opaque);
    folds(nan);

    if (!le_zero(0.0)) return 1;
    if (le_zero(1.0)) return 2;
    if (le_zero(nan)) return 3;

    if (!ge_zero(0.0)) return 4;
    if (!ge_zero(-1.0)) return 5;
    if (ge_zero(nan)) return 6;

    if (always_false()) return 7;
    if (big() != 3000000000L) return 8;
    if (overflows() != __builtin_inff()) return 9;
    if (by_zero(0.0) != __builtin_inf()) return 10;
    if (!neg_zero_keeps_its_sign()) return 11;

    return 0;
}
"#;
    // `link_error` is deliberately never defined, so a fold that does not
    // happen is a link failure. It has to happen for that to link at all,
    // which is why `-O0` is not in this list -- constant branches survive
    // there by decision.
    // `-lm` because `Fabs64` is lowered as a call to `fabs`, not as an
    // instruction: recognizing the name buys the optimizer visibility, not a
    // different code sequence.
    for opt in ["-O1", "-O2"] {
        assert_eq!(
            compile_and_run(
                "c17_float_constant_fold",
                code,
                &[opt.to_string(), "-lm".to_string()]
            ),
            0,
            "at {opt}"
        );
    }
}

/// Floating-point builtins and folds, one program run on the host at the
/// matrix levels, -O0 and -O2, and on aarch64 at -O0 and -O2.
///
/// Consolidates these tests, one section each (each original `main` is
/// a `noinline` section function; the program exits with the section's
/// base plus the original code):
/// - `codegen_expression_temporaries_do_not_grow_the_stack`: 1..=5
/// - `codegen_float_comparison_folds_agree_with_run_time`: 6..=8
/// - `codegen_stdio_library_builtins`: 9..=17
/// - `codegen_builtin_iseqsig`: 18..=30
#[test]
fn codegen_floating_everywhere_mega() {
    let code = r#"
/* ---- codegen_expression_temporaries_do_not_grow_the_stack: exits 1..5
 * A complex value, a complex cast, `__builtin_complex`, a `__sync` CAS and an
 * atomic floating-point read-modify-write each need a temporary in memory.
 * They were `alloca`s, which grow the stack on every evaluation and are
 * released only at return, so the same expression in a loop exhausted the
 * stack: two million iterations is far past 8 MB.
 *
 * The halves are read through the array representation C17 6.2.5p13
 * guarantees rather than `creal`/`conj`, which live in libm and would need a
 * `-lm` this harness does not pass.
 */
#include <complex.h>

volatile double v = 1.0;
volatile long word;
_Atomic double ad;

static double re(double complex c) { return ((double *)&c)[0]; }
static double im(double complex c) { return ((double *)&c)[1]; }

static __attribute__((noinline)) int t_expression_temporaries_do_not_grow_the_stack(void)
{
    double complex z = 0;
    double acc = 0;
    long n = 2000000;
    for (long i = 0; i < n; i++) {
        double complex a = v + v * I;
        z += a * a + (re(a) - im(a) * I);
        z -= (double complex)v;
        z += __builtin_complex(v, v);
        acc += re(a / (a + 1.0));
        acc += re(-a) + re(~a);
        __sync_val_compare_and_swap(&word, i, i + 1);
        ad += 1.0;
    }
    if (re(z) != (double)n) return 1;
    if (im(z) != 2.0 * n) return 2;
    if (word != n) return 3;
    if (ad != (double)n) return 4;
    if (acc < 0) return 5;
    return 0;
}

/* ---- codegen_float_comparison_folds_agree_with_run_time: exits 6..8
 * Every float comparison the optimizer decides without knowing its operands
 * -- against a NaN or an infinity, a value against itself, and `&&`/`||`/`!`
 * over comparisons of one pair -- must give the answer the unoptimized
 * program gives, for NaN, both infinities and both zeros, in `float`,
 * `double` and `long double`. The expected answers come from a rank table
 * in integer arithmetic, never from a float comparison the compiler could
 * fold the same wrong way; a fold that forgets a NaN is a wrong answer here.
 */
/* Every comparison c17 now folds must give the answer the unfolded program
   gives. The expected answers are computed in integer arithmetic from a
   rank table, never by a floating comparison the compiler could fold the
   same wrong way. */
enum { LT = 1, EQ = 2, GT = 4, UN = 8 };

/* NaN, +Inf, -Inf, +0, -0, 1, -1 -- rank -1 is unordered. */
static const int rank[] = { -1, 4, 0, 2, 2, 3, 1 };
#define NV 7

static int outcome(int i, int j)
{
    if (rank[i] < 0 || rank[j] < 0)
        return UN;
    return rank[i] < rank[j] ? LT : rank[i] == rank[j] ? EQ : GT;
}

#define T(e, m) do { int got = !!(e); int want = (o & (m)) != 0; \
    if (got != want) return k; k++; } while (0)
#define MIR(m) (((m) & (EQ | UN)) | (((m) & LT) ? GT : 0) | (((m) & GT) ? LT : 0))

/* The six predicates against one side, both ways round. */
#define SIX(x, c)                                                        \
    T(x < c, LT); T(x <= c, LT | EQ); T(x > c, GT); T(x >= c, GT | EQ);  \
    T(x == c, EQ); T(x != c, LT | GT | UN);                              \
    o = MIR(o);                                                          \
    T(c < x, LT); T(c <= x, LT | EQ); T(c > x, GT); T(c >= x, GT | EQ);  \
    T(c == x, EQ); T(c != x, LT | GT | UN);                              \
    o = MIR(o);

#define DEFINE(TY, SUF)                                                  \
static TY vals_##SUF[NV];                                                \
__attribute__((noinline)) static int consts_##SUF(TY x, int i)           \
{                                                                        \
    int k = 1, o;                                                        \
    o = outcome(i, 0); SIX(x, (TY)__builtin_nan(""));                    \
    o = outcome(i, 1); SIX(x, (TY)__builtin_inf());                      \
    o = outcome(i, 2); SIX(x, -(TY)__builtin_inf());                     \
    o = outcome(i, 3); SIX(x, (TY)0.0);                                  \
    o = outcome(i, 4); SIX(x, (TY)-0.0);                                 \
    o = outcome(i, 5); SIX(x, (TY)1.0);                                  \
    o = rank[i] < 0 ? UN : EQ;                                           \
    T(x < x, LT); T(x <= x, LT | EQ); T(x > x, GT); T(x >= x, GT | EQ);  \
    T(x == x, EQ); T(x != x, LT | GT | UN);                              \
    return 0;                                                            \
}                                                                        \
__attribute__((noinline)) static int pairs_##SUF(TY a, TY b, int o)      \
{                                                                        \
    int k = 100;                                                         \
    SIX(a, b);                                                           \
    T((a < b) && (a > b), 0);                                            \
    T((a == b) && (a != b), 0);                                          \
    T((a < b) && (b < a), 0);                                            \
    T((a == b) || (a != b), LT | EQ | GT | UN);                          \
    T(__builtin_isunordered(a, b) || a >= b || a < b, LT | EQ | GT | UN);\
    T(__builtin_isunordered(b, a) || a <= b || b < a, LT | EQ | GT | UN);\
    T(__builtin_isunordered(a, b) || !__builtin_isunordered(a, b),       \
      LT | EQ | GT | UN);                                                \
    T(__builtin_isunordered(a, b), UN);                                  \
    T(!__builtin_isunordered(b, a), LT | EQ | GT);                       \
    T((a < b) || (a >= b), LT | EQ | GT);                                \
    T((a <= b) && (a >= b), EQ);                                         \
    T((a < b) || (a == b), LT | EQ);                                     \
    T(!(a < b) && !(a > b), EQ | UN);                                    \
    T((a < b) || (a > b), LT | GT);                                      \
    T((a != b) && !__builtin_isunordered(a, b), LT | GT);                \
    T((a != b) && (a == b), 0);                                          \
    T(!(a >= b) || (a >= b), LT | EQ | GT | UN);                         \
    T(!(a >= b), LT | UN);                                               \
    T((a > b) ? 1 : (a <= b), LT | EQ | GT);                             \
    T((a < b) ? (b > a) : 0, LT);                                        \
    return 0;                                                            \
}                                                                        \
static int run_##SUF(void)                                               \
{                                                                        \
    vals_##SUF[0] = __builtin_nan("");                                   \
    vals_##SUF[1] = __builtin_inf();                                     \
    vals_##SUF[2] = -__builtin_inf();                                    \
    vals_##SUF[3] = 0.0;                                                 \
    vals_##SUF[4] = -0.0;                                                \
    vals_##SUF[5] = 1.0;                                                 \
    vals_##SUF[6] = -1.0;                                                \
    for (int i = 0; i < NV; i++) {                                       \
        int r = consts_##SUF(vals_##SUF[i], i);                          \
        if (r) return r * 16 + i;                                        \
        for (int j = 0; j < NV; j++) {                                   \
            r = pairs_##SUF(vals_##SUF[i], vals_##SUF[j], outcome(i, j));\
            if (r) return r * 256 + i * 16 + j;                          \
        }                                                                \
    }                                                                    \
    return 0;                                                            \
}

DEFINE(float, f)
DEFINE(double, d)
DEFINE(long double, l)

static __attribute__((noinline)) int t_float_comparison_folds_agree_with_run_time(void)
{
    int r;
    if ((r = run_f())) return 1;
    if ((r = run_d())) return 2;
    if ((r = run_l())) return 3;
    return 0;
}
#undef NV
#undef T
#undef MIR
#undef SIX
#undef DEFINE

/* ---- codegen_stdio_library_builtins: exits 9..17
 * `__builtin_fprintf`, `__builtin_fputs`, `__builtin_fputc` and
 * `__builtin_fwrite` are the library functions under gcc's reserved names
 * (execute/builtins/fprintf.c, fputs.c).
 */
/* No <stdio.h>: the builtins must not need their library declarations. */
typedef struct stdio_file FILE;
typedef __SIZE_TYPE__ size_t;
FILE *tmpfile(void);
FILE *fdopen(int, const char *);
void rewind(FILE *);
size_t fread(void *, size_t, size_t, FILE *);
int fflush(FILE *);
int fclose(FILE *);
int strcmp(const char *, const char *);

static __attribute__((noinline)) int t_stdio_library_builtins(void)
{
    char buf[64];
    FILE *f = tmpfile();
    if (!f) return 1;
    if (__builtin_fprintf(f, "%d-%s", 42, "ab") != 5) return 2;
    if (__builtin_fputs("xy", f) < 0) return 3;
    if (__builtin_fputc('!', f) != '!') return 4;
    if (__builtin_fwrite("123", 1, 3, f) != 3) return 5;
    rewind(f);
    size_t n = fread(buf, 1, sizeof buf - 1, f);
    buf[n] = 0;
    fclose(f);
    if (strcmp(buf, "42-abxy!123") != 0) return 6;
    FILE *out = fdopen(1, "w");
    if (!out) return 7;
    if (__builtin_fprintf(out, "out %d\n", 7) != 6) return 8;
    if (__builtin_fputs("done\n", out) < 0) return 9;
    fflush(out);
    return 0;
}

/* ---- codegen_builtin_iseqsig: exits 18..30
 * `__builtin_iseqsig` is C23's `iseqsig`: ordered and equal, so false for
 * any NaN, with the usual arithmetic conversions applied to mixed operands
 * and each operand evaluated once (compile/pr122588-1).
 */
static volatile double zero = 0.0;
static int eq_d(double a, double b) { return __builtin_iseqsig(a, b); }
static int eq_f(float a, float b) { return __builtin_iseqsig(a, b); }
static int eq_l(long double a, long double b) { return __builtin_iseqsig(a, b); }
static int calls;
static double next(double v) { calls++; return v; }
static __attribute__((noinline)) int t_builtin_iseqsig(void)
{
    double nan = zero / zero;
    float fnan = (float)nan;
    if (eq_d(1.5, 1.5) != 1) return 1;
    if (eq_d(1.5, 2.5) != 0) return 2;
    if (eq_d(nan, nan) != 0) return 3;
    if (eq_d(nan, 1.0) != 0) return 4;
    if (eq_d(0.0, -0.0) != 1) return 5;
    if (eq_f(2.0f, 2.0f) != 1) return 6;
    if (eq_f(fnan, 2.0f) != 0) return 7;
    if (eq_l(3.0L, 3.0L) != 1) return 8;
    if (eq_l(3.0L, (long double)nan) != 0) return 9;
    /* Mixed operands: float and double, int and double. */
    if (__builtin_iseqsig(0.1f, 0.1) != 0) return 10;
    if (__builtin_iseqsig(0.5f, 0.5) != 1) return 11;
    if (__builtin_iseqsig(3, 3.0) != 1) return 12;
    /* Each operand evaluated exactly once. */
    if (__builtin_iseqsig(next(1.0), next(1.0)) != 1 || calls != 2) return 13;
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_expression_temporaries_do_not_grow_the_stack()) != 0) return r;
    if ((r = t_float_comparison_folds_agree_with_run_time()) != 0) return 5 + r;
    if ((r = t_stdio_library_builtins()) != 0) return 8 + r;
    if ((r = t_builtin_iseqsig()) != 0) return 17 + r;
    return 0;
}
"#;
    compile_and_run_everywhere("floating_everywhere_mega", code);
}

/// The same folds as proofs: every `link_error` below is behind a comparison
/// that is false for every operand, NaN included -- gcc.c-torture's
/// `ieee/fp-cmp-6`, `-7`, `-9` and `compare-fp-3` in one program -- so it
/// links only when the optimizer deletes all of them. Not at -O0, where no
/// branch is folded.
#[test]
fn codegen_float_comparison_folds_remove_dead_calls() {
    for opt in ["-O1", "-O2"] {
        assert_eq!(
            compile_and_run("fcmp_fold_link", FCMP_FOLD_LINK, &[opt.to_string()]),
            0,
            "at {opt}"
        );
        if let Some(code) = compile_and_run_aarch64("fcmp_fold_link", FCMP_FOLD_LINK, opt) {
            assert_eq!(code, 0, "on aarch64 at {opt}");
        }
    }
}

// ============================================================================
// long double to integer on the x86-64 baseline
// ============================================================================

// Truncation under every rounding mode, and the mode left as it was found.
const LD_TO_INT_PROGRAM: &str = r#"
#include <fenv.h>
static int check(void) {
    volatile long double a = 2.9L, b = -2.9L, c = 0x1p62L + 0.5L, d = -0.99L, e = 65535.75L;
    if ((int)a != 2 || (int)b != -2) return 1;
    if ((long)c != 0x4000000000000000L) return 2;
    if ((long long)d != 0 || (unsigned short)e != 65535 || (short)b != -2) return 3;
    if ((unsigned)a != 2u || (unsigned long)c != 0x4000000000000000UL) return 4;
    return 0;
}
int main(void) {
    const int modes[] = { FE_TONEAREST, FE_UPWARD, FE_DOWNWARD, FE_TOWARDZERO };
    for (int i = 0; i < 4; i++) {
        fesetround(modes[i]);
        int r = check();
        if (r) return 10 * (i + 1) + r;
        /* The conversion must leave the rounding mode as it found it. */
        if (fegetround() != modes[i]) return 50 + i;
        volatile long double h = 0.5L;
        volatile long double one = 1.0L;
        long double s = h + one * 0x1p-64L;   /* an inexact add: rounds per mode */
        (void)s;
    }
    fesetround(FE_TONEAREST);
    return 0;
}
"#;

/// `fisttp` is SSE3, which the x86-64 baseline c17 targets does not include:
/// gcc converts a long double to an integer by switching the x87 control word
/// to truncation around a `fistp`, then restoring it. The value was always
/// right; the instruction raised SIGILL on a processor without SSE3.
#[test]
fn codegen_x86_64_long_double_to_integer_is_baseline() {
    if !cfg!(all(target_arch = "x86_64", target_os = "linux")) {
        return;
    }
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(
                &format!("ld_to_int{opt}"),
                LD_TO_INT_PROGRAM,
                &[opt.to_string(), "-lm".to_string()]
            ),
            0,
            "{opt}"
        );
    }
    // The assembly half, no `fisttp` at either level, is
    // `codegen_x86_64_long_double_to_integer_emits_no_fisttp` in
    // `cc/test_asm/codegen_floating.rs`.
}

/// A cast to `void` discards the value (C17 6.3.2.2) and converts nothing.
/// `(void)x` of a floating `x` was lowered as a conversion to an integer --
/// `cvttss2si` at `-O0` -- which raises `FE_INVALID` for a NaN, so
/// `(void)b;` in a function that ignores a NaN argument raised it.
#[test]
fn codegen_void_cast_of_a_float_converts_nothing() {
    let src = r#"
#include <fenv.h>
__attribute__((noinline)) static void ignore(float f, double d, long double l) {
    (void)f; (void)d; (void)l;
}
int main(void) {
    volatile float nan = __builtin_nanf("");
    volatile double big = 1e300;
    feclearexcept(FE_ALL_EXCEPT);
    ignore(nan, big, -big);
    return fetestexcept(FE_INVALID) ? 1 : 0;
}
"#;
    for level in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(
                "void_cast_float",
                src,
                &[level.to_string(), "-lm".to_string()]
            ),
            0,
            "{level}"
        );
        if let Some(rc) = crate::common::compile_and_run_aarch64_with(
            "void_cast_float_a64",
            src,
            &[level],
            &["-lm"],
        ) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}

/// A conversion to `_Bool` compares against zero; it is not a truncation.
///
/// C17 6.3.1.2p1: the result is 0 if the value compares equal to 0 and 1
/// otherwise, whatever the source type. Two separate paths got it wrong:
///
/// - `linearize_cast` has a conversion of its own and had no `_Bool` case, so
///   an explicit cast from a floating type fell into its float-to-integer arm
///   and `(_Bool)0.5` became a `cvttsd2si` -- 0, where every other compiler
///   says 1. `emit_convert` has the rule, and the cast path now defers to it.
/// - `emit_convert`'s own `_Bool` compare carried `_Bool` as the instruction
///   type, but a comparison must carry its *operand* type: that is how
///   `emit_compare` sizes it (see `linearize_emit`, where every other
///   comparison passes `operand_typ`). Sized at `_Bool`, it emitted `cmpl`
///   for a 64-bit operand, so `(_Bool)0x100000000L` read only the low half
///   and came out 0.
///
/// The static initializers are the control: `constexpr` folds those and was
/// right all along, so the two halves of the language disagreed.
#[test]
fn codegen_conversion_to_bool_compares_against_zero() {
    let code = r#"
static _Bool sd = 0.5;
static _Bool sl = 0x100000000L;

int main(void) {
    volatile double h = 0.5, nh = -0.5, tiny = 1e-300, zero = 0.0;
    volatile float fh = 0.5f;
    volatile long big = 0x100000000L, lzero = 0;
    volatile int i256 = 256;
    volatile char *p = (char *)1;

    /* A fraction is not zero, so it converts to 1. */
    if ((_Bool)h != 1) return 1;
    if ((_Bool)nh != 1) return 2;
    if ((_Bool)tiny != 1) return 3;
    if ((_Bool)fh != 1) return 4;
    /* A value whose low 32 bits are zero is still not zero. */
    if ((_Bool)big != 1) return 5;
    if ((_Bool)i256 != 1) return 6;
    if ((_Bool)p != 1) return 7;
    /* The implicit conversion takes the same rule. */
    _Bool b = big;
    if (b != 1) return 8;
    /* Zero is still zero, by either path. */
    if ((_Bool)zero != 0) return 9;
    if ((_Bool)lzero != 0) return 10;
    /* And the constant-folded initializers agree. */
    if (sd != 1 || sl != 1) return 11;
    return 0;
}
"#;
    for opt in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run("bool_convert", code, &[opt.to_string()]),
            0,
            "{opt}"
        );
    }
}
