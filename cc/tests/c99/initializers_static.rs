//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 initializers: constant folding in static initializers
//

use crate::common::{compile_and_run, compile_and_run_aarch64};

// ============================================================================
// Mega-test: constant folding in global initializers
// ============================================================================

// Original test documentation, in section order:
//
// ---- c99_initializers_ptr_arithmetic_global ----
// ============================================================================
// BUG 1: Pointer arithmetic in global initializers + hard error for unknown exprs
// ============================================================================

// ---- c99_global_initializer_folds_floating_arithmetic ----
// ============================================================================
// Constant folding in global initializers
// ============================================================================

// Arithmetic on floating constants must fold at file scope.
//
// C99 6.6p8 allows any arithmetic constant expression as the initializer of
// an object with static storage duration. c17 folded `*` and `/` correctly
// but not `+` or `-`: those two were intercepted by the *pointer*-arithmetic
// arm, which fell back to an integer-only evaluator and then rejected the
// program. `double a = 1.0 + 2.0;` did not compile.
//
// Worse, `-(1.0 + 2.0)` was accepted and silently became `0.0` -- the
// negation arm returned "no initializer" without a diagnostic, which lands
// the object in `.bss`. A wrong answer with a zero exit status.
// ---- c99_global_initializer_folds_pointer_arithmetic ----
// Pointer arithmetic in a global initializer must keep working.
//
// The float fix touches the arm that handles it, so this pins the behaviour
// the arm was written for: a symbol address plus a scaled constant offset.
// ---- c99_decimal_literals_reach_long_double_precision ----
// ============================================================================
// Decimal floating literals at the target's precision
// ============================================================================

// A decimal literal must reach `long double` at full width.
//
// Decimal literals went through `f64::from_str`, so a `long double` was
// correct only to 53 of its 64 significand bits, and one outside double's
// range collapsed entirely -- `LDBL_MAX` written in decimal became `inf` and
// `1e-4900L` became zero. Hex literals were already exact, which is what the
// comparisons below use as the reference: every right-hand side is a hex
// literal naming the value gcc produces for the decimal on its left.
//
// `float` and `double` were never affected: `f64::from_str` is correctly
// rounded, and for those two the target format is `f64` or narrower.
//
// Each reference is spelled for the format the target has. A decimal
// correctly rounded to binary128's 113 bits is *not* the same value as one
// correctly rounded to x87's 64, so one hex spelling cannot serve both --
// which is the whole claim being made here, that the literal reaches the
// width the target actually has. Both sets were taken from gcc.
// ---- c99_address_of_a_static_compound_literal ----
// The address of a compound literal with static storage duration.
//
// C99 6.5.2.5p5 gives a compound literal at file scope static storage
// duration, so its address is a constant expression and may initialize a
// pointer. c17 dropped the initializer silently: the object went to `.bss`,
// the pointer was null, and the program segfaulted on first use.
//
// The address-of arm returned "no initializer" when it could not evaluate the
// operand, which is the same silent-zero shape that made `-(1.0 + 2.0)` come
// out as `0.0`. The other arms were fixed then; this one was missed because
// no test reached it.
// ---- c99_scalar_initializers_take_the_objects_type ----
// A static initializer is converted to the *object's* type, not kept in the
// constant's own.
//
// Folding floating constants in global initializers made `int c = 1.0 + 2.0;`
// compile, which it should -- but it stored the IEEE bits of 3.0 into a
// 4-byte integer, so `c` read back as 1077936128. The mirror case,
// `double d = 1 + 2;`, stored the integer 3 into a double and read back as
// 1.5e-323. C17 6.7.9p11 converts the initializer as an assignment would.
// ---- c99_offsetof_by_null_pointer_is_a_constant ----
// The pre-<stddef.h> spelling of offsetof is an integer constant expression.
//
// `(size_t)&((struct S *)0)->member` dereferences nothing -- it is arithmetic
// on a null pointer -- so there is no symbol to relocate against and the
// whole thing folds to an integer. It used to fold to nothing: the static
// initializer silently became zero, and after global initializers started
// rejecting what they could not fold, it became a hard error instead.
// ---- c99_global_initializer_folds_non_integer_conditions ----
// A conditional in a static initializer is folded rather than emitted, so its
// condition is decided at compile time -- and that test used to be
// integer-only, which rejected every constant condition that is not an
// integer.
//
/// Static initializers are folded at compile time: pointer, floating,
/// conditional and offsetof-style constant expressions, converted to the
/// object's type.
///
/// Consolidates (one C section each, a `t_<name>` function):
/// - `c99_initializers_ptr_arithmetic_global`
/// - `c99_global_initializer_folds_floating_arithmetic`
/// - `c99_global_initializer_folds_pointer_arithmetic`
/// - `c99_decimal_literals_reach_long_double_precision`
/// - `c99_address_of_a_static_compound_literal`
/// - `c99_scalar_initializers_take_the_objects_type`
/// - `c99_offsetof_by_null_pointer_is_a_constant`
/// - `c99_global_initializer_folds_non_integer_conditions`
///
/// Exit codes: see the map at the top of the program.
#[test]
fn c99_initializers_static_folding_mega() {
    let code = r#"
/* Exit-code map: each section returns its original code, offset by
   the base listed in its banner.
       1..  6  c99_initializers_ptr_arithmetic_global
       7.. 19  c99_global_initializer_folds_floating_arithmetic
      20.. 25  c99_global_initializer_folds_pointer_arithmetic
      26.. 41  c99_decimal_literals_reach_long_double_precision
      42.. 48  c99_address_of_a_static_compound_literal
      49.. 65  c99_scalar_initializers_take_the_objects_type
      66.. 73  c99_offsetof_by_null_pointer_is_a_constant
      74.. 82  c99_global_initializer_folds_non_integer_conditions
*/

/* ==== c99_initializers_ptr_arithmetic_global: exit codes 1..6 (original code + 0) ==== */
int pa_arr[10] = {0,1,2,3,4,5,6,7,8,9};
int *pa_p1 = pa_arr + 5;
int *pa_p2 = &pa_arr[0] + 3;
int *pa_p3 = pa_arr + 0;

struct pa_S { int x; int y; int z; };
struct pa_S pa_global_s = {10, 20, 30};
int *pa_sp = (int*)&pa_global_s + 1;

// Pointer to middle of array
int *pa_mid = &pa_arr[4];

static __attribute__((noinline)) int t_c99_initializers_ptr_arithmetic_global(void) {
    if (*pa_p1 != 5) return 1;
    if (*pa_p2 != 3) return 2;
    if (*pa_p3 != 0) return 3;
    if (*pa_sp != 20) return 4;
    if (*pa_mid != 4) return 5;

    // Pointer subtraction in arithmetic
    int *p4 = pa_arr + 9;
    if (*p4 != 9) return 6;

    return 0;
}

/* ==== c99_global_initializer_folds_floating_arithmetic: exit codes 7..19 (original code + 6) ==== */
#include <math.h>

double ff_d_add = 1.0 + 2.0;
double ff_d_sub = 5.0 - 2.0;
double ff_d_mul = 3.0 * 2.0;
double ff_d_div = 12.0 / 4.0;
double ff_d_mixed = 1.0 + 2;            /* int operand promotes */
float  ff_f_add = 1.5f + 1.5f;
long double ff_l_add = 1.0L + 2.0L;
double ff_d_nested = (1.0 + 2.0) * 3.0 - 6.0;

/* The silent-zero cases: a negated constant expression. */
double ff_d_neg_sum = -(1.0 + 2.0);
double ff_d_neg_mul = -(2.0 * 1.5);
double ff_d_neg_lit = -3.0;

/* Integer folding must keep working unchanged. */
int ff_i_add = 1 + 2;
int ff_i_sub = 5 - 2;

static __attribute__((noinline)) int t_c99_global_initializer_folds_floating_arithmetic(void)
{
    if (ff_d_add != 3.0) return 1;
    if (ff_d_sub != 3.0) return 2;
    if (ff_d_mul != 6.0) return 3;
    if (ff_d_div != 3.0) return 4;
    if (ff_d_mixed != 3.0) return 5;
    if (ff_f_add != 3.0f) return 6;
    if (ff_l_add != 3.0L) return 7;
    if (ff_d_nested != 3.0) return 8;

    if (ff_d_neg_sum != -3.0) return 9;
    if (ff_d_neg_mul != -3.0) return 10;
    if (ff_d_neg_lit != -3.0) return 11;

    if (ff_i_add != 3) return 12;
    if (ff_i_sub != 3) return 13;

    return 0;
}

/* ==== c99_global_initializer_folds_pointer_arithmetic: exit codes 20..25 (original code + 19) ==== */
#include <string.h>
static int pf_arr[10] = {0,1,2,3,4,5,6,7,8,9};
int *pf_p_fwd = pf_arr + 3;
int *pf_p_back = &pf_arr[7] - 2;
int *pf_p_zero = pf_arr + 0;

/* A string literal has a static address too, but it only acquires a label
   when it is interned -- which the address evaluator could not do, so this
   was rejected while `arr + 1` was accepted. */
const char *pf_s_off = "hello" + 1;
const char *pf_s_end = "world" + 5;

static __attribute__((noinline)) int t_c99_global_initializer_folds_pointer_arithmetic(void)
{
    if (*pf_p_fwd != 3) return 1;
    if (*pf_p_back != 5) return 2;
    if (*pf_p_zero != 0) return 3;
    if (pf_p_fwd - pf_arr != 3) return 4;
    if (strcmp(pf_s_off, "ello") != 0) return 5;
    if (*pf_s_end != '\0') return 6;
    return 0;
}

/* ==== c99_decimal_literals_reach_long_double_precision: exit codes 26..41 (original code + 25) ==== */
#include <float.h>

#if LDBL_MANT_DIG == 113
#define dl_PI_REF    0x1.921fb54442d18469834ef156fa8fp+1L
#define dl_TENTH_REF 0x1.999999999999999999999999999ap-4L
#define dl_BIG_REF   0x1.fffffffffffffffdf5f7837da5b2p+16383L
#define dl_TINY_REF  0x1.7769bead75ec52e4d25544b1042ep-16278L
#else
/* x87's 64 bits. A target whose long double is double rounds both sides of
   each comparison to the same value, so these serve there too. */
#define dl_PI_REF    0xc.90fdaa22168c235p-2L
#define dl_TENTH_REF 0xc.ccccccccccccccdp-7L
#define dl_BIG_REF   0xf.fffffffffffffffp+16380L
#define dl_TINY_REF  0xb.bb4df56baf62972p-16281L
#endif

static __attribute__((noinline)) int t_c99_decimal_literals_reach_long_double_precision(void)
{
    /* Needs every significand bit the format has: the tail is lost at 53. */
    if (3.14159265358979323846L != dl_PI_REF) return 1;

    /* A value with no exact binary form, rounded at the wrong width. */
    if (0.1L != dl_TENTH_REF) return 2;

    /* Outside double's range in both directions. */
    if (1.18973149535723176502e+4932L != dl_BIG_REF) return 3;
    if (1e-4900L != dl_TINY_REF) return 4;

    /* Ordinary magnitudes must stay exact, not merely close. */
    if (1.0L != 0x1p+0L) return 5;
    if (0.5L != 0x1p-1L) return 6;
    if (1e10L != 0x2.540be4p+32L) return 7;
    if (123456789.0L != 0x7.5bcd15p+24L) return 8;

    /* Zero, and a value that underflows to it, keep their sign. */
    if (0.0L != 0x0p+0L) return 9;
    if (1e-5000L != 0.0L) return 10;

    /* float and double are unchanged. */
    if (0.1 != 0x1.999999999999ap-4) return 11;
    if (0.1f != 0x1.99999ap-4f) return 12;
    if (DBL_MAX <= 0.0 || LDBL_MAX <= 0.0) return 13;

    /* The exponent forms all agree. */
    if (1.5e3L != 1500.0L) return 14;
    if (15e2L != 1500.0L) return 15;
    if (150000e-2L != 1500.0L) return 16;

    return 0;
}

/* ==== c99_address_of_a_static_compound_literal: exit codes 42..48 (original code + 41) ==== */
#include <stdio.h>
#include <string.h>

struct sl_P { int x, y; };

static struct sl_P *sl_gp = &(struct sl_P){1, 2};
static int *sl_ga = (int[]){10, 20, 30};
static const char *sl_gs = (const char[]){'h', 'i', 0};

static __attribute__((noinline)) int t_c99_address_of_a_static_compound_literal(void)
{
    if (sl_gp->x != 1 || sl_gp->y != 2) return 1;
    if (sl_ga[0] != 10 || sl_ga[1] != 20 || sl_ga[2] != 30) return 2;
    if (strcmp(sl_gs, "hi") != 0) return 3;

    /* Writing through it must work: it is an object, not a constant. */
    sl_gp->x = 42;
    if (sl_gp->x != 42) return 5;

    /* The block-scope forms already worked and must keep working. */
    struct sl_P *lp = &(struct sl_P){7, 8};
    if (lp->x != 7 || lp->y != 8) return 6;
    int *la = (int[]){4, 5};
    if (la[0] != 4 || la[1] != 5) return 7;

    return 0;
}

/* ==== c99_scalar_initializers_take_the_objects_type: exit codes 49..65 (original code + 48) ==== */
#include <limits.h>

/* Floating constants initializing integer objects: the fraction is
   discarded (C17 6.3.1.4), it is not reinterpreted. */
int st_i_add = 1.0 + 2.0;
int st_i_sub = 9.5 - 2.0;
int st_i_mul = 2.5 * 2.0;
int st_i_div = 7 / 2.0;
int st_i_neg = -(1.5 + 2.0);
long st_l_wide = 2.9 * 2.0;
char st_c_narrow = 65.9;
_Bool st_b_from_float = 0.5;
short st_s_neg = -3.9;

/* Integer constants initializing floating objects: widened exactly. */
double st_d_add = 1 + 2;
float st_f_int = 7;
long double st_ld_int = -5;
double st_d_big = 2147483647;

/* And the same-type cases must not have moved. */
double st_d_add_f = 1.0 + 2.0;
int st_i_add_i = 1 + 2;

struct st_S { int i; double d; };
struct st_S st_agg = { 1.0 + 2.0, 1 + 2 };
int st_arr[3] = { 1.5, 2.5, 3.5 };

static __attribute__((noinline)) int t_c99_scalar_initializers_take_the_objects_type(void)
{
    if (st_i_add != 3) return 1;
    if (st_i_sub != 7) return 2;
    if (st_i_mul != 5) return 3;
    if (st_i_div != 3) return 4;
    if (st_i_neg != -3) return 5;
    if (st_l_wide != 5) return 6;
    if (st_c_narrow != 'A') return 7;
    if (st_b_from_float != 1) return 8;
    if (st_s_neg != -3) return 9;

    if (st_d_add != 3.0) return 10;
    if (st_f_int != 7.0f) return 11;
    if (st_ld_int != -5.0L) return 12;
    if (st_d_big != 2147483647.0) return 13;

    if (st_d_add_f != 3.0) return 14;
    if (st_i_add_i != 3) return 15;

    if (st_agg.i != 3 || st_agg.d != 3.0) return 16;
    if (st_arr[0] != 1 || st_arr[1] != 2 || st_arr[2] != 3) return 17;

    return 0;
}

/* ==== c99_offsetof_by_null_pointer_is_a_constant: exit codes 66..73 (original code + 65) ==== */
#include <stddef.h>

struct on_S { int a; double b; char c[8]; struct { int x, y; } n; };

static const size_t on_off_a = (size_t)&((struct on_S *)0)->a;
static const size_t on_off_b = (size_t)&((struct on_S *)0)->b;
static const size_t on_off_c = (size_t)&((struct on_S *)0)->c;
static const size_t on_off_c2 = (size_t)&((struct on_S *)0)->c[2];
static const size_t on_off_ny = (size_t)&((struct on_S *)0)->n.y;

/* An integer constant expression is usable as an array bound and a case
   label, not only as an initializer. */
static char on_sized[(size_t)&((struct on_S *)0)->b];

int on_pick(int k)
{
    switch (k) {
    case (int)(size_t)&((struct on_S *)0)->b: return 1;
    default: return 0;
    }
}

static __attribute__((noinline)) int t_c99_offsetof_by_null_pointer_is_a_constant(void)
{
    if (on_off_a != offsetof(struct on_S, a)) return 1;
    if (on_off_b != offsetof(struct on_S, b)) return 2;
    if (on_off_c != offsetof(struct on_S, c)) return 3;
    if (on_off_c2 != offsetof(struct on_S, c) + 2) return 4;
    if (on_off_ny != offsetof(struct on_S, n.y)) return 5;
    if (sizeof(on_sized) != offsetof(struct on_S, b)) return 6;
    if (on_pick((int)offsetof(struct on_S, b)) != 1) return 7;

    /* The address of a real object is still a relocation, not an integer. */
    static int obj[4];
    static int *p = &obj[2];
    if (p != &obj[2]) return 8;

    return 0;
}

/* ==== c99_global_initializer_folds_non_integer_conditions: exit codes 74..82 (original code + 73) ==== */
int nc_arr[4];
int nc_fn(void) { return 9; }
int nc_obj;

/* A string literal has storage of its own, so its address is never null. */
static const char *nc_g = "a" ?: "b";
/* A floating constant is a constant condition too, and 1.5 is not zero. */
static double nc_d = 1.5 ?: 2.5;
static double nc_dz = 0.0 ? 1.5 : 2.5;
/* An array and a function decay to addresses, which are never null. */
static int *nc_pa = nc_arr ?: 0;
static int (*nc_pf)(void) = nc_fn ?: 0;
static int *nc_po = &nc_obj ?: 0;
/* The integer path that already worked, kept as the control. */
static int nc_i = 0 ?: 7;

static __attribute__((noinline)) int t_c99_global_initializer_folds_non_integer_conditions(void) {
    if (nc_g[0] != 'a') return 1;
    if (nc_d != 1.5) return 2;
    if (nc_dz != 2.5) return 3;
    if (nc_pa != nc_arr) return 4;
    if (nc_pf != nc_fn) return 5;
    if (nc_po != &nc_obj) return 6;
    if (nc_i != 7) return 7;
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_c99_initializers_ptr_arithmetic_global()) != 0) return 0 + r;
    if ((r = t_c99_global_initializer_folds_floating_arithmetic()) != 0) return 6 + r;
    if ((r = t_c99_global_initializer_folds_pointer_arithmetic()) != 0) return 19 + r;
    if ((r = t_c99_decimal_literals_reach_long_double_precision()) != 0) return 25 + r;
    if ((r = t_c99_address_of_a_static_compound_literal()) != 0) return 41 + r;
    if ((r = t_c99_scalar_initializers_take_the_objects_type()) != 0) return 48 + r;
    if ((r = t_c99_offsetof_by_null_pointer_is_a_constant()) != 0) return 65 + r;
    if ((r = t_c99_global_initializer_folds_non_integer_conditions()) != 0) return 73 + r;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_initializers_static_folding_mega", code, &[]),
        0
    );
}

/// A `_Complex` object with static storage duration can be initialized.
///
/// Every spelling was rejected before: `__builtin_complex` had no arm at all,
/// and `1.0 + 2.0*I` reached the arithmetic arm, which had no notion of a
/// complex value. Only the function-local path worked.
///
/// Note `double _Complex z = {1.0, 2.0};` is *not* 1.0 + 2.0i. A complex type
/// is a scalar type (C11 6.2.5p21), so that is a braced scalar initializer
/// with an excess element; gcc warns and keeps only the first. Matching gcc
/// here means the imaginary part stays zero.
#[test]
fn c99_global_initializer_accepts_complex() {
    let src = r#"
#include <complex.h>
#include <stdio.h>

double _Complex z_builtin = __builtin_complex(1.0, 2.0);
double _Complex z_cmplx   = CMPLX(1.0, 2.0);
double _Complex z_imag    = 1.0 + 2.0*I;
double _Complex z_real    = 3.0;               /* real constant, zero imag */
double _Complex z_neg     = -(1.0 + 2.0*I);
double _Complex z_mul     = (1.0 + 2.0*I) * (3.0 + 4.0*I);   /* -5 + 10i */
double _Complex z_brace   = {1.0};             /* braced scalar */
float _Complex  f_imag    = 1.5f + 2.5f*I;     /* narrower base type */

static double _Complex s_imag = 1.0 + 2.0*I;   /* internal linkage */

int main(void)
{
    if (creal(z_builtin) != 1.0 || cimag(z_builtin) != 2.0) return 1;
    if (creal(z_cmplx) != 1.0 || cimag(z_cmplx) != 2.0) return 2;
    if (creal(z_imag) != 1.0 || cimag(z_imag) != 2.0) return 3;
    if (creal(z_real) != 3.0 || cimag(z_real) != 0.0) return 4;
    if (creal(z_neg) != -1.0 || cimag(z_neg) != -2.0) return 5;
    if (creal(z_mul) != -5.0 || cimag(z_mul) != 10.0) return 6;
    if (creal(z_brace) != 1.0 || cimag(z_brace) != 0.0) return 7;
    if (crealf(f_imag) != 1.5f || cimagf(f_imag) != 2.5f) return 8;
    if (creal(s_imag) != 1.0 || cimag(s_imag) != 2.0) return 9;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("global_init_complex", src, &["-lm".to_string()]),
        0
    );
}

// ============================================================================
// Mega-test: long double and complex static initializers, host and aarch64
// ============================================================================

// Original test documentation, in section order:
//
// ---- c99_long_double_static_initializers_keep_their_precision ----
// A static initializer is evaluated at the precision of its operands' own
// type. A `long double` constant carries 64 significand bits on x86-64 and
// 113 on aarch64 Linux, so each value below is exact in both -- and was read
// off gcc on both. They came out rounded to `double`: the float-to-integer
// conversion and the complex arithmetic both went through `f64`. Apple
// arm64's `long double` is `double`, where each sum rounds to its leading
// term, so what is expected is the exact value in a wide `long double` and
// that rounding in a `double` one.
// ---- c99_long_double_static_initializers_match_run_time_evaluation ----
// The same expressions as above, and a few more, evaluated at run time from
// `volatile` operands: a static initializer is folded by the compiler and an
// automatic one computed by the program, and the two must agree, bit for
// bit, on both targets. Complex `*` and `/` run through libgcc's `__mul?c3`
// and `__div?c3`, which is what the fold models.
// ---- c99_complex_constants_in_scalar_static_initializers ----
// Complex constants in the scalar shapes a static initializer can take. Each
// value is gcc's, on both targets. `(_Complex float)(0.5) == 0.5` is
// gcc.c-torture compile/pr30433.
//
/// `long double` and complex static initializers keep the target's
/// precision and agree with run-time evaluation, on both targets.
///
/// Consolidates (one C section each, a `t_<name>` function):
/// - `c99_long_double_static_initializers_keep_their_precision`
/// - `c99_long_double_static_initializers_match_run_time_evaluation`
/// - `c99_complex_constants_in_scalar_static_initializers`
///
/// Exit codes: see the map at the top of the program.
#[test]
fn c99_initializers_long_double_and_complex_static_mega() {
    let code = r#"
/* Exit-code map: each section returns its original code, offset by
   the base listed in its banner.
       1..  7  LONG_DOUBLE_STATIC_INIT_PROGRAM
       8.. 18  LONG_DOUBLE_FOLD_MATCHES_RUNTIME_PROGRAM
      19.. 26  COMPLEX_SCALAR_STATIC_INIT_PROGRAM
*/

/* ==== LONG_DOUBLE_STATIC_INIT_PROGRAM: exit codes 1..7 (original code + 0) ==== */
static long long lp_a = 0x1p62L + 1.0L;
static unsigned long long lp_b = 0x1p63L + 3.0L;
static long long lp_c = -0x1p62L - 5.0L;
static long double _Complex lp_z = (1.0L + 0x1p-60L) + 2.0iL;
static long double _Complex lp_w = (1.0L + 0x1p-60L) * (1.0L + 1.0iL);
static long double _Complex lp_q = (2.0L + 0x1p-59L) / 2.0L;
static long double _Complex lp_s = (1.0L + 1.0iL) - 0x1p-60L;
#define lp_WIDE (__LDBL_MANT_DIG__ > 53)
#define lp_T60 (lp_WIDE ? 0x1p-60L : 0.0L)
static __attribute__((noinline)) int t_c99_long_double_static_initializers_keep_their_precision(void) {
    if (lp_a != 4611686018427387904LL + lp_WIDE) return 1;
    if (lp_b != 9223372036854775808ULL + 3 * lp_WIDE) return 2;
    if (lp_c != -4611686018427387904LL - 5 * lp_WIDE) return 3;
    if (__real__ lp_z - 1.0L != lp_T60 || __imag__ lp_z != 2.0L) return 4;
    if (__real__ lp_w - 1.0L != lp_T60 || __imag__ lp_w - 1.0L != lp_T60) return 5;
    if (__real__ lp_q - 1.0L != lp_T60 || __imag__ lp_q != 0.0L) return 6;
    if (1.0L - __real__ lp_s != lp_T60 || __imag__ lp_s != 1.0L) return 7;
    return 0;
}

/* ==== LONG_DOUBLE_FOLD_MATCHES_RUNTIME_PROGRAM: exit codes 8..18 (original code + 7) ==== */
static long long lr_a = 0x1p62L + 1.0L;
static unsigned long long lr_b = 0x1p63L + 3.0L;
static long long lr_c = -0x1p62L - 5.0L;
static long double _Complex lr_z = (1.0L + 0x1p-60L) + 2.0iL;
static long double _Complex lr_w = (1.0L + 0x1p-60L) * (1.0L + 1.0iL);
static long double _Complex lr_q = (2.0L + 0x1p-59L) / 2.0L;
static long double _Complex lr_s = (1.0L + 1.0iL) - 0x1p-60L;
static long double _Complex lr_r = (1.0L + 0x1p-60L + 1.0iL) / (1.0L + 1.0iL);
static long double _Complex lr_m = (3.0L + 0x1p-58L + 5.0iL) * (7.0L - 0x1p-57iL);
static long double _Complex lr_d = (3.0L + 0x1p-58L + 5.0iL) / (7.0L - 2.0iL);
static _Complex int lr_iq = (-9 + 38i) / (5 + 6i);

/* The halves compared as C compares them, and their signs as well. */
static int lr_same(long double _Complex x, long double _Complex y) {
    return __real__ x == __real__ y && __imag__ x == __imag__ y
        && __builtin_signbit(__real__ x) == __builtin_signbit(__real__ y)
        && __builtin_signbit(__imag__ x) == __builtin_signbit(__imag__ y);
}

static __attribute__((noinline)) int t_c99_long_double_static_initializers_match_run_time_evaluation(void) {
    volatile long double one = 1.0L, two = 2.0L, three = 3.0L, seven = 7.0L;
    volatile long double p62 = 0x1p62L, p63 = 0x1p63L, t58 = 0x1p-58L, t60 = 0x1p-60L;
    volatile long double t59 = 0x1p-59L, t57 = 0x1p-57L;
    volatile long double _Complex i = 1.0iL;

    long long ra = p62 + one;
    unsigned long long rb = p63 + 3.0L;
    long long rc = -p62 - 5.0L;
    if (lr_a != ra) return 1;
    if (lr_b != rb) return 2;
    if (lr_c != rc) return 3;

    if (!lr_same(lr_z, (one + t60) + two * i)) return 4;
    if (!lr_same(lr_w, (one + t60) * (one + i))) return 5;
    if (!lr_same(lr_q, (two + t59) / (long double _Complex)two)) return 6;
    if (!lr_same(lr_s, (one + i) - t60)) return 7;
    if (!lr_same(lr_r, (one + t60 + i) / (one + i))) return 8;
    if (!lr_same(lr_m, (three + t58 + 5.0L * i) * (seven - t57 * i))) return 9;
    /* Apple's `__divdc3` is compiler-rt's, which scales by `logb` where
       libgcc's divides by Smith's method; c17 models both and folds as the
       target's own routine computes, so the two agree here too. */
    if (!lr_same(lr_d, (three + t58 + 5.0L * i) / (seven - two * i))) return 10;

    volatile _Complex int n = -9 + 38i, e = 5 + 6i;
    _Complex int rq = n / e;
    if (__real__ lr_iq != __real__ rq || __imag__ lr_iq != __imag__ rq) return 11;
    return 0;
}

/* ==== COMPLEX_SCALAR_STATIC_INIT_PROGRAM: exit codes 19..26 (original code + 18) ==== */
int cs_f = (_Complex float)(0.5) == 0.5;
int cs_f2 = (1.0 + 2.0i) != (1.0 + 2.0i);
int cs_f3 = (1.0f + 2.0fi) == (1.0L + 2.0iL);
int cs_f4 = 3 == (3 + 0i);
int cs_f5 = (0.1f + 0i) == 0.1;
int cs_f6 = __builtin_complex(__builtin_nan(""), 0.0) == __builtin_complex(__builtin_nan(""), 0.0);
double cs_r1 = __real__ (1.5 + 2.5i);
double cs_r2 = __imag__ (1.5 + 2.5i);
int cs_r3 = __real__ (3 + 4i);
int cs_r4 = __imag__ (3 + 4i);
double cs_r5 = __imag__ 2.5;
int cs_n1 = !(0.0 + 0.0i);
int cs_n2 = !(0.0 + 1.0i);
int cs_n3 = !0.5;
int cs_l1 = (0.0 + 1.0i) && 1;
int cs_l2 = (0.0 + 0.0i) || 0.5;
int cs_c1 = (0.0 + 1.0i) ? 7 : 8;
int cs_c2 = (0.0 + 0.0i) ? 7 : 8;
double cs_k1 = (double)(1.5 + 2.5i);
int cs_k2 = (int)(3.75 + 2.5i);
_Bool cs_k3 = (_Bool)(0.0 + 1.0i);
_Bool cs_k4 = (_Bool)(0.0 + 0.0i);
double cs_k5 = 1.25 + 2.0i;
int cs_k6 = 3.75 + 2.5i;
float cs_k8 = (float)(0x1p-30L + 1.0L + 1.0iL);
long long cs_k9 = (long long)(0x1p62L + 1.0L + 1.0iL);
int cs_k10 = (int)(5 + 6i);
double cs_k11 = (double)(5 + 6i);
_Complex double cs_k12 = (_Complex double)(3 + 4i);
double cs_q1 = (1 ? 2.5 : 3) * 2;
_Complex double cs_q2 = 0 ? 1.0 : (2.0 + 3.0i);

static __attribute__((noinline)) int t_c99_complex_constants_in_scalar_static_initializers(void) {
    if (cs_f != 1 || cs_f2 != 0 || cs_f3 != 1 || cs_f4 != 1 || cs_f5 != 0 || cs_f6 != 0) return 1;
    if (cs_r1 != 1.5 || cs_r2 != 2.5 || cs_r3 != 3 || cs_r4 != 4 || cs_r5 != 0.0) return 2;
    if (cs_n1 != 1 || cs_n2 != 0 || cs_n3 != 0 || cs_l1 != 1 || cs_l2 != 1) return 3;
    if (cs_c1 != 7 || cs_c2 != 8) return 4;
    if (cs_k1 != 1.5 || cs_k2 != 3 || cs_k3 != 1 || cs_k4 != 0 || cs_k5 != 1.25 || cs_k6 != 3) return 5;
    /* 2^62 + 1 is exact in a wide long double, and 2^62 in Apple's. */
    if (cs_k8 != 1.0f || cs_k9 != 4611686018427387904LL + (__LDBL_MANT_DIG__ > 53)) return 6;
    if (cs_k10 != 5 || cs_k11 != 5.0) return 6;
    if (__real__ cs_k12 != 3.0 || __imag__ cs_k12 != 4.0) return 7;
    if (cs_q1 != 5.0 || __real__ cs_q2 != 2.0 || __imag__ cs_q2 != 3.0) return 8;
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_c99_long_double_static_initializers_keep_their_precision()) != 0) return 0 + r;
    if ((r = t_c99_long_double_static_initializers_match_run_time_evaluation()) != 0) return 7 + r;
    if ((r = t_c99_complex_constants_in_scalar_static_initializers()) != 0) return 18 + r;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_initializers_ld_complex_static_mega", code, &[]),
        0
    );
    if let Some(rc) =
        compile_and_run_aarch64("c99_initializers_ld_complex_static_mega_a64", code, "-O0")
    {
        assert_eq!(rc, 0);
    }
}
