//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Variadic functions: va_arg of every class, the register save area,
// and many-argument calls. The assembly check of the largest admitted frame
// compiles in process, in `cc/test_asm/codegen_varargs.rs`.
//

// Used only by the assembly assertion below, which is linux-x86-64 only.
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
use super::floating::OVER_ALIGNED_LONG_DOUBLE;
use crate::common::{
    compile_and_run, compile_and_run_aarch64, compile_and_run_everywhere,
    compile_and_run_optimized, compile_with_host_cc,
};

// ============================================================================
// Variadic functions with 64-bit args — exercises VaArg .with_size() for
// long and pointer types. Values above 2^32 prove no 32-bit truncation.
// ============================================================================

/// One section per consolidated test, each under that test's own
/// documentation. Exit codes:
/// `codegen_variadic_64bit_args` 11..=41.
/// `codegen_variadic_narrow_integer_promotion` 51..=62.
/// `codegen_va_start_stack_overflow_params` 71..=73.
/// `codegen_stacked_sixteen_byte_argument_lands_on_its_boundary` 81..=86.
const VARIADIC_AND_STACKED_SCALAR_ARGUMENTS: &str = r#"
/* ====================================================================== */
/* codegen_variadic_64bit_args: exit codes 11..41 */
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

static int t_codegen_variadic_64bit_args(void) {
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

/* ====================================================================== */
/* codegen_variadic_narrow_integer_promotion: exit codes 51..62 */
// Regression test: narrow *integer* arguments to variadic functions were not
// promoted to int per C99 6.5.2.2p7, so `printf("%02x", (unsigned char)c)`
// with a negative `signed char` printed `ffffff80` where gcc prints `80`
// (audit #C5).
//
// The formal-parameter conversion in the linearizer is guarded by
// `arg_idx < params.len()`, which is never true for a variadic argument, so
// nothing promoted these. The cast alone emits no IR either, because
// `emit_convert` short-circuits same-size integer conversions -- leaving the
// sign-extended value from the load in place.
//
// Every case is checked in both directions (cast and bare) so the test cannot
// pass by promoting too eagerly, and the sibling float test above cannot catch
// any of this because all of its integer arguments are already `int`.
#include <stdio.h>
#include <string.h>

static int t_codegen_variadic_narrow_integer_promotion(void) {
    char buf[64];

    /* The original repro: a negative signed char cast to unsigned char.
       The cast is same-size, so it emits no conversion of its own. */
    signed char sc = -128;
    snprintf(buf, sizeof(buf), "%02x", (unsigned char)sc);
    if (strcmp(buf, "80") != 0) return 1;

    /* Same shape one width up. */
    short s = -1;
    snprintf(buf, sizeof(buf), "%04x", (unsigned short)s);
    if (strcmp(buf, "ffff") != 0) return 2;

    /* Without a cast, a negative signed char sign-extends to int. */
    snprintf(buf, sizeof(buf), "%d", sc);
    if (strcmp(buf, "-128") != 0) return 3;

    /* An unsigned char variable zero-extends. */
    unsigned char uc = 200;
    snprintf(buf, sizeof(buf), "%d", uc);
    if (strcmp(buf, "200") != 0) return 4;

    /* A negative short sign-extends. */
    short neg = -12345;
    snprintf(buf, sizeof(buf), "%d", neg);
    if (strcmp(buf, "-12345") != 0) return 5;

    /* An unsigned short zero-extends. */
    unsigned short us = 60000;
    snprintf(buf, sizeof(buf), "%d", us);
    if (strcmp(buf, "60000") != 0) return 6;

    /* _Bool promotes to int. */
    _Bool b = 1;
    snprintf(buf, sizeof(buf), "%d", b);
    if (strcmp(buf, "1") != 0) return 7;

    /* char through a cast that narrows from int. */
    snprintf(buf, sizeof(buf), "%d", (signed char)300);
    if (strcmp(buf, "44") != 0) return 8;

    /* Routing through a variable was always correct -- the stack reload
       zero-extends -- so this pins that the fix did not break it. */
    unsigned char via_var = (unsigned char)sc;
    snprintf(buf, sizeof(buf), "%02x", via_var);
    if (strcmp(buf, "80") != 0) return 9;

    /* An outer widening cast was also always correct. */
    snprintf(buf, sizeof(buf), "%02x", (unsigned)(unsigned char)sc);
    if (strcmp(buf, "80") != 0) return 10;

    /* Several narrow arguments in one call, mixed with an int. */
    snprintf(buf, sizeof(buf), "%02x,%d,%04x", (unsigned char)sc, 7, (unsigned short)s);
    if (strcmp(buf, "80,7,ffff") != 0) return 11;

    /* A narrow argument after the format string in a wide call, to exercise
       the stack-passed side of the ABI rather than only registers. */
    snprintf(buf, sizeof(buf), "%d%d%d%d%d%d%d%02x",
             1, 2, 3, 4, 5, 6, 7, (unsigned char)sc);
    if (strcmp(buf, "123456780") != 0) return 12;

    return 0;
}

/* ====================================================================== */
/* codegen_va_start_stack_overflow_params: exit codes 71..73 */
// Regression test: va_start overflow_arg_area didn't skip past fixed params
// that were passed on the stack (>6 int params). va_arg read the last fixed
// param as the first variadic arg, producing garbage values.
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

static int t_codegen_va_start_stack_overflow_params(void) {
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

/* ====================================================================== */
/* codegen_stacked_sixteen_byte_argument_lands_on_its_boundary: exit codes 81..86 */
// The other half of #C43. A sixteen-byte-aligned argument that overflows to
// the stack begins on a sixteen-byte boundary, so with an odd number of
// eight-byte slots ahead of it there is a gap. The caller pushed arguments in
// reverse with a single pad at the top of the area, which cannot express a gap
// *between* two arguments, so the value landed eight bytes low and the callee
// read half of it plus the padding.
//
// Every pairing against gcc now agrees; this pins the c17-to-c17 half, which
// is the one a test can run. Both `__int128` and `long double` were affected;
// `__float128` was not, and is here as the control that must not move.
#include <stdio.h>

typedef struct { long a, b; } S16;

static long long take_i128(long a, long b, long c, long d, long e, long f, long g, __int128 v)
{ return (long long)v; }
static long double take_ld(long a, long b, long c, long d, long e, long f, long g, long double v)
{ return v; }
/* macOS on aarch64 has no binary128 at all -- its `long double` is a double --
   so the control is compiled only where the type exists. */
#ifdef __SIZEOF_FLOAT128__
static double take_f128(long a, long b, long c, long d, long e, long f, long g, __float128 v)
{ return (double)v; }
#endif
static long take_s16(long a, long b, long c, long d, long e, long f, long g, S16 v)
{ return v.a + v.b; }
/* The argument after it must not be swallowed by the gap. */
static long long take_tail(long a, long b, long c, long d, long e, long f, long g,
                           __int128 v, long tail)
{ return (long long)v + tail; }

static int t_codegen_stacked_sixteen_byte_argument_lands_on_its_boundary(void) {
    __int128 x = 424242;
    long double l = 424242.0L;
    S16 s = { 11, 22 };

    if (take_i128(1,2,3,4,5,6,7, x) != 424242) return 1;
    if (take_ld(1,2,3,4,5,6,7, l) != 424242.0L) return 2;
#ifdef __SIZEOF_FLOAT128__
    { __float128 q = 424242.0Q;
      if (take_f128(1,2,3,4,5,6,7, q) != 424242.0) return 3; }
#endif
    if (take_s16(1,2,3,4,5,6,7, s) != 33) return 4;
    if (take_tail(1,2,3,4,5,6,7, x, 5) != 424247) return 5;

    /* An even number of eight-byte slots ahead needs no gap, and must not
       grow one. */
    if (take_i128(1,2,3,4,5,6,7, x) != 424242) return 6;

    return 0;
}

int main(void)
{
    int r;
    if ((r = t_codegen_variadic_64bit_args()) != 0) return 10 + r;
    if ((r = t_codegen_variadic_narrow_integer_promotion()) != 0) return 50 + r;
    if ((r = t_codegen_va_start_stack_overflow_params()) != 0) return 70 + r;
    if ((r = t_codegen_stacked_sixteen_byte_argument_lands_on_its_boundary()) != 0) return 80 + r;
    return 0;
}
"#;

/// 64-bit and narrow variadic arguments, `va_start` past stacked fixed
/// parameters, and a stacked sixteen-byte argument, at the matrix levels.
/// Consolidates `codegen_variadic_64bit_args`,
/// `codegen_variadic_narrow_integer_promotion`,
/// `codegen_va_start_stack_overflow_params` and
/// `codegen_stacked_sixteen_byte_argument_lands_on_its_boundary`.
#[test]
fn codegen_variadic_and_stacked_scalar_arguments() {
    assert_eq!(
        compile_and_run(
            "variadic_stacked_scalars",
            VARIADIC_AND_STACKED_SCALAR_ARGUMENTS,
            &[]
        ),
        0
    );
}

/// One section per consolidated test, each under that test's own
/// documentation. Exit codes:
/// `codegen_va_start_counts_fixed_parameter_registers` 11..=21.
/// `codegen_stacked_hfa_element_counts` 31..=36.
/// `codegen_variadic_hfa_argument` 41..=45.
/// `codegen_variadic_aggregate_results_coexist` 51..=60.
/// `codegen_rsp_relative_locals_across_stacked_args` 71..=72.
const VARIADIC_AND_STACKED_AGGREGATES: &str = r#"
/* ====================================================================== */
/* codegen_va_start_counts_fixed_parameter_registers: exit codes 11..21 */
// `va_start` counts every register a fixed parameter actually spends.
//
// The register-save-area indices are derived from how many general and SSE
// registers the fixed parameters occupy. Counting each one as a single
// general register put those indices out, so the first variadic argument was
// read from the wrong slot -- for a `_Complex` (two XMMs), an `__int128` (two
// general registers), an all-SSE struct of eight bytes or fewer (one XMM,
// since that shape started travelling in one), a nine-to-sixteen-byte struct
// (one register per eightbyte), and a MEMORY-class one (none at all).
#include <stdarg.h>

struct LL  { long a, b; };
struct DD  { double a, b; };
struct F1  { float v; };
struct DI  { double a; int b; };
struct BIG { long a, b, c; };

#define VA(name, type)                                                      \
    __attribute__((noinline)) static long name(type s, ...)                 \
    {                                                                       \
        va_list ap; va_start(ap, s);                                        \
        long a = va_arg(ap, long);                                          \
        double b = va_arg(ap, double);                                      \
        va_end(ap);                                                         \
        (void)s;                                                            \
        return a + (long)b;                                                 \
    }

VA(v_cx,   double _Complex)
VA(v_i128, __int128)
VA(v_f1,   struct F1)
VA(v_dd,   struct DD)
VA(v_ll,   struct LL)
VA(v_di,   struct DI)
VA(v_big,  struct BIG)
VA(v_ld,   long double)
VA(v_int,  int)
VA(v_dbl,  double)

/* Sixteen bytes, SSE+SSEUP: one XMM, not the register pair the shapes above
   take. Only where the type exists -- Apple arm64 has no runtime for it. */
#ifdef __FLT128_MANT_DIG__
struct Q { __float128 v; };
VA(v_q, struct Q)
#endif

static int t_codegen_va_start_counts_fixed_parameter_registers(void)
{
    struct LL  ll  = { 1, 2 };
    struct DD  dd  = { 1, 2 };
    struct F1  f1  = { 1 };
    struct DI  di  = { 1, 2 };
    struct BIG big = { 1, 2, 3 };
    __int128 q = 1;

    if (v_cx(__builtin_complex(1.0, 2.0), 10L, 5.0) != 15) return 1;
    if (v_i128(q, 10L, 5.0) != 15) return 2;
    if (v_f1(f1, 10L, 5.0) != 15) return 3;
    if (v_dd(dd, 10L, 5.0) != 15) return 4;
    if (v_ll(ll, 10L, 5.0) != 15) return 5;
    if (v_di(di, 10L, 5.0) != 15) return 6;
    if (v_big(big, 10L, 5.0) != 15) return 7;
    if (v_ld(1.0L, 10L, 5.0) != 15) return 8;

    /* Controls: the two shapes that always cost exactly one register. */
    if (v_int(1, 10L, 5.0) != 15) return 9;
    if (v_dbl(1.0, 10L, 5.0) != 15) return 10;

#ifdef __FLT128_MANT_DIG__
    struct Q qs;
    qs.v = 1.0q;
    if (v_q(qs, 10L, 5.0) != 15) return 11;
#endif

    return 0;
}
#undef VA

/* ====================================================================== */
/* codegen_stacked_hfa_element_counts: exit codes 31..36 */
// An HFA that does not fit in the remaining V registers is laid on the stack,
// and the caller writes its elements there itself. Every multi-element
// argument took the `_Complex` path to do that: a fixed two-element loop at
// the complex element stride. For a three- or four-element HFA that wrote the
// wrong number of elements at the wrong stride, and reserved the wrong number
// of bytes, so the argument after it landed inside it.
//
// The first two arguments exist only to consume V0-V7, forcing the third onto
// the stack. `noinline` keeps the parameters arriving through the ABI rather
// than being substituted -- an inlined stacked HFA is a separate defect
// (#C38) and would mask this one.
typedef struct { float a, b, c, d; }        SF4;
typedef struct { float a, b, c; }           SF3;
typedef struct { double a, b; }             SD2;
typedef struct { double a, b, c; }          SD3;

__attribute__((noinline)) double s_f4(SF4 p, SF4 q, SF4 r) {
    return (double)r.a * 1000 + r.b * 100 + r.c * 10 + r.d;
}
__attribute__((noinline)) double s_f3(SF4 p, SF4 q, SF3 r) {
    return (double)r.a * 100 + r.b * 10 + r.c;
}
__attribute__((noinline)) double s_d2(SF4 p, SF4 q, SD2 r) {
    return r.a * 10 + r.b;
}
__attribute__((noinline)) double s_d3(SF4 p, SF4 q, SD3 r) {
    return r.a * 100 + r.b * 10 + r.c;
}
/* an argument after the stacked HFA: too small a slot puts it inside */
__attribute__((noinline)) double s_tail(SF4 p, SF4 q, SF4 r, double t) {
    return (double)r.a * 1000 + r.b * 100 + r.c * 10 + r.d + t;
}
__attribute__((noinline)) double s_tail3(SF4 p, SF4 q, SF3 r, double t) {
    return (double)r.a * 100 + r.b * 10 + r.c + t;
}

static int t_codegen_stacked_hfa_element_counts(void) {
    SF4 z = {0, 0, 0, 0};
    SF4 f4 = {1, 2, 3, 4};
    SF3 f3 = {1, 2, 3};
    SD2 d2 = {1, 2};
    SD3 d3 = {1, 2, 3};

    if (s_f4(z, z, f4) != 1234) return 1;
    if (s_f3(z, z, f3) != 123) return 2;
    if (s_d2(z, z, d2) != 12) return 3;
    if (s_d3(z, z, d3) != 123) return 4;
    if (s_tail(z, z, f4, 5) != 1239) return 5;
    if (s_tail3(z, z, f3, 5) != 128) return 6;
    return 0;
}

/* ====================================================================== */
/* codegen_variadic_hfa_argument: exit codes 41..45 */
// An HFA passed to a variadic function arrives in the SIMD registers, like
// any other HFA, so `va_arg` has to read it out of *their* save area -- one
// element per 16-byte slot -- rather than out of the general-register one.
//
// Aggregates took the integer path unconditionally. That agreed with a caller
// which also sent them in general registers, and with nothing else: gcc's
// callee read `s0-s3` and got garbage. Once the caller was corrected to follow
// AAPCS64 §5.4.2, the two halves of a single c17-compiled program disagreed.
//
// The two sources space the elements differently -- 16 bytes apart in the save
// area, packed at their own stride on the stack -- which is why the copy walks
// a selected stride instead of branching on which source won.
//
// x86-64 had the same defect by another route and is covered here too: every
// aggregate went through the integer path as one wide read from the general
// save area, which is unrelated data for anything the classifier put in SSE
// registers.
#include <stdarg.h>
typedef struct { float a, b; }          VF2;
typedef struct { float a, b, c; }       VF3;
typedef struct { float a, b, c, d; }    VF4;
typedef struct { double a, b; }         VD2;
typedef struct { double a, b, c; }      VD3;

/* a trailing scalar catches a wrong step through the save area */
double v_f2(int n, ...) { va_list ap; va_start(ap, n);
    VF2 v = va_arg(ap, VF2); double t = va_arg(ap, double); va_end(ap);
    return (double)(v.a * 10 + v.b) + t; }
double v_f3(int n, ...) { va_list ap; va_start(ap, n);
    VF3 v = va_arg(ap, VF3); double t = va_arg(ap, double); va_end(ap);
    return (double)(v.a * 100 + v.b * 10 + v.c) + t; }
double v_f4(int n, ...) { va_list ap; va_start(ap, n);
    VF4 v = va_arg(ap, VF4); double t = va_arg(ap, double); va_end(ap);
    return (double)(v.a * 1000 + v.b * 100 + v.c * 10 + v.d) + t; }
double v_d2(int n, ...) { va_list ap; va_start(ap, n);
    VD2 v = va_arg(ap, VD2); double t = va_arg(ap, double); va_end(ap);
    return v.a * 10 + v.b + t; }
double v_d3(int n, ...) { va_list ap; va_start(ap, n);
    VD3 v = va_arg(ap, VD3); double t = va_arg(ap, double); va_end(ap);
    return v.a * 100 + v.b * 10 + v.c + t; }

int printf(const char *, ...);
int fflush(void *);

/* Traced, because this runs on a target that cannot be run locally: a crash
   loses the return code, so the log has to show how far it got and with what
   value. The flush is the load-bearing part -- the harness captures output
   through a pipe, so stdout is fully buffered and a segfault takes the whole
   buffer with it. `fflush(0)` flushes every stream. */
#define T(n) (printf("try " n "\n"), fflush(0))
#define G(n, g) (printf("got " n " %.1f\n", (double)(g)), fflush(0))

static int t_codegen_variadic_hfa_argument(void) {
    VF2 f2 = {1, 2};
    VF3 f3 = {1, 2, 3};
    VF4 f4 = {1, 2, 3, 4};
    VD2 d2 = {1, 2};
    VD3 d3 = {1, 2, 3};

    T("f2"); { double g = v_f2(1, f2, 5.0); G("f2", g); if (g != 17) return 1; }
    T("f3"); { double g = v_f3(1, f3, 5.0); G("f3", g); if (g != 128) return 2; }
    T("f4"); { double g = v_f4(1, f4, 5.0); G("f4", g); if (g != 1239) return 3; }
    T("d2"); { double g = v_d2(1, d2, 5.0); G("d2", g); if (g != 17) return 4; }
    T("d3"); { double g = v_d3(1, d3, 5.0); G("d3", g); if (g != 128) return 5; }
    return 0;
}
#undef T
#undef G

/* ====================================================================== */
/* codegen_variadic_aggregate_results_coexist: exit codes 51..60 */
#include <stdarg.h>
typedef struct { float a, b; }          AF2;
typedef struct { float a, b, c; }       AF3;
typedef struct { float a, b, c, d; }    AF4;
typedef struct { double a, b; }         AD2;
typedef struct { double a, b, c; }      AD3;
typedef struct { long a, b; }           AL2;   /* two general-register slots */
typedef struct { long a, b, c; }        AL3;   /* over 16 bytes: passed by pointer */
typedef struct { char a, b, c; }        AC3;   /* three bytes: fits a register */
typedef struct { char a, b, c, d, e; }  AC5;   /* five bytes: not a power of two */

int printf(const char *, ...);
int fflush(void *);

double g_f2(int n, ...) { va_list ap; va_start(ap, n);
    AF2 v = va_arg(ap, AF2); va_end(ap); return (double)(v.a * 10 + v.b); }
double g_f3(int n, ...) { va_list ap; va_start(ap, n);
    AF3 v = va_arg(ap, AF3); va_end(ap); return (double)(v.a * 100 + v.b * 10 + v.c); }
double g_f4(int n, ...) { va_list ap; va_start(ap, n);
    AF4 v = va_arg(ap, AF4); va_end(ap); return (double)(v.a * 1000 + v.b * 100 + v.c * 10 + v.d); }
double g_d2(int n, ...) { va_list ap; va_start(ap, n);
    AD2 v = va_arg(ap, AD2); va_end(ap); return v.a * 10 + v.b; }
double g_d3(int n, ...) { va_list ap; va_start(ap, n);
    AD3 v = va_arg(ap, AD3); va_end(ap); return v.a * 100 + v.b * 10 + v.c; }
int    g_c3(int n, ...) { va_list ap; va_start(ap, n);
    AC3 v = va_arg(ap, AC3); va_end(ap); return v.a * 100 + v.b * 10 + v.c; }
int    g_c5(int n, ...) { va_list ap; va_start(ap, n);
    AC5 v = va_arg(ap, AC5); va_end(ap);
    return v.a * 10000 + v.b * 1000 + v.c * 100 + v.d * 10 + v.e; }
long   g_l2(int n, ...) { va_list ap; va_start(ap, n);
    AL2 v = va_arg(ap, AL2); va_end(ap); return v.a * 10 + v.b; }
long   g_l3(int n, ...) { va_list ap; va_start(ap, n);
    AL3 v = va_arg(ap, AL3); va_end(ap); return v.a * 100 + v.b * 10 + v.c; }

/* two aggregates out of one va_list, so the areas must advance correctly */
double g_two(int n, ...) { va_list ap; va_start(ap, n);
    AF4 p = va_arg(ap, AF4); AD2 q = va_arg(ap, AD2); va_end(ap);
    return (double)(p.a * 1000 + p.b * 100 + p.c * 10 + p.d) + q.a * 10 + q.b; }

static int t_codegen_variadic_aggregate_results_coexist(void) {
    AF2 f2 = {1, 2};
    AF3 f3 = {1, 2, 3};
    AF4 f4 = {1, 2, 3, 4};
    AD2 d2 = {1, 2};
    AD3 d3 = {1, 2, 3};
    AC3 c3 = {1, 2, 3};
    AC5 c5 = {1, 2, 3, 4, 5};
    AL2 l2 = {1, 2};
    AL3 l3 = {1, 2, 3};

    /* all live in one function: this is what used to fall over. Traced
       because this runs on a target that cannot be run locally -- a crash
       loses the return code, so the log has to show which shape it died on.
       The flush matters: output is captured through a pipe, so stdout is
       fully buffered and a segfault would take the trace with it. */
#define T(n) (printf("try " n "\n"), fflush(0))
#define G(n, g) (printf("got " n " %.1f\n", (double)(g)), fflush(0))
    T("f2"); { double g = g_f2(0, f2); G("f2", g); if (g != 12) return 1; }
    T("f3"); { double g = g_f3(0, f3); G("f3", g); if (g != 123) return 2; }
    T("f4"); { double g = g_f4(0, f4); G("f4", g); if (g != 1234) return 3; }
    T("d2"); { double g = g_d2(0, d2); G("d2", g); if (g != 12) return 4; }
    T("d3"); { double g = g_d3(0, d3); G("d3", g); if (g != 123) return 5; }
    T("l2"); { long g = g_l2(0, l2); G("l2", g); if (g != 12) return 6; }
    T("l3"); { long g = g_l3(0, l3); G("l3", g); if (g != 123) return 7; }
    T("two"); { double g = g_two(0, f4, d2); G("two", g); if (g != 1246) return 8; }
    /* an aggregate that fits in a register is one load, not a chunked copy:
       chunking wrote each piece over the last and kept only the final byte */
    if (g_c3(0, c3) != 123) return 9;
    if (g_c5(0, c5) != 12345) return 10;
    return 0;
}
#undef T
#undef G

/* ====================================================================== */
/* codegen_rsp_relative_locals_across_stacked_args: exit codes 71..72 */
// A frame that addresses its locals through `%rsp` has them displaced while
// the outgoing argument area is reserved. The adjustment was undone as soon
// as the stacked arguments were written, but `%rsp` stays lowered until after
// the call -- and the *register* arguments are set up in between, so every
// one of them was read a slot off.
__attribute__((noinline)) static long f8(long a, long b, long c, long d,
                                         long e, long f, long g, long h)
{ return a + b + c + d + e + f + g + h; }
__attribute__((noinline)) static double d8(double a, double b, double c, double d,
                                           double e, double f, double g, double h,
                                           double i, double j)
{ return a + b + c + d + e + f + g + h + i + j; }

static int t_codegen_rsp_relative_locals_across_stacked_args(void) {
    /* Over-aligned locals are what force the frame onto %rsp. */
    __attribute__((aligned(64))) long v[8] = {1, 2, 3, 4, 5, 6, 7, 8};
    __attribute__((aligned(64))) double w[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    if (f8(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7]) != 36) return 1;
    if (d8(w[0], w[1], w[2], w[3], w[4], w[5], w[6], w[7], w[8], w[9]) != 55.0) return 2;
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_codegen_va_start_counts_fixed_parameter_registers()) != 0) return 10 + r;
    if ((r = t_codegen_stacked_hfa_element_counts()) != 0) return 30 + r;
    if ((r = t_codegen_variadic_hfa_argument()) != 0) return 40 + r;
    if ((r = t_codegen_variadic_aggregate_results_coexist()) != 0) return 50 + r;
    if ((r = t_codegen_rsp_relative_locals_across_stacked_args()) != 0) return 70 + r;
    return 0;
}
"#;

/// `va_start` after every class of fixed parameter, stacked and variadic
/// HFAs, several aggregate `va_arg` results at once, and `%rsp`-relative
/// locals across stacked arguments, at the matrix levels and at -O1.
/// Consolidates `codegen_va_start_counts_fixed_parameter_registers`,
/// `codegen_stacked_hfa_element_counts`, `codegen_variadic_hfa_argument`,
/// `codegen_variadic_aggregate_results_coexist` and
/// `codegen_rsp_relative_locals_across_stacked_args`.
#[test]
fn codegen_variadic_and_stacked_aggregates() {
    let src = VARIADIC_AND_STACKED_AGGREGATES;
    assert_eq!(compile_and_run("variadic_stacked_aggs", src, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("variadic_stacked_aggs_opt", src),
        0
    );
}

/// One section per consolidated test, each under that test's own
/// documentation. Exit codes:
/// `codegen_va_start_past_named_memory_class_params` 11..=15.
/// `codegen_va_arg_small_struct_assigned` 21..=28.
/// `codegen_register_aggregate_after_a_stacked_argument` 31..=30.
const VA_START_VA_ARG_AND_REGISTER_AGGREGATES: &str = r#"
/* ====================================================================== */
/* codegen_va_start_past_named_memory_class_params: exit codes 11..15 */
// `va_start` must point `overflow_arg_area` past the named parameters that
// live *there*, not merely past the ones that overflowed a register file.
//
// A named `long double` is X87 class and a named aggregate over sixteen bytes
// is MEMORY class: per System V AMD64 psABI 3.2.3 each occupies real bytes in
// the incoming argument area while consuming no register at all. The old
// tally counted only `max(gp - 6, 0) + max(fp - 8, 0)` eight-byte slots, so
// it charged nothing for either, and the first variadic argument was read
// from inside the named ones. `IncomingOff::take`'s alignment padding is
// invisible to such a tally for the same reason.
//
// The `-O2` run is not redundant: the defect is in the prologue, which the
// optimizer does not touch, but the allocator the values now come from does
// behave differently once values are folded away.
#include <stdarg.h>

struct Big { long q[3]; };          /* 24 bytes, MEMORY class */

/* Six named ints fill the GP file, `g` stacks, then a stacked long double. */
__attribute__((noinline)) static int
after_ld(int a, int b, int c, int d, int e, int f, int g, long double h, ...)
{
    va_list ap; va_start(ap, h);
    int v = va_arg(ap, int);
    va_end(ap);
    (void)a; (void)b; (void)c; (void)d; (void)e; (void)f; (void)g; (void)h;
    return v;
}

/* Four long doubles interleaved with ints, all past the register files. */
__attribute__((noinline)) static int
interleaved(int a, int b, int c, int d, int e, int f, int g, long double h,
            int i, long double j, int k, long double l, int m, long double n, ...)
{
    va_list ap; va_start(ap, n);
    int v = va_arg(ap, int);
    va_end(ap);
    (void)a; (void)b; (void)c; (void)d; (void)e; (void)f; (void)g;
    (void)h; (void)i; (void)j; (void)k; (void)l; (void)m; (void)n;
    return v;
}

/* Seven doubles stay in XMM0-6, so only the long double is stacked. The
   variadic double comes out of the SSE save area and never consults
   `overflow_arg_area` -- a control that must keep passing. */
__attribute__((noinline)) static double
fp_only(double a, double b, double c, double d, double e, double f, double g,
        long double h, ...)
{
    va_list ap; va_start(ap, h);
    double v = va_arg(ap, double);
    va_end(ap);
    (void)a; (void)b; (void)c; (void)d; (void)e; (void)f; (void)g; (void)h;
    return v;
}

/* A MEMORY-class aggregate alone: no register overflowed, yet 24 bytes of the
   incoming area are occupied.
   The first six variadic arguments come out of the GP save area and so prove
   nothing; the seventh is the first to consult `overflow_arg_area`, which is
   why the list is this long. */
__attribute__((noinline)) static int
after_big(struct Big s, ...)
{
    va_list ap; va_start(ap, s);
    int v = 0;
    for (int n = 0; n < 7; n++) v = va_arg(ap, int);
    va_end(ap);
    (void)s;
    return v;
}

/* Over-aligned, so `IncomingOff::take` inserts padding the old tally could
   not express either. Same seven-argument reason. */
struct __attribute__((aligned(16))) Wide { long q[3]; };

__attribute__((noinline)) static int
after_wide(int a, struct Wide s, ...)
{
    va_list ap; va_start(ap, s);
    int v = 0;
    for (int n = 0; n < 7; n++) v = va_arg(ap, int);
    va_end(ap);
    (void)a; (void)s;
    return v;
}

static int t_codegen_va_start_past_named_memory_class_params(void)
{
    struct Big  bg = { { 1, 2, 3 } };
    struct Wide wd = { { 1, 2, 3 } };

    if (after_ld(1, 2, 3, 4, 5, 6, 7, 8.0L, 1234) != 1234) return 1;
    if (interleaved(1, 2, 3, 4, 5, 6, 7, 8.0L, 9, 10.0L, 11, 12.0L, 13, 14.0L,
                    1234) != 1234) return 2;
    if (fp_only(1, 2, 3, 4, 5, 6, 7, 8.0L, 1234.0) != 1234.0) return 3;
    if (after_big(bg, 1, 2, 3, 4, 5, 6, 1234) != 1234) return 4;
    if (after_wide(1, wd, 1, 2, 3, 4, 5, 6, 1234) != 1234) return 5;

    return 0;
}

/* ====================================================================== */
/* codegen_va_arg_small_struct_assigned: exit codes 21..28 */
// `va_arg` of an aggregate, assigned rather than used to initialize.
//
// `linearize_va_op` gave the result a stack local only when the aggregate was
// wider than 64 bits. Below that the result pseudo held the struct's *bytes*,
// which is the crate-wide convention -- but `emit_assign`'s struct path does
// not read it that way. It calls `linearize_lvalue`, which falls through to
// `rvalue_addr`, which returns any non-`Sym` pseudo unchanged on the
// assumption that it already holds a pointer. So `emit_block_copy` took four
// bytes of struct data and dereferenced them as an address.
//
// The call-return path hit exactly this and was fixed by giving small struct
// returns a `__sret1_` local, with a comment naming the hazard. `VaArg` never
// got the same treatment.
//
// Every existing aggregate-`va_arg` test initializes a fresh declaration
// (`C3 v = va_arg(ap, C3);`), which goes through `linearize_stmt` -- a path
// with the opposite convention hard-coded. So `struct tiny v = va_arg(...)`
// worked while `v = va_arg(...)` crashed, and nothing noticed.
//
// Sizes straddle the 8-byte boundary deliberately: 4, 8, 12 and 16 bytes,
// integer and floating, since the value/address split is at 8 and the
// register/memory ABI split is at 16.
#include <stdarg.h>

struct s4  { int a; };
struct s8  { int a, b; };
struct s12 { int a, b, c; };
struct s16 { long a, b; };
struct f8  { float x, y; };
struct f16 { double x, y; };

static int take4(int n, ...) {
    struct s4 v;
    va_list ap; va_start(ap, n);
    int ok = 1;
    for (int i = 0; i < n; i++) {
        v = va_arg(ap, struct s4);      /* assignment, not initialization */
        if (v.a != i + 10) ok = 0;
    }
    va_end(ap);
    return ok;
}

static int take8(int n, ...) {
    struct s8 v;
    va_list ap; va_start(ap, n);
    int ok = 1;
    for (int i = 0; i < n; i++) {
        v = va_arg(ap, struct s8);
        if (v.a != i + 20 || v.b != i + 21) ok = 0;
    }
    va_end(ap);
    return ok;
}

static int take12(int n, ...) {
    struct s12 v;
    va_list ap; va_start(ap, n);
    int ok = 1;
    for (int i = 0; i < n; i++) {
        v = va_arg(ap, struct s12);
        if (v.a != i + 30 || v.b != i + 31 || v.c != i + 32) ok = 0;
    }
    va_end(ap);
    return ok;
}

static int take16(int n, ...) {
    struct s16 v;
    va_list ap; va_start(ap, n);
    int ok = 1;
    for (int i = 0; i < n; i++) {
        v = va_arg(ap, struct s16);
        if (v.a != i + 40 || v.b != i + 41) ok = 0;
    }
    va_end(ap);
    return ok;
}

static int takef8(int n, ...) {
    struct f8 v;
    va_list ap; va_start(ap, n);
    int ok = 1;
    for (int i = 0; i < n; i++) {
        v = va_arg(ap, struct f8);
        if (v.x != (float)(i + 50) || v.y != (float)(i + 51)) ok = 0;
    }
    va_end(ap);
    return ok;
}

static int takef16(int n, ...) {
    struct f16 v;
    va_list ap; va_start(ap, n);
    int ok = 1;
    for (int i = 0; i < n; i++) {
        v = va_arg(ap, struct f16);
        if (v.x != (double)(i + 60) || v.y != (double)(i + 61)) ok = 0;
    }
    va_end(ap);
    return ok;
}

/* The declaration-initializer form, which already worked: it must keep
   working, since the fix changes the shape of the pseudo it consumes. */
static int take4_init(int n, ...) {
    va_list ap; va_start(ap, n);
    int ok = 1;
    for (int i = 0; i < n; i++) {
        struct s4 v = va_arg(ap, struct s4);
        if (v.a != i + 10) ok = 0;
    }
    va_end(ap);
    return ok;
}

static int t_codegen_va_arg_small_struct_assigned(void) {
    struct s4  a0 = {10}, a1 = {11}, a2 = {12};
    struct s8  b0 = {20,21}, b1 = {21,22};
    struct s12 c0 = {30,31,32}, c1 = {31,32,33};
    struct s16 d0 = {40,41}, d1 = {41,42};
    struct f8  e0 = {50.0f,51.0f}, e1 = {51.0f,52.0f};
    struct f16 g0 = {60.0,61.0}, g1 = {61.0,62.0};

    if (!take4(3, a0, a1, a2)) return 1;
    if (!take8(2, b0, b1)) return 2;
    if (!take12(2, c0, c1)) return 3;
    if (!take16(2, d0, d1)) return 4;
    if (!takef8(2, e0, e1)) return 5;
    if (!takef16(2, g0, g1)) return 6;
    if (!take4_init(3, a0, a1, a2)) return 7;

    /* Mixed with scalars, which move the register save area along. */
    if (!take4(1, a0)) return 8;
    return 0;
}

/* ====================================================================== */
/* codegen_register_aggregate_after_a_stacked_argument: exit codes 31..30 */
// System V 3.2.3 step 5 says an argument passed in memory consumes no
// register -- so the running tallies of *consumed* registers must not move
// for it. The callee-side tallies advanced anyway. One that had run past
// its file still answered `used < file_len` correctly, which is why this
// survived, but not `used + needed <= file_len` when `needed` is zero:
// after nine
// `double`s, the ninth of them stacked, the callee asked whether the SSE
// file had room for none of a two-general-eightbyte aggregate and was told
// no, so it read the struct off the stack while the caller -- which has the
// same question written with a guard -- had put it in RDI/RSI.
typedef struct { long long a, b; } GG;
typedef struct { double x, y; } DD;
typedef struct { double x; long long y; } MIX;

#define DP double p1,double p2,double p3,double p4,double p5,double p6, \
           double p7,double p8,double p9
#define D9 1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0
#define LP long p1,long p2,long p3,long p4,long p5,long p6,long p7
#define L7 1L,2L,3L,4L,5L,6L,7L

/* The SSE file is spent and the ninth double is stacked; the aggregate that
   follows still belongs in the general registers. */
__attribute__((noinline)) static int gp_after_stacked_fp(DP, GG s, int tail)
{ return (p9 == 9.0 && s.a == 1 && s.b == 2 && tail == 7) ? 0 : 1; }

/* The mirror: the general file is spent and the seventh long is stacked; the
   all-SSE aggregate that follows still belongs in XMM0/XMM1. */
__attribute__((noinline)) static int fp_after_stacked_gp(LP, DD s, double tail)
{ return (p7 == 7 && s.x == 1.5 && s.y == 2.5 && tail == 3.5) ? 0 : 2; }

/* A mixed pair after the SSE file is spent has nowhere to put its SSE half,
   so the whole argument does go to memory -- the tally must not make this
   one wrong in the other direction. */
__attribute__((noinline)) static int mix_after_stacked_fp(DP, MIX s, int tail)
{ return (s.x == 4.5 && s.y == 6 && tail == 7) ? 0 : 3; }

static int t_codegen_register_aggregate_after_a_stacked_argument(void)
{
    GG g = { 1, 2 };
    DD d = { 1.5, 2.5 };
    MIX m = { 4.5, 6 };
    int r;
    if ((r = gp_after_stacked_fp(D9, g, 7))) return r;
    if ((r = fp_after_stacked_gp(L7, d, 3.5))) return r;
    if ((r = mix_after_stacked_fp(D9, m, 7))) return r;
    return 0;
}
#undef DP
#undef D9
#undef LP
#undef L7

int main(void)
{
    int r;
    if ((r = t_codegen_va_start_past_named_memory_class_params()) != 0) return 10 + r;
    if ((r = t_codegen_va_arg_small_struct_assigned()) != 0) return 20 + r;
    if ((r = t_codegen_register_aggregate_after_a_stacked_argument()) != 0) return 30 + r;
    return 0;
}
"#;

/// `va_start` past memory-class named parameters, `va_arg` of a small
/// struct by assignment, and a register aggregate after a stacked
/// argument, at the matrix levels and at -O2. Consolidates
/// `codegen_va_start_past_named_memory_class_params`,
/// `codegen_va_arg_small_struct_assigned` and
/// `codegen_register_aggregate_after_a_stacked_argument`.
#[test]
fn codegen_va_start_va_arg_and_register_aggregates_past_the_stack() {
    let src = VA_START_VA_ARG_AND_REGISTER_AGGREGATES;
    assert_eq!(compile_and_run("va_start_va_arg_reg_aggs", src, &[]), 0);
    assert_eq!(
        compile_and_run("va_start_va_arg_reg_aggs_o2", src, &["-O2".to_string()]),
        0
    );
}

/// A variadic function's register save area must start on a 16-byte boundary.
///
/// The SIMD half is written with `str q`, whose immediate is either scaled by
/// 16 or unscaled within +/-256. An offset that is neither has no encoding,
/// and the assembler rejects the whole function with "immediate offset out of
/// range". The save area sat at `16 + callee_saved + stack_size`, so any odd
/// `stack_size` misaligned it -- but `q0` at 223 still assembles as the
/// unscaled form and only `q5` at 303 does not, so the failure needs a frame
/// large enough to push the later registers past 256. An over-aligned local
/// is the easiest way to get one.
///
/// x86-64 has no such encoding limit, so this is an aarch64 regression test
/// that happens to be written in C; the host run is there to keep it honest.
#[test]
fn codegen_aarch64_variadic_save_area_is_16_aligned() {
    let code = r#"
#include <stdarg.h>

/* The over-aligned local is what inflates the frame; `pad` keeps it alive. */
__attribute__((noinline)) static long wide_frame(int n, ...)
{
    _Alignas(32) double pad[9];
    va_list ap;
    va_start(ap, n);
    long acc = 0;
    for (int i = 0; i < n; i++) {
        pad[i] = va_arg(ap, double);
        acc += (long) pad[i];
    }
    va_end(ap);
    return acc + (long) pad[0];
}

int main(void)
{
    if (wide_frame(9, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0) != 46) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_va_save_area_align", code, &[]), 0);
    for opt in ["-O0", "-O2"] {
        if let Some(status) = compile_and_run_aarch64("codegen_va_save_area_align_a64", code, opt) {
            assert_eq!(status, 0, "aarch64 at {opt}");
        }
    }
}

/// A zero-sized argument is not passed, so neither side may charge it a slot.
///
/// System V AMD64 psABI 3.2.3 and AAPCS64 both give such a type no class --
/// `ArgClass::Ignore` -- and the call site already skipped it. `va_arg` did
/// not: it rounded the size up to one byte, folded `Ignore` into the same
/// empty class vector as MEMORY, took the overflow path, copied a byte the
/// object does not own and advanced the cursor by eight, so every later
/// argument in the list came out eight bytes low.
///
/// On aarch64 the *caller* had the mirror of the same bug, in both of its
/// argument loops, and the two errors cancelled: c17 talking to c17 agreed
/// with itself while disagreeing with gcc, which is why only a cross-compiler
/// probe found it. Reading arguments both before and after the zero-sized one
/// is what makes a one-slot shift visible here.
#[test]
fn codegen_zero_sized_variadic_argument_consumes_no_slot() {
    let code = r#"
#include <stdarg.h>

struct Z  { char x[0]; };
struct Z2 { };

__attribute__((noinline)) static long mixed(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    long acc = 0;
    acc = acc * 10 + va_arg(ap, int);       /* before */
    (void)va_arg(ap, struct Z);             /* nothing at all */
    acc = acc * 10 + va_arg(ap, int);       /* after */
    (void)va_arg(ap, struct Z2);
    acc = acc * 10 + va_arg(ap, long);
    va_end(ap);
    (void)n;
    return acc;
}

/* Enough leading arguments that the ones after the zero-sized member are past
   the register file and read from the overflow area instead. */
__attribute__((noinline)) static long spilled(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    long acc = 0;
    for (int i = 0; i < 6; i++) acc = acc * 10 + va_arg(ap, int);
    (void)va_arg(ap, struct Z);
    acc = acc * 10 + va_arg(ap, int);
    va_end(ap);
    (void)n;
    return acc;
}

int main(void)
{
    struct Z  z;
    struct Z2 z2;

    if (mixed(0, 1, z, 2, z2, 3L) != 123) return 1;
    if (spilled(0, 1, 2, 3, 4, 5, 6, z, 7) != 1234567) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_va_arg_zero_sized", code, &[]), 0);
    assert_eq!(
        compile_and_run("codegen_va_arg_zero_sized_o2", code, &["-O2".to_string()]),
        0
    );

    // Run the same program on aarch64 under qemu. `compile_and_run` always
    // targets the host, so without this the aarch64 half of the fix -- which
    // was two separate sites there -- is asserted by nothing that executes.
    for opt in ["-O0", "-O2"] {
        if let Some(status) = compile_and_run_aarch64("codegen_va_arg_zero_sized_a64", code, opt) {
            assert_eq!(status, 0, "aarch64 at {opt}");
        }
    }
}

/// Several aggregate `va_arg` results live at once.
///
/// An aggregate wider than a register was given an ordinary pseudo with no
/// storage behind it, so the backend was handed a register that held whatever
/// happened to be there and treated it as the destination's address. One such
/// call could survive on luck; four in a function did not, which is why this
/// looked cumulative rather than shape-specific and hid behind register
/// pressure.

/// AAPCS64 §6.4.2 stage C rounds the next stacked-argument address up to
/// `max(8, alignof(type))` before placing the argument, not just its size
/// afterwards. Advancing by the rounded size alone put a sixteen-byte-aligned
/// argument eight bytes low whenever an odd number of eight-byte slots came
/// before it -- and the caller and the callee made the same mistake, so c17
/// agreed with itself and only a c17/gcc boundary showed it.
///
/// Eight integers fill X0-X7, then `pad` takes the first eight-byte stack
/// slot, so everything after it starts at an odd multiple of eight.
///
/// Gated to aarch64. x86-64 has the same defect by a different mechanism -- it
/// *pushes* stacked arguments rather than placing them at computed offsets, so
/// a sixteen-byte-aligned one lands wherever the running push count leaves it
/// -- and that is #C43, fixed separately. Running this there would assert a
/// defect this commit does not address.
#[test]
#[cfg(target_arch = "aarch64")]
fn codegen_stacked_argument_alignment() {
    let code = r#"
__attribute__((noinline))
static long f_i128(int a, int b, int c, int d, int e, int f, int g, int h,
                   long pad, __int128 v, long tail) {
    return (long)v + pad * 0 + tail * 0;
}
__attribute__((noinline))
static long double f_ld(int a, int b, int c, int d, int e, int f, int g, int h,
                        long pad, long double v, long tail) {
    return v + (long double)(pad * 0 + tail * 0);
}
/* the argument after the over-aligned one must not land inside it */
__attribute__((noinline))
static long f_after(int a, int b, int c, int d, int e, int f, int g, int h,
                    long pad, __int128 v, long tail) {
    return (long)v * 10 + tail;
}

int main(void) {
    /* through variables: an `__int128` *constant* argument crashes the x86-64
       backend, which is #C42 and not what this test is about */
    __int128 big = 424242;
    __int128 five = 5;
    if (f_i128(1,2,3,4,5,6,7,8, 99L, big, 77L) != 424242) return 1;
    if (f_ld(1,2,3,4,5,6,7,8, 99L, 12345.5L, 77L) != 12345.5L) return 2;
    if (f_after(1,2,3,4,5,6,7,8, 99L, five, 7L) != 57) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_stacked_arg_align", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("codegen_stacked_arg_align_opt", code),
        0
    );
}

/// `va_arg` of an SSE+SSEUP aggregate — one XMM register carrying all sixteen
/// bytes, rather than two registers of eight.
///
/// Two independent defects, each losing the same upper half:
///
/// 1. The variadic prologue saved each XMM into the register save area with
///    `movsd`, eight bytes into a sixteen-byte slot, so the top half of every
///    slot held whatever the frame did. A `double` never noticed, and neither
///    did `struct { double a, b; }` -- that arrives in *two* registers, and
///    each one's low half is all there is to save.
/// 2. `emit_va_arg_aggregate` walked the ABI classification as one eightbyte
///    per entry. `classes` counts registers: `sse_struct_regs` documents
///    SSE+SSEUP as a single entry covering sixteen bytes, so the copy took
///    eight and left the rest of the destination untouched.
///
/// A third, in the same shape, for a bare `__float128` rather than one inside
/// a struct: `emit_va_arg_float` sized the move from `<= 32 ? Single : Double`,
/// so a 128-bit type moved eight bytes, and its overflow-area cursor advanced
/// by eight where the slot is sixteen.
///
/// Either defect alone reproduces the loss, so both are checked here with a
/// value whose upper half is the part that matters.
///
/// x86-64 Linux only. All three defects are in `cc/arch/x86_64/`, and
/// `__float128` is that target's spelling for binary128 -- aarch64 reaches the
/// same type through `long double`, with its own lowering and its own save
/// area, so this source does not describe it. macOS is excluded on either
/// architecture because `arch::has_float128` is false for the whole OS, so the
/// type does not parse there at all. `compile_and_run` builds for the host,
/// which is what makes the guard necessary rather than merely tidy.
#[cfg(all(target_arch = "x86_64", not(target_os = "macos")))]
#[test]
fn codegen_va_arg_sse_up_aggregate() {
    let code = r#"
#include <stdarg.h>

struct q  { __float128 v; };
struct dd { double a, b; };

static int take_q(int n, ...) {
    va_list ap; va_start(ap, n);
    int ok = 1;
    for (int i = 0; i < n; i++) {
        struct q x = va_arg(ap, struct q);
        if (x.v != (__float128)(i + 1)) ok = 0;
    }
    va_end(ap);
    return ok;
}

/* Assigned rather than initialized, so the small-aggregate local path runs
   as well as the sixteen-byte one. */
static int take_dd(int n, ...) {
    va_list ap; va_start(ap, n);
    struct dd x;
    int ok = 1;
    for (int i = 0; i < n; i++) {
        x = va_arg(ap, struct dd);
        if (x.a != (double)(i + 1) || x.b != (double)(i + 2)) ok = 0;
    }
    va_end(ap);
    return ok;
}

/* A bare __float128 travels the same way, without a struct around it. */
static int take_f128(int n, ...) {
    va_list ap; va_start(ap, n);
    int ok = 1;
    for (int i = 0; i < n; i++) {
        __float128 v = va_arg(ap, __float128);
        if (v != (__float128)(i + 1)) ok = 0;
    }
    va_end(ap);
    return ok;
}

/* Mixed with doubles, which move the SSE save area's cursor along. */
static int take_mixed(int n, ...) {
    va_list ap; va_start(ap, n);
    double d = va_arg(ap, double);
    struct q x = va_arg(ap, struct q);
    double e = va_arg(ap, double);
    va_end(ap);
    (void)n;
    return d == 1.0 && x.v == (__float128)1 && e == 3.0;
}

static int take_q_va(__float128 want, int n, ...) {
    va_list ap; va_start(ap, n);
    struct q x = va_arg(ap, struct q);
    va_end(ap);
    return x.v == want;
}

int main(void) {
    struct q q1, q2;
    q1.v = (__float128)1;
    q2.v = (__float128)2;
    struct dd d1 = {1, 2}, d2 = {2, 3};

    if (!take_q(2, q1, q2)) return 1;
    if (!take_dd(2, d1, d2)) return 2;
    if (!take_f128(2, (__float128)1, (__float128)2)) return 3;
    if (!take_mixed(3, 1.0, q1, 3.0)) return 4;

    /* A value whose mantissa fills the part that was being dropped: an
       eight-byte save leaves the low half only, and 1/3 differs from
       anything that could survive that. */
    {
        struct q third;
        third.v = (__float128)1 / (__float128)3;
        if (!take_q_va((__float128)1 / (__float128)3, 1, third)) return 5;
    }
    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_va_arg_sse_up", code, &[]), 0);
    assert_eq!(
        compile_and_run("codegen_va_arg_sse_up_o2", code, &["-O2".to_string()]),
        0
    );
}

const OVER_ALIGNED_STACKED_AGGREGATE: &str = r#"
typedef struct { long long a, b, c; } Big;                    /* MEMORY class */
typedef struct __attribute__((aligned(32))) { double a, b, c, d; } Over;

__attribute__((noinline)) static int take(long p1, long p2, long p3, long p4,
                                          long p5, long p6, Big s, int tail)
{ return (s.a == 1 && s.b == 2 && s.c == 3 && tail == 7) ? 0 : 1; }

int main(void)
{
    Over o = { 1, 2, 3, 4 };    /* over-aligns main's frame */
    Big b = { 1, 2, 3 };
    if (o.a != 1 || o.d != 4) return 2;
    return take(1, 2, 3, 4, 5, 6, b, 7);
}
"#;

/// `stack_mem` picks the frame's base register, and an over-aligned frame
/// addresses its locals through a second base rather than `%rbp`.
/// `address_of_pseudo` spelled `-(offset + callee_saved_offset)(%rbp)` by
/// hand, which in such a frame names a different address entirely -- so the
/// copy of a stacked aggregate argument loaded its source pointer from
/// garbage and the caller segfaulted.
#[test]
fn codegen_over_aligned_frame_addresses_a_stacked_aggregate_through_its_own_base() {
    assert_eq!(
        compile_and_run(
            "c17_overaligned_stacked_agg",
            OVER_ALIGNED_STACKED_AGGREGATE,
            &[]
        ),
        0
    );
    assert_eq!(
        compile_and_run(
            "c17_overaligned_stacked_agg_o2",
            OVER_ALIGNED_STACKED_AGGREGATE,
            &["-O2".to_string()]
        ),
        0
    );
}

/// The behavioural tests above only fail when the bogus `%rbp` displacement
/// happens to land somewhere that matters, which the exact layout decides;
/// adding a second call to the stacked-aggregate one was enough to make it
/// pass while it still generated the wrong address. So assert the invariant
/// directly: once the prologue has realigned the stack, the only uses of
/// `%rbp` are establishing it, the epilogue's `lea` back to the callee-saved
/// area, and restoring it.
#[test]
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn codegen_over_aligned_frame_keeps_no_local_at_an_rbp_displacement() {
    for (what, src) in [
        ("stacked_agg", OVER_ALIGNED_STACKED_AGGREGATE),
        ("long_double", OVER_ALIGNED_LONG_DOUBLE),
    ] {
        let asm = crate::codegen::asm_probe::host_asm(&format!("c17_overaligned_rbp_{what}_"), src);
        let main = asm
            .split("\nmain:\n")
            .nth(1)
            .and_then(|s| s.split(".cfi_endproc").next())
            .unwrap_or_else(|| panic!("no main in assembly for {what}"));
        assert!(
            main.contains("andq $-32, %rsp"),
            "{what}: main's frame was not over-aligned, so this proves nothing:\n{main}"
        );
        let strays: Vec<&str> = main
            .lines()
            .map(str::trim)
            // A `.cfi_*` rule names the register; it addresses nothing.
            .filter(|l| l.contains("%rbp") && !l.starts_with(".cfi_"))
            .filter(|l| {
                *l != "pushq %rbp"
                    && *l != "popq %rbp"
                    && *l != "movq %rsp, %rbp"
                    && !(l.starts_with("leaq ") && l.ends_with("(%rbp), %rsp"))
            })
            .collect();
        assert!(
            strays.is_empty(),
            "{what}: an over-aligned frame addressed something \
             through %rbp: {strays:?}\n{main}"
        );
    }
}

/// Stacked aggregate arguments past the unroll threshold are copied with
/// `rep movsq`, and arrive intact at a callee another compiler built.
///
/// One load/store pair per eightbyte made a 600 MB argument 75 million
/// instructions and tens of gigabytes of compiler memory. The copy runs after
/// the outgoing area is reserved and before the register arguments are set up,
/// so RDI, RSI and RCX -- which `rep movsq` needs -- can still hold arguments;
/// here they hold `a`, `c` and `e`, and must survive it.
#[test]
fn codegen_large_stacked_argument_block_copy() {
    let caller = r#"
struct big { long v[8192]; };
long take(int a, struct big b, int c, struct big d, int e);
static struct big x, y;
__attribute__((noinline)) long go(int a, int c, int e) { return take(a, x, c, y, e); }
int main(void)
{
    for (int i = 0; i < 8192; i++) { x.v[i] = i * 3; y.v[i] = i; }
    long want = 1 + 2 + 3;
    for (int i = 0; i < 8192; i++) want += x.v[i] - y.v[i];
    want += x.v[0] * 1000 + y.v[8191];
    return go(1, 2, 3) == want ? 0 : 1;
}
"#;
    let callee = r#"
struct big { long v[8192]; };
long take(int a, struct big b, int c, struct big d, int e)
{
    long s = a + c + e;
    for (int i = 0; i < 8192; i++) s += b.v[i] - d.v[i];
    return s + b.v[0] * 1000 + d.v[8191];
}
"#;
    if let Some(code) = compile_with_host_cc("big_stacked_args", caller, callee) {
        assert_eq!(code, 0);
    }
    let asm = crate::common::asm_for_at(
        "big_stacked_args",
        caller,
        &["--target", "x86_64-unknown-linux-gnu"],
    );
    assert!(
        asm.contains("rep movsq"),
        "expected a block copy:
{asm}"
    );
}

/// A type-name of variably modified type -- in a cast, a compound literal or
/// `va_arg` -- carries its extents to the value it yields. The type alone is
/// `int (*)[]`, so `(int (*)[n])buf + 1` stepped by 0, `sizeof *(int (*)[n])buf`
/// was 0, and the size expressions were never evaluated at all (C17 6.8p4).
/// `&` steps back out of an index chain, and the `[]` of `int (*)[][m]` takes
/// no size expression -- declared or cast, `m` went to it and the rows were 0.
#[test]
fn codegen_type_name_of_variably_modified_type_carries_its_extents() {
    let src = r#"
#include <stdarg.h>

#define STEP(p) ((char *)((p) + 1) - (char *)(p))

static long through_va_arg(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    char *base = va_arg(ap, char *);
    long r = (char *)(va_arg(ap, int (*)[n]) + 1) - base;
    va_end(ap);
    return r;
}

static int f(int n, int m)
{
    int buf[60];
    for (int i = 0; i < 60; i++)
        buf[i] = i;
    typedef int T[n];
    int v[n];
    const long row = n * (long)sizeof(int);

    /* A cast's extents step the pointer it yields... */
    if (STEP((int (*)[n])buf) != row) return 1;
    if ((char *)(1 + (int (*)[n])buf) - (char *)buf != row) return 2;
    if ((int (*)[n])buf + 3 - (int (*)[n])buf != 3) return 3;
    if ((char *)&((int (*)[n])buf)[2] - (char *)buf != 2 * row) return 4;
    if (((int (*)[n])buf)[2][3] != 13) return 5;
    /* ...size what it points at... */
    if (sizeof *(int (*)[n])buf != row) return 6;
    if (sizeof((int (*)[n][m])buf)[0][1] != m * sizeof(int)) return 7;
    if ((*((int (*)[n][m])buf + 1))[1][2] != 20) return 8;
    /* ...whether written out, through a typedef, or through typeof. */
    if (STEP((T *)buf) != row) return 9;
    if (STEP((typeof(v) *)buf) != row) return 10;
    if (STEP((typeof((int (*)[n])buf))buf) != row) return 11;
    /* The size expressions are evaluated once, where the cast is. */
    int k = n;
    long s = (char *)((int (*)[k++])buf + 1) - (char *)buf;
    if (s != row || k != n + 1) return 12;
    /* An initialized pointer takes its own extents, as before. */
    int (*q)[n] = (int (*)[n])buf;
    q++;
    if ((char *)q - (char *)buf != row) return 13;
    /* A compound literal of pointer-to-VLA type is sized the same way. */
    if ((char *)&(int (*)[n]){ (void *)buf }[1] - (char *)buf != row) return 14;
    if (through_va_arg(n, buf, buf) != row) return 15;
    /* `sizeof` of a pointer type evaluates nothing, as gcc has it. */
    int b = 0;
    if (sizeof(int (*)[b++]) != sizeof(void *) || b != 0) return 16;
    /* `&` steps back out: `&*p` is `p`, and `&v` points at all of `v`. */
    if (STEP(&*(int (*)[n])buf) != row) return 18;
    if (STEP(&v) != row || sizeof *&v != row) return 19;
    typeof(&v) pv = &v;
    if (STEP(pv) != row || (char *)&(&v)[1] - (char *)v != row) return 20;
    /* An incomplete outermost level takes no size expression: `m` sizes
       the rows of `int (*)[][m]`, not the `[]`. */
    int (*inc)[][m] = (int (*)[][m])buf;
    if ((*inc)[2][1] != 7 || ((*(int (*)[][m])buf))[2][1] != 7) return 21;
    if (sizeof (*inc)[0] != m * sizeof(int)) return 22;
    typeof(inc) inc2 = inc;
    if ((*inc2)[3][2] != 11) return 23;
    /* Each evaluation of the cast reads the extent afresh. */
    for (int w = 1; w <= 3; w++)
        if (STEP((int (*)[w])buf) != w * (long)sizeof(int)) return 17;
    return 0;
}

int main(void)
{
    return f(5, 3);
}
"#;
    compile_and_run_everywhere("vm_type_name_extents", src);
}

/// `va_arg` through a pointer to a `va_list`, for every class the x86-64
/// lowering has a path for, and a 64-bit constant converted to `long double`.
///
/// Both were reachable only once copy propagation stopped putting every
/// operand through a register first. The floating `va_arg` path sign-extended
/// `fp_offset` into R11 -- the register holding the `va_list` pointer -- and
/// stored the advanced offset through the clobbered value; the x87 path
/// advanced `overflow_arg_area` in R11 the same way. And the x87 conversion
/// stored a constant past 32 bits straight to memory, which no x86-64
/// instruction encodes.
#[test]
fn codegen_va_arg_through_a_pointer_and_wide_constant_conversions() {
    let src = r#"
#include <stdarg.h>
__attribute__((noinline)) int take(va_list *ap) {
    if (va_arg(*ap, int) != 7) return 1;
    if (va_arg(*ap, double) != 0.5) return 2;
    if (va_arg(*ap, long double) != 2.25L) return 3;
    if (va_arg(*ap, int) != 9) return 4;
    return 0;
}
__attribute__((noinline)) int outer(int n, ...) {
    va_list ap;
    va_start(ap, n);
    int r = take(&ap);
    va_end(ap);
    return r;
}
__attribute__((noinline)) long double widest(void) { return 9223372036854775807LL; }
int main(void) {
    int r = outer(0, 7, 0.5, 2.25L, 9);
    if (r) return r;
    if (widest() != 9223372036854775807.0L) return 10;
    return 0;
}
"#;
    for level in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(&format!("va_arg_ptr{level}"), src, &[level.to_string()]),
            0,
            "{level}"
        );
    }
}

// ============================================================================
// A variadic callee reached through a pointer
// ============================================================================

/// A call through a pointer to a variadic function is a variadic call: the
/// arguments past the prototype take the default argument promotions (C17
/// 6.5.2.2p7), whatever spelling reaches the pointer -- a variable, a
/// typedef, a struct member, a call returning one. The callee's type was
/// read off the pointer, which is not variadic, so none of that happened:
/// `(unsigned char)0x80` arrived sign-extended and a `float` arrived as a
/// `float` where `va_arg(ap, double)` reads.
///
/// A `_Noreturn` callee through a pointer ends the path the same way.
#[test]
fn codegen_variadic_call_through_a_pointer_promotes() {
    let src = r#"
#include <stdarg.h>
#include <stdlib.h>

static int check(int n, ...) {
    va_list ap;
    va_start(ap, n);
    int c = va_arg(ap, int);
    int s = va_arg(ap, int);
    double f = va_arg(ap, double);
    long l = va_arg(ap, long);
    va_end(ap);
    if (c != 0x80) return n + 1;
    if (s != 0xffff) return n + 2;
    if (f != 1.5) return n + 3;
    if (l != 7) return n + 4;
    return 0;
}

typedef int (*VF)(int, ...);
struct ops { long pad; VF fn; };
static VF get(void) { return check; }

_Noreturn static void die(int code) { exit(code); }

int main(void) {
    signed char c = -128;
    short s = -1;
    float f = 1.5f;
    int r;
    if ((r = check(0, (unsigned char)c, (unsigned short)s, f, 7L))) return r;

    int (*p)(int, ...) = check;
    if ((r = p(10, (unsigned char)c, (unsigned short)s, f, 7L))) return r;
    if ((r = (*p)(20, (unsigned char)c, (unsigned short)s, f, 7L))) return r;

    VF q = check;
    if ((r = q(30, (unsigned char)c, (unsigned short)s, f, 7L))) return r;

    struct ops o = { 0, check };
    struct ops *op = &o;
    if ((r = o.fn(40, (unsigned char)c, (unsigned short)s, f, 7L))) return r;
    if ((r = op->fn(50, (unsigned char)c, (unsigned short)s, f, 7L))) return r;

    if ((r = get()(60, (unsigned char)c, (unsigned short)s, f, 7L))) return r;

    __typeof__(die) *np = die;
    np(0);
    return 99;
}
"#;
    compile_and_run_everywhere("va_through_ptr", src);
}
