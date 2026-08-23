//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 Complex Number Tests

use crate::common::{compile_and_run, compile_and_run_optimized};

#[test]
fn c99_complex_mega() {
    let code = r#"
#include <complex.h>

int main(void) {
    // ========== BASIC COMPLEX (returns 1-9) ==========
    {
        // complex macro expands to _Complex
        double complex z1 = __builtin_complex(3.0, 4.0);
        if (creal(z1) < 2.9 || creal(z1) > 3.1) return 1;
        if (cimag(z1) < 3.9 || cimag(z1) > 4.1) return 2;

        // I macro
        double complex z2 = I;
        if (cimag(z2) < 0.9 || cimag(z2) > 1.1) return 3;

        // 3 + 4i using I macro
        double complex z3 = 3.0 + 4.0 * I;
        if (creal(z3) < 2.9 || creal(z3) > 3.1) return 4;
        if (cimag(z3) < 3.9 || cimag(z3) > 4.1) return 5;

        // Real scalar init (imag = 0)
        double complex z4 = 5.0;
        if (creal(z4) < 4.9 || creal(z4) > 5.1) return 6;
        if (cimag(z4) < -0.1 || cimag(z4) > 0.1) return 7;
    }

    // ========== COMPLEX ARITHMETIC (returns 10-19) ==========
    {
        double complex a = __builtin_complex(1.0, 2.0);
        double complex b = __builtin_complex(3.0, 4.0);

        // Addition
        double complex sum = a + b;
        if (creal(sum) < 3.9 || creal(sum) > 4.1) return 10;
        if (cimag(sum) < 5.9 || cimag(sum) > 6.1) return 11;

        // Subtraction
        double complex diff = b - a;
        if (creal(diff) < 1.9 || creal(diff) > 2.1) return 12;
        if (cimag(diff) < 1.9 || cimag(diff) > 2.1) return 13;

        // Multiplication: (1+2i)(3+4i) = 3+4i+6i+8i² = 3+10i-8 = -5+10i
        double complex prod = a * b;
        if (creal(prod) < -5.1 || creal(prod) > -4.9) return 14;
        if (cimag(prod) < 9.9 || cimag(prod) > 10.1) return 15;

        // Division: (3+4i)/(1+2i) = (3+4i)(1-2i)/((1+2i)(1-2i)) = (11-2i)/5
        double complex quot = b / a;
        double qr = creal(quot);
        double qi = cimag(quot);
        if (qr < 2.1 || qr > 2.3) return 16;   // 11/5 = 2.2
        if (qi < -0.5 || qi > 0.1) return 17;    // -2/5 = -0.4
    }

    // ========== MIXED REAL + COMPLEX (returns 20-29) ==========
    {
        double complex z = __builtin_complex(2.0, 3.0);

        // Real + Complex
        double complex r1 = 10.0 + z;
        if (creal(r1) < 11.9 || creal(r1) > 12.1) return 20;
        if (cimag(r1) < 2.9 || cimag(r1) > 3.1) return 21;

        // Complex - Real
        double complex r2 = z - 1.0;
        if (creal(r2) < 0.9 || creal(r2) > 1.1) return 22;

        // Real * Complex
        double complex r3 = 2.0 * z;
        if (creal(r3) < 3.9 || creal(r3) > 4.1) return 23;
        if (cimag(r3) < 5.9 || cimag(r3) > 6.1) return 24;
    }

    // ========== CREAL / CIMAG (returns 30-39) ==========
    {
        double complex z = 7.0 + 8.0 * I;
        double r = creal(z);
        double i = cimag(z);
        if (r < 6.9 || r > 7.1) return 30;
        if (i < 7.9 || i > 8.1) return 31;
    }

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_complex_mega", code, &["-lm".to_string()],),
        0,
    );
}

/// A `_Complex` member of an *automatic* aggregate gets its value.
///
/// A complex value lives in memory and travels by address, so the aggregate
/// initializer -- which assumes a scalar member holds its own value -- stored
/// the address instead. Every complex member of a local struct, union or array
/// read back as a stack address reinterpreted as a double (about 6.9e-310),
/// and a *real* initializer for one crashed outright. The static forms were
/// always right, so `static struct S s = {1.0+2.0*I};` and the same
/// declaration without `static` disagreed.
///
/// Checked against gcc on the same source, at -O0 and -O2.
#[test]
fn c99_complex_members_of_automatic_aggregates_are_initialized() {
    let code = r#"
/* complex.h's I, spelled out so the test needs no libm call. */
#define I __builtin_complex(0.0, 1.0)

struct S { double _Complex z; };
struct P { int tag; double _Complex z; };
struct N { struct S inner; };
struct T { double _Complex a, b; };
struct V { double _Complex z; int n; };
struct F { float _Complex z; };
struct L { long double _Complex z; };
union  U { double _Complex z; double d[2]; };

/* The value is read through a pointer rather than passed by value: this is a
   test about initialization, and passing a complex by value is a separate
   path with its own defects. */
static int eq(const double _Complex *v, double re, double im)
{
    const double *p = (const double *)v;
    return p[0] == re && p[1] == im;
}
static int eqf(const float _Complex *v, float re, float im)
{
    const float *p = (const float *)v;
    return p[0] == re && p[1] == im;
}
static int eql(const long double _Complex *v, long double re, long double im)
{
    const long double *p = (const long double *)v;
    return p[0] == re && p[1] == im;
}

int main(void)
{
    /* A struct member, from every shape of initializer. */
    struct S a = { 1.0 + 2.0*I };            if (!eq(&a.z, 1, 2)) return 1;
    struct S b = { 3.0 };                    if (!eq(&b.z, 3, 0)) return 2;
    struct S c = { __builtin_complex(4.0, 5.0) }; if (!eq(&c.z, 4, 5)) return 3;
    double _Complex src = __builtin_complex(6.0, 7.0);
    struct S d = { src };                    if (!eq(&d.z, 6, 7)) return 4;
    struct S e = { 7 };                      if (!eq(&e.z, 7, 0)) return 5;

    /* Alongside other members, in both orders, and designated. */
    struct P f = { 9, 1.5 + 2.5*I };
    if (f.tag != 9 || !eq(&f.z, 1.5, 2.5)) return 6;
    struct V g = { 1.0 + 2.0*I, 42 };
    if (g.n != 42 || !eq(&g.z, 1, 2)) return 7;
    struct P h = { .z = 7.0 + 8.0*I, .tag = 2 };
    if (h.tag != 2 || !eq(&h.z, 7, 8)) return 8;
    struct T i = { 1.0 + 2.0*I, 3.0 + 4.0*I };
    if (!eq(&i.a, 1, 2) || !eq(&i.b, 3, 4)) return 9;

    /* An omitted member is still zeroed. */
    struct P j = { 5 };
    if (j.tag != 5 || !eq(&j.z, 0, 0)) return 10;

    /* Nested, union, compound literal. */
    struct N k = { { 8.0 + 9.0*I } };        if (!eq(&k.inner.z, 8, 9)) return 11;
    union U l = { 1.0 + 2.0*I };             if (!eq(&l.z, 1, 2)) return 12;
    struct S m = (struct S){ 2.0 + 3.0*I };  if (!eq(&m.z, 2, 3)) return 13;

    /* Arrays: of complex, and of structs holding one. */
    double _Complex n[2] = { 1.0 + 2.0*I, 3.0 };
    if (!eq(&n[0], 1, 2) || !eq(&n[1], 3, 0)) return 14;
    double _Complex o[3] = { 1.0 + 2.0*I };
    if (!eq(&o[0], 1, 2) || !eq(&o[1], 0, 0) || !eq(&o[2], 0, 0)) return 15;
    double _Complex p[3] = { [2] = 1.0 + 2.0*I };
    if (!eq(&p[0], 0, 0) || !eq(&p[2], 1, 2)) return 16;
    struct S q[2] = { { 1.0 + 2.0*I }, { 3.0 + 4.0*I } };
    if (!eq(&q[0].z, 1, 2) || !eq(&q[1].z, 3, 4)) return 17;

    /* A braced complex scalar reaches the initializer-list path, not the
       complex arm of a plain declaration. */
    double _Complex r = { 5.0 };             if (!eq(&r, 5, 0)) return 18;

    /* Other base precisions. `I` is a *double* complex, so a float member is
       a conversion in both halves. */
    struct F s = { 2.0f + 3.0f*I };          if (!eqf(&s.z, 2, 3)) return 19;
    float _Complex t[2] = { 1.0f + 2.0f*I, 3.0f };
    if (!eqf(&t[0], 1, 2) || !eqf(&t[1], 3, 0)) return 20;
    struct L u = { 1.5L + 2.5L*I };          if (!eql(&u.z, 1.5L, 2.5L)) return 21;

    /* Controls: these were always right and must stay so. */
    double _Complex v = 1.0 + 2.0*I;         if (!eq(&v, 1, 2)) return 22;
    struct S w; w.z = 4.0 + 5.0*I;           if (!eq(&w.z, 4, 5)) return 23;
    static struct S x = { 1.0 + 2.0*I };     if (!eq(&x.z, 1, 2)) return 24;

    return 0;
}
"#;
    assert_eq!(compile_and_run("complex_aggregate_init", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("complex_aggregate_init_opt", code),
        0
    );
}

/// GCC's `__real__` and `__imag__`, as rvalues and as lvalues.
///
/// Both are long-standing extensions and the natural way to reach a complex
/// value's halves without `<complex.h>` -- c17 offered only `creal`/`cimag`,
/// which need libm. They are lvalues when the operand is one, so assignment,
/// compound assignment and `&` all have to work; and gcc accepts them on a
/// *real* operand too, where `__real__ x` is `x` and `__imag__ x` is a zero of
/// its type. Every expectation checked against gcc on the same source.
#[test]
fn c99_real_and_imag_operators() {
    let code = r#"
int main(void)
{
    double _Complex z = __builtin_complex(1.5, 2.5);

    /* Rvalue, both spellings. */
    if (__real__ z != 1.5) return 1;
    if (__imag__ z != 2.5) return 2;
    if (__real z != 1.5) return 3;
    if (__imag z != 2.5) return 4;

    /* Every base precision. */
    float _Complex f = __builtin_complex(3.5f, 4.5f);
    if (__real__ f != 3.5f || __imag__ f != 4.5f) return 5;
    long double _Complex l = __builtin_complex(5.5L, 6.5L);
    if (__real__ l != 5.5L || __imag__ l != 6.5L) return 6;

    /* Lvalue: plain and compound assignment. */
    __real__ z = 10.0;
    if (__real__ z != 10.0 || __imag__ z != 2.5) return 7;
    __imag__ z = 20.0;
    if (__imag__ z != 20.0 || __real__ z != 10.0) return 8;
    __real__ z += 5.0;
    if (__real__ z != 15.0) return 9;
    __imag__ z *= 2.0;
    if (__imag__ z != 40.0) return 10;

    /* Taking the address of a half. */
    double *pr = &__real__ z;
    double *pi = &__imag__ z;
    if (*pr != 15.0 || *pi != 40.0) return 11;
    *pi = 1.0;
    if (__imag__ z != 1.0) return 12;

    /* A real operand: the real part is the value, the imaginary part zero. */
    double r = 7.5;
    if (__real__ r != 7.5) return 13;
    if (__imag__ r != 0.0) return 14;
    int n = 3;
    if (__real__ n != 3) return 15;

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_real_imag", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("c99_real_imag_opt", code), 0);
}

/// Converting a value to a truth value has to respect its type. Two ways it
/// did not:
///
/// A complex value is nonzero when *either* half is (C17 6.3.1.2 via
/// 6.5.3.3p5), but the condition reached `cbr` as a single 128-bit value and
/// only the low half -- the real part -- was tested.
///
/// A floating-point value is compared against `0.0`, not against its bit
/// pattern, so `-0.0` is false. It was reaching an *integer* compare against
/// an integer zero, and `-0.0`'s bit pattern is not zero.
///
/// `!` and `(_Bool)` were already right, which is what made the split
/// visible: there are several independent conversions and only some of them
/// looked at the type.
#[test]
fn c99_truth_value_respects_the_type() {
    let code = r#"
/* Opaque to the optimizer, so the condition is a real run-time value. */
static double neg_zero(void) { return -0.0; }
static double _Complex mkc(double r, double i) {
    double _Complex z = r;
    __imag__ z = i;
    return z;
}
static int take_bool(_Bool b) { return (int)b; }
static _Bool ret_bool(double _Complex z) { return z; }
static double _Complex both_zero_v(void) { return mkc(0.0, 0.0); }
/* A complex value returned from a real-typed function keeps its real part
   (C17 6.3.1.7p2), rather than the address it travels by. */
static double real_of(double _Complex z) { return z; }

int main(void) {
    /* ---- floating point: -0.0 is false ---- */
    double n = neg_zero();
    if (n) return 1;
    if (n && 1) return 2;
    if (n || 0) return 3;
    if (!n != 1) return 4;
    if ((int)(_Bool)n != 0) return 5;
    if (n ? 1 : 0) return 6;
    while (n) return 7;
    for (; n; ) return 8;
    { int hit = 0; switch (1) { case 1: while (n) { hit = 1; break; } } if (hit) return 9; }

    /* a nonzero double is still true */
    double h = 0.5;
    if (!h) return 10;
    if (!(h && 1)) return 11;

    /* ---- complex: either half counts ---- */
    double _Complex imag_only = mkc(0.0, 3.0);
    if (!imag_only) return 20;
    if (!(imag_only && 1)) return 21;
    if (!(imag_only || 0)) return 22;
    if (!imag_only != 0) return 23;
    if ((int)(_Bool)imag_only != 1) return 24;
    if (!(imag_only ? 1 : 0)) return 25;
    { int hit = 0; while (imag_only) { hit = 1; break; } if (!hit) return 26; }
    { int hit = 0; for (; imag_only; ) { hit = 1; break; } if (!hit) return 27; }
    { int hit = 0; switch (1) { case 1: while (imag_only) { hit = 1; break; } } if (!hit) return 28; }
    /* every spelling of the conversion, not just the cast */
    { _Bool b = imag_only; if ((int)b != 1) return 29; }
    { _Bool b; b = imag_only; if ((int)b != 1) return 60; }
    { if (take_bool(imag_only) != 1) return 61; }
    { if (ret_bool(imag_only) != 1) return 62; }
    { _Bool b = both_zero_v(); if ((int)b != 0) return 63; }

    double _Complex real_only = mkc(3.0, 0.0);
    if (!real_only) return 30;

    double _Complex both_zero = mkc(0.0, 0.0);
    if (both_zero) return 40;
    if ((int)(_Bool)both_zero != 0) return 41;
    if (!both_zero != 1) return 42;

    /* -0.0 in both halves is still zero */
    double _Complex neg_zeros = mkc(-0.0, -0.0);
    if (neg_zeros) return 50;
    if ((int)(_Bool)neg_zeros != 0) return 51;

    if (real_of(mkc(4.0, 9.0)) != 4.0) return 70;

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_truth_value", code, &[]), 0);
    assert_eq!(compile_and_run_optimized("c99_truth_value_opt", code), 0);
}

/// Only `==` and `!=` are defined on complex operands, and both have to look
/// at both halves. The complex arm of `linearize_binary` keyed off the
/// *result* type, which for a comparison is `int`, so equality fell into the
/// scalar path and compared whatever the low half held.
#[test]
fn c99_complex_equality_compares_both_halves() {
    let code = r#"
static double _Complex mkc(double r, double i) {
    double _Complex z = r;
    __imag__ z = i;
    return z;
}

int main(void) {
    double _Complex a = mkc(1.0, 2.0);
    double _Complex same = mkc(1.0, 2.0);
    double _Complex imag_differs = mkc(1.0, 9.0);
    double _Complex real_differs = mkc(7.0, 2.0);
    double _Complex both_differ = mkc(7.0, 9.0);

    if (!(a == same)) return 1;
    if (a == imag_differs) return 2;
    if (a == real_differs) return 3;
    if (a == both_differ) return 4;

    if (a != same) return 5;
    if (!(a != imag_differs)) return 6;
    if (!(a != real_differs)) return 7;
    if (!(a != both_differ)) return 8;

    /* against a real operand: the imaginary half must still count */
    if (!(mkc(0.0, 0.0) == 0)) return 9;
    if (mkc(0.0, 3.0) == 0) return 10;
    if (!(mkc(0.0, 3.0) != 0)) return 11;
    if (!(mkc(5.0, 0.0) == 5)) return 12;
    if (mkc(5.0, 1.0) == 5) return 13;

    /* -0.0 compares equal to 0.0 */
    if (!(mkc(-0.0, -0.0) == mkc(0.0, 0.0))) return 14;

    /* float _Complex, so the base width is not the pointer width */
    float _Complex f = 1.0f;
    __imag__ f = 2.0f;
    float _Complex g = 1.0f;
    __imag__ g = 3.0f;
    if (f == g) return 15;
    if (!(f != g)) return 16;

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_complex_equality", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("c99_complex_equality_opt", code),
        0
    );
}

/// A conditional whose result is complex.
///
/// Two defects met here. The parser held complex back from the usual
/// arithmetic conversions, so `c ? 1 : z` typed as `int` and dropped the
/// imaginary half outright while `c ? z : 1` did not -- the arm order decided
/// the type. And the linearizer phi-ed the arms *by value* where a complex
/// value travels by address everywhere else, so the merged pseudo's bits were
/// dereferenced as a pointer and every complex conditional that reached
/// codegen died, including the arm order that already typed correctly.
///
/// Both operators are covered: `?:` shares the shape and additionally has to
/// evaluate its left operand exactly once.
#[test]
fn c99_complex_conditional() {
    let code = r#"
#include <complex.h>

static int calls;
static int nz(void) { return 1; }               /* opaque: defeats folding */
static double _Complex f(double re) { calls++; return re + 1.0 * I; }

static int eq(double _Complex a, double re, double im) {
    return __real__ a == re && __imag__ a == im;
}

int main(void) {
    int c = nz();
    double _Complex z = 3.0 + 4.0 * I;
    double _Complex w = 5.0 - 6.0 * I;
    float _Complex fz = 1.0f + 2.0f * I;

    /* The type is the common type whichever arm the complex one is. */
    if (sizeof(c ? 1 : z) != sizeof(double _Complex)) return 1;
    if (sizeof(c ? z : 1) != sizeof(double _Complex)) return 2;
    if (sizeof(c ? 1 : z) != sizeof(c ? z : 1)) return 3;
    /* Mixed precision widens to the greater, again either way round. */
    if (sizeof(c ? fz : z) != sizeof(double _Complex)) return 4;
    if (sizeof(c ? z : fz) != sizeof(double _Complex)) return 5;
    /* A real arm against float _Complex stays float _Complex. */
    if (sizeof(c ? 1.0f : fz) != sizeof(float _Complex)) return 6;

    /* Both halves survive the merge, on both branches. */
    if (!eq(c ? z : w, 3.0, 4.0)) return 10;
    if (!eq(nz() - 1 ? z : w, 5.0, -6.0)) return 11;

    /* A real arm is converted, not truncated to the real part of the other. */
    if (!eq(c ? z : 1, 3.0, 4.0)) return 12;
    if (!eq(c ? 1 : z, 1.0, 0.0)) return 13;
    if (!eq(nz() - 1 ? 1 : z, 3.0, 4.0)) return 14;

    /* Mixed precision reads the narrow arm with its own stride. */
    if (!eq(c ? fz : z, 1.0, 2.0)) return 15;
    if (!eq(nz() - 1 ? fz : z, 3.0, 4.0)) return 16;

    /* A constant condition takes one arm outright. */
    if (!eq(1 ? z : w, 3.0, 4.0)) return 17;
    if (!eq(0 ? z : w, 5.0, -6.0)) return 18;
    if (!eq(0 ? 1 : z, 3.0, 4.0)) return 19;

    /* Consumed in place rather than stored: the result must be an address the
       caller can read halves through. */
    if (__real__ (c ? z : w) != 3.0) return 20;
    if (!eq((c ? z : w) + (c ? w : z), 8.0, -2.0)) return 21;

    /* GNU `?:` -- same merge, and the left operand evaluated exactly once. */
    calls = 0;
    if (!eq(f(3.0) ?: (7.0 + 8.0 * I), 3.0, 1.0)) return 30;
    if (calls != 1) return 31;

    /* f(0.0) is 0+1i, which is nonzero: the imaginary half counts. */
    calls = 0;
    if (!eq(f(0.0) ?: (7.0 + 8.0 * I), 0.0, 1.0)) return 32;
    if (calls != 1) return 33;

    /* Genuinely zero, so the right operand is taken -- still one call. */
    calls = 0;
    if (!eq(f(0.0) - 1.0 * I ?: (7.0 + 8.0 * I), 7.0, 8.0)) return 34;
    if (calls != 1) return 35;

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_complex_conditional", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("c99_complex_conditional_opt", code),
        0
    );
}

/// A complex conditional is merged **by address**, and the type is named.
///
/// The behavioural test above proves the answers; this pins the shape that
/// makes them right. A complex value travels by address everywhere in the
/// linearizer, so the conditional's arms must reach the merge as addresses and
/// the phi must be pointer-wide -- exactly what `return a;` already produced.
/// Phi-ing the loaded 128-bit value instead is what got those bits
/// dereferenced as a pointer.
///
/// Also asserted here: `_Complex` is a modifier rather than a kind, so the
/// type speller dropped it and named `double _Complex` as plain `double` --
/// in the IR dump and, more importantly, in every diagnostic.
#[test]
fn c99_complex_conditional_merges_by_address() {
    use crate::common::run_c17;

    let dir = tempfile::Builder::new()
        .prefix("c17_complex_ir_")
        .tempdir()
        .expect("work dir");
    let src = dir.path().join("t.c");
    std::fs::write(
        &src,
        "double _Complex pick(int c, double _Complex a, double _Complex b)\n\
         { return c ? a : b; }\n",
    )
    .expect("write source");

    // An explicit target so "pointer width" is a known number.
    let r = run_c17(&[
        "--target",
        "x86_64-unknown-linux-gnu",
        "--dump-ir",
        "post-linearize",
        "-o",
        "/dev/null",
        &src.to_string_lossy(),
    ]);
    let ir = format!("{}{}", r.stdout, r.stderr);

    assert!(
        ir.contains("phi.64"),
        "the arms merge as addresses, so the phi is pointer-wide:\n{ir}"
    );
    assert!(
        !ir.contains("phi.128"),
        "a 128-bit phi means the arms were merged by value:\n{ir}"
    );
    assert!(
        !ir.contains("load.128"),
        "neither arm should load the complex object to merge it:\n{ir}"
    );
    assert!(
        ir.contains("double _Complex"),
        "the type speller must not drop the _Complex modifier:\n{ir}"
    );
}

/// GNU `a ?: b` where only the *result* is complex.
///
/// The complex path was dispatched on the result type but then assumed the
/// left operand was complex too. It need not be: the result is complex as soon
/// as *either* operand is, so `d ?: z` has a `double` left operand and a
/// `double _Complex` result.
///
/// Taking a real operand's address as though it were a complex object read the
/// neighbouring stack slot as the imaginary half, and for an rvalue
/// `rvalue_addr` hands back the value's own bits -- so `g() ?: z`
/// dereferenced a `double` as a pointer and died. The truth test was wrong too:
/// an `int` whose bits happen to spell `-0.0f` compared equal to zero.
#[test]
fn c99_complex_elvis_with_a_real_left_operand() {
    let code = r#"
#include <complex.h>

static int calls;
static double g(double v) { calls++; return v; }

static int eq(double _Complex a, double re, double im) {
    return __real__ a == re && __imag__ a == im;
}

int main(void) {
    double _Complex z = 7.0 + 8.0 * I;

    /* A real lvalue: the imaginary half must be zero, not the next slot. */
    double d = 3.0;
    if (!eq(d ?: z, 3.0, 0.0)) return 1;
    if (sizeof(d ?: z) != sizeof(double _Complex)) return 2;

    /* An integer lvalue, converted to the result's base type. */
    int iv = 7;
    if (!eq(iv ?: z, 7.0, 0.0)) return 3;

    /* INT_MIN's bit pattern is -0.0f: a float compare against zero on the
       raw bits would call this false and take the wrong arm. */
    int neg = -2147483647 - 1;
    if (!eq(neg ?: z, (double)neg, 0.0)) return 4;

    /* A real *rvalue* has no address of its own to take. */
    calls = 0;
    if (!eq(g(2.5) ?: z, 2.5, 0.0)) return 5;
    if (calls != 1) return 6;

    /* Zero takes the right-hand operand -- still exactly one evaluation. */
    calls = 0;
    if (!eq(g(0.0) ?: z, 7.0, 8.0)) return 7;
    if (calls != 1) return 8;

    /* A complex left operand still works: the arm that already existed. */
    calls = 0;
    if (!eq(z ?: (1.0 + 2.0 * I), 7.0, 8.0)) return 9;

    /* Mixed precision, real left operand. Checked in `float _Complex` rather
       than through `eq`: passing a `float _Complex` argument to a
       `double _Complex` parameter is separately broken, and routing through
       it would test that instead of this. */
    float _Complex fz = 1.0f + 2.0f * I;
    float ff = 4.0f;
    float _Complex fr = ff ?: fz;
    if (__real__ fr != 4.0f || __imag__ fr != 0.0f) return 10;
    if (sizeof(ff ?: fz) != sizeof(float _Complex)) return 11;

    float fzero = 0.0f;
    fr = fzero ?: fz;
    if (__real__ fr != 1.0f || __imag__ fr != 2.0f) return 12;

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_complex_elvis_real_left", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("c99_complex_elvis_real_left_opt", code),
        0
    );
}

/// A complex value crossing a call boundary is converted to the precision the
/// other side declared.
///
/// A complex value is read with its base type's stride, so the two sides must
/// agree on which base type that is. Assignment, initialization and the binary
/// operators all went through `complex_operand_at_precision`; the **argument**
/// and **return** paths did not, and handed the storage over unconverted. A
/// `float _Complex` given to a `double _Complex` parameter had the callee read
/// an 8-byte-strided pair out of 4-byte-strided memory, so `1.0f + 2.0f*I`
/// arrived as `2+1i` -- and the wider directions read past the object.
#[test]
fn c99_complex_precision_crosses_calls() {
    let code = r#"
#include <complex.h>

static int eq(double re, double im, double want_re, double want_im) {
    return re == want_re && im == want_im;
}

static int take_d(double _Complex a) { return eq(__real__ a, __imag__ a, 1.0, 2.0); }
static int take_f(float _Complex a) { return eq(__real__ a, __imag__ a, 1.0, 2.0); }
static int take_ld(long double _Complex a) { return eq(__real__ a, __imag__ a, 1.0, 2.0); }

/* noinline so the argument really crosses a call; the inlined form is
   covered separately below. */
__attribute__((noinline)) static int ni_d(double _Complex a) { return take_d(a); }
__attribute__((noinline)) static int ni_f(float _Complex a) { return take_f(a); }

static double _Complex widen(float _Complex x) { return x; }
static float _Complex narrow(double _Complex x) { return x; }

int main(void) {
    float _Complex f = 1.0f + 2.0f * I;
    double _Complex d = 1.0 + 2.0 * I;
    long double _Complex l = 1.0L + 2.0L * I;

    /* Every precision into every parameter width. */
    if (!take_d(f)) return 1;
    if (!take_d(d)) return 2;
    if (!take_d(l)) return 3;
    if (!take_f(d)) return 4;
    if (!take_f(f)) return 5;
    if (!take_f(l)) return 6;
    if (!take_ld(f)) return 7;
    if (!take_ld(d)) return 8;
    if (!take_ld(l)) return 9;

    /* Across a call the optimizer cannot fold away. */
    if (!ni_d(f)) return 10;
    if (!ni_f(d)) return 11;

    /* Returns convert too, in both directions. */
    double _Complex w = widen(f);
    if (!eq(__real__ w, __imag__ w, 1.0, 2.0)) return 12;
    float _Complex n = narrow(d);
    if (!eq(__real__ n, __imag__ n, 1.0, 2.0)) return 13;

    /* The paths that already worked, kept as regression guards. */
    double _Complex a = f;
    if (!eq(__real__ a, __imag__ a, 1.0, 2.0)) return 14;
    double _Complex s = f + d;
    if (!eq(__real__ s, __imag__ s, 2.0, 4.0)) return 15;

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_complex_precision_calls", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("c99_complex_precision_calls_opt", code),
        0
    );
}

/// An inlined function's `_Complex` parameter is copied through its address.
///
/// The implicit parameter copies that stand in for the backend prologue decide
/// by *size* whether the caller's argument pseudo holds the value or a pointer
/// to it. That is right for an aggregate -- `struct { float a, b; }` fits in a
/// register and travels as its value -- and wrong for a `_Complex`, which
/// travels by address at every size. The eight-byte `float _Complex` is exactly
/// the same size as that struct, so it took the by-value path and the inlined
/// body read the pointer as a pair of floats.
///
/// Only at -O2, where the inliner's threshold admits these functions.
#[test]
fn c99_inlined_complex_param_is_copied_through_its_address() {
    let code = r#"
#include <complex.h>

/* Each taker has one call site: several change the inliner's answer. */
static int t_f(float _Complex a) { return __real__ a == 1.0f && __imag__ a == 2.0f; }
static int t_d(double _Complex a) { return __real__ a == 1.0 && __imag__ a == 2.0; }
static int t_ld(long double _Complex a) { return __real__ a == 1.0L && __imag__ a == 2.0L; }

/* The register-sized aggregate that must keep travelling by value. */
struct F2 { float a, b; };
static int t_s(struct F2 s) { return s.a == 1.0f && s.b == 2.0f; }

int main(void) {
    float _Complex f = 1.0f + 2.0f * I;
    double _Complex d = 1.0 + 2.0 * I;
    long double _Complex l = 1.0L + 2.0L * I;
    struct F2 s;
    s.a = 1.0f;
    s.b = 2.0f;

    if (!t_f(f)) return 1;
    if (!t_d(d)) return 2;
    if (!t_ld(l)) return 3;
    if (!t_s(s)) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_inlined_complex_param", code, &[]), 0);
    // Explicitly -O2: `compile_and_run_optimized` builds at -O1, and the
    // inliner's size threshold only admits these functions at -O2, so the
    // helper alone never reaches the defect.
    assert_eq!(
        compile_and_run("c99_inlined_complex_param_o2", code, &["-O2".to_string()]),
        0
    );
}
