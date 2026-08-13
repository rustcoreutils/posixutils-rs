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
