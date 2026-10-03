//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 complex conversions: real to complex and complex to real, at every
// site that converts as if by assignment.

use crate::common::compile_and_run_everywhere;

/// Every conversion into or out of a complex type, at every site that makes
/// one: `return`, initialization (scalar, braced, aggregate member, static),
/// plain and compound assignment, a prototyped argument, a cast and a
/// conditional arm.
///
/// A complex value travels by address and a real one as its value, so a site
/// that converted with the scalar rule handed one representation where the
/// other belongs: `double _Complex f(void) { return 1; }` returned the number
/// 1 where its caller read an address and died, and `double d = z;` stored the
/// address's bit pattern converted to `double`.
#[test]
fn c99_complex_conversion_at_every_site() {
    let code = r#"
#define CHECK(n, v, re, im) do { if (__real__ (v) != (re) || __imag__ (v) != (im)) return (n); } while (0)

struct pair { int tag; float _Complex q; };
struct holder { double _Complex z; long double _Complex lz; };

/* Real operands returned from complex functions: static (inlined at -O2)
   and extern, from every real type, at every complex precision. */
static double _Complex ret_int_s(void) { return 1; }
double _Complex ret_dbl_e(double d) { return d; }
static float _Complex ret_flt_s(void) { return 3.5f; }
float _Complex ret_dbl_to_f(void) { return 4.5; }
long double _Complex ret_ld_e(void) { return 5.0L; }
static long double _Complex ret_int_ld(int i) { return i; }
static _Complex int ret_cint(long v) { return v; }
static double _Complex ret_cond(int x, double _Complex z) { return x ? 1 : z; }
/* Complex of another precision, and a complex constant. */
double _Complex ret_prec(float _Complex z) { return z; }
static float _Complex ret_narrow(long double _Complex z) { return z; }
double _Complex ret_const(void) { return 1.0 + 2.0i; }

/* Complex operands returned from, and passed to, real ones. */
static int ret_to_int(double _Complex z) { return z; }
float ret_to_float(long double _Complex z) { return z; }
_Bool ret_to_bool(double _Complex z) { return z; }
double take_real(double d) { return d; }
static int take_int(int i) { return i; }
_Bool take_bool(_Bool b) { return b; }
double _Complex take_c(double _Complex z) { return z; }
static long double _Complex take_lc(long double _Complex z) { return z; }
float _Complex take_fc(float _Complex z) { return z; }

int main(void) {
    volatile double one = 1.0;
    double _Complex z = 3.5 + 4.0i;
    double _Complex r;

    r = ret_int_s();            CHECK(1, r, 1, 0);
    r = ret_dbl_e(2.5);         CHECK(2, r, 2.5, 0);
    float _Complex f = ret_flt_s(); CHECK(3, f, 3.5f, 0);
    f = ret_dbl_to_f();         CHECK(4, f, 4.5f, 0);
    long double _Complex l = ret_ld_e(); CHECK(5, l, 5.0L, 0);
    l = ret_int_ld(6);          CHECK(6, l, 6.0L, 0);
    _Complex int ci = ret_cint(7); CHECK(7, ci, 7, 0);
    r = ret_cond(1, z);         CHECK(8, r, 1, 0);
    r = ret_cond(0, z);         CHECK(9, r, 3.5, 4);
    r = ret_prec(1.5f + 2.5fi); CHECK(10, r, 1.5, 2.5);
    f = ret_narrow(7.0L + 8.0Li); CHECK(11, f, 7, 8);
    r = ret_const();            CHECK(12, r, 1, 2);

    if (ret_to_int(z) != 3) return 13;
    if (ret_to_float(9.25L + 1.0Li) != 9.25f) return 14;
    if (ret_to_bool(0.0 + 1.0i) != 1) return 15;
    if (ret_to_bool(0.0 * one) != 0) return 16;

    /* Initialization and assignment, each direction. */
    double d = z;               if (d != 3.5) return 17;
    long n = z;                 if (n != 3) return 18;
    float fl; fl = z;           if (fl != 3.5f) return 19;
    _Bool b = 0.0 + 2.0i;       if (b != 1) return 20;
    double _Complex a = 1;      CHECK(21, a, 1, 0);
    a = 2;                      CHECK(22, a, 2, 0);
    a = 3.5f;                   CHECK(23, a, 3.5, 0);
    long double _Complex c = 6; CHECK(24, c, 6, 0);
    c = f;                      CHECK(25, c, 7, 8);
    double _Complex *p = &a; *p = 17; CHECK(26, a, 17, 0);
    d = 1 ? z : 2;              if (d != 3.5) return 27;

    /* Compound assignment, each direction. */
    a = 1.0 + 2.0i;
    a += 1.0;                   CHECK(28, a, 2, 2);
    a *= 2;                     CHECK(29, a, 4, 4);
    d = 1.0;
    d += z;                     if (d != 4.5) return 30;
    n = 10;
    n *= z;                     if (n != 35) return 31;
    b = 0;
    b += 0.0 + 1.0i;            if (b != 1) return 32;

    /* Initializer lists, local and static. */
    double darr[2] = { z, 1 };  if (darr[0] != 3.5 || darr[1] != 1) return 33;
    double _Complex carr[2] = { 14, 15.0f }; CHECK(34, carr[0], 14, 0); CHECK(35, carr[1], 15, 0);
    struct pair s = { 1, 16 };  CHECK(36, s.q, 16, 0);
    struct holder h = { 2.0f, 3 }; CHECK(37, h.z, 2, 0); CHECK(38, h.lz, 3, 0);
    struct { double re; } sr = { z }; if (sr.re != 3.5) return 39;
    static double _Complex sc = 13; CHECK(40, sc, 13, 0);
    double _Complex braced = { 2.5 }; CHECK(41, braced, 2.5, 0);

    /* Arguments, each direction. */
    if (take_real(z) != 3.5) return 42;
    if (take_int(z) != 3) return 43;
    if (take_bool(0.0 + 1.0i) != 1) return 44;
    r = take_c(3);              CHECK(45, r, 3, 0);
    l = take_lc(3.0f);          CHECK(46, l, 3, 0);
    f = take_fc(z);             CHECK(47, f, 3.5f, 4);

    /* Casts and conditionals. */
    r = (double _Complex)19;    CHECK(48, r, 19, 0);
    ci = (_Complex int)5;       CHECK(49, ci, 5, 0);
    int k = 2;
    a = k ? 18 : a;             CHECK(50, a, 18, 0);
    return 0;
}
"#;
    compile_and_run_everywhere("complex_conversion_sites", code);
}
