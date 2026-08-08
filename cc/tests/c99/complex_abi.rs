//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Complex-type calling convention (audit #C1, #C2).
//
// System V AMD64 §3.2.3 classifies the three complex types differently, and
// the classifier returned two SSE eightbytes for all of them:
//
//   float _Complex        8 bytes  -> ONE  SSE eightbyte (both floats packed)
//   double _Complex      16 bytes  -> TWO  SSE eightbytes (xmm0, xmm1)
//   long double _Complex 32 bytes  -> MEMORY (args), st(0)/st(1) (return)
//
// Storage and returns were already right; passing was not. These tests cover
// construction, argument passing, return, and round trips, for each base type.
//

use crate::common::compile_and_run;

/// #C1: `float _Complex` is one packed eightbyte, so it occupies a single
/// XMM register. Passing it in two registers left the imaginary part in a
/// register the callee never reads — `cimagf` returned 0 while `crealf`
/// happened to be right, which is exactly the shape of a silent miscompile.
#[test]
fn c99_complex_float_argument_passing() {
    let src = r#"
        #include <complex.h>
        /* Read through a pointer rather than crealf/cimagf, so the test
           exercises our ABI on both sides rather than libm's. */
        static float re(float _Complex z) { float *f = (float *)&z; return f[0]; }
        static float im(float _Complex z) { float *f = (float *)&z; return f[1]; }
        static float _Complex make(void) { return __builtin_complex(6.0f, 7.0f); }

        int main(void) {
            float _Complex a = __builtin_complex(2.0f, 3.0f);

            /* storage */
            float *f = (float *)&a;
            if (f[0] != 2.0f || f[1] != 3.0f) return 1;

            /* argument */
            if (re(a) != 2.0f) return 2;
            if (im(a) != 3.0f) return 3;

            /* return */
            float _Complex b = make();
            float *g = (float *)&b;
            if (g[0] != 6.0f || g[1] != 7.0f) return 4;

            /* round trip through both */
            if (re(make()) != 6.0f || im(make()) != 7.0f) return 5;

            /* and the standard accessors, which go through libm */
            if (crealf(a) != 2.0f || cimagf(a) != 3.0f) return 6;
            return 0;
        }
    "#;
    assert_eq!(
        compile_and_run("c99_complex_float_abi", src, &["-lm".to_string()]),
        0
    );
}

/// `double _Complex` is two SSE eightbytes. Returns already worked; passing
/// did not, so a callee saw zeros.
#[test]
fn c99_complex_double_argument_passing() {
    let src = r#"
        #include <complex.h>
        static double re(double _Complex z) { double *d = (double *)&z; return d[0]; }
        static double im(double _Complex z) { double *d = (double *)&z; return d[1]; }
        static double _Complex make(void) { return __builtin_complex(6.0, 7.0); }

        int main(void) {
            double _Complex a = __builtin_complex(2.0, 3.0);

            double *d = (double *)&a;
            if (d[0] != 2.0 || d[1] != 3.0) return 1;

            if (re(a) != 2.0) return 2;
            if (im(a) != 3.0) return 3;

            double _Complex b = make();
            double *e = (double *)&b;
            if (e[0] != 6.0 || e[1] != 7.0) return 4;

            if (re(make()) != 6.0 || im(make()) != 7.0) return 5;

            if (creal(a) != 2.0 || cimag(a) != 3.0) return 6;
            return 0;
        }
    "#;
    assert_eq!(
        compile_and_run("c99_complex_double_abi", src, &["-lm".to_string()]),
        0
    );
}

/// #C2: `long double _Complex` is COMPLEX_X87 — passed in memory, never in
/// XMM registers. Routing it through the SSE path made the emitter build the
/// mnemonic `mov` + the x87 size suffix `t`, producing `movt %xmm0, ...`,
/// which the assembler rejects outright (`no such instruction`). Any
/// translation unit that so much as constructed one failed to build.
#[test]
fn c99_complex_long_double_argument_passing() {
    let src = r#"
        static long double re(long double _Complex z) {
            long double *p = (long double *)&z; return p[0];
        }
        static long double im(long double _Complex z) {
            long double *p = (long double *)&z; return p[1];
        }
        static long double _Complex make(void) {
            return __builtin_complex(6.0L, 7.0L);
        }

        int main(void) {
            long double _Complex a = __builtin_complex(2.0L, 3.0L);

            long double *p = (long double *)&a;
            if (p[0] != 2.0L || p[1] != 3.0L) return 1;

            if (re(a) != 2.0L) return 2;
            if (im(a) != 3.0L) return 3;

            long double _Complex b = make();
            long double *q = (long double *)&b;
            if (q[0] != 6.0L || q[1] != 7.0L) return 4;

            if (re(make()) != 6.0L || im(make()) != 7.0L) return 5;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c99_complex_ld_abi", src, &[]), 0);
}

/// Complex arithmetic, including the multiply that is lowered to libgcc's
/// `__mul?c3`. For `long double` that call returns COMPLEX_X87 in st(0)/st(1),
/// so this is what pins the return convention against the real library.
///
/// Each literal is built at its own precision. Writing `6.0L + 7.0L*I` would
/// instead exercise a *cross-precision* complex conversion, because `I` is a
/// `double _Complex` — that path is separately broken and is recorded as #C4
/// in cc/audit.md rather than tested here.
#[test]
fn c99_complex_arithmetic_compiles_and_works() {
    let src = r#"
        int main(void) {
            float _Complex fa = __builtin_complex(1.0f, 2.0f);
            float _Complex fb = __builtin_complex(3.0f, 4.0f);
            float _Complex fs = fa + fb;
            float *pf = (float *)&fs;
            if (pf[0] != 4.0f || pf[1] != 6.0f) return 1;
            float _Complex fm = fa * fb;
            pf = (float *)&fm;
            if (pf[0] != -5.0f || pf[1] != 10.0f) return 2;

            double _Complex da = __builtin_complex(1.0, 2.0);
            double _Complex db = __builtin_complex(3.0, 4.0);
            double _Complex dm = da * db;
            double *pd = (double *)&dm;
            if (pd[0] != -5.0 || pd[1] != 10.0) return 3;

            long double _Complex la = __builtin_complex(1.0L, 2.0L);
            long double _Complex lb = __builtin_complex(3.0L, 4.0L);
            long double _Complex ls = la + lb;
            long double *pl = (long double *)&ls;
            if (pl[0] != 4.0L || pl[1] != 6.0L) return 4;
            /* Goes through __mulxc3, which returns in st(0)/st(1). */
            long double _Complex lm = la * lb;
            pl = (long double *)&lm;
            if (pl[0] != -5.0L || pl[1] != 10.0L) return 5;
            return 0;
        }
    "#;
    assert_eq!(
        compile_and_run("c99_complex_arith", src, &["-lm".to_string()]),
        0
    );
}

/// A complex argument mixed with other arguments, so the register allocator
/// has to advance the FP index by the right amount. `float _Complex` consumes
/// **one** SSE register, not two; getting that wrong shifts every later
/// floating-point argument.
#[test]
fn c99_complex_argument_register_accounting() {
    let src = r#"
        static int check_f(float _Complex z, float after) {
            float *f = (float *)&z;
            return (f[0] == 1.0f && f[1] == 2.0f && after == 9.0f) ? 0 : 1;
        }
        static int check_d(double _Complex z, double after) {
            double *d = (double *)&z;
            return (d[0] == 1.0 && d[1] == 2.0 && after == 9.0) ? 0 : 1;
        }
        static int check_two(float _Complex a, float _Complex b) {
            float *x = (float *)&a, *y = (float *)&b;
            return (x[0] == 1.0f && x[1] == 2.0f && y[0] == 3.0f && y[1] == 4.0f) ? 0 : 1;
        }
        int main(void) {
            if (check_f(__builtin_complex(1.0f, 2.0f), 9.0f)) return 1;
            if (check_d(__builtin_complex(1.0, 2.0), 9.0)) return 2;
            if (check_two(__builtin_complex(1.0f, 2.0f),
                          __builtin_complex(3.0f, 4.0f))) return 3;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c99_complex_reg_accounting", src, &[]), 0);
}
