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

// ============================================================================
// Value-versus-address regressions found in review of the #C1/#C2 fix
// ============================================================================

/// A complex object's stack slot *is* its storage, but a temporary's slot
/// holds a pointer to it. The #C1/#C2 fix switched the complex paths from
/// `linearize_lvalue` to `linearize_expr` to make rvalues work, and applied
/// the `is_lvalue` guard on the argument path but not on assignment — so
/// `g = w` between two complex variables loaded the float bits and used them
/// as the source address.
#[test]
fn c99_complex_assignment_between_lvalues() {
    let src = r#"
        static float _Complex fg;
        static double _Complex dg;
        static long double _Complex lg;
        struct box { double _Complex z; };

        static void store(double _Complex a, double _Complex *p) { *p = a; }

        int main(void) {
            /* variable -> global, each base type */
            float _Complex fw = __builtin_complex(1.0f, 2.0f);
            fg = fw;
            float *pf = (float *)&fg;
            if (pf[0] != 1.0f || pf[1] != 2.0f) return 1;

            double _Complex dw = __builtin_complex(3.0, 4.0);
            dg = dw;
            double *pd = (double *)&dg;
            if (pd[0] != 3.0 || pd[1] != 4.0) return 2;

            long double _Complex lw = __builtin_complex(5.0L, 6.0L);
            lg = lw;
            long double *pl = (long double *)&lg;
            if (pl[0] != 5.0L || pl[1] != 6.0L) return 3;

            /* through a pointer, i.e. `*p = a` */
            double _Complex out;
            store(dw, &out);
            double *po = (double *)&out;
            if (po[0] != 3.0 || po[1] != 4.0) return 4;

            /* from a struct member, and back into one */
            struct box b;
            b.z = dw;
            double _Complex fromMember = b.z;
            double *pm = (double *)&fromMember;
            if (pm[0] != 3.0 || pm[1] != 4.0) return 5;

            /* from a dereference */
            double _Complex *q = &dw;
            double _Complex deref = *q;
            double *pq = (double *)&deref;
            if (pq[0] != 3.0 || pq[1] != 4.0) return 6;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c99_complex_assign_lvalue", src, &[]), 0);
}

/// The same value-versus-address confusion on the `return` path: returning a
/// complex *rvalue* (the result of a call, or of arithmetic) must not ask for
/// its address as though it were a variable.
#[test]
fn c99_complex_return_of_an_rvalue() {
    let src = r#"
        static double _Complex mk(double a, double b) {
            return __builtin_complex(a, b);
        }
        /* returns the result of another call — an rvalue */
        static double _Complex fwd(void) { return mk(3.0, 4.0); }
        /* returns the result of arithmetic — also an rvalue */
        static double _Complex sum(double _Complex x, double _Complex y) {
            return x + y;
        }
        static float _Complex fmk(float a, float b) {
            return __builtin_complex(a, b);
        }
        static float _Complex ffwd(void) { return fmk(1.5f, 2.5f); }
        static long double _Complex lmk(long double a, long double b) {
            return __builtin_complex(a, b);
        }
        static long double _Complex lfwd(void) { return lmk(7.0L, 8.0L); }

        int main(void) {
            double _Complex r = fwd();
            double *p = (double *)&r;
            if (p[0] != 3.0 || p[1] != 4.0) return 1;

            double _Complex s = sum(__builtin_complex(1.0, 2.0),
                                    __builtin_complex(10.0, 20.0));
            p = (double *)&s;
            if (p[0] != 11.0 || p[1] != 22.0) return 2;

            float _Complex fr = ffwd();
            float *pf = (float *)&fr;
            if (pf[0] != 1.5f || pf[1] != 2.5f) return 3;

            long double _Complex lr = lfwd();
            long double *pl = (long double *)&lr;
            if (pl[0] != 7.0L || pl[1] != 8.0L) return 4;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c99_complex_return_rvalue", src, &[]), 0);
}

/// A complex argument that does not fit in the remaining FP registers is
/// passed in memory. Both halves have to agree: the caller must write the
/// value to the stack, and the callee's prologue must copy it into the local.
/// Previously the prologue's register guard simply skipped the copy when the
/// value had spilled, leaving the parameter uninitialized, and shifted every
/// argument after it.
///
/// x86_64 only. AArch64 has the same gap — `aarch64/codegen.rs`'s prologue
/// likewise skips the copy when `fp_arg_idx + 1` runs past the register file —
/// but the rule for what happens next is not the same one: AAPCS64 §6.4.2 sets
/// NSRN to 8 once an argument is laid out on the stack, so *every* later
/// floating-point argument follows it there, where System V leaves the unused
/// registers available. Closing it means implementing that, and the audit
/// records it (#H13) rather than guessing at it from the wrong architecture.
#[cfg(target_arch = "x86_64")]
#[test]
fn c99_complex_argument_spilled_to_the_stack() {
    let src = r#"
        static double wide(double a, double b, double c, double d,
                           double e, double f, double g, double h,
                           double _Complex z, double after) {
            double *p = (double *)&z;
            return a + b + c + d + e + f + g + h
                 + p[0] * 100.0 + p[1] * 1000.0 + after;
        }
        /* Seven doubles leave one XMM free — not enough for a two-eightbyte
           complex, so it goes to memory while `after` still gets a register. */
        static double straddle(double a, double b, double c, double d,
                               double e, double f, double g,
                               double _Complex z, double after) {
            double *p = (double *)&z;
            return a + b + c + d + e + f + g
                 + p[0] * 100.0 + p[1] * 1000.0 + after;
        }
        static float narrow(float a, float b, float c, float d,
                            float e, float f, float g, float h,
                            float _Complex z, float after) {
            float *p = (float *)&z;
            return a + b + c + d + e + f + g + h
                 + p[0] * 100.0f + p[1] * 1000.0f + after;
        }

        int main(void) {
            if (wide(1, 1, 1, 1, 1, 1, 1, 1,
                     __builtin_complex(2.0, 3.0), 5.0) != 3213.0) return 1;
            if (straddle(1, 1, 1, 1, 1, 1, 1,
                         __builtin_complex(2.0, 3.0), 5.0) != 3212.0) return 2;
            if (narrow(1, 1, 1, 1, 1, 1, 1, 1,
                       __builtin_complex(2.0f, 3.0f), 5.0f) != 3213.0f) return 3;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c99_complex_arg_spilled", src, &[]), 0);
}

/// #C4: mixed-precision complex arithmetic and initialization.
///
/// This is not an exotic case — `<complex.h>` defines `I` as
/// `__builtin_complex(0.0, 1.0)`, a **double** complex, so the textbook
/// spelling `x + y*I` is a conversion at every precision except `double`.
/// Both the binary operator and the initializer read the source with the
/// *target's* base type and stride, so an 8-byte-strided value read with a
/// 16-byte stride picked up the wrong bytes: `3.0L * I` gave `0 + 9i`.
#[test]
fn c99_complex_mixed_precision() {
    let src = r#"
        #include <complex.h>
        int main(void) {
            /* the spelling every C book uses, at all three precisions */
            float _Complex f = 2.0f + 3.0f * I;
            float *pf = (float *)&f;
            if (pf[0] != 2.0f || pf[1] != 3.0f) return 1;

            double _Complex d = 2.0 + 3.0 * I;
            double *pd = (double *)&d;
            if (pd[0] != 2.0 || pd[1] != 3.0) return 2;

            long double _Complex l = 2.0L + 3.0L * I;
            long double *pl = (long double *)&l;
            if (pl[0] != 2.0L || pl[1] != 3.0L) return 3;

            /* the multiply alone, which is where the stride went wrong */
            long double _Complex m = 3.0L * I;
            pl = (long double *)&m;
            if (pl[0] != 0.0L || pl[1] != 3.0L) return 4;

            /* narrowing as well as widening */
            double _Complex wide = __builtin_complex(1.5, 2.5);
            float _Complex narrow = wide;
            pf = (float *)&narrow;
            if (pf[0] != 1.5f || pf[1] != 2.5f) return 5;

            long double _Complex wider = wide;
            pl = (long double *)&wider;
            if (pl[0] != 1.5L || pl[1] != 2.5L) return 6;

            /* assignment, not just initialization */
            float _Complex assigned;
            assigned = wide;
            pf = (float *)&assigned;
            if (pf[0] != 1.5f || pf[1] != 2.5f) return 7;

            /* both operands complex, different precisions */
            long double _Complex mixed = wider + wide;
            pl = (long double *)&mixed;
            if (pl[0] != 3.0L || pl[1] != 5.0L) return 8;
            return 0;
        }
    "#;
    assert_eq!(
        compile_and_run("c99_complex_mixed_precision", src, &["-lm".to_string()]),
        0
    );
}
