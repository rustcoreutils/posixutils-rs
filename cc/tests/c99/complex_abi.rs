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

use crate::common::{
    aarch64_cross_available, c17_object, compile_and_run, compile_and_run_aarch64,
    compile_and_run_optimized, create_c_file, cross_link_and_run, host_link_and_run,
    interop_aarch64, interop_host, run_c17,
};

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
/// in the cc audit (`git log --grep '#C4'`) rather than tested here.
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
/// Runs on both architectures. AArch64 had the same gap, but not the same
/// rule for what follows: AAPCS64 §6.4.2 sets NSRN to 8 once an argument is
/// laid out on the stack, so *every* later floating-point argument follows it
/// there, where System V leaves the unused registers available. Both sides now
/// implement that, so the `#[cfg(target_arch = "x86_64")]` this carried while
/// #H13 was open is gone -- macOS CI executes it on aarch64.
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

/// Assigning a *real* to a complex object is a conversion, not a copy.
///
/// C99 6.3.1.7 gives the result that value as its real part and a zero
/// imaginary part. The complex-assignment path took the address of the
/// right-hand side unconditionally, which treats a non-lvalue as one:
/// `dc = 1.0` emitted `movabsq $4607182418800017408, %r11` -- the bit pattern
/// of 1.0 -- and then dereferenced it, so every such assignment segfaulted.
///
/// Pre-existing on main; found while testing atomic aggregates.
#[test]
fn c99_assigning_a_real_to_a_complex_converts() {
    let src = r#"
        #include <string.h>

        double _Complex g;
        float _Complex gf;
        long double _Complex gl;

        int main(void) {
            /* Global, double. */
            g = 1.0;
            double parts[2];
            memcpy(parts, &g, sizeof parts);
            if (parts[0] != 1.0 || parts[1] != 0.0) return 1;

            /* Local. */
            double _Complex l = __builtin_complex(9.0, 9.0);
            l = 2.5;
            memcpy(parts, &l, sizeof parts);
            if (parts[0] != 2.5 || parts[1] != 0.0) return 2;

            /* An integer right-hand side converts too. */
            l = 3;
            memcpy(parts, &l, sizeof parts);
            if (parts[0] != 3.0 || parts[1] != 0.0) return 3;

            /* float _Complex, whose base is narrower than the value. */
            gf = 1.5;
            float fparts[2];
            memcpy(fparts, &gf, sizeof fparts);
            if (fparts[0] != 1.5f || fparts[1] != 0.0f) return 4;

            /* long double _Complex, whose base is wider. */
            gl = 4.25;
            long double lparts[2];
            memcpy(lparts, &gl, sizeof lparts);
            if (lparts[0] != 4.25L || lparts[1] != 0.0L) return 5;

            /* A previously-nonzero imaginary part must be cleared. */
            l = __builtin_complex(7.0, 8.0);
            l = 1.0;
            memcpy(parts, &l, sizeof parts);
            if (parts[1] != 0.0) return 6;

            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c99_real_to_complex_assign", src, &[]), 0);
}

/// A stack parameter after a `long double _Complex` is not read sixteen bytes
/// low.
///
/// `long double _Complex` is COMPLEX_X87: thirty-two bytes on the stack. The
/// allocator asked `kind()` whether the parameter was a `long double`, and that
/// answers the *base* kind for a complex type, so it took the plain
/// long-double branch and advanced the incoming-argument cursor by sixteen.
/// Every parameter after it then landed on the complex's upper half, and each
/// subsequent one was shifted a slot further.
///
/// Filed as not reproducible at -O2; it is. The original probe's callee was
/// being inlined, which removes the ABI from the question entirely.
#[test]
fn c99_complex_long_double_then_stack_scalars() {
    let src = r#"
__attribute__((noinline))
static void probe(long double _Complex v, long double re, long double im,
                  long double *o)
{
    const long double *p = (const long double *)&v;
    o[0] = p[0]; o[1] = p[1]; o[2] = re; o[3] = im;
}

__attribute__((noinline))
static long double three(long double _Complex v, long double a, long double b,
                         long double c)
{ return a * 100 + b * 10 + c; }

/* Two complexes back to back, then a scalar. */
__attribute__((noinline))
static long double after_two(long double _Complex u, long double _Complex v,
                             long double a)
{ return a; }

int main(void)
{
    long double _Complex z = __builtin_complex(7.0L, 8.0L);
    long double o[4];

    probe(z, 1.5L, 2.5L, o);
    if (o[0] != 7.0L || o[1] != 8.0L) return 1;   /* the complex itself */
    if (o[2] != 1.5L) return 2;                   /* read the imag half */
    if (o[3] != 2.5L) return 3;

    if (three(z, 1.0L, 2.0L, 3.0L) != 123.0L) return 4;
    if (after_two(z, z, 9.5L) != 9.5L) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_complex_ld_stack_scalars", src, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("c99_complex_ld_stack_scalars_opt", src),
        0
    );
}

/// A *real* argument bound to a complex parameter converts as if by
/// assignment (C17 6.5.2.2p2), so the imaginary half is a zero.
///
/// The argument path keyed on the *argument's* type, not the parameter's, so
/// this case never reached the complex arm at all: the raw scalar was passed
/// where the callee expected an address. With a floating parameter that
/// arrived as garbage -- `f(7)` read `0 + 3.2e-319i` -- and with a
/// `_Complex int` one the callee dereferenced the number 7 and died.
#[test]
fn c99_real_argument_to_a_complex_parameter() {
    let src = r#"
        static double re_d(double _Complex c) { return __real__ c; }
        static double im_d(double _Complex c) { return __imag__ c; }
        static float re_f(float _Complex c) { return __real__ c; }
        static float im_f(float _Complex c) { return __imag__ c; }
        static long double re_l(long double _Complex c) { return __real__ c; }
        static long double im_l(long double _Complex c) { return __imag__ c; }
        static int re_i(_Complex int c) { return __real__ c; }
        static int im_i(_Complex int c) { return __imag__ c; }
        static long re_cl(_Complex long c) { return __real__ c; }
        static long im_cl(_Complex long c) { return __imag__ c; }
        static unsigned re_u(_Complex unsigned c) { return __real__ c; }
        static unsigned im_u(_Complex unsigned c) { return __imag__ c; }

        int main(void) {
            /* An integer literal, which needs a conversion as well as a
               promotion. */
            if (re_d(7) != 7.0 || im_d(7) != 0.0) return 1;
            if (re_f(7) != 7.0f || im_f(7) != 0.0f) return 2;
            if (re_l(7) != 7.0L || im_l(7) != 0.0L) return 3;
            if (re_i(7) != 7 || im_i(7) != 0) return 4;
            if (re_cl(7) != 7 || im_cl(7) != 0) return 5;
            if (re_u(7) != 7u || im_u(7) != 0u) return 6;

            /* A floating value into an integer complex, and the reverse. */
            if (re_i(9.75) != 9 || im_i(9.75) != 0) return 7;
            if (re_d(9) != 9.0 || im_d(9) != 0.0) return 8;

            /* A variable rather than a constant, so nothing is folded. */
            double d = 2.5;
            if (re_d(d) != 2.5 || im_d(d) != 0.0) return 9;
            int n = 3;
            if (re_i(n) != 3 || im_i(n) != 0) return 10;
            if (re_d(n) != 3.0 || im_d(n) != 0.0) return 11;

            /* Narrowing on the way in: a `long` into a `_Complex int`. */
            long big = 5;
            if (re_i(big) != 5 || im_i(big) != 0) return 12;
            return 0;
        }
    "#;
    assert_eq!(compile_and_run("c99_real_arg_complex_param", src, &[]), 0);
    assert_eq!(
        compile_and_run("c99_real_arg_complex_param_o2", src, &["-O2".to_string()]),
        0
    );
}

// ============================================================================
// va_arg of a complex type
// ============================================================================

// A variadic callee reading every complex type, interleaved with an int and a
// double, over three rounds so the later arguments are on the stack. The
// caller is a separate translation unit, so each side can be built by c17 or
// by gcc: agreeing with gcc about the variadic ABI, not only with itself, is
// the point. Both halves pass under gcc on x86-64 and aarch64.
const VA_COMPLEX_CALLEE: &str = r#"
typedef __builtin_va_list va_list;
typedef float _Complex fc; typedef double _Complex dc; typedef long double _Complex lc;
typedef int _Complex ic; typedef _Float16 _Complex hc;
/* Reads, `rounds` times over: int, fc, double, dc, lc, ic, hc. Round r's
   values are offset by r, so a slot read from the wrong place is caught. */
int check(int rounds, ...) {
    va_list ap;
    __builtin_va_start(ap, rounds);
    for (int r = 0; r < rounds; r++) {
        if (__builtin_va_arg(ap, int) != 10 + r) return 1 + 10 * r;
        fc f = __builtin_va_arg(ap, fc);
        if (__real__ f != 1.5f + r || __imag__ f != -2.5f) return 2 + 10 * r;
        if (__builtin_va_arg(ap, double) != 3.25 + r) return 3 + 10 * r;
        dc d = __builtin_va_arg(ap, dc);
        if (__real__ d != 4.5 + r || __imag__ d != -5.5) return 4 + 10 * r;
        lc l = __builtin_va_arg(ap, lc);
        if (__real__ l != 6.5L + r || __imag__ l != -7.5L) return 5 + 10 * r;
        ic i = __builtin_va_arg(ap, ic);
        if (__real__ i != 8 + r || __imag__ i != -9) return 6 + 10 * r;
        hc h = __builtin_va_arg(ap, hc);
        if (__real__ h != (_Float16)(0.5f + r) || __imag__ h != (_Float16)-1.5f) return 7 + 10 * r;
    }
    __builtin_va_end(ap);
    return 0;
}
"#;

const VA_COMPLEX_CALLER: &str = r#"
typedef float _Complex fc; typedef double _Complex dc; typedef long double _Complex lc;
typedef int _Complex ic; typedef _Float16 _Complex hc;
int check(int rounds, ...);
#define ROUND(r) 10 + r, __builtin_complex(1.5f + r, -2.5f), 3.25 + r, \
    __builtin_complex(4.5 + r, -5.5), __builtin_complex(6.5L + r, -7.5L), \
    (ic)(8 + r) - 9 * (ic)1i, __builtin_complex((_Float16)(0.5f + r), (_Float16)-1.5f)
int main(void) {
    if (check(1, ROUND(0))) return 1;
    return check(3, ROUND(0), ROUND(1), ROUND(2));
}
"#;

#[test]
fn c99_complex_va_arg_c17_both_sides() {
    let program = format!("{VA_COMPLEX_CALLEE}\n{VA_COMPLEX_CALLER}");
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(&format!("va_complex{opt}"), &program, &[opt.to_string()]),
            0,
            "host {opt}"
        );
    }
}

/// The host half of the interop check, gcc and c17 on either side. Linux
/// x86-64 only, where the host gcc is gcc and the ABI is System V.
#[test]
fn c99_complex_va_arg_interoperates_with_gcc_host() {
    if !cfg!(all(target_os = "linux", target_arch = "x86_64")) {
        return;
    }
    let dir = plib::tmp::Builder::new()
        .prefix("va_complex_")
        .tempdir()
        .unwrap();
    for opt in ["-O0", "-O2"] {
        let callee = c17_object("va_callee", VA_COMPLEX_CALLEE, opt, dir.path());
        assert_eq!(
            host_link_and_run(
                "gcc_caller",
                &[&callee],
                &[VA_COMPLEX_CALLER],
                opt,
                dir.path()
            ),
            0,
            "gcc caller, c17 callee, {opt}"
        );
        let caller = c17_object("va_caller", VA_COMPLEX_CALLER, opt, dir.path());
        assert_eq!(
            host_link_and_run(
                "gcc_callee",
                &[&caller],
                &[VA_COMPLEX_CALLEE],
                opt,
                dir.path()
            ),
            0,
            "c17 caller, gcc callee, {opt}"
        );
    }
}

/// The aarch64 half: every pairing of c17 and aarch64 gcc, under qemu.
#[test]
fn c99_complex_va_arg_interoperates_with_gcc_aarch64() {
    if !aarch64_cross_available() {
        eprintln!("SKIP: no aarch64 cross toolchain");
        return;
    }
    let dir = plib::tmp::Builder::new()
        .prefix("va_complex_a64_")
        .tempdir()
        .unwrap();
    let callee_c = create_c_file("va_callee_a64", VA_COMPLEX_CALLEE);
    let caller_c = create_c_file("va_caller_a64", VA_COMPLEX_CALLER);
    let callee_src = callee_c.path().to_string_lossy().into_owned();
    let caller_src = caller_c.path().to_string_lossy().into_owned();
    for opt in ["-O0", "-O2"] {
        let asm = |src: &str, n: &str| {
            let s = dir.path().join(format!("{n}{opt}.s"));
            let run = run_c17(&[
                "--target",
                "aarch64-unknown-linux-gnu",
                opt,
                "-w",
                "-S",
                "-o",
                s.to_str().unwrap(),
                src,
            ]);
            assert!(run.success, "c17 failed on {n}:\n{}", run.stderr);
            s.to_string_lossy().into_owned()
        };
        let callee_s = asm(&callee_src, "callee");
        let caller_s = asm(&caller_src, "caller");
        assert_eq!(
            cross_link_and_run("va_cc", &[&caller_s, &callee_s]),
            0,
            "c17 both, {opt}"
        );
        assert_eq!(
            cross_link_and_run("va_gc", &[&caller_src, &callee_s]),
            0,
            "gcc caller, c17 callee, {opt}"
        );
        assert_eq!(
            cross_link_and_run("va_cg", &[&caller_s, &callee_src]),
            0,
            "c17 caller, gcc callee, {opt}"
        );
    }
}

// ============================================================================
// va_arg of the composites a complex value is classified alongside
// ============================================================================

// Complex integers of every width, HFAs of each float width (two, three and
// four members), small integer composites, mixed composites and one too large
// for registers, over three rounds so each is read from the register save area
// and from the stack. The aarch64 caller put a 9-16 byte integer composite
// whose address had been spilled into x6/x7 as the address's own bytes;
// va_arg of a complex integer read one half; and System V classified
// `_Float128 _Complex` as two SSE eightbytes where gcc, and c17's own call
// lowering, pass it in memory.
const VA_AGG_DECLS: &str = r#"
typedef __builtin_va_list va_list;
typedef signed char _Complex cc_t; typedef short _Complex cs_t; typedef long long _Complex cl_t;
struct HF { float a, b; }; struct HD { double a, b; }; struct HH { _Float16 a, b; };
struct HL { long double a, b; }; struct HF3 { float a, b, c; }; struct HD4 { double a, b, c, d; };
struct SI { int a, b; }; struct SI3 { int a, b, c; }; struct SL { long a, b; };
struct MIX { float f; int i; }; struct MD { double d; long l; }; struct BIG { long a, b, c; };
struct C1 { char c; };
#ifdef __APPLE__ /* Darwin has no _Float128 */
#define QC_ARG(r)
#else
typedef _Float128 _Complex qc_t;
#define QC_ARG(r) , __builtin_complex(8.5F128 + r, -8.5F128)
#endif
"#;

const VA_AGG_CALLEE: &str = r#"
int check(int rounds, ...) {
    va_list ap; __builtin_va_start(ap, rounds);
    for (int r = 0; r < rounds; r++) {
        int e = 100 * r;
        cc_t c = __builtin_va_arg(ap, cc_t); if (__real__ c != 1 + r || __imag__ c != -2) return e + 1;
        cs_t s = __builtin_va_arg(ap, cs_t); if (__real__ s != 3 + r || __imag__ s != -4) return e + 2;
        cl_t l = __builtin_va_arg(ap, cl_t); if (__real__ l != 5 + r || __imag__ l != -6) return e + 3;
        struct HF hf = __builtin_va_arg(ap, struct HF); if (hf.a != 1.5f + r || hf.b != -1.5f) return e + 4;
        struct HD hd = __builtin_va_arg(ap, struct HD); if (hd.a != 2.5 + r || hd.b != -2.5) return e + 5;
        struct HH hh = __builtin_va_arg(ap, struct HH); if (hh.a != (_Float16)(0.5f + r) || hh.b != (_Float16)-0.5f) return e + 6;
        struct HL hl = __builtin_va_arg(ap, struct HL); if (hl.a != 3.5L + r || hl.b != -3.5L) return e + 7;
        struct HF3 h3 = __builtin_va_arg(ap, struct HF3); if (h3.a != 1 + r || h3.b != 2 || h3.c != 3) return e + 8;
        struct HD4 h4 = __builtin_va_arg(ap, struct HD4); if (h4.a != 1 + r || h4.b != 2 || h4.c != 3 || h4.d != 4) return e + 9;
        struct SI si = __builtin_va_arg(ap, struct SI); if (si.a != 7 + r || si.b != -7) return e + 10;
        struct SI3 s3 = __builtin_va_arg(ap, struct SI3); if (s3.a != 8 + r || s3.b != -8 || s3.c != 9) return e + 11;
        struct SL sl = __builtin_va_arg(ap, struct SL); if (sl.a != 10 + r || sl.b != -10) return e + 12;
        struct MIX mx = __builtin_va_arg(ap, struct MIX); if (mx.f != 4.5f + r || mx.i != -11) return e + 13;
        struct MD md = __builtin_va_arg(ap, struct MD); if (md.d != 5.5 + r || md.l != -12) return e + 14;
        struct BIG bg = __builtin_va_arg(ap, struct BIG); if (bg.a != 13 + r || bg.b != -13 || bg.c != 14) return e + 15;
        struct C1 c1 = __builtin_va_arg(ap, struct C1); if (c1.c != 15 + r) return e + 16;
        double _Complex dc = __builtin_va_arg(ap, double _Complex); if (__real__ dc != 6.5 + r || __imag__ dc != -6.5) return e + 17;
        if (__builtin_va_arg(ap, double) != 7.25 + r) return e + 18;
        if (__builtin_va_arg(ap, int) != 16 + r) return e + 19;
#ifndef __APPLE__
        qc_t q = __builtin_va_arg(ap, qc_t); if (__real__ q != 8.5F128 + r || __imag__ q != -8.5F128) return e + 20;
#endif
    }
    __builtin_va_end(ap);
    return 0;
}
"#;

const VA_AGG_CALLER: &str = r#"
int check(int rounds, ...);
#define ROUND(r) (cc_t)((1 + r) - 2 * 1i), (cs_t)((3 + r) - 4 * 1i), (cl_t)(5 + r) - 6 * (cl_t)1i, \
  (struct HF){1.5f + r, -1.5f}, (struct HD){2.5 + r, -2.5}, (struct HH){0.5f + r, -0.5f}, \
  (struct HL){3.5L + r, -3.5L}, (struct HF3){1 + r, 2, 3}, (struct HD4){1 + r, 2, 3, 4}, \
  (struct SI){7 + r, -7}, (struct SI3){8 + r, -8, 9}, (struct SL){10 + r, -10}, \
  (struct MIX){4.5f + r, -11}, (struct MD){5.5 + r, -12}, (struct BIG){13 + r, -13, 14}, (struct C1){15 + r}, \
  __builtin_complex(6.5 + r, -6.5), 7.25 + r, 16 + r QC_ARG(r)
int main(void) {
    int rc = check(1, ROUND(0));
    if (rc) return rc;
    return check(3, ROUND(0), ROUND(1), ROUND(2));
}
"#;

#[test]
fn c99_va_arg_aggregates_c17_both_sides() {
    let program = format!("{VA_AGG_DECLS}\n{VA_AGG_CALLEE}\n{VA_AGG_CALLER}");
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(&format!("va_agg{opt}"), &program, &[opt.to_string()]),
            0,
            "host {opt}"
        );
    }
}

/// Linux x86-64 only, where the host gcc is gcc and the ABI is System V.
#[test]
fn c99_va_arg_aggregates_interoperate_with_gcc_host() {
    if !cfg!(all(target_os = "linux", target_arch = "x86_64")) {
        return;
    }
    interop_host(
        "va_agg",
        &format!("{VA_AGG_DECLS}\n{VA_AGG_CALLEE}"),
        &format!("{VA_AGG_DECLS}\n{VA_AGG_CALLER}"),
    );
}

#[test]
fn c99_va_arg_aggregates_interoperate_with_gcc_aarch64() {
    if !aarch64_cross_available() {
        eprintln!("SKIP: no aarch64 cross toolchain");
        return;
    }
    interop_aarch64(
        "va_agg",
        &format!("{VA_AGG_DECLS}\n{VA_AGG_CALLEE}"),
        &format!("{VA_AGG_DECLS}\n{VA_AGG_CALLER}"),
    );
}

// ============================================================================
// _Float128 _Complex through calls
// ============================================================================

// Returned from, passed to and computed by functions: segfaulted on x86-64,
// where _Float128 is not long double. Passes under gcc on both targets.
// Darwin has no `_Float128`, and c17 rejects it there, as clang does.
#[cfg(not(target_os = "macos"))]
const FLOAT128_COMPLEX_CALLS: &str = r#"
typedef _Float128 _Complex qc;
__attribute__((noinline)) qc mk(int a, double d) { return (qc)(a + 0.5F128) - (qc)(d * 1.0if128); }
__attribute__((noinline)) qc id(qc z) { return z; }
__attribute__((noinline)) qc mul(qc x, qc y) { return x * y; }
__attribute__((noinline)) qc dv(qc x, qc y) { return x / y; }
int main(void) {
    volatile int a = 3; volatile double d = 2.0;
    qc z = mk(a, d);
    if (__real__ z != 3.5F128 || __imag__ z != -2.0F128) return 1;
    qc w = id(z);
    if (__real__ w != 3.5F128 || __imag__ w != -2.0F128) return 2;
    qc p = mul(z, __builtin_complex((_Float128)1, (_Float128)1));
    if (__real__ p != 5.5F128 || __imag__ p != 1.5F128) return 3;
    qc q = dv(p, __builtin_complex((_Float128)1, (_Float128)1));
    if (__real__ q != 3.5F128 || __imag__ q != -2.0F128) return 4;
    return 0;
}
"#;

#[cfg(not(target_os = "macos"))]
#[test]
fn c99_float128_complex_through_calls() {
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(
                &format!("f128_complex{opt}"),
                FLOAT128_COMPLEX_CALLS,
                &[opt.to_string()]
            ),
            0,
            "host {opt}"
        );
    }
}

// ============================================================================
// _Float128 _Complex operations, across translation units
// ============================================================================

// On x86-64 `_Float128 _Complex` is MEMORY class both ways, so it is returned
// through the hidden pointer, and `__multc3`/`__divtc3` -- ordinary functions
// of that return type -- write their result the same way. Every operation is
// in the callee's unit, so each side can be built by c17 or by gcc. The `E`
// half is one only binary128 holds: an eight-byte move, or a trip through
// `double` or the x87 format, loses it.
const F128C_DECLS: &str = r#"
typedef __builtin_va_list va_list;
typedef _Float128 _Complex qc; typedef double _Complex dc;
typedef float _Complex fc; typedef long double _Complex lc;
#define E 0x1p-100F128
qc q_add(qc x, qc y); qc q_sub(qc x, qc y); qc q_mul(qc x, qc y); qc q_div(qc x, qc y);
qc q_neg(qc x); qc q_conj(qc x);
qc q_from_d(dc z); dc d_from_q(qc z); lc l_from_q(qc z); qc q_from_f(fc z);
_Float128 q_re(qc z); _Float128 q_im(qc z); qc q_make(_Float128 re, _Float128 im);
qc q_va(int n, ...);
"#;

const F128C_CALLEE: &str = r#"
qc q_add(qc x, qc y) { return x + y; }
qc q_sub(qc x, qc y) { return x - y; }
qc q_mul(qc x, qc y) { return x * y; }
qc q_div(qc x, qc y) { return x / y; }
qc q_neg(qc x) { return -x; }
qc q_conj(qc x) { return ~x; }
qc q_from_d(dc z) { return z; }
dc d_from_q(qc z) { return z; }
lc l_from_q(qc z) { return (lc)z; }
qc q_from_f(fc z) { return (qc)z; }
_Float128 q_re(qc z) { return __real__ z; }
_Float128 q_im(qc z) { return __imag__ z; }
qc q_make(_Float128 re, _Float128 im) { qc z; __real__ z = re; __imag__ z = im; return z; }
/* n rounds of (int k, qc z, double d), summing z * k + d. */
qc q_va(int n, ...) {
    va_list ap;
    __builtin_va_start(ap, n);
    qc s = 0;
    for (int i = 0; i < n; i++) {
        int k = __builtin_va_arg(ap, int);
        qc z = __builtin_va_arg(ap, qc);
        double d = __builtin_va_arg(ap, double);
        s += z * k + d;
    }
    __builtin_va_end(ap);
    return s;
}
"#;

const F128C_CALLER: &str = r#"
#define EQ(z, re, im) (__real__ (z) == (re) && __imag__ (z) == (im))
#define R(k) k, a, 0.5
int main(void) {
    qc a = __builtin_complex(3 + E, -2 - E);
    qc b = __builtin_complex((_Float128)1, (_Float128)1);
    if (!EQ(q_add(a, b), 4 + E, -1 - E)) return 1;
    if (!EQ(q_sub(a, b), 2 + E, -3 - E)) return 2;
    qc m = q_mul(a, b);
    if (!EQ(m, 5 + 2 * E, (_Float128)1)) return 3;
    if (!EQ(q_div(m, b), 3 + E, -2 - E)) return 4;
    if (!EQ(q_neg(a), -3 - E, 2 + E)) return 5;
    if (!EQ(q_conj(a), 3 + E, 2 + E)) return 6;
    if (!EQ(q_from_d(__builtin_complex(1.5, -2.25)), (_Float128)1.5, (_Float128)-2.25)) return 7;
    dc d = d_from_q(a);
    if (!EQ(d, 3.0, -2.0)) return 8;
    lc l = l_from_q(a);
    if (!EQ(l, (long double)(3 + E), (long double)(-2 - E))) return 9;
    if (!EQ(q_from_f(__builtin_complex(0.5f, 4.0f)), (_Float128)0.5, (_Float128)4)) return 10;
    if (q_re(a) != 3 + E || q_im(a) != -2 - E) return 11;
    if (!EQ(q_make(3 + E, -2 - E), 3 + E, -2 - E)) return 12;
    /* Nine rounds: the ints and doubles run out of registers too. */
    qc s = q_va(9, R(1), R(1), R(1), R(1), R(1), R(1), R(1), R(1), R(1));
    if (!EQ(s, 31.5F128 + 9 * E, -18 - 9 * E)) return 13;
    if (!EQ(q_va(1, 2, b, 0.25), 2.25F128, (_Float128)2)) return 14;
    return 0;
}
"#;

#[cfg(not(target_os = "macos"))]
#[test]
fn c99_float128_complex_operations_c17_both_sides() {
    let program = format!("{F128C_DECLS}\n{F128C_CALLEE}\n{F128C_CALLER}");
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(&format!("f128c_ops{opt}"), &program, &[opt.to_string()]),
            0,
            "host {opt}"
        );
    }
}

/// Linux x86-64 only, where the host gcc is gcc and the ABI is System V.
#[test]
fn c99_float128_complex_operations_interoperate_with_gcc_host() {
    if !cfg!(all(target_os = "linux", target_arch = "x86_64")) {
        return;
    }
    interop_host(
        "f128c",
        &format!("{F128C_DECLS}\n{F128C_CALLEE}"),
        &format!("{F128C_DECLS}\n{F128C_CALLER}"),
    );
}

/// On aarch64 `_Float128` is `long double`, and its complex an HFA.
#[test]
fn c99_float128_complex_operations_interoperate_with_gcc_aarch64() {
    if !aarch64_cross_available() {
        eprintln!("SKIP: no aarch64 cross toolchain");
        return;
    }
    interop_aarch64(
        "f128c",
        &format!("{F128C_DECLS}\n{F128C_CALLEE}"),
        &format!("{F128C_DECLS}\n{F128C_CALLER}"),
    );
}

// ============================================================================
// Parameters of a function that returns through the hidden pointer
// ============================================================================

// The hidden pointer is `Arg(0)`, so each declared parameter is one `Arg`
// further along. Two allocator sites looked a parameter's type up by the `Arg`
// number itself, and so took the *next* parameter's: on x86-64 a complex
// parameter was not recognised as one, spilled as a lone `double`, and its
// local never written; on aarch64 a `long double` spilled across a call kept
// eight of its sixteen bytes.
const SRET_COMPLEX_PARAMS: &str = r#"
typedef double _Complex dc;
struct Big { long a[4]; };
__attribute__((noinline)) struct Big f(dc z) {
    struct Big b = {{(long)__real__ z, (long)__imag__ z, 0, 0}};
    return b;
}
__attribute__((noinline)) struct Big g(int x, dc z, float _Complex w) {
    struct Big b = {{(long)__real__ z, (long)__imag__ z, x, (long)__imag__ w}};
    return b;
}
int main(void) {
    struct Big b = f(__builtin_complex(3.0, 4.0));
    if (b.a[0] != 3 || b.a[1] != 4) return 1;
    b = g(7, __builtin_complex(3.0, 4.0), __builtin_complex(1.0f, 9.0f));
    if (b.a[0] != 3 || b.a[1] != 4 || b.a[2] != 7 || b.a[3] != 9) return 2;
    return 0;
}
"#;

const SRET_LONG_DOUBLE_PARAM: &str = r#"
struct Big { long a[4]; };
__attribute__((noinline)) long double id(long double x) { return x; }
__attribute__((noinline)) struct Big f(long double x) {
    long double y = id(1.0L);
    struct Big b = {{(long)(x * 4), (long)y, (long)(x * 1e30L / 1e29L), 0}};
    return b;
}
int main(void) {
    struct Big b = f(2.5L);
    return b.a[0] == 10 && b.a[1] == 1 && b.a[2] == 25 ? 0 : 1;
}
"#;

#[test]
fn c99_sret_function_reads_its_own_parameters() {
    for opt in ["-O0", "-O2"] {
        for (name, src) in [
            ("sret_complex", SRET_COMPLEX_PARAMS),
            ("sret_ld", SRET_LONG_DOUBLE_PARAM),
        ] {
            assert_eq!(
                compile_and_run(&format!("{name}{opt}"), src, &[opt.to_string()]),
                0,
                "{name} host {opt}"
            );
            if let Some(rc) = compile_and_run_aarch64(&format!("{name}_a64"), src, opt) {
                assert_eq!(rc, 0, "{name} aarch64 {opt}");
            }
        }
    }
}
