/*
 * tgmath.h - C11 7.25 type-generic math macros
 *
 * Builtin header for c17.
 *
 * Written entirely in _Generic, which is the idiom the standard itself
 * describes. Deliberately independent of the host's <tgmath.h>: every glibc
 * version of that header requires compiler internals c17 does not have --
 * __builtin_tgmath for GCC >= 8, and __builtin_classify_type plus __real__ for
 * the older path -- and it additionally #errors out unless __HAVE_FLOAT128
 * agrees with __HAVE_FLOAT64X, which is decided purely by the __GNUC__ version
 * we advertise. None of that is reachable from here, and none of it is needed:
 * _Generic expresses the whole facility.
 *
 * A program may #undef any of these to reach the underlying function, as
 * C11 7.1.4p1 permits.
 */
#ifndef _TGMATH_H
#define _TGMATH_H

#include <math.h>
#include <complex.h>

/* ------------------------------------------------------------------------
 * Dispatch helpers.
 *
 * The controlling expression of a _Generic is not evaluated (C17 6.5.1.1p2);
 * only its type is used, after lvalue conversion. That is what lets these
 * dispatch on an argument that has side effects, and what lets the
 * two-argument forms dispatch on `(x)+(y)` -- the usual arithmetic conversions
 * pick the common type without either operand being evaluated.
 * ------------------------------------------------------------------------ */

/* Real-only: float / double / long double, integers promoted to double. */
#define __tg_real(fn, x) _Generic((x), \
    float: fn##f, \
    long double: fn##l, \
    default: fn)

/* Real or complex, one argument. */
#define __tg_rc(fn, cfn, x) _Generic((x), \
    float: fn##f, \
    long double: fn##l, \
    float _Complex: cfn##f, \
    double _Complex: cfn, \
    long double _Complex: cfn##l, \
    default: fn)

/* Complex-only, one argument. A real argument is treated as a complex value
   with a zero imaginary part, so the real types map to the double forms. */
#define __tg_cplx(cfn, x) _Generic((x), \
    float _Complex: cfn##f, \
    long double _Complex: cfn##l, \
    default: cfn)

/* ------------------------------------------------------------------------
 * 7.25.2 - real or complex
 * ------------------------------------------------------------------------ */

#define acos(x)  __tg_rc(acos,  cacos,  x)(x)
#define asin(x)  __tg_rc(asin,  casin,  x)(x)
#define atan(x)  __tg_rc(atan,  catan,  x)(x)
#define acosh(x) __tg_rc(acosh, cacosh, x)(x)
#define asinh(x) __tg_rc(asinh, casinh, x)(x)
#define atanh(x) __tg_rc(atanh, catanh, x)(x)
#define cos(x)   __tg_rc(cos,   ccos,   x)(x)
#define sin(x)   __tg_rc(sin,   csin,   x)(x)
#define tan(x)   __tg_rc(tan,   ctan,   x)(x)
#define cosh(x)  __tg_rc(cosh,  ccosh,  x)(x)
#define sinh(x)  __tg_rc(sinh,  csinh,  x)(x)
#define tanh(x)  __tg_rc(tanh,  ctanh,  x)(x)
#define exp(x)   __tg_rc(exp,   cexp,   x)(x)
#define log(x)   __tg_rc(log,   clog,   x)(x)
#define sqrt(x)  __tg_rc(sqrt,  csqrt,  x)(x)
#define fabs(x)  __tg_rc(fabs,  cabs,   x)(x)

/* pow dispatches on the combined type of both arguments. */
#define pow(x, y) __tg_rc(pow, cpow, (x) + (y))((x), (y))

/* ------------------------------------------------------------------------
 * 7.25.3 - real only
 * ------------------------------------------------------------------------ */

#define atan2(x, y)     __tg_real(atan2,     (x) + (y))((x), (y))
#define cbrt(x)         __tg_real(cbrt,      x)(x)
#define ceil(x)         __tg_real(ceil,      x)(x)
#define copysign(x, y)  __tg_real(copysign,  (x) + (y))((x), (y))
#define erf(x)          __tg_real(erf,       x)(x)
#define erfc(x)         __tg_real(erfc,      x)(x)
#define exp2(x)         __tg_real(exp2,      x)(x)
#define expm1(x)        __tg_real(expm1,     x)(x)
#define fdim(x, y)      __tg_real(fdim,      (x) + (y))((x), (y))
#define floor(x)        __tg_real(floor,     x)(x)
#define fma(x, y, z)    __tg_real(fma,       (x) + (y) + (z))((x), (y), (z))
#define fmax(x, y)      __tg_real(fmax,      (x) + (y))((x), (y))
#define fmin(x, y)      __tg_real(fmin,      (x) + (y))((x), (y))
#define fmod(x, y)      __tg_real(fmod,      (x) + (y))((x), (y))
#define frexp(x, p)     __tg_real(frexp,     x)((x), (p))
#define hypot(x, y)     __tg_real(hypot,     (x) + (y))((x), (y))
#define ilogb(x)        __tg_real(ilogb,     x)(x)
#define ldexp(x, n)     __tg_real(ldexp,     x)((x), (n))
#define lgamma(x)       __tg_real(lgamma,    x)(x)
#define llrint(x)       __tg_real(llrint,    x)(x)
#define llround(x)      __tg_real(llround,   x)(x)
#define log10(x)        __tg_real(log10,     x)(x)
#define log1p(x)        __tg_real(log1p,     x)(x)
#define log2(x)         __tg_real(log2,      x)(x)
#define logb(x)         __tg_real(logb,      x)(x)
#define lrint(x)        __tg_real(lrint,     x)(x)
#define lround(x)       __tg_real(lround,    x)(x)
#define nearbyint(x)    __tg_real(nearbyint, x)(x)
#define nextafter(x, y) __tg_real(nextafter, (x) + (y))((x), (y))
#define remainder(x, y) __tg_real(remainder, (x) + (y))((x), (y))
#define remquo(x, y, q) __tg_real(remquo,    (x) + (y))((x), (y), (q))
#define rint(x)         __tg_real(rint,      x)(x)
#define round(x)        __tg_real(round,     x)(x)
#define scalbln(x, n)   __tg_real(scalbln,   x)((x), (n))
#define scalbn(x, n)    __tg_real(scalbn,    x)((x), (n))
#define tgamma(x)       __tg_real(tgamma,    x)(x)
#define trunc(x)        __tg_real(trunc,     x)(x)

/* nexttoward's second argument is always long double, so only the first
   participates in the dispatch. */
#define nexttoward(x, y) __tg_real(nexttoward, x)((x), (y))

/* ------------------------------------------------------------------------
 * 7.25.4 - complex only
 * ------------------------------------------------------------------------ */

#define carg(x)  __tg_cplx(carg,  x)(x)
#define cimag(x) __tg_cplx(cimag, x)(x)
#define conj(x)  __tg_cplx(conj,  x)(x)
#define cproj(x) __tg_cplx(cproj, x)(x)
#define creal(x) __tg_cplx(creal, x)(x)

#endif /* _TGMATH_H */
