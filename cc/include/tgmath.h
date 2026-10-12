/*
 * tgmath.h - C11 7.25 type-generic math macros
 *
 * Builtin header for c17.
 *
 * Every macro is a `__builtin_tgmath` call over the functions the C library
 * declares for it, as glibc's own <tgmath.h> is under gcc 13: the builtin
 * picks the function by the types of the arguments (C2x 7.27p3, and its
 * rules for the macros that round to a narrower type), and expands and
 * evaluates each argument once. Which macros exist, and which functions each
 * may reach, follow glibc's feature tests, so a program sees under c17 what
 * it sees under gcc: `sqrt` of a `_Float128` is `sqrtf128` where <math.h>
 * declares that, and is refused where it does not, never a `double` call.
 * Elsewhere than glibc (macOS) there is no `__GLIBC_USE`, so a macro reaches
 * only the `float`, `double` and `long double` functions of C17.
 *
 * glibc's own header cannot serve: it picks its implementation by the gcc
 * version claimed, and below 13 needs __builtin_classify_type tricks.
 *
 * A program may #undef any of these to reach the underlying function, as
 * C11 7.1.4p1 permits.
 */
#ifndef _TGMATH_H
#define _TGMATH_H

#include <math.h>
#include <complex.h>

/* The C library's feature tests, as flags this header can test without
   `__GLIBC_USE`, which only glibc defines. */
#ifdef __GLIBC_USE
#if __GLIBC_USE (IEC_60559_TYPES_EXT)
#define __TG_TYPES 1
#endif
#if __GLIBC_USE (IEC_60559_BFP_EXT)
#define __TG_BFP 1
#endif
#if __GLIBC_USE (IEC_60559_BFP_EXT_C2X)
#define __TG_BFP_C2X 1
#endif
#if __GLIBC_USE (IEC_60559_FUNCS_EXT_C2X)
#define __TG_FUNCS_C2X 1
#endif
#if __GLIBC_USE (ISOC2X)
#define __TG_C2X 1
#endif
#endif

/* The function of each _FloatN and _FloatNx type <math.h> declares, as a
   list entry with its comma, or nothing. */
#if defined __TG_TYPES && __HAVE_FLOAT16
#define __TG_F16(F) F##f16,
#else
#define __TG_F16(F)
#endif
#if defined __TG_TYPES && __HAVE_FLOAT32
#define __TG_F32(F) F##f32,
#else
#define __TG_F32(F)
#endif
#if defined __TG_TYPES && __HAVE_FLOAT64
#define __TG_F64(F) F##f64,
#else
#define __TG_F64(F)
#endif
#if defined __TG_TYPES && __HAVE_FLOAT128
#define __TG_F128(F) F##f128,
#else
#define __TG_F128(F)
#endif
#if defined __TG_TYPES && __HAVE_FLOAT32X
#define __TG_F32X(F) F##f32x,
#else
#define __TG_F32X(F)
#endif
#if defined __TG_TYPES && __HAVE_FLOAT64X
#define __TG_F64X(F) F##f64x,
#else
#define __TG_F64X(F)
#endif
#if defined __TG_TYPES && __HAVE_FLOAT128X
#define __TG_F128X(F) F##f128x,
#else
#define __TG_F128X(F)
#endif

/* Every function of a family, the standard three first. */
#define __TG_FNS(F) F##f, F, F##l, \
    __TG_F16(F) __TG_F32(F) __TG_F64(F) __TG_F128(F) \
    __TG_F32X(F) __TG_F64X(F) __TG_F128X(F)

#define __TG_1(F, x)       __builtin_tgmath(__TG_FNS(F) (x))
#define __TG_2(F, x, y)    __builtin_tgmath(__TG_FNS(F) (x), (y))
#define __TG_3(F, x, y, z) __builtin_tgmath(__TG_FNS(F) (x), (y), (z))
/* Real or complex: both families, the complex one picked for a complex
   argument. */
#define __TG_1C(F, C, x)    __builtin_tgmath(__TG_FNS(F) __TG_FNS(C) (x))
#define __TG_2C(F, C, x, y) __builtin_tgmath(__TG_FNS(F) __TG_FNS(C) (x), (y))
/* Only the standard three: no C library has the others. */
#define __TG_2STD(F, x, y) __builtin_tgmath(F##f, F, F##l, (x), (y))

/* ------------------------------------------------------------------------
 * 7.25.2 - real or complex
 * ------------------------------------------------------------------------ */

#define acos(x)  __TG_1C(acos,  cacos,  x)
#define asin(x)  __TG_1C(asin,  casin,  x)
#define atan(x)  __TG_1C(atan,  catan,  x)
#define acosh(x) __TG_1C(acosh, cacosh, x)
#define asinh(x) __TG_1C(asinh, casinh, x)
#define atanh(x) __TG_1C(atanh, catanh, x)
#define cos(x)   __TG_1C(cos,   ccos,   x)
#define sin(x)   __TG_1C(sin,   csin,   x)
#define tan(x)   __TG_1C(tan,   ctan,   x)
#define cosh(x)  __TG_1C(cosh,  ccosh,  x)
#define sinh(x)  __TG_1C(sinh,  csinh,  x)
#define tanh(x)  __TG_1C(tanh,  ctanh,  x)
#define exp(x)   __TG_1C(exp,   cexp,   x)
#define log(x)   __TG_1C(log,   clog,   x)
#define sqrt(x)  __TG_1C(sqrt,  csqrt,  x)
#define fabs(x)  __TG_1C(fabs,  cabs,   x)
#define pow(x, y) __TG_2C(pow, cpow, x, y)

/* ------------------------------------------------------------------------
 * 7.25.3 - real only
 * ------------------------------------------------------------------------ */

#define atan2(x, y)     __TG_2(atan2,     x, y)
#define cbrt(x)         __TG_1(cbrt,      x)
#define ceil(x)         __TG_1(ceil,      x)
#define copysign(x, y)  __TG_2(copysign,  x, y)
#define erf(x)          __TG_1(erf,       x)
#define erfc(x)         __TG_1(erfc,      x)
#define exp2(x)         __TG_1(exp2,      x)
#define expm1(x)        __TG_1(expm1,     x)
#define fdim(x, y)      __TG_2(fdim,      x, y)
#define floor(x)        __TG_1(floor,     x)
#define fma(x, y, z)    __TG_3(fma,       x, y, z)
#define fmod(x, y)      __TG_2(fmod,      x, y)
#define frexp(x, p)     __TG_2(frexp,     x, p)
#define hypot(x, y)     __TG_2(hypot,     x, y)
#define ilogb(x)        __TG_1(ilogb,     x)
#define ldexp(x, n)     __TG_2(ldexp,     x, n)
#define lgamma(x)       __TG_1(lgamma,    x)
#define llrint(x)       __TG_1(llrint,    x)
#define llround(x)      __TG_1(llround,   x)
#define log1p(x)        __TG_1(log1p,     x)
#define log2(x)         __TG_1(log2,      x)
#define logb(x)         __TG_1(logb,      x)
#define lrint(x)        __TG_1(lrint,     x)
#define lround(x)       __TG_1(lround,    x)
#define nearbyint(x)    __TG_1(nearbyint, x)
#define nextafter(x, y) __TG_2(nextafter, x, y)
#define remainder(x, y) __TG_2(remainder, x, y)
#define remquo(x, y, q) __TG_3(remquo,    x, y, q)
#define rint(x)         __TG_1(rint,      x)
#define round(x)        __TG_1(round,     x)
#define scalbln(x, n)   __TG_2(scalbln,   x, n)
#define scalbn(x, n)    __TG_2(scalbn,    x, n)
#define tgamma(x)       __TG_1(tgamma,    x)
#define trunc(x)        __TG_1(trunc,     x)
/* The second argument is `long double` for each function. */
#define nexttoward(x, y) __TG_2STD(nexttoward, x, y)

/* glibc's complex base-10 logarithm, under _GNU_SOURCE. */
#ifdef __USE_GNU
#define log10(x) __TG_1C(log10, clog10, x)
#else
#define log10(x) __TG_1(log10, x)
#endif

#if defined __TG_C2X && !defined __USE_GNU
#define fmax(x, y) __TG_2STD(fmax, x, y)
#define fmin(x, y) __TG_2STD(fmin, x, y)
#else
#define fmax(x, y) __TG_2(fmax, x, y)
#define fmin(x, y) __TG_2(fmin, x, y)
#endif

/* ------------------------------------------------------------------------
 * 7.25.4 - complex only
 * ------------------------------------------------------------------------ */

#define carg(x)  __TG_1(carg,  x)
#define cimag(x) __TG_1(cimag, x)
#define conj(x)  __TG_1(conj,  x)
#define cproj(x) __TG_1(cproj, x)
#define creal(x) __TG_1(creal, x)

/* ------------------------------------------------------------------------
 * The C library's extensions: glibc's, under its feature tests
 * ------------------------------------------------------------------------ */

#ifdef __USE_MISC
#define scalb(x, y) __TG_2STD(scalb, x, y)
#endif

#ifdef __TG_FUNCS_C2X
#define exp10(x) __TG_1(exp10, x)
#endif

#ifdef __TG_BFP_C2X
#define nextdown(x)        __TG_1(nextdown,  x)
#define nextup(x)          __TG_1(nextup,    x)
#define roundeven(x)       __TG_1(roundeven, x)
#define llogb(x)           __TG_1(llogb,     x)
#define fromfp(x, r, w)    __TG_3(fromfp,    x, r, w)
#define ufromfp(x, r, w)   __TG_3(ufromfp,   x, r, w)
#define fromfpx(x, r, w)   __TG_3(fromfpx,   x, r, w)
#define ufromfpx(x, r, w)  __TG_3(ufromfpx,  x, r, w)
#endif

#ifdef __TG_BFP
#define fmaxmag(x, y) __TG_2(fmaxmag, x, y)
#define fminmag(x, y) __TG_2(fminmag, x, y)
#endif

#ifdef __TG_C2X
#define fmaximum(x, y)         __TG_2(fmaximum,         x, y)
#define fminimum(x, y)         __TG_2(fminimum,         x, y)
#define fmaximum_num(x, y)     __TG_2(fmaximum_num,     x, y)
#define fminimum_num(x, y)     __TG_2(fminimum_num,     x, y)
#define fmaximum_mag(x, y)     __TG_2(fmaximum_mag,     x, y)
#define fminimum_mag(x, y)     __TG_2(fminimum_mag,     x, y)
#define fmaximum_mag_num(x, y) __TG_2(fmaximum_mag_num, x, y)
#define fminimum_mag_num(x, y) __TG_2(fminimum_mag_num, x, y)
#endif

/* ------------------------------------------------------------------------
 * The macros that round their result to a narrower type (TS 18661-1 and
 * -3, C2x 7.12.14). A macro whose functions all return one type picks by
 * the narrowing rule of `__builtin_tgmath`: the function for the type the
 * arguments determine, else the first listed whose parameter type holds
 * every value of that type. The `d` and `f64x` macros have one function,
 * called directly.
 * ------------------------------------------------------------------------ */

#define __TG_NARROW_F(F)   F, F##l,
#define __TG_NARROW_F16(F) __TG_F32(F) __TG_F64(F) __TG_F128(F) \
    __TG_F32X(F) __TG_F64X(F) __TG_F128X(F)
#define __TG_NARROW_F32(F) __TG_F64(F) __TG_F128(F) \
    __TG_F32X(F) __TG_F64X(F) __TG_F128X(F)
#define __TG_NARROW_F64(F) __TG_F128(F) __TG_F64X(F) __TG_F128X(F)
#define __TG_NARROW_F32X(F) __TG_F64X(F) __TG_F128X(F) __TG_F64(F) __TG_F128(F)

#ifdef __TG_BFP_C2X
#define fadd(x, y)    __builtin_tgmath(__TG_NARROW_F(fadd) (x), (y))
#define fsub(x, y)    __builtin_tgmath(__TG_NARROW_F(fsub) (x), (y))
#define fmul(x, y)    __builtin_tgmath(__TG_NARROW_F(fmul) (x), (y))
#define fdiv(x, y)    __builtin_tgmath(__TG_NARROW_F(fdiv) (x), (y))
#define fsqrt(x)      __builtin_tgmath(__TG_NARROW_F(fsqrt) (x))
#define ffma(x, y, z) __builtin_tgmath(__TG_NARROW_F(ffma) (x), (y), (z))
#define dadd(x, y)    (daddl(x, y))
#define dsub(x, y)    (dsubl(x, y))
#define dmul(x, y)    (dmull(x, y))
#define ddiv(x, y)    (ddivl(x, y))
#define dsqrt(x)      (dsqrtl(x))
#define dfma(x, y, z) (dfmal(x, y, z))
#endif

#ifdef __TG_TYPES

#if __HAVE_FLOAT16
#define f16add(x, y)    __builtin_tgmath(__TG_NARROW_F16(f16add) (x), (y))
#define f16sub(x, y)    __builtin_tgmath(__TG_NARROW_F16(f16sub) (x), (y))
#define f16mul(x, y)    __builtin_tgmath(__TG_NARROW_F16(f16mul) (x), (y))
#define f16div(x, y)    __builtin_tgmath(__TG_NARROW_F16(f16div) (x), (y))
#define f16sqrt(x)      __builtin_tgmath(__TG_NARROW_F16(f16sqrt) (x))
#define f16fma(x, y, z) __builtin_tgmath(__TG_NARROW_F16(f16fma) (x), (y), (z))
#endif

#if __HAVE_FLOAT32
#define f32add(x, y)    __builtin_tgmath(__TG_NARROW_F32(f32add) (x), (y))
#define f32sub(x, y)    __builtin_tgmath(__TG_NARROW_F32(f32sub) (x), (y))
#define f32mul(x, y)    __builtin_tgmath(__TG_NARROW_F32(f32mul) (x), (y))
#define f32div(x, y)    __builtin_tgmath(__TG_NARROW_F32(f32div) (x), (y))
#define f32sqrt(x)      __builtin_tgmath(__TG_NARROW_F32(f32sqrt) (x))
#define f32fma(x, y, z) __builtin_tgmath(__TG_NARROW_F32(f32fma) (x), (y), (z))
#endif

#if __HAVE_FLOAT64 && (__HAVE_FLOAT64X || __HAVE_FLOAT128)
#define f64add(x, y)    __builtin_tgmath(__TG_NARROW_F64(f64add) (x), (y))
#define f64sub(x, y)    __builtin_tgmath(__TG_NARROW_F64(f64sub) (x), (y))
#define f64mul(x, y)    __builtin_tgmath(__TG_NARROW_F64(f64mul) (x), (y))
#define f64div(x, y)    __builtin_tgmath(__TG_NARROW_F64(f64div) (x), (y))
#define f64sqrt(x)      __builtin_tgmath(__TG_NARROW_F64(f64sqrt) (x))
#define f64fma(x, y, z) __builtin_tgmath(__TG_NARROW_F64(f64fma) (x), (y), (z))
#endif

#if __HAVE_FLOAT32X
#if __HAVE_FLOAT128
#define f32xadd(x, y)    __builtin_tgmath(__TG_NARROW_F32X(f32xadd) (x), (y))
#define f32xsub(x, y)    __builtin_tgmath(__TG_NARROW_F32X(f32xsub) (x), (y))
#define f32xmul(x, y)    __builtin_tgmath(__TG_NARROW_F32X(f32xmul) (x), (y))
#define f32xdiv(x, y)    __builtin_tgmath(__TG_NARROW_F32X(f32xdiv) (x), (y))
#define f32xsqrt(x)      __builtin_tgmath(__TG_NARROW_F32X(f32xsqrt) (x))
#define f32xfma(x, y, z) __builtin_tgmath(__TG_NARROW_F32X(f32xfma) (x), (y), (z))
#else
#define f32xadd(x, y)    (f32xaddf64(x, y))
#define f32xsub(x, y)    (f32xsubf64(x, y))
#define f32xmul(x, y)    (f32xmulf64(x, y))
#define f32xdiv(x, y)    (f32xdivf64(x, y))
#define f32xsqrt(x)      (f32xsqrtf64(x))
#define f32xfma(x, y, z) (f32xfmaf64(x, y, z))
#endif
#endif

#if __HAVE_FLOAT64X && (__HAVE_FLOAT128X || __HAVE_FLOAT128)
#define f64xadd(x, y)    (f64xaddf128(x, y))
#define f64xsub(x, y)    (f64xsubf128(x, y))
#define f64xmul(x, y)    (f64xmulf128(x, y))
#define f64xdiv(x, y)    (f64xdivf128(x, y))
#define f64xsqrt(x)      (f64xsqrtf128(x))
#define f64xfma(x, y, z) (f64xfmaf128(x, y, z))
#endif

#endif /* __TG_TYPES */

#endif /* _TGMATH_H */
