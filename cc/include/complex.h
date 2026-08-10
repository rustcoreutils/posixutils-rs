/*
 * complex.h - C99 complex arithmetic <complex.h>
 *
 * Builtin header for c17 compiler.
 */
#ifndef _COMPLEX_H
#define _COMPLEX_H

/* C99 7.3.1: macros */
#define complex _Complex

/* I macro: imaginary unit constant */
#define _Complex_I __builtin_complex(0.0, 1.0)
#define I _Complex_I

/* C99 7.3.9: manipulation functions (implemented by libm) */
double creal(double _Complex);
float crealf(float _Complex);
long double creall(long double _Complex);

double cimag(double _Complex);
float cimagf(float _Complex);
long double cimagl(long double _Complex);

double cabs(double _Complex);
float cabsf(float _Complex);
long double cabsl(long double _Complex);

double carg(double _Complex);
float cargf(float _Complex);
long double cargl(long double _Complex);

double _Complex conj(double _Complex);
float _Complex conjf(float _Complex);
long double _Complex conjl(long double _Complex);

/* C99 7.3.5-7.3.8: trigonometric, hyperbolic, exponential, power.

   All three precisions are declared. <tgmath.h> dispatches over them
   with _Generic, so the `f` and `l` variants have to be visible even
   though a program that spells the function directly usually wants the
   double one. */
double _Complex ccos(double _Complex);
float _Complex ccosf(float _Complex);
long double _Complex ccosl(long double _Complex);

double _Complex csin(double _Complex);
float _Complex csinf(float _Complex);
long double _Complex csinl(long double _Complex);

double _Complex ctan(double _Complex);
float _Complex ctanf(float _Complex);
long double _Complex ctanl(long double _Complex);

double _Complex cacos(double _Complex);
float _Complex cacosf(float _Complex);
long double _Complex cacosl(long double _Complex);

double _Complex casin(double _Complex);
float _Complex casinf(float _Complex);
long double _Complex casinl(long double _Complex);

double _Complex catan(double _Complex);
float _Complex catanf(float _Complex);
long double _Complex catanl(long double _Complex);

double _Complex ccosh(double _Complex);
float _Complex ccoshf(float _Complex);
long double _Complex ccoshl(long double _Complex);

double _Complex csinh(double _Complex);
float _Complex csinhf(float _Complex);
long double _Complex csinhl(long double _Complex);

double _Complex ctanh(double _Complex);
float _Complex ctanhf(float _Complex);
long double _Complex ctanhl(long double _Complex);

double _Complex cacosh(double _Complex);
float _Complex cacoshf(float _Complex);
long double _Complex cacoshl(long double _Complex);

double _Complex casinh(double _Complex);
float _Complex casinhf(float _Complex);
long double _Complex casinhl(long double _Complex);

double _Complex catanh(double _Complex);
float _Complex catanhf(float _Complex);
long double _Complex catanhl(long double _Complex);

double _Complex cexp(double _Complex);
float _Complex cexpf(float _Complex);
long double _Complex cexpl(long double _Complex);

double _Complex clog(double _Complex);
float _Complex clogf(float _Complex);
long double _Complex clogl(long double _Complex);

double _Complex csqrt(double _Complex);
float _Complex csqrtf(float _Complex);
long double _Complex csqrtl(long double _Complex);

double _Complex cproj(double _Complex);
float _Complex cprojf(float _Complex);
long double _Complex cprojl(long double _Complex);

double _Complex cpow(double _Complex, double _Complex);
float _Complex cpowf(float _Complex, float _Complex);
long double _Complex cpowl(long double _Complex, long double _Complex);

/* Exact complex construction (C11 7.3.9.5-7).

   These exist precisely because `x + y*I` cannot construct a value whose
   imaginary part is a NaN or an infinity: the multiplication and addition
   propagate the special value into the real part too. __builtin_complex
   assembles the two halves directly. */
#define CMPLX(x, y)  __builtin_complex((double)(x), (double)(y))
#define CMPLXF(x, y) __builtin_complex((float)(x), (float)(y))
#define CMPLXL(x, y) __builtin_complex((long double)(x), (long double)(y))

#endif /* _COMPLEX_H */
