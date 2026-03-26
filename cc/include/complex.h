/*
 * complex.h - C99 complex arithmetic <complex.h>
 *
 * Builtin header for pcc compiler.
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

/* C99 7.3.5-7.3.8: trigonometric, hyperbolic, exponential, power */
double _Complex ccos(double _Complex);
double _Complex csin(double _Complex);
double _Complex ctan(double _Complex);
double _Complex cacos(double _Complex);
double _Complex casin(double _Complex);
double _Complex catan(double _Complex);

double _Complex ccosh(double _Complex);
double _Complex csinh(double _Complex);
double _Complex ctanh(double _Complex);
double _Complex cacosh(double _Complex);
double _Complex casinh(double _Complex);
double _Complex catanh(double _Complex);

double _Complex cexp(double _Complex);
double _Complex clog(double _Complex);
double _Complex cpow(double _Complex, double _Complex);
double _Complex csqrt(double _Complex);
double _Complex cproj(double _Complex);

#endif /* _COMPLEX_H */
