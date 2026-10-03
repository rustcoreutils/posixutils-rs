/*
 * c17 builtin emmintrin.h - SSE2 intrinsics
 *
 * This file is part of the posixutils-rs project covered under
 * the MIT License. For the full license text, please see the LICENSE
 * file in the root directory of this project.
 * SPDX-License-Identifier: MIT
 *
 * Written from the instruction semantics in Intel's Intrinsics Guide, over
 * GNU vectors and c17's vector builtins: no `__builtin_ia32_*`. Every
 * function is available whatever -m flags are given; the feature macros
 * (`__SSE4_1__` and the rest) follow the flags, as gcc's do.
 */

#ifndef _EMMINTRIN_H_INCLUDED
#define _EMMINTRIN_H_INCLUDED

#include <xmmintrin.h>

typedef double __m128d __attribute__((__vector_size__(16), __may_alias__));
typedef long long __m128i __attribute__((__vector_size__(16), __may_alias__));
typedef double __m128d_u __attribute__((__vector_size__(16), __may_alias__, __aligned__(1)));
typedef long long __m128i_u __attribute__((__vector_size__(16), __may_alias__, __aligned__(1)));

/* Internal lane views of the 128-bit types. */
typedef double __v2df __attribute__((__vector_size__(16)));
typedef long long __v2di __attribute__((__vector_size__(16)));
typedef unsigned long long __v2du __attribute__((__vector_size__(16)));
typedef int __v4si __attribute__((__vector_size__(16)));
typedef unsigned int __v4su __attribute__((__vector_size__(16)));
typedef short __v8hi __attribute__((__vector_size__(16)));
typedef unsigned short __v8hu __attribute__((__vector_size__(16)));
typedef char __v16qi __attribute__((__vector_size__(16)));
typedef signed char __v16qs __attribute__((__vector_size__(16)));
typedef unsigned char __v16qu __attribute__((__vector_size__(16)));

/* Build the immediate of _mm_shuffle_pd. */
#define _MM_SHUFFLE2(fp1, fp0) (((fp1) << 1) | (fp0))

/* ---- Internal helpers ---------------------------------------------------- */

/* Lane-wise select: where the mask lane is all ones take __t, else __f. */
__C17_INTRIN __m128i __c17_sse2_sel_si128(__m128i __m, __m128i __t, __m128i __f)
{
    return (__m128i)(((__v2du)__m & (__v2du)__t) | (~(__v2du)__m & (__v2du)__f));
}

__C17_INTRIN __m128d __c17_sse2_sel_pd(__m128i __m, __m128d __t, __m128d __f)
{
    return (__m128d)__c17_sse2_sel_si128(__m, (__m128i)__t, (__m128i)__f);
}

/* Replace lane 0 of __a, keeping lane 1. */
__C17_INTRIN __m128d __c17_sse2_set_low_pd(__m128d __a, double __x)
{
    __v2df __r = (__v2df)__a;
    __r[0] = __x;
    return (__m128d)__r;
}

/* Replace lane 0 of __a with a 64-bit mask, keeping lane 1. */
__C17_INTRIN __m128d __c17_sse2_set_low_mask(__m128d __a, int __c)
{
    __v2di __r = (__v2di)__a;
    __r[0] = __c ? -1LL : 0LL;
    return (__m128d)__r;
}

/* The integer the conversion instructions return for NaN or a value out of
   range: the "integer indefinite". */
#define __C17_SSE2_INDEF32 (-2147483647 - 1)
#define __C17_SSE2_INDEF64 (-9223372036854775807LL - 1)

/* CVTSD2SI / CVTTSD2SI / CVTSS2SI / CVTTSS2SI, 32- and 64-bit results.
   Rounding follows the current mode, as MXCSR does; NaN fails every range
   test and so yields the integer indefinite. */
__C17_INTRIN int __c17_sse2_cvt_f64_i32(double __x)
{
    double __r = __builtin_rint(__x);
    return (__r >= -2147483648.0 && __r <= 2147483647.0) ? (int)__r : __C17_SSE2_INDEF32;
}

__C17_INTRIN int __c17_sse2_cvtt_f64_i32(double __x)
{
    return (__x > -2147483649.0 && __x < 2147483648.0) ? (int)__x : __C17_SSE2_INDEF32;
}

__C17_INTRIN long long __c17_sse2_cvt_f64_i64(double __x)
{
    double __r = __builtin_rint(__x);
    return (__r >= -9223372036854775808.0 && __r < 9223372036854775808.0)
               ? (long long)__r
               : __C17_SSE2_INDEF64;
}

__C17_INTRIN long long __c17_sse2_cvtt_f64_i64(double __x)
{
    return (__x >= -9223372036854775808.0 && __x < 9223372036854775808.0)
               ? (long long)__x
               : __C17_SSE2_INDEF64;
}

__C17_INTRIN int __c17_sse2_cvt_f32_i32(float __x)
{
    float __r = __builtin_rintf(__x);
    return (__r >= -2147483648.0f && __r < 2147483648.0f) ? (int)__r : __C17_SSE2_INDEF32;
}

__C17_INTRIN int __c17_sse2_cvtt_f32_i32(float __x)
{
    return (__x >= -2147483648.0f && __x < 2147483648.0f) ? (int)__x : __C17_SSE2_INDEF32;
}

/* SQRTSD on one lane. __builtin_sqrt would become a call to libm's sqrt,
   which an intrinsic must not need; the instruction itself is exact. */
__C17_INTRIN double __c17_sse2_sqrt(double __x)
{
    double __r;
    __asm__("sqrtsd %1, %0" : "=x"(__r) : "x"(__x));
    return __r;
}

/* Saturate an int to the given lane range. */
__C17_INTRIN int __c17_sse2_clamp(int __x, int __lo, int __hi)
{
    return __x < __lo ? __lo : __x > __hi ? __hi : __x;
}

/* ---- Set, load and store ------------------------------------------------- */

__C17_INTRIN __m128d _mm_undefined_pd(void)
{
    return (__m128d){0.0, 0.0};
}

__C17_INTRIN __m128i _mm_undefined_si128(void)
{
    return (__m128i){0, 0};
}

__C17_INTRIN __m128d _mm_setzero_pd(void)
{
    return (__m128d){0.0, 0.0};
}

__C17_INTRIN __m128i _mm_setzero_si128(void)
{
    return (__m128i){0, 0};
}

__C17_INTRIN __m128d _mm_set_sd(double __w)
{
    return (__m128d){__w, 0.0};
}

__C17_INTRIN __m128d _mm_set1_pd(double __w)
{
    return (__m128d){__w, __w};
}

__C17_INTRIN __m128d _mm_set_pd1(double __w)
{
    return (__m128d){__w, __w};
}

__C17_INTRIN __m128d _mm_set_pd(double __w1, double __w0)
{
    return (__m128d){__w0, __w1};
}

__C17_INTRIN __m128d _mm_setr_pd(double __w0, double __w1)
{
    return (__m128d){__w0, __w1};
}

__C17_INTRIN __m128i _mm_set_epi64x(long long __q1, long long __q0)
{
    return (__m128i){__q0, __q1};
}

__C17_INTRIN __m128i _mm_set_epi64(__m64 __q1, __m64 __q0)
{
    return (__m128i){(long long)__q0, (long long)__q1};
}

__C17_INTRIN __m128i _mm_setr_epi64(__m64 __q0, __m64 __q1)
{
    return (__m128i){(long long)__q0, (long long)__q1};
}

__C17_INTRIN __m128i _mm_set1_epi64x(long long __q)
{
    return (__m128i){__q, __q};
}

__C17_INTRIN __m128i _mm_set1_epi64(__m64 __q)
{
    return (__m128i){(long long)__q, (long long)__q};
}

__C17_INTRIN __m128i _mm_set_epi32(int __i3, int __i2, int __i1, int __i0)
{
    return (__m128i)(__v4si){__i0, __i1, __i2, __i3};
}

__C17_INTRIN __m128i _mm_setr_epi32(int __i0, int __i1, int __i2, int __i3)
{
    return (__m128i)(__v4si){__i0, __i1, __i2, __i3};
}

__C17_INTRIN __m128i _mm_set1_epi32(int __i)
{
    return (__m128i)(__v4si){__i, __i, __i, __i};
}

__C17_INTRIN __m128i _mm_set_epi16(short __w7, short __w6, short __w5, short __w4,
                                   short __w3, short __w2, short __w1, short __w0)
{
    return (__m128i)(__v8hi){__w0, __w1, __w2, __w3, __w4, __w5, __w6, __w7};
}

__C17_INTRIN __m128i _mm_setr_epi16(short __w0, short __w1, short __w2, short __w3,
                                    short __w4, short __w5, short __w6, short __w7)
{
    return (__m128i)(__v8hi){__w0, __w1, __w2, __w3, __w4, __w5, __w6, __w7};
}

__C17_INTRIN __m128i _mm_set1_epi16(short __w)
{
    return (__m128i)(__v8hi){__w, __w, __w, __w, __w, __w, __w, __w};
}

__C17_INTRIN __m128i _mm_set_epi8(char __b15, char __b14, char __b13, char __b12,
                                  char __b11, char __b10, char __b9, char __b8,
                                  char __b7, char __b6, char __b5, char __b4,
                                  char __b3, char __b2, char __b1, char __b0)
{
    return (__m128i)(__v16qi){__b0, __b1, __b2, __b3, __b4, __b5, __b6, __b7,
                              __b8, __b9, __b10, __b11, __b12, __b13, __b14, __b15};
}

__C17_INTRIN __m128i _mm_setr_epi8(char __b0, char __b1, char __b2, char __b3,
                                   char __b4, char __b5, char __b6, char __b7,
                                   char __b8, char __b9, char __b10, char __b11,
                                   char __b12, char __b13, char __b14, char __b15)
{
    return (__m128i)(__v16qi){__b0, __b1, __b2, __b3, __b4, __b5, __b6, __b7,
                              __b8, __b9, __b10, __b11, __b12, __b13, __b14, __b15};
}

__C17_INTRIN __m128i _mm_set1_epi8(char __b)
{
    return (__m128i)(__v16qi){__b, __b, __b, __b, __b, __b, __b, __b,
                              __b, __b, __b, __b, __b, __b, __b, __b};
}

__C17_INTRIN __m128d _mm_load_pd(double const *__p)
{
    return *(__m128d const *)__p;
}

__C17_INTRIN __m128d _mm_loadu_pd(double const *__p)
{
    return *(__m128d_u const *)__p;
}

__C17_INTRIN __m128d _mm_load1_pd(double const *__p)
{
    double __d = *__p;
    return (__m128d){__d, __d};
}

__C17_INTRIN __m128d _mm_load_pd1(double const *__p)
{
    double __d = *__p;
    return (__m128d){__d, __d};
}

__C17_INTRIN __m128d _mm_load_sd(double const *__p)
{
    return (__m128d){*__p, 0.0};
}

__C17_INTRIN __m128d _mm_loadr_pd(double const *__p)
{
    __m128d __v = *(__m128d const *)__p;
    return (__m128d){__v[1], __v[0]};
}

__C17_INTRIN __m128d _mm_loadh_pd(__m128d __a, double const *__p)
{
    __v2df __r = (__v2df)__a;
    __r[1] = *__p;
    return (__m128d)__r;
}

__C17_INTRIN __m128d _mm_loadl_pd(__m128d __a, double const *__p)
{
    __v2df __r = (__v2df)__a;
    __r[0] = *__p;
    return (__m128d)__r;
}

__C17_INTRIN __m128i _mm_load_si128(__m128i const *__p)
{
    return *__p;
}

__C17_INTRIN __m128i _mm_loadu_si128(__m128i_u const *__p)
{
    return *__p;
}

__C17_INTRIN __m128i _mm_loadl_epi64(__m128i_u const *__p)
{
    long long __q;
    __builtin_memcpy(&__q, __p, 8);
    return (__m128i){__q, 0};
}

__C17_INTRIN __m128i _mm_loadu_si64(void const *__p)
{
    long long __q;
    __builtin_memcpy(&__q, __p, 8);
    return (__m128i){__q, 0};
}

__C17_INTRIN __m128i _mm_loadu_si32(void const *__p)
{
    int __d;
    __builtin_memcpy(&__d, __p, 4);
    return (__m128i)(__v4si){__d, 0, 0, 0};
}

__C17_INTRIN __m128i _mm_loadu_si16(void const *__p)
{
    short __w;
    __builtin_memcpy(&__w, __p, 2);
    return (__m128i)(__v8hi){__w, 0, 0, 0, 0, 0, 0, 0};
}

__C17_INTRIN void _mm_store_pd(double *__p, __m128d __a)
{
    *(__m128d *)__p = __a;
}

__C17_INTRIN void _mm_storeu_pd(double *__p, __m128d __a)
{
    *(__m128d_u *)__p = __a;
}

__C17_INTRIN void _mm_store_sd(double *__p, __m128d __a)
{
    *__p = __a[0];
}

__C17_INTRIN void _mm_store1_pd(double *__p, __m128d __a)
{
    *(__m128d *)__p = (__m128d){__a[0], __a[0]};
}

__C17_INTRIN void _mm_store_pd1(double *__p, __m128d __a)
{
    *(__m128d *)__p = (__m128d){__a[0], __a[0]};
}

__C17_INTRIN void _mm_storer_pd(double *__p, __m128d __a)
{
    *(__m128d *)__p = (__m128d){__a[1], __a[0]};
}

__C17_INTRIN void _mm_storeh_pd(double *__p, __m128d __a)
{
    *__p = __a[1];
}

__C17_INTRIN void _mm_storel_pd(double *__p, __m128d __a)
{
    *__p = __a[0];
}

__C17_INTRIN void _mm_store_si128(__m128i *__p, __m128i __a)
{
    *__p = __a;
}

__C17_INTRIN void _mm_storeu_si128(__m128i_u *__p, __m128i __a)
{
    *__p = __a;
}

__C17_INTRIN void _mm_storel_epi64(__m128i_u *__p, __m128i __a)
{
    long long __q = __a[0];
    __builtin_memcpy(__p, &__q, 8);
}

__C17_INTRIN void _mm_storeu_si64(void *__p, __m128i __a)
{
    long long __q = __a[0];
    __builtin_memcpy(__p, &__q, 8);
}

__C17_INTRIN void _mm_storeu_si32(void *__p, __m128i __a)
{
    int __d = ((__v4si)__a)[0];
    __builtin_memcpy(__p, &__d, 4);
}

__C17_INTRIN void _mm_storeu_si16(void *__p, __m128i __a)
{
    short __w = ((__v8hi)__a)[0];
    __builtin_memcpy(__p, &__w, 2);
}

/* MASKMOVDQU: store each byte of __d whose mask byte has its top bit set. */
__C17_INTRIN void _mm_maskmoveu_si128(__m128i __d, __m128i __n, char *__p)
{
    __v16qu __v = (__v16qu)__d;
    __v16qu __m = (__v16qu)__n;
    for (int __i = 0; __i < 16; __i++)
        if (__m[__i] & 0x80)
            __p[__i] = (char)__v[__i];
}

/* The non-temporal stores: the cache hint has no C spelling, and the stored
   values are the same as a plain store's. */
__C17_INTRIN void _mm_stream_pd(double *__p, __m128d __a)
{
    *(__m128d *)__p = __a;
}

__C17_INTRIN void _mm_stream_si128(__m128i *__p, __m128i __a)
{
    *__p = __a;
}

__C17_INTRIN void _mm_stream_si32(int *__p, int __a)
{
    *(volatile int *)__p = __a;
}

__C17_INTRIN void _mm_stream_si64(long long *__p, long long __a)
{
    *(volatile long long *)__p = __a;
}

/* ---- Moves and scalar extraction ----------------------------------------- */

__C17_INTRIN __m128d _mm_move_sd(__m128d __a, __m128d __b)
{
    return (__m128d){__b[0], __a[1]};
}

__C17_INTRIN __m128i _mm_move_epi64(__m128i __a)
{
    return (__m128i){__a[0], 0};
}

__C17_INTRIN __m64 _mm_movepi64_pi64(__m128i __a)
{
    return (__m64)__a[0];
}

__C17_INTRIN __m128i _mm_movpi64_epi64(__m64 __a)
{
    return (__m128i){(long long)__a, 0};
}

__C17_INTRIN double _mm_cvtsd_f64(__m128d __a)
{
    return __a[0];
}

__C17_INTRIN int _mm_cvtsi128_si32(__m128i __a)
{
    return ((__v4si)__a)[0];
}

__C17_INTRIN long long _mm_cvtsi128_si64(__m128i __a)
{
    return __a[0];
}

__C17_INTRIN long long _mm_cvtsi128_si64x(__m128i __a)
{
    return __a[0];
}

__C17_INTRIN __m128i _mm_cvtsi32_si128(int __a)
{
    return (__m128i)(__v4si){__a, 0, 0, 0};
}

__C17_INTRIN __m128i _mm_cvtsi64_si128(long long __a)
{
    return (__m128i){__a, 0};
}

__C17_INTRIN __m128i _mm_cvtsi64x_si128(long long __a)
{
    return (__m128i){__a, 0};
}

/* ---- Casts (bit reinterpretation) ----------------------------------------- */

__C17_INTRIN __m128 _mm_castpd_ps(__m128d __a)
{
    return (__m128)__a;
}

__C17_INTRIN __m128i _mm_castpd_si128(__m128d __a)
{
    return (__m128i)__a;
}

__C17_INTRIN __m128d _mm_castps_pd(__m128 __a)
{
    return (__m128d)__a;
}

__C17_INTRIN __m128i _mm_castps_si128(__m128 __a)
{
    return (__m128i)__a;
}

__C17_INTRIN __m128 _mm_castsi128_ps(__m128i __a)
{
    return (__m128)__a;
}

__C17_INTRIN __m128d _mm_castsi128_pd(__m128i __a)
{
    return (__m128d)__a;
}

/* ---- Double-precision arithmetic ------------------------------------------ */

__C17_INTRIN __m128d _mm_add_pd(__m128d __a, __m128d __b)
{
    return (__m128d)((__v2df)__a + (__v2df)__b);
}

__C17_INTRIN __m128d _mm_add_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_pd(__a, __a[0] + __b[0]);
}

__C17_INTRIN __m128d _mm_sub_pd(__m128d __a, __m128d __b)
{
    return (__m128d)((__v2df)__a - (__v2df)__b);
}

__C17_INTRIN __m128d _mm_sub_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_pd(__a, __a[0] - __b[0]);
}

__C17_INTRIN __m128d _mm_mul_pd(__m128d __a, __m128d __b)
{
    return (__m128d)((__v2df)__a * (__v2df)__b);
}

__C17_INTRIN __m128d _mm_mul_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_pd(__a, __a[0] * __b[0]);
}

__C17_INTRIN __m128d _mm_div_pd(__m128d __a, __m128d __b)
{
    return (__m128d)((__v2df)__a / (__v2df)__b);
}

__C17_INTRIN __m128d _mm_div_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_pd(__a, __a[0] / __b[0]);
}

__C17_INTRIN __m128d _mm_sqrt_pd(__m128d __a)
{
    return (__m128d){__c17_sse2_sqrt(__a[0]), __c17_sse2_sqrt(__a[1])};
}

/* SQRTSD: the square root of __b's low lane, __a's high lane. */
__C17_INTRIN __m128d _mm_sqrt_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_pd(__a, __c17_sse2_sqrt(__b[0]));
}

/* MINPD/MAXPD return the second operand unless the first compares strictly
   less (greater): so a NaN in either, or two zeros, give the second. */
__C17_INTRIN __m128d _mm_min_pd(__m128d __a, __m128d __b)
{
    return __c17_sse2_sel_pd((__m128i)((__v2df)__a < (__v2df)__b), __a, __b);
}

__C17_INTRIN __m128d _mm_min_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_pd(__a, __a[0] < __b[0] ? __a[0] : __b[0]);
}

__C17_INTRIN __m128d _mm_max_pd(__m128d __a, __m128d __b)
{
    return __c17_sse2_sel_pd((__m128i)((__v2df)__a > (__v2df)__b), __a, __b);
}

__C17_INTRIN __m128d _mm_max_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_pd(__a, __a[0] > __b[0] ? __a[0] : __b[0]);
}

/* ---- Double-precision logic ------------------------------------------------ */

__C17_INTRIN __m128d _mm_and_pd(__m128d __a, __m128d __b)
{
    return (__m128d)((__v2du)__a & (__v2du)__b);
}

__C17_INTRIN __m128d _mm_andnot_pd(__m128d __a, __m128d __b)
{
    return (__m128d)(~(__v2du)__a & (__v2du)__b);
}

__C17_INTRIN __m128d _mm_or_pd(__m128d __a, __m128d __b)
{
    return (__m128d)((__v2du)__a | (__v2du)__b);
}

__C17_INTRIN __m128d _mm_xor_pd(__m128d __a, __m128d __b)
{
    return (__m128d)((__v2du)__a ^ (__v2du)__b);
}

/* ---- Double-precision comparisons ----------------------------------------- */

/* Packed: each lane all ones where the predicate holds. The "not" forms are
   true for unordered operands, as the instruction's predicates are. */
__C17_INTRIN __m128d _mm_cmpeq_pd(__m128d __a, __m128d __b)
{
    return (__m128d)((__v2df)__a == (__v2df)__b);
}

__C17_INTRIN __m128d _mm_cmplt_pd(__m128d __a, __m128d __b)
{
    return (__m128d)((__v2df)__a < (__v2df)__b);
}

__C17_INTRIN __m128d _mm_cmple_pd(__m128d __a, __m128d __b)
{
    return (__m128d)((__v2df)__a <= (__v2df)__b);
}

__C17_INTRIN __m128d _mm_cmpgt_pd(__m128d __a, __m128d __b)
{
    return (__m128d)((__v2df)__a > (__v2df)__b);
}

__C17_INTRIN __m128d _mm_cmpge_pd(__m128d __a, __m128d __b)
{
    return (__m128d)((__v2df)__a >= (__v2df)__b);
}

__C17_INTRIN __m128d _mm_cmpneq_pd(__m128d __a, __m128d __b)
{
    return (__m128d)((__v2df)__a != (__v2df)__b);
}

__C17_INTRIN __m128d _mm_cmpnlt_pd(__m128d __a, __m128d __b)
{
    return (__m128d)(~((__v2df)__a < (__v2df)__b));
}

__C17_INTRIN __m128d _mm_cmpnle_pd(__m128d __a, __m128d __b)
{
    return (__m128d)(~((__v2df)__a <= (__v2df)__b));
}

__C17_INTRIN __m128d _mm_cmpngt_pd(__m128d __a, __m128d __b)
{
    return (__m128d)(~((__v2df)__a > (__v2df)__b));
}

__C17_INTRIN __m128d _mm_cmpnge_pd(__m128d __a, __m128d __b)
{
    return (__m128d)(~((__v2df)__a >= (__v2df)__b));
}

__C17_INTRIN __m128d _mm_cmpord_pd(__m128d __a, __m128d __b)
{
    return (__m128d)(((__v2df)__a == (__v2df)__a) & ((__v2df)__b == (__v2df)__b));
}

__C17_INTRIN __m128d _mm_cmpunord_pd(__m128d __a, __m128d __b)
{
    return (__m128d)(((__v2df)__a != (__v2df)__a) | ((__v2df)__b != (__v2df)__b));
}

/* Scalar: the predicate's mask in lane 0, __a's lane 1 kept. */
__C17_INTRIN __m128d _mm_cmpeq_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_mask(__a, __a[0] == __b[0]);
}

__C17_INTRIN __m128d _mm_cmplt_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_mask(__a, __a[0] < __b[0]);
}

__C17_INTRIN __m128d _mm_cmple_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_mask(__a, __a[0] <= __b[0]);
}

__C17_INTRIN __m128d _mm_cmpgt_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_mask(__a, __a[0] > __b[0]);
}

__C17_INTRIN __m128d _mm_cmpge_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_mask(__a, __a[0] >= __b[0]);
}

__C17_INTRIN __m128d _mm_cmpneq_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_mask(__a, __a[0] != __b[0]);
}

__C17_INTRIN __m128d _mm_cmpnlt_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_mask(__a, !(__a[0] < __b[0]));
}

__C17_INTRIN __m128d _mm_cmpnle_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_mask(__a, !(__a[0] <= __b[0]));
}

__C17_INTRIN __m128d _mm_cmpngt_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_mask(__a, !(__a[0] > __b[0]));
}

__C17_INTRIN __m128d _mm_cmpnge_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_mask(__a, !(__a[0] >= __b[0]));
}

__C17_INTRIN __m128d _mm_cmpord_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_mask(__a, __a[0] == __a[0] && __b[0] == __b[0]);
}

__C17_INTRIN __m128d _mm_cmpunord_sd(__m128d __a, __m128d __b)
{
    return __c17_sse2_set_low_mask(__a, __a[0] != __a[0] || __b[0] != __b[0]);
}

/* COMISD / UCOMISD on lane 0: 1 where the relation holds. An unordered pair
   satisfies only "not equal". The two forms differ only in which NaNs raise
   the invalid exception. */
__C17_INTRIN int _mm_comieq_sd(__m128d __a, __m128d __b)
{
    return __a[0] == __b[0];
}

__C17_INTRIN int _mm_comilt_sd(__m128d __a, __m128d __b)
{
    return __a[0] < __b[0];
}

__C17_INTRIN int _mm_comile_sd(__m128d __a, __m128d __b)
{
    return __a[0] <= __b[0];
}

__C17_INTRIN int _mm_comigt_sd(__m128d __a, __m128d __b)
{
    return __a[0] > __b[0];
}

__C17_INTRIN int _mm_comige_sd(__m128d __a, __m128d __b)
{
    return __a[0] >= __b[0];
}

__C17_INTRIN int _mm_comineq_sd(__m128d __a, __m128d __b)
{
    return __a[0] != __b[0];
}

__C17_INTRIN int _mm_ucomieq_sd(__m128d __a, __m128d __b)
{
    return __a[0] == __b[0];
}

__C17_INTRIN int _mm_ucomilt_sd(__m128d __a, __m128d __b)
{
    return __a[0] < __b[0];
}

__C17_INTRIN int _mm_ucomile_sd(__m128d __a, __m128d __b)
{
    return __a[0] <= __b[0];
}

__C17_INTRIN int _mm_ucomigt_sd(__m128d __a, __m128d __b)
{
    return __a[0] > __b[0];
}

__C17_INTRIN int _mm_ucomige_sd(__m128d __a, __m128d __b)
{
    return __a[0] >= __b[0];
}

__C17_INTRIN int _mm_ucomineq_sd(__m128d __a, __m128d __b)
{
    return __a[0] != __b[0];
}

/* ---- Conversions ------------------------------------------------------------ */

__C17_INTRIN __m128d _mm_cvtepi32_pd(__m128i __a)
{
    __v4si __v = (__v4si)__a;
    return (__m128d){(double)__v[0], (double)__v[1]};
}

__C17_INTRIN __m128 _mm_cvtepi32_ps(__m128i __a)
{
    return (__m128)__builtin_convertvector((__v4si)__a, __v4sf);
}

__C17_INTRIN __m128i _mm_cvtpd_epi32(__m128d __a)
{
    return (__m128i)(__v4si){__c17_sse2_cvt_f64_i32(__a[0]), __c17_sse2_cvt_f64_i32(__a[1]),
                             0, 0};
}

__C17_INTRIN __m128i _mm_cvttpd_epi32(__m128d __a)
{
    return (__m128i)(__v4si){__c17_sse2_cvtt_f64_i32(__a[0]), __c17_sse2_cvtt_f64_i32(__a[1]),
                             0, 0};
}

__C17_INTRIN __m64 _mm_cvtpd_pi32(__m128d __a)
{
    return (__m64)(__v2si){__c17_sse2_cvt_f64_i32(__a[0]), __c17_sse2_cvt_f64_i32(__a[1])};
}

__C17_INTRIN __m64 _mm_cvttpd_pi32(__m128d __a)
{
    return (__m64)(__v2si){__c17_sse2_cvtt_f64_i32(__a[0]), __c17_sse2_cvtt_f64_i32(__a[1])};
}

__C17_INTRIN __m128d _mm_cvtpi32_pd(__m64 __a)
{
    __v2si __v = (__v2si)__a;
    return (__m128d){(double)__v[0], (double)__v[1]};
}

__C17_INTRIN __m128 _mm_cvtpd_ps(__m128d __a)
{
    return (__m128)(__v4sf){(float)__a[0], (float)__a[1], 0.0f, 0.0f};
}

__C17_INTRIN __m128d _mm_cvtps_pd(__m128 __a)
{
    return (__m128d){(double)__a[0], (double)__a[1]};
}

__C17_INTRIN __m128i _mm_cvtps_epi32(__m128 __a)
{
    return (__m128i)(__v4si){__c17_sse2_cvt_f32_i32(__a[0]), __c17_sse2_cvt_f32_i32(__a[1]),
                             __c17_sse2_cvt_f32_i32(__a[2]), __c17_sse2_cvt_f32_i32(__a[3])};
}

__C17_INTRIN __m128i _mm_cvttps_epi32(__m128 __a)
{
    return (__m128i)(__v4si){__c17_sse2_cvtt_f32_i32(__a[0]), __c17_sse2_cvtt_f32_i32(__a[1]),
                             __c17_sse2_cvtt_f32_i32(__a[2]), __c17_sse2_cvtt_f32_i32(__a[3])};
}

__C17_INTRIN int _mm_cvtsd_si32(__m128d __a)
{
    return __c17_sse2_cvt_f64_i32(__a[0]);
}

__C17_INTRIN int _mm_cvttsd_si32(__m128d __a)
{
    return __c17_sse2_cvtt_f64_i32(__a[0]);
}

__C17_INTRIN long long _mm_cvtsd_si64(__m128d __a)
{
    return __c17_sse2_cvt_f64_i64(__a[0]);
}

__C17_INTRIN long long _mm_cvtsd_si64x(__m128d __a)
{
    return __c17_sse2_cvt_f64_i64(__a[0]);
}

__C17_INTRIN long long _mm_cvttsd_si64(__m128d __a)
{
    return __c17_sse2_cvtt_f64_i64(__a[0]);
}

__C17_INTRIN long long _mm_cvttsd_si64x(__m128d __a)
{
    return __c17_sse2_cvtt_f64_i64(__a[0]);
}

__C17_INTRIN __m128 _mm_cvtsd_ss(__m128 __a, __m128d __b)
{
    __v4sf __r = (__v4sf)__a;
    __r[0] = (float)__b[0];
    return (__m128)__r;
}

__C17_INTRIN __m128d _mm_cvtss_sd(__m128d __a, __m128 __b)
{
    return __c17_sse2_set_low_pd(__a, (double)__b[0]);
}

__C17_INTRIN __m128d _mm_cvtsi32_sd(__m128d __a, int __b)
{
    return __c17_sse2_set_low_pd(__a, (double)__b);
}

__C17_INTRIN __m128d _mm_cvtsi64_sd(__m128d __a, long long __b)
{
    return __c17_sse2_set_low_pd(__a, (double)__b);
}

__C17_INTRIN __m128d _mm_cvtsi64x_sd(__m128d __a, long long __b)
{
    return __c17_sse2_set_low_pd(__a, (double)__b);
}

/* ---- Double-precision shuffles ---------------------------------------------- */

__C17_INTRIN __m128d _mm_unpackhi_pd(__m128d __a, __m128d __b)
{
    return (__m128d){__a[1], __b[1]};
}

__C17_INTRIN __m128d _mm_unpacklo_pd(__m128d __a, __m128d __b)
{
    return (__m128d){__a[0], __b[0]};
}

__C17_INTRIN int _mm_movemask_pd(__m128d __a)
{
    __v2du __v = (__v2du)__a;
    return (int)((__v[0] >> 63) | ((__v[1] >> 63) << 1));
}

/* SHUFPD: lane 0 from __a, lane 1 from __b, each chosen by one bit. */
#define _mm_shuffle_pd(A, B, N)                                                     \
    ((__m128d)__builtin_shufflevector((__v2df)(__m128d)(A), (__v2df)(__m128d)(B),   \
                                      (int)(N) & 1, 2 + (((int)(N) >> 1) & 1)))

/* ---- Integer arithmetic ------------------------------------------------------ */

/* Wrapping lane arithmetic is done in the unsigned lane types, where C
   defines the overflow. */
__C17_INTRIN __m128i _mm_add_epi8(__m128i __a, __m128i __b)
{
    return (__m128i)((__v16qu)__a + (__v16qu)__b);
}

__C17_INTRIN __m128i _mm_add_epi16(__m128i __a, __m128i __b)
{
    return (__m128i)((__v8hu)__a + (__v8hu)__b);
}

__C17_INTRIN __m128i _mm_add_epi32(__m128i __a, __m128i __b)
{
    return (__m128i)((__v4su)__a + (__v4su)__b);
}

__C17_INTRIN __m128i _mm_add_epi64(__m128i __a, __m128i __b)
{
    return (__m128i)((__v2du)__a + (__v2du)__b);
}

__C17_INTRIN __m128i _mm_sub_epi8(__m128i __a, __m128i __b)
{
    return (__m128i)((__v16qu)__a - (__v16qu)__b);
}

__C17_INTRIN __m128i _mm_sub_epi16(__m128i __a, __m128i __b)
{
    return (__m128i)((__v8hu)__a - (__v8hu)__b);
}

__C17_INTRIN __m128i _mm_sub_epi32(__m128i __a, __m128i __b)
{
    return (__m128i)((__v4su)__a - (__v4su)__b);
}

__C17_INTRIN __m128i _mm_sub_epi64(__m128i __a, __m128i __b)
{
    return (__m128i)((__v2du)__a - (__v2du)__b);
}

__C17_INTRIN __m128i _mm_adds_epi8(__m128i __a, __m128i __b)
{
    __v16qs __x = (__v16qs)__a, __y = (__v16qs)__b, __r;
    for (int __i = 0; __i < 16; __i++)
        __r[__i] = (signed char)__c17_sse2_clamp(__x[__i] + __y[__i], -128, 127);
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_adds_epi16(__m128i __a, __m128i __b)
{
    __v8hi __x = (__v8hi)__a, __y = (__v8hi)__b, __r;
    for (int __i = 0; __i < 8; __i++)
        __r[__i] = (short)__c17_sse2_clamp(__x[__i] + __y[__i], -32768, 32767);
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_adds_epu8(__m128i __a, __m128i __b)
{
    __v16qu __x = (__v16qu)__a, __y = (__v16qu)__b, __r;
    for (int __i = 0; __i < 16; __i++)
        __r[__i] = (unsigned char)__c17_sse2_clamp(__x[__i] + __y[__i], 0, 255);
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_adds_epu16(__m128i __a, __m128i __b)
{
    __v8hu __x = (__v8hu)__a, __y = (__v8hu)__b, __r;
    for (int __i = 0; __i < 8; __i++)
        __r[__i] = (unsigned short)__c17_sse2_clamp(__x[__i] + __y[__i], 0, 65535);
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_subs_epi8(__m128i __a, __m128i __b)
{
    __v16qs __x = (__v16qs)__a, __y = (__v16qs)__b, __r;
    for (int __i = 0; __i < 16; __i++)
        __r[__i] = (signed char)__c17_sse2_clamp(__x[__i] - __y[__i], -128, 127);
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_subs_epi16(__m128i __a, __m128i __b)
{
    __v8hi __x = (__v8hi)__a, __y = (__v8hi)__b, __r;
    for (int __i = 0; __i < 8; __i++)
        __r[__i] = (short)__c17_sse2_clamp(__x[__i] - __y[__i], -32768, 32767);
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_subs_epu8(__m128i __a, __m128i __b)
{
    __v16qu __x = (__v16qu)__a, __y = (__v16qu)__b, __r;
    for (int __i = 0; __i < 16; __i++)
        __r[__i] = (unsigned char)__c17_sse2_clamp(__x[__i] - __y[__i], 0, 255);
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_subs_epu16(__m128i __a, __m128i __b)
{
    __v8hu __x = (__v8hu)__a, __y = (__v8hu)__b, __r;
    for (int __i = 0; __i < 8; __i++)
        __r[__i] = (unsigned short)__c17_sse2_clamp(__x[__i] - __y[__i], 0, 65535);
    return (__m128i)__r;
}

/* PMADDWD: adjacent signed 16x16 products summed into 32-bit lanes. The one
   overflowing case (all four -32768) wraps to 0x80000000, done unsigned. */
__C17_INTRIN __m128i _mm_madd_epi16(__m128i __a, __m128i __b)
{
    __v8hi __x = (__v8hi)__a, __y = (__v8hi)__b;
    __v4si __r;
    for (int __i = 0; __i < 4; __i++) {
        unsigned int __lo = (unsigned int)(__x[2 * __i] * __y[2 * __i]);
        unsigned int __hi = (unsigned int)(__x[2 * __i + 1] * __y[2 * __i + 1]);
        __r[__i] = (int)(__lo + __hi);
    }
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_mulhi_epi16(__m128i __a, __m128i __b)
{
    __v8hi __x = (__v8hi)__a, __y = (__v8hi)__b, __r;
    for (int __i = 0; __i < 8; __i++)
        __r[__i] = (short)((__x[__i] * __y[__i]) >> 16);
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_mulhi_epu16(__m128i __a, __m128i __b)
{
    __v8hu __x = (__v8hu)__a, __y = (__v8hu)__b, __r;
    for (int __i = 0; __i < 8; __i++)
        __r[__i] = (unsigned short)(((unsigned int)__x[__i] * __y[__i]) >> 16);
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_mullo_epi16(__m128i __a, __m128i __b)
{
    return (__m128i)((__v8hu)__a * (__v8hu)__b);
}

/* PMULUDQ: the low unsigned 32 bits of each 64-bit lane, multiplied to 64. */
__C17_INTRIN __m128i _mm_mul_epu32(__m128i __a, __m128i __b)
{
    __v2du __m = {0xffffffffULL, 0xffffffffULL};
    return (__m128i)(((__v2du)__a & __m) * ((__v2du)__b & __m));
}

__C17_INTRIN __m64 _mm_mul_su32(__m64 __a, __m64 __b)
{
    __v2si __x = (__v2si)__a, __y = (__v2si)__b;
    return (__m64)((unsigned long long)(unsigned int)__x[0] * (unsigned int)__y[0]);
}

__C17_INTRIN __m128i _mm_max_epi16(__m128i __a, __m128i __b)
{
    return __c17_sse2_sel_si128((__m128i)((__v8hi)__a > (__v8hi)__b), __a, __b);
}

__C17_INTRIN __m128i _mm_min_epi16(__m128i __a, __m128i __b)
{
    return __c17_sse2_sel_si128((__m128i)((__v8hi)__a < (__v8hi)__b), __a, __b);
}

__C17_INTRIN __m128i _mm_max_epu8(__m128i __a, __m128i __b)
{
    return __c17_sse2_sel_si128((__m128i)((__v16qu)__a > (__v16qu)__b), __a, __b);
}

__C17_INTRIN __m128i _mm_min_epu8(__m128i __a, __m128i __b)
{
    return __c17_sse2_sel_si128((__m128i)((__v16qu)__a < (__v16qu)__b), __a, __b);
}

/* PAVGB/PAVGW: (a + b + 1) >> 1 without the intermediate overflowing. */
__C17_INTRIN __m128i _mm_avg_epu8(__m128i __a, __m128i __b)
{
    __v16qu __x = (__v16qu)__a, __y = (__v16qu)__b;
    return (__m128i)((__x | __y) - ((__x ^ __y) >> 1));
}

__C17_INTRIN __m128i _mm_avg_epu16(__m128i __a, __m128i __b)
{
    __v8hu __x = (__v8hu)__a, __y = (__v8hu)__b;
    return (__m128i)((__x | __y) - ((__x ^ __y) >> 1));
}

/* PSADBW: per 8-byte half, the sum of absolute byte differences, in the low
   16 bits of each 64-bit lane. */
__C17_INTRIN __m128i _mm_sad_epu8(__m128i __a, __m128i __b)
{
    __v16qu __x = (__v16qu)__a, __y = (__v16qu)__b;
    __v2di __r;
    for (int __h = 0; __h < 2; __h++) {
        int __s = 0;
        for (int __i = 8 * __h; __i < 8 * __h + 8; __i++)
            __s += __x[__i] > __y[__i] ? __x[__i] - __y[__i] : __y[__i] - __x[__i];
        __r[__h] = __s;
    }
    return (__m128i)__r;
}

/* ---- Integer logic ------------------------------------------------------------ */

__C17_INTRIN __m128i _mm_and_si128(__m128i __a, __m128i __b)
{
    return (__m128i)((__v2du)__a & (__v2du)__b);
}

__C17_INTRIN __m128i _mm_andnot_si128(__m128i __a, __m128i __b)
{
    return (__m128i)(~(__v2du)__a & (__v2du)__b);
}

__C17_INTRIN __m128i _mm_or_si128(__m128i __a, __m128i __b)
{
    return (__m128i)((__v2du)__a | (__v2du)__b);
}

__C17_INTRIN __m128i _mm_xor_si128(__m128i __a, __m128i __b)
{
    return (__m128i)((__v2du)__a ^ (__v2du)__b);
}

/* ---- Integer comparisons --------------------------------------------------------- */

__C17_INTRIN __m128i _mm_cmpeq_epi8(__m128i __a, __m128i __b)
{
    return (__m128i)((__v16qs)__a == (__v16qs)__b);
}

__C17_INTRIN __m128i _mm_cmpeq_epi16(__m128i __a, __m128i __b)
{
    return (__m128i)((__v8hi)__a == (__v8hi)__b);
}

__C17_INTRIN __m128i _mm_cmpeq_epi32(__m128i __a, __m128i __b)
{
    return (__m128i)((__v4si)__a == (__v4si)__b);
}

__C17_INTRIN __m128i _mm_cmpgt_epi8(__m128i __a, __m128i __b)
{
    return (__m128i)((__v16qs)__a > (__v16qs)__b);
}

__C17_INTRIN __m128i _mm_cmpgt_epi16(__m128i __a, __m128i __b)
{
    return (__m128i)((__v8hi)__a > (__v8hi)__b);
}

__C17_INTRIN __m128i _mm_cmpgt_epi32(__m128i __a, __m128i __b)
{
    return (__m128i)((__v4si)__a > (__v4si)__b);
}

__C17_INTRIN __m128i _mm_cmplt_epi8(__m128i __a, __m128i __b)
{
    return (__m128i)((__v16qs)__a < (__v16qs)__b);
}

__C17_INTRIN __m128i _mm_cmplt_epi16(__m128i __a, __m128i __b)
{
    return (__m128i)((__v8hi)__a < (__v8hi)__b);
}

__C17_INTRIN __m128i _mm_cmplt_epi32(__m128i __a, __m128i __b)
{
    return (__m128i)((__v4si)__a < (__v4si)__b);
}

/* ---- Integer shifts ------------------------------------------------------------- */

/* Counts at or past the lane width clear the lane (logical shifts) or fill it
   with the sign (arithmetic shifts). The register-count forms read the low
   64 bits of the count operand. */
__C17_INTRIN __m128i _mm_slli_epi16(__m128i __a, int __c)
{
    if ((unsigned int)__c > 15)
        return (__m128i){0, 0};
    return (__m128i)((__v8hu)__a << __c);
}

__C17_INTRIN __m128i _mm_slli_epi32(__m128i __a, int __c)
{
    if ((unsigned int)__c > 31)
        return (__m128i){0, 0};
    return (__m128i)((__v4su)__a << __c);
}

__C17_INTRIN __m128i _mm_slli_epi64(__m128i __a, int __c)
{
    if ((unsigned int)__c > 63)
        return (__m128i){0, 0};
    return (__m128i)((__v2du)__a << __c);
}

__C17_INTRIN __m128i _mm_srli_epi16(__m128i __a, int __c)
{
    if ((unsigned int)__c > 15)
        return (__m128i){0, 0};
    return (__m128i)((__v8hu)__a >> __c);
}

__C17_INTRIN __m128i _mm_srli_epi32(__m128i __a, int __c)
{
    if ((unsigned int)__c > 31)
        return (__m128i){0, 0};
    return (__m128i)((__v4su)__a >> __c);
}

__C17_INTRIN __m128i _mm_srli_epi64(__m128i __a, int __c)
{
    if ((unsigned int)__c > 63)
        return (__m128i){0, 0};
    return (__m128i)((__v2du)__a >> __c);
}

__C17_INTRIN __m128i _mm_srai_epi16(__m128i __a, int __c)
{
    if ((unsigned int)__c > 15)
        __c = 15;
    return (__m128i)((__v8hi)__a >> __c);
}

__C17_INTRIN __m128i _mm_srai_epi32(__m128i __a, int __c)
{
    if ((unsigned int)__c > 31)
        __c = 31;
    return (__m128i)((__v4si)__a >> __c);
}

/* The register count, clamped to one past the widest lane so it fits an int
   without changing which side of each lane width it falls on. */
__C17_INTRIN int __c17_sse2_shift_count(__m128i __c)
{
    unsigned long long __n = (unsigned long long)__c[0];
    return __n > 64 ? 64 : (int)__n;
}

__C17_INTRIN __m128i _mm_sll_epi16(__m128i __a, __m128i __c)
{
    return _mm_slli_epi16(__a, __c17_sse2_shift_count(__c));
}

__C17_INTRIN __m128i _mm_sll_epi32(__m128i __a, __m128i __c)
{
    return _mm_slli_epi32(__a, __c17_sse2_shift_count(__c));
}

__C17_INTRIN __m128i _mm_sll_epi64(__m128i __a, __m128i __c)
{
    return _mm_slli_epi64(__a, __c17_sse2_shift_count(__c));
}

__C17_INTRIN __m128i _mm_srl_epi16(__m128i __a, __m128i __c)
{
    return _mm_srli_epi16(__a, __c17_sse2_shift_count(__c));
}

__C17_INTRIN __m128i _mm_srl_epi32(__m128i __a, __m128i __c)
{
    return _mm_srli_epi32(__a, __c17_sse2_shift_count(__c));
}

__C17_INTRIN __m128i _mm_srl_epi64(__m128i __a, __m128i __c)
{
    return _mm_srli_epi64(__a, __c17_sse2_shift_count(__c));
}

__C17_INTRIN __m128i _mm_sra_epi16(__m128i __a, __m128i __c)
{
    return _mm_srai_epi16(__a, __c17_sse2_shift_count(__c));
}

__C17_INTRIN __m128i _mm_sra_epi32(__m128i __a, __m128i __c)
{
    return _mm_srai_epi32(__a, __c17_sse2_shift_count(__c));
}

/* PSLLDQ/PSRLDQ: whole-register byte shifts by an immediate; 16 or more
   clears the register. Result byte I takes index __C17_SSE2_BSL/BSR(I, N) of the
   pair (zero, A): 0..15 name a zero byte, 16..31 the bytes of A. */
#define __C17_SSE2_BSL(I, N) ((N) > 15 ? 0 : (I) < (N) ? 0 : 16 + (I) - (N))
#define __C17_SSE2_BSR(I, N) ((N) > 15 ? 0 : (I) + (N) > 15 ? 0 : 16 + (I) + (N))

#define __C17_SSE2_BYTESHIFT(A, N, S)                                               \
    ((__m128i)__builtin_shufflevector(                                              \
        (__v16qu){0}, (__v16qu)(__m128i)(A), S(0, N), S(1, N), S(2, N), S(3, N),    \
        S(4, N), S(5, N), S(6, N), S(7, N), S(8, N), S(9, N), S(10, N), S(11, N),   \
        S(12, N), S(13, N), S(14, N), S(15, N)))

#define _mm_bslli_si128(A, N) __C17_SSE2_BYTESHIFT(A, ((int)(N) & 0xff), __C17_SSE2_BSL)
#define _mm_bsrli_si128(A, N) __C17_SSE2_BYTESHIFT(A, ((int)(N) & 0xff), __C17_SSE2_BSR)
#define _mm_slli_si128(A, N) _mm_bslli_si128(A, N)
#define _mm_srli_si128(A, N) _mm_bsrli_si128(A, N)

/* ---- Integer pack, unpack and shuffle ------------------------------------------ */

__C17_INTRIN __m128i _mm_packs_epi16(__m128i __a, __m128i __b)
{
    __v8hi __x = (__v8hi)__a, __y = (__v8hi)__b;
    __v16qs __r;
    for (int __i = 0; __i < 8; __i++) {
        __r[__i] = (signed char)__c17_sse2_clamp(__x[__i], -128, 127);
        __r[__i + 8] = (signed char)__c17_sse2_clamp(__y[__i], -128, 127);
    }
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_packs_epi32(__m128i __a, __m128i __b)
{
    __v4si __x = (__v4si)__a, __y = (__v4si)__b;
    __v8hi __r;
    for (int __i = 0; __i < 4; __i++) {
        __r[__i] = (short)__c17_sse2_clamp(__x[__i], -32768, 32767);
        __r[__i + 4] = (short)__c17_sse2_clamp(__y[__i], -32768, 32767);
    }
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_packus_epi16(__m128i __a, __m128i __b)
{
    __v8hi __x = (__v8hi)__a, __y = (__v8hi)__b;
    __v16qu __r;
    for (int __i = 0; __i < 8; __i++) {
        __r[__i] = (unsigned char)__c17_sse2_clamp(__x[__i], 0, 255);
        __r[__i + 8] = (unsigned char)__c17_sse2_clamp(__y[__i], 0, 255);
    }
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_unpackhi_epi8(__m128i __a, __m128i __b)
{
    return (__m128i)__builtin_shufflevector((__v16qu)__a, (__v16qu)__b, 8, 24, 9, 25, 10, 26,
                                            11, 27, 12, 28, 13, 29, 14, 30, 15, 31);
}

__C17_INTRIN __m128i _mm_unpackhi_epi16(__m128i __a, __m128i __b)
{
    return (__m128i)__builtin_shufflevector((__v8hu)__a, (__v8hu)__b, 4, 12, 5, 13, 6, 14, 7,
                                            15);
}

__C17_INTRIN __m128i _mm_unpackhi_epi32(__m128i __a, __m128i __b)
{
    return (__m128i)__builtin_shufflevector((__v4su)__a, (__v4su)__b, 2, 6, 3, 7);
}

__C17_INTRIN __m128i _mm_unpackhi_epi64(__m128i __a, __m128i __b)
{
    return (__m128i){__a[1], __b[1]};
}

__C17_INTRIN __m128i _mm_unpacklo_epi8(__m128i __a, __m128i __b)
{
    return (__m128i)__builtin_shufflevector((__v16qu)__a, (__v16qu)__b, 0, 16, 1, 17, 2, 18, 3,
                                            19, 4, 20, 5, 21, 6, 22, 7, 23);
}

__C17_INTRIN __m128i _mm_unpacklo_epi16(__m128i __a, __m128i __b)
{
    return (__m128i)__builtin_shufflevector((__v8hu)__a, (__v8hu)__b, 0, 8, 1, 9, 2, 10, 3, 11);
}

__C17_INTRIN __m128i _mm_unpacklo_epi32(__m128i __a, __m128i __b)
{
    return (__m128i)__builtin_shufflevector((__v4su)__a, (__v4su)__b, 0, 4, 1, 5);
}

__C17_INTRIN __m128i _mm_unpacklo_epi64(__m128i __a, __m128i __b)
{
    return (__m128i){__a[0], __b[0]};
}

__C17_INTRIN int _mm_movemask_epi8(__m128i __a)
{
    __v16qu __v = (__v16qu)__a;
    int __r = 0;
    for (int __i = 0; __i < 16; __i++)
        __r |= (__v[__i] >> 7) << __i;
    return __r;
}

/* PEXTRW zero-extends the selected word; PINSRW replaces it with the low 16
   bits of D. */
__C17_INTRIN int __c17_sse2_extract_epi16(__m128i __a, int __n)
{
    return ((__v8hu)__a)[__n];
}

__C17_INTRIN __m128i __c17_sse2_insert_epi16(__m128i __a, int __d, int __n)
{
    __v8hi __r = (__v8hi)__a;
    __r[__n] = (short)__d;
    return (__m128i)__r;
}

#define _mm_extract_epi16(A, N) __c17_sse2_extract_epi16((__m128i)(A), (int)(N) & 7)
#define _mm_insert_epi16(A, D, N) __c17_sse2_insert_epi16((__m128i)(A), (int)(D), (int)(N) & 7)

/* PSHUFD / PSHUFHW / PSHUFLW: two immediate bits pick each source lane; the
   HW/LW forms copy the other half of the register unchanged. A is named once
   (the second shuffle operand is an unused zero) so it is evaluated once. */
#define _mm_shuffle_epi32(A, N)                                                     \
    ((__m128i)__builtin_shufflevector((__v4si)(__m128i)(A), (__v4si){0},            \
                                      (int)(N) & 3, ((int)(N) >> 2) & 3,            \
                                      ((int)(N) >> 4) & 3, ((int)(N) >> 6) & 3))

#define _mm_shufflelo_epi16(A, N)                                                   \
    ((__m128i)__builtin_shufflevector((__v8hi)(__m128i)(A), (__v8hi){0},            \
                                      (int)(N) & 3, ((int)(N) >> 2) & 3,            \
                                      ((int)(N) >> 4) & 3, ((int)(N) >> 6) & 3,     \
                                      4, 5,                                         \
                                      6, 7))

#define _mm_shufflehi_epi16(A, N)                                                   \
    ((__m128i)__builtin_shufflevector((__v8hi)(__m128i)(A), (__v8hi){0}, 0,         \
                                      1, 2, 3, 4 + ((int)(N) & 3),                  \
                                      4 + (((int)(N) >> 2) & 3),                    \
                                      4 + (((int)(N) >> 4) & 3),                    \
                                      4 + (((int)(N) >> 6) & 3)))

/* ---- Cache control and ordering -------------------------------------------------- */

__C17_INTRIN void _mm_clflush(void const *__p)
{
    __asm__ __volatile__("clflush %0" : : "m"(*(char const *)__p) : "memory");
}

__C17_INTRIN void _mm_lfence(void)
{
    __asm__ __volatile__("lfence" : : : "memory");
}

__C17_INTRIN void _mm_mfence(void)
{
    __asm__ __volatile__("mfence" : : : "memory");
}

#endif /* _EMMINTRIN_H_INCLUDED */
