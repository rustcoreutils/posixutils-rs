/*
 * c17 builtin smmintrin.h - SSE4.1 intrinsics
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

#ifndef _SMMINTRIN_H_INCLUDED
#define _SMMINTRIN_H_INCLUDED

#include <tmmintrin.h>

/* SSE4.1 functions follow. */

/* Rounding-control immediates of the round family. */
#define _MM_FROUND_TO_NEAREST_INT 0x00
#define _MM_FROUND_TO_NEG_INF 0x01
#define _MM_FROUND_TO_POS_INF 0x02
#define _MM_FROUND_TO_ZERO 0x03
#define _MM_FROUND_CUR_DIRECTION 0x04
#define _MM_FROUND_RAISE_EXC 0x00
#define _MM_FROUND_NO_EXC 0x08
#define _MM_FROUND_NINT (_MM_FROUND_TO_NEAREST_INT | _MM_FROUND_RAISE_EXC)
#define _MM_FROUND_FLOOR (_MM_FROUND_TO_NEG_INF | _MM_FROUND_RAISE_EXC)
#define _MM_FROUND_CEIL (_MM_FROUND_TO_POS_INF | _MM_FROUND_RAISE_EXC)
#define _MM_FROUND_TRUNC (_MM_FROUND_TO_ZERO | _MM_FROUND_RAISE_EXC)
#define _MM_FROUND_RINT (_MM_FROUND_CUR_DIRECTION | _MM_FROUND_RAISE_EXC)
#define _MM_FROUND_NEARBYINT (_MM_FROUND_CUR_DIRECTION | _MM_FROUND_NO_EXC)

/* ROUNDSS/ROUNDSD on one value. A NaN comes back quieted with its payload;
   bit 2 of the immediate selects the current MXCSR mode, otherwise bits 1:0
   name the mode. Ties-to-even is computed here, never by the current mode. */
__C17_INTRIN float __c17_sse41_round_f32(float __x, int __m)
{
    union {
        float __f;
        unsigned int __u;
    } __b;
    float __f, __d;
    long long __i;

    if (__x != __x) {
        __b.__f = __x;
        __b.__u |= 0x00400000u;
        return __b.__f;
    }
    if (__m & 4)
        return __builtin_rintf(__x);
    switch (__m & 3) {
    case 1:
        return __builtin_floorf(__x);
    case 2:
        return __builtin_ceilf(__x);
    case 3:
        return __builtin_truncf(__x);
    default:
        break;
    }
    if (!(__builtin_fabsf(__x) < 8388608.0f))
        return __x;
    __f = __builtin_floorf(__x);
    __d = __x - __f;
    __i = (long long)__f;
    if (__d > 0.5f || (__d == 0.5f && (__i & 1)))
        __f += 1.0f;
    return __builtin_copysignf(__f, __x);
}

__C17_INTRIN double __c17_sse41_round_f64(double __x, int __m)
{
    union {
        double __f;
        unsigned long long __u;
    } __b;
    double __f, __d;
    long long __i;

    if (__x != __x) {
        __b.__f = __x;
        __b.__u |= 0x0008000000000000ull;
        return __b.__f;
    }
    if (__m & 4)
        return __builtin_rint(__x);
    switch (__m & 3) {
    case 1:
        return __builtin_floor(__x);
    case 2:
        return __builtin_ceil(__x);
    case 3:
        return __builtin_trunc(__x);
    default:
        break;
    }
    if (!(__builtin_fabs(__x) < 4503599627370496.0))
        return __x;
    __f = __builtin_floor(__x);
    __d = __x - __f;
    __i = (long long)__f;
    if (__d > 0.5 || (__d == 0.5 && (__i & 1)))
        __f += 1.0;
    return __builtin_copysign(__f, __x);
}

__C17_INTRIN __m128d _mm_round_pd(__m128d __v, const int __m)
{
    __v2df __r = (__v2df)__v;

    __r[0] = __c17_sse41_round_f64(__r[0], __m);
    __r[1] = __c17_sse41_round_f64(__r[1], __m);
    return (__m128d)__r;
}

__C17_INTRIN __m128d _mm_round_sd(__m128d __d, __m128d __v, const int __m)
{
    __v2df __r = (__v2df)__d;

    __r[0] = __c17_sse41_round_f64(((__v2df)__v)[0], __m);
    return (__m128d)__r;
}

__C17_INTRIN __m128 _mm_round_ps(__m128 __v, const int __m)
{
    __v4sf __r = (__v4sf)__v;
    int __i;

    for (__i = 0; __i < 4; __i++)
        __r[__i] = __c17_sse41_round_f32(__r[__i], __m);
    return (__m128)__r;
}

__C17_INTRIN __m128 _mm_round_ss(__m128 __d, __m128 __v, const int __m)
{
    __v4sf __r = (__v4sf)__d;

    __r[0] = __c17_sse41_round_f32(((__v4sf)__v)[0], __m);
    return (__m128)__r;
}

#define _mm_ceil_pd(V) _mm_round_pd((V), _MM_FROUND_CEIL)
#define _mm_ceil_sd(D, V) _mm_round_sd((D), (V), _MM_FROUND_CEIL)
#define _mm_floor_pd(V) _mm_round_pd((V), _MM_FROUND_FLOOR)
#define _mm_floor_sd(D, V) _mm_round_sd((D), (V), _MM_FROUND_FLOOR)
#define _mm_ceil_ps(V) _mm_round_ps((V), _MM_FROUND_CEIL)
#define _mm_ceil_ss(D, V) _mm_round_ss((D), (V), _MM_FROUND_CEIL)
#define _mm_floor_ps(V) _mm_round_ps((V), _MM_FROUND_FLOOR)
#define _mm_floor_ss(D, V) _mm_round_ss((D), (V), _MM_FROUND_FLOOR)

/* PTEST: ZF is "M AND V is zero", CF is "NOT M AND V is zero". */
__C17_INTRIN int _mm_testz_si128(__m128i __m, __m128i __v)
{
    __v2du __t = (__v2du)__m & (__v2du)__v;

    return (__t[0] | __t[1]) == 0;
}

__C17_INTRIN int _mm_testc_si128(__m128i __m, __m128i __v)
{
    __v2du __t = ~(__v2du)__m & (__v2du)__v;

    return (__t[0] | __t[1]) == 0;
}

__C17_INTRIN int _mm_testnzc_si128(__m128i __m, __m128i __v)
{
    return !_mm_testz_si128(__m, __v) && !_mm_testc_si128(__m, __v);
}

__C17_INTRIN int _mm_test_all_zeros(__m128i __m, __m128i __v)
{
    return _mm_testz_si128(__m, __v);
}

__C17_INTRIN int _mm_test_all_ones(__m128i __v)
{
    __v2du __t = (__v2du)__v;

    return (__t[0] & __t[1]) == ~0ull;
}

__C17_INTRIN int _mm_test_mix_ones_zeros(__m128i __m, __m128i __v)
{
    return _mm_testnzc_si128(__m, __v);
}

/* Blends: a set selector takes the lane from Y. */
__C17_INTRIN __m128i _mm_blend_epi16(__m128i __x, __m128i __y, const int __m)
{
    __v8hi __s;
    int __i;

    for (__i = 0; __i < 8; __i++)
        __s[__i] = (short)-((__m >> __i) & 1);
    return (__m128i)(((__v8hi)__x & ~__s) | ((__v8hi)__y & __s));
}

__C17_INTRIN __m128i _mm_blendv_epi8(__m128i __x, __m128i __y, __m128i __m)
{
    __v16qs __s = (__v16qs)__m < (__v16qs){0};

    return (__m128i)(((__v16qs)__x & ~__s) | ((__v16qs)__y & __s));
}

__C17_INTRIN __m128 _mm_blend_ps(__m128 __x, __m128 __y, const int __m)
{
    __v4si __s;
    int __i;

    for (__i = 0; __i < 4; __i++)
        __s[__i] = -((__m >> __i) & 1);
    return (__m128)(((__v4si)__x & ~__s) | ((__v4si)__y & __s));
}

__C17_INTRIN __m128 _mm_blendv_ps(__m128 __x, __m128 __y, __m128 __m)
{
    __v4si __s = (__v4si)__m < (__v4si){0};

    return (__m128)(((__v4si)__x & ~__s) | ((__v4si)__y & __s));
}

__C17_INTRIN __m128d _mm_blend_pd(__m128d __x, __m128d __y, const int __m)
{
    __v2di __s;

    __s[0] = -(long long)(__m & 1);
    __s[1] = -(long long)((__m >> 1) & 1);
    return (__m128d)(((__v2di)__x & ~__s) | ((__v2di)__y & __s));
}

__C17_INTRIN __m128d _mm_blendv_pd(__m128d __x, __m128d __y, __m128d __m)
{
    __v2di __s = (__v2di)__m < (__v2di){0};

    return (__m128d)(((__v2di)__x & ~__s) | ((__v2di)__y & __s));
}

/* DPPS: the products the high nibble selects (others +0.0) are summed, and
   the sum goes to the lanes the low nibble selects (others +0.0). Operand
   order matters only for NaNs -- x86 arithmetic returns the first operand's
   NaN when both are NaN -- and the hardware sums in a different order for
   each lane: lane K gets T[K] + T[K^2] with T[K] = P[K^1] + P[K]. */
__C17_INTRIN __m128 _mm_dp_ps(__m128 __x, __m128 __y, const int __m)
{
    __v4sf __a = (__v4sf)__x, __b = (__v4sf)__y, __r;
    float __p[4], __t[4];
    int __i;

    for (__i = 0; __i < 4; __i++)
        __p[__i] = ((__m >> (4 + __i)) & 1) ? __a[__i] * __b[__i] : 0.0f;
    for (__i = 0; __i < 4; __i++)
        __t[__i] = __p[__i ^ 1] + __p[__i];
    for (__i = 0; __i < 4; __i++)
        __r[__i] = ((__m >> __i) & 1) ? __t[__i] + __t[__i ^ 2] : 0.0f;
    return (__m128)__r;
}

/* DPPD: as DPPS over two lanes; lane K gets P[K] + P[K^1]. */
__C17_INTRIN __m128d _mm_dp_pd(__m128d __x, __m128d __y, const int __m)
{
    __v2df __a = (__v2df)__x, __b = (__v2df)__y, __r;
    double __p0, __p1;

    __p0 = (__m & 0x10) ? __a[0] * __b[0] : 0.0;
    __p1 = (__m & 0x20) ? __a[1] * __b[1] : 0.0;
    __r[0] = (__m & 1) ? __p0 + __p1 : 0.0;
    __r[1] = (__m & 2) ? __p1 + __p0 : 0.0;
    return (__m128d)__r;
}

__C17_INTRIN __m128i _mm_cmpeq_epi64(__m128i __x, __m128i __y)
{
    return (__m128i)((__v2di)__x == (__v2di)__y);
}

/* Packed integer min/max, selected through comparison masks. */
__C17_INTRIN __m128i _mm_min_epi8(__m128i __x, __m128i __y)
{
    __v16qs __a = (__v16qs)__x, __b = (__v16qs)__y, __s = __a < __b;

    return (__m128i)((__a & __s) | (__b & ~__s));
}

__C17_INTRIN __m128i _mm_max_epi8(__m128i __x, __m128i __y)
{
    __v16qs __a = (__v16qs)__x, __b = (__v16qs)__y, __s = __a > __b;

    return (__m128i)((__a & __s) | (__b & ~__s));
}

__C17_INTRIN __m128i _mm_min_epu16(__m128i __x, __m128i __y)
{
    __v8hu __a = (__v8hu)__x, __b = (__v8hu)__y;
    __v8hu __s = (__v8hu)(__a < __b);

    return (__m128i)((__a & __s) | (__b & ~__s));
}

__C17_INTRIN __m128i _mm_max_epu16(__m128i __x, __m128i __y)
{
    __v8hu __a = (__v8hu)__x, __b = (__v8hu)__y;
    __v8hu __s = (__v8hu)(__a > __b);

    return (__m128i)((__a & __s) | (__b & ~__s));
}

__C17_INTRIN __m128i _mm_min_epi32(__m128i __x, __m128i __y)
{
    __v4si __a = (__v4si)__x, __b = (__v4si)__y, __s = __a < __b;

    return (__m128i)((__a & __s) | (__b & ~__s));
}

__C17_INTRIN __m128i _mm_max_epi32(__m128i __x, __m128i __y)
{
    __v4si __a = (__v4si)__x, __b = (__v4si)__y, __s = __a > __b;

    return (__m128i)((__a & __s) | (__b & ~__s));
}

__C17_INTRIN __m128i _mm_min_epu32(__m128i __x, __m128i __y)
{
    __v4su __a = (__v4su)__x, __b = (__v4su)__y;
    __v4su __s = (__v4su)(__a < __b);

    return (__m128i)((__a & __s) | (__b & ~__s));
}

__C17_INTRIN __m128i _mm_max_epu32(__m128i __x, __m128i __y)
{
    __v4su __a = (__v4su)__x, __b = (__v4su)__y;
    __v4su __s = (__v4su)(__a > __b);

    return (__m128i)((__a & __s) | (__b & ~__s));
}

/* PMULLD keeps the low 32 bits: multiply unsigned so it wraps. */
__C17_INTRIN __m128i _mm_mullo_epi32(__m128i __x, __m128i __y)
{
    return (__m128i)((__v4su)__x * (__v4su)__y);
}

/* PMULDQ: signed 32x32->64 of the even lanes. */
__C17_INTRIN __m128i _mm_mul_epi32(__m128i __x, __m128i __y)
{
    __v4si __a = (__v4si)__x, __b = (__v4si)__y;
    __v2di __r;

    __r[0] = (long long)__a[0] * (long long)__b[0];
    __r[1] = (long long)__a[2] * (long long)__b[2];
    return (__m128i)__r;
}

/* INSERTPS: S lane N[7:6] goes to lane N[5:4] of D, then the lanes set in
   N[3:0] are zeroed. */
__C17_INTRIN __m128 _mm_insert_ps(__m128 __d, __m128 __s, const int __n)
{
    __v4sf __r = (__v4sf)__d;
    int __i;

    __r[(__n >> 4) & 3] = ((__v4sf)__s)[(__n >> 6) & 3];
    for (__i = 0; __i < 4; __i++)
        if ((__n >> __i) & 1)
            __r[__i] = 0.0f;
    return (__m128)__r;
}

/* EXTRACTPS returns the lane's bit pattern. */
__C17_INTRIN int _mm_extract_ps(__m128 __x, const int __n)
{
    return ((__v4si)__x)[__n & 3];
}

/* Lane N of S as a float, stored to D. */
#define _MM_EXTRACT_FLOAT(D, S, N) \
    do { \
        (D) = ((__v4sf)(S))[(N) & 3]; \
    } while (0)

#define _MM_MK_INSERTPS_NDX(S, D, M) (((S) << 6) | ((D) << 4) | (M))

/* The single-lane shuffle insertps names in its immediate. */
#define _MM_PICK_OUT_PS(X, N) _mm_insert_ps((__m128)(__v4sf){0}, (X), _MM_MK_INSERTPS_NDX((N), 0, 0x0e))

__C17_INTRIN __m128i _mm_insert_epi8(__m128i __d, int __s, const int __n)
{
    __v16qs __r = (__v16qs)__d;

    __r[__n & 15] = (signed char)__s;
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_insert_epi32(__m128i __d, int __s, const int __n)
{
    __v4si __r = (__v4si)__d;

    __r[__n & 3] = __s;
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_insert_epi64(__m128i __d, long long __s, const int __n)
{
    __v2di __r = (__v2di)__d;

    __r[__n & 1] = __s;
    return (__m128i)__r;
}

/* PEXTRB zero-extends the byte. */
__C17_INTRIN int _mm_extract_epi8(__m128i __x, const int __n)
{
    return ((__v16qu)__x)[__n & 15];
}

__C17_INTRIN int _mm_extract_epi32(__m128i __x, const int __n)
{
    return ((__v4si)__x)[__n & 3];
}

__C17_INTRIN long long _mm_extract_epi64(__m128i __x, const int __n)
{
    return ((__v2di)__x)[__n & 1];
}

/* PHMINPOSUW: the smallest unsigned word in lane 0, its lowest index in
   lane 1, zeros above. */
__C17_INTRIN __m128i _mm_minpos_epu16(__m128i __x)
{
    __v8hu __a = (__v8hu)__x, __r = {0};
    int __i, __k = 0;

    for (__i = 1; __i < 8; __i++)
        if (__a[__i] < __a[__k])
            __k = __i;
    __r[0] = __a[__k];
    __r[1] = (unsigned short)__k;
    return (__m128i)__r;
}

/* PMOVSX / PMOVZX: widen the low lanes. */
__C17_INTRIN __m128i _mm_cvtepi8_epi16(__m128i __x)
{
    __v16qs __a = (__v16qs)__x;
    __v8hi __r;
    int __i;

    for (__i = 0; __i < 8; __i++)
        __r[__i] = __a[__i];
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_cvtepi8_epi32(__m128i __x)
{
    __v16qs __a = (__v16qs)__x;
    __v4si __r;
    int __i;

    for (__i = 0; __i < 4; __i++)
        __r[__i] = __a[__i];
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_cvtepi8_epi64(__m128i __x)
{
    __v16qs __a = (__v16qs)__x;
    __v2di __r;

    __r[0] = __a[0];
    __r[1] = __a[1];
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_cvtepi16_epi32(__m128i __x)
{
    __v8hi __a = (__v8hi)__x;
    __v4si __r;
    int __i;

    for (__i = 0; __i < 4; __i++)
        __r[__i] = __a[__i];
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_cvtepi16_epi64(__m128i __x)
{
    __v8hi __a = (__v8hi)__x;
    __v2di __r;

    __r[0] = __a[0];
    __r[1] = __a[1];
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_cvtepi32_epi64(__m128i __x)
{
    __v4si __a = (__v4si)__x;
    __v2di __r;

    __r[0] = __a[0];
    __r[1] = __a[1];
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_cvtepu8_epi16(__m128i __x)
{
    __v16qu __a = (__v16qu)__x;
    __v8hi __r;
    int __i;

    for (__i = 0; __i < 8; __i++)
        __r[__i] = __a[__i];
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_cvtepu8_epi32(__m128i __x)
{
    __v16qu __a = (__v16qu)__x;
    __v4si __r;
    int __i;

    for (__i = 0; __i < 4; __i++)
        __r[__i] = __a[__i];
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_cvtepu8_epi64(__m128i __x)
{
    __v16qu __a = (__v16qu)__x;
    __v2di __r;

    __r[0] = __a[0];
    __r[1] = __a[1];
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_cvtepu16_epi32(__m128i __x)
{
    __v8hu __a = (__v8hu)__x;
    __v4si __r;
    int __i;

    for (__i = 0; __i < 4; __i++)
        __r[__i] = __a[__i];
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_cvtepu16_epi64(__m128i __x)
{
    __v8hu __a = (__v8hu)__x;
    __v2di __r;

    __r[0] = __a[0];
    __r[1] = __a[1];
    return (__m128i)__r;
}

__C17_INTRIN __m128i _mm_cvtepu32_epi64(__m128i __x)
{
    __v4su __a = (__v4su)__x;
    __v2di __r;

    __r[0] = __a[0];
    __r[1] = __a[1];
    return (__m128i)__r;
}

/* PACKUSDW: signed dwords saturated to unsigned words, X then Y. */
__C17_INTRIN __m128i _mm_packus_epi32(__m128i __x, __m128i __y)
{
    __v4si __a = (__v4si)__x, __b = (__v4si)__y;
    __v8hu __r;
    int __i, __v;

    for (__i = 0; __i < 8; __i++) {
        __v = __i < 4 ? __a[__i] : __b[__i - 4];
        __r[__i] = (unsigned short)(__v < 0 ? 0 : __v > 0xffff ? 0xffff : __v);
    }
    return (__m128i)__r;
}

/* MPSADBW: eight sums of absolute differences between the 4-byte block of
   Y at Y-offset M[1:0]*4 and sliding 4-byte windows of X from M[2]*4. */
__C17_INTRIN __m128i _mm_mpsadbw_epu8(__m128i __x, __m128i __y, const int __m)
{
    __v16qu __a = (__v16qu)__x, __b = (__v16qu)__y;
    __v8hu __r;
    int __ao = ((__m >> 2) & 1) * 4, __bo = (__m & 3) * 4;
    int __j, __k, __s, __d;

    for (__j = 0; __j < 8; __j++) {
        __s = 0;
        for (__k = 0; __k < 4; __k++) {
            __d = (int)__a[__ao + __j + __k] - (int)__b[__bo + __k];
            __s += __d < 0 ? -__d : __d;
        }
        __r[__j] = (unsigned short)__s;
    }
    return (__m128i)__r;
}

/* MOVNTDQA: the non-temporal hint has no portable spelling; a plain
   aligned load gives the same value. */
__C17_INTRIN __m128i _mm_stream_load_si128(const void *__p)
{
    return *(const __m128i *)__p;
}

/* gcc and clang declare the SSE4.2 functions in this header too. */
#include <nmmintrin.h>

#endif /* _SMMINTRIN_H_INCLUDED */
