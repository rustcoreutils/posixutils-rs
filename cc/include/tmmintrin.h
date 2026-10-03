/*
 * c17 builtin tmmintrin.h - SSSE3 intrinsics
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

#ifndef _TMMINTRIN_H_INCLUDED
#define _TMMINTRIN_H_INCLUDED

#include <pmmintrin.h>

/* SSSE3 functions follow. */

/* The 64-bit (__m64) forms run the 128-bit form on the operands placed in
   the low quadword and keep the low quadword of the result. */
__C17_INTRIN __m128i __c17_ssse3_widen(__m64 __lo, __m64 __hi)
{
    return (__m128i)(__v2di){((__v1di)__lo)[0], ((__v1di)__hi)[0]};
}

__C17_INTRIN __m64 __c17_ssse3_low(__m128i __x)
{
    return (__m64)(__v1di){((__v2di)__x)[0]};
}

/* Signed 16-bit saturating add and subtract, computed with wrapping
   unsigned lanes: a lane overflowed when the sign of the wrapped result is
   impossible for its operands, and then saturates toward the sign of __x. */
__C17_INTRIN __v8hi __c17_ssse3_adds16(__v8hi __x, __v8hi __y)
{
    __v8hi __s = (__v8hi)((__v8hu)__x + (__v8hu)__y);
    __v8hi __ov = (__v8hi)(((__x ^ __s) & (__y ^ __s)) < 0);
    __v8hi __sat = (__x >> 15) ^ 0x7fff;
    return (__s & ~__ov) | (__sat & __ov);
}

__C17_INTRIN __v8hi __c17_ssse3_subs16(__v8hi __x, __v8hi __y)
{
    __v8hi __s = (__v8hi)((__v8hu)__x - (__v8hu)__y);
    __v8hi __ov = (__v8hi)(((__x ^ __y) & (__x ^ __s)) < 0);
    __v8hi __sat = (__x >> 15) ^ 0x7fff;
    return (__s & ~__ov) | (__sat & __ov);
}

/* Even-indexed and odd-indexed lanes of the concatenation a:b. */
__C17_INTRIN __v8hi __c17_ssse3_even16(__m128i __a, __m128i __b)
{
    return __builtin_shufflevector((__v8hi)__a, (__v8hi)__b,
                                   0, 2, 4, 6, 8, 10, 12, 14);
}

__C17_INTRIN __v8hi __c17_ssse3_odd16(__m128i __a, __m128i __b)
{
    return __builtin_shufflevector((__v8hi)__a, (__v8hi)__b,
                                   1, 3, 5, 7, 9, 11, 13, 15);
}

/* PHADDW: wrapping sums of adjacent pairs, a's pairs then b's. */
__C17_INTRIN __m128i _mm_hadd_epi16(__m128i __a, __m128i __b)
{
    return (__m128i)((__v8hu)__c17_ssse3_even16(__a, __b) +
                     (__v8hu)__c17_ssse3_odd16(__a, __b));
}

/* PHADDD */
__C17_INTRIN __m128i _mm_hadd_epi32(__m128i __a, __m128i __b)
{
    __v4su __e = __builtin_shufflevector((__v4su)__a, (__v4su)__b, 0, 2, 4, 6);
    __v4su __o = __builtin_shufflevector((__v4su)__a, (__v4su)__b, 1, 3, 5, 7);
    return (__m128i)(__e + __o);
}

/* PHADDSW: signed-saturating sums of adjacent pairs. */
__C17_INTRIN __m128i _mm_hadds_epi16(__m128i __a, __m128i __b)
{
    return (__m128i)__c17_ssse3_adds16(__c17_ssse3_even16(__a, __b),
                                       __c17_ssse3_odd16(__a, __b));
}

/* PHSUBW: wrapping differences of adjacent pairs (even minus odd). */
__C17_INTRIN __m128i _mm_hsub_epi16(__m128i __a, __m128i __b)
{
    return (__m128i)((__v8hu)__c17_ssse3_even16(__a, __b) -
                     (__v8hu)__c17_ssse3_odd16(__a, __b));
}

/* PHSUBD */
__C17_INTRIN __m128i _mm_hsub_epi32(__m128i __a, __m128i __b)
{
    __v4su __e = __builtin_shufflevector((__v4su)__a, (__v4su)__b, 0, 2, 4, 6);
    __v4su __o = __builtin_shufflevector((__v4su)__a, (__v4su)__b, 1, 3, 5, 7);
    return (__m128i)(__e - __o);
}

/* PHSUBSW */
__C17_INTRIN __m128i _mm_hsubs_epi16(__m128i __a, __m128i __b)
{
    return (__m128i)__c17_ssse3_subs16(__c17_ssse3_even16(__a, __b),
                                       __c17_ssse3_odd16(__a, __b));
}

__C17_INTRIN __m64 _mm_hadd_pi16(__m64 __a, __m64 __b)
{
    __m128i __x = __c17_ssse3_widen(__a, __b);
    return __c17_ssse3_low(_mm_hadd_epi16(__x, __x));
}

__C17_INTRIN __m64 _mm_hadd_pi32(__m64 __a, __m64 __b)
{
    __m128i __x = __c17_ssse3_widen(__a, __b);
    return __c17_ssse3_low(_mm_hadd_epi32(__x, __x));
}

__C17_INTRIN __m64 _mm_hadds_pi16(__m64 __a, __m64 __b)
{
    __m128i __x = __c17_ssse3_widen(__a, __b);
    return __c17_ssse3_low(_mm_hadds_epi16(__x, __x));
}

__C17_INTRIN __m64 _mm_hsub_pi16(__m64 __a, __m64 __b)
{
    __m128i __x = __c17_ssse3_widen(__a, __b);
    return __c17_ssse3_low(_mm_hsub_epi16(__x, __x));
}

__C17_INTRIN __m64 _mm_hsub_pi32(__m64 __a, __m64 __b)
{
    __m128i __x = __c17_ssse3_widen(__a, __b);
    return __c17_ssse3_low(_mm_hsub_epi32(__x, __x));
}

__C17_INTRIN __m64 _mm_hsubs_pi16(__m64 __a, __m64 __b)
{
    __m128i __x = __c17_ssse3_widen(__a, __b);
    return __c17_ssse3_low(_mm_hsubs_epi16(__x, __x));
}

/* PMADDUBSW: unsigned bytes of a times signed bytes of b; adjacent
   products summed with signed saturation. Each product fits in 16 bits. */
__C17_INTRIN __m128i _mm_maddubs_epi16(__m128i __a, __m128i __b)
{
    __v8hi __ae = (__v8hi)((__v8hu)__a & 0xff);
    __v8hi __ao = (__v8hi)((__v8hu)__a >> 8);
    __v8hi __be = (__v8hi)((__v8hu)__b << 8) >> 8;
    __v8hi __bo = (__v8hi)__b >> 8;
    return (__m128i)__c17_ssse3_adds16(__ae * __be, __ao * __bo);
}

__C17_INTRIN __m64 _mm_maddubs_pi16(__m64 __a, __m64 __b)
{
    return __c17_ssse3_low(_mm_maddubs_epi16(__c17_ssse3_widen(__a, __a),
                                             __c17_ssse3_widen(__b, __b)));
}

/* PMULHRSW: (((a * b) >> 14) + 1) >> 1, low 16 bits; computed per 32-bit
   lane on the sign-extended low and high halves. */
__C17_INTRIN __m128i _mm_mulhrs_epi16(__m128i __a, __m128i __b)
{
    __v4si __al = (__v4si)((__v4su)__a << 16) >> 16;
    __v4si __ah = (__v4si)__a >> 16;
    __v4si __bl = (__v4si)((__v4su)__b << 16) >> 16;
    __v4si __bh = (__v4si)__b >> 16;
    __v4su __rl = (__v4su)((((__al * __bl) >> 14) + 1) >> 1);
    __v4su __rh = (__v4su)((((__ah * __bh) >> 14) + 1) >> 1);
    return (__m128i)((__rl & 0xffff) | (__rh << 16));
}

__C17_INTRIN __m64 _mm_mulhrs_pi16(__m64 __a, __m64 __b)
{
    return __c17_ssse3_low(_mm_mulhrs_epi16(__c17_ssse3_widen(__a, __a),
                                            __c17_ssse3_widen(__b, __b)));
}

/* PSHUFB: each byte of b selects a byte of a by its low four bits, or
   gives zero when its high bit is set. */
__C17_INTRIN __m128i _mm_shuffle_epi8(__m128i __a, __m128i __b)
{
    __v16qu __r = __builtin_shuffle((__v16qu)__a, (__v16qu)__b & 0x0f);
    __v16qu __z = (__v16qu)((__v16qs)__b < 0);
    return (__m128i)(__r & ~__z);
}

/* The 64-bit form indexes with three bits; the high bit still zeroes. */
__C17_INTRIN __m64 _mm_shuffle_pi8(__m64 __a, __m64 __b)
{
    __m128i __m = (__m128i)((__v16qu)__c17_ssse3_widen(__b, __b) & 0x87);
    return __c17_ssse3_low(_mm_shuffle_epi8(__c17_ssse3_widen(__a, __a), __m));
}

/* PSIGNB/W/D: a negated (wrapping) where b < 0, zero where b == 0. */
__C17_INTRIN __m128i _mm_sign_epi8(__m128i __a, __m128i __b)
{
    __v16qu __n = (__v16qu)((__v16qs)__b < 0);
    __v16qu __z = (__v16qu)((__v16qs)__b == 0);
    return (__m128i)((((__v16qu)__a ^ __n) - __n) & ~__z);
}

__C17_INTRIN __m128i _mm_sign_epi16(__m128i __a, __m128i __b)
{
    __v8hu __n = (__v8hu)((__v8hi)__b < 0);
    __v8hu __z = (__v8hu)((__v8hi)__b == 0);
    return (__m128i)((((__v8hu)__a ^ __n) - __n) & ~__z);
}

__C17_INTRIN __m128i _mm_sign_epi32(__m128i __a, __m128i __b)
{
    __v4su __n = (__v4su)((__v4si)__b < 0);
    __v4su __z = (__v4su)((__v4si)__b == 0);
    return (__m128i)((((__v4su)__a ^ __n) - __n) & ~__z);
}

__C17_INTRIN __m64 _mm_sign_pi8(__m64 __a, __m64 __b)
{
    return __c17_ssse3_low(_mm_sign_epi8(__c17_ssse3_widen(__a, __a),
                                         __c17_ssse3_widen(__b, __b)));
}

__C17_INTRIN __m64 _mm_sign_pi16(__m64 __a, __m64 __b)
{
    return __c17_ssse3_low(_mm_sign_epi16(__c17_ssse3_widen(__a, __a),
                                          __c17_ssse3_widen(__b, __b)));
}

__C17_INTRIN __m64 _mm_sign_pi32(__m64 __a, __m64 __b)
{
    return __c17_ssse3_low(_mm_sign_epi32(__c17_ssse3_widen(__a, __a),
                                          __c17_ssse3_widen(__b, __b)));
}

/* PALIGNR: the 32-byte concatenation a:b (b in the low half) shifted right
   by __n bytes; the low 16 bytes are the result, zero past the end. */
__C17_INTRIN __m128i __c17_ssse3_alignr(__m128i __a, __m128i __b, int __n)
{
    unsigned int __c = (unsigned int)__n & 0xff;
    if (__c >= 32)
        return (__m128i)(__v2di){0, 0};
    __v16qu __i = (__v16qu){0, 1, 2, 3, 4, 5, 6, 7,
                            8, 9, 10, 11, 12, 13, 14, 15} + (unsigned char)__c;
    __v16qu __r = __builtin_shuffle((__v16qu)__b, (__v16qu)__a, __i & 31);
    __v16qu __keep = (__v16qu)(__i < 32);
    return (__m128i)(__r & __keep);
}

#define _mm_alignr_epi8(a, b, n) \
    __c17_ssse3_alignr((__m128i)(a), (__m128i)(b), (int)(n))

/* The 64-bit form: a:b is 16 bytes, so zero from a 16-byte shift on. */
#define _mm_alignr_pi8(a, b, n) \
    __c17_ssse3_low(__c17_ssse3_alignr( \
        (__m128i)(__v2di){0, 0}, \
        __c17_ssse3_widen((__m64)(b), (__m64)(a)), (int)(n)))

/* PABSB/W/D: absolute value; the most negative value maps to itself. */
__C17_INTRIN __m128i _mm_abs_epi8(__m128i __a)
{
    __v16qu __n = (__v16qu)((__v16qs)__a < 0);
    return (__m128i)(((__v16qu)__a ^ __n) - __n);
}

__C17_INTRIN __m128i _mm_abs_epi16(__m128i __a)
{
    __v8hu __n = (__v8hu)((__v8hi)__a < 0);
    return (__m128i)(((__v8hu)__a ^ __n) - __n);
}

__C17_INTRIN __m128i _mm_abs_epi32(__m128i __a)
{
    __v4su __n = (__v4su)((__v4si)__a < 0);
    return (__m128i)(((__v4su)__a ^ __n) - __n);
}

__C17_INTRIN __m64 _mm_abs_pi8(__m64 __a)
{
    return __c17_ssse3_low(_mm_abs_epi8(__c17_ssse3_widen(__a, __a)));
}

__C17_INTRIN __m64 _mm_abs_pi16(__m64 __a)
{
    return __c17_ssse3_low(_mm_abs_epi16(__c17_ssse3_widen(__a, __a)));
}

__C17_INTRIN __m64 _mm_abs_pi32(__m64 __a)
{
    return __c17_ssse3_low(_mm_abs_epi32(__c17_ssse3_widen(__a, __a)));
}

#endif /* _TMMINTRIN_H_INCLUDED */
