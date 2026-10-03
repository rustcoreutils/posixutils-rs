/*
 * c17 builtin nmmintrin.h - SSE4.2 intrinsics
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

#ifndef _NMMINTRIN_H_INCLUDED
#define _NMMINTRIN_H_INCLUDED

#include <smmintrin.h>
#include <popcntintrin.h>

/* The string-compare machinery is large: left to the inliner's judgement,
   not forced into every call, or a file of many compares takes the compiler
   minutes. */
#define __C17_SSE42_HELPER static __inline__

/* SSE4.2 functions follow. */

/* PCMPxSTRx immediate fields. */
#define _SIDD_UBYTE_OPS 0x00
#define _SIDD_UWORD_OPS 0x01
#define _SIDD_SBYTE_OPS 0x02
#define _SIDD_SWORD_OPS 0x03

#define _SIDD_CMP_EQUAL_ANY 0x00
#define _SIDD_CMP_RANGES 0x04
#define _SIDD_CMP_EQUAL_EACH 0x08
#define _SIDD_CMP_EQUAL_ORDERED 0x0c

#define _SIDD_POSITIVE_POLARITY 0x00
#define _SIDD_NEGATIVE_POLARITY 0x10
#define _SIDD_MASKED_POSITIVE_POLARITY 0x20
#define _SIDD_MASKED_NEGATIVE_POLARITY 0x30

#define _SIDD_LEAST_SIGNIFICANT 0x00
#define _SIDD_MOST_SIGNIFICANT 0x40

#define _SIDD_BIT_MASK 0x00
#define _SIDD_UNIT_MASK 0x40

/* Element I of V in the immediate's format (bits 1:0). */
__C17_SSE42_HELPER int __c17_sse42_elem(__m128i __v, int __i, int __m)
{
    switch (__m & 3) {
    case 0:
        return ((__v16qu)__v)[__i];
    case 1:
        return ((__v8hu)__v)[__i];
    case 2:
        return ((__v16qs)__v)[__i];
    default:
        return ((__v8hi)__v)[__i];
    }
}

/* Elements per register: 16 bytes or 8 words. */
__C17_INTRIN int __c17_sse42_count(int __m)
{
    return (__m & 1) ? 8 : 16;
}

/* Implicit length: the elements before the first zero element. */
__C17_SSE42_HELPER int __c17_sse42_ilen(__m128i __v, int __m)
{
    int __n = __c17_sse42_count(__m), __i;

    for (__i = 0; __i < __n; __i++)
        if (__c17_sse42_elem(__v, __i, __m) == 0)
            return __i;
    return __n;
}

/* Explicit length: the absolute value of the length register, saturated
   to the element count. */
__C17_SSE42_HELPER int __c17_sse42_elen(int __l, int __m)
{
    long long __a = __l < 0 ? -(long long)__l : (long long)__l;
    int __n = __c17_sse42_count(__m);

    return __a > __n ? __n : (int)__a;
}

/* Result bits of __c17_sse42_pcmpstr above IntRes2. */
#define __C17_SSE42_CF 0x10000
#define __C17_SSE42_ZF 0x20000
#define __C17_SSE42_SF 0x40000
#define __C17_SSE42_OF 0x80000

/* The PCMPxSTRx core: A (the first operand, with LA valid elements) is
   compared against B (with LB valid elements) by the aggregation the
   immediate names, polarity applied; returns IntRes2 in bits 15:0 and the
   flags as __C17_SSE42_*. */
__C17_SSE42_HELPER int __c17_sse42_pcmpstr(__m128i __a, int __la, __m128i __b, int __lb, int __m)
{
    int __n = __c17_sse42_count(__m);
    int __full = (1 << __n) - 1;
    int __r1 = 0, __r2, __i, __j, __e, __ok;

    switch ((__m >> 2) & 3) {
    case 0: /* equal any: B[j] is one of the valid A elements */
        for (__j = 0; __j < __lb; __j++) {
            __e = __c17_sse42_elem(__b, __j, __m);
            for (__i = 0; __i < __la; __i++)
                if (__c17_sse42_elem(__a, __i, __m) == __e) {
                    __r1 |= 1 << __j;
                    break;
                }
        }
        break;
    case 1: /* ranges: B[j] lies in some valid [A[i], A[i+1]] pair */
        for (__j = 0; __j < __lb; __j++) {
            __e = __c17_sse42_elem(__b, __j, __m);
            for (__i = 0; __i + 1 < __la; __i += 2)
                if (__e >= __c17_sse42_elem(__a, __i, __m) && __e <= __c17_sse42_elem(__a, __i + 1, __m)) {
                    __r1 |= 1 << __j;
                    break;
                }
        }
        break;
    case 2: /* equal each: both invalid compares true, one invalid false */
        for (__i = 0; __i < __n; __i++) {
            if (__i >= __la || __i >= __lb)
                __ok = __i >= __la && __i >= __lb;
            else
                __ok = __c17_sse42_elem(__a, __i, __m) == __c17_sse42_elem(__b, __i, __m);
            __r1 |= __ok << __i;
        }
        break;
    default: /* equal ordered: A occurs in B at j; invalid A matches all */
        for (__j = 0; __j < __n; __j++) {
            __ok = 1;
            for (__i = 0; __i < __n - __j && __i < __la; __i++)
                if (__i + __j >= __lb ||
                    __c17_sse42_elem(__a, __i, __m) != __c17_sse42_elem(__b, __i + __j, __m)) {
                    __ok = 0;
                    break;
                }
            __r1 |= __ok << __j;
        }
        break;
    }

    switch ((__m >> 4) & 3) {
    case 1:
        __r2 = ~__r1 & __full;
        break;
    case 3:
        __r2 = __r1 ^ ((1 << __lb) - 1);
        break;
    default:
        __r2 = __r1;
        break;
    }

    return __r2 | (__r2 ? __C17_SSE42_CF : 0) | (__lb < __n ? __C17_SSE42_ZF : 0) |
           (__la < __n ? __C17_SSE42_SF : 0) | ((__r2 & 1) ? __C17_SSE42_OF : 0);
}

/* The index result: lowest or highest set bit, or the element count. */
__C17_INTRIN int __c17_sse42_index(int __r, int __m)
{
    __r &= 0xffff;
    if (__r == 0)
        return __c17_sse42_count(__m);
    if (__m & 0x40)
        return 31 - __builtin_clz((unsigned int)__r);
    return __builtin_ctz((unsigned int)__r);
}

/* The mask result: IntRes2 zero-extended, or expanded to whole elements. */
__C17_SSE42_HELPER __m128i __c17_sse42_mask(int __r, int __m)
{
    __v8hu __w = {0};
    __v16qu __c = {0};
    int __i;

    if (!(__m & 0x40)) {
        __w[0] = (unsigned short)__r;
        return (__m128i)__w;
    }
    if (__m & 1) {
        for (__i = 0; __i < 8; __i++)
            __w[__i] = ((__r >> __i) & 1) ? 0xffff : 0;
        return (__m128i)__w;
    }
    for (__i = 0; __i < 16; __i++)
        __c[__i] = ((__r >> __i) & 1) ? 0xff : 0;
    return (__m128i)__c;
}

/* Implicit-length forms. */
__C17_SSE42_HELPER int __c17_sse42_pcmpistr(__m128i __x, __m128i __y, int __m)
{
    return __c17_sse42_pcmpstr(__x, __c17_sse42_ilen(__x, __m), __y, __c17_sse42_ilen(__y, __m), __m);
}

__C17_INTRIN __m128i _mm_cmpistrm(__m128i __x, __m128i __y, const int __m)
{
    return __c17_sse42_mask(__c17_sse42_pcmpistr(__x, __y, __m), __m);
}

__C17_INTRIN int _mm_cmpistri(__m128i __x, __m128i __y, const int __m)
{
    return __c17_sse42_index(__c17_sse42_pcmpistr(__x, __y, __m), __m);
}

__C17_INTRIN int _mm_cmpistra(__m128i __x, __m128i __y, const int __m)
{
    return (__c17_sse42_pcmpistr(__x, __y, __m) & (__C17_SSE42_CF | __C17_SSE42_ZF)) == 0;
}

__C17_INTRIN int _mm_cmpistrc(__m128i __x, __m128i __y, const int __m)
{
    return (__c17_sse42_pcmpistr(__x, __y, __m) & __C17_SSE42_CF) != 0;
}

__C17_INTRIN int _mm_cmpistro(__m128i __x, __m128i __y, const int __m)
{
    return (__c17_sse42_pcmpistr(__x, __y, __m) & __C17_SSE42_OF) != 0;
}

__C17_INTRIN int _mm_cmpistrs(__m128i __x, __m128i __y, const int __m)
{
    return (__c17_sse42_pcmpistr(__x, __y, __m) & __C17_SSE42_SF) != 0;
}

__C17_INTRIN int _mm_cmpistrz(__m128i __x, __m128i __y, const int __m)
{
    return (__c17_sse42_pcmpistr(__x, __y, __m) & __C17_SSE42_ZF) != 0;
}

/* Explicit-length forms. */
__C17_SSE42_HELPER int __c17_sse42_pcmpestr(__m128i __x, int __lx, __m128i __y, int __ly, int __m)
{
    return __c17_sse42_pcmpstr(__x, __c17_sse42_elen(__lx, __m), __y, __c17_sse42_elen(__ly, __m), __m);
}

__C17_INTRIN __m128i _mm_cmpestrm(__m128i __x, int __lx, __m128i __y, int __ly, const int __m)
{
    return __c17_sse42_mask(__c17_sse42_pcmpestr(__x, __lx, __y, __ly, __m), __m);
}

__C17_INTRIN int _mm_cmpestri(__m128i __x, int __lx, __m128i __y, int __ly, const int __m)
{
    return __c17_sse42_index(__c17_sse42_pcmpestr(__x, __lx, __y, __ly, __m), __m);
}

__C17_INTRIN int _mm_cmpestra(__m128i __x, int __lx, __m128i __y, int __ly, const int __m)
{
    return (__c17_sse42_pcmpestr(__x, __lx, __y, __ly, __m) & (__C17_SSE42_CF | __C17_SSE42_ZF)) == 0;
}

__C17_INTRIN int _mm_cmpestrc(__m128i __x, int __lx, __m128i __y, int __ly, const int __m)
{
    return (__c17_sse42_pcmpestr(__x, __lx, __y, __ly, __m) & __C17_SSE42_CF) != 0;
}

__C17_INTRIN int _mm_cmpestro(__m128i __x, int __lx, __m128i __y, int __ly, const int __m)
{
    return (__c17_sse42_pcmpestr(__x, __lx, __y, __ly, __m) & __C17_SSE42_OF) != 0;
}

__C17_INTRIN int _mm_cmpestrs(__m128i __x, int __lx, __m128i __y, int __ly, const int __m)
{
    return (__c17_sse42_pcmpestr(__x, __lx, __y, __ly, __m) & __C17_SSE42_SF) != 0;
}

__C17_INTRIN int _mm_cmpestrz(__m128i __x, int __lx, __m128i __y, int __ly, const int __m)
{
    return (__c17_sse42_pcmpestr(__x, __lx, __y, __ly, __m) & __C17_SSE42_ZF) != 0;
}

/* PCMPGTQ: signed 64-bit greater-than mask. */
__C17_INTRIN __m128i _mm_cmpgt_epi64(__m128i __x, __m128i __y)
{
    return (__m128i)((__v2di)__x > (__v2di)__y);
}

/* CRC32: CRC-32C (Castagnoli), bit-reflected, polynomial 0x82F63B78, no
   pre- or post-inversion; the value's bytes are consumed low first. */
__C17_INTRIN unsigned int __c17_sse42_crc32c(unsigned int __c, unsigned long long __v, int __bytes)
{
    int __i, __k;

    for (__i = 0; __i < __bytes; __i++) {
        __c ^= (unsigned int)(__v >> (8 * __i)) & 0xffu;
        for (__k = 0; __k < 8; __k++)
            __c = (__c >> 1) ^ (0x82f63b78u & (0u - (__c & 1u)));
    }
    return __c;
}

__C17_INTRIN unsigned int _mm_crc32_u8(unsigned int __c, unsigned char __v)
{
    return __c17_sse42_crc32c(__c, __v, 1);
}

__C17_INTRIN unsigned int _mm_crc32_u16(unsigned int __c, unsigned short __v)
{
    return __c17_sse42_crc32c(__c, __v, 2);
}

__C17_INTRIN unsigned int _mm_crc32_u32(unsigned int __c, unsigned int __v)
{
    return __c17_sse42_crc32c(__c, __v, 4);
}

__C17_INTRIN unsigned long long _mm_crc32_u64(unsigned long long __c, unsigned long long __v)
{
    return __c17_sse42_crc32c((unsigned int)__c, __v, 8);
}

#endif /* _NMMINTRIN_H_INCLUDED */
