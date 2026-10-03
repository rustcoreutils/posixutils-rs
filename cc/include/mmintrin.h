/*
 * c17 builtin mmintrin.h - MMX intrinsics
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

#ifndef _MMINTRIN_H_INCLUDED
#define _MMINTRIN_H_INCLUDED

/* The attributes every intrinsic carries: always inlined, and no symbol of
   its own. */
#ifndef __C17_INTRIN
#define __C17_INTRIN static __inline__ __attribute__((__always_inline__, __artificial__))
#endif

typedef int __m64 __attribute__((__vector_size__(8), __may_alias__));
typedef int __m64_u __attribute__((__vector_size__(8), __may_alias__, __aligned__(1)));

/* Internal lane views of an __m64. */
typedef int __v2si __attribute__((__vector_size__(8)));
typedef short __v4hi __attribute__((__vector_size__(8)));
typedef char __v8qi __attribute__((__vector_size__(8)));
typedef long long __v1di __attribute__((__vector_size__(8)));
typedef float __v2sf __attribute__((__vector_size__(8)));

/* Signedness-explicit lane views, and the 16-byte views the saturating
   operations widen into. */
typedef signed char __c17_mmx_v8qs __attribute__((__vector_size__(8)));
typedef unsigned char __c17_mmx_v8qu __attribute__((__vector_size__(8)));
typedef unsigned short __c17_mmx_v4hu __attribute__((__vector_size__(8)));
typedef unsigned int __c17_mmx_v2su __attribute__((__vector_size__(8)));
typedef unsigned long long __c17_mmx_v1du __attribute__((__vector_size__(8)));
typedef short __c17_mmx_v8hw __attribute__((__vector_size__(16)));
typedef int __c17_mmx_v4sw __attribute__((__vector_size__(16)));

/* Clamp every lane of a widened vector into [__lo, __hi], given as
   vectors. (Scalar bounds here are miscompiled by c17 at -O1 and above
   once inlined: the blend came out as all-__lo.) */
__C17_INTRIN __c17_mmx_v8hw __c17_mmx_clamp_v8hw(__c17_mmx_v8hw __v, __c17_mmx_v8hw __lo,
                                                 __c17_mmx_v8hw __hi)
{
    __c17_mmx_v8hw __below = __v < __lo;
    __c17_mmx_v8hw __above = __v > __hi;
    __v = (__v & ~__below) | (__lo & __below);
    return (__v & ~__above) | (__hi & __above);
}

__C17_INTRIN __c17_mmx_v4sw __c17_mmx_clamp_v4sw(__c17_mmx_v4sw __v, __c17_mmx_v4sw __lo,
                                                 __c17_mmx_v4sw __hi)
{
    __c17_mmx_v4sw __below = __v < __lo;
    __c17_mmx_v4sw __above = __v > __hi;
    __v = (__v & ~__below) | (__lo & __below);
    return (__v & ~__above) | (__hi & __above);
}

#define __C17_MMX_SPLAT8(x) ((__c17_mmx_v8hw){x, x, x, x, x, x, x, x})
#define __C17_MMX_SPLAT4(x) ((__c17_mmx_v4sw){x, x, x, x})

/* Saturate widened lanes back to the narrow type. */
__C17_INTRIN __m64 __c17_mmx_sat_s8(__c17_mmx_v8hw __v)
{
    __v = __c17_mmx_clamp_v8hw(__v, __C17_MMX_SPLAT8(-128), __C17_MMX_SPLAT8(127));
    return (__m64)__builtin_convertvector(__v, __c17_mmx_v8qs);
}

__C17_INTRIN __m64 __c17_mmx_sat_u8(__c17_mmx_v8hw __v)
{
    __v = __c17_mmx_clamp_v8hw(__v, __C17_MMX_SPLAT8(0), __C17_MMX_SPLAT8(255));
    return (__m64)__builtin_convertvector(__v, __c17_mmx_v8qu);
}

__C17_INTRIN __m64 __c17_mmx_sat_s16(__c17_mmx_v4sw __v)
{
    __v = __c17_mmx_clamp_v4sw(__v, __C17_MMX_SPLAT4(-32768), __C17_MMX_SPLAT4(32767));
    return (__m64)__builtin_convertvector(__v, __v4hi);
}

__C17_INTRIN __m64 __c17_mmx_sat_u16(__c17_mmx_v4sw __v)
{
    __v = __c17_mmx_clamp_v4sw(__v, __C17_MMX_SPLAT4(0), __C17_MMX_SPLAT4(65535));
    return (__m64)__builtin_convertvector(__v, __c17_mmx_v4hu);
}

/* EMMS: c17 keeps no value in the x87/MMX register file across intrinsics,
   so there is no state to clear. */
__C17_INTRIN void _mm_empty(void) {}
__C17_INTRIN void _m_empty(void) {}

/* Moves between integers and __m64. */
__C17_INTRIN __m64 _mm_cvtsi32_si64(int __i) { return (__m64)(__v2si){__i, 0}; }
__C17_INTRIN __m64 _m_from_int(int __i) { return _mm_cvtsi32_si64(__i); }
__C17_INTRIN int _mm_cvtsi64_si32(__m64 __m) { return ((__v2si)__m)[0]; }
__C17_INTRIN int _m_to_int(__m64 __m) { return _mm_cvtsi64_si32(__m); }
__C17_INTRIN __m64 _mm_cvtsi64_m64(long long __i) { return (__m64)(__v1di){__i}; }
__C17_INTRIN __m64 _m_from_int64(long long __i) { return _mm_cvtsi64_m64(__i); }
__C17_INTRIN __m64 _mm_cvtsi64x_si64(long long __i) { return _mm_cvtsi64_m64(__i); }
__C17_INTRIN __m64 _mm_set_pi64x(long long __i) { return _mm_cvtsi64_m64(__i); }
__C17_INTRIN long long _mm_cvtm64_si64(__m64 __m) { return ((__v1di)__m)[0]; }
__C17_INTRIN long long _m_to_int64(__m64 __m) { return _mm_cvtm64_si64(__m); }
__C17_INTRIN long long _mm_cvtsi64_si64x(__m64 __m) { return _mm_cvtm64_si64(__m); }

/* Packing with saturation: the first operand fills the low half. */
__C17_INTRIN __m64 _mm_packs_pi16(__m64 __a, __m64 __b)
{
    __c17_mmx_v8hw __w = __builtin_shufflevector((__v4hi)__a, (__v4hi)__b, 0, 1, 2, 3, 4, 5, 6, 7);
    return __c17_mmx_sat_s8(__w);
}

__C17_INTRIN __m64 _mm_packs_pi32(__m64 __a, __m64 __b)
{
    __c17_mmx_v4sw __w = __builtin_shufflevector((__v2si)__a, (__v2si)__b, 0, 1, 2, 3);
    return __c17_mmx_sat_s16(__w);
}

__C17_INTRIN __m64 _mm_packs_pu16(__m64 __a, __m64 __b)
{
    __c17_mmx_v8hw __w = __builtin_shufflevector((__v4hi)__a, (__v4hi)__b, 0, 1, 2, 3, 4, 5, 6, 7);
    return __c17_mmx_sat_u8(__w);
}

/* Interleaving. */
__C17_INTRIN __m64 _mm_unpackhi_pi8(__m64 __a, __m64 __b)
{
    return (__m64)__builtin_shufflevector((__v8qi)__a, (__v8qi)__b, 4, 12, 5, 13, 6, 14, 7, 15);
}

__C17_INTRIN __m64 _mm_unpackhi_pi16(__m64 __a, __m64 __b)
{
    return (__m64)__builtin_shufflevector((__v4hi)__a, (__v4hi)__b, 2, 6, 3, 7);
}

__C17_INTRIN __m64 _mm_unpackhi_pi32(__m64 __a, __m64 __b)
{
    return (__m64)__builtin_shufflevector((__v2si)__a, (__v2si)__b, 1, 3);
}

__C17_INTRIN __m64 _mm_unpacklo_pi8(__m64 __a, __m64 __b)
{
    return (__m64)__builtin_shufflevector((__v8qi)__a, (__v8qi)__b, 0, 8, 1, 9, 2, 10, 3, 11);
}

__C17_INTRIN __m64 _mm_unpacklo_pi16(__m64 __a, __m64 __b)
{
    return (__m64)__builtin_shufflevector((__v4hi)__a, (__v4hi)__b, 0, 4, 1, 5);
}

__C17_INTRIN __m64 _mm_unpacklo_pi32(__m64 __a, __m64 __b)
{
    return (__m64)__builtin_shufflevector((__v2si)__a, (__v2si)__b, 0, 2);
}

/* Wrapping addition and subtraction, done on unsigned lanes. */
__C17_INTRIN __m64 _mm_add_pi8(__m64 __a, __m64 __b) { return (__m64)((__c17_mmx_v8qu)__a + (__c17_mmx_v8qu)__b); }
__C17_INTRIN __m64 _mm_add_pi16(__m64 __a, __m64 __b) { return (__m64)((__c17_mmx_v4hu)__a + (__c17_mmx_v4hu)__b); }
__C17_INTRIN __m64 _mm_add_pi32(__m64 __a, __m64 __b) { return (__m64)((__c17_mmx_v2su)__a + (__c17_mmx_v2su)__b); }
__C17_INTRIN __m64 _mm_add_si64(__m64 __a, __m64 __b) { return (__m64)((__c17_mmx_v1du)__a + (__c17_mmx_v1du)__b); }
__C17_INTRIN __m64 _mm_sub_pi8(__m64 __a, __m64 __b) { return (__m64)((__c17_mmx_v8qu)__a - (__c17_mmx_v8qu)__b); }
__C17_INTRIN __m64 _mm_sub_pi16(__m64 __a, __m64 __b) { return (__m64)((__c17_mmx_v4hu)__a - (__c17_mmx_v4hu)__b); }
__C17_INTRIN __m64 _mm_sub_pi32(__m64 __a, __m64 __b) { return (__m64)((__c17_mmx_v2su)__a - (__c17_mmx_v2su)__b); }
__C17_INTRIN __m64 _mm_sub_si64(__m64 __a, __m64 __b) { return (__m64)((__c17_mmx_v1du)__a - (__c17_mmx_v1du)__b); }

/* Saturating addition and subtraction, computed in wider lanes. */
__C17_INTRIN __c17_mmx_v8hw __c17_mmx_widen_s8(__m64 __a) { return __builtin_convertvector((__c17_mmx_v8qs)__a, __c17_mmx_v8hw); }
__C17_INTRIN __c17_mmx_v8hw __c17_mmx_widen_u8(__m64 __a) { return __builtin_convertvector((__c17_mmx_v8qu)__a, __c17_mmx_v8hw); }
__C17_INTRIN __c17_mmx_v4sw __c17_mmx_widen_s16(__m64 __a) { return __builtin_convertvector((__v4hi)__a, __c17_mmx_v4sw); }
__C17_INTRIN __c17_mmx_v4sw __c17_mmx_widen_u16(__m64 __a) { return __builtin_convertvector((__c17_mmx_v4hu)__a, __c17_mmx_v4sw); }

__C17_INTRIN __m64 _mm_adds_pi8(__m64 __a, __m64 __b)
{
    return __c17_mmx_sat_s8(__c17_mmx_widen_s8(__a) + __c17_mmx_widen_s8(__b));
}

__C17_INTRIN __m64 _mm_adds_pi16(__m64 __a, __m64 __b)
{
    return __c17_mmx_sat_s16(__c17_mmx_widen_s16(__a) + __c17_mmx_widen_s16(__b));
}

__C17_INTRIN __m64 _mm_adds_pu8(__m64 __a, __m64 __b)
{
    return __c17_mmx_sat_u8(__c17_mmx_widen_u8(__a) + __c17_mmx_widen_u8(__b));
}

__C17_INTRIN __m64 _mm_adds_pu16(__m64 __a, __m64 __b)
{
    return __c17_mmx_sat_u16(__c17_mmx_widen_u16(__a) + __c17_mmx_widen_u16(__b));
}

__C17_INTRIN __m64 _mm_subs_pi8(__m64 __a, __m64 __b)
{
    return __c17_mmx_sat_s8(__c17_mmx_widen_s8(__a) - __c17_mmx_widen_s8(__b));
}

__C17_INTRIN __m64 _mm_subs_pi16(__m64 __a, __m64 __b)
{
    return __c17_mmx_sat_s16(__c17_mmx_widen_s16(__a) - __c17_mmx_widen_s16(__b));
}

__C17_INTRIN __m64 _mm_subs_pu8(__m64 __a, __m64 __b)
{
    return __c17_mmx_sat_u8(__c17_mmx_widen_u8(__a) - __c17_mmx_widen_u8(__b));
}

__C17_INTRIN __m64 _mm_subs_pu16(__m64 __a, __m64 __b)
{
    return __c17_mmx_sat_u16(__c17_mmx_widen_u16(__a) - __c17_mmx_widen_u16(__b));
}

/* Multiplication. PMADDWD sums two 32-bit products; their sum wraps only
   for -32768 * -32768 twice, so the addition is done unsigned. */
__C17_INTRIN __m64 _mm_madd_pi16(__m64 __a, __m64 __b)
{
    __c17_mmx_v4sw __p = __c17_mmx_widen_s16(__a) * __c17_mmx_widen_s16(__b);
    __c17_mmx_v2su __even = (__c17_mmx_v2su)__builtin_shufflevector(__p, __p, 0, 2);
    __c17_mmx_v2su __odd = (__c17_mmx_v2su)__builtin_shufflevector(__p, __p, 1, 3);
    return (__m64)(__even + __odd);
}

__C17_INTRIN __m64 _mm_mulhi_pi16(__m64 __a, __m64 __b)
{
    __c17_mmx_v4sw __p = __c17_mmx_widen_s16(__a) * __c17_mmx_widen_s16(__b);
    return (__m64)__builtin_convertvector(__p >> 16, __v4hi);
}

__C17_INTRIN __m64 _mm_mullo_pi16(__m64 __a, __m64 __b)
{
    return (__m64)((__c17_mmx_v4hu)__a * (__c17_mmx_v4hu)__b);
}

/* Shifts. A count wider than the lane clears it, or for an arithmetic right
   shift fills it with the sign. */
__C17_INTRIN __m64 _mm_slli_pi16(__m64 __a, int __n)
{
    if ((unsigned int)__n > 15)
        return (__m64)(__v2si){0, 0};
    return (__m64)((__c17_mmx_v4hu)__a << (unsigned int)__n);
}

__C17_INTRIN __m64 _mm_slli_pi32(__m64 __a, int __n)
{
    if ((unsigned int)__n > 31)
        return (__m64)(__v2si){0, 0};
    return (__m64)((__c17_mmx_v2su)__a << (unsigned int)__n);
}

__C17_INTRIN __m64 _mm_slli_si64(__m64 __a, int __n)
{
    if ((unsigned int)__n > 63)
        return (__m64)(__v2si){0, 0};
    return (__m64)((__c17_mmx_v1du)__a << (unsigned int)__n);
}

__C17_INTRIN __m64 _mm_srli_pi16(__m64 __a, int __n)
{
    if ((unsigned int)__n > 15)
        return (__m64)(__v2si){0, 0};
    return (__m64)((__c17_mmx_v4hu)__a >> (unsigned int)__n);
}

__C17_INTRIN __m64 _mm_srli_pi32(__m64 __a, int __n)
{
    if ((unsigned int)__n > 31)
        return (__m64)(__v2si){0, 0};
    return (__m64)((__c17_mmx_v2su)__a >> (unsigned int)__n);
}

__C17_INTRIN __m64 _mm_srli_si64(__m64 __a, int __n)
{
    if ((unsigned int)__n > 63)
        return (__m64)(__v2si){0, 0};
    return (__m64)((__c17_mmx_v1du)__a >> (unsigned int)__n);
}

__C17_INTRIN __m64 _mm_srai_pi16(__m64 __a, int __n)
{
    if ((unsigned int)__n > 15)
        __n = 15;
    return (__m64)((__v4hi)__a >> __n);
}

__C17_INTRIN __m64 _mm_srai_pi32(__m64 __a, int __n)
{
    if ((unsigned int)__n > 31)
        __n = 31;
    return (__m64)((__v2si)__a >> __n);
}

/* The register-count forms read the whole 64-bit count. */
__C17_INTRIN int __c17_mmx_shift_count(__m64 __c, unsigned int __limit)
{
    unsigned long long __n = ((__c17_mmx_v1du)__c)[0];
    return __n > __limit ? (int)__limit + 1 : (int)__n;
}

__C17_INTRIN __m64 _mm_sll_pi16(__m64 __a, __m64 __c) { return _mm_slli_pi16(__a, __c17_mmx_shift_count(__c, 15)); }
__C17_INTRIN __m64 _mm_sll_pi32(__m64 __a, __m64 __c) { return _mm_slli_pi32(__a, __c17_mmx_shift_count(__c, 31)); }
__C17_INTRIN __m64 _mm_sll_si64(__m64 __a, __m64 __c) { return _mm_slli_si64(__a, __c17_mmx_shift_count(__c, 63)); }
__C17_INTRIN __m64 _mm_srl_pi16(__m64 __a, __m64 __c) { return _mm_srli_pi16(__a, __c17_mmx_shift_count(__c, 15)); }
__C17_INTRIN __m64 _mm_srl_pi32(__m64 __a, __m64 __c) { return _mm_srli_pi32(__a, __c17_mmx_shift_count(__c, 31)); }
__C17_INTRIN __m64 _mm_srl_si64(__m64 __a, __m64 __c) { return _mm_srli_si64(__a, __c17_mmx_shift_count(__c, 63)); }
__C17_INTRIN __m64 _mm_sra_pi16(__m64 __a, __m64 __c) { return _mm_srai_pi16(__a, __c17_mmx_shift_count(__c, 15)); }
__C17_INTRIN __m64 _mm_sra_pi32(__m64 __a, __m64 __c) { return _mm_srai_pi32(__a, __c17_mmx_shift_count(__c, 31)); }

/* Bitwise operations on the whole 64 bits. */
__C17_INTRIN __m64 _mm_and_si64(__m64 __a, __m64 __b) { return __a & __b; }
__C17_INTRIN __m64 _mm_andnot_si64(__m64 __a, __m64 __b) { return ~__a & __b; }
__C17_INTRIN __m64 _mm_or_si64(__m64 __a, __m64 __b) { return __a | __b; }
__C17_INTRIN __m64 _mm_xor_si64(__m64 __a, __m64 __b) { return __a ^ __b; }

/* Comparisons give all-ones lanes for true. */
__C17_INTRIN __m64 _mm_cmpeq_pi8(__m64 __a, __m64 __b) { return (__m64)((__c17_mmx_v8qs)__a == (__c17_mmx_v8qs)__b); }
__C17_INTRIN __m64 _mm_cmpeq_pi16(__m64 __a, __m64 __b) { return (__m64)((__v4hi)__a == (__v4hi)__b); }
__C17_INTRIN __m64 _mm_cmpeq_pi32(__m64 __a, __m64 __b) { return (__m64)((__v2si)__a == (__v2si)__b); }
__C17_INTRIN __m64 _mm_cmpgt_pi8(__m64 __a, __m64 __b) { return (__m64)((__c17_mmx_v8qs)__a > (__c17_mmx_v8qs)__b); }
__C17_INTRIN __m64 _mm_cmpgt_pi16(__m64 __a, __m64 __b) { return (__m64)((__v4hi)__a > (__v4hi)__b); }
__C17_INTRIN __m64 _mm_cmpgt_pi32(__m64 __a, __m64 __b) { return (__m64)((__v2si)__a > (__v2si)__b); }

/* Construction. The _set forms list lanes from the highest down. */
__C17_INTRIN __m64 _mm_setzero_si64(void) { return (__m64)(__v2si){0, 0}; }

__C17_INTRIN __m64 _mm_set_pi32(int __i1, int __i0) { return (__m64)(__v2si){__i0, __i1}; }

__C17_INTRIN __m64 _mm_set_pi16(short __w3, short __w2, short __w1, short __w0)
{
    return (__m64)(__v4hi){__w0, __w1, __w2, __w3};
}

__C17_INTRIN __m64 _mm_set_pi8(char __b7, char __b6, char __b5, char __b4,
                               char __b3, char __b2, char __b1, char __b0)
{
    return (__m64)(__v8qi){__b0, __b1, __b2, __b3, __b4, __b5, __b6, __b7};
}

__C17_INTRIN __m64 _mm_setr_pi32(int __i0, int __i1) { return _mm_set_pi32(__i1, __i0); }

__C17_INTRIN __m64 _mm_setr_pi16(short __w0, short __w1, short __w2, short __w3)
{
    return _mm_set_pi16(__w3, __w2, __w1, __w0);
}

__C17_INTRIN __m64 _mm_setr_pi8(char __b0, char __b1, char __b2, char __b3,
                                char __b4, char __b5, char __b6, char __b7)
{
    return _mm_set_pi8(__b7, __b6, __b5, __b4, __b3, __b2, __b1, __b0);
}

__C17_INTRIN __m64 _mm_set1_pi32(int __i) { return _mm_set_pi32(__i, __i); }
__C17_INTRIN __m64 _mm_set1_pi16(short __w) { return _mm_set_pi16(__w, __w, __w, __w); }
__C17_INTRIN __m64 _mm_set1_pi8(char __b) { return _mm_set_pi8(__b, __b, __b, __b, __b, __b, __b, __b); }

/* The MMX mnemonic spellings. */
__C17_INTRIN __m64 _m_packsswb(__m64 __a, __m64 __b) { return _mm_packs_pi16(__a, __b); }
__C17_INTRIN __m64 _m_packssdw(__m64 __a, __m64 __b) { return _mm_packs_pi32(__a, __b); }
__C17_INTRIN __m64 _m_packuswb(__m64 __a, __m64 __b) { return _mm_packs_pu16(__a, __b); }
__C17_INTRIN __m64 _m_punpckhbw(__m64 __a, __m64 __b) { return _mm_unpackhi_pi8(__a, __b); }
__C17_INTRIN __m64 _m_punpckhwd(__m64 __a, __m64 __b) { return _mm_unpackhi_pi16(__a, __b); }
__C17_INTRIN __m64 _m_punpckhdq(__m64 __a, __m64 __b) { return _mm_unpackhi_pi32(__a, __b); }
__C17_INTRIN __m64 _m_punpcklbw(__m64 __a, __m64 __b) { return _mm_unpacklo_pi8(__a, __b); }
__C17_INTRIN __m64 _m_punpcklwd(__m64 __a, __m64 __b) { return _mm_unpacklo_pi16(__a, __b); }
__C17_INTRIN __m64 _m_punpckldq(__m64 __a, __m64 __b) { return _mm_unpacklo_pi32(__a, __b); }
__C17_INTRIN __m64 _m_paddb(__m64 __a, __m64 __b) { return _mm_add_pi8(__a, __b); }
__C17_INTRIN __m64 _m_paddw(__m64 __a, __m64 __b) { return _mm_add_pi16(__a, __b); }
__C17_INTRIN __m64 _m_paddd(__m64 __a, __m64 __b) { return _mm_add_pi32(__a, __b); }
__C17_INTRIN __m64 _m_paddsb(__m64 __a, __m64 __b) { return _mm_adds_pi8(__a, __b); }
__C17_INTRIN __m64 _m_paddsw(__m64 __a, __m64 __b) { return _mm_adds_pi16(__a, __b); }
__C17_INTRIN __m64 _m_paddusb(__m64 __a, __m64 __b) { return _mm_adds_pu8(__a, __b); }
__C17_INTRIN __m64 _m_paddusw(__m64 __a, __m64 __b) { return _mm_adds_pu16(__a, __b); }
__C17_INTRIN __m64 _m_psubb(__m64 __a, __m64 __b) { return _mm_sub_pi8(__a, __b); }
__C17_INTRIN __m64 _m_psubw(__m64 __a, __m64 __b) { return _mm_sub_pi16(__a, __b); }
__C17_INTRIN __m64 _m_psubd(__m64 __a, __m64 __b) { return _mm_sub_pi32(__a, __b); }
__C17_INTRIN __m64 _m_psubsb(__m64 __a, __m64 __b) { return _mm_subs_pi8(__a, __b); }
__C17_INTRIN __m64 _m_psubsw(__m64 __a, __m64 __b) { return _mm_subs_pi16(__a, __b); }
__C17_INTRIN __m64 _m_psubusb(__m64 __a, __m64 __b) { return _mm_subs_pu8(__a, __b); }
__C17_INTRIN __m64 _m_psubusw(__m64 __a, __m64 __b) { return _mm_subs_pu16(__a, __b); }
__C17_INTRIN __m64 _m_pmaddwd(__m64 __a, __m64 __b) { return _mm_madd_pi16(__a, __b); }
__C17_INTRIN __m64 _m_pmulhw(__m64 __a, __m64 __b) { return _mm_mulhi_pi16(__a, __b); }
__C17_INTRIN __m64 _m_pmullw(__m64 __a, __m64 __b) { return _mm_mullo_pi16(__a, __b); }
__C17_INTRIN __m64 _m_psllw(__m64 __a, __m64 __c) { return _mm_sll_pi16(__a, __c); }
__C17_INTRIN __m64 _m_pslld(__m64 __a, __m64 __c) { return _mm_sll_pi32(__a, __c); }
__C17_INTRIN __m64 _m_psllq(__m64 __a, __m64 __c) { return _mm_sll_si64(__a, __c); }
__C17_INTRIN __m64 _m_psrlw(__m64 __a, __m64 __c) { return _mm_srl_pi16(__a, __c); }
__C17_INTRIN __m64 _m_psrld(__m64 __a, __m64 __c) { return _mm_srl_pi32(__a, __c); }
__C17_INTRIN __m64 _m_psrlq(__m64 __a, __m64 __c) { return _mm_srl_si64(__a, __c); }
__C17_INTRIN __m64 _m_psraw(__m64 __a, __m64 __c) { return _mm_sra_pi16(__a, __c); }
__C17_INTRIN __m64 _m_psrad(__m64 __a, __m64 __c) { return _mm_sra_pi32(__a, __c); }
__C17_INTRIN __m64 _m_psllwi(__m64 __a, int __n) { return _mm_slli_pi16(__a, __n); }
__C17_INTRIN __m64 _m_pslldi(__m64 __a, int __n) { return _mm_slli_pi32(__a, __n); }
__C17_INTRIN __m64 _m_psllqi(__m64 __a, int __n) { return _mm_slli_si64(__a, __n); }
__C17_INTRIN __m64 _m_psrlwi(__m64 __a, int __n) { return _mm_srli_pi16(__a, __n); }
__C17_INTRIN __m64 _m_psrldi(__m64 __a, int __n) { return _mm_srli_pi32(__a, __n); }
__C17_INTRIN __m64 _m_psrlqi(__m64 __a, int __n) { return _mm_srli_si64(__a, __n); }
__C17_INTRIN __m64 _m_psrawi(__m64 __a, int __n) { return _mm_srai_pi16(__a, __n); }
__C17_INTRIN __m64 _m_psradi(__m64 __a, int __n) { return _mm_srai_pi32(__a, __n); }
__C17_INTRIN __m64 _m_pand(__m64 __a, __m64 __b) { return _mm_and_si64(__a, __b); }
__C17_INTRIN __m64 _m_pandn(__m64 __a, __m64 __b) { return _mm_andnot_si64(__a, __b); }
__C17_INTRIN __m64 _m_por(__m64 __a, __m64 __b) { return _mm_or_si64(__a, __b); }
__C17_INTRIN __m64 _m_pxor(__m64 __a, __m64 __b) { return _mm_xor_si64(__a, __b); }
__C17_INTRIN __m64 _m_pcmpeqb(__m64 __a, __m64 __b) { return _mm_cmpeq_pi8(__a, __b); }
__C17_INTRIN __m64 _m_pcmpeqw(__m64 __a, __m64 __b) { return _mm_cmpeq_pi16(__a, __b); }
__C17_INTRIN __m64 _m_pcmpeqd(__m64 __a, __m64 __b) { return _mm_cmpeq_pi32(__a, __b); }
__C17_INTRIN __m64 _m_pcmpgtb(__m64 __a, __m64 __b) { return _mm_cmpgt_pi8(__a, __b); }
__C17_INTRIN __m64 _m_pcmpgtw(__m64 __a, __m64 __b) { return _mm_cmpgt_pi16(__a, __b); }
__C17_INTRIN __m64 _m_pcmpgtd(__m64 __a, __m64 __b) { return _mm_cmpgt_pi32(__a, __b); }

#endif /* _MMINTRIN_H_INCLUDED */
