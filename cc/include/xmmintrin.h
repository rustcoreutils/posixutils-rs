/*
 * c17 builtin xmmintrin.h - SSE intrinsics
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

#ifndef _XMMINTRIN_H_INCLUDED
#define _XMMINTRIN_H_INCLUDED

#include <mmintrin.h>
#include <mm_malloc.h>

typedef float __m128 __attribute__((__vector_size__(16), __may_alias__));
typedef float __m128_u __attribute__((__vector_size__(16), __may_alias__, __aligned__(1)));

/* Internal lane view of an __m128. */
typedef float __v4sf __attribute__((__vector_size__(16)));

#define _MM_SHUFFLE(fp3, fp2, fp1, fp0) \
    (((fp3) << 6) | ((fp2) << 4) | ((fp1) << 2) | (fp0))

/* Integer lane views of an __m128, for masks and bit operations. */
typedef int __c17_sse_v4ss __attribute__((__vector_size__(16)));
typedef unsigned int __c17_sse_v4su __attribute__((__vector_size__(16)));

/* Lane views of an __m64, and the 16-byte views its lanes widen into. */
typedef signed char __c17_sse_v8qs __attribute__((__vector_size__(8)));
typedef unsigned char __c17_sse_v8qu __attribute__((__vector_size__(8)));
typedef unsigned short __c17_sse_v4hu __attribute__((__vector_size__(8)));
typedef short __c17_sse_v8hw __attribute__((__vector_size__(16)));
typedef int __c17_sse_v4sw __attribute__((__vector_size__(16)));

__C17_INTRIN __c17_sse_v8hw __c17_sse_widen_u8(__m64 __a)
{
    return __builtin_convertvector((__c17_sse_v8qu)__a, __c17_sse_v8hw);
}

__C17_INTRIN __c17_sse_v4sw __c17_sse_widen_u16(__m64 __a)
{
    return __builtin_convertvector((__c17_sse_v4hu)__a, __c17_sse_v4sw);
}

/* Prefetch hints: bit 2 asks for a prefetch with intent to write, the low
   two bits give the cache level. */
enum _mm_hint {
    _MM_HINT_IT0 = 19,
    _MM_HINT_IT1 = 18,
    _MM_HINT_ET0 = 7,
    _MM_HINT_ET1 = 6,
    _MM_HINT_T0 = 3,
    _MM_HINT_T1 = 2,
    _MM_HINT_T2 = 1,
    _MM_HINT_NTA = 0
};

/* The hint must be a constant, so this is a macro. */
#define _mm_prefetch(P, I) \
    __builtin_prefetch((const void *)(P), ((I) >> 2) & 1, (I) & 3)

/* MXCSR fields. */
#define _MM_EXCEPT_MASK 0x003f
#define _MM_EXCEPT_INVALID 0x0001
#define _MM_EXCEPT_DENORM 0x0002
#define _MM_EXCEPT_DIV_ZERO 0x0004
#define _MM_EXCEPT_OVERFLOW 0x0008
#define _MM_EXCEPT_UNDERFLOW 0x0010
#define _MM_EXCEPT_INEXACT 0x0020

#define _MM_MASK_MASK 0x1f80
#define _MM_MASK_INVALID 0x0080
#define _MM_MASK_DENORM 0x0100
#define _MM_MASK_DIV_ZERO 0x0200
#define _MM_MASK_OVERFLOW 0x0400
#define _MM_MASK_UNDERFLOW 0x0800
#define _MM_MASK_INEXACT 0x1000

#define _MM_ROUND_MASK 0x6000
#define _MM_ROUND_NEAREST 0x0000
#define _MM_ROUND_DOWN 0x2000
#define _MM_ROUND_UP 0x4000
#define _MM_ROUND_TOWARD_ZERO 0x6000

#define _MM_FLUSH_ZERO_MASK 0x8000
#define _MM_FLUSH_ZERO_ON 0x8000
#define _MM_FLUSH_ZERO_OFF 0x0000

__C17_INTRIN unsigned int _mm_getcsr(void)
{
    unsigned int __csr;
    __asm__ __volatile__("stmxcsr %0" : "=m"(__csr) : : "memory");
    return __csr;
}

__C17_INTRIN void _mm_setcsr(unsigned int __csr)
{
    __asm__ __volatile__("ldmxcsr %0" : : "m"(__csr) : "memory");
}

#define _MM_GET_EXCEPTION_STATE() (_mm_getcsr() & _MM_EXCEPT_MASK)
#define _MM_GET_EXCEPTION_MASK() (_mm_getcsr() & _MM_MASK_MASK)
#define _MM_GET_ROUNDING_MODE() (_mm_getcsr() & _MM_ROUND_MASK)
#define _MM_GET_FLUSH_ZERO_MODE() (_mm_getcsr() & _MM_FLUSH_ZERO_MASK)
#define _MM_SET_EXCEPTION_STATE(mask) \
    _mm_setcsr((_mm_getcsr() & ~_MM_EXCEPT_MASK) | (mask))
#define _MM_SET_EXCEPTION_MASK(mask) \
    _mm_setcsr((_mm_getcsr() & ~_MM_MASK_MASK) | (mask))
#define _MM_SET_ROUNDING_MODE(mode) \
    _mm_setcsr((_mm_getcsr() & ~_MM_ROUND_MASK) | (mode))
#define _MM_SET_FLUSH_ZERO_MODE(mode) \
    _mm_setcsr((_mm_getcsr() & ~_MM_FLUSH_ZERO_MASK) | (mode))

/* Construction. */
__C17_INTRIN __m128 _mm_setzero_ps(void) { return (__m128){0.0f, 0.0f, 0.0f, 0.0f}; }

/* The contents are unspecified; zero is as good as any. */
__C17_INTRIN __m128 _mm_undefined_ps(void) { return _mm_setzero_ps(); }

__C17_INTRIN __m128 _mm_set_ss(float __w) { return (__m128){__w, 0.0f, 0.0f, 0.0f}; }
__C17_INTRIN __m128 _mm_set1_ps(float __w) { return (__m128){__w, __w, __w, __w}; }
__C17_INTRIN __m128 _mm_set_ps1(float __w) { return _mm_set1_ps(__w); }

__C17_INTRIN __m128 _mm_set_ps(float __z, float __y, float __x, float __w)
{
    return (__m128){__w, __x, __y, __z};
}

__C17_INTRIN __m128 _mm_setr_ps(float __w, float __x, float __y, float __z)
{
    return (__m128){__w, __x, __y, __z};
}

__C17_INTRIN float _mm_cvtss_f32(__m128 __a) { return ((__v4sf)__a)[0]; }

/* Replace lane 0 of __a. */
__C17_INTRIN __m128 __c17_sse_with_lane0(__m128 __a, float __f)
{
    __v4sf __r = (__v4sf)__a;
    __r[0] = __f;
    return (__m128)__r;
}

/* Arithmetic. The _ss forms operate on lane 0 and keep __a's upper lanes. */
__C17_INTRIN __m128 _mm_add_ps(__m128 __a, __m128 __b) { return __a + __b; }
__C17_INTRIN __m128 _mm_sub_ps(__m128 __a, __m128 __b) { return __a - __b; }
__C17_INTRIN __m128 _mm_mul_ps(__m128 __a, __m128 __b) { return __a * __b; }
__C17_INTRIN __m128 _mm_div_ps(__m128 __a, __m128 __b) { return __a / __b; }
__C17_INTRIN __m128 _mm_add_ss(__m128 __a, __m128 __b) { return __c17_sse_with_lane0(__a, __a[0] + __b[0]); }
__C17_INTRIN __m128 _mm_sub_ss(__m128 __a, __m128 __b) { return __c17_sse_with_lane0(__a, __a[0] - __b[0]); }
__C17_INTRIN __m128 _mm_mul_ss(__m128 __a, __m128 __b) { return __c17_sse_with_lane0(__a, __a[0] * __b[0]); }
__C17_INTRIN __m128 _mm_div_ss(__m128 __a, __m128 __b) { return __c17_sse_with_lane0(__a, __a[0] / __b[0]); }

/* SQRTSS on one lane. c17 lowers __builtin_sqrtf to a call to libm's
   sqrtf, which would make every user of these link with -lm; the
   instruction is correctly rounded, exactly as sqrtf is. (It is one lane
   at a time because c17 cannot yet bind a 16-byte vector to an "x"
   operand.) */
__C17_INTRIN float __c17_sse_sqrtf(float __f)
{
    float __r;
    __asm__("sqrtss %1, %0" : "=x"(__r) : "x"(__f));
    return __r;
}

__C17_INTRIN __m128 _mm_sqrt_ps(__m128 __a)
{
    return (__m128){__c17_sse_sqrtf(__a[0]), __c17_sse_sqrtf(__a[1]),
                    __c17_sse_sqrtf(__a[2]), __c17_sse_sqrtf(__a[3])};
}

__C17_INTRIN __m128 _mm_sqrt_ss(__m128 __a) { return __c17_sse_with_lane0(__a, __c17_sse_sqrtf(__a[0])); }

/* RCPPS and RSQRTPS are approximations with a relative error of at most
   1.5 * 2^-12, and their exact bits are model-specific. These compute the
   exact quotient instead, so results can differ from the hardware's in the
   low bits; they agree on zeros, infinities, NaNs and negative operands. */
__C17_INTRIN __m128 _mm_rcp_ps(__m128 __a) { return 1.0f / __a; }
__C17_INTRIN __m128 _mm_rcp_ss(__m128 __a) { return __c17_sse_with_lane0(__a, 1.0f / __a[0]); }
__C17_INTRIN __m128 _mm_rsqrt_ps(__m128 __a) { return 1.0f / _mm_sqrt_ps(__a); }
__C17_INTRIN __m128 _mm_rsqrt_ss(__m128 __a) { return __c17_sse_with_lane0(__a, 1.0f / __c17_sse_sqrtf(__a[0])); }

/* MINPS/MAXPS answer the second operand unless the comparison holds, so a
   NaN in either operand, or two zeros of any sign, give __b. */
__C17_INTRIN __m128 __c17_sse_select_ps(__c17_sse_v4ss __m, __m128 __a, __m128 __b)
{
    return (__m128)(((__c17_sse_v4ss)__a & __m) | ((__c17_sse_v4ss)__b & ~__m));
}

__C17_INTRIN __m128 _mm_min_ps(__m128 __a, __m128 __b) { return __c17_sse_select_ps(__a < __b, __a, __b); }
__C17_INTRIN __m128 _mm_max_ps(__m128 __a, __m128 __b) { return __c17_sse_select_ps(__a > __b, __a, __b); }
__C17_INTRIN __m128 _mm_min_ss(__m128 __a, __m128 __b) { return __c17_sse_with_lane0(__a, __a[0] < __b[0] ? __a[0] : __b[0]); }
__C17_INTRIN __m128 _mm_max_ss(__m128 __a, __m128 __b) { return __c17_sse_with_lane0(__a, __a[0] > __b[0] ? __a[0] : __b[0]); }

/* Bitwise operations. */
__C17_INTRIN __m128 _mm_and_ps(__m128 __a, __m128 __b) { return (__m128)((__c17_sse_v4su)__a & (__c17_sse_v4su)__b); }
__C17_INTRIN __m128 _mm_andnot_ps(__m128 __a, __m128 __b) { return (__m128)(~(__c17_sse_v4su)__a & (__c17_sse_v4su)__b); }
__C17_INTRIN __m128 _mm_or_ps(__m128 __a, __m128 __b) { return (__m128)((__c17_sse_v4su)__a | (__c17_sse_v4su)__b); }
__C17_INTRIN __m128 _mm_xor_ps(__m128 __a, __m128 __b) { return (__m128)((__c17_sse_v4su)__a ^ (__c17_sse_v4su)__b); }

/* Packed comparisons give all-ones lanes for true. A NaN makes every
   predicate false except the negated ones (NEQ, NLT, NLE, NGT, NGE) and
   UNORD. */
__C17_INTRIN __m128 _mm_cmpeq_ps(__m128 __a, __m128 __b) { return (__m128)(__a == __b); }
__C17_INTRIN __m128 _mm_cmplt_ps(__m128 __a, __m128 __b) { return (__m128)(__a < __b); }
__C17_INTRIN __m128 _mm_cmple_ps(__m128 __a, __m128 __b) { return (__m128)(__a <= __b); }
__C17_INTRIN __m128 _mm_cmpgt_ps(__m128 __a, __m128 __b) { return (__m128)(__a > __b); }
__C17_INTRIN __m128 _mm_cmpge_ps(__m128 __a, __m128 __b) { return (__m128)(__a >= __b); }
__C17_INTRIN __m128 _mm_cmpneq_ps(__m128 __a, __m128 __b) { return (__m128)~(__a == __b); }
__C17_INTRIN __m128 _mm_cmpnlt_ps(__m128 __a, __m128 __b) { return (__m128)~(__a < __b); }
__C17_INTRIN __m128 _mm_cmpnle_ps(__m128 __a, __m128 __b) { return (__m128)~(__a <= __b); }
__C17_INTRIN __m128 _mm_cmpngt_ps(__m128 __a, __m128 __b) { return (__m128)~(__a > __b); }
__C17_INTRIN __m128 _mm_cmpnge_ps(__m128 __a, __m128 __b) { return (__m128)~(__a >= __b); }
__C17_INTRIN __m128 _mm_cmpord_ps(__m128 __a, __m128 __b) { return (__m128)((__a == __a) & (__b == __b)); }
__C17_INTRIN __m128 _mm_cmpunord_ps(__m128 __a, __m128 __b) { return (__m128)~((__a == __a) & (__b == __b)); }

/* The scalar comparisons put the lane-0 mask under __a's upper lanes. */
__C17_INTRIN __m128 __c17_sse_mask_lane0(__m128 __a, __m128 __m)
{
    __c17_sse_v4ss __r = (__c17_sse_v4ss)__a;
    __r[0] = ((__c17_sse_v4ss)__m)[0];
    return (__m128)__r;
}

__C17_INTRIN __m128 _mm_cmpeq_ss(__m128 __a, __m128 __b) { return __c17_sse_mask_lane0(__a, _mm_cmpeq_ps(__a, __b)); }
__C17_INTRIN __m128 _mm_cmplt_ss(__m128 __a, __m128 __b) { return __c17_sse_mask_lane0(__a, _mm_cmplt_ps(__a, __b)); }
__C17_INTRIN __m128 _mm_cmple_ss(__m128 __a, __m128 __b) { return __c17_sse_mask_lane0(__a, _mm_cmple_ps(__a, __b)); }
__C17_INTRIN __m128 _mm_cmpgt_ss(__m128 __a, __m128 __b) { return __c17_sse_mask_lane0(__a, _mm_cmpgt_ps(__a, __b)); }
__C17_INTRIN __m128 _mm_cmpge_ss(__m128 __a, __m128 __b) { return __c17_sse_mask_lane0(__a, _mm_cmpge_ps(__a, __b)); }
__C17_INTRIN __m128 _mm_cmpneq_ss(__m128 __a, __m128 __b) { return __c17_sse_mask_lane0(__a, _mm_cmpneq_ps(__a, __b)); }
__C17_INTRIN __m128 _mm_cmpnlt_ss(__m128 __a, __m128 __b) { return __c17_sse_mask_lane0(__a, _mm_cmpnlt_ps(__a, __b)); }
__C17_INTRIN __m128 _mm_cmpnle_ss(__m128 __a, __m128 __b) { return __c17_sse_mask_lane0(__a, _mm_cmpnle_ps(__a, __b)); }
__C17_INTRIN __m128 _mm_cmpngt_ss(__m128 __a, __m128 __b) { return __c17_sse_mask_lane0(__a, _mm_cmpngt_ps(__a, __b)); }
__C17_INTRIN __m128 _mm_cmpnge_ss(__m128 __a, __m128 __b) { return __c17_sse_mask_lane0(__a, _mm_cmpnge_ps(__a, __b)); }
__C17_INTRIN __m128 _mm_cmpord_ss(__m128 __a, __m128 __b) { return __c17_sse_mask_lane0(__a, _mm_cmpord_ps(__a, __b)); }
__C17_INTRIN __m128 _mm_cmpunord_ss(__m128 __a, __m128 __b) { return __c17_sse_mask_lane0(__a, _mm_cmpunord_ps(__a, __b)); }

/* Lane-0 comparisons returning 0 or 1. Only != holds for a NaN. COMISS
   and UCOMISS differ only in which NaNs raise the invalid exception. */
__C17_INTRIN int _mm_comieq_ss(__m128 __a, __m128 __b) { return __a[0] == __b[0]; }
__C17_INTRIN int _mm_comilt_ss(__m128 __a, __m128 __b) { return __a[0] < __b[0]; }
__C17_INTRIN int _mm_comile_ss(__m128 __a, __m128 __b) { return __a[0] <= __b[0]; }
__C17_INTRIN int _mm_comigt_ss(__m128 __a, __m128 __b) { return __a[0] > __b[0]; }
__C17_INTRIN int _mm_comige_ss(__m128 __a, __m128 __b) { return __a[0] >= __b[0]; }
__C17_INTRIN int _mm_comineq_ss(__m128 __a, __m128 __b) { return __a[0] != __b[0]; }
__C17_INTRIN int _mm_ucomieq_ss(__m128 __a, __m128 __b) { return __a[0] == __b[0]; }
__C17_INTRIN int _mm_ucomilt_ss(__m128 __a, __m128 __b) { return __a[0] < __b[0]; }
__C17_INTRIN int _mm_ucomile_ss(__m128 __a, __m128 __b) { return __a[0] <= __b[0]; }
__C17_INTRIN int _mm_ucomigt_ss(__m128 __a, __m128 __b) { return __a[0] > __b[0]; }
__C17_INTRIN int _mm_ucomige_ss(__m128 __a, __m128 __b) { return __a[0] >= __b[0]; }
__C17_INTRIN int _mm_ucomineq_ss(__m128 __a, __m128 __b) { return __a[0] != __b[0]; }

/* Float to integer. CVT rounds in the MXCSR rounding mode (rintf follows
   it), CVTT truncates; a NaN or an out-of-range value gives the "integer
   indefinite" value, the most negative integer. */
__C17_INTRIN int __c17_sse_f32_to_i32(float __f)
{
    if (__f >= -2147483648.0f && __f < 2147483648.0f)
        return (int)__f;
    return (int)0x80000000u;
}

__C17_INTRIN long long __c17_sse_f32_to_i64(float __f)
{
    if (__f >= -9223372036854775808.0f && __f < 9223372036854775808.0f)
        return (long long)__f;
    return (long long)0x8000000000000000ull;
}

__C17_INTRIN int _mm_cvtss_si32(__m128 __a) { return __c17_sse_f32_to_i32(__builtin_rintf(__a[0])); }
__C17_INTRIN int _mm_cvt_ss2si(__m128 __a) { return _mm_cvtss_si32(__a); }
__C17_INTRIN int _mm_cvttss_si32(__m128 __a) { return __c17_sse_f32_to_i32(__a[0]); }
__C17_INTRIN int _mm_cvtt_ss2si(__m128 __a) { return _mm_cvttss_si32(__a); }
__C17_INTRIN long long _mm_cvtss_si64(__m128 __a) { return __c17_sse_f32_to_i64(__builtin_rintf(__a[0])); }
__C17_INTRIN long long _mm_cvtss_si64x(__m128 __a) { return _mm_cvtss_si64(__a); }
__C17_INTRIN long long _mm_cvttss_si64(__m128 __a) { return __c17_sse_f32_to_i64(__a[0]); }
__C17_INTRIN long long _mm_cvttss_si64x(__m128 __a) { return _mm_cvttss_si64(__a); }

__C17_INTRIN __m64 _mm_cvtps_pi32(__m128 __a)
{
    return (__m64)(__v2si){__c17_sse_f32_to_i32(__builtin_rintf(__a[0])),
                           __c17_sse_f32_to_i32(__builtin_rintf(__a[1]))};
}

__C17_INTRIN __m64 _mm_cvt_ps2pi(__m128 __a) { return _mm_cvtps_pi32(__a); }

__C17_INTRIN __m64 _mm_cvttps_pi32(__m128 __a)
{
    return (__m64)(__v2si){__c17_sse_f32_to_i32(__a[0]), __c17_sse_f32_to_i32(__a[1])};
}

__C17_INTRIN __m64 _mm_cvtt_ps2pi(__m128 __a) { return _mm_cvttps_pi32(__a); }

/* Four lanes to 16-bit integers: each converts to 32 bits as CVTPS2PI
   does (so out of range becomes 0x80000000), then saturates. */
__C17_INTRIN __m64 _mm_cvtps_pi16(__m128 __a)
{
    __m64 __lo = _mm_cvtps_pi32(__a);
    __m64 __hi = _mm_cvtps_pi32((__m128)__builtin_shufflevector((__v4sf)__a, (__v4sf)__a, 2, 3, 2, 3));
    return _mm_packs_pi32(__lo, __hi);
}

/* The same into the low four bytes, signed-saturated; the high four are
   zero. */
__C17_INTRIN __m64 _mm_cvtps_pi8(__m128 __a)
{
    return _mm_packs_pi16(_mm_cvtps_pi16(__a), _mm_setzero_si64());
}

/* Integer to float, rounding in the current mode. */
__C17_INTRIN __m128 _mm_cvtsi32_ss(__m128 __a, int __i) { return __c17_sse_with_lane0(__a, (float)__i); }
__C17_INTRIN __m128 _mm_cvt_si2ss(__m128 __a, int __i) { return _mm_cvtsi32_ss(__a, __i); }
__C17_INTRIN __m128 _mm_cvtsi64_ss(__m128 __a, long long __i) { return __c17_sse_with_lane0(__a, (float)__i); }
__C17_INTRIN __m128 _mm_cvtsi64x_ss(__m128 __a, long long __i) { return _mm_cvtsi64_ss(__a, __i); }

/* Two 32-bit integers into lanes 0 and 1; lanes 2 and 3 come from __a. */
__C17_INTRIN __m128 _mm_cvtpi32_ps(__m128 __a, __m64 __b)
{
    __v4sf __r = (__v4sf)__a;
    __r[0] = (float)((__v2si)__b)[0];
    __r[1] = (float)((__v2si)__b)[1];
    return (__m128)__r;
}

__C17_INTRIN __m128 _mm_cvt_pi2ps(__m128 __a, __m64 __b) { return _mm_cvtpi32_ps(__a, __b); }

__C17_INTRIN __m128 _mm_cvtpi16_ps(__m64 __a) { return (__m128)__builtin_convertvector((__v4hi)__a, __v4sf); }
__C17_INTRIN __m128 _mm_cvtpu16_ps(__m64 __a) { return (__m128)__builtin_convertvector((__c17_sse_v4hu)__a, __v4sf); }

__C17_INTRIN __m128 _mm_cvtpi8_ps(__m64 __a)
{
    __c17_sse_v8qs __b = (__c17_sse_v8qs)__a;
    return (__m128){(float)__b[0], (float)__b[1], (float)__b[2], (float)__b[3]};
}

__C17_INTRIN __m128 _mm_cvtpu8_ps(__m64 __a)
{
    __c17_sse_v8qu __b = (__c17_sse_v8qu)__a;
    return (__m128){(float)__b[0], (float)__b[1], (float)__b[2], (float)__b[3]};
}

__C17_INTRIN __m128 _mm_cvtpi32x2_ps(__m64 __a, __m64 __b)
{
    __c17_sse_v4ss __i = __builtin_shufflevector((__v2si)__a, (__v2si)__b, 0, 1, 2, 3);
    return (__m128)__builtin_convertvector(__i, __v4sf);
}

/* Shuffles. The selector of _mm_shuffle_ps must be a constant. */
#define _mm_shuffle_ps(A, B, MASK)                                          \
    ((__m128)__builtin_shufflevector((__v4sf)(__m128)(A), (__v4sf)(__m128)(B), \
                                     (MASK) & 3, ((MASK) >> 2) & 3,         \
                                     4 + (((MASK) >> 4) & 3),               \
                                     4 + (((MASK) >> 6) & 3)))

__C17_INTRIN __m128 _mm_unpackhi_ps(__m128 __a, __m128 __b)
{
    return (__m128)__builtin_shufflevector((__v4sf)__a, (__v4sf)__b, 2, 6, 3, 7);
}

__C17_INTRIN __m128 _mm_unpacklo_ps(__m128 __a, __m128 __b)
{
    return (__m128)__builtin_shufflevector((__v4sf)__a, (__v4sf)__b, 0, 4, 1, 5);
}

__C17_INTRIN __m128 _mm_move_ss(__m128 __a, __m128 __b)
{
    return (__m128)__builtin_shufflevector((__v4sf)__a, (__v4sf)__b, 4, 1, 2, 3);
}

__C17_INTRIN __m128 _mm_movehl_ps(__m128 __a, __m128 __b)
{
    return (__m128)__builtin_shufflevector((__v4sf)__a, (__v4sf)__b, 6, 7, 2, 3);
}

__C17_INTRIN __m128 _mm_movelh_ps(__m128 __a, __m128 __b)
{
    return (__m128)__builtin_shufflevector((__v4sf)__a, (__v4sf)__b, 0, 1, 4, 5);
}

/* The sign bit of each lane, lane 0 in bit 0. */
__C17_INTRIN int _mm_movemask_ps(__m128 __a)
{
    __c17_sse_v4su __s = (__c17_sse_v4su)__a >> 31;
    return (int)(__s[0] | __s[1] << 1 | __s[2] << 2 | __s[3] << 3);
}

/* Loads. The _ps forms without a u need 16-byte alignment. */
__C17_INTRIN __m128 _mm_load_ss(float const *__p) { return _mm_set_ss(*__p); }
__C17_INTRIN __m128 _mm_load1_ps(float const *__p) { return _mm_set1_ps(*__p); }
__C17_INTRIN __m128 _mm_load_ps1(float const *__p) { return _mm_load1_ps(__p); }
__C17_INTRIN __m128 _mm_load_ps(float const *__p) { return *(__m128 const *)__p; }
__C17_INTRIN __m128 _mm_loadu_ps(float const *__p) { return *(__m128_u const *)__p; }

__C17_INTRIN __m128 _mm_loadr_ps(float const *__p)
{
    __m128 __v = _mm_load_ps(__p);
    return (__m128)__builtin_shufflevector((__v4sf)__v, (__v4sf)__v, 3, 2, 1, 0);
}

/* Two floats from memory into the high (h) or low (l) half. */
__C17_INTRIN __m128 _mm_loadh_pi(__m128 __a, __m64 const *__p)
{
    __v2sf __t;
    __builtin_memcpy(&__t, __p, sizeof(__t));
    __v4sf __r = (__v4sf)__a;
    __r[2] = __t[0];
    __r[3] = __t[1];
    return (__m128)__r;
}

__C17_INTRIN __m128 _mm_loadl_pi(__m128 __a, __m64 const *__p)
{
    __v2sf __t;
    __builtin_memcpy(&__t, __p, sizeof(__t));
    __v4sf __r = (__v4sf)__a;
    __r[0] = __t[0];
    __r[1] = __t[1];
    return (__m128)__r;
}

/* Stores. */
__C17_INTRIN void _mm_store_ss(float *__p, __m128 __a) { *__p = __a[0]; }
__C17_INTRIN void _mm_store_ps(float *__p, __m128 __a) { *(__m128 *)__p = __a; }
__C17_INTRIN void _mm_storeu_ps(float *__p, __m128 __a) { *(__m128_u *)__p = __a; }
__C17_INTRIN void _mm_store1_ps(float *__p, __m128 __a) { _mm_store_ps(__p, _mm_set1_ps(__a[0])); }
__C17_INTRIN void _mm_store_ps1(float *__p, __m128 __a) { _mm_store1_ps(__p, __a); }

__C17_INTRIN void _mm_storer_ps(float *__p, __m128 __a)
{
    _mm_store_ps(__p, (__m128)__builtin_shufflevector((__v4sf)__a, (__v4sf)__a, 3, 2, 1, 0));
}

__C17_INTRIN void _mm_storeh_pi(__m64 *__p, __m128 __a)
{
    __v2sf __t = __builtin_shufflevector((__v4sf)__a, (__v4sf)__a, 2, 3);
    __builtin_memcpy(__p, &__t, sizeof(__t));
}

__C17_INTRIN void _mm_storel_pi(__m64 *__p, __m128 __a)
{
    __v2sf __t = __builtin_shufflevector((__v4sf)__a, (__v4sf)__a, 0, 1);
    __builtin_memcpy(__p, &__t, sizeof(__t));
}

/* Non-temporal stores; the cache hint has no C spelling and is dropped. */
__C17_INTRIN void _mm_stream_ps(float *__p, __m128 __a) { _mm_store_ps(__p, __a); }
__C17_INTRIN void _mm_stream_pi(__m64 *__p, __m64 __a) { *__p = __a; }

__C17_INTRIN void _mm_sfence(void) { __asm__ __volatile__("sfence" : : : "memory"); }
__C17_INTRIN void _mm_pause(void) { __asm__ __volatile__("pause" : : : "memory"); }

/* The integer operations SSE added to MMX. */
__C17_INTRIN __m64 _mm_max_pi16(__m64 __a, __m64 __b)
{
    __v4hi __x = (__v4hi)__a, __y = (__v4hi)__b, __m = __x > __y;
    return (__m64)((__x & __m) | (__y & ~__m));
}

__C17_INTRIN __m64 _mm_min_pi16(__m64 __a, __m64 __b)
{
    __v4hi __x = (__v4hi)__a, __y = (__v4hi)__b, __m = __x < __y;
    return (__m64)((__x & __m) | (__y & ~__m));
}

__C17_INTRIN __m64 _mm_max_pu8(__m64 __a, __m64 __b)
{
    __c17_sse_v8qu __x = (__c17_sse_v8qu)__a, __y = (__c17_sse_v8qu)__b;
    __c17_sse_v8qu __m = (__c17_sse_v8qu)(__x > __y);
    return (__m64)((__x & __m) | (__y & ~__m));
}

__C17_INTRIN __m64 _mm_min_pu8(__m64 __a, __m64 __b)
{
    __c17_sse_v8qu __x = (__c17_sse_v8qu)__a, __y = (__c17_sse_v8qu)__b;
    __c17_sse_v8qu __m = (__c17_sse_v8qu)(__x < __y);
    return (__m64)((__x & __m) | (__y & ~__m));
}

__C17_INTRIN __m64 _mm_mulhi_pu16(__m64 __a, __m64 __b)
{
    __c17_sse_v4su __p = (__c17_sse_v4su)__c17_sse_widen_u16(__a) * (__c17_sse_v4su)__c17_sse_widen_u16(__b);
    return (__m64)__builtin_convertvector(__p >> 16, __c17_sse_v4hu);
}

/* Rounded average: (a + b + 1) >> 1 without overflow. */
__C17_INTRIN __m64 _mm_avg_pu8(__m64 __a, __m64 __b)
{
    __c17_sse_v8hw __s = (__c17_sse_widen_u8(__a) + __c17_sse_widen_u8(__b) + 1) >> 1;
    return (__m64)__builtin_convertvector(__s, __c17_sse_v8qu);
}

__C17_INTRIN __m64 _mm_avg_pu16(__m64 __a, __m64 __b)
{
    __c17_sse_v4sw __s = (__c17_sse_widen_u16(__a) + __c17_sse_widen_u16(__b) + 1) >> 1;
    return (__m64)__builtin_convertvector(__s, __c17_sse_v4hu);
}

/* Sum of absolute byte differences, in the low 16 bits. */
__C17_INTRIN __m64 _mm_sad_pu8(__m64 __a, __m64 __b)
{
    __c17_sse_v8hw __d = __c17_sse_widen_u8(__a) - __c17_sse_widen_u8(__b);
    __c17_sse_v8hw __neg = __d < 0;
    __d = (__d & ~__neg) | (-__d & __neg);
    unsigned int __sum = 0;
    for (int __i = 0; __i < 8; __i++)
        __sum += (unsigned int)__d[__i];
    return (__m64)(__v1di){(long long)__sum};
}

/* The sign bit of each byte, byte 0 in bit 0. */
__C17_INTRIN int _mm_movemask_pi8(__m64 __a)
{
    __c17_sse_v8qu __s = (__c17_sse_v8qu)__a >> 7;
    int __r = 0;
    for (int __i = 0; __i < 8; __i++)
        __r |= __s[__i] << __i;
    return __r;
}

/* Store the bytes of __d whose selector byte in __n has its top bit set. */
__C17_INTRIN void _mm_maskmove_si64(__m64 __d, __m64 __n, char *__p)
{
    __c17_sse_v8qu __v = (__c17_sse_v8qu)__d, __s = (__c17_sse_v8qu)__n;
    for (int __i = 0; __i < 8; __i++)
        if (__s[__i] & 0x80)
            __p[__i] = (char)__v[__i];
}

/* Lane selectors must be constants, so these are macros. */
#define _mm_extract_pi16(A, N) \
    ((int)(unsigned short)((__v4hi)(__m64)(A))[(N) & 3])

#define _mm_insert_pi16(A, D, N) (__extension__({  \
    __v4hi __c17_sse_ins = (__v4hi)(__m64)(A);         \
    __c17_sse_ins[(N) & 3] = (short)(D);               \
    (__m64)__c17_sse_ins;                              \
}))

#define _mm_shuffle_pi16(A, N)                                     \
    ((__m64)__builtin_shufflevector((__v4hi)(__m64)(A), (__v4hi)(__m64)(A), \
                                    (N) & 3, ((N) >> 2) & 3,       \
                                    ((N) >> 4) & 3, ((N) >> 6) & 3))

/* The mnemonic spellings. */
#define _m_pextrw(A, N) _mm_extract_pi16(A, N)
#define _m_pinsrw(A, D, N) _mm_insert_pi16(A, D, N)
#define _m_pshufw(A, N) _mm_shuffle_pi16(A, N)
__C17_INTRIN __m64 _m_pmaxsw(__m64 __a, __m64 __b) { return _mm_max_pi16(__a, __b); }
__C17_INTRIN __m64 _m_pmaxub(__m64 __a, __m64 __b) { return _mm_max_pu8(__a, __b); }
__C17_INTRIN __m64 _m_pminsw(__m64 __a, __m64 __b) { return _mm_min_pi16(__a, __b); }
__C17_INTRIN __m64 _m_pminub(__m64 __a, __m64 __b) { return _mm_min_pu8(__a, __b); }
__C17_INTRIN int _m_pmovmskb(__m64 __a) { return _mm_movemask_pi8(__a); }
__C17_INTRIN __m64 _m_pmulhuw(__m64 __a, __m64 __b) { return _mm_mulhi_pu16(__a, __b); }
__C17_INTRIN __m64 _m_pavgb(__m64 __a, __m64 __b) { return _mm_avg_pu8(__a, __b); }
__C17_INTRIN __m64 _m_pavgw(__m64 __a, __m64 __b) { return _mm_avg_pu16(__a, __b); }
__C17_INTRIN __m64 _m_psadbw(__m64 __a, __m64 __b) { return _mm_sad_pu8(__a, __b); }
__C17_INTRIN void _m_maskmovq(__m64 __d, __m64 __n, char *__p) { _mm_maskmove_si64(__d, __n, __p); }

/* Transpose the 4x4 matrix whose rows are the four arguments, in place. */
#define _MM_TRANSPOSE4_PS(row0, row1, row2, row3)          \
    do {                                                   \
        __m128 __c17_sse_t0 = _mm_unpacklo_ps((row0), (row1)); \
        __m128 __c17_sse_t1 = _mm_unpacklo_ps((row2), (row3)); \
        __m128 __c17_sse_t2 = _mm_unpackhi_ps((row0), (row1)); \
        __m128 __c17_sse_t3 = _mm_unpackhi_ps((row2), (row3)); \
        (row0) = _mm_movelh_ps(__c17_sse_t0, __c17_sse_t1);        \
        (row1) = _mm_movehl_ps(__c17_sse_t1, __c17_sse_t0);        \
        (row2) = _mm_movelh_ps(__c17_sse_t2, __c17_sse_t3);        \
        (row3) = _mm_movehl_ps(__c17_sse_t3, __c17_sse_t2);        \
    } while (0)

/* The SSE2 types and functions come with this header too, as they do with
   gcc's: code that includes <xmmintrin.h> and uses __m128i compiles. */
#include <emmintrin.h>

#endif /* _XMMINTRIN_H_INCLUDED */
