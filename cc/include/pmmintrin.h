/*
 * c17 builtin pmmintrin.h - SSE3 intrinsics
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

#ifndef _PMMINTRIN_H_INCLUDED
#define _PMMINTRIN_H_INCLUDED

#include <emmintrin.h>

/* SSE3 functions follow. */

/* MXCSR.DAZ: denormal inputs are read as zero. */
#define _MM_DENORMALS_ZERO_MASK 0x0040
#define _MM_DENORMALS_ZERO_ON 0x0040
#define _MM_DENORMALS_ZERO_OFF 0x0000

#if defined(__x86_64__) || defined(__i386__)
__C17_INTRIN unsigned int __c17_daz_getcsr(void)
{
    unsigned int __r;
    __asm__ __volatile__("stmxcsr %0" : "=m"(__r));
    return __r;
}

__C17_INTRIN void __c17_daz_setcsr(unsigned int __v)
{
    __asm__ __volatile__("ldmxcsr %0" : : "m"(__v));
}

#define _MM_SET_DENORMALS_ZERO_MODE(mode) \
    __c17_daz_setcsr((__c17_daz_getcsr() & ~_MM_DENORMALS_ZERO_MASK) | (mode))
#define _MM_GET_DENORMALS_ZERO_MODE() \
    (__c17_daz_getcsr() & _MM_DENORMALS_ZERO_MASK)

/* MONITOR: arm address monitoring on __p (RAX/EAX), extensions in ECX,
   hints in EDX. */
__C17_INTRIN void _mm_monitor(void const *__p, unsigned int __e, unsigned int __h)
{
    __asm__ __volatile__("monitor" : : "a"(__p), "c"(__e), "d"(__h) : "memory");
}

/* MWAIT: hints in EAX, extensions in ECX. */
__C17_INTRIN void _mm_mwait(unsigned int __e, unsigned int __h)
{
    __asm__ __volatile__("mwait" : : "a"(__h), "c"(__e) : "memory");
}
#endif

/* ADDSUBPS: subtract in the even lanes, add in the odd lanes. */
__C17_INTRIN __m128 _mm_addsub_ps(__m128 __a, __m128 __b)
{
    __v4sf __d = (__v4sf)__a - (__v4sf)__b;
    __v4sf __s = (__v4sf)__a + (__v4sf)__b;
    return (__m128)__builtin_shufflevector(__d, __s, 0, 5, 2, 7);
}

/* ADDSUBPD */
__C17_INTRIN __m128d _mm_addsub_pd(__m128d __a, __m128d __b)
{
    __v2df __d = (__v2df)__a - (__v2df)__b;
    __v2df __s = (__v2df)__a + (__v2df)__b;
    return (__m128d)__builtin_shufflevector(__d, __s, 0, 3);
}

/* HADDPS: {a0+a1, a2+a3, b0+b1, b2+b3}. */
__C17_INTRIN __m128 _mm_hadd_ps(__m128 __a, __m128 __b)
{
    __v4sf __e = __builtin_shufflevector((__v4sf)__a, (__v4sf)__b, 0, 2, 4, 6);
    __v4sf __o = __builtin_shufflevector((__v4sf)__a, (__v4sf)__b, 1, 3, 5, 7);
    return (__m128)(__e + __o);
}

/* HSUBPS: {a0-a1, a2-a3, b0-b1, b2-b3}. */
__C17_INTRIN __m128 _mm_hsub_ps(__m128 __a, __m128 __b)
{
    __v4sf __e = __builtin_shufflevector((__v4sf)__a, (__v4sf)__b, 0, 2, 4, 6);
    __v4sf __o = __builtin_shufflevector((__v4sf)__a, (__v4sf)__b, 1, 3, 5, 7);
    return (__m128)(__e - __o);
}

/* HADDPD: {a0+a1, b0+b1}. */
__C17_INTRIN __m128d _mm_hadd_pd(__m128d __a, __m128d __b)
{
    __v2df __e = __builtin_shufflevector((__v2df)__a, (__v2df)__b, 0, 2);
    __v2df __o = __builtin_shufflevector((__v2df)__a, (__v2df)__b, 1, 3);
    return (__m128d)(__e + __o);
}

/* HSUBPD: {a0-a1, b0-b1}. */
__C17_INTRIN __m128d _mm_hsub_pd(__m128d __a, __m128d __b)
{
    __v2df __e = __builtin_shufflevector((__v2df)__a, (__v2df)__b, 0, 2);
    __v2df __o = __builtin_shufflevector((__v2df)__a, (__v2df)__b, 1, 3);
    return (__m128d)(__e - __o);
}

/* MOVSHDUP: {a1, a1, a3, a3}. */
__C17_INTRIN __m128 _mm_movehdup_ps(__m128 __a)
{
    return (__m128)__builtin_shufflevector((__v4sf)__a, (__v4sf)__a, 1, 1, 3, 3);
}

/* MOVSLDUP: {a0, a0, a2, a2}. */
__C17_INTRIN __m128 _mm_moveldup_ps(__m128 __a)
{
    return (__m128)__builtin_shufflevector((__v4sf)__a, (__v4sf)__a, 0, 0, 2, 2);
}

/* MOVDDUP from a register: {a0, a0}. */
__C17_INTRIN __m128d _mm_movedup_pd(__m128d __a)
{
    return (__m128d)__builtin_shufflevector((__v2df)__a, (__v2df)__a, 0, 0);
}

/* MOVDDUP from memory: {*p, *p}. */
__C17_INTRIN __m128d _mm_loaddup_pd(double const *__p)
{
    double __x = *__p;
    return (__m128d)(__v2df){__x, __x};
}

/* LDDQU: unaligned 128-bit load. */
__C17_INTRIN __m128i _mm_lddqu_si128(__m128i const *__p)
{
    return *(__m128i_u const *)__p;
}

#endif /* _PMMINTRIN_H_INCLUDED */
