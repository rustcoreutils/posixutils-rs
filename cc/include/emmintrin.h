/*
 * pcc builtin emmintrin.h - SSE2 intrinsics
 *
 * This file is part of the posixutils-rs project covered under
 * the MIT License. For the full license text, please see the LICENSE
 * file in the root directory of this project.
 * SPDX-License-Identifier: MIT
 */

#ifndef _EMMINTRIN_H
#define _EMMINTRIN_H

/* Include SSE intrinsics (emmintrin.h includes xmmintrin.h in GCC) */
#include <xmmintrin.h>

/* _mm_pause - inserts a PAUSE instruction for spin-wait loops
 * This improves performance in spin-lock scenarios by reducing
 * pipeline stalls and power consumption */
static __inline__ void __attribute__((__always_inline__))
_mm_pause(void) {
    __asm__ __volatile__("pause" ::: "memory");
}

/* _mm_clflush - flush cache line containing address p */
static __inline__ void __attribute__((__always_inline__))
_mm_clflush(void const *__p) {
    __asm__ __volatile__("clflush (%0)" : : "r"(__p) : "memory");
}

/* _mm_lfence - load fence */
static __inline__ void __attribute__((__always_inline__))
_mm_lfence(void) {
    __asm__ __volatile__("lfence" ::: "memory");
}

/* _mm_mfence - memory fence */
static __inline__ void __attribute__((__always_inline__))
_mm_mfence(void) {
    __asm__ __volatile__("mfence" ::: "memory");
}

/* _mm_sfence - store fence (from SSE, but commonly used with SSE2) */
static __inline__ void __attribute__((__always_inline__))
_mm_sfence(void) {
    __asm__ __volatile__("sfence" ::: "memory");
}

#endif /* _EMMINTRIN_H */
