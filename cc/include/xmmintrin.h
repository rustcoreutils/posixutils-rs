/*
 * pcc builtin xmmintrin.h - SSE intrinsics
 *
 * This file is part of the posixutils-rs project covered under
 * the MIT License. For the full license text, please see the LICENSE
 * file in the root directory of this project.
 * SPDX-License-Identifier: MIT
 */

#ifndef _XMMINTRIN_H
#define _XMMINTRIN_H

/* SSE prefetch hints */
#define _MM_HINT_T0  1
#define _MM_HINT_T1  2
#define _MM_HINT_T2  3
#define _MM_HINT_NTA 0

/* _mm_prefetch - prefetch data into cache */
static __inline__ void __attribute__((__always_inline__))
_mm_prefetch(const void *__p, int __i) {
    (void)__i;
    __builtin_prefetch(__p);
}

#endif /* _XMMINTRIN_H */
