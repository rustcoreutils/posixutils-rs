/*
 * c17 builtin mm_malloc.h - _mm_malloc and _mm_free
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

#ifndef _MM_MALLOC_H_INCLUDED
#define _MM_MALLOC_H_INCLUDED

#include <stdlib.h>

/* Aligned allocation for SIMD data, freed by _mm_free. An alignment below a
   pointer's is raised to one, as posix_memalign requires. */
static __inline__ void *_mm_malloc(size_t __size, size_t __align)
{
    void *__p;
    if (__align < sizeof(void *))
        __align = sizeof(void *);
    if (posix_memalign(&__p, __align, __size))
        return 0;
    return __p;
}

static __inline__ void _mm_free(void *__p)
{
    free(__p);
}

#endif /* _MM_MALLOC_H_INCLUDED */
