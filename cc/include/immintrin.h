/*
 * c17 builtin immintrin.h - x86 intrinsics, through SSE4.2
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

#ifndef _IMMINTRIN_H_INCLUDED
#define _IMMINTRIN_H_INCLUDED

/* c17 implements the 128-bit instruction sets, SSE through SSE4.2. The AVX
   families are not here: code that names one of their types or functions
   does not compile, and code that tests `__AVX__` and friends takes its
   other path. */
#include <nmmintrin.h>

#endif /* _IMMINTRIN_H_INCLUDED */
