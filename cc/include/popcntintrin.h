/*
 * c17 builtin popcntintrin.h - POPCNT intrinsics
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

#ifndef _POPCNTINTRIN_H_INCLUDED
#define _POPCNTINTRIN_H_INCLUDED

#ifndef __C17_INTRIN
#define __C17_INTRIN static __inline__ __attribute__((__always_inline__, __artificial__))
#endif

__C17_INTRIN int _mm_popcnt_u32(unsigned int __x) { return __builtin_popcount(__x); }
__C17_INTRIN long long _mm_popcnt_u64(unsigned long long __x) { return __builtin_popcountll(__x); }

#endif /* _POPCNTINTRIN_H_INCLUDED */
