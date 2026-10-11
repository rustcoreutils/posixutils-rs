/*
 * c17 builtin x86intrin.h - x86 intrinsics and ia32 helpers
 *
 * This file is part of the posixutils-rs project covered under
 * the MIT License. For the full license text, please see the LICENSE
 * file in the root directory of this project.
 * SPDX-License-Identifier: MIT
 *
 * Written from the instruction semantics in Intel's Intrinsics Guide, over
 * GNU vectors and c17's vector builtins: no `__builtin_ia32_*` but the
 * time-stamp counter's, which glibc calls by those names too. Every
 * function is available whatever -m flags are given; the feature macros
 * (`__SSE4_1__` and the rest) follow the flags, as gcc's do.
 */

#ifndef _X86INTRIN_H_INCLUDED
#define _X86INTRIN_H_INCLUDED

#include <immintrin.h>

/* The time-stamp counter, alone and with the processor's signature: gcc's
   builtins, which c17 has too. */
__C17_INTRIN unsigned long long __rdtsc(void) { return __builtin_ia32_rdtsc(); }
__C17_INTRIN unsigned long long __rdtscp(unsigned int *__aux) { return __builtin_ia32_rdtscp(__aux); }

__C17_INTRIN int __bsfd(int __x) { return __builtin_ctz((unsigned int)__x); }
__C17_INTRIN int __bsrd(int __x) { return 31 - __builtin_clz((unsigned int)__x); }
__C17_INTRIN int __bswapd(int __x) { return (int)__builtin_bswap32((unsigned int)__x); }
__C17_INTRIN long long __bswapq(long long __x) { return (long long)__builtin_bswap64((unsigned long long)__x); }
#define _bswap(x) __bswapd(x)
#define _bswap64(x) __bswapq(x)
#define _bit_scan_forward(x) __bsfd(x)
#define _bit_scan_reverse(x) __bsrd(x)
#define _rdtsc() __rdtsc()

#endif /* _X86INTRIN_H_INCLUDED */
