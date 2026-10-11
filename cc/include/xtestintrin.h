/*
 * c17 builtin xtestintrin.h - XTEST, whether a transaction is running
 *
 * This file is part of the posixutils-rs project covered under
 * the MIT License. For the full license text, please see the LICENSE
 * file in the root directory of this project.
 * SPDX-License-Identifier: MIT
 *
 * Written from the instruction semantics in Intel's Intrinsics Guide, in
 * inline assembly: no `__builtin_ia32_*`. Available whatever -m flags are
 * given.
 */

#ifndef _X86GPRINTRIN_H_INCLUDED
#error "Never use <xtestintrin.h> directly; include <x86gprintrin.h> instead."
#endif

#ifndef _XTESTINTRIN_H_INCLUDED
#define _XTESTINTRIN_H_INCLUDED

#ifndef __C17_INTRIN
#define __C17_INTRIN static __inline__ __attribute__((__always_inline__, __artificial__))
#endif

/* 1 inside an RTM transaction or an HLE-elided region, else 0. */
__C17_INTRIN int _xtest(void)
{
    unsigned char __inside;
    __asm__ __volatile__("xtest\n\tsetnz %0" : "=q"(__inside) : : "cc", "memory");
    return __inside;
}

#endif /* _XTESTINTRIN_H_INCLUDED */
