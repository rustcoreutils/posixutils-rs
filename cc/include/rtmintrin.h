/*
 * c17 builtin rtmintrin.h - Restricted Transactional Memory intrinsics
 *
 * This file is part of the posixutils-rs project covered under
 * the MIT License. For the full license text, please see the LICENSE
 * file in the root directory of this project.
 * SPDX-License-Identifier: MIT
 *
 * Written from the instruction semantics in Intel's Intrinsics Guide, in
 * inline assembly: no `__builtin_ia32_*`. Every function is available
 * whatever -m flags are given; `__RTM__` follows -mrtm, as gcc's does.
 */

#ifndef _X86GPRINTRIN_H_INCLUDED
#error "Never use <rtmintrin.h> directly; include <x86gprintrin.h> instead."
#endif

#ifndef _RTMINTRIN_H_INCLUDED
#define _RTMINTRIN_H_INCLUDED

#ifndef __C17_INTRIN
#define __C17_INTRIN static __inline__ __attribute__((__always_inline__, __artificial__))
#endif

/* What _xbegin answers when the transaction has begun. */
#define _XBEGIN_STARTED (~0u)
/* The abort status: why the transaction ended, and the code _xabort gave
   in its top byte. */
#define _XABORT_EXPLICIT (1 << 0)
#define _XABORT_RETRY (1 << 1)
#define _XABORT_CONFLICT (1 << 2)
#define _XABORT_CAPACITY (1 << 3)
#define _XABORT_DEBUG (1 << 4)
#define _XABORT_NESTED (1 << 5)
#define _XABORT_CODE(x) (((x) >> 24) & 0xFF)

/* Begin a transaction: _XBEGIN_STARTED now, or -- when it aborts, at this
   same point -- the abort status, which XBEGIN leaves in %eax for the
   fallback path, here the instruction after it. */
__C17_INTRIN unsigned int _xbegin(void)
{
    unsigned int __status = _XBEGIN_STARTED;
    __asm__ __volatile__("xbegin 1f\n1:" : "+a"(__status) : : "memory");
    return __status;
}

/* Commit the innermost transaction. */
__C17_INTRIN void _xend(void)
{
    __asm__ __volatile__("xend" : : : "memory");
}

/* Abort the transaction with the 8-bit code IMM, which must be a constant:
   XABORT takes it as an immediate. */
#define _xabort(imm) __asm__ __volatile__("xabort %0" : : "i"(imm) : "memory")

#endif /* _RTMINTRIN_H_INCLUDED */
