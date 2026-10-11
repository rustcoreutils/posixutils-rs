/*
 * c17 builtin cetintrin.h - CET shadow-stack intrinsics
 *
 * This file is part of the posixutils-rs project covered under
 * the MIT License. For the full license text, please see the LICENSE
 * file in the root directory of this project.
 * SPDX-License-Identifier: MIT
 *
 * Written from the instruction semantics in Intel's Intrinsics Guide, in
 * inline assembly: no `__builtin_ia32_*`. Every function is available
 * whatever -m flags are given; `__SHSTK__` follows -mshstk, as gcc's does.
 * RDSSP is a no-op where the shadow stack is not enabled, so _get_ssp
 * answers 0 there; the others fault without one.
 */

#ifndef _X86GPRINTRIN_H_INCLUDED
#error "Never use <cetintrin.h> directly; include <x86gprintrin.h> instead."
#endif

#ifndef _CETINTRIN_H_INCLUDED
#define _CETINTRIN_H_INCLUDED

#ifndef __C17_INTRIN
#define __C17_INTRIN static __inline__ __attribute__((__always_inline__, __artificial__))
#endif

/* The shadow-stack pointer, or 0 without a shadow stack. */
__C17_INTRIN unsigned long long _get_ssp(void)
{
    unsigned long long __ssp = 0;
    __asm__ __volatile__("rdsspq %0" : "+r"(__ssp));
    return __ssp;
}

/* Pop N (its low eight bits) entries off the shadow stack. */
__C17_INTRIN void _inc_ssp(unsigned int __n)
{
    __asm__ __volatile__("incsspq %0" : : "r"((unsigned long long)__n) : "memory");
}

__C17_INTRIN void _saveprevssp(void)
{
    __asm__ __volatile__("saveprevssp" : : : "memory");
}

__C17_INTRIN void _rstorssp(void *__p)
{
    __asm__ __volatile__("rstorssp %0" : : "m"(*(char *)__p) : "memory");
}

__C17_INTRIN void _wrssd(unsigned int __v, void *__p)
{
    __asm__ __volatile__("wrssd %0, %1" : : "r"(__v), "m"(*(unsigned int *)__p) : "memory");
}

__C17_INTRIN void _wrssq(unsigned long long __v, void *__p)
{
    __asm__ __volatile__("wrssq %0, %1" : : "r"(__v), "m"(*(unsigned long long *)__p) : "memory");
}

__C17_INTRIN void _wrussd(unsigned int __v, void *__p)
{
    __asm__ __volatile__("wrussd %0, %1" : : "r"(__v), "m"(*(unsigned int *)__p) : "memory");
}

__C17_INTRIN void _wrussq(unsigned long long __v, void *__p)
{
    __asm__ __volatile__("wrussq %0, %1" : : "r"(__v), "m"(*(unsigned long long *)__p) : "memory");
}

__C17_INTRIN void _setssbsy(void)
{
    __asm__ __volatile__("setssbsy" : : : "memory");
}

__C17_INTRIN void _clrssbsy(void *__p)
{
    __asm__ __volatile__("clrssbsy %0" : : "m"(*(unsigned long long *)__p) : "memory");
}

#endif /* _CETINTRIN_H_INCLUDED */
