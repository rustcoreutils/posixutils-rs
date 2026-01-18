/*
 * Minimal cpuid.h - GCC-compatible CPUID intrinsics
 * For pcc (posixutils-rs C compiler)
 */
#ifndef _CPUID_H_INCLUDED
#define _CPUID_H_INCLUDED

/* __cpuid_count: Execute CPUID instruction with leaf/subleaf
 * Parameters:
 *   __leaf    - Main function number (EAX input)
 *   __subleaf - Sub-function number (ECX input)
 *   __eax, __ebx, __ecx, __edx - Output registers
 */
#define __cpuid_count(__leaf, __subleaf, __eax, __ebx, __ecx, __edx) \
    __asm__ __volatile__ ( \
        "cpuid" \
        : "=a" (__eax), "=b" (__ebx), "=c" (__ecx), "=d" (__edx) \
        : "a" (__leaf), "c" (__subleaf) \
    )

/* __cpuid: Execute CPUID instruction (subleaf 0)
 * Parameters:
 *   __leaf - Main function number (EAX input)
 *   __eax, __ebx, __ecx, __edx - Output registers
 */
#define __cpuid(__leaf, __eax, __ebx, __ecx, __edx) \
    __cpuid_count(__leaf, 0, __eax, __ebx, __ecx, __edx)

/* __get_cpuid_max: Get maximum supported CPUID leaf
 * Parameters:
 *   __ext - 0 for basic leaves, 0x80000000 for extended leaves
 *   __sig - Optional pointer to store EBX (vendor signature part)
 * Returns: Maximum supported leaf number in EAX
 */
static __inline unsigned int
__get_cpuid_max(unsigned int __ext, unsigned int *__sig)
{
    unsigned int __eax, __ebx, __ecx, __edx;
    __cpuid(__ext, __eax, __ebx, __ecx, __edx);
    if (__sig)
        *__sig = __ebx;
    return __eax;
}

/* __get_cpuid: Execute CPUID and return results
 * Parameters:
 *   __leaf - Main function number
 *   __eax, __ebx, __ecx, __edx - Pointers to output registers
 * Returns: 1 if CPUID leaf is supported, 0 otherwise
 */
static __inline int
__get_cpuid(unsigned int __leaf, unsigned int *__eax, unsigned int *__ebx,
            unsigned int *__ecx, unsigned int *__edx)
{
    unsigned int __max = __get_cpuid_max(__leaf >= 0x80000000 ? 0x80000000 : 0, 0);
    if (__leaf > __max)
        return 0;
    __cpuid_count(__leaf, 0, *__eax, *__ebx, *__ecx, *__edx);
    return 1;
}

/* __get_cpuid_count: Execute CPUID with subleaf and return results
 * Parameters:
 *   __leaf, __subleaf - Function numbers
 *   __eax, __ebx, __ecx, __edx - Pointers to output registers
 * Returns: 1 if CPUID leaf is supported, 0 otherwise
 */
static __inline int
__get_cpuid_count(unsigned int __leaf, unsigned int __subleaf,
                  unsigned int *__eax, unsigned int *__ebx,
                  unsigned int *__ecx, unsigned int *__edx)
{
    unsigned int __max = __get_cpuid_max(__leaf >= 0x80000000 ? 0x80000000 : 0, 0);
    if (__leaf > __max)
        return 0;
    __cpuid_count(__leaf, __subleaf, *__eax, *__ebx, *__ecx, *__edx);
    return 1;
}

#endif /* _CPUID_H_INCLUDED */
