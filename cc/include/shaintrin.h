/*
 * c17 builtin shaintrin.h - SHA-1 and SHA-256 extensions (SHA-NI)
 *
 * This file is part of the posixutils-rs project covered under
 * the MIT License. For the full license text, please see the LICENSE
 * file in the root directory of this project.
 * SPDX-License-Identifier: MIT
 *
 * Written from the instruction semantics in Intel's Intrinsics Guide, in
 * inline assembly over `__m128i` operands: no `__builtin_ia32_*`. Every
 * function is available whatever -m flags are given; `__SHA__` follows
 * -msha, as gcc's does.
 */

#ifndef _IMMINTRIN_H_INCLUDED
#error "Never use <shaintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _SHAINTRIN_H_INCLUDED
#define _SHAINTRIN_H_INCLUDED

#ifndef __C17_INTRIN
#define __C17_INTRIN static __inline__ __attribute__((__always_inline__, __artificial__))
#endif

/* Each instruction updates its first operand from it and the second. */
#define __C17_SHA_BINARY(name, insn)                                       \
    __C17_INTRIN __m128i name(__m128i __a, __m128i __b)                    \
    {                                                                      \
        __asm__(insn " %1, %0" : "+x"(__a) : "x"(__b));                    \
        return __a;                                                        \
    }

__C17_SHA_BINARY(_mm_sha1msg1_epu32, "sha1msg1")
__C17_SHA_BINARY(_mm_sha1msg2_epu32, "sha1msg2")
__C17_SHA_BINARY(_mm_sha1nexte_epu32, "sha1nexte")
__C17_SHA_BINARY(_mm_sha256msg1_epu32, "sha256msg1")
__C17_SHA_BINARY(_mm_sha256msg2_epu32, "sha256msg2")

#undef __C17_SHA_BINARY

/* Four SHA-1 rounds of the function FUNC, 0 to 3: an immediate, so this
   is a macro and FUNC a constant expression. */
#define _mm_sha1rnds4_epu32(A, B, FUNC)                                    \
    (__extension__({                                                       \
        __m128i __sha_a = (A), __sha_b = (B);                              \
        __asm__("sha1rnds4 %2, %1, %0"                                     \
                : "+x"(__sha_a)                                            \
                : "x"(__sha_b), "i"(FUNC));                                \
        __sha_a;                                                           \
    }))

/* Two SHA-256 rounds of the state halves A (CDGH) and B (ABEF), with the
   message plus round constants K. SHA256RNDS2 reads K from %xmm0 alone,
   so K is moved there, and %xmm0 is no operand's. */
__C17_INTRIN __m128i _mm_sha256rnds2_epu32(__m128i __a, __m128i __b, __m128i __k)
{
    __asm__("movdqa %2, %%xmm0\n\tsha256rnds2 %%xmm0, %1, %0"
            : "+x"(__a)
            : "x"(__b), "x"(__k)
            : "xmm0");
    return __a;
}

#endif /* _SHAINTRIN_H_INCLUDED */
