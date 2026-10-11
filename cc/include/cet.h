/*
 * c17 builtin cet.h - Control-flow Enforcement Technology for assembly
 *
 * This file is part of the posixutils-rs project covered under
 * the MIT License. For the full license text, please see the LICENSE
 * file in the root directory of this project.
 * SPDX-License-Identifier: MIT
 *
 * gcc's interface, for x86-64 sources assembled with `-fcf-protection`:
 * `_CET_ENDBR` is the landing pad an indirect branch needs, `endbr64`,
 * under branch protection (bit 0 of `__CET__`) and nothing otherwise; and
 * including the header in an ELF assembly source gives the object the
 * `.note.gnu.property` note that marks it as built for the protections
 * `__CET__` names, as c17's own objects are marked. A C source sees
 * nothing but the include guard.
 */

#ifndef _CET_H_INCLUDED
#define _CET_H_INCLUDED

#ifdef __ASSEMBLER__

#if defined(__CET__) && (__CET__ & 1) != 0
#define _CET_ENDBR endbr64
#else
#define _CET_ENDBR
#endif

#if defined(__ELF__) && defined(__CET__)
/* NT_GNU_PROPERTY_TYPE_0 holding GNU_PROPERTY_X86_FEATURE_1_AND, whose
   bits are IBT (1) and SHSTK (2): __CET__'s own. Eight-byte aligned, as
   the LP64 ELF ABI has it. */
	.pushsection ".note.gnu.property", "a"
	.p2align 3
	.long 1f - 0f
	.long 4f - 1f
	.long 5
0:
	.asciz "GNU"
1:
	.p2align 3
	.long 0xc0000002
	.long 3f - 2f
2:
	.long (__CET__ & 3)
3:
	.p2align 3
4:
	.popsection
#endif

#endif /* __ASSEMBLER__ */

#endif /* _CET_H_INCLUDED */
