/*
 * c17 builtin stddef.h
 *
 * This file is part of the posixutils-rs project covered under
 * the MIT License. For the full license text, please see the LICENSE
 * file in the root directory of this project.
 * SPDX-License-Identifier: MIT
 */

/*
 * glibc asks this header for one definition at a time. <string.h> does
 *
 *     #define __need_size_t
 *     #define __need_NULL
 *     #include <stddef.h>
 *
 * and must get only those two, because C17 7.1.3 leaves every other name for
 * the program to use as it likes -- 7.24.1 gives <string.h> size_t and NULL
 * and nothing else. Defining the whole file unconditionally made <string.h>
 * declare wchar_t, ptrdiff_t and max_align_t as well, so a program that used
 * one of those names for itself was rejected where gcc accepts it.
 *
 * The shape below is gcc's, and each part of it is load-bearing:
 *   - enter on any __need_*, even once a previous include has been through;
 *   - set the whole-file guard only when no __need_* is present, so a later
 *     plain include still has work to do;
 *   - guard each definition on its own, so that later include completes the
 *     set rather than redefining what is already there;
 *   - #undef each __need_* on the way out, since the caller does not.
 */

#if (!defined(_STDDEF_H) && !defined(_STDDEF_H_) && !defined(_ANSI_STDDEF_H)) \
	|| defined(__need_ptrdiff_t) || defined(__need_size_t)                \
	|| defined(__need_wchar_t) || defined(__need_NULL)                    \
	|| defined(__need_wint_t)

#if !defined(__need_ptrdiff_t) && !defined(__need_size_t) \
	&& !defined(__need_wchar_t) && !defined(__need_NULL) \
	&& !defined(__need_wint_t)
#define _STDDEF_H
#define _STDDEF_H_
#define _ANSI_STDDEF_H
#endif

#if defined(_STDDEF_H) || defined(__need_ptrdiff_t)
#ifndef _PTRDIFF_T
#define _PTRDIFF_T
#define _PTRDIFF_T_
#define _BSD_PTRDIFF_T_
#define _PTRDIFF_T_DECLARED
#define __DEFINED_ptrdiff_t
typedef __PTRDIFF_TYPE__ ptrdiff_t;
#endif
#undef __need_ptrdiff_t
#endif

#if defined(_STDDEF_H) || defined(__need_size_t)
#ifndef _SIZE_T
#define _SIZE_T
#define _SIZE_T_
#define _BSD_SIZE_T_
#define _SIZE_T_DEFINED
#define _SIZE_T_DECLARED
#define __DEFINED_size_t
typedef __SIZE_TYPE__ size_t;
#endif
#undef __need_size_t
#endif

#if defined(_STDDEF_H) || defined(__need_wchar_t)
#ifndef _WCHAR_T
#define _WCHAR_T
#define _WCHAR_T_
#define _BSD_WCHAR_T_
#define _WCHAR_T_DEFINED
#define _WCHAR_T_DECLARED
#define __DEFINED_wchar_t
typedef __WCHAR_TYPE__ wchar_t;
#endif
#undef __need_wchar_t
#endif

/*
 * wint_t is <wchar.h>'s and <wctype.h>'s, not this header's (7.19 does not
 * list it), so it appears only when asked for by name.
 */
#if defined(__need_wint_t)
#ifndef _WINT_T
#define _WINT_T
#define __DEFINED_wint_t
typedef __WINT_TYPE__ wint_t;
#endif
#undef __need_wint_t
#endif

#if defined(_STDDEF_H) || defined(__need_NULL)
#ifndef NULL
#define NULL ((void *)0)
#endif
#undef __need_NULL
#endif

/* The rest belongs to a full include only. */
#ifdef _STDDEF_H

#undef offsetof /* in case a system header has defined it */
#define offsetof(type, member) __builtin_offsetof(type, member)

/*
 * An object type whose alignment is as great as is supported by the
 * implementation in every context (C17 7.19p2). `long double` carries the
 * largest fundamental alignment on both targets.
 */
#ifndef __max_align_t_defined
#define __max_align_t_defined
typedef long double max_align_t;
#endif

#endif /* _STDDEF_H */

#endif /* not already fully included, or a __need_* was asked for */
