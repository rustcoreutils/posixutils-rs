/*
 * c17 builtin stdarg.h
 *
 * This file is part of the posixutils-rs project covered under
 * the MIT License. For the full license text, please see the LICENSE
 * file in the root directory of this project.
 * SPDX-License-Identifier: MIT
 */

/*
 * glibc's <stdio.h> and <wchar.h> want the va_list *type* without the macros
 * that go with it:
 *
 *     #define __need___va_list
 *     #include <stdarg.h>
 *
 * C17 7.21.1 does not give <stdio.h> `va_list`, so defining it there left a
 * name the program is entitled to use -- see the matching note in <stddef.h>.
 * Only __gnuc_va_list appears in that case; va_list and the va_* macros wait
 * for a real include.
 */

#ifndef _STDARG_H
#ifndef _ANSI_STDARG_H_

#ifndef __need___va_list
#define _STDARG_H
#define _ANSI_STDARG_H_
#endif
#undef __need___va_list

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
typedef __builtin_va_list __gnuc_va_list;
#endif

#ifdef _STDARG_H

typedef __builtin_va_list va_list;

#define va_start(ap, param) __builtin_va_start(ap, param)
#define va_end(ap) __builtin_va_end(ap)
#define va_arg(ap, type) __builtin_va_arg(ap, type)
#define va_copy(dest, src) __builtin_va_copy(dest, src)

/* GCC compatibility */
#define __va_copy(dest, src) __builtin_va_copy(dest, src)

#endif /* _STDARG_H */

#endif /* _ANSI_STDARG_H_ */
#endif /* _STDARG_H */
