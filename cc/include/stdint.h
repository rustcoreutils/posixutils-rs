//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// <stdint.h> — C17 7.20
//
// C17 4p6 puts this header in the *freestanding* set, so the implementation
// must supply it rather than lean on the host's. Everything here is spelled
// against the compiler's own __INTn_TYPE__ / __INTn_MAX__ predefines (see
// cc/arch/mod.rs), which is what makes it target-independent and what let the
// host's copy work in the first place.
//

/*
 * A hosted glibc build uses the C library's own <stdint.h>, as gcc's does:
 * glibc's headers include <stdint.h> expecting it to bring in <features.h>
 * and <sys/cdefs.h> (`__BEGIN_DECLS`, `__THROW`, `__flexarr`), and
 * <sys/eventfd.h>, <sys/inotify.h> and others include nothing else. Its guard
 * is also `_STDINT_H`, so this test must come before the one below. The
 * test is the GNU/Linux target, not `__GLIBC__`, which only glibc's
 * <features.h> defines.
 */
#if __STDC_HOSTED__ && defined(__gnu_linux__) && !defined(_STDINT_H)
#include_next <stdint.h>
#endif

#ifndef _STDINT_H
#define _STDINT_H

/* 7.20.1.1 Exact-width integer types */
typedef __INT8_TYPE__   int8_t;
typedef __INT16_TYPE__  int16_t;
typedef __INT32_TYPE__  int32_t;
typedef __INT64_TYPE__  int64_t;
typedef __UINT8_TYPE__  uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;

/* 7.20.1.2 Minimum-width integer types */
typedef __INT_LEAST8_TYPE__   int_least8_t;
typedef __INT_LEAST16_TYPE__  int_least16_t;
typedef __INT_LEAST32_TYPE__  int_least32_t;
typedef __INT_LEAST64_TYPE__  int_least64_t;
typedef __UINT_LEAST8_TYPE__  uint_least8_t;
typedef __UINT_LEAST16_TYPE__ uint_least16_t;
typedef __UINT_LEAST32_TYPE__ uint_least32_t;
typedef __UINT_LEAST64_TYPE__ uint_least64_t;

/* 7.20.1.3 Fastest minimum-width integer types */
typedef __INT_FAST8_TYPE__   int_fast8_t;
typedef __INT_FAST16_TYPE__  int_fast16_t;
typedef __INT_FAST32_TYPE__  int_fast32_t;
typedef __INT_FAST64_TYPE__  int_fast64_t;
typedef __UINT_FAST8_TYPE__  uint_fast8_t;
typedef __UINT_FAST16_TYPE__ uint_fast16_t;
typedef __UINT_FAST32_TYPE__ uint_fast32_t;
typedef __UINT_FAST64_TYPE__ uint_fast64_t;

/* 7.20.1.4 Integer types capable of holding object pointers */
typedef __INTPTR_TYPE__  intptr_t;
typedef __UINTPTR_TYPE__ uintptr_t;

/* 7.20.1.5 Greatest-width integer types */
typedef __INTMAX_TYPE__  intmax_t;
typedef __UINTMAX_TYPE__ uintmax_t;

/* 7.20.2.1 Limits of exact-width integer types */
#define INT8_MAX   __INT8_MAX__
#define INT16_MAX  __INT16_MAX__
#define INT32_MAX  __INT32_MAX__
#define INT64_MAX  __INT64_MAX__
#define INT8_MIN   (-INT8_MAX - 1)
#define INT16_MIN  (-INT16_MAX - 1)
#define INT32_MIN  (-INT32_MAX - 1)
#define INT64_MIN  (-INT64_MAX - 1)
#define UINT8_MAX  __UINT8_MAX__
#define UINT16_MAX __UINT16_MAX__
#define UINT32_MAX __UINT32_MAX__
#define UINT64_MAX __UINT64_MAX__

/* 7.20.2.2 Limits of minimum-width integer types */
#define INT_LEAST8_MAX   __INT_LEAST8_MAX__
#define INT_LEAST16_MAX  __INT_LEAST16_MAX__
#define INT_LEAST32_MAX  __INT_LEAST32_MAX__
#define INT_LEAST64_MAX  __INT_LEAST64_MAX__
#define INT_LEAST8_MIN   (-INT_LEAST8_MAX - 1)
#define INT_LEAST16_MIN  (-INT_LEAST16_MAX - 1)
#define INT_LEAST32_MIN  (-INT_LEAST32_MAX - 1)
#define INT_LEAST64_MIN  (-INT_LEAST64_MAX - 1)
#define UINT_LEAST8_MAX  __UINT_LEAST8_MAX__
#define UINT_LEAST16_MAX __UINT_LEAST16_MAX__
#define UINT_LEAST32_MAX __UINT_LEAST32_MAX__
#define UINT_LEAST64_MAX __UINT_LEAST64_MAX__

/* 7.20.2.3 Limits of fastest minimum-width integer types */
#define INT_FAST8_MAX   __INT_FAST8_MAX__
#define INT_FAST16_MAX  __INT_FAST16_MAX__
#define INT_FAST32_MAX  __INT_FAST32_MAX__
#define INT_FAST64_MAX  __INT_FAST64_MAX__
#define INT_FAST8_MIN   (-INT_FAST8_MAX - 1)
#define INT_FAST16_MIN  (-INT_FAST16_MAX - 1)
#define INT_FAST32_MIN  (-INT_FAST32_MAX - 1)
#define INT_FAST64_MIN  (-INT_FAST64_MAX - 1)
#define UINT_FAST8_MAX  __UINT_FAST8_MAX__
#define UINT_FAST16_MAX __UINT_FAST16_MAX__
#define UINT_FAST32_MAX __UINT_FAST32_MAX__
#define UINT_FAST64_MAX __UINT_FAST64_MAX__

/* 7.20.2.4 Limits of integer types capable of holding object pointers */
#define INTPTR_MAX  __INTPTR_MAX__
#define INTPTR_MIN  (-INTPTR_MAX - 1)
#define UINTPTR_MAX __UINTPTR_MAX__

/* 7.20.2.5 Limits of greatest-width integer types */
#define INTMAX_MAX  __INTMAX_MAX__
#define INTMAX_MIN  (-INTMAX_MAX - 1)
#define UINTMAX_MAX __UINTMAX_MAX__

/* 7.20.3 Limits of other integer types */
#define PTRDIFF_MAX    __PTRDIFF_MAX__
#define PTRDIFF_MIN    (-PTRDIFF_MAX - 1)
/* Not `-MAX - 1`: these three are unsigned on some targets, so each minimum
   is its own predefine rather than the two's-complement companion of its
   maximum. */
#define SIG_ATOMIC_MAX __SIG_ATOMIC_MAX__
#define SIG_ATOMIC_MIN __SIG_ATOMIC_MIN__
#define SIZE_MAX       __SIZE_MAX__
#define WCHAR_MAX      __WCHAR_MAX__
#define WCHAR_MIN      __WCHAR_MIN__
#define WINT_MAX       __WINT_MAX__
#define WINT_MIN       __WINT_MIN__

/* 7.20.4 Macros for integer constants. Each __INTN_C predefine pastes the
   suffix of int_leastN_t's promoted type, so the result is target-correct:
   int64_t is `long` on Linux and `long long` on Darwin. */
#define INT8_C(c)    __INT8_C(c)
#define INT16_C(c)   __INT16_C(c)
#define INT32_C(c)   __INT32_C(c)
#define INT64_C(c)   __INT64_C(c)
#define UINT8_C(c)   __UINT8_C(c)
#define UINT16_C(c)  __UINT16_C(c)
#define UINT32_C(c)  __UINT32_C(c)
#define UINT64_C(c)  __UINT64_C(c)
#define INTMAX_C(c)  __INTMAX_C(c)
#define UINTMAX_C(c) __UINTMAX_C(c)

#endif /* _STDINT_H */
