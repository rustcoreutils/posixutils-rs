/*
 * c17 builtin limits.h - sizes of integer types (C17 7.10, 5.2.4.2.1)
 *
 * Two owners share <limits.h>. The compiler knows how big each integer type
 * is; the system knows its POSIX limits (LINE_MAX, NGROUPS_MAX, IOV_MAX,
 * SSIZE_MAX, the _POSIX_* minima, ...). So this header forwards to the
 * system's, then defines the compiler's part itself, as gcc's and clang's
 * do.
 */

#ifndef _C17_LIMITS_H
#define _C17_LIMITS_H

/* glibc's <limits.h>, seeing __GNUC__, would #include_next the compiler's
   own header for the ISO constants unless this, the name gcc's header
   defines, says it has been reached already. That is this header. */
#ifndef _GCC_LIMITS_H_
#define _GCC_LIMITS_H_
#endif

#if __has_include_next(<limits.h>)
#include_next <limits.h>
#endif

/* The system header may have defined any of these its own way (glibc
   spells LLONG_MIN as (-LLONG_MAX-1), Apple spells them all as numbers).
   The compiler knows the sizes, so its definitions replace them. */
#undef CHAR_BIT
#undef SCHAR_MIN
#undef SCHAR_MAX
#undef UCHAR_MAX
#undef CHAR_MIN
#undef CHAR_MAX
#undef SHRT_MIN
#undef SHRT_MAX
#undef USHRT_MAX
#undef INT_MIN
#undef INT_MAX
#undef UINT_MAX
#undef LONG_MIN
#undef LONG_MAX
#undef ULONG_MAX
#undef LLONG_MIN
#undef LLONG_MAX
#undef ULLONG_MAX

/* Number of bits in a char */
#define CHAR_BIT __CHAR_BIT__

/* Maximum length of a multibyte character: the C library's, which knows
   its locales, when it has said. */
#ifndef MB_LEN_MAX
#define MB_LEN_MAX 16
#endif

/* Minimum and maximum values a signed char can hold */
#define SCHAR_MIN (-__SCHAR_MAX__ - 1)
#define SCHAR_MAX __SCHAR_MAX__

/* Maximum value an unsigned char can hold (minimum is 0) */
#define UCHAR_MAX (__SCHAR_MAX__ * 2 + 1)

/* Minimum and maximum values a char can hold */
#ifdef __CHAR_UNSIGNED__
#define CHAR_MIN 0
#define CHAR_MAX UCHAR_MAX
#else
#define CHAR_MIN SCHAR_MIN
#define CHAR_MAX SCHAR_MAX
#endif

/* Minimum and maximum values a signed short int can hold */
#define SHRT_MIN (-__SHRT_MAX__ - 1)
#define SHRT_MAX __SHRT_MAX__

/* Maximum value an unsigned short int can hold (minimum is 0) */
#define USHRT_MAX (__SHRT_MAX__ * 2 + 1)

/* Minimum and maximum values a signed int can hold */
#define INT_MIN (-__INT_MAX__ - 1)
#define INT_MAX __INT_MAX__

/* Maximum value an unsigned int can hold (minimum is 0) */
#define UINT_MAX (__INT_MAX__ * 2U + 1U)

/* Minimum and maximum values a signed long int can hold */
#define LONG_MIN (-__LONG_MAX__ - 1L)
#define LONG_MAX __LONG_MAX__

/* Maximum value an unsigned long int can hold (minimum is 0) */
#define ULONG_MAX (__LONG_MAX__ * 2UL + 1UL)

/* Minimum and maximum values a signed long long int can hold */
#define LLONG_MIN (-__LONG_LONG_MAX__ - 1LL)
#define LLONG_MAX __LONG_LONG_MAX__

/* Maximum value an unsigned long long int can hold (minimum is 0) */
#define ULLONG_MAX (__LONG_LONG_MAX__ * 2ULL + 1ULL)

#endif /* _C17_LIMITS_H */
