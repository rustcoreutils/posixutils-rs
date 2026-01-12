/*
 * pcc builtin limits.h - Implementation limits for C99
 *
 * This header provides the standard integer type limits defined by C99.
 * It uses compiler-predefined macros to determine the actual limits
 * for the target platform.
 */

#ifndef _PCC_LIMITS_H
#define _PCC_LIMITS_H

/* Number of bits in a char */
#define CHAR_BIT __CHAR_BIT__

/* Maximum length of a multibyte character */
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

/* POSIX limits - often needed for compatibility */
#ifndef _POSIX_ARG_MAX
#define _POSIX_ARG_MAX 4096
#endif

#ifndef _POSIX_CHILD_MAX
#define _POSIX_CHILD_MAX 25
#endif

#ifndef _POSIX_HOST_NAME_MAX
#define _POSIX_HOST_NAME_MAX 255
#endif

#ifndef _POSIX_LINK_MAX
#define _POSIX_LINK_MAX 8
#endif

#ifndef _POSIX_LOGIN_NAME_MAX
#define _POSIX_LOGIN_NAME_MAX 9
#endif

#ifndef _POSIX_MAX_CANON
#define _POSIX_MAX_CANON 255
#endif

#ifndef _POSIX_MAX_INPUT
#define _POSIX_MAX_INPUT 255
#endif

#ifndef _POSIX_NAME_MAX
#define _POSIX_NAME_MAX 14
#endif

#ifndef _POSIX_NGROUPS_MAX
#define _POSIX_NGROUPS_MAX 8
#endif

#ifndef _POSIX_OPEN_MAX
#define _POSIX_OPEN_MAX 20
#endif

#ifndef _POSIX_PATH_MAX
#define _POSIX_PATH_MAX 256
#endif

#ifndef _POSIX_PIPE_BUF
#define _POSIX_PIPE_BUF 512
#endif

#ifndef _POSIX_SSIZE_MAX
#define _POSIX_SSIZE_MAX 32767
#endif

#ifndef _POSIX_STREAM_MAX
#define _POSIX_STREAM_MAX 8
#endif

#ifndef _POSIX_SYMLINK_MAX
#define _POSIX_SYMLINK_MAX 255
#endif

#ifndef _POSIX_TTY_NAME_MAX
#define _POSIX_TTY_NAME_MAX 9
#endif

#ifndef _POSIX_TZNAME_MAX
#define _POSIX_TZNAME_MAX 6
#endif

/* Runtime invariant values - actual system limits may be larger */
#ifndef NAME_MAX
#define NAME_MAX 255
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#ifndef PIPE_BUF
#define PIPE_BUF 4096
#endif

/* Word size helpers */
#if __SIZEOF_LONG__ == 8
#define __WORDSIZE 64
#else
#define __WORDSIZE 32
#endif

#endif /* _PCC_LIMITS_H */
