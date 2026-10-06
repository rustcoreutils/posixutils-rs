//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The `_FORTIFY_SOURCE` checking functions, decided at compile time as gcc
// decides them.
//

use crate::common::compile_and_run_everywhere;

/// Every `_chk` form glibc's headers call, with the program's own checking
/// functions counting the calls that reach them -- the scheme of
/// gcc.c-torture's `execute/builtins/*-chk` tests. Every expectation was
/// checked against gcc 13 at -O0, -O1 and -O2 on x86-64 and aarch64, which
/// also leave the same `_chk` calls in place.
///
/// - `fits`: a known destination and a write known to fit -- a constant
///   length, the largest of two (`n = c ? 8 : 4`), a known string or the
///   longest of two, an empty append, a format with no directive or `"%s"`
///   of a known string -- leaves no check once optimized.
/// - `unknown`: a destination the compiler cannot see is `(size_t)-1`, and
///   the plain function is called: no check, and none of an unknown size.
/// - `at_run_time`: a known destination and a length known only at run
///   time keeps the check, which passes.
/// - `overflows`: a write that provably overflows keeps the check at every
///   level, and it fails.
#[test]
fn builtins_fortified_calls_are_decided_as_gcc_decides() {
    compile_and_run_everywhere("builtins_fortified_calls", FORTIFIED);
}

const FORTIFIED: &str = r#"
typedef __SIZE_TYPE__ size_t;
typedef __builtin_va_list va_list;
#define va_start(ap, p) __builtin_va_start(ap, p)
#define va_end(ap) __builtin_va_end(ap)

extern void *memcpy(void *, const void *, size_t);
extern void *mempcpy(void *, const void *, size_t);
extern void *memmove(void *, const void *, size_t);
extern void *memset(void *, int, size_t);
extern char *strcpy(char *, const char *);
extern char *stpcpy(char *, const char *);
extern char *strncpy(char *, const char *, size_t);
extern char *stpncpy(char *, const char *, size_t);
extern char *strcat(char *, const char *);
extern char *strncat(char *, const char *, size_t);
extern size_t strlen(const char *);
extern int memcmp(const void *, const void *, size_t);
extern int vsnprintf(char *, size_t, const char *, va_list);
extern void abort(void);

/* The library's checking functions, counting the calls that reach them:
   `chk_calls` every one, `unknown_calls` those of an unknown object, which
   an optimizing compiler always makes the plain call. A check that fails
   jumps back to the test that expects it. */
static volatile int chk_calls, unknown_calls, failing;
static void *fail_buf[5];

#define CHECK(os, n)                        \
    do {                                    \
        ++chk_calls;                        \
        if ((os) == (size_t)-1)             \
            ++unknown_calls;                \
        if ((n) > (os)) {                   \
            if (failing)                    \
                __builtin_longjmp(fail_buf, 1); \
            abort();                        \
        }                                   \
    } while (0)

__attribute__((noinline)) void *__memcpy_chk(void *d, const void *s, size_t n, size_t os)
{ CHECK(os, n); return memcpy(d, s, n); }
__attribute__((noinline)) void *__mempcpy_chk(void *d, const void *s, size_t n, size_t os)
{ CHECK(os, n); return (char *)memcpy(d, s, n) + n; }
__attribute__((noinline)) void *__memmove_chk(void *d, const void *s, size_t n, size_t os)
{ CHECK(os, n); return memmove(d, s, n); }
__attribute__((noinline)) void *__memset_chk(void *d, int c, size_t n, size_t os)
{ CHECK(os, n); return memset(d, c, n); }
__attribute__((noinline)) char *__strcpy_chk(char *d, const char *s, size_t os)
{ CHECK(os, strlen(s) + 1); return strcpy(d, s); }
__attribute__((noinline)) char *__stpcpy_chk(char *d, const char *s, size_t os)
{ CHECK(os, strlen(s) + 1); return stpcpy(d, s); }
__attribute__((noinline)) char *__strncpy_chk(char *d, const char *s, size_t n, size_t os)
{ CHECK(os, n); return strncpy(d, s, n); }
__attribute__((noinline)) char *__stpncpy_chk(char *d, const char *s, size_t n, size_t os)
{ CHECK(os, n); return stpncpy(d, s, n); }
__attribute__((noinline)) char *__strcat_chk(char *d, const char *s, size_t os)
{ CHECK(os, strlen(d) + strlen(s) + 1); return strcat(d, s); }
__attribute__((noinline)) char *__strncat_chk(char *d, const char *s, size_t n, size_t os)
{
    size_t k = strlen(s) < n ? strlen(s) : n;
    CHECK(os, strlen(d) + k + 1);
    return strncat(d, s, n);
}
__attribute__((noinline)) int __vsprintf_chk(char *d, int flag, size_t os, const char *fmt, va_list ap)
{
    char tmp[256];
    int r = vsnprintf(tmp, sizeof tmp, fmt, ap);
    (void)flag;
    CHECK(os, (size_t)r + 1);
    memcpy(d, tmp, r + 1);
    return r;
}
__attribute__((noinline)) int __sprintf_chk(char *d, int flag, size_t os, const char *fmt, ...)
{
    char tmp[256];
    va_list ap;
    int r;
    va_start(ap, fmt);
    r = vsnprintf(tmp, sizeof tmp, fmt, ap);
    va_end(ap);
    (void)flag;
    CHECK(os, (size_t)r + 1);
    memcpy(d, tmp, r + 1);
    return r;
}
__attribute__((noinline)) int __vsnprintf_chk(char *d, size_t n, int flag, size_t os, const char *fmt, va_list ap)
{
    (void)flag;
    CHECK(os, n);
    return vsnprintf(d, n, fmt, ap);
}
__attribute__((noinline)) int __snprintf_chk(char *d, size_t n, int flag, size_t os, const char *fmt, ...)
{
    va_list ap;
    int r;
    (void)flag;
    CHECK(os, n);
    va_start(ap, fmt);
    r = vsnprintf(d, n, fmt, ap);
    va_end(ap);
    return r;
}

/* What glibc's headers make of each call under _FORTIFY_SOURCE. */
#define os(p) __builtin_object_size(p, 0)
#define MEMCPY(d, s, n) __builtin___memcpy_chk(d, s, n, os(d))
#define MEMPCPY(d, s, n) __builtin___mempcpy_chk(d, s, n, os(d))
#define MEMMOVE(d, s, n) __builtin___memmove_chk(d, s, n, os(d))
#define MEMSET(d, c, n) __builtin___memset_chk(d, c, n, os(d))
#define STRCPY(d, s) __builtin___strcpy_chk(d, s, os(d))
#define STPCPY(d, s) __builtin___stpcpy_chk(d, s, os(d))
#define STRNCPY(d, s, n) __builtin___strncpy_chk(d, s, n, os(d))
#define STPNCPY(d, s, n) __builtin___stpncpy_chk(d, s, n, os(d))
#define STRCAT(d, s) __builtin___strcat_chk(d, s, os(d))
#define STRNCAT(d, s, n) __builtin___strncat_chk(d, s, n, os(d))
#define SPRINTF(d, ...) __builtin___sprintf_chk(d, 0, os(d), __VA_ARGS__)
#define SNPRINTF(d, n, ...) __builtin___snprintf_chk(d, n, 0, os(d), __VA_ARGS__)
#define VSPRINTF(d, f, ap) __builtin___vsprintf_chk(d, 0, os(d), f, ap)
#define VSNPRINTF(d, n, f, ap) __builtin___vsnprintf_chk(d, n, 0, os(d), f, ap)

#ifdef __OPTIMIZE__
#define OPTIMIZED 1
#else
#define OPTIMIZED 0
#endif

static char buf[16];
char *volatile vsrc = "abcdef";
volatile size_t vlen = 3;
volatile int which = 1;

static int vs(int unused, ...)
{
    va_list ap;
    int r;
    va_start(ap, unused);
    r = VSPRINTF(buf, "foo", ap);
    va_end(ap);
    return r;
}

static int vsn(int i, ...)
{
    va_list ap;
    int r;
    va_start(ap, i);
    r = VSNPRINTF(buf, 8, "%d", ap);
    va_end(ap);
    return r;
}

/* Every write is known to fit its known destination: no check is left. */
static int fits(void)
{
    char *p;
    size_t n = which ? 8 : 4;

    chk_calls = 0;
    if (MEMCPY(buf, "abcdef", 7) != buf || memcmp(buf, "abcdef", 7)) return 1;
    if (MEMPCPY(buf + 1, "XY", 2) != buf + 3 || memcmp(buf, "aXYdef", 7)) return 2;
    if (MEMMOVE(buf + 8, "12345678", 8) != buf + 8 || memcmp(buf + 8, "12345678", 8)) return 3;
    if (MEMSET(buf, 'z', 16) != buf || buf[15] != 'z') return 4;
    if (MEMSET(buf, 0, n) != buf || buf[7] != 0 || buf[8] != 'z') return 5;
    if (STRCPY(buf, "hello") != buf || memcmp(buf, "hello", 6)) return 6;
    p = which ? "e" : "gh";
    if (STRCPY(buf + 13, p) != buf + 13) return 7;
    if (STPCPY(buf, "abc") != buf + 3 || memcmp(buf, "abc", 4)) return 8;
    if (STRNCPY(buf, "ab", 8) != buf || memcmp(buf, "ab\0\0\0\0\0\0", 8)) return 9;
    if (STPNCPY(buf, "xyz", 8) != buf + 3 || memcmp(buf, "xyz\0\0\0\0\0", 8)) return 10;
    if (STRCAT(buf, "") != buf || memcmp(buf, "xyz", 4)) return 11;
    if (STRNCAT(buf, "abc", 0) != buf || memcmp(buf, "xyz", 4)) return 12;
    if (SPRINTF(buf, "foo") != 3 || memcmp(buf, "foo", 4)) return 13;
    if (SPRINTF(buf, "%s", "bar") != 3 || memcmp(buf, "bar", 4)) return 14;
    if (SNPRINTF(buf, 8, "%d", 42) != 2 || memcmp(buf, "42", 3)) return 15;
    if (SNPRINTF(buf, n, "%d", 123) != 3 || memcmp(buf, "123", 4)) return 16;
    if (vs(0) != 3 || memcmp(buf, "foo", 4)) return 17;
    if (vsn(0, 7) != 1 || memcmp(buf, "7", 2)) return 18;
    if (OPTIMIZED && chk_calls) return 19;
    return 0;
}

/* The destination is a parameter of a function called through a pointer:
   an unknown object, so the plain function is called and nothing is
   checked. */
static int unknown(char *d)
{
    chk_calls = unknown_calls = 0;
    MEMCPY(d, vsrc, vlen);
    if (MEMPCPY(d, vsrc, vlen) != d + 3) return 21;
    MEMMOVE(d, vsrc, vlen);
    MEMSET(d, 'q', vlen);
    STRCPY(d, vsrc);
    if (STPCPY(d, vsrc) != d + 6) return 22;
    STRNCPY(d, vsrc, vlen);
    if (STPNCPY(d, vsrc, vlen) != d + 3) return 23;
    d[3] = 0;
    STRCAT(d, vsrc);
    STRNCAT(d, vsrc, vlen);
    if (memcmp(d, "abcabcdefabc", 13)) return 24;
    SPRINTF(d, "%d", (int)vlen);
    SNPRINTF(d, vlen, "%s", vsrc);
    if (memcmp(d, "ab", 3)) return 25;
    if (OPTIMIZED && (chk_calls || unknown_calls)) return 26;
    return 0;
}

/* A known destination and a length known only at run time: the check
   stays, and passes. */
static int at_run_time(void)
{
    chk_calls = 0;
    MEMCPY(buf, vsrc, vlen);
    STRCPY(buf + 8, vsrc);
    SPRINTF(buf, "%d", (int)vlen);
    if (memcmp(buf, "3\0c", 3) || memcmp(buf + 8, "abcdef", 7)) return 31;
    if (OPTIMIZED && chk_calls != 3) return 32;
    return 0;
}

/* A write that provably overflows keeps its check, which fails. */
static int overflows(void)
{
    int failed = 0;
    failing = 1;
    if (__builtin_setjmp(fail_buf) == 0)
        MEMCPY(&buf[15], "ab", 2);
    else
        failed++;
    if (__builtin_setjmp(fail_buf) == 0)
        STRCPY(&buf[14], "abc");
    else
        failed++;
    if (__builtin_setjmp(fail_buf) == 0)
        SPRINTF(&buf[13], "%s", "abc");
    else
        failed++;
    if (__builtin_setjmp(fail_buf) == 0)
        MEMSET(buf, 0, 17);
    else
        failed++;
    failing = 0;
    return failed == 4 ? 0 : 40 + failed;
}

static int (*volatile unknown_object)(char *) = unknown;

int main(void)
{
    static char other[16];
    int r;
    if ((r = fits()) != 0) return r;
    if ((r = unknown_object(other)) != 0) return r;
    if ((r = at_run_time()) != 0) return r;
    if ((r = overflows()) != 0) return r;
    return 0;
}
"#;
