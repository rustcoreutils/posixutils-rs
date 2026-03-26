//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 Standard Library Headers + Predefined Macros Mega-Test
//
// Proves all C89/C99 standard headers compile and key functions work.
// Headers come from system libc (glibc/macOS); our compiler just parses them.
//

use crate::common::compile_and_run;

#[test]
fn c99_stdlib_headers_mega() {
    let code = r#"
/* Include ALL C89 headers */
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* Include ALL C99 new headers */
#include <complex.h>
#include <fenv.h>
#include <inttypes.h>
#include <iso646.h>
#include <stdbool.h>
#include <stdint.h>
#include <wchar.h>
#include <wctype.h>

int main(void) {
    // ========== C89 HEADERS (returns 1-29) ==========

    // <ctype.h>
    if (!isalpha('A')) return 1;
    if (!isdigit('5')) return 2;
    if (toupper('a') != 'A') return 3;

    // <errno.h>
    errno = 0;
    if (errno != 0) return 4;

    // <float.h>
    if (FLT_MAX < 1e37) return 5;
    if (DBL_MAX < 1e307) return 6;

    // <limits.h>
    if (CHAR_BIT != 8) return 7;
    if (INT_MAX < 2147483647) return 8;

    // <math.h>
    if (sqrt(4.0) < 1.99 || sqrt(4.0) > 2.01) return 9;
    if (fabs(-3.0) < 2.99 || fabs(-3.0) > 3.01) return 10;

    // <setjmp.h>
    jmp_buf env;
    int val = setjmp(env);
    if (val == 0) {
        // first return
    } else {
        return 11;  // should not get here in this test
    }

    // <signal.h>
    // Just verify SIG_DFL exists (don't actually install handlers)
    void (*handler)(int) = SIG_DFL;
    (void)handler;

    // <stdarg.h> — already tested in other mega-tests

    // <stddef.h>
    if (sizeof(size_t) < 4) return 12;
    if (sizeof(ptrdiff_t) < 4) return 13;

    // <stdio.h>
    char buf[64];
    snprintf(buf, sizeof(buf), "%d", 42);
    if (strcmp(buf, "42") != 0) return 14;

    // <stdlib.h>
    if (abs(-42) != 42) return 15;
    if (atoi("123") != 123) return 16;

    // <string.h>
    char s[16];
    memset(s, 0, sizeof(s));
    strcpy(s, "test");
    if (strlen(s) != 4) return 17;
    if (memcmp(s, "test", 4) != 0) return 18;

    // <time.h>
    time_t t = time(NULL);
    if (t == (time_t)-1) return 19;

    // ========== C99 HEADERS (returns 30-49) ==========

    // <stdbool.h>
    bool b = true;
    if (!b) return 30;
    if (false) return 31;

    // <stdint.h>
    int8_t i8 = INT8_MAX;
    uint8_t u8 = UINT8_MAX;
    int32_t i32 = INT32_MAX;
    uint64_t u64 = UINT64_MAX;
    intptr_t ip = (intptr_t)&i8;
    if (i8 != 127) return 32;
    if (u8 != 255) return 33;
    if (i32 != 2147483647) return 34;
    if (ip == 0) return 35;
    (void)u64;

    // <inttypes.h>
    char ibuf[64];
    snprintf(ibuf, sizeof(ibuf), "%" PRId32, (int32_t)42);
    if (strcmp(ibuf, "42") != 0) return 36;
    snprintf(ibuf, sizeof(ibuf), "%" PRIx64, (uint64_t)255);
    if (strcmp(ibuf, "ff") != 0) return 37;

    // <iso646.h>
    int a = 1, c = 0;
    if (a and not c) { /* pass: 1 && !0 = true */ }
    else return 38;
    int d = 0xF0;
    d or_eq 0x0F;  // d |= 0x0F
    if (d != 0xFF) return 39;

    // <fenv.h>
    int round_mode = fegetround();
    if (round_mode != FE_TONEAREST) return 40;

    // <wchar.h>
    wchar_t wc = L'A';
    if (wc != 65) return 41;

    // <wctype.h>
    if (!iswalpha(L'A')) return 42;

    // ========== PREDEFINED MACROS (returns 50-59) ==========
    {
        // __DATE__ — "Mmm dd yyyy"
        const char *date = __DATE__;
        if (strlen(date) != 11) return 50;

        // __TIME__ — "hh:mm:ss"
        const char *tm = __TIME__;
        if (strlen(tm) != 8) return 51;

        // __FILE__
        const char *file = __FILE__;
        if (strlen(file) == 0) return 52;

        // __LINE__
        int line = __LINE__;
        if (line <= 0) return 53;

        // __STDC__ — 1 for conforming
        if (__STDC__ != 1) return 54;

        // __STDC_VERSION__ — at least 199901L (C99)
        if (__STDC_VERSION__ < 199901L) return 55;

        // __STDC_HOSTED__ — 1 for hosted
        if (__STDC_HOSTED__ != 1) return 56;

        // __func__
        if (strcmp(__func__, "main") != 0) return 57;
    }

    // ========== MATH.H C99 ADDITIONS (returns 60-69) ==========
    {
        // Classification macros
        if (!isfinite(1.0)) return 60;
        if (!isinf(INFINITY)) return 61;
        if (!isnan(NAN)) return 62;

        // C99 math functions
        if (round(2.5) < 1.9 || round(2.5) > 3.1) return 63;
        if (fmax(3.0, 5.0) < 4.9 || fmax(3.0, 5.0) > 5.1) return 64;
        if (fmin(3.0, 5.0) < 2.9 || fmin(3.0, 5.0) > 3.1) return 65;
        if (log2(8.0) < 2.9 || log2(8.0) > 3.1) return 66;
        if (exp2(3.0) < 7.9 || exp2(3.0) > 8.1) return 67;
        if (cbrt(27.0) < 2.9 || cbrt(27.0) > 3.1) return 68;

        // HUGE_VALF, INFINITY, NAN
        if (!isinf(HUGE_VALF)) return 69;
    }

    // ========== STDIO.H C99 ADDITIONS (returns 70-79) ==========
    {
        char buf[64];

        // snprintf
        int n = snprintf(buf, 5, "hello world");
        if (n != 11) return 70;  // returns total that would be written
        if (strcmp(buf, "hell") != 0) return 71;

        // %lld
        snprintf(buf, sizeof(buf), "%lld", (long long)1234567890123LL);
        if (strcmp(buf, "1234567890123") != 0) return 72;

        // %zu
        snprintf(buf, sizeof(buf), "%zu", (size_t)42);
        if (strcmp(buf, "42") != 0) return 73;
    }

    // ========== STDLIB.H C99 ADDITIONS (returns 80-89) ==========
    {
        // strtoll
        long long ll = strtoll("9876543210", NULL, 10);
        if (ll != 9876543210LL) return 80;

        // llabs
        if (llabs(-100LL) != 100LL) return 81;

        // atoll
        if (atoll("5000000000") != 5000000000LL) return 82;
    }

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_stdlib_headers_mega", code, &["-lm".to_string()]),
        0,
    );
}
