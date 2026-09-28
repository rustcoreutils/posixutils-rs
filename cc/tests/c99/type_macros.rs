//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The predefined macros that describe the target's integer types, checked
// against the types themselves (C17 7.20.2p2, 7.20.4p2) and against the
// values gcc predefines for the same target.
//

use crate::common::{compile_and_run, compile_and_run_aarch64, preprocess_text};

/// The `#define` lines `c17 -dM -E` prints for `triple`.
fn predefines(triple: &str) -> String {
    let r = preprocess_text("type_macros_dm", "", &["-dM", "--target", triple]);
    assert!(r.success, "-dM for {triple} failed: {}", r.stderr);
    r.stdout
}

fn assert_defines(triple: &str, want: &[&str]) {
    let out = predefines(triple);
    for line in want {
        assert!(
            out.lines().any(|l| l.trim_end() == *line),
            "{triple}: expected `{line}` in:\n{out}"
        );
    }
}

/// Each `<stdint.h>` limit has to be a constant of the promoted type of the
/// typedef it bounds, with the typedef's own extremes, and each `INTn_C`
/// constant has to come out that type too. The limits were typed out one
/// macro at a time, and `INT64_MAX` was `long long` where `int64_t` is `long`.
const LIMITS_AGREE_WITH_TYPES: &str = r#"
#include <stdint.h>
#include <stddef.h>

/* The promoted type of T: `+ 0` performs the promotions, and for an integer
   type no narrower than `int` the conversions leave it alone. */
#define PROMOTED(T) __typeof__((T)0 + 0)
#define IS(E, T) _Generic((E), PROMOTED(T): 1, default: 0)
#define SMAX(T) ((T)((1ULL << (sizeof(T) * 8 - 1)) - 1))

#define CHECK_S(n, T, MAX, MIN)                                          \
    do {                                                                 \
        if (!IS(MAX, T)) return n;                                       \
        if (MAX != SMAX(T)) return n + 1;                                \
        if (!IS(MIN, T)) return n + 2;                                   \
        if (MIN != -MAX - 1) return n + 3;                               \
    } while (0)
#define CHECK_U(n, T, MAX)                                               \
    do {                                                                 \
        if (!IS(MAX, T)) return n;                                       \
        if (MAX != (T)-1) return n + 1;                                  \
    } while (0)

int main(void) {
    CHECK_S(10, int8_t, INT8_MAX, INT8_MIN);
    CHECK_S(14, int16_t, INT16_MAX, INT16_MIN);
    CHECK_S(18, int32_t, INT32_MAX, INT32_MIN);
    CHECK_S(22, int64_t, INT64_MAX, INT64_MIN);
    CHECK_U(26, uint8_t, UINT8_MAX);
    CHECK_U(28, uint16_t, UINT16_MAX);
    CHECK_U(30, uint32_t, UINT32_MAX);
    CHECK_U(32, uint64_t, UINT64_MAX);

    CHECK_S(34, int_least8_t, INT_LEAST8_MAX, INT_LEAST8_MIN);
    CHECK_S(38, int_least16_t, INT_LEAST16_MAX, INT_LEAST16_MIN);
    CHECK_S(42, int_least32_t, INT_LEAST32_MAX, INT_LEAST32_MIN);
    CHECK_S(46, int_least64_t, INT_LEAST64_MAX, INT_LEAST64_MIN);
    CHECK_U(50, uint_least8_t, UINT_LEAST8_MAX);
    CHECK_U(52, uint_least16_t, UINT_LEAST16_MAX);
    CHECK_U(54, uint_least32_t, UINT_LEAST32_MAX);
    CHECK_U(56, uint_least64_t, UINT_LEAST64_MAX);

    CHECK_S(58, int_fast8_t, INT_FAST8_MAX, INT_FAST8_MIN);
    CHECK_S(62, int_fast16_t, INT_FAST16_MAX, INT_FAST16_MIN);
    CHECK_S(66, int_fast32_t, INT_FAST32_MAX, INT_FAST32_MIN);
    CHECK_S(70, int_fast64_t, INT_FAST64_MAX, INT_FAST64_MIN);
    CHECK_U(74, uint_fast8_t, UINT_FAST8_MAX);
    CHECK_U(76, uint_fast16_t, UINT_FAST16_MAX);
    CHECK_U(78, uint_fast32_t, UINT_FAST32_MAX);
    CHECK_U(80, uint_fast64_t, UINT_FAST64_MAX);

    CHECK_S(82, intptr_t, INTPTR_MAX, INTPTR_MIN);
    CHECK_U(86, uintptr_t, UINTPTR_MAX);
    CHECK_S(88, intmax_t, INTMAX_MAX, INTMAX_MIN);
    CHECK_U(92, uintmax_t, UINTMAX_MAX);
    CHECK_S(94, ptrdiff_t, PTRDIFF_MAX, PTRDIFF_MIN);
    CHECK_U(98, size_t, SIZE_MAX);

    /* 7.20.4p2: INTN_C(value) has the promoted type of int_leastN_t. */
    if (!IS(INT8_C(1), int_least8_t)) return 100;
    if (!IS(INT16_C(1), int_least16_t)) return 101;
    if (!IS(INT32_C(1), int_least32_t)) return 102;
    if (!IS(INT64_C(1), int_least64_t)) return 103;
    if (!IS(UINT8_C(1), uint_least8_t)) return 104;
    if (!IS(UINT16_C(1), uint_least16_t)) return 105;
    if (!IS(UINT32_C(1), uint_least32_t)) return 106;
    if (!IS(UINT64_C(1), uint_least64_t)) return 107;
    if (!IS(INTMAX_C(1), intmax_t)) return 108;
    if (!IS(UINTMAX_C(1), uintmax_t)) return 109;

    /* sig_atomic_t: a signed type on every target here. */
    if (!IS(SIG_ATOMIC_MAX, __SIG_ATOMIC_TYPE__)) return 110;
    if (SIG_ATOMIC_MIN != -SIG_ATOMIC_MAX - 1) return 111;
    if (SIG_ATOMIC_MIN != __SIG_ATOMIC_MIN__) return 112;

    /* The printf length modifiers match the type they print. (clang's
       macros; gcc has none.) */
#ifdef __INT64_FMTd__
    if (_Generic((int64_t)0, long: 1, default: 0)) {
        if (__builtin_strcmp(__INT64_FMTd__, "ld") != 0) return 113;
    } else {
        if (__builtin_strcmp(__INT64_FMTd__, "lld") != 0) return 114;
    }
#endif
    return 0;
}
"#;

#[test]
fn integer_limits_agree_with_their_types() {
    assert_eq!(
        compile_and_run("limits_agree_with_types", LIMITS_AGREE_WITH_TYPES, &[]),
        0
    );
}

#[test]
fn integer_limits_agree_with_their_types_aarch64() {
    if let Some(rc) = compile_and_run_aarch64(
        "limits_agree_with_types_a64",
        LIMITS_AGREE_WITH_TYPES,
        "-O0",
    ) {
        assert_eq!(rc, 0);
    }
}

/// The integer predefines match gcc's for the same Linux target, and Darwin's
/// `long long` `int64_t` beside its `long` `intmax_t`.
#[test]
fn integer_predefines_match_the_platform() {
    for triple in ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"] {
        assert_defines(
            triple,
            &[
                "#define __INT64_TYPE__ long int",
                "#define __INT64_MAX__ 0x7fffffffffffffffL",
                "#define __UINT64_MAX__ 0xffffffffffffffffUL",
                "#define __INT_LEAST64_MAX__ 0x7fffffffffffffffL",
                "#define __INT64_FMTd__ \"ld\"",
                "#define __UINT64_FMTu__ \"lu\"",
                "#define __INT16_TYPE__ short int",
                "#define __UINT16_MAX__ 0xffff",
                "#define __UINT32_MAX__ 0xffffffffU",
                "#define __LONG_LONG_MAX__ 0x7fffffffffffffffLL",
                "#define __LONG_LONG_WIDTH__ 64",
                "#define __SIG_ATOMIC_MIN__ (-__SIG_ATOMIC_MAX__ - 1)",
                "#define __SIZE_MAX__ 0xffffffffffffffffUL",
            ],
        );
    }
    assert_defines(
        "aarch64-apple-darwin",
        &[
            "#define __INT64_TYPE__ long long int",
            "#define __INT64_MAX__ 0x7fffffffffffffffLL",
            "#define __INT64_FMTd__ \"lld\"",
            "#define __INTMAX_TYPE__ long int",
            "#define __INTMAX_MAX__ 0x7fffffffffffffffL",
            "#define __INTMAX_FMTd__ \"ld\"",
        ],
    );
}

/// `int_fastN_t` is the C library's choice, and glibc makes the 16- and
/// 32-bit ones `long` on a 64-bit target. c17 said `short` and `int`, so the
/// same typedef was 2 bytes in c17 and 8 in gcc, and INT_FAST16_MAX was 32767.
const FAST_TYPES_ARE_GLIBCS: &str = r#"
#include <stdint.h>
int main(void) {
    if (!_Generic((int_fast8_t)0, signed char: 1, default: 0)) return 1;
    if (!_Generic((int_fast16_t)0, long: 1, default: 0)) return 2;
    if (!_Generic((int_fast32_t)0, long: 1, default: 0)) return 3;
    if (!_Generic((int_fast64_t)0, long: 1, default: 0)) return 4;
    if (!_Generic((uint_fast8_t)0, unsigned char: 1, default: 0)) return 5;
    if (!_Generic((uint_fast16_t)0, unsigned long: 1, default: 0)) return 6;
    if (!_Generic((uint_fast32_t)0, unsigned long: 1, default: 0)) return 7;
    if (!_Generic((uint_fast64_t)0, unsigned long: 1, default: 0)) return 8;
    if (INT_FAST16_MAX != 0x7fffffffffffffffL) return 9;
    if (UINT_FAST32_MAX != 0xffffffffffffffffUL) return 10;
    if (INT_FAST32_MIN != -0x7fffffffffffffffL - 1) return 11;
    if (sizeof(int_fast16_t) != 8) return 12;
    return 0;
}
"#;

#[cfg(target_os = "linux")]
#[test]
fn int_fast_types_are_glibcs() {
    assert_eq!(
        compile_and_run("fast_types_glibc", FAST_TYPES_ARE_GLIBCS, &[]),
        0
    );
}

#[test]
fn int_fast_types_are_glibcs_aarch64() {
    if let Some(rc) = compile_and_run_aarch64("fast_types_glibc_a64", FAST_TYPES_ARE_GLIBCS, "-O0")
    {
        assert_eq!(rc, 0);
    }
}

/// The same facts as the predefines state them, against gcc's for Linux, and
/// Darwin's exact-width choice.
#[test]
fn int_fast_predefines_match_the_platform() {
    for triple in ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"] {
        assert_defines(
            triple,
            &[
                "#define __INT_FAST8_TYPE__ signed char",
                "#define __INT_FAST16_TYPE__ long int",
                "#define __INT_FAST16_MAX__ 0x7fffffffffffffffL",
                "#define __INT_FAST16_WIDTH__ 64",
                "#define __INT_FAST32_TYPE__ long int",
                "#define __UINT_FAST16_TYPE__ long unsigned int",
                "#define __UINT_FAST32_MAX__ 0xffffffffffffffffUL",
            ],
        );
    }
    assert_defines(
        "aarch64-apple-darwin",
        &[
            "#define __INT_FAST16_TYPE__ short int",
            "#define __INT_FAST32_TYPE__ int",
            "#define __INT_FAST64_TYPE__ long long int",
        ],
    );
}

/// `wchar_t` is `unsigned int` under AAPCS64, which Linux follows, and `int`
/// on x86-64 and Apple arm64. c17 made it `int` everywhere -- the predefine,
/// the type of `L'x'` and of `L"..."`'s elements alike -- so on aarch64 Linux
/// `(wchar_t)-1 > 0` was false where gcc says true, and WCHAR_MIN was
/// negative.
const WCHAR_FOLLOWS_THE_ABI: &str = r#"
#include <stddef.h>
#include <stdint.h>

#if defined(__aarch64__) && !defined(__APPLE__)
#define WCHAR_UNSIGNED 1
#else
#define WCHAR_UNSIGNED 0
#endif

/* glibc's <bits/wchar.h> asks the preprocessor this way. */
#if L'\0' - 1 > 0
#define PP_WCHAR_UNSIGNED 1
#else
#define PP_WCHAR_UNSIGNED 0
#endif

int main(void) {
    if (!_Generic(L'a', wchar_t: 1, default: 0)) return 1;
    if (!_Generic(L"a"[0], wchar_t: 1, default: 0)) return 2;
    if (!_Generic(L'a', __WCHAR_TYPE__: 1, default: 0)) return 3;
    if (((wchar_t)-1 > 0) != WCHAR_UNSIGNED) return 4;
    if ((L'A' - L'B' > 0) != WCHAR_UNSIGNED) return 5;
    if (PP_WCHAR_UNSIGNED != WCHAR_UNSIGNED) return 6;
    if (WCHAR_UNSIGNED) {
        if (WCHAR_MIN != 0) return 7;
        if (WCHAR_MAX != 0xffffffffU) return 8;
    } else {
        if (WCHAR_MIN != -2147483647 - 1) return 9;
        if (WCHAR_MAX != 2147483647) return 10;
    }
    if (!_Generic(WCHAR_MAX, __typeof__((wchar_t)0 + 0): 1, default: 0)) return 11;
    if (sizeof(wchar_t) != __SIZEOF_WCHAR_T__) return 12;

    /* A wide literal initializes an array of its own element type. */
    wchar_t w[] = L"hi";
    if (sizeof(w) != 3 * sizeof(wchar_t) || w[1] != L'i') return 13;
    return 0;
}
"#;

#[test]
fn wchar_follows_the_abi() {
    assert_eq!(
        compile_and_run("wchar_follows_abi", WCHAR_FOLLOWS_THE_ABI, &[]),
        0
    );
}

#[test]
fn wchar_follows_the_abi_aarch64() {
    if let Some(rc) = compile_and_run_aarch64("wchar_follows_abi_a64", WCHAR_FOLLOWS_THE_ABI, "-O0")
    {
        assert_eq!(rc, 0);
    }
}

/// The predefines agree with gcc's for both Linux targets.
#[test]
fn wchar_predefines_match_the_platform() {
    assert_defines(
        "x86_64-unknown-linux-gnu",
        &[
            "#define __WCHAR_TYPE__ int",
            "#define __WCHAR_MAX__ 0x7fffffff",
            "#define __WCHAR_MIN__ (-__WCHAR_MAX__ - 1)",
        ],
    );
    assert_defines(
        "aarch64-unknown-linux-gnu",
        &[
            "#define __WCHAR_TYPE__ unsigned int",
            "#define __WCHAR_MAX__ 0xffffffffU",
            "#define __WCHAR_MIN__ 0U",
        ],
    );
    assert_defines(
        "aarch64-apple-darwin",
        &[
            "#define __WCHAR_TYPE__ int",
            "#define __WCHAR_MIN__ (-__WCHAR_MAX__ - 1)",
        ],
    );
}

/// Every `ATOMIC_*_LOCK_FREE` of `<stdatomic.h>` (C17 7.17.1p2) expands to a
/// constant, in code and in `#if`. `ATOMIC_CHAR16_T_LOCK_FREE`,
/// `ATOMIC_CHAR32_T_LOCK_FREE` and `ATOMIC_WCHAR_T_LOCK_FREE` named
/// `__GCC_ATOMIC_*` predefines c17 did not have, so each was an undeclared
/// identifier -- and silently 0 in `#if`.
const ATOMIC_LOCK_FREE_MACROS: &str = r#"
#include <stdatomic.h>
#include <stddef.h>

#if ATOMIC_CHAR16_T_LOCK_FREE != 2 || ATOMIC_CHAR32_T_LOCK_FREE != 2 \
    || ATOMIC_WCHAR_T_LOCK_FREE != 2
#error prefixed character types are not lock-free
#endif

int main(void) {
    int all[] = {
        ATOMIC_BOOL_LOCK_FREE, ATOMIC_CHAR_LOCK_FREE,
        ATOMIC_CHAR16_T_LOCK_FREE, ATOMIC_CHAR32_T_LOCK_FREE,
        ATOMIC_WCHAR_T_LOCK_FREE, ATOMIC_SHORT_LOCK_FREE,
        ATOMIC_INT_LOCK_FREE, ATOMIC_LONG_LOCK_FREE,
        ATOMIC_LLONG_LOCK_FREE, ATOMIC_POINTER_LOCK_FREE,
    };
    for (unsigned i = 0; i < sizeof all / sizeof all[0]; i++)
        if (all[i] != 2) return 1 + i;
    if (!__atomic_always_lock_free(sizeof(wchar_t), 0)) return 20;
    if (__atomic_always_lock_free(16, 0)) return 21;
    if (__GCC_ATOMIC_TEST_AND_SET_TRUEVAL != 1) return 22;
    atomic_flag f = ATOMIC_FLAG_INIT;
    if (atomic_flag_test_and_set(&f)) return 23;
    if (*(unsigned char *)&f != __GCC_ATOMIC_TEST_AND_SET_TRUEVAL) return 24;
    return 0;
}
"#;

#[test]
fn atomic_lock_free_macros_are_defined() {
    assert_eq!(
        compile_and_run("atomic_lock_free_macros", ATOMIC_LOCK_FREE_MACROS, &[]),
        0
    );
}

#[test]
fn atomic_lock_free_macros_are_defined_aarch64() {
    if let Some(rc) = compile_and_run_aarch64(
        "atomic_lock_free_macros_a64",
        ATOMIC_LOCK_FREE_MACROS,
        "-O0",
    ) {
        assert_eq!(rc, 0);
    }
}

/// Representation facts gcc predefines and c17 did not: the evaluation
/// method <float.h> and glibc's `float_t` read, the floating word order, and
/// the `__INTN_C(c)` constant macros.
const REPRESENTATION_MACROS: &str = r#"
#include <float.h>
#include <stdint.h>

#ifndef __FLT_EVAL_METHOD__
#error no __FLT_EVAL_METHOD__
#endif
#if __FLT_EVAL_METHOD__ != 0 || FLT_EVAL_METHOD != __FLT_EVAL_METHOD__
#error floating operations are evaluated in their own type
#endif
#if !defined(__FLOAT_WORD_ORDER__) || __FLOAT_WORD_ORDER__ != __BYTE_ORDER__
#error the float word order follows the byte order on these targets
#endif

int main(void) {
    if (!_Generic(__INT8_C(1), int: 1, default: 0)) return 1;
    if (!_Generic(__UINT16_C(1), int: 1, default: 0)) return 2;
    if (!_Generic(__UINT32_C(1), unsigned int: 1, default: 0)) return 3;
    if (!_Generic(__INT64_C(1), int64_t: 1, default: 0)) return 4;
    if (!_Generic(__UINT64_C(1), uint64_t: 1, default: 0)) return 5;
    if (!_Generic(__INTMAX_C(1), intmax_t: 1, default: 0)) return 6;
    if (!_Generic(__UINTMAX_C(1), uintmax_t: 1, default: 0)) return 7;
    if (__INT64_C(0x7fffffffffffffff) != INT64_MAX) return 8;
    if (!_Generic(INT64_C(1), int64_t: 1, default: 0)) return 9;
    if (!_Generic(UINTMAX_C(1), uintmax_t: 1, default: 0)) return 10;

    /* The low word of a double comes first in memory. */
    double d = 1.0;
    unsigned int w[2];
    __builtin_memcpy(w, &d, sizeof d);
    if (w[0] != 0 || w[1] != 0x3ff00000) return 11;
    return 0;
}
"#;

#[test]
fn representation_macros_are_defined() {
    assert_eq!(
        compile_and_run("representation_macros", REPRESENTATION_MACROS, &[]),
        0
    );
}

#[test]
fn representation_macros_are_defined_aarch64() {
    if let Some(rc) =
        compile_and_run_aarch64("representation_macros_a64", REPRESENTATION_MACROS, "-O0")
    {
        assert_eq!(rc, 0);
    }
}

/// The `-dM` spelling matches gcc's, parameter name and `##` included.
#[test]
fn constant_fn_macros_match_gcc() {
    for triple in ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"] {
        assert_defines(
            triple,
            &[
                "#define __INT8_C(c) c",
                "#define __UINT32_C(c) c ## U",
                "#define __INT64_C(c) c ## L",
                "#define __UINT64_C(c) c ## UL",
                "#define __INTMAX_C(c) c ## L",
                "#define __FLOAT_WORD_ORDER__ __ORDER_LITTLE_ENDIAN__",
                "#define __FLT_EVAL_METHOD__ 0",
            ],
        );
    }
    assert_defines("aarch64-apple-darwin", &["#define __INT64_C(c) c ## LL"]);
}
