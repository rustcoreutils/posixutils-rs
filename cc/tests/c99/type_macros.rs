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
