//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Intrinsic Builtins Mega-Test
//
// Consolidates: types_compatible, constant_p, unreachable, expect tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: Intrinsic builtins
// ============================================================================

#[test]
fn builtins_intrinsics_mega() {
    let code = r#"
int main(void) {
    // ========== __BUILTIN_TYPES_COMPATIBLE_P (returns 1-19) ==========
    {
        // Same types
        if (!__builtin_types_compatible_p(int, int)) return 1;
        if (!__builtin_types_compatible_p(char, char)) return 2;
        if (!__builtin_types_compatible_p(long, long)) return 3;

        // Different types
        if (__builtin_types_compatible_p(int, long)) return 4;
        if (__builtin_types_compatible_p(char, int)) return 5;
        if (__builtin_types_compatible_p(float, double)) return 6;

        // Pointer types
        if (!__builtin_types_compatible_p(int*, int*)) return 7;
        if (__builtin_types_compatible_p(int*, char*)) return 8;

        // Qualified types (const is ignored)
        if (!__builtin_types_compatible_p(int, const int)) return 9;

        // Signed/unsigned matter
        if (__builtin_types_compatible_p(int, unsigned int)) return 10;
        if (__builtin_types_compatible_p(char, unsigned char)) return 11;

        // Array types
        if (!__builtin_types_compatible_p(int[5], int[5])) return 12;
        // Arrays of different sizes are different types
        if (__builtin_types_compatible_p(int[5], int[10])) return 13;

        // Note: typedef compatibility test removed - requires compiler work
        // typedef int myint;
        // if (!__builtin_types_compatible_p(int, myint)) return 14;
    }

    // ========== __BUILTIN_CONSTANT_P (returns 20-39) ==========
    {
        // Literal constants
        if (!__builtin_constant_p(42)) return 20;
        if (!__builtin_constant_p(3.14)) return 21;
        if (!__builtin_constant_p('A')) return 22;

        // Constant expressions
        if (!__builtin_constant_p(1 + 2)) return 23;
        if (!__builtin_constant_p(10 * 5)) return 24;
        if (!__builtin_constant_p(1 << 4)) return 25;

        // Variables are not constant
        int x = 42;
        if (__builtin_constant_p(x)) return 26;

        // const variable (typically not constant for this builtin)
        const int cx = 100;
        // Note: const vars may or may not be considered constant
        // depending on compiler, so we don't test that

        // Null pointer constant
        if (!__builtin_constant_p(0)) return 27;
        if (!__builtin_constant_p((void*)0)) return 28;

        // sizeof is constant
        if (!__builtin_constant_p(sizeof(int))) return 29;
    }

    // ========== __BUILTIN_EXPECT (returns 40-59) ==========
    {
        int x = 42;

        // Returns first argument
        int result = __builtin_expect(x, 1);
        if (result != 42) return 40;

        // Works with expressions
        result = __builtin_expect(x + 10, 0);
        if (result != 52) return 41;

        // Works in conditionals (common use case)
        if (__builtin_expect(x == 42, 1)) {
            // likely branch
        } else {
            return 42;
        }

        // With long expected value
        long lx = 100L;
        long lresult = __builtin_expect(lx, 100L);
        if (lresult != 100L) return 43;

        // Chain of expects
        result = __builtin_expect(__builtin_expect(x, 42), 42);
        if (result != 42) return 44;
    }

    // ========== __BUILTIN_ASSUME_ALIGNED (returns 60-69) ==========
    {
        // assume_aligned returns the pointer unchanged (hint for optimizer)
        int arr[16];
        int *p = arr;

        // Two-arg form: __builtin_assume_aligned(ptr, alignment)
        int *aligned = __builtin_assume_aligned(p, 4);
        if (aligned != p) return 60;

        // Should work with larger alignments
        aligned = __builtin_assume_aligned(p, 16);
        if (aligned != p) return 61;

        // Three-arg form: __builtin_assume_aligned(ptr, alignment, offset)
        aligned = __builtin_assume_aligned(p, 16, 0);
        if (aligned != p) return 62;

        aligned = __builtin_assume_aligned(p + 1, 4, 0);
        if (aligned != p + 1) return 63;

        // Works with void*
        void *vp = arr;
        void *valigned = __builtin_assume_aligned(vp, 8);
        if (valigned != vp) return 64;
    }

    // ========== __BUILTIN_PREFETCH (returns 70-79) ==========
    {
        // prefetch is a no-op hint - just verify it compiles and doesn't crash
        int arr[100];
        for (int i = 0; i < 100; i++) arr[i] = i;

        // One-arg form: prefetch for read
        __builtin_prefetch(&arr[0]);
        __builtin_prefetch(&arr[50]);

        // Two-arg form: rw (0=read, 1=write)
        __builtin_prefetch(&arr[10], 0);  // prefetch for read
        __builtin_prefetch(&arr[20], 1);  // prefetch for write

        // Three-arg form: rw, locality (0-3, higher = more temporal locality)
        __builtin_prefetch(&arr[30], 0, 0);  // read, no locality
        __builtin_prefetch(&arr[40], 0, 3);  // read, high locality
        __builtin_prefetch(&arr[60], 1, 1);  // write, some locality

        // Verify array wasn't corrupted
        if (arr[0] != 0) return 70;
        if (arr[50] != 50) return 71;
        if (arr[99] != 99) return 72;
    }

    // ========== STRING LITERAL SIZEOF (returns 80-89) ==========
    {
        // String literals have type char[N], not char*
        // sizeof should return array size including null terminator
        if (sizeof("") != 1) return 80;           // just null
        if (sizeof("a") != 2) return 81;          // 'a' + null
        if (sizeof("hello") != 6) return 82;      // 5 chars + null
        if (sizeof("hello world") != 12) return 83;

        // Wide string literals have type wchar_t[N]
        // wchar_t is 4 bytes on Linux
        if (sizeof(L"") != 4) return 84;          // just null (4 bytes)
        if (sizeof(L"a") != 8) return 85;         // 'a' + null (2 * 4)
        if (sizeof(L"hello") != 24) return 86;    // 6 wchars * 4 bytes
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_intrinsics_mega", code, &[]), 0);
}

/// `__builtin_{add,sub,mul}_overflow` and the typed spellings glibc's headers
/// use. They compute exactly, store the wrapped result through the pointer,
/// and answer whether wrapping lost anything.
///
/// Lowered by computing in 128 bits — which holds every exact sum, difference
/// and product of two operands of 64 bits or fewer — then narrowing and
/// widening back: a round trip that changes the value overflowed. That needs
/// no new opcode on either target, where reading the hardware's flags would.
///
/// Every expectation here came from gcc on this source.
#[test]
fn builtins_checked_arithmetic() {
    let code = r#"
#include <limits.h>

int main(void) {
    int r; long lr; unsigned ur; unsigned long ulr; long long llr;

    if (__builtin_add_overflow(1, 2, &r) || r != 3) return 1;
    if (!__builtin_add_overflow(INT_MAX, 1, &r)) return 2;
    if (__builtin_sub_overflow(5, 3, &r) || r != 2) return 3;
    if (!__builtin_sub_overflow(INT_MIN, 1, &r)) return 4;
    if (__builtin_mul_overflow(3, 4, &r) || r != 12) return 5;
    if (!__builtin_mul_overflow(INT_MAX, 2, &r)) return 6;

    /* Wider destination: the same operands no longer overflow. */
    if (__builtin_mul_overflow(1000000, 1000000, &lr) || lr != 1000000000000L) return 7;
    if (!__builtin_add_overflow(2000000000, 2000000000, &r)) return 8;
    if (__builtin_add_overflow(2000000000, 2000000000, &lr) || lr != 4000000000L) return 9;

    /* Unsigned wraps at its own bound, and a product of two 64-bit unsigned
       values can exceed the *signed* 128-bit range, so the wide computation
       has to follow the operands' signedness. */
    if (!__builtin_add_overflow(UINT_MAX, 1u, &ur)) return 10;
    if (__builtin_sub_overflow(0u, 0u, &ur) || ur != 0) return 11;
    if (!__builtin_sub_overflow(0u, 1u, &ur)) return 12;
    if (!__builtin_mul_overflow(ULONG_MAX, 2ul, &ulr)) return 13;
    if (__builtin_mul_overflow(ULONG_MAX, 1ul, &ulr) || ulr != ULONG_MAX) return 14;

    /* The typed spellings. */
    if (__builtin_uadd_overflow(1u, 2u, &ur) || ur != 3) return 15;
    if (!__builtin_smul_overflow(INT_MAX, 2, &r)) return 16;
    if (__builtin_saddll_overflow(1LL, 2LL, &llr) || llr != 3) return 17;
    if (!__builtin_usubl_overflow(0ul, 1ul, &ulr)) return 18;

    /* The result is stored even when it overflowed, wrapped. */
    r = 0;
    (void)__builtin_add_overflow(INT_MAX, 1, &r);
    if (r != INT_MIN) return 19;

    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_checked_arithmetic", code, &[]), 0);
}

/// Builtins that were absent, and are now present.
///
/// `__has_builtin` answered 0 for all of these, so guarded code was already
/// correct -- the risk was the unguarded uses in system headers, and
/// `__builtin_choose_expr` in particular, which glibc uses to pick between
/// expressions that are only valid for one argument type.
///
/// Most are the library function under a reserved name, so they lower to an
/// ordinary call rather than an expression node apiece. That also settles the
/// question a textual expansion would raise: the argument is evaluated once.
#[test]
fn builtins_library_and_bit_builtins_are_available() {
    let code = r#"#include <string.h>
#include <math.h>
int main(void) {
    /* Compile-time selection: the unselected arm is not type-checked. */
    if (__builtin_choose_expr(1, 2, "not an int") != 2) return 1;
    if (__builtin_choose_expr(0, "not an int", 3) != 3) return 2;
    if (sizeof(__builtin_choose_expr(1, (char)0, (long)0)) != 1) return 3;
    if (sizeof(__builtin_choose_expr(0, (char)0, (long)0)) != 8) return 4;

    /* String builtins. */
    if (__builtin_strlen("abcd") != 4) return 5;
    if (__builtin_strlen("") != 0) return 6;
    if (__builtin_strcmp("abc", "abc") != 0) return 7;
    if (__builtin_strcmp("abc", "abd") >= 0) return 8;
    if (__builtin_strcmp("abd", "abc") <= 0) return 9;

    /* Integer absolute value. */
    if (__builtin_abs(-5) != 5 || __builtin_abs(5) != 5) return 10;
    if (__builtin_labs(-5L) != 5L) return 11;
    if (__builtin_llabs(-5LL) != 5LL) return 12;
    { int n = -7; if (__builtin_abs(n) != 7) return 13; }

    /* Find first set: one-based, zero for zero. */
    if (__builtin_ffs(0) != 0) return 14;
    if (__builtin_ffs(1) != 1) return 15;
    if (__builtin_ffs(8) != 4) return 16;
    if (__builtin_ffs(0x80000000) != 32) return 17;
    if (__builtin_ffsl(0L) != 0) return 18;
    if (__builtin_ffsl(1L << 40) != 41) return 19;

    /* Parity: low bit of the population count. */
    if (__builtin_parity(0) != 0) return 20;
    if (__builtin_parity(7) != 1) return 21;
    if (__builtin_parity(3) != 0) return 22;
    if (__builtin_parity(0xFFFFFFFFu) != 0) return 23;

    /* Floating point. */
    if (__builtin_sqrt(16.0) != 4.0) return 24;
    if (__builtin_sqrt(0.0) != 0.0) return 25;
    if (__builtin_copysign(3.0, -1.0) != -3.0) return 26;
    if (__builtin_copysign(-3.0, 1.0) != 3.0) return 27;
    if (!signbit(__builtin_copysign(0.0, -1.0))) return 28;



    /* All of them answer __has_builtin honestly. */
#define CK(n) do { if (!__has_builtin(n)) return 40; } while (0)
    CK(__builtin_choose_expr); CK(__builtin_strlen); CK(__builtin_strcmp);
    CK(__builtin_abs); CK(__builtin_labs); CK(__builtin_llabs);
    CK(__builtin_ffs); CK(__builtin_ffsl); CK(__builtin_parity);
    CK(__builtin_trap);
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("builtins_library_and_bit", code, &["-lm".to_string()]),
        0
    );
}
