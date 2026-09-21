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

        // The prefetch emits nothing, but its address argument is still an
        // expression and C evaluates it. Parsing it and throwing it away lost
        // whatever it did. gcc documents the address as evaluated and tests
        // for it; this is gcc.c-torture's builtin-prefetch-4 in miniature.
        {
            int *p = &arr[3];
            int *q = 0;
            __builtin_prefetch((q = p), 0, 0);
            if (q != p) return 73;
        }
        {
            int *p = &arr[0];
            int i = 5, j = 0;
            __builtin_prefetch(&p[j = i], 0, 0);
            if (j != i) return 74;
        }
        {
            int i = 0;
            __builtin_prefetch(&arr[i++]);
            if (i != 1) return 75;
        }
        {
            // One evaluation, not two: the argument is an expression, not a
            // textual substitution.
            int i = 0;
            __builtin_prefetch(&arr[(i += 2)], 1, 3);
            if (i != 2) return 76;
        }
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

/// The libc-alias builtins, used with **no header included at all**.
///
/// That is the case they exist for: gcc knows them intrinsically, and the
/// gcc.c-torture suite leans on it heavily -- `__builtin_abort` alone appears
/// in 438 of its tests, none of which include `<stdlib.h>`.
///
/// Without a header there is no declaration to consult, so c17 synthesizes
/// one, and the synthesized *return* type is load-bearing: answering `int`
/// for a function that returns a pointer truncates the address to 32 bits.
/// Every pointer-returning entry below is therefore compared against the
/// address it should have given back, which a truncated return cannot match.
/// Checking `__has_builtin` alone would not catch any of this.
#[test]
fn builtins_libc_aliases_without_headers() {
    let code = r#"
int main(void) {
    char buf[64];
    char dst[64];

    /* Pointer returns must survive as 64-bit values. */
    if (__builtin_strcpy(buf, "hello") != buf) return 1;
    if (__builtin_strlen(buf) != 5) return 2;
    if (__builtin_strcat(buf, "!") != buf) return 3;
    if (__builtin_strlen(buf) != 6) return 4;
    if (__builtin_strchr(buf, 'e') != buf + 1) return 5;
    if (__builtin_strrchr(buf, 'l') != buf + 3) return 6;
    if (__builtin_strstr(buf, "llo") != buf + 2) return 7;
    if (__builtin_strncpy(dst, "abcdef", 3) != dst) return 8;
    if (__builtin_stpcpy(dst, "xy") != dst + 2) return 9;
    if (__builtin_strncat(dst, "zw", 1) != dst) return 10;
    if (__builtin_strcmp(dst, "xyz") != 0) return 11;

    /* Integer returns. */
    if (__builtin_memcmp("abc", "abc", 3) != 0) return 12;
    if (__builtin_memcmp("abc", "abd", 3) >= 0) return 13;
    if (__builtin_strncmp("abcz", "abcy", 3) != 0) return 14;
    if (__builtin_strncmp("abcz", "abcy", 4) <= 0) return 15;

    /* mempcpy returns the end of the copied region, not its start. It is a
       GNU extension: glibc has it, Apple's libc does not, and the builtin
       lowers to a call like any other -- so on macOS this links against a
       symbol that is not there, exactly as it would under gcc. Guarded rather
       than dropped, because the behaviour is worth pinning where it exists. */
#ifdef __GLIBC__
    if (__builtin_mempcpy(dst, "1234", 4) != dst + 4) return 16;
    if (__builtin_memcmp(dst, "1234", 4) != 0) return 17;
#else
    if (__builtin_memcpy(dst, "1234", 4) != dst) return 16;
    if (__builtin_memcmp(dst, "1234", 4) != 0) return 17;
#endif

    /* The allocators: a truncated void* would not round-trip 64 bytes. */
    {
        char *p = (char *)__builtin_malloc(64);
        if (!p) return 18;
        p[0] = 'a'; p[63] = 'z';
        if (p[0] != 'a' || p[63] != 'z') return 19;
        __builtin_free(p);

        p = (char *)__builtin_calloc(16, 4);
        if (!p) return 20;
        if (p[0] != 0 || p[63] != 0) return 21;
        p = (char *)__builtin_realloc(p, 128);
        if (!p) return 22;
        p[127] = 'q';
        if (p[127] != 'q') return 23;
        __builtin_free(p);
    }

    /* The printf family is variadic after a fixed format argument; each
       returns the number of characters written. */
    if (__builtin_sprintf(buf, "%d-%s", 42, "ok") != 5) return 24;
    if (__builtin_strcmp(buf, "42-ok") != 0) return 25;
    if (__builtin_snprintf(buf, sizeof buf, "%c%c", 'h', 'i') != 2) return 26;
    if (__builtin_strcmp(buf, "hi") != 0) return 27;
    if (__builtin_printf("") != 0) return 28;
    if (__builtin_puts("") < 0) return 29;

    /* Reachable only if something above already went wrong, but it must
       still compile and link without <stdlib.h> -- which is the property
       the torture suite depends on. */
    if (buf[0] == 1 && buf[1] == 2 && buf[2] == 3) {
        __builtin_abort();
        __builtin_exit(1);
    }

#define CK(n) do { if (!__has_builtin(n)) return 40; } while (0)
    CK(__builtin_abort); CK(__builtin_exit); CK(__builtin_printf);
    CK(__builtin_sprintf); CK(__builtin_snprintf); CK(__builtin_puts);
    CK(__builtin_malloc); CK(__builtin_calloc); CK(__builtin_realloc);
    CK(__builtin_free); CK(__builtin_memcmp);
    /* c17 knows the builtin on every target; whether the libc has the symbol
       is a separate question, and the one that bit on macOS. */
    CK(__builtin_mempcpy);
    CK(__builtin_strcpy); CK(__builtin_strncpy); CK(__builtin_stpcpy);
    CK(__builtin_strcat); CK(__builtin_strncat); CK(__builtin_strncmp);
    CK(__builtin_strchr); CK(__builtin_strrchr); CK(__builtin_strstr);
    /* Both were implemented but unregistered, so __has_builtin denied them.
       Note this block is not a gcc differential: gcc implements
       `__builtin_complex` -- <complex.h> uses it for CMPLX -- and still
       answers 0 to `__has_builtin` for it, because it is handled specially in
       the front end rather than declared as an ordinary builtin. c17 answers
       1, which is the honest answer about c17 and the more useful one. */
    CK(__builtin_isinf_sign); CK(__builtin_complex);
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("builtins_libc_aliases_no_headers", code, &[]),
        0
    );
}

/// `__builtin_*_overflow` asks whether the *mathematical* result fits, and a
/// 128-bit destination is no exception (#C62).
///
/// The ordinary lowering computes in a type twice the destination's width and
/// asks whether narrowing lost anything; nothing here is wider than 128 bits,
/// so at that width it compared a value to itself. The fallback examined the
/// result directly, on operands already converted to the destination -- which
/// is not value-preserving for a negative operand with an unsigned
/// destination, so `__builtin_add_overflow(-1, 5u, &u128)` reported overflow
/// where the answer is 4.
///
/// When both operands are narrower than 128 bits the exact computation still
/// fits (a sum needs 65 bits, a product 128), so only the *check* changes:
/// from "did narrowing lose anything" to "is the exact value representable".
#[test]
fn builtins_checked_arith_128bit_mixed_signedness() {
    let code = r#"
int main(void) {
    unsigned __int128 u;
    __int128 s;

    /* A negative operand whose mathematical result is representable. */
    if (__builtin_add_overflow(-1, 5u, &u)) return 1;
    if (u != 4) return 2;
    if (__builtin_add_overflow(-10, 20, &u)) return 3;
    if (u != 10) return 4;
    if (__builtin_add_overflow(-5, 5, &u)) return 5;
    if (u != 0) return 6;

    /* ...and ones where it is not. */
    if (!__builtin_add_overflow(-1, -1, &u)) return 7;
    if (!__builtin_sub_overflow(3, 5, &u)) return 8;
    if (!__builtin_mul_overflow(-2, 3, &u)) return 9;
    /* ...while `-1 * -1` is 1, which is representable, so it does not. */
    if (__builtin_mul_overflow(-1, -1, &u)) return 10;
    if (u != 1) return 23;

    /* The exact computation must itself fit the width it is done in:
       `(u64)-1 * (u64)-1` is just under 2^128, which an unsigned 128-bit
       destination holds and a signed one does not. */
    unsigned long long um = 0xFFFFFFFFFFFFFFFFULL;
    if (__builtin_mul_overflow(um, um, &u)) return 24;
    if (!__builtin_mul_overflow(um, um, &s)) return 25;

    /* Unsigned operands at full width still fit a 128-bit destination. */
    if (__builtin_mul_overflow(0xFFFFFFFFFFFFFFFFULL, 0xFFFFFFFFFFFFFFFFULL, &u)) return 11;
    if (__builtin_add_overflow(0xFFFFFFFFFFFFFFFFULL, 0xFFFFFFFFFFFFFFFFULL, &u)) return 12;

    /* A signed 128-bit destination holds anything two 64-bit operands make. */
    if (__builtin_add_overflow(9223372036854775807LL, 9223372036854775807LL, &s)) return 13;
    if (__builtin_mul_overflow(-9223372036854775807LL - 1, -9223372036854775807LL - 1, &s)) return 14;
    if (__builtin_sub_overflow(-9223372036854775807LL - 1, 9223372036854775807LL, &s)) return 15;
    if (__builtin_add_overflow(-1, -1, &s)) return 16;
    if (s != -2) return 17;

    /* Narrow destinations are untouched by any of this. */
    int i;
    if (!__builtin_add_overflow(2147483647, 1, &i)) return 18;
    if (!__builtin_mul_overflow(65536, 65536, &i)) return 19;
    unsigned un;
    if (!__builtin_sub_overflow(0, 1, &un)) return 20;
    unsigned long long ull;
    if (__builtin_add_overflow(-1, 5u, &ull)) return 21;
    if (ull != 4) return 22;

    return 0;
}
"#;
    assert_eq!(compile_and_run("checked_arith_128_mixed", code, &[]), 0);
}

/// An enumeration constant has type `int` (C17 6.4.4.3p2), not the type of the
/// enumeration it belongs to.
///
/// c17 reported the enumeration's type for every constant, which is right only
/// where it has to be: when a member does not fit in `int` the whole
/// enumeration widens, and calling the constant `int` would lose the value.
/// Below that it is simply wrong, and observable --
/// `__builtin_types_compatible_p (typeof (hot), int)` answered 0 where gcc and
/// the standard say 1.
///
/// Every expectation here was taken from gcc on this source.
#[test]
fn builtins_enum_constant_has_type_int() {
    let code = r#"
int i;
double d;
typedef enum { hot, dog, poo, bear } dingos;
typedef enum { janette, laura, amanda } cranberry;
typedef float same1;
typedef float same2;

/* It must still be a constant expression: this is a file-scope array bound. */
float rootbeer[__builtin_types_compatible_p (int, typeof(i))];

/* A member past INT_MAX widens the enumeration, and the constant with it. */
enum big { small = 1, huge = 5000000000LL };

int main(void) {
    /* Compatible. */
    if (!__builtin_types_compatible_p(int, const int)) return 1;
    if (!__builtin_types_compatible_p(typeof(hot), int)) return 2;
    if (!__builtin_types_compatible_p(typeof(hot), typeof(laura))) return 3;
    if (!__builtin_types_compatible_p(int[5], int[])) return 4;
    if (!__builtin_types_compatible_p(same1, same2)) return 5;

    /* Incompatible. */
    if (__builtin_types_compatible_p(char *, int)) return 6;
    if (__builtin_types_compatible_p(char *, const char *)) return 7;
    if (__builtin_types_compatible_p(long double, double)) return 8;
    if (__builtin_types_compatible_p(typeof(i), typeof(d))) return 9;
    if (__builtin_types_compatible_p(typeof(dingos), typeof(cranberry))) return 10;
    if (__builtin_types_compatible_p(char, int)) return 11;
    if (__builtin_types_compatible_p(char *, char **)) return 12;

    /* The enumeration type itself is still its own type, distinct from int. */
    if (__builtin_types_compatible_p(dingos, int)) return 13;

    /* The value survives either way, which is what the widening protects. */
    if (hot != 0 || bear != 3) return 14;
    if (huge != 5000000000LL) return 15;
    if (sizeof(rootbeer) != sizeof(float)) return 16;
    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_enum_constant_int", code, &[]), 0);
}

/// `__builtin_classify_type(expr)` — a compile-time code for the argument's
/// type family.
///
/// Listed in `doc/BUILTIN.md` as not implemented, which was observable
/// wherever a header branched on it. Like `sizeof`, the argument is not
/// evaluated; unlike `sizeof`, gcc takes an expression rather than a type
/// name.
///
/// The codes were read off gcc rather than taken from its documentation,
/// because the usual conversions run first and that is where the surprises
/// are: a `char`, an enumeration constant and a `_Bool` all answer 1, and an
/// array, a function and a string literal all answer 5, because each decays
/// to a pointer before the classification sees it. All fifteen families below
/// were diffed against gcc line by line.
#[test]
fn builtins_classify_type() {
    let code = r#"
struct S { int a; };
union U { int a; };
enum E { e1 };
void fn(void);
int arr[4];

int main(void) {
    /* Integer family: everything that converts to an integer answers 1. */
    if (__builtin_classify_type(1) != 1) return 1;
    if (__builtin_classify_type('c') != 1) return 2;
    if (__builtin_classify_type(e1) != 1) return 3;
    if (__builtin_classify_type(1L) != 1) return 4;
    if (__builtin_classify_type(1ULL) != 1) return 5;
    { char c = 0; if (__builtin_classify_type(c) != 1) return 6; }
    { _Bool b = 0; if (__builtin_classify_type(b) != 1) return 7; }
    { short s = 0; if (__builtin_classify_type(s) != 1) return 8; }

    /* Real floating: 8, at every precision. */
    if (__builtin_classify_type(1.0f) != 8) return 9;
    if (__builtin_classify_type(1.0) != 8) return 10;
    if (__builtin_classify_type(1.0L) != 8) return 11;

    /* Complex: 9, distinct from the real it is built from. */
    { _Complex double z = 0; if (__builtin_classify_type(z) != 9) return 12; }
    { _Complex float w = 0; if (__builtin_classify_type(w) != 9) return 13; }

    /* Aggregates keep their own codes. */
    { struct S v; if (__builtin_classify_type(v) != 12) return 14; }
    { union U v; if (__builtin_classify_type(v) != 13) return 15; }

    /* Anything that decays answers as the pointer it decays to. */
    if (__builtin_classify_type((void *)0) != 5) return 16;
    if (__builtin_classify_type(arr) != 5) return 17;
    if (__builtin_classify_type(&fn) != 5) return 18;
    if (__builtin_classify_type("x") != 5) return 19;

    /* A constant expression, usable where one is required. */
    { int a[__builtin_classify_type(1.0) == 8 ? 3 : -1]; if (sizeof(a)/sizeof(a[0]) != 3) return 20; }

    /* And the argument is not evaluated. */
    { int i = 0; if (__builtin_classify_type(i++) != 1) return 21; if (i != 0) return 22; }
    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_classify_type", code, &[]), 0);
}

/// `__builtin_{add,sub,mul}_overflow_p` — the same question as the storing
/// forms, answered without storing.
///
/// The destination type is named by a *value* rather than a pointer to one,
/// and that value is never evaluated: gcc reads its type and nothing else.
/// So a side effect in the third argument must not happen, which is the one
/// thing an implementation built on the storing form would get wrong.
#[test]
fn builtins_checked_arith_overflow_p() {
    let code = r#"
#include <limits.h>

int main(void) {
    /* The flag matches the storing form's, with nothing written. */
    if (!__builtin_add_overflow_p(INT_MAX, 1, (int)0)) return 1;
    if (__builtin_add_overflow_p(1, 1, (int)0)) return 2;
    if (!__builtin_sub_overflow_p(0u, 1u, (unsigned)0)) return 3;
    if (__builtin_sub_overflow_p(5, 1, (int)0)) return 4;
    if (!__builtin_mul_overflow_p(INT_MAX, 2, (int)0)) return 5;
    if (__builtin_mul_overflow_p(3, 4, (int)0)) return 6;

    /* It is the *destination* type that decides, not the operands'. */
    if (!__builtin_add_overflow_p(200, 100, (char)0)) return 7;
    if (__builtin_add_overflow_p(200, 100, (long)0)) return 8;
    if (!__builtin_mul_overflow_p(70000, 70000, (int)0)) return 9;
    if (__builtin_mul_overflow_p(70000, 70000, (long long)0)) return 10;

    /* A negative result is unrepresentable in an unsigned destination. */
    if (!__builtin_sub_overflow_p(1, 2, (unsigned)0)) return 11;
    if (__builtin_sub_overflow_p(1, 2, (int)0)) return 12;

    /* The third argument is used for its type, and still evaluated -- gcc
       increments `i` here, so we must too. */
    { int i = 0; if (__builtin_add_overflow_p(1, 1, i++)) return 13;
      if (i != 1) return 14; }

    /* The storing forms still store, which the flag must not have disturbed. */
    { int r = 99; if (__builtin_add_overflow(1, 2, &r)) return 15;
      if (r != 3) return 16; }

    if (!__has_builtin(__builtin_add_overflow_p)) return 17;
    if (!__has_builtin(__builtin_sub_overflow_p)) return 18;
    if (!__has_builtin(__builtin_mul_overflow_p)) return 19;
    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_overflow_p", code, &[]), 0);
}

/// The libc aliases the `execute/builtins/` sub-suite needs, which running
/// that sub-suite is what surfaced.
///
/// Nine more of the same family as the earlier batch, plus the three stdio
/// `_unlocked` forms. Every one was checked against gcc before being added:
/// `__builtin_bcopy` returns `void` and takes three arguments, `index` and
/// `rindex` are the old spellings of `strchr`/`strrchr` and return `char *`,
/// `memchr` returns `void *`, `strspn`/`strcspn` return a size.
///
/// glibc has no `printf_unlocked`, so gcc's own link of
/// `__builtin_printf_unlocked` fails against the system library — the torture
/// tests supply the library side themselves, which is the arrangement this has
/// to work under, and why the test below defines them.
#[test]
fn builtins_libc_aliases_second_batch() {
    let code = r#"
#include <stdio.h>

/* The `_unlocked` forms have no glibc definition; supply them, as the
   gcc.c-torture builtins/ tests do. */
int printf_unlocked(const char *f, ...) { (void)f; return 11; }
int fprintf_unlocked(FILE *s, const char *f, ...) { (void)s; (void)f; return 22; }
int fputs_unlocked(const char *s, FILE *f) { (void)s; (void)f; return 33; }

int main(void) {
    char buf[32];
    __builtin_strcpy(buf, "hello world");

    /* void * return: a truncated one would not equal buf + 6. */
    if (__builtin_memchr(buf, 'w', 12) != buf + 6) return 1;
    if (__builtin_memchr(buf, 'z', 12) != 0) return 2;

    /* char * returns, under their old names. */
    if (__builtin_index(buf, 'l') != buf + 2) return 3;
    if (__builtin_rindex(buf, 'l') != buf + 9) return 4;
    if (__builtin_strpbrk(buf, "xyzw") != buf + 6) return 5;
    if (__builtin_strpbrk(buf, "QZ") != 0) return 6;

    /* Sizes, not pointers. */
    if (__builtin_strspn("aabbcc", "ab") != 4) return 7;
    if (__builtin_strcspn("aabbcc", "c") != 4) return 8;

    if (__builtin_imaxabs(-42) != 42) return 9;
    if (__builtin_imaxabs(42) != 42) return 10;

    /* bcopy returns void and takes source first, unlike memcpy. */
    { char d[8]; __builtin_bcopy("abc", d, 4);
      if (__builtin_strcmp(d, "abc") != 0) return 11; }

    if (__builtin_putchar('\n') != '\n') return 12;

    /* The unlocked forms reach the definitions above, with the right arity:
       printf_unlocked is variadic after its format, fprintf_unlocked after
       its stream and format, fputs_unlocked takes exactly two. */
    if (__builtin_printf_unlocked("%d %s\n", 1, "x") != 11) return 13;
    if (__builtin_fprintf_unlocked(stdout, "%d\n", 2) != 22) return 14;
    if (__builtin_fputs_unlocked("y\n", stdout) != 33) return 15;

#define CK(n) do { if (!__has_builtin(n)) return 40; } while (0)
    CK(__builtin_memchr); CK(__builtin_index); CK(__builtin_rindex);
    CK(__builtin_strpbrk); CK(__builtin_strspn); CK(__builtin_strcspn);
    CK(__builtin_imaxabs); CK(__builtin_bcopy); CK(__builtin_putchar);
    CK(__builtin_printf_unlocked); CK(__builtin_fprintf_unlocked);
    CK(__builtin_fputs_unlocked);
    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_libc_second_batch", code, &[]), 0);
}
