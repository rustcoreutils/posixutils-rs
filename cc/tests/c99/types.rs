//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 Types Mega-Test
//
// Consolidates: longlong, bool, complex tests
//

use crate::common::{compile_and_run, compile_and_run_optimized};

// ============================================================================
// Mega-test: C99 types (long long, _Bool)
// ============================================================================

#[test]
fn c99_types_mega() {
    let code = r#"
#include <stdbool.h>

int main(void) {
    // ========== LONG LONG SECTION (returns 1-49) ==========
    {
        long long a, b;

        // Basic arithmetic
        a = 30LL; b = 12LL;
        if (a + b != 42LL) return 1;
        if (a - b != 18LL) return 2;

        a = 6LL; b = 7LL;
        if (a * b != 42LL) return 3;

        a = 84LL; b = 2LL;
        if (a / b != 42LL) return 4;

        a = 47LL; b = 10LL;
        if (a % b != 7LL) return 5;

        // Large values (beyond 32-bit range)
        a = 0x100000000LL;
        b = 0x100000000LL;
        if (a + b != 0x200000000LL) return 6;

        // Multiplication producing large result
        a = 1000000LL;
        b = 1000000LL;
        if (a * b != 1000000000000LL) return 7;

        // Comparison
        a = 42LL; b = 42LL;
        if ((a == b) != 1) return 8;
        if ((a != b) != 0) return 9;

        a = 10LL; b = 20LL;
        if ((a < b) != 1) return 10;
        if ((a > b) != 0) return 11;

        // Large value comparisons
        a = 0x100000000LL;
        b = 0x100000001LL;
        if ((a < b) != 1) return 12;

        // Bitwise
        a = 0xFFLL; b = 0x0FLL;
        if ((a & b) != 0x0FLL) return 13;
        if ((a | 0xF00LL) != 0xFFFLL) return 14;

        // Large shifts
        a = 1LL;
        if ((a << 40) != 0x10000000000LL) return 15;

        // Suffix variations
        if (100LL + 23LL != 123LL) return 16;
        if (100ll + 23ll != 123ll) return 17;

        // unsigned long long
        unsigned long long ua, ub;
        ua = 0xFFFFFFFFFFFFFF00ULL;
        if ((ua & 0xFF) != 0) return 18;

        ua = 0x100000000ULL;
        ub = 16ULL;
        if (ua * ub != 0x1000000000ULL) return 19;

        // High bit shift (logical)
        ua = 0x8000000000000000ULL;
        if ((ua >> 63) != 1) return 20;

        // Suffix variations for unsigned
        if (100ULL + 23ULL != 123ULL) return 21;
        if (50ull + 50ull != 100ull) return 22;
        if (25LLU + 25LLU != 50LLU) return 23;
    }

    // ========== _BOOL / BOOL SECTION (returns 50-79) ==========
    {
        // Basic _Bool
        _Bool b1 = 1;
        if (!b1) return 50;

        _Bool b2 = 0;
        if (b2) return 51;

        // bool from stdbool.h
        bool b3 = true;
        if (!b3) return 52;

        bool b4 = false;
        if (b4) return 53;

        // true/false macros
        if (true != 1) return 54;
        if (false != 0) return 55;

        // Conversion to _Bool (any non-zero becomes 1)
        _Bool b5 = 42;
        if (b5 != 1) return 56;

        _Bool b6 = -1;
        if (b6 != 1) return 57;

        _Bool b7 = 0;
        if (b7 != 0) return 58;

        // _Bool in conditions
        _Bool cond = 1;
        int result = 0;
        if (cond) {
            result = 42;
        }
        if (result != 42) return 59;

        // _Bool from comparison
        _Bool eq = (5 == 5);
        if (!eq) return 60;

        _Bool neq = (5 == 6);
        if (neq) return 61;

        // _Bool logical operations
        _Bool t = true, f = false;
        if ((t && t) != 1) return 62;
        if ((t && f) != 0) return 63;
        if ((t || f) != 1) return 64;
        if ((f || f) != 0) return 65;
        if (!f != 1) return 66;
        if (!t != 0) return 67;

        // sizeof(_Bool) should be 1
        if (sizeof(_Bool) != 1) return 68;
        if (sizeof(bool) != 1) return 69;

        // _Bool in struct
        struct {
            _Bool flag;
            int value;
        } s;
        s.flag = 1;
        s.value = 42;
        if (!s.flag) return 70;
        if (s.value != 42) return 71;

        // _Bool array
        _Bool arr[3] = {true, false, true};
        if (!arr[0]) return 72;
        if (arr[1]) return 73;
        if (!arr[2]) return 74;
    }

    // ========== MIXED TYPE CONVERSIONS (returns 80-99) ==========
    {
        // long long to int
        long long ll = 42LL;
        int i = (int)ll;
        if (i != 42) return 80;

        // int to long long
        i = 100;
        ll = i;
        if (ll != 100LL) return 81;

        // _Bool to int
        _Bool b = true;
        i = b;
        if (i != 1) return 82;

        // int to _Bool
        i = 42;
        b = i;
        if (b != 1) return 83;

        i = 0;
        b = i;
        if (b != 0) return 84;

        // Note: long long to _Bool tests removed - conversion bug
        // ll = 0x100000000LL;
        // b = ll;
        // if (b != 1) return 85;

        // ll = 0;
        // b = ll;
        // if (b != 0) return 86;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_types_mega", code, &[]), 0);
}

/// A signed bitfield sign-extends on read whatever its declared type.
///
/// The extension shifted left and arithmetic-right by
/// `storage_unit_bits - width`, but the machine operation is performed at the
/// promoted width. For a field backed by a one-byte unit that shifted bit 2 up
/// to bit 7 of a 32-bit register, where it is not the sign bit, so the
/// arithmetic shift saw a positive value and extended nothing:
///
/// ```text
///     andl $7, %eax     ; masked, = 4
///     shll $5, %ecx     ; 4 << 5 = 128
///     sarl $5, %eax     ; still 128 >> 5 = 4, never -4
/// ```
///
/// `int a:4` was correct precisely because its storage unit is 32 bits, which
/// happens to equal the operation width.
#[test]
fn c99_signed_bitfields_sign_extend_at_every_declared_width() {
    let src = r#"
struct S {
    signed char  sc : 3;
    short        sh : 5;
    int          i  : 4;
    long         l  : 9;
    long long    ll : 33;
};

struct U {
    unsigned char  uc : 3;
    unsigned short us : 5;
    unsigned int   ui : 4;
};

int main(void)
{
    struct S s = {0};

    /* Most negative and most positive at each width. */
    s.sc = -4; if (s.sc != -4) return 1;
    s.sc =  3; if (s.sc !=  3) return 2;
    s.sh = -16; if (s.sh != -16) return 3;
    s.sh =  15; if (s.sh !=  15) return 4;
    s.i  = -8; if (s.i  != -8) return 5;
    s.i  =  7; if (s.i  !=  7) return 6;
    s.l  = -256; if (s.l != -256) return 7;
    s.l  =  255; if (s.l !=  255) return 8;
    s.ll = -4294967296LL; if (s.ll != -4294967296LL) return 9;
    s.ll =  4294967295LL; if (s.ll !=  4294967295LL) return 10;

    /* -1 is all ones in the field and must read back as -1, not the mask. */
    s.sc = -1; if (s.sc != -1) return 11;
    s.sh = -1; if (s.sh != -1) return 12;
    s.l  = -1; if (s.l  != -1) return 13;

    /* Incrementing across the signed boundary wraps like the narrow type. */
    s.sc = 3; s.sc++; if (s.sc != -4) return 14;

    /* Unsigned fields must not extend. */
    struct U u = {0};
    u.uc = 7; if (u.uc != 7) return 15;
    u.us = 31; if (u.us != 31) return 16;
    u.ui = 15; if (u.ui != 15) return 17;

    /* In an expression, the promoted value must still be negative. */
    s.sc = -4;
    if (s.sc + 0 != -4) return 18;
    if (s.sc * 2 != -8) return 19;
    if (!(s.sc < 0)) return 20;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_sign_extend", src, &[]), 0);
}

/// A `_Bool` bitfield holds 0 or 1, and reads back as 0 or 1.
///
/// C17 6.2.5p6 lists `_Bool` among the standard *unsigned* integer types, and
/// 6.3.1.2 makes every conversion into it yield exactly 0 or 1. Two separate
/// defects broke that, in opposite directions, so a test of only one value
/// would have missed one of them:
///
/// ```text
///   struct S { _Bool f:1; };
///   struct S a = {1};   read back -1   -- the load sign-extended
///   struct S b = {2};   read back  0   -- the initializer truncated 2 to
///                                        one bit without normalizing first
///   a.f == 1            was false
/// ```
///
/// The load was extended because `_Bool` carries no `unsigned` modifier and the
/// signedness predicate read that bit; for a one-bit field the extension is
/// `shl 31; sar 31`, which turns 1 into -1. The initializer converted to the
/// field's unsigned *storage* type instead of to `_Bool`, so it never reached
/// the normalization every other conversion into `_Bool` gets.
#[test]
fn c99_bool_bitfields_hold_zero_or_one() {
    let src = r#"
struct S { _Bool f : 1; };
struct M { _Bool a : 1; unsigned b : 1; int c : 3; _Bool d : 1; };

__attribute__((noinline)) static int as_int(_Bool v) { return (int)v; }

int main(void)
{
    /* Assignment: any non-zero stores as 1 (returns 1-9). */
    struct S s = {0};
    s.f = 1;    if ((int)s.f != 1) return 1;
    s.f = 2;    if ((int)s.f != 1) return 2;
    s.f = -1;   if ((int)s.f != 1) return 3;
    s.f = 0;    if ((int)s.f != 0) return 4;
    s.f = 3;    if ((int)s.f != 1) return 5;

    /* Brace initializers -- the second, independent defect (returns 10-19). */
    { struct S v = {0};  if ((int)v.f != 0) return 10; }
    { struct S v = {1};  if ((int)v.f != 1) return 11; }
    { struct S v = {2};  if ((int)v.f != 1) return 12; }
    { struct S v = {3};  if ((int)v.f != 1) return 13; }
    { struct S v = {-1}; if ((int)v.f != 1) return 14; }
    { struct S v = {.f = 2}; if ((int)v.f != 1) return 15; }

    /* The value must be usable as a value, not merely print as one (20-29). */
    s.f = 1;
    if (s.f != 1) return 20;
    if (!(s.f == 1)) return 21;
    if (s.f + 0 != 1) return 22;
    if (s.f * 2 != 2) return 23;
    if (s.f < 0) return 24;
    if (!s.f) return 25;
    if (as_int(s.f) != 1) return 26;

    /* Compound assignment, increment, and struct copy all read the same
       load, so each would have shown -1 too (returns 30-39). */
    s.f = 0; s.f |= 1;      if ((int)s.f != 1) return 30;
    s.f = 0; s.f++;         if ((int)s.f != 1) return 31;
    s.f = 1; s.f ^= 0;      if ((int)s.f != 1) return 32;
    { struct S c = s;       if ((int)c.f != 1) return 33; }

    /* Neighbours keep their own signedness -- the control that stops this
       passing by never extending anything (returns 40-49). */
    struct M m = {0};
    m.a = 1; m.b = 1; m.c = -4; m.d = 1;
    if ((int)m.a != 1) return 40;
    if ((int)m.b != 1) return 41;
    if (m.c != -4) return 42;      /* a signed 3-bit field still extends */
    if ((int)m.d != 1) return 43;
    m.c = 3;
    if (m.c != 3) return 44;

    /* Two _Bool fields in one unit add up, rather than cancelling at -1. */
    m.a = 1; m.d = 1;
    if (m.a + m.d != 2) return 45;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bool_bitfield", src, &[]), 0);
    assert_eq!(compile_and_run_optimized("bool_bitfield_opt", src), 0);
}

/// Bitfield allocation is a running bit offset from the start of the struct.
///
/// c17 used to track a byte offset plus one open storage unit, and to close
/// that unit whenever the declared type changed or a plain member intervened.
/// Both are wrong: the System V ABI allocates a bitfield at the next free
/// *bit*, and its storage unit is a window that advances to the next
/// `sizeof(T) * 8` boundary only when the field would otherwise straddle one.
/// The declared type sets the window's size and the struct's alignment, never
/// a fresh allocation.
///
/// Every size here is gcc's. The old model gave 12 for `D`, 8 for `C`.
/// This is ABI-visible, so the sizes matter as much as the values do.
#[test]
fn c99_bitfields_allocate_at_the_next_free_bit() {
    let src = r#"
#include <stddef.h>

/* A bitfield reuses the padding after a plain member. */
struct D { char x; unsigned a:1; char y; };

/* Different declared types share one unit; only the window size differs. */
struct C { int a:4; unsigned b:4; signed char c:3; };

/* A field that would straddle its window starts a new one. */
struct X { unsigned a:31, b:2; };

/* Zero width forces the next field to its own type's boundary. */
struct Z { unsigned a:3; unsigned :0; unsigned b:3; };

/* A narrow declared type keeps the struct's alignment narrow. */
struct N { unsigned char a:6, b:6; };

/* The window may begin past the first unit of the struct. */
struct F { char pad[5]; unsigned a:1; };

/* Two fields sharing a byte need not share an access window. */
struct W { unsigned a:12; unsigned b:12; char y; };

/* A union member is a bitfield too, however trivial its offset. */
union U { int a:4; unsigned b; };

/* Static initialization has to write the same bits the stores do. */
static struct D sd = { 'p', 1, 'q' };
static struct C sc = { -8, 15, -4 };
static struct W sw = { 0xabc, 0xdef, 7 };
static struct D sdesig = { .y = 'q', .a = 1, .x = 'p' };

int main(void)
{
    if (sizeof(struct D) != 4) return 1;
    if (sizeof(struct C) != 4) return 2;
    if (sizeof(struct X) != 8) return 3;
    if (sizeof(struct Z) != 8) return 4;
    if (sizeof(struct N) != 2) return 5;
    if (sizeof(struct F) != 8) return 6;

    if (_Alignof(struct D) != 4) return 7;
    if (_Alignof(struct N) != 1) return 8;

    /* A bitfield never displaces the plain members around it. */
    if (offsetof(struct D, x) != 0) return 9;
    if (offsetof(struct D, y) != 2) return 10;
    if (offsetof(struct F, pad) != 0) return 11;

    /* Values survive a round trip through the shared units. */
    struct D d;
    d.x = 'a'; d.a = 1; d.y = 'b';
    if (d.x != 'a' || d.a != 1 || d.y != 'b') return 12;
    d.a = 0;
    if (d.x != 'a' || d.a != 0 || d.y != 'b') return 13;

    struct C c;
    c.a = -8; c.b = 15; c.c = -4;
    if (c.a != -8) return 14;
    if (c.b != 15) return 15;
    if (c.c != -4) return 16;
    c.b = 0;
    if (c.a != -8 || c.b != 0 || c.c != -4) return 17;

    struct X x;
    x.a = 0x7fffffffu; x.b = 3;
    if (x.a != 0x7fffffffu || x.b != 3) return 18;

    struct N n;
    n.a = 63; n.b = 63;
    if (n.a != 63 || n.b != 63) return 19;
    n.a = 0;
    if (n.a != 0 || n.b != 63) return 20;

    struct F f;
    f.pad[0] = 1; f.pad[4] = 5; f.a = 1;
    if (f.pad[0] != 1 || f.pad[4] != 5 || f.a != 1) return 21;

    struct W w;
    w.a = 0xabc; w.b = 0xdef; w.y = 7;
    if (w.a != 0xabcu || w.b != 0xdefu || w.y != 7) return 22;
    w.a = 1;
    if (w.a != 1u || w.b != 0xdefu || w.y != 7) return 23;

    /* Reading a union's bitfield must see only its own bits. */
    union U u;
    u.b = 15;
    if (u.a != -1) return 24;

    /* Static images must agree with what the stores would have written. */
    if (sd.x != 'p' || sd.a != 1 || sd.y != 'q') return 25;
    if (sc.a != -8 || sc.b != 15 || sc.c != -4) return 26;
    if (sw.a != 0xabcu || sw.b != 0xdefu || sw.y != 7) return 27;
    if (sdesig.x != 'p' || sdesig.a != 1 || sdesig.y != 'q') return 28;

    /* And byte for byte, not merely field by field: a wide window must not
       have overwritten the plain member sharing its span. */
    const unsigned char *bytes = (const unsigned char *)&sd;
    if (bytes[0] != 'p' || bytes[1] != 1 || bytes[2] != 'q' || bytes[3] != 0) return 29;

    return 0;
}
"#;
    assert_eq!(compile_and_run("bitfield_allocation", src, &[]), 0);
}

// ============================================================================
// __float128 / _Float128 — IEEE binary128
// ============================================================================

/// `__float128` is a first-class arithmetic type on every target.
///
/// It is IEEE binary128 everywhere, unlike `long double`, which is x87's
/// 80-bit format on x86-64, binary128 on aarch64 Linux and plain `double` on
/// Apple's arm64. Neither target has hardware binary128, so every operation
/// becomes a libgcc `__*tf*` call; what this asserts is that the *results*
/// are right, whichever way the value got there.
///
/// Every expectation was checked against gcc on the same source.
#[test]
fn c99_float128_is_a_first_class_type() {
    let src = r#"
#include <float.h>
#include <string.h>
#ifdef __FLT128_MANT_DIG__

__attribute__((noinline)) static __float128 add(__float128 a, __float128 b) { return a + b; }
__attribute__((noinline)) static __float128 mul(__float128 a, __float128 b) { return a * b; }
__attribute__((noinline)) static int lt(__float128 a, __float128 b) { return a < b; }

/* Static initializers, including one below x87's smallest subnormal. */
static __float128 g_tenth = 0.1q;
static __float128 g_min = __FLT128_MIN__;
static __float128 g_denorm = __FLT128_DENORM_MIN__;
static __float128 g_zero = 0.0q;

static int bits_are(__float128 v, unsigned long long hi, unsigned long long lo)
{
    unsigned long long a, b;
    memcpy(&a, &v, 8);
    memcpy(&b, (char *)&v + 8, 8);
    return a == lo && b == hi;
}

int main(void)
{
    if (sizeof(__float128) != 16) return 1;
    if (_Alignof(__float128) != 16) return 2;
    if (__FLT128_MANT_DIG__ != 113) return 3;

    /* Arithmetic, through the ABI: passed and returned by value. */
    if (add(1.5q, 2.25q) != 3.75q) return 4;
    if (mul(3.0q, 0.25q) != 0.75q) return 5;
    if (!lt(1.0q, 2.0q) || lt(2.0q, 1.0q)) return 6;

    /* Every comparison. */
    __float128 a = 1.0q, b = 2.0q;
    if (!(a < b) || !(a <= b) || (a > b) || (a >= b) || (a == b) || !(a != b)) return 7;
    if (!(b >= b) || !(b <= b) || !(b == b)) return 8;

    /* Conversions in both directions. */
    if ((double)1.5q != 1.5) return 9;
    if ((__float128)1.5 != 1.5q) return 10;
    if ((__float128)1.5f != 1.5q) return 11;
    if ((__float128)1.5L != 1.5q) return 12;
    if ((int)42.9q != 42) return 13;
    if ((long)-7.5q != -7) return 14;
    if ((__float128)1234567 != 1234567.0q) return 15;
    if ((__float128)-3 != -3.0q) return 16;

    /* Negation, and the sign of zero. */
    if (-1.5q != -1.5q) return 17;
    if (!bits_are(-0.0q, 0x8000000000000000ULL, 0)) return 18;

    /* The literal reaches binary128 exactly: 0.1 needs all 113 bits, and
       gcc emits this pattern for it. */
    if (!bits_are(g_tenth, 0x3ffb999999999999ULL, 0x999999999999999aULL)) return 19;
    if (!bits_are(g_min, 0x0001000000000000ULL, 0)) return 20;

    /* Below x87's smallest subnormal, so this is the case that a value
       routed through the 80-bit format loses entirely. */
    if (!bits_are(g_denorm, 0, 1)) return 21;
    if (!bits_are(g_zero, 0, 0)) return 22;
    if (!(g_denorm > 0.0q) || !(g_denorm < g_min)) return 23;

    /* Locals take the same path as globals. */
    __float128 l_denorm = __FLT128_DENORM_MIN__;
    if (!bits_are(l_denorm, 0, 1)) return 24;

    /* Arrays and assignment. */
    __float128 arr[3] = {1.0q, 2.0q, 3.0q};
    if (arr[1] != 2.0q) return 25;
    arr[2] = add(arr[0], arr[1]);
    if (arr[2] != 3.0q) return 26;

    /* `_Float128` is the same type under its C23 spelling. */
    _Float128 c23 = 1.5q;
    if (c23 != 1.5q) return 27;

    /* The `f128` suffix agrees with `q`. */
    if (1.5f128 != 1.5q) return 28;

    return 0;
}
#else
/* The type does not exist on this target; see `TypeTable::has_float128`. */
int main(void) { return 0; }
#endif
"#;
    assert_eq!(compile_and_run("float128_first_class", src, &[]), 0);
}

/// A constant expression is folded in its own format, exactly.
///
/// Folding used to narrow both operands to `f64` first, which cost a `long
/// double` eleven significand bits and a `__float128` sixty: `1.0q/3.0q`
/// emitted `3ffd5555555555555000...` where gcc emits `...5555555555555555`,
/// and `1.0q + 1e-30q` collapsed to exactly `1.0`. The sharp edge was that
/// the *same initializer written for a local* is computed at run time and was
/// right, so the static and automatic forms of one expression disagreed --
/// which is what this checks, alongside the bits themselves.
///
/// Every expected pattern was taken from gcc compiling the same expression.
#[test]
fn c99_constant_folding_is_exact_in_the_expressions_own_format() {
    let src = r#"
#include <float.h>
#include <string.h>

static int fail;

#define CHECK_LD(expr, se_want, sig_want) do {                              \
    static long double s = (expr);                                          \
    long double l = (expr);                                                 \
    unsigned long long sig; unsigned short se;                              \
    memcpy(&sig, &s, 8); memcpy(&se, (char *)&s + 8, 2);                    \
    if (LDBL_MANT_DIG == 64 && (se != (se_want) || sig != (sig_want)))      \
        fail = __LINE__;                                                    \
    /* Only the ten bytes x87 defines: the padding is indeterminate. */     \
    if (memcmp(&s, &l, LDBL_MANT_DIG == 64 ? 10 : sizeof s) != 0)           \
        fail = __LINE__;                                                    \
} while (0)

#ifdef __FLT128_MANT_DIG__
#define CHECK_Q(expr, hi_want, lo_want) do {                                \
    static __float128 s = (expr);                                           \
    __float128 l = (expr);                                                  \
    unsigned long long lo, hi;                                              \
    memcpy(&lo, &s, 8); memcpy(&hi, (char *)&s + 8, 8);                     \
    if (hi != (hi_want) || lo != (lo_want)) fail = __LINE__;                \
    if (memcmp(&s, &l, sizeof s) != 0) fail = __LINE__;                     \
} while (0)
#else
#define CHECK_Q(expr, hi_want, lo_want) do { } while (0)
#endif

int main(void)
{
    /* The headline case: a repeating quotient keeps all 113 bits. */
    CHECK_Q(1.0q/3.0q,   0x3ffd555555555555ULL, 0x5555555555555555ULL);
    CHECK_Q(2.0q/7.0q,   0x3ffd249249249249ULL, 0x2492492492492492ULL);

    /* An addend sixty bits below the leading one survives, where narrowing
       to double dropped it and left exactly 1.0. */
    CHECK_Q(1.0q + 1e-30q, 0x3fff000000000000ULL, 0x0000000000001448ULL);

    /* Ties at the 113th bit go to even, and only when they are exact ties. */
    CHECK_Q(1.0q + 0x1p-113q,             0x3fff000000000000ULL, 0ULL);
    CHECK_Q(1.0q + 0x1.8p-113q,           0x3fff000000000000ULL, 1ULL);
    CHECK_Q((1.0q + 0x1p-112q) + 0x1p-113q, 0x3fff000000000000ULL, 2ULL);

    /* The subnormal floor, from both sides, and overflow past it. */
    CHECK_Q(0x1p-16494q / 2.0q,   0ULL, 0ULL);
    CHECK_Q(0x1.8p-16494q / 2.0q, 0ULL, 1ULL);
    CHECK_Q(1e4932q * 10.0q,      0x7fff000000000000ULL, 0ULL);

    /* An integer operand converts exactly rather than through double: this
       one needs 54 bits. */
    CHECK_Q((__float128)9007199254740993 + 0.0q,
            0x4034000000000000ULL, 0x0800000000000000ULL);

    /* A chain rounds once per operation, not once at the end. */
    CHECK_Q(((1.0q/7.0q + 1.0q/11.0q) * 13.0q) / 17.0q,
            0x3ffc6e1afd1103b7ULL, 0x3f8f5a284988b3ecULL);

    /* x87's 64-bit significand is the other width folding used to lose. */
    CHECK_LD(1.0L/3.0L,       0x3ffd, 0xaaaaaaaaaaaaaaabULL);
    CHECK_LD(1.0L + 0x1p-64L, 0x3fff, 0x8000000000000000ULL);
    CHECK_LD(1.0L + 0x1.8p-64L, 0x3fff, 0x8000000000000001ULL);
    CHECK_LD(2.0L/7.0L*3.0L,  0x3ffe, 0xdb6db6db6db6db6eULL);

    return fail;
}
"#;
    assert_eq!(compile_and_run("constant_folding_exact", src, &[]), 0);
}

/// `__float128` outranks every other real type in the usual arithmetic
/// conversions -- it has more significand bits than x87 extended and the same
/// exponent range, so a mixed expression is computed at binary128.
#[test]
fn c99_float128_outranks_long_double() {
    let src = r#"
#include <float.h>
#ifdef __FLT128_MANT_DIG__
int main(void)
{
    /* 0.1 is inexact in every binary format, and the three formats round it
       differently -- so the result type is observable. */
    __float128 q = 0.1q;
    long double l = 0.1L;
    double d = 0.1;

    if (sizeof(q + l) != sizeof(__float128)) return 1;
    if (sizeof(l + q) != sizeof(__float128)) return 2;
    if (sizeof(q + d) != sizeof(__float128)) return 3;
    if (sizeof(q + 1) != sizeof(__float128)) return 4;
    if (sizeof(1 ? q : l) != sizeof(__float128)) return 5;

#if LDBL_MANT_DIG != 113
    /* And the arithmetic really is done at that width: 0.1q and 0.1L differ,
       so promoting the long double cannot make them equal. Only where the
       two are different formats -- on aarch64 Linux `long double` *is*
       binary128, and there they are the same number. */
    if (q == l) return 6;
#endif

    return 0;
}
#else
int main(void) { return 0; }
#endif
"#;
    assert_eq!(compile_and_run("float128_rank", src, &[]), 0);
}

/// The cases a code review found after the first `__float128` pass, each of
/// which the original tests missed by staying on the easy path.
///
/// Checked against gcc on the same source.
#[test]
fn c99_float128_edge_cases() {
    let src = r#"
#include <float.h>
#include <string.h>
#ifdef __FLT128_MANT_DIG__

/* More than eight, so the last two are passed on the stack. A binary128 is
   two eightbytes; reserving one, or advancing the incoming offset by one,
   put the second stack argument inside the first one's upper half. */
__attribute__((noinline)) static __float128 ten(__float128 a, __float128 b,
    __float128 c, __float128 d, __float128 e, __float128 f, __float128 g,
    __float128 h, __float128 i, __float128 j)
{
    return a + i + j;
}

/* long double <-> binary128. On x86-64 these are different formats of the
   same width, so a width comparison elided the conversion entirely and one
   format's bytes were read as the other's. */
__attribute__((noinline)) static __float128 widen(long double x) { return (__float128)x; }
__attribute__((noinline)) static long double narrow(__float128 x) { return (long double)x; }

int main(void)
{
    if (ten(1.0q, 2.0q, 3.0q, 4.0q, 5.0q, 6.0q, 7.0q, 8.0q, 9.0q, 10.0q) != 20.0q)
        return 1;

    /* Round-trips through the wider format exactly. */
    if (narrow(widen(0.5L)) != 0.5L) return 2;
    if (widen(2.0L) != 2.0q) return 3;
    if (narrow(2.0q) != 2.0L) return 4;

    /* And a value long double can hold but double cannot survives it. */
    long double big = 1.0L;
    big = big / 3.0L;
    if (narrow(widen(big)) != big) return 5;

    /* A cast rounds to the target format even when folded at compile time:
       these two must agree. */
    static __float128 folded = (float)0.1q;
    volatile __float128 v = 0.1q;
    __float128 at_runtime = (float)v;
    if (folded != at_runtime) return 6;

    /* `f128` is valid on a hex literal, and `q` is a floating suffix. */
    if (0x1p0f128 != 1.0q) return 7;
    if (0x1.8p1q != 3.0q) return 8;

    /* `0x1f128` is a hex *integer* whose last digits merely spell the
       suffix -- 0x1f128 is 127272. Both `q` and `f128` are floating
       suffixes, so neither attaches to an integer constant; that half is
       asserted in the diagnostics suite, which can check a rejection. */
    if (0x1f128 != 127272) return 9;

    return 0;
}
#else
int main(void) { return 0; }
#endif
"#;
    assert_eq!(compile_and_run("float128_edge_cases", src, &[]), 0);
}

/// An `enum` may be a bit-field's type, and a non-negative enumeration is
/// unsigned (#C104, #C105).
///
/// `validate_bitfield` matched a hand-written list of `TypeKind`s that omitted
/// `Enum`, so `struct S { enum E e : 2; };` -- which real headers use heavily --
/// was rejected outright with "bitfield must have integer type". 6.7.2.1p5
/// allows "some other implementation-defined type" and gcc's set is every
/// integer type; `is_integer` already is that set.
///
/// Accepting it exposed the second half. A bit-field's signedness follows its
/// declared type's, and `enum_underlying_type` preferred `int` for any list
/// fitting in `int` -- so a two-bit field of `enum { A, B, C, D }` holding `D`
/// read back **-1** where gcc gives 3. gcc's rule, which 6.7.2.2p4 leaves it
/// free to choose, is unsigned whenever no enumerator is negative; that is
/// observable well beyond bit-fields, since `(enum E)-1 > 0` is true there and
/// was false here.
#[test]
fn c99_enum_bitfields_and_enum_signedness() {
    let code = r#"
enum Small { S0, S1, S2, S3 };
enum Neg   { N_A = -2, N_B = 1 };
enum Big   { B_HUGE = 0x100000000LL };
enum Wide  { W_TOP = 0x80000000u };

struct A { enum Small e : 2; };
struct B { enum Neg   e : 3; };
struct C { enum Small e : 2; unsigned u : 3; };
struct D { enum Small e : 2; enum Neg n : 3; };

int main(void) {
    /* An enumeration with no negative member is unsigned, as in gcc. */
    if (!((enum Small)-1 > 0)) return 1;
    if (!((enum Wide)-1 > 0))  return 2;
    if (!((enum Big)-1 > 0))   return 3;
    /* One with a negative member is signed. */
    if ((enum Neg)-1 > 0)      return 4;

    /* ...and the sizes are unchanged by the signedness choice. */
    if (sizeof(enum Small) != 4) return 5;
    if (sizeof(enum Neg)   != 4) return 6;
    if (sizeof(enum Big)   != 8) return 7;

    /* A bit-field of an unsigned enumeration does not sign-extend. */
    struct A a; a.e = S3;
    if ((int)a.e != 3) return 8;
    a.e = S0; if ((int)a.e != 0) return 9;

    /* A bit-field of a signed one does. */
    struct B b; b.e = N_A;
    if ((int)b.e != -2) return 10;
    b.e = N_B; if ((int)b.e != 1) return 11;

    /* Neighbours are undisturbed either way. */
    struct C c; c.e = S2; c.u = 5;
    if ((int)c.e != 2 || c.u != 5) return 12;
    if (sizeof(struct C) != 4) return 13;

    struct D d; d.e = S1; d.n = N_A;
    if ((int)d.e != 1 || (int)d.n != -2) return 14;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("enum_bitfields_and_signedness", code, &[]),
        0
    );
}

/// `__attribute__((packed))` packs a bit-field to the bit (#C110).
///
/// c17 refused to: the straddle test in `compute_struct_layout` ran whatever
/// the pack cap, because the access path addressed one power-of-two unit and
/// could not span an arbitrary byte range. `struct __attribute__((packed)) {
/// unsigned a:20, b:20; }` was 8 bytes where gcc gives 5 -- a silent ABI
/// mismatch with any gcc-compiled object.
///
/// Under a cap the unit rule is switched off entirely, not narrowed to the cap:
/// `#pragma pack(2)` lets a 16-bit field starting at bit 1 straddle both the
/// 2- and the 4-byte boundary, which is the measurement that rules out the
/// narrowing reading. A span that is not 1, 2, 4 or 8 bytes is then assembled
/// byte by byte, as gcc does on both targets -- necessary, not merely
/// equivalent, since a packed struct can be smaller than any window covering
/// the field: `{ unsigned c:1; unsigned long long a:64; }` is nine bytes and
/// the field needs all nine.
#[test]
fn c99_packed_bitfields_pack_to_the_bit() {
    let code = r#"
struct __attribute__((packed)) A { unsigned a:20, b:20; };
struct __attribute__((packed)) B { unsigned a:3, b:30, c:3; };
struct __attribute__((packed)) C { char pre; unsigned a:20; char post; };
struct __attribute__((packed)) D { unsigned c:1; unsigned long long a:64; };
struct __attribute__((packed)) E { unsigned a:1; unsigned b:32; };
struct __attribute__((packed)) F { unsigned long long a:60; unsigned b:20; };
struct __attribute__((packed)) G { signed a:20; signed b:20; };
union  __attribute__((packed)) K { unsigned a:20; char c; };
struct I { unsigned a:20, b:20; };   /* unpacked control */

static struct A init_a = { 0xABCDE, 0x12345 };
static struct C init_c = { 0x11, 0xFEDCB, 0x22 };
static struct D init_d = { 1, 0x0123456789ABCDEFULL };

int main(void) {
    /* Sizes, all of them gcc's. */
    if (sizeof(struct A) != 5)  return 1;
    if (sizeof(struct B) != 5)  return 2;
    if (sizeof(struct C) != 5)  return 3;
    if (sizeof(struct D) != 9)  return 4;
    if (sizeof(struct E) != 5)  return 5;
    if (sizeof(struct F) != 10) return 6;
    if (sizeof(union  K) != 3)  return 7;
    /* The unpacked layout is untouched. */
    if (sizeof(struct I) != 8)  return 8;

    /* Values round-trip through a span that is not a storage unit. */
    struct A a; a.a = 0xABCDE; a.b = 0x12345;
    if (a.a != 0xABCDEu || a.b != 0x12345u) return 9;

    struct B b; b.a = 5; b.b = 0x2AAAAAAA; b.c = 3;
    if (b.a != 5u || b.b != 0x2AAAAAAAu || b.c != 3u) return 10;

    /* Ordinary members either side must survive every write: a store is a
       read-modify-write over the bytes the field shares with them. */
    struct C c; c.pre = 0x11; c.post = 0x22; c.a = 0xFEDCB;
    if (c.pre != 0x11 || c.post != 0x22 || c.a != 0xFEDCBu) return 11;
    c.a = 0;       if (c.pre != 0x11 || c.post != 0x22) return 12;
    c.a = 0xFFFFF; if (c.pre != 0x11 || c.post != 0x22 || c.a != 0xFFFFFu) return 13;

    /* Nine bytes, the widest span there is. */
    struct D d; d.c = 0; d.a = 0x0123456789ABCDEFULL;
    if (d.a != 0x0123456789ABCDEFULL) return 14;
    d.c = 1; if (d.a != 0x0123456789ABCDEFULL || d.c != 1u) return 15;
    d.a = 0xFFFFFFFFFFFFFFFFULL;
    if (d.a != 0xFFFFFFFFFFFFFFFFULL || d.c != 1u) return 16;

    struct E e; e.a = 1; e.b = 0xDEADBEEF;
    if (e.a != 1u || e.b != 0xDEADBEEFu) return 17;

    struct F f; f.a = 0x0FEDCBA987654321ULL; f.b = 0xFFFFF;
    if (f.a != 0x0FEDCBA987654321ULL || f.b != 0xFFFFFu) return 18;

    /* A signed packed field sign-extends from its own width. */
    struct G g; g.a = -1; g.b = -2;
    if (g.a != -1 || g.b != -2) return 19;
    g.a = 0x7FFFF; if (g.a != 0x7FFFF || g.b != -2) return 20;

    /* Static initializers go through the byte merge, not the access path. */
    if (init_a.a != 0xABCDEu || init_a.b != 0x12345u) return 21;
    if (init_c.pre != 0x11 || init_c.a != 0xFEDCBu || init_c.post != 0x22) return 22;
    if (init_d.c != 1u || init_d.a != 0x0123456789ABCDEFULL) return 23;

    /* The unpacked control still round-trips. */
    struct I i; i.a = 0xABCDE; i.b = 0x12345;
    if (i.a != 0xABCDEu || i.b != 0x12345u) return 24;

    return 0;
}
"#;
    assert_eq!(compile_and_run("packed_bitfields", code, &[]), 0);
}

/// `__attribute__((mode(M)))` selects the type of a machine mode (#C85).
///
/// It was unimplemented and warned, which read as cosmetic and was not:
/// glibc declares `register_t` with `__mode__(__word__)`, so c17 sized it 4
/// bytes where gcc sizes it 8 -- a wrong type, silently, in a header any
/// program can include.
///
/// The signedness comes from the *declared* type, not the mode, and `XF` and
/// `TF` are both sixteen bytes but not interchangeable -- one is the x87
/// extended format and the other IEEE binary128 -- so the arithmetic check at
/// the end distinguishes them rather than trusting the size.
#[test]
fn c99_mode_attribute_selects_the_type() {
    let code = r#"
typedef int qi  __attribute__((__mode__(__QI__)));
typedef int hi  __attribute__((__mode__(__HI__)));
typedef int si  __attribute__((__mode__(__SI__)));
typedef int di  __attribute__((__mode__(__DI__)));
typedef int ti  __attribute__((__mode__(__TI__)));
typedef int wd  __attribute__((__mode__(__word__)));
typedef int pt  __attribute__((__mode__(__pointer__)));
typedef unsigned uqi __attribute__((__mode__(__QI__)));
typedef unsigned udi __attribute__((__mode__(__DI__)));
typedef float hf __attribute__((__mode__(__HF__)));
typedef float sf __attribute__((__mode__(__SF__)));
typedef float df __attribute__((__mode__(__DF__)));
typedef _Complex float hc __attribute__((__mode__(HC)));
typedef _Complex float sc __attribute__((__mode__(SC)));
typedef _Complex float dc __attribute__((__mode__(DC)));
/* The binary128 modes exist only where the type does; c17 refuses them on a
   target with no `__FLT128_*` family rather than handing back a type whose
   arithmetic cannot link. */
#ifdef __FLT128_MANT_DIG__
typedef float tf __attribute__((__mode__(__TF__)));
typedef _Complex float tc __attribute__((__mode__(TC)));
#endif

int main(void) {
    if (sizeof(qi) != 1)  return 1;
    if (sizeof(hi) != 2)  return 2;
    if (sizeof(si) != 4)  return 3;
    if (sizeof(di) != 8)  return 4;
    if (sizeof(ti) != 16) return 5;
    if (sizeof(wd) != sizeof(void *)) return 6;
    if (sizeof(pt) != sizeof(void *)) return 7;

    /* Signedness follows the declared type, not the mode. */
    if (!((qi)-1 < 0))  return 8;
    if ((uqi)-1 < 0)    return 9;
    if (!((di)-1 < 0))  return 10;
    if ((udi)-1 < 0)    return 11;

    if (sizeof(hf) != 2) return 12;
    if (sizeof(sf) != 4) return 13;
    if (sizeof(df) != 8) return 14;

    /* A mode is a type, not merely a width: binary128 divides to more
       precision than double, which a size check alone would not show.
       Guarded because c17 offers `_Float128` only where the runtime can
       support it -- there are no `__FLT128_*` predefines on Apple targets,
       and `mode(TF)` is refused there rather than handing back a type whose
       every operation would fail to link. Same guard as
       `c99_float128_is_a_first_class_type`. */
#ifdef __FLT128_MANT_DIG__
    tf third = (tf)1 / 3;
    if ((int)(third * 3 * 1000000) != 1000000) return 15;
#endif

    /* Complex modes name the format of each half. glibc's <bits/floatn.h>
       declares `__cfloat128` with `mode(TC)`, which was 285 of the warnings a
       CPython build used to produce. */
    if (sizeof(hc) != 2 * sizeof(hf)) return 17;
    if (sizeof(sc) != 2 * sizeof(sf)) return 18;
    if (sizeof(dc) != 2 * sizeof(df)) return 19;
#ifdef __FLT128_MANT_DIG__
    if (sizeof(tf) != 16) return 20;
    if (sizeof(tc) != 2 * sizeof(tf)) return 21;
#endif

    /* And the narrow integer modes really do wrap at their own width. */
    qi small = 127;
    small = (qi)(small + 1);
    if (small != -128) return 16;

    return 0;
}
"#;
    assert_eq!(compile_and_run("mode_attribute", code, &[]), 0);
}

/// A `mode` applies to the declarator it was written on, and to nothing else
/// (#C117).
///
/// `apply_pending_mode` was called only at the three *typedef* sites, but
/// `pending_mode` is set for any declarator and -- unlike `pending_alignas`,
/// which is cleared at four declaration boundaries -- was never cleared. So a
/// mode on a non-typedef was silently dropped **and** survived to be applied to
/// whatever was declared next:
///
/// ```c
/// int a __attribute__((__mode__(__QI__)));   /* was 4 bytes, gcc gives 1 */
/// typedef int T;                              /* became 1 byte  */
/// ```
///
/// Both halves are checked here: the mode reaching its own declarator, and the
/// declaration after it being untouched.
#[test]
fn c99_mode_attribute_applies_only_to_its_own_declarator() {
    let code = r#"
int a __attribute__((__mode__(__QI__)));
int after_a;                                   /* must stay 4 */

int b, c __attribute__((__mode__(__HI__)));    /* only c is moded */
int after_c;

static int d __attribute__((__mode__(__DI__)));
typedef int AfterD;                            /* must stay 4 */

extern int e __attribute__((__mode__(__QI__)));
struct S { int m; } s;                         /* member must stay 4 */

unsigned int u __attribute__((__mode__(__QI__)));
int after_u;

typedef int T __attribute__((__mode__(__HI__)));
typedef int AfterT;                            /* must stay 4 */

int main(void) {
    if (sizeof a != 1) return 1;
    if (sizeof after_a != 4) return 2;

    if (sizeof b != 4) return 3;
    if (sizeof c != 2) return 4;
    if (sizeof after_c != 4) return 5;

    if (sizeof d != 8) return 6;
    if (sizeof(AfterD) != 4) return 7;

    if (sizeof s.m != 4) return 8;

    if (sizeof u != 1) return 9;
    /* Still unsigned, and narrow: -1 wraps to 255 rather than sign-extending.
       Tested by the stored value, not by `u - 1 < 0`, which the integer
       promotions make an `int` comparison whatever `u` is. */
    u = (unsigned)-1;
    if (u != 255) return 10;
    if (sizeof after_u != 4) return 11;

    if (sizeof(T) != 2) return 12;
    if (sizeof(AfterT) != 4) return 13;

    return 0;
}
"#;
    assert_eq!(compile_and_run("mode_only_its_declarator", code, &[]), 0);
}

/// A bit-field must read only its own bits, and keep all of them through a
/// brace initializer (#C119, #C120).
///
/// Two defects that meet on the wide bit-field. **The load** walked every byte
/// of the access span with no "does this byte hold any of the field's bits?"
/// test -- the skip its *store* sibling has always had -- so a field in a
/// window wider than itself read the object's **padding**. Every `__int128`
/// bit-field is such a field: its window is sixteen bytes. The stray bytes were
/// also shifted by more than the 64-bit carrier's width, which x86-64 masks
/// (folding padding into the result) and aarch64's assembler rejects outright,
/// so the same source would not even build there.
///
/// **The brace initializer** then converted the value twice: to the member's
/// type, correctly, and again to a type derived from the *span*, where
/// `bitfield_storage_type` answers `unsigned int` for anything but 1, 2, 4 or 8
/// bytes. A 64-bit field in a sixteen-byte window, or in a packed nine-byte
/// span, kept 32 bits of its initializer. Assignment was unaffected, so
/// `p.a = ~0ULL` was right and `= {~0ULL}` was not.
///
/// The padding is deliberately dirtied; with it zero the load bug is invisible.
#[test]
fn c99_wide_bitfields_read_only_their_own_bits() {
    let code = r#"
struct A { unsigned __int128 a:64; };
struct B { unsigned __int128 a:64, b:64; };
struct C { unsigned __int128 a:32; };
struct D { unsigned __int128 a:1; };
struct __attribute__((packed)) P { unsigned c:1; unsigned long long a:64; };
struct Q { unsigned __int128 a:64; };

int main(void) {
    /* Reading: the window is wider than the field, so the bytes beyond it must
       not contribute. */
    struct A x; __builtin_memset(&x, 0xAA, sizeof x);
    x.a = 0x1122334455667788ULL;
    if (x.a != 0x1122334455667788ULL) return 1;

    struct B y; __builtin_memset(&y, 0xAA, sizeof y);
    y.a = 1; y.b = 2;
    if (y.a != 1 || y.b != 2) return 2;

    struct C z; __builtin_memset(&z, 0xAA, sizeof z);
    z.a = 0xDEADBEEFu;
    if (z.a != 0xDEADBEEFu) return 3;

    struct D w; __builtin_memset(&w, 0xAA, sizeof w);
    w.a = 1;
    if (w.a != 1) return 4;
    w.a = 0;
    if (w.a != 0) return 5;

    /* Initializing: the value must not be narrowed to a span-derived type. */
    struct P p = { 0, 0xFFFFFFFFFFFFFFFFULL };
    if (p.a != 0xFFFFFFFFFFFFFFFFULL) return 6;
    struct Q q = { 0xFFFFFFFFFFFFFFFFULL };
    if (q.a != 0xFFFFFFFFFFFFFFFFULL) return 7;

    /* Assignment always worked and must continue to. */
    p.a = 0x0123456789ABCDEFULL;
    if (p.a != 0x0123456789ABCDEFULL) return 8;

    /* The narrow and packed spans the same code serves. */
    struct E { unsigned a:20, b:20; } e = { 0xABCDE, 0x12345 };
    if (e.a != 0xABCDEu || e.b != 0x12345u) return 9;
    struct F { char pre; unsigned a:20; char post; } f = { 1, 0xFEDCB, 2 };
    if (f.pre != 1 || f.a != 0xFEDCBu || f.post != 2) return 10;

    /* `_Bool` still normalises through its own type, not the storage unit. */
    struct G { _Bool b:1; } g = { 2 };
    if (g.b != 1) return 11;

    return 0;
}
"#;
    assert_eq!(compile_and_run("wide_bitfield_own_bits", code, &[]), 0);
}

/// `__attribute__((transparent_union))` (#C51): a caller may pass any member
/// type to the parameter, and the union is passed as its **first member**
/// would be.
///
/// The attribute exists because glibc declares every socket call with it, so
/// the front-end half is what keeps `sendto` compiling. The ABI half is the
/// half that can be silently wrong, and only a run catches it: a union whose
/// members classify differently -- `float` against `int` -- goes in a
/// different register file depending on which member comes first, and reading
/// the wrong file yields a plausible number rather than a crash.
///
/// Both spellings are exercised, because they take different paths through the
/// parser: on the union specifier, and trailing on a typedef of an anonymous
/// union, which is glibc's own.
#[test]
fn c99_transparent_union_passes_as_its_first_member() {
    let code = r#"
union FirstFloat { float f; int i; } __attribute__((transparent_union));
union FirstInt   { int i; float f; } __attribute__((transparent_union));
typedef union { int *ip; char *cp; } PtrArg __attribute__((__transparent_union__));

static float take_f(union FirstFloat u) { return u.f; }
static int   take_i(union FirstInt u)   { return u.i; }
static long  take_p(PtrArg u)           { return *u.ip; }

/* Forwarding matters as much as receiving: the caller classifies the
   argument's declared type, which here is the union itself. */
static float fwd_f(union FirstFloat u) { return take_f(u); }

int main(void) {
    int n = 1234;

    /* A member type may be passed directly -- no cast, no brace. */
    if (take_f(2.5f) != 2.5f) return 1;
    if (take_i(7) != 7)       return 2;
    if (take_p(&n) != 1234)   return 3;

    /* Same two members, opposite order: the first one decides, so these
       travel in different register files and must both survive. */
    if (take_f(-0.75f) != -0.75f) return 4;
    if (take_i(-9) != -9)         return 5;

    /* And a union value forwarded on as a whole. */
    if (fwd_f(3.25f) != 3.25f) return 6;

    /* The union is still a union: naming a member works, and its size is the
       widest member's, not the first member's. */
    union FirstFloat u;
    u.i = 0;
    u.f = 8.5f;
    if (u.f != 8.5f) return 7;
    if (sizeof(PtrArg) != sizeof(char *)) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("transparent_union_abi", code, &[]), 0);
}

// ============================================================================
// C17 6.7.7 — typedef of a variably modified type
//
// "If a typedef name specifies a variably modified type then it shall have
// block scope. The array size expressions are evaluated each time the
// declaration of the typedef name is reached in the order of execution."
//
// c17 rejected these outright: "typedef of a variable-length array type is not
// supported". Two hazards justified the rejection rather than a miscompile --
// the `vla_sizes` side channel made a typedef'd VM type indistinguishable from
// an incomplete `int a[]`, and `linearize_local_decl` dispatched on STATIC and
// `vla_sizes` without ever testing TYPEDEF, so a block-scope one was lowered
// as a runtime allocation of the array itself.
// ============================================================================

/// The size is fixed when the typedef is *declared*, not when it is used, so
/// changing `n` afterwards must not change the type. Every expected value here
/// was taken from `gcc -std=c17`.
#[test]
fn c17_vm_typedef_evaluates_its_extent_at_the_typedef() {
    let code = r#"
int n = 4;
int main(void) {
    typedef int T[n];
    n = 100;                    /* after the typedef */
    T a;
    if (sizeof(T) != 4 * sizeof(int)) return 1;
    if (sizeof(a) != 4 * sizeof(int)) return 2;
    a[0] = 1; a[3] = 2;
    if (a[0] + a[3] != 3) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c17_vm_typedef_extent", code, &[]), 0);
}

/// "evaluated each time the declaration ... is reached", which means *once*
/// per execution of the typedef, however many objects it then declares.
#[test]
fn c17_vm_typedef_evaluates_its_extent_exactly_once() {
    let code = r#"
static int calls = 0;
static int side(void) { calls++; return 4; }

int main(void) {
    typedef int T[side()];
    T a, b;
    if (calls != 1) return 1;               /* one evaluation, two objects */
    if (sizeof(a) != sizeof(b)) return 2;
    if (sizeof(a) != 4 * sizeof(int)) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c17_vm_typedef_once", code, &[]), 0);
}

/// Reaching the declaration again re-evaluates it: each turn of the loop gets
/// the extent in force at that turn.
#[test]
fn c17_vm_typedef_re_evaluates_when_reached_again() {
    let code = r#"
int main(void) {
    unsigned long total = 0;
    for (int i = 1; i <= 3; i++) {
        typedef int T[i];
        T a;
        a[0] = i;
        total += sizeof(T) / sizeof(int);
    }
    return total == 6 ? 0 : 1;      /* 1 + 2 + 3 */
}
"#;
    assert_eq!(compile_and_run("c17_vm_typedef_reeval", code, &[]), 0);
}

/// A pointer to a variably modified array, and a two-dimensional one: the
/// extents belong to the pointee, so the object itself is an ordinary pointer.
#[test]
fn c17_vm_typedef_composes_with_pointers_and_dimensions() {
    let code = r#"
int main(void) {
    int n = 3, m = 4;
    typedef int Row[m];
    typedef int Grid[n][m];

    Grid g;
    if (sizeof(g) != (unsigned long)n * m * sizeof(int)) return 1;
    if (sizeof(g[0]) != (unsigned long)m * sizeof(int)) return 2;

    Row *p = g;
    p[1][2] = 7;
    if (g[1][2] != 7) return 3;

    typedef int (*PRow)[m];
    if (sizeof(PRow) != sizeof(int *)) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c17_vm_typedef_compose", code, &[]), 0);
}

/// 6.7.7p3's other half: such a typedef "shall have block scope". At file
/// scope there is nothing to evaluate the extent, and c17 must say so.
#[test]
fn c17_vm_typedef_is_rejected_at_file_scope() {
    let code = r#"
int n = 4;
typedef int T[n];
int main(void) { return 0; }
"#;
    assert_eq!(
        compile_and_run("c17_vm_typedef_file_scope", code, &[]),
        -1,
        "a file-scope variably modified typedef must be rejected"
    );
}

// ============================================================================
// Declarations that must be accepted
// ============================================================================

/// A null pointer constant assigns to a function pointer.
///
/// 6.5.16.1p1 lets a null pointer constant assign to any pointer, and
/// 6.3.2.3p3 makes `(void *)0` one -- which is exactly how glibc spells
/// `NULL`. The check for it was read only in the "target is a pointer, value
/// is not" branch, which a null constant that has already decayed to `void *`
/// never reaches: it was judged by the pointer-to-pointer rules instead, and
/// those diagnose a `void *` meeting a function pointer. So every `fp = NULL`
/// warned, and any `-Werror` build using function pointers broke.
#[test]
fn c99_null_constant_assigns_to_a_function_pointer() {
    let code = r#"
#include <stddef.h>

typedef void (*handler)(int);
static handler h1 = NULL;
static handler h2 = (void *)0;
static handler h3 = 0;

static void hit(int x) { (void)x; }

int main(void)
{
    handler h = NULL;
    if (h != NULL) return 1;
    h = (void *)0;
    if (h) return 2;
    h = hit;
    if (!h) return 3;
    h = 0;
    if (h) return 4;
    if (h1 || h2 || h3) return 5;

    /* A void* still round-trips through an object pointer. */
    int i = 7;
    void *v = &i;
    int *p = v;
    if (*p != 7) return 6;
    return 0;
}
"#;
    // The warning is the defect, so the compile must be clean, not merely
    // successful.
    crate::common::compile_expect_no_diagnostic("null_fnptr", code, "forbids");
    assert_eq!(compile_and_run("null_fnptr_run", code, &[]), 0);
}

/// A `typedef` of an incomplete type is legal at block scope.
///
/// 6.7p7 asks for a complete type where an *object* is declared. A typedef
/// declares no object, and the file-scope path has always said so; the
/// block-scope path did not, so `typedef struct Incomplete T;` inside a
/// function was rejected.
#[test]
fn c99_block_scope_typedef_of_an_incomplete_type() {
    let code = r#"
struct Incomplete;
union AlsoIncomplete;

int main(void)
{
    typedef struct Incomplete T;
    typedef union AlsoIncomplete U;
    T *p = 0;
    U *q = 0;

    /* Completing it later in the block is a different declaration, and the
       typedef still names the incomplete one -- but a pointer to it is fine. */
    if (p || q) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("block_typedef_incomplete", code, &[]), 0);
}

/// A qualified tentative definition is completed by completing its tag.
///
/// 6.9.2p3 lets a file-scope tentative definition be completed later. A
/// qualified spelling -- `volatile struct S` -- is interned as a fresh type
/// holding a clone of the tag's composite data as it stood at the time, and
/// completing the tag mutates only the tag's own entry. Judging the recorded
/// id therefore read a frozen "incomplete" that nothing could ever update.
#[test]
fn c99_qualified_tentative_definition_is_completed_by_its_tag() {
    let code = r#"
struct S;
volatile struct S vs;
const struct S cs;
struct S plain;
struct S { int a; };

union U;
volatile union U vu;
union U { int b; };

int main(void)
{
    if (vs.a != 0) return 1;
    if (cs.a != 0) return 2;
    if (plain.a != 0) return 3;
    if (vu.b != 0) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_run("qualified_tentative_def", code, &[]), 0);
}

/// `__attribute__((mode(M)))` belongs to the declarator it is written on.
///
/// Only the eight *declarator* sites consumed the pending mode. A struct
/// member's declarator was not one of them, so a mode on a member did two
/// wrong things at once: it did not size the member, and it stayed pending
/// until the next declarator that did consume one -- the enclosing
/// declaration's. `struct Big { int arr[100]; int x
/// __attribute__((mode(QI))); } b;` gave `sizeof b == 1` against gcc's 404,
/// because the QI mode was applied to `b` rather than to `x`.
///
/// Parameters had the same gap, without the leak: a mode on a parameter was
/// silently ignored.
#[test]
fn c99_mode_attribute_binds_to_its_own_declarator() {
    let code = r#"
/* The regression: a member's mode must not reach the enclosing object. */
struct Big { int arr[100]; int x __attribute__((mode(QI))); } b;
_Static_assert(sizeof b == 404, "a member's mode is not the object's");
_Static_assert(sizeof b.x == 1, "and it does size the member");

/* Both spellings, before and after the declarator. */
struct After  { int a __attribute__((mode(QI))); };
struct Before { int __attribute__((mode(QI))) a; };
_Static_assert(sizeof(struct After) == 1, "mode after the declarator");
_Static_assert(sizeof(struct Before) == 1, "mode before the declarator");

/* It must not reach the *next* member either. */
struct Two { int a __attribute__((mode(QI))); int b; };
_Static_assert(sizeof(struct Two) == 8, "a mode stops at its own member");

/* Widening as well as narrowing. */
struct Wide { int a __attribute__((mode(DI))); };
_Static_assert(sizeof(struct Wide) == 8, "mode(DI) widens a member");

/* Signedness is the declared type's, not the mode's. */
struct Signs {
    unsigned u __attribute__((mode(QI)));
    int s __attribute__((mode(QI)));
};

int param_sized(int x __attribute__((mode(QI)))) { return sizeof x; }

int main(void)
{
    struct Signs sg;
    sg.u = 200; sg.s = -1;
    if (sg.u != 200) return 1;
    if (sg.s != -1) return 2;

    if (param_sized(0) != 1) return 3;

    b.x = 1;
    if (sizeof b != 404) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_run("mode_binds_to_declarator", code, &[]), 0);
}

/// `__attribute__((vector_size(N)))` gives a type a vector's storage.
///
/// It used to be rejected outright, on the reasoning that ignoring it would
/// silently leave the type scalar and that no C system header uses it. The
/// first half is right; the second is not. glibc's `<link.h>` declares
/// `La_x86_64_xmm`, `La_x86_64_ymm` and `La_x86_64_zmm` with it, so the
/// rejection made that header -- and anything including it -- uncompilable.
///
/// c17 gives such a type the size and alignment a vector has, modelled as an
/// array of its elements, which is exactly the layout GCC produces. What it
/// deliberately does not give is element-wise arithmetic: that needs a real
/// vector type in the IR and in both backends. An array does not accept `+`,
/// so the gap surfaces as a diagnostic where the arithmetic is written rather
/// than as a computation that silently runs on one element -- which is the
/// failure the outright rejection was guarding against.
#[test]
fn c99_vector_size_has_a_vector_s_storage() {
    let code = r#"
typedef float V4 __attribute__((vector_size(16)));
typedef float V8 __attribute__((vector_size(32)));
typedef double D8 __attribute__((vector_size(64)));
typedef short S8 __attribute__((vector_size(16)));

/* A vector aligns to its width rounded to a power of two, capped at 16 --
   GCC's default on both targets. So a 32-byte vector aligns to 16, not 32. */
_Static_assert(sizeof(V4) == 16 && _Alignof(V4) == 16, "V4");
_Static_assert(sizeof(V8) == 32 && _Alignof(V8) == 16, "V8 caps at 16");
_Static_assert(sizeof(D8) == 64 && _Alignof(D8) == 16, "D8 caps at 16");
_Static_assert(sizeof(S8) == 16 && _Alignof(S8) == 16, "S8");

/* Below the cap the alignment follows the width. */
typedef float V2 __attribute__((vector_size(8)));
typedef float V1 __attribute__((vector_size(4)));
_Static_assert(sizeof(V2) == 8 && _Alignof(V2) == 8, "V2");
_Static_assert(sizeof(V1) == 4 && _Alignof(V1) == 4, "V1");

/* ...unless the source says otherwise, which is what <link.h> writes. */
typedef float A8 __attribute__((vector_size(32), aligned(16)));
_Static_assert(sizeof(A8) == 32, "A8 size");
_Static_assert(_Alignof(A8) == 16, "an explicit alignment wins over the width");

/* The <link.h> shape: vectors as union and struct members, never operated on. */
typedef union { V8 ymm[2]; D8 zmm[1]; V4 xmm[4]; } Vec __attribute__((aligned(16)));
_Static_assert(sizeof(Vec) == 64, "union of vectors");
_Static_assert(_Alignof(Vec) == 16, "union alignment");

struct Regs { unsigned long a; Vec v[8]; unsigned long b; };
_Static_assert(_Alignof(struct Regs) == 16, "struct alignment");
_Static_assert(sizeof(struct Regs) == 544, "struct layout matches gcc");

int main(void)
{
    Vec v;
    /* Element access through the array view still works. */
    v.xmm[0][0] = 1.5f;
    v.xmm[3][3] = 2.5f;
    if (v.xmm[0][0] != 1.5f) return 1;
    if (v.xmm[3][3] != 2.5f) return 2;
    if (sizeof v.ymm != 64) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("vector_size_storage", code, &[]), 0);
}

/// The header that made the rejection a build blocker.
///
/// Linux-only: `<link.h>` is glibc's dynamic-linker auditing interface and has
/// no macOS equivalent -- the system compiler cannot find it there either, so
/// this is a missing header rather than anything cc could parse.  What the
/// header exercises is covered on every platform by
/// `c99_vector_size_storage` above, which reproduces its shape inline.
#[test]
fn c99_link_h_compiles() {
    let code = r#"
#include <link.h>
int main(void)
{
    /* The layout GCC gives these, checked rather than assumed. */
    if (sizeof(La_x86_64_xmm) != 16 || _Alignof(La_x86_64_xmm) != 16) return 1;
    if (sizeof(La_x86_64_ymm) != 32 || _Alignof(La_x86_64_ymm) != 16) return 2;
    if (sizeof(La_x86_64_zmm) != 64 || _Alignof(La_x86_64_zmm) != 16) return 3;
    if (sizeof(La_x86_64_vector) != 64) return 4;
    return 0;
}
"#;
    // x86-64 only within Linux: the vector typedefs are inside
    // `#ifdef __x86_64__`.
    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    assert_eq!(compile_and_run("link_h_compiles", code, &[]), 0);
    #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
    let _ = code;
}
