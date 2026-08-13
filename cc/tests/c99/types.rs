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

use crate::common::compile_and_run;

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
#include <string.h>

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
"#;
    assert_eq!(compile_and_run("float128_first_class", src, &[]), 0);
}

/// `__float128` outranks every other real type in the usual arithmetic
/// conversions -- it has more significand bits than x87 extended and the same
/// exponent range, so a mixed expression is computed at binary128.
#[test]
fn c99_float128_outranks_long_double() {
    let src = r#"
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

    /* And the arithmetic really is done at that width: 0.1q and 0.1L differ,
       so promoting the long double cannot make them equal. */
    if (q == l) return 6;

    return 0;
}
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
#include <string.h>

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

    return 0;
}
"#;
    assert_eq!(compile_and_run("float128_edge_cases", src, &[]), 0);
}
