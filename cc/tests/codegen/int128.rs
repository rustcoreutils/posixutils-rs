//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// __int128: arithmetic, shifts, conversions and passing.
//

use crate::common::{
    compile_and_run, compile_and_run_aarch64, compile_and_run_everywhere, compile_and_run_optimized,
};

/// Test __int128 / __uint128_t / __int128_t type support.
/// Validates sizeof, alignof, struct members, arrays, and pointer declarations.
#[test]
fn codegen_int128_basic() {
    let code = r#"
int main(void) {
    /* sizeof checks for all spelling variants */
    if (sizeof(__uint128_t) != 16) return 1;
    if (sizeof(__int128_t) != 16) return 2;
    if (sizeof(__int128) != 16) return 3;

    /* Struct member usage (mirrors macOS mach/arm/_structs.h) */
    struct { __uint128_t v[4]; } regs;
    if (sizeof(regs) != 64) return 4;

    /* Array of __int128 */
    __int128 arr[3];
    if (sizeof(arr) != 48) return 5;

    /* Alignment check */
    if (__alignof__(__int128) != 16) return 6;
    if (__alignof__(__uint128_t) != 16) return 7;

    /* Pointer to __int128 */
    __int128 val;
    __int128 *p = &val;
    if (sizeof(*p) != 16) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("int128_basic", code, &[]), 0);
}

// ============================================================================
// __int128 codegen tests
// ============================================================================

#[test]
fn codegen_int128_mega() {
    let code = r#"
typedef unsigned __int128 uint128;
typedef __int128 int128;

int main(void) {
    // ========== ADD/SUB (returns 1-9) ==========
    {
        int128 a = 100;
        int128 b = 200;
        int128 c = a + b;
        if (c != 300) return 1;
        int128 d = b - a;
        if (d != 100) return 2;
        uint128 big = (uint128)1 << 63;
        uint128 sum = big + big;
        if ((unsigned long long)(sum >> 64) != 1) return 3;
        if ((unsigned long long)sum != 0) return 4;
        uint128 one_hi = (uint128)1 << 64;
        uint128 diff = one_hi - 1;
        if ((unsigned long long)diff != (unsigned long long)-1) return 5;
        if ((unsigned long long)(diff >> 64) != 0) return 6;
    }
    // ========== BITWISE (returns 10-19) ==========
    {
        int128 a = 0xFF00FF00;
        int128 b = 0x0F0F0F0F;
        if ((a & b) != 0x0F000F00) return 10;
        if ((a | b) != 0xFF0FFF0F) return 11;
        if ((a ^ b) != 0xF00FF00F) return 12;
    }
    // ========== MULTIPLY (returns 20-29) ==========
    {
        int128 a = 1000000000LL;
        int128 b = 1000000000LL;
        int128 c = a * b;
        if (c != 1000000000000000000LL) return 20;
        uint128 x = (uint128)1 << 63;
        uint128 y = 4;
        uint128 z = x * y;
        if ((unsigned long long)(z >> 64) != 2) return 21;
        if ((unsigned long long)z != 0) return 22;
    }
    // ========== SHIFTS (returns 30-49) ==========
    {
        int128 a = 1;
        int128 b = a << 10;
        if (b != 1024) return 30;
        uint128 c = (uint128)1 << 64;
        if ((unsigned long long)c != 0) return 31;
        if ((unsigned long long)(c >> 64) != 1) return 32;
        uint128 d = (uint128)1 << 100;
        if ((unsigned long long)d != 0) return 33;
        if ((unsigned long long)(d >> 64) != ((unsigned long long)1 << 36)) return 34;
        uint128 e = (uint128)1 << 100;
        uint128 f = e >> 36;
        if ((unsigned long long)f != 0) return 35;
        if ((unsigned long long)(f >> 64) != 1) return 36;
        uint128 g = (uint128)1 << 100;
        uint128 h = g >> 100;
        if (h != 1) return 37;
        int128 i = -1;
        int128 j = i >> 1;
        if (j != -1) return 38;
        int k = 10;
        int128 m = (int128)1 << k;
        if (m != 1024) return 39;
    }
    // ========== COMPARISONS (returns 50-69) ==========
    {
        int128 a = 100;
        int128 b = 200;
        int128 c = 100;
        if (a != c) return 50;
        if (a == b) return 51;
        if (a < b) { } else return 54;
        if (b < a) return 55;
        if (a <= c) { } else return 56;
        if (a <= b) { } else return 57;
        if (b <= a) return 58;
        if (b > a) { } else return 59;
        if (a > b) return 60;
        if (a >= c) { } else return 61;
        if (a >= b) return 62;
        uint128 big1 = (uint128)1 << 100;
        uint128 big2 = (uint128)2 << 100;
        if (big1 < big2) { } else return 63;
        if (big2 < big1) return 64;
        int128 neg = -1;
        int128 pos = 1;
        if (neg < pos) { } else return 65;
        if (pos < neg) return 66;
    }
    // ========== UNARY (returns 70-79) ==========
    {
        int128 a = 42;
        int128 b = -a;
        if (b != -42) return 70;
        int128 c2 = 0;
        int128 d2 = -c2;
        if (d2 != 0) return 71;
        uint128 e = 0;
        uint128 f = ~e;
        if ((unsigned long long)f != (unsigned long long)-1) return 72;
        if ((unsigned long long)(f >> 64) != (unsigned long long)-1) return 73;
    }
    // ========== EXTEND/TRUNC (returns 80-89) ==========
    {
        unsigned int small_u = 0xDEADBEEF;
        uint128 big_u = small_u;
        if ((unsigned long long)big_u != 0xDEADBEEF) return 80;
        if ((unsigned long long)(big_u >> 64) != 0) return 81;
        int small_s = -1;
        int128 big_s = small_s;
        if (big_s != -1) return 82;
        int128 large = 0x1234567890ABCDEFLL;
        int trunc = (int)large;
        if (trunc != (int)0x90ABCDEF) return 83;
        unsigned long long trunc_ll = (unsigned long long)large;
        if (trunc_ll != 0x1234567890ABCDEFULL) return 84;
    }
    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_int128_mega", code, &[]), 0);
}

#[test]
fn codegen_int128_param_return() {
    let code = r#"
typedef unsigned __int128 uint128;
typedef __int128 int128;

/* Function that takes __int128 params and returns __int128 */
uint128 add128(uint128 a, uint128 b) {
    return a + b;
}

/* Function that takes multiple __int128 params */
int128 select128(int128 a, int128 b, int c) {
    if (c) return a;
    return b;
}

/* Function with mixed param types including __int128 */
long mixed_params(int x, uint128 big, long y) {
    /* Extract lo 64 bits of big */
    long lo = (long)big;
    return x + lo + y;
}

/* Return __int128 with specific hi and lo halves */
uint128 make128(unsigned long lo, unsigned long hi) {
    return ((uint128)hi << 64) | lo;
}

int main(void) {
    /* Test 1: __int128 param passed correctly (both halves) */
    uint128 a = ((uint128)0xDEADBEEFULL << 64) | 0x12345678ULL;
    uint128 b = 1;
    uint128 c = add128(a, b);
    unsigned long lo = (unsigned long)c;
    unsigned long hi = (unsigned long)(c >> 64);
    if (lo != 0x12345679ULL) return 1;
    if (hi != 0xDEADBEEFULL) return 2;

    /* Test 2: __int128 return value has correct hi half */
    uint128 d = make128(0xAAAAULL, 0xBBBBULL);
    lo = (unsigned long)d;
    hi = (unsigned long)(d >> 64);
    if (lo != 0xAAAAULL) return 3;
    if (hi != 0xBBBBULL) return 4;

    /* Test 3: multiple __int128 params */
    int128 x = ((int128)0x1111ULL << 64) | 0x2222ULL;
    int128 y = ((int128)0x3333ULL << 64) | 0x4444ULL;
    int128 r = select128(x, y, 1);
    if ((unsigned long)r != 0x2222ULL) return 5;
    if ((unsigned long)((uint128)r >> 64) != 0x1111ULL) return 6;
    r = select128(x, y, 0);
    if ((unsigned long)r != 0x4444ULL) return 7;
    if ((unsigned long)((uint128)r >> 64) != 0x3333ULL) return 8;

    /* Test 4: mixed params — __int128 between regular args */
    uint128 big = 100;
    long result = mixed_params(10, big, 20);
    if (result != 130) return 9;

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_int128_param_return", code, &[]), 0);
}

#[test]
fn codegen_int128_param_return_optimized() {
    let code = r#"
typedef unsigned __int128 uint128;

uint128 add128(uint128 a, uint128 b) {
    return a + b;
}

uint128 make128(unsigned long lo, unsigned long hi) {
    return ((uint128)hi << 64) | lo;
}

int main(void) {
    uint128 a = ((uint128)0xDEADBEEFULL << 64) | 0x12345678ULL;
    uint128 b = 1;
    uint128 c = add128(a, b);
    unsigned long lo = (unsigned long)c;
    unsigned long hi = (unsigned long)(c >> 64);
    if (lo != 0x12345679ULL) return 1;
    if (hi != 0xDEADBEEFULL) return 2;

    uint128 d = make128(0xAAAAULL, 0xBBBBULL);
    lo = (unsigned long)d;
    hi = (unsigned long)(d >> 64);
    if (lo != 0xAAAAULL) return 3;
    if (hi != 0xBBBBULL) return 4;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run_optimized("codegen_int128_param_return_opt", code),
        0
    );
}

#[test]
fn codegen_int128_ternary() {
    let code = r#"
typedef unsigned __int128 uint128;

uint128 pick(uint128 a, uint128 b, int cond) {
    return cond ? a : b;
}

int main(void) {
    uint128 a = ((uint128)0xDEADULL << 64) | 0xBEEFULL;
    uint128 b = ((uint128)0xCAFEULL << 64) | 0xBABEULL;

    /* Function call path */
    uint128 r = pick(a, b, 1);
    if (r != a) return 1;
    r = pick(a, b, 0);
    if (r != b) return 2;

    /* Pure ternary (inline) */
    r = (a > b) ? a : b;
    if (r != a) return 3;
    r = (a < b) ? a : b;
    if (r != b) return 4;

    /* Ternary with same-value check */
    r = (1) ? a : b;
    if (r != a) return 5;
    r = (0) ? a : b;
    if (r != b) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_int128_ternary", code, &[]), 0);
}

#[test]
fn codegen_int128_many_args() {
    let code = r#"
typedef unsigned __int128 uint128;

/* 4 int128 params: on x86_64, 3 fit in GP regs (6 regs / 2 = 3), 4th spills */
uint128 sum4(uint128 a, uint128 b, uint128 c, uint128 d) {
    return a + b + c + d;
}

/* 5 int128 params to stress stack args further */
uint128 sum5(uint128 a, uint128 b, uint128 c, uint128 d, uint128 e) {
    return a + b + c + d + e;
}

int main(void) {
    /* Use int128 variables to ensure correct arg types at call site */
    uint128 v1 = 1, v2 = 2, v3 = 3, v4 = 4, v5 = 5;

    /* Basic small values */
    uint128 r = sum4(v1, v2, v3, v4);
    if (r != 10) return 1;

    /* With hi-word values */
    uint128 big = (uint128)1 << 100;
    r = sum4(big, big, big, big);
    if (r != ((uint128)4 << 100)) return 2;

    /* Mixed hi/lo */
    uint128 a = ((uint128)1 << 64) | 1;
    r = sum4(a, a, a, a);
    unsigned long lo = (unsigned long)r;
    unsigned long hi = (unsigned long)(r >> 64);
    if (lo != 4) return 3;
    if (hi != 4) return 4;

    /* 5 args */
    r = sum5(v1, v2, v3, v4, v5);
    if (r != 15) return 5;

    r = sum5(big, big, big, big, big);
    if (r != ((uint128)5 << 100)) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_int128_many_args", code, &[]), 0);
}

#[test]
fn codegen_int128_divmod() {
    let code = r#"
typedef __int128 int128;
typedef unsigned __int128 uint128;

int main(void) {
    /* Signed division: positive / positive */
    int128 a = 100;
    int128 b = 7;
    if (a / b != 14) return 1;

    /* Signed division: negative / positive */
    a = -100;
    if (a / b != -14) return 2;

    /* Division of zero */
    a = 0;
    if (a / b != 0) return 3;

    /* Unsigned division: large value crossing 64-bit boundary */
    uint128 ua = ((uint128)1 << 64) | 0;
    uint128 ub = 2;
    uint128 uc = ua / ub;
    if (uc != ((uint128)1 << 63)) return 4;

    /* Signed modulo: positive */
    a = 100;
    if (a % b != 2) return 5;

    /* Signed modulo: negative dividend */
    a = -100;
    if (a % b != -2) return 6;

    /* Unsigned modulo */
    ua = ((uint128)1 << 64) | 3;
    if (ua % 4 != 3) return 7;

    /* Division by 1 */
    a = ((int128)0x1234 << 64) | 0x5678;
    if (a / 1 != a) return 8;

    /* Division by power of 2 */
    ua = (uint128)1 << 100;
    if (ua / ((uint128)1 << 50) != ((uint128)1 << 50)) return 9;

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_int128_divmod", code, &[]), 0);
}

#[test]
fn codegen_int128_globals() {
    let code = r#"
typedef unsigned __int128 uint128;
typedef __int128 int128;

uint128 g1 = 0;
uint128 g2 = 42;
int128 g4 = -1;

int main(void) {
    /* Zero-initialized */
    if (g1 != 0) return 1;
    if ((unsigned long)g1 != 0) return 2;
    if ((unsigned long)(g1 >> 64) != 0) return 3;

    /* Lo-only initializer */
    if (g2 != 42) return 4;
    if ((unsigned long)(g2 >> 64) != 0) return 5;

    /* Both halves: set at runtime */
    uint128 g3 = ((uint128)0xDEAD << 64) | 0xBEEF;
    if ((unsigned long)g3 != 0xBEEF) return 6;
    if ((unsigned long)(g3 >> 64) != 0xDEAD) return 7;

    /* All bits set (-1 signed) */
    if ((unsigned long)g4 != 0xFFFFFFFFFFFFFFFFULL) return 8;
    if ((unsigned long)((uint128)g4 >> 64) != 0xFFFFFFFFFFFFFFFFULL) return 9;

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_int128_globals", code, &[]), 0);
}

#[test]
fn codegen_int128_ptr_deref() {
    let code = r#"
typedef unsigned __int128 uint128;

int main(void) {
    uint128 val = ((uint128)0xAAAA << 64) | 0xBBBB;
    uint128 storage;

    /* Write through pointer */
    uint128 *p = &storage;
    *p = val;
    if (storage != val) return 1;

    /* Read through pointer */
    uint128 readback = *p;
    if (readback != val) return 2;

    /* Verify both halves */
    if ((unsigned long)readback != 0xBBBB) return 3;
    if ((unsigned long)(readback >> 64) != 0xAAAA) return 4;

    /* Array indexing */
    uint128 arr[4];
    arr[0] = 10;
    arr[1] = ((uint128)1 << 64) | 20;
    arr[2] = ((uint128)2 << 64) | 30;
    arr[3] = ((uint128)3 << 64) | 40;

    if (arr[0] != 10) return 5;
    if ((unsigned long)arr[1] != 20) return 6;
    if ((unsigned long)(arr[1] >> 64) != 1) return 7;
    if ((unsigned long)arr[3] != 40) return 8;
    if ((unsigned long)(arr[3] >> 64) != 3) return 9;

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_int128_ptr_deref", code, &[]), 0);
}

#[test]
fn codegen_int128_compound_assign() {
    let code = r#"
typedef unsigned __int128 uint128;

int main(void) {
    uint128 x;

    /* += */
    x = ((uint128)1 << 64) | 10;
    x += 5;
    if ((unsigned long)x != 15) return 1;
    if ((unsigned long)(x >> 64) != 1) return 2;

    /* -= */
    x = ((uint128)2 << 64) | 20;
    x -= 10;
    if ((unsigned long)x != 10) return 3;
    if ((unsigned long)(x >> 64) != 2) return 4;

    /* *= */
    x = ((uint128)1 << 64) | 3;
    x *= 2;
    if ((unsigned long)x != 6) return 5;
    if ((unsigned long)(x >> 64) != 2) return 6;

    /* /= */
    x = ((uint128)4 << 64) | 100;
    x /= 2;
    if ((unsigned long)x != 50) return 7;
    if ((unsigned long)(x >> 64) != 2) return 8;

    /* %= */
    x = 100;
    x %= 7;
    if (x != 2) return 9;

    /* <<= */
    x = 1;
    x <<= 64;
    if ((unsigned long)x != 0) return 10;
    if ((unsigned long)(x >> 64) != 1) return 11;

    /* >>= */
    x = (uint128)1 << 64;
    x >>= 64;
    if (x != 1) return 12;

    /* &= */
    x = ((uint128)0xFF << 64) | 0xFF;
    x &= ((uint128)0x0F << 64) | 0x0F;
    if ((unsigned long)x != 0x0F) return 13;
    if ((unsigned long)(x >> 64) != 0x0F) return 14;

    /* |= */
    x = ((uint128)0xF0 << 64) | 0xF0;
    x |= ((uint128)0x0F << 64) | 0x0F;
    if ((unsigned long)x != 0xFF) return 15;
    if ((unsigned long)(x >> 64) != 0xFF) return 16;

    /* ^= */
    x = ((uint128)0xFF << 64) | 0xFF;
    x ^= ((uint128)0x0F << 64) | 0x0F;
    if ((unsigned long)x != 0xF0) return 17;
    if ((unsigned long)(x >> 64) != 0xF0) return 18;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_int128_compound_assign", code, &[]),
        0
    );
}

#[test]
fn codegen_int128_shift_boundaries() {
    let code = r#"
typedef unsigned __int128 uint128;
typedef __int128 int128;

int main(void) {
    uint128 one = 1;

    /* shl by 0 */
    if ((one << 0) != 1) return 1;
    /* shl by 1 */
    if ((one << 1) != 2) return 2;
    /* shl by 63 — last bit of lo */
    if ((one << 63) != ((uint128)1 << 63)) return 3;
    /* shl by 64 — first bit of hi */
    uint128 r = one << 64;
    if ((unsigned long)r != 0) return 4;
    if ((unsigned long)(r >> 64) != 1) return 5;
    /* shl by 65 */
    r = one << 65;
    if ((unsigned long)(r >> 64) != 2) return 6;
    /* shl by 127 — top bit */
    r = one << 127;
    if ((unsigned long)(r >> 64) != (1ULL << 63)) return 7;

    /* lsr by 0 */
    uint128 big = (uint128)1 << 127;
    if ((big >> 0) != big) return 8;
    /* lsr by 1 */
    if ((big >> 1) != ((uint128)1 << 126)) return 9;
    /* lsr by 63: 1<<127 >> 63 = 1<<64, so lo=0, hi=1 */
    r = big >> 63;
    if ((unsigned long)(r >> 64) != 1) return 10;
    if ((unsigned long)r != 0) return 11;
    /* lsr by 64 */
    r = big >> 64;
    if (r != ((uint128)1 << 63)) return 12;
    /* lsr by 127 */
    r = big >> 127;
    if (r != 1) return 13;

    /* asr: negative value */
    int128 neg = (int128)-1 << 100;
    int128 sr = neg >> 50;
    /* Should still be negative (sign-extended) */
    if (sr >= 0) return 14;

    /* Variable shift amounts */
    volatile int amt = 64;
    r = one << amt;
    if ((unsigned long)r != 0) return 15;
    if ((unsigned long)(r >> 64) != 1) return 16;

    amt = 0;
    r = one << amt;
    if (r != 1) return 17;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_int128_shift_boundaries", code, &[]),
        0
    );
}

#[test]
fn codegen_int128_float_convert() {
    let code = r#"
typedef __int128 int128;
typedef unsigned __int128 uint128;

int main(void) {
    /* double -> int128 (positive) */
    double d = 42.0;
    int128 iv = (int128)d;
    if (iv != 42) return 1;

    /* double -> int128 (negative) */
    d = -100.0;
    iv = (int128)d;
    if (iv != -100) return 2;

    /* double -> int128 (large) */
    d = 1e18;
    iv = (int128)d;
    if (iv != (int128)1000000000000000000LL) return 3;

    /* int128 -> double (small) */
    iv = 42;
    d = (double)iv;
    if (d != 42.0) return 4;

    /* int128 -> double (negative) */
    iv = -100;
    d = (double)iv;
    if (d != -100.0) return 5;

    /* float -> int128 */
    float f = 123.0f;
    iv = (int128)f;
    if (iv != 123) return 6;

    /* int128 -> float */
    iv = 456;
    f = (float)iv;
    if (f != 456.0f) return 7;

    /* Round-trip */
    iv = 42;
    d = (double)iv;
    if (d != 42.0) return 8;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_int128_float_convert", code, &[]),
        0
    );
}

#[test]
fn codegen_int128_struct_array() {
    let code = r#"
typedef unsigned __int128 uint128;

struct S128 {
    uint128 val;
    int tag;
};

uint128 get_val(struct S128 s) {
    return s.val;
}

int main(void) {
    /* Struct with int128 member */
    struct S128 s;
    s.val = ((uint128)0xAAAA << 64) | 0xBBBB;
    s.tag = 42;
    if ((unsigned long)s.val != 0xBBBB) return 1;
    if ((unsigned long)(s.val >> 64) != 0xAAAA) return 2;
    if (s.tag != 42) return 3;

    /* Array of int128 */
    uint128 arr[3];
    arr[0] = 10;
    arr[1] = ((uint128)0x1111 << 64) | 0x2222;
    arr[2] = ((uint128)0x3333 << 64) | 0x4444;

    if (arr[0] != 10) return 4;
    if ((unsigned long)arr[1] != 0x2222) return 5;
    if ((unsigned long)(arr[1] >> 64) != 0x1111) return 6;
    if ((unsigned long)arr[2] != 0x4444) return 7;
    if ((unsigned long)(arr[2] >> 64) != 0x3333) return 8;

    /* Struct passed by value */
    uint128 r = get_val(s);
    if (r != s.val) return 9;

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_int128_struct_array", code, &[]), 0);
}

#[test]
fn codegen_int128_inc_dec() {
    let code = r#"
typedef unsigned __int128 uint128;

int main(void) {
    /* Pre-increment */
    uint128 x = ((uint128)1 << 64) - 1;
    uint128 y = ++x;
    if ((unsigned long)x != 0) return 1;
    if ((unsigned long)(x >> 64) != 1) return 2;
    if (y != x) return 3;

    /* Post-increment */
    x = 10;
    y = x++;
    if (y != 10) return 4;
    if (x != 11) return 5;

    /* Pre-decrement */
    x = (uint128)1 << 64;
    y = --x;
    if ((unsigned long)x != 0xFFFFFFFFFFFFFFFFULL) return 6;
    if ((unsigned long)(x >> 64) != 0) return 7;
    if (y != x) return 8;

    /* Post-decrement */
    x = 10;
    y = x--;
    if (y != 10) return 9;
    if (x != 9) return 10;

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_int128_inc_dec", code, &[]), 0);
}

#[test]
fn codegen_int128_optimized_mega() {
    // Run key int128 operations through the optimizer to catch optimizer+codegen bugs
    let code = r#"
typedef unsigned __int128 uint128;
typedef __int128 int128;

uint128 add128(uint128 a, uint128 b) { return a + b; }
uint128 sub128(uint128 a, uint128 b) { return a - b; }
uint128 mul128(uint128 a, uint128 b) { return a * b; }
uint128 shl128(uint128 a, int n) { return a << n; }
uint128 shr128(uint128 a, int n) { return a >> n; }
uint128 and128(uint128 a, uint128 b) { return a & b; }
uint128 or128(uint128 a, uint128 b) { return a | b; }
uint128 xor128(uint128 a, uint128 b) { return a ^ b; }
int cmp128(uint128 a, uint128 b) { return a == b; }

int main(void) {
    uint128 a = ((uint128)0xDEAD << 64) | 0xBEEF;
    uint128 b = ((uint128)0xCAFE << 64) | 0xBABE;

    /* Arithmetic */
    uint128 one = 1;
    uint128 r = add128(a, one);
    if ((unsigned long)r != 0xBEF0) return 1;
    if ((unsigned long)(r >> 64) != 0xDEAD) return 2;

    uint128 beef = 0xBEEF;
    r = sub128(a, beef);
    if ((unsigned long)r != 0) return 3;
    if ((unsigned long)(r >> 64) != 0xDEAD) return 4;

    uint128 two = 2, three = 3;
    r = mul128(two, three);
    if (r != 6) return 5;

    /* Shifts */
    r = shl128(one, 64);
    if ((unsigned long)r != 0) return 6;
    if ((unsigned long)(r >> 64) != 1) return 7;

    uint128 hi1 = (uint128)1 << 64;
    r = shr128(hi1, 64);
    if (r != 1) return 8;

    /* Bitwise */
    uint128 xff = 0xFF, x0f = 0x0F, xf0 = 0xF0;
    r = and128(xff, x0f);
    if (r != 0x0F) return 9;

    r = or128(xf0, x0f);
    if (r != 0xFF) return 10;

    r = xor128(xff, x0f);
    if (r != 0xF0) return 11;

    /* Comparison */
    if (!cmp128(a, a)) return 12;
    if (cmp128(a, b)) return 13;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run_optimized("codegen_int128_optimized_mega", code),
        0
    );
}

/// Test that the AddC→AdcC carry chain survives optimization.
/// The optimizer must not insert flag-clobbering instructions between
/// the add-with-carry pair. This test exercises large int128 values
/// that require actual carry propagation.
#[test]
fn codegen_int128_carry_chain_optimized() {
    let code = r#"
typedef __int128 int128;
typedef unsigned __int128 uint128;

int main(void) {
    /* Add with carry: build 0xFFFFFFFFFFFFFFFF via runtime to avoid
       constant-folding into a negative i128 literal */
    unsigned long long max64 = ~0ULL;
    uint128 a = (uint128)max64;
    uint128 b = 1;
    uint128 sum = a + b;
    /* sum should be 0x0000000000000001_0000000000000000 */
    if ((unsigned long long)sum != 0) return 1;
    if ((unsigned long long)(sum >> 64) != 1) return 2;

    /* Sub with borrow: 0x1_0000000000000000 - 1 must borrow */
    uint128 f = (uint128)1 << 64;
    uint128 g = f - 1;
    if ((unsigned long long)g != 0xFFFFFFFFFFFFFFFFULL) return 5;
    if ((unsigned long long)(g >> 64) != 0) return 6;

    /* Negation of 1: should produce all-1s */
    int128 h = 1;
    int128 neg_h = -h;
    if (neg_h != -1) return 7;

    /* Multiply with carry: (2^63) * 2 = 2^64 (crosses lo/hi boundary) */
    unsigned long long half = 0x8000000000000000ULL;
    uint128 i = (uint128)half;
    uint128 j = 2;
    uint128 prod = i * j;
    if ((unsigned long long)prod != 0) return 8;
    if ((unsigned long long)(prod >> 64) != 1) return 9;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run_optimized("codegen_int128_carry_chain_optimized", code),
        0
    );
}

/// Test uint128 large constant sign-extension bug fix.
/// Verifies that (uint128)0xFFFFFFFFFFFFFFFFULL has hi=0, lo=max64.
#[test]
fn codegen_uint128_large_constant() {
    let code = r#"
typedef unsigned __int128 uint128;

int main(void) {
    /* Build 0xFFFFFFFFFFFFFFFF via runtime to ensure it's not constant-folded
       differently. */
    unsigned long long max64 = ~0ULL;
    uint128 val = (uint128)max64;

    /* lo half should be all 1s, hi half should be 0 */
    unsigned long long lo = (unsigned long long)val;
    unsigned long long hi = (unsigned long long)(val >> 64);
    if (lo != max64) return 1;
    if (hi != 0) return 2;

    /* Zero */
    uint128 z = 0;
    if ((unsigned long long)z != 0) return 3;
    if ((unsigned long long)(z >> 64) != 0) return 4;

    /* Value 1 */
    uint128 one = 1;
    if ((unsigned long long)one != 1) return 5;
    if ((unsigned long long)(one >> 64) != 0) return 6;

    /* Constant that fills both halves */
    uint128 full = ((uint128)max64 << 64) | (uint128)max64;
    if ((unsigned long long)full != max64) return 7;
    if ((unsigned long long)(full >> 64) != max64) return 8;

    /* Value that fits in 64 bits exactly */
    uint128 mid = (uint128)0x123456789ABCDEF0ULL;
    if ((unsigned long long)mid != 0x123456789ABCDEF0ULL) return 9;
    if ((unsigned long long)(mid >> 64) != 0) return 10;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_uint128_large_constant", code, &[]),
        0
    );
}

/// Test int128 constant shifts (Shl/Lsr/Asr) decomposed in the mapping pass.
#[test]
fn codegen_int128_const_shifts() {
    let code = r#"
typedef unsigned __int128 uint128;
typedef __int128 int128;

int main(void) {
    unsigned long long max64 = ~0ULL;

    /* ===== SHL tests (returns 1-19) ===== */
    {
        uint128 a = 1;

        /* shift by 0: identity */
        uint128 r = a << 0;
        if ((unsigned long long)r != 1) return 1;
        if ((unsigned long long)(r >> 64) != 0) return 2;

        /* shift by 1 */
        r = a << 1;
        if ((unsigned long long)r != 2) return 3;

        /* shift by 32 */
        r = a << 32;
        if ((unsigned long long)r != (1ULL << 32)) return 4;

        /* shift by 63: crosses lo/hi boundary */
        r = a << 63;
        if ((unsigned long long)r != (1ULL << 63)) return 5;
        if ((unsigned long long)(r >> 64) != 0) return 6;

        /* shift by 64: lo moves to hi entirely */
        r = a << 64;
        if ((unsigned long long)r != 0) return 7;
        if ((unsigned long long)(r >> 64) != 1) return 8;

        /* shift by 65 */
        r = a << 65;
        if ((unsigned long long)r != 0) return 9;
        if ((unsigned long long)(r >> 64) != 2) return 10;

        /* shift by 127 */
        r = a << 127;
        if ((unsigned long long)r != 0) return 11;
        if ((unsigned long long)(r >> 64) != (1ULL << 63)) return 12;
    }

    /* ===== LSR tests (returns 20-39) ===== */
    {
        /* Start with hi bit set */
        uint128 a = (uint128)1 << 127;

        /* shift by 0: identity */
        uint128 r = a >> 0;
        if ((unsigned long long)(r >> 64) != (1ULL << 63)) return 20;

        /* shift by 1 */
        r = a >> 1;
        if ((unsigned long long)(r >> 64) != (1ULL << 62)) return 21;

        /* shift by 32 */
        r = a >> 32;
        if ((unsigned long long)(r >> 64) != (1ULL << 31)) return 22;

        /* shift by 63 */
        r = a >> 63;
        if ((unsigned long long)(r >> 64) != 1) return 23;
        if ((unsigned long long)r != 0) return 24;

        /* shift by 64 */
        r = a >> 64;
        if ((unsigned long long)(r >> 64) != 0) return 25;
        if ((unsigned long long)r != (1ULL << 63)) return 26;

        /* shift by 65 */
        r = a >> 65;
        if ((unsigned long long)r != (1ULL << 62)) return 27;

        /* shift by 127 */
        r = a >> 127;
        if ((unsigned long long)r != 1) return 28;
        if ((unsigned long long)(r >> 64) != 0) return 29;
    }

    /* ===== ASR tests (returns 40-59) ===== */
    {
        /* Negative int128 */
        int128 neg = -1;

        /* shift by 0: identity */
        int128 r = neg >> 0;
        if (r != -1) return 40;

        /* shift by 1: still all 1s */
        r = neg >> 1;
        if (r != -1) return 41;

        /* shift by 63 */
        r = neg >> 63;
        if (r != -1) return 42;

        /* shift by 64 */
        r = neg >> 64;
        if (r != -1) return 43;

        /* shift by 127 */
        r = neg >> 127;
        if (r != -1) return 44;

        /* Negative with specific pattern: -2 = 0xFFF...FFFE */
        int128 neg2 = -2;
        r = neg2 >> 1;
        if (r != -1) return 45;

        /* Large positive shifted right arithmetically stays positive */
        int128 big = (int128)1 << 126;  /* 0x40...0 */
        r = big >> 1;
        if ((unsigned long long)(r >> 64) != (1ULL << 61)) return 46;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_int128_const_shifts", code, &[]), 0);
}

/// `va_arg` of an `__int128` reads **two** eightbytes, and the cursor is
/// committed before the value is read.
///
/// Three defects met here. The type had no arm of its own, so it fell into the
/// scalar path where `OperandSize::from_bits(128)` saturates at 64: only the
/// low half moved, the guard was the scalar `gp_offset < 48` rather than "both
/// eightbytes fit", and the cursor advanced 8 instead of 16 -- so the *next*
/// `va_arg` re-read this one's high half.
///
/// The other two are not `__int128`-specific and had been latent in every
/// integer `va_arg`. The helper used R11 as its shuttle while `emit_va_arg`
/// puts the `va_list` pointer in R11 whenever `ap` is a slot holding a pointer
/// rather than the object, so the base was destroyed before the write-back --
/// `movl %r10d, (%r11)` faulting through the value it had just loaded. And the
/// overflow path read the value into the same register that held the area
/// pointer it then advanced, so with enough leading arguments to exhaust the
/// register file it stored a *value* back as the cursor. Both are why the
/// cursor is now committed before the copy, as the aggregate path already did.
///
/// The leading-argument counts are chosen to land the `__int128` in the
/// register save area (0, 1, 5) and in the overflow area (9): with six general
/// argument registers, nine leading `int`s exhaust them.
#[test]
fn codegen_va_arg_int128_reads_both_eightbytes() {
    let code = r#"
#include <stdarg.h>

__attribute__((noinline)) static __int128 wide(int x, ...)
{
    __int128 r;
    va_list ap;
    va_start(ap, x);
    while (x--) va_arg(ap, int);
    r = va_arg(ap, __int128);
    va_end(ap);
    return r;
}

/* The argument after the __int128 proves the cursor advanced by sixteen and
   not by eight -- an eight-byte advance hands this one the high half. */
__attribute__((noinline)) static long follows(int x, ...)
{
    va_list ap;
    va_start(ap, x);
    while (x--) va_arg(ap, int);
    (void)va_arg(ap, __int128);
    long after = va_arg(ap, long);
    va_end(ap);
    return after;
}

/* Ordinary integers past the register file, which is where the overflow
   cursor was being overwritten with a value. */
__attribute__((noinline)) static long many(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    long acc = 0;
    for (int i = 0; i < 9; i++) acc = acc * 10 + va_arg(ap, long);
    va_end(ap);
    (void)n;
    return acc;
}

int main(void)
{
    __int128 u = ((__int128) 0xaaaaaaaaaaaaaaaaULL << 64) | 0x5555555555555555ULL;

    if (wide(0, u) != u) return 1;
    if (wide(1, 0, u) != u) return 2;
    if (wide(5, 0, 0, 0, 0, 0, u) != u) return 3;
    if (wide(9, 0, 0, 0, 0, 0, 0, 0, 0, 0, u) != u) return 4;

    if (follows(0, u, 1234L) != 1234) return 5;
    if (follows(9, 0, 0, 0, 0, 0, 0, 0, 0, 0, u, 1234L) != 1234) return 6;

    if (many(0, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L) != 123456789L) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_va_arg_int128", code, &[]), 0);
    assert_eq!(
        compile_and_run("codegen_va_arg_int128_o2", code, &["-O2".to_string()]),
        0
    );

    // aarch64 had the same saturating move, plus a rule of its own: AAPCS64
    // stage C.10 starts a 16-byte integral argument at an even general
    // register, so `__gr_offs` rounds up to a multiple of 16 first.
    for opt in ["-O0", "-O2"] {
        if let Some(status) = compile_and_run_aarch64("codegen_va_arg_int128_a64", code, opt) {
            assert_eq!(status, 0, "aarch64 at {opt}");
        }
    }
}

/// An `__int128` lives in sixteen bytes of memory wherever it goes: every
/// consumer reaches it through the lo/hi memory helpers, which take an address
/// and panic on anything else. A *constant* was the one shape that never got
/// those bytes -- the allocator's immediate arm claimed it first and skipped
/// the slot -- so `f((__int128)1)` aborted the compiler outright with
/// "int128_hi_mem_loc: expected stack loc, got Imm".
///
/// Giving it a slot exposed the second half: `SetVal` emitted only into a
/// register, so the slot was allocated and never written and the constant read
/// back as zero. Both halves are needed for the value to survive.
///
/// The `return` site had the same latent panic and no test reached it.
#[test]
fn codegen_int128_constant_argument_and_return() {
    let code = r#"
#include <stdio.h>

typedef __int128 i128;
typedef unsigned __int128 u128;

static long long taken(i128 v) { return (long long)v; }
static i128 returned(void) { return (i128)424242; }
/* Wider than 64 bits, so each half needs its own movabs. */
static i128 wide(void) { return ((i128)0x0123456789abcdefLL << 64) | 0x5555aaaa1234beefULL; }
static long long unsigned_taken(u128 v) { return (long long)v; }

int main(void) {
    if (taken((i128)424242) != 424242) return 1;
    if (returned() != 424242) return 2;
    if (unsigned_taken((u128)7) != 7) return 3;

    i128 w = wide();
    if ((long long)(unsigned long long)w != (long long)0x5555aaaa1234beefULL) return 4;
    if ((long long)(w >> 64) != 0x0123456789abcdefLL) return 5;

    /* A negative constant: the high half is all ones, not zero. */
    if (taken((i128)-424242) != -424242) return 6;

    /* A narrow constant feeding a 128-bit operation stays an ordinary
       immediate -- it is widened at the use site, not given a slot. */
    i128 v = (i128)42;
    if (v != 42) return 7;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_int128_constant_argument_and_return", code, &[]),
        0
    );
}

/// AAPCS64 §5.4.2 stage C.10 allocates a 128-bit integer to a pair of
/// consecutive, *even-numbered* X registers, so an odd NGRN skips a register
/// and leaves it unused. Stage C.11 then says an argument that does not fit
/// sets NGRN to 8, putting everything after it on the stack too.
///
/// Neither rule was applied, in any of the three places that had to agree --
/// the caller, the allocator, and the prologue -- and each computed the pair
/// independently. With five leading `long`s the pair was taken as x5/x6 where
/// gcc uses x6/x7; with seven, the argument *after* the `__int128` was handed
/// x7, which the callee was not reading. Verified against
/// `aarch64-linux-gnu-gcc` in all four caller/callee pairings.
///
/// The trailing argument is the point of the test: the `__int128` itself
/// survived the seven-`long` case, and only what followed it was lost.
#[test]
fn codegen_int128_register_pair_is_even_aligned() {
    let code = r#"
#include <stdio.h>

/* NGRN is even here: the pair is x4/x5 on aarch64. */
static long long p4(long a, long b, long c, long d, __int128 v, long t)
{ return (long long)v + t; }
/* Odd: stage C.10 skips x5 and uses x6/x7. */
static long long p5(long a, long b, long c, long d, long e, __int128 v, long t)
{ return (long long)v + t; }
/* Even, and exactly fills the file: x6/x7. */
static long long p6(long a, long b, long c, long d, long e, long f, __int128 v, long t)
{ return (long long)v + t; }
/* No room: the value and everything after it go on the stack. */
static long long p7(long a, long b, long c, long d, long e, long f, long g, __int128 v, long t)
{ return (long long)v + t; }

int main(void) {
    __int128 x = 424242;
    if (p4(1, 2, 3, 4, x, 9) != 424251) return 1;
    if (p5(1, 2, 3, 4, 5, x, 9) != 424251) return 2;
    if (p6(1, 2, 3, 4, 5, 6, x, 9) != 424251) return 3;
    if (p7(1, 2, 3, 4, 5, 6, 7, x, 9) != 424251) return 4;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_int128_register_pair_is_even_aligned", code, &[]),
        0
    );
}

/// A 128-bit product is assembled from four 64-bit pieces, and three of them
/// are read *after* the one instruction that destroys a hardware register.
///
/// `expand_int128_mul` lowers `a * b` to
/// `lo = a_lo*b_lo`, `hi = umulhi(a_lo,b_lo) + a_lo*b_hi + a_hi*b_lo`.
/// On x86-64 the `umulhi` is `mulq`, which takes one operand in `%rax` and
/// writes the whole product back over `%rdx:%rax` -- so `a_lo` is gone by the
/// time `a_lo*b_hi` wants it. Every assertion here therefore reads the *high*
/// half: a product whose low half is right and whose high half is garbage is
/// exactly what this bug produces, and `(long)(a*b)` cannot see it.
///
/// The `b_hi == 0` cases are controls. They were correct throughout, because
/// the cross term the clobber ruins is multiplied by zero.
#[test]
fn codegen_int128_mul_reads_operands_after_the_hardware_multiply() {
    let code = r#"
typedef __int128 i128;
typedef unsigned __int128 u128;

__attribute__((noinline)) static i128 mul(i128 a, i128 b) { return a * b; }
__attribute__((noinline)) static u128 umul(u128 a, u128 b) { return a * b; }

static long hi(i128 v) { return (long)(v >> 64); }
static long lo(i128 v) { return (long)v; }

int main(void) {
    i128 m1 = -1, m3 = -3, m4 = -4, p3 = 3, p4 = 4;

    /* Both operands negative: every half of both is set. */
    if (hi(mul(m1, m1)) != 0) return 1;
    if (lo(mul(m1, m1)) != 1) return 2;
    if (hi(mul(m3, m4)) != 0) return 3;
    if (lo(mul(m3, m4)) != 12) return 4;

    /* Control: b_hi == 0, so the ruined cross term contributes nothing. */
    if (hi(mul(m3, p4)) != -1) return 5;
    if (lo(mul(m3, p4)) != -12) return 6;

    /* The same shape with the negative operand second -- this is the half
       that broke, and it is the one a symmetric-looking test omits. */
    if (hi(mul(p3, m4)) != -1) return 7;
    if (lo(mul(p3, m4)) != -12) return 8;

    /* Neither operand negative, but both high halves are set. */
    {
        i128 a = (i128)1 << 70;
        i128 b = (i128)1 << 70;
        if (mul(a, b) != 0) return 9;            /* overflows away exactly */
        if (hi(mul(a, 3)) != 0xc0) return 10;
    }
    {
        i128 a = (i128)-1 << 40;
        i128 b = (i128)-1 << 20;
        if (hi(mul(a, b)) != 0) return 11;
        if (lo(mul(a, b)) != 0x1000000000000000L) return 12;
    }

    /* Unsigned travels the same expansion. */
    {
        u128 a = ~(u128)0;
        u128 b = 3;
        u128 r = umul(a, b);
        if ((unsigned long)(r >> 64) != ~0UL) return 13;
        if ((unsigned long)r != ~0UL - 2) return 14;
    }

    /* The overflow builtins compute in 128 bits, so they inherit the fault:
       (-1) * (-1) is 1, which every unsigned destination represents. */
    {
        unsigned u;
        unsigned long long ull;
        int a = -1, b = -1;
        if (__builtin_mul_overflow(a, b, &u)) return 15;
        if (u != 1u) return 16;
        if (__builtin_mul_overflow(a, b, &ull)) return 17;
        if (ull != 1ull) return 18;
    }

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_int128_mul_operand_clobber", code, &[]),
        0
    );
    assert_eq!(
        compile_and_run_optimized("codegen_int128_mul_operand_clobber_opt", code),
        0
    );
}

/// Widening an integer to a 128-bit *parameter* is a conversion like any
/// other, and the argument path was the one place that did not do it.
///
/// The predicate that decides whether a call argument needs converting was
/// bounded at `arg_size <= 32 && param_size <= 64` -- written for int->long
/// and never widened when `__int128` arrived. Assignment, `return`, binary
/// operators and initializers all convert correctly, so the type looks
/// supported everywhere except where it is passed.
///
/// Every assertion reads the *high* half. Passing a positive value happens to
/// leave the right bits in the low half, so a test that checks only `(long)v`
/// passes against the broken compiler for five of the seven cases below.
#[test]
fn codegen_integer_argument_widens_to_a_128_bit_parameter() {
    let code = r#"
typedef __int128 i128;
typedef unsigned __int128 u128;

__attribute__((noinline)) static long hi(i128 v) { return (long)(v >> 64); }
__attribute__((noinline)) static long lo(i128 v) { return (long)v; }
__attribute__((noinline)) static long uhi(u128 v) { return (long)(v >> 64); }

int main(void) {
    /* Signed sources sign-extend into both halves. `signed char`, not plain
       `char`: plain char's signedness is implementation-defined (C17
       6.2.5p15) and it is *unsigned* on aarch64, where -3 is 253 and neither
       half would be negative. This test is about widening, not about which
       sign plain char happens to have. */
    {
        signed char c = -3;
        if (lo(c) != -3 || hi(c) != -1) return 1;
    }
    {
        short s = -3;
        if (lo(s) != -3 || hi(s) != -1) return 2;
    }
    {
        int i = -3;
        if (lo(i) != -3 || hi(i) != -1) return 3;
    }
    {
        long l = -3;
        if (lo(l) != -3 || hi(l) != -1) return 4;
    }

    /* Literals travel the same path as variables. */
    if (lo(-3) != -3 || hi(-3) != -1) return 5;
    if (lo(-3L) != -3 || hi(-3L) != -1) return 6;

    /* An expression, not just a leaf. */
    {
        int i = -1;
        if (lo(i - 2) != -3 || hi(i - 2) != -1) return 7;
    }

    /* Unsigned sources zero-extend; the low half alone cannot tell these
       apart from the signed cases above, which is the point. */
    {
        unsigned u = 3;
        if (lo(u) != 3 || hi(u) != 0) return 8;
    }
    {
        unsigned long ul = ~0UL;
        if (uhi(ul) != 0) return 9;
        if (lo((i128)ul) != -1) return 10;
    }

    /* An explicit cast was always correct -- keep it as the control. */
    {
        int i = -3;
        if (lo((i128)i) != -3 || hi((i128)i) != -1) return 11;
    }

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_int_argument_widens_to_int128", code, &[]),
        0
    );
    assert_eq!(
        compile_and_run_optimized("codegen_int_argument_widens_to_int128_opt", code),
        0
    );
}

/// `_Complex __int128` is thirty-two bytes: MEMORY class, returned through
/// the hidden pointer.
///
/// Both predicates that decide this -- `param_is_memory_class` and
/// `returns_via_hidden_pointer` -- tested the type's *kind* and so excluded
/// every complex type, and the `kind == Int128` arms then claimed it and gave
/// a thirty-two-byte value two general registers.
///
/// Its arithmetic also uncovered a 128-bit load that had nothing to do with
/// complex types: the lowering treated any `Loc::Stack` address operand as
/// though the slot *were* the value, so reading through an `Alloca` result --
/// a pointer in a slot -- copied the pointer's own bits as the low half.
#[test]
fn codegen_complex_int128_memory_class() {
    let code = r#"
_Complex __int128 id128(_Complex __int128 x) { return x; }
/* An sret return together with a stacked argument, at several register
   pressures: the hidden pointer takes a register the arguments then cannot. */
_Complex __int128 add0(_Complex __int128 z) { return z; }
_Complex __int128 add1(long a, _Complex __int128 z) { return z + (__int128)a; }
_Complex __int128 add5(long a, long b, long c, long d, long e,
                       _Complex __int128 z) { return z + (__int128)(a + e); }
_Complex __int128 add8(long a, long b, long c, long d, long e, long f, long g,
                       long h, _Complex __int128 z) {
    return z + (__int128)(a + h);
}
/* Reading a half of a stacked one, with no complex arithmetic at all. */
int real_of(long a, long b, long c, long d, long e, long f, long g, long h,
            _Complex __int128 z) {
    return (int)(__real__ z >> 70) + (int)(__imag__ z >> 65) + (int)(a + h);
}

static int check(_Complex __int128 v, __int128 want_re, __int128 want_im) {
    return __real__ v == want_re && __imag__ v == want_im;
}

int main(void) {
    if (sizeof(_Complex __int128) != 32) return 1;

    _Complex __int128 z;
    __real__ z = (__int128)1 << 90;
    __imag__ z = (__int128)3 << 80;

    /* Reading the halves of a local, with no call involved. */
    if (__real__ z != ((__int128)1 << 90)) return 2;
    if (__imag__ z != ((__int128)3 << 80)) return 3;

    if (!check(id128(z), (__int128)1 << 90, (__int128)3 << 80)) return 4;
    if (!check(add0(z), (__int128)1 << 90, (__int128)3 << 80)) return 5;
    if (!check(add1(3, z), ((__int128)1 << 90) + 3, (__int128)3 << 80)) return 6;
    if (!check(add5(1, 2, 3, 4, 5, z), ((__int128)1 << 90) + 6,
               (__int128)3 << 80)) return 7;
    if (!check(add8(1, 2, 3, 4, 5, 6, 7, 8, z), ((__int128)1 << 90) + 9,
               (__int128)3 << 80)) return 8;

    _Complex __int128 w;
    __real__ w = (__int128)5 << 70;
    __imag__ w = (__int128)7 << 65;
    if (real_of(1, 2, 3, 4, 5, 6, 7, 8, w) != 5 + 7 + 9) return 9;
    return 0;
}
"#;
    assert_eq!(compile_and_run("cg_complex_int128_mem", code, &[]), 0);
    assert_eq!(
        compile_and_run("cg_complex_int128_mem_o2", code, &["-O2".to_string()]),
        0
    );
}

/// A 128-bit instruction `instcombine` proves constant -- `0 << n`, `0 >> n`
/// and an all-ones `>> n`, with `n` unknown -- keeps the shape a 128-bit
/// constant needs. Rewritten into a copy of a constant with no `SetVal` of its
/// own, the constant was forwarded into a call, a return and a phi with no
/// width to say it is sixteen bytes: x86-64 aborted the compiler
/// (`int128_lo_mem_loc` on an immediate) and aarch64 dropped the high half of
/// both arms of the phi. Each result is combined with values whose high halves
/// are not zero, so a lost half shows.
#[test]
fn codegen_int128_folded_shift_of_constant() {
    let code = r#"
typedef __int128 i128;
typedef unsigned __int128 u128;

static const u128 BIG = ((u128)0x1122334455667788ull << 64) | 0x99aabbccddeeff00ull;

__attribute__((noinline)) u128 idu(u128 x) { return x; }
__attribute__((noinline)) i128 idi(i128 x) { return x; }
__attribute__((noinline)) int idn(int x) { return x; }
static u128 gu;

__attribute__((noinline)) u128 z_ret(int n) { u128 z = (u128)0; return z << n; }
__attribute__((noinline)) u128 z_arg(int n) { u128 z = (u128)0; return idu(z >> n); }
__attribute__((noinline)) i128 z_sar(int n) { i128 z = (i128)0; return idi(z >> n); }
__attribute__((noinline)) void z_store(int n) { u128 z = (u128)0; gu = z << n; }
__attribute__((noinline)) u128 z_add(int n) { u128 z = (u128)0; return (z << n) + BIG; }
__attribute__((noinline)) u128 z_or(int n) { u128 z = (u128)0; return (z >> n) | BIG; }
__attribute__((noinline)) u128 z_shift(int n) {
    u128 z = (u128)0;
    return BIG >> ((z << n) + 4);
}
__attribute__((noinline)) u128 z_sel(int n, int c) {
    u128 z = (u128)0;
    u128 a = z << n;
    return c ? a : BIG;
}
__attribute__((noinline)) u128 z_phi(int n, int c) {
    u128 z = (u128)0, r = BIG;
    if (c)
        r = z << n;
    return r;
}
__attribute__((noinline)) int z_cmp(int n) { u128 z = (u128)0; return (z << n) == 0; }
__attribute__((noinline)) u128 z_mul(int n) { u128 z = (u128)0; return (z << n) * BIG; }
__attribute__((noinline)) u128 z_two(int n) {
    u128 z = (u128)0;
    u128 a = z << n;
    u128 b = z >> n;
    return idu(a) + idu(b) + idu(a) + BIG;
}
__attribute__((noinline)) i128 m_sar(int n) { i128 m = (i128)-1; return idi(m >> n); }

int main(void) {
    int n = idn(5);
    if (z_ret(n) != 0) return 1;
    if (z_arg(n) != 0) return 2;
    if (z_sar(n) != 0) return 3;
    gu = BIG;
    z_store(n);
    if (gu != 0) return 4;
    if (z_add(n) != BIG) return 5;
    if (z_or(n) != BIG) return 6;
    if (z_shift(n) != BIG >> 4) return 7;
    if (z_sel(n, 1) != 0) return 8;
    if (z_sel(n, 0) != BIG) return 9;
    if (z_phi(n, 1) != 0) return 10;
    if (z_phi(n, 0) != BIG) return 11;
    if (z_cmp(n) != 1) return 12;
    if (z_mul(n) != 0) return 13;
    if (z_two(n) != BIG) return 14;
    if (m_sar(n) != (i128)-1) return 15;
    if ((u128)m_sar(n) >> 64 != 0xffffffffffffffffull) return 16;
    return 0;
}
"#;
    compile_and_run_everywhere("cg_int128_folded_shift", code);
    if let Some(rc) = compile_and_run_aarch64("cg_int128_folded_shift_a64_o1", code, "-O1") {
        assert_eq!(rc, 0, "aarch64 at -O1");
    }
}
