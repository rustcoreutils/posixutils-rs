//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU `vector_size` values: arithmetic, comparisons, splats, casts,
// assignment and initialization, lowered lane by lane. The expected values
// are gcc's.
//

use crate::common::compile_and_run_everywhere;

const PRELUDE: &str = r#"
typedef int v4si __attribute__((vector_size(16)));
typedef unsigned v4su __attribute__((vector_size(16)));
typedef float v4sf __attribute__((vector_size(16)));
typedef double v2df __attribute__((vector_size(16)));
typedef long v2dl __attribute__((vector_size(16)));
typedef short v8hi __attribute__((vector_size(16)));
typedef unsigned char v16qu __attribute__((vector_size(16)));
typedef int v2si __attribute__((vector_size(8)));
typedef double v4df __attribute__((vector_size(32)));
/* Return a distinct code from the line of the first lane that differs. */
#define C4(v, a, b, c, d) do { __typeof__(v) t_ = (v); \
    if (t_[0] != (a) || t_[1] != (b) || t_[2] != (c) || t_[3] != (d)) \
        return __LINE__ % 200 + 1; } while (0)
#define C2(v, a, b) do { __typeof__(v) t_ = (v); \
    if (t_[0] != (a) || t_[1] != (b)) return __LINE__ % 200 + 1; } while (0)
"#;

fn run(name: &str, body: &str) {
    compile_and_run_everywhere(name, &format!("{PRELUDE}{body}"));
}

#[test]
fn vector_integer_arithmetic() {
    run(
        "vec_int",
        r#"
int main(void) {
    v4si a = {1, -2, 3, -4}, b = {5, 6, -7, 8}, k1234 = {1, 2, 3, 4};
    v4su u = {1, 2, 3, 0xffffffffu};
    int k = 3;
    C4(a + b, 6, 4, -4, 4); C4(a - b, -4, -8, 10, -12); C4(a * b, 5, -12, -21, -32);
    C4(b / a, 5, -3, -2, -2); C4(b % a, 0, 0, -1, 0);
    C4(a & b, 1, 6, 1, 8); C4(a | b, 5, -2, -5, -4); C4(a ^ b, 4, -8, -6, -12);
    C4(a << 2, 4, -8, 12, -16); C4(b >> 1, 2, 3, -4, 4); C4(b << k1234, 10, 24, -56, 128);
    C4(u >> 1, 0, 1, 1, 0x7fffffffu);
    C4(-a, -1, 2, -3, 4); C4(+a, 1, -2, 3, -4); C4(~a, -2, 1, -4, 3);
    C4(a + 1, 2, -1, 4, -3); C4(10 - a, 9, 12, 7, 14); C4(a * k, 3, -6, 9, -12);
    C4(1 << (k1234 - 1), 1, 2, 4, 8);
    v8hi h = {1, 2, 3, 4, 5, 6, 7, -8};
    v16qu q = {250, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 255};
    if ((h + h)[7] != -16 || (h * 3)[2] != 9 || (q + 10)[0] != 4 || (q + 1)[15] != 0) return 50;
    return 0;
}
"#,
    );
}

#[test]
fn vector_comparisons_and_floating_lanes() {
    run(
        "vec_cmp_float",
        r#"
int main(void) {
    v4si a = {1, -2, 3, -4}, b = {5, 6, -7, 8}, k1030 = {1, 0, 3, 0};
    v4su u = {1, 2, 3, 0xffffffffu}, two = {2, 2, 2, 2};
    v4sf f = {1.5f, -2.0f, 3.25f, 0.0f}, g = {0.5f, 2.0f, -1.0f, 4.0f};
    v2df d = {1.25, -3.5}, kd = {2.0, -4.0};
    C4(a < b, -1, -1, 0, -1); C4(a == k1030, -1, 0, -1, 0); C4(a >= b, 0, 0, -1, 0);
    C4(u < two, -1, 0, 0, 0);
    C4(f + g, 2, 0, 2.25f, 4); C4(f * g, 0.75f, -4, -3.25f, 0); C4(f / g, 3, -1, -3.25f, 0);
    C4(-f, -1.5f, 2, -3.25f, 0); C4(f * 2, 3, -4, 6.5f, 0);
    C4(f < g, 0, -1, 0, -1); C4(f == f, -1, -1, -1, -1);
    C2(d + d, 2.5, -7); C2(d * 2.5, 3.125, -8.75); C2(d < kd, -1, 0);
    if (_Generic(d < kd, v2dl: 0, default: 1)) return 60;
    if (!__builtin_signbit((-f)[3])) return 61;
    v4df w = {1, 2, 3, 4};
    w = w * w + 1;
    C4(w, 2, 5, 10, 17);
    v4si m = (a > 0) & a;
    C4(m, 1, 0, 3, 0);
    v4su um = a < b;
    C4(um, 0xffffffffu, 0xffffffffu, 0, 0xffffffffu);
    if (sizeof(a + b) != 16 || sizeof(a < b) != 16 || (a + b)[3] != 4) return 62;
    return 0;
}
"#,
    );
}

#[test]
fn vector_casts_reinterpret_bits() {
    run(
        "vec_cast",
        r#"
int main(void) {
    v4sf f = {1.5f, -2.0f, 3.25f, 0.0f};
    v4si bits = {0x3f800000, 0x40000000, 0, 0};
    C4((v4si)f, 1069547520, -1073741824, 1078984704, 0);
    C4((v4sf)bits, 1, 2, 0, 0);
    v2si pair = {1, 2};
    if ((long long)pair != 0x200000001LL) return 70;
    v2si from = (v2si)0x500000004LL;
    C2(from, 4, 5);
    long long k = 0x500000004LL;
    v2si from2 = (v2si)k;
    C2(from2, 4, 5);
    return 0;
}
"#,
    );
}

#[test]
fn vector_objects_assignment_and_initialization() {
    run(
        "vec_objects",
        r#"
struct S { char c; v4si v; v2df d; };
union U { v4si v; int i[4]; float f[4]; };
static v4si gv = {10, 20, 30, 40};
static int sum(const v4si *p) { v4si t = *p + *p; return t[0] + t[1] + t[2] + t[3]; }
int main(void) {
    v4si a = {1, 2, 3, 4}, b = {10, 20, 30, 40};
    v4si c = a;
    c += b; C4(c, 11, 22, 33, 44);
    c <<= 1; C4(c, 22, 44, 66, 88);
    c = b; C4(c, 10, 20, 30, 40);
    ++c; C4(c, 11, 21, 31, 41);
    c--; C4(c, 10, 20, 30, 40);
    v4si old = c++;
    C4(old, 10, 20, 30, 40); C4(c, 11, 21, 31, 41);
    c[2] = 99; C4(c, 11, 21, 99, 41);
    struct S s = {'x', a + b, {1.5, 2.5}};
    C4(s.v, 11, 22, 33, 44);
    struct S t = s;
    t.v *= 3;
    C4(s.v, 11, 22, 33, 44); C4(t.v, 33, 66, 99, 132);
    struct S *sp = &t;
    sp->v -= s.v; C4(sp->v, 22, 44, 66, 88);
    v4si arr[3] = {a, b, a * b};
    C4(arr[2], 10, 40, 90, 160);
    v4si *p = &arr[1];
    C4(*p - 1, 9, 19, 29, 39); C4(p[1] + p[-1], 11, 42, 93, 164);
    if (sum(&a) != 20) return 80;
    union U u = {.v = a << 4};
    if (u.i[1] != 32 || u.i[3] != 64) return 81;
    gv -= 1; C4(gv, 9, 19, 29, 39);
    int k = 1;
    C4((k ? a : b) * 2, 2, 4, 6, 8); C4(!k ? a : b, 10, 20, 30, 40);
    if ((k ? a : b)[0] != 1) return 82;
    volatile v4si vv = a;
    vv = vv + 1;
    C4(vv, 2, 3, 4, 5);
    for (int i = 0; i < 3; i++) a += b;
    C4(a, 31, 62, 93, 124);
    v4si mask = a > (v4si){50, 70, 100, 200};
    v4si select = (mask & a) | (~mask & b);
    C4(select, 10, 20, 30, 40);
    return 0;
}
"#,
    );
}

/// A `vector_size` among the declaration specifiers makes the type they name
/// a vector -- for every declarator, and as a function's return type -- of
/// elements of the width a `mode` written with it names.
#[test]
fn vector_attribute_in_declaration_specifiers() {
    run(
        "vec_spec_attr",
        r#"
__attribute__((vector_size(8))) signed char v4, v5, v6;
signed char __attribute__((vector_size(8))) w4, w5;
_Static_assert(sizeof v6 == 8 && sizeof w5 == 8, "every declarator");
/* The mode names the element's width; the vector is made of that. */
typedef int __attribute__((mode(SI))) __attribute__((vector_size(8))) vecint;
_Static_assert(sizeof(vecint) == 8 && sizeof(((vecint){0})[0]) == 4, "mode, then vector");
int main(void) {
    v5 = (__typeof__(v5)){1, 2, 3, 4, 5, 6, 7, 8};
    v6 = v5 * 2;
    v4 = v5 + v6;
    return (v4[0] == 3 && v4[7] == 24) ? 0 : 1;
}
"#,
    );
}

/// `__builtin_shuffle` (one or two operands, a run-time mask taken modulo
/// the lanes), `__builtin_shufflevector` (constant indices, operands of
/// different lengths, a result of a new length) and
/// `__builtin_convertvector`.
#[test]
fn vector_shuffle_and_convert_builtins() {
    run(
        "vec_shuffle",
        r#"
typedef int v8si __attribute__((vector_size(32)));
typedef char v16qi __attribute__((vector_size(16)));
int main(void) {
    v4si a = {10, 20, 30, 40}, b = {50, 60, 70, 80};
    v4si m1 = {3, 2, 1, 0}, m2 = {0, 5, 2, 7}, mw = {4, 9, -1, 13};
    v4su um = {1, 1, 6, 6};
    v4sf f = {1.5f, -2.5f, 3.75f, 9.75f};
    C4(__builtin_shuffle(a, m1), 40, 30, 20, 10);
    C4(__builtin_shuffle(a, b, m2), 10, 60, 30, 80);
    C4(__builtin_shuffle(a, mw), 10, 20, 40, 20);
    C4(__builtin_shuffle(a, b, mw), 50, 20, 80, 60);
    C4(__builtin_shuffle(a, b, um), 20, 20, 70, 70);
    C4(__builtin_shuffle(f, m1), 9.75f, 3.75f, -2.5f, 1.5f);
    C4(__builtin_shufflevector(a, b, 7, 0, 5, 2), 80, 10, 60, 30);
    v2si half = __builtin_shufflevector(a, a, 1, 3);
    C2(half, 20, 40);
    v2si d = {1, 2};
    C4(__builtin_shufflevector(a, d, 0, 4, 5, 3), 10, 1, 2, 40);
    v8si wide = __builtin_shufflevector(a, a, 0, 1, 2, 3, 0, 1, 2, 3);
    if (sizeof wide != 32 || wide[5] != 20) return 90;
    C4(__builtin_convertvector(f, v4si), 1, -2, 3, 9);
    C4(__builtin_convertvector(a, v4sf), 10, 20, 30, 40);
    v2df dd = __builtin_convertvector(half, v2df);
    C2(dd, 20, 40);
    v16qi bytes = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16};
    v16qi rev = {15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
    v16qi r = __builtin_shuffle(bytes, rev);
    if (r[0] != 16 || r[7] != 9 || r[15] != 1) return 91;
    int k = 2;
    m1[0] = k;
    C4(__builtin_shuffle(a, m1), 30, 30, 20, 10);
    return 0;
}
"#,
    );
}

/// A vector in a register operand of inline asm is its value in the
/// register: an x86 `"x"` operand, or an aarch64 `"w"` one. It was handed
/// over as its address in a general register, and read back eight bytes
/// wide.
#[test]
fn vector_inline_asm_register_operands() {
    let src = r#"
typedef double v2df __attribute__((__vector_size__(16)));
int main(void) {
    v2df a = {4.0, 9.0}, r;
#if defined(__x86_64__)
    __asm__("sqrtpd %1, %0" : "=x"(r) : "x"(a));
    v2df s = a;
    __asm__("addpd %0, %0" : "+x"(s));
#else
    __asm__("fsqrt %0.2d, %1.2d" : "=w"(r) : "w"(a));
    v2df s = a;
    __asm__("fadd %0.2d, %0.2d, %0.2d" : "+w"(s));
#endif
    return (r[0] == 2 && r[1] == 3 && s[0] == 8 && s[1] == 18) ? 0 : 1;
}
"#;
    crate::common::compile_and_run_everywhere("vec_asm", src);
}

/// A comparison of 8- or 16-bit lanes widens both operands itself, as its
/// signedness says. After inlining made one operand the constant -128,
/// x86-64 kept that operand zero-extended in its slot -- 65408 -- and
/// compared it with the other's sign-extended lane, so every lane of
/// `v < -128` came out true.
#[test]
fn vector_narrow_lane_compare_against_a_constant() {
    let src = r#"
typedef short v8h __attribute__((vector_size(16)));
typedef signed char v16b __attribute__((vector_size(16)));
typedef unsigned short v8hu __attribute__((vector_size(16)));
static inline __attribute__((always_inline)) v8h clamp_lo(v8h v, short lo)
{
    v8h m = v < lo;
    return (v & ~m) | (((v8h){0} + lo) & m);
}
static inline __attribute__((always_inline)) v16b below(v16b v, signed char k) { return v < k; }
static inline __attribute__((always_inline)) v8hu above(v8hu v, unsigned short k) { return (v8hu)(v > k); }
int main(void) {
    volatile short s = 1;
    v8h w = {0, s, 1, -200, 0, 0, 0, 0};
    v8h r = clamp_lo(w, -128);
    if (r[0] != 0 || r[1] != 1 || r[3] != -128) return 1;
    v16b b = {0, -100, -128, 5};
    v16b m = below(b, -99);
    if (m[0] != 0 || m[1] != -1 || m[2] != -1 || m[3] != 0) return 2;
    v8hu u = {65535, 1, 32768, 0};
    v8hu n = above(u, 32767);
    if (n[0] != 65535 || n[1] != 0 || n[2] != 65535 || n[3] != 0) return 3;
    return 0;
}
"#;
    crate::common::compile_and_run_everywhere("vec_narrow_cmp", src);
}

/// Integer-lane vectors differing in signedness compare unsigned, whichever
/// side is unsigned; arithmetic computes at the left operand's lane type,
/// which is the result's. The values are gcc's.
#[test]
fn vector_mixed_signedness_operands() {
    run(
        "vec_mixed_sign",
        r#"
typedef unsigned short v8hu __attribute__((vector_size(16)));
int main(void) {
    v8hi a = {-1, 2, 1, 1, 1, 1, 1, 1};
    v8hu b = {1, 1, 1, 1, 1, 1, 1, 1};
    v8hi lt = a < b, gt = b > a, ge = a >= b;
    if (lt[0] != 0 || lt[1] != 0 || gt[0] != 0 || gt[1] != 0 || ge[0] != -1) return 1;
    v4si s = {-7, 1, 1, 1};
    v4su u = {2, 1, 1, 1};
    C4(s < u, 0, 0, 0, 0); C4(u > s, 0, 0, 0, 0); C4(s == u, 0, -1, -1, -1);
    C4(s / u, -3, 1, 1, 1);
    v4su q = (v4su){0xfffffff9u, 1, 1, 1} / (v4si){2, 1, 1, 1};
    if (q[0] != 2147483644u) return 2;
    C4(((v4si){-8, 1, 1, 1} >> (v4su){1, 1, 1, 1}), -4, 0, 0, 0);
    if (_Generic(s + u, v4si: 0, default: 1) || _Generic(u + s, v4su: 0, default: 1)) return 3;
    return 0;
}
"#,
    );
}
