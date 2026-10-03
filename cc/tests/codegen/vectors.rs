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

use crate::common::{compile_and_run_everywhere, compile_expect_error};

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
/// a vector -- for every declarator, and as a function's return type.
#[test]
fn vector_attribute_in_declaration_specifiers() {
    run(
        "vec_spec_attr",
        r#"
__attribute__((vector_size(8))) signed char v4, v5, v6;
signed char __attribute__((vector_size(8))) w4, w5;
_Static_assert(sizeof v6 == 8 && sizeof w5 == 8, "every declarator");
int main(void) {
    v5 = (__typeof__(v5)){1, 2, 3, 4, 5, 6, 7, 8};
    v6 = v5 * 2;
    v4 = v5 + v6;
    return (v4[0] == 3 && v4[7] == 24) ? 0 : 1;
}
"#,
    );
}

#[test]
fn vector_operand_constraints() {
    for (name, expr, expected) in [
        ("vec_mixed_lanes", "a + f", "invalid operands to binary +"),
        (
            "vec_float_splat",
            "a + 1.5",
            "cannot convert value to a vector",
        ),
        (
            "vec_truncating_splat",
            "a + l",
            "conversion of scalar 'long' to vector '__vector(4) int' involves truncation",
        ),
        ("vec_float_mod", "f % f", "invalid operands to binary %"),
        (
            "vec_float_complement",
            "~f",
            "wrong type argument to bit-complement",
        ),
        (
            "vec_not",
            "!a",
            "wrong type argument to unary exclamation mark",
        ),
        ("vec_deref", "*a", "invalid type argument of unary '*'"),
        (
            "vec_logical",
            "a && a",
            "used vector type where scalar is required",
        ),
        (
            "vec_cond_mismatch",
            "l ? a : u",
            "type mismatch in conditional expression",
        ),
        (
            "vec_assign_signedness",
            "a = u",
            "incompatible types when assigning",
        ),
        ("vec_cast_size", "(v2si)l2[0]", "which has different size"),
        (
            "vec_cast_float",
            "(double)l2",
            "aggregate value used where a floating-point was expected",
        ),
    ] {
        compile_expect_error(
            name,
            &format!(
                "{PRELUDE}void g(long l) {{ v4si a = {{0}}; v4su u = {{0}}; v4sf f = {{0}}; \
                 v2si l2 = {{0}}; (void)({expr}); }}\n"
            ),
            expected,
        );
    }
}
