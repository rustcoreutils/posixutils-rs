//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU vector operations against their own lanes: every C operator on every
// lane type and both register shapes, each result lane compared bit for bit
// with the same operation done on scalars in the same program. Whether a
// target computes an operation with packed instructions or lane by lane
// (`arch::simd::native`), the answers must agree -- including at the edges:
// INT_MIN, wrapping, -0.0, infinities, NaN and subnormals. Checked against
// gcc on x86-64 and aarch64.
//

use crate::codegen::asm_probe::{asm_for, X86_64_LINUX};
use crate::common::compile_and_run_everywhere;

/// A vector type of the matrix: its name, lane type, lane count, and the
/// signed integer type of its comparison mask's lanes.
struct Shape {
    vec: &'static str,
    lane: &'static str,
    count: usize,
    mask: &'static str,
    float: bool,
}

const fn int(vec: &'static str, lane: &'static str, count: usize, mask: &'static str) -> Shape {
    Shape {
        vec,
        lane,
        count,
        mask,
        float: false,
    }
}

const fn flt(vec: &'static str, lane: &'static str, count: usize, mask: &'static str) -> Shape {
    Shape {
        vec,
        lane,
        count,
        mask,
        float: true,
    }
}

const INT_SHAPES: &[Shape] = &[
    int("v16qi", "signed char", 16, "signed char"),
    int("v16qu", "unsigned char", 16, "signed char"),
    int("v8hi", "short", 8, "short"),
    int("v8hu", "unsigned short", 8, "short"),
    int("v4si", "int", 4, "int"),
    int("v4su", "unsigned", 4, "int"),
    int("v2di", "long long", 2, "long long"),
    int("v2du", "unsigned long long", 2, "long long"),
    int("v8qi", "signed char", 8, "signed char"),
    int("v4hi", "short", 4, "short"),
    int("v2si", "int", 2, "int"),
    int("v2su", "unsigned", 2, "int"),
    int("v1di", "long long", 1, "long long"),
    int("v2hi", "short", 2, "short"),
];

const FLOAT_SHAPES: &[Shape] = &[
    flt("v4sf", "float", 4, "int"),
    flt("v2df", "double", 2, "long long"),
    flt("v2sf", "float", 2, "int"),
];

const PRELUDE: &str = r#"
#include <string.h>
typedef unsigned long long ull;
static volatile long long ival[] = {
    0, 1, -1, 2, -2, 3, 42, 0x7f, 0x80, 0xff, 0x7fff, 0x8000, 0xffff,
    0x7fffffff, 0x80000000LL, 12345, -98765, 0x123456789LL,
    0x7fffffffffffffffLL, -0x7fffffffffffffffLL - 1, -77777777777LL,
};
#define NI (int)(sizeof ival / sizeof ival[0])
static volatile double fval[] = {
    0.0, -0.0, 1.5, -2.25, 3.0, -7.0, 0.1, 1e-40, 2.5e-310, 3.4e38, 1e300,
    -1e30, __builtin_inf(), -__builtin_inf(), __builtin_nan(""), 12345.678,
};
#define NF (int)(sizeof fval / sizeof fval[0])
"#;

/// One check function: `expr` computed on vectors `a` and `b` (and scalar
/// `s`), each lane compared with `want`, a scalar expression of lane `i`.
/// Returns 0, or where it first differed.
fn check(
    out: &mut String,
    id: usize,
    s: &Shape,
    (expr, want, rtype): (&str, &str, &str),
    fix_b: &str,
) {
    let (src, n) = if s.float {
        ("fval", "NF")
    } else {
        ("ival", "NI")
    };
    let (vec, lane, count) = (s.vec, s.lane, s.count);
    out.push_str(&format!(
        "static int t{id}(void) {{\n\
         \x20   for (int k = 0; k < {n}; k++) {{\n\
         \x20       {vec} a, b;\n\
         \x20       for (int i = 0; i < {count}; i++) {{\n\
         \x20           a[i] = ({lane}){src}[(k + i) % {n}];\n\
         \x20           b[i] = ({lane}){src}[(k * 7 + i * 3 + 1) % {n}];\n\
         \x20           {fix_b}\n\
         \x20       }}\n\
         \x20       {lane} s = b[0];\n\
         \x20       (void)s;\n\
         \x20       {rtype} r = {expr};\n\
         \x20       for (int i = 0; i < {count}; i++) {{\n\
         \x20           __typeof__(r[0]) got = r[i], want = {want};\n\
         \x20           if (memcmp(&got, &want, sizeof got)) return k * 64 + i + 1;\n\
         \x20       }}\n\
         \x20   }}\n\
         \x20   return 0;\n\
         }}\n"
    ));
}

/// The program checking `shapes` with `ops`, each a (vector expression,
/// lane expression, result type) for a shape -- the result type `V` the
/// shape's own and `M` its mask's.
fn program(shapes: &[Shape], ops: &[(&str, &str, &str, &str)]) -> String {
    let mut out = String::from(PRELUDE);
    let mut ids = Vec::new();
    for s in shapes {
        let bytes = s.count * lane_bytes(s.lane);
        out.push_str(&format!(
            "typedef {} {} __attribute__((vector_size({bytes})));\n",
            s.lane, s.vec
        ));
        let mask = format!("{}_mask", s.vec);
        out.push_str(&format!(
            "typedef {} {mask} __attribute__((vector_size({bytes})));\n",
            s.mask
        ));
        for &(expr, want, result, fix) in ops {
            let rtype = if result == "M" { mask.as_str() } else { s.vec };
            let want = want.replace('T', s.lane).replace('M', s.mask);
            let id = ids.len();
            check(&mut out, id, s, (expr, &want, rtype), fix);
            ids.push(format!("{} {expr}", s.vec));
        }
    }
    out.push_str("int main(void) {\n    int r;\n");
    for (id, what) in ids.iter().enumerate() {
        let what = what.replace('"', "'");
        out.push_str(&format!(
            "    if ((r = t{id}())) {{ __builtin_printf(\"%s: %d\\n\", \"{what}\", r); return 1; }}\n"
        ));
    }
    out.push_str("    return 0;\n}\n");
    out
}

fn lane_bytes(lane: &str) -> usize {
    match lane {
        "signed char" | "unsigned char" => 1,
        "short" | "unsigned short" => 2,
        "int" | "unsigned" | "float" => 4,
        _ => 8,
    }
}

/// A divisor that is never zero, nor -1 (whose quotient of the minimum
/// overflows).
const NONZERO: &str = "if (b[i] == 0 || b[i] == -1) b[i] = 3;";
/// A shift count within the lane.
const IN_LANE: &str = "b[i] = (__typeof__(b[i]))((ull)b[i] % (sizeof b[i] * 8));";

/// Integer operators: computed in `ull` where the lane's own arithmetic
/// would overflow, which wraps exactly as a lane does.
const INT_OPS: &[(&str, &str, &str, &str)] = &[
    ("a + b", "(T)((ull)a[i] + (ull)b[i])", "V", ""),
    ("a - b", "(T)((ull)a[i] - (ull)b[i])", "V", ""),
    ("a * b", "(T)((ull)a[i] * (ull)b[i])", "V", ""),
    ("a / b", "(T)(a[i] / b[i])", "V", NONZERO),
    ("a % b", "(T)(a[i] % b[i])", "V", NONZERO),
    ("a & b", "(T)(a[i] & b[i])", "V", ""),
    ("a | b", "(T)(a[i] | b[i])", "V", ""),
    ("a ^ b", "(T)(a[i] ^ b[i])", "V", ""),
    ("a << b", "(T)((ull)a[i] << b[i])", "V", IN_LANE),
    ("a >> b", "(T)(a[i] >> b[i])", "V", IN_LANE),
    ("-a", "(T)(0 - (ull)a[i])", "V", ""),
    ("~a", "(T)~a[i]", "V", ""),
    ("a + s", "(T)((ull)a[i] + (ull)s)", "V", ""),
    ("s - a", "(T)((ull)s - (ull)a[i])", "V", ""),
    ("a * s", "(T)((ull)a[i] * (ull)s)", "V", ""),
    ("a << s", "(T)((ull)a[i] << s)", "V", IN_LANE),
    ("a >> s", "(T)(a[i] >> s)", "V", IN_LANE),
    ("a == b", "(M)(a[i] == b[i] ? -1 : 0)", "M", ""),
    ("a != b", "(M)(a[i] != b[i] ? -1 : 0)", "M", ""),
    ("a < b", "(M)(a[i] < b[i] ? -1 : 0)", "M", ""),
    ("a >= b", "(M)(a[i] >= b[i] ? -1 : 0)", "M", ""),
    ("a > s", "(M)(a[i] > s ? -1 : 0)", "M", ""),
];

const FLOAT_OPS: &[(&str, &str, &str, &str)] = &[
    ("a + b", "(T)(a[i] + b[i])", "V", ""),
    ("a - b", "(T)(a[i] - b[i])", "V", ""),
    ("a * b", "(T)(a[i] * b[i])", "V", ""),
    ("a / b", "(T)(a[i] / b[i])", "V", ""),
    ("-a", "(T)-a[i]", "V", ""),
    ("a * s", "(T)(a[i] * s)", "V", ""),
    ("s - a", "(T)(s - a[i])", "V", ""),
    ("a == b", "(M)(a[i] == b[i] ? -1 : 0)", "M", ""),
    ("a != b", "(M)(a[i] != b[i] ? -1 : 0)", "M", ""),
    ("a < b", "(M)(a[i] < b[i] ? -1 : 0)", "M", ""),
    ("a <= b", "(M)(a[i] <= b[i] ? -1 : 0)", "M", ""),
    ("a > b", "(M)(a[i] > b[i] ? -1 : 0)", "M", ""),
    ("a >= b", "(M)(a[i] >= b[i] ? -1 : 0)", "M", ""),
];

#[test]
fn vector_native_integer_lanes_match_scalars() {
    compile_and_run_everywhere("vec_native_int", &program(INT_SHAPES, INT_OPS));
}

#[test]
fn vector_native_floating_lanes_match_scalars() {
    compile_and_run_everywhere("vec_native_float", &program(FLOAT_SHAPES, FLOAT_OPS));
}

/// On x86-64 the operations SSE2 has are one packed instruction each, with
/// no lane loop left: the integer ones at every lane width, and the
/// floating ones on sixteen-byte vectors.
#[test]
fn vector_native_x86_64_uses_packed_instructions() {
    let src = r#"
typedef signed char v16qi __attribute__((vector_size(16)));
typedef short v8hi __attribute__((vector_size(16)));
typedef int v4si __attribute__((vector_size(16)));
typedef long long v2di __attribute__((vector_size(16)));
typedef int v2si __attribute__((vector_size(8)));
typedef float v4sf __attribute__((vector_size(16)));
typedef double v2df __attribute__((vector_size(16)));
void addb(v16qi *d, v16qi *a, v16qi *b) { *d = *a + *b; }
void subw(v8hi *d, v8hi *a, v8hi *b) { *d = *a - *b; }
void addd(v4si *d, v4si *a, v4si *b) { *d = *a + *b; }
void subq(v2di *d, v2di *a, v2di *b) { *d = *a - *b; }
void add8(v2si *d, v2si *a, v2si *b) { *d = *a + *b; }
void bits(v4si *d, v4si *a, v4si *b) { *d = (*a & *b) | (*a ^ ~*b); }
void negd(v4si *d, v4si *a) { *d = -*a; }
void fops(v4sf *d, v4sf *a, v4sf *b) { *d = -((*a + *b) * (*a - *b) / *b); }
void dops(v2df *d, v2df *a, v2df *b) { *d = -((*a + *b) * (*a - *b) / *b); }
"#;
    let asm = asm_for("vec_native_x86", X86_64_LINUX, src);
    for (f, want) in [
        ("addb", &["paddb"][..]),
        ("subw", &["psubw"]),
        ("addd", &["paddd"]),
        ("subq", &["psubq"]),
        ("add8", &["paddd"]),
        ("bits", &["pand", "por", "pxor", "pcmpeqd"]),
        ("negd", &["psubd"]),
        ("fops", &["addps", "subps", "mulps", "divps", "xorps"]),
        ("dops", &["addpd", "subpd", "mulpd", "divpd", "xorpd"]),
    ] {
        let body = crate::codegen::asm_probe::body_of(&asm, f);
        for m in want {
            assert!(body.contains(m), "{f}: no {m}:\n{body}");
        }
        // No lane is computed one at a time.
        let lane_ops = [
            "addl", "subl", "addb", "addw", "subb", "subw", "negl", "addss", "addsd",
        ];
        for line in body.lines() {
            let first = line.split_whitespace().next().unwrap_or("");
            assert!(
                !lane_ops.contains(&first),
                "{f}: lane loop ({line}):\n{body}"
            );
        }
    }
}

/// On aarch64 every listed operation is one NEON instruction, at both
/// widths -- the D-register forms for eight bytes, and the scalar `d` form
/// for a single 64-bit lane -- with the standard syntax both Linux and
/// Apple's assembler take.
#[test]
fn vector_native_aarch64_uses_neon_instructions() {
    let src = r#"
typedef signed char v16qi __attribute__((vector_size(16)));
typedef short v4hi __attribute__((vector_size(8)));
typedef int v4si __attribute__((vector_size(16)));
typedef long long v1di __attribute__((vector_size(8)));
typedef float v2sf __attribute__((vector_size(8)));
typedef double v2df __attribute__((vector_size(16)));
void addb(v16qi *d, v16qi *a, v16qi *b) { *d = *a + *b; }
void subh(v4hi *d, v4hi *a, v4hi *b) { *d = *a - *b; }
void bits(v4si *d, v4si *a, v4si *b) { *d = (*a & *b) | (*a ^ ~*b); }
void neg1(v1di *d, v1di *a) { *d = -*a; }
void fops(v2sf *d, v2sf *a, v2sf *b) { *d = -((*a + *b) * (*a - *b) / *b); }
void dops(v2df *d, v2df *a, v2df *b) { *d = *a * *b; }
"#;
    for triple in [
        crate::codegen::asm_probe::AARCH64_LINUX,
        crate::codegen::asm_probe::AARCH64_DARWIN,
    ] {
        let asm = asm_for("vec_native_a64", triple, src);
        for (f, want) in [
            ("addb", &["add v", ".16b"][..]),
            ("subh", &["sub v", ".4h"]),
            ("bits", &["and v", "orr v", "eor v", "not v"]),
            ("neg1", &["neg d"]),
            (
                "fops",
                &["fadd v", "fsub v", "fmul v", "fdiv v", "fneg v", ".2s"],
            ),
            ("dops", &["fmul v", ".2d"]),
        ] {
            let body = crate::codegen::asm_probe::body_of(&asm, f);
            for m in want {
                assert!(body.contains(m), "{triple} {f}: no {m}:\n{body}");
            }
        }
    }
}
