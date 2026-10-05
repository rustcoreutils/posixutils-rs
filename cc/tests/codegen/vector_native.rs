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
    int("v4hu", "unsigned short", 4, "short"),
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

/// A program under construction: its text, the description of each check
/// function `t<id>` so far, and the vector types already declared.
struct Program {
    out: String,
    checks: Vec<String>,
    declared: Vec<String>,
}

impl Program {
    fn new() -> Self {
        Program {
            out: String::from(PRELUDE),
            checks: Vec::new(),
            declared: Vec::new(),
        }
    }

    /// Declare vector type `vec` of `lane`s in `bytes`, once.
    fn typedef(&mut self, lane: &str, vec: &str, bytes: usize) {
        if !self.declared.iter().any(|d| d == vec) {
            self.out.push_str(&format!(
                "typedef {lane} {vec} __attribute__((vector_size({bytes})));\n"
            ));
            self.declared.push(vec.to_string());
        }
    }

    /// `main`, running every check and naming the first that fails.
    fn finish(mut self) -> String {
        self.out.push_str("int main(void) {\n    int r;\n");
        for (id, what) in self.checks.iter().enumerate() {
            let what = what.replace('"', "'");
            self.out.push_str(&format!(
                "    if ((r = t{id}())) {{ __builtin_printf(\"%s: %d\\n\", \"{what}\", r); return 1; }}\n"
            ));
        }
        self.out.push_str("    return 0;\n}\n");
        self.out
    }
}

/// The checks of `shapes` with `ops`, each a (vector expression, lane
/// expression, result type) for a shape -- the result type `V` the shape's
/// own and `M` its mask's.
fn matrix(p: &mut Program, shapes: &[Shape], ops: &[(&str, &str, &str, &str)]) {
    for s in shapes {
        let bytes = s.count * lane_bytes(s.lane);
        p.typedef(s.lane, s.vec, bytes);
        let mask = format!("{}_mask", s.vec);
        p.typedef(s.mask, &mask, bytes);
        for &(expr, want, result, fix) in ops {
            let rtype = if result == "M" { mask.as_str() } else { s.vec };
            let want = want.replace('T', s.lane).replace('M', s.mask);
            let id = p.checks.len();
            check(&mut p.out, id, s, (expr, &want, rtype), fix);
            p.checks.push(format!("{} {expr}", s.vec));
        }
    }
}

/// One program holding every lane check in this file: the integer and
/// floating operator matrices, and every shuffle and conversion.
fn native_program() -> String {
    let mut p = Program::new();
    matrix(&mut p, INT_SHAPES, INT_OPS);
    matrix(&mut p, FLOAT_SHAPES, FLOAT_OPS);
    shuffles_and_conversions(&mut p);
    pressure(&mut p);
    p.finish()
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
    ("a > b", "(M)(a[i] > b[i] ? -1 : 0)", "M", ""),
    ("a <= b", "(M)(a[i] <= b[i] ? -1 : 0)", "M", ""),
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

/// Every lane check -- integer and floating operators, shuffles and
/// conversions -- in one program, run everywhere.
///
/// Consolidates: vector_native_integer_lanes_match_scalars,
/// vector_native_floating_lanes_match_scalars,
/// vector_native_shuffles_and_conversions_match_scalars.
#[test]
fn vector_native_lanes_match_scalars() {
    compile_and_run_everywhere("vec_native", &native_program());
}

/// `__builtin_shufflevector` patterns over `n` lanes: each result lane an
/// index into `a` then `b`, or `-1` for one whose value is unspecified.
fn shuffle_patterns(n: usize) -> Vec<Vec<i32>> {
    let n_i = n as i32;
    let mut pats = vec![
        (0..n_i).collect(),                                  // identity
        (0..n_i).rev().collect(),                            // reverse
        vec![1 % n_i; n],                                    // broadcast
        (0..n_i).map(|k| (k / 2) + (k % 2) * n_i).collect(), // interleave low
        (0..n_i)
            .map(|k| {
                if k < n_i / 2 {
                    n_i / 2 - 1 - k
                } else {
                    n_i + k
                }
            })
            .collect(), // a's low half reversed, then b's high half
        (0..n_i).map(|k| n_i + (k + 1) % n_i).collect(),     // b only, rotated
    ];
    let mut with_unspecified: Vec<i32> = (0..n_i).rev().collect();
    with_unspecified[n / 2] = -1;
    pats.push(with_unspecified);
    pats
}

/// The checks of every shuffle pattern on every shape, and every
/// same-width conversion between integer and floating lanes, against
/// scalar lanes.
fn shuffles_and_conversions(p: &mut Program) {
    let shapes: &[(&str, &str, usize, bool)] = &[
        ("v4si", "int", 4, false),
        ("v4sf", "float", 4, true),
        ("v2di", "long long", 2, false),
        ("v2df", "double", 2, true),
        ("v8hi", "short", 8, false),
        ("v16qi", "signed char", 16, false),
        ("v2si", "int", 2, false),
        ("v8qi", "signed char", 8, false),
        ("v4hi", "short", 4, false),
        ("v2sf", "float", 2, true),
    ];
    for &(vec, lane, n, float) in shapes {
        let bytes = n * lane_bytes(lane);
        p.typedef(lane, vec, bytes);
        let (src, nv) = if float {
            ("fval", "NF")
        } else {
            ("ival", "NI")
        };
        for pat in shuffle_patterns(n) {
            let id = p.checks.len();
            let list: Vec<String> = pat.iter().map(|i| i.to_string()).collect();
            let idx: Vec<String> = pat.iter().map(|i| i.to_string()).collect();
            p.out.push_str(&format!(
                "static int t{id}(void) {{\n\
                 \x20   static const int idx[] = {{{}}};\n\
                 \x20   for (int k = 0; k < {nv}; k++) {{\n\
                 \x20       {vec} a, b;\n\
                 \x20       for (int i = 0; i < {n}; i++) {{\n\
                 \x20           a[i] = ({lane}){src}[(k + i) % {nv}];\n\
                 \x20           b[i] = ({lane}){src}[(k * 5 + i + 3) % {nv}];\n\
                 \x20       }}\n\
                 \x20       {vec} r = __builtin_shufflevector(a, b, {});\n\
                 \x20       for (int i = 0; i < {n}; i++) {{\n\
                 \x20           if (idx[i] < 0) continue;\n\
                 \x20           {lane} got = r[i], want = idx[i] < {n} ? a[idx[i]] : b[idx[i] - {n}];\n\
                 \x20           if (memcmp(&got, &want, sizeof got)) return k * 64 + i + 1;\n\
                 \x20       }}\n\
                 \x20   }}\n\
                 \x20   return 0;\n\
                 }}\n",
                idx.join(", "),
                list.join(", ")
            ));
            p.checks.push(format!("shuffle {vec} {}", list.join(" ")));
        }
    }
    // Conversions: values each conversion defines -- in range, and not
    // negative for an unsigned result.
    let conversions: &[(&str, &str, &str, &str, usize, &str)] = &[
        ("v4si", "int", "v4sf", "float", 4, "ival"),
        ("v4su", "unsigned", "v4sf", "float", 4, "ival"),
        ("v4sf", "float", "v4si", "int", 4, "sval"),
        ("v4sf", "float", "v4su", "unsigned", 4, "uval"),
        ("v2di", "long long", "v2df", "double", 2, "ival"),
        ("v2du", "unsigned long long", "v2df", "double", 2, "ival"),
        ("v2df", "double", "v2di", "long long", 2, "sval"),
        ("v2df", "double", "v2du", "unsigned long long", 2, "uval"),
        ("v2si", "int", "v2sf", "float", 2, "ival"),
        ("v2sf", "float", "v2si", "int", 2, "sval"),
    ];
    p.out.push_str(
        "static volatile double sval[] = {0.0, -0.0, 1.5, -2.5, 1e6, -7.75, 123456.7, -2e9, 0.99};\n\
         static volatile double uval[] = {0.0, 1.5, 1e6, 7.75, 123456.7, 3.9e9, 0.99, 2.5};\n",
    );
    for &(from, from_lane, to, to_lane, n, vals) in conversions {
        for (v, lane) in [(from, from_lane), (to, to_lane)] {
            p.typedef(lane, v, n * lane_bytes(lane));
        }
        let id = p.checks.len();
        let count = format!("(int)(sizeof {vals} / sizeof {vals}[0])");
        p.out.push_str(&format!(
            "static int t{id}(void) {{\n\
             \x20   for (int k = 0; k < {count}; k++) {{\n\
             \x20       {from} a;\n\
             \x20       for (int i = 0; i < {n}; i++) a[i] = ({from_lane}){vals}[(k + i) % {count}];\n\
             \x20       {to} r = __builtin_convertvector(a, {to});\n\
             \x20       for (int i = 0; i < {n}; i++) {{\n\
             \x20           {to_lane} got = r[i], want = ({to_lane})a[i];\n\
             \x20           if (memcmp(&got, &want, sizeof got)) return k * 64 + i + 1;\n\
             \x20       }}\n\
             \x20   }}\n\
             \x20   return 0;\n\
             }}\n"
        ));
        p.checks.push(format!("convert {from} to {to}"));
    }
}

/// Fourteen vectors live across 32-bit multiplies and unsigned orders of
/// words and dwords: every XMM register the allocator hands out holds one,
/// so a temporary that SSE2's multi-instruction forms of those write
/// without the allocator knowing destroys a value the sum then reads.
const PRESSURE: &str = r#"
static v4si pressure_src[16];
static unsigned pressure_word_order(const v4si *x, const v4si *y, int i, int le) {
    unsigned short hx[8], hy[8], m[8];
    memcpy(hx, x, 16);
    memcpy(hy, y, 16);
    for (int w = 0; w < 8; w++)
        m[w] = (le ? hx[w] <= hy[w] : hx[w] < hy[w]) ? 0xffff : 0;
    unsigned out[4];
    memcpy(out, m, 16);
    return out[i];
}
static int tID(void) {
    for (int j = 0; j < 16; j++)
        for (int i = 0; i < 4; i++)
            pressure_src[j][i] = (int)ival[(j * 5 + i * 3 + j / 4) % NI];
    for (int k = 0; k < 16; k++) {
        volatile v4si *s = pressure_src;
#define S(j) ((v4su)s[(k + (j)) % 16])
        /* Right-nested, so each left term is held while the rest -- the
           multiplies and orders innermost -- is computed. */
        v4su sum = S(0) + ((S(1) << 1) + ((S(2) << 2) + ((S(3) << 3)
            + ((S(4) << 4) + ((S(5) << 5) + ((S(6) << 6) + ((S(7) << 7)
            + ((S(8) << 8) + ((S(9) << 9) + ((S(10) << 10) + ((S(11) << 11)
            + ((S(12) << 12) + ((S(13) << 13)
            + (S(0) * S(1) + (((v4su)(S(2) > S(3)) << 1)
            + (((v4su)((v8hu)S(4) <= (v8hu)S(5)) << 2)
            + (((v4su)(S(6) >= S(7)) << 3)
            + (((v4su)((v8hu)S(8) < (v8hu)S(9)) << 4)
            + ((S(10) * S(11)) << 5)))))))))))))))))));
#undef S
        for (int i = 0; i < 4; i++) {
#define A(j) (unsigned)pressure_src[(k + (j)) % 16][i]
#define V(j) &pressure_src[(k + (j)) % 16]
            unsigned want = A(0) * A(1) + ((A(2) > A(3) ? ~0u : 0) << 1)
                + (pressure_word_order(V(4), V(5), i, 1) << 2)
                + ((A(6) >= A(7) ? ~0u : 0) << 3)
                + (pressure_word_order(V(8), V(9), i, 0) << 4)
                + ((A(10) * A(11)) << 5);
            unsigned mix = 0;
            for (int j = 0; j < 14; j++)
                mix += A(j) << j;
            want += mix;
#undef A
#undef V
            if ((unsigned)sum[i] != want) return k * 64 + i + 1;
        }
    }
    return 0;
}
"#;

/// The [`PRESSURE`] check.
fn pressure(p: &mut Program) {
    p.typedef("int", "v4si", 16);
    p.typedef("unsigned", "v4su", 16);
    p.typedef("unsigned short", "v8hu", 16);
    let id = p.checks.len();
    p.out.push_str(&PRESSURE.replace("tID", &format!("t{id}")));
    p.checks
        .push("vectors live across multiplies and unsigned orders".into());
}

/// The lane matrices again with the `-m` flags that widen what x86-64 has
/// packed -- SSSE3's `pshufb`, SSE4.1's `pmulld`, `pcmpeqq` and unsigned
/// minima, SSE4.2's `pcmpgtq` -- on a host that runs them.
#[cfg(target_arch = "x86_64")]
#[test]
fn vector_native_matrices_at_sse4() {
    if !std::arch::is_x86_feature_detected!("sse4.2") {
        eprintln!("vector_native_matrices_at_sse4: the host has no SSE4.2 to run it");
        return;
    }
    // One program: the integer matrix and the shuffles and conversions
    // this ran as two, with the floating matrix besides.
    let src = native_program();
    let name = "vec_native_sse4";
    for flag in ["-mssse3", "-msse4.2"] {
        for opt in ["-O0", "-O2"] {
            let args = [flag.to_string(), opt.to_string()];
            assert_eq!(
                crate::common::compile_and_run(name, &src, &args),
                0,
                "{name} {flag} {opt}"
            );
        }
    }
}
