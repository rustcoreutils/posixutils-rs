//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Floating complex `*` and `/` of constants, folded by the optimizer.
//
// Each is a call to libgcc's `__mul?c3` or `__div?c3`, which the optimizer
// computes when its operands are constant -- bit for bit as the routine
// would, aarch64's fused `__divdc3` included -- and which stays a call
// wherever an operand or the result is an infinity or a NaN.
//

use crate::common::{aarch64_cross_available, compile_and_run, compile_and_run_aarch64};

/// Every product and quotient folded from constants, against the same
/// operations on `volatile` operands and against libgcc's routine called
/// directly -- gcc's own run-time answer -- bit for bit, specials included.
///
/// Returns the number of the first case that disagrees. `BYTES` is how much
/// of a half is its value: x87's ten bytes, not the sixteen it occupies.
const EXACT: &str = r#"
#include <string.h>
typedef TY ty;
typedef TY _Complex cty;
RTY _Complex MULFN(RTY, RTY, RTY, RTY);
RTY _Complex DIVFN(RTY, RTY, RTY, RTY);

static int n;
static int same(cty x, cty y)
{
    ty a[2], b[2];
    memcpy(a, &x, sizeof a);
    memcpy(b, &y, sizeof b);
    return memcmp(&a[0], &b[0], BYTES) == 0 && memcmp(&a[1], &b[1], BYTES) == 0;
}

static int check(cty mul, cty quo, ty a, ty b, ty c, ty d)
{
    volatile ty va = a, vb = b, vc = c, vd = d;
    cty x = __builtin_complex((ty)va, (ty)vb), y = __builtin_complex((ty)vc, (ty)vd);
    cty rmul = x * y, rquo = x / y;
    cty lmul = MULFN(va, vb, vc, vd), lquo = DIVFN(va, vb, vc, vd);
    n++;
    return same(mul, rmul) && same(mul, lmul) && same(quo, rquo) && same(quo, lquo);
}

#define C(A, B) __builtin_complex((ty)(A), (ty)(B))
#define CASE(A, B, C_, D) \
    if (!check(C(A, B) * C(C_, D), C(A, B) / C(C_, D), (ty)(A), (ty)(B), (ty)(C_), (ty)(D))) \
        return n;
#define INF __builtin_inf()
#define NAN __builtin_nan("")

int main(void)
{
    CASE(1, 2, 3, -1)
    CASE(1, 2, 3, 4)
    CASE(1, 1, 3, 7)
    CASE(1, 1, 7, 3)
    CASE(1, 2, 5, 6)
    CASE(0.1, 0.2, 0.3, 0.7)
    CASE(-1.5, 2.25, 0.125, -3)
    CASE(-0.0, 0.0, 1, 2)
    CASE(0.0, -0.0, -1, 2)
    CASE(0, 0, 0, 0)
    CASE(1, -1, 0, 0)
    CASE(INF, 0, 1, 1)
    CASE(INF, NAN, 1, 1)
    CASE(NAN, 1, 2, 3)
    CASE(1, 1, INF, 0)
    CASE(1, 1, 0, INF)
    CASE(INF, INF, INF, INF)
    CASE(300, 400, 0.001, 200)
    CASE(1000, 2000, 30000, 40000)
    CASE(0.0001, 0.0002, 0.0003, 0.0004)
    EXTRA
    return 0;
}
"#;

/// Operands at the ends of each range: overflow, underflow, subnormals, and
/// the scalings libgcc applies against them.
const WIDE: &str = "CASE(1e300, 1e300, 1e300, 1e300) CASE(1e-300, 2e-300, 3e-300, 4e-300) \
    CASE(1.7e308, 1.7e308, 1.7e308, 1.7e308) CASE(1e-310, 1e-310, 3e-310, 1e-320) \
    CASE(1 + 0x1p-30, 1, 3, 1) CASE(1, 0x1p-600, 0x1p-600, 1) CASE(1e300, 1, 1e-300, 1e300) \
    CASE(0.1, 0.7, 3, 0x1p-1060)";
const FLOAT: &str = "CASE(1e30, 1e30, 1e30, 1e30) CASE(1e-30, 2e-30, 3e-30, 4e-30) \
    CASE(3e38, 3e38, 3e38, 3e38) CASE(1e-40, 1e-40, 1e-40, 1e-40)";
const HALF: &str = "CASE(60000, 60000, 60000, 60000) CASE(0.00001, 0.00002, 0.00003, 0.00004)";

/// One format for `EXACT`: the type, the type its routine takes halves of
/// (`float` for `_Float16`), the routine's letter in `__mulRc3`/`__divRc3`,
/// the bytes of a half that are its value, and the extra cases.
type Format = (&'static str, &'static str, &'static str, u32, &'static str);

/// The names `EXACT` and `F16_EXACT` define at file scope. Each section of
/// the program `exact_all` builds renames them with a `#define`, so one
/// program holds every format.
const EXACT_NAMES: [&str; 5] = ["ty", "cty", "n", "same", "check"];

/// The macros `EXACT` and its parameters define, undone after each section.
const EXACT_MACROS: [&str; 10] = [
    "TY", "RTY", "MULFN", "DIVFN", "BYTES", "EXTRA", "C", "CASE", "INF", "NAN",
];

/// How far apart the sections' exit codes are: `EXACT` returns the number of
/// its first disagreeing case, at most 28.
const SECTION_CODES: i32 = 30;

/// One section of [`exact_all`]: `body`, whose `int main(void)` becomes
/// section `k`'s function, with its file-scope names renamed.
fn exact_section(k: usize, defs: &str, body: &str) -> String {
    let mut s = String::new();
    for name in EXACT_NAMES {
        s.push_str(&format!("#define {name} {name}_{k}\n"));
    }
    s.push_str(defs);
    s.push_str(&body.replacen(
        "int main(void)",
        &format!("__attribute__((noinline)) static int exact_{k}(void)"),
        1,
    ));
    for name in EXACT_NAMES.iter().chain(EXACT_MACROS.iter()) {
        s.push_str(&format!("#undef {name}\n"));
    }
    s
}

/// `EXACT` for every format in `formats`, followed by `tail` (a program with
/// the same shape, or nothing), as one program. Section `k` exits with
/// `k * SECTION_CODES` plus the number of its first case that disagrees.
fn exact_all(formats: &[Format], tail: Option<&str>) -> String {
    let mut src = String::from("#include <string.h>\n");
    let mut sections = 0;
    for (k, (ty, rty, r, bytes, extra)) in formats.iter().enumerate() {
        let defs = format!(
            "#define TY {ty}\n#define RTY {rty}\n#define MULFN __mul{r}c3\n\
             #define DIVFN __div{r}c3\n#define BYTES {bytes}\n#define EXTRA {extra}\n"
        );
        src.push_str(&exact_section(k, &defs, EXACT));
        sections += 1;
    }
    if let Some(tail) = tail {
        src.push_str(&exact_section(sections, "", tail));
        sections += 1;
    }
    src.push_str("int main(void)\n{\n    int r;\n");
    for k in 0..sections {
        src.push_str(&format!(
            "    if ((r = exact_{k}())) return {} + r;\n",
            k as i32 * SECTION_CODES
        ));
    }
    src.push_str("    return 0;\n}\n");
    src
}

/// Which section, and which case in it, an exit code from [`exact_all`] is.
fn exact_failure(code: i32, formats: &[Format]) -> String {
    let k = (code / SECTION_CODES) as usize;
    let what = formats.get(k).map_or("_Float16 arithmetic", |f| f.0);
    format!("{what}: case {} disagrees", code % SECTION_CODES)
}

/// On the host, where libgcc is the runtime: x86-64 Linux.
///
/// Also `float16_folds_exactly_on_x86_64`: `_Float16` folded from constants
/// and computed from `volatile` operands on the host, bit for bit -- the
/// last section of the same program.
#[test]
fn complex_folds_exactly_on_x86_64() {
    if !cfg!(all(target_os = "linux", target_arch = "x86_64")) {
        return;
    }
    let formats: [Format; 5] = [
        ("float", "float", "s", 4, FLOAT),
        ("double", "double", "d", 8, WIDE),
        ("long double", "long double", "x", 10, WIDE),
        ("_Float16", "float", "s", 2, HALF),
        ("_Float128", "_Float128", "t", 16, WIDE),
    ];
    let src = exact_all(&formats, Some(F16_EXACT));
    for opt in ["-O0", "-O2"] {
        let code = compile_and_run("cfold_exact", &src, &[opt.to_string()]);
        assert_eq!(code, 0, "{opt}: {}", exact_failure(code, &formats));
    }
}

/// On aarch64 Linux under qemu, where `__divdc3` fuses its steps.
#[test]
fn complex_folds_exactly_on_aarch64() {
    if !aarch64_cross_available() {
        eprintln!("skipping: no aarch64 cross toolchain");
        return;
    }
    let formats: [Format; 5] = [
        ("float", "float", "s", 4, FLOAT),
        ("double", "double", "d", 8, WIDE),
        ("long double", "long double", "t", 16, WIDE),
        ("_Float16", "float", "s", 2, HALF),
        ("_Float128", "_Float128", "t", 16, WIDE),
    ];
    // Two programs: `long double` and `_Float128` are distinct types that
    // share the binary128 routines here, so one unit cannot declare
    // `__multc3` for both.
    for part in [&formats[..4], &formats[4..]] {
        let src = exact_all(part, None);
        for opt in ["-O0", "-O2"] {
            let code = compile_and_run_aarch64("cfold_exact_a64", &src, opt);
            assert_eq!(
                code,
                Some(0),
                "{opt}: {}",
                code.map_or_else(|| "no exit code".to_string(), |c| exact_failure(c, part))
            );
        }
    }
}

/// `_Float16` folded from constants and computed from `volatile` operands
/// on the host, bit for bit. Run as the last section of
/// `complex_folds_exactly_on_x86_64`'s program; returns the number of the
/// first check that disagrees.
const F16_EXACT: &str = r#"
        #include <string.h>
        static int n;
        static int same(_Float16 c, _Float16 r) { n++; return memcmp(&c, &r, 2) == 0; }
        volatile _Float16 v1 = 1.0, v3 = 3.0, v01 = 0.1, vbig = 60000, vtiny = 0x1p-24;
        int main(void) {
            _Float16 c1 = 1.0, c3 = 3.0, c01 = 0.1, cbig = 60000, ctiny = 0x1p-24;
            _Float16 r1 = v1, r3 = v3, r01 = v01, rbig = vbig, rtiny = vtiny;
            if (!same(c1 / c3, r1 / r3)) return n;
            if (!same(c01 * c3, r01 * r3)) return n;
            if (!same(c01 - c3, r01 - r3)) return n;
            if (!same(cbig + cbig, rbig + rbig)) return n;
            if (!same(ctiny / c3, rtiny / r3)) return n;
            if (!same(-c01, -r01)) return n;
            if (!same((_Float16)(double)c01, (_Float16)(double)r01)) return n;
            if (!same((_Float16)12345, (_Float16)(int)(r1 * (_Float16)12345))) return n;
            n++; if ((c1 < c3) != (r1 < r3)) return n;
            n++; if ((int)(c3 * c3) != (int)(r3 * r3)) return n;
            return 0;
        }
    "#;

/// A folded product or quotient equals the one the target's own routine
/// computes, on the host, for operands the folder folds.
///
/// The fold and the routine have to agree, and which routine that is depends
/// on the target: Linux ships libgcc's `__mul?c3`/`__div?c3`, Apple and
/// FreeBSD ship compiler-rt's, and the two divide differently -- libgcc by
/// Smith's method and its own thresholds, compiler-rt by the textbook
/// formula with the divisor scaled from `logb` of its larger half. c17
/// folded as libgcc divides wherever it folded at all, so on Darwin a static
/// quotient sat one place from the `__divdc3` beside it:
///
/// ```text
///     static double _Complex s = (0.1 + 0.7i) / (0.3 + 0.9i);
///     static   0.73333333333333328152
///     runtime  0.73333333333333339255
/// ```
///
/// Each pair below is computed twice in the one program -- once as a static
/// initializer, which c17 folds, and once from `volatile` operands, which
/// calls the routine -- so the comparison is against whatever the host
/// actually links, and the test says nothing about which routine that is.
#[test]
fn folded_complex_arithmetic_matches_the_hosts_own_routine() {
    let mut src = String::from(
        r#"
#include <complex.h>
#include <stdio.h>
static int bad;
/* Prints the first few disagreements: a bare count leaves whoever sees this
   fail with nothing to go on but the number, and the operands are what say
   which step of the routine was modelled wrongly. */
static void chk(const char *what, double _Complex s, double _Complex r) {
    if (__real__ s != __real__ r || __imag__ s != __imag__ r) {
        if (bad < 8)
            printf("%s\n  folded  (%.17g, %.17g)\n  routine (%.17g, %.17g)\n",
                   what, __real__ s, __imag__ s, __real__ r, __imag__ r);
        bad++;
    }
}
"#,
    );
    // Magnitudes from subnormal-adjacent to huge, so the scaling each
    // routine does for an extreme divisor is exercised as well as the
    // ordinary case neither scales.
    let vals = [
        "0.1", "0.7", "0.3", "0.9", "2.25", "1e-5", "1e5", "123.456", "7.0", "1e-100", "1e100",
        "6.02e23",
    ];
    let mut cases = Vec::new();
    for (i, a) in vals.iter().enumerate() {
        for (j, b) in vals.iter().enumerate() {
            let (c, d) = (vals[(i + 5) % vals.len()], vals[(j + 7) % vals.len()]);
            let sign = if (i + j) % 2 == 0 { "-" } else { "" };
            cases.push((i * vals.len() + j, *a, *b, c, format!("{sign}{d}")));
        }
    }
    for (n, a, b, c, d) in &cases {
        src.push_str(&format!(
            "static double _Complex sm{n} = ({a} + {b}i) * ({c} + {d}i);\n\
             static double _Complex sd{n} = ({a} + {b}i) / ({c} + {d}i);\n"
        ));
    }
    src.push_str("int main(void) {\n");
    for (n, a, b, c, d) in &cases {
        src.push_str(&format!(
            "  {{ volatile double _Complex x = {a} + {b}i, y = {c} + {d}i;\n\
             \x20   chk(\"({a} + {b}i) * ({c} + {d}i)\", sm{n}, x * y);\n\
             \x20   chk(\"({a} + {b}i) / ({c} + {d}i)\", sd{n}, x / y); }}\n"
        ));
    }
    src.push_str("  return bad;\n}\n");

    for opt in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run("fold_vs_routine", &src, &[opt.to_string()]),
            0,
            "{opt}: a folded product or quotient differs from the routine's"
        );
    }
}
