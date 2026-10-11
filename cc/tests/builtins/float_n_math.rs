//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The math builtins glibc's sources name that c17 lacked: `roundeven`,
// `lrint`/`llrint`/`lround`/`llround`, `scalbn`/`scalbln` in every type,
// and the `_FloatN`/`_FloatNx` forms of the rounding, `sqrt`, `fmin`,
// `fmax`, `fma`, `logb`, `ilogb`, `ldexp`, `frexp`, `modf`, `nextafter`,
// `fdim`, `cbrt` and `hypot` families. Each is computed as gcc 13
// computes it: in place where gcc has an instruction for it, else by a
// call of the libm function of the builtin's own name.
//

use crate::common::{asm_for_at, preprocess_text};

/// The suffixes of every type, and of the `_FloatN` ones alone.
const EVERY_TYPE: &[&str] = &["", "f", "l", "f32", "f64", "f32x", "f64x", "f128"];
const FLOAT_N: &[&str] = &["f32", "f64", "f32x", "f64x", "f128"];

/// Every family this adds names to, with the suffixes it adds.
const FAMILIES: &[(&str, &[&str])] = &[
    ("roundeven", EVERY_TYPE),
    ("lrint", EVERY_TYPE),
    ("llrint", EVERY_TYPE),
    ("lround", EVERY_TYPE),
    ("llround", EVERY_TYPE),
    ("scalbn", EVERY_TYPE),
    ("scalbln", EVERY_TYPE),
    ("round", FLOAT_N),
    ("rint", FLOAT_N),
    ("nearbyint", FLOAT_N),
    ("trunc", FLOAT_N),
    ("floor", FLOAT_N),
    ("ceil", FLOAT_N),
    ("logb", FLOAT_N),
    ("sqrt", FLOAT_N),
    ("cbrt", FLOAT_N),
    ("fmin", FLOAT_N),
    ("fmax", FLOAT_N),
    ("fma", FLOAT_N),
    ("ilogb", FLOAT_N),
    ("ldexp", FLOAT_N),
    ("frexp", FLOAT_N),
    ("modf", FLOAT_N),
    ("nextafter", FLOAT_N),
    ("fdim", FLOAT_N),
    ("hypot", FLOAT_N),
];

/// Every one is a builtin `__has_builtin` knows, on both Linux targets.
#[test]
fn float_n_math_builtins_are_known() {
    let mut src = String::new();
    for (family, suffixes) in FAMILIES {
        for suffix in *suffixes {
            let name = format!("__builtin_{family}{suffix}");
            src += &format!("#if !__has_builtin({name})\n#error {name}\n#endif\n");
        }
    }
    for target in ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"] {
        let r = preprocess_text("float_n_known", &src, &[&format!("--target={target}")]);
        assert!(r.success, "{target}: {}", r.stderr);
    }
}

/// Each `_FloatN` builtin computes what the standard function of its
/// format does, and the families new in every type compute their
/// functions' values. Linked with -lm, where a builtin is a call.
#[cfg(target_os = "linux")]
#[test]
fn float_n_math_builtins_compute_their_functions() {
    // The standard type and suffix sharing each `_FloatN` format: x86-64's
    // `long double` and aarch64 Linux's are `_Float64x`'s.
    let formats = [
        ("f32", "_Float32", "f"),
        ("f64", "_Float64", ""),
        ("f32x", "_Float32x", ""),
        ("f64x", "_Float64x", "l"),
    ];
    let mut body = String::new();
    let mut code = 0;
    let mut check = |cond: String| {
        code += 1;
        body += &format!("    if (!({cond})) return failed({code});\n");
    };
    for (suffix, ty, std) in formats {
        for x in ["2.5", "-3.75", "0.5", "1048576.5", "7.0"] {
            for f in [
                "round",
                "rint",
                "nearbyint",
                "trunc",
                "floor",
                "ceil",
                "logb",
                "cbrt",
                "roundeven",
            ] {
                check(format!(
                    "__builtin_{f}{suffix}(({ty}){x}) == __builtin_{f}{std}({x})"
                ));
            }
            check(format!(
                "__builtin_sqrt{suffix}(({ty})__builtin_fabs({x})) == \
                 __builtin_sqrt{std}(__builtin_fabs({x}))"
            ));
            check(format!(
                "__builtin_ilogb{suffix}(({ty}){x}) == __builtin_ilogb{std}({x})"
            ));
            for f in ["fmin", "fmax", "nextafter", "fdim", "hypot"] {
                check(format!(
                    "__builtin_{f}{suffix}(({ty}){x}, ({ty})1.25) == __builtin_{f}{std}({x}, 1.25)"
                ));
            }
            check(format!(
                "__builtin_fma{suffix}(({ty}){x}, ({ty})3, ({ty})0.125) == \
                 __builtin_fma{std}({x}, 3, 0.125)"
            ));
            check(format!(
                "__builtin_ldexp{suffix}(({ty}){x}, 3) == __builtin_ldexp{std}({x}, 3)"
            ));
            check(format!(
                "({{ int e1, e2; {ty} m = __builtin_frexp{suffix}(({ty}){x}, &e1); \
                 m == __builtin_frexp{std}({x}, &e2) && e1 == e2; }})"
            ));
            check(format!(
                "({{ {ty} i1; __typeof__(__builtin_modf{std}(0, 0)) i2; \
                 __builtin_modf{suffix}(({ty}){x}, &i1) == __builtin_modf{std}({x}, &i2) \
                 && i1 == i2; }})"
            ));
        }
    }
    // The families new in every type, by value.
    for (suffix, ty) in [
        ("", "double"),
        ("f", "float"),
        ("l", "long double"),
        ("f32", "_Float32"),
        ("f64", "_Float64"),
        ("f32x", "_Float32x"),
        ("f64x", "_Float64x"),
        ("f128", "_Float128"),
    ] {
        for (x, even, rint, round) in [
            ("2.5", "2", "2", "3"),
            ("3.5", "4", "4", "4"),
            ("-2.5", "-2", "-2", "-3"),
            ("0.25", "0", "0", "0"),
            ("-7.75", "-8", "-8", "-8"),
        ] {
            check(format!("__builtin_roundeven{suffix}(({ty}){x}) == {even}"));
            check(format!("__builtin_lrint{suffix}(({ty}){x}) == {rint}"));
            check(format!("__builtin_llrint{suffix}(({ty}){x}) == {rint}"));
            check(format!("__builtin_lround{suffix}(({ty}){x}) == {round}"));
            check(format!("__builtin_llround{suffix}(({ty}){x}) == {round}"));
        }
        check(format!(
            "__builtin_signbit(__builtin_roundeven{suffix}(({ty})-0.5))"
        ));
        check(format!("__builtin_scalbn{suffix}(({ty})3, 4) == 48"));
        check(format!("__builtin_scalbln{suffix}(({ty})3, -1L) == 1.5"));
        check(format!(
            "__builtin_lrint{suffix}(({ty})-3.0) == -3L && sizeof(__builtin_llrint{suffix}(1)) == 8"
        ));
    }
    // binary128, which no standard type of x86-64 has, by value.
    for (expr, want) in [
        ("__builtin_floorf128(-2.5f128)", "-3"),
        ("__builtin_ceilf128(-2.5f128)", "-2"),
        ("__builtin_truncf128(-2.5f128)", "-2"),
        ("__builtin_roundf128(-2.5f128)", "-3"),
        ("__builtin_rintf128(2.5f128)", "2"),
        ("__builtin_nearbyintf128(3.5f128)", "4"),
        ("__builtin_logbf128(48.0f128)", "5"),
        ("__builtin_ilogbf128(48.0f128)", "5"),
        ("__builtin_cbrtf128(27.0f128)", "3"),
        ("__builtin_sqrtf128(2.25f128)", "1.5"),
        ("__builtin_fminf128(1.0f128, -2.0f128)", "-2"),
        ("__builtin_fmaxf128(1.0f128, -2.0f128)", "1"),
        ("__builtin_fmaf128(2.0f128, 3.0f128, 0.5f128)", "6.5"),
        ("__builtin_ldexpf128(3.0f128, 2)", "12"),
        ("__builtin_fdimf128(5.0f128, 1.5f128)", "3.5"),
        ("__builtin_hypotf128(3.0f128, 4.0f128)", "5"),
        ("__builtin_nextafterf128(1.0f128, 2.0f128) > 1", "1"),
        (
            "({ int e; __builtin_frexpf128(48.0f128, &e) * 0 + e; })",
            "6",
        ),
        (
            "({ _Float128 i; __builtin_modff128(2.75f128, &i) + i; })",
            "2.75",
        ),
    ] {
        check(format!("(double)({expr}) == {want}"));
    }
    // More checks than exit statuses: each failure says which it was.
    let program = format!(
        "static int failed(int n) {{ __builtin_printf(\"check %d failed\\n\", n); return 1; }}\n\
         int main(void) {{\n{body}    return 0;\n}}\n"
    );
    super::libm::run_everywhere("float_n_math", &program, &[], true);
    // And with SSE4.1's `roundsd` computing the roundings in place.
    if super::libm::host_has("sse4.1") {
        for opt in ["-O0", "-O2"] {
            let flags: Vec<String> = [opt, "-msse4.1", "-lm"].map(String::from).to_vec();
            let rc = crate::common::compile_and_run("float_n_math_sse41", &program, &flags);
            assert_eq!(rc, 0, "-msse4.1 {opt}");
        }
    }
}

/// `roundeven` is an instruction where gcc has one -- `frintn` on aarch64,
/// SSE4.1's `roundsd $8` on x86-64 -- and a call of `roundeven` where it
/// has not; SSE4.1 takes the other roundings to `roundsd` too, but for
/// `round`, which it cannot do. A constant one is folded.
#[test]
fn float_n_math_roundeven_as_gcc_emits_it() {
    let src = "double e(double x) { return __builtin_roundeven(x); }\n\
               float ef(float x) { return __builtin_roundevenf32(x); }\n\
               double n(double x) { return __builtin_nearbyint(x); }\n\
               double f(double x) { return __builtin_floorf64(x); }\n\
               double r(double x) { return __builtin_round(x); }\n\
               double k(void) { return __builtin_roundeven(2.5) + __builtin_roundeven(-3.5); }\n";
    let x86 = "--target=x86_64-unknown-linux-gnu";
    let base = asm_for_at("c17_re_", src, &[x86, "-O2"]);
    for callee in ["roundeven", "roundevenf32", "nearbyint", "round"] {
        assert!(
            base.lines()
                .any(|l| l.contains("call") && l.contains(&format!(" {callee}"))
                    || l.contains("jmp") && l.contains(&format!(" {callee}"))),
            "no call of {callee}:\n{base}"
        );
    }
    assert!(!base.contains("roundsd"), "{base}");
    let sse41 = asm_for_at("c17_re41_", src, &[x86, "-O2", "-msse4.1"]);
    for insn in ["roundsd $8,", "roundss $8,", "roundsd $12,", "roundsd $9,"] {
        assert!(sse41.contains(insn), "no {insn}:\n{sse41}");
    }
    assert!(!sse41.contains("roundeven"), "{sse41}");
    assert!(sse41.contains("round"), "{sse41}");
    let a64 = asm_for_at(
        "c17_re_a64_",
        src,
        &["--target=aarch64-unknown-linux-gnu", "-O2"],
    );
    assert!(
        a64.contains("frintn d") && a64.contains("frintn s"),
        "{a64}"
    );
    // `k` is 2 + -4, a constant.
    let k = base.split("k:").nth(1).expect("k is defined");
    let k = k.split(".size").next().unwrap_or(k);
    assert!(!k.contains("call"), "{k}");
}
