//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU vector operations, seen in the assembly: the packed instructions each
// target uses. Moved from `tests/codegen/vector_native.rs`, whose lane
// matrices check the same operations' answers by running them.
//

use super::asm_probe::{asm_for, X86_64_LINUX};

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
        let body = super::asm_probe::body_of(&asm, f);
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
        super::asm_probe::AARCH64_LINUX,
        super::asm_probe::AARCH64_DARWIN,
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
            let body = super::asm_probe::body_of(&asm, f);
            for m in want {
                assert!(body.contains(m), "{triple} {f}: no {m}:\n{body}");
            }
        }
    }
}

/// Scalar operands, shifts and multiplies: SSE2 spreads a scalar with a
/// shuffle, shifts every lane by one count (an immediate, or a count in an
/// XMM register) and multiplies words; NEON spreads with `dup`, shifts by
/// per-lane counts (right as a left shift by negated counts) and multiplies
/// up to 32-bit lanes.
#[test]
fn vector_native_splat_shift_and_multiply() {
    let src = r#"
typedef short v8hi __attribute__((vector_size(16)));
typedef int v4si __attribute__((vector_size(16)));
typedef unsigned v4su __attribute__((vector_size(16)));
typedef float v4sf __attribute__((vector_size(16)));
void mulw(v8hi *d, v8hi *a, v8hi *b) { *d = *a * *b; }
void shifts(v4si *d, v4si *a, int s) { *d = (*a << s) + (*a >> 3); }
void lsr(v4su *d, v4su *a, unsigned s) { *d = *a >> s; }
void splat(v4sf *d, v4sf *a, float s) { *d = *a * s; }
void splati(v4si *d, v4si *a, int s) { *d = *a + s; }
"#;
    let x86 = asm_for("vec_native_x86_ext", X86_64_LINUX, src);
    let a64 = asm_for("vec_native_a64_ext", super::asm_probe::AARCH64_LINUX, src);
    for (asm, f, want) in [
        (&x86, "mulw", &["pmullw"][..]),
        (&x86, "shifts", &["pslld", "psrad $3", "paddd"]),
        (&x86, "lsr", &["psrld"]),
        (&x86, "splat", &["shufps $0", "mulps"]),
        (&x86, "splati", &["pshufd $0", "paddd"]),
        (&a64, "mulw", &["mul v", ".8h"]),
        (&a64, "shifts", &["dup v", "ushl v", "neg v", "sshl v"]),
        (&a64, "lsr", &["neg v", "ushl v"]),
        (&a64, "splat", &["dup v", ".s[0]", "fmul v"]),
        (&a64, "splati", &["dup v", "add v"]),
    ] {
        let body = super::asm_probe::body_of(asm, f);
        for m in want {
            assert!(body.contains(m), "{f}: no {m}:\n{body}");
        }
    }
}

/// Comparisons give their masks with packed compares: SSE2's `pcmpeq` and
/// signed `pcmpgt` (with an inverse for `!=` and `>=`) and `cmpps` with C's
/// predicates; NEON's `cmeq`/`cmgt`/`cmge`/`cmhi` and floating forms.
#[test]
fn vector_native_comparisons() {
    let src = r#"
typedef int v4si __attribute__((vector_size(16)));
typedef unsigned v4su __attribute__((vector_size(16)));
typedef float v4sf __attribute__((vector_size(16)));
void ints(v4si *d, v4si *a, v4si *b) { *d = (*a < *b) | (*a >= *b) | (*a != *b); }
void uns(v4si *d, v4su *a, v4su *b) { *d = *a > *b; }
void flts(v4si *d, v4sf *a, v4sf *b) { *d = (*a > *b) | (*a != *b) | (*a <= *b); }
"#;
    let x86 = asm_for("vec_cmp_x86", X86_64_LINUX, src);
    let a64 = asm_for("vec_cmp_a64", super::asm_probe::AARCH64_LINUX, src);
    for (asm, f, want) in [
        (&x86, "ints", &["pcmpgtd", "pcmpeqd", "pxor"][..]),
        (&x86, "flts", &["cmpltps", "cmpneqps", "cmpleps"]),
        (&a64, "ints", &["cmgt v", "cmge v", "cmeq v", "not v"]),
        (&a64, "uns", &["cmhi v"]),
        (&a64, "flts", &["fcmgt v", "fcmeq v", "fcmge v"]),
    ] {
        let body = super::asm_probe::body_of(asm, f);
        for m in want {
            assert!(body.contains(m), "{f}: no {m}:\n{body}");
        }
    }
}

/// Constant shuffles and conversions: SSE2's pshufd, shufps and shufpd and
/// cvtdq2ps/cvttps2dq; NEON's `tbl` -- one table register or a pair -- and
/// scvtf/ucvtf/fcvtzs/fcvtzu.
#[test]
fn vector_native_shuffle_and_convert_instructions() {
    let src = r#"
typedef int v4si __attribute__((vector_size(16)));
typedef unsigned v4su __attribute__((vector_size(16)));
typedef float v4sf __attribute__((vector_size(16)));
typedef double v2df __attribute__((vector_size(16)));
void rev(v4si *d, v4si *a) { *d = __builtin_shufflevector(*a, *a, 3, 2, 1, 0); }
void mix(v4sf *d, v4sf *a, v4sf *b) { *d = __builtin_shufflevector(*a, *b, 1, 0, 6, 7); }
void pd(v2df *d, v2df *a, v2df *b) { *d = __builtin_shufflevector(*a, *b, 3, 0); }
void cvt(v4si *d, v4sf *a) { *d = __builtin_convertvector(*a, v4si); }
void ucvt(v4sf *d, v4su *a) { *d = __builtin_convertvector(*a, v4sf); }
"#;
    let x86 = asm_for("vec_shuf_x86", X86_64_LINUX, src);
    let a64 = asm_for("vec_shuf_a64", super::asm_probe::AARCH64_LINUX, src);
    for (asm, f, want) in [
        (&x86, "rev", &["pshufd $27"][..]),
        (&x86, "mix", &["shufps $225"]),
        (&x86, "pd", &["shufpd $1"]),
        (&x86, "cvt", &["cvttps2dq"]),
        (&a64, "rev", &["tbl v", "{v17.16b}"]),
        (&a64, "mix", &["tbl v", "{v17.16b, v18.16b}"]),
        (&a64, "cvt", &["fcvtzs v", ".4s"]),
        (&a64, "ucvt", &["ucvtf v", ".4s"]),
    ] {
        let body = super::asm_probe::body_of(asm, f);
        for m in want {
            assert!(body.contains(m), "{f}: no {m}:\n{body}");
        }
    }
}

/// What SSE2 builds from several instructions where SSE4.1 has one: a
/// 32-bit lane multiply from two `pmuludq` (even lanes, then odd ones moved
/// down) gathered with `pshufd`/`punpckldq`, and an unsigned order of
/// words or dwords as the signed `pcmpgt` of operands whose sign bits are
/// flipped. Both register widths: an eight-byte vector rides in the low
/// half of an XMM register. No lane is computed one at a time.
#[test]
fn vector_native_x86_64_sse2_sequences() {
    let src = r#"
typedef int v4si __attribute__((vector_size(16)));
typedef int v2si __attribute__((vector_size(8)));
typedef unsigned v4su __attribute__((vector_size(16)));
typedef unsigned v2su __attribute__((vector_size(8)));
typedef short v8hi __attribute__((vector_size(16)));
typedef short v4hi __attribute__((vector_size(8)));
typedef unsigned short v8hu __attribute__((vector_size(16)));
typedef unsigned short v4hu __attribute__((vector_size(8)));
void mul(v4si *d, v4si *a, v4si *b) { *d = *a * *b; }
void mulu(v4su *d, v4su *a, v4su *b) { *d = *a * *b; }
void mul8(v2si *d, v2si *a, v2si *b) { *d = *a * *b; }
void gtd(v4si *d, v4su *a, v4su *b) { *d = *a > *b; }
void ged(v4si *d, v4su *a, v4su *b) { *d = *a >= *b; }
void ltd(v4si *d, v4su *a, v4su *b) { *d = *a < *b; }
void led(v4si *d, v4su *a, v4su *b) { *d = *a <= *b; }
void gtw(v8hi *d, v8hu *a, v8hu *b) { *d = *a > *b; }
void gew(v8hi *d, v8hu *a, v8hu *b) { *d = *a >= *b; }
void ltw(v8hi *d, v8hu *a, v8hu *b) { *d = *a < *b; }
void lew(v8hi *d, v8hu *a, v8hu *b) { *d = *a <= *b; }
void gtd8(v2si *d, v2su *a, v2su *b) { *d = *a > *b; }
void led8(v2si *d, v2su *a, v2su *b) { *d = *a <= *b; }
void gtw8(v4hi *d, v4hu *a, v4hu *b) { *d = *a > *b; }
void lew8(v4hi *d, v4hu *a, v4hu *b) { *d = *a <= *b; }
"#;
    // Scalar arithmetic, compares and the flag reads a lane loop needs.
    let scalar = [
        "imul", "cmpl", "cmpw", "seta", "setb", "setae", "setbe", "sbb", "cmov",
    ];
    for opt in ["-O0", "-O2"] {
        let asm = super::asm_probe::asm_for_with("vec_sse2_seq", X86_64_LINUX, src, &[opt]);
        for (f, want) in [
            ("mul", &["pmuludq", "punpckldq"][..]),
            ("mulu", &["pmuludq", "punpckldq"]),
            ("mul8", &["pmuludq", "punpckldq"]),
            ("gtd", &["pcmpgtd", "pxor"]),
            ("ged", &["pcmpgtd", "pxor"]),
            ("ltd", &["pcmpgtd", "pxor"]),
            ("led", &["pcmpgtd", "pxor"]),
            ("gtw", &["pcmpgtw", "pxor"]),
            ("gew", &["pcmpgtw", "pxor"]),
            ("ltw", &["pcmpgtw", "pxor"]),
            ("lew", &["pcmpgtw", "pxor"]),
            ("gtd8", &["pcmpgtd"]),
            ("led8", &["pcmpgtd"]),
            ("gtw8", &["pcmpgtw"]),
            ("lew8", &["pcmpgtw"]),
        ] {
            let body = super::asm_probe::body_of(&asm, f);
            for m in want {
                assert!(body.contains(m), "{opt} {f}: no {m}:\n{body}");
            }
            for line in body.lines() {
                let first = line.split_whitespace().next().unwrap_or("");
                assert!(
                    !scalar.iter().any(|s| first.starts_with(s)),
                    "{opt} {f}: lane loop ({line}):\n{body}"
                );
            }
            // The SSE4.1 forms need the flag.
            for m in ["pmulld", "pminu"] {
                assert!(!body.contains(m), "{opt} {f}: {m} without -msse4.1");
            }
        }
    }
}

/// The SSE2 sequences need a third XMM temporary beyond the two scratch
/// registers, which the allocator keeps free of every value live across
/// them. Under an `ms_abi` function, where that register is callee-saved,
/// the prologue saves it.
#[test]
fn vector_native_x86_64_sse2_third_scratch_is_saved_under_ms_abi() {
    let src = r#"
typedef int v4si __attribute__((vector_size(16)));
__attribute__((ms_abi)) void mul(v4si *d, v4si *a, v4si *b) { *d = *a * *b; }
"#;
    let asm = asm_for("vec_sse2_ms_abi", X86_64_LINUX, src);
    let body = super::asm_probe::body_of(&asm, "mul");
    assert!(body.contains("pmuludq"), "no pmuludq:\n{body}");
    assert!(
        body.lines()
            .any(|l| l.contains("%xmm13") && l.contains("(%r")),
        "xmm13 not saved:\n{body}"
    );
}

/// What each `-m` level adds to the packed instructions x86-64 uses.
#[test]
fn vector_native_x86_64_isa_levels() {
    let src = r#"
typedef int v4si __attribute__((vector_size(16)));
typedef long long v2di __attribute__((vector_size(16)));
typedef unsigned v4su __attribute__((vector_size(16)));
typedef unsigned char v16qu __attribute__((vector_size(16)));
typedef short v8hi __attribute__((vector_size(16)));
void mul(v4si *d, v4si *a, v4si *b) { *d = *a * *b; }
void eqq(v2di *d, v2di *a, v2di *b) { *d = *a == *b; }
void gtq(v2di *d, v2di *a, v2di *b) { *d = *a > *b; }
void geu(v4si *d, v4su *a, v4su *b) { *d = *a >= *b; }
void gtub(v16qu *d, v16qu *a, v16qu *b) { *d = *a > *b; }
void rev(v8hi *d, v8hi *a) { *d = __builtin_shufflevector(*a, *a, 7, 6, 5, 4, 3, 2, 1, 0); }
"#;
    let at = |flags: &[&str]| {
        let mut args = vec!["-O"];
        args.extend(flags);
        super::asm_probe::asm_for_with("vec_isa", X86_64_LINUX, src, &args)
    };
    let has = |asm: &str, f: &str, m: &str| super::asm_probe::body_of(asm, f).contains(m);
    let base = at(&[]);
    assert!(
        has(&base, "gtub", "pminub"),
        "SSE2 has the unsigned byte order"
    );
    // SSE2 builds what SSE4.1 has as one instruction.
    assert!(has(&base, "mul", "pmuludq") && has(&base, "geu", "pcmpgtd"));
    for (f, m) in [
        ("mul", "pmulld"),
        ("eqq", "pcmpeqq"),
        ("gtq", "pcmpgtq"),
        ("geu", "pminud"),
        ("rev", "pshufb"),
    ] {
        assert!(!has(&base, f, m), "{f}: {m} without the flag");
    }
    let ssse3 = at(&["-mssse3"]);
    assert!(has(&ssse3, "rev", "pshufb") && !has(&ssse3, "mul", "pmulld"));
    let sse41 = at(&["-msse4.1"]);
    for (f, m) in [("mul", "pmulld"), ("eqq", "pcmpeqq"), ("geu", "pminud")] {
        assert!(has(&sse41, f, m), "{f}: no {m} at SSE4.1");
    }
    assert!(!has(&sse41, "mul", "pmuludq") && !has(&sse41, "geu", "pcmpgtd"));
    assert!(!has(&sse41, "gtq", "pcmpgtq"));
    let sse42 = at(&["-msse4.2"]);
    assert!(has(&sse42, "gtq", "pcmpgtq"));
}
