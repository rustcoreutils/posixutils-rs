//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The x86-64 instruction-set options -- `-mavx2`, `-mfma`, `-mrtm`,
// `-mno-mmx` and the rest of gcc 13's roster -- and what each turns on:
// the feature macros, `__BIGGEST_ALIGNMENT__`, the code chosen by feature,
// and `-msse2avx`'s word to the assembler. Every expectation is gcc 13's.
//

use super::{s, WorkDir};
use crate::common::{asm_for_at, preprocess_text, run_c17};

/// Every macro an x86-64 ISA option can define, in the order the tests
/// list them.
const ISA_MACROS: &[&str] = &[
    "__MMX__",
    "__FXSR__",
    "__SSE3__",
    "__SSSE3__",
    "__SSE4_1__",
    "__SSE4_2__",
    "__POPCNT__",
    "__CRC32__",
    "__SSE4A__",
    "__XSAVE__",
    "__AVX__",
    "__AVX2__",
    "__F16C__",
    "__FMA__",
    "__FMA4__",
    "__XOP__",
    "__AVX512F__",
    "__AVX512BW__",
    "__AVX512DQ__",
    "__AVX512VP2INTERSECT__",
    "__LZCNT__",
    "__ABM__",
    "__RTM__",
    "__SHSTK__",
    "__SHA__",
    "__AMX_TILE__",
    "__AMX_INT8__",
    "__3dNOW__",
    "__FP_FAST_FMA",
    "__FP_FAST_FMAF",
];

/// The ISA macros `flags` define for x86-64, in [`ISA_MACROS`] order with
/// the `__` trimmed, and `__BIGGEST_ALIGNMENT__`'s value.
fn isa_macros(flags: &[&str]) -> (String, String) {
    let mut argv = vec!["--target=x86_64-unknown-linux-gnu", "-dM"];
    argv.extend_from_slice(flags);
    let r = preprocess_text("isa_macros", "", &argv);
    assert!(r.success, "{flags:?}: {}", r.stderr);
    let defined: Vec<(&str, &str)> = r
        .stdout
        .lines()
        .filter_map(|l| l.strip_prefix("#define "))
        .filter_map(|l| l.split_once(' '))
        .collect();
    let on: Vec<&str> = ISA_MACROS
        .iter()
        .filter(|m| defined.iter().any(|(name, _)| name == *m))
        .map(|m| m.trim_matches('_'))
        .collect();
    let biggest = defined
        .iter()
        .find(|(name, _)| *name == "__BIGGEST_ALIGNMENT__")
        .map_or(String::new(), |(_, v)| v.to_string());
    (on.join(" "), biggest)
}

/// What each ISA option defines, alone and in the combinations whose order
/// gcc gives a meaning: an option enables what it needs and disables what
/// needs it, SSE4.2 brings POPCNT and CRC32 only at the end and only if no
/// option named them, and AVX512VP2INTERSECT survives AVX512DQ's removal.
/// Each row is `gcc -dM -E` of gcc 13.3.
#[test]
fn isa_flags_define_gccs_macros() {
    const BASE: &str = "MMX FXSR";
    const SSE42: &str = "MMX FXSR SSE3 SSSE3 SSE4_1 SSE4_2 POPCNT CRC32";
    let avx = format!("{SSE42} XSAVE AVX");
    let rows: Vec<(&str, String, &str)> = vec![
        ("", BASE.into(), "16"),
        ("-msse4", SSE42.into(), "16"),
        ("-mavx", avx.clone(), "32"),
        ("-mavx2", format!("{avx} AVX2"), "32"),
        ("-mfma", format!("{avx} FMA FP_FAST_FMA FP_FAST_FMAF"), "32"),
        (
            "-mfma4",
            "MMX FXSR SSE3 SSSE3 SSE4_1 SSE4_2 POPCNT CRC32 SSE4A XSAVE AVX FMA4 \
             FP_FAST_FMA FP_FAST_FMAF"
                .into(),
            "32",
        ),
        (
            "-mavx512f",
            format!("{avx} AVX2 AVX512F FP_FAST_FMA FP_FAST_FMAF"),
            "64",
        ),
        (
            "-mavx512vp2intersect -mno-avx512dq",
            format!("{avx} AVX2 AVX512F AVX512VP2INTERSECT FP_FAST_FMA FP_FAST_FMAF"),
            "64",
        ),
        ("-mavx2 -mno-avx", format!("{SSE42} XSAVE"), "16"),
        ("-mno-avx -mavx2", format!("{avx} AVX2"), "32"),
        (
            "-mavx -mno-sse4.1",
            "MMX FXSR SSE3 SSSE3 XSAVE".into(),
            "16",
        ),
        (
            "-mno-crc32 -msse4.2",
            "MMX FXSR SSE3 SSSE3 SSE4_1 SSE4_2 POPCNT".into(),
            "16",
        ),
        ("-mabm", "MMX FXSR POPCNT LZCNT ABM".into(), "16"),
        ("-mno-lzcnt -mabm", "MMX FXSR POPCNT ABM".into(), "16"),
        ("-mrtm", "MMX FXSR RTM".into(), "16"),
        ("-mshstk", "MMX FXSR SHSTK".into(), "16"),
        ("-msha", "MMX FXSR SHA".into(), "16"),
        ("-mamx-tile", "MMX FXSR XSAVE AMX_TILE".into(), "16"),
        ("-mamx-int8", "MMX FXSR AMX_TILE AMX_INT8".into(), "16"),
        ("-mamx-int8 -mno-amx-tile", BASE.into(), "16"),
        ("-m3dnow", "MMX FXSR 3dNOW".into(), "16"),
        ("-m3dnow -mno-mmx", "FXSR".into(), "16"),
        ("-mno-fxsr", "MMX".into(), "16"),
        ("-msse2avx", BASE.into(), "16"),
        ("-mno-vzeroupper", BASE.into(), "16"),
    ];
    for (flags, want, biggest) in rows {
        let argv: Vec<&str> = flags.split_whitespace().collect();
        let (got, got_biggest) = isa_macros(&argv);
        assert_eq!(got, want, "{flags}");
        assert_eq!(got_biggest, biggest, "{flags}: __BIGGEST_ALIGNMENT__");
    }
}

/// `__attribute__((aligned))` with no argument asks for
/// `__BIGGEST_ALIGNMENT__`, which AVX raises to 32 and AVX512F to 64.
#[test]
fn isa_flags_bare_aligned_follows_biggest_alignment() {
    let src = "struct __attribute__((aligned)) s { char c; };\n\
               _Static_assert(_Alignof(struct s) == __BIGGEST_ALIGNMENT__, \"bare aligned\");\n\
               _Static_assert(__BIGGEST_ALIGNMENT__ == WANT, \"biggest\");\n\
               int x;\n";
    for (flag, want) in [("-msse4.2", "16"), ("-mavx2", "32"), ("-mavx512f", "64")] {
        let define = format!("-DWANT={want}");
        asm_for_at(
            "c17_isa_aligned_",
            src,
            &["--target=x86_64-unknown-linux-gnu", flag, &define],
        );
    }
}

/// The x86-64 ISA options are not aarch64 options: gcc's aarch64 driver
/// does not know them.
#[test]
fn isa_flags_are_unrecognized_on_aarch64() {
    let w = WorkDir::new("isa_a64");
    let src = w.write("t.c", "int x;\n");
    let out = w.join("t.s");
    for flag in ["-mavx2", "-mno-avx2", "-msse4.2", "-mrtm", "-msse2avx"] {
        let r = run_c17(&[
            "--target=aarch64-unknown-linux-gnu",
            flag,
            "-S",
            &s(&src),
            "-o",
            &s(&out),
        ]);
        assert!(!r.success, "{flag} accepted for aarch64");
        assert!(
            r.stderr
                .contains(&format!("unrecognized command-line option '{flag}'")),
            "{flag}: {}",
            r.stderr
        );
    }
}

/// `-msse2avx` is the assembler's: gcc passes it on, and leaves its own
/// output as it was, so `as` encodes every SSE instruction with a VEX
/// prefix. With `-mavx` it is not passed, as gcc's spec has it. On x86-64
/// only: -msse2avx is an x86-64 option.
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
#[test]
fn isa_flags_sse2avx_reaches_the_assembler() {
    use std::os::unix::fs::PermissionsExt;
    let w = WorkDir::new("sse2avx");
    let dir = w.path().join("pfx");
    std::fs::create_dir(&dir).unwrap();
    let log = dir.join("as-args");
    std::fs::write(
        dir.join("as"),
        format!("#!/bin/sh\necho \"$@\" >> '{}'\nexec as \"$@\"\n", s(&log)),
    )
    .unwrap();
    std::fs::set_permissions(dir.join("as"), std::fs::Permissions::from_mode(0o755)).unwrap();
    let src = w.write("m.c", "double f(double a, double b) { return a * b; }\n");
    let obj = w.join("m.o");
    let prefix = format!("-B{}", s(&dir));
    for (flags, passed) in [
        (&["-msse2avx"][..], true),
        (&["-mavx", "-msse2avx"][..], false),
        (&[][..], false),
    ] {
        let _ = std::fs::remove_file(&log);
        let mut argv = vec![prefix.as_str(), "-c", "-o"];
        let obj_s = s(&obj);
        let src_s = s(&src);
        argv.push(&obj_s);
        argv.push(&src_s);
        argv.extend_from_slice(flags);
        let r = run_c17(&argv);
        assert!(r.success, "{flags:?}: {}", r.stderr);
        let args = std::fs::read_to_string(&log).expect("the -B as ran");
        assert_eq!(
            args.split_whitespace().any(|a| a == "-msse2avx"),
            passed,
            "{flags:?}: {args}"
        );
    }
}

/// With FMA -- `-mfma`, `-mfma4` or AVX512F -- `fma` is one instruction,
/// as in gcc, rounded once; without it, a call to the library's.
#[test]
fn isa_flags_fma_is_inline_with_fma() {
    let src = "double f(double a, double b, double c) { return __builtin_fma(a, b, c); }\n\
               float g(float a, float b, float c) { return __builtin_fmaf(a, b, c); }\n";
    let x86 = "--target=x86_64-unknown-linux-gnu";
    let asm = asm_for_at("c17_isa_fma_", src, &[x86, "-O2", "-mfma"]);
    assert!(asm.contains("vfmadd") && asm.contains("sd"), "{asm}");
    assert!(!asm.contains("call"), "{asm}");
    let asm = asm_for_at("c17_isa_fma512_", src, &[x86, "-O2", "-mavx512f"]);
    assert!(asm.contains("vfmadd"), "{asm}");
    let asm = asm_for_at("c17_isa_fma4_", src, &[x86, "-O2", "-mfma4"]);
    assert!(
        asm.contains("vfmaddsd") && asm.contains("vfmaddss"),
        "{asm}"
    );
    let asm = asm_for_at("c17_isa_nofma_", src, &[x86, "-O2"]);
    assert!(asm.contains("fma"), "{asm}");
    assert!(!asm.contains("vfmadd"), "{asm}");
}

/// The inline `fma` rounds once: 1 + 2^-52 squared, less 1 + 2^-51, is
/// 2^-104 only when the product is not rounded first. Run where the CPU
/// has FMA; the test of that is the program's own.
#[cfg(target_arch = "x86_64")]
#[test]
fn isa_flags_fma_rounds_once() {
    let src = r#"
static double fused(double a, double b, double c) { return __builtin_fma(a, b, c); }
static float fusedf(float a, float b, float c) { return __builtin_fmaf(a, b, c); }
int main(void) {
    __builtin_cpu_init();
    if (!__builtin_cpu_supports("fma"))
        return 0;
    volatile double x = 1.0 + 0x1p-52, y = -(1.0 + 0x1p-51);
    if (fused(x, x, y) != 0x1p-104)
        return 1;
    volatile float xf = 1.0f + 0x1p-23f, yf = -(1.0f + 0x1p-22f);
    if (fusedf(xf, xf, yf) != 0x1p-46f)
        return 2;
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        let flags = vec![opt.to_string(), "-mfma".to_string()];
        assert_eq!(
            crate::common::compile_and_run("isa_fma_rounds_once", src, &flags),
            0,
            "{opt}"
        );
    }
}

/// The baseline's own macros, which the kernel's options take away, as
/// gcc 13's `-dM -E` does: `-mno-sse` takes SSE and everything built on it,
/// SSE2 and SSE4.2 among them; `-mno-sse2` leaves SSE; `-mno-80387` and its
/// spelling `-msoft-float` define `_SOFT_FLOAT`; and `-mgeneral-regs-only`
/// takes all of it, MMX too. Without SSE2 c17 computes no floating point,
/// so it no longer claims IEC 60559 arithmetic.
#[test]
fn isa_flags_baseline_macros() {
    const NAMES: [&str; 8] = [
        "__SSE__",
        "__SSE2__",
        "__SSE_MATH__",
        "__SSE2_MATH__",
        "__SSE4_2__",
        "__MMX__",
        "_SOFT_FLOAT",
        "__STDC_IEC_559__",
    ];
    let rows: &[(&[&str], &str)] = &[
        (
            &[],
            "__SSE__ __SSE2__ __SSE_MATH__ __SSE2_MATH__ __MMX__ __STDC_IEC_559__",
        ),
        (&["-msse4.2", "-mno-sse"], "__MMX__"),
        (&["-mno-sse2"], "__SSE__ __SSE_MATH__ __MMX__"),
        (
            &["-mno-80387"],
            "__SSE__ __SSE2__ __SSE_MATH__ __SSE2_MATH__ __MMX__ _SOFT_FLOAT __STDC_IEC_559__",
        ),
        (
            &["-msoft-float"],
            "__SSE__ __SSE2__ __SSE_MATH__ __SSE2_MATH__ __MMX__ _SOFT_FLOAT __STDC_IEC_559__",
        ),
        (&["-mgeneral-regs-only"], "_SOFT_FLOAT"),
        (
            &[
                "-mno-sse",
                "-mno-mmx",
                "-mno-sse2",
                "-mno-3dnow",
                "-mno-avx",
                "-mno-80387",
                "-mno-fp-ret-in-387",
            ],
            "_SOFT_FLOAT",
        ),
        (
            &["-mno-sse", "-msse2"],
            "__SSE__ __SSE2__ __SSE_MATH__ __SSE2_MATH__ __MMX__ __STDC_IEC_559__",
        ),
    ];
    for (flags, want) in rows {
        let mut argv = vec!["--target=x86_64-unknown-linux-gnu", "-dM"];
        argv.extend_from_slice(flags);
        let r = preprocess_text("baseline_macros", "", &argv);
        assert!(r.success, "{flags:?}: {}", r.stderr);
        let on: Vec<&str> = NAMES
            .iter()
            .copied()
            .filter(|m| {
                r.stdout
                    .lines()
                    .any(|l| l.starts_with(&format!("#define {m} ")))
            })
            .collect();
        assert_eq!(on.join(" "), *want, "{flags:?}");
    }
}
