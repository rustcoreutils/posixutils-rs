//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The bundled intrinsic headers -- SSE through SSE4.2 on x86-64, a core
// `arm_neon.h` on aarch64 -- checked against values gcc computed on the
// hardware. Each fixture in simd/ exits 0, or the number of its first
// failing check. They need no -m flags: every function is available at
// every level, and only the feature macros follow the flags.
//

#[cfg(target_arch = "x86_64")]
use crate::common::compile_and_run;
use crate::common::{aarch64_cross_available, compile_and_run_aarch64};

/// Run an x86-64 fixture at -O0 and -O2.
#[cfg(target_arch = "x86_64")]
fn run_x86(name: &str, src: &str) {
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(name, src, &[opt.to_string()]),
            0,
            "{name} at {opt}"
        );
    }
}

/// Also consolidates `simd_xmmintrin_brings_sse2` (check 180 of the
/// fixture): gcc's `<xmmintrin.h>` brings the SSE2 header with it, and mesa's
/// `half_float.h` relies on that: it includes only `<xmmintrin.h>` and
/// writes `__m128i`, beside a `"v"` asm operand of type `__m128`. The
/// fixture includes only `<xmmintrin.h>`, so that property holds.
#[cfg(target_arch = "x86_64")]
#[test]
fn simd_mmintrin_xmmintrin() {
    run_x86("simd_sse", include_str!("simd/e2e_xmmintrin.c"));
}

#[cfg(target_arch = "x86_64")]
#[test]
fn simd_emmintrin() {
    run_x86("simd_sse2", include_str!("simd/e2e_emmintrin.c"));
}

#[cfg(target_arch = "x86_64")]
#[test]
fn simd_pmmintrin_tmmintrin() {
    run_x86("simd_ssse3", include_str!("simd/e2e_tmmintrin.c"));
}

#[cfg(target_arch = "x86_64")]
#[test]
fn simd_smmintrin_nmmintrin() {
    run_x86("simd_sse4", include_str!("simd/e2e_smmintrin.c"));
}

/// The umbrella headers gather the rest, `<x86intrin.h>` adds the time-stamp
/// counter and the bit helpers, and the intrinsic headers exist on x86-64
/// alone.
#[cfg(target_arch = "x86_64")]
#[test]
fn simd_umbrella_headers() {
    let src = r#"
#include <x86intrin.h>
int main(void) {
    __m128i a = _mm_set_epi32(4, 3, 2, 1);
    __m128i b = _mm_shuffle_epi8(a, _mm_set1_epi8(0));
    __m128 f = _mm_set1_ps(2.0f);
    void *p = _mm_malloc(64, 64);
    int ok = _mm_cvtsi128_si32(b) == 0x01010101
        && _mm_cvtss_f32(_mm_sqrt_ps(_mm_mul_ps(f, f))) == 2.0f
        && _mm_popcnt_u32(0xff) == 8
        && _mm_crc32_u8(0, 1) != 0
        && ((unsigned long)p & 63) == 0
        && __rdtsc() != 0
        && _bit_scan_forward(8) == 3;
    _mm_free(p);
    return ok ? 0 : 1;
}
"#;
    run_x86("simd_umbrella", src);
}

/// SHA-NI (`<shaintrin.h>`), checked against gcc's values where the CPU
/// has the instructions.
#[cfg(target_arch = "x86_64")]
#[test]
fn simd_shaintrin() {
    run_x86("simd_sha", include_str!("simd/e2e_shaintrin.c"));
}

/// RTM (`<rtmintrin.h>` and `_xtest`, through `<immintrin.h>` and
/// `<x86intrin.h>`), as glibc's lock elision uses them: the status
/// constants are Intel's, and where the CPU has RTM a transaction that
/// aborts explicitly reports its code to the fallback path. glibc includes
/// `<x86intrin.h>`.
#[cfg(target_arch = "x86_64")]
#[test]
fn simd_rtm_intrinsics() {
    let src = r#"
#include <x86intrin.h>
_Static_assert(_XBEGIN_STARTED == ~0u, "started");
_Static_assert(_XABORT_EXPLICIT == 1 && _XABORT_RETRY == 2 && _XABORT_CONFLICT == 4, "a");
_Static_assert(_XABORT_CAPACITY == 8 && _XABORT_DEBUG == 16 && _XABORT_NESTED == 32, "b");
_Static_assert(_XABORT_CODE(0xfe000001u) == 0xfe, "code");
__attribute__((noinline)) static unsigned attempt(volatile int *x) {
    unsigned status = _xbegin();
    if (status == _XBEGIN_STARTED) {
        int inside = _xtest();
        *x += 1;
        if (inside)
            _xabort(0xfe);
        _xend();
    }
    return status;
}
int main(void) {
    volatile int x = 0;
    __builtin_cpu_init();
    if (!__builtin_cpu_supports("rtm"))
        return _xbegin == 0;
    if (_xtest())
        return 2;
    for (int tries = 0; tries < 100; tries++) {
        unsigned status = attempt(&x);
        if (status == _XBEGIN_STARTED)
            return 3;
        if ((status & _XABORT_EXPLICIT) && _XABORT_CODE(status) == 0xfe)
            return x == 0 ? 0 : 4;
    }
    return 0;
}
"#;
    run_x86("simd_rtm", src);
    let asm = crate::common::asm_for_at("c17_rtm_", src, &["-O2", "-mrtm"]);
    for insn in ["xbegin", "xend", "xabort", "xtest"] {
        assert!(asm.contains(insn), "no {insn}:\n{asm}");
    }
}

/// `<cpuid.h>`'s `bit_*` and `signature_*` macros are gcc's, name for name
/// and value for value: the host gcc's header answers the same table.
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
#[test]
fn simd_cpuid_bits_match_gcc() {
    const NAMES: &[&str] = &[
        "bit_SSE3",
        "bit_PCLMUL",
        "bit_LZCNT",
        "bit_SSSE3",
        "bit_FMA",
        "bit_CMPXCHG16B",
        "bit_SSE4_1",
        "bit_SSE4_2",
        "bit_MOVBE",
        "bit_POPCNT",
        "bit_AES",
        "bit_XSAVE",
        "bit_OSXSAVE",
        "bit_AVX",
        "bit_F16C",
        "bit_RDRND",
        "bit_CMPXCHG8B",
        "bit_CMOV",
        "bit_MMX",
        "bit_FXSAVE",
        "bit_SSE",
        "bit_SSE2",
        "bit_LAHF_LM",
        "bit_ABM",
        "bit_SSE4a",
        "bit_PRFCHW",
        "bit_XOP",
        "bit_LWP",
        "bit_FMA4",
        "bit_TBM",
        "bit_MWAITX",
        "bit_MMXEXT",
        "bit_LM",
        "bit_3DNOWP",
        "bit_3DNOW",
        "bit_CLZERO",
        "bit_WBNOINVD",
        "bit_FSGSBASE",
        "bit_SGX",
        "bit_BMI",
        "bit_HLE",
        "bit_AVX2",
        "bit_BMI2",
        "bit_RTM",
        "bit_AVX512F",
        "bit_AVX512DQ",
        "bit_RDSEED",
        "bit_ADX",
        "bit_AVX512IFMA",
        "bit_CLFLUSHOPT",
        "bit_CLWB",
        "bit_AVX512PF",
        "bit_AVX512ER",
        "bit_AVX512CD",
        "bit_SHA",
        "bit_AVX512BW",
        "bit_AVX512VL",
        "bit_PREFETCHWT1",
        "bit_AVX512VBMI",
        "bit_PKU",
        "bit_OSPKE",
        "bit_WAITPKG",
        "bit_AVX512VBMI2",
        "bit_SHSTK",
        "bit_GFNI",
        "bit_VAES",
        "bit_AVX512VNNI",
        "bit_VPCLMULQDQ",
        "bit_AVX512BITALG",
        "bit_AVX512VPOPCNTDQ",
        "bit_RDPID",
        "bit_MOVDIRI",
        "bit_MOVDIR64B",
        "bit_ENQCMD",
        "bit_CLDEMOTE",
        "bit_KL",
        "bit_AVX5124VNNIW",
        "bit_AVX5124FMAPS",
        "bit_AVX512VP2INTERSECT",
        "bit_AVX512FP16",
        "bit_IBT",
        "bit_UINTR",
        "bit_PCONFIG",
        "bit_SERIALIZE",
        "bit_TSXLDTRK",
        "bit_AMX_BF16",
        "bit_AMX_TILE",
        "bit_AMX_INT8",
        "bit_AMX_COMPLEX",
        "bit_RAOINT",
        "bit_AVXVNNI",
        "bit_AVX512BF16",
        "bit_CMPCCXADD",
        "bit_AMX_FP16",
        "bit_HRESET",
        "bit_AVXIFMA",
        "bit_AVXVNNIINT8",
        "bit_AVXNECONVERT",
        "bit_PREFETCHI",
        "bit_XSAVEOPT",
        "bit_XSAVEC",
        "bit_XSAVES",
        "bit_PTWRITE",
        "bit_AESKLE",
        "bit_WIDEKL",
        "signature_AMD_ebx",
        "signature_AMD_ecx",
        "signature_AMD_edx",
        "signature_INTEL_ebx",
        "signature_INTEL_ecx",
        "signature_INTEL_edx",
        "signature_CENTAUR_ebx",
        "signature_CENTAUR_ecx",
        "signature_CENTAUR_edx",
        "signature_CYRIX_ebx",
        "signature_CYRIX_ecx",
        "signature_CYRIX_edx",
        "signature_TM1_ebx",
        "signature_TM1_ecx",
        "signature_TM1_edx",
        "signature_TM2_ebx",
        "signature_TM2_ecx",
        "signature_TM2_edx",
        "signature_NSC_ebx",
        "signature_NSC_ecx",
        "signature_NSC_edx",
        "signature_NEXGEN_ebx",
        "signature_NEXGEN_ecx",
        "signature_NEXGEN_edx",
        "signature_RISE_ebx",
        "signature_RISE_ecx",
        "signature_RISE_edx",
        "signature_SIS_ebx",
        "signature_SIS_ecx",
        "signature_SIS_edx",
        "signature_UMC_ebx",
        "signature_UMC_ecx",
        "signature_UMC_edx",
        "signature_VIA_ebx",
        "signature_VIA_ecx",
        "signature_VIA_edx",
        "signature_VORTEX_ebx",
        "signature_VORTEX_ecx",
        "signature_VORTEX_edx",
        "signature_SHANGHAI_ebx",
        "signature_SHANGHAI_ecx",
        "signature_SHANGHAI_edx",
    ];
    let table = |var: &str| {
        let values: Vec<String> = NAMES.iter().map(|n| format!("    {n},")).collect();
        format!(
            "#include <cpuid.h>\nconst unsigned {var}[] = {{\n{}\n}};\n",
            values.join("\n")
        )
    };
    let c17_unit = table("c17_bits");
    let host_unit = format!(
        "{}extern const unsigned c17_bits[];\nint main(void) {{\n    \
         for (unsigned i = 0; i < sizeof host_bits / sizeof *host_bits; i++)\n        \
         if (c17_bits[i] != host_bits[i]) return 10 + (int)i;\n    return 0;\n}}\n",
        table("host_bits")
    );
    if let Some(code) = crate::common::compile_with_host_cc("cpuid_bits", &c17_unit, &host_unit) {
        assert_eq!(
            code,
            0,
            "first mismatch: {:?}",
            NAMES.get((code - 10) as usize)
        );
    }
    // And `__get_cpuid` answers as the instruction does on this machine.
    let src = r#"
#include <cpuid.h>
int main(void) {
    unsigned a, b, c, d, max = __get_cpuid_max(0, 0);
    if (max < 1 || !__get_cpuid(1, &a, &b, &c, &d))
        return 1;
    __builtin_cpu_init();
    if (!!(d & bit_SSE2) != !!__builtin_cpu_supports("sse2"))
        return 2;
    if (!!(c & bit_SSE4_2) != !!__builtin_cpu_supports("sse4.2"))
        return 3;
    if (max >= 7 && __get_cpuid_count(7, 0, &a, &b, &c, &d)
        && !!(b & bit_AVX2) != !!__builtin_cpu_supports("avx2"))
        return 4;
    return __get_cpuid(0x7fffffff, &a, &b, &c, &d) ? 5 : 0;
}
"#;
    run_x86("simd_cpuid_bits", src);
}

#[test]
fn simd_arm_neon() {
    let src = include_str!("simd/e2e_arm_neon.c");
    if cfg!(target_arch = "aarch64") {
        for opt in ["-O0", "-O2"] {
            assert_eq!(
                crate::common::compile_and_run("simd_neon", src, &[opt.to_string()]),
                0,
                "simd_neon at {opt}"
            );
        }
        return;
    }
    if !aarch64_cross_available() {
        return;
    }
    for opt in ["-O0", "-O2"] {
        if let Some(code) = compile_and_run_aarch64("simd_neon", src, opt) {
            assert_eq!(code, 0, "simd_neon on aarch64 at {opt}");
        }
    }
}
