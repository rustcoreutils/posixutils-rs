//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly cases of tests/codegen/floating.rs, in process.
//

use crate::test_asm::asm_probe::{
    asm_for, asm_for_at, calls_any, mentions, AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX,
};

/// `__int128` <-> `_Float16` goes through `float`, never a half<->TI helper:
/// Apple's runtime has no `__fixhfti` or `__floattihf`, so a call to one
/// failed the link on macOS arm64. Through `float` is exact both ways: a half
/// widens to a float exactly, and an integer too large for a float to hold
/// exactly (2^24 and up) is beyond the largest half (65504) and becomes
/// infinite either way, so the two roundings never differ from one.
#[test]
fn codegen_int128_float16_conversions_go_through_float() {
    let src = "_Float16 s2h(__int128 x) { return x; }\n\
               _Float16 u2h(unsigned __int128 x) { return x; }\n\
               __int128 h2s(_Float16 h) { return h; }\n\
               unsigned __int128 h2u(_Float16 h) { return h; }\n";
    for triple in [AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX] {
        let asm = asm_for("int128_half", triple, src);
        for helper in ["__fixhfti", "__fixunshfti", "__floattihf", "__floatuntihf"] {
            assert!(!mentions(&asm, helper), "{triple}: calls {helper}:\n{asm}");
        }
        for via_float in ["__floattisf", "__floatuntisf", "__fixsfti", "__fixunssfti"] {
            assert!(
                calls_any(&asm, &[via_float]),
                "{triple}: no call of {via_float}:\n{asm}"
            );
        }
    }
}

/// `fisttp` is SSE3, which the x86-64 baseline c17 targets does not include:
/// gcc converts a long double to an integer by switching the x87 control word
/// to truncation around a `fistp`, then restoring it. The value was always
/// right; the instruction raised SIGILL on a processor without SSE3. (The
/// program half, every rounding mode, is
/// `codegen_x86_64_long_double_to_integer_is_baseline` in
/// tests/codegen/floating.rs.)
#[test]
fn codegen_x86_64_long_double_to_integer_emits_no_fisttp() {
    if !cfg!(all(target_arch = "x86_64", target_os = "linux")) {
        return;
    }
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_at(
            "ld_to_int_asm",
            "int f(long double x) { return (int)x; }\n\
             long g(long double x) { return (long)x; }\n\
             unsigned long h(long double x) { return (unsigned long)x; }\n\
             short k(long double x) { return (short)x; }\n",
            &[opt],
        );
        assert!(!asm.contains("fisttp"), "{opt}: fisttp emitted:\n{asm}");
    }
}
