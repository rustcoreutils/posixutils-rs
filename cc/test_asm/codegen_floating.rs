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

use crate::test_asm::asm_probe::asm_for_at;

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
