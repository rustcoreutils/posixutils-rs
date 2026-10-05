//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly cases of tests/builtins/bit_ops.rs, in process.
//

use crate::test_compile::asm_for;

/// `popcnt` is not in the x86-64 baseline -- it arrived with SSE4.2-era
/// processors -- and c17 targets the baseline, as gcc does without
/// `-mpopcnt`. It was emitted unconditionally, so a popcount or parity
/// raised SIGILL on a processor without it. gcc calls libgcc's
/// `__popcountdi2`; c17 counts inline instead. (The run halves are in
/// tests/builtins/bit_ops.rs.)
#[test]
fn builtins_popcount_uses_baseline_instructions() {
    for opt in ["-O0", "-O2"] {
        let asm = asm_for(
            "popcount_asm",
            "int a(unsigned x) { return __builtin_popcount(x); }\n\
             int b(unsigned long x) { return __builtin_popcountl(x); }\n\
             int c(unsigned x) { return __builtin_parity(x); }\n\
             int d(unsigned long long x) { return __builtin_parityll(x); }\n",
            &["--target=x86_64-unknown-linux-gnu", opt],
        );
        assert!(!asm.contains("popcnt"), "{opt}: popcnt emitted:\n{asm}");
    }
}
