//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// IEEE binary128: `long double` on aarch64 Linux, `_Float128` everywhere.
// No target computes it in hardware, so every operation is a libgcc call --
// but only after the optimizer, which folds the operations exactly first.
// The assembly half, compiled in process; the programs that run are in
// `tests/codegen/binary128.rs`.
//

use super::asm_probe::{asm_for_with, AARCH64_LINUX, X86_64_LINUX};

/// Each constant operation below has its answer at compile time, and every
/// `link_error` call goes with its branch.
///
/// The operations used to become `__addtf3`, `__netf2` and the rest before the
/// optimizer ran, and a call is not a value any folder can see into: all four
/// branches survived `-O2` on aarch64, and `builtins/complex-1` failed to link
/// for its `long double _Complex` cases. `-3.5L` was a `__negtf2` call.
#[test]
fn binary128_constants_fold_before_the_libcalls() {
    let src = "
        extern void link_error(void);
        TY neg(void) { return -3.5SFX; }
        void f(void) {
            if (1.0SFX + 2.0SFX != 3.0SFX) link_error();
            if (6.0SFX * 0.5SFX != 3.0SFX) link_error();
            if ((double)(1.0SFX / 4.0SFX) != 0.25) link_error();
            if ((int)(7.0SFX - 0.5SFX) != 6) link_error();
            if (!(-1.0SFX < 0.0SFX)) link_error();
        }
    ";
    for (triple, ty, sfx) in [
        (AARCH64_LINUX, "long double", "L"),
        (X86_64_LINUX, "_Float128", "F128"),
        (AARCH64_LINUX, "_Float128", "F128"),
    ] {
        let src = src.replace("TY", ty).replace("SFX", sfx);
        for opt in ["-O1", "-O2"] {
            let asm = asm_for_with("b128_fold", triple, &src, &[opt]);
            for callee in [
                "link_error",
                "__addtf3",
                "__multf3",
                "__divtf3",
                "__subtf3",
                "__negtf2",
                "__netf2",
                "__lttf2",
                "__trunctfdf2",
                "__fixtfsi",
            ] {
                assert!(
                    !asm.contains(callee),
                    "{triple} {ty} {opt}: {callee} should have folded away:\n{asm}"
                );
            }
        }
    }
}

/// Without the optimizer the same operations are still the libgcc calls.
#[test]
fn binary128_is_still_a_libcall_at_o0() {
    let src = "long double f(long double a, long double b) { return -(a + b); }";
    let asm = asm_for_with("b128_o0", AARCH64_LINUX, src, &["-O0"]);
    for callee in ["__addtf3", "__negtf2"] {
        assert!(asm.contains(callee), "missing {callee}:\n{asm}");
    }
}
