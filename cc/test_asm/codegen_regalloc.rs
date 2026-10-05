//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Register allocation, seen in the assembly: properties a program's exit
// status cannot show. Moved from `tests/codegen/regalloc.rs`.
//

use super::asm_probe::{asm_for_with, body_of, AARCH64_LINUX};

/// A value is read back at the width it was stored at.
///
/// A comparison's result is an `int`, and spilled to the frame it is stored
/// as one -- `str w16, [x29, #136]` -- but `Cbr` and `Select` read their
/// condition as sixty-four bits whatever it was: `ldr x9, [x29, #136]`. The
/// upper half is whatever the frame held there, and only the prologue's
/// zeroing of the whole frame made it 0. Fourteen comparisons live across a
/// call, more than aarch64 has callee-saved registers for, put some of them
/// in the frame.
///
/// The aarch64 run of the same program is the integration test of the same
/// name.
#[test]
fn codegen_a_spilled_condition_is_read_at_its_own_width() {
    let src = r#"
__attribute__((noinline)) int touch(int x) { return x + 1; }
__attribute__((noinline)) int many_fcmp(double a, double b) {
    int c0 = a < b, c1 = a > b, c2 = a <= b, c3 = a >= b, c4 = a == b,
        c5 = a != b, c6 = a < b, c7 = a > b, c8 = a <= b, c9 = a >= b,
        c10 = a == b, c11 = a != b, c12 = a < b, c13 = a > b;
    touch(0);
    int r = 0;
    if (c0) r |= 1;    if (c1) r |= 2;    if (c2) r |= 4;    if (c3) r |= 8;
    if (c4) r |= 16;   if (c5) r |= 32;   if (c6) r |= 64;   if (c7) r |= 128;
    if (c8) r |= 256;  if (c9) r |= 512;  if (c10) r |= 1024; if (c11) r |= 2048;
    if (c12 ? c13 : 0) r |= 4096;
    return r;
}
"#;
    for opt in ["-O1", "-O2"] {
        let asm = asm_for_with("spilled_cond", AARCH64_LINUX, src, &[opt]);
        let body = body_of(&asm, "many_fcmp");
        let mut stored_w: std::collections::HashSet<&str> = std::collections::HashSet::new();
        for line in body.lines().map(str::trim) {
            let Some((op, rest)) = line.split_once(' ') else {
                continue;
            };
            let Some((reg, addr)) = rest.split_once(", ") else {
                continue;
            };
            if op == "str" && reg.starts_with('w') {
                stored_w.insert(addr);
            }
            assert!(
                !(op == "ldr" && reg.starts_with('x') && stored_w.contains(addr)),
                "at {opt}, `{line}` reads 64 bits of a slot stored as 32:\n{body}"
            );
        }
    }
}
