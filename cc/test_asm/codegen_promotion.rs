//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly cases of tests/codegen/promotion.rs, in process: promotion of
// locals out of memory. Frame size is invisible to a program's exit status, so
// these go through `frame_size` rather than through a return code.
//

use crate::test_asm::asm_probe::{
    asm_for_with, body_of, count_in_body, frame_size, AARCH64_LINUX, X86_64_LINUX,
};

/// A frame holds only what the function uses.
///
/// Two reservations were made whatever the function did: the x87 scratch,
/// sixteen bytes at the bottom of every x86-64 frame though only an x87
/// conversion or an x87 `asm` operand stages a value through it, and every
/// local the linearizer created, though the optimizer may forward and delete
/// every access to one -- `folded` reads back the element it just stored.
/// Now neither costs a function that does not use it, and a function that
/// does still gets the scratch. (The program that runs these functions is the
/// test of the same name in tests/codegen/promotion.rs.)
#[test]
fn codegen_a_frame_holds_only_what_the_function_uses() {
    let src = "\
int plus1(int x) { return x + 1; }
int folded(void) { int a[4] = {1, 2, 3, 4}; return a[2]; }
int sum(const int *p, int n) { int s = 0; for (int i = 0; i < n; i++) s += p[i]; return s; }
long double widen(int x) { return x; }
float root(float x) { __asm__(\"fsqrt\" : \"+t\"(x)); return x; }
";
    let asm = asm_for_with("frame_uses", X86_64_LINUX, src, &["-O2"]);
    for f in ["plus1", "folded", "sum"] {
        let body = body_of(&asm, f);
        assert!(
            frame_size(&asm, f).is_none() && !body.contains("subq"),
            "{f} needs no frame:\n{body}"
        );
    }
    for f in ["widen", "root"] {
        let frame = frame_size(&asm, f).unwrap_or(0);
        assert!(
            frame >= 16,
            "{f} stages a value through the x87 scratch:\n{asm}"
        );
    }
    let a64 = asm_for_with(
        "frame_uses_a64",
        AARCH64_LINUX,
        "int folded(void) { int a[4] = {1, 2, 3, 4}; return a[2]; }\n",
        &["-O2"],
    );
    assert!(frame_size(&a64, "folded").is_none(), "aarch64:\n{a64}");
}

/// Ten address-free int locals in straight-line code.
///
/// Each unpromoted local costs 8 bytes of frame (slots are `size.max(8)` and
/// never reused), so before promotion this reserved 112 bytes.
#[test]
fn codegen_straight_line_locals_leave_memory() {
    let src = r#"
int straight(int x) {
    int a = x + 1;  int b = a * 2;  int c = b - 3;
    int d = c + 4;  int e = d * 5;  int f = e - 6;
    int g = f + 7;  int h = g * 8;  int i = h - 9;
    int j = i + 10;
    return j;
}
"#;
    let asm = asm_for_with("straight_line_locals", X86_64_LINUX, src, &["-O2"]);
    let frame = frame_size(&asm, "straight").unwrap_or(0);
    assert!(
        frame <= 16,
        "ten straight-line int locals must not each keep a stack slot, \
         got a {frame}-byte frame:\n{asm}"
    );
}

/// A function with no locals at all still framed its incoming parameter,
/// because the parameter's own spill slot was itself a single-block local.
#[test]
fn codegen_parameter_only_function_needs_no_frame() {
    let src = "int nolocal(int x) { return x + 1; }\n";
    let asm = asm_for_with("parameter_only", X86_64_LINUX, src, &["-O2"]);
    let frame = frame_size(&asm, "nolocal").unwrap_or(0);
    assert!(
        frame == 0,
        "a function whose only value is its parameter needs no frame, \
         got {frame} bytes:\n{asm}"
    );
}

/// Constant folding has to survive the hop from one statement to the next.
///
/// Promotion alone is not enough: it turns the `Load` into a `Copy`, and
/// `instcombine` reads constants off the pseudo's kind, which a `Copy`
/// target does not have. Both halves are needed for this to reach `21`.
#[test]
fn codegen_constants_fold_across_statements() {
    let src = "int trivial(void) { int a = 2 + 3; int b = a * 4; return b + 1; }\n";
    let asm = asm_for_with("fold_across_statements", X86_64_LINUX, src, &["-O2"]);
    assert_eq!(
        count_in_body(&asm, "trivial", "imul"),
        0,
        "a chain of integer constants must fold, not multiply at run time:\n{asm}"
    );
    assert!(
        asm.contains("$21"),
        "2+3 then *4 then +1 is 21, and it should appear as an immediate:\n{asm}"
    );
}
