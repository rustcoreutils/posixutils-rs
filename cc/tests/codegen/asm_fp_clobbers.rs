//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// An inline asm that clobbers floating-point or vector registers: no value
// may be live in one across it, and a callee-saved one (aarch64 d8-d15) is
// the function's to preserve for its caller.
//

#[cfg(target_arch = "x86_64")]
use crate::common::compile_and_run;
use crate::common::compile_and_run_aarch64;

/// x86-64: `c` and `a` are live across an asm that zeroes %xmm0-%xmm2. The
/// clobbers were ignored, so `c` sat in %xmm0 through it and the function
/// returned 0 + a.
#[cfg(target_arch = "x86_64")]
#[test]
fn asm_fp_clobbers_end_live_values_x86_64() {
    let src = r#"
__attribute__((noinline)) double f(double a, double b) {
    double c = a * b;
    __asm__ volatile("xorps %%xmm0, %%xmm0\n\txorps %%xmm1, %%xmm1\n\txorps %%xmm2, %%xmm2"
                     ::: "xmm0", "xmm1", "xmm2");
    return c + a;
}
int main(void) { return f(3.0, 4.0) != 15.0; }
"#;
    assert_eq!(compile_and_run("asm_fp_clobbers", src, &[]), 0);
    assert_eq!(
        compile_and_run("asm_fp_clobbers_o2", src, &["-O2".to_string()]),
        0
    );
}

/// aarch64: the same with v0-v2, and a caller in assembly that keeps
/// values in d8-d15 across a function whose asm clobbers them.
#[test]
fn asm_fp_clobbers_end_live_values_and_save_d8_d15_aarch64() {
    let src = r#"
__asm__(".text\n.globl check\ncheck:\n"
        "\tstp x29, x30, [sp, #-80]!\n\tmov x29, sp\n"
        "\tstp d8, d9, [sp, #16]\n\tstp d10, d11, [sp, #32]\n"
        "\tstp d12, d13, [sp, #48]\n\tstp d14, d15, [sp, #64]\n"
        "\tfmov d8, #8.0\n\tfmov d9, #9.0\n\tfmov d10, #10.0\n\tfmov d11, #11.0\n"
        "\tfmov d12, #12.0\n\tfmov d13, #13.0\n\tfmov d14, #14.0\n\tfmov d15, #15.0\n"
        "\tblr x0\n"
        "\tmov w0, #1\n"
        "\tfmov d0, #8.0\n\tfcmp d8, d0\n\tb.ne 1f\n"
        "\tfmov d0, #11.0\n\tfcmp d11, d0\n\tb.ne 1f\n"
        "\tfmov d0, #15.0\n\tfcmp d15, d0\n\tb.ne 1f\n"
        "\tmov w0, #0\n"
        "1:\tldp d8, d9, [sp, #16]\n\tldp d10, d11, [sp, #32]\n"
        "\tldp d12, d13, [sp, #48]\n\tldp d14, d15, [sp, #64]\n"
        "\tldp x29, x30, [sp], #80\n\tret\n");
int check(void (*fn)(void));
__attribute__((noinline)) void clobbers(void) {
    __asm__ volatile("fmov d8, xzr\n\tfmov d11, xzr\n\tfmov d15, xzr"
                     ::: "d8", "v11", "d15");
}
__attribute__((noinline)) double f(double a, double b) {
    double c = a * b;
    __asm__ volatile("fmov d0, xzr\n\tfmov d1, xzr\n\tfmov d2, xzr"
                     ::: "v0", "v1", "d2");
    return c + a;
}
int main(void) {
    if (check(clobbers)) return 1;
    return f(3.0, 4.0) != 15.0 ? 2 : 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        if let Some(rc) = compile_and_run_aarch64("asm_fp_clobbers_a64", src, opt) {
            assert_eq!(rc, 0, "{opt}");
        }
    }
}
