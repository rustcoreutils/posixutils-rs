//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Definitions of `aarch64_vector_pcs` functions at run time, under qemu: a
// caller written in assembly fills q8-q23, all 128 bits of each, calls a
// c17-defined vector-PCS function, and hands back what the registers hold
// afterwards, which the vector procedure call standard says is what they
// held before. The assembly half of the tests is
// `test_asm/codegen_vector_pcs.rs`.
//

use crate::common::{aarch64_cross_available, run_c17, work_dir, AARCH64_TARGET_ARGS};

/// The caller: `probe(pattern, out)` loads q8-q23 from `pattern[0..16]` and
/// the arguments q0 and q1 from `pattern[16..18]`, calls `vf`, and stores
/// q8-q23 to `out[0..16]` and the result to `out[16]`. It is a base-PCS
/// function itself, so it keeps d8-d15 for its own caller.
const PROBE_S: &str = r#"
    .text
    .global probe
    .type probe, %function
probe:
    .cfi_startproc
    stp x29, x30, [sp, #-96]!
    .cfi_def_cfa_offset 96
    .cfi_offset 29, -96
    .cfi_offset 30, -88
    mov x29, sp
    stp d8, d9, [sp, #16]
    stp d10, d11, [sp, #32]
    stp d12, d13, [sp, #48]
    stp d14, d15, [sp, #64]
    str x19, [sp, #80]
    .cfi_offset 19, -16
    mov x19, x1
    ldp q8, q9, [x0, #0]
    ldp q10, q11, [x0, #32]
    ldp q12, q13, [x0, #64]
    ldp q14, q15, [x0, #96]
    ldp q16, q17, [x0, #128]
    ldp q18, q19, [x0, #160]
    ldp q20, q21, [x0, #192]
    ldp q22, q23, [x0, #224]
    ldp q0, q1, [x0, #256]
    bl vf
    str q0, [x19, #256]
    stp q8, q9, [x19, #0]
    stp q10, q11, [x19, #32]
    stp q12, q13, [x19, #64]
    stp q14, q15, [x19, #96]
    stp q16, q17, [x19, #128]
    stp q18, q19, [x19, #160]
    stp q20, q21, [x19, #192]
    stp q22, q23, [x19, #224]
    ldr x19, [sp, #80]
    ldp d8, d9, [sp, #16]
    ldp d10, d11, [sp, #32]
    ldp d12, d13, [sp, #48]
    ldp d14, d15, [sp, #64]
    ldp x29, x30, [sp], #96
    .cfi_def_cfa_offset 0
    ret
    .cfi_endproc
    .size probe, .-probe
"#;

/// Compiled by gcc, under the base standard: `noise` changes every register
/// that standard lets it -- v16-v31 whole and the top half of v8-v15 -- and
/// walks the stack, which must go through the vector-PCS frame that called
/// it to `main`. `main` runs `probe` and checks the registers came back.
const DRIVER_C: &str = r#"
#include <string.h>
#include <unwind.h>
typedef float v4sf __attribute__((vector_size(16)));
int probe(const void *pattern, void *out);
__attribute__((aarch64_vector_pcs)) v4sf vf(v4sf a, v4sf b);
int main(void);

static int frames, saw_main;
static _Unwind_Reason_Code step(struct _Unwind_Context *ctx, void *arg)
{
    (void)arg;
    void *fn = _Unwind_FindEnclosingFunction((void *)_Unwind_GetIP(ctx));
    frames++;
    if (fn == (void *)main)
        saw_main = 1;
    return _URC_NO_REASON;
}

__attribute__((noinline)) double noise(double x)
{
    __asm__ volatile(
        "movi v8.2d, #0\n movi v9.2d, #0\n movi v10.2d, #0\n movi v11.2d, #0\n"
        "movi v12.2d, #0\n movi v13.2d, #0\n movi v14.2d, #0\n movi v15.2d, #0\n"
        "movi v16.2d, #0\n movi v17.2d, #0\n movi v18.2d, #0\n movi v19.2d, #0\n"
        "movi v20.2d, #0\n movi v21.2d, #0\n movi v22.2d, #0\n movi v23.2d, #0\n"
        "movi v24.2d, #0\n movi v31.2d, #0\n"
        ::: "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",
            "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v31");
    _Unwind_Backtrace(step, 0);
    return x + 1;
}

int main(void)
{
    unsigned char pattern[18][16], out[17][16];
    for (int r = 0; r < 18; r++)
        for (int i = 0; i < 16; i++)
            pattern[r][i] = (unsigned char)(r * 16 + i + 1);
    v4sf a = {1, 2, 3, 4}, b = {0.5f, 0.25f, 2, 8};
    memcpy(pattern[16], &a, 16);
    memcpy(pattern[17], &b, 16);
    memset(out, 0, sizeof out);
    probe(pattern, out);
    for (int r = 0; r < 16; r++)
        if (memcmp(out[r], pattern[r], 16) != 0)
            return 10 + r;
    v4sf got, want = vf(a, b);
    memcpy(&got, out[16], 16);
    for (int i = 0; i < 4; i++)
        if (got[i] != want[i] || got[i] == 0)
            return 2;
    /* Through noise, vf (and inner) and probe: a walk that stopped in the
       vector-PCS frame never gets there. */
    if (frames && !saw_main)
        return 3;
    return 0;
}
"#;

/// The vector-PCS functions c17 defines, one per entry point `vf` may be:
/// selected by `-DVARIANT=n`. Each computes `a * b + a` on its way and must
/// give back q8-q23 as it found them.
///
/// 1. calls `noise`, a base-PCS function: every one of q8-q23 is the
///    callee's to destroy, so all are saved;
/// 2. keeps a dozen vectors live at once without a call, so the allocator
///    reaches into v16-v23 and its own scratch registers;
/// 3. clobbers some of them in an asm statement;
/// 4. calls another vector-PCS function, defined here too.
const CALLEE_C: &str = r#"
typedef float v4sf __attribute__((vector_size(16)));
double noise(double);
#define VPCS __attribute__((aarch64_vector_pcs, noinline))
#if VARIANT == 1
VPCS v4sf vf(v4sf a, v4sf b)
{
    double k = noise(1.0);
    return a * b + a * (float)(k - 1.0);
}
#elif VARIANT == 2
VPCS v4sf vf(v4sf a, v4sf b)
{
    v4sf t0 = a * b, t1 = a + b, t2 = a - b, t3 = b * b, t4 = a * a, t5 = t0 + t1;
    v4sf t6 = t2 * t3, t7 = t4 - t5, t8 = t6 + t7, t9 = t0 * t2, t10 = t1 * t3;
    v4sf t11 = t9 - t10;
    v4sf s = t0 + t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11;
    return t0 + a + (s - s);
}
#elif VARIANT == 3
VPCS v4sf vf(v4sf a, v4sf b)
{
    __asm__ volatile("movi v8.2d, #0\n movi v12.2d, #0\n movi v16.2d, #0\n movi v23.2d, #0"
                     ::: "v8", "v12", "v16", "v23");
    return a * b + a;
}
#else
VPCS v4sf inner(v4sf a, v4sf b)
{
    double k = noise(2.0);
    return a * b * (float)(k - 2.0);
}
VPCS v4sf vf(v4sf a, v4sf b)
{
    return inner(a, b) + a;
}
#endif
"#;

/// Each variant, at -O0 and -O2: q8-q23 come back whole, the result is
/// right, and the stack walk from inside the callee reaches `main`. Under
/// the stack protector too, which moves the saved registers to the top of
/// the frame.
#[test]
fn vector_pcs_definition_preserves_q8_to_q23() {
    if !aarch64_cross_available() {
        eprintln!("SKIP vector_pcs_definition_preserves_q8_to_q23: no aarch64 cross toolchain");
        return;
    }
    let dir = work_dir("vpcs_def");
    let probe = dir.path().join("probe.s");
    std::fs::write(&probe, PROBE_S).unwrap();
    let driver = dir.path().join("driver.c");
    std::fs::write(&driver, DRIVER_C).unwrap();
    let callee = dir.path().join("callee.c");
    std::fs::write(&callee, CALLEE_C).unwrap();
    let runs: [&[&str]; 3] = [&["-O0"], &["-O2"], &["-O2", "-fstack-protector-all"]];
    for variant in 1..=4 {
        for (i, opt) in runs.iter().enumerate() {
            let s = dir.path().join(format!("callee{variant}_{i}.s"));
            let define = format!("-DVARIANT={variant}");
            let mut args = AARCH64_TARGET_ARGS.to_vec();
            args.extend_from_slice(opt);
            args.extend_from_slice(&[
                &define,
                "-S",
                "-o",
                s.to_str().unwrap(),
                callee.to_str().unwrap(),
            ]);
            let run = run_c17(&args);
            assert!(run.success, "variant {variant} {opt:?}: {}", run.stderr);
            let code = crate::common::cross_link_and_run(
                "vpcs_def",
                &[
                    s.to_str().unwrap(),
                    probe.to_str().unwrap(),
                    driver.to_str().unwrap(),
                ],
            );
            assert_eq!(code, 0, "variant {variant} {opt:?}");
        }
    }
}
